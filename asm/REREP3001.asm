*          DATA SET REREP3001  AT LEVEL 001 AS OF 07/25/91                      
*PHASE RE3001A,*                                                                
         TITLE 'RE3001 - REREP3001 - SPECS FOR RADAR REPORT'                    
*                                                                               
*********************************************************************           
*                                                                   *           
*        REREP3001 --- SPECS FOR RADAR REPORT                       *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* 14MAY91 (EFJ) --- INITIAL RELEASE                                 *           
*                                                                   *           
*                                                                   *           
*                                                                   *           
*                                                                   *           
*                                                                   *           
*                                                                   *           
*                                                                   *           
*********************************************************************           
*                                                                               
         PRINT NOGEN                                                            
RE3001   CSECT                                                                  
         FSPEC READ,CONTRACTS                                                   
         FSPEC GET,ADVERTISER                                                   
         FSPEC GET,PRODUCT                                                      
         FSPEC GET,STATION                                                      
         FSPEC GET,AGENCY                                                       
         SPROG 0,1,2,3,4,5,6,7,8                                                
         ASPEC H01,002,REP                                                      
         ASPEC H01,054,C'R A D A R  R E P O R T'                                
         ASPEC H01,100,RENUM                                                    
         ASPEC H01,120,PAGE                                                     
         ASPEC H02,002,REQUESTOR                                                
         ASPEC H02,100,RUN                                                      
         ASPEC H07,002,C'ADVERTISER'                                            
         ASPEC H07,044,C'SPW'                                                   
         ASPEC H08,002,C'PRODUCT'                                               
*                                                                               
         SPROG 0                                                                
         ASPEC H07,023,C'STATION  FLIGHT'                                       
         ASPEC H08,023,C'CONTRACT SPOT LENS'                                    
         ASPEC H08,045,C'#W'                                                    
*                                                                               
         SPROG 1                                                                
         ASPEC H07,023,C'AGENCY'                                                
         ASPEC H08,023,C'SPOT LENGTHS'                                          
*                                                                               
         SPROG 0,1,2,3,4,5,6,7,8                                                
         ASPEC F01,002,REQDETS                                                  
         DC    X'00'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001REREP3001 07/25/91'                                      
         END                                                                    
