*          DATA SET REREP2001S AT LEVEL 035 AS OF 08/31/94                      
*PHASE RE2001A,                                                                 
         TITLE 'REREP2001 - DISCREPANCY REPORT SPECS'                           
**********************************************************************          
*                                                                    *          
*        REREP2001 --- REPPACK DISCREPANCY REPORT HEADLINE SPEC      *          
*                                                                    *          
* ------------------------------------------------------------------ *          
* UPDATE HISTORY:                                                    *          
*                                                                    *          
* MAR10/94 (BU ) --- NEW VERSION WITH INTERFACE CODE                 *          
*                     MATCH THE HEADLINE 'STANDARD' DU JOUR.         *          
*                                                                    *          
* AUG31/94 (BU ) --- INCLUDE INV # IN HEADER FIELDS                  *          
*                                                                    *          
*                                                                    *          
*                                                                    *          
*                                                                    *          
*                    ***  END TOMBSTONE  ***                         *          
**********************************************************************          
RE2001   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,CONTRACTS                                                   
         SPROG 0,1,2,3                                                          
         ASPEC H01,002,REP                                                      
         ASPEC H01,050,C'CONTRACT RECONCILIATION REPORT'                        
         ASPEC H01,100,RENUM                                                    
         ASPEC H01,120,PAGE                                                     
         SPACE 1                                                                
         ASPEC H02,002,REQUESTOR                                                
         ASPEC H02,050,30C'-'                                                   
         ASPEC H02,100,RUN                                                      
         SPACE 1                                                                
         ASPEC H04,054,PERIOD                                                   
         SPACE 1                                                                
         ASPEC H08,090,C'ORDERED         INVOICE        DIFFERENCE'             
         ASPEC H09,086,15C'-'                                                   
         ASPEC H09,102,15C'-'                                                   
         ASPEC H09,118,15C'-'                                                   
         SPACE 1                                                                
         SPROG 0,1                                                              
         ASPEC H04,002,GROUP                                                    
         ASPEC H04,021,SUBGROUP                                                 
         ASPEC H05,002,STATION                                                  
         SPACE 1                                                                
         SPROG 3                                                                
         ASPEC H05,002,C'INTERFACE CODE'                                        
         SPACE 1                                                                
         SPROG 0,1,3                                                            
         ASPEC H07,006,C'AGENCY CODE'                                           
         ASPEC H08,006,C'AGENCY NAME'                                           
         ASPEC H09,001,20C'-'                                                   
         ASPEC H07,025,C'ADVERTISER CODE'                                       
         ASPEC H08,025,C'ADVERTISER NAME'                                       
         ASPEC H09,023,20C'-'                                                   
         ASPEC H07,049,C'PRODUCT CODE'                                          
         ASPEC H08,049,C'PRODUCT NAME'                                          
         ASPEC H09,045,20C'-'                                                   
         ASPEC H07,067,C'OFF  CON # T'                                          
         ASPEC H08,067,C'SAL  INV # Y'                                          
         ASPEC H09,067,C'--- --------'                                          
         SPACE 1                                                                
         SPROG 0,3                                                              
         ASPEC H08,080,C'MONTH'                                                 
         ASPEC H09,080,C'-----'                                                 
*                                                                               
*                                                                               
* > > > > > > > > >  > > END OF REREP2001 < < < < < < < < < < < < <             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'035REREP2001S08/31/94'                                      
         END                                                                    
