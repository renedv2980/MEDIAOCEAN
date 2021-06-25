*          DATA SET REREP2001  AT LEVEL 043 AS OF 02/07/97                      
*PHASE RE2001C,                                                                 
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
* AUG31/94 (DBU) --- INCLUDE TRF # IN HEADER FIELDS                  *          
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
         ASPEC H10,086,15C'-'                                                   
         ASPEC H10,102,15C'-'                                                   
         ASPEC H10,118,15C'-'                                                   
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
         ASPEC H10,001,20C'-'                                                   
         ASPEC H07,025,C'ADVERTISER CODE'                                       
         ASPEC H08,025,C'ADVERTISER NAME'                                       
         ASPEC H10,023,20C'-'                                                   
         ASPEC H07,049,C'PRODUCT CODE'                                          
         ASPEC H08,049,C'PRODUCT NAME'                                          
         ASPEC H10,045,20C'-'                                                   
         ASPEC H07,067,C'OFF CON # T'                                           
         ASPEC H08,067,C'SAL INV # Y'                                           
         ASPEC H09,067,C'    TRF #  '                                           
         ASPEC H10,067,C'--- -------'                                           
         SPACE 1                                                                
         SPROG 0,3                                                              
         ASPEC H08,080,C'MONTH'                                                 
         ASPEC H10,080,C'-----'                                                 
*                                                                               
*                                                                               
* > > > > > > > > >  > > END OF REREP2001 < < < < < < < < < < < < <             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'043REREP2001 02/07/97'                                      
         END                                                                    
