*          DATA SET REREP7001S AT LEVEL 009 AS OF 05/17/02                      
*PHASE RE7001A,*                                                                
         TITLE 'REREP7001 (RE7001) --- SPECS FOR STATION LISTING'               
*                                                                               
*********************************************************************           
*                                                                   *           
*   REREP7001 - RE7001 - SPECS FOR STATION LISTING                  *           
*                                                                   *           
*-------------------------------------------------------------------*           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* MAR18/92 (MRR) --- REFORMAT TO STANDARD HEADER                    *           
*                                                                   *           
* DEC08/92 (BU ) --- REMOVED THE REQUEST DETAILS FOOTING, WHICH     *           
*                    WAS NOT FUNCTIONING CORRECTLY.                 *           
*                                                                   *           
* JUL02/93 (SKU) --- ADD NEW HEADER FOR INTEREP -> MARKET CODE/NAME *           
*                                                                   *           
*                                                                   *           
*********************************************************************           
*                                                                               
         PRINT NOGEN                                                            
RE7001   CSECT                                                                  
         FSPEC READ,STATIONS                                                    
         FSPEC GET,OWNER                                                        
                                                                                
         SPROG 0,1,2                                                            
         ASPEC H01,002,REP                                                      
         ASPEC H01,059,C'STATION LISTING'                                       
         ASPEC H01,100,RENUM                                                    
         ASPEC H01,120,PAGE                                                     
         ASPEC H02,001,REQUESTOR                                                
         ASPEC H02,059,15C'-'                                                   
         ASPEC H02,100,RUN                                                      
****>    ASPEC F01,002,REQDETS                                                  
                                                                                
         SPROG 0,1,3                                                            
         ASPEC H07,031,C'CHANN AFF'                                             
         ASPEC H08,031,C'----- ---'                                             
         ASPEC H06,041,C'JOINING/ GROUP/'                                       
         ASPEC H07,041,C'LEAVING  SUB-GROUP'                                    
         ASPEC H08,041,C'-------- ------------'                                 
         ASPEC H06,066,C'STA'                                                   
         ASPEC H07,063,C'RK CON TS'                                             
         ASPEC H08,063,C'-- --- --'                                             
         ASPEC H07,074,C'TVB REGION'                                            
         ASPEC H08,073,12C'-'                                                   
         ASPEC H07,090,C'OWNER'                                                 
         ASPEC H08,086,13C'-'                                                   
         ASPEC H07,107,C'COMPETING STATIONS'                                    
         ASPEC H08,100,32C'-'                                                   
                                                                                
         SPROG 0                                                                
         ASPEC H07,002,C'STATION        MARKET'                                 
         ASPEC H08,002,7C'-'                                                    
         ASPEC H08,010,20C'-'                                                   
                                                                                
         SPROG 1                                                                
         ASPEC H07,009,C'MARKET        STATION'                                 
         ASPEC H08,002,20C'-'                                                   
         ASPEC H08,023,7C'-'                                                    
                                                                                
         SPROG 3                                                                
         ASPEC H07,002,C'STATION        MARKET'                                 
         ASPEC H08,002,7C'-'                                                    
         ASPEC H08,010,20C'-'                                                   
         ASPEC H07,107,C'MARKET CODE/NAME  '                                    
         ASPEC H08,100,32C'-'                                                   
                                                                                
         SPROG 4,5,6                                                            
         ASPEC H01,002,REP                                                      
         ASPEC H01,055,C'CLOSED STATION LISTING'                                
         ASPEC H01,100,RENUM                                                    
         ASPEC H01,120,PAGE                                                     
         ASPEC H02,001,REQUESTOR                                                
         ASPEC H02,055,22C'-'                                                   
         ASPEC H02,100,RUN                                                      
****>    ASPEC F01,002,REQDETS                                                  
                                                                                
         SPROG 4,5,7                                                            
         ASPEC H07,031,C'CHANN AFF'                                             
         ASPEC H08,031,C'----- ---'                                             
         ASPEC H06,041,C'JOINING/ GROUP/'                                       
         ASPEC H07,041,C'LEAVING  SUB-GROUP'                                    
         ASPEC H08,041,C'-------- ------------'                                 
         ASPEC H06,066,C'STA'                                                   
         ASPEC H07,063,C'RK CON TS'                                             
         ASPEC H08,063,C'-- --- --'                                             
         ASPEC H07,074,C'TVB REGION'                                            
         ASPEC H08,073,12C'-'                                                   
         ASPEC H07,090,C'OWNER'                                                 
         ASPEC H08,086,13C'-'                                                   
         ASPEC H07,107,C'COMPETING STATIONS'                                    
         ASPEC H08,100,32C'-'                                                   
                                                                                
         SPROG 4                                                                
         ASPEC H07,002,C'STATION        MARKET'                                 
         ASPEC H08,002,7C'-'                                                    
         ASPEC H08,010,20C'-'                                                   
                                                                                
         SPROG 5                                                                
         ASPEC H07,009,C'MARKET        STATION'                                 
         ASPEC H08,002,20C'-'                                                   
         ASPEC H08,023,7C'-'                                                    
                                                                                
         SPROG 7                                                                
         ASPEC H07,002,C'STATION        MARKET'                                 
         ASPEC H08,002,7C'-'                                                    
         ASPEC H08,010,20C'-'                                                   
         ASPEC H07,107,C'MARKET CODE/NAME  '                                    
         ASPEC H08,100,32C'-'                                                   
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009REREP7001S05/17/02'                                      
         END                                                                    
