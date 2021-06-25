*          DATA SET REREPTK01  AT LEVEL 064 AS OF 01/31/02                      
*PHASE RETK01A,                                                                 
         TITLE 'REREPTK01 - TAKEOVER REPORT SPECS'                              
**********************************************************************          
*                                                                    *          
*        REREPTK01 --- REPPACK TAKEOVER REPORT HDLINE SPEC           *          
*                                                                    *          
* ------------------------------------------------------------------ *          
* UPDATE HISTORY:                                                    *          
*                                                                    *          
* OCT07/97 (BU ) --- INITIAL ENTRY                                   *          
*                                                                    *          
* DEC08/97 (BU ) --- MODIFY FORMAT OF REPORT                         *          
*                                                                    *          
* APR01/99 (JRD) --- ALTERNATE NAME HEADERS                          *          
*                                                                    *          
**********************************************************************          
RETK01   CSECT                                                                  
         PRINT GEN                                                              
*--->    FSPEC READ,CONTRACTS                                                   
         SPROG 0                                                                
         ASPEC H01,002,REP                                                      
         ASPEC H01,058,C'TAKEOVER REPORT'                                       
         ASPEC H01,100,RENUM                                                    
         ASPEC H01,120,PAGE                                                     
         SPACE 1                                                                
         ASPEC H02,002,REQUESTOR                                                
         ASPEC H02,100,RUN                                                      
         SPACE 1                                                                
         ASPEC H04,002,C'ORIGINAL REP'                                          
         ASPEC H04,080,C'STATION'                                               
         ASPEC H05,002,C'CONTRACTS AVAILABLE'                                   
         ASPEC H05,080,C'CONTRACTS TAKEN'                                       
         ASPEC H06,002,C'TAKEOVER DATE'                                         
*                                1         2         3         4                
*                        2.4.6.8.0.2.4.6.8.0.2.4.6.8.0.2.4.6.8.0.2.4.           
         ASPEC H08,002,C'OFF  AGENCY   ADVERT   PRODUCT     CON S/P'            
         ASPEC H08,046,C'      FLIGHT        OLD CON#   NEW CON# '              
         ASPEC H08,096,C'OLD  REP      NEW REP'                                 
         ASPEC H09,002,C'                                   TYP    '            
         ASPEC H09,046,C' --START- --END---                          '          
         ASPEC H09,096,C'----$$----  ----$$----                      '          
         SPACE 1                                                                
*                                                                               
         SPROG 1                                                                
         ASPEC H01,002,REP                                                      
         ASPEC H01,058,C'TAKEOVER REPORT'                                       
         ASPEC H01,100,RENUM                                                    
         ASPEC H01,120,PAGE                                                     
         SPACE 1                                                                
         ASPEC H02,002,REQUESTOR                                                
         ASPEC H02,100,RUN                                                      
         SPACE 1                                                                
*                                1         2         3         4                
*                        2.4.6.8.0.2.4.6.8.0.2.4.6.8.0.2.4.6.8.0.2.4.           
         ASPEC H04,002,C'OFF  AGENCY   ADVERT   PRODUCT     CON S/P'            
***<<<                                                                          
         ASPEC H04,046,C'      FLIGHT        OLD CON#   NEW CON# '              
         ASPEC H04,096,C'OLD  REP      NEW REP'                                 
         ASPEC H05,002,C'                                   TYP    '            
         ASPEC H05,046,C' --START- --END---                          '          
         ASPEC H05,096,C'----$$----  ----$$----                      '          
***<<<                                                                          
         SPACE 1                                                                
         SPROG 2                                                                
         ASPEC H01,002,REP                                                      
         ASPEC H01,058,C'TAKEOVER REPORT'                                       
         ASPEC H01,100,RENUM                                                    
         ASPEC H01,120,PAGE                                                     
         SPACE 1                                                                
         ASPEC H02,002,REQUESTOR                                                
         ASPEC H02,100,RUN                                                      
         SPACE 1                                                                
         ASPEC H04,002,C'ORIGINAL REP'                                          
         ASPEC H04,080,C'STATION'                                               
         ASPEC H05,002,C'CONTRACTS AVAILABLE'                                   
         ASPEC H05,080,C'CONTRACTS TAKEN'                                       
         ASPEC H06,002,C'TAKEOVER DATE'                                         
*                                1         2         3         4                
*                        2.4.6.8.0.2.4.6.8.0.2.4.6.8.0.2.4.6.8.0.2.4.           
         ASPEC H08,002,C'AGENCY  OFF   ADVERT   PRODUCT     CON S/P'            
***:::                                                                          
         ASPEC H08,046,C'      FLIGHT        OLD CON#   NEW CON# '              
         ASPEC H08,096,C'OLD  REP      NEW REP'                                 
         ASPEC H09,002,C'                                   TYP    '            
         ASPEC H09,046,C' --START- --END---                          '          
         ASPEC H09,096,C'----$$----  ----$$----                      '          
***:::                                                                          
*                                                                               
         SPROG 3                                                                
         ASPEC H01,002,REP                                                      
         ASPEC H01,058,C'TAKEOVER REPORT'                                       
         ASPEC H01,100,RENUM                                                    
         ASPEC H01,120,PAGE                                                     
         SPACE 1                                                                
         ASPEC H02,002,REQUESTOR                                                
         ASPEC H02,100,RUN                                                      
         SPACE 1                                                                
*                                1         2         3         4                
*                        2.4.6.8.0.2.4.6.8.0.2.4.6.8.0.2.4.6.8.0.2.4.           
         ASPEC H08,002,C'AGENCY  OFF   ADVERT   PRODUCT     CON S/P'            
***+++                                                                          
         ASPEC H04,046,C'      FLIGHT        OLD CON#   NEW CON# '              
         ASPEC H04,096,C'OLD  REP      NEW REP'                                 
         ASPEC H05,002,C'                                   TYP    '            
         ASPEC H05,046,C' --START- --END---                          '          
         ASPEC H05,096,C'----$$----  ----$$----                      '          
***+++                                                                          
         SPACE 1                                                                
*                                                                               
         SPROG 4                                                                
         ASPEC H01,002,REP                                                      
         ASPEC H01,058,C'TAKEOVER REPORT'                                       
         ASPEC H01,100,RENUM                                                    
         ASPEC H01,120,PAGE                                                     
         SPACE 1                                                                
         ASPEC H02,002,REQUESTOR                                                
         ASPEC H02,100,RUN                                                      
         SPACE 1                                                                
*                                1         2         3         4                
*                        2.4.6.8.0.2.4.6.8.0.2.4.6.8.0.2.4.6.8.0.2.4.           
         ASPEC H08,002,C'      ADVERTISER       PRODUCT     CON S/P'            
***+++                                                                          
         ASPEC H04,046,C'      FLIGHT        OLD CON#   NEW CON# '              
         ASPEC H04,096,C'OLD  REP      NEW REP'                                 
         ASPEC H05,002,C'                                   TYP    '            
         ASPEC H05,046,C' --START- --END---                          '          
         ASPEC H05,096,C'----$$----  ----$$----                      '          
***+++                                                                          
***<<<                                                                          
         SPACE 1                                                                
         SPROG 5                                                                
         ASPEC H01,002,REP                                                      
         ASPEC H01,058,C'TAKEOVER REPORT'                                       
         ASPEC H01,100,RENUM                                                    
         ASPEC H01,120,PAGE                                                     
         SPACE 1                                                                
         ASPEC H02,002,REQUESTOR                                                
         ASPEC H02,100,RUN                                                      
         SPACE 1                                                                
         ASPEC H04,002,C'ORIGINAL REP'                                          
         ASPEC H04,080,C'STATION'                                               
         ASPEC H05,002,C'CONTRACTS AVAILABLE'                                   
         ASPEC H05,080,C'CONTRACTS TAKEN'                                       
         ASPEC H06,002,C'TAKEOVER DATE'                                         
*                                1         2         3         4                
*                        2.4.6.8.0.2.4.6.8.0.2.4.6.8.0.2.4.6.8.0.2.4.           
         ASPEC H08,002,C'      ADVERTISER       PRODUCT     CON S/P'            
***:::                                                                          
         ASPEC H08,046,C'      FLIGHT        OLD CON#   NEW CON# '              
         ASPEC H08,096,C'OLD  REP      NEW REP'                                 
         ASPEC H09,002,C'                                   TYP    '            
         ASPEC H09,046,C' --START- --END---                          '          
         ASPEC H09,096,C'----$$----  ----$$----                      '          
***+++                                                                          
***<<<                                                                          
         SPACE 1                                                                
         SPROG 6                                                                
         ASPEC H01,002,REP                                                      
         ASPEC H01,058,C'TAKEOVER REPORT'                                       
         ASPEC H01,100,RENUM                                                    
         ASPEC H01,120,PAGE                                                     
         SPACE 1                                                                
         ASPEC H02,002,REQUESTOR                                                
         ASPEC H02,100,RUN                                                      
*                                1         2         3         4                
*                        2.4.6.8.0.2.4.6.8.0.2.4.6.8.0.2.4.6.8.0.2.4.           
         ASPEC H09,002,C'   OFF  AGY-OFF AGENCY NAME          ADV  AD'          
***:::                                                                          
         ASPEC H09,046,C'V NAME             SP    PRODUCT           CO'         
         ASPEC H09,091,C'N       FLIGHT          CON#       DOLLAR '            
         ASPEC H10,002,C'                                           '           
         ASPEC H10,046,C'                                           TY'         
         ASPEC H10,091,C'P   --START- -END--   --------    --------'            
***:::                                                                          
         SPACE 1                                                                
         SPROG 7                                                                
         ASPEC H01,002,REP                                                      
         ASPEC H01,058,C'TAKEOVER REPORT'                                       
         ASPEC H01,100,RENUM                                                    
         ASPEC H01,120,PAGE                                                     
         SPACE 1                                                                
         ASPEC H02,002,REQUESTOR                                                
         ASPEC H02,100,RUN                                                      
         SPACE 1                                                                
         ASPEC H04,002,C'ORIGINAL REP'                                          
         ASPEC H04,080,C'STATION'                                               
         ASPEC H05,002,C'NEW REP'                                               
         ASPEC H06,002,C'CONTRACTS AVAILABLE'                                   
         ASPEC H06,080,C'CONTRACTS TAKEN'                                       
         ASPEC H07,002,C'TAKEOVER DATE'                                         
*                                1         2         3         4                
*                        2.4.6.8.0.2.4.6.8.0.2.4.6.8.0.2.4.6.8.0.2.4.           
         ASPEC H09,002,C'   OFF  AGY-OFF AGENCY NAME          ADV  AD'          
***:::                                                                          
         ASPEC H09,046,C'V NAME             SP    PRODUCT           CO'         
         ASPEC H09,091,C'N       FLIGHT          CON#       DOLLAR '            
         ASPEC H10,002,C'                                           '           
         ASPEC H10,046,C'                                           TY'         
         ASPEC H10,091,C'P   --START- -END--   --------    --------'            
***:::                                                                          
*                                                                               
         SPACE 1                                                                
         SPROG 8                                                                
         ASPEC H01,002,REP                                                      
         ASPEC H01,058,C'TAKEOVER REPORT'                                       
         ASPEC H01,100,RENUM                                                    
         ASPEC H01,120,PAGE                                                     
         SPACE 1                                                                
         ASPEC H02,002,REQUESTOR                                                
         ASPEC H02,100,RUN                                                      
*                                1         2         3         4                
*                        2.4.6.8.0.2.4.6.8.0.2.4.6.8.0.2.4.6.8.0.2.4.           
         ASPEC H09,002,C'   AGY-OFF  OFF AGENCY NAME          ADV  AD'          
***:::                                                                          
         ASPEC H09,046,C'V NAME             SP    PRODUCT           CO'         
         ASPEC H09,091,C'N       FLIGHT          CON#       DOLLAR '            
         ASPEC H10,002,C'                                           '           
         ASPEC H10,046,C'                                           TY'         
         ASPEC H10,091,C'P   --START- -END--   --------    --------'            
***:::                                                                          
         SPACE 1                                                                
         SPROG 9                                                                
         ASPEC H01,002,REP                                                      
         ASPEC H01,058,C'TAKEOVER REPORT'                                       
         ASPEC H01,100,RENUM                                                    
         ASPEC H01,120,PAGE                                                     
         SPACE 1                                                                
         ASPEC H02,002,REQUESTOR                                                
         ASPEC H02,100,RUN                                                      
         SPACE 1                                                                
         ASPEC H04,002,C'ORIGINAL REP'                                          
         ASPEC H04,080,C'STATION'                                               
         ASPEC H05,002,C'NEW REP'                                               
         ASPEC H06,002,C'CONTRACTS AVAILABLE'                                   
         ASPEC H06,080,C'CONTRACTS TAKEN'                                       
         ASPEC H07,002,C'TAKEOVER DATE'                                         
*                                1         2         3         4                
*                        2.4.6.8.0.2.4.6.8.0.2.4.6.8.0.2.4.6.8.0.2.4.           
         ASPEC H09,002,C'   AGY-OFF  OFF AGENCY NAME          ADV  AD'          
***:::                                                                          
         ASPEC H09,046,C'V NAME             SP    PRODUCT           CO'         
         ASPEC H09,091,C'N       FLIGHT          CON#       DOLLAR '            
         ASPEC H10,002,C'                                           '           
         ASPEC H10,046,C'                                           TY'         
         ASPEC H10,091,C'P   --START- -END--   --------    --------'            
***:::                                                                          
*                                                                               
* > > > > > > > > >  > > END OF REREPTK01 < < < < < < < < < < < < <             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'064REREPTK01 01/31/02'                                      
         END                                                                    
