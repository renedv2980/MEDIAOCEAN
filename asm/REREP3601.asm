*          DATA SET REREP3601  AT LEVEL 008 AS OF 08/31/00                      
*          DATA SET REREP3601  AT LEVEL 007 AS OF 06/04/80                      
*PHASE RE3601A                                                                  
         TITLE 'SPECS FOR OFFICE SHARE SUMMARY'                                 
RE3601   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,CONTRACTS                                                   
         RSPEC MAXLINES,56                                                      
         SPROG 0,1,2                                                            
         ASPEC F1,1,REQDETS                                                     
         ASPEC H1,2,RUN                                                         
         ASPEC H1,47,C'OFFICE SHARE SUMMARY'                                    
         ASPEC H2,47,20C'-'                                                     
         ASPEC H1,88,RENUM                                                      
         ASPEC H1,101,PAGE                                                      
         ASPEC H4,2,REP                                                         
         ASPEC H4,46,PERIOD                                                     
         ASPEC H4,88,REQUESTOR                                                  
         ASPEC H5,46,BASIS                                                      
         ASPEC H8,2,C'STATION OFFICE'                                           
         ASPEC H9,2,C'------- ------'                                           
         ASPEC H8,35,C'THIS YEAR ESTIMATED       LAST YEAR ESTIMATED'           
         ASPEC H9,35,C'AMOUNT        SHARE       AMOUNT        SHARE'           
         ASPEC H8,87,C'LAST YEAR    ACTUAL'                                     
         ASPEC H9,87,C'AMOUNT        SHARE'                                     
         SPROG 0                                                                
         ASPEC H5,2,GROUP                                                       
         ASPEC H6,2,SUBGROUP                                                    
         SPROG 1                                                                
         ASPEC H5,88,C'STATION ORDER'                                           
         SPROG 0,2                                                              
         ASPEC H5,88,C'STATIONS BY GROUP'                                       
         SPROG 3                                                                
         ASPEC F1,1,REQDETS                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008REREP3601 08/31/00'                                      
         END                                                                    
