*          DATA SET PPREPA901  AT LEVEL 004 AS OF 11/14/08                      
*PHASE PPA901A                                                                  
         TITLE 'PPA901 - ACCENT FILE EXTRACT  - SPECS'                          
PPA901   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,ESTIMATES                                                   
*                                                                               
         SPROG 0                                                                
         PSPEC H1,1,MEDIANAME                                                   
         PSPEC H1,46,C'PRINTPAK ESTIMATE EXTRACT'                               
         PSPEC H2,46,C'-------------------------'                               
*                                                                               
         PSPEC H1,98,AGYNAME                                                    
         PSPEC H2,98,AGYADD                                                     
         PSPEC H3,98,REPORT                                                     
         PSPEC H4,98,PAGE                                                       
*                                                                               
         PSPEC H8,02,C'AGY'                                                     
         PSPEC H8,07,C'MED'                                                     
         PSPEC H8,12,C'CLT'                                                     
         PSPEC H8,17,C'PRD'                                                     
         PSPEC H8,22,C'EST'                                                     
         PSPEC H6,96,C'   CURRENT'                                              
         PSPEC H7,96,C'     MONTH'                                              
         PSPEC H7,117,C'       NET'                                             
         PSPEC H8,33,C'   ORDERED'                                              
         PSPEC H8,54,C'      PAID'                                              
         PSPEC H8,75,C'    BILLED'                                              
         PSPEC H8,96,C'    BILLED'                                              
         PSPEC H8,117,C'PAID TODAY'                                             
*                                                                               
         PSPEC H9,02,C'---'                                                     
         PSPEC H9,07,C'---'                                                     
         PSPEC H9,12,C'---'                                                     
         PSPEC H9,17,C'---'                                                     
         PSPEC H9,22,C'---'                                                     
         PSPEC H9,33,C'   -------'                                              
         PSPEC H9,54,C'      ----'                                              
         PSPEC H9,75,C'    ------'                                              
         PSPEC H9,96,C'    ------'                                              
         PSPEC H9,117,C'----------'                                             
*                                                                               
         DC    X'00'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004PPREPA901 11/14/08'                                      
         END                                                                    
