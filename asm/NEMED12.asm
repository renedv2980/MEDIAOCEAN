*          DATA SET NEMED12    AT LEVEL 005 AS OF 08/10/00                      
*PHASE T31E12A                                                                  
         TITLE 'T31E12 - SPECS FOR WEEKLY FLOWCHART'                            
T31E12   CSECT                                                                  
         SPROG 0,1                                                              
         SSPEC H1,1,C'MEDIA     NETWORK T.V.'                                   
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,75,AGYNAME                                                    
         SSPEC H2,75,AGYADD                                                     
         SSPEC H4,1,C'CLIENT'                                                   
         SSPEC H5,1,C'PRODUCT'                                                  
         SSPEC H6,1,C'ESTIMATE'                                                 
         SSPEC H4,41,PERIOD                                                     
         SSPEC H4,75,NETREP                                                     
         SSPEC H4,102,PAGE                                                      
         SSPEC H5,75,C'DAYPART'                                                 
         SSPEC H6,75,C'TARGET  XXXXXXX ESTIMATED'                               
         SPROG 0                                                                
         SSPEC H10,1,C'NET  EST     PROG   PROGRAM NAME   UNITS'                
         SSPEC H11,1,C'---  ---     CODE   ------------   -----'                
         SPROG 1                                                                
         SSPEC H10,1,C'NET  SPT     PROG   PROGRAM NAME   UNITS'                
         SSPEC H11,1,C'---  LEN     CODE   ------------   -----'                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005NEMED12   08/10/00'                                      
         END                                                                    
