*          DATA SET NEMED11    AT LEVEL 002 AS OF 08/10/00                      
*PHASE T31E11A                                                                  
         TITLE 'T31E11 - SPECS FOR WEEKLY STEWARDSHIP REPORT'                   
T31E11   CSECT                                                                  
         PRINT NOGEN                                                            
         SPROG 0,1,2,3                                                          
         SSPEC H1,1,C'MEDIA     NETWORK T.V.'                                   
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,75,AGYNAME                                                    
         SSPEC H2,75,AGYADD                                                     
         SSPEC H4,1,C'CLIENT'                                                   
         SSPEC H5,1,C'PRODUCT'                                                  
         SSPEC H6,1,C'ESTIMATE'                                                 
         SSPEC H4,38,PERIOD                                                     
         SSPEC H4,75,NETREP                                                     
         SSPEC H4,102,PAGE                                                      
         SSPEC H5,75,C'DAYPART'                                                 
         SSPEC H6,75,C'TARGET  XXXXXXX ESTIMATED'                               
         SSPEC H10,1,C'DAYPART NET  TOTAL   TOTAL   ----CPM----'                
         SSPEC H11,1,C'------- ---  GRPS.  DOLLARS  HOME TARGET'                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002NEMED11   08/10/00'                                      
         END                                                                    
