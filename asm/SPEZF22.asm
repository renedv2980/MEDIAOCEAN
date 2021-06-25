*          DATA SET SPEZF22    AT LEVEL 003 AS OF 08/11/00                      
*PHASE T23022                                                                   
*                                                                               
T23022   TITLE 'SPEZF22 - ESTIMATE, FILM, INV NUM, AND INV ITEM TABLES'         
T23022   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
*        TABLES                                                                 
*                                                                               
         ORG   *+(LENTBLS+500)                                                  
         EJECT                                                                  
*                                                                               
*TABLE DSECTS                                                                   
       ++INCLUDE SPEZF21TBL                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003SPEZF22   08/11/00'                                      
         END                                                                    
