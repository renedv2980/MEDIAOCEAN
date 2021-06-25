*          DATA SET SPWRI1A    AT LEVEL 008 AS OF 07/15/13                      
*PHASE T2041AA                                                                  
*                                                                               
*        THIS MODULE HOLDS TABLES FOR CASHFLOW                                  
*              ONLY USED OFFLINE                                                
*                                                                               
T2041A   CSECT                                                                  
         DC    CL8'*CASHTBLS'      EYECATCHER                                   
*                                                                               
*        DISPLACEMENTS TO WORKAREAS                                             
*                                                                               
CSHTBLAS DS    0A                  DISPLACEMENTS TABLE TO WORKAREAS             
AINVTBL  DC    A(INVTABLE-T2041A)  INVOICE         TABLE                        
ASTBTBL  DC    A(STBTABLE-T2041A)  STATION BILLING TABLE                        
ACSHREC  DC    A(SVCSHREC-T2041A)  CASHIER RECORD SAVEAREA                      
ACLTREC  DC    A(CLTRECSV-T2041A)  CLIENT RECORD  SAVEAREA                      
         DS    16A                 SPARE                                        
*                                                                               
         DS    0D                                                               
         DC    CL8'INVTABLE'                                                    
INVTABLE DS    2000XL18            INVOICE TABLE                                
         DC    X'FF'               EOT                                          
*                                                                               
         DS    0D                                                               
         DC    CL8'STBTABLE'                                                    
STBTABLE DS    1000XL25            STATION BILLING TABLE                        
         DC    X'FF'               EOT                                          
*                                                                               
         DS    0D                                                               
         DC    CL8'CASHREC'                                                     
SVCSHREC DS    XL4096              CASHIER RECORD SAVEAREA                      
*                                                                               
         DS    0D                                                               
         DC    CL8'CLTREC'                                                      
CLTRECSV DS    XL2000              CLIENT RECORD SAVEAREA                       
*                                                                               
         DS    2000XL1             SPARE                                        
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008SPWRI1A   07/15/13'                                      
         END                                                                    
