*          DATA SET ACPOP50    AT LEVEL 001 AS OF 01/18/05                      
*PHASE T60A50A                                                                  
         TITLE 'ACPOP50 - PRODUCTION ORDERS PLUS -  JOBBER BUFFERS'             
T60A50   CSECT                                                                  
         PRINT NOGEN                                                            
         DC    A(COLTAB-T60A50)                                                 
         DC    A(COLTABX-COLTAB)                                                
         DC    A(OPVTAB-T60A50)                                                 
         DC    A(OPVTABX-OPVTAB)                                                
*                                                                               
COLTABLB DC    CL8'*COLTAB*'                                                    
COLTAB   DC    3600X'00'           COLUMN OUTPUT TABLE                          
COLTABX  EQU   *                                                                
*                                                                               
OPVTABLB DC    CL8'*OPVTAB*'                                                    
OPVTAB   DC    3600X'00'           OPERAND VALUE TABLE                          
OPVTABX  EQU   *                                                                
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001ACPOP50   01/18/05'                                      
         END                                                                    
