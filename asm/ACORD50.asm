*          DATA SET ACORD50    AT LEVEL 003 AS OF 10/17/18                      
*PHASE T60F50A,*                                                                
         TITLE 'PRODUCTION ORDERS JOBBER BUFFERS - T60F50'                      
T60F50   CSECT                                                                  
         PRINT NOGEN                                                            
         DC    A(COLTAB-T60F50)                                                 
         DC    A(COLTABX-COLTAB)                                                
         DC    A(OPVTAB-T60F50)                                                 
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
**PAN#1  DC    CL21'003ACORD50   10/17/18'                                      
         END                                                                    
