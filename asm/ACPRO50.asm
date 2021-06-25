*          DATA SET ACPRO50    AT LEVEL 002 AS OF 08/10/00                      
*PHASE T60B50A                                                                  
         TITLE 'T60B50 - ONLINE JOBBER BUFFER'                                  
T60B50   CSECT                                                                  
         PRINT NOGEN                                                            
         DC    A(COLTAB-T60B50)                                                 
         DC    A(COLTABX-COLTAB)                                                
         DC    A(OPVTAB-T60B50)                                                 
         DC    A(OPVTABX-OPVTAB)                                                
*                                                                               
COLTABLB DC    CL8'*COLTAB*'                                                    
COLTAB   DC    8192X'00'           COLUMN OUTPUT TABLE                          
COLTABX  EQU   *                                                                
*                                                                               
OPVTABLB DC    CL8'*OPVTAB*'                                                    
OPVTAB   DC    10240X'00'          OPERAND VALUE TABLE                          
OPVTABX  EQU   *                                                                
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002ACPRO50   08/10/00'                                      
         END                                                                    
