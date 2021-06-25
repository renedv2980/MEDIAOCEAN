*          DATA SET ACBAT50    AT LEVEL 002 AS OF 08/10/00                      
*PHASE T60250A                                                                  
         TITLE 'BATCH PROGRAM - JOBBER BUFFERS - T60250'                        
T60250   CSECT                                                                  
         PRINT NOGEN                                                            
         DC    A(COLTAB-T60250)                                                 
         DC    A(COLTABX-COLTAB)                                                
         DC    A(OPVTAB-T60250)                                                 
         DC    A(OPVTABX-OPVTAB)                                                
*                                                                               
COLTABLB DC    CL8'*COLTAB*'                                                    
COLTAB   DC    2400X'00'           COLUMN OUTPUT TABLE                          
COLTABX  EQU   *                                                                
*                                                                               
OPVTABLB DC    CL8'*OPVTAB*'                                                    
OPVTAB   DC    1200X'00'           OPERAND VALUE TABLE                          
OPVTABX  EQU   *                                                                
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002ACBAT50   08/10/00'                                      
         END                                                                    
