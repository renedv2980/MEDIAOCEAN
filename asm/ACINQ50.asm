*          DATA SET ACINQ50    AT LEVEL 002 AS OF 08/10/00                      
*PHASE T60650A                                                                  
         TITLE 'ACCOUNT ENQUIRY MK2 - JOBBER BUFFER - T60650'                   
T60650   CSECT                                                                  
         PRINT NOGEN                                                            
         DC    A(COLTAB-T60650)                                                 
         DC    A(COLTABX-COLTAB)                                                
         DC    A(OPVTAB-T60650)                                                 
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
**PAN#1  DC    CL21'002ACINQ50   08/10/00'                                      
         END                                                                    
