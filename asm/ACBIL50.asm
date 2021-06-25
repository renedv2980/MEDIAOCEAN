*          DATA SET ACBIL50    AT LEVEL 003 AS OF 08/10/00                      
*PHASE T60E50A                                                                  
         TITLE 'ACBIL50 - CREATIVE BILLING - JOBBER TABLES'                     
T60E50   CSECT                                                                  
         PRINT NOGEN                                                            
         DC    A(COLTAB-T60E50)                                                 
         DC    A(COLTABX-COLTAB)                                                
         DC    A(OPVTAB-T60E50)                                                 
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
**PAN#1  DC    CL21'003ACBIL50   08/10/00'                                      
         END                                                                    
