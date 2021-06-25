*          DATA SET DDTEST1    AT LEVEL 022 AS OF 11/06/97                      
*PHASE TEST1                                                                    
*INCLUDE CARDS                                                                  
*INCLUDE CUREDIT                                                                
*INCLUDE PRINT                                                                  
*                                                                               
TEST     CSECT                                                                  
*                                                                               
         NBASE 0,TEST,WORK=A(WORK)                                              
*                                                                               
         CURED  DUB,(15,DUB),0,DECS=NO,FLOAT=1,ZERO=BLANK                       
*                                                                               
         CUREDA DUB,(15,DUB),0,DECS=NO,FLOAT=1,ZERO=BLANK                       
*                                                                               
         CUREDA DUB,(15,DUB),0,DECS=NO,FLOAT=1,DECPNT=FORCE,ZERO=B              
*                                                                               
TESTX    XBASE                                                                  
         EJECT                                                                  
         LTORG                                                                  
CUREDIT  DC    V(CUREDIT)                                                       
SPACES   DC    CL40' '                                                          
COUNTRY  DS    0X                                                               
*&&UK*&& DC    AL1(1)                                                           
*&&US*&& DC    AL1(2)                                                           
*                                                                               
DUB      DS    D                                                                
         DS    CL6                                                              
PVOPT1   DS    X                                                                
PVOPT2   DS    X                                                                
PERVEYE  DS    CL8                                                              
PERVBLK  DS    XL80                                                             
PERVBLKX DS    XL80                                                             
PERVWRK  DS    XL48                                                             
         DS    0D                                                               
ANSBIN   DS    PL8                                                              
FULL     DS    F                                                                
HALF     DS    H                                                                
DATEIN   DS    CL6                                                              
         DS    CL2                                                              
DATEMID  DS    CL12                                                             
DATEOUT  DS    CL12                                                             
NUM1     DS    PL5                                                              
NUM2     DS    PL5                                                              
ANSWER   DS    PL10                                                             
PLIST    DS    6F                                                               
PLISTX   DS    6F                                                               
DMCB     DS    6F                                                               
AOPER    DS    A                                                                
SAVERE   DS    A                                                                
OPERATOR DS    CL1                                                              
CLEANED  DS    CL80                                                             
PLINE    DC    XL1'00'                                                          
PDATA    DC    CL132' '                                                         
*                                                                               
         DS    0D                                                               
         DC    C'WKWKWKWK'                                                      
WORK     DC    200D'0'                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'022DDTEST1   11/06/97'                                      
         END                                                                    
