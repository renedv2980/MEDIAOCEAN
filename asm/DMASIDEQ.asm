*          DATA SET DMASIDEQ   AT LEVEL 002 AS OF 05/12/87                      
*PHASE ASIDEQ,*,NOAUTO                                                          
*INCLUDE DMDMGRL                                                                
         TITLE 'ASIDEQ - PROGRAM DEQUEUES ALL CPU RESOURCES'                    
         PRINT NOGEN                                                            
ASIDEQ   CSECT                                                                  
         ENTRY SSB                                                              
         ENTRY UTL                                                              
         NBASE 0,**ASID**,WORK=A(DEQWORK)                                       
*                                                                               
         XC    ENQP1(12),ENQP1     BUILD ENQDEQ PARAM LIST                      
         LA    R2,=CL8'CPU'                                                     
         ST    R2,ENQP1                                                         
         MVI   ENQP1,C'D'                                                       
         GOTO1 =V(DMENQDEQ),ENQP1                                               
*                                                                               
ASIEOJ   XBASE                                                                  
         EJECT                                                                  
DUB      DC    D'0'                                                             
DMCB     DC    6F'0'                                                            
*                                                                               
         DC    C'**PARM**'                                                      
ENQP1    DC    F'0'                                                             
ENQP2    DC    F'0'                                                             
ENQP3    DC    F'0'                                                             
ENQP4    DC    F'0'                                                             
*                                                                               
         LTORG                                                                  
*                                                                               
         DS    0D                                                               
         DC    C'**WORK**'                                                      
DEQWORK  DC    200D'0'                                                          
*                                                                               
         DC    C'**UTL***'                                                      
UTL      DC    F'0',X'01000000'                                                 
*                                                                               
         DC    C'**SSB***'                                                      
SSB      DC    D'0'                                                             
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002DMASIDEQ  05/12/87'                                      
         END                                                                    
