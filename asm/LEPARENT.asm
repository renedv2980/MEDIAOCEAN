*          DATA SET LEPARENT   AT LEVEL 036 AS OF 10/21/99                      
*PHASE LEPARENT                                                                 
         TITLE 'LEPARENT - LE STORED PROCEDURE SHELL'                           
         PRINT GEN                                                              
LEPARENT CSECT                                                                  
LEPARENT RMODE 24                                                               
LEPARENT AMODE 24                                                               
LEPARENT CEEENTRY AUTO=WORKSIZE,MAIN=YES                                        
         SPACE 1                                                                
         REQUS                                                                  
         USING WORKAREA,RD                                                      
         SPACE 1                                                                
         CALL  CEEMOUT,(HELLOMSG,DEST,FBCODE),VL,MF=(E,CALLMOUT)                
         SPACE 1                                                                
         XC    TASKECB,TASKECB     INITIALIZE ECB FOR SPAWNED PROCESS           
         LA    R3,TASKECB                                                       
         LA    R1,PARMMSG          R1=A(PARAMETER STRING TO SUB-TASK)           
         ATTACH EP=LEDDS,ECB=(R3),SZERO=NO                                      
         ST    R1,TASKTCB          SAVE A(SUB-TASK TCB)                         
         WAIT  ECB=TASKECB                                                      
         TM    TASKECB,X'40'       TEST COMPLETE BIT ON                         
         BZ    LEPAR10                                                          
*                                                                               
         CALL  CEEMOUT,(AFTER,DEST,FBCODE),VL,MF=(E,CALLMOUT)                   
*                                                                               
LEPAR10  DETACH TASKTCB                                                         
         SPACE 1                                                                
LEPARX   CEETERM RC=0                                                           
*                                                                               
* CONSTANTS                                                                     
*                                                                               
DEST     DC    F'2'                                                             
CEE000   DC    3F'0'                                                            
*                                                                               
HELLOMSG DC    Y(L'HELLOSTR)                                                    
HELLOSTR DC    C'LANGUAGE ENVIRONMENT CALLED ME!'                               
*                                                                               
AFTER    DC    Y(L'AFTERST)                                                     
AFTERST  DC    C'AFTER RETURN FROM SUB-TASK'                                    
*                                                                               
PARMMSG  DC    C'I AM A MESSAGE PASSED TO SUB-TASK'                             
*                                                                               
PPA      CEEPPA ,                                                               
         SPACE 2                                                                
WORKAREA DSECT                                                                  
         ORG   *+CEEDSASZ                                                       
         SPACE 1                                                                
CALLMOUT CALL  ,(,,),VL,MF=L                                                    
FBCODE   DS    3F                                                               
TASKECB  DS    F                                                                
TASKTCB  DS    A                                                                
DMCB     DS    6F                                                               
         SPACE 1                                                                
         DS    0D                                                               
WORKSIZE EQU   *-WORKAREA                                                       
         CEEDSA ,                                                               
         CEECAA ,                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'036LEPARENT  10/21/99'                                      
         END   LEPARENT                                                         
