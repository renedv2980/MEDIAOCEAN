*          DATA SET DDPAPTDONE AT LEVEL 004 AS OF 05/08/12                      
*PHASE PAPTDONA                                                                 
*INCLUDE REGSAVE                                                                
         TITLE 'PANAPT MOVE REQUEST: LLA REFRESH IS FINISHED'                   
***********************************************************************         
*                                                                     *         
* WHEN THE PANAPT-GENERATED "LOAD" JOB RUNS WHICH PROMOTES THE LOAD   *         
* MODULES IN A MOVE REQUEST, THE INSTALLATION OF THE LOAD MODULES     *         
* ISN'T ACTUALLY COMPLETE UNTIL THE "PR" JOB COMPLETES, WHICH         *         
* REFRESHES THE LLA CACHE ON SY1. THIS PROGRAM RUNS ON SY1 AFTER THE  *         
* LLA REFRESH IS COMPLETE. ALL IT DOES IS ISSUE A WTO, WHICH OPS/MVS  *         
* SEES, CAUSING IT TO REPLY TO THE CORRESPONDING WTOR ON SY7 WHICH    *         
* HAD BEEN PREVIOUSLY ISSUED BY THE "LOAD" JOB (VIA PROGRAM           *         
* PAPTWAIT). THE "LOAD" JOB THEN COMPLETES.                           *         
*                                                                     *         
* THIS PROGRAM RECEIVES AN MVS PARAMETER WHICH IS OF THE FORMAT       *         
* EEENNNNNNSSS IN WHICH:                                              *         
*   EEE:  PANAPT ENVIRONMENT (DDS OR SBX)                             *         
*   NNNNNN: MOVE REQUEST NUMBER                                       *         
*   SSS:  ORIGINATING SYSTEM ID (E.G., SY7)                           *         
*                                                                     *         
***********************************************************************         
PAPTDONE CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,PAPTDONE,=V(REGSAVE)                                           
*                                                                               
         LR    R1,RC                                                            
         AHI   R1,-4                                                            
         L     R1,0(R1)                                                         
         L     R1,0(R1)            A(PARM= FROM EXEC JCL CARD)                  
         CLC   =H'9',0(R1)         IF PARM LEN IS EXACTLY 9...                  
         BE    MAIN10              ...SYSTEM ID IS NOT PRESENT                  
         CLC   =H'12',0(R1)        IF PARM LEN IS EXACTLY 12...                 
         BE    *+6                 ...SYSTEM ID IS PRESENT                      
         DC    H'0'                PARM ABSENT OR LENGTH INCORRECT              
         MVC   OPERSYS,11(R1)      ORIGINATING SYSTEM ID                        
*                                                                               
MAIN10   DS    0H                                                               
         MVC   OPERENV,2(R1)       PANAPT ENVIRONMENT                           
         MVC   OPERMR#,5(R1)       MOVE REQUEST NUMBER                          
*                                                                               
         LA    R3,OPERMSGL                                                      
         WTO   TEXT=(R3)                                                        
*                                                                               
         XBASE                                                                  
         EJECT                                                                  
OPERMSGL DC    AL2(60)                                                          
OPERMSG  DC    CL60' '                                                          
         ORG   OPERMSG                                                          
         DC    C'PANAPT02: '                                                    
OPERENV  DS    CL3                 PANAPT ENVIRONMENT (DDS OR SBX)              
         DC    C' MR '                                                          
OPERMR#  DS    CL6                 MOVE REQUEST NUMBER                          
         DC    C' FROM '                                                        
OPERSYS  DS    CL3                 ORIGINATING SYSTEM ID                        
         DC    C' LLA REFRESH COMPLETE'                                         
         ORG                                                                    
         SPACE 3                                                                
         LTORG                                                                  
         SPACE 3                                                                
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004DDPAPTDONE05/08/12'                                      
         END                                                                    
