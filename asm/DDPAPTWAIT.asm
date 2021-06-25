*          DATA SET DDPAPTWAIT AT LEVEL 005 AS OF 03/30/12                      
*PHASE PAPTWAIA                                                                 
*INCLUDE REGSAVE                                                                
         TITLE 'PANAPT MOVE REQUEST: WAIT FOR LLA REFRESH TO FINISH'            
***********************************************************************         
*                                                                     *         
* WHEN THE PANAPT-GENERATED "LOAD" JOB RUNS WHICH PROMOTES THE LOAD   *         
* MODULES IN A MOVE REQUEST, THE INSTALLATION OF THE LOAD MODULES     *         
* ISN'T ACTUALLY COMPLETE UNTIL THE "PR" JOB COMPLETES, WHICH         *         
* REFRESHES THE LLA CACHE ON SY1. THIS PROGRAM RUNS JUST AFTER THE    *         
* "PR" JOB IS SHIPPED TO SY1. ALL IT DOES IS ISSUE A WTOR, AND WAIT   *         
* FOR OPS/MVS TO ISSUE A REPLY. THAT REPLY HAPPENS ONLY AFTER THE     *         
* "PR" JOB COMPLETES ON SY1 (VIA PROGRAM PAPTDONE). WHEN THIS JOB     *         
* COMPLETES, THE "LOAD" JOB CONTINUES EXECUTION.                      *         
*                                                                     *         
* THIS PROGRAM RECEIVES AN MVS PARAMETER WHICH IS OF THE FORMAT       *         
* XXXNNNNNN, WHERE XXX IS THE PANAPT ENVIRONMENT (DDS OR SBX), AND    *         
* NNNNNN IS THE MOVE REQUEST NUMBER.                                  *         
*                                                                     *         
***********************************************************************         
PAPTWAIT CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,PAPTWAIT,=V(REGSAVE)                                           
*                                                                               
         LR    R1,RC                                                            
         AHI   R1,-4                                                            
         L     R1,0(R1)                                                         
         L     R1,0(R1)            A(PARM= FROM EXEC JCL CARD)                  
         CLC   =H'9',0(R1)                                                      
         BE    *+6                                                              
         DC    H'0'                PARM ABSENT OR LENGTH INCORRECT              
         MVC   OPERENV,2(R1)       PANAPT ENVIRONMENT                           
         MVC   OPERMR#,5(R1)       MOVE REQUEST NUMBER                          
*                                                                               
*&&DO                                                                           
* DEIS: WE NO LONGER NEED TO DO THIS CHECK, BECAUSE THE DDJMLOAD                
*       MODEL WON'T GENERATE THE JCL TO EXECUTE THIS PROGRAM UNLESS             
*       THE MOVE REQUEST WAS SUBMITTED FROM SY7.  -- APR/2012                   
*                                                                               
         L     R1,X'10'(,0)        POINT TO CPU ID                              
         L     R1,X'C4'(R1)                                                     
         LA    R1,X'10'(R1)        A(FOUR CHARACTER CPU ID)                     
         CLC   =C'SYT ',0(R1)      ARE WE RUNNING ON SYT?                       
         BE    DONE                YES: EXIT! (WE'RE ONLY TESTING)              
*&&                                                                             
*                                                                               
LOOP     DS    0H                                                               
         WTOR  TEXT=(OPERMSGL,REPLY,4,OPERECB)                                  
         WAIT  ECB=OPERECB                                                      
*                                                                               
         CLC   REPLY,=C'OKAY'      REPLY MUST BE 'OKAY'                         
         BE    DONE                                                             
*                                                                               
         XC    OPERECB,OPERECB                                                  
         XC    REPLY,REPLY                                                      
         LA    R3,INVMSGL          INVALID RESPONSE                             
         WTO   TEXT=(R3)                                                        
         B     LOOP                                                             
*                                                                               
DONE     DS    0H                                                               
         XBASE                                                                  
         EJECT                                                                  
         DS    0D                                                               
         DC    C'***ECB**'         EYE-CATCHER                                  
OPERECB  DC    F'0'                OPERATOR REPLY ECB                           
*                                                                               
         DC    C'**REPLY='         EYE-CATCHER                                  
REPLY    DC    XL4'00'             OPS/MVS REPLY (MUST BE 'OKAY')               
*                                                                               
OPERMSGL DC    AL2(60)                                                          
OPERMSG  DC    CL60' '                                                          
         ORG   OPERMSG                                                          
         DC    C'PANAPT01: '                                                    
OPERENV  DS    CL3                 PANAPT ENVIRONMENT (DDS OR SBX)              
         DC    C' MR '                                                          
OPERMR#  DS    CL6                 MOVE REQUEST NUMBER                          
         DC    C' PAUSED. READY TO CONTINUE?'                                   
         ORG                                                                    
*                                                                               
INVMSGL  DC    AL2(60)                                                          
         DC    CL60'INVALID REPLY. REPLY MUST BE "OKAY".'                       
         SPACE 3                                                                
         LTORG                                                                  
         SPACE 3                                                                
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005DDPAPTWAIT03/30/12'                                      
         END                                                                    
