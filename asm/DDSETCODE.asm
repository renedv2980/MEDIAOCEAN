*          DATA SET DDSETCODE  AT LEVEL 001 AS OF 07/07/09                      
*PHASE SETCODEA                                                                 
*INCLUDE REGSAVE                                                                
         TITLE 'SET A STEP COND CODE OR ISSUE ABEND'                            
*******************************************************************             
*                                                                               
*  THIS IS THE "JSETCODE" PROGRAM ORIGINALLY WRITTEN BY JOHN                    
*  MCCONNELL, AND MADE DDS-STANDARD BY DEIS.                                    
*                                                                               
*  THIS IS A UTILITY PROGRAM FOR CONTROLLING FLOW OF A BATCH JOB                
*  BY MEANS OF RETURN CODES AND USER ABENDS.                                    
*                                                                               
*  IF NO PARM IS SUPPLIED, THE PROGRAM RETURNS CC=0.                            
*                                                                               
*  IF A PARM IS SUPPLIED, IT MUST CONFORM TO THE FOLLOWING RULES:               
*   1.  MAXIMUM PARM LENGTH IS 10.                                              
*   2.  PARM MUST EITHER BE:                                                    
*         A) FOUR OR FEWER DECIMAL DIGITS                                       
*         B) THE STRING "ABEND," FOLLOWED BY DECIMAL DIGITS AS ABOVE            
*  IF THE PARM IS INVALID, THIS PROGRAM ABENDS WITH A USER 555.                 
*                                                                               
*  THE PARM IS PROCESSED AS FOLLOWS:                                            
*   1.  IF THE NUMBER IS 1000 OR GREATER, IT IS ISSUED AS A USER                
*       ABEND CODE.                                                             
*   2.  IF THE NUMBER IS 999 OR LESS, IT IS RETURNED AS THE STEP                
*       CONDITION CODE IN RF, UNLESS IT IS PREFIXED WITH "ABEND,",              
*       IN WHICH CASE A USER ABEND WILL BE FORCED REGARDLESS OF THE             
*       NUMBER.                                                                 
*                                                                               
*******************************************************************             
SETCODE  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,SETCODE,=V(REGSAVE)                                            
*                                                                               
         LR    R1,RC                                                            
         SHI   R1,4                                                             
         L     R1,0(R1)                                                         
         L     R1,0(R1)            A(PARM= FROM EXEC JCL CARD)                  
*                                                                               
         LH    R2,0(R1)            PARM LENGTH                                  
         LTR   R2,R2               PARM PRESENT ?                               
         BZ    AA0110              NO: RETURN 0                                 
*                                                                               
         LA    R1,2(R1)            POINT TO PARM                                
         CHI   R2,10               TOO LONG ?                                   
         BH    AA01ERR             YES: ABORT                                   
*                                                                               
         MVI   ABENDSW,C'N'        INITIALIZE EXPLICIT ABEND SWITCH             
         CHI   R2,4                GREATER THAN MAX PARM LENGTH?                
         BNH   AA0105              NO: PROCESS NUMERIC VALUE                    
*                                                                               
         CLC   =C'ABEND,',0(R1)    EXPLICIT ABEND ?                             
         BNE   AA01ERR             NO: ABORT                                    
         MVI   ABENDSW,C'Y'        YES: SET SWITCH                              
         SHI   R2,6                DECREMENT COUNTER                            
         LA    R1,6(R1)            ADVANCE POINTER                              
*                                                                               
AA0105   DS    0H                                                               
         LA    R3,4                LENGTH OF RECEIVING FIELD                    
         SR    R3,R2               OFFSET FOR MOVE                              
         LA    R3,PARM#(R3)        POSITION FOR MOVE                            
         BCTR  R2,0                DECREMENT FOR EX                             
         EX    R2,*+8              MOVE TO NUMERIC FIELD                        
         B     *+10                                                             
         MVC   0(0,R3),0(R1)                                                    
*                                                                               
         MVC   FULL,PARM#                                                       
         MVZ   FULL,=C'0000'                                                    
         CLC   FULL,PARM#          ALL NUMERIC?                                 
         BE    AA02                YES: PROCESS REQUEST                         
*                                                                               
AA01ERR  DS    0H                                                               
         ABEND 555                                                              
*                                                                               
AA0110   DS    0H                                                               
         SR    RF,RF                                                            
         ST    RF,RCODE            INDICATE NO PARM                             
*                                                                               
AA02     DS    0H                                                               
         PACK  DUB,PARM#                                                        
         CVB   R3,DUB                                                           
         ST    R3,RCODE            STORE AS RETURN CODE                         
         CLI   ABENDSW,C'Y'        EXPLICIT ABEND?                              
         BE    AA03                YES: DO IT                                   
*                                                                               
         CLI   PARM#,C'0'          GREATER THAN 999 ?                           
         BE    EX01                NO: SET RETURN CODE                          
*                                                                               
AA03     DS    0H                                                               
         ABEND (R3)                YES: ISSUE ABEND                             
*                                                                               
EX01     DS    0H                                                               
         XBASE RC=RCODE                                                         
         SPACE 3                                                                
         LTORG                                                                  
         SPACE 2                                                                
RCODE    DC    F'0'                RETURN CODE: POSIT "NO PARM"                 
*                                                                               
DUB      DS    D                                                                
FULL     DS    F                                                                
PARM#    DC    CL4'0000'                                                        
ABENDSW  DC    C'N'                                                             
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001DDSETCODE 07/07/09'                                      
         END                                                                    
