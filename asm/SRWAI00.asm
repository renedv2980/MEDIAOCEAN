*          DATA SET SRWAI00    AT LEVEL 003 AS OF 08/22/00                      
*PHASE T14800A                                                                  
         TITLE '$WAIT - TIMER CONTROLLED SYSTEM WAIT'                           
WAIT     CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 1,*$WAIT**                                                       
         USING WORKD,RC            RC=A(W/S)                                    
         LR    R2,R1                                                            
         USING SRPARMD,R2          R2=A(PARM LIST)                              
         L     R3,SRPARM6                                                       
         USING SRWAIFFD,R3         R3=A(TWA)                                    
         L     RA,SRPARM1                                                       
         USING SYSFACD,RA          RA=A(SYSFAC)                                 
         L     R9,VSSB                                                          
         USING SSBD,R9             R9=A(SSB)                                    
*                                  VALIDATE P1                                  
         LA    R4,SRVP1H                                                        
         LA    R0,15               DEFAULT IS 15 SECOND WAIT                    
         CLI   5(R4),0                                                          
         BE    WAIT2                                                            
         TM    4(R4),X'08'         TEST NUMERIC                                 
         BZ    WAIT2                                                            
         ZIC   R1,5(R4)                                                         
         BCTR  R1,0                                                             
         MVC   DUB,=8C'0'                                                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVZ   DUB(0),8(R4)                                                     
         CLC   DUB,=8C'0'                                                       
         BNE   WAIT2                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R4)                                                      
         CVB   RE,DUB                                                           
         C     RE,MINVAL           AND WITHIN LIMITS                            
         BL    WAIT2                                                            
         C     RE,MAXVAL                                                        
         BH    WAIT2                                                            
         LR    R0,RE                                                            
*                                  SET TIMER 3 GOING                            
WAIT2    GOTO1 VTICTOC,DUB,C'1SET',0                                            
         GOTO1 VTICTOC,DUB,C'3SET',(R0)                                         
         TM    SSBT2+4,X'80'       LOOP TILL TIMER EXPIRES                      
         BZ    *-4                                                              
         XC    SSBT2+4(4),SSBT2+4                                               
WAITX    XMOD1 1                                                                
         EJECT                                                                  
*              LITERALS ETC.                                                    
*                                                                               
MINVAL   DC    F'05'                                                            
MAXVAL   DC    F'60'                                                            
         LTORG                                                                  
*                                                                               
WORKD    DSECT                                                                  
DUB      DS    D                                                                
* FADSECTS                                                                      
       ++INCLUDE FADSECTS                                                       
*                                                                               
SRWAIFFD DSECT                                                                  
         DS    CL64                                                             
* SRWAIFFD                                                                      
       ++INCLUDE SRWAIFFD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003SRWAI00   08/22/00'                                      
         END                                                                    
