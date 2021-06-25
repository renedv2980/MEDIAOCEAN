*          DATA SET SRLOG00    AT LEVEL 005 AS OF 08/22/00                      
*PHASE T11400A                                                                  
         TITLE '$LOG - TURN ON CRT LOGGING'                                     
LOG      CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 005,**$LOG**                                                     
         USING LOGWORK,RC          RC=A(W/S)                                    
         LR    R2,R1                                                            
         USING SRPARMD,R2          R2=A(PARMS)                                  
         L     RA,SRPARM6                                                       
         USING SRLOGFFD,RA         RA=A(TWA)                                    
         L     R5,SRPARM3                                                       
         USING UTLD,R5             R5,A(UTL)                                    
         L     R6,SRPARM1                                                       
         USING SYSFACD,R6          R6=A(SYSFACS)                                
         L     R7,VSSB                                                          
         USING SSBD,R7             R7=A(SSB)                                    
         XC    LOGMESS,LOGMESS                                                  
*                                                                               
         LA    R1,SRVP1H                                                        
         CLI   5(R1),0                                                          
         BE    LOG2                                                             
         CLC   8(5,R1),=C'CLEAR'                                                
         BNE   ERR1                                                             
         OC    SSBLGUTL,SSBLGUTL                                                
         BZ    ERR2                                                             
         XC    SSBLGUTL,SSBLGUTL                                                
         XC    SSBLGLN,SSBLGLN                                                  
         MVC   TSVCREQ,=X'01EE'    $BYE                                         
         B     OK1                                                              
*                                                                               
LOG2     OC    SSBLGUTL,SSBLGUTL                                                
         BNZ   ERR3                                                             
         B     OK2                                                              
         EJECT                                                                  
*              ERRORS AND EXIT                                                  
*                                                                               
ERR1     MVC   LOGMESS(19),=C'INVALID INPUT FIELD'                              
         B     ERRX                                                             
ERR2     MVC   LOGMESS(27),=C'ERROR LOGGING IS NOT ACTIVE'                      
         B     ERRX                                                             
ERR3     MVC   LOGMESS(31),=C'ERROR LOGGING IS ALREADY ACTIVE'                  
         B     ERRX                                                             
ERRX     OI    SRVP1H+6,X'40'                                                   
         XC    SRVMSG,SRVMSG                                                    
         MVC   SRVMSG(11),=C'** ERROR **'                                       
         MVC   SRVMSG+12(40),LOGMESS                                            
         XC    SRVMSG2,SRVMSG2                                                  
         MVC   TSVCREQ,=X'01EE'    $BYE                                         
         B     EXIT                                                             
*                                                                               
OK1      XC    SRVMSG,SRVMSG                                                    
         MVC   SRVMSG(24),=C'ERROR LOGGING TERMINATED'                          
         XC    SRVMSG2,SRVMSG2                                                  
         B     EXIT                                                             
OK2      B     EXIT                                                             
*                                                                               
EXIT     XMOD1 1                                                                
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* FADSECTS                                                                      
       ++INCLUDE FADSECTS                                                       
*                                                                               
LOGWORK  DSECT                                                                  
LOGMESS  DS    CL40                                                             
* SRLOGFFD                                                                      
SRLOGFFD DSECT                                                                  
         DS    CL64                                                             
       ++INCLUDE SRLOGFFD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005SRLOG00   08/22/00'                                      
         END                                                                    
