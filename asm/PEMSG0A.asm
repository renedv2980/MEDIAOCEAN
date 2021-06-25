*          DATA SET PEMSG0A    AT LEVEL 010 AS OF 08/22/00                      
*PHASE TE1D0AA                                                                  
         TITLE 'TE1D0A - SPECIAL ZAP FUNCTIONS'                                 
TE1D0A   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**TE1D0A                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING TE1DFFD,RA          BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         LA    R4,KEY                                                           
         USING MSGKEYD,R4                                                       
         XC    SGNMSG,SGNMSG                                                    
         EJECT                                                                  
*              BUILD FUNCTION HERE                                              
         SPACE 3                                                                
         XC    KEY,KEY                                                          
         MVI   MSGSYS,MSGSYSQ                                                   
         MVC   MSGAGY,=X'0050'                                                  
         MVC   MSGNAM,=C'JZIE    '                                              
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   MSGKTYPE,X'13'                                                   
         BNE   LOOPE                                                            
*                                                                               
LOOP     GOTO1 GETREC                                                           
         MVC   MSGAGY,=X'0417'                                                  
         MVC   MSGNAM,=C'ZIEGLER '                                              
         L     R6,AIO                                                           
         MVC   0(12,R6),KEY                                                     
         GOTO1 HIGH                                                             
         CLC   KEY(36),KEYSAVE                                                  
         MVC   KEY,KEYSAVE                                                      
         BE    LOOPN                                                            
         GOTO1 ADDREC                                                           
         MVC   KEY,0(R6)                                                        
*                                                                               
LOOPN    MVC   MSGAGY,=X'0050'                                                  
         MVC   MSGNAM,=C'JZIE    '                                              
         GOTO1 HIGH                                                             
LOOPE    GOTO1 SEQ                                                              
         CLC   KEY(12),KEYSAVE                                                  
         BNE   XIT                                                              
         CLI   MSGKTYPE,X'13'                                                   
         BE    LOOP                                                             
         B     LOOPE                                                            
         EJECT                                                                  
*              MISC                                                             
         SPACE 3                                                                
BUMP     ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         BR    RE                                                               
         SPACE 3                                                                
EX2      OI    6(R2),X'C0'                                                      
         OI    SGNMSGH+6,X'80'                                                  
XIT      XIT1                                                                   
         SPACE 1                                                                
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 1                                                                
TRAPERR  GOTO1 ERREX                                                            
         B     XIT                                                              
         SPACE 1                                                                
SPLAT    NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         EJECT                                                                  
         SPACE 1                                                                
RELO     DS    A                                                                
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE PEMSGFILE                                                      
       ++INCLUDE PEMSGFFD                                                       
         PRINT ON                                                               
         ORG   SGNIH                                                            
       ++INCLUDE PEMSGF5D                                                       
         PRINT OFF                                                              
CONHEADH EQU   SGNMSGH                                                          
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE PEMSGWORKD                                                     
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010PEMSG0A   08/22/00'                                      
         END                                                                    
