*          DATA SET PEMSG09    AT LEVEL 039 AS OF 05/01/02                      
*PHASE TE1D09A                                                                  
         TITLE 'TE1D09 - USER PROFILE DISPLAY/CHANGE'                           
TE1D09   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**TE1D09                                                       
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
*              GET USER RECORD                                                  
         SPACE 3                                                                
         XC    KEY,KEY                                                          
         MVI   MSGSYS,MSGSYSQ                                                   
         MVC   MSGAGY,TWAORIG                                                   
         MVC   MSGNAM,=C'SYS     '                                              
         MVI   USRKTYP,USRKTYPQ                                                 
         MVC   USRNAME,SGNONUN                                                  
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
         SPACE 3                                                                
*              CHECK FOR UPDATE                                                 
         SPACE 3                                                                
         CLI   PROCMMDH+5,0                                                     
         BNE   *+12                                                             
         CLI   PROCMMD,C'D'                                                     
         BE    DREC                                                             
         CLI   PROCMMD,C'K'                                                     
         BE    UREC                                                             
         MVC   SGNMSG(15),=C'INVALID COMMAND'                                   
         OI    PROCMMDH+6,X'C0'                                                 
         B     XIT                                                              
         EJECT                                                                  
*                  VALIDATE RECORD FIELDS                                       
         SPACE 3                                                                
UREC     MVC   SGNMSG(15),=C'PROFILE UPDATED'                                   
         MVI   MAX,1                                                            
         LA    R2,PROPSWDH              PASSWORD                                
         CLI   5(R2),0                                                          
         BE    EX2                                                              
         MVI   ELCODE,X'22'                                                     
         GOTO1 VALICHAT                                                         
*                                                                               
         MVI   MAX,2                                                            
         LA    R2,PROTWXNH              TWX NUMBER OR GRAPH=                    
         MVI   ELCODE,X'25'                AND ANSWERBACK                       
         GOTO1 REMELEM                                                          
         CLI   5(R2),0                                                          
         BE    UREC2                                                            
         GOTO1 VALICHAT                                                         
*                                                                               
UREC2    LA    R2,PROFMTLH              FORMAT FILE LOCATION                    
         MVI   MAX,1                                                            
         CLI   5(R2),0                                                          
         BNE   UREC2A                                                           
         MVC   SGNMSG(28),=C'INVALID FORMAT FILE LOCATION'                      
         B     EX2                                                              
UREC2A   MVC   WORK,SPACES                                                      
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EXMVC R1,WORK+1,8(R2)                                                  
         MVI   WORK,2                                                           
         GOTO1 GETSGN                                                           
         CLC   WORK(2),=H'0'                                                    
         BNE   V01                                                              
         MVC   SGNMSG(18),=C'INVALID SIGN ON ID'                                
         B     EX2                                                              
V01      GOTO1 ANY                                                              
         MVI   ELCODE,X'24'                                                     
         GOTO1 VALICHAT                                                         
*                                                                               
         MVI   ELCODE,X'23'             OPTIONS                                 
         GOTO1 REMELEM                                                          
         MVI   ELEMENT,X'23'                                                    
         MVI   ELEMENT+1,14                                                     
         LA    R2,PRODADH                                                       
         LA    R3,ELEMENT+4                                                     
         BAS   RE,VEFYN                                                         
         LA    R2,PROOIH                                                        
         BAS   RE,VEFFL                                                         
         LA    R2,PROAOH                                                        
         BAS   RE,VEFYN                                                         
         LA    R2,PROFOH                                                        
         BAS   RE,VEFYN                                                         
         LA    R2,PROOOH                                                        
         BAS   RE,VEFFL                                                         
         LA    R2,PROMSH                                                        
         BAS   RE,VEFYN                                                         
         CLI   PROMS,C'Y'                                                       
         BNE   V02                                                              
         CLC   PROMS,PROAO                                                      
         BNE   V02                                                              
         MVC   SGNMSG(27),=C'CANNOT ALSO AUTO OPEN INBOX'                       
         B     EX2                                                              
V02      MVC   0(1,R3),PROMC                                                    
         LA    R3,1(R3)                                                         
         LA    R2,PROUCH                                                        
         BAS   RE,VEFYN                                                         
         LA    R2,PROAPH                                                        
         BAS   RE,VEFYN                                                         
         LA    R2,PROUPH                                                        
         CLI   PROUP,C'N'                                                       
         BE    VOK                                                              
         CLI   SGNONUP,C'N'                                                     
         BE    VER                                                              
         CLI   PROUP,C'A'                                                       
         BE    VOK                                                              
         CLI   SGNONUP,C'S'                                                     
         BNE   VER                                                              
         CLI   PROUP,C'S'                                                       
         BNE   VER                                                              
VOK      MVC   ELEMENT+13(1),PROUP                                              
         GOTO1 ADDELEM                                                          
         GOTO1 PUTREC                                                           
         B     DREC                                                             
VER      MVC   SGNMSG(15),=C'INVALID CODE   '                                   
         B     EX2                                                              
         SPACE 3                                                                
VEFFL    CLI   8(R2),C'F'                                                       
         BE    *+12                                                             
         CLI   8(R2),C'L'                                                       
         BNE   VEFFLX                                                           
         MVC   0(1,R3),8(R2)                                                    
         LA    R3,1(R3)                                                         
         BR    RE                                                               
VEFFLX   MVC   SGNMSG(15),=C'MUST BE F OR L '                                   
         B     EX2                                                              
         SPACE 3                                                                
VEFYN    CLI   8(R2),C'Y'                                                       
         BE    *+12                                                             
         CLI   8(R2),C'N'                                                       
         BNE   VEFYNX                                                           
         MVC   0(1,R3),8(R2)                                                    
         LA    R3,1(R3)                                                         
         BR    RE                                                               
VEFYNX   MVC   SGNMSG(15),=C'MUST BE Y OR N '                                   
         B     EX2                                                              
         EJECT                                                                  
*              DISPLAY INFO                                                     
         SPACE 3                                                                
DREC     L     R6,AIO                DISPLAY PASSWORD                           
         MVI   ELCODE,X'22'                                                     
         BAS   RE,GETEL                                                         
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'5'                                                         
         MVC   WORK,SPACES                                                      
         EXMVC R1,WORK,4(R6)                                                    
         MVC   PROPSWD,WORK                                                     
         OI    PROPSWDH+6,X'80'                                                 
*                                                                               
         MVI   ELCODE,X'25'         DISPLAY TWX DATA:                           
         LA    R2,PROTWXNH              TWX NUMBER OR GRAPH=                    
         MVI   MAX,2                    AND ANSWERBACK                          
         GOTO1 DISPCHAT                                                         
         MVI   MAX,1                                                            
*                                                                               
         L     R6,AIO                DISPLAY FORMAT FILE LOCATION               
         MVI   ELCODE,X'24'                                                     
         BAS   RE,GETEL                                                         
         BNE   DREC1                                                            
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'5'                                                         
         MVC   WORK,SPACES                                                      
         EXMVC R1,WORK,4(R6)                                                    
         MVC   PROFMTL,WORK                                                     
         OI    PROFMTLH+6,X'80'                                                 
*                                                                               
DREC1    L     R6,AIO                DISPLAY OPTIONS                            
         MVI   ELCODE,X'23'                                                     
         BAS   RE,GETEL                                                         
         MVC   SGNONOP,4(R6)                                                    
*                                                                               
         MVC   PRODAD,SGNONDA                                                   
         OI    PRODADH+6,X'80'                                                  
         MVC   PROOI,SGNONOI                                                    
         OI    PROOIH+6,X'80'                                                   
         MVC   PROAO,SGNONAO                                                    
         OI    PROAOH+6,X'80'                                                   
         MVC   PROFO,SGNONFO                                                    
         OI    PROFOH+6,X'80'                                                   
         MVC   PROOO,SGNONOO                                                    
         OI    PROOOH+6,X'80'                                                   
         MVC   PROMS,SGNONMS                                                    
         OI    PROMSH+6,X'80'                                                   
         MVC   PROMC,SGNONMC                                                    
         OI    PROMCH+6,X'80'                                                   
         MVC   PROUC,SGNONUC                                                    
         OI    PROUCH+6,X'80'                                                   
         MVC   PROAP,SGNONAP                                                    
         OI    PROAPH+6,X'80'                                                   
         MVC   PROUP,SGNONUP                                                    
         OI    PROUPH+6,X'80'                                                   
*                                                                               
         MVI   PROCMMD,C' '                                                     
         OI    PROCMMDH+6,X'C0'                                                 
         B     XIT                                                              
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
       ++INCLUDE PEMSGF6D                                                       
         PRINT OFF                                                              
CONHEADH EQU   SGNMSGH                                                          
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE PEMSGWORKD                                                     
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'039PEMSG09   05/01/02'                                      
         END                                                                    
