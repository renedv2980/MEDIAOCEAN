*          DATA SET PEMSG02    AT LEVEL 071 AS OF 05/01/02                      
*PHASE TE1D02A                                                                  
         TITLE 'TE1D02 - MSG RECORD LIST'                                       
TE1D02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**TE1D02                                                       
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
*                                                                               
         MVI   MSADFLG,0  INSURANCE                                             
         CLI   LSTONTWA,C'Y'                                                    
         BE    SELECT                                                           
*                                                                               
         CLI   MSGFILE,0          FIRST TIME                                    
         BNE   VECT                                                             
         MVI   MSGCMMD,C'T'                                                     
         OI    MSGFILEH+6,X'80'                                                 
         MVI   MSGFILEH+5,8                                                     
         MVC   MSGFILE,OPNFILE                                                  
*                                                                               
VECT     CLI   MSGCMMD,C'T'        LIST RECORDS FROM TOP                        
         BE    TOP                                                              
         CLI   MSGCMMD,C'N'        LIST NEXT PAGE OF RECORDS                    
         BE    NEXT                                                             
         CLI   MSGCMMD,C'R'        READ ALL RECORDS                             
         BE    MARK                                                             
         CLI   MSGCMMDH+5,0        CHECK FOR SELECTS                            
         BE    CHECK                                                            
         MVC   SGNMSG(22),=C'COMMAND NOT RECOGNIZED'                            
         OI    SGNMSGH+6,X'80'                                                  
         B     XITC                                                             
         EJECT                                                                  
*              LIST NEXT PAGE                                                   
         SPACE 3                                                                
NEXT     MVC   MSGDA,LASTLIST                                                   
         OC    MSGDA,MSGDA                                                      
         BZ    TOP                                                              
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVC   KEY,0(R6)                                                        
         MVC   MSGWTNG,SPACES                                                   
         OI    MSGWTNGH+6,X'80'                                                 
         CLC   OPNFILE,=C'INBOX   '                                             
         BE    NEXTB                                                            
*                                  CHECK INBOX                                  
         MVC   MSGKFILE,=C'INBOX   '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(21),KEYSAVE                                                  
         BNE   NEXTB                                                            
         MVC   MSGWTNG,=C'MAIL WAITING'                                         
NEXTB    MVC   KEY,0(R6)                                                        
         MVI   MSGKPAGE,X'FF'                                                   
         B     TOPB                                                             
         EJECT                                                                  
*              LIST RECORDS                                                     
         SPACE 3                                                                
TOP      XC    KEY,KEY                                                          
         MVC   MSGAGY,TWAORIG                                                   
         MVI   MSGSYS,C'M'                                                      
         MVC   MSGNAM,SGNONUN                                                   
         MVI   MSGKTYPE,MSGKTYPQ                                                
         OC    SGNMSG,SGNMSG                                                    
         BNZ   *+10                                                             
         MVC   SGNMSG(11),=C'TOP OF FILE'                                       
         MVC   MSGWTNG,SPACES                                                   
         OI    MSGWTNGH+6,X'80'                                                 
         CLC   OPNFILE,=C'INBOX   '                                             
         BE    TOPB                                                             
*                                  CHECK INBOX                                  
         MVC   MSGKFILE,=C'INBOX   '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(21),KEYSAVE                                                  
         BNE   TOPC                                                             
         MVC   MSGWTNG,=C'MAIL WAITING'                                         
TOPC     MVC   KEY,KEYSAVE                                                      
*                                                                               
TOPB     MVC   MSGKFILE,OPNFILE                                                 
         XC    LASTLIST,LASTLIST                                                
         LA    R2,MSGSELH                                                       
TOPA     OI    6(R2),X'80'                                                      
         MVC   8(3,R2),SPACES                                                   
         LA    R2,11(R2)                                                        
         OI    6(R2),X'80'                                                      
         MVC   8(74,R2),SPACES                                                  
         LA    R2,82(R2)                                                        
         CLI   0(R2),11                                                         
         BE    TOPA                                                             
LIST1    GOTO1 HIGH                                                             
         LA    R2,MSGSELH+11                                                    
         LA    R3,16                                                            
         LA    R5,LISTDIR                                                       
         XC    LISTDIR,LISTDIR                                                  
         CLC   KEY(21),KEYSAVE                                                  
         BE    LIST4                                                            
         CLI   MSGCMMD,C'N'                                                     
         MVI   MSGCMMD,0                                                        
         BE    TOP                                                              
         MVC   SGNMSG(19),=C'NO MESSAGES IN FILE'                               
         B     XITC                                                             
         SPACE 1                                                                
LIST2    MVI   MSGKPAGE,X'FF'                                                   
         GOTO1 HIGH                                                             
         SPACE 1                                                                
LIST4    CLC   KEY(21),KEYSAVE                                                  
         BNE   XITC                                                             
         MVI   0(R5),C' '                                                       
         MVC   2(4,R5),MSGDA                                                    
         MVC   LASTLIST,MSGDA                                                   
         BAS   RE,LISTITEM                                                      
         BAS   RE,BUMP                                                          
         BAS   RE,BUMP                                                          
         LA    R5,6(R5)                                                         
         BCT   R3,LIST2                                                         
         B     XITC                                                             
         EJECT                                                                  
*              INSERT RECORD DETAILS IN TWA                                     
         SPACE 3                                                                
LISTITEM NTR1                                                                   
         GOTO1 GETREC                                                           
         MVC   LISTAR,SPACES       SHOW CODE                                    
         LA    R3,LISTAR                                                        
         MVI   MAX,1                                                            
         MVI   OPTION,C'W'                                                      
*                                                                               
*                                  FROM                                         
         CLC   MSGKFROM,MSGNAM                                                  
         BNE   *+14                                                             
         CLC   MSGKFRAT,MSGAGY                                                  
         BE    OTFA1                                                            
         MVI   LISTAR+1,C'F'                                                    
         MVC   LISTAR+1+2(8),MSGKFROM                                           
*                                  AT                                           
         CLC   MSGKFRAT,TWAORIG                                                 
         BE    OTFA                                                             
         MVC   WORK+1(2),MSGKFRAT                                               
         MVI   WORK,1                                                           
         GOTO1 GETSGN                                                           
         MVC   LISTAR+1+11(8),WORK                                              
         B     OTFA                                                             
*                           CHECK IF SENT                                       
OTFA1    MVI   ELCODE,X'33'                                                     
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   OTFA                                                             
         MVI   LISTAR+1,C'T'                                                    
         MVC   LISTAR+1+2(8),4(R6)  FROM                                        
         MVC   LISTAR+1+11(8),12(R6) TO                                         
OTFA     LA    R3,LISTAR+21                                                     
*                                  ADJUST DATE AND TIME                         
         MVC   WORK(1),SGNONOO                                                  
         CLC   OPNFILE,=C'INBOX   '                                             
         BNE   *+10                                                             
         MVC   WORK(1),SGNONOI                                                  
         CLI   WORK,C'F'                                                        
         BE    *+10                                                             
         XC    MSGKDATE(4),=X'FFFFFFFF'                                         
*                                  DATE                                         
         GOTO1 DATCON,DMCB,(2,MSGKDATE),(5,WORK)                                
         MVC   0(8,R3),WORK                                                     
         LA    R3,9(R3)                                                         
*                                  TIME                                         
         SR    R0,R0                                                            
         ST    R0,WORK                                                          
         MVC   WORK+2(2),MSGKTIME                                               
         L     R1,WORK                                                          
         D     R0,=F'30'                                                        
         SR    R0,R0                                                            
         D     R0,=F'60'                                                        
         LA    R1,8(R1)                                                         
         CVD   R1,WORK       HOURS                                              
         UNPK  WORK(4),WORK+6(3)                                                
         MVC   0(2,R3),WORK                                                     
         MVI   2(R3),C':'                                                       
         CVD   R0,WORK       MINUTES                                            
         UNPK  WORK(4),WORK+6(3)                                                
         MVC   3(2,R3),WORK                                                     
         LA    R3,6(R3)                                                         
*                                  SUBJECT                                      
         MVI   ELCODE,X'30'                                                     
         GOTO1 DISPCHAT                                                         
         CLI   SGNONUC,C'Y'                                                     
         BNE   *+10                                                             
         OC    WORK,SPACES                                                      
         MVC   0(40,R3),WORK                                                    
*                                                                               
         MVC   8(74,R2),LISTAR                                                  
         OI    6(R2),X'80'                                                      
*                                  CLEAN UP DATE AND TIME                       
         MVC   WORK(1),SGNONOO                                                  
         CLC   OPNFILE,=C'INBOX   '                                             
         BNE   *+10                                                             
         MVC   WORK(1),SGNONOI                                                  
         CLI   WORK,C'F'                                                        
         BE    *+10                                                             
         XC    MSGKDATE(4),=X'FFFFFFFF'                                         
         B     XIT                                                              
         EJECT                                                                  
*              MARK ALL ITEMS ON SCREEN FOR SELECTION                           
         SPACE 3                                                                
MARK     LA    R5,LISTDIR                                                       
MARK1    CLI   0(R5),0                                                          
         BE    SELECT                                                           
         MVI   0(R5),C'S'                                                       
         LA    R5,6(R5)                                                         
         B     MARK1                                                            
         SPACE 6                                                                
*              CHECK FOR SELECTED ITEMS AND MARK                                
CHECK    LA    R5,LISTDIR                                                       
         LA    R3,16                                                            
         LA    R2,MSGSELH                                                       
CHECK1   CLI   5(R2),0                                                          
         BE    *+8                                                              
         MVI   0(R5),C'S'                                                       
         LA    R5,6(R5)                                                         
         BAS   RE,BUMP                                                          
         BAS   RE,BUMP                                                          
         BCT   R3,CHECK1                                                        
         SPACE 6                                                                
*              PROCESS SELECTS                                                  
         SPACE 3                                                                
SELECT   LA    R3,LISTDIR                                                       
         MVI   LSTONTWA,C'Y'                                                    
         LA    R2,16                                                            
SELECT1  CLI   0(R3),C'S'                                                       
         BE    SELECT2                                                          
         LA    R3,6(R3)                                                         
         BCT   R2,SELECT1                                                       
         MVI   LSTONTWA,0                                                       
         MVI   MSGCMMD,C'N'                                                     
         B     VECT                                                             
SELECT2  MVI   0(R3),C' '                                                       
         MVC   SVMSGDA,2(R3)                                                    
         LA    R3,SGNIH                                                         
         MVI   OVERLAY,X'FA'                                                    
         GOTO1 LOADSOPH,DMCB,1                                                  
         MVI   SYSSTAT,3                                                        
         MVI   PAGCMMD,C'D'                                                     
         B     XIT                                                              
         EJECT                                                                  
*              MISC                                                             
         SPACE 3                                                                
BUMP     ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         BR    RE                                                               
         SPACE 3                                                                
XITC     MVI   MSGCMMD,C' '                                                     
         OI    MSGCMMDH+6,X'C1'                                                 
         MVI   MSGCMMDH+5,1                                                     
         XC    MSGNAME,MSGNAME                                                  
         OI    MSGNAMEH+6,X'80'                                                 
         SPACE 1                                                                
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
       ++INCLUDE PEMSGFDD                                                       
         ORG   SGNIH                                                            
       ++INCLUDE PEMSGFAD                                                       
         PRINT OFF                                                              
CONHEADH EQU   SGNMSGH                                                          
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE PEMSGWORKD                                                     
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'071PEMSG02   05/01/02'                                      
         END                                                                    
