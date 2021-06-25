*          DATA SET PEMSG08    AT LEVEL 013 AS OF 05/01/02                      
*PHASE TE1D08A                                                                  
         TITLE 'TE1D08 - LIST ADDRESS LISTS'                                    
TE1D08   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**TE1D08                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING TE1DFFD,RA                                                       
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         LA    R4,KEY                                                           
         USING MSGKEYD,R4                                                       
*                                                                               
         MVI   MSADFLG,0  INSURANCE                                             
         CLI   LSTONTWA,C'Y'                                                    
         BE    SELECT                                                           
*                                                                               
VECT     CLI   LADCMMD,C'T'                                                     
         BE    TOP                                                              
         CLI   LADCMMD,C'N'                                                     
         BE    NEXT                                                             
         CLI   LADCMMD,C'P'                                                     
*(TEMP)  BE    PREV                                                             
         CLI   LADCMMDH+5,0                                                     
         BE    CHECK                                                            
         MVC   SGNMSG(22),=C'COMMAND NOT RECOGNIZED'                            
         B     XITC                                                             
         EJECT                                                                  
*                               LIST RECORDS                                    
         SPACE 3                                                                
TOP      XC    KEY,KEY                                                          
         MVC   MSGAGY,TWAORIG                                                   
         MVI   MSGSYS,C'M'                                                      
         MVC   MSGNAM,SGNONUN                                                   
         MVI   ADLKTYP,ADLKTYPQ                                                 
         MVC   SGNMSG(11),=C'TOP OF LIST'                                       
LIST1    GOTO1 HIGH                                                             
LIST1A   LA    R2,LADSELH+11                                                    
         LA    R3,15                                                            
         LA    R5,LISTDIR                                                       
         XC    LISTDIR,LISTDIR                                                  
         CLC   KEY(13),KEYSAVE                                                  
         BE    LIST4                                                            
         MVC   SGNMSG(23),=C'NO ADDRESS LISTS SET UP'                           
         B     XITC                                                             
*                                                                               
LIST2    MVI   ADLKPAG,X'FF'                                                    
         GOTO1 HIGH                                                             
*                                                                               
LIST4    CLC   KEY(13),KEYSAVE                                                  
         BNE   XITC                                                             
         MVC   TWAKEYSV,KEY                                                     
         MVI   0(R5),C' '                                                       
         MVC   2(4,R5),MSGDA                                                    
         BAS   RE,LISTITEM                                                      
*                                                                               
         BAS   RE,BUMP                                                          
         BAS   RE,BUMP                                                          
         LA    R5,6(R5)                                                         
         BCT   R3,LIST2                                                         
         B     XITC                                                             
         EJECT                                                                  
*              DO NEXT PAGE OF LIST                                             
         SPACE 3                                                                
NEXT     MVC   KEY,TWAKEYSV                                                     
         MVI   ADLKPAG,X'FF'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   TOP                                                              
*                                                                               
         B     LIST1A                                                           
         EJECT                                                                  
*              INSERT INFO IN TWA                                               
         SPACE 3                                                                
LISTITEM NTR1                                                                   
*                                                                               
         GOTO1 GETREC                                                           
         MVC   LISTAR,SPACES                                                    
*                                                                               
         MVC   LISTAR(8),ADLKNAM      SHOW NAME                                 
*                                                                               
LIST4SD  MVI   ELCODE,X'40'           SHOW DESCRIPTION                          
         MVI   MAX,1                                                            
         MVI   OPTION,C'W'                                                      
         GOTO1 DISPCHAT                                                         
         MVC   LISTAR+15(30),WORK                                               
*                                                                               
         MVC   8(50,R2),LISTAR                                                  
         OI    6(R2),X'80'                                                      
         B     XIT                                                              
         EJECT                                                                  
*              CHECK FOR SELECTED ITEMS AND MARK                                
         SPACE 3                                                                
CHECK    LA    R5,LISTDIR                                                       
         LA    R3,15                                                            
         LA    R2,LADSELH                                                       
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
         LA    R2,15                                                            
SELECT1  CLI   0(R3),C'S'                                                       
         BE    SELECT2                                                          
         LA    R3,6(R3)                                                         
         BCT   R2,SELECT1                                                       
         MVI   LSTONTWA,0                                                       
         MVI   LADCMMD,C'N'                                                     
         B     VECT                                                             
SELECT2  MVI   0(R3),C' '                                                       
         MVC   SVMSGDA,2(R3)                                                    
         LA    R3,SGNIH                                                         
         MVI   OVERLAY,X'F8'                                                    
         GOTO1 LOADSOPH,DMCB,1                                                  
         MVI   SYSSTAT,6                                                        
         MVI   ADLCMMD,C'T'                                                     
         B     XIT                                                              
         EJECT                                                                  
BUMP     ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         BR    RE                                                               
         SPACE 1                                                                
XITC     MVI   LADCMMD,0                                                        
         OI    LADCMMDH+6,X'C0'                                                 
XIT      XIT1                                                                   
         SPACE 1                                                                
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 1                                                                
TRAPERR  GOTO1 ERREX                                                            
         B     XIT                                                              
         SPACE 1                                                                
         EJECT                                                                  
         SPACE 1                                                                
RELO     DS    A                                                                
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE PEMSGFILE                                                      
       ++INCLUDE PEMSGFFD                                                       
         PRINT ON                                                               
         ORG   SGNIH                                                            
       ++INCLUDE PEMSGF7D                                                       
         EJECT                                                                  
         ORG   SGNIH                                                            
       ++INCLUDE PEMSGF8D                                                       
         PRINT OFF                                                              
CONHEADH EQU   SGNMSGH                                                          
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE PEMSGWORKD                                                     
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013PEMSG08   05/01/02'                                      
         END                                                                    
