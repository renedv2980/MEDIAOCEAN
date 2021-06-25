*          DATA SET PEMSG03    AT LEVEL 057 AS OF 05/01/02                      
*PHASE TE1D03A                                                                  
         TITLE 'TE1D03 - USER RECORD ADD'                                       
TE1D03   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**TE1D03                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING TE1DFFD,RA      BASE SCREEN FOR SYSTEM + THIS PROG               
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         LA    R4,KEY                                                           
         USING MSGKEY,R4                                                        
         XC    KEY,KEY                                                          
         SPACE 1                                                                
         EJECT                                                                  
*              CHECK USER AUTHORITY                                             
         SPACE 3                                                                
CKUP     CLI   SGNONUP,C'N'                                                     
         BNE   CKUP2                                                            
         CLI   NEWCMMD,C'L'                                                     
         BNE   CKUPNA                                                           
         CLI   NEWCONNH+5,0                                                     
         BE    LIST                                                             
CKUPNA   MVC   SGNMSG(14),=C'NOT AUTHORIZED'                                    
         B     XIT                                                              
CKUP2    CLI   NEWCONNH+5,0                                                     
         BE    VCMMD                                                            
         CLI   SGNONUP,C'S'                                                     
         BNE   CKUPNA                                                           
*                                                                               
VCMMD    CLI   NEWCMMD,C'A'                                                     
         BE    ADD0                                                             
         CLI   NEWCMMD,C'D'                                                     
         BE    DEL0                                                             
         CLI   NEWCMMD,C'L'                                                     
         BE    LIST                                                             
         MVC   SGNMSG(22),=C'COMMAND NOT RECOGNIZED'                            
         B     EX1                                                              
         EJECT                                                                  
*              VALIDATE KEY                                                     
         SPACE 3                                                                
ADD0     CLI   NEWCONNH+5,0                                                     
         MVC   MSGAGY,TWAORIG                                                   
         BE    VKEY1                                                            
         MVC   WORK,SPACES                                                      
         ZIC   R1,NEWCONNH+5                                                    
         BCTR  R1,0                                                             
         EXMVC R1,WORK+1,NEWCONN                                                
         MVI   WORK,2                                                           
         GOTO1 GETSGN                                                           
         MVC   MSGAGY,WORK                                                      
         OC    WORK(2),WORK                                                     
         BNZ   VKEY1                                                            
         OI    NEWCONNH+6,X'C0'                                                 
         MVC   SGNMSG(18),=C'INVALID CONNECT ID'                                
         B     EX2                                                              
*                                                                               
VKEY1    MVI   MSGSYS,C'M'                                                      
         MVC   MSGNAM,=C'SYS     '                                              
         MVI   USRKTYP,USRKTYPQ                                                 
         LA    R2,NEWNAMEH              NAME                                    
         GOTO1 ANY1                                                             
         CLI   WORK,C' '                                                        
         BNE   VKEY2                                                            
         OI    NEWNAMEH+6,X'C0'                                                 
         MVC   SGNMSG(21),=C'NAME MUST BE SUPPLIED'                             
         B     EX2                                                              
VKEY2    MVC   USRNAME,WORK                                                     
         GOTO1 HIGH                                                             
         CLC   KEY(36),KEYSAVE                                                  
         BNE   VKEY3                                                            
         NI    KEY+36,X'7F'                                                     
         BNZ   VPKEY                                                            
         B     VKEYDUP                                                          
VKEY3    MVC   KEY,KEYSAVE                                                      
         MVI   USRPTYP,USRPTYPQ                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(36),KEYSAVE                                                  
         BNE   VPKEY                                                            
         NI    KEY+36,X'7F'                                                     
         BNZ   VPKEY                                                            
VKEYDUP  OI    NEWNAMEH+6,X'C0'                                                 
         MVC   SGNMSG(19),=C'NAME ALREADY IN USE'                               
         B     EX2                                                              
         EJECT                                                                  
*                  VALIDATE PASSIVE KEY                                         
         SPACE 3                                                                
VPKEY    MVC   KEY,KEYSAVE                                                      
         MVI   USRPTYP,USRPTYPQ                                                 
         LA    R2,NEWNICKH              NICK                                    
         GOTO1 ANY1                                                             
         CLI   WORK,C' '                                                        
         BNE   VPKEY1                                                           
         OI    NEWNICKH+6,X'C0'                                                 
         MVC   SGNMSG(25),=C'NICKNAME MUST BE SUPPLIED'                         
         B     EX2                                                              
VPKEY1   MVC   USRNAME,WORK                                                     
         GOTO1 HIGH                                                             
         CLC   KEY(36),KEYSAVE                                                  
         BNE   VPKEY2                                                           
         NI    KEY+36,X'7F'                                                     
         BNZ   VREC                                                             
         B     VPKEYDUP                                                         
VPKEY2   MVC   KEY,KEYSAVE                                                      
         MVI   USRKTYP,USRKTYPQ                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(36),KEYSAVE                                                  
         BNE   VREC                                                             
         NI    KEY+36,X'7F'                                                     
         BNZ   VREC                                                             
VPKEYDUP OI    NEWNICKH+6,X'C0'                                                 
         MVC   SGNMSG(23),=C'NICKNAME ALREADY IN USE'                           
         B     EX2                                                              
         EJECT                                                                  
*                  VALIDATE RECORD FIELDS                                       
         SPACE 3                                                                
VREC     MVI   MAX,1                                                            
         L     R1,AIO                                                           
         XCEF  (R1),512                                                         
         LA    R2,NEWNAMEH              NAME                                    
         MVI   ELCODE,X'20'                                                     
         GOTO1 VALICHAT                                                         
         LA    R2,NEWNICKH              NICK                                    
         MVI   ELCODE,X'21'                                                     
         GOTO1 VALICHAT                                                         
         LA    R2,NEWPSWDH              PASSWORD                                
         GOTO1 ANY1                                                             
         CLI   WORK,C' '                                                        
         BNE   VREC1                                                            
         OI    NEWPSWDH+6,X'C0'                                                 
         MVC   SGNMSG(25),=C'PASSWORD MUST BE SUPPLIED'                         
         B     EX2                                                              
VREC1    MVI   ELCODE,X'22'                                                     
         GOTO1 VALICHAT                                                         
         MVI   ELCODE,X'23'             OPTIONS                                 
         GOTO1 REMELEM                                                          
         MVI   ELEMENT,X'23'                                                    
         MVI   ELEMENT+1,14                                                     
         MVC   ELEMENT+4(09),=C'YFYNLNKN '                                      
         MVC   ELEMENT+13(1),NEWUP                                              
         GOTO1 ADDELEM                                                          
         CLI   NEWUP,C'N'                                                       
         BE    ADDRECD                                                          
         CLI   NEWUP,C'A'                                                       
         BE    ADDRECD                                                          
         CLI   NEWUP,C'S'                                                       
         BNE   VREC2                                                            
         CLI   SGNONUP,C'S'                                                     
         BE    ADDRECD                                                          
VREC2    OI    NEWUPH+6,X'C0'                                                   
         MVC   SGNMSG(24),=C'INVALID USER PERMISSIONS'                          
         B     EX2                                                              
         EJECT                                                                  
*              ADD NEW USER RECORDS                                             
         SPACE 3                                                                
ADDRECD  MVC   KEY,KEYSAVE                                                      
         MVI   USRKTYP,USRKTYPQ                                                 
         LA    R2,NEWNAMEH                                                      
         GOTO1 ANY1                                                             
         MVC   USRNAME,WORK                                                     
         L     R1,AIO                                                           
         MVC   0(36,R1),KEY                                                     
         OI    DMINBTS,X'08'    PASS DELETED                                    
         GOTO1 HIGH                                                             
         CLC   KEY(36),KEYSAVE                                                  
         BE    ADD2                                                             
         NI    DMINBTS,X'F7'                                                    
         MVC   KEY,KEYSAVE                                                      
         GOTO1 ADDREC                                                           
         MVC   MSGDA,KEY                                                        
         MVC   KEY(36),KEYSAVE                                                  
         B     ADDPASS                                                          
*                                                                               
ADD2     MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         L     R1,AIO1                                                          
         L     R2,AIO2                                                          
         ST    R1,AIO                                                           
         MVC   38(6,R1),38(R2)                                                  
         NI    38(R1),X'7F'                                                     
         GOTO1 PUTREC                                                           
         GOTO1 READ                                                             
         NI    KEY+36,X'7F'                                                     
         GOTO1 WRITE                                                            
*        B     ADDPASS                                                          
         EJECT                                                                  
*                           ADD PASSIVE POINTER                                 
         SPACE 3                                                                
ADDPASS  MVI   USRPTYP,USRPTYPQ                                                 
         LA    R2,NEWNICKH                                                      
         GOTO1 ANY1                                                             
         MVC   USRNICK,WORK                                                     
         OI    DMINBTS,X'08'    PASS DELETED                                    
         GOTO1 HIGH                                                             
         CLC   KEY(36),KEYSAVE                                                  
         BNE   ADDP1                                                            
         GOTO1 READ                                                             
         NI    KEY+36,X'7F'                                                     
         GOTO1 WRITE                                                            
         B     ADDP2                                                            
ADDP1    MVC   KEY,KEYSAVE                                                      
         GOTO1 ADD                                                              
ADDP2    MVC   SGNMSG(14),=C'NEW USER ADDED'                                    
         NI    DMINBTS,X'F7'                                                    
         B     XIT                                                              
         EJECT                                                                  
*              DELETE USER (AND ALL HIS RECORDS)                                
         SPACE 3                                                                
DEL0     CLI   NEWCONNH+5,0                                                     
         MVC   MSGAGY,TWAORIG                                                   
         BE    DEL1                                                             
         MVC   WORK,SPACES                                                      
         ZIC   R1,NEWCONNH+5                                                    
         BCTR  R1,0                                                             
         EXMVC R1,WORK+1,NEWCONN                                                
         MVI   WORK,2                                                           
         GOTO1 GETSGN                                                           
         MVC   MSGAGY,WORK                                                      
         OC    WORK(2),WORK                                                     
         BNZ   DEL1                                                             
         OI    NEWCONNH+6,X'C0'                                                 
         MVC   SGNMSG(18),=C'INVALID CONNECT ID'                                
         B     EX2                                                              
*                                                                               
DEL1     MVI   MSGSYS,C'M'                                                      
         MVC   MSGNAM,=C'SYS     '                                              
         MVI   USRKTYP,USRKTYPQ                                                 
         LA    R2,NEWNAMEH              NAME                                    
         GOTO1 ANY1                                                             
         CLI   WORK,C' '                                                        
         BNE   DEL2                                                             
         OI    NEWNAMEH+6,X'C0'                                                 
         MVC   SGNMSG(21),=C'NAME MUST BE SUPPLIED'                             
         B     EX2                                                              
DEL2     MVC   USRNAME,WORK                                                     
         GOTO1 HIGH                                                             
         CLC   KEY(36),KEYSAVE                                                  
         BE    DEL3                                                             
         MVC   KEY,KEYSAVE                                                      
         MVI   USRPTYP,USRPTYPQ                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(36),KEYSAVE                                                  
         BE    DEL3                                                             
         OI    NEWNAMEH+6,X'C0'                                                 
         MVC   SGNMSG(14),=C'USER NOT FOUND'                                    
         B     EX2                                                              
*                                                                               
DEL3     GOTO1 GETREC              ERASE MASTER RECORD                          
         L     R1,AIO                                                           
         OI    38(R1),X'80'                                                     
         GOTO1 PUTREC                                                           
         MVC   AIO,AIO2                                                         
*                                                                               
         L     R6,AIO1             ERASE PASSIVE KEY                            
         MVI   ELCODE,X'21'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'5'                                                         
         MVC   USRNAME,SPACES                                                   
         EXMVC R1,USRNAME,4(R6)                                                 
         MVI   USRPTYP,USRPTYPQ                                                 
         GOTO1 READ                                                             
         OI    KEY+36,X'80'                                                     
         GOTO1 WRITE                                                            
*                                                                               
         L     R6,AIO1             ERASE ACTIVE KEY                             
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'5'                                                         
         MVC   USRNAME,SPACES                                                   
         EXMVC R1,USRNAME,4(R6)                                                 
         MVI   USRKTYP,USRKTYPQ                                                 
         GOTO1 READ                                                             
         OI    KEY+36,X'80'                                                     
         GOTO1 WRITE                                                            
*                                                                               
         MVC   MSGNAM,USRNAME       ERASE REST                                  
         MVI   MSGKTYP,0                                                        
DELLOOP  GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE                                                  
         BNE   DELDONE                                                          
*                                                                               
         GOTO1 GETREC                                                           
         L     R1,AIO                                                           
         OI    38(R1),X'80'                                                     
         GOTO1 PUTREC                                                           
*                                                                               
         GOTO1 READ                                                             
         OI    KEY+36,X'80'                                                     
         GOTO1 WRITE                                                            
*                                                                               
         B     DELLOOP                                                          
*                                                                               
DELDONE  MVC   SGNMSG(12),=C'USER DELETED'                                      
         B     XIT                                                              
         EJECT                                                                  
*                             LIST ACTIVE USERS                                 
         SPACE 3                                                                
LIST     XC    KEY,KEY         SET UP FIRST KEY                                 
         MVI   MSGSYS,C'M'                                                      
         CLI   NEWCONNH+5,0      CONNECT ID                                     
         BE    LIST01                                                           
         CLI   NEWCONN,C'*'                                                     
         BNE   LIST00                                                           
         GOTO1 HIGH                                                             
         XC    KEY+4(32),KEY+4                                                  
         B     LIST02                                                           
LIST00   MVC   WORK,SPACES                                                      
         ZIC   R1,NEWCONNH+5                                                    
         BCTR  R1,0                                                             
         EXMVC R1,WORK+1,NEWCONN                                                
         MVI   WORK,2                                                           
         GOTO1 GETSGN                                                           
         MVC   MSGAGY,WORK                                                      
         OC    WORK(2),WORK                                                     
         BNZ   LIST02                                                           
         OI    NEWCONNH+6,X'C0'                                                 
         MVC   SGNMSG(18),=C'INVALID CONNECT ID'                                
         B     EX2                                                              
LIST01   MVC   MSGAGY,TWAORIG                                                   
LIST02   MVC   MSGNAM,=C'SYS     '                                              
         MVI   USRKTYP,USRKTYPQ                                                 
*                                SET UP FOR SPOOL                               
         MVC   REMUSER,=C'MSG'                                                  
         MVI   TWAWHEN,0                                                        
         XC    TWAOUT,TWAOUT                                                    
         XC    TWADEST,TWADEST                                                  
         MVI   WHEN,X'40'                                                       
*                                START PRINT INTERVAL                           
         GOTO1 STRTPRNT                                                         
         LA    R1,LSPECS                                                        
         ST    R1,SPECS                                                         
         LA    R1,LHOOK                                                         
         ST    R1,HEADHOOK                                                      
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
LIST1    GOTO1 HIGH                                                             
LIST4    CLI   USRKTYP,USRKTYPQ                                                 
         BNE   LISTNC      NEXT CONN ID                                         
*                                 GENERATE LISTING FOR ONE ID                   
         GOTO1 GETREC                                                           
*                                                                               
         MVI   WORK,1              ID                                           
         MVC   WORK+1(2),MSGAGY                                                 
         GOTO1 GETSGN                                                           
         MVC   P+5(8),WORK                                                      
*                                                                               
         MVC   P+20(8),USRNAME     NAME                                         
*                                                                               
         MVI   ELCODE,X'21'        NICKNAME                                     
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         ZIC   R1,1(R6)                                                         
         BCTR  R1,0                                                             
         EXMVC R1,P1+35,4(R6)                                                   
*                                                                               
         CLI   SGNONUP,C'N'                                                     
         BE    LISTP                                                            
         MVI   ELCODE,X'22'        PASSWORD                                     
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         ZIC   R1,1(R6)                                                         
         BCTR  R1,0                                                             
         EXMVC R1,P1+50,4(R6)                                                   
*                                                                               
LISTP    GOTO1 SPOOL,DMCB,(R8)                                                  
*                                 GET NEXT ID                                   
         ZIC   R1,USRNAME+7                                                     
         LA    R1,1(R1)                                                         
         STC   R1,USRNAME+7                                                     
         B     LIST1                                                            
*                                                                               
LISTNC   CLI   NEWCONN,C'*'                                                     
         BNE   LISTX                ALL DONE                                    
         CLI   MSGSYS,C'M'                                                      
         BNE   LISTX                                                            
         CLC   KEY(4),KEYSAVE                                                   
         BNE   LISTNC1                                                          
         MVI   MSGNAM,X'FF'                                                     
         GOTO1 HIGH                                                             
         CLI   MSGSYS,C'M'                                                      
         BNE   LISTX                                                            
LISTNC1  XC    KEY+4(32),KEY+4                                                  
         MVC   MSGNAM,=C'SYS     '                                              
         MVI   USRKTYP,USRKTYPQ                                                 
         B     LIST1                                                            
*                                                                               
LISTX    GOTO1 STOPPRNT                                                         
         B     XIT                                                              
*                                                                               
LHOOK    BR    RE                                                               
LSPECS   SSPEC H1,2,C'USER LIST'                                                
         SSPEC H2,5,C'ID'                                                       
         SSPEC H2,20,C'USER NAME'                                               
         SSPEC H2,35,C'NICKNAME'                                                
         SSPEC H2,50,C'PASSWORD'                                                
         DC    X'00'                                                            
         EJECT                                                                  
BUMP     ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         BR    RE                                                               
         SPACE 1                                                                
XIT      OI    NEWNAMEH+6,X'80'                                                 
         MVC   NEWNAME,SPACES                                                   
         OI    NEWNICKH+6,X'80'                                                 
         MVC   NEWNICK,SPACES                                                   
         OI    NEWPSWDH+6,X'80'                                                 
         MVC   NEWPSWD,SPACES                                                   
EX1      MVI   NEWCMMD,0                                                        
         OI    NEWCMMDH+6,X'C0'                                                 
EX2      XIT1                                                                   
         SPACE 1                                                                
         GETEL R6,DATADISP,ELCODE                                               
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
       ++INCLUDE PEMSGFCD                                                       
         PRINT OFF                                                              
CONHEADH EQU   SGNMSGH                                                          
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE PEMSGWORKD                                                     
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'057PEMSG03   05/01/02'                                      
         END                                                                    
