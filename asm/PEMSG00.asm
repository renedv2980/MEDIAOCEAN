*          DATA SET PEMSG00    AT LEVEL 184 AS OF 05/01/02                      
*PHASE TE1D00A                                                                  
*INCLUDE KHDUMMY                                                                
         TITLE 'TE1D00 - MSG CONTROLLER'                                        
TE1D00   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 1500,**TE1D00,RR=R2                                              
         LA    R7,2048(RB)                                                      
         LA    R7,2048(R7)                                                      
         USING TE1D00+4096,R7                                                   
         ST    R2,RELO                                                          
         LR    R9,R1                                                            
         LR    R8,RC                                                            
         USING SPOOLD,R8                                                        
         LR    RE,R8                                                            
         LA    RF,1500             CLEAR SELECTED STORAGE                       
         SLL   RF,3                                                             
         XCEF                                                                   
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(131),SPACES                                             
         LA    RC,SPOOLEND                                                      
         USING GEND,RC                                                          
         ST    R9,SYSPARMS                                                      
         LA    R9,IO                                                            
         AH    R9,=H'3072'         GRABBING 3 1024 BYTE I/O AREAS               
         LA    R9,24(R9)           NEED SPACE FOR 3 8BYTE LABELS                
         USING SYSD,R9                                                          
         BAS   RE,SYSINIT          INITIALIZE SYSTEM DEPENDENT                  
         ST    RB,SYSRB            SYSTEM BASE REGS                             
         ST    RD,SYSRD                                                         
         L     R2,SYSPARMS                                                      
         L     R2,8(R2)            A(SYSLIST)                                   
         L     RF,4(R2)            A(CALLOV)                                    
         XC    DMCB(12),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A30'                                           
         LA    R1,DMCB                                                          
         BASR  RE,RF               GO TO CALLOV                                 
         L     RF,DMCB             PICK UP A(GENERAL CONTROLLER)                
         GOTO1 (RF),DMCB,(R8)      GO THERE - PASS A(WORKING STORAGE)           
         L     R3,SYSDUMMY                                                      
         ST    R3,AOVERLAY                                                      
         L     RA,ATWA                                                          
         USING TE1DFFD,RA                                                       
         XC    SGNMSG,SGNMSG                                                    
         OI    SGNMSGH+6,X'80'                                                  
*                                                                               
*              SWITCH TO PER SYSTEM                                             
*                                                                               
         XC    DMCB(20),DMCB                                                    
         GOTO1 SWITCH,DMCB,=C'PER'                                              
         CLI   DMCB+4,0                                                         
         BE    STRT                                                             
         CLI   DMCB+4,1                                                         
         BNE   TE1DM2                                                           
         MVC   SGNMSG(38),=C'USER NOT AUTHORIZED FOR MESSAGE SYSTEM'            
         B     XIT                                                              
TE1DM2   CLI   DMCB+4,2                                                         
         BNE   TE1DM3                                                           
         MVC   SGNMSG(23),=C'MESSAGE SYSTEM NOT OPEN'                           
         B     XIT                                                              
TE1DM3   DC    H'0'                                                             
         EJECT                                                                  
*              USE PHASE DEPENDING ON SYSSTAT                                   
         SPACE 3                                                                
STRT     CLI   OFFLINE,C'Y'                                                     
         BNE   LDPHASE                                                          
         XI    TWALREC,X'80'                                                    
         BZ    XIT                                                              
         MVI   SYSSTAT,8                                                        
LDPHASE  ZIC   R1,SYSSTAT                                                       
         SLL   R1,2                                                             
         B     PHASLIST(R1)                                                     
         SPACE 3                                                                
PHASLIST B     SGNON        0      SIGNON SCREEN                                
         B     MENU         1      MENU SCREEN                                  
         B     MSGLST       2      LIST MESSAGES                                
         B     MSGPROC      3      PROCESS MESSAGES                             
         B     FILELST      4      LIST FILES                                   
         B     ADDLIST      5      LIST ADDRESS LISTS                           
         B     ADLPROC      6      PROCESS ADDRESS LISTS                        
         B     PROFILE      7      DISPLAY AND UPDATE                           
         B     WRITREP      8      WRITE REPORTS                                
         B     NEWUSER      9      ADD NEW USERS                                
         B     SPECIAL     10      SPECIAL ZAP FUNCTIONS                        
         EJECT                                                                  
*              SIGN ON SCREEN                                                   
         SPACE 3                                                                
SGNON    DS    0H                                                               
         CLC   SGNID(3),=C'JCZ'         CHECK SPECIAL ID                        
         BNE   SIGNONA                                                          
         CLC   SGNPSWD,=C'31415927'                                             
         BNE   SIGNONA                                                          
         MVC   SGNONUN,=C'JCZ     '                                             
         MVC   SGNONOP,=C'NFNNLY    '                                           
         MVC   SGNONUP,SGNID+3                                                  
         B     SGNONG                                                           
         SPACE 3                                                                
*                    READ AND CHECK USER RECORD                                 
         SPACE 3                                                                
SIGNONA  ZIC   R1,SGNIDH+5              GET SIGNON INFO                         
         BCTR  R1,0                                                             
         MVC   SGNONUN,=C'        '                                             
         EXMVC R1,SGNONUN,SGNID                                                 
         ZIC   R1,SGNPSWDH+5                                                    
         BCTR  R1,0                                                             
         MVC   SGNONPW,=C'        '                                             
         EXMVC R1,SGNONPW,SGNPSWD                                               
         SPACE 1                                                                
         XC    KEY,KEY                  BUILD KEY                               
         LA    R2,KEY                                                           
         USING MSGKEYD,R2                                                       
         MVI   MSGSYS,MSGSYSQ                                                   
         MVC   MSGAGY,TWAORIG                                                   
         MVC   MSGNAM,=C'SYS     '                                              
         MVI   USRKTYP,USRKTYPQ                                                 
         MVC   USRNAME,SGNONUN                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(36),KEYSAVE        FOUND?                                    
         BE    SIGNONR                                                          
         SPACE 1                                                                
         MVC   KEY,KEYSAVE                                                      
         MVI   USRPTYP,USRPTYPQ                                                 
         DROP  R2                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(36),KEYSAVE       FOUND?                                     
         BNE   SIGNONB                                                          
         SPACE 1                                                                
SIGNONR  GOTO1 GETREC                READ RECORD                                
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'   SHOULD BE THERE                                           
         SPACE 1                                                                
         LA    R6,IO                GET TRUE NAME                               
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'   SHOULD BE THERE                                           
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'5'                                                         
         MVC   WORK,=C'        '                                                
         EXMVC R1,WORK,4(R6)                                                    
         MVC   SGNONUN,WORK                                                     
         SPACE 1                                                                
         LA    R6,IO                CHECK PASSWORD                              
         MVI   ELCODE,X'22'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'   SHOULD BE THERE                                           
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'5'                                                         
         MVC   WORK,=C'        '                                                
         EXMVC R1,WORK,4(R6)                                                    
         CLC   SGNONPW,WORK                                                     
         BNE   SIGNONB                                                          
         SPACE 1                                                                
         LA    R6,IO                GET OPTIONS                                 
         MVI   ELCODE,X'23'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'   SHOULD BE THERE                                           
         MVC   SGNONOP,4(R6)                                                    
         SPACE 1                                                                
         CLI   SGNONAO,C'Y'        CHECK AUTO OPEN                              
         BE    MENUI                                                            
         CLI   SGNONMS,C'Y'        CHECK AUTO SEND                              
         BNE   SGNONG                                                           
*                                  AUTO SEND MESSAGE                            
         MVC   OPNFILE,=C'MESSAGES'                                             
         LA    R3,SGNIH            GET MESSAGE SCREEN                           
         MVI   OVERLAY,X'FA'                                                    
         GOTO1 LOADSOPH,DMCB,1                                                  
         MVI   MSGCMMD,C'A'        SET UP ADD                                   
         MVI   SYSSTAT,3                                                        
         B     LDPHASE                                                          
*                                                                               
SGNONG   LA    R3,SGNIH            GET MENU SCREEN                              
         MVI   OVERLAY,X'FE'                                                    
         GOTO1 LOADSOPH,DMCB,1                                                  
         MVI   SYSSTAT,1                                                        
         B     XITS                                                             
         SPACE 1                                                                
SIGNONB  MVC   SGNMSG(34),=C'INVALID SIGN ON...PLEASE TRY AGAIN'                
         OI    SGNMSGH+6,X'80'                                                  
         OI    SGNIDH+6,X'40'                                                   
         B     XITS                                                             
         EJECT                                                                  
*              PROCESS MENU CHOICE                                              
         SPACE 3                                                                
MENU     CLI   MENCMMD,C'I'                                                     
         BE    MENUI                                                            
         CLI   MENCMMD,C'O'                                                     
         BE    MENUO                                                            
         CLI   MENCMMD,C'F'                                                     
         BE    MENUF                                                            
         CLI   MENCMMD,C'A'                                                     
         BE    MENUA                                                            
         CLI   MENCMMD,C'P'                                                     
         BE    MENUP                                                            
         CLI   MENCMMD,C'U'                                                     
         BE    MENUU                                                            
         CLI   MENCMMD,C'S'                                                     
         BE    MENUS                                                            
MENUNR   MVC   SGNMSG(21),=C'CHOICE NOT RECOGNIZED'                             
         OI    SGNMSGH+6,X'80'                                                  
         OI    MENCMMDH+6,X'C0'                                                 
         MVI   MENCMMD,0                                                        
         B     XITS                                                             
         SPACE 1                                                                
MENUS    CLI   SGNONUP,C'S'                                                     
         BNE   MENUNR                                                           
         LA    R3,SGNIH            GET SPECIAL SCREEN                           
         MVI   OVERLAY,X'F5'                                                    
         GOTO1 LOADSOPH,DMCB,1                                                  
         MVI   SYSSTAT,10                                                       
         XC    SGNMSG,SGNMSG                                                    
         B     LDPHASE                                                          
         SPACE 1                                                                
MENUI    MVC   OPNFILE,=C'INBOX   '                                             
         B     MENUIO                                                           
         SPACE 1                                                                
MENUO    MVC   WORK,SPACES                                                      
         LA    R2,MENNAMEH                                                      
         GOTO1 ANY                                                              
         CLI   WORK,C' '                                                        
         MVC   OPNFILE,WORK                                                     
         BNE   MENUIO                                                           
         XC    SGNMSG,SGNMSG                                                    
         MVC   SGNMSG(18),=C'FILE NAME REQUIRED'                                
         OI    SGNMSGH+6,X'80'                                                  
         OI    6(R2),X'40'                                                      
         B     XITS                                                             
         SPACE 1                                                                
MENUIO   LA    R3,SGNIH            GET MESSAGE LIST SCREEN                      
         MVI   OVERLAY,X'FD'                                                    
         GOTO1 LOADSOPH,DMCB,1                                                  
         MVI   SYSSTAT,2                                                        
         XC    SGNMSG,SGNMSG                                                    
         B     LDPHASE                                                          
         SPACE 1                                                                
MENUF    LA    R3,SGNIH            GET FILE LIST SCREEN                         
         MVI   OVERLAY,X'F9'                                                    
         GOTO1 LOADSOPH,DMCB,1                                                  
         MVI   SYSSTAT,4                                                        
         XC    SGNMSG,SGNMSG                                                    
         B     LDPHASE                                                          
         SPACE 1                                                                
MENUA    LA    R3,SGNIH            GET LIST ADDRESS LISTS SCREEN                
         MVI   OVERLAY,X'F7'                                                    
         GOTO1 LOADSOPH,DMCB,1                                                  
         MVI   SYSSTAT,5                                                        
         XC    SGNMSG,SGNMSG                                                    
         MVI   LADCMMD,C'T'                                                     
         B     LDPHASE                                                          
         SPACE 1                                                                
MENUP    LA    R3,SGNIH            GET PROFILE DISPLAY SCREEN                   
         MVI   OVERLAY,X'F6'                                                    
         GOTO1 LOADSOPH,DMCB,1                                                  
         MVI   SYSSTAT,7                                                        
         XC    SGNMSG,SGNMSG                                                    
         MVI   PROCMMD,C'D'                                                     
         B     LDPHASE                                                          
         SPACE 1                                                                
MENUU    LA    R3,SGNIH            GET LIST/ADD/DELETE USER SCREEN              
         MVI   OVERLAY,X'FC'                                                    
         GOTO1 LOADSOPH,DMCB,1                                                  
         MVI   SYSSTAT,9                                                        
         XC    SGNMSG,SGNMSG                                                    
         B     XITS                                                             
         SPACE 1                                                                
         EJECT                                                                  
*              PROCESS MESSAGE LIST REQUEST                                     
         SPACE 3                                                                
MSGLST   CLI   MSGCMMD,C'M'                                                     
         BE    SGNONG                                                           
         CLI   MSGCMMD,C'O'                                                     
         BE    MENUO                                                            
         CLI   MSGCMMD,C'I'                                                     
         BE    MENUI                                                            
         CLI   MSGCMMD,C'A'                                                     
         BE    MSGLA                                                            
         CLI   MSGCMMD,C'G'                                                     
         BE    MSGLG                                                            
         CLI   MSGCMMD,C'Q'                                                     
         BE    MSGLQ                                                            
*                                                                               
         MVI   OVERLAY,2                                                        
         L     R3,AOVERLAY                                                      
         GOTO1 LOADSOPH,DMCB,0                                                  
         LR    RF,R3                                                            
         GOTO1 (RF),DMCB,(RC)                                                   
         CLI   SYSSTAT,2                                                        
         BNE   LDPHASE                                                          
         B     XITS                                                             
*                                                                               
MSGLQ    LA    R3,SGNIH                                                         
         MVI   OVERLAY,X'FB'                                                    
         GOTO1 LOADSOPH,DMCB,1                                                  
         MVI   SYSSTAT,8                                                        
         B     LDPHASE                                                          
*                                                                               
MSGLG    CLC   OPNFILE,=C'OUTBOX  '   CAN'T GET IN OUTBOX                       
         BNE   MSGLG1                                                           
         MVC   SGNMSG(19),=C'CAN''T GET IN OUTBOX'                              
         MVI   PAGCMMD,0                                                        
         OI    PAGCMMDH+6,X'C0'                                                 
         B     XIT                                                              
*                                                                               
MSGLG1   CLC   OPNFILE,=C'INBOX   '   CAN'T GET IN INBOX                        
         BNE   MSGLG2                                                           
         MVC   SGNMSG(18),=C'CAN''T GET IN INBOX'                               
         MVI   PAGCMMD,0                                                        
         OI    PAGCMMDH+6,X'C0'                                                 
         B     XIT                                                              
*                                                                               
MSGLG2   LA    R2,MSGNAMEH                                                      
         CLI   5(R2),0                                                          
         BNE   MSGLG2A                                                          
         MVC   SGNMSG(12),=C'FORMAT NAME?'                                      
         OI    6(R2),X'40'                                                      
         B     XIT                                                              
MSGLG2A  GOTO1 ANY                                                              
         MVC   WORK+8(1),5(R2)                                                  
*                                                                               
MSGLG3   LA    R3,SGNIH                                                         
         MVI   OVERLAY,X'FA'                                                    
         GOTO1 LOADSOPH,DMCB,1                                                  
         MVI   MSGCMMD,C'G'                                                     
         MVC   PAGNAM1,WORK                                                     
         MVC   PAGNAM1H+5(1),WORK+8                                             
         MVI   SYSSTAT,3                                                        
         B     LDPHASE                                                          
*                                                                               
MSGLA    CLC   OPNFILE,=C'OUTBOX  '   CAN'T ADD IN OUTBOX                       
         BNE   MSGLA1                                                           
         CLI   SGNONFO,C'Y'           IF AUTOFILING                             
         BNE   MSGLA1                                                           
         MVC   SGNMSG(19),=C'CAN''T ADD IN OUTBOX'                              
         MVI   PAGCMMD,0                                                        
         OI    PAGCMMDH+6,X'C0'                                                 
         B     XIT                                                              
*                                                                               
MSGLA1   LA    R3,SGNIH                                                         
         MVI   OVERLAY,X'FA'                                                    
         GOTO1 LOADSOPH,DMCB,1                                                  
         MVI   MSGCMMD,C'A'                                                     
         MVI   SYSSTAT,3                                                        
         B     LDPHASE                                                          
         EJECT                                                                  
*              PROCESS MESSAGES                                                 
         SPACE 3                                                                
MSGPROC  CLI   PAGCMMDH+5,0                                                     
         BE    MSGPLD                                                           
         CLI   PAGCMMD,C'M'                                                     
         BE    MSGPM                                                            
*                                                                               
MSGPLD   MVI   OVERLAY,5                                                        
         L     R3,AOVERLAY                                                      
         GOTO1 LOADSOPH,DMCB,0                                                  
         LR    RF,R3                                                            
         GOTO1 (RF),DMCB,(RC)                                                   
         CLI   MSADFLG,0                                                        
         BNE   MSGADL                                                           
         CLI   PAGCMMD,C'K'                                                     
         BE    MSGPK                                                            
         CLI   PAGCMMD,C'N'                                                     
         BE    MSGPK2                                                           
         B     XITS                                                             
*                                                                               
MSGADL   B     ADDA                                                             
*                                                                               
MSGPM    MVI   LSTONTWA,0                                                       
         B     SGNONG                                                           
*                                                                               
MSGPK    CLC   OPNFILE,=C'INBOX   '   CAN'T KEEP IN INBOX                       
         BNE   MSGPK2                                                           
         MVC   SGNMSG(19),=C'CAN''T KEEP IN INBOX'                              
         MVI   PAGCMMD,0                                                        
         OI    PAGCMMDH+6,X'C0'                                                 
         B     XIT                                                              
*                                                                               
MSGPK2   LA    R3,SGNIH            KEEP-GO FOR NEXT                             
         MVI   OVERLAY,X'FD'                                                    
         GOTO1 LOADSOPH,DMCB,1                                                  
         MVI   MSGCMMD,C'N'                                                     
         MVC   MSGFILE,OPNFILE                                                  
         OI    MSGFILEH+6,X'80'                                                 
         MVI   SYSSTAT,2                                                        
         B     LDPHASE                                                          
         EJECT                                                                  
*              WRITE REPORTS                                                    
         SPACE 3                                                                
WRITREP  CLI   REPCMMD,C'M'                                                     
         BE    SGNONG                                                           
         CLI   REPCMMD,C'C'                                                     
         BE    MENUIO                                                           
         CLI   REPCMMD,C' '                                                     
         BE    XITS                                                             
         CLI   REPCMMD,0                                                        
         BE    XITS                                                             
         MVI   OVERLAY,4                                                        
         L     R3,AOVERLAY                                                      
         GOTO1 LOADSOPH,DMCB,0                                                  
         LR    RF,R3                                                            
         GOTO1 (RF),DMCB,(RC)                                                   
         CLI   OFFLINE,C'Y'                                                     
         BNE   XITS                                                             
         L     R1,SYSPARMS                                                      
         MVI   0(R1),0                                                          
         B     XIT                                                              
         EJECT                                                                  
*              LIST FILES                                                       
         SPACE 3                                                                
FILELST  CLI   PF9CMMD,C'M'                                                     
         BE    SGNONG                                                           
         CLI   PF9CMMD,C'O'                                                     
         BE    MENUO                                                            
         CLI   PF9CMMD,C'I'                                                     
         BE    MENUI                                                            
*                                                                               
         OI    PF9CMMDH+6,X'C0'                                                 
         MVI   PF9CMMD,0                                                        
         LA    R4,KEY                                                           
         USING MSGKEYD,R4                                                       
         XC    KEY,KEY          BUILD KEY                                       
         MVC   MSGAGY,TWAORIG                                                   
         MVI   MSGSYS,C'M'                                                      
         MVC   MSGNAM,SGNONUN                                                   
         MVI   MSGKTYPE,MSGKTYPQ                                                
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    FILE1                                                            
         MVC   SGNMSG(25),=C'NO FILES CONTAIN MESSAGES'                         
         OI    PF9CMMDH+6,X'C0'                                                 
         B     XITS                                                             
*                                                                               
FILE1    LA    R2,PF9LISTH      SET UP LIST LOOPS                               
         OI    6(R2),X'80'                                                      
         MVC   8(60,R2),SPACES                                                  
         LA    R2,8(R2)                                                         
         LA    R6,5                  ENTRIES/LINE                               
         LA    R5,16                 LINES/PAGE                                 
*                                                                               
FILELP   GOTO1 HIGH                GET FILE                                     
         CLC   KEY(13),KEYSAVE                                                  
         MVI   MSGKDATE,X'FF'                                                   
         BNE   XITS                                                             
*                                                                               
         MVC   0(8,R2),MSGKFILE    MOVE IN NAME                                 
*                                                                               
         LA    R2,15(R2)           NEXT SLOT                                    
         BCT   R6,FILELP                                                        
*                                                                               
         LA    R6,5                NEXT LINE                                    
         OI    6(R2),X'80'                                                      
         MVC   8(60,R2),SPACES                                                  
         LA    R2,8(R2)                                                         
         BCT   R5,FILELP                                                        
         B     XITS                                                             
         EJECT                                                                  
*              LIST ADDRESS LISTS                                               
         SPACE 3                                                                
ADDLIST  CLI   LADCMMD,C'M'                                                     
         BE    SGNONG                                                           
         CLI   LADCMMD,C'A'                                                     
         BE    ADDA                                                             
*                                                                               
         MVI   OVERLAY,8                                                        
         L     R3,AOVERLAY                                                      
         GOTO1 LOADSOPH,DMCB,0                                                  
         LR    RF,R3                                                            
         GOTO1 (RF),DMCB,(RC)                                                   
         CLI   SYSSTAT,5                                                        
         BNE   LDPHASE                                                          
         B     XITS                                                             
*                                                                               
ADDA     LA    R3,SGNIH                                                         
         MVI   OVERLAY,X'F8'                                                    
         GOTO1 LOADSOPH,DMCB,1                                                  
         MVI   ADLCMMD,C'A'                                                     
         MVI   SYSSTAT,6                                                        
         B     LDPHASE                                                          
         EJECT                                                                  
*              PROCESS ADDRESS LISTS                                            
         SPACE 3                                                                
ADLPROC  CLI   ADLCMMDH+5,0                                                     
         BE    ADLPLD                                                           
         CLI   ADLCMMD,C'M'                                                     
         BE    ADLPM                                                            
ADLPLD   MVI   OVERLAY,7                                                        
         L     R3,AOVERLAY                                                      
         GOTO1 LOADSOPH,DMCB,0                                                  
         LR    RF,R3                                                            
         GOTO1 (RF),DMCB,(RC)                                                   
         CLI   MSADFLG,0                                                        
         BNE   ADLMSG                                                           
         CLI   ADLCMMD,C'K'                                                     
         BE    ADLPK                                                            
         CLI   ADLCMMD,C'N'                                                     
         BE    ADLPK                                                            
         B     XITS                                                             
*                                                                               
ADLMSG   CLI   ADLCMMD,C'K'                                                     
         BE    MSGLA1                                                           
         B     XITS                                                             
*                                                                               
ADLPM    MVI   LSTONTWA,0                                                       
         B     SGNONG                                                           
*                                                                               
ADLPK    LA    R3,SGNIH            KEEP-GO FOR NEXT                             
         MVI   OVERLAY,X'F7'                                                    
         GOTO1 LOADSOPH,DMCB,1                                                  
         MVI   LADCMMD,C'N'                                                     
         MVI   SYSSTAT,5                                                        
         B     LDPHASE                                                          
         EJECT                                                                  
*              DISPLAY AND MAINTAIN PROFILE                                     
         SPACE 3                                                                
PROFILE  CLI   PROCMMD,C'M'                                                     
         BE    SGNONG                                                           
         MVI   OVERLAY,9                                                        
         L     R3,AOVERLAY                                                      
         GOTO1 LOADSOPH,DMCB,0                                                  
         LR    RF,R3                                                            
         GOTO1 (RF),DMCB,(RC)                                                   
         B     XITS                                                             
         EJECT                                                                  
*              DO SPECIAL ZAP FUNCTIONS                                         
         SPACE 3                                                                
SPECIAL  CLI   NEWCMMD,C'M'                                                     
         BE    SGNONG                                                           
         MVI   OVERLAY,10                                                       
         L     R3,AOVERLAY                                                      
         GOTO1 LOADSOPH,DMCB,0                                                  
         LR    RF,R3                                                            
         GOTO1 (RF),DMCB,(RC)                                                   
         B     XITS                                                             
         EJECT                                                                  
*              LIST/ADD/DELETE USER RECORDS                                     
         SPACE 3                                                                
NEWUSER  CLI   NEWCMMD,C'M'                                                     
         BE    SGNONG                                                           
         MVI   OVERLAY,3                                                        
         L     R3,AOVERLAY                                                      
         GOTO1 LOADSOPH,DMCB,0                                                  
         LR    RF,R3                                                            
         GOTO1 (RF),DMCB,(RC)                                                   
         B     XITS                                                             
         EJECT                                                                  
*                                  SET SYSTEM DEPENDENT VALUES                  
SYSINIT  NTR1                                                                   
         SPACE 3                                                                
         LA    R2,SYSV                                                          
         LA    R3,SYSVCON                                                       
         LA    R4,NVTYPES                                                       
         SPACE 1                                                                
SYS2     L     R1,0(R3)            RELOCATE SYSTEM VTYPES                       
         A     R1,RELO                                                          
         ST    R1,0(R2)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,SYS2                                                          
         SPACE 1                                                                
         LA    R2,VCOMMON          SET UP COMMON ENTRIES                        
         SR    R3,R3                                                            
         LA    R4,SYSCOMM                                                       
         LA    R5,VCOUNT                                                        
         SPACE 1                                                                
SYS4     ST    R2,0(R4)                                                         
         STC   R3,0(R4)                                                         
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   R5,SYS4                                                          
         SPACE 1                                                                
         MVI   GCMODE,C'S'         USE GENCON IN SLAVE MODE                     
         MVI   ACTELOPT,C'N'       SUPPRESS ACTIVITY ELS                        
         LA    R1,SYSSTAT          FIRST BYTE TO SAVE/RESTORE                   
         ST    R1,ASTARTSV                                                      
         MVI   SYSTEM,C'Q'         PERSONNEL                                    
         MVI   MAXIOS,3            USES 3 I/O AREAS                             
         MVC   SIZEIO,=F'1024'     EACH I/O IS 1024 BYTES                       
         MVC   SYSDUMMY,VDUMMY     END OF SYSTEM BASE                           
         MVC   GETUSER,GETAGY      GET USER NAME AND ADDRESS                    
         MVC   LKEY,=H'36'         DETAILS OF DIRECTORY AND KEY                 
         MVC   LSTATUS,=H'2'                                                    
         MVC   DATADISP,=H'44'     USUALLY PERFIL                               
         MVC   SYSFIL,=C'PERFIL  '                                              
         MVC   SYSDIR,=C'PERDIR  '                                              
         MVI   GETMSYS,5           USES GETMSG FOR SYSTEM 5                     
         MVC   LWORK,=F'12000'     WE TOOK 12000 BYTES IN NMOD                  
         MVC   RCPROG(2),=C'PE'    PREFIX FOR REPORT NO.                        
         MVC   SYSPHASE,=X'D90E1D00'    PRESET FOR SYSTEM CALLOVS               
         SPACE 1                                                                
         B     XIT                                                              
         EJECT                                                                  
*              SYSTEM ROUTINES ENTERABLE FROM BASE OR OVERLAY                   
         SPACE 3                                                                
         DS    0H                                                               
VCOMMON  NTR1  BASE=SYSRB                                                       
         ST    RD,COMMRD                                                        
         SRL   RF,24                                                            
         B     VBRANCH(RF)                                                      
         SPACE 1                                                                
VBRANCH  B     VUSER                                                            
         B     VCHAT                                                            
         B     DCHAT                                                            
         B     VSGN                                                             
         B     VANY1                                                            
         SPACE 1                                                                
VCOUNT   EQU   (*-VBRANCH)/4                                                    
         EJECT                                                                  
*              USER NAME AND ADDRESS                                            
         SPACE 3                                                                
VUSER    L     R1,SYSPARMS                                                      
         MVC   AGENCY,0(R1)                                                     
*                                                                               
*              SWITCH TO PER SYSTEM                                             
*                                                                               
         XC    DMCB(20),DMCB                                                    
         GOTO1 SWITCH,DMCB,=C'PER'                                              
         CLI   DMCB+4,0                                                         
         BNE   VUSER2                                                           
*                                  AGENCY NAME & ADDRESS FROM ID REC.           
VUSER1   MVC   FILENAME,=CL8'CTFILE'                                            
         MVI   USEIO,C'Y'                                                       
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTIKEY,R4                                                        
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),10(RA)                                               
         GOTO1 READ                                                             
         L     R4,AIO                                                           
         LA    R6,CTIDATA                                                       
         MVI   ELCODE,X'36'                                                     
         BAS   RE,FIRSTEL                                                       
         BNE   VUSER2                                                           
         USING CTORGD,R6                                                        
         MVC   USERNAME,CTORGNAM                                                
         MVC   USERADDR,CTORGADD                                                
         SPACE 1                                                                
VUSER2   XC    FILENAME,FILENAME                                                
         MVI   USEIO,0                                                          
         B     XIT                                                              
         EJECT                                                                  
*              USER SIGNON CODE                                                 
         SPACE 3                                                                
VSGN     MVC   SGKEYSV,KEY                                                      
         MVC   FILENAME,=CL8'CTFILE'                                            
         MVI   USEIO,C'Y'                                                       
         MVC   AIO,AIO3                                                         
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTIKEY,R4                                                        
         MVI   CTIKTYP,C'I'                                                     
         CLI   WORK,1                                                           
         BNE   *+14                                                             
         MVC   CTIKID+8(2),WORK+1                                               
         B     VSGNRD                                                           
         CLI   WORK,2                                                           
         BNE   XIT                                                              
         MVC   CTIKTYP+15(10),WORK+1                                            
VSGNRD   GOTO1 HIGH                                                             
         CLI   WORK,1                                                           
         BNE   *+10                                                             
         MVC   WORK(6),=C'NO ORG'                                               
         CLI   WORK,2                                                           
         BNE   *+10                                                             
         XC    WORK(2),WORK                                                     
         CLC   KEY(25),KEYSAVE                                                  
         BNE   VSGN2                                                            
         L     R4,AIO                                                           
         LA    R6,CTIDATA                                                       
         MVI   ELCODE,X'02'                                                     
         BAS   RE,FIRSTEL                                                       
         BNE   VSGN2                                                            
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'3'                                                         
         EXMVC R1,WORK,2(R6)                                                    
         SPACE 1                                                                
VSGN2    XC    FILENAME,FILENAME                                                
         MVI   USEIO,0                                                          
         MVC   KEY,SGKEYSV                                                      
         MVC   AIO,AIO1                                                         
         GOTO1 HIGH                                                             
         MVC   KEY,KEYSAVE                                                      
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ANY1                                                             
         SPACE 3                                                                
VANY1    MVC   WORK,SPACES                                                      
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     XIT                                                              
         MVC   WORK(0),8(R2)                                                    
         EJECT                                                                  
*              HANDLE COMMENT TYPE ELEMENTS                                     
         SPACE 3                                                                
VCHAT    CLI   MODE,DISPREC                                                     
         BE    DCHAT                                                            
         ZIC   R0,MAX              MAX FIELDS                                   
         LA    R4,1                SEQUENCE NUMBER                              
         GOTO1 REMELEM             TAKE OUT EXISTING                            
         SPACE 1                                                                
VCHAT2   TM    1(R2),X'20'                                                      
         BZ    *+8                                                              
         BAS   RE,BUMP             SKIP PAST PROTECTED FIELD                    
         XC    ELEMENT,ELEMENT                                                  
         MVC   ELEMENT(1),ELCODE   BUILD AN ELEMENT                             
         LA    R3,4                                                             
         CLI   5(R2),0                                                          
         BE    VCHAT3                                                           
         ZIC   R3,5(R2)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   ELEMENT+4(0),8(R2)  MOVE IN DATA                                 
         LA    R3,5(R3)                                                         
VCHAT3   STC   R3,ELEMENT+1        LENGTH OF L'DATA+4                           
         STC   R4,ELEMENT+2        SEQUENCE NO.                                 
         MVC   ELEMENT+3(1),OPTION USER CAN SUPPLY BYTE 4                       
         GOTO1 ADDELEM                                                          
         BAS   RE,BUMP                                                          
         LA    R4,1(R4)                                                         
         BCT   R0,VCHAT2                                                        
         B     XIT                                                              
         SPACE 1                                                                
DCHAT    ZIC   R0,MAX              DISPLAY UP TO MAX FIELDS                     
         L     R6,AIO              USER SUPPLIED ELCODE                         
         BAS   RE,GETEL                                                         
         B     *+8                                                              
         SPACE 1                                                                
DCHAT2   BAS   RE,NEXTEL                                                        
         BNE   DCHAT4                                                           
         MVC   WORK,SPACES                                                      
         ZIC   R1,1(R6)                                                         
         CH    R1,=H'4'                                                         
         BE    DCHAT3                                                           
         SH    R1,=H'5'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),4(R6)       PICK OUT DATA                                
DCHAT3   BAS   RE,GENDISP                                                       
         BAS   RE,BUMP                                                          
         TM    1(R2),X'20'                                                      
         BZ    *+8                                                              
         BAS   RE,BUMP             SKIP PAST PROTECTED FIELD                    
         BCT   R0,DCHAT2                                                        
         B     XIT                                                              
         SPACE 1                                                                
DCHAT4   MVC   WORK,SPACES         CLEAR REST                                   
         BAS   RE,GENDISP                                                       
         BAS   RE,BUMP                                                          
         TM    1(R2),X'20'                                                      
         BZ    *+8                                                              
         BAS   RE,BUMP             SKIP PAST PROTECTED FIELD                    
         BCT   R0,DCHAT4                                                        
         B     XIT                                                              
         EJECT                                                                  
*              ODDMENTS                                                         
         SPACE 1                                                                
GENDISP  NTR1                                                                   
         CLI   OPTION,C'W'         OPTION TO RETURN DATA IN WORK                
         BE    XIT                                                              
         ZIC   R1,0(R2)                                                         
         SH    R1,=H'9'                                                         
         EX    R1,GENCLC           IS DATA ON SCREEN ALREADY                    
         BE    XIT                                                              
         EX    R1,GENMVC           NO - SO MOVE IT THERE                        
         OI    6(R2),X'80'              AND TRANSMIT                            
         B     XIT                                                              
         SPACE 1                                                                
GENCLC   CLC   8(0,R2),WORK                                                     
GENMVC   MVC   8(0,R2),WORK                                                     
         SPACE 1                                                                
BUMP     ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         BR    RE                                                               
         SPACE 1                                                                
VTRYHARD LA    R3,35                                                            
         BAS   RE,VTRYSOFT                                                      
         B     XIT                                                              
         SPACE 1                                                                
VTRYSOFT NTR1                                                                   
         GOTO1 HIGH                SEE IF WE CAN GET RECORD                     
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   KEY(00),KEYSAVE                                                  
         BNE   TRAPERR             NO - OUT                                     
         B     XIT                                                              
         SPACE 1                                                                
TRAPERR  GOTO1 ERREX                                                            
         SPACE 2                                                                
XITS     CLI   OFFLINE,C'Y'                                                     
         BE    XIT                                                              
         GOTO1 SAVEUWKA          SAVE TEMP STORAGE                              
         SPACE 2                                                                
XIT      XIT1                                                                   
         SPACE 1                                                                
         GETEL (R6),DATADISP,ELCODE                                             
         EJECT                                                                  
*              CONSTANTS TABLES ETC                                             
         SPACE 3                                                                
RELO     DS    A                                                                
         SPACE 1                                                                
SYSVCON  DS    0F                                                               
         DC    V(DUMMY)                                                         
         SPACE 1                                                                
NVTYPES  EQU   (*-SYSVCON)/4                                                    
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE PEMSGFFD                                                       
         EJECT                                                                  
         ORG     SGNIH                                                          
       ++INCLUDE PEMSGFED                                                       
         EJECT                                                                  
         ORG     SGNIH                                                          
       ++INCLUDE PEMSGFDD                                                       
         EJECT                                                                  
         ORG     SGNIH                                                          
       ++INCLUDE PEMSGFCD                                                       
         EJECT                                                                  
         ORG     SGNIH                                                          
       ++INCLUDE PEMSGFBD                                                       
         EJECT                                                                  
         ORG     SGNIH                                                          
       ++INCLUDE PEMSGFAD                                                       
         EJECT                                                                  
         ORG     SGNIH                                                          
       ++INCLUDE PEMSGF9D                                                       
         EJECT                                                                  
         ORG     SGNIH                                                          
       ++INCLUDE PEMSGF8D                                                       
         EJECT                                                                  
         ORG     SGNIH                                                          
       ++INCLUDE PEMSGF7D                                                       
         EJECT                                                                  
         ORG     SGNIH                                                          
       ++INCLUDE PEMSGF6D                                                       
         EJECT                                                                  
CONHEADH EQU     SGNMSGH                                                        
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE PEMSGWORKD                                                     
         EJECT                                                                  
       ++INCLUDE PEMSGFILE                                                      
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'184PEMSG00   05/01/02'                                      
         END                                                                    
