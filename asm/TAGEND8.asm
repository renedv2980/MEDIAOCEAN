*          DATA SET TAGEND8    AT LEVEL 017 AS OF 07/20/12                      
*PHASE T702D8C,*                                                                
         TITLE 'T702D8 - ALIAS LIST '                                           
T702D8   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T702D8                                                         
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
         SPACE 1                                                                
         GOTO1 INITIAL,DMCB,0                                                   
         SPACE 1                                                                
         CLI   MODE,VALKEY                                                      
         BNE   *+12                                                             
         BAS   RE,VKEY             VALIDATE KEY FIELDS                          
         B     XIT                                                              
         SPACE 1                                                                
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BNE   AK30                                                             
         MVI   NLISTS,15           SET N'LIST LINES                             
         OI    GLSTSTAT,RETEXTRA   SET OKAY TO RETURN EXTRA FOR EOL             
         MVC   LISTAR,SPACES       CLEAR PREVIOUS LINE                          
         LA    R2,LISTAR           R2=A(DISPLAY LINE)                           
         B     AK40                                                             
         SPACE 1                                                                
AK30     CLI   MODE,PRINTREP       PRINT REPORT                                 
         BNE   XIT                                                              
         ZAP   COUNTER,=P'0'       CLEAR LINE COUNTER                           
         LA    R2,HOOK             SET A(HEADLINE HOOK)                         
         ST    R2,HEADHOOK                                                      
         LA    R2,MYSPECS          SET A(SPECS)                                 
         ST    R2,SPECS                                                         
         LA    R2,P                R2=A(DISPLAY LINE)                           
         SPACE 1                                                                
AK40     BAS   RE,LREC             GO LIST THE RECORDS                          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE KEY FIELDS                                   
         SPACE 1                                                                
VKEY     NTR1                                                                   
*                                                                               
         LA    R2,SAKAGYH          AGENCY                                       
         CLI   SCRSTAT,0           IF SCREEN CHANGED                            
         BNE   VK5                                                              
         TM    4(R2),X'20'         OR NOT PREVIOUSLY VALIDATED                  
         BO    VK10                                                             
VK5      NI    SAKNCLIH+4,X'DF'    FORCE VALIDATION OF NEXT FIELD               
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'28',(R2)),SAKAGYNH                        
         MVC   TIFAGY,TGAGY        SET SYSIO FILTER                             
         GOTO1 RAVPPLSA,DMCB,0     SEE IF REC / ACT INVALID FOR P+              
         JNE   ERPPLSI                                                          
*                                                                               
VK10     OI    4(R2),X'20'         SET PREVIOUS FIELD VALIDATED                 
         LA    R2,SAKNCLIH         R2=A(NETWORK/SPOT CLIENT FIELD)              
         TM    4(R2),X'20'                                                      
         BO    VK20                                                             
         NI    SAKNPRDH+4,X'DF'    FORCE VALIDATION OF NEXT FIELD               
         XC    TIFCLI,TIFCLI                                                    
         CLI   5(R2),0                                                          
         BE    VK20                                                             
         GOTO1 ANY                                                              
         MVC   TIFCLI,WORK                                                      
*                                                                               
VK20     OI    4(R2),X'20'         SET PREVIOUS FIELD VALIDATED                 
         LA    R2,SAKNPRDH         R2=A(NETWORK/SPOT PRODUCT FIELD)             
         TM    4(R2),X'20'                                                      
         BO    VK25                                                             
         NI    SAKMEDH+4,X'DF'     FORCE VALIDATION OF NEXT FIELD               
         XC    TIFPRD,TIFPRD                                                    
         CLI   5(R2),0                                                          
         BE    VK25                                                             
         GOTO1 ANY                                                              
         MVC   TIFPRD,WORK                                                      
*                                                                               
VK25     OI    4(R2),X'20'         SET PREVIOUS FIELD VALIDATED                 
         LA    R2,SAKMEDH          R2=A(NETWORK/SPOT MEDIA FIELD)               
         TM    4(R2),X'20'                                                      
         BO    VK30                                                             
         NI    SAKNIDH+4,X'DF'     FORCE VALIDATION OF NEXT FIELD               
         XC    TIFMED,TIFMED                                                    
         CLI   5(R2),0                                                          
         BE    VK30                                                             
         GOTO1 MEDVAL,DMCB,8(R2)                                                
         BNE   FLDINV                                                           
         MVC   TIFMED,TGMENAME                                                  
*                                                                               
VK30     OI    4(R2),X'20'         SET PREVIOUS FIELD VALIDATED                 
         LA    R2,SAKNIDH          R2=A(START NETWORK/SPOT COMML ID)            
         TM    4(R2),X'20'                                                      
         BO    VKEYX                                                            
         XC    TIQSTART,TIQSTART                                                
         CLI   5(R2),0                                                          
         BE    VK40                                                             
         MVC   TIQSTART(L'SAKNID),SAKNID                                        
         OC    TIQSTART,SPACES                                                  
*                                                                               
VK40     OI    4(R2),X'20'         SET PREVIOUS FIELD VALIDATED                 
         XC    KEY,KEY             RE-INITIALIZE LIST                           
*                                                                               
VKEYX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE CONTROLS RECORD LISTING                                  
         SPACE 1                                                                
LREC     NTR1                                                                   
         LA    R0,LRHOOK           SET HOOK TO SYSIO                            
         ST    R0,TIHOOK                                                        
*                                                                               
         MVC   TIUSERID,TWAORIG    SET UP SOME FIELDS FOR SYSIO                 
         MVC   TIQSTAFF,TGCTSTAF                                                
         MVI   TIREAD,TLAKCDQ      SET RECORD TYPE FOR READS                    
         MVC   TIKHOOK,SETLSTK                                                  
         MVC   TIACOMFC,ACOMFACS                                                
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
*                                                                               
         XC    TIQSKEY,TIQSKEY     END OF LIST - CLEAR CONTINUE KEY             
         CLI   MODE,PRINTREP                                                    
         BNE   LRX                                                              
         CP    COUNTER,=P'0'       IF ANYTHING REPORTED                         
         BE    LRX                                                              
         BAS   RE,PRNTIT           SKIP A LINE                                  
         EDIT  COUNTER,(5,P),COMMAS=YES,ALIGN=LEFT                              
         LR    R1,R0                                                            
         LA    R1,P+1(R1)                                                       
         MVC   0(13,R1),=C'ALIAS RECORDS'                                       
         BAS   RE,PRNTIT                                                        
*                                                                               
         TM    WHEN,X'40'          IF SPOOLING AND NOW                          
         BZ    LRX                                                              
         XC    CONSERV,CONSERV     AUTO $DQU                                    
         MVC   CONSERV(4),=C'$DQU'                                              
         TWAXC SAKSELH,PROT=Y                                                   
*                                                                               
LRX      B     XIT                                                              
         EJECT                                                                  
*               PROCESS SYSIO RECORDS TO SCREEN                                 
         SPACE 1                                                                
LRHOOK   NTR1                                                                   
         CLI   TIMODE,PROCREC      PROCESS RECORDS                              
         BNE   LRHX                                                             
*                                                                               
         USING TLAKD,R6                                                         
         L     R6,TIAREC           SKIP VERSION ALIASES                         
         CLI   TLAKVER,0                                                        
         BNE   LRHX                                                             
         DROP  R6                                                               
*                                                                               
         BAS   RE,DISPLAY          DISPLAY RECORD                               
*                                                                               
         CLI   MODE,PRINTREP       IF DISPLAYING TO SCREEN                      
         BE    LRH30                                                            
         MVC   DMDSKADD,TIDSKADD   PASS DISK ADDRESS TO LISTMON                 
         GOTO1 LISTMON             DISPLAY LINE TO SCREEN                       
         B     LRHX                                                             
*                                                                               
LRH30    GOTO1 SPOOL,DMCB,(R8)                                                  
         AP    COUNTER,=P'1'       DISPLAY LINE TO REPORT                       
*                                                                               
LRHX     B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO DISPLAY ONE LINE OF RECORD INFO                       
         SPACE                                                                  
         USING LINED,R2            R2=A(OUTPUT AREA)                            
DISPLAY  NTR1                                                                   
         XC    LINED(LINLNQ),LINED                                              
         L     R6,TIAREC           R6=A(SYSIO RECORD)                           
         USING TLAKD,R6                                                         
*                                                                               
         MVC   LINNID,TICID        NETWORK/SPOT COMMERCIAL ID                   
         MVC   LINNCLI,TICLI       NETWORK/SPOT CLIENT CODE                     
         MVC   LINNPRD,TIPRD       NETWORK/SPOT PRODUCT CODE                    
         MVC   LINNMED,TIMED       NETWORK/SPOT MEDIA CODE                      
*                                                                               
         MVC   TGCOM,TICOM                                                      
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'20',0)                                   
         BNE   DISP10                                                           
         L     R4,AIO                                                           
         MVI   ELCODE,TACOELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACOD,R4                                                         
         MVC   LINCID,TACOCID      TALENT CID                                   
*                                                                               
DISP10   MVI   LINPLFT,C'N'                                                     
         TM    TLAKSTAT,TLAKSLFT   IF ALIAS(LIFT)                               
         BZ    *+8                                                              
         MVI   LINPLFT,C'Y'        INDICATE SO                                  
*                                                                               
         XC    KEY,KEY             RESET READ SEQUENCE FOR SYSIO                
         MVC   KEY(L'TIKEY),TIKEY                                               
         GOTO1 HIGH                                                             
         B     XIT                                                              
         DROP  R2,R4,R6                                                         
         EJECT                                                                  
*              ROUTINE TO PRINT A LINE                                          
         SPACE                                                                  
PRNTIT   NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         SPACE 3                                                                
*              HEADLINE HOOK                                                    
         SPACE                                                                  
HOOK     NTR1                                                                   
         MVI   BYTE,C'H'           MOVE KEY FIELDS TO HEADS                     
         GOTO1 PRTSCRN,DMCB,CONTAGH,SAKHD1H,H4-1                                
         GOTO1 (RF),(R1),SAKHD1H,SAKSELH,H7-5                                   
         MVC   H7-5(5),SPACES      CLEAR SELECT FIELD                           
         MVI   BYTE,C'P'           RESET                                        
         B     XIT                                                              
         SPACE 3                                                                
         GETEL R4,DATADISP,ELCODE                                               
         SPACE 3                                                                
*              EXITS, CONSTANTS, ETC.                                           
         SPACE 1                                                                
FLDINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         J     THEEND                                                           
                                                                                
ERPPLSI  MVC   MYMSGNO,=Y(ERRIAPPA)   RECORD / ACTION INVALID FOR P+            
         J     ERREND                                                           
                                                                                
ERREND   MVI   MYMTYP,GTMERR       ERROR MESSAGE EXIT                           
         OI    GENSTAT2,USGETTXT                                                
         J     THEEND                                                           
                                                                                
         SPACE 1                                                                
THEEND   GOTO1 EXIT,DMCB,0                                                      
         SPACE 1                                                                
XIT      XIT1                                                                   
         SPACE 2                                                                
XFFS     DC    6X'FF'                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              REPORT SPECS                                                     
         SPACE 2                                                                
MYSPECS  DS    0H                                                               
         SSPEC H1,2,RUN                                                         
         SSPEC H1,56,REPORT                                                     
         SSPEC H1,73,PAGE                                                       
         SSPEC H2,56,REQUESTOR                                                  
         SPACE 1                                                                
         SSPEC H1,33,C'ALIAS LIST'                                              
         SSPEC H2,33,C'----------'                                              
         SPACE 1                                                                
         SSPEC H8,1,C'------------  ---  ---- -----  ----------'                
         SSPEC H8,46,C'---------'                                               
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
*              DSECT TO COVER LIST LINE                                         
         SPACE 1                                                                
LINED    DSECT                                                                  
LINNID   DS    CL12                NETWORK/SPOT COMMERCIAL ID                   
         DS    CL2                                                              
LINNCLI  DS    CL3                 NETWORK/SPOT CLIENT CODE                     
         DS    CL2                                                              
LINNPRD  DS    CL3                 NETWORK/SPOT PRODUCT CODE                    
         DS    CL2                                                              
         DS    CL2                                                              
LINNMED  DS    CL1                 NETWORK/SPOT MEDIA CODE                      
         DS    CL2                                                              
         DS    CL2                                                              
LINCID   DS    CL12                TALENT COMMERCIAL ID                         
         DS    CL2                                                              
         DS    CL4                                                              
LINPLFT  DS    CL1                 PAY LIFT                                     
LINLNQ   EQU   *-LINED             LENGTH OF LINE                               
*                                                                               
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRD8D                                                       
COUNTER  DS    PL4                 RECORD COUNTER                               
         EJECT                                                                  
* TASYSIOD      (MUST FOLLOW LAST SCREEN)                                       
* DDGENTWA      (MUST FOLLOW LAST SCREEN)                                       
* TASYSDSECT                                                                    
* TASYSEQUS                                                                     
* TAGENWORKD                                                                    
* TAGENEQUS                                                                     
* TAGENFILE                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE TASYSIOD                                                       
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENEQUS                                                      
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017TAGEND8   07/20/12'                                      
         END                                                                    
