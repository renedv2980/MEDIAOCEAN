*          DATA SET TAGENCC    AT LEVEL 030 AS OF 07/20/12                      
*PHASE T702CCC                                                                  
         TITLE 'T702CC - SOAP GUARANTEE LIST'                                   
T702CC   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T702CC                                                         
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
         CLI   MODE,VALKEY         FIRST TIME IN                                
         BNE   *+12                                                             
         BAS   RE,VKEY                                                          
         B     XIT                                                              
*                                                                               
         CLI   MODE,LISTRECS                                                    
         BNE   SG30                                                             
         MVI   NLISTS,15           SET N'LIST LINES                             
         OI    GLSTSTAT,RETEXTRA   SET OKAY TO RETURN EXTRA FOR EOL             
         MVC   LISTAR,SPACES       CLEAR PREVIOUS LINE                          
         LA    R2,LISTAR                                                        
         B     SG40                                                             
         SPACE 1                                                                
SG30     CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
         ZAP   COUNTER,=P'0'       CLEAR LINE COUNTER                           
         XC    KEY,KEY             ENSURE START AT TOP OF LIST                  
         LA    R2,MYSPECS                                                       
         ST    R2,SPECS                                                         
         LA    R2,HOOK                                                          
         ST    R2,HEADHOOK                                                      
         LA    R2,P                                                             
         SPACE 1                                                                
SG40     BAS   RE,LREC             GO LIST THE RECORDS                          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE THE KEY                                      
         SPACE 1                                                                
VKEY     NTR1                                                                   
         TM    SSGSSNH+4,X'20'     VALIDATE S/S NUMBER                          
         BO    VK10                                                             
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'08',SSGSSNH),SSGSSNNH                     
         MVC   TIFSSN,TGSSN                                                     
         NI    SSGCODEH+4,X'DF'                                                 
*                                                                               
VK10     LA    R2,SSGCODEH         START AT SPECIFIC GUARANTEE                  
         TM    4(R2),X'20'                                                      
         BO    VK20                                                             
         XC    TIQSTART,TIQSTART   START FROM BEGINING                          
         CLI   5(R2),0                                                          
         BE    VK15                                                             
         MVC   TIQSTART(4),8(R2)                                                
VK15     OI    4(R2),X'20'         SET PREVIOUSLY VALIDATED                     
         NI    SSGAGYH+4,X'DF'     FORCE VALIDATION OF NEXT FIELD               
*                                                                               
VK20     LA    R2,SSGAGYH          OPTIONAL AGENCY FILTER                       
         TM    4(R2),X'20'                                                      
         BO    VK30                                                             
         XC    TIFAGY,TIFAGY                                                    
         XC    SSGAGYN,SSGAGYN     PRE-CLEAR NAME                               
         OI    SSGAGYNH+6,X'80'                                                 
         CLI   5(R2),0                                                          
         BE    VK30                                                             
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'28',(R2)),SSGAGYNH  AGENCY                
         MVC   TIFAGY,TGAGY                                                     
         GOTO1 RAVPPLSA,DMCB,0     SEE IF REC / ACT INVALID FOR P+              
         JNE   ERPPLSI                                                          
*                                                                               
VK30     OI    4(R2),X'20'         SET PREVIOUSLY VALIDATED                     
         XC    KEY,KEY                                                          
*                                                                               
VKX      B     XIT                                                              
         EJECT                                                                  
*              ROUTINE CONTROLS RECORD LISTING                                  
*                                                                               
LREC     NTR1                                                                   
         LA    R0,LRHOOK           SET HOOK TO SYSIO                            
         ST    R0,TIHOOK                                                        
         MVC   TIACOMFC,ACOMFACS                                                
         MVC   TIKHOOK,SETLSTK                                                  
         MVC   TIUSERID,TWAORIG    SET UP SOME FIELDS FOR SYSIO                 
         MVC   TIQSTAFF,TGCTSTAF                                                
         MVI   TIREAD,TLSGCDQ                                                   
         SPACE 1                                                                
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
*                                                                               
         XC    TIQSKEY,TIQSKEY     CLEAR CONTINUE KEY                           
         CLI   MODE,PRINTREP                                                    
         BNE   LRX                                                              
         CP    COUNTER,=P'0'       IF ANYTHING REPORTED                         
         BE    LRX                                                              
         EDIT  COUNTER,(5,P),COMMAS=YES,ALIGN=LEFT                              
         LR    R1,R0                                                            
         LA    R1,P+1(R1)                                                       
         MVC   0(22,R1),=C'SOAP GUARANTEE RECORDS'                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
         TM    WHEN,X'40'          IF SPOOLING AND NOW                          
         BZ    LRX                                                              
         XC    CONSERV,CONSERV     AUTO $DQU                                    
         MVC   CONSERV(4),=C'$DQU'                                              
         TWAXC SSGSELH,PROT=Y                                                   
*                                                                               
LRX      B     XIT                                                              
         EJECT                                                                  
*               PROCESS SYSIO RECORDS                                           
*                                                                               
         USING LINED,R2            R2=A(OUTPUT AREA)                            
LRHOOK   NTR1                                                                   
         CLI   TIMODE,PROCREC      PROCESS RECORDS                              
         BNE   XIT                                                              
         CLI   MODE,PRINTREP                                                    
         BE    LR40                                                             
*                                                                               
         CLC   LISTNUM,NLISTS      IF ALREADY DISPLAYED MAX                     
         BE    LR20                                                             
         BAS   RE,DISPLAY                                                       
         MVC   DMDSKADD,TIDSKADD   PASS DISK ADDRESS TO LISTMON                 
LR20     GOTO1 LISTMON             CALL LISTMON                                 
         B     XIT                                                              
*                                                                               
LR40     BAS   RE,DISPLAY                                                       
         GOTO1 SPOOL,DMCB,(R8)                                                  
         AP    COUNTER,=P'1'                                                    
         B     XIT                                                              
         EJECT                                                                  
*              DISPLAY RECORD ROUTINE                                           
*                                                                               
DISPLAY  NTR1                                                                   
         MVC   LINGUA,TIGUA        GUARANTEE CODE                               
         SPACE 1                                                                
         L     R4,TIAREC                                                        
         MVI   ELCODE,TASGELQ      LOOK FOR SOAP GUARANTEE DETAILS              
         BAS   RE,GETEL                                                         
         BNE   DISPX                                                            
         USING TASGD,R4                                                         
*                                                                               
         MVC   LINAGY,TASGAGY      AGENCY                                       
         MVC   LINCAT,TASGCAT      CATEGORY                                     
         MVC   LINCRP,TASGCRP      CORPORATION NUMBER                           
         XC    LINAGNT,LINAGNT                                                  
         OC    TASGNCDE,TASGNCDE   AGENT CODE                                   
         BZ    DISP10                                                           
         GOTO1 TRNSAGT,DMCB,(X'40',TASGNCDE),LINAGNT                            
DISP10   LA    RE,PYMTBL                                                        
         USING PYMTBLD,RE                                                       
DISP12   CLC   TASGPYMT,PYMTST     MATCH ON STATUS BYTE                         
         BE    DISP15                                                           
         LA    RE,PYMTLNQ(RE)      BUMP TO NEXT ENTRY IN TABLE                  
         CLI   0(RE),X'FF'                                                      
         BNE   DISP12                                                           
         DC    H'0'                                                             
DISP15   MVC   LINPYMT(L'PYMTTYP),PYMTTYP                                       
*                                                                               
         XC    LINPD,LINPD         CLEAR PERIOD                                 
         XC    WORK(6),WORK        GET PERIOD DATES                             
         L     R4,TIAREC                                                        
         MVI   ELCODE,TASLELQ      LOOK FOR SOAP CYCLE ELEMENT                  
         BAS   RE,GETEL                                                         
         BNE   DISP30              NO CYCLE LINES                               
         USING TASLD,R4                                                         
         MVC   WORK(L'TASLSSTR),TASLSSTR                                        
         MVC   WORK+3(L'TASLSEND),TASLSEND                                      
DISP20   BAS   RE,NEXTEL                                                        
         BNE   DISP25                                                           
         MVC   WORK+3(L'TASLSEND),TASLSEND                                      
         B     DISP20                                                           
*                                                                               
DISP25   GOTO1 DATCON,DMCB,(1,WORK),(8,LINPD)                                   
         MVI   LINPD+8,C'-'                                                     
         GOTO1 DATCON,DMCB,(1,WORK+3),(8,LINPD+9)                               
*                                                                               
DISP30   XC    BLOCK(8),BLOCK      SET UP FAKE HEADER                           
         MVI   BLOCK,L'LINROLE+8                                                
         MVC   AIO,TIAREC                                                       
         GOTO1 CHAROUT,DMCB,TAFNELQ,BLOCK,TAFNTROL                              
         MVC   AIO,AIO1                                                         
         MVC   LINROLE,BLOCK+8     DISPLAY ROLE                                 
DISPX    B     XIT                                                              
         EJECT                                                                  
*              HEADHOOK ROUTINE                                                 
         SPACE 2                                                                
HOOK     NTR1                                                                   
         MVI   BYTE,C'H'           PRINT HEADLINES                              
         GOTO1 PRTSCRN,DMCB,CONTAGH,SSGHD1H,H4-1                                
         GOTO1 (RF),(R1),SSGHD1H,SSGSELH,H7-5                                   
         MVC   H7-5(5),SPACES                                                   
         B     XIT                                                              
         EJECT                                                                  
*              EXITS, ETC.                                                      
         SPACE 2                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 2                                                                
INVERR   MVI   ERROR,INVALID                                                    
         B     THEEND                                                           
         SPACE 1                                                                
ERPPLSI  MVC   MYMSGNO,=Y(ERRIAPPA)   RECORD / ACTION INVALID FOR P+            
         J     ERREND                                                           
                                                                                
ERREND   MVI   MYMTYP,GTMERR       ERROR MESSAGE EXIT                           
         OI    GENSTAT2,USGETTXT                                                
         J     THEEND                                                           
                                                                                
THEEND   GOTO1 EXIT,DMCB,0                                                      
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 2                                                                
HEXFFS   DC    4X'FF'                                                           
         SPACE 1                                                                
PYMTBL   DS    0CL8                                                             
         DC    CL7'WEEK',AL1(TASGPWK)                                           
         DC    CL7'PERFORM',AL1(TASGPPER)                                       
         DC    X'FF'                                                            
         SPACE                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              REPORT SPECS                                                     
         SPACE 2                                                                
MYSPECS  DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,56,REQUESTOR                                                  
         SSPEC H2,56,REPORT                                                     
         SSPEC H2,73,PAGE                                                       
         SPACE 1                                                                
         SSPEC H1,30,C'SOAP GUARANTEE LIST'                                     
         SSPEC H2,30,C'-------------------'                                     
         SPACE 1                                                                
         SSPEC H8,1,C'----  ------  ---  ---  -----------------  -----'         
         SSPEC H8,50,C'-------  ----------------'                               
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
*              DSECT TO COVER LIST LINE                                         
         SPACE 2                                                                
LINED    DSECT                                                                  
LINGUA   DS    CL4                 SOAP GUARANTEE CODE                          
         DS    CL2                                                              
LINAGY   DS    CL6                 AGENCY                                       
         DS    CL2                                                              
LINCAT   DS    CL3                 CATEGORY                                     
         DS    CL2                                                              
         DS    CL1                                                              
LINCRP   DS    CL1                 CORP CODE                                    
         DS    CL1                                                              
         DS    CL2                                                              
LINPD    DS    CL17                AIR PERIOD                                   
         DS    CL2                                                              
LINAGNT  DS    CL4                 AGENT CODE                                   
         DS    CL2                                                              
LINPYMT  DS    CL7                 PAY PER (WEEK)/PERFORM                       
         DS    CL2                                                              
LINROLE  DS    CL16                ROLE                                         
         SPACE                                                                  
*              DSECT TO PAYMENT METHODS TABLE                                   
         SPACE 2                                                                
PYMTBLD  DSECT                                                                  
PYMTTYP  DS    CL7                 PAYMENT TYPE                                 
PYMTST   DS    XL1                 PAYMENT EQUATE FOR THIS TYPE                 
PYMTLNQ  EQU   *-PYMTBLD                                                        
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRCCD                                                       
         SPACE 3                                                                
COUNTER  DS    PL4                 RECORD COUNTER                               
SVKEY    DS    CL(L'KEY)           SAVED KEY                                    
         EJECT                                                                  
* TASYSIOD      (MUST FOLLOW LAST SCREEN)                                       
* DDGENTWA      (MUST FOLLOW LAST SCREEN)                                       
* TASYSDSECT                                                                    
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE TASYSIOD                                                       
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'030TAGENCC   07/20/12'                                      
         END                                                                    
