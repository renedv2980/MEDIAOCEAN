*          DATA SET TAGENA9    AT LEVEL 003 AS OF 05/01/02                      
*PHASE T702A9A                                                                  
         TITLE 'T702A9 - PMUSIC RECORD LIST'                                    
T702A9   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T702A9                                                         
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
*                                                                               
         GOTO1 INITIAL,DMCB,PFTAB                                               
*                                                                               
         CLI   MODE,VALKEY         FIRST TIME IN                                
         BNE   *+12                                                             
         BAS   RE,VKEY                                                          
         B     XIT                                                              
*                                                                               
         CLI   MODE,LISTRECS                                                    
         BNE   MUS10                                                            
         MVC   LISTAR,SPACES       CLEAR PREVIOUS LINE                          
         LA    R2,LISTAR                                                        
         B     MUS20                                                            
*                                                                               
MUS10    CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
         ZAP   COUNTER,=P'0'       CLEAR LINE COUNTER                           
         XC    KEY,KEY             ENSURE START AT TOP OF LIST                  
         LA    R2,MYSPECS                                                       
         ST    R2,SPECS                                                         
         LA    R2,P+1                                                           
*                                                                               
MUS20    BAS   RE,LREC             GO LIST THE RECORDS                          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE THE KEY                                      
*                                                                               
VKEY     NTR1                                                                   
*                                                                               
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'08',SMUAGYH),SMUAGYNH                     
         MVC   TIFAGY,TGAGY                                                     
         LA    R2,SMUSTRH          START AT SPECIFIC MUSIC                      
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED                         
         BO    VK10                                                             
         NI    SMUOPTSH+4,X'DF'    FORCE VALIDATION OF NEXT FIELD               
         XC    TIQSTART,TIQSTART   START FROM BEGINING                          
         CLI   5(R2),0                                                          
         BE    *+10                                                             
         MVC   TIQSTART(L'TLMUMUS),8(R2)                                        
         OI    4(R2),X'20'         VALIDATED                                    
*                                                                               
VK10     LA    R2,SMUOPTSH         OPTIONS                                      
         TM    4(R2),X'20'                                                      
         BO    VKX                                                              
         OI    4(R2),X'20'         VALIDATED                                    
*                                                                               
         XC    KEY,KEY             INITIALIZE KEY                               
         MVC   TIUSERID,TWAORIG    SET UP SOME FIELDS FOR SYSIO                 
         MVC   TIQSTAFF,TGCTSTAF                                                
         MVI   TIREAD,TLMUCDQ                                                   
*                                                                               
VKX      B     XIT                                                              
         EJECT                                                                  
*              ROUTINE CONTROLS RECORD LISTING                                  
*                                                                               
LREC     NTR1                                                                   
*                                                                               
         LA    R0,LRHOOK           SET HOOK TO SYSIO                            
         ST    R0,TIHOOK                                                        
         MVC   TIACOMFC,ACOMFACS                                                
         MVC   TIKHOOK,SETLSTK                                                  
*                                                                               
         MVI   NLISTS,17           IN ORDER TO GET CONTROL                      
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
         MVI   NLISTS,16           BACK AFTER 1 FULL PAGE                       
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BNE   LRX                                                              
         CP    COUNTER,=P'0'       IF ANYTHING REPORTED                         
         BE    LRX                                                              
         EDIT  COUNTER,(3,P+1),COMMAS=YES,ALIGN=LEFT                            
         LR    R1,R0                                                            
         LA    R1,P+2(R1)                                                       
         MVC   0(13,R1),=C'MUSIC RECORDS'                                       
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         TM    WHEN,X'40'          IF SPOOLING AND NOW                          
         BZ    LRX                                                              
         XC    CONSERV,CONSERV     AUTO $DQU                                    
         MVC   CONSERV(4),=C'$DQU'                                              
*                                                                               
LRX      B     XIT                                                              
         EJECT                                                                  
*               PROCESS SYSIO RECORDS                                           
*                                                                               
LRHOOK   NTR1                                                                   
*                                                                               
         CLI   TIMODE,PROCREC      PROCESS RECORDS                              
         BNE   LRHOOKX                                                          
*                                                                               
         USING LISTD,R2            R2=A(OUTPUT AREA)                            
         MVC   LSTMUSIC,TIMUSIC    MUSIC CODE                                   
*                                                                               
         MVC   SAVEAIO,AIO                                                      
         MVC   AIO,TIAREC          A(RECORD FROM SYSIO)                         
*                                                                               
         L     R3,AIO                                                           
         USING TANAD,R3                                                         
         MVI   ELCODE,TANAELQ      LOOK FOR COMPOSITION NAME ELEMENT            
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   WORK,SPACES                                                      
         ZIC   R1,TANALEN          ELEMENT LENGTH                               
         SH    R1,=Y(TANALNQ)      R1 = L'NAME                                  
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),TANANAME    COMPOSITION NAME                             
         MVC   LSTNAME,WORK                                                     
         DROP  R3                                                               
*                                                                               
         L     R3,AIO                                                           
         USING TAMUD,R3                                                         
         MVI   ELCODE,TAMUELQ      LOOK FOR PUBLISHER 1 ELEMENT                 
         GOTO1 GETL,DMCB,(1,=AL1(TAMUTP1))                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   WORK,SPACES                                                      
         L     R3,TGELEM                                                        
         ZIC   R1,TAMULEN          ELEMENT LENGTH                               
         SH    R1,=Y(TAMULNQ)      R1 = L'NAME                                  
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),TAMUNAME    PUBLISHER 1 NAME                             
         MVC   LSTPUB1,WORK                                                     
         MVC   LSTLICEN,TAMULIC    LICENSER                                     
         DROP  R3                                                               
*                                                                               
         MVC   AIO,SAVEAIO         RESTORE AIO                                  
         CLI   MODE,PRINTREP                                                    
         BNE   LRHOOK10                                                         
         GOTO1 SPOOL,DMCB,(R8)     PRINT REPORT                                 
         AP    COUNTER,=P'1'       INCREMENT COUNTER                            
         B     LRHOOKX                                                          
*                                                                               
LRHOOK10 CLI   LISTNUM,16          END OF 1 PAGE                                
         BNE   LRHOOK20                                                         
         MVC   MYMSGNO1,OKNO       MSG - HIT ENTER FOR NEXT                     
         MVI   MYMSYS,X'FF'                                                     
         OI    GENSTAT2,USGETTXT                                                
         LA    R2,SMUSELH                                                       
         GOTO1 EXIT,DMCB,0         AND DON'T COME BACK NO MORE, NO MORE         
*                                                                               
LRHOOK20 MVC   DMDSKADD,TIDSKADD   PASS DISK ADDRESS TO LISTMON                 
         GOTO1 LISTMON             CALL LISTMON                                 
*                                                                               
LRHOOKX  B     XIT                                                              
         EJECT                                                                  
*              EXITS, ETC.                                                      
         SPACE 2                                                                
XIT      XIT1                                                                   
         SPACE 2                                                                
         GETEL R3,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 2                                                                
PFTAB    DS    0C                                                               
*                                                                               
         DC    AL1(PF13X-*,13,0,(PF13X-PF13)/KEYLNQ,0)                          
         DC    CL3'   ',CL8'COM',CL8'LIST'                                      
PF13     DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYCUR,L'LSTMUSIC-1),AL2(LSTMUSIC-LISTD)                   
PF13X    EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              REPORT SPECS                                                     
         SPACE 2                                                                
MYSPECS  DS    0H                                                               
         SSPEC H1,2,RUN                                                         
         SSPEC H1,56,REQUESTOR                                                  
         SSPEC H2,56,REPORT                                                     
         SSPEC H2,73,PAGE                                                       
*                                                                               
         SSPEC H1,33,C'MUSIC LIST'                                              
         SSPEC H2,33,C'----------'                                              
*                                                                               
         SSPEC H4,2,C'MUSIC    COMPSITION NAME'                                 
         SSPEC H5,2,C'-----    ---------------'                                 
         SSPEC H4,43,C'PUBLISHER 1                     L'                       
         SSPEC H5,43,C'-----------                     -'                       
*                                                                               
         DC    X'00'                                                            
         EJECT                                                                  
*              DSECT TO COVER LIST LINE                                         
         SPACE 2                                                                
LISTD    DSECT                                                                  
LSTMUSIC DS    CL8                 MUSIC NUMBER                                 
         DS    C                                                                
LSTNAME  DS    CL31                COMPOSITION NAME                             
         DS    C                                                                
LSTPUB1  DS    CL31                PUBLISHER 1                                  
         DS    C                                                                
LSTLICEN DS    C                   PUBLISHER 1 LICENSER                         
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRA9D                                                       
         SPACE 3                                                                
SAVEAIO  DS    A                                                                
COUNTER  DS    PL4                 RECORD COUNTER                               
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
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003TAGENA9   05/01/02'                                      
         END                                                                    
