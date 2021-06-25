*          DATA SET TAGEN7C    AT LEVEL 003 AS OF 04/08/14                      
*PHASE T7027CA                                                                  
         TITLE 'T7027C - PRODUCT TYPE MAINTENANCE AND LIST'                     
T7027C   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T7027C                                                         
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
         CLI   ACTNUM,ACTLIST                                                   
         BE    PRT60                                                            
*                                                                               
         GOTO1 INITIAL,DMCB,PFTABLE          INITIALIZE                         
         SPACE 1                                                                
         CLI   MODE,VALKEY         FIRST TIME IN                                
         BE    VK                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   THISLSEL,C'D'       IF DELETING FROM LIST                        
         BE    PRT15                  DON'T DISPLAY FIRST                       
         CLI   MODE,DISPREC        IF MODE IS DISPLAY                           
         BE    PRT30                                                            
*                                                                               
PRT15    CLI   MODE,RECDEL         DELETE RECORD                                
         BE    PRT40                                                            
         CLI   MODE,RECREST        RESTORE RECORD                               
         BE    PRT40                                                            
         CLI   MODE,XRECDEL        RECORD DELETED                               
         BE    PRT20                                                            
         CLI   MODE,XRECREST       RECORD RESTORED                              
         BE    PRT20                                                            
         CLI   MODE,XRECADD        OR NEW RECORD ADDED                          
         BE    PRT20                                                            
         CLI   MODE,XRECPUT        OR RECORD CHANGED                            
         BNE   PRT50                                                            
*                                                                               
PRT20    GOTO1 ADDPTRS,DMCB,PTRBLK    ADD PASSIVE POINTERS                      
*                                                                               
PRT30    BAS   RE,DISPLAY          (RE-)DISPLAY THE RECORD                      
         B     PRTX                                                             
*                                                                               
PRT40    GOTO1 SAVPTRS,DMCB,PTRBLK HANDLE PASSIVE PONTERS                       
         CLI   MODE,RECDEL         IF DELETING                                  
         BE    CHKDEL              CHECK IF OK FOR DELETION                     
         B     PRTX                                                             
*                                                                               
PRT50    CLI   MODE,VALREC         VALIDATE THE RECORD                          
         BNE   PRTX                                                             
         BAS   RE,BLDREC                                                        
         B     PRTX                                                             
*                                                                               
*        LISTING RECORDS                                                        
*                                                                               
PRT60    GOTO1 INITIAL,DMCB,0                                                   
         CLI   MODE,VALKEY         FIRST TIME IN                                
         BE    LVK                                                              
         CLI   MODE,PRINTREP                                                    
         BNE   PRT70                                                            
         ZAP   COUNTER,=P'0'       CLEAR LINE COUNTER                           
         XC    KEY,KEY             CLEAR KEY FOR REPORT                         
         LA    R2,MYSPECS          SET UP SPECS FOR REPORT                      
         ST    R2,SPECS                                                         
         LA    R2,P                                                             
         B     LR                                                               
*                                                                               
PRT70    CLI   MODE,LISTRECS                                                    
         BNE   PRTX                                                             
         LA    R2,LISTAR                                                        
         MVC   LISTAR,SPACES       CLEAR PREVIOUS LINE                          
         B     LR                                                               
*                                                                               
PRTX     B     XIT                                                              
         EJECT                                                                  
*                                                                               
*              VALIDATE KEY                                                     
*                                                                               
VK       GOTO1 RECVAL,DMCB,TLAYCDQ,(X'08',SPTAGYH),SPTAGYNH                     
         GOTO1 RECVAL,DMCB,TLPTCDQ,(X'40',SPTPRTH)   BUILD THE KEY              
         B     XIT                                                              
         SPACE 3                                                                
*                                                                               
*              DISPLAY KEY                                                      
*                                                                               
DK       MVC   SVKEY,KEY           SAVE KEY                                     
         MVC   AIO,AIO2            AND SWITCH I/O AREAS                         
*                                                                               
         L     R3,AIO1             R3=A(PRODUCT TYPE RECORD)                    
         USING TLPTD,R3                                                         
*                                                                               
         MVC   SPTAGY,TLPTAGY      AGENCY                                       
         OI    SPTAGYH+6,X'80'                                                  
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'88',SPTAGY),SPTAGYNH                      
*                                                                               
         MVC   SPTPRT,TLPTPRT      PRODUCT TYPE                                 
         OI    SPTPRTH+6,X'80'                                                  
*                                                                               
         MVC   KEY,SVKEY           RESTORE KEY                                  
         MVC   AIO,AIO1            AND I/O AREA                                 
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
*              DISPLAY THE RECORD                                               
*                                                                               
         SPACE 1                                                                
DISPLAY  NTR1                                                                   
         TWAXC SPTNAMEH                                                         
         GOTO1 CHAROUT,DMCB,TANAELQ,SPTNAMEH NAME                               
         L     R4,AIO                                                           
         MVI   ELCODE,TAPTELQ      PTYPE DETAILS ELEMENT                        
         BAS   RE,GETEL                                                         
         BNE   DIS10                                                            
         USING TAPTD,R4                                                         
         LA    R2,SPTODTEH         GET OBSOLETE DATE                            
         GOTO1 DATCON,DMCB,(1,TAPTODTE),(8,8(R2))                               
         MVI   5(R2),8                                                          
DIS10    GOTO1 ACTVOUT,DMCB,SPTLCHGH         LAST CHANGED                       
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*              BUILD THE RECORD                                                 
*                                                                               
BLDREC   NTR1                                                                   
         GOTO1 SAVPTRS,DMCB,PTRBLK                                              
         GOTO1 NAMIN,DMCB,TANAELQ,SPTNAMEH                                      
*                                                                               
         MVI   ELCODE,TAPTELQ      REMOVE OLD PRD TYPE DETAILS ELEMENT          
         GOTO1 REMELEM                                                          
         LA    R2,SPTODTEH         OBSOLETE DATE - NOT REQUIRED                 
         CLI   5(R2),0                                                          
         BE    BLD10                                                            
         XC    ELEMENT,ELEMENT                                                  
         LA    R4,ELEMENT                                                       
         USING TAPTD,R4                                                         
         MVI   TAPTEL,TAPTELQ                                                   
         MVI   TAPTLEN,TAPTLNQ                                                  
         GOTO1 DTVAL,DMCB,TAPTODTE               VALIDATE DATE                  
         GOTO1 DATCON,DMCB,(1,TAPTODTE),(8,8(R2))                               
         GOTO1 ADDELEM                                                          
         DROP  R4                                                               
*                                                                               
BLD10    GOTO1 ACTVIN,DMCB,SPTLCHGH        LAST CHANGED                         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE CHECKS THAT THERE ARE NO PRODUCTS IN THIS                
*              PRODUCT TYPE BEFORE DELETING                                     
         SPACE                                                                  
         USING TLPTD,R3                                                         
         USING TLPRPD,R4                                                        
CHKDEL   DS    0H                                                               
         MVC   SVKEY,KEY           GENCON USES KEY TO DELETE ACTIVE PTR         
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         L     R3,AIO                                                           
         MVI   TLPRPCD,TLPRTCDQ    BUILD PASSIVE KEY FOR PRODUCT                
         MVC   TLPRTAGY,TLPTAGY    USING SAME AGENCY AS RECORD                  
         MVC   TLPRTPRT,TLPTPRT               PRODUCT TYPE                      
         NI    DMINBTS,X'F7'       DON'T READ DELETED                           
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(TLPRTPRT+L'TLPRTPRT-TLPRPKEY),KEYSAVE                        
         BE    CHKD15                                                           
*                                                                               
         MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY                                  
         B     XIT                                                              
*                                                                               
CHKD15   LA    R2,SPTPRTH          IF PRODUCTS EXIST WITH THIS PTYPE,           
         B     NODELETE            ERROR - CAN'T DELETE                         
         EJECT                                                                  
*                                                                               
* VALIDATE THE KEY FOR LIST                                                     
*                                                                               
LVK      LA    R2,LPTAGYH                                                       
         TM    4(R2),X'20'         VALIDATED                                    
         BO    LVK2                                                             
         NI    LPTSTRH+4,X'DF'     FORCE VALIDATION OF NEXT FIELD               
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLAYCDQ,(R2)   VALIDATE AGENCY                       
         MVC   AIO,AIO1                                                         
*                                                                               
LVK2     LA    R2,LPTSTRH          START AT?                                    
         TM    4(R2),X'20'         ALREADY VALIDATED                            
         BO    LVK5                                                             
         XC    TIQSTART,TIQSTART                                                
         NI    LPTFMTH+4,X'DF'     FORCE VALIDATION OF NEXT FIELD               
*                                                                               
         CLI   5(R2),0                                                          
         BE    LVK5                                                             
         ZIC   R3,5(R2)                                                         
         SH    R3,=H'1'                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   TIQSTART(0),8(R2)                                                
*                                                                               
LVK5     OI    4(R2),X'20'                                                      
         LA    R2,LPTFMTH          VALIDATE FORMAT                              
         TM    4(R2),X'20'         ALREADY VALIDATED                            
         BO    LVK10                                                            
         NI    LPTOPTSH+4,X'DF'    FORCE VALIDATION OF NEXT FIELD               
         MVI   RDSEQ,C'A'          DEFAULT TO ALPHA SEQUENCE                    
         CLI   5(R2),0                                                          
         BE    LVK10                                                            
         CLI   5(R2),1             ONLY 1 CHAR CAN BE INPUT                     
         BNE   INVERR                                                           
         CLI   8(R2),C'A'                                                       
         BE    LVK10                                                            
         MVI   RDSEQ,C'C'                                                       
         CLI   8(R2),C'C'          CODE SEQUENCE                                
         BNE   INVERR                                                           
*                                                                               
LVK10    OI    4(R2),X'20'                                                      
         LA    R2,LPTOPTSH         OPTIONS                                      
         TM    4(R2),X'20'         ALREADY VALIDATED                            
         BO    LVKX                                                             
*                                                                               
         CLI   5(R2),0                                                          
         BE    LVK60                                                            
         LA    R3,BLOCK            SET FOR SCANNER                              
         USING SCAND,R3                                                         
         GOTO1 SCANNER,DMCB,(R2),(R3)                                           
         CLI   4(R1),0             INVALID INPUT                                
         BE    INVERR                                                           
         ZIC   R0,4(R1)                                                         
*                                                                               
LVK20    DS    0H                  LEAVE ROOM FOR OPTIONS                       
*                                                                               
LVK50    LA    R3,SCANNEXT                                                      
         BCT   R0,LVK20                                                         
*                                                                               
LVK60    OI    4(R2),X'20'                                                      
         XC    KEY,KEY            DEFAULT TO EQUAL ZERO                         
         BAS   RE,INIT                                                          
*                                                                               
LVKX     B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        SET UP SYSIO                                                           
*                                                                               
INIT     NTR1                                                                   
         MVC   TIUSERID,TWAORIG    SET UP NECCESSARY DATA FROM                  
         MVC   TIQSTAFF,TGCTSTAF   GLOBAL STORAGE                               
         MVI   TIREAD,TLPTNCDQ     LIST BY PRODUCT TYPE NAME                    
         MVC   TIFAGY,TGAGY        FILTER ON AGENCY                             
*                                                                               
         CLI   RDSEQ,C'A'                                                       
         BE    INITX                                                            
         MVI   TIREAD,TLPTCDQ      LIST BY PRD TYPE CODE                        
*                                                                               
INITX    B     XIT                                                              
         EJECT                                                                  
*                                                                               
LR       LA    R0,LRHOOK           SET HOOK TO SYSIO                            
         ST    R0,TIHOOK                                                        
         MVC   TIACOMFC,ACOMFACS                                                
         MVC   TIKHOOK,SETLSTK                                                  
         MVI   NLISTS,15           IN ORDER TO GET CONTROL BACK                 
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
         MVI   NLISTS,14              IF LIST EXACTLY = 1 PAGE                  
         CLI   MODE,PRINTREP                                                    
         BNE   LRX                                                              
         EDIT  COUNTER,(8,P+1),COMMAS=YES,ALIGN=LEFT                            
         LR    R1,R0                                                            
         LA    R1,P+2(R1)                                                       
         MVC   0(20,R1),=CL20'PRODUCT TYPE RECORDS'                             
         GOTO1 SPOOL,DMCB,(R8)                                                  
         TM    WHEN,X'40'          IF SPOOLING & NOW                            
         BZ    LRX                                                              
         XC    CONSERV,CONSERV     GO STRAIGHT TO $DQU                          
         MVC   CONSERV(4),=C'$DQU'                                              
*                                                                               
LRX      MVI   NLISTS,15                                                        
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        PROCESS SYSIO RECORDS                                                  
*                                                                               
LRHOOK   NTR1                                                                   
         CLI   TIMODE,PROCREC      PROCESS RECORDS                              
         BE    PRREC                                                            
*                                                                               
LRHX     B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        PROCESS RECORD                                                         
*                                                                               
PRREC    DS    0H                                                               
         USING LISTD,R2                                                         
*                                                                               
PR10     MVC   LISPRT,TIPRT        PRODUCT TYPE CODE                            
         MVC   PTNAME,SPACES       CLEAR PTYPE NAME                             
         MVI   PTNAMEH,44                                                       
         MVC   AIO,TIAREC                                                       
         GOTO1 CHAROUT,DMCB,TANAELQ,PTNAMEH NAME                                
         MVC   AIO,AIO1                                                         
         MVC   LISNAME,PTNAME                                                   
*                                                                               
         XC    LISODTE,LISODTE                                                  
         L     R4,TIAREC                                                        
         MVI   ELCODE,TAPTELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   PR15                                                             
         USING TAPTD,R4                                                         
         GOTO1 DATCON,DMCB,(1,TAPTODTE),(8,LISODTE)    OBSOLETE DATE            
         DROP  R4                                                               
*                                                                               
PR15     MVC   DMDSKADD,TIDSKADD   PASS DISK ADDRESS TO LISTMON                 
         CLI   MODE,PRINTREP                                                    
         BNE   PR20                                                             
         GOTO1 CATCHIOS            ENSURE NOT TOO MANY IOS                      
         GOTO1 SPOOL,DMCB,(R8)     PRINT REPORT                                 
         AP    COUNTER,=P'1'                                                    
         B     PRX                                                              
*                                                                               
PR20     CLI   LISTNUM,15          IF ALREADY FILLED PAGE                       
         BNE   PR30                                                             
         MVC   MYMSGNO1,OKNO       HIT ENTER FOR NEXT - MSG                     
         MVI   MYMSYS,X'FF'                                                     
         OI    GENSTAT2,USGETTXT                                                
         LA    R2,LPTSELH                                                       
         B     ERRXIT                                                           
*                                                                               
PR30     GOTO1 LISTMON             CALL LISTMON                                 
*                                                                               
PRX      B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
XIT      XIT1                                                                   
*                                                                               
         SPACE 2                                                                
INVERR   MVI   ERROR,INVALID                                                    
         B     ERRXIT                                                           
         SPACE 2                                                                
MISSERR  MVI   ERROR,MISSING                                                    
         B     ERRXIT                                                           
         SPACE 2                                                                
NODELETE MVI   ERROR,ERINVDEL      RECORD NOT AVAILABLE FOR DELETION            
         B     ERRXIT                                                           
         SPACE 2                                                                
*                                                                               
ERRXIT   XC    DMCB,DMCB                                                        
         GOTO1 EXIT,DMCB                                                        
*                                                                               
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 2                                                                
PFTABLE  DS    0C                  PF KEYS TABLE                                
*                                                                               
         DC    AL1(PF13X-*,13,0,0,0)                                            
         DC    CL3' ',CL8'PGROUP  ',CL8'LIST'                                   
PF13X    EQU   *                                                                
*                                                                               
         DC    AL1(PF14X-*,14,0,(PF14X-PF14)/KEYLNQ,0)                          
         DC    CL3' ',CL8'PRODUCT ',CL8'LIST'                                   
PF14     DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYGLB,L'TGCLI-1),AL2(TGCLI-TGD)                           
         DC    AL1(KEYTYGLB,L'TGPRG-1),AL2(TGPRG-TGD)                           
PF14X    EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*        SPECS FOR SPOOLING                                                     
*                                                                               
MYSPECS  DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,56,REQUESTOR                                                  
         SSPEC H2,56,REPORT                                                     
         SSPEC H2,73,PAGE                                                       
         SPACE 1                                                                
         SSPEC H1,32,C'PRODUCT TYPE LIST'                                       
         SSPEC H2,32,C'-----------------'                                       
         SPACE 1                                                                
         SSPEC H4,2,C'PTYPE   DESCRIPTION'                                      
         SSPEC H4,48,C'OBSOLETE DATE'                                           
         SSPEC H5,2,C'-----   -----------'                                      
         SSPEC H5,48,C'-------- ----'                                           
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
LISTD    DSECT                                                                  
         DS    CL1                                                              
LISPRT   DS    CL6                                                              
         DS    CL2                                                              
LISNAME  DS    CL36                                                             
         DS    CL2                                                              
LISODTE  DS    CL8                                                              
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR7CD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRF9D                                                       
         EJECT                                                                  
*                                                                               
         ORG   LPTWORK                                                          
*                                                                               
SVKEY    DS    CL38                SAVE THE KEY                                 
COUNTER  DS    PL4                 COUNT OUTPUT LINES                           
RDSEQ    DS    CL1                 READ WITH ACTIVE/PASSIVE POINTER             
PTNAMEH  DS    CL8                 PTYPE NAME HEADER                            
PTNAME   DS    CL36                PTYPE NAME                                   
         DS    0D                                                               
PTRBLK   DS    CL(2*L'TLDRREC+1)   1 ACTIVE & 1 PASSIVE                         
         EJECT                                                                  
       ++INCLUDE TASYSIOD                                                       
         EJECT                                                                  
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSEQUS                                                                     
* TASYSDSECT                                                                    
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003TAGEN7C   04/08/14'                                      
         END                                                                    
