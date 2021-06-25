*          DATA SET NESFM16    AT LEVEL 003 AS OF 05/01/02                      
*PHASE T31C16A,*                                                                
*INCLUDE DECODE                                                                 
***********************************************************************         
*                                                                               
*  TITLE: T31C01 - MAINTENANCE/LIST OF UNIT BILL RECORDS                        
*                                                                               
*  INPUTS: SCREENS NESFMEF (T31CEF) -- MAINTENANCE                              
*                                   -- LIST                                     
*                                                                               
*  OUTPUTS: UPDATED OR NEW CLIENTS                                              
*                                                                               
*  LOCALS: REGISTER USAGE                                                       
*          R0 - WORK                                                            
*          R1 - WORK                                                            
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CURSOR              
*          R3 - WORK                                                            
*          R4 - WORK                                                            
*          R5 - WORK                                                            
*          R6 - USED FOR GETEL ELEMENT DSECT POINTER                            
*          R7 - WORK                                                            
*          R8 - SPOOLD                                                          
*          R9 - SYSD                                                            
*          RA - TWA                                                             
*          RB - FIRST BASE                                                      
*          RC - GEND                                                            
*          RD - SYSTEM                                                          
*          RE - SYSTEM                                                          
*          RF - SYSTEM                                                          
*                                                                               
***********************************************************************         
         TITLE 'T31C16 NETWORK UNIT BILLING RECORD'                             
T31C16   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,NEUBIL,R7,RR=R2                                                
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         ST    R2,RELO                                                          
*                                                                               
         MVI   IOOPT,C'Y'          CONTROL MY OWN ADDREC/PUTREC                 
*                                                                               
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
***      CLI   MODE,RECDEL         RECORD DELETE                                
***      BE    RDEL                                                             
***      CLI   MODE,XRECDEL        AFTER RECORD HAS BEEN DELETED                
***      BE    XD                                                               
***      CLI   MODE,XRECREST       AFTER RECORD HAS BEEN RESTORED               
***      BE    XR                                                               
***      CLI   MODE,XRECADD        AFTER RECORD HAS BEEN ADDED                  
***      BE    XA                                                               
***      CLI   MODE,LISTRECS       LIST RECORDS                                 
***      BE    LR                                                               
***      CLI   MODE,PRINTREP       PRINT RECORDS                                
***      BE    LR                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE KEY                                                                  
***********************************************************************         
VK       DS    0H                                                               
         LA    R3,SVKEY                                                         
         USING NUBKEY,R3                                                        
         XC    SVKEY,SVKEY                                                      
         GOTO1 =V(DECODE),DMCB,UBLUBK,SVKEY,RR=RELO                             
         LA    R2,SVCLIST                                                       
         MVC   0(32,R2),SVKEY                                                   
         GOTO1 DATAMGR,DMCB,(0,=CL8'DMRDHI'),=C'XSPDIR',(R3),(R3),0             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   0(32,R3),0(R2)      KEY MATCH                                    
         BNE   NOSUCH                                                           
         MVC   UBLUBR,UBLUBK                                                    
         OI    UBLUBRH+6,X'80'                                                  
VKX      B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY KEY                                                                   
***********************************************************************         
DK       DS    0H                                                               
DKX      B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE RECORD                                                               
* AIO = OLD REC TO BE DELETED AND ADDED WITH NEW KEY                            
* SVKEY = KEY OF OLD REC +DISKADDR                                              
* UBLUBK WILL HAVE BEEN CHANGED TO NEW DESIRED KEY                              
***********************************************************************         
VR       DS    0H                                                               
* CHECK IF NEW DESIRED KEY ALREADY EXISTS?                                      
         XC    KEY2,KEY2                                                        
         GOTO1 =V(DECODE),DMCB,UBLUBR,KEY2,RR=RELO                              
         MVC   KEY2SV,KEY2                                                      
         LA    R2,KEY2                                                          
         GOTO1 DATAMGR,DMCB,(0,=CL8'DMRDHI'),=C'XSPDIR',(R2),(R2),0             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY2(32),KEY2SV     IS THERE ALREADY SUCH A KEY?                 
         BNE   VR20                NO/OK                                        
         LA    R2,UBLUBRH         YES/GIVE ALREADY EXISTS ERROR                 
         B     EXISTS                                                           
*                                                                               
VR20     EQU   *                                                                
         LA    R3,SVKEY             GET OLD RECORD                              
         USING NUBKEY,R3                                                        
         LA    R2,SVCLIST                                                       
         MVC   0(32,R2),SVKEY                                                   
         GOTO1 DATAMGR,DMCB,(0,=CL8'DMRDHI'),=C'XSPDIR',(R3),(R3),0             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   0(32,R3),0(R2)      KEY MATCH                                    
         BE    *+6                                                              
         DC    H'0'                SHOULD NEVER GET HERE                        
         LA    R2,IO               USE IO+2000                                  
         LA    R2,2000(R2)                                                      
         ST    R2,AIO                                                           
         LA    R3,NUBDA                                                         
         MVC   WORK(8),=C'XSPFIL  '                                             
         LA    R4,WORK                                                          
         GOTO1 DATAMGR,DMCB,(X'80',=CL8'GETREC'),(R4),(R3),(R2),DMWORK          
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R3,SVKEY             DELETE OLD KEY                              
         OI    NUBKSTAT,X'80'             TURN ON DELETE                        
         GOTO1 DATAMGR,DMCB,(0,=CL8'DMWRT'),=C'XSPDIR',(R3),(R3),0              
         DROP  R3                                                               
                                                                                
*                                   DELETE REC                                  
         L     R2,AIO                                                           
         USING NUBRECD,R2                                                       
         OI    NUBSTAT,X'80'             TURN ON DELETE                         
         DROP  R2                                                               
         LA    R3,SVKEY                                                         
         USING NUBRECD,R3                                                       
         LA    R4,NUBDA                                                         
         GOTO1 DATAMGR,DMCB,=CL8'PUTREC',=C'XSPFIL',(R4),(R2),DMWORK            
         DROP  R3                                                               
*                                   ADD OLD REC WITH NEW KEY                    
         L     R2,AIO                                                           
         USING NUBRECD,R2                                                       
         NI    NUBSTAT,X'FF'-X'80'             TURN OFF DELETE                  
         MVC   0(32,R2),KEY2SV      NEW KEY                                     
         LA    R3,SVKEY                                                         
         GOTO1 DATAMGR,DMCB,=CL8'ADDREC',=C'XSPFIL',(R3),(R2),DMWORK            
         DROP  R2                                                               
VRX      B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY RECORD                                                                
* DOES NOT GET HERE                                                             
***********************************************************************         
DR       DS    0H                                                               
         B     EXIT                **************                               
         LA    R3,SVKEY                                                         
         USING NUBKEY,R3                                                        
         LA    RF,DATAMGR                                                       
         LA    R2,SVCLIST                                                       
         GOTO1 (RF),DMCB,(0,=CL8'DMRDHI'),=C'XSPDIR',(R3),(R2),0                
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   0(32,R3),0(R2)                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R3,NUBDA                                                         
         L     R2,AIO                                                           
         GOTO1 DATAMGR,DMCB,=CL8'GETREC',=C'XSPFIL  ',(R3),(R2),DMWORK          
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   UBLUBR(50),0(R2)                                                 
         OI    UBLUBRH+6,X'80'                TRANSMIT                          
         B     EXIT                                                             
         DROP  R3                                                               
*                                                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
* FIND NEXT UNPROTECTED FIELD                                                   
***********************************************************************         
FNDNXUF  ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BNE   FNDUF                                                            
         DC    H'0'           END OF SCREEN                                     
FNDUF    TM    1(R2),X'20'    FIND NEXT UNPROTECTED FIELD                       
         BO    FNDNXUF                                                          
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DATAMGR INTERFACE                                                             
***********************************************************************         
MYFILADD NTR1                                                                   
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'SPTFILE ',KEY+14,AIO,MYDMWRK           
         BAS   RE,DMCHECK                                                       
         B     YES                                                              
*                                                                               
MYFILWRT NTR1                                                                   
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'SPTFILE ',KEY+14,AIO,MYDMWRK           
         BAS   RE,DMCHECK                                                       
         B     YES                                                              
*                                                                               
MYDIRWRT NTR1                                                                   
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'SPTDIR  ',KEY,KEY                      
         BAS   RE,DMCHECK                                                       
         B     YES                                                              
*                                                                               
MYDIRADD NTR1                                                                   
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'SPTDIR  ',KEY,KEY                      
         BAS   RE,DMCHECK                                                       
         B     YES                                                              
*                                                                               
DMCHECK  CLI   8(R1),0                                                          
         BER   RE                                                               
         TM    8(R1),X'90'                                                      
         BM    NO                                                               
         DC    H'0'                                                             
         SPACE 1                                                                
YES      SR    R1,R1                                                            
         B     *+8                                                              
NO       LA    R1,1                                                             
         LTR   R1,R1                                                            
         SPACE 1                                                                
XIT      XIT1  REGS=(R0,R1)                                                     
         EJECT                                                                  
***********************************************************************         
* GENERATE REQUEST RECORD                                                       
***********************************************************************         
*REQREC   XC    REC(150),REC                                                    
*        LA    R1,REC                                                           
*        MVI   10(R1),41                                                        
*        MVI   14(R1),106                                                       
*        LA    R1,REC+26                                                        
*        MVI   0(R1),X'40'                                                      
*        MVC   1(79,R1),0(R1)                                                   
*        MVC   0(2,R1),=C'41'                                                   
*        MVC   2(2,R1),14(RA)                                                   
*        MVC   4(1,R1),CLTMED                                                   
*        MVC   5(3,R1),CLTCLT                                                   
*        OC    5(3,R1),SPACES                                                   
*        MVC   68(7,R1),=C'CONTROL'                                             
*        MVI   61(R1),C'C'                                                      
*        MVI   63(R1),C'A'                                                      
*        CLI   ACTNUM,ACTADD                                                    
*        BE    *+8                                                              
*        MVI   63(R1),C'C'                                                      
*        GOTO1 DATAMGR,DMCB,=C'DMADD',=C'REQUEST',REC,REC                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* HEADER AND HEAD HOOK ROUTINES                                                 
***********************************************************************         
HEADING  DS    0H                                                               
         SSPEC H1,3,REQUESTOR                                                   
         SSPEC H3,3,C'MEDIA N'                                                  
         SSPEC H1,46,C'NETWORK CLIENT RECORDS'                                  
         SSPEC H2,46,C'----------------------'                                  
         SSPEC H1,93,AGYNAME                                                    
         SSPEC H2,93,AGYADD                                                     
         SSPEC H3,93,REPORT                                                     
         SSPEC H4,93,RUN                                                        
         SSPEC H5,103,PAGE                                                      
         DC    X'00'                                                            
*                                                                               
HDRTN    NTR1                                                                   
         LA    R2,H8                                                            
         USING PLINED,R2                                                        
         MVC   PCLT(3),=C'CLT'                                                  
         MVC   PCLT+132(3),=50C'-'                                              
         MVC   POFFICES-1(3),=C'OF#'                                            
         MVC   POFFICES-1+132(3),=50C'-'                                        
         MVC   PACCOFF-1(4),=C'AOF#'                                            
         MVC   PACCOFF-1+132(4),=50C'-'                                         
         MVC   PINTER(8),=C'INT CODE'                                           
         MVC   PINTER+132(8),=50C'-'                                            
         MVC   PTITLE+1(8),=C'ID TITLE'                                         
         MVC   PTITLE+132(10),=50C'-'                                           
         MVC   PNME+4(11),=C'CLIENT NAME'                                       
         MVC   PNME+132(20),=50C'-'                                             
         MVC   PBILL(8),=C'BILL EST'                                            
         MVC   PBILL+132(8),=50C'-'                                             
         MVC   PCLTCDE(7),=C'PRT CLT'                                           
         MVC   PCLTCDE+132(7),=50C'-'                                           
         MVC   PCLTRTE(7),=C'CLT RTE'                                           
         MVC   PCLTRTE+132(7),=50C'-'                                           
         MVC   PESTFLR(8),=C'EST FTRS'                                          
         MVC   PESTFLR+132(8),=50C'-'                                           
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ERROR MESSAGES                                                                
***********************************************************************         
INVLFLD  MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
MISSFLD  MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
INVLCLI  MVI   ERROR,INVCLI                                                     
         B     TRAPERR                                                          
*                                                                               
INVLACT  MVI   ERROR,INVACT                                                     
         LA    R2,CONACTH                                                       
         B     TRAPERR                                                          
*                                                                               
EXISTS   DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(21),=C'RECORD ALREADY EXISTS'                            
         B     MYERR                                                            
NOSUCH   DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(18),=C'NO SUCH KEY EXISTS'                               
         B     MYERR                                                            
*                                                                               
*                                                                               
*                                                                               
MYERR    GOTO1 ERREX2                                                           
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
PLINED   DSECT                                                                  
PCLT     DS    CL3                                                              
         DS    CL2                                                              
POFFICES DS    CL1                                                              
         DS    CL3                                                              
PACCOFF  DS    CL2                                                              
         DS    CL2                                                              
PINTER   DS    CL8                                                              
         DS    CL1                                                              
PTITLE   DS    CL10                                                             
         DS    CL1                                                              
PNME     DS    CL20                                                             
         DS    CL1                                                              
PBILL    DS    CL8                                                              
         DS    CL1                                                              
PCLTCDE  DS    CL7                                                              
         DS    CL1                                                              
PCLTRTE  DS    CL7                                                              
         DS    CL1                                                              
PESTFLR  DS    CL8                                                              
PLENGTH  EQU   *-PCLT                                                           
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE NEGENUBILL                                                     
         EJECT                                                                  
       ++INCLUDE NESFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMEF2D                                                      
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMC2D                                                       
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE NESFMWORKD                                                     
         ORG   SYSSPARE                                                         
*                           *******  T31C01 WORK AREA  *******                  
WORKAREA DS    0CL1                                                             
MYDMWRK  DS    12D                                                              
RELO     DS    F                                                                
KEY2     DS    CL48                                                             
KEY2SV   DS    CL48                FOR TURNAROUND REPORT                        
*                                                                               
PREVCLT  DS    XL2                                                              
*                                                                               
SYSSW    DS    CL1                                                              
SWDSYS   DS    CL1                                                              
POWCODE  DS    CL2                                                              
ACCOFF   DS    CL2                                                              
OFFLEN   DS    CL1                                                              
COMPCD   DS    CL1                                                              
OLDCOPT2 DS    CL1                                                              
GTFACTB  DS    CL88                                                             
WORKEND  EQU   *                                                                
         EJECT                                                                  
*                                                                               
SCAND    DSECT                                                                  
*         DSECT TO COVER SCANNER LINES                                          
FLD1LEN  DS    CL1                                                              
FLD2LEN  DS    CL1                                                              
FLD1VAL  DS    CL1                                                              
FLD2VAL  DS    CL1                                                              
FLD1B    DS    CL4                                                              
FLD2B    DS    CL4                                                              
FLD1     DS    CL10                                                             
FLD2     DS    CL10                                                             
*                                                                               
         PRINT GEN                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003NESFM16   05/01/02'                                      
         END                                                                    
