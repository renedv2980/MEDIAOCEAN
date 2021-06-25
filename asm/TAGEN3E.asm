*          DATA SET TAGEN3E    AT LEVEL 038 AS OF 04/08/14                      
*PHASE T7023EA                                                                  
         TITLE 'T7023E - AGENT LIST'                                            
T7023E   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T7023E                                                         
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         LA    R7,TWAHOLE                                                       
         USING TWAHD,R7                                                         
         EJECT                                                                  
*                                                                               
*              MODE CONTROLLED ROUTINES                                         
*                                                                               
AGE10    GOTO1 INITIAL,DMCB,PFTAB                                               
         CLI   MODE,VALKEY         FIRST TIME IN                                
         BE    VK                                                               
*                                                                               
AGE20    CLI   MODE,LISTRECS                                                    
         BNE   AGE30                                                            
         MVI   NLISTS,15           SET N'LIST LINES                             
         OI    GLSTSTAT,RETEXTRA   GET CONTROL BACK FOR EOL                     
         MVC   LISTAR,SPACES       CLEAR PREVIOUS LINE                          
         LA    R2,LISTAR                                                        
         B     LR                                                               
*                                                                               
AGE30    CLI   MODE,PRINTREP                                                    
         BNE   AGEX                                                             
         XC    TIKEY,TIKEY         START REPORT AT BEGIN OF LIST                
         ZAP   COUNTER,=P'0'                                                    
         LA    R2,MYSPECS                                                       
         ST    R2,SPECS                                                         
         LA    R2,P                                                             
         B     LR                                                               
*                                                                               
AGEX     B     XIT                                                              
*                                                                               
         SPACE 2                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*                                                                               
* VALIDATE THE KEY                                                              
*                                                                               
VK       LA    R2,SANSTRH          START AT SPECIFIC AGENT                      
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED                         
         BO    VK5                                                              
         NI    SANFMTH+4,X'DF'     FORCE VALIDATION OF NEXT FIELD               
         XC    TIQSTART,TIQSTART                                                
         CLI   5(R2),0                                                          
         BE    VK5                                                              
         ZIC   R3,5(R2)                                                         
         BCTR  R3,0                SUBTRACT 1                                   
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   TIQSTART(0),8(R2)                                                
*                                                                               
VK5      OI    4(R2),X'20'         VALIDATED                                    
         LA    R2,SANFMTH                                                       
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED                         
         BO    VK9                                                              
         NI    SANOPTSH+4,X'DF'    FORCE VALIDATION OF NEXT FIELD               
         MVI   RDSEQ,C'A'          DEFAULT TO NAME SEQUENCE                     
         CLI   5(R2),0                                                          
         BE    VK9                                                              
         CLI   5(R2),1             ONLY 1 CHAR CAN BE INPUT                     
         BNE   INVERR                                                           
         CLI   8(R2),C'A'                                                       
         BE    VK9                                                              
         MVI   RDSEQ,C'C'                                                       
         CLI   8(R2),C'C'          NUMBER SEQUENCE                              
         BNE   INVERR                                                           
*                                                                               
VK9      OI    4(R2),X'20'         VALIDATED                                    
         LA    R2,SANOPTSH         OPTIONS                                      
         TM    4(R2),X'20'                                                      
         BO    VKX                                                              
         MVI   PRTLBL,C'N'         DEFAULT NOT TO PRINT LABLES                  
         MVI   ADDRF,C'N'          ADDRESSES Y/N                                
*                                                                               
         CLI   5(R2),0                                                          
         BE    VK60                                                             
         LA    R3,BLOCK            SET FOR SCANNER                              
         USING SCAND,R3                                                         
         GOTO1 SCANNER,DMCB,(R2),(R3)                                           
         CLI   4(R1),0             INVALID INPUT                                
         BE    INVERR                                                           
         ZIC   R0,4(R1)                                                         
*                                                                               
VK20     CLI   SCLEN1,1            MAX OF 1 CHAR INPUT                          
         BNE   INVERR                                                           
         CLI   SCDATA1,C'A'        ADDRESSES                                    
         BNE   VK40                                                             
         CLI   SCDATA2,C'N'                                                     
         BE    VK40                                                             
         MVI   ADDRF,C'Y'                                                       
         CLI   SCDATA2,C'Y'                                                     
         BE    VK50                                                             
         B     INVERR                                                           
*                                                                               
VK40     CLI   SCDATA1,C'L'        PRINT LABLES                                 
         BNE   INVERR                                                           
         CLI   SCDATA2,C'Y'                                                     
         BNE   INVERR                                                           
         MVI   PRTLBL,C'Y'                                                      
*                                                                               
VK50     LA    R3,SCANNEXT                                                      
         BCT   R0,VK20                                                          
*                                                                               
VK60     OI    4(R2),X'20'         VALIDATED                                    
         BAS   RE,INIT                                                          
         MVI   LISTSW,C'F'         START LIST FROM BEGINING                     
*                                                                               
VKX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        SET UP SYSIO                                                           
*                                                                               
INIT     NTR1                                                                   
*                                                                               
         MVC   TIUSERID,TWAORIG    SET UP NECCESSARY DATA FROM                  
         MVC   TIQSTAFF,TGCTSTAF       GLOBAL STORAGE                           
         MVI   TIREAD,TLANNCDQ     LIST BY AGENT NAME                           
*                                                                               
         CLI   RDSEQ,C'A'                                                       
         BE    INITX                                                            
         MVI   TIREAD,TLANCCDQ     LIST BY AGENT NUMBER                         
*                                                                               
INITX    B     XIT                                                              
*                                                                               
         SPACE 5                                                                
LR       LA    R0,LRHOOK           SET HOOK TO SYSIO                            
         ST    R0,TIHOOK                                                        
         MVC   TIACOMFC,ACOMFACS                                                
         GOTO1 PGCNTL,DMCB,TABLE,TIKEY,TIQSKEY                                  
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
         CLI   MODE,PRINTREP                                                    
         BNE   LRX                                                              
         EDIT  COUNTER,(8,P),COMMAS=YES,ALIGN=LEFT                              
         LR    R1,R0                                                            
         LA    R1,P+1(R1)                                                       
         MVC   0(13,R1),=C'AGENT RECORDS'                                       
         GOTO1 SPOOL,DMCB,(R8)                                                  
         TM    WHEN,X'40'          IF SPOOLING AND NOW                          
         BZ    LRX                                                              
         XC    CONSERV,CONSERV     AUTO $DQU                                    
         MVC   CONSERV(4),=C'$DQU'                                              
*                                                                               
*                                                                               
LRX      B     XIT                                                              
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
         MVC   ANNUM,TIAGT         AGENT NUMBER                                 
         CLC   TISHORT,SPACES                                                   
         BE    PRR10                                                            
         MVC   ANALPHA,TISHORT     CLIENT ALPHA CODE = 1ST 12 BYTES             
         B     PRR20                                                            
*                                                                               
PRR10    MVC   ANALPHA,TINAME                                                   
*                                                                               
PRR20    MVC   ANNAME,TINAME       AGENT NAME                                   
*                                                                               
         L     R4,TIAREC                                                        
         USING TAAND,R4                                                         
         MVC   ANTEL,SPACES                                                     
         MVI   ELCODE,TAANELQ      POINT TO AGENT ELEMENT                       
         BAS   RE,GETEL                                                         
         BNE   PRR30                                                            
         OC    TAANTEL,TAANTEL     IS TELEPHONE GIVEN                           
         BZ    PRR30                                                            
         MVI   ANTEL,C'('                                                       
         MVC   ANTEL+1(3),TAANTEL  AREA CODE                                    
         MVI   ANTEL+4,C')'                                                     
         MVC   ANTEL+5(3),TAANTEL+3                                             
         MVI   ANTEL+8,C'-'                                                     
         MVC   ANTEL+9(4),TAANTEL+6                                             
*                                                                               
PRR30    MVC   DMDSKADD,TIDSKADD   PASS DISK ADDRESS TO LISTMON                 
         CLI   MODE,PRINTREP                                                    
         BNE   PRR40                                                            
         GOTO1 CATCHIOS                                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
         AP    COUNTER,=P'1'                                                    
         B     PRRX                                                             
*                                                                               
PRR40    GOTO1 LISTMON             CALL LISTMON                                 
*                                                                               
PRRX     B     XIT                                                              
         DROP  R4,R2                                                            
         EJECT                                                                  
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
         B     ERRXIT                                                           
*                                                                               
ERRXIT   XC    DMCB,DMCB                                                        
         GOTO1 EXIT,DMCB                                                        
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 2                                                                
PFTAB    DS    0C                                                               
*                                                                               
         DC    AL1(PF13X-*,13,0,(PF13X-PF13)/KEYLNQ,0)                          
         DC    CL3'CAG',CL8'CAGENT  ',CL8'LIST    '                             
PF13     DC    AL1(KEYTYCUR,L'ANNUM-1),AL2(ANNUM-LISTD)                         
PF13X    EQU   *                                                                
         DC    X'FF'                                                            
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
MYSPECS  DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,56,REQUESTOR                                                  
         SSPEC H2,56,REPORT                                                     
         SSPEC H2,73,PAGE                                                       
         SPACE 1                                                                
         SSPEC H1,32,C'AGENT LIST'                                              
         SSPEC H2,32,C'----------'                                              
         SPACE 1                                                                
         SSPEC H4,1,C'ALPHA'                                                    
         SSPEC H4,15,C'NUMERIC'                                                 
         SSPEC H4,24,C'AGENT NAME'                                              
         SSPEC H4,62,C'TELEPHONE'                                               
         SPACE 1                                                                
         SSPEC H5,1,C'-----'                                                    
         SSPEC H5,15,C'-------'                                                 
         SSPEC H5,24,C'----------'                                              
         SSPEC H5,62,C'---------'                                               
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
*                                                                               
LISTD    DSECT                                                                  
ANALPHA  DS    CL12                                                             
         DS    CL2                                                              
ANNUM    DS    CL4                                                              
         DS    CL5                                                              
ANNAME   DS    CL36                                                             
         DS    CL2                                                              
ANTEL    DS    CL13                                                             
*                                                                               
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR3ED                                                       
         EJECT                                                                  
*                                                                               
COUNTER  DS    PL4                 LINE COUNTER                                 
RDSEQ    DS    CL1                 READ WITH ACTIVE/PASSIVE POINTER             
PRTLBL   DS    CL1                 PRINT LABELS                                 
ADDRF    DS    CL1                 ADDRESS Y/N                                  
         SPACE 1                                                                
       ++INCLUDE TASYSIOD                                                       
         EJECT                                                                  
TWAHD    DSECT                                                                  
TABLE    DS    16CL(L'TLRCKEY)     TABLE TO HOLD 16 KEYS FOR PAGING             
         SPACE 3                                                                
*                                                                               
* TASYSDSECT                                                                    
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'038TAGEN3E   04/08/14'                                      
         END                                                                    
