*          DATA SET TAGEN7E    AT LEVEL 002 AS OF 07/21/08                      
*PHASE T7027EA                                                                  
         TITLE 'T7027E - COMMERCIAL POOL LIST'                                  
T7027E   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T7027E                                                         
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         EJECT                                                                  
*                                                                               
         GOTO1 INITIAL,DMCB,PFTABLE                                             
*                                                                               
         CLI   MODE,VALKEY         FIRST TIME IN                                
         BE    VK                                                               
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BNE   PGR10                                                            
         ZAP   COUNTER,=P'0'       CLEAR LINE COUNTER                           
         XC    KEY,KEY             CLEAR KEY FOR REPORT                         
         LA    R2,MYSPECS          SET UP SPECS FOR REPORT                      
         ST    R2,SPECS                                                         
         LA    R2,P                                                             
         B     LR                                                               
*                                                                               
PGR10    CLI   MODE,LISTRECS                                                    
         BNE   XIT                                                              
         LA    R2,LISTAR                                                        
         MVC   LISTAR,SPACES       CLEAR PREVIOUS LINE                          
         B     LR                                                               
*                                                                               
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
*              ROUTINE TO VALIDATE KEY FIELDS                                   
         SPACE 2                                                                
VK       GOTO1 FLDVAL,DMCB,(X'40',CGLAGYH),(X'80',CGLFMTH)                      
         BE    VKX                                                              
         SPACE                                                                  
         LA    R2,CGLAGYH          VALIDATE AGENCY                              
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLAYCDQ,(R2)                                         
         MVC   AIO,AIO1                                                         
         SPACE                                                                  
VK10     LA    R2,CGLCLIH          VALIDATE CLIENT                              
         XC    TGCLI,TGCLI                                                      
         CLI   5(R2),0                                                          
         BE    VK20                                                             
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLCLCDQ,(R2)                                         
         MVC   AIO,AIO1                                                         
         SPACE                                                                  
VK20     LA    R2,CGLPRDH          VALIDATE PRODUCT                             
         XC    TGPRD,TGPRD                                                      
         CLI   5(R2),0                                                          
         BE    VK30                                                             
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLPRCDQ,(R2)                                         
         MVC   AIO,AIO1                                                         
         SPACE                                                                  
VK30     LA    R2,CGLFMTH          VALIDATE FORMAT                              
         MVI   RDSEQ,C'A'                                                       
         CLI   5(R2),0             DEFAULT FORMAT TO LISTING                    
         BE    VK40                RECORDS ALPHABETICALLY BY                    
         CLI   8(R2),C'A'          COMMERCIAL POOL NAME                         
         BE    VK40                                                             
         MVI   RDSEQ,C'C'          IF FORMAT INPUT IS C                         
         CLI   8(R2),C'C'          LIST RECORDS ALPHRABETICALLY                 
         BNE   INVERR              BY CLIENT/PRODUCT/NAME                       
         SPACE                                                                  
VK40     LA    R2,CGLSTRH          VALIDATE START AT POINT                      
         XC    TIQSTART,TIQSTART                                                
         CLI   5(R2),0                                                          
         BE    VK60                                                             
         CLI   RDSEQ,C'C'          IF READING BY CLIENT/PRODUCT/NAME            
         BNE   VK50                CANNOT HAVE CLIENT FILTER                    
         OC    TGCLI,TGCLI         AND START DEFINED AT SAME TIME               
         BNZ   INVERR                                                           
VK50     ZIC   R3,5(R2)                                                         
         SH    R3,=H'1'                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   TIQSTART(0),8(R2)                                                
         SPACE                                                                  
VK60     LA    R2,CGLOPTSH         OPTIONS                                      
         CLI   5(R2),0                                                          
         BE    VK90                                                             
         LA    R3,BLOCK            SET FOR SCANNER                              
         USING SCAND,R3                                                         
         GOTO1 SCANNER,DMCB,(R2),(R3)                                           
         ZIC   R0,4(R1)                                                         
         CHI   R0,0                INVALID INPUT                                
         BE    INVERR                                                           
         SPACE                                                                  
VK70     DS    0H                  LEAVE ROOM FOR OPTIONS                       
         SPACE                                                                  
VK80     LA    R3,SCANNEXT                                                      
         BCT   R0,VK70                                                          
         SPACE                                                                  
VK90     GOTO1 FLDVAL,DMCB,(X'20',CGLAGYH),(X'80',CGLFMTH)                      
         XC    KEY,KEY            DEFAULT TO EQUAL ZERO                         
         BAS   RE,INIT                                                          
VKX      B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO SET UP SYSIO VARIABLES                                
         SPACE 2                                                                
INIT     NTR1                                                                   
         MVC   TIUSERID,TWAORIG    SET UP NECCESSARY DATA FROM                  
         MVC   TIQSTAFF,TGCTSTAF       GLOBAL STORAGE                           
         MVI   TIREAD,TLOGNCDQ     LIST BY COM GRP CODE                         
         SPACE                                                                  
         MVC   TIFAGY,TGAGY        FILTER ON AGENCY                             
         MVC   TIFCLI,TGCLI        FILTER ON CLIENT                             
         MVC   TIFPRD,TGPRD        FILTER ON PRODUCT                            
         SPACE                                                                  
         CLI   RDSEQ,C'A'                                                       
         BE    INITX                                                            
         MVI   TIREAD,TLOGCDQ      LIST BY COMM GRP NAME                        
INITX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO LIST RECORDS                                          
         SPACE 2                                                                
LR       LA    R0,LRHOOK           SET HOOK TO SYSIO                            
         ST    R0,TIHOOK                                                        
         MVC   TIACOMFC,ACOMFACS                                                
         MVC   TIKHOOK,SETLSTK                                                  
         MVI   NLISTS,15           IN ORDER TO GET CONTROL BACK                 
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
         MVI   NLISTS,14                                                        
         CLI   MODE,PRINTREP                                                    
         BNE   LRX                                                              
         EDIT  COUNTER,(8,P+1),COMMAS=YES,ALIGN=LEFT                            
         LR    R1,R0                                                            
         LA    R1,P+2(R1)                                                       
         MVC   0(23,R1),=C'COMMERCIAL POOL RECORDS'                             
         GOTO1 SPOOL,DMCB,(R8)                                                  
         TM    WHEN,X'40'          IF SPOOLING & NOW                            
         BZ    LRX                                                              
         XC    CONSERV,CONSERV     GO STRAIGHT TO $DQU                          
         MVC   CONSERV(4),=C'$DQU'                                              
LRX      MVI   NLISTS,15                                                        
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PROCESS SYSIO RECORDS                                 
         SPACE 2                                                                
LRHOOK   NTR1                                                                   
         CLI   TIMODE,PROCREC                                                   
         BNE   LRHX                                                             
         BAS   RE,FILTREC                                                       
         BNE   NO                                                               
         B     DISPLAY                                                          
LRHX     B     XIT                                                              
         EJECT                                                                  
*              FILTER RECORD                                                    
         SPACE 2                                                                
FILTREC  NTR1                                                                   
         CLI   RDSEQ,C'A'          IF READING BY ALPHABETIC                     
         BNE   YES                 COM POOL NAME                                
         SPACE                                                                  
         USING TLOGD,R4                                                         
         L     R4,TIAREC                                                        
         SPACE                                                                  
         OC    TIFCLI,TIFCLI       AND CLIENT FILTER DEFINED                    
         BZ    YES                 COMMERCIAL POOL MUST MATCH                   
         CLC   TIFCLI,TLOGCLI                                                   
         BNE   NO                                                               
         SPACE                                                                  
         OC    TIFPRD,TIFPRD       AND PRODUCT FILTER DEFINED                   
         BZ    YES                 COMMERCIAL POOL MUST MATCH                   
         CLC   TIFPRD,TLOGPRD                                                   
         BNE   NO                                                               
         B     YES                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              DISPLAY THE RECORD                                               
         SPACE 2                                                                
         USING LISTD,R2                                                         
DISPLAY  MVC   LISCOG,TIPRG        COMMERCIAL POOL CODE                         
         MVC   LISCLI,TICLI        CLIENT                                       
         MVC   LISPRD,TIPRD        PRODUCT                                      
         MVC   LISNAME,TINAME      FULL NAME OR                                 
         CLC   TISHORT,SPACES                                                   
         BE    D10                                                              
         MVC   LISNAME,TISHORT     SHORT NAME                                   
         SPACE                                                                  
D10      MVC   DMDSKADD,TIDSKADD   PASS DISK ADDRESS TO LISTMON                 
         CLI   MODE,PRINTREP                                                    
         BNE   D20                                                              
         GOTO1 CATCHIOS            ENSURE NOT TOO MANY IOS                      
         GOTO1 SPOOL,DMCB,(R8)     PRINT REPORT                                 
         AP    COUNTER,=P'1'                                                    
         B     PRX                                                              
         SPACE                                                                  
D20      CLI   LISTNUM,15          IF ALREADY FILLED PAGE                       
         BNE   D30                                                              
         MVC   MYMSGNO1,OKNO       HIT ENTER FOR NEXT - MSG                     
         MVI   MYMSYS,X'FF'                                                     
         OI    GENSTAT2,USGETTXT                                                
         LA    R2,CGLSELH                                                       
         B     ERRXIT                                                           
         SPACE                                                                  
D30      GOTO1 LISTMON             CALL LISTMON                                 
         SPACE                                                                  
PRX      B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*              ERROR MESSAGES                                                   
         SPACE 2                                                                
INVERR   MVI   ERROR,INVALID                                                    
         B     ERRXIT                                                           
         SPACE                                                                  
ERRXIT   XC    DMCB,DMCB                                                        
         GOTO1 EXIT,DMCB                                                        
*        EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 2                                                                
PFTABLE  DS    0C                  PF KEYS TABLE                                
         DC    AL1(PF13X-*,13,0,(PF13X-PF13)/KEYLNQ,0)                          
         DC    CL3' ',CL8'COMMER  ',CL8'LIST'                                   
PF13     DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYCUR,L'LISCLI-1),AL2(LISCLI-LISTD)                       
         DC    AL1(KEYTYCUR,L'LISPRD-1),AL2(LISPRD-LISTD)                       
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYCUR,L'LISCOG-1),AL2(LISCOG-LISTD)                       
PF13X    EQU   *                                                                
         DC    X'FF'                                                            
         SPACE 2                                                                
         LTORG                                                                  
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              SPECS FOR SPOOLING                                               
         SPACE                                                                  
MYSPECS  DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,56,REQUESTOR                                                  
         SSPEC H2,56,REPORT                                                     
         SSPEC H2,73,PAGE                                                       
         SPACE 1                                                                
         SSPEC H1,31,C'COMMERCIAL POOL LIST'                                    
         SSPEC H2,31,C'--------------------'                                    
         SPACE 1                                                                
         SSPEC H4,2,C'CPOOL        CLIENT  PRODUCT   NAME'                      
         SSPEC H5,2,C'------       ------  -------   ----'                      
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
LISTD    DSECT                                                                  
         DS    CL1                                                              
LISCOG   DS    CL6                                                              
         DS    CL7                                                              
LISCLI   DS    CL6                                                              
         DS    CL2                                                              
LISPRD   DS    CL6                                                              
         DS    CL4                                                              
LISNAME  DS    CL16                                                             
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR7ED                                                       
         EJECT                                                                  
*                                                                               
COUNTER  DS    PL4                 COUNT OUTPUT LINES                           
RDSEQ    DS    CL1                 READ WITH ACTIVE/PASSIVE POINTER             
SAVKEY   DS    CL48                SAVED KEY                                    
         SPACE 2                                                                
       ++INCLUDE TASYSIOD                                                       
         EJECT                                                                  
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSDSECT                                                                    
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002TAGEN7E   07/21/08'                                      
         END                                                                    
