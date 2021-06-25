*          DATA SET TAGEN37    AT LEVEL 017 AS OF 04/08/14                      
*PHASE T70237A                                                                  
         TITLE 'T70237 - PRODUCT GROUP LIST'                                    
T70237   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70237                                                         
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
*              MODE CONTROLLED ROUTINES                                         
*                                                                               
PGR10    GOTO1 INITIAL,DMCB,PFTABLE                                             
         CLI   MODE,VALKEY         FIRST TIME IN                                
         BE    VK                                                               
*                                                                               
PGR20    CLI   MODE,PRINTREP                                                    
         BNE   PGR30                                                            
         ZAP   COUNTER,=P'0'       CLEAR LINE COUNTER                           
         XC    KEY,KEY             CLEAR KEY FOR REPORT                         
         LA    R2,MYSPECS          SET UP SPECS FOR REPORT                      
         ST    R2,SPECS                                                         
         LA    R2,P                                                             
         B     LR                                                               
*                                                                               
PGR30    CLI   MODE,LISTRECS                                                    
         BNE   PGRX                                                             
         LA    R2,LISTAR                                                        
         MVC   LISTAR,SPACES       CLEAR PREVIOUS LINE                          
         B     LR                                                               
*                                                                               
PGRX     B     XIT                                                              
*                                                                               
         SPACE 2                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*                                                                               
* VALIDATE THE KEY                                                              
*                                                                               
VK       LA    R2,SPGAGYH                                                       
         TM    4(R2),X'20'         VALIDATED                                    
         BO    VK1                                                              
         NI    SPGCLIH+4,X'DF'     FORCE VALIDATION OF NEXT FIELD               
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLAYCDQ,(R2)   VALIDATE AGENCY                       
         MVC   AIO,AIO1                                                         
*                                                                               
VK1      LA    R2,SPGCLIH                                                       
         TM    4(R2),X'20'         VALIDATED                                    
         BO    VK2                                                              
         NI    SPGSTRH+4,X'DF'     FORCE VALIDATION OF NEXT FIELD               
         CLI   SPGFMT,C'C'                                                      
         BE    *+12                                                             
         CLI   5(R2),0                                                          
         BE    VK2                                                              
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLCLCDQ,(R2)   VALIDATE CLIENT                       
         MVC   AIO,AIO1                                                         
*                                                                               
VK2      OI    4(R2),X'20'                                                      
         LA    R2,SPGSTRH          START AT?                                    
         TM    4(R2),X'20'         ALREADY VALIDATED                            
         BO    VK5                                                              
         XC    TIQSTART,TIQSTART                                                
         NI    SPGFMTH+4,X'DF'     FORCE VALIDATION OF NEXT FIELD               
*                                                                               
         CLI   5(R2),0                                                          
         BE    VK5                                                              
         ZIC   R3,5(R2)                                                         
         SH    R3,=H'1'                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   TIQSTART(0),8(R2)                                                
*                                                                               
VK5      OI    4(R2),X'20'                                                      
         LA    R2,SPGFMTH          VALIDATE FORMAT                              
         TM    4(R2),X'20'         ALREADY VALIDATED                            
         BO    VK10                                                             
         NI    SPGOPTSH+4,X'DF'    FORCE VALIDATION OF NEXT FIELD               
         MVI   RDSEQ,C'A'          DEFAULT TO ALPHA SEQUENCE                    
         CLI   5(R2),0                                                          
         BE    VK10                                                             
         CLI   5(R2),1             ONLY 1 CHAR CAN BE INPUT                     
         BNE   INVERR                                                           
         CLI   8(R2),C'A'                                                       
         BE    VK10                                                             
         MVI   RDSEQ,C'C'                                                       
         CLI   8(R2),C'C'          CODE SEQUENCE                                
         BNE   INVERR                                                           
*                                                                               
VK10     OI    4(R2),X'20'                                                      
         LA    R2,SPGOPTSH         OPTIONS                                      
         TM    4(R2),X'20'         ALREADY VALIDATED                            
         BO    VKX                                                              
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
VK20     DS    0H                  LEAVE ROOM FOR OPTIONS                       
*                                                                               
VK50     LA    R3,SCANNEXT                                                      
         BCT   R0,VK20                                                          
*                                                                               
VK60     OI    4(R2),X'20'                                                      
         XC    KEY,KEY            DEFAULT TO EQUAL ZERO                         
         BAS   RE,INIT                                                          
*                                                                               
VKX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        SET UP SYSIO                                                           
*                                                                               
INIT     NTR1                                                                   
         MVC   TIUSERID,TWAORIG    SET UP NECCESSARY DATA FROM                  
         MVC   TIQSTAFF,TGCTSTAF       GLOBAL STORAGE                           
         MVI   TIREAD,TLPGNCDQ     LIST BY PRODUCT GRP NAME                     
         MVC   TIFAGY,TGAGY        FILTER ON AGENCY                             
*                                                                               
         CLI   RDSEQ,C'A'                                                       
         BE    INITX                                                            
         MVC   TIFCLI,TGCLI        FILTER ON AGENCY                             
         MVI   TIREAD,TLPGCDQ      LIST BY PRD GRP CODE                         
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
         MVC   0(21,R1),=C'PRODUCT GROUP RECORDS'                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         TM    WHEN,X'40'          IF SPOOLING & NOW                            
         BZ    LRX                                                              
         XC    CONSERV,CONSERV     GO STRAIGHT TO $DQU                          
         MVC   CONSERV(4),=C'$DQU'                                              
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
*                                                                               
PR10     MVC   LISPRG,TIPRG        PRODUCT GRP CODE                             
         MVC   LISCLI,TICLI        CLIENT                                       
         CLC   TISHORT,SPACES                                                   
         BNE   PR15                                                             
         MVC   LISNAME,TINAME                                                   
         B     PR17                                                             
*                                                                               
PR15     MVC   LISNAME,TISHORT                                                  
*                                                                               
PR17     MVC   DMDSKADD,TIDSKADD   PASS DISK ADDRESS TO LISTMON                 
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
         LA    R2,SPGSELH                                                       
         B     ERRXIT                                                           
*                                                                               
PR30     GOTO1 LISTMON             CALL LISTMON                                 
*                                                                               
PRX      B     XIT                                                              
         DROP  R2                                                               
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
PFTABLE  DS    0C                  PF KEYS TABLE                                
         DC    AL1(PF13X-*,13,0,(PF13X-PF13)/KEYLNQ,0)                          
         DC    CL3' ',CL8'PRODUCT ',CL8'LIST'                                   
PF13     DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYCUR,L'LISCLI-1),AL2(LISCLI-LISTD)                       
         DC    AL1(KEYTYCUR,L'LISPRG-1),AL2(LISPRG-LISTD)                       
PF13X    EQU   *                                                                
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
         SSPEC H1,32,C'PRODUCT GROUP LIST'                                      
         SSPEC H2,32,C'------------------'                                      
         SPACE 1                                                                
         SSPEC H4,2,C'PGROUP  CLIENT  NAME'                                     
         SSPEC H5,2,C'------  ------  ----'                                     
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
LISTD    DSECT                                                                  
         DS    CL1                                                              
LISPRG   DS    CL6                                                              
         DS    CL2                                                              
LISCLI   DS    CL6                                                              
         DS    CL2                                                              
LISNAME  DS    CL16                                                             
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR37D                                                       
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
**PAN#1  DC    CL21'017TAGEN37   04/08/14'                                      
         END                                                                    
