*          DATA SET PRSFM0A    AT LEVEL 007 AS OF 05/01/02                      
*PHASE T41C0AA,*                                                                
*INCLUDE SRCHCALL                                                               
*INCLUDE PUBED                                                                  
         TITLE 'T41C0A  PG SPECIAL COST RECORDS'                                
T41C0A   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T41C0A,RR=R3                                                   
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         ST    R3,RELO                                                          
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,XRECADD        RE-DISPLAY RECORD                            
         BE    DR                                                               
         CLI   MODE,XRECPUT        RE-DISPLAY RECORD                            
         BE    DR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PR                                                               
         CLI   MODE,RECDEL         DELETE RECORD                                
         BE    DR                                                               
         CLI   MODE,RECREST        RESTORE RECORD                               
         BE    DR                                                               
         B     XIT                                                              
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*                                                                               
*     VALIDATE KEY ROUTINE                                                      
*                                                                               
VK       DS    0H                                                               
         CLI   ACTNUM,ACTLIST      LIST                                         
         BE    VKL                                                              
*                                                                               
         LA    R2,PGCMEDH          MEDIA                                        
         GOTO1 VALIMED                                                          
*                                                                               
         LA    R2,PGCCLTH          CLIENT                                       
         GOTO1 VALICLT                                                          
*                                                                               
         BAS   RE,MYPUBVAL                                                      
*                                                                               
         LA    R2,PGCSPAH          SPACE                                        
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
         XC    TSPACE,TSPACE                                                    
         CLI   5(R2),3             SPECIAL FOR ALL                              
         BNE   VK10                                                             
         CLC   =C'ALL',8(R2)                                                    
         BE    VK20                                                             
*                                                                               
VK10     MVC   TSPACE,SPACES                                                    
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TSPACE(0),PGCSPA                                                 
*                                                                               
VK20     XC    KEY,KEY             BUILD KEY                                    
         LA    R4,KEY                                                           
         USING PGCOST,R4                                                        
         MVC   PGCOAGY,AGENCY                                                   
         MVC   PGCOMED,QMED                                                     
         MVI   PGCOTYPE,X'26'                                                   
         MVC   PGCOCLT,QCLT                                                     
         MVC   PGCOPUB,BPUB                                                     
         MVC   PGCOSPAC,TSPACE                                                  
         MVC   ORIGKEY,KEY         SAVE THIS KEY (USE WITH LIST)                
         B     VKXIT                                                            
*                                                                               
*        VALIDATE LIST KEY                                                      
*                                                                               
VKL      LA    R2,PGLMEDH          MEDIA                                        
         GOTO1 VALIMED                                                          
*                                                                               
         LA    R2,PGLCLTH          CLIENT                                       
         GOTO1 VALICLT                                                          
*                                                                               
VKXIT    B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*        VALIDATE PUB FIELD                                                     
*                                                                               
MYPUBVAL NTR1                                                                   
         MVI   ALLZE,C'N'          INITIALIZE                                   
         MVI   ERROR,MISSING                                                    
         LA    R2,PGCPUBH                                                       
         CLI   5(R2),0                                                          
         BE    ERRX                                                             
         CLI   8(R2),C'='          PUB NAME SEARCH                              
         BNE   VKPUB10                                                          
         SR    R2,RA                                                            
         LA    R3,WORK                                                          
         USING DSPARM,R3                                                        
         XC    DSPARM(DSPARML),DSPARM                                           
         MVC   DSMEDCOD,QMED                                                    
         GOTO1 =V(SRCHCALL),DMCB,(3,(R2)),(X'80',(RA)),ACOMFACS,       X        
               ('DSPARML',WORK),(1,=CL8'PUB'),0,RR=RELO                         
         B     VKPUB30                                                          
         DROP  R3                                                               
*                                                                               
VKPUB10  DS    0H                                                               
         LA    R2,PGCPUBH                                                       
         MVI   ERROR,INVALID                                                    
         XC    SCANBLK,SCANBLK                                                  
         GOTO1 SCANNER,DMCB,(R2),(2,SCANBLK)                                    
         CLI   DMCB+4,0                                                         
         BE    ERRX                                                             
         LA    R1,SCANBLK                                                       
         CLC   =C'ALL',44(R1)                                                   
         BNE   VKPUB30                                                          
         MVI   ALLZE,C'Y'          WANT ONLY ACROSS ALL ZONES AND ED            
         ZIC   R3,0(R1)                                                         
         STC   R3,5(R2)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),12(R1)                                                   
*                                                                               
VKPUB30  LA    R2,PGCPUBH                                                       
         GOTO1 VALIPUB                                                          
         MVC   PGCPUBN,PUBNM       DISPLAY PUB NAME                             
         OI    PGCPUBNH+6,X'80'                                                 
         CLI   ALLZE,C'Y'                                                       
         BNE   VKPX                                                             
         MVC   BPUB+4(2),=X'FFFF'  ALL ZONES/EDTNS                              
         ZIC   R1,5(R2)                                                         
         AH    R1,=H'4'            PUT BACK THE ORIGINAL LEN                    
         STC   R1,5(R2)                                                         
*                                                                               
VKPX     B     XIT                                                              
         EJECT                                                                  
*                                                                               
* DISPLAY KEY                                                                   
*                                                                               
DK       L     R6,AIO                                                           
         USING PGCOST,R6                                                        
         MVC   PGCMED,PGCOMED      MEDIA                                        
         OI    PGCMEDH+6,X'80'     TRANSMIT                                     
*                                                                               
         MVC   PGCCLT,PGCOCLT      CLIENT                                       
         OI    PGCCLTH+6,X'80'                                                  
*                                                                               
         GOTO1 =V(PUBED),DMCB,PGCOPUB,PGCPUB,RR=RELO                            
         OI    PGCPUBH+6,X'80'                                                  
*                                                                               
         OC    PGCOSPAC,PGCOSPAC   ALL                                          
         BNZ   DK10                                                             
         MVC   PGCSPA(3),=C'ALL'                                                
         B     *+10                                                             
*                                                                               
DK10     MVC   PGCSPA,PGCOSPAC     SPACE                                        
         OI    PGCSPAH+6,X'80'                                                  
*                                                                               
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* VALIDATE RECORD                                                               
*                                                                               
VR       DS    0H                                                               
         LA    R2,PGCEDATH         EFFECTIVE DATE                               
         XR    R5,R5               COUNTER                                      
         XC    DATETAB(L'DATETAB),DATETAB                                       
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         GOTO1 REMELEM             REMOVE ALL OLD ELEMENTS                      
*                                                                               
VR10     LA    R3,ELEMENT                                                       
         USING PGCOEL01,R3                                                      
         XC    ELEMENT,ELEMENT                                                  
         ZAP   PGCOAVG$,=P'0'                                                   
         ZAP   PGCOOPEN,=P'0'                                                   
         MVI   ADDYN,0                                                          
         CLI   5(R2),0             IF NO INPUT                                  
         BNE   VR15                                                             
         LR    R0,R2               SAVE R2                                      
         BAS   RE,BUMP             & SEE IF ANY INPUT ON THIS LINE              
         CLI   5(R2),0                                                          
         BE    VR11                                                             
         LR    R2,R0               RESET R2 FOR ERROR                           
         B     MISSERR                                                          
*                                                                               
VR11     BAS   RE,BUMP                                                          
         CLI   5(R2),0                                                          
         BE    VR30                                                             
         LR    R2,R0               RESET R2 FOR ERROR                           
         B     MISSERR                                                          
*                                                                               
VR15     CLC   =C'DELETE',8(R2)    DELETE THIS ENTRY                            
         BNE   VR17                                                             
         BAS   RE,BUMP             BUMP TO NEXT LINE                            
         BAS   RE,BUMP                                                          
         B     VR30                                                             
*                                                                               
VR17     MVI   0(R3),X'01'         ELEMENT CODE                                 
         MVI   1(R3),PGCOELLN      ELEMENT LENGTH                               
         GOTO1 DATVAL,DMCB,8(R2),WORK                                           
         OC    0(4,R1),0(R1)                                                    
         BE    DATERR                                                           
         GOTO1 DATCON,DMCB,(0,WORK),(3,PGCODATE)                                
         BAS   RE,DUPDATE          MAKE SURE NO DUPLICATE DATE ENTRIES          
         BE    DUPERR                                                           
         MVI   ADDYN,1                                                          
         BAS   RE,BUMP             BUMP TO AVG$ FIELD                           
*                                                                               
         ST    R2,FULL             SAVE A(AVG FIELD)                            
         ZIC   R4,5(R2)                                                         
         LTR   R4,R4               IF NO INPUT                                  
         BZ    VR20                CHECK NEXT FIELD                             
         ZIC   R1,ADDYN                                                         
         LA    R1,1(R1)                                                         
         STC   R1,ADDYN                                                         
         GOTO1 CASHVAL,DMCB,(X'80',8(R2)),(R4)                                  
         CLI   0(R1),X'FF'                                                      
         BE    INVERR                                                           
         OC    4(3,R1),4(R1)                                                    
         BNZ   INVERR                                                           
         MVC   PGCOAVG$,7(R1)                                                   
*                                                                               
VR20     BAS   RE,BUMP             BUMP TO OPEN FIELD                           
         ZIC   R4,5(R2)                                                         
         LTR   R4,R4               IF NO INPUT                                  
         BZ    VR30                CHECK NEXT LINE                              
         ZIC   R1,ADDYN                                                         
         LA    R1,1(R1)                                                         
         STC   R1,ADDYN                                                         
         GOTO1 CASHVAL,DMCB,(X'80',8(R2)),(R4)                                  
         CLI   0(R1),X'FF'                                                      
         BE    INVERR                                                           
         OC    4(3,R1),4(R1)                                                    
         BNZ   INVERR                                                           
         MVC   PGCOOPEN,7(R1)                                                   
*                                                                               
VR30     CLI   ADDYN,0             EMPTY LINE                                   
         BE    VR40                                                             
         CLI   ADDYN,1             ONLY A DATE ON THE LINE                      
         BNE   VR35                                                             
         L     R2,FULL             POINT TO AVG FIELD                           
         B     MISSERR                                                          
*                                                                               
VR35     CH    R5,=H'14'                                                        
         BE    NOMORE                                                           
         GOTO1 ADDELEM                                                          
         LA    R5,1(R5)            COUNTER                                      
*                                                                               
VR40     BAS   RE,BUMP             BUMP TO NEXT LINE                            
         LA    R1,PGCLAST                                                       
         CR    R2,R1                                                            
         BL    VR10                                                             
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
* DISPLAY RECORD                                                                
*                                                                               
DR       DS    0H                                                               
         TWAXC PGCEDATH                                                         
         LA    R2,PGCDEDTH                                                      
         LA    R4,14               SET FOR LOOP                                 
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         USING PGCOEL01,R6                                                      
         BAS   RE,GETEL                                                         
         B     DR20                                                             
*                                                                               
DR10     BAS   RE,NEXTEL                                                        
*                                                                               
DR20     BNE   DRX                 NO ELEMENTS LEFT                             
         OC    PGCODATE,PGCODATE                                                
         BZ    DR25                                                             
         GOTO1 DATCON,DMCB,(3,PGCODATE),(5,8(R2))                               
*                                                                               
DR25     BAS   RE,BUMP                                                          
         CLC   PGCOAVG$,=PL5'0'                                                 
         BE    DR30                                                             
         EDIT  PGCOAVG$,(10,8(R2)),2                                            
*                                                                               
DR30     BAS   RE,BUMP                                                          
         CLC   PGCOOPEN,=PL5'0'                                                 
         BE    DR40                                                             
         EDIT  PGCOOPEN,(10,8(R2)),2                                            
*                                                                               
DR40     BAS   RE,BUMP                                                          
         BCT   R4,DR10                                                          
*                                                                               
DRX      B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* LIST RECORDS                                                                  
*                                                                               
LR       DS    0H                                                               
         OI    GLSTSTAT,RETEXTRA                                                
         LA    R6,KEY                                                           
         USING PGCOST,R6                                                        
         OC    KEY,KEY             TEST FIRST TIME                              
         BNZ   LR030               KEY IS LAST RECORD READ                      
*                                  SO GO CHECK VS. KEYSAVE                      
*                                                                               
         MVC   PGCOAGY,AGENCY      CREATE KEY - AGENCY                          
         MVC   PGCOMED,QMED                     MEDIA CODE                      
         MVI   PGCOTYPE,X'26'                   TYPE                            
         MVC   PGCOCLT,QCLT                     CLIENT                          
*                                                                               
LR010    GOTO1 HIGH                                                             
         B     LR030                                                            
*                                                                               
LR020    GOTO1 SEQ                                                              
*                                                                               
LR030    CLC   KEY(7),KEYSAVE      TEST FOR ALL DONE                            
         BE    LR035                                                            
         XC    KEY,KEY                                                          
         B     LRX                                                              
*                                                                               
LR035    GOTO1 GETREC              GET THE COST RECORD                          
         MVC   MYDSKADD,DMDSKADD   SAVE D/A FOR LIST                            
*                                                                               
         L     R6,AIO                                                           
         USING LISTD,R5                                                         
         LA    R5,P1               USE P LINES                                  
         CLI   MODE,PRINTREP                                                    
         BE    LR090                                                            
         LA    R5,LISTAR           OR LIST AREA                                 
         MVC   LISTAR,SPACES                                                    
         MVC   LSPACE,PGCOSPAC                                                  
         OC    PGCOSPAC,PGCOSPAC                                                
         BNZ   *+10                                                             
         MVC   LSPACE(3),=C'ALL'                                                
*                                                                               
         GOTO1 =V(PUBED),DMCB,PGCOPUB,LPUB,RR=RELO                              
         MVC   BPUB,PGCOPUB                                                     
         BAS   RE,MYVPUB                                                        
         MVC   LPUBN,PUBNM         DISPLAY PUB NAME                             
*                                                                               
         USING PGCOEL01,R6                                                      
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BNE   LR080                                                            
         GOTO1 DATCON,DMCB,(3,PGCODATE),(5,LEFFDT)                              
         OC    PGCOAVG$,PGCOAVG$                                                
         BZ    LR040                                                            
         EDIT  PGCOAVG$,(10,LAVG),2                                             
*                                                                               
LR040    OC    PGCOOPEN,PGCOOPEN                                                
         BZ    LR080                                                            
         EDIT  PGCOOPEN,(10,LOPEN),2                                            
*                                                                               
LR080    MVC   DMDSKADD,MYDSKADD   RESTORE D/A                                  
         GOTO1 LISTMON             CALL LISTMON                                 
         B     LR020                                                            
*                                                                               
LR090    DS    0H                  **NB- PRINTREP NOT FULLY CODED               
         GOTO1 SPOOL,DMCB,SPOOLD                                                
         B     LR020                                                            
*                                                                               
LRX      B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* SET UP THE PUB NUMBER & GET NAME                                              
*                                                                               
MYVPUB   NTR1                                                                   
*                                                                               
         MVC   MYKEY,KEY           SAVE KEY                                     
         MVC   PUBNM,SPACES                                                     
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PUBRECD,R4                                                       
         MVC   PUBKMED,QMED                                                     
         MVC   PUBKPUB(6),BPUB     MOVE PUB/ZONE/EDTN                           
         MVC   PUBKAGY,AGENCY                                                   
         MVI   PUBKCOD,X'81'                                                    
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'PUBDIR'                                            
         GOTO1 HIGH                                                             
         XC    FILENAME,FILENAME                                                
*                                                                               
         CLC   KEY(25),KEYSAVE                                                  
         BNE   VPNO                                                             
*                                                                               
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         MVC   FILENAME,=CL8'PUBFILE'                                           
         GOTO1 GETREC                                                           
         XC    FILENAME,FILENAME                                                
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PUBNAMEL,R6                                                      
         MVC   PUBNM,PUBNAME                                                    
*                                                                               
         MVC   AIO,AIO1                                                         
         MVC   KEY,MYKEY           RESTORE KEY                                  
         GOTO1 HIGH                                                             
         B     XIT                                                              
*                                                                               
VPNO     MVC   AIO,AIO1                                                         
         MVC   KEY,MYKEY           RESTORE KEY                                  
         GOTO1 HIGH                                                             
         LTR   RB,RB                                                            
         B     XIT                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
*                                                                               
* PRINT REPORT                                                                  
*                                                                               
PR       L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         LA    R1,HEDSPECS         SET UP HEADHOOK AND SPECS                    
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
*                                                                               
PRX      B     XIT                                                              
         EJECT                                                                  
HOOK     NTR1                      HEADLINE ROUTINES                            
*                                                                               
HOOKX    B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        CHECK IF DATE ALREADY INPUT                                            
*        R3 - ELEMENT                                                           
*                                                                               
DUPDATE  NTR1                                                                   
         USING PGCOEL01,R3                                                      
         LA    R2,DATETAB          KEEP TABLE OF DATES INPUT                    
         LA    R1,14               MAXIMUM OF 14 DATES                          
*                                                                               
DUP10    OC    0(3,R2),0(R2)       END OF TABLE                                 
         BE    DUP20                                                            
         CLC   PGCODATE,0(R2)                                                   
         BE    YES                                                              
         LA    R2,3(R2)            BUMP TABLE                                   
         BCT   R1,DUP10                                                         
*                                                                               
DUP20    MVC   0(3,R2),PGCODATE                                                 
         B     NO                                                               
         DROP  R3                                                               
         SPACE 3                                                                
*                                                                               
*        BUMP TO NEXT FIELD                                                     
*                                                                               
BUMP     ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         BR    RE                                                               
*                                                                               
DATERR   MVI   ERROR,INVDATE                                                    
         B     ERRX                                                             
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
         B     ERRX                                                             
*                                                                               
DUPERR   MVI   ERROR,DUPDATER                                                   
         B     ERRX                                                             
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
         B     ERRX                                                             
*                                                                               
NOMORE   MVI   ERROR,NOMORERM                                                   
         LA    R2,PGCEDATH                                                      
         B     ERRX                                                             
*                                                                               
ERRX     GOTO1 ERREX                                                            
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
HEDSPECS SSPEC H1,55,C' P&&G COST REPORT'                                       
         SSPEC H2,55,C'----------------'                                        
         SSPEC H1,95,AGYNAME                                                    
         SSPEC H2,95,AGYADD                                                     
         SSPEC H4,95,RUN                                                        
         SSPEC H5,95,REPORT                                                     
         SSPEC H1,1,REQUESTOR                                                   
         SSPEC H7,1,C'PUB                         SPACE'                        
         SSPEC H7,31,C'EFF DATE   AVG COST   OPEN RATE'                         
         SSPEC H8,1,C'---                         -----'                        
         SSPEC H8,31,C'--------   --------   ---------'                         
         DC    X'00'                                                            
         EJECT                                                                  
         PRINT GEN                                                              
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
         LTORG                                                                  
ADDYN    DS    CL1                                                              
TSPACE   DS    CL12                                                             
MYDSKADD DS    XL4                                                              
DATETAB  DS    XL(14*3)                                                         
         EJECT                                                                  
       ++INCLUDE PRSFMFFD                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMEAD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMFAD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE PRSFMWORKD                                                     
*                                                                               
         ORG   SYSSPARE                                                         
RELO     DS    F                                                                
ORIGKEY  DS    XL(L'KEY)                                                        
MYKEY    DS    XL(L'KEY)                                                        
ALLZE    DS    CL1                                                              
SCANBLK  DS    CL70                                                             
*                                                                               
* *******************                                                           
* ON-SCREEN LIST LINE                                                           
* *******************                                                           
*                                                                               
LISTD    DSECT                                                                  
LPUB     DS    CL7                 PUBLICATION ZONE AND EDITION                 
         DS    CL1                                                              
LPUBN    DS    CL20                PUB NAME                                     
         DS    CL1                                                              
LSPACE   DS    CL12                SPACE                                        
         DS    CL1                                                              
LEFFDT   DS    CL8                 EFFECTIVE DATE                               
         DS    CL1                                                              
LAVG     DS    CL10                AVERAGE $                                    
         DS    CL1                                                              
LOPEN    DS    CL10                OPEN RATE                                    
         EJECT                                                                  
       ++INCLUDE PRGENPGC                                                       
         EJECT                                                                  
PUBRECD  DSECT                                                                  
       ++INCLUDE PUBREC                                                         
       ++INCLUDE PUBNAMEL                                                       
         EJECT                                                                  
*DDSPOOLD                                                                       
*DDCOMFACS                                                                      
*DDFLDIND                                                                       
*PPSRCHPARM                                                                     
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE PPSRCHPARM                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007PRSFM0A   05/01/02'                                      
         END                                                                    
