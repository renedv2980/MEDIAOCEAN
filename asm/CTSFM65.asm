*          DATA SET CTSFM65    AT LEVEL 001 AS OF 09/26/06                      
*PHASE TA0A65A                                                                  
***********************************************************************         
*                                                                     *         
*  TITLE:        CTSFM65 -- DEMOSTA RECORD MAINTENANCE/LIST           *         
*                                                                     *         
*  CALLED FROM:  SFM CONTROLLER (TA0A00), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREENS CTSFMA5 (MAINTENANCE)                        *         
*                        CTSFMA6 (LIST)                               *         
*                                                                     *         
*  OUTPUTS:      UPDATED DEMOSTA RECORDS, LIST, OR REPORT.            *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- SCREEN FIELD HEADER                            *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- WORK                                           *         
*                R6 -- GETEL REGISTER                                 *         
*                R7 -- SECOND BASE                                    *         
*                R8 -- SPOOLD                                         *         
*                R9 -- SYSD                                           *         
*                RA -- TWA                                            *         
*                RB -- FIRST BASE                                     *         
*                RC -- GEND                                           *         
*                RD -- SYSTEM                                         *         
*                RE -- SYSTEM                                         *         
*                RF -- SYSTEM                                         *         
*                                                                     *         
***********************************************************************         
         TITLE 'TA0A65 - DEMOSTA RECORD MAINT/LIST/REPORT'                      
TA0A65   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**0A65**                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING TA0AFFD,RA          BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
*                                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BNE   *+12                                                             
         BAS   RE,DK                                                            
         B     XIT                                                              
*                                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,XRECADD        AFTER ADDREC (MAINTAIN PASSIVES)             
         BE    XA                                                               
         CLI   MODE,XRECPUT        AFTER PUTREC (MAINTAIN PASSIVES)             
         BE    XP                                                               
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEY                                                                  
*                                                                               
VK       DS    0H                                                               
         MVI   SVSUBFIL,0                                                       
         LA    R2,SFMSBFH          SUB-FILE                                     
         CLI   5(R2),0                                                          
         BNE   *+16                                                             
         CLI   ACTEQU,ACTLIST                                                   
         BE    VK20                                                             
         B     MISSERR             REQUIRED FOR MAINTENANCE ACTIONS             
*                                                                               
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         LA    RE,SBFTAB           TABLE OF VALID SUB-FILES                     
VK10     CLI   0(RE),X'FF'         EOT?                                         
         BE    INVERR              YES: INVALID                                 
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SFMSBF(0),0(RE)                                                  
         BE    *+12                                                             
         LA    RE,L'SBFTAB(RE)                                                  
         B     VK10                                                             
         MVC   SVSUBFIL,8(RE)                                                   
         MVC   SFMSBF,0(RE)        DISPLAY FULL SUB-FILE DESCRIPTION            
         OI    SFMSBFH+6,X'80'                                                  
*                                                                               
VK20     DS    0H                                                               
         MVI   SVSOURCE,0                                                       
         LA    R2,SFMSRCH          RATINGS SOURCE                               
         CLI   5(R2),0                                                          
         BNE   *+16                                                             
         CLI   ACTEQU,ACTLIST                                                   
         BE    VK50                                                             
         B     MISSERR             REQUIRED FOR MAINTENANCE ACTIONS             
*                                                                               
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         LA    RE,SRCTAB           TABLE OF VALID SOURCES                       
VK30     CLI   0(RE),X'FF'         EOT?                                         
         BE    INVERR              YES: INVALID                                 
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SFMSRC(0),0(RE)                                                  
         BE    *+12                                                             
         LA    RE,L'SRCTAB(RE)                                                  
         B     VK30                                                             
         MVC   SVSOURCE,8(RE)                                                   
         MVC   SFMSRC,0(RE)        DISPLAY FULL SOURCE DESCRIPTION              
         OI    SFMSRCH+6,X'80'                                                  
*                                                                               
         LA    RE,MSTAB            TABLE OF VALID SOURCE/SUBFILE COMBOS         
VK40     CLI   0(RE),X'FF'         EOT?                                         
         BE    INVERR              YES: INVALID                                 
         CLC   SVSBFSRC,0(RE)                                                   
         BE    *+12                                                             
         LA    RE,L'MSTAB(RE)                                                   
         B     VK40                                                             
*                                                                               
VK50     DS    0H                                                               
         XC    SVSTATN,SVSTATN     STATION                                      
         LA    R2,SFMSTATH                                                      
         CLI   5(R2),0                                                          
         BNE   *+16                                                             
         CLI   ACTEQU,ACTLIST                                                   
         BE    VKX                                                              
         B     MISSERR             REQUIRED FOR MAINTENANCE ACTIONS             
*                                                                               
         OC    SFMSTAT,SPACES                                                   
         CLI   SFMSTAT+4,C' '      IF FIFTH BYTE OF STATION IS BLANK...         
         BNE   *+8                                                              
         MVI   SFMSTAT+4,C'T'      ...SUBSTITUTE A "T"                          
*                                                                               
         MVC   SVSTATN,SFMSTAT                                                  
*                                                                               
         CLI   ACTEQU,ACTLIST      ACTION LIST?                                 
         BE    VKX                 YES: NO BOOK FIELD TO VALIDATE               
*                                                                               
         LA    R2,SFMLSBKH         BOOK IS REQUIRED                             
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
         GOTO1 DATVAL,DMCB,(2,SFMLSBK),WORK                                     
         CLC   =C'000000',WORK                                                  
         BE    DATEERR                                                          
         MVC   WORK+4(2),=C'01'    SET TO FIRST DAY OF MONTH                    
         GOTO1 DATCON,DMCB,WORK,(3,FULL)                                        
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY              BUILD KEY                                    
         USING DSTKEYD,R4                                                       
         MVI   DSTKMSYS,DSTKMSYQ                                                
         MVI   DSTKSYS,DSTKSYSQ                                                 
         MVI   DSTKSTYP,DSTKSTYQ                                                
         MVC   DSTKSBF,SVSUBFIL                                                 
         MVC   DSTKSRC,SVSOURCE                                                 
         MVC   DSTKSTN,SVSTATN                                                  
         MVC   DSTKLSBK,FULL                                                    
         MVC   SAVEKEY,KEY         HANG ON TO KEY                               
         DROP  R4                                                               
*                                                                               
VKX      DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
* VALIDATE RECORD                                                               
*                                                                               
VR       DS    0H                                                               
         L     R4,AIO              BUILD RECORD                                 
         USING DSTKEY,R4                                                        
*                                                                               
         MVI   ELCODE,DSTSTACQ     STATION ELEMENT                              
         GOTO1 REMELEM                                                          
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING DSTSTAD,R6                                                       
         MVI   DSTSTACD,DSTSTACQ   ELEMENT CODE                                 
         MVI   DSTSTAEL,DSTSTALQ   ELEMENT LENGTH                               
*                                                                               
         MVC   HALF,DSTKLSBK       LAST BOOK OF ORIGINAL STATION                
         LA    R2,SFMALTSH         FIRST EQUIVALENT STATION                     
*                                                                               
VR10     DS    0H                                                               
         CLI   5(R2),0             STATION MUST BE PRESENT                      
         BE    MISSERR                                                          
*                                                                               
         OC    8(5,R2),SPACES                                                   
         CLI   12(R2),C' '         IF FIFTH BYTE OF STATION IS BLANK...         
         BNE   *+8                                                              
         MVI   12(R2),C'T'         ...SUBSTITUTE A "T"                          
         MVC   DSTSTAST,8(R2)                                                   
*                                                                               
         MVC   DSTSTLBK,=X'FFFF'   ASSUME THIS IS THE LAST STATION              
         ZIC   R0,0(R2)            BUMP TO THE END DATE FIELD...                
         AR    R2,R0               ...FOR THIS STATION                          
         CLI   5(R2),0                                                          
         BE    VR20                NO END DATE: IT'S THE LAST STATION           
*                                                                               
         GOTO1 DATVAL,DMCB,(2,8(R2)),WORK                                       
         CLC   =C'000000',WORK                                                  
         BE    DATEERR                                                          
         MVC   WORK+4(2),=C'01'    SET TO FIRST DAY OF MONTH                    
         GOTO1 DATCON,DMCB,WORK,(3,FULL)                                        
         MVC   DSTSTLBK,FULL       SAVE LAST BOOK                               
         CLC   DSTSTLBK,HALF       IS DATE SUBSEQUENT TO PREVIOUS?              
         BNH   DATEERR             NO: ERROR                                    
         MVC   HALF,DSTSTLBK                                                    
*                                                                               
VR20     DS    0H                                                               
         ZIC   R0,0(R2)            BUMP TO COMMENT FIELD FOR STATION            
         AR    R2,R0                                                            
         MVC   DSTSTCOM,8(R2)      PUT COMMENT IN ELEMENT                       
*                                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
         CLC   DSTSTLBK,=X'FFFF'   WAS THIS THE LAST STATION?                   
         BE    VR30                YES                                          
         ZIC   R0,0(R2)            BUMP TO NEXT STATION FIELD                   
         AR    R2,R0                                                            
         B     VR10                                                             
         DROP  R6                                                               
*                                                                               
VR30     DS    0H                                                               
         MVI   ELCODE,DSTCOMCQ     GENERAL COMMENT ELEMENT                      
         GOTO1 REMELEM                                                          
*                                                                               
         CLI   SFMCOM1H+5,0        ANY GENERAL COMMENTS PRESENT?                
         BE    VR40                NO                                           
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING DSTCOMD,R6                                                       
         MVI   DSTCOMCD,DSTCOMCQ   ELEMENT CODE                                 
         MVI   DSTCOMEL,DSTCOMLQ   ELEMENT LENGTH                               
         MVC   DSTCOML1,SFMCOM1    COMMENT LINE 1                               
         MVC   DSTCOML2,SFMCOM2    COMMENT LINE 1                               
         MVC   DSTCOML3,SFMCOM3    COMMENT LINE 1                               
         GOTO1 ADDELEM                                                          
         DROP  R6                                                               
*                                                                               
VR40     DS    0H                                                               
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
* ADD PASSIVE POINTERS AFTER RECORD ADD OR CHANGE                               
*                                                                               
XA       DS    0H                                                               
XP       DS    0H                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),SYSDIR,SAVEKEY,KEY                   
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                CURRENT ACTIVE POINTER MISSING ?!?           
*                                                                               
         LA    R4,KEY                                                           
         USING DSTKEYD,R4                                                       
         MVC   SVDSKADR,DSTKDA     SAVE DISK ADDRESS IN ACTIVE POINTER          
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,DSTSTACQ     STATION ELEMENT                              
         USING DSTSTAD,R6                                                       
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE AT LEAST ONE OF THESE!               
*                                                                               
XA10     DS    0H                                                               
         XC    KEY,KEY             SEE IF PASSIVE ALREADY EXISTS                
         LA    R4,KEY                                                           
         USING DSTKEYD,R4                                                       
         MVI   DSTKMSYS,DSTKMSYQ   SYSTEM                                       
         MVI   DSTKSYS,DSTKSYSQ    DEMOS                                        
         MVI   DSTKSTYP,DSTKSTYQ   RECORD TYPE                                  
         MVC   DSTKSBF,SVSUBFIL    SUB-FILE                                     
         MVC   DSTKSRC,SVSOURCE    SOURCE                                       
         MVC   DSTKSTN,DSTSTAST    STATION                                      
         MVC   DSTKLSBK,DSTSTLBK   LAST RATINGS BOOK                            
*                                                                               
         MVC   SAVEKEY,KEY                                                      
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),SYSDIR,SAVEKEY,KEY                   
         CLI   8(R1),X'10'                                                      
         BE    XA20                NOT FOUND: WE MUST ADD IT                    
*                                                                               
         CLI   8(R1),0                                                          
         BE    *+6                 IT'S ALREADY THERE                           
         DC    H'0'                FATAL ERROR ON READ                          
         TM    DSTKSTAT,X'40'                                                   
         BO    *+6                                                              
         DC    H'0'                PASSIVE FLAG ISN'T ON ?!?                    
*                                                                               
*                                  UPDATE THE PASSIVE POINTER                   
         GOTO1 DATAMGR,DMCB,(0,=C'DMWRT'),SYSDIR,,KEY                           
         CLI   8(R1),0                                                          
         BE    XA30                LOOK FOR MORE STATIONS                       
         DC    H'0'                                                             
*                                                                               
XA20     DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(32),SAVEKEY     RESTORE THE PASSIVE KEY                      
         MVC   DSTKDA,SVDSKADR     INSERT DISK ADDRESS                          
         OI    DSTKSTAT,X'40'      SET PASSIVE FLAG                             
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMADD'),SYSDIR,,KEY                           
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
XA30     DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BE    XA10                ADD NEXT PASSIVE IF NECESSARY                
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
* DISPLAY RECORD                                                                
*                                                                               
DR       DS    0H                                                               
         BAS   RE,DK               REDISPLAY THE KEY (IN CASE THE USER          
*                                   ENTERED A PASSIVE KEY)                      
*                                                                               
         TWAXC SFMALTSH            CLEAR DATA FIELDS                            
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,DSTSTACQ     STATION ELEMENT                              
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE AT LEAST ONE!                        
*                                                                               
         LA    R2,SFMALTSH         FIRST STATION TWA FIELD                      
*                                                                               
         USING DSTSTAD,R6                                                       
DR10     DS    0H                                                               
         MVC   8(5,R2),DSTSTAST    STATION                                      
         OI    6(R2),X'80'         XMIT                                         
*                                                                               
         ZIC   R0,0(R2)            BUMP TO BOOK FIELD FOR THIS STATION          
         AR    R2,R0                                                            
*                                                                               
         CLC   DSTSTLBK,=X'FFFF'   LAST STATION?                                
         BE    DR20                YES                                          
*                                                                               
         MVC   FULL(2),DSTSTLBK    YEAR/MONTH                                   
         MVI   FULL+2,1            FIRST DAY OF MONTH (FOR DATCON)              
         GOTO1 DATCON,DMCB,(3,FULL),(6,8(R2))                                   
         OI    6(R2),X'80'         XMIT                                         
*                                                                               
DR20     DS    0H                                                               
         ZIC   R0,0(R2)            BUMP TO COMMENT FIELD FOR STATION            
         AR    R2,R0                                                            
         MVC   8(L'DSTSTCOM,R2),DSTSTCOM                                        
         OI    6(R2),X'80'         XMIT COMMENT                                 
*                                                                               
         CLC   DSTSTLBK,=X'FFFF'   LAST STATION?                                
         BE    DR30                YES                                          
*                                                                               
         ZIC   R0,0(R2)            BUMP TO NEXT STATION FIELD                   
         AR    R2,R0                                                            
         BAS   RE,NEXTEL                                                        
         BE    DR10                                                             
         DROP  R6                                                               
*                                                                               
DR30     DS    0H                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,DSTCOMCQ     GENERAL COMMENT ELEMENT                      
         BAS   RE,GETEL                                                         
         BNE   DRX                                                              
         USING DSTCOMD,R6                                                       
         MVC   SFMCOM1,DSTCOML1                                                 
         OI    SFMCOM1H+6,X'80'                                                 
         MVC   SFMCOM2,DSTCOML2                                                 
         OI    SFMCOM2H+6,X'80'                                                 
         MVC   SFMCOM3,DSTCOML3                                                 
         OI    SFMCOM3H+6,X'80'                                                 
         DROP  R6                                                               
*                                                                               
DRX      B     XIT                                                              
         EJECT                                                                  
* DISPLAY KEY                                                                   
*                                                                               
DK       NTR1                                                                   
*                                                                               
         L     R4,AIO              SELECTED RECORD                              
         USING DSTKEYD,R4                                                       
*                                                                               
         LA    RE,SBFTAB                                                        
DK10     CLC   DSTKSBF,8(RE)       FIND SUB-FILE IN TABLE                       
         BE    DK20                GOT IT                                       
         LA    RE,L'SBFTAB(RE)                                                  
         CLI   0(RE),X'FF'                                                      
         BNE   DK10                                                             
         DC    H'0'                INVALID SUB-FILE IN KEY!                     
DK20     MVC   SFMSBF,0(RE)                                                     
         OI    SFMSBFH+6,X'80'                                                  
*                                                                               
         LA    RE,SRCTAB                                                        
DK30     CLC   DSTKSRC,8(RE)       FIND SOURCE IN TABLE                         
         BE    DK40                GOT IT                                       
         LA    RE,L'SRCTAB(RE)                                                  
         CLI   0(RE),X'FF'                                                      
         BNE   DK30                                                             
         DC    H'0'                INVALID SOURCE IN KEY!                       
DK40     MVC   SFMSRC,0(RE)                                                     
         OI    SFMSRCH+6,X'80'                                                  
*                                                                               
         MVC   SFMSTAT,DSTKSTN     STATION                                      
         OI    SFMSTATH+6,X'80'                                                 
*                                                                               
         MVC   FULL(2),DSTKLSBK    LAST BOOK YEAR/MONTH                         
         MVI   FULL+2,1            FIRST DAY OF MONTH (FOR DATCON)              
         GOTO1 DATCON,DMCB,(3,FULL),(6,SFMLSBK)                                 
         OI    SFMLSBKH+6,X'80'                                                 
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
* ON-SCREEN LIST                                                                
*                                                                               
LR       LA    R4,KEY                                                           
         OC    KEY,KEY             FIRST TIME THROUGH?                          
         BNZ   LR10                                                             
*                                                                               
         MVI   DSTKMSYS,DSTKMSYQ   SYSTEM                                       
         MVI   DSTKSYS,DSTKSYSQ    DEMOS                                        
         MVI   DSTKSTYP,DSTKSTYQ   RECORD TYPE                                  
*                                                                               
LR10     GOTO1 HIGH                FIRST RECORD                                 
         CLI   DMCB+8,0                                                         
         BE    LR30                                                             
         DC    H'0'                                                             
*                                                                               
LR20     LA    R4,KEY              NEXT RECORD                                  
         GOTO1 SEQ                                                              
         CLI   DMCB+8,0                                                         
         BE    LR30                                                             
         DC    H'0'                                                             
*                                                                               
LR30     DS    0H                                                               
         CLI   DSTKMSYS,DSTKMSYQ   EXIT AFTER LAST DEMOSTA KEY                  
         BNE   LRX                                                              
         CLI   DSTKSYS,DSTKSYSQ                                                 
         BNE   LRX                                                              
         CLI   DSTKSTYP,DSTKSTYQ                                                
         BNE   LRX                                                              
*                                                                               
         CLI   SVSUBFIL,0          ANY SUB-FILE FILTER?                         
         BE    *+14                NO                                           
         CLC   DSTKSBF,SVSUBFIL    YES: MAKE SURE THIS KEY MATCHES              
         BNE   LR20                                                             
*                                                                               
         CLI   SVSOURCE,0          ANY SOURCE FILTER?                           
         BE    *+14                NO                                           
         CLC   DSTKSRC,SVSOURCE    YES: MAKE SURE THIS KEY MATCHES              
         BNE   LR20                                                             
*                                                                               
         OC    SVSTATN,SVSTATN     ANY STATION FILTER?                          
         BZ    *+14                NO                                           
         CLC   DSTKSTN,SVSTATN     YES: MAKE SURE THIS KEY MATCHES              
         BNE   LR20                                                             
*                                                                               
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   LISTAR,SPACES       CLEAR LIST LINE                              
*                                                                               
         LA    RE,SBFTAB                                                        
LR40     CLC   DSTKSBF,8(RE)       FIND SUB-FILE IN TABLE                       
         BE    LR50                GOT IT                                       
         LA    RE,L'SBFTAB(RE)                                                  
         CLI   0(RE),X'FF'                                                      
         BNE   LR40                                                             
         DC    H'0'                INVALID SUB-FILE IN KEY!                     
LR50     MVC   LSTSUBFL,0(RE)                                                   
*                                                                               
         LA    RE,SRCTAB                                                        
LR60     CLC   DSTKSRC,8(RE)       FIND SOURCE IN TABLE                         
         BE    LR70                GOT IT                                       
         LA    RE,L'SRCTAB(RE)                                                  
         CLI   0(RE),X'FF'                                                      
         BNE   LR60                                                             
         DC    H'0'                INVALID SOURCE IN KEY!                       
LR70     MVC   LSTSRC,0(RE)                                                     
*                                                                               
         MVC   LSTSTATN,DSTKSTN    STATION                                      
*                                                                               
         CLC   DSTKLSBK,=X'FFFF'   ANY BOOK PRESENT?                            
         BNE   *+14                YES                                          
         MVC   LSTLSBK,=C'(CURRENT)'                                            
         B     LR80                                                             
*                                                                               
         MVC   FULL(2),DSTKLSBK    LAST BOOK YEAR/MONTH                         
         MVI   FULL+2,1            FIRST DAY OF MONTH (FOR DATCON)              
         GOTO1 DATCON,DMCB,(3,FULL),(6,LSTLSBK)                                 
*                                                                               
LR80     DS    0H                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,DSTCOMCQ     GENERAL COMMENT ELEMENT                      
         BAS   RE,GETEL                                                         
         BNE   LR90                MAY NOT BE THERE                             
         USING DSTCOMD,R6                                                       
         MVC   LSTCOMM1,DSTCOML1   COMMENT LINE 1                               
         DROP  R6                                                               
*                                                                               
LR90     DS    0H                                                               
         GOTO1 LISTMON             SEND RECORD TO SCREEN                        
         B     LR20                NEXT RECORD                                  
*                                                                               
LRX      B     XIT                                                              
         EJECT                                                                  
INVERR   MVC   GERROR,=AL2(INVALID)                                             
         B     VSFMERR                                                          
*                                                                               
MISSERR  MVC   GERROR,=AL2(MISSING)                                             
         B     VSFMERR                                                          
*                                                                               
DATEERR  MVC   GERROR,=AL2(INVDATE)                                             
         B     VSFMERR                                                          
*                                                                               
VSFMERR  GOTO1 SFMERR                                                           
         SPACE 2                                                                
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                  TABLE OF VALID SUB-FILES                     
SBFTAB   DS    0CL9                                                             
         DC    C'USTV    ',C'T'                                                 
         DC    C'CANTV   ',C'C'                                                 
         DC    C'MPA     ',C'P'                                                 
         DC    C'NETWORK ',C'N'                                                 
         DC    C'RADIO   ',C'R'                                                 
         DC    X'FF'                                                            
*                                  TABLE OF VALID SOURCES                       
SRCTAB   DS    0CL9                                                             
         DC    C'NSI     ',C'N'                                                 
         DC    C'ARB     ',C'A'                                                 
         DC    C'SRC     ',C'S'                                                 
         DC    X'FF'                                                            
*                                  TABLE OF VALID SUBFILE/SOURCE COMBOS         
MSTAB    DS    0CL2                                                             
         DC    C'TN'                                                            
         DC    C'TA'                                                            
         DC    C'TS'                                                            
         DC    C'CN'                                                            
         DC    C'CA'                                                            
         DC    C'RN'                                                            
         DC    C'RA'                                                            
         DC    C'NN'                                                            
         DC    C'PN'                                                            
         DC    X'FF'                                                            
         EJECT                                                                  
* DDSPLWORKD                                                                    
* DDSPOOLD                                                                      
* CTSFMFFD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE CTSFMFFD                                                       
         EJECT                                                                  
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFMA5D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFMA6D                                                       
         EJECT                                                                  
       ++INCLUDE CTGENDSTA                                                      
         EJECT                                                                  
       ++INCLUDE CTSFMWORKD                                                     
         EJECT                                                                  
* START OF SAVED STORAGE (6144)                                                 
*                                                                               
         ORG   SYSSPARE                                                         
SVSBFSRC DS    0CL2                SUB-FILE/SOURCE                              
SVSUBFIL DS    C                   SUB-FILE CODE                                
SVSOURCE DS    C                   RATINGS SOURCE                               
SVSTATN  DS    CL5                 STATION                                      
SAVEKEY  DS    XL32                GENFILE KEY                                  
SVDSKADR DS    XL4                 DISK ADDRESS (FOR PASSIVE POINTERS)          
         SPACE 5                                                                
* ON-SCREEN LIST LINE                                                           
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LSTSUBFL DS    CL7                 SUB-FILE                                     
         DS    CL2                                                              
LSTSRC   DS    CL7                 RATINGS SOURCE                               
         DS    CL2                                                              
LSTSTATN DS    CL5                 STATION                                      
         DS    CL5                                                              
LSTLSBK  DS    CL9                 LAST BOOK OR "(CURRENT)"                     
         DS    CL3                                                              
LSTCOMM1 DS    CL33                GENERAL COMMENT LINE 1                       
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001CTSFM65   09/26/06'                                      
         END                                                                    
