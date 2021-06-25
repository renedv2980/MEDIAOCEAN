*          DATA SET TAREP33    AT LEVEL 016 AS OF 04/08/14                      
*PHASE T70333A,*                                                                
         TITLE 'T70333 - GUARANTEE BALANCE REFRESH'                             
T70333   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70333                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          SYSTEM SPECIFIC WORK                         
         L     RA,ATWA                                                          
         USING T703FFD,RA           SCREEN                                      
*                                                                               
         SPACE 1                                                                
         GOTO1 INITIAL,DMCB,0                                                   
         CLI   MODE,VALKEY                                                      
         BE    VS                                                               
         CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
         BAS   RE,INITPR                                                        
         B     PRREC                                                            
*                                                                               
YES      SR    RC,RC               SET CONDITION CODE                           
NO       LTR   RC,RC                                                            
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*                                                                               
*              VALIDATE SCREEN                                                  
*                                                                               
VS       DS    0H                                                               
         BAS   RE,INIT                                                          
         LA    R2,SGTSSNH          SSN                                          
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'08',(R2)),SGTSSNNH                        
         LA    R2,SGTCODEH         GRT CODE                                     
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
         MVC   TGGUA,8(R2)         MOVE CODE TO GLOBAL                          
         MVC   GRTNC,8(R2)              (SAVE IT UNCOMPLEMENTED)                
         XC    TGGUA,HEXFFS        AND UNCOMPLEMENT IT                          
         GOTO1 RECVAL,DMCB,TLGUCDQ,(X'A0',0)                                    
         BNE   ERXIT                                                            
*                                                                               
         LA    R3,BLOCK                                                         
         USING PERVALD,R3                                                       
         LA    R2,SGTPDH           PERIOD                                       
         L     R4,AIO              FINDS OUT IF PERIOD REQUIRED                 
         USING TAGUD,R4                                                         
         MVI   ELCODE,TAGUELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   GUSTAT,TAGUSTAT     SAVE STATUS BYTE                             
         CLI   5(R2),0                                                          
         BNE   VS05                                                             
         OC    TAGUCOM,TAGUCOM     IF COMMERCIAL ON GRT                         
         BNZ   MISSERR             THEN PERIOD IS REQUIRED                      
         MVI   5(R2),17            ELSE SET LENGTH OF PERIOD FIELD              
         GOTO1 DATCON,DMCB,(X'11',TAGUSTRT),(5,8(R2))                           
*                                                                               
VS05     GOTO1 PDVAL,DMCB,(R3)                                                  
         OC    TAGUCOM,TAGUCOM     IF COMMERCIAL ON GRT                         
         BZ    VS20                                                             
         L     R4,AIO                                                           
         MVI   ELCODE,TAGCELQ                                                   
         MVC   WORK(3),PVALPSTA    SET SEARCH ARGUMENT TO VALIDATE              
         MVC   WORK+3(3),PVALPEND  PERIOD                                       
         GOTO1 GETL,DMCB,(6,WORK)                                               
         BNE   INVERR                                                           
*                                                                               
         MVC   ENDDTE,PVALPEND                                                  
         XC    ENDDTE,HEXFFS       COMPLEMENTED                                 
         MVC   STRDTE,PVALPSTA                                                  
         XC    STRDTE,HEXFFS       COMPLEMENTED                                 
*                                                                               
VS20     LA    R2,SGTBALH          STARTING BALANCE                             
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
         ZIC   R3,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,8(R2),(R3)                                          
         CLI   0(R1),0                                                          
         BNE   INVERR                                                           
         MVC   BALANCE,4(R1)     AMOUNT RETURNED FROM CASHVAL                   
*                                                                               
         LA    R2,SGTINVH          STARTING INVOICE NUMBER                      
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
         GOTO1 TINVCON,DMCB,8(R2),INVOICE,DATCON                                
         CLI   0(R1),X'FF'                                                      
         BE    INVERR                                                           
*                                                                               
VSX      B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
*        INITIALIZE                                                             
*                                                                               
INIT     NTR1                                                                   
         XC    STRGBLK(STRGLNQ),STRGBLK                                         
         LA    RF,DSKLNQ                                                        
         XCEFL DSKTAB              CLEAR D/A TABLE                              
         B     XIT                                                              
         SPACE 2                                                                
*                                                                               
*        INITIALIZE                                                             
*                                                                               
INITPR   NTR1                                                                   
         MVC   TBCHUNK,SPACES      CLEAR PRINT CHUNK                            
         ZAP   TBCNT,=P'0'                                                      
         LA    R1,MYSPECS          SET UP SPECS FOR REPORT                      
         ST    R1,SPECS                                                         
         LA    R1,HOOK             SET UP HEADHOOK                              
         ST    R1,HEADHOOK                                                      
         MVI   RCSUBPRG,0                                                       
         CLI   OFFLINE,C'Y'                                                     
         BE    *+8                                                              
         MVI   RCSUBPRG,1                                                       
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*              PROCESS THE RECORD                                               
*                                                                               
PRREC    DS    0H                                                               
         LA    R3,DSKTAB                                                        
         ST    R3,THISTAB                                                       
         LA    R4,DSKX             END OF TABLE                                 
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING TLGTD,R2                                                         
         MVI   TLGTCD,TLGTCDQ      READ GUAR TRACKING REC                       
         MVC   TLGTSSN,TGSSN                                                    
         MVC   TLGTGUA,GRTNC       UN COMPLEMENTED GRT #                        
         MVC   TLGTSTRT,STRDTE     START DATE (OR X'0000000')                   
         MVC   TLGTEND,ENDDTE      END DATE          "                          
         GOTO1 HIGH                                                             
         CLC   KEY(TLGTTRK-TLGTD),KEYSAVE                                       
         BNE   PRX                 IF FIRST TIME THROUGH                        
         CLI   OFFLINE,C'Y'                                                     
         BE    PR20                & ONLINE                                     
         LA    R2,KEY              ONLY ALLOW 200 RECORDS                       
         MVC   NUMREC,TLGTTRK                                                   
         XC    NUMREC,=X'FFFF'     TO BE PROCESSED 'NOW'                        
         CLC   NUMREC,=H'200'                                                   
         BH    NOTNOW                                                           
         B     PR20                                                             
*                                                                               
PR10     GOTO1 SEQ                                                              
         CLC   TLGTKEY(TLGTTRK-TLGTD),KEYSAVE                                   
         BNE   PR40                                                             
*                                                                               
         USING TLDRD,R2                                                         
PR20     MVC   0(4,R3),TLDRDA      SAVE THIS DISK ADDRESS                       
         LA    R3,4(R3)            BUMP TO NEXT ENTRY IN TABLE                  
         ST    R3,THISTAB                                                       
         CR    R3,R4               IF REACH END OF TABLE                        
         BL    PR10                                                             
         DC    H'0'                DIE                                          
*                                                                               
PR40     BAS   RE,GETGT            GET TLGT REC & TAGT ELEM                     
         BNE   PRX                                                              
         USING TAGTD,R4                                                         
         L     R4,FULL             A(TAGTEL)                                    
         CLC   TAGTINV,INVOICE     IF WE REACHED THE START INVOICE              
         BNE   PR40                                                             
         MVC   TAGTBAL,BALANCE     SET INVOICE WITH NEW BALANCE                 
         B     PR100               AND GO WRITE IT BACK                         
*                                                                               
PR70     BAS   RE,GETGT            GET TLGT REC & TAGT ELEM                     
         BNE   PRX                                                              
         USING TAGTD,R4                                                         
         L     R4,FULL                                                          
         L     R1,PREVBAL          THE PREVIOUS BALANCE                         
         TM    GUSTAT,TAGUSDES     IF THIS ISN'T A DESCENDING BALANCE           
         BO    PR80                                                             
         S     R1,TAGTCRD          - THIS CREDIT AMOUNT                         
         B     PR90                                                             
*                                                                               
PR80     A     R1,TAGTCRD          ELSE + THIS CREDIT AMOUNT                    
*                                                                               
PR90     ST    R1,TAGTBAL          = THIS BALANCE                               
*                                                                               
PR100    MVC   PREVBAL,TAGTBAL     AND NEXT RECORD'S 'PREV' BAL                 
         OI    TAGTSTAT,TAGTSREF   RECORD CHANGED BY REFRESH                    
         GOTO1 ACTVIN,DMCB,0                                                    
         GOTO1 PUTREC              & WRITE BACK THE RECORD                      
         BAS   RE,PRREP                                                         
         B     PR70                                                             
*                                                                               
PRX      BAS   RE,PRINTIT          PRINT LAST LINE OF DATA                      
         MVI   SPACING,2                                                        
         BAS   RE,BOXBOT                                                        
         BAS   RE,PRINTIT                                                       
         BAS   RE,UPDGT            UPDATE THE GUARANTEE RECORD                  
         USING LINED,R2            USE PRINT LINE DSECT                         
         LA    R2,P                                                             
         MVC   PINV(L'LTTOT),LTTOT                                              
         EDIT  TBCNT,(5,PCRD+9),COMMAS=YES,ALIGN=LEFT                           
         BAS   RE,PRINTIT                                                       
*                                                                               
         TM    WHEN,X'40'          IF SPOOLING AND NOW                          
         BZ    XIT                                                              
         XC    CONSERV,CONSERV     AUTO $DQU                                    
         MVC   CONSERV(4),=C'$DQU'                                              
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*              SET D/A IN KEY & FIND THE TAGT ELEMENT                           
*                                                                               
GETGT    NTR1                                                                   
         L     R3,THISTAB                                                       
*                                                                               
GT10     SH    R3,=H'4'            GET PREV D/A (READ TABLE BACKWARDS)          
         LA    R1,DSKTAB                                                        
         CR    R3,R1                                                            
         BNL   GT20                                                             
         B     NO                                                               
*                                                                               
GT20     XC    KEY,KEY             CLEAR KEY                                    
         LA    R2,KEY                                                           
         USING TLDRD,R2                                                         
         MVC   TLDRDA,0(R3)        SET D/A IN KEY                               
         MVI   RDUPDATE,C'Y'       SET READ FOR UPDATE                          
         GOTO1 GETREC                                                           
         L     R4,AIO              FIND TAGTELQ                                 
         MVI   ELCODE,TAGTELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   GT10                                                             
         ST    R4,FULL                                                          
         ST    R3,THISTAB                                                       
         B     YES                                                              
         EJECT                                                                  
*                                                                               
*              GET GRT RECORD & UPDATE THE BALNACE                              
*                                                                               
UPDGT    NTR1                                                                   
         GOTO1 RECVAL,DMCB,TLGUCDQ,(X'B0',0)                                    
         BE    *+6                 ALREADY READ THIS RECORD                     
         DC    H'0'                                                             
         L     R4,AIO              FIND TAGUELQ                                 
         USING TAGUD,R4                                                         
         MVI   ELCODE,TAGUELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   UG10                                                             
         OI    TAGUSTAT,TAGUSREF   SET GRT WAS REFRESHED                        
         OC    TAGUCOM,TAGUCOM     IF NO COMMERCIAL IS PRESENT                  
         BNZ   UG10                                                             
         MVC   TAGUBAL,PREVBAL     UPDATE TAGUBAL                               
         B     UGX                                                              
*                                                                               
UG10     L     R4,AIO              ELSE UPDATE TAGCBAL                          
         USING TAGCD,R4                                                         
         MVI   ELCODE,TAGCELQ                                                   
         MVC   WORK(3),STRDTE      SET SEARCH ARGUMENT TO VALIDATE              
         MVC   WORK+3(3),ENDDTE    PERIOD                                       
         XC    WORK(6),HEXFFS      UN-COMPLEMENTED                              
         GOTO1 GETL,DMCB,(6,WORK)                                               
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,TGELEM                                                        
         MVC   TAGCBAL,PREVBAL     WITH UPDATED BALANCE                         
*                                                                               
UGX      GOTO1 ACTVIN,DMCB,0                                                    
         GOTO1 PUTREC                                                           
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        PRINT REPORT                                                           
*        R4 - TAGTELQ                                                           
*                                                                               
         USING TAGTD,R4                                                         
PRREP    NTR1                                                                   
         LA    R2,TBCHUNK                                                       
         USING LINED,R2                                                         
         GOTO1 TINVCON,DMCB,TAGTINV,PINV,DATCON                                 
         EDIT  TAGTCRD,(11,PCRD),2,COMMAS=YES,MINUS=YES                         
         EDIT  TAGTBAL,(13,PBAL),2,COMMAS=YES,MINUS=YES                         
         BAS   RE,CHUNK                                                         
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*              ROUTINE PLACES CHUNK IN PRINT LINE                               
*                                                                               
         SPACE 1                                                                
CHUNK    NTR1                                                                   
         CLI   TBNXTCHK,NCHUNKS    IF WE'RE ABOUT TO EXCEED MAX.                
         BL    *+8                                                              
         BAS   RE,PRINTIT          PRINT PREVIOUS LINE                          
         SPACE 1                                                                
         ZIC   R1,TBNXTCHK                                                      
         LA    RF,1(R1)            BUMP NEXT CHUNK IND.                         
         STC   RF,TBNXTCHK                                                      
         SPACE 1                                                                
         MH    R1,=AL2(LINLNQ)     R1=DISP. INTO P FOR THIS CHUNK               
         LA    R1,P(R1)                                                         
         MVC   0(LINLNQ,R1),TBCHUNK  MOVE TO PRINT LINE                         
         MVC   TBCHUNK,SPACES        AND CLEAR                                  
         AP    TBCNT,=P'1'         INCREMENT COUNTER                            
         B     XIT                                                              
         SPACE 3                                                                
*                                                                               
*        PRINT A LINE                                                           
*                                                                               
         SPACE 1                                                                
PRINTIT  NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   TBNXTCHK,0                                                       
         MVI   SPACING,1           RESET SPACING                                
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        INSERT A BOTTOM LINE                                                   
*                                                                               
         SPACE 1                                                                
BOXBOT   NTR1                                                                   
         CLI   OFFLINE,C'Y'        BOXES OFFLINE ONLY                           
         BNE   XIT                                                              
         L     R3,ABOX                                                          
         USING BOXD,R3                                                          
         MVC   BOXROWS,SPACES                                                   
         ZIC   RF,LINE                                                          
         LA    RF,BOXROWS-1(RF)                                                 
         MVI   0(RF),C'B'                                                       
         MVI   BOXINIT,0                                                        
         B     XIT                                                              
         DROP  R3                                                               
         SPACE 2                                                                
*                                                                               
*        HEADHOOK                                                               
*                                                                               
HOOK     NTR1                                                                   
         MVC   HEAD4+10(L'TGSSN),TGSSN     SET SSN                              
         MVC   HEAD4+20(L'SGTSSNN),SGTSSNN                                      
         MVC   HEAD4+113(L'SGTPD),SGTPD    PERIOD                               
         MVC   HEAD5+10(L'GRTNC),GRTNC     GRT CODE                             
*                                                                               
         CLI   OFFLINE,C'Y'        BOXES OFFLINE ONLY                           
         BNE   XIT                                                              
         L     R4,ABOX             SET UP BOXES                                 
         USING BOXD,R4                                                          
         MVC   BOXROWS,SPACES      SET ROWS                                     
         MVI   BOXROWS+7,C'T'      TOP                                          
         MVI   BOXROWS+9,C'M'      MIDDLE                                       
         MVI   BOXROWS+60,C'B'     BOTTOM                                       
*                                                                               
         MVC   BOXCOLS,SPACES      SET COLUMNS                                  
         LA    R2,BOXCOLS                                                       
         USING LINED,R2            USE PRINT LINE DSECT                         
         LA    R0,NCHUNKS                                                       
*                                                                               
HK4      MVI   BL,C'L'                                                          
         MVI   BC1,C'C'                                                         
         MVI   BC2,C'C'                                                         
         MVI   BR,C'R'                                                          
         LA    R2,LINNEXT                                                       
         BCT   R0,HK4                                                           
*                                                                               
         MVI   BOXYORN,C'Y'        INITIALIZE REMAINING FLDS                    
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
         B     ERXIT                                                            
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
         B     ERXIT                                                            
*                                                                               
STRINV   MVI   ERROR,ERINVSDT      INVALID START DATE                           
         B     ERXIT                                                            
*                                                                               
ENDINV   MVI   ERROR,ERINVEDT      INVALID END DATE                             
         B     ERXIT                                                            
*                                                                               
NOTNOW   MVI   ERROR,NOTONLIN      CANNOT PROCESS ON LINE                       
         LA    R2,CONRECH                                                       
         B     ERXIT                                                            
*                                                                               
ERXIT    DS    0H                                                               
         GOTO1 ERREX                                                            
         SPACE 2                                                                
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*                                                                               
*        CONSTANTS ETC.                                                         
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
*                                                                               
NCHUNKS  EQU   3                                                                
HEXFFS   DC    6X'FF'                                                           
LTTOT    DC    C'Records changed:'                                              
*                                                                               
*        SPECS                                                                  
*                                                                               
MYSPECS  DS    0H                                                               
         SPROG 0,1                                                              
         SSPEC H1,1,RUN                                                         
         SSPEC H1,107,REPORT                                                    
         SSPEC H1,123,PAGE                                                      
         SSPEC H2,107,REQUESTOR                                                 
         SPACE 1                                                                
         SSPEC H1,59,C'Guarantee Refresh'                                       
         SSPEC H2,59,17X'BF'                                                    
         SSPEC H4,1,C'S/S Num'                                                  
         SSPEC H4,107,C'Period'                                                 
         SSPEC H5,1,C'Code'                                                     
         SSPEC H9,001,C' Invoice    Credit      Balance'                        
         SSPEC H9,037,C' Invoice    Credit      Balance'                        
         SSPEC H9,073,C' Invoice    Credit      Balance'                        
         SPROG 1                                                                
         SSPEC H10,001,C' -------    ------      -------'                       
         SSPEC H10,037,C' -------    ------      -------'                       
         SSPEC H10,073,C' -------    ------      -------'                       
         DC    X'00'                                                            
*                                                                               
         EJECT                                                                  
*                                                                               
LINED    DSECT                                                                  
BL       DS    CL1                                                              
PINV     DS    CL7                 INVOICE NUMBER                               
BC1      DS    CL1                                                              
PCRD     DS    CL11                CREDIT                                       
BC2      DS    CL1                                                              
PBAL     DS    CL13                BALANCE                                      
BR       DS    CL1                                                              
         DS    CL1                                                              
LINLNQ   EQU   *-LINED             L'CHUNK                                      
LINNEXT  EQU   *                                                                
         EJECT                                                                  
*                                                                               
       ++INCLUDE TAREPFFD                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPD3D                                                       
         EJECT                                                                  
         ORG   SGTWORK                                                          
         SPACE 2                                                                
STRGBLK  DS    0H                                                               
NUMREC   DS    H                   NUMBER OF RECORDS (GTTRK)                    
GUSTAT   DS    XL1                 TAGUSTAT                                     
STRDTE   DS    PL3                 COMPLEMENTED                                 
ENDDTE   DS    PL3                                                              
INVOICE  DS    PL6                                                              
GRTNC    DS    CL4                 GUARANTEE # - NOT COMPLEMENTED               
*                                                                               
TBNXTCHK DS    XL1                 INDICATOR FOR CHUNK PRINTING                 
TBCHUNK  DS    CL(LINLNQ)          TEMP AREA FOR PRINTING                       
TBCNT    DS    PL4                 # RECORDS PRECESSED                          
*                                                                               
BALANCE  DS    F                                                                
PREVBAL  DS    F                   PREVIOUS INVOICE'S BALANCE                   
THISTAB  DS    F                   THIS TABLE ENTRY                             
STRGLNQ  EQU   *-STRGBLK           STORAGE LENGTH                               
*                                                                               
         DS    0D                                                               
         DC    CL8'*DSKTAB*'                                                    
DSKTAB   DS    500XL4              TABLE OF 500 DISK ADDRESSES                  
DSKX     EQU   *                                                                
DSKLNQ   EQU   *-DSKTAB                                                         
         EJECT                                                                  
*DDPERVAL                                                                       
*DDSPOOLD                                                                       
*DDBIGBOX                                                                       
*DDSPLWORKD                                                                     
*TAGENFILE                                                                      
*TASYSDSECT                                                                     
*TASYSEQUS                                                                      
*TAREPWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAREPWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016TAREP33   04/08/14'                                      
         END                                                                    
