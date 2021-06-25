*          DATA SET TAREP15    AT LEVEL 060 AS OF 10/21/14                      
*PHASE T70315A                                                                  
*INCLUDE TALIM                                                                  
         TITLE 'T70315 - ALLTAX UPDATE'                                         
T70315   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70315                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          SYSTEM SPECIFIC WORK                         
         L     RA,ATWA                                                          
         USING T703FFD,RA          SCREEN                                       
*                                                                               
         SPACE 1                                                                
         GOTO1 INITIAL,DMCB,0                                                   
         CLI   MODE,VALKEY                                                      
         BE    VALSCRN                                                          
         SPACE                                                                  
         CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
         B     PRREC                                                            
*                                                                               
YES      SR    RC,RC               SET CONDITION CODE                           
NO       LTR   RC,RC                                                            
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*              ROUTINE VALIDATES THE SCREEN                                     
         SPACE                                                                  
VALSCRN  DS    0H                                                               
         CLI   ACTEQU,ACTUPD       TEST NOT ACTION UPDATE - REPORT              
         BE    VS2                                                              
         SPACE                                                                  
         GOTO1 RECVAL,DMCB,TLEMCDQ,(X'08',SAREMPH),SAREMPNH  EMPLOYER           
         MVC   EMPN(L'SAREMPN),SAREMPN                                          
         SPACE                                                                  
         LA    R2,SAREDTEH         R2=A(EFFECTIVE DATE)                         
         BAS   RE,VALEFDTE         VALIDATE EFF DATE - RETURNS EFDTE0           
         SPACE                                                                  
VS1      GOTO1 DATCON,DMCB,(0,EFDTE0),(8,EFDTE8)  SET DISPLAYABLE DATE          
         B     XIT                                                              
         SPACE                                                                  
         USING SCRLINED,R3         ACTION UPDATE                                
VS2      MVC   SAVEDTE0,=6X'FF'    INIT EARLIEST EFFECTIVE DATE                 
         SPACE                                                                  
         GOTO1 RECVAL,DMCB,TLEMCDQ,(X'08',TAXEMPH),TAXEMPNH  EMPLOYER           
         MVC   TAXEMP,TGEMP                                                     
         MVC   EMPN(L'TAXEMPN),TAXEMPN                                          
         LA    R3,TAXDATAH                                                      
         LA    R0,(TAXLAST-TAXDATAH)/SLNLNQ  R0=N'LINES                         
         B     VS4                 REQUIRE INPUT IN FIRST LINE                  
         SPACE                                                                  
VS3      XC    SLNNAME,SLNNAME     CLEAR NAME FIELD                             
         OI    SLNNAMEH+6,X'80'                                                 
         BAS   RE,ANYINPUT         ANY INPUT ON THIS LINE                       
         BE    VS15                                                             
VS4      LA    R2,SLNUNITH         VALIDATE UNIT                                
         GOTO1 ANY                                                              
         LA    R1,SLNUNIT                                                       
         BAS   RE,INVSTATE         TEST INVALID STATE                           
         BE    INVERR                                                           
         GOTO1 TAXVAL,DMCB,(2,SLNUNIT)                                          
         BNE   INVERR                                                           
         MVC   SLNNAME,TGTANAME                                                 
         OI    SLNNAMEH+6,X'80'                                                 
         SPACE                                                                  
         LA    R2,SLNRATEH         VALIDATE RATE FIELD                          
         GOTO1 ANY                                                              
         ZIC   RF,5(R2)            RF=L'INPUT                                   
         GOTO1 CASHVAL,DMCB,(3,8(R2)),(RF)                                      
         CLI   0(R1),X'FF'                                                      
         BE    AMTINV                                                           
         SPACE                                                                  
         LA    R2,SLNDATEH         R2=A(EFFECTIVE DATE)                         
         BAS   RE,VALEFDTE         VALIDATE EFF DATE - RETURNS EFDTE0           
         SPACE                                                                  
         CLI   OFFLINE,C'Y'        WHEN REQUESTING REPORT                       
         BE    VS10                                                             
         CLC   TGTODAY0,EFDTE0     CAN'T BE BEFORE TODAY                        
         BH    INVERR                                                           
VS10     GOTO1 DATCON,DMCB,(0,EFDTE0),(10,8(R2)) RE-DISPLAY MM/DD/YY            
         MVI   5(R2),8                                                          
         OI    6(R2),X'80'                                                      
         SPACE                                                                  
VS15     CLC   SAVEDTE0,EFDTE0     SAVE THIS EFFECTIVE DATE IF EARLIEST         
         BNH   *+10                                                             
         MVC   SAVEDTE0,EFDTE0                                                  
         LA    R3,SLNNEXT          BUMP TO NEXT LINE                            
         BCT   R0,VS3                                                           
         SPACE                                                                  
         MVC   EFDTE0,SAVEDTE0     SET EFFECTIVE DATE FOR REPORT                
         GOTO1 DATCON,DMCB,(0,EFDTE0),(8,EFDTE8)  SET DISPLAYABLE DATE          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE VALIDATES EFFECTIVE DATE AT R2 AND RETURNS               
*              THE DATE IN DATCON FORMAT 0 IN EFDTE0                            
         SPACE                                                                  
VALEFDTE NTR1                                                                   
         MVC   EFDTE0,TGTODAY0            USE TODAY'S DATE                      
         GOTO1 DTVAL,DMCB,(X'80',FULL)                                          
         OC    FULL(3),FULL               IF NO INPUT                           
         BZ    XIT                                                              
         GOTO1 DATCON,DMCB,(1,FULL),(0,EFDTE0)  ELSE CONVERT INPUT              
         B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE RETURNS CC EQUAL IF THERE'S NO INPUT ON THE LINE         
*              ELSE RETURNS NOT EQUAL IF THERE IS INPUT                         
         SPACE                                                                  
         USING SCRLINED,R3         R3=A(LINE)                                   
ANYINPUT DS    0H                                                               
         CLI   SLNUNITH+5,0                                                     
         BNER  RE                                                               
         CLI   SLNRATEH+5,0                                                     
         BNER  RE                                                               
         CLI   SLNDATEH+5,0                                                     
         BR    RE                                                               
         SPACE 3                                                                
*              ROUTINE RETURNS CC EQUAL IF THE STATE AT R1 IS INVALID           
*              ELSE RETURNS NOT EQUAL IF IT'S VALID                             
         SPACE                                                                  
INVSTATE DS    0H                                                               
         CLC   =C'FD',0(R1)        INVALID STATES ARE FEDERAL,                  
         BER   RE                                                               
* NO-OP  CLC   =C'PR',0(R1)        PUERTO RICO, (NOT ANYMORE)                   
* NO-OP  BER   RE                                                               
         CLC   =C'VI',0(R1)        VIRGIN ISLANDS,                              
         BER   RE                                                               
         CLC   =C'CN',0(R1)        CANADA,                                      
         BER   RE                                                               
         CLC   =C'OT',0(R1)        AND OTHERS                                   
         BR    RE                                                               
         EJECT                                                                  
*              PROCESS THE RECORD                                               
         SPACE                                                                  
PRREC    DS    0H                                                               
         CLI   OFFLINE,C'Y'        OFFLINE ONLY                                 
         BNE   XIT                                                              
         MVC   TBCHUNK,SPACES      CLEAR PRINT CHUNK                            
         ZAP   TBCNT,=P'0'                                                      
         LA    R1,MYSPECS          SET UP SPECS FOR REPORT                      
         ST    R1,SPECS                                                         
         LA    R1,HOOK             SET UP HEADHOOK                              
         ST    R1,HEADHOOK                                                      
         MVI   RCSUBPRG,2          INIT SPROG FOR REPORT                        
         SPACE                                                                  
         CLI   ACTEQU,ACTUPD       TEST ACTION UPDATE                           
         BNE   PR45                                                             
         MVI   RCSUBPRG,1          SET SPROG                                    
         SPACE                                                                  
         OPEN  (ALLTAXIN,(OUTPUT))                                              
         SPACE 1                                                                
         USING SCRLINED,R3                                                      
         LA    R3,TAXDATAH         NOW LOOP THROUGH DATA INPUT LINES            
         LA    R0,(TAXLAST-TAXDATAH)/SLNLNQ R0=N'LINES                          
         SPACE 1                                                                
PR24     CLI   SLNUNITH+5,0                 TEST FOR INPUT ON THIS LINE         
         BE    PR40                                                             
         MVC   P+1(L'UNILIT),UNILIT                                             
         MVC   P+1+L'UNILIT(2),SLNUNIT      MOVE UNIT CODE SUFFIX               
         MVI   P+1+L'UNILIT+L'SLNUNIT,C'0'  MOVE 0 IN AT END FOR STATE          
         MVC   DUB,P+1+L'UNILIT+3-8         SAVE FULL ALLTAX UNIT CODE          
         BAS   RE,PUTIT                                                         
         SPACE 1                                                                
         LA    R2,SLNRATEH         WRITE OUT A RATE COMMAND                     
         LA    RF,RATELIT                                                       
         BAS   RE,SETLINE1                                                      
         BAS   RE,PUTIT                                                         
         BAS   RE,SETLINE2                                                      
         BAS   RE,PUTIT                                                         
         SPACE 1                                                                
PR40     LA    R3,SLNNEXT                                                       
         BCT   R0,PR24                                                          
         SPACE                                                                  
         CLOSE ALLTAXIN                                                         
         SPACE                                                                  
         MVI   RCSUBPRG,3          SET SPROG FOR REPORT AFTER UPDATE            
         SPACE                                                                  
         USING LINED,R2                                                         
         USING TALUNITD,R3                                                      
         USING TMD,R4                                                           
PR45     LA    R4,LIMBLOCK         R4=A(LIMIT BLOCK)                            
         L     R3,TGAUNITS         R3=A(TAX UNIT TABLE)                         
         LA    R2,TBCHUNK          R2=A(CHUNK TO PRINT)                         
         MVI   FORCEHED,C'Y'       START A NEW PAGE                             
         BAS   RE,SETBOXES         SET UP BOXES                                 
         BAS   RE,INITLIM          INITIALIZE FOR TALIM                         
         SPACE                                                                  
PR50     CLI   0(R3),X'FF'         TEST NOT END OF TABLE                        
         BE    PRX                                                              
         CLI   TALUCODE+2,X'40'    TEST STATE CODE                              
         BH    PR60                                                             
         LA    R1,TALUCODE                                                      
         BAS   RE,INVSTATE         TEST INVALID STATE                           
         BE    PR60                                                             
         MVC   TMUNIT,TALUCODE                                                  
         GOTO1 =V(TALIM),DMCB,(R4)                                              
         BNE   PR60                                                             
         MVC   PSTATE,TALUNAME                                                  
         EDIT  TMRSUI,(7,PRATE),3                                               
         EDIT  TMBSUI,(12,PWGBASE),2                                            
         BAS   RE,CHUNK                                                         
PR60     LA    R3,TALUNEXT                                                      
         B     PR50                                                             
         SPACE                                                                  
PRX      BAS   RE,PRINTIT          PRINT LAST LINE OF DATA                      
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE PUTS A LINE TO DATASET AND PRINTS IT                     
         SPACE                                                                  
PUTIT    NTR1                                                                   
         PUT   ALLTAXIN,P+1                                                     
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE PRINTS A LINE                                            
         SPACE                                                                  
PRINTIT  NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   TBNXTCHK,0                                                       
         B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE PLACES CHUNK IN PRINT LINE                               
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
         LA    R1,P+15(R1)                                                      
         MVC   0(LINLNQ,R1),TBCHUNK  MOVE TO PRINT LINE                         
         MVC   TBCHUNK,SPACES        AND CLEAR                                  
         AP    TBCNT,=P'1'           INCREMENT COUNTER                          
         B     XIT                                                              
         SPACE 3                                                                
*              INITIALIZE TALIM BLOCK                                           
         SPACE 1                                                                
         USING TMD,R4              R4=A(TALIM BLOCK)                            
INITLIM  NTR1                                                                   
         LA    R0,LIMBLOCK                                                      
         LHI   R1,L'LIMBLOCK                                                    
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         ST    RC,TMRC                                                          
         MVC   TMEFDTE0,EFDTE0                                                  
         MVC   TMEMP,TGEMP                                                      
         OI    TMSTAT,TMSBSRT                                                   
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE SETS 1ST LINE OF COMMAND                                 
         SPACE 1                                                                
*                                  R2=A(AMOUNT FIELD HEADER)                    
*                                  RF=A(CONSTANT NAME)                          
*                                  DUB=ALLTAX UNIT CODE                         
SETLINE1 NTR1                                                                   
         MVC   P+1(L'LINE1),LINE1            BASE OF 1ST LINE                   
         MVC   P+1+DSPLIT1(L'LITERALS),0(RF) CONSTANT LITERAL                   
         MVC   P+1+DSPUNIT(8),DUB            ALLTAX UNIT CODE                   
         MVC   P+1+DSPEMP1(3),TAXEMP         EMPLOYER                           
         SPACE                                                                  
         LA    R3,P+1+L'LINE1      R3=A(POSITION IN LINE1 FOR RATE)             
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),8(R2)       AMOUNT                                       
         AR    R3,R1                                                            
         MVI   1(R3),C'%'          MOVE IN %                                    
         B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE SETS 2ND LINE OF COMMAND                                 
         SPACE 1                                                                
*                                  RF=A(CONSTANT NAME)                          
         USING SCRLINED,R3            R3=A(SCREEN LINE)                         
SETLINE2 NTR1                                                                   
         MVC   P+5(L'LINE2),LINE2              BASE OF 2ND LINE                 
         MVC   P+5+DSPDATE(8),SLNDATE          EFFECTIVE DATE                   
         MVC   P+5+DSPNAME(8),SLNNAME          UNIT NAME                        
         MVC   P+5+DSPLIT2(4),L'LITERALS-4(RF) CONSTANT LITERAL                 
         MVC   P+5+DSPEMP2(3),TAXEMP           EMPLOYER                         
         GOTO1 SQUASHER,DMCB,P+5,L'LINE2                                        
         B     XIT                                                              
         EJECT                                                                  
*              HEADHOOK                                                         
         SPACE                                                                  
HOOK     NTR1                                                                   
         MVC   HEAD4+15(L'TGEMP),TGEMP      SET EMPLOYER                        
         MVC   HEAD4+24(L'EMPN),EMPN        AND NAME                            
         SPACE                                                                  
         CLI   RCSUBPRG,1                   DONE IF SPROG 1                     
         BE    XIT                                                              
         MVC   HEAD5+15(L'EFDTE8),EFDTE8    SET EFFECTIVE DATE                  
         B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE SETS UP BOXES FOR THE REPORT AT THE VERY END             
         SPACE                                                                  
SETBOXES NTR1                                                                   
         MVC   HEAD7+15(L'HEADING),HEADING  SET UP HEADING FOR REPORT           
         SPACE                                                                  
         L     R4,ABOX             SET UP BOXES                                 
         USING BOXD,R4                                                          
         MVC   BOXROWS,SPACES      SET ROWS                                     
         MVI   BOXROWS+5,C'T'      TOP                                          
         MVI   BOXROWS+7,C'M'      MIDDLE                                       
         MVI   BOXROWS+32,C'B'     BOTTOM                                       
*                                                                               
         MVC   BOXCOLS,SPACES      SET COLUMNS                                  
         LA    R2,BOXCOLS+15                                                    
         USING LINED,R2            USE PRINT LINE DSECT                         
         LA    R0,NCHUNKS                                                       
*                                                                               
SB4      MVI   BL,C'L'                                                          
         MVI   BC1,C'C'                                                         
         MVI   BC2,C'C'                                                         
         MVI   BR,C'R'                                                          
         LA    R2,LINNEXT                                                       
         BCT   R0,SB4                                                           
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
DATEINV  MVI   ERROR,INVDATE       INVALID DATE                                 
         B     ERXIT                                                            
*                                                                               
AMTINV   MVI   ERROR,ERINVAMT      INVALID AMOUNT                               
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
         EJECT                                                                  
*              CONSTANTS ETC.                                                   
         SPACE                                                                  
NCHUNKS  EQU   3                                                                
ACTUPD   EQU   15                   ACTEQU FOR UPDATE                           
         SPACE                                                                  
HEADING  DC    C'  State   Rate (%)   Wage Base     State   Rate (%)   X        
               Wage Base     State   Rate (%)   Wage Base'                      
         SPACE                                                                  
*              DCB TO COVER COMMAND FILE TO ALLTAX PROGRAM                      
         SPACE 1                                                                
ALLTAXIN DCB   DDNAME=ALLTAXIN,DSORG=PS,RECFM=FB,LRECL=80,BLKSIZE=80,  X        
               MACRF=PM                                                         
         SPACE 2                                                                
*              BASIC ALLTAX COMMANDS                                            
         SPACE 1                                                                
UNILIT   DC    C'*$*$*$ ATTXU'                                                  
LINE1    DC    C'ENTER <          > FOR <        > FOR <   > AS '               
LINE2    DC    C'EFFECTIVE-DATE          NOTE <         SUTA      FOR  X        
                 >;'                                                            
         SPACE 1                                                                
*                                  DISPLACEMENTS TO AREAS IN COMMANDS           
DSPLIT1  EQU   7                   CONSTANT LITERAL                             
DSPUNIT  EQU   24                  ALLTAX UNIT CODE                             
DSPEMP1  EQU   39                  EMPLOYER ON 1ST LINE                         
DSPDATE  EQU   15                  EFFECTIVE DATE                               
DSPNAME  EQU   30                  UNIT NAME                                    
DSPLIT2  EQU   44                  LAST 4 CHARS OF ALLTAX UNIT CODE             
DSPEMP2  EQU   L'LINE2-5           EMPLOYER ON 2ND LINE                         
         SPACE 1                                                                
LITERALS DS    0CL10               CONSTANT LITERALS                            
RATELIT  DC    C'ERUNMPRATE'       UNEMPLOYMENT RATE                            
BASELIT  DC    C'ERUNMPWGBS'       UNEMPLOYMENT WAGE BASE                       
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              SPECS FOR SPOOLING                                               
         SPACE                                                                  
MYSPECS  DS    0H                                                               
         SPROG 1,2,3                                                            
         SSPEC H1,1,RUN                                                         
         SSPEC H1,107,REPORT                                                    
         SSPEC H1,123,PAGE                                                      
         SSPEC H2,107,REQUESTOR                                                 
         SSPEC H4,1,C'Employer'                                                 
         SSPEC H2,59,13X'BF'                                                    
         SPROG 1,3                                                              
         SSPEC H1,59,C'Alltax Update'                                           
         SPROG 2                                                                
         SSPEC H1,59,C'Alltax Report'                                           
         SPROG 2,3                                                              
         SSPEC H5,1,C'Effective Date'                                           
         DC    X'00'                                                            
*                                                                               
         EJECT                                                                  
*              DSECT TO COVER SCREEN LINE                                       
         SPACE 1                                                                
SCRLINED DSECT                                                                  
SLNUNITH DS    CL8                                                              
SLNUNIT  DS    CL2                 UNIT CODE                                    
SLNNAMEH DS    CL8                                                              
SLNNAME  DS    CL8                 UNIT NAME                                    
SLNRATEH DS    CL8                                                              
SLNRATE  DS    CL12                UNEMPLOYMENT WAGE RATE                       
SLNDATEH DS    CL8                                                              
SLNDATE  DS    CL8                 EFFECTIVE DATE                               
SLNBASEH DS    CL8                                                              
SLNBASE  DS    CL12                UNEMPLOYMENT WAGE BASE-NOT USED NOW          
SLNLNQ   EQU   *-SCRLINED                                                       
SLNNEXT  EQU   *                                                                
         SPACE 3                                                                
LINED    DSECT                     DSECT FOR PRINT LINE                         
BL       DS    CL1                                                              
PSTATE   DS    CL8                 STATE NAME                                   
BC1      DS    CL1                                                              
PRATE    DS    CL7                 RATE                                         
         DS    CL1                                                              
BC2      DS    CL1                                                              
PWGBASE  DS    CL12                WAGE BASE                                    
BR       DS    CL1                                                              
         DS    CL1                                                              
LINLNQ   EQU   *-LINED                                                          
LINNEXT  EQU   *                                                                
         EJECT                                                                  
*                                                                               
       ++INCLUDE TAREPFFD                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPE4D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPE5D                                                       
         ORG   TAXWORK             ORG ONTO END OF LARGEST SCREEN               
         SPACE 2                                                                
TBNXTCHK DS    XL1                 INDICATOR FOR CHUNK PRINTING                 
TBCHUNK  DS    CL(LINLNQ)          TEMP AREA FOR PRINTING                       
TBCNT    DS    PL4                 # RECORDS PRECESSED                          
         SPACE                                                                  
EFDTE0   DS    CL6                 EBCDIC EFF DATE FOR ACTION REPORT            
EFDTE8   DS    CL8                 DISPLAYABLE EFF DATE                         
SAVEDTE0 DS    CL6                 EARLIEST EFF DATE FOR UPDATE                 
EMPN     DS    CL36                EMPLOYER NAME                                
         SPACE                                                                  
LIMBLOCK DS    CL(TMLNQ)           TALIM BLOCK                                  
         EJECT                                                                  
       ++INCLUDE TALIMD                                                         
       DSECT                                                                    
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
**PAN#1  DC    CL21'060TAREP15   10/21/14'                                      
         END                                                                    
