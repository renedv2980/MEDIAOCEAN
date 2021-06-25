*          DATA SET TAGENF9    AT LEVEL 169 AS OF 08/11/14                      
*PHASE T702F9E,*                                                                
         TITLE 'T702F9 - WCHECK'                                                
T702F9   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 YTDTLNQ,T702F9,R8,R7,R6                                          
         LR    RE,RC                                                            
         SPACE 1                                                                
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         ST    RE,AYTDTAB                                                       
         EJECT                                                                  
* INITALIZE OVERLAY AND PASS CONTROL TO CORRECT MODE CONTROL ROUTINE.           
*                                                                               
         GOTO1 INITIAL,DMCB,PFTAB  INITIALIZE OVERLAY                           
         TM    TGSYSTAT,TASYSPID                                                
         BZ    INIT50                                                           
INIT10   MVC   SCKSHED(7),=C'Pid Num'                                           
         OI    SCKSHEDH+6,X'80'                                                 
         ST    RD,SAVERD           SAVE RD FOR RETURNING TO GENCON              
*                                                                               
INIT50   BAS   RE,SETCHK           SET SYSDIR/SYSFIL FOR CHECKS                 
*                                                                               
         CLI   MODE,DISPKEY        MODE DISPKEY                                 
         BE    DISPKY                                                           
         CLI   MODE,VALKEY         MODE VALKEY                                  
         BE    VALKY                                                            
         CLI   MODE,DISPREC        MODE DISPREC                                 
         BE    DISPRC                                                           
         B     XIT                                                              
         EJECT                                                                  
* ROUTINE TO HANDLE MODE DISPKEY.                                               
*                                                                               
DISPKY   DS    0H                                                               
         OI    SCKCHKH+6,X'80'     TRANSMIT CHECK NUMBER FIELD                  
         XC    SCKCHK,SCKCHK       PRE-CLEAR                                    
*                                                                               
         MVC   SCKCHK(3),=C'N/A'   INIT CHECK NUMBER TO N/A                     
*                                                                               
         L     R4,AIO              R4 = A(CHECK DETAILS ELEMENT)                
         MVI   ELCODE,TACDELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   DK20                                                             
         USING TACDD,R4                                                         
*                                                                               
         OC    TACDCHK,TACDCHK     IF THERE IS A CHECK NUMBER                   
         BZ    DK20                                                             
         MVC   SCKCHK,TACDCHK      DISPLAY IT                                   
         MVC   TGCHK,TACDCHK       AND SAVE IN GLOBAL STORAGE                   
*                                                                               
DK20     DS    0H                                                               
DKX      B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
* ROUTINE TO HANDLE MODE VALKEY.                                                
*                                                                               
VALKY    DS    0H                                                               
         CLI   ACTNUM,ACTDIS       ACTION DISPLAY                               
         BE    VKCHK                                                            
*                                                                               
VALKYX   DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
* ROUTINE TO HANDLE THE ACTIONS DISPLAY, SELECT, VOID, REISSUE, AND             
* ISSUE UNDER MODE VALKEY                                                       
*                                                                               
VKCHK    DS    0H                                                               
         LA    R2,SCKCHKH          VALIDATE CHECK NUMBER                        
*                                                                               
         TM    SCKCHKH+4,X'20'     IF FIELD ALREADY VALID                       
         BO    VKX                 THEN RETURN                                  
*                                                                               
         MVI   ASKED,C'N'          SET USER HAS NOT BEEN ASKED TO PF19          
*                                                                               
         CLI   5(R2),0             AND NOTHING INPUT                            
         BNE   VK10                                                             
         OC    TGCHK,TGCHK         AND THERE'S A GLOBAL NUMBER                  
         BZ    VK10                                                             
         MVC   8(L'TLCKCCHK,R2),TGCHK  USE IT                                   
         OI    6(R2),X'80'                                                      
         MVI   5(R2),L'TLCKCCHK                                                 
*                                                                               
VK10     GOTO1 ANY                 INPUT REQUIRED                               
         CLI   5(R2),L'TLCKCCHK    INSURE FULL CHECK NUMBER INPUT               
         BNE   ERRINV                                                           
         MVC   TGCHK,WORK          SAVE UNCOMPLEMENTED IN GLOBAL                
*                                                                               
         LA    R3,KEY              R3 = A(CHECK KEY)                            
         USING TLCKPD,R3                                                        
         XC    KEY,KEY                                                          
         MVI   TLCKPCD,TLCKCCDQ    RECORD CODE                                  
         MVC   TLCKCCHK,TGCHK      CHECK NUMBER                                 
         XC    TLCKCCHK,ALLFF      COMPLEMENTED                                 
*                                                                               
         GOTO1 HIGH                READ FOR KEY                                 
*                                                                               
VK30     CLC   KEY(TLCKCBNK-TLCKPD),KEYSAVE                                     
         BNE   ERRNOFND                                                         
*                                                                               
         GOTO1 GETREC              READ RECORD                                  
*                                                                               
         BRAS  RE,LIMITCHK         CHECK LIMIT ACCESS                           
         BNE   ERRNOFND                                                         
*                                                                               
VK40     BAS   RE,GETPDEL          R4 = A(PAYMENT DETAILS ELEMENT)              
*                                                                               
VK90     OI    SCKCHKH+4,X'20'     SET CHECK NUMBER FIELD VALID                 
*                                                                               
VKX      B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
* ROUTINE TO HANDLE MODE DISPREC.                                               
*                                                                               
DISPRC   DS    0H                                                               
         BAS   RE,DISPLAY          DISPLAY RECORD IN AIO1                       
         B     XIT                                                              
*                                                                               
* THIS ROUTINE DISPLAYS THE CHECK RECORD FOUND IN AIO1 TO THE SCREEN.           
*                                                                               
         DS    0D                                                               
DISPLAY  NTR1                                                                   
         TWAXC SCKWSHTH,SCKLSTH,PROT=Y                                          
*                                                                               
         L     R4,AIO                                                           
         USING TLCKD,R4                                                         
         MVC   SCKSSN,TLCKSSN                                                   
         MVC   TGSSN,TLCKSSN                                                    
         TM    TGSYSTAT,TASYSPID   ARE WE USING PID#                            
         BZ    DISPA                                                            
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   SCKSSN,SSNSPACE                                                  
         MVC   SCKSSN(L'TGPID),TGPID                                            
         MVI   SCKSSNH+5,6                                                      
DISPA    OI    SCKSSNH+6,X'80'                                                  
*                                                                               
         XC    SCKSSNN,SCKSSNN     CLEAR PERFORMER NAME                         
         OI    SCKSSNNH+6,X'80'                                                 
         LA    R2,SCKWSHTH                                                      
*                                                                               
         LA    R3,KEY                                                           
         USING TLDRD,R3                                                         
         GOTO1 HEXOUT,DMCB,TLDRDA,SCKDADD,L'TLDRDA                              
*&&DO                                                                           
         BAS   RE,SETTAL           SET SYSDIR/SYSFIL FOR NON-CHECKS             
         USING TLCKD,R3                                                         
         MVC   SCKSSN,TLCKSSN      ELSE GET SS NUMBER FROM CHECK KEY            
*                                                                               
DISP0    MVC   AIO,AIO2            DISPLAY PERF NAME                            
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'8C',SCKSSN),SCKSSNNH                      
         TM    TGSYSTAT,TASYSPID                                                
         BZ    DISP0A                                                           
         GOTO1 SSNPACK,DMCB,SCKSSN,TGPID                                        
         MVC   SCKSSN,=CL9' '                                                   
         MVC   SCKSSN(L'TGPID),TGPID                                            
         MVI   SCKSSNH+5,6                                                      
         BAS   RE,SETCHK                                                        
*&&                                                                             
*                                                                               
DISP0A   MVC   AIO,AIO1                                                         
         L     R4,AIO                                                           
*                                                                               
         USING TAPDD,R4                                                         
         MVI   ELCODE,TAPDELQ      R4 = A(BREAKOUT ELEMENT)                     
         BAS   RE,GETEL                                                         
         BNE   DISPX                                                            
         MVC   PDELEM,TAPDEL       SAVE FOR LATER                               
*                                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,TABKELQ      R4 = A(BREAKOUT ELEMENT)                     
         BAS   RE,GETEL                                                         
         BNE   DISP995                                                          
*                                                                               
         USING TABKD,R4                                                         
         USING WSHTD,R2                                                         
         SR    R3,R3                                                            
         ZIC   R5,TABKNSUB                                                      
         LTR   R5,R5                                                            
         BZ    DISP995                                                          
DISP10   CLI   TABKCODE,0                                                       
         BE    DISP20                                                           
         LA    RF,BRKTABL                                                       
DISP13   CLI   0(RF),0             NO MATCHING DESCRIPTION                      
         BE    DISP900                                                          
         CLC   TABKCODE,0(RF)                                                   
         BE    DISP15                                                           
         AHI   RF,L'BRKTABL                                                     
         B     DISP13                                                           
*                                                                               
DISP15   MVC   WSHDESC,1(RF)                                                    
         CLI   TABKCODE,51                                                      
         BE    *+8                                                              
         CLI   TABKCODE,52                                                      
         BNE   DISP30                                                           
         LA    RF,WSHDESC+11                                                    
         EDIT  TABKUN2,(2,(RF)),0,ZERO=NOBLANK,ALIGN=LEFT                       
         B     DISP900                                                          
*                                                                               
DISP20   EDIT  TABKUN1,(8,WSHDESC),0,ZERO=NOBLANK,ALIGN=LEFT                    
         LA    RF,WSHDESC                                                       
         AR    RF,R0               NUMBER OF SIGNIFICANT CHAR                   
         MVI   0(RF),C'-'                                                       
         AHI   RF,1                                                             
         EDIT  TABKUN2,(8,(RF)),0,ZERO=NOBLANK,ALIGN=LEFT                       
         B     DISP900                                                          
*                                                                               
DISP30   CLI   TABKCODE,55                                                      
         BE    *+8                                                              
         CLI   TABKCODE,56                                                      
         BNE   DISP900                                                          
         EDIT  TABKNOUN,WSHUNIT,2  SHOW UNIT                                    
         EDIT  TABKRATE,WSHRATE,2,FLOAT=-                                       
         B     DISP910                                                          
*                                                                               
DISP900  CLI   TABKCODE,2          PREV APPLIED                                 
         BNE   DISP905                                                          
         ICM   RF,15,TABKRATE      RATE                                         
         B     DISP990                                                          
*                                                                               
DISP905  CLI   TABKCODE,66                                                      
         BNE   DISP907                                                          
         EDIT  TABKNOUN,WSHUNIT,2                                               
         LA    RF,WSHUNIT+L'WSHUNIT-3                                           
         MVI   0(RF),C':'                                                       
         B     DISP908                                                          
*                                                                               
DISP907  EDIT  TABKNOUN,WSHUNIT,0  SHOW UNIT                                    
DISP908  EDIT  TABKRATE,WSHRATE,2,FLOAT=-                                       
DISP910  CLI   TABKCODE,66                                                      
         BNE   DISP915                                                          
         ICM   RF,15,TABKUN2       AMOUNT CALCULATED IN PAY                     
         B     DISP990                                                          
*                                                                               
DISP915  XR    RE,RE                                                            
         ICM   RF,15,TABKRATE      RATE                                         
         ICM   R1,15,TABKNOUN      NUMBER OF UNITS                              
         MR    RE,R1                                                            
         CLI   TABKCODE,55                                                      
         BE    *+8                                                              
         CLI   TABKCODE,56                                                      
         BNE   DISP990                                                          
         D     RE,=F'50'                                                        
         LTR   RF,RF                                                            
         BM    *+8                                                              
         AHI   RF,1                                                             
         SRA   RF,1                                                             
*                                                                               
DISP990  EDIT  (RF),WSHAMNT,2,FLOAT=-                                           
         OI    WSHHDR+6,X'80'      TRANSMIT                                     
         AR    R3,RF               RUNNING TOTAL                                
         LA    R4,TABKSLNQ(R4)     DUMP NEXT SUB ELEMENT                        
         LA    R2,WSHLNQ(R2)       BUMP NEXT LINE                               
         BCT   R5,DISP10                                                        
*                                                                               
         MVC   WSHAMNT,=C'==========='                                          
         OI    WSHHDR+6,X'80'      TRANSMIT                                     
         LA    R2,WSHLNQ(R2)                                                    
         EDIT  (R3),WSHAMNT,2      SHOW AMOUNT                                  
         OI    WSHHDR+6,X'80'      TRANSMIT                                     
         LA    R2,WSHLNQ(R2)                                                    
         B     DISP998                                                          
*                                                                               
         USING TAPDD,R4                                                         
DISP995  LA    R4,PDELEM                                                        
         MVC   WSHDESC(20),=CL20'Work Performed'                                
         EDIT  TAPDGRS,WSHAMNT,2,ZERO=NOBLANK                                   
         OI    WSHHDR+6,X'80'      TRANSMIT                                     
         B     DISPX                                                            
*                                                                               
DISP998  LA    R4,PDELEM                                                        
         TM    TAPDSTAT,TAPDSMAN   MANUAL OVERRIDE                              
         BZ    DISPX                                                            
         MVC   WSHUNIT(20),=CL20'* Manual Override *'                           
         EDIT  TAPDGRS,WSHAMNT,2,ZERO=NOBLANK                                   
         OI    WSHHDR+6,X'80'      TRANSMIT                                     
*                                                                               
DISPX    B     XIT                                                              
         DROP  R3,R4                                                            
         EJECT                                                                  
*              ROUTINE TO SET SYSDIR/SYSFIL FOR NON-CHECKS                      
         SPACE                                                                  
SETTAL   MVC   SYSDIR,=CL8'TALDIR'                                              
         MVC   SYSFIL,=CL8'TALFIL'                                              
         BR    RE                                                               
         SPACE 2                                                                
*              ROUTINE TO SET SYSDIR/SYSFIL TO CHECKS                           
         SPACE                                                                  
SETCHK   MVC   SYSDIR,=CL8'CHKDIR'                                              
         MVC   SYSFIL,=CL8'CHKFIL'                                              
         BR    RE                                                               
         EJECT                                                                  
* THIS ROUTINE EDITS THE AMOUNT IN FULL FOR 10 TO THE FIELD POINTED TO          
* BY R2 AND THEN TRANSMITS IT.  ITS TWO ENTRY POINTS DIFFER IN THAT THE         
* FIRST WILL DISPLAY ZERO AMOUNTS AS BLANKS RATHER THAN .00.                    
*                                                                               
EDIT10   DS    0H                                                               
         XC    8(10,R2),8(R2)                                                   
         OC    FULL,FULL                                                        
         BZ    EDIT10X                                                          
EDIT10F  DS    0H                                                               
         EDIT  (4,FULL),(10,8(R2)),2,FLOAT=-                                    
EDIT10X  OI    6(R2),X'80'                                                      
         BR    RE                                                               
         SPACE 2                                                                
* THIS ROUTINE ADDS THE DEDUCTION AMOUNT IN FULL TO THE TOTDED.                 
*                                                                               
ADDDED   L     RF,TOTDED           ADD FULL TO TOTDED                           
         A     RF,FULL                                                          
         ST    RF,TOTDED                                                        
         BR    RE                                                               
         SPACE 2                                                                
* THIS ROUTINE POINTS R4 TO THE CHECK DETAILS ELEMENT IN AIO.                   
*                                                                               
GETCDEL  LR    R1,RE               USE R1 TO RETURN                             
*                                                                               
         L     R4,AIO              R4 = A(CHECK DETAILS ELEMENT)                
         MVI   ELCODE,TACDELQ                                                   
         B     ALLELS                                                           
         SPACE 2                                                                
* THIS ROUTINE POINTS R4 TO THE PAYMENT DETAILS ELEMENT IN AIO.                 
*                                                                               
GETPDEL  LR    R1,RE               USE R1 TO RETURN                             
*                                                                               
         L     R4,AIO              R4 = A(PAYMENT DETAILS ELEMENT)              
         MVI   ELCODE,TAPDELQ                                                   
         B     ALLELS                                                           
         SPACE 2                                                                
* THIS ROUTINE POINTS R4 TO THE INVOICE STATUS ELEMENT IN AIO.                  
*                                                                               
GETINEL  LR    R1,RE               USE R1 TO RETURN                             
*                                                                               
         L     R4,AIO              R4 = A(INVOICE STATUS ELEMENT)               
         MVI   ELCODE,TAINELQ                                                   
         B     ALLELS                                                           
         SPACE 2                                                                
* THIS ROUTINE POINTS R4 TO THE CAST DETAILS ELEMENT IN AIO.                    
*                                                                               
GETCAEL  LR    R1,RE               USE R1 TO RETURN                             
*                                                                               
         L     R4,AIO              R4 = A(CAST DETAILS ELEMENT)                 
         MVI   ELCODE,TACAELQ                                                   
         B     ALLELS                                                           
         SPACE 2                                                                
* ALL ELEMENT FINDING ROUTINES END UP HERE TO GET ELEMENT AND DIE IF            
* NOT FOUND.                                                                    
*                                                                               
ALLELS   BAS   RE,GETEL                                                         
         BER   R1                                                               
         DC    H'0'                                                             
         EJECT                                                                  
* LOCAL ERROR/EXIT ROUTINES                                                     
*                                                                               
ERRAGY   MVI   ERROR,ERAGYERR      CAN'T ASSIGN NEW NUMBER - AGY ERROR          
         B     ERREND                                                           
*                                                                               
ERREXIST MVI   ERROR,RECEXIST      RECORD ALREADY EXISTS                        
         B     ERREND                                                           
*                                                                               
ERRMISS  MVI   ERROR,MISSING       MISSING INPUT FIELD                          
         B     ERREND                                                           
*                                                                               
ERRNUM   MVI   ERROR,NOTNUM        INVALID NUMERIC INPUT                        
         B     ERREND                                                           
*                                                                               
ERRINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     ERREND                                                           
*                                                                               
ERRCANL  MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     ERREND                                                           
*                                                                               
ERRNOFND MVI   ERROR,NOTFOUND      RECORD NOT FOUND                             
         B     ERREND                                                           
*                                                                               
ERRNOINP MVI   ERROR,ERNOINP       INPUT NOT ALLOWED                            
         B     ERREND                                                           
*                                                                               
ERRCASH  MVI   ERROR,ERCASHED      CHECK HAS ALREADY BEEN CASHED                
         B     ERREND                                                           
*                                                                               
ERRVOI   MVI   ERROR,ERVOIDED      CHECK HAS ALREADY BEEN VOIDED                
         B     ERREND                                                           
*                                                                               
ERRADD   MVI   ERROR,ERADDUP       CHECK AMOUNTS DON'T ADD UP                   
         B     ERREND                                                           
*                                                                               
ERRPAY   MVI   ERROR,ERNOPAY       INVOICE HAS NOT BEEN PAID                    
         B     ERREND                                                           
*                                                                               
ERRDED   MVI   ERROR,ERDEDERN      DEDUCTIONS EXCEED EARNINGS                   
         B     ERREND                                                           
*                                                                               
ERRDRCR  MVI   ERROR,ERDRCR        DEBITS DO NOT EQUAL CREDITS                  
         B     ERREND                                                           
*                                                                               
ERRNOINV MVI   ERROR,ERNOINV       ORIGINAL INVOICE NOT ON FILE                 
         B     ERREND                                                           
*                                                                               
ERREARN  MVI   ERROR,ERXEARN       ERROR TRANSFERRING EARNINGS                  
         LA    R2,STTONLYH                                                      
         B     ERREND                                                           
*                                                                               
ERRACT   MVI   ERROR,INVACT        INVALID ACTION                               
         LA    R2,CONACTH                                                       
         B     ERREND                                                           
*                                                                               
ERREND   MVI   ASKED,C'N'          MAKE USER HIT PFKEY NEXT TIME                
         B     CALLEXIT                                                         
*                                                                               
MSGPFVR  CLI   ACTNUM,ACTREIS      IF ACTION REISSUE                            
         BNE   MSGPFVD                                                          
         MVI   MYMSGNO1,42         THEN SAY 'HIT PF19 TO VOID & REISS'          
         B     MSGPF                                                            
*                                                                               
MSGPFVD  MVI   MYMSGNO1,43         ELSE SAY 'HIT PF19 TO VOID'                  
         B     MSGPF                                                            
*                                                                               
MSGPFIS  MVI   MYMSGNO1,45         SAY 'HIT PF19 TO ISSUE'                      
         B     MSGPF                                                            
*                                                                               
MSGPFTR  MVI   MYMSGNO1,46         SAY 'HIT PF19 TO REFUND'                     
         B     MSGPF                                                            
*                                                                               
MSGPFTT  MVI   MYMSGNO1,47         IF RECORD SSN                                
         CLI   RECNUM,SS                                                        
         BE    MSGPF               OR OLDNET = NEWNET                           
         CLC   OLDNET,NETAMNT      THEN SAY 'INPUT ACCEPTED - ...'              
         BE    MSGPF                                                            
         MVI   MYMSGNO1,64         ELSE SAY 'OUT OF BALANCE - ....'             
         B     MSGPF                                                            
*                                                                               
MSGPFMR  MVI   MYMSGNO1,49         SAY 'NNNNN CHECKS WILL BE REISSUED'          
*                                                                               
         LA    R3,BLOCK            BUILD SUBSTITUTION BLOCK FOR GETTXT          
*                                                                               
         L     RF,ENDCHK           RF = NUMBER OF CHECKS TO MREISSUE            
         S     RF,STARTCHK                                                      
         LA    RF,1(RF)                                                         
*                                  EDIT NUMBER INTO SUBSTITUTION BLOCK          
         EDIT  (RF),(10,1(R3)),ALIGN=LEFT                                       
*                                                                               
         LR    R1,R0               SAVE LENGTH IN BLOCK AND BUMP TO END         
         LA    R1,1(R1)                                                         
         STC   R1,0(R3)                                                         
         AR    R3,R1                                                            
*                                                                               
         MVI   0(R3),0             MARK END OF BLOCK                            
         B     MSGPF                                                            
*                                                                               
MSGPF    MVI   ASKED,C'Y'          SET USER HAS BEEN ASKED TO HIT PF19          
         B     MSGEND                                                           
*                                                                               
GENEND   MVI   MYMSYS,X'FF'        USE GENERAL MESSAGES                         
*                                                                               
MSGEND   OI    GENSTAT2,USGETTXT   TELL GENCON TO USE GETTEXT                   
*                                                                               
CALLEXIT GOTO1 EXIT,DMCB,0         GO TO CONTROLLER ERROR ROUTINE               
*                                                                               
THEEND   MVI   ASKED,C'N'          MAKE USER HIT PFKEY NEXT TIME                
         L     RD,SAVERD           RETURN WITHOUT AN ERROR                      
         B     XIT                                                              
*                                                                               
BUMP2    ZIC   R0,0(R2)            BUMP R2 TO NEXT SCREEN FIELD                 
         AR    R2,R0                                                            
BUMP     ZIC   R0,0(R2)            BUMP R2 TO NEXT SCREEN FIELD                 
         AR    R2,R0                                                            
         BR    RE                                                               
*                                                                               
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
       ++INCLUDE TAPAYBRKT                                                      
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
*                                                                               
ALLFF    DC    8X'FF'              SHOULD BE SAME LENGTH AS LONGEST             
SSNSPACE DC    CL9' '              SPACES FIELD FOR SS#                         
*                                                                               
PFTAB    DS    0C                  PF TABLE                                     
         DC    AL1(PF13X-*,13,0,0,0)                                            
         DC    CL3' ',CL8'HISTORY',CL8'DISPLAY'                                 
PF13X    EQU   *                                                                
*                                                                               
         DC    AL1(PF14X-*,14,0,0,0)                                            
         DC    CL3' ',CL8'CHECK ',CL8'DISPLAY'                                  
PF14X    EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
         SPACE 2                                                                
         LTORG                                                                  
*                                                                               
         DROP  R6,R7,R8            DROP SECONDARY BASE REGISTERS                
         EJECT                                                                  
*              ROUTINE TO CHECK AGENCY AND CLIENT LIMIT ACCESS                  
         SPACE 1                                                                
LIMITCHK NTR1  BASE=*,LABEL=*                                                   
         LHI   R2,1                                                             
*                                                                               
         USING TLCKD,R4                                                         
         L     R4,AIO                                                           
         MVC   CHKAGY,TLCKAGY      INITIALIZE CHECK'S AGENCY                    
         XC    CHKCLI,CHKCLI       AND CLIENT                                   
         DROP  R4                                                               
*                                                                               
         USING TAOID,R4                                                         
         MVI   ELCODE,TAOIELQ      SAVE CHECK'S AGENCY                          
         BRAS  RE,GETEL                                                         
         JNE   LIMCHK10                                                         
         MVC   CHKAGY,TAOIAGY                                                   
         DROP  R4                                                               
*                                                                               
         USING TAPDD,R4                                                         
LIMCHK10 L     R4,AIO              CHECK CLIENT LIMIT ACCESS                    
         MVI   ELCODE,TAPDELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   LIMCHK20                                                         
         MVC   CHKCLI,TAPDCLI                                                   
         DROP  R4                                                               
*                                                                               
         USING FAWSSVRD,R1                                                      
LIMCHK20 LA    R1,LIMBLK                                                        
         MVC   FAWSTOKN(3),=C'STF'                                              
         STC   R2,FAWSTOKN+3                                                    
         MVI   FAWSACTN,FAWSARST   RECALL STAFF2 INFORMATION VIA WWSVR          
         XC    FAWSLEN,FAWSLEN                                                  
         MVC   FAWSADR,TGAS2ACC                                                 
         GOTO1 WSSVR,(R1)                                                       
         CLI   FAWSRTN,0           IF NOT FOUND, STAFF HAS NO ACCESS            
         JNE   NO                                                               
         DROP  R1                                                               
*                                                                               
         AHI   R2,1                                                             
*                                                                               
         USING TAVAD,R1                                                         
         L     R1,TGAS2ACC                                                      
         OC    TAVAAGY,TAVAAGY     IF STAFF HAS NO AGENCY LIMITS,               
         JZ    YES                 STAFF HAS ACCESS TO ALL RECORDS              
*                                                                               
LIMCHK30 CLI   0(R1),0             RECALL NEXT RECORD FROM WSSVR                
         JE    LIMCHK20                                                         
*                                                                               
         CLC   CHKAGY,TAVAAGY      IF AGENCY IS FOUND IN STAFF LIMITS           
         JNE   LIMCHK50                                                         
*                                                                               
         CLI   TAVALEN,TAVALNQ     IF NO CLIENT LIMITS ARE DEFINED              
         JE    YES                 ACCESS IS GRANTED                            
*                                                                               
         ZIC   RE,TAVALEN                                                       
         SHI   RE,TAVALNQ                                                       
         LA    RF,TAVACLI                                                       
LIMCHK40 CLC   CHKCLI,0(RF)        IF CLIENT IS FOUND IN STAFF LIMITS           
         JE    YES                 ACCESS IS GRANTED                            
         LA    RF,L'TAVACLI(RF)                                                 
         SHI   RE,L'TAVACLI                                                     
         LTR   RE,RE                                                            
         JNZ   LIMCHK40                                                         
*                                                                               
LIMCHK50 ZIC   RE,TAVALEN          BUMP TO NEXT VALID AGENCY/CLIENT             
         AR    R1,RE               ELEMENT                                      
         J     LIMCHK30                                                         
         DROP  R1                                                               
*                                                                               
CHKAGY   DS    CL6                 CHECK'S AGENCY                               
CHKCLI   DS    CL6                 CHECK'S CLIENT                               
LIMBLK   DS    XL100               AGENCY/CLIENT LIMIT WORK BLOCK               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*                                                                               
* THIS ROUTINE CALLS SPOOL AND RETURNS.                                         
*                                                                               
CALLSPL  NTR1  BASE=*,LABEL=*                                                   
         GOTO1 SPOOL,DMCB,ASPOOLD                                               
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
* THIS ROUTINE INITIALIZES THE CHECK RECORD FOUND IN AIO1 WITH THE              
* APPROPRIATE CW ELEMENTS FOR THE PERFORMER WHOSE W4 RECORD IS FOUND            
* AT TWAHOLE+2000.                                                              
*                                                                               
EXTRW4   NTR1  BASE=*,LABEL=*                                                   
         MVI   ELCODE,TACWELQ      RESET CHECK WITHHOLDING BLOCK                
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R4,TWAHOLE          R4 = A(FIRST W4 WITHHOLDING ELEMENT)         
         LA    R4,2000(R4)                                                      
         MVI   ELCODE,TAWHELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   EWX                 DONE IF NO ELEMENTS                          
         USING TAWHD,R4                                                         
*                                                                               
         LA    R5,ELEM             BUILD SKELETON CHECK WITHHOLD ELEM           
         USING TACWD,R5                                                         
         XC    ELEM,ELEM                                                        
         MVI   TACWEL,TACWELQ                                                   
         MVI   TACWLEN,TACWLNQ                                                  
*                                                                               
EW10     CLC   TAWHEMP,TGTPEMP     ONLY USING RULES FOR EMPLOYER TP             
         BNE   EW90                                                             
*                                                                               
         MVC   TACWUNIT,TAWHUNIT   MOVE TAX UNIT TO CW ELEMENT                  
         MVC   TACWMST,TAWHSTAT         MARITAL STATUS                          
*                                                                               
*                                  MOVE EXEMPTIONS TO CW ELEMENT                
         EDIT  (1,TAWHEXS),(2,TACWEXS),ALIGN=RIGHT,FILL=0                       
*                                                                               
         OI    TACWSTAT,TACWSRES   SET RESIDENT BIT IN STATUS                   
*                                                                               
         GOTO1 ADDELEM             ADD CW ELEMENT TO CHECK RECORD               
*                                                                               
         CLC   TACWUNIT,=C'FD '    IF UNIT IS A STATE OR A CITY                 
         BE    EW90                                                             
         CLC   TACWUNIT,=C'CN '                                                 
         BE    EW90                                                             
*                                                                               
         CLI   TACWUNIT+2,C' '     THEN IF UNIT IS A STATE                      
         BH    EW20                                                             
*                                                                               
         MVC   SOR,TACWUNIT        THEN SET SOR                                 
         B     EW90                                                             
*                                                                               
EW20     MVC   COR,TACWUNIT        ELSE SET COR                                 
*                                                                               
EW90     BRAS  RE,NEXTEL           GET NEXT WH ELEMENT                          
         BE    EW10                REPEAT UNTIL NO MORE                         
*                                                                               
EWX      XIT1                                                                   
         DROP  R5                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* THIS ROUTINE EXTRACT SOW, COW, SOR, AND COR FROM THE CHECK RECORD.            
*                                                                               
EXTRSC   NTR1  BASE=*,LABEL=*                                                   
         XC    SOW,SOW             PRE-CLEAR SOW, COW, SOR, AND COR             
         XC    COW,COW                                                          
         XC    SOR,SOR                                                          
         XC    COR,COR                                                          
*                                                                               
         L     R4,AIO              R4 = A(FIRST CHECK WITHHOLD ELEMENT)         
         MVI   ELCODE,TACWELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   ESCX                DONE IF NONE FOUND                           
         USING TACWD,R4                                                         
*                                                                               
ESC50    CLC   TACWUNIT,=C'FD '    IF UNIT IS FEDERAL                           
         BE    ESC100              THEN SKIP                                    
*                                                                               
         CLI   TACWUNIT+2,C' '     ELSE IF UNIT IS A STATE                      
         BH    ESC70                                                            
*                                                                               
         TM    TACWSTAT,TACWSWRK   IF SOW                                       
         BZ    *+10                                                             
         MVC   SOW,TACWUNIT        THEN EXTRACT SOW                             
*                                                                               
         TM    TACWSTAT,TACWSRES   IF SOR                                       
         BZ    *+10                                                             
         MVC   SOR,TACWUNIT        THEN EXTRACT SOR                             
         B     ESC100                                                           
*                                                                               
ESC70    TM    TACWSTAT,TACWSWRK   IF COW                                       
         BZ    *+10                                                             
         MVC   COW,TACWUNIT        THEN EXTRACT COW                             
*                                                                               
         TM    TACWSTAT,TACWSRES   IF COR                                       
         BZ    *+10                                                             
         MVC   COR,TACWUNIT        THEN EXTRACT COR                             
*                                                                               
ESC100   BRAS  RE,NEXTEL           GET NEXT CW ELEM AND LOOP BACK               
         BE    ESC50                                                            
*                                                                               
ESCX     XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
* THIS ROUTINE INITIALIZES THE INVOICE RECORD IN AIO2 WHICH WILL BE             
* UPDATED AND ADDED BY ADDINV.                                                  
*                                                                               
         DS    0D                                                               
INITINV  NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,SETTAL           SET SYSDIR/SYSFIL FOR NON-CHECKS             
         MVC   AIO,AIO2            SET AIO TO AIO2                              
*                                                                               
         L     RF,AIO              INITIALIZE IOAREA WITH NULL KEY              
         LH    RE,DATADISP                                                      
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    0(0,RF),0(RF)                                                    
*                                                                               
         MVI   0(RF),TLINCDQ       SAVE RECORD CODE IN KEY                      
*                                                                               
         LA    R4,ELEM             BUILD SKELETON INVOICE STATUS ELEM           
         USING TAIND,R4                                                         
         XC    ELEM,ELEM                                                        
         MVI   TAINEL,TAINELQ                                                   
         MVI   TAINLEN,TAINLNQ                                                  
*                                                                               
         GOTO1 ADDELEM             ADD ELEMENT TO RECORD                        
*                                                                               
         LA    R4,ELEM             BUILD SKELETON DUE DATE ELEMENT              
         USING TADDD,R4                                                         
         XC    ELEM,ELEM                                                        
         MVI   TADDEL,TADDELQ                                                   
         MVI   TADDLEN,TADDLNQ                                                  
*                                                                               
         GOTO1 ADDELEM             ADD ELEMENT TO RECORD                        
*                                                                               
         BRAS  RE,SETCHK           RESET SYSDIR/SYSFIL FOR CHECKS               
         MVC   AIO,AIO1            RESET AIO TO AIO1                            
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
* THIS ROUTINE DETERMINES THE SEQUENCE NUMBER FOR THE CHECK RECORD.             
* IT GETS THE FIRST CAST PAYMENT HISTORY POINTER FOR THIS RECORD AND            
* SUBTRACTS TWO FROM ITS SEQUENCE NUMBER TO DETERMINE THE NEXT                  
* AVAILABLE ONE.  FOR TAX TRANSFER OR REFUND CHECKS (MYBYTE=X'80'),             
* USES CHECK NUMBER POINTER INSTEAD BECAUSE NO CAST PAYMENT HISTORY             
* PTR IS GENERATED.  THE SEQUENCE NUMBER IS SAVED IN THE CHECK KEY.             
*                                                                               
         DS    0D                                                               
GETSEQ   NTR1  BASE=*,LABEL=*                                                   
         LA    R4,X'FF'            IF NONE FOUND THEN SEQ = X'FF'               
*                                                                               
GS10     CLI   0(R3),0             IF END OF BLOCK THEN NONE FOUND              
         BE    GS30                                                             
*                                                                               
         CLI   0(R3),TLCKHCDQ      IF CAST PAYMENT HISTORY POINTER              
         BE    GS20                    THEN DONE                                
*                                                                               
         CLI   MYBYTE,X'80'        FOR TAX XFER OR REFUND CHECK                 
         BNE   GS12                                                             
         CLI   0(R3),TLCKCCDQ      IF CHECK NUMBER POINTER                      
         BE    GS15                    THEN DONE                                
*                                                                               
GS12     LA    R3,L'TLDRREC(R3)    ELSE BUMP R3 TO NEXT POINTER                 
         B     GS10                LOOP BACK                                    
*                                                                               
GS15     MVC   KEY(L'TLDRREC),0(R3)                                             
*                                                                               
         LA    R3,KEY              CLEAR SEQUENCE NUMBER AND READ HIGH          
         USING TLCKPD,R3               TO GET FIRST CHECK NUMBER                
         MVI   TLCKCSEQ,0              KEY ON FILE                              
         GOTO1 HIGH                                                             
*                                  IF NO MATCHING CHECK NUM POINTER             
GS16     CLC   KEY(TLCKCSEQ-TLCKPD),KEYSAVE                                     
         BNE   GS30                THEN NOT FOUND                               
*                                                                               
         ZIC   R4,TLCKCSEQ         R4 = LAST USED SEQ                           
*                                                                               
         CLI   TLCKCSEQ,40         PROTECT AGAINST CONVERTED RECORDS            
         BH    GS18                (SEQ MAY HAVE BEEN CONV AS NOT FF)           
         AHI   R4,2                SEQ = LAST USED + 2                          
         GOTO1 SEQ                 AND WE NEED TO CHECK FOR ANOTHER             
         B     GS16                                                             
*                                                                               
GS18     AHI   R4,-2               ELSE SEQ = LAST USED - 2                     
         B     GS30                                                             
*                                                                               
*                                  BUILD KEY WITH CAST PAY KEY DETAILS          
GS20     MVC   KEY(L'TLDRREC),0(R3)                                             
*                                                                               
         LA    R3,KEY              CLEAR SEQUENCE NUMBER AND READ HIGH          
         USING TLCKPD,R3               TO GET FIRST CAST PAYMENT                
         MVI   TLCKHCSQ,0              HISTORY KEY ON FILE                      
         GOTO1 HIGH                                                             
*                                  IF NO MATCHING CAST PAY POINTER              
GS25     CLC   KEY(TLCKHCSQ-TLCKPD),KEYSAVE                                     
         BNE   GS30                THEN NOT FOUND                               
*                                                                               
         ZIC   R4,TLCKHCSQ         R4 = LAST USED SEQ                           
*                                                                               
         CLI   TLCKHCSQ,40         PROTECT AGAINST CONVERTED RECORDS            
         BH    GS28                (SEQ MAY HAVE BEEN CONV AS NOT FF)           
         AHI   R4,2                SEQ = LAST USED + 2                          
         GOTO1 SEQ                 AND WE NEED TO CHECK FOR ANOTHER             
         B     GS25                                                             
*                                                                               
GS28     AHI   R4,-2               ELSE SEQ = LAST USED - 2                     
*                                                                               
GS30     L     R3,AIO              SAVE SEQUENCE NUMBER IN CHECK KEY            
         USING TLCKD,R3                                                         
         STC   R4,TLCKSEQ                                                       
*                                                                               
GSX      XIT1                                                                   
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
* THIS ROUTINE LOOKS FOR A WITHHOLDING ELEMENT WITH THE UNIT FOUND IN           
* PARM 1.  IF IT DOESN'T FIND IT, IT ADDS ONE AND RETURNS CC NOT EQ.            
* IF IT DOES FIND IT, IT OR'S THE STATUS BIT OF THE ELEMENT WITH THE            
* CONTENTS OF BYTE 0 OF PARM 1 AND RETURNS CC EQ.                               
*                                                                               
         DS    0D                                                               
WITHUNIT NTR1 BASE=*,LABEL=*                                                    
         LA    R4,ELEM             BUILD NEW CW ELEMENT WITH UNIT AND           
         USING TACWD,R4                STATUS                                   
         XC    ELEM(TACWLNQ),ELEM                                               
         MVI   TACWEL,TACWELQ                                                   
         MVI   TACWLEN,TACWLNQ                                                  
         L     R3,0(R1)                                                         
         MVC   TACWUNIT,0(R3)                                                   
         MVC   TACWSTAT,0(R1)                                                   
*                                                                               
         MVI   ELCODE,TACWELQ      IF ELEMENT FOR UNIT DOES NOT EXIST           
         GOTO1 GETL,DMCB,(3,0(R3))                                              
         BE    WU10                                                             
*                                                                               
         CLI   RECNUM,SS           IF NOT SSN TRANSFER                          
         BE    WU5                                                              
         CLI   EARNONLY,C'Y'       AND NOT TRANSFERRING ONLY EARNINGS           
         BE    WU5                                                              
         CLI   W4TYPE,TAW4TYFO     FOREIGNERS DON'T HAVE TXBLE EARNINGS         
         BE    WU4                                                              
         CLI   W4TYPE,TAW4TYCA     NEITHER DO CANADIANS                         
         BE    WU4                                                              
         CLI   W4TYPE,TAW4TYTR     NEITHER DO TRUSTEES                          
         BE    WU4                                                              
         CLI   W4TYPE,TAW4TYCO     NEITHER TO CORPS                             
         BNE   WU5                                                              
         CLC   TACWUNIT,=C'CA '    EXCEPT FOR CALIFORNIA                        
         BE    WU5                                                              
WU4      CLC   TACWUNIT,=C'CN '    THEY CAN HAVE CANADIAN TAXES                 
         BE    WU4B                                                             
         OC    TACWWHLD,TACWWHLD   IF NO WITHHOLDINGS                           
         BZ    WU6                 DON'T BOTHER ADDING                          
WU4B     NI    TACWSTAT,ALL-TACWSTAX  ELSE TURN OFF TAXABLE BIT                 
*                                                                               
WU5      GOTO1 ADDELEM             THEN ADD NEW ELEMENT FOR UNIT                
WU6      B     WUNO                AND SET CC NOT EQ                            
*                                                                               
WU10     MVC   BYTE,TACWSTAT       ELSE SET REQUESTED STATUS BITS               
         L     R4,TGELEM                                                        
         OC    TACWSTAT,BYTE                                                    
*                                                                               
         CLI   EARNONLY,C'Y'       IF NOT TRANSFERRING ONLY EARNINGS            
         BE    WUX                                                              
         CLI   W4TYPE,TAW4TYFO     FOREIGNERS DON'T HAVE TXBLE EARNINGS         
         BE    WU15                                                             
         CLI   W4TYPE,TAW4TYCA     NEITHER DO CANADIANS                         
         BE    WU15                                                             
         CLI   W4TYPE,TAW4TYTR     NEITHER DO TRUSTEES                          
         BE    WU15                                                             
         CLI   W4TYPE,TAW4TYCO     NEITHER DO CORPS                             
         BNE   WUX                                                              
         CLC   TACWUNIT,=C'CA '    EXCEPT FOR CALIFORNIA                        
         BE    *+8                                                              
WU15     NI    TACWSTAT,ALL-TACWSTAX                                            
*                                                                               
WUYES    XR    RC,RC                                                            
WUNO     LTR   RC,RC                                                            
WUX      XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*        ROUTINE UPDATES W4 RECORD WITH W4 TRUSTEE AMOUNT W/HELD                
         SPACE 1                                                                
         DS    0D                                                               
UPDW4    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         BRAS  RE,SETTAL           SET SYSDIR/SYSFIL FOR NON-CHECKS             
         MVI   ELCODE,TAODELQ      IF W4 TRUSTEE AMOUNT DEDUCTED                
         GOTO1 GETL,DMCB,(1,=AL1(TAODTYPT))                                     
         BNE   UPDW4X                                                           
*                                                                               
         L     R4,TGELEM                                                        
         USING TAODD,R4                                                         
         ICM   R0,15,TAODAMT       R0=AMOUNT DEDUCTED                           
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'34',0)                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,AIO                                                           
         MVI   ELCODE,TAWXELQ                                                   
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAWXD,R4                                                         
         OC    TAWXTSSN,TAWXTSSN   IF NO TRUSTEE SPECIFIED                      
         BNZ   UPDW4X                                                           
         CLC   TGEMP,=C'TP '       ADD TO AMOUNT HELD BY EMPLOYER               
         BNE   *+14                                                             
         ICM   R1,15,TAWXHTP                                                    
         AR    R1,R0                                                            
         STCM  R1,15,TAWXHTP                                                    
         CLC   TGEMP,=C'P+ '                                                    
         BNE   *+14                                                             
         ICM   R1,15,TAWXHPPL                                                   
         AR    R1,R0                                                            
         STCM  R1,15,TAWXHPPL                                                   
         CLC   TGEMP,=C'PP '                                                    
         BNE   *+14                                                             
         ICM   R1,15,TAWXHPP                                                    
         AR    R1,R0                                                            
         STCM  R1,15,TAWXHPP                                                    
         GOTO1 PUTREC                                                           
*                                                                               
UPDW4X   MVC   AIO,AIO1            RESTORE AIO TO AIO1                          
         BRAS  RE,SETCHK           RESET SYSDIR/SYSFIL FOR CHECKS               
         XIT1                                                                   
         EJECT                                                                  
* THIS ROUTINE EDITS THE AMOUNT AT R3 TO THE ADDRESS AT R5.                     
*                                                                               
EDITAMT  DS    0H                                                               
         LTR   R3,R3               RETURN IF AMOUNT IS ZERO                     
         BZR   RE                                                               
*                                  EDIT AMOUNT                                  
EDITAMTZ EDIT  (R3),(11,0(R5)),2,MINUS=YES,ALIGN=LEFT                           
         BR    RE                                                               
         SPACE 2                                                                
         EJECT                                                                  
XIT1     XIT1                                                                   
         SPACE 2                                                                
LTRCHK   DC    C'This check was'                                                
         LTORG                                                                  
         EJECT                                                                  
* THIS ROUTINE GETS THE YTD AMOUNTS FOR THE CURRENT EMPLOYER/SSN FROM           
* TAYTD AND SAVES THE AMOUNTS FOR THE CURRENT SOT, COT IN LOCAL                 
* STORAGE (MYDEARN, MYDFTAX, ETC.).                                             
*                                                                               
         DS    0D                                                               
GETYTD   NTR1  BASE=*,LABEL=*                                                   
*                                  PRE-CLEAR YTD AMOUNTS                        
         XC    MYDAMNTS(MYDAMNTL),MYDAMNTS                                      
*                                                                               
         MVC   TAXUNIT,COT         TAX UNIT = COT IF EXISTS                     
         OC    COT,COT                                                          
         BNZ   *+10                                                             
         MVC   TAXUNIT,SOT         ELSE TAX UNIT = SOT                          
*                                                                               
         LA    R3,BLOCK            R3 = A(TAYTD PARAMETER BLOCK)                
         USING TYD,R3                                                           
*                                                                               
         L     R4,AYTDTAB          R4=A(YTD TABLE)                              
         USING YTDD,R4                                                          
*                                                                               
         XC    0(TYLNQ,R3),0(R3)   BUILD TAYTD PARAMETER BLOCK                  
         MVC   TYASYSIO,TASYSIO    A(SYSIO)                                     
         ST    R4,TYATAB           A(YTD TABLE)                                 
         MVC   TYPEND,TGTODAY1     END DATE = TODAY                             
*                                                                               
         CLI   LASTYEAR,C'Y'       IF FORCING LAST YEAR                         
         BNE   GY5                                                              
         GOTO1 ADDAY,DMCB,(C'Y',TGTODAY0),DUB,-1                                
         MVC   DUB+2(4),=C'1231'                                                
         GOTO1 DATCON,DMCB,(0,DUB),(1,TYPEND)                                   
*                                                                               
*        PACK  DUB,TGTODAY0(2)     CALC. LAST YEAR                              
*        SP    DUB,=P'1'                                                        
*        MP    DUB,=P'10'                                                       
*        MVC   TYPEND(1),DUB+6                                                  
*        MVC   TYPEND+1(2),=X'1231'  END DATE IS 12/31 OF LAST YEAR             
*                                                                               
GY5      MVC   TYEMP,TGEMP         EMPLOYER                                     
         MVC   TYSSN,TGSSN         SOCIAL SECURITY NUMBER                       
         MVC   TYCUR,TGCUR         CURRENCY                                     
*                                                                               
*                                  FILL YTD TABLE                               
         GOTO1 TGTAYTD,DMCB,(RC),SYSCOMM,(R3)                                   
*                                                                               
         MVC   MYDEARN,TYCEARN     SET TAXABLE EARNINGS                         
         MVC   MYDNTAX,TYCNTAX     AND NON-TAXABLE EARNINGS                     
*                                                                               
GY10     CLI   0(R4),0             WHILE NOT END OF YTD TABLE                   
         BE    GYX                                                              
*                                                                               
         CLC   YTDUNIT,=C'CN '     IF UNIT IS CANADA                            
         BNE   GY15                                                             
         MVC   MYDCTX,YTDTAX       SAVE YTD TAX IN LOCAL STORAGE                
         B     GY90                                                             
*                                                                               
GY15     CLC   YTDUNIT,=C'FD '     IF UNIT IS FEDERAL                           
         BNE   GY20                                                             
         MVC   MYDFTAX,YTDTAX      SAVE YTD FTAX IN LOCAL STORAGE               
         MVC   MYDFICA,YTDFICA              FICA                                
         MVC   MYDREXP,YTDREXP              REIMBURSED EXPENSES                 
         L     R0,MYDNTAX                                                       
         S     R0,MYDREXP          SUBTRACT REIMB EXP FROM NON-TAXABLE          
         ST    R0,MYDNTAX                                                       
         B     GY90                                                             
*                                                                               
GY20     CLI   YTDUNIT+2,C' '      ELSE IF UNIT IS A STATE                      
         BH    GY30                                                             
         CLC   YTDUNIT,SOT         AND IT IS SOT                                
         BNE   GY90                                                             
         MVC   MYDSTAX,YTDTAX      THEN SAVE YTD STAX IN LOCAL STORAGE          
         MVC   MYDSUI,YTDSUI                     SUI                            
         MVC   MYDSDI,YTDSDI                     SDI                            
         B     GY90                                                             
*                                                                               
GY30     CLC   YTDUNIT,COT         ELSE IF CITY IS COT                          
         BNE   GY90                                                             
         MVC   MYDLTAX,YTDTAX      THEN SAVE YTD LTAX IN LOCAL STORAGE          
*                                                                               
GY90     CLC   TAXUNIT,YTDUNIT     IF THIS UNIT MATCHES TAX UNIT                
         BNE   *+10                                                             
         MVC   MYDEARN,YTDEARN     THEN SAVE YTD EARN IN LOCAL STORAGE          
*                                                                               
         LA    R4,YTDNEXT          BUMP R4 TO NEXT YTD TABLE ENTRY              
         B     GY10                                                             
*                                                                               
GYX      XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         DROP  R3,R4                                                            
         EJECT                                                                  
       ++INCLUDE TAYTDD                                                         
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR99D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR07D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR08D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR09D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR02D                                                       
         EJECT                                                                  
         ORG   SCKWORK                                                          
SAVERD   DS    A                   SAVED RD                                     
*                                                                               
DSKADD   DS    XL4                                                              
*                                                                               
CHKTRAN  DS    C                   SINGLE CHECK TRANSFER (Y/N)                  
EARNONLY DS    C                   TRANSFER EARNINGS ONLY (Y/N)                 
LASTYEAR DS    C                   TREAT AS CHECK FOR LAST YEAR (Y/N)           
*                                                                               
ASKED    DS    C                   USER HAS BEEN ASKED TO PF19 (Y/N)            
PFADJ    EQU   19                                                               
ADJTYPE  DS    X                   ADJUSTMENT TYPE (FOR ACTION REISSUE)         
TRACEOPT DS    C                   TRACE VOIDS/REISSUES (Y/N)                   
*                                                                               
OIELEM   DS    XL(TAOILNQ)         ORIGINAL AGENCY/INVOICE ELEMENT              
ORIGDTE  DS    PL3                 ORIGINAL CHECK DATE                          
ORIGSEQ  DS    XL4                 ORIGINAL CHECK PROCESSED SEQNUM              
OKELEM   DS    XL(TAOKLNQ)         ORIGINAL CHECK DATE/SEQNUM ELEMENT           
PDELEM   DS    XL(TAPDLNQ)         PAYMENT DETAILS ELEMENT FROM CHECK           
PINVNUM  DS    PL3                 PACKED NEXT INVOICE NUMBER                   
CHKSSN   DS    CL9                 ISSUE CHECK SSN                              
W4TYPE   DS    C                   W4 TYPE FROM W4 RECORD                       
W4STA2   DS    C                   W4 2ND STATUS BYTE                           
PAYTYPE  DS    C                   W4 TYPE THAT CHECK IS PAID UNDER             
*                                                                               
SVCADTL  DS    0CL(L'TACAUN+L'TACALOCL+L'TACAYEAR)                              
SVCAUN   DS    CL(L'TACAUN)        ORIGINAL INVOICE TACAD UNION                 
SVCALOCL DS    CL(L'TACALOCL)      ORIGINAL INVOICE TACAD LOCAL                 
SVCAYEAR DS    CL(L'TACAYEAR)      ORIGINAL INVOICE TACAD YEAR                  
*                                                                               
SOW      DS    CL3                 STATE OF WORK TAX UNIT                       
COW      DS    CL3                 CITY OF WORK TAX UNIT                        
SOR      DS    CL3                 STATE OF RESIDENCE TAX UNIT                  
COR      DS    CL3                 CITY OF RESIDENCE TAX UNIT                   
SOT      DS    CL3                 TAXABLE STATE TAX UNIT                       
SOTST    DS    XL1                 TAXABLE STATE TAX UNIT STATUS BYTE           
COT      DS    CL3                 TAXABLE CITY TAX UNIT                        
TAXUNIT  DS    CL3                 COT IF EXISTS ELSE SOT                       
OLDSOT   DS    CL3                 OLD STATE OF TAX UNIT                        
OLDSOTST DS    XL1                 OLD STATE OF TAX UNIT STATUS BYTE            
OLDCOT   DS    CL3                 OLD CITY OF TAX UNIT                         
NEWSOT   DS    CL3                 NEW STATE OF TAX UNIT                        
NEWSOTST DS    XL1                 NEW STATE OF TAX UNIT STATUS BYTE            
NEWCOT   DS    CL3                 NEW CITY OF TAX UNIT                         
*                                                                               
DRCRFLAG DS    C                   DEBIT/CREDIT AMOUNTS FLAG                    
*                                                                               
STARTCHK DS    F                   STARTING CHECK NUMBER FOR MREISSUE           
ENDCHK   DS    F                   ENDING CHECK NUMBER FOR MREISSUE             
CHKCNT   DS    F                   CHECK NUMBER COUNTER FOR MREISSUE            
*                                                                               
EARNINGS DS    F                   EARNINGS (AFTER DUE COMPANY)                 
NONTAX   DS    F                   NON-TAXABLE EARNINGS (AFTER DUECOMP)         
REXP     DS    F                   REIMBURSED EXPENSES                          
MDED     DS    F                   MISCELLANEOUS DEDUCTIONS                     
DUES     DS    F                   UNION DUES                                   
DIRCT    DS    F                   DIRECT DEPOSIT AMOUNT                        
WIRE     DS    F                   WIRE TRANSFER AMOUNT                         
NETAMNT  DS    F                   NET CHECK AMOUNT                             
DUETOT   DS    F                   DUE COMP TOTAL                               
TOTDED   DS    F                   TOTAL DEDUCTIONS (MINUS DUE COMPANY)         
*                                                                               
OLDDED   DS    F                   TOTAL CREDIT DEDUCTIONS                      
OLDEARN  DS    F                   CREDIT TAXABLE EARNINGS                      
OLDNTAX  DS    F                   CREDIT NON-TAXABLE EARNINGS                  
OLDREXP  DS    F                   CREDIT REIMBURSED EXPENSES                   
OLDNET   DS    F                   CREDIT NET                                   
*                                                                               
MYDAMNTS DS    0F                  YTD AMOUNTS                                  
MYDEARN  DS    F                   YTD TAXABLE EARNINGS                         
MYDNTAX  DS    F                   YTD NON-TAXABLE EARNINGS                     
MYDREXP  DS    F                   YTD REIMBURSED EXPENSES                      
MYDFICA  DS    F                   YTD FICA                                     
MYDSUI   DS    F                   YTD SUI                                      
MYDSDI   DS    F                   YTD SDI                                      
MYDFTAX  DS    F                   YTD FEDERAL TAX                              
MYDSTAX  DS    F                   YTD STATE TAX                                
MYDLTAX  DS    F                   YTD LOCAL TAX                                
MYDMDED  DS    F                   YTD MISCELLANEOUS DEDUCTIONS                 
MYDDUES  DS    F                   YTD UNION DUES                               
MYDMPR   DS    F                   YTD MPR FUND                                 
MYDPCHAR DS    F                   YTD PERMANENT CHARITY                        
MYDCTX   DS    F                   YTD CANADIAN TAX                             
MYDNET   DS    F                   NET - FOR CHECK TAX TRANSFERS ONLY           
MYDAMNTL EQU   *-MYDAMNTS                                                       
*                                                                               
AEMPFLD  DS    A                   A(EMPLOYER FIELD)                            
ASSNFLD  DS    A                   A(SSN FIELD)                                 
ASSNNFLD DS    A                   A(SSN NAME FIELD)                            
*                                                                               
AEARNFLD DS    A                   A(TAXABLE EARNINGS FIELD)                    
ANTAXFLD DS    A                   A(NON-TAXABLE EARNINGS FIELD)                
AREXPFLD DS    A                   A(REIMBURSED EXPENSES FIELD)                 
AFTAXFLD DS    A                   A(FEDERAL TAX FIELD)                         
AFICAFLD DS    A                   A(FICA FIELD)                                
ASTAXFLD DS    A                   A(STATE TAX FIELD)                           
ASUIFLD  DS    A                   A(SUI FIELD)                                 
ASDIFLD  DS    A                   A(SDI FIELD)                                 
ALTAXFLD DS    A                   A(LOCAL TAX FIELD)                           
AMDEDFLD DS    A                   A(MISCELLANEOUS DEDUCTIONS FIELD)            
ADUESFLD DS    A                   A(UNION DUES FIELD)                          
AMPRFLD  DS    A                   A(MPR FUND FIELD)                            
APCHRFLD DS    A                   A(PERM CHARITY FIELD)                        
ACTXFLD  DS    A                   A(CANADIAN TAX FIELD)                        
NAMTS1   EQU   (*-AFTAXFLD)/L'AFTAXFLD                                          
NAMTS2   EQU   (*-AEARNFLD)/L'AEARNFLD                                          
ANETFLD  DS    A                   A(NET FIELD) - FOR TAX TRANSFERS             
NAMTS3   EQU   (*-AEARNFLD)/L'AEARNFLD                                          
         SPACE 1                                                                
AYTDTAB  DS    A                   A(YTD TABLE)                                 
*                                                                               
MYBYTE   DS    XL1                 LOCAL BYTE                                   
MYSSN    DS    CL9                 MY SSN                                       
MYPID    DS    CL6                 MY PID                                       
WSEND    EQU   *                                                                
*                                                                               
         EJECT                                                                  
WSHTD    DSECT                                                                  
WSHHDR   DS    CL8                                                              
WSHDESC  DS    CL25                DESCRIPTION                                  
         DS    CL2                                                              
WSHUNIT  DS    CL7                 UNIT                                         
         DS    CL2                                                              
WSHRATE  DS    CL10                RATE                                         
         DS    CL2                                                              
WSHAMNT  DS    CL11                AMOUNT                                       
*                                                                               
         ORG   WSHDESC                                                          
         DS    CL77                                                             
WSHLNQ   EQU   *-WSHHDR                                                         
         EJECT                                                                  
* TAGENWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE TAGENWORKD                                                     
         PRINT ON                                                               
* TAGENFILE                                                                     
* TASYSEQUS                                                                     
* TASYSDSECT                                                                    
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* TAGENEQUS                                                                     
* FAWSSVRD                                                                      
         PRINT OFF                                                              
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE TAGENEQUS                                                      
       ++INCLUDE FAWSSVRD                                                       
         PRINT ON                                                               
**********************************************************************          
*        DSECT FOR YTD TABLE                                         *          
**********************************************************************          
                                                                                
YTDTD    DSECT                                                                  
YTDTAB   DS    CL(NYTD*YTDLNQ+1)   YTD TABLE                                    
YTDTLNQ  EQU   *-YTDTD                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'169TAGENF9   08/11/14'                                      
         END                                                                    
