*          DATA SET TAREP44    AT LEVEL 085 AS OF 11/20/14                      
*PHASE T70344C,*                                                                
         TITLE 'T70344 - STALE DATED CHECKS FROM BANK TAPE'                     
T70344   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70344                                                         
         L     RC,0(R1)            RC=A(CONTROLLER W/S)                         
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(SCREEN)                                 
         USING T703FFD,RA                                                       
         L     R9,ASUBSYSD         R9=A(SYSTEM W/S)                             
         USING SUBSYSD,R9                                                       
         L     R8,ASPOOLD          R8=A(SPOOL DSECT)                            
         USING SPOOLD,R8                                                        
         LA    R7,BUFF                                                          
         LA    R7,8(R7)                                                         
         USING MYD,R7              R7=A(LOCAL W/S)                              
         SPACE 3                                                                
         GOTO1 INITIAL,DMCB,0                                                   
         SPACE 3                                                                
         CLI   MODE,VALKEY                                                      
         BNE   MODE2                                                            
         BAS   RE,VREC             VALIDATE SCREEN FIELDS                       
         B     XIT                                                              
         SPACE 3                                                                
MODE2    CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
         BAS   RE,VREC             VALIDATE SCREEN FIELDS                       
         BAS   RE,INIT             INITIALIZE                                   
         BAS   RE,PREP             READ TAPE,UPDATE CHKS,PRINT REPORT           
         B     XIT                                                              
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              VALIDATE RECORD                                                  
         SPACE 3                                                                
VREC     NTR1                                                                   
*                                                                               
         BAS   RE,VALOPT           VALIDATE OPTIONS                             
*                                                                               
         B     XIT                                                              
         SPACE 2                                                                
*              VALIDATE OPTIONS                                                 
VALOPT   NTR1                                                                   
         LA    R2,SPLOPTH          R2=A(OPTIONS FIELD)                          
         CLI   5(R2),0                                                          
         BE    VALOPTX                                                          
         LA    R3,BLOCK            R3=A(SCAN BLOCK)                             
         USING SCAND,R3                                                         
         GOTO1 SCANNER,DMCB,(R2),(R3),0                                         
         CLI   4(R1),0                                                          
         BE    INVERR                                                           
         ZIC   R0,4(R1)                                                         
*                                                                               
VALOPT2  CLC   =C'TRACE',SCDATA1  TRACE OPTION                                  
         BNE   INVERR                                                           
         CLI   SCDATA2,C'Y'                                                     
         BE    *+12                                                             
         CLI   SCDATA2,C'N'                                                     
         BE    *+8                                                              
         OI    SDOPTION,SDTRACE    SET TRACE ON                                 
         B     VALOPT30                                                         
*                                                                               
VALOPT30 LA    R3,SCANNEXT         BUMP TO NEXT                                 
         BCT   R0,VALOPT2                                                       
*                                                                               
VALOPTX  B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              ROUTINE TO INITIALIZE SOME COUNTERS, ADDRESSES                   
         SPACE                                                                  
INIT     NTR1                                                                   
*                                                                               
         ZAP   CHKCOUNT,=P'0'      INITIALIZE COUNTERS                          
         ZAP   ERRCOUNT,=P'0'                                                   
*                                                                               
         LA    R1,MYSPECS          SET A(SPECS FOR PRINTING)                    
         ST    R1,SPECS                                                         
         LA    R1,HOOK             SET A(HEADLINE HOOK)                         
         ST    R1,HEADHOOK                                                      
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------          
*              GET SYSTEM RECORD                                                
*              UPDATE 1 YR OLD CHECKS THAT WASN'T CASHED                        
*---------------------------------------------------------------------          
PREP     NTR1                                                                   
         BAS   RE,SETTAL                                                        
         XC    LASTACC,LASTACC                                                  
*                                                                               
PREP5    LA    R0,NCHUNKS          R0=A(NUMBER OF CHUNKS PER LINE)              
         LA    R2,P+1              R2=A(PRINT LINE)                             
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(0,TODAYD)                                     
         GOTO1 ADDAY,DMCB,(C'Y',TODAYD),WORK,-1                                 
         GOTO1 DATCON,DMCB,(0,WORK),(1,LYEARD)                                  
*                                                                               
PREP100  BAS   RE,GETSYS           GET SYSTEM RECORD FOR CHECK LOOKUPS          
*                                                                               
         LA    R3,STLUCHK          R3=A(TAPE RECORD)                            
PREP200  CLI   0(R3),X'FF'                                                      
         BE    PREP900                                                          
*                                                                               
         BAS   RE,SETCHK           SET CHECK FILES                              
         BAS   RE,CHECKUP          FIND AND UPDATE CHECK RECORDS                
*                                                                               
PREP800  AHI   R3,8                BUMP TO NEXT LOOKUP                          
         B     PREP200             BANK CODE MUST BE REVERSAL                   
*                                                                               
PREP900  BAS   RE,SETTAL                                                        
         BAS   RE,UPDSYS                                                        
*                                                                               
         BAS   RE,PRNTOTS          PRINT LAST LINE & TOTALS                     
         B     XIT                                                              
         EJECT                                                                  
*              GET THE SYSTEM RECORD                                            
GETSYS   NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING TLSYD,R4                                                         
         MVI   TLSYCD,TLSYCDQ      READ SYSTEM RECORD                           
         GOTO1 HIGH                                                             
         CLC   KEY(TLCKCBNK-TLCKPD),KEYSAVE                                     
         BE    GSYS010                                                          
         DC    H'0'                                                             
*                                                                               
GSYS010  GOTO1 GETREC              GET SYSTEM RECORD                            
         L     R4,AIO                                                           
         MVI   ELCODE,TASUELQ      GET STALE DATE ELEMENT                       
         BAS   RE,GETEL                                                         
         BE    GSYS020                                                          
         MVC   STLUCHK,=C'30000000'    DEFAULT (LAST LASALLE ONES)              
         MVC   STLCCHK,=C'01200000'                                             
         MVC   STLPCHK,=C'00400000'                                             
         MVC   STLPLCK,=C'80000000'                                             
         B     GSYSX                                                            
*                                                                               
         USING TASUD,R4                                                         
GSYS020  MVC   STLUCHK,TASUUCSD    USE MOST CURRENT CHECK LOOKUPS               
         MVC   STLCCHK,TASUCCSD                                                 
         MVC   STLPCHK,TASUPCSD                                                 
         MVC   STLPLCK,TASULCSD                                                 
*                                                                               
GSYSX    MVI   STLEND,X'FF'                                                     
         GOTO1 MYTRACE,DMCB,=C'UPDATED SYSTEM RECORD'                           
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
*              UPDATE SYSTEM RECORD                                             
UPDSYS   NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING TLSYD,R4                                                         
         MVI   TLSYCD,TLSYCDQ      READ SYSTEM RECORD                           
         GOTO1 HIGH                                                             
         CLC   KEY(TLCKCBNK-TLCKPD),KEYSAVE                                     
         BE    USYS010                                                          
         DC    H'0'                                                             
*                                                                               
USYS010  MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC              GET SYSTEM RECORD                            
*                                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,TASUELQ      GET STALE DATE ELEMENT                       
         GOTO1 REMELEM                                                          
*                                                                               
         USING TASUD,R4                                                         
         LA    R4,ELEM                                                          
         MVI   TASUEL,TASUELQ      GET STALE DATE ELEMENT                       
         MVI   TASULEN,TASULNQ                                                  
         MVC   TASUUCSD,STLUCHK    UPDATE LATEST LOOKUP CHECK NUMS              
         MVC   TASUCCSD,STLCCHK                                                 
         MVC   TASUPCSD,STLPCHK                                                 
         MVC   TASULCSD,STLPLCK                                                 
         GOTO1 ADDELEM                                                          
*                                                                               
         GOTO1 PUTREC                                                           
         GOTO1 MYTRACE,DMCB,=C'UPDATED SYSTEM RECORD'                           
USYSX    B     XIT                                                              
         EJECT                                                                  
*              UPDATE CHECK AS STALE DATED                                      
*                                                                               
CHECKUP  NTR1                                                                   
         MVC   WORK(8),0(R3)       CONVERT LOOKUP NUMBER TO PACK                
         NI    WORK+7,X'CF'                                                     
         PACK  CURRLOOK,WORK(8)                                                 
         XR    R5,R5                                                            
*                                                                               
CHKUP100 XC    KEY,KEY             READ CHECK RECORD                            
         LA    R4,KEY                                                           
         USING TLCKPD,R4                                                        
         MVI   TLCKPCD,TLCKCCDQ    READ CHECK PASSIVE                           
         MVC   TLCKCCHK,0(R3)      CHECK NUMBER                                 
         XC    TLCKCCHK,XFFS       COMPLEMENTED                                 
         GOTO1 HIGH                                                             
         CLC   KEY(TLCKCBNK-TLCKPD),KEYSAVE                                     
         BNE   CHKUP500            NOT FOUND, GET NEXT ONE                      
*                                                                               
         GOTO1 GETREC              GET CHECK RECORD                             
         L     R4,AIO                                                           
         MVI   ELCODE,TAPDELQ      GET PAY DETAILS ELEMENT                      
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAPDD,R4                                                         
         TM    TAPDADJS,TAPDADVD   ADJUSTMENT, VOID                             
         BO    CHKUP500                                                         
*                                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,TACDELQ      GET CHECK DETAILS ELEMENT                    
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACDD,R4                                                         
*                                                                               
         CLC   TACDDTE,LYEARD      CHECK DATE LESS THAN 1 YEAR OLD?             
         BH    CHKUP900            YES, SKIP TO NEXT ONE                        
                                                                                
         OC    TACDCSH,TACDCSH     CASHED?                                      
         BNZ   CHKUP500            YES, SKIP TO NEXT ONE                        
         TM    TACDSTAT,TACDSVOI+TACDSSTA                                       
         BNZ   CHKUP500                                                         
         OC    TACDNET,TACDNET     DON'T BOTHER WITH $0 CHECKS                  
         BZ    CHKUP500                                                         
*                                                                               
         LA    R2,P+1                                                           
         CHI   R5,0                                                             
         BE    CHKUP200                                                         
         LA    R2,P+42                                                          
         CHI   R5,1                                                             
         BE    CHKUP200                                                         
         LA    R2,P+83                                                          
*                                                                               
CHKUP200 MVC   0(8,R2),TACDCHK                                                  
         EDIT  TACDNET,(10,12(R2)),2,MINUS=YES                                  
         OI    TACDSTAT,TACDSSTA   SET CHECK RECORD STALE DATED                 
         AHI   R5,1                                                             
         CHI   R5,NCHUNKS          FILLED UP LINE WITH 3 CHECKS?                
         BL    CHKUP300            NO, CONTINUE                                 
         XR    R5,R5                                                            
*                                                                               
         BAS   RE,SPLAT            YES, PRINT LINE                              
*                                                                               
CHKUP300 GOTO1 PUTREC                                                           
         AP    CHKCOUNT,=P'1'      ADD TO CHECKS UPDATED                        
         MVI   TGBYTE,1                                                         
*                                                                               
         GOTO1 MYTRACE,DMCB,=C'STALE CHECK REC'                                 
*                                                                               
CHKUP500 AP    CURRLOOK,=P'1'      BUMP CHECK NUMBER UP                         
         UNPK  0(8,R3),CURRLOOK                                                 
         OI    7(R3),X'F0'                                                      
         B     CHKUP100                                                         
*                                                                               
CHKUP900 CHI   R5,1                PRINT ANY CHECKS LEFT OVER                   
         BL    XIT                                                              
         BAS   RE,SPLAT                                                         
         B     XIT                                                              
*                                                                               
         DROP  R3,R4                                                            
         EJECT                                                                  
*              ROUTINE TO PRINT ONE LINE OF CHECK INFO                          
         SPACE                                                                  
         USING BANKD,R3            R3=A(BANK RECORD)                            
         USING PRINTD,R2           R2=A(PRINT LINE)                             
PLINE    NTR1                                                                   
*                                                                               
         MVC   PRINTCHK,BANKCHK+2               CHECK NUMBER                    
         EDIT  (C10,BANKAMT),(12,PRINTAMT),2    CHECK AMOUNT                    
*                                                                               
         MVC   PRINTERR,SPACES                                                  
         TM    CHKERR,CHKENFND     IF CHECK RECORD NOT FOUND                    
         BZ    *+14                                                             
         MVC   PRINTERR,LTNOTFND   PRINT NOT FOUND                              
         B     PLINEX                                                           
*                                                                               
         TM    CHKERR,CHKENEQ      IF CHECK RECORD AMOUNTS DON'T MATCH          
         BZ    *+10                                                             
         MVC   PRINTERR,LTNOTEQ    PRINT AMOUNTS DON'T MATCH                    
*                                                                               
PLINEX   B     XIT                                                              
         DROP  R2,R3                                                            
         SPACE 2                                                                
*              ROUTINE TO PRINT A LINE                                          
         SPACE                                                                  
SPLAT    NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO PRINT LAST LINE AND TOTALS                            
         SPACE                                                                  
PRNTOTS  NTR1                                                                   
         CLC   P,SPACES            IF ANYTHING LEFT TO PRINT                    
         BE    *+8                                                              
         BAS   RE,SPLAT            PRINT IT                                     
*                                                                               
         CP    CHKCOUNT,=P'0'      IF CHECK RECORDS UPDATED                     
         BE    PRNTOT5                                                          
         EDIT  CHKCOUNT,(12,P+1),COMMAS=YES,ALIGN=LEFT                          
         LR    R1,R0                                                            
         LA    R1,P+2(R1)                                                       
         MVC   0(21,R1),=CL21'CHECK RECORDS UPDATED'                            
         BAS   RE,SPLAT                                                         
*                                                                               
PRNTOT5  CP    ERRCOUNT,=P'0'                                                   
         BE    PRNTOTX                                                          
         EDIT  ERRCOUNT,(12,P+1),COMMAS=YES,ALIGN=LEFT                          
         LR    R1,R0                                                            
         LA    R1,P+2(R1)                                                       
         MVC   0(22,R1),=CL22'CHECK RECORDS IN ERROR'                           
         BAS   RE,SPLAT                                                         
*                                                                               
PRNTOTX  ZAP   CHKCOUNT,=P'0'      INITIALIZE COUNTERS                          
         ZAP   ERRCOUNT,=P'0'                                                   
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO SET FILES TO CHECK                                    
         SPACE                                                                  
SETCHK   NTR1                                                                   
         MVC   SYSFIL,=CL8'CHKFIL'                                              
         MVC   SYSDIR,=CL8'CHKDIR'                                              
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO SET FILES TO TALENT                                   
         SPACE                                                                  
SETTAL   NTR1                                                                   
         MVC   SYSFIL,=CL8'TALFIL'                                              
         MVC   SYSDIR,=CL8'TALDIR'                                              
         B     XIT                                                              
         EJECT                                                                  
*              HEADLINES HOOK                                                   
         SPACE                                                                  
         USING BANKD,R3            R3=A(BANK RECORD)                            
HOOK     NTR1                                                                   
         MVC   H2+11(L'BANKACC),BANKACC SET BANK ACCOUNT                        
         XIT1                                                                   
         DROP  R3                                                               
         SPACE 2                                                                
*              TRACE ROUTINE                                                    
         SPACE                                                                  
MYTRACE  NTR1                                                                   
         TM    SDOPTION,SDTRACE    IF TRACE ON                                  
         BZ    MYTRACEX                                                         
*                                                                               
         L     R2,0(R1)            LITERAL                                      
         ZIC   R3,0(R1)            LENGTH OF LITERAL                            
         GOTO1 TRACE,DMCB,AIO,0,(R2),(R3)                                       
MYTRACEX B     XIT                                                              
         SPACE 2                                                                
INVERR   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         GOTO1 ERREX                                                            
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         LTORG                                                                  
         EJECT                                                                  
*              REPORT SPECS                                                     
         SPACE 1                                                                
MYSPECS  SSPEC H1,2,RUN                                                         
         SSPEC H1,100,REPORT                                                    
         SSPEC H1,120,PAGE                                                      
         SSPEC H2,100,REQUESTOR                                                 
         SSPEC H1,53,C'STALE DATED CHECKS'                                      
         SSPEC H2,53,18X'BF'                                                    
         SSPEC H2,2,C'BANK ACC'                                                 
         SPACE 1                                                                
         SSPEC H7,02,C'CHECK NO   NET AMOUNT ERROR MESSAGE'                     
         SSPEC H8,02,C'--------   ---------- -------------'                     
         SSPEC H7,43,C'CHECK NO   NET AMOUNT ERROR MESSAGE'                     
         SSPEC H8,43,C'--------   ---------- -------------'                     
         SSPEC H7,84,C'CHECK NO   NET AMOUNT ERROR MESSAGE'                     
         SSPEC H8,84,C'--------   ---------- -------------'                     
         DC    H'0'                                                             
         SPACE 3                                                                
XFFS     DC    8X'FF'                                                           
LTNOTFND DC    CL17'RECORD NOT FOUND'                                           
LTNOTEQ  DC    CL17'AMOUNTS NOT EQUAL'                                          
*                                                                               
         EJECT                                                                  
*              MY WORKING STORAGE                                               
         SPACE 3                                                                
MYD      DSECT                                                                  
*                                                                               
NCHUNKS  EQU   3                   NUMBER OF CHUNKS PER LINE                    
*                                                                               
SDOPTION DS    XL1                 OPTIONS                                      
SDTRACE  EQU   X'80'               TRACE RECORDS                                
*                                                                               
LASTACC  DS    XL10                LAST BANK ACCOUNT                            
*                                                                               
CHKERR   DS    XL1                 CHECK ERRORS                                 
CHKENFND EQU   X'80'               CHECK RECORD NOT FOUND                       
CHKENEQ  EQU   X'40'               CHECK AMTS <> BANK AMOUNTS                   
*                                                                               
CHKCOUNT DS    PL8                 CHECK COUNT                                  
ERRCOUNT DS    PL8                 ERROR COUNT                                  
*                                                                               
TODAYD   DS    CL6                 TODAY'S DATE                                 
LYEARD   DS    XL3                 LAST YEAR'S DATE                             
*                                                                               
CURRLOOK DS    PL5                 CUURENT CHECK LOOKUP                         
STLUCHK  DS    CL8                 US$ STALE DATED NEXT LOOKUP                  
STLCCHK  DS    CL8                 CAN$ STALE DATED NEXT LOOKUP                 
STLPCHK  DS    CL8                 PRINT STALE DATED NEXT LOOKUP                
STLPLCK  DS    CL8                 P+ STALE DATED NEXT LOOKUP                   
STLEND   DS    X'FFFF'                                                          
*                                                                               
BANKREC  DS    0CL(BANKLNQ)        BANK RECORD FROM TAPE                        
MYEND    DS    0D                                                               
*                                                                               
         EJECT                                                                  
*              DSECT TO COVER BANK RECORD 1 (BANKTYPR)                          
         SPACE                                                                  
BANKD    DSECT                                                                  
BANKTYP  DS    CL1                 RECORD TYPE                                  
BANKTYPR EQU   C'*'                DETAIL CHECK RECORD                          
BANKCODE DS    CL2                 09                                           
         DS    CL3                 BANK NUMBER                                  
BANKACC  DS    CL10                ACCOUNT #                                    
BANKCHK  DS    CL10                BANK CHECK NUMBER                            
         DS    CL8                 CHECK ISSUE DATE                             
BANKAMT  DS    CL10                BANK AMOUNT                                  
         DS    CL38                FILLER                                       
*BANKTYP  DS    CL1                 RECORD TYPE                                 
*BANKTYPH EQU   C'H'                HEADER RECORD                               
*BANKTYPR EQU   C'R'                DETAIL CHECK RECORD                         
*BANKTYPT EQU   C'T'                TRAILER RECORD                              
*BANKACC  DS    CL10                ACCOUNT #                                   
*         DS    CL2                 BANK NUMBER                                 
*BANKCHK  DS    CL8                 BANK CHECK NUMBER                           
*BANKAMT  DS    CL10                BANK AMOUNT                                 
*         DS    CL6                 CHECK ISSUE DATE                            
*BANKCODE DS    CL2                 SD                                          
*         DS    CL39                CLIENTS DESCRIPTION                         
BANKLNQ  EQU   *-BANKD                                                          
         EJECT                                                                  
*               DSECT FOR PRINT LINE                                            
PRINTD   DSECT                                                                  
PRINTCHK DS    CL8                 BANK CHECK NUMBER                            
         DS    CL1                                                              
PRINTAMT DS    CL12                BANK CHECK AMOUNT                            
         DS    CL1                                                              
PRINTERR DS    CL17                ERROR MESSAGE                                
         DS    CL2                                                              
PRINTLNQ EQU   *-PRINTD                                                         
         EJECT                                                                  
       ++INCLUDE TAREPFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPE2D                                                       
         EJECT                                                                  
*DDGENTWA  (MUST FOLLOW LAST SCREEN)                                            
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*DDBIGBOX                                                                       
*TAGENFILE                                                                      
*TASYSDSECT                                                                     
*TASYSEQUS                                                                      
*DDTWADCONS                                                                     
*TAREPWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
TWADCOND DSECT                                                                  
       ++INCLUDE DDTWADCONS                                                     
       ++INCLUDE TAREPWORKD                                                     
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'085TAREP44   11/20/14'                                      
         END                                                                    
