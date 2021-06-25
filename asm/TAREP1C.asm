*          DATA SET TAREP1C    AT LEVEL 091 AS OF 03/04/16                      
*PHASE T7031CC,*                                                                
*INCLUDE POWWOW                                                                 
         TITLE 'T7031C - BANK RECONCILIATION TAPE/REPORT'                       
T7031C   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T7031C,R6                                                      
         L     RC,0(R1)                                                         
         USING GEND,RC             RC=A(CONTROLLER W/S)                         
         L     RA,ATWA                                                          
         USING T703FFD,RA          RA=A(SCREEN)                                 
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          R9=A(SYSTEM W/S)                             
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8           R8=A(SPOOL DSECT)                            
         LA    R7,BUFF                                                          
         LA    R7,8(R7)                                                         
         USING TBD,R7              R7=A(LOCAL W/S)                              
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
         SPACE 1                                                                
         GOTO1 INITIAL,DMCB,0      INITIALIZE                                   
         SPACE 1                                                                
         CLI   MODE,VALKEY         VALIDATE SCREEN                              
         BNE   *+12                                                             
         BAS   RE,VKEY                                                          
         B     XIT                                                              
         SPACE 3                                                                
         CLI   MODE,PRINTREP       PROCESS REPORT                               
         BNE   XIT                                                              
         MVC   PPBANK,CPPBANK      DEFAULT - CHASE ACCOUNTS                     
         MVC   USABANK,CUSABANK                                                 
*        CLI   RECNUM,CF                                                        
*        BNE   *+16                                                             
*        MVC   PPBANK,LPPBANK      LASALLE ACCOUNTS                             
*        MVC   USABANK,LUSABANK                                                 
         BAS   RE,PREP                                                          
         B     XIT                                                              
*        OPEN  (BCTAPE,INPUT)      ERROR REPORT                                 
*        BAS   RE,PREPC                                                         
*        CLOSE (BCTAPE)                                                         
*        B     XIT                                                              
         EJECT                                                                  
*              VALIDATE SCREEN                                                  
         SPACE 1                                                                
VKEY     NTR1                                                                   
***      XC    TBD(TBLNQ),TBD  CLEAR LOCAL W/S                                  
         BAS   RE,CLRWS        CLEAR LOACAL WORKING STORAGE                     
*                                                                               
         MVC   TBCHUNK,SPACES                                                   
*                                                                               
*        CLI   RECNUM,CF                                                        
*        BE    VK05                                                             
         LA    R2,SBKPDH           VALIDATE PERIOD                              
         LA    R3,BLOCK                                                         
         USING PERVALD,R3                                                       
         GOTO1 PDVAL,DMCB,(R3)     GO TO SYSTEM PERIOD VAL. ROUTINE             
         MVC   TBPERIOD,PVALCPER   SAVE DISPLAYABLE PERIOD IN W/S               
         MVC   TIQPSTR,PVALPSTA    SET PWOS DATES FOR SYSIO                     
         MVC   TIQPEND,PVALPEND                                                 
         SPACE 1                                                                
         LA    R2,SBKRUNH          VALIDATE RUN DATES                           
         GOTO1 PDVAL,DMCB,(X'80',(R3)) GO TO SYSTEM PERIOD VAL. ROUTINE         
         MVC   TIQRSTR,PVALPSTA    SET PWOS DATES FOR SYSIO                     
         MVC   TIQREND,PVALPEND                                                 
         SPACE 1                                                                
VK05     BAS   RE,VALOPT           VALIDATE OPTIONS                             
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO VALIDATE OPTIONS                                      
         SPACE 1                                                                
VALOPT   NTR1                                                                   
         LA    R2,SBKOPTH          VALIDATE OPTIONS                             
         CLI   5(R2),0                                                          
         BE    VOPTX                                                            
         SPACE 1                                                                
         LA    R3,BLOCK            R3=A(SCAN BLOCK)                             
         USING SCAND,R3                                                         
         GOTO1 SCANNER,DMCB,(R2),(X'80',(R3))                                   
         CLI   4(R1),0                                                          
         BE    FLDINV                                                           
         ZIC   R0,4(R1)            R0=N'SCAN BLOCK ENTRIES                      
         SPACE 1                                                                
VOPT4    CLC   =C'TRACE',SCDATA1   TRACE                                        
         BNE   VOPT6                                                            
         CLI   SCDATA2,C'Y'                                                     
         BNE   FLDINV                                                           
         OI    TBOPTS,TBTRACE      SET TRACE ON                                 
         B     VOPT40                                                           
*                                                                               
VOPT6    DS    0H                                                               
*        CLI   RECNUM,CF                                                        
*        BE    FLDINV                                                           
         CLC   =C'NOPOW',SCDATA1   DON'T PERFORM PWOWWOW CALLS                  
         BNE   FLDINV                                                           
         OI    TBOPTS,TBNOPOW      SET INDICTOR ON                              
*                                                                               
VOPT40   LA    R3,SCANNEXT         BUMP TO NEXT                                 
         BCT   R0,VOPT4            AND CONTINUE                                 
         SPACE 1                                                                
VOPTX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE CONTROLS REPORT GENERATION                               
         SPACE 1                                                                
PREP     NTR1                                                                   
         MVC   SPECS,=A(MYSPECS)   SET A(SPECS) FOR PRINTING                    
         LA    R1,HOOK             SET A(HEADLINE HOOK)                         
         ST    R1,HEADHOOK                                                      
         LA    R1,IOHOOK           A(I/O HOOK)                                  
         ST    R1,TIHOOK                                                        
         MVC   TIACOMFC,ACOMFACS   SET UP SYSIO BLOCK                           
         MVC   TIACCS,TWAACCS      LIMIT ACCESS                                 
         MVC   TIAUTH,TWAAUTH      AUTHORIZATION                                
         MVC   TIUSERID,TWAORIG    REQUESTING ID                                
         MVI   TIREAD,TLCKCDQ      READ CHECK RECORDS                           
         MVI   TIQDTYPE,TIQDCHK    SET FILTERING ON CHECK DATE                  
         XC    TIFEMP,TIFEMP       FILTER EMPLOYER AT CHECK LEVEL               
         MVI   TIFCUR,C'U'         US                                           
         CLI   RECNUM,CB           CANADIAN BANK REC?                           
         BNE   *+8                                                              
         MVI   TIFCUR,C'C'         CANADIAN                                     
*                                                                               
         MVC   TIFEMP,=C'DM '      EXCLUDE EMP DM                               
         NI    TIFEMP,X'BF'                                                     
         SPACE 1                                                                
         GOTO1 TASYSIO,DMCB,TASYSIOD  OFF TO SYSIO TO DO I/O                    
         TM    TBSTAT,TBSORTNG     DON'T BOTHER IF SORT NOT ACTIVE              
         BZ    PRX                                                              
*                                                                               
         CLI   RECNUM,CB           CANADIAN BANK REC?                           
         BNE   PR080                                                            
         OPEN  (CBTAPE,OUTPUT)     ELSE PROCESS OPEN TAPE                       
         MVC   DSNME,=CL8'CBTAPE'                                               
         B     PR100                                                            
*                                                                               
PR080    CLI   RECNUM,BX           P+ BANK REC?                                 
         BNE   PR090                                                            
         OPEN  (BXTAPE,OUTPUT)     ELSE PROCESS BK/PK OPEN TAPE                 
         MVC   DSNME,=CL8'BXTAPE'                                               
         B     PR100                                                            
                                                                                
PR090    OPEN  (BKTAPE,OUTPUT)     ELSE PROCESS BK/PK OPEN TAPE                 
PR100    LINKX MF=(E,PARMLST),SF=(E,LINKLST)  GET GENERATION NUMBER             
*                                                                               
         LA    R3,TBRECORD         R3=A(TAPE RECORD AREA)                       
         USING RECD,R3                                                          
         BAS   RE,GETSORT          GET SORT RECORDS/WRITE TO TAPE               
*                                                                               
         LA    R2,BKTAPE                                                        
         CLI   RECNUM,CB                                                        
         BNE   *+8                                                              
         LA    R2,CBTAPE                                                        
         CLI   RECNUM,BX                                                        
         BNE   *+8                                                              
         LA    R2,BXTAPE                                                        
         CLOSE (R2)                CLOSE TAPE                                   
         BRAS  RE,CALLPOW          CALL POWWOW TO SEND TO ADVANTIS              
         SPACE 1                                                                
PRX      B     XIT                                                              
         EJECT                                                                  
*              PROCESS RECORDS FROM SYSIO AND WRITE TO SORT                     
         SPACE 1                                                                
IOHOOK   NTR1                                                                   
         LA    R2,TBSRTREC         R2=A(SORT RECORD)                            
         USING SORTD,R2                                                         
*                                                                               
         L     R4,TIAREC                                                        
         CLI   0(R4),TLINCDQ       IF INVOICE RECORD                            
         BNE   *+12                                                             
         BAS   RE,PRCINV           PROCESS INVOICE                              
         B     IOHX                                                             
         SPACE 1                                                                
         CLI   0(R4),TLCKCDQ       IF CHECK RECORDS                             
         BNE   *+8                                                              
         BAS   RE,PRCCHK           PROCESS CHECKS                               
*                                                                               
IOHX     B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PROCESS INVOICE RECORDS FROM SYSIO                    
         SPACE 1                                                                
PRCINV   NTR1                                                                   
                                                                                
         XC    TBSRTREC,TBSRTREC   PRE-CLEAR SORT RECORD                        
         MVI   ELCODE,TAPDELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAPDD,R4            R4=A(PAYMENT DETAILS ELEMENT)                
*                                  IF TAX REFUND OR TANSFER INVOICE             
                                                                                
         CLI   RECNUM,BX           IF BANK REC FOR P+ ?                         
         BNE   PRCINV1                                                          
         CLC   TAPDEMP,=C'P+ '     MUST BE EMPLOYER P+                          
         BNE   XIT                                                              
         B     PRCINV3                                                          
                                                                                
PRCINV1  CLC   TAPDEMP,=C'P+ '     CANT BE EMPLOYER P+                          
         BE    XIT                                                              
                                                                                
PRCINV3  TM    TAPDADJS,TAPDADTR+TAPDADTT+TAPDADST                              
         BZ    PRCINV5                                                          
         MVC   SORTBNK,PPBANK                                                   
         CLC   TAPDEMP,=C'PP '     CHECK EMP BECAUSE NO TACOD ELEMENT           
         BE    *+10                                                             
         MVC   SORTBNK,USABANK                                                  
         CLC   TAPDEMP,=C'TP '     TP                                           
         BE    *+10                                                             
         MVC   SORTBNK,PPLSBANK    P+                                           
         CLI   RECNUM,CB                                                        
         BNE   *+10                                                             
         MVC   SORTBNK,CANBANK                                                  
         B     XIT                                                              
*                                                                               
PRCINV5  L     R4,TIAREC                                                        
         MVI   ELCODE,TACOELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACOD,R4            R4=A(COMMERCIAL DETAILS ELEMENT)             
*                                                                               
         CLI   TACOMED,TACOMEDP                                                 
         BNE   *+14                                                             
         MVC   SORTBNK,PPBANK                                                   
         B     XIT                                                              
                                                                                
         CLI   TACOMED,TACOMEDE                                                 
         BNE   *+14                                                             
         MVC   SORTBNK,PPLSBANK                                                 
         B     XIT                                                              
                                                                                
         MVC   SORTBNK,USABANK                                                  
         CLI   RECNUM,CB                                                        
         BNE   *+10                                                             
         MVC   SORTBNK,CANBANK                                                  
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PROCESS CHECK RECORDS FROM SYSIO                      
         SPACE 1                                                                
PRCCHK   NTR1                                                                   
         MVI   ELCODE,TACDELQ      GET CHECK DETAILS ELEMENT                    
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACDD,R4            R4=A(CHECK DETAILS EL.)                      
*                                                                               
         LA    RF,=CL6'8004'       TBK CHECKS FOR BANK ACCT 8004                
         CLI   RECNUM,BK                                                        
         BE    PRCCHK03                                                         
         LA    RF,=CL6'8304'       TCB CHECKS FOR BANK ACCT 8304                
         CLI   RECNUM,CB                                                        
         BE    PRCCHK03                                                         
         LA    RF,=CL6'7004'       TCF CHECKS FOR BANK ACCT 7004                
                                                                                
PRCCHK03 CLC   SORTBNK,PPLSBANK                                                 
         BNE   *+8                                                              
         LA    RF,=CL6'8005'                                                    
         CLC   TACDBNK,0(RF)                                                    
         BNE   XIT                 INGORE IF NOT 7004                           
*                                                                               
*&&DO                                                                           
*----------------------------------------------------------------------         
**  FOR TAL2                                                                    
         LA    RF,=CL6'2004'       TBK CHECKS FOR BANK ACCT 8004                
         CLI   RECNUM,BK                                                        
         BE    PRCCHK03                                                         
         LA    RF,=CL6'1004'       TCB CHECKS FOR BANK ACCT 8304                
         CLI   RECNUM,CB                                                        
         BE    PRCCHK03                                                         
         LA    RF,=CL6'1004'       TCF CHECKS FOR BANK ACCT 7004                
         CLI   RECNUM,CF                                                        
         BNE   XIT                                                              
                                                                                
PRCCHK03 CLC   SORTBNK,PPLSBANK                                                 
         BNE   *+8                                                              
         LA    RF,=CL6'2005'                                                    
         CLC   TACDBNK,0(RF)                                                    
         BNE   XIT                 INGORE IF NOT 7004                           
**  FOR TAL2                                                                    
*&&                                                                             
*----------------------------------------------------------------------         
         OC    TACDNET,TACDNET                                                  
         BZ    XIT                 IGNORE ZERO DOLLAR CHECKS                    
         OC    TACDCHK,TACDCHK                                                  
         BZ    XIT                 IGNORE IF NO CHECK NUMBER                    
         OC    TACDBNK,TACDBNK                                                  
         BZ    XIT                 IGNORE IF NO BANK ACCOUNT                    
*        CLI   RECNUM,CB                                                        
*        BNE   PRCCHK05                                                         
*        CLC   =C'8304',TACDBNK                                                 
*        BNE   XIT                                                              
*        B     PRCCHK08                                                         
*RCCHK05 CLC   =C'8004',TACDBNK                                                 
*        BNE   XIT                 INGORE IF NOT 7004                           
*                                                                               
PRCCHK08 TM    TBSTAT,TBSORTNG     IS SORT ACTIVE YET                           
         BO    PRCCHK10                                                         
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD                                     
         OI    TBSTAT,TBSORTNG     SORT IS ACTIVE                               
*                                                                               
PRCCHK10 MVC   SORTCHK,TACDCHK     CHECK NUMBER                                 
         MVC   SORTDTE,TACDDTE     CHECK DATE                                   
         MVC   SORTAMT,TACDNET     CHECK AMOUNT                                 
         MVC   SORTSTAT,TACDSTAT   CHECK STATUS                                 
*                                                                               
         BAS   RE,EXTRW4           EXTRACT INFO FROM W4 RECORD                  
*                                                                               
PRCCHK20 GOTO1 SORTER,DMCB,=C'PUT',(R2)  WRITE OUT SORT RECORD                  
         B     XIT                                                              
         EJECT                                                                  
* READ THE W4 RECORD FOR THE CURRENT PERFORMER AND EXTRACT INFORMATION          
* FROM IT INTO THE SORT RECORD.                                                 
*                                                                               
EXTRW4   NTR1                                                                   
*                                                                               
         MVC   SORTNAM2,SPACES                                                  
         MVC   SORTNAM3,SPACES                                                  
*                                                                               
         MVC   AIO,AIO2            READ W4 RECORD INTO AIO2                     
*                                                                               
         L     R4,TIAREC                                                        
         MVI   ELCODE,TATIELQ      IF TAX ID ELEMENT EXISTS,                    
         BAS   RE,GETEL            USE CORP ID SSN                              
         BNE   EW10                                                             
         USING TATID,R4                                                         
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'A0',TATIID)                               
         BE    *+6                                                              
         DC    H'00'                                                            
         BAS   RE,EXTRCRP          EXTRACT CORP INFO                            
         B     EW50                                                             
         DROP  R4                                                               
                                                                                
*                                                                               
         USING TLCKD,R4                                                         
EW10     L     R4,TIAREC                                                        
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'A0',TLCKSSN)                              
         BE    *+6                                                              
         DC    H'00'                                                            
         DROP  R4                                                               
         BRAS  RE,EXTRNAM          EXTRACT NAME INFO                            
*                                                                               
         MVC   SORTNAME,SAVNAME    SAVE IN SORT RECORD                          
         OC    SORTNAME,SPACES     GET RID OF ZEROS                             
*                                                                               
         BAS   RE,EXTRTRST         EXTRACT TRUSTEE INFO                         
         BNE   EW20                                                             
         MVC   SORTNAM2(12),=C'IN TRUST FOR'                                    
         MVC   SORTNAM2+13(22),SAVNAME  SAVE IN SORT RECORD                     
         OC    SORTNAM2,SPACES                                                  
         B     EW50                                                             
*                                                                               
EW20     BAS   RE,EXTRAGNT         EXTRACT AGENT INFO                           
         BNE   EW50                                                             
         MVC   SORTNAM2(3),=C'C/O'                                              
         MVC   SORTNAM2+4(31),SAVNAME  SAVE IN SORT RECORD                      
         OC    SORTNAM2,SPACES     GET RID OF ZEROS                             
*                                                                               
EW50     BAS   RE,SETCHK           SET SYSFIL/DIR TO CHECK FILE                 
         XC    KEY,KEY                                                          
         MVC   KEY,TIKEY           RESTORE LAST RECORD READ BY SYSIO            
         GOTO1 HIGH                                                             
         CLC   KEY(L'TIKEY),KEYSAVE                                             
         BE    *+6                                                              
         DC    H'00'                                                            
         GOTO1 GETREC                                                           
         MVC   AIO,TIAREC                                                       
         BAS   RE,SETTAL           RESET SYSFIL/DIR BACK TO TAL FILE            
*                                                                               
EWX      B     XIT                                                              
         EJECT                                                                  
*        SET SYSFIL/DIR TO CHECK FILE                                           
*                                                                               
         SPACE 1                                                                
SETCHK   MVC   SYSFIL,=CL8'CHKFIL'                                              
         MVC   SYSDIR,=CL8'CHKDIR'                                              
         BR    RE                                                               
         SPACE 2                                                                
*                                                                               
*        SET SYSFIL/DIR TO TALENT FILE                                          
*                                                                               
         SPACE 1                                                                
SETTAL   MVC   SYSFIL,=CL8'TALFIL'                                              
         MVC   SYSDIR,=CL8'TALDIR'                                              
         BR    RE                                                               
         EJECT                                                                  
*---------------------------------------------------------------------          
* EXTRCRP - EXTRACTS CORP INFORMATION FOR 3 PAYEE LINES                         
*---------------------------------------------------------------------          
EXTRCRP  NTR1                                                                   
         L     R4,AIO              R4 = A(W4 DETAILS ELEMENT)                   
         MVI   ELCODE,TAW4ELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         USING TAW4D,R4                                                         
*                                  SAVE NAME FOR SORT KEY                       
         MVC   LASTNAME(32),TAW4NAM2                                            
         MVC   MIDNAME,SPACES                                                   
         CLI   TAW4LEN,TAW4LN2Q    DO WE HAVE MIDDLE INITIAL?                   
         BNE   *+10                                                             
         MVC   MIDNAME,TAW4MIDN                                                 
*                                                                               
         CLI   TAW4TYPE,TAW4TYCO   IF PERFORMER IS NOT CORPORATION              
         BE    EXCRP10                                                          
         CLI   TAW4TYPE,TAW4TYTR   OR TRUSTEE                                   
         BE    EXCRP10                                                          
         CLI   TAW4TYPE,TAW4TYES   OR ESTATE                                    
         BE    EXCRP10                                                          
         BAS   RE,FITNAME                                                       
         MVC   SORTNAME,BLOCK      SAVE IN SORT RECORD                          
         B     *+10                                                             
*                                                                               
EXCRP10  MVC   SORTNAME,SAVNAME    ELSE SAVE WHOLE NAME IN SORT RECORD          
         OC    SORTNAME,SPACES     GET RID OF ZEROS                             
         DROP  R4                                                               
*                                                                               
         USING TLCKD,R4                                                         
         L     R4,TIAREC           READ INDIVIDUAL SSN                          
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'A0',TLCKSSN)                              
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         BAS   RE,EXTRNAM          EXTRACT NAME FOR 2ND LINE                    
         MVC   SORTNAM2(3),=C'FSO'                                              
         MVC   SORTNAM2+4(31),SAVNAME   SAVE IN SORT RECORD                     
         OC    SORTNAM2,SPACES     GET RID OF ZEROS                             
*                                                                               
         BAS   RE,EXTRAGNT         EXTRACT AGENT NAME                           
         BNE   EXCRPX                                                           
         MVC   SORTNAM3(3),=C'C/O'                                              
         MVC   SORTNAM3+4(31),SAVNAME  SAVE IN SORT RECORD                      
         OC    SORTNAM3,SPACES     GET RID OF ZEROS                             
*                                                                               
EXCRPX   B     XIT                                                              
*---------------------------------------------------------------------          
* EXTRNAM - EXTRACTS NAME FROM W4 RECORD                                        
*---------------------------------------------------------------------          
EXTRNAM  NTR1                                                                   
         L     R4,AIO              R4 = A(W4 DETAILS ELEMENT)                   
         MVI   ELCODE,TAW4ELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         USING TAW4D,R4                                                         
*                                  SAVE NAME FOR SORT KEY                       
         MVC   LASTNAME(32),TAW4NAM2                                            
         MVC   MIDNAME,SPACES                                                   
         CLI   TAW4LEN,TAW4LN2Q    DO WE HAVE MIDDLE INITIAL?                   
         BNE   *+10                                                             
         MVC   MIDNAME,TAW4MIDN                                                 
*                                                                               
         CLI   TAW4TYPE,TAW4TYCO   IF PERFORMER IS NOT CORPORATION              
         BE    EXNAM20                                                          
         CLI   TAW4TYPE,TAW4TYTR   OR TRUSTEE                                   
         BE    EXNAM20                                                          
         CLI   TAW4TYPE,TAW4TYES   OR ESTATE                                    
         BE    EXNAM20                                                          
         BAS   RE,FITNAME                                                       
*                                                                               
         XC    SAVNAME,SAVNAME                                                  
         MVC   SAVNAME,BLOCK       SAVE NAME                                    
*                                                                               
EXNAM20  OC    SAVNAME,SPACES      GET RID OF ZEROS                             
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*---------------------------------------------------------------------          
* EXTRAGNT - EXTRACT AGENT NAME FROM AGENT RECORD                               
*---------------------------------------------------------------------          
EXTRAGNT NTR1                                                                   
         L     R4,TIAREC                                                        
         MVI   ELCODE,TACAELQ      GET CAST DETAILS ELEMENT                     
         BAS   RE,GETEL                                                         
         BNE   NO                                                               
         USING TACAD,R4                                                         
         OC    TACANCDE,TACANCDE   IS THERE AN AGENT CODE?                      
         BZ    NO                                                               
         GOTO1 TRNSAGT,DMCB,(X'40',TACANCDE),FULL                               
         GOTO1 RECVAL,DMCB,TLANCDQ,(X'A0',FULL)                                 
         BNE   NO                                                               
         DROP  R4                                                               
*                                                                               
         XC    SAVNAME,SAVNAME                                                  
         L     R4,AIO                                                           
         MVI   ELCODE,TANAELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   NO                                                               
         USING TANAD,R4                                                         
         ZIC   RE,TANALEN                                                       
         AHI   RE,-3                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   SAVNAME(0),TANANAME                                              
         B     YES                                                              
*                                                                               
         EJECT                                                                  
*---------------------------------------------------------------------          
* EXTRTRST - EXTRACT TRUSTEE INFO FROM W4 RECORD                                
*---------------------------------------------------------------------          
EXTRTRST NTR1                                                                   
         L     R4,AIO                                                           
         MVI   ELCODE,TAW4ELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         USING TAW4D,R4                                                         
*                                  SAVE NAME FOR SORT KEY                       
         CLI   TAW4TYPE,TAW4TYTR   IF PERFORMER IS TRUSTEE                      
         BNE   NO                                                               
*                                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,TAA2ELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   EXTR10                                                           
         USING TAA2D,R4                                                         
         MVC   SORTNAM3(L'TAA2ADD1),TAA2ADD1   3RD NAME LINE = ADD1             
         OC    SORTNAM3,SPACES                                                  
         DROP  R4                                                               
*                                                                               
EXTR10   LA    R4,TIAREC                                                        
         MVC   AIO,TIAREC                                                       
         GOTO1 CHAROUT,DMCB,TANUELQ,0,TANUTRST    GET TRUSTEE SSN               
         MVC   AIO,AIO2                                                         
         BNE   NO                                                               
*                                                                               
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'A0',TGNAME)    READ TRUSTEE W4            
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         L     R4,AIO              R4 = A(W4 DETAILS ELEMENT)                   
         MVI   ELCODE,TAW4ELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         USING TAW4D,R4                                                         
*                                  SAVE NAME FOR SORT KEY                       
         MVC   LASTNAME(32),TAW4NAM2                                            
         MVC   MIDNAME,SPACES                                                   
         CLI   TAW4LEN,TAW4LN2Q    DO WE HAVE MIDDLE INITIAL?                   
         BNE   *+10                                                             
         MVC   MIDNAME,TAW4MIDN                                                 
         BAS   RE,FITNAME2                                                      
*                                                                               
         XC    SAVNAME,SAVNAME                                                  
         MVC   SAVNAME,BLOCK       SAVE NAME                                    
*                                                                               
         OC    SAVNAME,SPACES      GET RID OF ZEROS                             
         B     YES                                                              
*                                                                               
*---------------------------------------------------------------------          
* FITNAME - FITS FULL NAME INTO SORTNAME,                                       
*           IF MIDDLE NAME DOESN'T FIT, USE MIDDLE INITIAL                      
*---------------------------------------------------------------------          
FITNAME  NTR1                                                                   
*                                                                               
         MVC   BLOCK(50),SPACES                                                 
         MVC   BLOCK(16),FRSTNAME  THEN SQUASH FIRST NAME WITH LAST             
         MVC   BLOCK+17(16),MIDNAME                                             
         MVC   BLOCK+34(16),LASTNAME   NAME                                     
         GOTO1 SQUASHER,DMCB,BLOCK,50                                           
         CLC   MIDNAME,SPACES                                                   
         BE    FITNAMEX                                                         
         CLI   DMCB+7,L'SORTNAME   DID FULL NAME FIT?                           
         BNH   FITNAMEX                                                         
*                                                                               
         MVC   BLOCK(50),SPACES                                                 
         MVC   BLOCK(16),FRSTNAME  NO, USE MIDDLE INITIAL                       
         MVC   BLOCK+17(1),MIDNAME                                              
         MVC   BLOCK+19(16),LASTNAME                                            
         GOTO1 SQUASHER,DMCB,BLOCK,50                                           
         B     FITNAMEX                                                         
*                                                                               
FITNAMEX B     XIT                                                              
*                                                                               
*                                                                               
*---------------------------------------------------------------------          
* FITNAME2 - FITS FULL NAME INTO SORTNAME, LAST NAME FIRST                      
*---------------------------------------------------------------------          
FITNAME2 NTR1                                                                   
*                                                                               
         MVC   BLOCK(50),SPACES                                                 
         MVC   BLOCK(16),LASTNAME  THEN SQUASH FIRST NAME WITH LAST             
         MVC   BLOCK+17(16),FRSTNAME                                            
         MVC   BLOCK+34(16),MIDNAME   NAME                                      
         GOTO1 SQUASHER,DMCB,BLOCK,50                                           
*                                                                               
         B     XIT                                                              
*                                                                               
*              GET RECORDS FROM SORT / WRITE DETAIL RECORDS                     
         SPACE 1                                                                
         USING RECD,R3             R3=A(TAPE RECORD AREA)                       
GETSORT  NTR1                                                                   
         MVI   TBBNK,X'FF'         SET FIRST TIME FOR ACCOUNT                   
         SPACE 1                                                                
         LA    R4,TBCHUNK          R4=A(PRINT AREA)                             
         USING LINED,R4                                                         
         LA    R2,TBSRTREC         R2=A(SORT RECORD)                            
         USING SORTD,R2                                                         
GETS2    GOTO1 SORTER,DMCB,=C'GET',(R2)                                         
         ICM   RF,15,4(R1)                                                      
         BZ    GETSX                                                            
         MVC   TBSRTREC,0(RF)      MOVE SORT RECORD TO LOCAL AREA               
         SPACE 1                                                                
         CLC   TBBNK,SORTBNK       IF ACCOUNT CHANGED                           
         BE    GETS4                                                            
         CLI   TBBNK,X'FF'         UNLESS FIRST TIME IN                         
         BE    *+8                                                              
         BAS   RE,TOTAL            WRITE TOTAL RECORD                           
         SPACE 1                                                                
         MVC   TBEMPNM,PPNAME      SET EMPLOYER NAME                            
         CLC   SORTBNK,PPBANK                                                   
         BE    GETS3                                                            
         MVC   TBEMPNM,PPLSNAME      SET EMPLOYER NAME                          
         CLC   SORTBNK,PPLSBANK                                                 
         BE    GETS3                                                            
         MVC   TBEMPNM,TPNAME                                                   
         SPACE 1                                                                
GETS3    BAS   RE,HEADER           WRITE HEADER RECORD                          
         SPACE 1                                                                
         MVC   TBBNK,SORTBNK       SET NEW 55NK ACCOUNT                         
         ZAP   TBCNT,=P'0'         CLEAR ACCOUNT TOTALS                         
         ZAP   TBTOT,=P'0'                                                      
         ZAP   TBCRTOT,=P'0'                                                    
         ZAP   TBCRCNT,=P'0'                                                    
         ZAP   TBDBTOT,=P'0'                                                    
         ZAP   TBDBCNT,=P'0'                                                    
         MVI   FORCEHED,C'Y'       START NEW PAGE                               
         SPACE 1                                                                
GETS4    DS    0H                                                               
*ETS4    CLI   RECNUM,CB           CANADIAN BANK REC?                           
*        BE    GETS5                                                            
         BAS   RE,DETAIL           WRITE DETAIL RECORD TO TAPE                  
         B     GETS2               GET NEXT RECORD FROM SORT                    
*                                                                               
GETS5    BAS   RE,CNDETAIL         WRITE CANADIAN DETAIL TO TAPE                
         B     GETS2                                                            
*                                                                               
GETSX    BAS   RE,TOTAL            DONE - WRITE LAST TOTAL RECORD               
         SPACE 1                                                                
         GOTO1 SORTER,DMCB,=C'END'                                              
         XI    TBSTAT,TBSORTNG     SORT NO LONGER ACTIVE                        
         B     XIT                                                              
         EJECT                                                                  
*              WRITE TAPE HEADER RECORD                                         
         SPACE 1                                                                
         USING RECD,R3             R3=A(TAPE RECORD AREA)                       
HEADER   NTR1                                                                   
         GOTO1 DATCON,DMCB,(0,TGTODAY0),(X'20',WORK)                            
         LA    R2,CBTAPE                                                        
         CLI   RECNUM,CB           CANADIAN BANK REC?                           
         BE    HEAD010                                                          
         LA    R2,BXTAPE                                                        
         CLI   RECNUM,BX           BANKREC P+ANK REC?                           
         BE    HEAD010                                                          
*                                                                               
         LA    R2,BKTAPE                                                        
HEAD010  MVC   RECD(132),SPACES                                                 
         MVC   RECD+132(38),SPACES                                              
         MVI   RECTYPE,RECTYH        HEADER TYPE                                
         MVC   RECHCPYN,TBEMPNM      COMPANY NAME                               
         GOTO1 DATCON,DMCB,(0,TGTODAY0),(52,RECHDATE)                           
         B     HEADX                                                            
*&&DO                                                                           
         USING CNRECD,R3                                                        
HEADCN   MVC   CNREC,SPACES                                                     
         MVC   CNRECHDR,=C'ARSH'                                                
         MVC   CNRHFILT,=C'ISS'    ISSUE FILE                                   
         MVC   CNRHCUSN,=C'TAL PART'                                            
         MVC   CNRHDCHA,=C'0000001229526'                                       
         MVC   CNRHCRED(4),WORK+2  CREATION DATE - MMDDYY                       
         MVC   CNRHCRED+4(2),WORK                                               
         MVC   CNRHTEST,=C'TEST'   ** REMOVE WHEN YOU MAKE IT LIVE              
*&&                                                                             
HEADX    PUT   (R2),(R3)           WRITE THE RECORD                             
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
*              WRITE TAPE DETAIL RECORD FROM SORTER RECORD                      
*                                                                               
         USING SORTD,R2            R2=A(SORT RECORD)                            
         USING RECD,R3             R3=A(TAPE RECORD AREA)                       
         USING LINED,R4            R4=A(PRINT AREA)                             
DETAIL   NTR1                                                                   
         MVC   LINCHK,SORTCHK                 PRINT CHECK NUMBER                
         EDIT  (4,SORTAMT),(12,LINAMT),2,MINUS=YES  CHECK AMOUNT                
         GOTO1 DATCON,DMCB,(1,SORTDTE),(8,LINDTE)   CHECK DATE                  
         BAS   RE,CHUNK            SET TO PRINT CHUNK                           
*                                                                               
**NO-OP  MVC   RECD(RECLNQ),SPACES                                              
         MVC   RECD(132),SPACES                                                 
         MVC   RECD+132(38),SPACES                                              
         MVI   RECTYPE,RECTYD      DETAIL TYPE                                  
         MVC   RECDBNKN,=C'965'                                                 
         CLI   RECNUM,CB           CANADIAN BANK REC?                           
         BNE   *+10                                                             
         MVC   RECDBNKN,=C'466'                                                 
         MVC   RECDACCN,SORTBNK    BANK ACCOUNT                                 
         TM    SORTSTAT,TACDSVOI   HAS CHECK BEEN VOIDED?                       
         BO    DET05                                                            
         TM    SORTAMT,X'80'       CHECK AMOUNT NEGATIVE?                       
         BZ    *+8                                                              
DET05    MVI   RECDVOID,C'V'       YES, MARK IT VOID                            
         MVC   RECDCHKN(2),=C'00'                                               
         MVC   RECDCHKN+2(8),SORTCHK             CHECK NUMBER                   
         GOTO1 DATCON,DMCB,(1,SORTDTE),(52,RECDIDTE)                            
         EDIT  (4,SORTAMT),(10,RECDAMNT),FILL=0   CHECK AMOUNT                  
         MVC   RECPAYEE,SORTNAME   PAYEE NAME                                   
         MVC   RECPAYE2,SORTNAM2                                                
         MVC   RECPAYE3,SORTNAM3                                                
         LA    R2,CBTAPE                                                        
         CLI   RECNUM,CB           CANADIAN BANK REC?                           
         BE    DET08                                                            
         LA    R2,BXTAPE                                                        
         CLI   RECNUM,BX           P+ BANK REC?                                 
         BE    DET08                                                            
         LA    R2,BKTAPE                                                        
DET08    PUT   (R2),(R3)           WRITE THE RECORD                             
*                                                                               
         TM    SORTAMT,X'80'       IF AMOUNT IS NEGATIVE                        
         BZ    DETX                                                             
         AP    TBCRTOT,DUB         ADD TO CREDIT TOTAL                          
         AP    TBCRCNT,=P'1'       AND TO CREDIT COUNT                          
         B     XIT                                                              
*                                                                               
DETX     AP    TBDBTOT,DUB         ELSE, ADD TO DEBIT TOTAL                     
         AP    TBDBCNT,=P'1'             AND TO DEBIT COUNT                     
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------          
*              WRITE TAPE CANADIAN DETAIL RECORD FROM SORTER RECORD             
*---------------------------------------------------------------------          
         USING SORTD,R2            R2=A(SORT RECORD)                            
         USING CNRECD,R3           R3=A(TAPE RECORD AREA)                       
         USING LINED,R4            R4=A(PRINT AREA)                             
CNDETAIL NTR1                                                                   
         MVC   CNREC,SPACES                                                     
         MVC   LINCHK,SORTCHK                 PRINT CHECK NUMBER                
         EDIT  (4,SORTAMT),(12,LINAMT),2,MINUS=YES  CHECK AMOUNT                
         GOTO1 DATCON,DMCB,(1,SORTDTE),(8,LINDTE)   CHECK DATE                  
         BAS   RE,CHUNK            SET TO PRINT CHUNK                           
         SPACE 1                                                                
         MVC   CNRDACCN,SORTBNK                BANK ACCOUNT                     
         MVC   CNRDCHKN(2),=C'00'                                               
         MVC   CNRDCHKN+2(8),SORTCHK           CHECK NUMBER                     
         GOTO1 DATCON,DMCB,(1,SORTDTE),(X'20',WORK)                             
         MVC   CNRDISSD(4),WORK+2              CHECK DATE                       
         MVC   CNRDISSD+4(2),WORK                                               
         EDIT  (4,SORTAMT),(10,CNRDCHKA),FILL=0  CHECK AMOUNT                   
         PUT   CBTAPE,(R3)         WRITE THE RECORD                             
*                                                                               
CNDETX   AP    TBDBTOT,DUB         ELSE, ADD TO DEBIT TOTAL                     
         AP    TBDBCNT,=P'1'             AND TO DEBIT COUNT                     
         B     XIT                                                              
         EJECT                                                                  
*              WRITE TAPE TRAILER RECORD                                        
         SPACE 1                                                                
         USING RECD,R3             R3=A(TAPE RECORD AREA)                       
         USING LINED,R4            R4=A(PRINT AREA)                             
TOTAL    NTR1                                                                   
         BAS   RE,SKIPLINE         SKIP A LINE                                  
         ZAP   TBCNT,TBCRCNT       TOTAL COUNT                                  
         AP    TBCNT,TBDBCNT                                                    
         ZAP   TBTOT,TBCRTOT       TOTAL (HASHED/ABSOLUTE) DOLLARS              
         OI    TBTOT+7,X'0F'                                                    
         AP    TBTOT,TBDBTOT                                                    
*                                                                               
         BAS   RE,TOTHASH          PRINT TOTAL 'HASHED'                         
*        CLI   RECNUM,CB           CANADIAN BANK REC?                           
*        BE    TOT100                                                           
         BAS   RE,SKIPLINE         SKIP A LINE                                  
         BAS   RE,TOTCOMB          PRINT TOTAL 'COMBINED'                       
         BAS   RE,SKIPLINE         SKIP A LINE                                  
         BAS   RE,TOTCRED          PRINT TOTAL 'CREDITS'                        
         BAS   RE,SKIPLINE         SKIP A LINE                                  
         BAS   RE,TOTDEBT          PRINT TOTAL 'DEBITS'                         
*                                                                               
**NO-OP  MVC   RECD(RECLNQ),SPACES                                              
         MVC   RECD(132),SPACES                                                 
         MVC   RECD+132(38),SPACES                                              
         MVI   RECTYPE,RECTYT      TRAILER TYPE                                 
         MVC   RECTBNKN,=C'965'    BANK NUMBER                                  
         MVC   RECTACCN,TBBNK      BANK ACCOUNT                                 
         OI    TBCNT+L'TBCNT-1,X'0F'                                            
         UNPK  RECTRCNT,TBCNT      ITEM COUNT                                   
         OI    TBTOT+L'TBTOT-1,X'0F'                                            
         UNPK  RECTTAMT,TBTOT      TOTAL AMOUNT                                 
         B     TOT900                                                           
*&&DO                                                                           
         USING CNRECD,R3                                                        
TOT100   MVC   CNREC,SPACES                                                     
         MVC   CNRECTRL,=C'ARST'                                                
         OI    TBCNT+L'TBCNT-1,X'0F'                                            
         UNPK  CNRTNITM,TBCNT        ITEM COUNT                                 
         OI    TBTOT+L'TBTOT-1,X'0F'                                            
         UNPK  CNRTDLRA,TBTOT        TOTAL AMOUNT                               
*&&                                                                             
TOT900   LA    R2,BKTAPE                                                        
         CLI   RECNUM,CB           CANADIAN BANK REC?                           
         BNE   *+8                                                              
         LA    R2,CBTAPE                                                        
         CLI   RECNUM,BX           P+ BANK REC?                                 
         BNE   *+8                                                              
         LA    R2,BXTAPE                                                        
         PUT   (R2),(R3)           WRITE THE RECORD                             
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PRINT TOTAL HASHED TOTALS (ABSOLUTE VALUE)            
         SPACE 1                                                                
         USING RECD,R3             R3=A(TAPE RECORD AREA)                       
TOTHASH  NTR1                                                                   
         MVC   LINCHK(8),=CL8'TOTALS'                                           
         EDIT  TBTOT,(12,LINAMT),2                                              
         EDIT  TBCNT,(8,LINDTE),COMMAS=YES                                      
         BAS   RE,CHUNK            MOVE TO PRINT LINE                           
         BAS   RE,PRNTIT           PRINT IT NOW                                 
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO PRINT TOTAL COMBINED DOLLARS                          
         SPACE 1                                                                
TOTCOMB  NTR1                                                                   
         MVC   LINCHK(8),=CL8'COMB TOT'                                         
         ZAP   DUB,TBCRTOT                                                      
         AP    DUB,TBDBTOT                                                      
         EDIT  (P8,DUB),(12,LINAMT),2,MINUS=YES                                 
         EDIT  TBCNT,(8,LINDTE),COMMAS=YES                                      
         BAS   RE,CHUNK            MOVE TO PRINT LINE                           
         BAS   RE,PRNTIT           PRINT IT NOW                                 
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO PRINT TOTAL CREDIT DOLLARS                            
         SPACE 1                                                                
TOTCRED  NTR1                                                                   
         MVC   LINCHK(8),=CL8'CREDITS'                                          
         EDIT  TBCRTOT,(12,LINAMT),2,MINUS=YES                                  
         EDIT  TBCRCNT,(8,LINDTE),COMMAS=YES                                    
         BAS   RE,CHUNK            MOVE TO PRINT LINE                           
         BAS   RE,PRNTIT           PRINT IT NOW                                 
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO PRINT TOTAL DEBIT DOLLARS                             
         SPACE 1                                                                
TOTDEBT  NTR1                                                                   
         MVC   LINCHK(8),=CL8'DEBITS'                                           
         EDIT  TBDBTOT,(12,LINAMT),2                                            
         EDIT  TBDBCNT,(8,LINDTE),COMMAS=YES                                    
         BAS   RE,CHUNK            MOVE TO PRINT LINE                           
         BAS   RE,PRNTIT           PRINT IT NOW                                 
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE PLACES CHUNK IN PRINT LINE                               
         SPACE 1                                                                
CHUNK    NTR1                                                                   
         CLI   TBNXTCHK,NCHUNKS    IF WE'RE ABOUT TO EXCEED MAX.                
         BL    *+8                                                              
         BAS   RE,PRNTIT           PRINT PREVIOUS LINE                          
         SPACE 1                                                                
         ZIC   R1,TBNXTCHK                                                      
         LA    RF,1(R1)            BUMP NEXT CHUNK IND.                         
         STC   RF,TBNXTCHK                                                      
         SPACE 1                                                                
         MH    R1,=AL2(LINLNQ)     R1=DISP. INTO P FOR THIS CHUNK               
         LA    R1,P(R1)                                                         
         MVC   0(LINLNQ,R1),TBCHUNK  MOVE TO PRINT LINE                         
         MVC   TBCHUNK,SPACES        AND CLEAR                                  
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO SKIP A LINE                                           
         SPACE 1                                                                
SKIPLINE NTR1                                                                   
         MVI   SPACING,2                                                        
         BAS   RE,PRNTIT                                                        
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO PRINT A LINE                                          
         SPACE 1                                                                
PRNTIT   NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   TBNXTCHK,0          SET TO START AT BEGINNING AGAIN              
         MVI   SPACING,1                                                        
         B     XIT                                                              
         EJECT                                                                  
*              HEADLINE HOOK (HEADHOOK)                                         
         SPACE 1                                                                
HOOK     NTR1                                                                   
         MVC   H4+10(L'TBEMPNM),TBEMPNM    COMPANY                              
         MVC   H5+10(L'TBBNK),TBBNK        BANK ACCOUNT                         
         MVC   H4+99(17),TBPERIOD          REQUESTED PERIOD                     
         SPACE 1                                                                
         L     R4,ABOX             SET UP BOXES                                 
         USING BOXD,R4                                                          
         MVC   BOXROWS,SPACES      SET ROWS                                     
         MVI   BOXROWS+6,C'T'      TOP                                          
         MVI   BOXROWS+8,C'M'      MIDDLE                                       
         MVI   BOXROWS+60,C'B'     BOTTOM                                       
         SPACE 1                                                                
         MVC   BOXCOLS,SPACES      SET COLUMNS                                  
         LA    R2,BOXCOLS                                                       
         USING LINED,R2            USE PRINT LINE DSECT                         
         LA    R0,NCHUNKS                                                       
HK4      MVI   BL,C'L'                                                          
         MVI   BC1,C'C'                                                         
         MVI   BC2,C'C'                                                         
         MVI   BR,C'R'                                                          
         LA    R2,LINNEXT                                                       
         BCT   R0,HK4                                                           
         SPACE 1                                                                
         MVI   BOXYORN,C'Y'        INITIALIZE REMAINING FLDS                    
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         B     XIT                                                              
         EJECT                                                                  
*        ROUTINE CLEARS WORKING STORAGE                                         
CLRWS    NTR1                                                                   
         LA    R2,TBD                                                           
         LR    R3,R2                                                            
         AHI   R3,TBLNQ                                                         
*                                                                               
         SR    R3,R2               SIZE OF AREA TO CLEAR                        
         XR    RE,RE                                                            
         LR    RF,RE                                                            
         MVCL  R2,RE                                                            
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE GENERATES BANK CONFIRMATION REPORT                       
         SPACE 1                                                                
PREPC    NTR1                                                                   
         MVI   ERRCHK,C'N'                                                      
         MVC   SPECS,=A(MYSPECS2)                                               
         MVC   P(16),=C'NO DATA RECEIVED'                                       
         SPACE 1                                                                
PREPC10  BAS   RE,GETTAPE          GET FIRST(/NEXT) RECORD                      
         SPACE 1                                                                
         LA    R3,TBRECORD                                                      
         CLI   RECTYPE,X'FF'       IF END OF TAPE                               
         BNE   *+12                                                             
         BAS   RE,SPLAT                                                         
         B     XIT                 EXIT                                         
*                                                                               
         MVC   P(16),SPACES                                                     
         CLI   RECTYPE,RECTYH     IF HEADER RECORD ...                          
         BNE   PREPC20                                                          
         BAS   RE,PRTHEAD          PRINT HEADER                                 
         MVI   ERRCHK,C'N'                                                      
         B     PREPC10             AND GET NEXT                                 
*                                                                               
PREPC20  CLI   RECTYPE,RECTYT      IF TRAILER RECORD ...                        
         BNE   PREPC10                                                          
         CLI   ERRCHK,C'Y'                                                      
         BE    PREPC25                                                          
         BAS   RE,SPLAT                                                         
         MVC   P(9),=C'NO ERRORS'                                               
         BAS   RE,SPLAT                                                         
PREPC25  BAS   RE,PRTTOT           PRINT TRAILER                                
         B     PREPC10             AND GET NEXT                                 
         EJECT                                                                  
*&&DO          ROUTINE TO PRINT ERRORS ON BANK CONFIRMATION REPORT              
         SPACE 1                                                                
PRTERR   NTR1                                                                   
         LA    R2,P                                                             
         BAS   RE,SPLAT                                                         
         MVC   P(L'RECERRM),RECERRM                                             
         BAS   RE,SPLAT                                                         
         B     XIT                                                              
*&&                                                                             
*              ROUTINE TO PRINT TOTALS ON BANK CONFIRMATION REPORT              
         SPACE 1                                                                
PRTTOT   NTR1                                                                   
         LA    R2,P                                                             
         BAS   RE,SPLAT                                                         
         MVC   P(14),=C'TOTAL RECORDS:'                                         
         EDIT  (C10,RECTRCNT),(10,P+25),ALIGN=LEFT,ZERO=NOBLANK                 
         BAS   RE,SPLAT                                                         
         MVC   P(13),=C'TOTAL AMOUNT:'                                          
         EDIT  (C16,RECTTAMT),(16,P+25),2,ALIGN=LEFT,ZERO=NOBLANK,     +        
               MINUS=YES                                                        
         LA    R1,4                                                             
PRTOT10  BAS   RE,SPLAT                                                         
         BCT   R1,PRTOT10                                                       
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO PRINT HEADINGS ON BANK CONFIRMATION REPORT            
*                                                                               
PRTHEAD  NTR1                                                                   
         LA    R2,P                                                             
         BAS   RE,SPLAT                                                         
         MVC   P(14),=C'CREATION DATE:'                                         
         MVC   P+25(L'RECHDATE),RECHDATE                                        
         BAS   RE,SPLAT                                                         
*        MVC   P(20),=C'ACKNOWLEDGMENT DATE:'                                   
*        MVC   P+25(L'RECAKDT),RECAKDT                                          
*        BAS   RE,SPLAT                                                         
         B     XIT                                                              
*                                                                               
SPLAT    NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         EJECT                                                                  
*              TAPE ROUTINES                                                    
         SPACE 1                                                                
GETTAPE  NTR1                                                                   
         LA    R0,TBRECORD                                                      
         L     R1,=A(BCTAPE)                                                    
         GET   (1),(0)                                                          
         B     XIT                                                              
         SPACE 2                                                                
TAPEEOF  MVI   TBRECORD+RECTYPE-RECD,X'FF'  SET END OF TAPE                     
         B     XIT                                                              
         EJECT                                                                  
*              ERRORS, EXITS                                                    
         SPACE 2                                                                
FLDINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     THEEND                                                           
         SPACE 1                                                                
STRINV   MVI   ERROR,ERINVSDT      INVALID START DATE                           
         B     THEEND                                                           
         SPACE 1                                                                
ENDINV   MVI   ERROR,ERINVEDT      INVALID END DATE                             
         B     THEEND                                                           
         SPACE 1                                                                
THEEND   GOTO1 ERREX                                                            
         SPACE 1                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 3                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 1                                                                
LASALLE  DC    CL80'SUBJECT=CLA(TA0BKDS1),CHA(3),ACC(LNSC),USE(AASCMRP)X        
               ,MOD(0),'                                                        
SORTCARD DC    CL80'SORT FIELDS=(1,18,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=131'                                   
CPPBANK  DC    CL10'725672034 '    CHASE                                        
LPPBANK  DC    CL10'5590054309'    LASALLE                                      
PPNAME   DC    CL33'PRINT PAYROLL SERVICES'                                     
CUSABANK DC    CL10'725672042 '    CHASE                                        
LUSABANK DC    CL10'5590054283'    LASALLE                                      
TPNAME   DC    CL33'TALENT PARTNERS'                                            
CANBANK  DC    CL10'9872101   '                                                 
PPLSBANK DC    CL10'469162833 '    CHASE                                        
PPLSNAME DC    CL33'TALENT PARTNERS PAYROLL SERVICES'                           
         SPACE 1                                                                
*              DCB                                                              
         SPACE                                                                  
BKTAPE   DCB   DDNAME=BKTAPE,DSORG=PS,MACRF=PM,RECFM=FB,               X        
               LRECL=170,BLKSIZE=4250                                           
BXTAPE   DCB   DDNAME=BXTAPE,DSORG=PS,MACRF=PM,RECFM=FB,               X        
               LRECL=170,BLKSIZE=4250                                           
CBTAPE   DCB   DDNAME=CBTAPE,DSORG=PS,MACRF=PM,RECFM=FB,               X        
               LRECL=170,BLKSIZE=4250                                           
BCTAPE   DCB   DDNAME=BCTAPE,DSORG=PS,MACRF=GM,EODAD=TAPEEOF,          X        
               RECFM=FB,LRECL=170                                               
         SPACE 1                                                                
NCHUNKS  EQU   4                   4-UP LISTING                                 
         SPACE 1                                                                
PARMLST  CALL  ,(DSNME,RETAREA),MF=L                                            
LINKLST  LINKX EP=FRGETDSN,SF=L                                                 
*                                                                               
DSNME    DC    CL8'BKTAPE'                                                      
RETAREA  DS    0CL44                                                            
RTDSN    DC    CL17' '                                                          
RTGEN    DC    CL8' '        G0000000                                           
RTND     DC    CL19' '       SPARE                                              
ERRCHK   DS    CL1                                                              
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              REPORT SPECS                                                     
         SPACE 1                                                                
MYSPECS  DS    0H                                                               
         SSPEC H1,2,RUN                                                         
         SSPEC H1,100,REPORT                                                    
         SSPEC H1,117,PAGE                                                      
         SSPEC H2,100,REQUESTOR                                                 
*                                                                               
         SSPEC H1,54,C'Bank Reconciliation Report'                              
         SSPEC H2,54,26X'BF'                                                    
*                                                                               
         SSPEC H4,2,C'Company'                                                  
         SSPEC H5,2,C'Account'                                                  
*                                                                               
         SSPEC H8,002,C' Check      Amount      Date'                           
         SSPEC H8,035,C' Check      Amount      Date'                           
         SSPEC H8,068,C' Check      Amount      Date'                           
         SSPEC H8,101,C' Check      Amount      Date'                           
         DC    X'00'                                                            
         EJECT                                                                  
MYSPECS2 DS    0H                                                               
         SSPEC H1,2,RUN                                                         
         SSPEC H1,100,REPORT                                                    
         SSPEC H1,117,PAGE                                                      
         SSPEC H2,100,REQUESTOR                                                 
*                                                                               
         SSPEC H1,54,C'Bank Confirmation Report'                                
         SSPEC H2,54,24X'BF'                                                    
*                                                                               
         DC    X'00'                                                            
         EJECT                                                                  
*              ROUTINE TO SUBMIT JCL TO POWWOW                                  
*              FOR ADVANTIS TRANSMISSION                                        
         SPACE 1                                                                
CALLPOW  NTR1  BASE=*,LABEL=*                                                   
         CLI   RECNUM,BX           BANK REC P+ USES DIFF JCL                    
         JNE   XIT                                                              
*        JE    CALLP100                                                         
*&&DO                                                                           
         MVC   TAPEC4,RTGEN        UPDATE GENERATION NUMBER                     
         MVC   FILE3(5),RTGEN      "                                            
                                                                                
         LA    R0,NUMCRD           R0=N'OF JCL CARDS                            
         LA    R3,JCL              R3=A(JCL FOR POWWOW)                         
*                                                                               
*        CLI   RECNUM,CF           LASALLE HAS DIFF JCL                         
*        BNE   *+10                                                             
*        MVC   SUBJ1(80),LASALLE                                                
*                                                                               
         CLI   RECNUM,CB           CANADIAN HAS DIFF JCL                        
         JNE   CALLP5                                                           
         MVC   SUBJ2(5),=C'ISSPC'                                               
         MVC   FILE2(8),=C'TA0CBDS1'                                            
         MVC   TAPEC2(16),=C'TALDISK.TA0CBDS1'                                  
*                                                                               
CALLP5   TM    TBOPTS,TBTRACE      IF TRACING REQUESTED                         
         JZ    CALLP8                                                           
         MVC   P,0(R3)             PRINT JCL                                    
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
CALLP8   MVC   POWJCL,0(R3)                                                     
         TM    TBOPTS,TBNOPOW      UNLESS OTHERWISE REQUESTED                   
         JO    CALLP10                                                          
         GOTO1 =V(POWWOW),DMCB,=C'PUT',=C'POWER',POWKEY,POWHEAD                 
*                                                                               
CALLP10  LA    R3,L'JCL(R3)                                                     
         BCT   R0,CALLP5                                                        
         J     XIT                                                              
*&&                                                                             
*=====================================================================          
CALLP100 DS    0H                                                               
         MVC   PUTF4(5),RTGEN      UPDATE GENERATION NUMBER                     
         MVC   PUTF6(5),RTGEN      UPDATE GENERATION NUMBER                     
                                                                                
         LA    R0,NUMCRD2          R0=N'OF JCL CARDS                            
         LA    R3,JCL2             R3=A(JCL FOR POWWOW)                         
*                                                                               
CALLP200 TM    TBOPTS,TBTRACE      IF TRACING REQUESTED                         
         JZ    CALLP300                                                         
         MVC   P,0(R3)             PRINT JCL                                    
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
CALLP300 MVC   POWJCL,0(R3)                                                     
         TM    TBOPTS,TBNOPOW      UNLESS OTHERWISE REQUESTED                   
         JO    CALLP400                                                         
         GOTO1 =V(POWWOW),DMCB,=C'PUT',=C'POWER',POWKEY,POWHEAD                 
*                                                                               
CALLP400 LA    R3,L'JCL(R3)                                                     
         BCT   R0,CALLP200                                                      
         J     XIT                                                              
         EJECT                                                                  
*======================================================================         
JCL      DS    0CL80                                                            
JOBC     DC    CL80'//TPCHTBKP  JOB   ''CNTL'',MSGLEVEL=(1,1),COND=((0,X        
               NE)),'                                                           
         DC    CL80'//     MSGCLASS=X,CLASS=A,NOTIFY=CCOL'                      
         DC    CL80'//*MAIN CLASS=ADVANTIS,SYSTEM=SY1'                          
         DC    CL80'//SS  EXEC  BDEDICT'                                        
         DC    CL80'//EXTSYSIN DD *'                                            
*                                                                               
EDICT1   DC    C'EDICTKEY='                                                     
EDICT2   DC    C'DODS030'                                                       
EDICT3   DC    CL(80-(*-EDICT1))' '                                             
*                                                                               
SUBJ1    DC    C'SUBJECT=CLA('                                                  
SUBJ2    DC    C'ISSPY'                                                         
         DC    C'),CHA(1),ACC('                                                 
SUBJ3    DC    C'BOC1'                                                          
         DC    C'),USE('                                                        
SUBJ4    DC    C'BOC1219'                                                       
         DC    C'),MOD(0),'                                                     
         DC    CL(80-(*-SUBJ1))' '                                              
*                                                                               
FILE1    DC    C'FILE='                                                         
FILE2    DC    C'TA0BKDS1'                                                      
         DC    C'.'                                                             
FILE3    DC    C'G0000'                                                         
FILE4    DC    CL(80-(*-FILE1))' '                                              
*                                                                               
         DC    CL80'EXT=ROE'                                                    
*                                                                               
TAPEC1   DC    C'DSN='                                                          
TAPEC2   DC    CL16'TALDISK.TA0BKDS1'                                           
TAPEC3   DC    CL1'.'                                                           
TAPEC4   DC    CL8'G0000000'                                                    
TAPEC5   DC    CL(80-(*-TAPEC1))' '                                             
*                                                                               
NUMCRD   EQU   (*-JCL)/80                                                       
*======================================================================         
JCL2     DS    0CL80                                                            
JOBC2    DC    CL80'//TPCHTBXP  JOB   ''CNTL'',MSGLEVEL=(1,1),COND=((0,X        
               NE)),'                                                           
         DC    CL80'//     MSGCLASS=X,CLASS=A,NOTIFY=CCOL'                      
         DC    CL80'//*MAIN CLASS=A'                                            
         DC    CL80'//STEP0010 EXEC PGM=FTP,REGION=8M'                          
         DC    CL80'//SYSPRINT DD SYSOUT=*'                                     
         DC    CL80'//SYSTCPD DD DSN=DDS.TCPIP.DATA(TCPD1),DISP=SHR'            
         DC    CL80'//INPUT DD *'                                               
         DC    CL80'10.253.32.36'                                               
         DC    CL80'gpmorgan'                                                   
         DC    CL80'gP526L#'                                                    
         DC    CL80'ascii'                                                      
         DC    CL80'cd /outbound'                                               
*                                                                               
PUTF1    DC    C'PUT '''                                                        
PUTF2    DC    C'TALDISK.TA0BXDS1'                                              
PUTF3    DC    C'.'                                                             
PUTF4    DC    C'G0000V00'                                                      
PUTF5    DC    C''' TalentBX_'                                                  
PUTF6    DC    C'G0000'                                                         
         DC    C'.txt'                                                          
PUTF7    DC    CL(80-(*-PUTF1))' '                                              
*                                                                               
         DC    CL80'QUIT'                                                       
         DC    CL80'/*'                                                         
         DC    CL80'//'                                                         
*                                                                               
NUMCRD2  EQU   (*-JCL2)/80                                                      
*                                                                               
POWKEY   DC    CL10' '                                                          
POWHEAD  DC    XL8'00'                                                          
POWJCL   DS    CL80                                                             
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*              DSECT TO COVER LOCAL WORKING STORAGE                             
         SPACE 1                                                                
TBD      DSECT                                                                  
TBPERIOD DS    CL17                DISPLAYABLE REQUEST PERIOD                   
*                                                                               
TBOPTS   DS    XL1                 OPTIONS                                      
TBTRACE  EQU   X'80'               TRACE ACTIVE                                 
TBNOPOW  EQU   X'40'               DON'T CALL POWWOW                            
*                                                                               
TBSTAT   DS    XL1                 STATUS                                       
TBSORTNG EQU   X'80'               SORT IS ACTIVE                               
*                                                                               
TBSRTREC DS    CL(SORTLNQ)         SORT RECORD AREA                             
TBRECORD DS    CL(CNRECLNQ)        TAPE RECORD AREA                             
TBEMPNM  DS    CL33                EMPLOYER NAME                                
TBBNK    DS    CL10                SAVED BANK ACCOUNT                           
TBCNT    DS    PL4                 ITEM COUNT                                   
TBCRCNT  DS    PL4                 CREDIT ITEM COUNT                            
TBDBCNT  DS    PL4                 DEBIT ITEM COUNT                             
TBTOT    DS    PL8                 TOTAL AMOUNT                                 
TBCRTOT  DS    PL8                 CREDIT TOTAL AMOUNT                          
TBDBTOT  DS    PL8                 DEBIT TOTAL AMOUNT                           
*                                                                               
TBNXTCHK DS    XL1                 INDICATOR FOR CHUNK PRINTING                 
TBCHUNK  DS    CL(LINLNQ)          TEMP. AREA FOR PRINTING                      
*                                                                               
PPBANK   DS    CL10                BANK ACCTS                                   
USABANK  DS    CL10                                                             
PLSBANK  DS    CL10                                                             
*                                                                               
SAVNAME  DS    0CL35                                                            
LASTNAME DS    CL16                PERFORMER'S LAST NAME                        
FRSTNAME DS    CL16                PERFORMER'S FIRST NAME                       
MIDNAME  DS    CL16                PERFORMER'S MIDDLE NAME                      
*                                                                               
TBLNQ    EQU   *-TBD                                                            
         SPACE 3                                                                
*              DSECT TO COVER SORT RECORD                                       
         SPACE 1                                                                
SORTD    DSECT                                                                  
SORTBNK  DS    CL10                BANK ACCOUNT                                 
SORTCHK  DS    CL8                 CHECK NUMBER                                 
SORTDTE  DS    PL3                 CHECK DATE                                   
SORTAMT  DS    XL4                 CHECK AMOUNT                                 
SORTSTAT DS    XL1                 CHECK STATUS                                 
SORTNAME DS    XL35                PAYEE NAME                                   
SORTNAM2 DS    XL35                PAYEE NAME 2                                 
SORTNAM3 DS    XL35                PAYEE NAME 3                                 
SORTLNQ  EQU   *-SORTD                                                          
         SPACE 3                                                                
*              DSECT TO COVER PRINT LINE                                        
         SPACE 1                                                                
LINED    DSECT                                                                  
BL       DS    CL1                                                              
LINCHK   DS    CL8                 CHECK NUMBER                                 
BC1      DS    CL1                                                              
LINAMT   DS    CL12                CHECK AMOUNT                                 
BC2      DS    CL1                                                              
LINDTE   DS    CL8                 CHECK DATE                                   
BR       DS    CL1                                                              
         DS    CL1                                                              
LINLNQ   EQU   *-LINED             L'CHUNK                                      
LINNEXT  EQU   *                   A(NEXT CHUNK)                                
         EJECT                                                                  
*              DSECT TO COVER TAPE RECORD  (LA SALLE)                           
         SPACE 1                                                                
RECD     DSECT                                                                  
RECTYPE  DS    CL1                 RECORD TYPE                                  
RECTYH   EQU   C'H'                HEADER                                       
RECTYD   EQU   C'D'                DETAIL                                       
RECTYT   EQU   C'T'                TRAILER                                      
*                                                                               
RECHDR   EQU   *                   HEADER RECORD                                
RECHCPYN DS    CL30                COMPANY NAME                                 
RECHDATE DS    CL8                 RUN DATE (MMDDYY)                            
         DS    CL130               N/A                                          
*                                                                               
         ORG   RECHDR              DETAIL RECORD                                
RECDBNKN DS    CL3                 BANK NUMBER (965)                            
RECDACCN DS    CL10                ACCOUNT NUMBER                               
         DS    CL7                 N/A                                          
RECDVOID DS    CL1                 VOID INDICATOR 'V'                           
RECDCHKN DS    CL10                SERIAL NUMBER                                
RECDAMNT DS    CL10                DOLLAR AMOUNT                                
RECDIDTE DS    CL8                 ISSUE DATE                                   
RECDADDI DS    CL15                ADDITIONAL INFO                              
RECPAYEE DS    CL35                PAYEE LINE 1                                 
RECPAYE2 DS    CL35                PAYEE LINE 2                                 
RECPAYE3 DS    CL35                PAYEE LINE 3                                 
*                                                                               
         ORG   RECHDR              TRAILER RECORD                               
RECTBNKN DS    CL3                 BANK NUMBER (965)                            
RECTACCN DS    CL10                ACCOUNT NUMBER                               
         DS    CL8                 N/A                                          
RECTRCNT DS    CL10                CHECK ISSUE RECORD COUNT                     
RECTTAMT DS    CL10                TOTAL AMOUNT                                 
         DS    CL128               N/A                                          
RECLNQ   EQU   *-RECD                                                           
         EJECT                                                                  
*&&DO          DSECT TO COVER TAPE RECORD  (NORTHERN TRUST COMPANY)             
         SPACE 1                                                                
RECD     DSECT                                                                  
RECTYPE  DS    CL1                 RECORD TYPE                                  
RECTYH   EQU   C'H'                HEADER                                       
RECTYR   EQU   C'R'                REGISTER (DETAIL)                            
RECTYT   EQU   C'T'                TOTAL                                        
RECTYE1  EQU   C'E'                ERROR (DETAIL)                               
RECTYA1  EQU   C'G'                TRAILER                                      
*                                                                               
RECHDR   EQU   *                   HEADER RECORD                                
RECNAME  DS    CL33                COMPANY NAME                                 
RECRUN   DS    CL6                 RUN DATE (MMDDYY)                            
*                                                                               
         ORG   RECHDR              REGISTER (DETAIL) RECORD                     
RECBNK   DS    CL10                BANK ACCOUNT                                 
RECCHK   DS    CL10                CHECK NUMBER                                 
RECAMT   DS    CL10                CHECK AMOUNT                                 
RECDTE   DS    CL6                 CHECK DATE (MMDDYY)                          
RECTRNS  DS    CL3                 TRANSACTION CODE                             
RECTRADD EQU   C'SA '              SINGLE ADD                                   
RECTRDEL EQU   C'SD '              SINGLE DELETE                                
*                                                                               
         ORG   RECHDR              TOTAL RECORD                                 
         DS    CL10                BANK ACCOUNT                                 
RECCNT   DS    CL10                ITEM COUNT                                   
RECTOT   DS    CL10                TOTAL AMOUNT                                 
RECSPARE DS    CL9                 NOT USED                                     
RECLNQ   EQU   *-RECD                                                           
*                                                                               
         ORG   RECHDR                                                           
RECTYPE2 DS    CL1                                                              
RECTYE2  EQU   C'R'                                                             
RECTYA2  EQU   C'T'                                                             
*                                                                               
RECHDR2  EQU   *                   ERROR (DETAIL) RECORD                        
RECERRM  DS    CL78                ERROR MESSAGE                                
*                                                                               
         ORG   RECHDR2             TRAILER RECORD                               
RECTOTN  DS    CL10                TOTAL RECORDS                                
RECTOTA  DS    CL16                TOTAL AMOUNT                                 
REC1CHK  DS    CL10                FIRST CHECK                                  
REC2CHK  DS    CL10                SECOND CHECK                                 
*                                                                               
         ORG   RECHDR2             HEADER RECORD                                
RECCRDT  DS    CL8                 CREATION DATE                                
RECAKDT  DS    CL8                 ACKNOWLEDGEMENT DATE                         
RECAKTM  DS    CL6                 ACKNOWLEDGEMENT TIME                         
RECSHNM  DS    CL9                 SHORT NAME                                   
*&&                                                                             
         EJECT                                                                  
CNRECD   DSECT                                                                  
CNREC    DS    0CL170                                                           
CNRECDET EQU   *                   DETAIL RECORD                                
CNRDACCN DS    CL13                ACCOUNT NUMBER                               
CNRDCHKN DS    CL10                CHECK NUMBER                                 
CNRDCHKA DS    CL11                CHECK AMOUNT                                 
CNRDISSD DS    CL6                 ISSUE DATE                                   
CNRDPAYI DS    CL15                PAYEE IDENTIFICATION                         
         DS    CL115               FILLER                                       
*                                                                               
         ORG   CNRECDET                                                         
CNRECHDR DS    CL4                 HEADER STARTS WITH 'ARSH'                    
CNRHFILT DS    CL3                 FILE TYPE (ISS=ISSUE FILE)                   
CNRHCUSN DS    CL8                 CUSTOMER SHORT NAME                          
CNRHDCHA DS    CL13                DCH ACC NUMBER, MUST HAVE ALL 13 CH          
CNRHCRED DS    CL6                 FILE CREATION DATE                           
CNRHTEST DS    CL4                 "TEST" OR BLANK (ALL CAPS)                   
         DS    CL132               FILLER                                       
*                                                                               
         ORG   CNRECDET                                                         
CNRECTRL DS    CL4                 TRAILER STARTS WITH 'ARST'                   
CNRTNITM DS    CL9                 # OF ITEMS - (HEADER AND TRAILER)            
CNRTDLRA DS    CL13                DOLLAR AMOUNT OF ITEMS                       
         DS    CL144               FILLER                                       
CNRECLNQ EQU   *-CNRECD                                                         
         EJECT                                                                  
       ++INCLUDE TAREPFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPFCD                                                       
         SPACE 3                                                                
         PRINT OFF                                                              
* DDGENTWA   (MUST FOLLOW LAST SCREEN)                                          
* DDPERVAL                                                                      
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* DDBIGBOX                                                                      
* TAGENFILE                                                                     
* TASYSDSECT                                                                    
* TASYSEQUS                                                                     
* TAREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAREPWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'091TAREP1C   03/04/16'                                      
         END                                                                    
