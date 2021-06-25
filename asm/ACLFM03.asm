*          DATA SET ACLFM03    AT LEVEL 023 AS OF 02/21/15                      
*PHASE T60303A,+0                                                               
*INCLUDE ACPUTEL                                                                
*INCLUDE ACDELEL                                                                
*INCLUDE VATICAN                                                                
*INCLUDE AC1RMNT                                                                
*INCLUDE CONVERT                                                                
         TITLE 'MODULE TO HANDLE ACCOUNTS'                                      
T60303   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LWSX-LWSD,**LFM3**,R9,RR=R5,CLEAR=YES                            
         LR    R8,RC                                                            
         USING LWSD,R8                                                          
         L     RA,0(R1)                                                         
         USING T603FFD,RA                                                       
         L     RC,4(R1)                                                         
         USING LOGWORKD,RC                                                      
         ST    R5,PRELOC                                                        
         ST    RB,SAVRB                                                         
                                                                                
         USING COMFACSD,R5                                                      
         L     R5,COMFACS                                                       
         MVC   VSCANNER,CSCANNER   SAVE THESE IN MY LWS                         
         MVC   VUNSCAN,CUNSCAN                                                  
         MVC   VDICTAT,CDICTATE                                                 
         MVC   VGETTXT,CGETTXT                                                  
*                                                                               
         GOTO1 VDICTAT,DMCB,C'L   ',DICI,DICO                                   
*                                                                               
         EJECT ,                                                                
*------------------------------------------------------------------*            
* IS THIS A NEW RECORD                                             *            
* BUILD KEY FOR COMPANY/UNIT/LEDGER/ACCOUNT                        *            
* DISPLAY NAMES OF ABOVE                                           *            
*------------------------------------------------------------------*            
         MVI   ERROR,X'FF'                                                      
         CLI   MODE,BUILDKEY       IS THIS A NEW RECORD?                        
         BNE   AC10                NO                                           
*                                                                               
AC01     DS    0H                                                               
         MVC   KEY,SPACES          ZERO KEY                                     
         MVC   KEY(1),COMPANY      ADD COMPANY TO KEY                           
         LA    R2,LOGUNITH         R2 = UNIT HEADER                             
         GOTO1 ANY                 LENGTH RETURNED IN R1                        
         MVC   KEY+1(1),LOGUNIT                                                 
         TM    4(R2),X'20'         WAS PREVALID BIT SET?                        
         BO    AC2                 YES                                          
         NI    LOGLEDGH+4,X'DF'                                                 
         FOUT  LOGUNAMH,SPACES,36  SET TRANSBIT FOR UNIT NAME                   
         FOUT  LOGLNAMH,SPACES,36  SET TRANSBIT FOR LEDGER NAME                 
         MVI   KEY+1,C' '                                                       
         GOTO1 READ                                                             
         BAS   RE,SAVEM                                                         
         MVC   KEY+1(1),LOGUNIT    ADD UNIT TO KEY                              
         GOTO1 READ                                                             
         GOTO1 NAMOUT              DISPLAY UNIT NAME                            
         OI    4(R2),X'20'         SET PREVALID BIT                             
         MVI   ANYKEY,C'Y'                                                      
                                                                                
AC2      LA    R2,LOGLEDGH         R2 = LEDGER HEADER                           
         GOTO1 ANY                 LENGTH RETURNED IN R1                        
         MVC   KEY+2(1),LOGLEDG    ADD LEDGER TO KEY                            
*                                                                               
         OI    LOGVEN1H+6,X'80'    TRANSMIT VENDOR TYPE                         
         CLC   KEY+1(2),=C'SV'     PROVISIONAL ONLY FOR SV                      
         BE    *+12                                                             
         OI    LOGVEN1H+1,X'20'    PROTECT VENDOR TYPE                          
         B     *+8                                                              
         NI    LOGVEN1H+1,X'FF'-X'20'  UNPROTECT                                
*                                                                               
         LA    R4,SAVECOMP                                                      
         USING CPYELD,R4          COMPANY                                       
         CLC   CPYPROD,KEY+1       DOES UNIT/LEDGER MATCH?                      
         BNE   AC3                 NO                                           
         CLI   LOGACT,C'N'         DOES ACTION = N?                             
         BNE   AC3                 NO                                           
         MVI   ERROR,NOTVLREC      FLAG AS INVALID RECORD                       
         B     XXIT                EXIT                                         
         DROP  R4                                                               
                                                                                
AC3      DS    0H                                                               
         TM    4(R2),X'20'         WAS PREVALID BIT SET?                        
         BO    AC4                 YES                                          
         FOUT  LOGLNAMH,SPACES,36  SET TRANSBIT FOR LED NAME                    
         GOTO1 READ                                                             
         GOTO1 NAMOUT              DISPLAY LEDGER NAME                          
         BAS   RE,SAVEM                                                         
         OI    4(R2),X'20'         SET PREVALID BIT FOR LEDGER                  
*                                                                               
         MVI   ANYKEY,C'Y'                                                      
                                                                                
AC4      LA    R2,LOGACCH          R2 = ACCOUNT HEADER                          
         GOTO1 ANY                 LENGTH RETURNED IN R1                        
         GOTO1 MOVE                                                             
         MVC   KEY+3(12),WORK      MOVE ACCT NAME TO KEY                        
         TM    4(R2),X'20'         IS PREVALID BIT SET?                         
         BO    XXIT                YES                                          
         MVI   ANYKEY,C'Y'                                                      
         OI    4(R2),X'20'         SET PREVLID BIT                              
         B     XXIT                                                             
         EJECT ,                                                                
*------------------------------------------------------------------*            
* ROUTINE TO SAVE HEIRARCHY DETAILS                                             
*------------------------------------------------------------------*            
SAVEM    NTR1                                                                   
         LA    R4,IO                                                            
         AH    R4,DATADISP                                                      
         SR    R3,R3                                                            
                                                                                
SAVEH10  CLI   0(R4),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R4),ACLELQ        X'16' - ACCOUNT LENGTHS ELEMENT              
         BE    SAVEH20                                                          
         CLI   0(R4),CPYELQ        X'10' - COMPANY ELEMENT                      
         BE    SAVEH30                                                          
         IC    R3,1(R4)                                                         
         AR    R4,R3                                                            
         B     SAVEH10                                                          
                                                                                
SAVEH20  MVC   SAVEHEIR,0(R4)                                                   
         BAS   RE,GETLEVS          GET LEVEL STRUCTURE                          
         B     XXIT                                                             
*                                                                               
SAVEH30  MVC   SAVECOMP,0(R4)                                                   
         B     XXIT                                                             
*                                                                               
         EJECT ,                                                                
**********************************************************************          
* GETLEVS SUBROUTINE MAKES LEVELS SOFT, RETURNS INDIVIDUAL LEVEL     *          
* LENGTHS AND GIVES NUMBER OF NESTED LEVELS IN LEDGER RECDS          *          
**********************************************************************          
GETLEVS  NTR1                                                                   
         USING ACLELD,R4                                                        
         LA    R4,SAVEHEIR                                                      
         MVC   LEVELS(LEVLNQ),SPACES   CLEAR LEVELS LENGTH/DISC                 
*                                                                               
* CONVERT 1,3,5,12 -> 1,2,2,7                                                   
*                                                                               
         LHI   R0,LEVELQ                 R0 = MAXIMUM NUMBER OF LEVELS          
         STC   R0,LEVNUM                 ASSUME 4 LEVEL STRUCTURE               
         LA    R1,ACLVALS                R1 = FIRST LEVEL LENGTH                
         LA    RE,LEVELS                 STORE ACCUMULATIVE LNTH HERE           
         LA    R2,LEVLNQS          R2 = FIRST LEVEL INDIVIDUAL LENGTH           
         SR    R3,R3               R3 = PREV LEVEL LENGTH (INITIALLY 0)         
         SR    R5,R5                                                            
*                                                                               
GLEV10   ICM   R5,1,0(R1)                CURRENT LEVEL LENGTH                   
         BZ    GLEV20                    NO MORE LEVELS - ADJUST LEVNUM         
         STC   R5,0(RE)                                                         
         SR    R5,R3                     SUBTRACT CURRENT FROM PREVIOUS         
*                                                                               
         STC   R5,0(R2)                  CURRENT INDIVIDUAL LENGTH              
         IC    R3,0(R1)                  UPDATE R3                              
         LA    R1,L'ACLVALS(R1)                                                 
         LA    R2,1(R2)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,GLEV10                                                        
*                                                                               
         B     GLEVX                                                            
*                                                                               
GLEV20   LA    R1,LEVELQ                 R1 = MAXIMUM NUMBER OF LEVELS          
         SR    R1,R0               R0 = NUM OF LEVELS OF MAX NOT USED           
         STC   R1,LEVNUM           LEVNUM = ACTUAL NUMBER OF LEVELS             
*                                                                               
GLEVX    XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
*------------------------------------------------------------------*            
* DISPLAY ACCOUNT DETAILS *                                                     
*------------------------------------------------------------------*            
AC10     CLI   MODE,DSPLYREC       ARE WE IN DISPLAY MODE?                      
         BNE   AC20                NO                                           
         LA    R2,LOGNMDSH         R2 = NAME HEADER                             
         GOTO1 NAMOUT              DISPLAY ACCOUNT NAME                         
         LA    R2,LOGADD1H         R2 = ADDRESS HEADER                          
         GOTO1 ADDROUT             DISPLAY ACCOUNT ADDRESS                      
         GOTO1 =A(ACDSLY),DMCB,(RC),(R8),RR=PRELOC                              
         LA    R2,LOGANAMH                                                      
         OI    4(R2),X'20'         SET VALID BIT                                
         B     XXIT                                                             
                                                                                
*------------------------------------------------------------------*            
* BUILD AN ACCOUNT RECORD                                          *            
*------------------------------------------------------------------*            
*                                                                               
AC20     GOTO1 =A(ACBLD),DMCB,(RC),(R8),RR=PRELOC                               
*                                                                               
XXIT     XIT1  REGS=(R2)                                                        
         EJECT ,                                                                
         LTORG                                                                  
         EJECT ,                                                                
*------------------------------------------------------------------*            
* ACDSLY ROUTINE - DISPLAYS BOTTOM OF ACCOUNT SCREEN *                          
*------------------------------------------------------------------*            
*                                                                               
ACDSLY   DS    0D                                                               
         NMOD1 0,**DSPLY**,R9                                                   
         L     RC,0(R1)                                                         
         L     R8,4(R1)                                                         
*                                                                               
         GOTO1 =A(EQUACC),DMCB,(RC),(R8),RR=PRELOC                              
*                                                                               
         GOTO1 STATIN                                                           
         LA    R4,ELEMENT                                                       
         USING RSTELD,R4              RSTELD = STATUS                           
*                                                                               
         FOUT  LOGVEN1H,SPACES,1   SET TRANSBIT FOR VENDOR TYPE                 
         CLC   KEY+1(2),=C'SV'     PROVISIONAL ONLY FOR SV                      
         BNE   ACDSP1                                                           
         CLI   RSTLN,RSTLN3Q                                                    
         BL    ACDSP1                                                           
         TM    RSTSTAT5,RSTSPROV   PROVISIONAL ACCOUNT?                         
         BNO   ACDSP1              NO                                           
         MVI   LOGVEN1,C'P'        SHOW P - PROVISIONAL                         
*                                                                               
ACDSP1   FOUT  LOGFILTH,RSTFILT1,1    SET TRANSBIT FOR ACCT FILT1               
         FOUT  LOGFLT2H,RSTFILT2,1    SET TRANSBIT FOR ACCT FILT2               
         FOUT  LOGANALH,SPACES,1      SET TRANSBIT FOR ANAL FILT                
*&&US                                                                           
         OI    LOGANALH+6,X'80'                                                 
         MVC   LOGANAL,SPACES                                                   
         CLI   RSTFILT3,X'40'                                                   
         BNH   *+10                                                             
         MVC   LOGANAL,RSTFILT3                                                 
*&&                                                                             
*------------------------------------------------------------------*            
* COMPARE UNIT/LEDGER FOR UK                                       *            
*------------------------------------------------------------------*            
*&&UK                                                                           
         CLC   KEY+1(2),=C'SF'                                                  
         BE    ACDSP2                                                           
         FOUT  LOGANALH,RSTFILT3,1  SET TRANSBIT FOR ANALYSIS FILT              
*&&                                                                             
ACDSP2   FOUT  LOGSUBH,RSTFILT4     SET TRANSBIT FOR SUBCOMPANY                 
*                                                                               
         FOUT  LOGFLT5H,SPACES,1                                                
*                                                                               
         CLI   RSTLN,RSTLN2Q        FILTER 5 IS ONLY ON NEW ELEMS               
         BL    ACDSP3                                                           
         CLI   RSTFILT5,C' '                                                    
         BNH   ACDSP3                                                           
         FOUT  LOGFLT5H,RSTFILT5    SET TRANSBIT FOR FILTER #5                  
*                                                                               
ACDSP3   FOUT  LOGSECH,SPACES,3     SET TRANSBIT FOR SECURITY                   
         FOUT  LOGPROFH,SPACES,54   SET TRANSBIT FOR ACCT PROF                  
         FOUT  LOGPRO2H,SPACES,54   SET TRANSBIT FOR ACCT PROF                  
         FOUT  LOGPRO3H,SPACES,54   SET TRANSBIT FOR ACCT PROF                  
         FOUT  LOGMEMH,SPACES,54    SET TRANSBIT FOR MEMO                       
*                                                                               
         CLC   RSTSECY,SPACES       DOES SECURITY = BLANKS?                     
         BE    AC10A                YES                                         
         EDIT  (2,RSTSECY),(3,LOGSEC),ALIGN=LEFT   DISPLAY SECURITY             
*------------------------------------------------------------------*            
* DISPLAY PROFILES ASSOCIATED WITH ACCOUNT PROFILE *                            
*------------------------------------------------------------------*            
AC10A    LA    R2,BLOCK2            BLOCK2 = WORKAREA                           
         SR    R6,R6                                                            
         MVC   0(LUNSCNLN,R2),SPACES                                            
         TM    RSTSTAT1,X'80'       IS THIS A GEN PERSONAL EXP ITEM?            
         BZ    AC11                 NO                                          
         MVC   0(5,R2),=C'STAFF'                                                
         MVI   10(R2),C'Y'          DISPLAY STAFF=Y                             
         LA    R2,LUNSCNLN(,R2)                                                 
         LA    R6,1(,R6)                                                        
*                                                                               
AC11     MVC   0(LUNSCNLN,R2),SPACES                                            
         TM    RSTSTAT1,X'10'       DOES EXP A/C DEMAND DEPT?                   
         BZ    AC11A                NO                                          
         MVC   0(4,R2),=C'DEPT'                                                 
         MVI   10(R2),C'Y'                                                      
         LA    R2,LUNSCNLN(,R2)     DISPLAY DEPT=Y                              
         LA    R6,1(,R6)                                                        
*                                                                               
AC11A    MVC   0(LUNSCNLN,R2),SPACES                                            
         TM    RSTSTAT1,X'40'       IS ACCOUNT CLOSED?                          
         BZ    AC11B                NO                                          
         MVC   0(6,R2),=C'CLOSED'                                               
         MVI   10(R2),C'Y'                                                      
         LA    R2,LUNSCNLN(,R2)     CLOSED=Y                                    
         LA    R6,1(,R6)                                                        
*                                                                               
AC11B    MVC   0(LUNSCNLN,R2),SPACES                                            
         TM    RSTSTAT1,X'20'       IS ACCOUNT LOCKED OUT?                      
         BZ    AC11B1               NO                                          
         MVC   0(6,R2),=C'LOCKED'                                               
         MVI   10(R2),C'Y'                                                      
         LA    R2,LUNSCNLN(,R2)     DISPLAY LOCKED=Y                            
         LA    R6,1(,R6)                                                        
*                                                                               
AC11B1   MVC   0(LUNSCNLN,R2),SPACES                                            
         CLI   RSTLN,RSTLN1Q        IS THIS NEW LENGTH?                         
         BNH   AC11C                NO                                          
         TM    RSTSTAT2,X'20'       IS PAYMENT ON HOLD?                         
         BZ    AC11B2               NO                                          
         MVC   0(3,R2),=C'PAY'                                                  
         MVI   10(R2),C'N'                                                      
         LA    R2,LUNSCNLN(,R2)     DISPLAY PAY=N                               
         LA    R6,1(,R6)                                                        
*                                                                               
AC11B2   MVC   0(LUNSCNLN,R2),SPACES                                            
         CLC   KEY+1(2),=C'SE'      FOR EXPENSE ONLY                            
         BNE   AC11B3               NO                                          
         TM    RSTSTAT2,X'03'       MAKE COST POSTINGS                          
         BZ    AC11B3               NO                                          
         MVC   0(4,R2),=C'COST'                                                 
         MVI   10(R2),C'Y'          COST=Y                                      
         TM    RSTSTAT2,X'02'                                                   
         BO    *+8                                                              
         MVI   10(R2),C'N'          COST=N                                      
         LA    R2,LUNSCNLN(,R2)                                                 
         LA    R6,1(,R6)                                                        
*                                                                               
AC11B3   MVC   0(LUNSCNLN,R2),SPACES                                            
         CLI   RSTLN,RSTLN1Q        SKIP SHORT ELEMENTS                         
         BNH   AC11B4                                                           
         TM    RSTSTAT2,X'08'       IS DUPLICATE CHECKING DISABLED              
         BZ    AC11B4               NO                                          
         MVC   0(5,R2),=C'CKDUP'                                                
         MVI   10(R2),C'N'                                                      
         LA    R2,LUNSCNLN(,R2)     DISPLAY CKDUP=N                             
         LA    R6,1(,R6)                                                        
*                                                                               
AC11B4   MVC   0(LUNSCNLN,R2),SPACES                                            
         CLI   RSTLN,RSTLN3Q                                                    
         BL    AC11C                                                            
         TM    RSTX1099,RSTXRENT                                                
         BZ    AC11B4A                                                          
         MVC   10(3,R2),=C'RNT'                                                 
         B     AC11B4D                                                          
AC11B4A  TM    RSTX1099,RSTXROYL                                                
         BZ    AC11B4B                                                          
         MVC   10(3,R2),=C'ROY'                                                 
         B     AC11B4D                                                          
AC11B4B  TM    RSTX1099,RSTXPRIZ                                                
         BZ    AC11B4C                                                          
         MVC   10(3,R2),=C'PRZ'                                                 
         B     AC11B4D                                                          
AC11B4C  TM    RSTX1099,RSTXMEDC                                                
         BZ    AC11B5                                                           
         MVC   10(3,R2),=C'MED'                                                 
AC11B4D  MVC   0(4,R2),=C'1099'                                                 
         LA    R2,LUNSCNLN(,R2)     DISPLAY CKDUP=N                             
         LA    R6,1(,R6)                                                        
*                                                                               
AC11B5   TM    RSTSTAT5,RSTSNBIZ                                                
         BZ    AC11B5A                                                          
         MVC   0(LUNSCNLN,R2),SPACES                                            
         MVC   0(6,R2),=C'NEWBIZ'                                               
         MVI   10(R2),C'Y'                                                      
         LA    R2,LUNSCNLN(,R2)    DISPLAY CKDUP=N                              
         LA    R6,1(,R6)                                                        
*                                                                               
AC11B5A  TM    RSTSTAT5,RSTSBONO                                                
         BZ    AC11B5B                                                          
         MVC   0(LUNSCNLN,R2),SPACES                                            
         MVC   0(7,R2),=C'PROBONO'                                              
         MVI   10(R2),C'Y'                                                      
         LA    R2,LUNSCNLN(,R2)    DISPLAY CKDUP=N                              
         LA    R6,1(,R6)                                                        
*                                                                               
AC11B5B  TM    RSTSTAT5,RSTSHOUS                                                
         BZ    AC11B6                                                           
         MVC   0(LUNSCNLN,R2),SPACES                                            
         MVC   0(5,R2),=C'HOUSE'                                                
         MVI   10(R2),C'Y'                                                      
         LA    R2,LUNSCNLN(,R2)     DISPLAY CKDUP=N                             
         LA    R6,1(,R6)                                                        
*                                                                               
AC11B6   MVC   0(LUNSCNLN,R2),SPACES                                            
         TM    RSTSTAT5,RSTSPROD                                                
         BZ    AC11B6A                                                          
         MVI   10(R2),C'P'                                                      
         B     AC11B6C                                                          
AC11B6A  TM    RSTSTAT5,RSTSPRJB                                                
         BZ    AC11B6E                                                          
         MVI   10(R2),C'J'                                                      
AC11B6C  MVC   0(3,R2),=C'T/S'                                                  
         LA    R2,LUNSCNLN(,R2)     DISPLAY CKDUP=N                             
         LA    R6,1(,R6)                                                        
*                                                                               
AC11B6E  TM    RSTMAIL,RSTMAIOV                                                 
         BZ    AC11C                                                            
         MVC   0(4,R2),=C'MAIL'                                                 
         MVC   10(2,R2),=C'OV'      MAIL = OVERNIGHT FOR VENDORS                
         LA    R2,LUNSCNLN(,R2)                                                 
         LA    R6,1(,R6)                                                        
*                                                                               
AC11C    MVC   0(LUNSCNLN,R2),SPACES                                            
         TM    RSTSTAT1,X'08'       IS OUTPUT FILE FOR BANK RECON?              
         BZ    AC11D                NO                                          
         MVC   0(7,R2),=C'OUTFILE'                                              
         MVI   10(R2),C'Y'                                                      
         LA    R2,LUNSCNLN(,R2)    DISPLAY OUTFILE=Y                            
         LA    R6,1(,R6)                                                        
*                                                                               
AC11D    MVC   0(LUNSCNLN,R2),SPACES DEFAULT TASK CODE FOR TMS                  
         CLI   RSTLN,RSTLN3Q                                                    
         BL    AC11E                                                            
         CLC   RSTDFTSK,SPACES                                                  
         BNH   AC11E                                                            
         MVC   0(4,R2),=C'TASK'                                                 
         MVC   10(2,R2),RSTDFTSK    DISPLAY TASK CODE                           
         LA    R2,LUNSCNLN(,R2)                                                 
         LA    R6,1(,R6)                                                        
                                                                                
AC11E    MVC   0(LUNSCNLN,R2),SPACES                                            
         TM    RSTSTAT1,X'04'       IS THIS THE I/S LEDGER ACCT?                
         BZ    AC11F                NO                                          
*&&US*&& MVC   0(4,R2),=C'EXEC'     DISPLAY EXEC=Y FOR US                       
*&&UK*&& MVC   0(7,R2),=C'PLUSVAT'  DISPLAY PLUSVAT = Y FOR UK                  
         MVI   10(R2),C'Y'                                                      
         LA    R2,LUNSCNLN(,R2)                                                 
         LA    R6,1(,R6)                                                        
                                                                                
*&&US                                                                           
AC11F    MVC   0(LUNSCNLN,R2),SPACES                                            
         TM    RSTSTAT1,X'02'       IS THIS A B/S ACCT?                         
         BZ    AC11G                NO                                          
         MVC   0(6,R2),=C'VEND2C'                                               
         MVI   10(R2),C'Y'                                                      
         LA    R2,LUNSCNLN(,R2)     DISPLAY VEND2C=Y                            
         LA    R6,1(,R6)                                                        
*&&                                                                             
*&&UK                                                                           
AC11F    CLC   KEY+1(2),=C'SG'                                                  
         BNE   AC11G               VAT ACCOUNTS ONLY.                           
         MVC   0(LUNSCNLN,R2),SPACES                                            
         MVC   0(6,R2),=C'VATTYP'                                               
         MVI   10(R2),C'I'         X'02' = INPUT.                               
         TM    RSTSTAT1,X'02'                                                   
         BO    *+8                                                              
         MVI   10(R2),C'O'         BIT OFF = OUTPUT.                            
         LA    R2,LUNSCNLN(,R2)                                                 
         LA    R6,1(,R6)                                                        
*&&                                                                             
                                                                                
AC11G    MVC   0(LUNSCNLN,R2),SPACES                                            
         TM    RSTSTAT1,X'01'      IS SALARY FIGURE ACTUAL AMT?                 
         BZ    AC12                NO                                           
         MVC   0(6,R2),=C'ACTUAL'                                               
         MVI   10(R2),C'Y'                                                      
         LA    R2,LUNSCNLN(,R2)    DISPLAY ACTUAL=Y                             
         LA    R6,1(,R6)                                                        
                                                                                
AC12     MVC   0(LUNSCNLN,R2),SPACES                                            
         CLI   RSTCOSTG,C' '       DOES COSTING GROUP = SPACES?                 
         BE    AC13A               YES                                          
         CLI   RSTCOSTG,0          DOES COSTING GROUP = ZERO?                   
         BE    AC13A               YES                                          
         MVC   0(8,R2),=C'ANALYSIS'                                             
         MVC   10(1,R2),RSTCOSTG                                                
         LA    R2,LUNSCNLN(,R2)    DISPLAY ANALYSIS=                            
         LA    R6,1(,R6)                                                        
                                                                                
AC13A    MVC   0(LUNSCNLN,R2),SPACES                                            
         OC    RSTCCTR,SPACES                                                   
         CLC   RSTCCTR,SPACES      DOES COST CENTER = SPACES?                   
         BE    AC13B               YES                                          
         MVC   0(2,R2),=C'CC'                                                   
         CLI   RSTCCTRR,0          IS THERE A START POINT                       
         BE    *+14                                                             
         MVC   2(1,R2),RSTCCTRR    YES. GET IT                                  
         OI    2(R2),X'F0'         CHANGE IT FROM BIN. TO CHAR. FORM            
         MVC   10(3,R2),RSTCCTR                                                 
         LA    R2,LUNSCNLN(,R2)                                                 
         LA    R6,1(,R6)                                                        
                                                                                
AC13B    MVC   0(LUNSCNLN,R2),SPACES TYPE 21 & 22 CNOTRA-ACC ON 29              
         TM    RSTSTAT3,RSTSCAIV   POSTING IS VENDOR?                           
         BZ    AC13C               YES                                          
         MVC   0(6,R2),=C'VEND29'                                               
         MVI   10(R2),C'Y'         DISPLAY VEND29=Y                             
         LA    R2,LUNSCNLN(,R2)                                                 
         LA    R6,1(,R6)                                                        
                                                                                
AC13C    MVC   0(LUNSCNLN,R2),SPACES                                            
         CLC   KEY+1(2),=C'SC'     IS IT CASH SUBLEDGER?                        
         BNE   AC13E               NO                                           
         TM    RSTSTAT3,RSTSPRCR   IS IT CASHPAK CASH ACC FOR PEEL?             
         BZ    AC13D                                                            
         MVC   0(5,R2),=C'RECCR'                                                
         MVI   10(R2),C'Y'         DISPLAY RECCR=Y                              
         LA    R2,LUNSCNLN(,R2)                                                 
         LA    R6,1(,R6)                                                        
                                                                                
AC13D    MVC   0(LUNSCNLN,R2),SPACES                                            
         TM    RSTSTAT3,RSTSPRDR   IS IT PEELED RECONCILED DEBIT?               
         BZ    AC13E               NO                                           
         MVC   0(5,R2),=C'RECDR'                                                
         MVI   10(R2),C'Y'         DISPLAY RECDR=Y                              
         LA    R2,LUNSCNLN(,R2)                                                 
         LA    R6,1(,R6)                                                        
                                                                                
AC13E    MVC   0(LUNSCNLN,R2),SPACES                                            
         TM    RSTSTAT3,RSTSPRTS   IS PRO REQ ON TIMESHEET?                     
         BZ    AC13F               NO                                           
         MVC   0(2,R2),=C'PC'                                                   
         MVI   10(R2),C'Y'         DISPLAY PC=Y                                 
         LA    R2,LUNSCNLN(,R2)                                                 
         LA    R6,1(,R6)                                                        
                                                                                
AC13F    MVC   0(LUNSCNLN,R2),SPACES                                            
         TM    RSTSTAT3,RSTSIDPT   IS IT INDIRECT DEPT?                         
         BZ    AC13G               NO                                           
         MVC   0(3,R2),=C'IND'                                                  
         MVI   10(R2),C'Y'         DISPLAY IND=Y                                
         LA    R2,LUNSCNLN(,R2)                                                 
         LA    R6,1(,R6)                                                        
                                                                                
AC13G    MVC   0(LUNSCNLN,R2),SPACES                                            
         TM    RSTSTAT3,RSTSLAPL   IS LEDGER ACCT P/L?                          
         BZ    AC13H               NO                                           
         MVC   0(3,R2),=C'P/L'                                                  
         MVI   10(R2),C'Y'         DISPLAY P/L=Y                                
         LA    R2,LUNSCNLN(,R2)                                                 
         LA    R6,1(,R6)                                                        
                                                                                
AC13H    MVC   0(LUNSCNLN,R2),SPACES                                            
         TM    RSTSTAT3,X'02'      IS LEDGER ACCT B/S?                          
         BZ    AC13I               NO                                           
         MVC   0(3,R2),=C'BAL'                                                  
         MVI   10(R2),C'Y'         DISPLAY BAL=Y                                
         LA    R2,LUNSCNLN(,R2)                                                 
         LA    R6,1(,R6)                                                        
                                                                                
AC13I    MVC   0(LUNSCNLN,R2),SPACES                                            
         TM    RSTSTAT3,RSTSCCBJ   IS CLIENT COSTING BY JOB?                    
         BZ    AC13J               NO                                           
         MVC   0(5,R2),=C'JCOST'                                                
         MVI   10(R2),C'Y'         DISPLAY JCOST=Y                              
         LA    R2,LUNSCNLN(R2)                                                  
         LA    R6,1(,R6)                                                        
                                                                                
AC13J    MVC   0(LUNSCNLN,R2),SPACES                                            
         CLI   RSTOFFC,X'40'       IS THERE AN OFFICE CODE                      
         BNH   AC13K               NO                                           
         MVC   0(5,R2),=C'GLOFF'                                                
         MVC   10(2,R2),RSTOFFC    DISPLAY GLOFF=X(X)                           
         LA    R2,LUNSCNLN(,R2)                                                 
         LA    R6,1(,R6)                                                        
                                                                                
         USING RSTELD,R4                                                        
AC13K    MVC   0(LUNSCNLN,R2),SPACES                                            
         CLC   KEY+1(2),=C'SI'     SI ONLY                                      
         BNE   AC13Z                                                            
         OC    RSTSYSME,RSTSYSME   SYSTEM MEDIA?                                
         BZ    AC13Z               NONE                                         
         MVC   0(3,R2),=C'SMC'                                                  
         MVC   10(2,R2),RSTSYSME   SHOW IT                                      
         LA    R2,LUNSCNLN(,R2)                                                 
         LA    R6,1(,R6)                                                        
                                                                                
AC13Z    LA    R4,IO2              I02 = WORKAREA                               
         AH    R4,DATADISP         DATADISP = LENGTH OF ELEM                    
         SR    R3,R3                                                            
                                                                                
AC14     CLI   0(R4),0             IS ELEMENT CODE = 0?                         
         BE    AC19                YES                                          
         CLI   0(R4),RATEVATQ      X'36' - LOOK FOR VAT OR                      
         BE    AC16                                                             
         CLI   0(R4),RATEDSCQ      X'38' - DISCOUNT ELEMENTS                    
         BE    AC17                                                             
         CLI   0(R4),OTHELQ        X'23' - OR NUMBER                            
         BE    AC18A                                                            
         CLI   0(R4),MPYELQ        X'64' - OR HOMES                             
         BE    AC18C                                                            
         CLI   0(R4),GLPELQ        X'15' - OR GENERAL LEDGER                    
         BE    AC18D                                                            
         CLI   0(R4),SCMELQ        X'3E' - OR COMMENTS                          
         BE    AC18E                                                            
         CLI   0(R4),FFNELQ        X'25' - OR EXTRA NUMBER                      
         BE    AC18H                                                            
         CLI   0(R4),ABIELQ        X'27' - OR ACCT BILLING INFO                 
         BE    AC18I                                                            
         CLI   0(R4),OMEELQ        X'3F' - OR ON-LINE MEMO DATA                 
         BE    AC18J                                                            
         CLI   0(R4),RBRELQ        X'2B' - OR RETAIL BILLING                    
         BE    AC18K                                                            
         CLI   0(R4),SPAELQ        X'2C' - OR SPECIAL ACCOUNT                   
         BE    AC18L                                                            
         CLI   0(R4),FFTELQ        X'DB' - FREE FORM TEXT (FAX NUMBER)          
         BE    AC18M                                                            
         CLI   0(R4),ITCELQ        X'E4' - TAX TYPE EL?                         
         BE    AC18N                                                            
         CLI   0(R4),GDAELQ        X'E5' - GENERAL DATE ELEMENT ?               
         BE    AC18O                                                            
*MN                                                                             
         CLI   0(R4),DEXELQ        X'41' - DUE DATE EXPRESSION                  
         BE    AC18P                                                            
*MN                                                                             
                                                                                
AC15     IC    R3,1(,R4)           INSERT LENGTH INTO IO2                       
         AR    R4,R3                                                            
         B     AC14                                                             
                                                                                
AC16     MVC   0(LUNSCNLN,R2),SPACES DISPLAY VAT=                               
         MVC   0(3,R2),=C'VAT'                                                  
         MVC   10(4,R2),=C'ZERO'                                                
         USING RATELD,R4                                                        
         LA    R2,LUNSCNLN(,R2)                                                 
         LA    R6,1(,R6)                                                        
         OC    RATRATE,RATRATE                                                  
         BZ    AC15                                                             
         SH    R2,=AL2(LSCNDIVF)                                                
         B     AC18                                                             
                                                                                
AC17     MVC   0(LUNSCNLN,R2),SPACES DISPLAY DISCOUNT=                          
         MVC   0(8,R2),=C'DISCOUNT'                                             
         LA    R2,LUNSCNFX(,R2)                                                 
         LA    R6,1(,R6)                                                        
                                                                                
AC18     EDIT  (2,RATRATE),(6,(R2)),2,ALIGN=LEFT,MINUS=YES                      
         LA    R2,LSCNDIVF(,R2)                                                 
         B     AC15                                                             
                                                                                
AC18A    MVC   0(LUNSCNLN,R2),SPACES    NUMBER                                  
         USING OTHELD,R4                                                        
         CLC   OTHNUM,SPACES                                                    
         BE    AC18B                                                            
         CLI   OTHPROF,C'I'                                                     
         BNE   *+14                                                             
         MVC   0(2,R2),=C'ID'                                                   
         B     AC18A2                                                           
         MVC   0(6,R2),=C'NUMBER'                                               
         CLC   KEY+1(2),=C'1R'                                                  
         BNE   *+10                                                             
         MVC   0(7,R2),=C'BENEFIT'                                              
AC18A2   MVC   10(9,R2),OTHNUM                                                  
         LA    R2,LUNSCNLN(,R2)                                                 
         LA    R6,1(,R6)                                                        
                                                                                
AC18B    MVC   0(LUNSCNLN,R2),SPACES                                            
         CLC   OTHPROF,SPACES     BLANK ?                                       
         BE    AC18SKIP            YES, BRANCH TO SKIP DATA                     
         CLC   OTHPROF,=C'I   '   MUST BE ID                                    
         BE    AC18SKIP            YES, BRANCH TO SKIP DATA                     
         MVC   0(7,R2),=C'PROFILE'                                              
         MVC   10(4,R2),OTHPROF                                                 
         B     AC18EXIT            BRANCH TO STANDARD EXIT                      
                                                                                
         USING HOMELD,R4                                                        
AC18C    MVC   0(LUNSCNLN,R2),SPACES HOMES?                                     
         MVC   0(5,R2),=C'HOMES'                                                
         EDIT  HOMENO,(8,LUNSCNFX(R2)),ALIGN=LEFT                               
         B     AC18EXIT            BRANCH TO STANDARD EXIT                      
                                                                                
         USING GLPELD,R4          GENERAL LEDGER?                               
AC18D    MVC   0(LUNSCNLN,R2),SPACES                                            
         CLI   KEY+1,C'F'          FOR UNIT F DON'T DISPLAY                     
         BE    AC15 15 ELEMENT                                                  
         MVC   0(7,R2),=C'GENERAL'                                              
         MVC   10(10,R2),GLPACC1                                                
         CLI   GLPLN,26                                                         
         BL    *+10                                                             
         MVC   10(14,R2),GLPACC1                                                
         B     AC18EXIT            BRANCH TO STANDARD EXIT                      
                                                                                
         USING SCMELD,R4                                                        
AC18E    CLI   SCMTYPE,0          ONLY SHOW STANDARD COMMENTS                   
         BE    AC18SKIP            YES, BRANCH TO SKIP DATA                     
         MVC   0(LUNSCNLN,R2),SPACES                                            
         MVC   0(3,R2),=C'EST'                                                  
         TM    SCMTYPE,X'40'                                                    
         BO    *+10                                                             
         MVC   0(3,R2),=C'BIL'                                                  
         TM    SCMTYPE,X'C0'                                                    
         BM    *+10                                                             
         MVC   0(3,R2),=C'B+E'                                                  
                                                                                
         MVC   WORK(6),SCMNARR                                                  
         LA    RF,5                                                             
AC18F    CLI   WORK,C' '                                                        
         BNE   AC18G                                                            
         MVC   WORK(5),WORK+1                                                   
         MVI   WORK+5,C' '                                                      
         BCTR  RF,0                                                             
         B     AC18F                                                            
AC18G    EX    RF,*+4                                                           
         MVC   10(0,R2),WORK                                                    
         B     AC18EXIT            BRANCH TO STANDARD EXIT                      
                                                                                
         USING FFNELD,R4               EXTRA #?                                 
AC18H    MVC   0(LUNSCNLN,R2),SPACES                                            
         MVC   0(4,R2),=C'NUM2'                                                 
         CLC   KEY+1(2),=C'1R'                                                  
         BNE   *+10                                                             
         MVC   0(5,R2),=C'ADMIN'                                                
         ZIC   RF,FFNLN                                                         
         AHI   RF,-3                                                            
         EX    RF,*+4                                                           
         MVC   10(0,R2),FFNUMBER                                                
         B     AC18EXIT            BRANCH TO STANDARD EXIT                      
                                                                                
         USING ABIELD,R4           ACCOUNT BILL NUMBER?                         
AC18I    MVC   0(LUNSCNLN,R2),SPACES                                            
         CLC   ABIEANO,SPACES      IS THERE AN ACCOUNT BILL NUMBER ?            
         BE    AC18I1                                                           
         MVC   0(2,R2),=C'EA'                                                   
         MVC   10(14,R2),ABIEANO                                                
         LA    R2,LUNSCNLN(,R2)                                                 
         LA    R6,1(,R6)                                                        
                                                                                
AC18I1   MVC   0(LUNSCNLN,R2),SPACES                                            
         CLC   ABIACNO,SPACES      IS THERE AN ACCOUNT BILL NUMBER ?            
         BE    AC18I2                                                           
         MVC   0(2,R2),=C'AC'                                                   
         MVC   10(14,R2),ABIACNO                                                
         LA    R2,LUNSCNLN(,R2)                                                 
         LA    R6,1(,R6)                                                        
                                                                                
AC18I2   MVC   0(LUNSCNLN,R2),SPACES                                            
         CLI   ABILN,X'39'                                                      
         BL    AC18SKIP            NO,  BRANCH TO SKIP DATA                     
         CLC   ABIESNO,SPACES                                                   
         BE    AC18I3                                                           
         MVC   0(3,R2),=C'ENO'                                                  
         MVC   10(12,R2),ABIESNO                                                
         LA    R2,LUNSCNLN(,R2)                                                 
         LA    R6,1(,R6)                                                        
                                                                                
AC18I3   MVC   0(LUNSCNLN,R2),SPACES                                            
         CLC   ABIBUNO,SPACES                                                   
         BE    AC18I4                                                           
         MVC   0(3,R2),=C'BUD'                                                  
         MVC   10(15,R2),ABIBUNO                                                
         LA    R2,LUNSCNLN(,R2)                                                 
         LA    R6,1(,R6)                                                        
                                                                                
AC18I4   CLI   ABILN,X'39'                                                      
         BNH   AC18SKIP            NO,  BRANCH TO SKIP DATA                     
         MVC   0(LUNSCNLN,R2),SPACES                                            
         CLC   ABIBINO,SPACES                                                   
         BE    AC18I5                                                           
         MVC   0(2,R2),=C'BN'                                                   
         MVC   10(15,R2),ABIBINO                                                
         LA    R2,LUNSCNLN(,R2)                                                 
         LA    R6,1(,R6)                                                        
                                                                                
AC18I5   CLI   ABILN,X'48'                                                      
         BNH   AC18SKIP            NO,  BRANCH TO SKIP DATA                     
         MVC   0(LUNSCNLN,R2),SPACES                                            
         CLC   ABIBMEM,SPACES                                                   
         BE    AC18SKIP            NO,  BRANCH TO SKIP DATA                     
         MVC   0(2,R2),=C'BG'                                                   
         MVC   10(15,R2),ABIBMEM                                                
         B     AC18EXIT            BRANCH TO STANDARD EXIT                      
                                                                                
         USING OMEELD,R4                                                        
AC18J    ZIC   RF,OMELN          ON-LINE MEMO DATA?                             
         AHI   RF,-3                                                            
         CHI   RF,L'LOGMEM-1       CHECK IF WE EXCEEDED FIELD LENGTH            
         BL    *+8                                                              
         LHI   RF,L'LOGMEM-1     SET TO MAX FIELD LENGTH                        
         EX    RF,*+4                                                           
         MVC   LOGMEM(0),OMEMO                                                  
         FOUT  LOGMEMH                                                          
         B     AC15                                                             
                                                                                
         USING RBRELD,R4                                                        
AC18K    MVC   0(LUNSCNLN,R2),SPACES RETAIL OVERRIDES                           
         OC    RBRRECB,RBRRECB   FOR RECEIVABLES                                
         BZ    AC18K1                                                           
         MVC   0(4,R2),=C'RECV'                                                 
         MVC   10(12,R2),RBRRECB+2                                              
         LA    R2,LUNSCNLN(,R2)                                                 
         LA    R6,1(,R6)                                                        
                                                                                
AC18K1   MVC   0(LUNSCNLN,R2),SPACES                                            
         OC    RBRCOST,RBRCOST   OR COSTING                                     
         BZ    AC18SKIP            NO,  BRANCH TO SKIP DATA                     
         MVC   0(4,R2),=C'COST'                                                 
         MVC   10(12,R2),RBRCOST+2                                              
         B     AC18EXIT            BRANCH TO STANDARD EXIT                      
                                                                                
         USING SPAELD,R4                                                        
AC18L    MVC   0(LUNSCNLN,R2),SPACES SPECIAL ACCOUNT OVERRIDES                  
         L     RF,=A(PNTACC)       ACCOUNT POINTER TABLE                        
         A     RF,PRELOC                                                        
         USING PNTD,RF                                                          
AC18L2   CLI   0(RF),X'FF'         IN TABLE ?                                   
         BE    AC18SKIP            NO,  BRANCH TO SKIP DATA                     
         CLC   SPATYPE,PNTTYP                                                   
         BE    AC18L4                                                           
AC18L3   LA    RF,PNTLNQ(RF)                                                    
         B     AC18L2                                                           
*                                                                               
AC18L4   MVC   0(L'PNTCODE,R2),PNTCODE   LEFT SIDE TO UNSCAN BLOCK              
         LA    R1,L'SPAAULA       SET LENGTH                                    
         BCTR  R1,0                                                             
         LA    RE,SPAAULA         AND START OF ACCOUNT                          
         CLC   PNTLDG,SPACES       IF THERE'S A DEFAULT LEDGER                  
         BE    *+12                                                             
         AHI   R1,-2               DON'T DISPLAY UNIT/LEDGER                    
         LA    RE,2(RE)                                                         
         EX    R1,*+4                                                           
         MVC   10(0,R2),0(RE)      ACCOUNT TO RIGHT SIDE OF SCAN BLOCK          
         B     AC18EXIT            BRANCH TO STANDARD EXIT                      
                                                                                
         USING FFTELD,R4                                                        
AC18M    CLI   FFTTYPE,FFTTPFAX    FAX ELEMENT                                  
         BNE   AC18MA                                                           
         MVC   0(LUNSCNLN,R2),SPACES                                            
         MVC   0(3,R2),=C'FAX'                                                  
         ZIC   RF,FFTDLEN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   10(0,R2),FFTDATA                                                 
         B     AC18EXIT            BRANCH TO STANDARD EXIT                      
*                                                                               
AC18MA   CLI   FFTTYPE,FFTTEML     EMAIL ELEMENT                                
         BNE   AC18MB                                                           
         MVC   0(LUNSCNLN,R2),SPACES                                            
         MVC   0(2,R2),=C'EM'                                                   
         ZIC   RF,FFTDLEN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   10(0,R2),FFTDATA                                                 
         B     AC18EXIT            BRANCH TO STANDARD EXIT                      
*                                                                               
AC18MB   CLI   FFTTYPE,FFTTINDL    INDIR CLIST ELEMENT                          
         BNE   AC18MC                                                           
         MVC   0(LUNSCNLN,R2),SPACES                                            
         MVC   0(5,R2),=C'ILIST'                                                
         OC    FFTDATA(1),FFTDATA  IS THERE A METHOD?                           
         BZ    AC18B1                                                           
         MVC   5(1,R2),FFTDATA     MOVE IN METHOD EG 'ILIST1'                   
         MVI   6(R2),C'='                                                       
         MVC   7(3,R2),FFTDATA+1   MOVE IN LIST CODE                            
         B     AC18EXIT            BRANCH TO STANDARD EXIT                      
AC18B1   MVI   5(R2),C'='                                                       
         MVC   6(3,R2),FFTDATA+1   MOVE IN LIST CODE                            
         B     AC18EXIT                                                         
*                                                                               
AC18MC   CLI   FFTTYPE,FFTTCOMR    SPECIAL COMMISSION RATE ?                    
         BNE   AC18SKIP            NO,  SKIP                                    
         MVC   0(LUNSCNLN,R2),SPACES                                            
         MVC   0(4,R2),=C'COMM'                                                 
         UNPK  DUB(6),FFTDATA(4)   UNPACK                                       
         OI    DUB+5,X'F0'         CLEAR SIGN                                   
         LA    RF,10(,R2)          ->    'TO'  AREA                             
         CLI   DUB,C'0'            TENS  DIGIT ZERO ?                           
         BE    AC18MC1             YES,  SKIP                                   
         MVC   0(1,RF),DUB         MOVE  TENS  DIGIT                            
         LA    RF,1(,RF)           NEXT  'TO'  CHARACTER                        
*                                                                               
AC18MC1  CLI   DUB+1,C'0'          UNITS DIGIT ZERO ?                           
         BNE   AC18MC2             NO,   MOVE  IT                               
         CLI   DUB,C'0'            WAS   TENS  DIGIT ALSO ZERO ?                
         BE    AC18MC3             YES,  SKIP  UNITS DIGIT                      
*                                                                               
AC18MC2  MVC   0(1,RF),DUB+1       MOVE  UNITS DIGIT                            
         LA    RF,1(,RF)           NEXT  'TO'  CHARACTER                        
*                                                                               
AC18MC3  MVI   0(RF),C'.'          INSERT      DECIMAL   POINT                  
         MVC   1(4,RF),DUB+2       MOVE  THE   REST OF   THE  NUMBER            
         MVI   5(RF),C'%'          INSERT      PERCENT   SIGN                   
         B     AC18EXIT            BRANCH      TO   STD  EXIT                   
*                                                                               
         USING ITCELD,R4                                                        
AC18N    MVC   0(LUNSCNLN,R2),SPACES                                            
         OC    ITCPROV,ITCPROV                                                  
         BNZ   AC18N2                                                           
         MVC   0(7,R2),=C'GSTCODE'                                              
         B     AC18N3                                                           
AC18N2   MVC   0(2,R2),ITCPROV                                                  
AC18N3   MVC   10(1,R2),ITCTYPE                                                 
         MVI   11(R2),C'-'                                                      
         GOTO1 DATCON,DMCB,(1,ITCEFFD),(17,12(R2))                              
         B     AC18EXIT            BRANCH TO STANDARD EXIT                      
                                                                                
         USING GDAELD,R4           GENERAL DATE ELEMENT                         
AC18O    DS    0H                                                               
*                                  CLEAR THE ELEMENT LINE                       
         MVC   0(LUNSCNLN,R2),SPACES                                            
         CLI   GDATYPE,GDATRTLS    RETAIL EFFECTIVE START/END DATE              
         BE    AC18O2              NO,  BRANCH TO SKIP DATA                     
         CLI   GDATYPE,GDATRTLE    RETAIL EFFECTIVE START/END DATE              
         BNE   AC18SKIP            NO,  BRANCH TO SKIP DATA                     
         MVC   0(6,R2),=C'EFFEND'                                               
         B     *+10                                                             
AC18O2   MVC   0(8,R2),=C'EFFSTART'                                             
         GOTO1 DATCON,DMCB,(1,GDADATE),(6,LUNSCNFX(R2))                         
         B     AC18EXIT            BRANCH TO STANDARD EXIT                      
                                                                                
*MN                                                                             
AC18P    DS    0H                                                               
         USING DEXELD,R4                                                        
         MVC   0(7,R2),=C'DUEDATE'                                              
         LA    R1,WORK                                                          
         USING CONBLKD,R1                                                       
         XC    CONBLK(CONBLKL),CONBLK                                           
         MVI   CONACTN,CONATRAQ                                                 
         MVI   CONFLD,CONFIDUE                                                  
         MVI   CONILEN,L'DEXVAL                                                 
         LA    RF,DEXVAL                                                        
         STCM  RF,15,CONIADD                                                    
         LA    RF,10(R2)                                                        
         STCM  RF,15,CONOADD                                                    
         MVC   CONCOMF,COMFACS                                                  
         GOTO1 =V(CONVERT),WORK,RR=PRELOC                                       
         B     AC18EXIT            BRANCH TO STANDARD EXIT                      
*MN                                                                             
                                                                                
AC18EXIT DS    0H                  STANDARD AC18. EXIT                          
         LA    R2,LUNSCNLN(,R2)    BUMP TO NEXT LINE                            
         LA    R6,1(,R6)           ADD 1 TO LINES COUNTER                       
AC18SKIP DS    0H                  END OF PROCESSING FOR THIS ELEMENT           
         B     AC15                RETURN                                       
                                                                                
AC19     LTR   R6,R6                                                            
         BZ    DXIT                                                             
         GOTO1 VUNSCAN,DMCB,((R6),BLOCK2),('LSCNDIVF',LOGPROFH),0               
         CLI   DMCB,0                                                           
         BE    DXIT                                                             
         GOTO1 (RF),(R1),,('LSCNDIVF',LOGPRO2H)                                 
         CLI   DMCB,0                                                           
         BE    DXIT                                                             
         GOTO1 (RF),(R1),,('LSCNDIVF',LOGPRO3H)                                 
*                                                                               
DXIT     OI    LOGPROFH+4,X'20'         SET VALID BIT                           
         OI    LOGPRO2H+4,X'20'                                                 
         OI    LOGPRO3H+4,X'20'                                                 
         XIT1  REGS=(R2)                                                        
*                                                                               
DICI     DS    0X                                                               
         DCDDL AC#PST,3                                                         
         DCDDL AC#QST,3                                                         
         DCDDL AC#ONT,3                                                         
         DCDDL AC#HST,3                                                         
         DC    AL1(0)                                                           
*                                                                               
         EJECT ,                                                                
         LTORG                                                                  
         EJECT ,                                                                
*------------------------------------------------------------------*            
* EQUACC ROUTINE - DISPLAYS EQUIVALENT ACCOUNTS IF FFTTYPE=71      *            
*------------------------------------------------------------------*            
         SPACE 1                                                                
         USING EQUTBD,R6                                                        
*                                                                               
EQUACC   DS    0D                                                               
         NMOD1 0,**EQUAC**,R9                                                   
         L     RC,0(R1)                                                         
         L     R8,4(R1)                                                         
*                                                                               
         BAS   RE,BLDETAB          BUILD ACCNT EQUIV RULES TABLE                
*                                                                               
         LHI   R1,0                DISP OR NOT TO DISP EQU ACC FLDS             
EQUAC10  DS    0H                                                               
         CHI   R1,EQUMAX-1                                                      
         BH    EQUAC20                                                          
         LR    R4,R1                                                            
         MHI   R4,LOGLAB2H-LOGLAB1H                                             
         LA    R2,LOGLAB1H         POINT TO LABEL 1                             
         AR    R2,R4                                                            
         OI    6(R2),X'80'         SET TRANSMIT BIT ON                          
         OI    LOGEQUH+6,X'80'     SET TRANSMIT BIT ON                          
*                                                                               
         CLI   EQUACNT,0           DO WE HAVE ANY X'CA'/EQUIVA ACC RULE         
         BE    *+16                NO                                           
         NI    1(R2),X'FF'-X'0C'   MAKE LABELS A. B. C. D. E. NML INT           
         NI    LOGEQUH+1,X'FF'-X'0C'  NORMAL INTENSITY                          
         B     *+12                                                             
         OI    1(R2),X'0C'         MAKE LABELS A. B. C. D. E. LOW INT           
         OI    LOGEQUH+1,X'0C'     MAKE IT LOW INTENSITY                        
*                                                                               
         AHI   R1,1                START HIDING FROM LABEL E. --> A.            
         B     EQUAC10                                                          
*                                                                               
EQUAC20  LHI   R1,0                SPACE OUT ALL EQUVALENT ACC FLDS             
EQUAC30  CHI   R1,EQUMAX-1         HARDCODED 5 FIELDS ON SCREEN                 
         BH    EQUAC50                                                          
         LR    R4,R1                                                            
         MHI   R4,LOGEQU2H-LOGEQU1H                                             
         LA    R2,LOGEQU1H         CLEAR ALL EQUIVALENTACCOUNT FLDS             
         AR    R2,R4                                                            
*                                                                               
         LHI   R5,2                TO CLEAR NAME AND THEN CODE                  
EQUAC40  ZIC   R3,0(R2)            CLEARING EQU ACC NAME/ ACC CODE              
         SHI   R3,8                GET LENGTH OF FIELD                          
         OI    1(R2),X'20'         PROTECT IT                                   
         FOUT  (R2),SPACES,(R3)                                                 
         AHI   R3,8                                                             
         AR    R2,R3               POINT TO EQUIVALENT ACC CODE                 
         BCT   R5,EQUAC40                                                       
*                                                                               
         AHI   R1,1                BUMP TO NEXT LINE                            
         B     EQUAC30                                                          
*                                                                               
EQUAC50  LA    R4,IO2                                                           
         AH    R4,DATADISP                                                      
         LHI   R5,0                WHICH LINE TO START PRINTING                 
         LA    R6,EQUATAB          POINT TO TABLE                               
         USING FFTELD,R4                                                        
         SPACE 1                                                                
EQUAC60  DS    0H                                                               
         CLI   0(R4),0                                                          
         BE    EQUAC100                                                         
         CLI   0(R4),FFTELQ        X'DB' FREE FORM TEXT                         
         BNE   EQUAC70                                                          
         CLI   FFTTYPE,FFTTEPTR    TYPE 71                                      
         BE    EQUAC80                                                          
EQUAC70  ZIC   R3,1(R4)                                                         
         AR    R4,R3                                                            
         B     EQUAC60                                                          
         SPACE 1                                                                
EQUAC80  DS    0H                                                               
         ZIC   R3,EQUACNT                                                       
         CR    R5,R3               NO MORE THAN 5 LINES ALLOWED                 
         BL    *+6                                                              
         DC    H'0'                ACCOUNT EQU PNTR EXIST W/O A RULE            
*                                                                               
         ZIC   R1,EQUSEQ                                                        
         MHI   R1,LOGEQU2H-LOGEQU1H                                             
         LA    R2,LOGEQU1H         POINT WHERE TO PUT THE ACCOUNT               
         AR    R2,R1               POINT WHERE TO PUT ACCOUNT NAME              
         FOUT  (R2),EQUCODE,10                                                  
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         NI    1(R2),X'FF'-X'20'   UNPROTECT IT                                 
*                                                                               
         CLC   EQUSEQ,FFTSEQ       DOES SEQUENCE NUMBERS MATCH                  
         BE    EQUAC90                                                          
         LA    R6,EQULNQ(R6)       BUMP TO NEXT SEQUENCE NUM                    
         AHI   R5,1                                                             
         B     EQUAC80                                                          
*                                                                               
EQUAC90  DS     0H                                                              
         ZIC   R1,FFTDLEN          GET LENGTH OF ACCOUNT TO OUTPUT              
         FOUT  (R2),FFTDATA,(R1)                                                
*                                                                               
         LA    R6,EQULNQ(R6)       BUMP TO NEXT SEQUENCE NUM                    
         AHI   R5,1                MAX 5 LINES ALLOWED FOR NOW                  
         B     EQUAC70             CHECK IF MORE EQUIVALENT ACCOUNTS            
*                                                                               
EQUAC100 DS    0H                  PRINT LEFTOVERS FROM TABLE                   
         ZIC   R3,EQUACNT          NO. OF EQUIVALENT PNTR RULES                 
         CR    R5,R3               NO MORE THAN 5 LINES ALLOWED                 
         BNL   EQUACX                                                           
*                                                                               
         ZIC   R1,EQUSEQ           BUMP TO THIS FLD                             
         MHI   R1,LOGEQU2H-LOGEQU1H                                             
         LA    R2,LOGEQU1H         POINT WHERE TO PUT THE ACCOUNT               
         AR    R2,R1               POINT WHERE TO PUT ACCOUNT NAME              
         FOUT  (R2),EQUCODE,10                                                  
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         NI    1(R2),X'FF'-X'20'   UNPROTECT IT                                 
         LA    R6,EQULNQ(R6)       BUMP TO NEXT SEQUENCE NUM                    
         AHI   R5,1                MAX 5 LINES ALLOWED FOR NOW                  
         B     EQUAC100                                                         
EQUACX   XIT1                                                                   
         DROP  R6                                                               
*                                                                               
*------------------------------------------------------------------*            
* ROUTINE TO BUILD EQUIVALENT ACCOUNT TABLE IF X'CA' ELEMENT EXISTS             
*------------------------------------------------------------------*            
         USING EQUTBD,R2                                                        
         USING APRELD,R4           ONLY IF FOUND                                
*                                                                               
BLDETAB  NTR1                                                                   
         SPACE 1                                                                
         MVC   KEY,SPACES          ZERO KEY                                     
         MVC   KEY(1),COMPANY      ADD COMPANY TO KEY                           
         MVC   KEY+1(1),LOGUNIT    ADD UNIT TO KEY                              
         MVC   KEY+2(1),LOGLEDG    LEDGER TO KEY                                
         GOTO1 READ                                                             
*                                                                               
         LA    R4,IO                                                            
         AH    R4,DATADISP                                                      
         SR    R3,R3                                                            
*                                                                               
         XC    EQUATAB,EQUATAB     CLEAR TABLE                                  
         MVI   EQUACNT,0           NO. OF ENTRIES IN A TABLE                    
         LA    R2,EQUATAB                                                       
*                                                                               
BLDE10   DS    0H                                                               
         CLI   0(R4),0                                                          
         BE    BLDEX                                                            
         CLI   0(R4),APRELQ        X'CA' EQUIVALENT ACC RULES ELEM              
         BE    BLDE30                                                           
BLDE20   IC    R3,1(R4)            LOOK FOR X'CA' ELEMENTS BUILD TABLE          
         AR    R4,R3                                                            
         B     BLDE10                                                           
*                                                                               
BLDE30   DS    0H                                                               
         CLI   EQUACNT,EQUMAX      IS NO. OF ENTRIES HIGHER THAN MAX NO         
         BNH   *+6                                                              
         DC    H'0'                IF YOU DIE HERE INCREASE EQUMAX              
         ZIC   R1,EQUACNT                                                       
         AHI   R1,1                NO. OF X'CA' ELEMENTS/EQU ACC RULES          
         STC   R1,EQUACNT                                                       
*                                                                               
         MVC   EQUSEQ,APRSEQ       START FILING IN TABLE                        
         MVC   EQUCODE,APRDESC     FIL IN EQU ACC NAME/ SYSTEM NAME             
         LA    R2,EQULNQ(R2)                                                    
         B     BLDE20              GET NEXT RULE                                
*                                                                               
BLDEX    DS    0H                                                               
         XIT1                                                                   
         DROP  R2,R4                                                            
         EJECT ,                                                                
         LTORG                                                                  
         EJECT ,                                                                
*------------------------------------------------------------------*            
* BUILD AN ACCOUNT RECORD *                                                     
*------------------------------------------------------------------*            
*                                                                               
ACBLD    DS    0D                                                               
         NMOD1 0,**BLD**,R9                                                     
         L     RC,0(R1)                                                         
         L     R8,4(R1)                                                         
*                                                                               
         LA    R2,LOGANAMH                                                      
         GOTO1 ANY                                                              
         GOTO1 MOVE                                                             
         CLI   ACTION,C'A'         FOR ACTION AMEND SAVE NAME                   
         BNE   ACBLD00                                                          
         GOTO1 CHKNAM,DMCB,(C'B',IO),NAMESAVE                                   
         LA    RF,NAMESAVE-LOCALS  SAVE DISP TO SAVED NAME                      
         STCM  RF,3,DSAVNAM        FOR SEARCH                                   
ACBLD00  GOTO1 NAMIN                                                            
         MVC   HOLDKEY,KEY         SAVE KEY                                     
*                                                                               
         CLC   KEY+1(2),=C'1R'         FOR 1R MUST CHECK AND SEE IF ANY         
         BNE   ACBLD01A                PERSON '0F' RECORDS SET UP -             
         BRAS  RE,CHKPRSN                                                       
         CLI   ERROR,COSTPRGM          NO ERROR MEANS NO PERSON RECS            
         BNE   ACBLD01                 SO CONTINUE ON                           
*                                                                               
*                                      ** PERSON RECORDS FOUND **               
         CLC   WORK(7),=C'DELETE '     DO WE WANT TO DELETE ACCT?               
         BE    XIT                     YES - ERROR, SO EXIT WITH MSG            
         BRAS  RE,CHKOVER              EXCEPT FOR OVERHEAD ACCTS                
         CLI   ERROR,X'FE'             IF OVERHEAD SKIP CHECK                   
         BE    ACBLD01                 PERSON SCREEN                            
         CLI   LOGACT,C'N'             CAN'T ADD NEW RECORDS IF ON              
         BNE   ACBLD01                 NEW PERSON RECORDS                       
         MVI   ERROR,COSTPRGM                                                   
         B     XIT                                                              
ACBLD01  MVI   ERROR,0                                                          
         MVC   LOGHEAD,SPACES                                                   
         B     ACBLD02                                                          
*                                                                               
ACBLD01A CLC   KEY+1(2),=C'1C'         FOR 1C MUST CHECK AND SEE IF ANY         
         BNE   ACBLD02                 METHOD BUCKETS                           
         CLC   WORK(7),=C'DELETE '     DO WE WANT TO DELETE ACCT?               
         BNE   ACBLD02                                                          
         MVI   ERROR,INVALID                                                    
         CLI   LOGACT,C'N'             CAN'T ADD WITH NAME DELETE               
         BE    XIT                                                              
         BRAS  RE,CHKBUCK                                                       
         CLI   ERROR,67                                                         
         BE    XIT                                                              
*                                                                               
ACBLD02  CLC   WORK(7),=C'DELETE '     DO WE WANT TO DELETE ACCT?               
         BE    AC30                    YES                                      
         CLI   ACCEMU,C'Y'         IS IT THE NEW FILE                           
         BNE   ACBLD03                                                          
         CLI   LOGACT,C'A'         MUST BE AMEND                                
         BNE   ACBLD03                                                          
         TM    4(R2),X'20'         HAS NAME CHANGED                             
         BO    ACBLD03                                                          
         CLI   ACTION,C'A'         IF AMEND ADD PTR FOR NAME CHANGE             
         BNE   ACBLD03                                                          
         GOTO1 CHKNAM,DMCB,(C'A',IO2),NAMESAVE                                  
ACBLD03  MVI   DRFTITMS,C'N'       SET NO DRAFT ITEMS                           
         LA    R7,IO2                                                           
         MVI   ELCODE,ASTELQ                                                    
         BRAS  RE,GETEL                                                         
         BNE   *+18                                                             
         OC    ASTDRAFT-ASTELD(L'ASTDRAFT,R7),ASTDRAFT-ASTELD(R7)               
         BZ    *+8                                                              
         MVI   DRFTITMS,C'Y'       DRAFT ITEMS ON ACCOUNT                       
         LA    R2,LOGADD1H                                                      
         GOTO1 ADDRIN                  BUILD AN ADD AN ADDRESS ELEM             
*                                                                               
         BRAS  RE,EQUBLD           BUILD EQUIVALENT ACCOUNT POINTER             
         BNE   XIT                                                              
         MVC   KEY,HOLDKEY                                                      
*                                                                               
         GOTO1 STATIN                  ADDS STATUS ELEMENT TO RECORD            
         LA    R4,ELEMENT                                                       
         USING RSTELD,R4                                                        
*                                                                               
         LA    R2,LOGVEN1H                                                      
         CLC   KEY+1(2),=C'SV'         PROVISIONAL ACCS ONLY ON SV              
         BE    ACBLD04                                                          
*                                                                               
         CLI   LOGVEN1,X'00'           ERROR IF NOT SV AND INPUT                
         BE    AC200                                                            
         MVI   ERROR,INVALID                                                    
         B     XIT                                                              
*                                                                               
ACBLD04  CLI   LOGVEN1,X'00'           NOT PROVISIONAL                          
         BNE   *+12                                                             
         NI    RSTSTAT5,X'FF'-RSTSPROV                                          
         B     AC200                                                            
         MVI   ERROR,INVALID                                                    
         CLI   LOGVEN1,C'P'            P = PROVISIONAL ACCOUNT                  
         BNE   XIT                                                              
         MVI   ERROR,INVALID                                                    
         CLI   KEY+4,C' '              MUST BE AT LOW LEV FOR SV ACC            
         BE    XIT                                                              
         CLI   LOGACT,C'N'             ACTION NEW DON'T CHK FOR TRANS           
         BE    ACBLD05                                                          
*                                                                               
         BRAS  RE,CHKPROV                                                       
         BNE   XIT                                                              
*                                                                               
ACBLD05  OI    RSTSTAT5,RSTSPROV       SET PROVISIONAL                          
         MVI   RSTLN,RSTLN3Q           LENGTH TO COVER STAT 3                   
*                                                                               
*                                                                               
*              ACCOUNT FILTERS                                                  
*                                                                               
AC200    DS    0H                                                               
         BRAS  RE,SETFILT1         UPDATE FILTER 1                              
         BE    XIT                 INVALID FILTER EXIT                          
*                                                                               
AC201    DS    0H                                                               
         CLI   LOGFLT2H+5,0                                                     
         BNE   *+12                                                             
         MVI   RSTFILT2,X'40'          ACCOUNT FILTER #2                        
         B     AC202                                                            
         CLC   RSTFILT2,LOGFLT2        DID FILTER CHANGE                        
         BE    *+8                     NO                                       
         OI    LFMINDS,LFMIFLCH        FILTER HAS CHANGED                       
         CLI   LOGFLT2H+5,0                                                     
         BE    AC202                                                            
         LA    R2,LOGFLT2H                                                      
         MVC   RSTFILT2,LOGFLT2                                                 
         MVI   ERROR,INVALID                                                    
         CLI   LOGFLT2,C'.'                                                     
         BE    XIT                                                              
*&&US                                                                           
AC202    MVI   ERROR,INVALID                                                    
*&&                                                                             
*&&UK*&& CLC   KEY+1(2),=C'SF'     OR UK MEDLINE PAYEES                         
*&&UK*&& BE    AC20A                                                            
*                                                                               
AC2023   LA    R5,SAVECOMP                                                      
         USING CPYELD,R5                                                        
         CLC   KEY+1(2),CPYPROD                                                 
*&&US*&& B     AC20AA                                                           
*&&UK*&& BNE   AC20AA                                                           
         CLI   LOGANALH+5,0                                                     
         BE    AC20AA                                                           
         MVI   ERROR,INVALID                                                    
         LA    R2,LOGANALH                                                      
         B     XIT                                                              
*                                                                               
AC20AA   DS    0H                                                               
         CLI   LOGANALH+5,0                                                     
         BNE   *+12                                                             
         MVI   RSTFILT3,X'40'          ACCOUNT FILTER #3 / ANALYSIS FLT         
         B     AC20A                                                            
         CLC   RSTFILT3,LOGANAL        DID FILTER CHANGE                        
         BE    *+8                     NO                                       
         OI    LFMINDS,LFMIFLCH        FILTER HAS CHANGED                       
         LA    R2,LOGANALH                                                      
         MVI   ERROR,INVALID                                                    
         CLI   LOGANAL,C'.'                                                     
         BE    XIT                                                              
         MVC   RSTFILT3,LOGANAL                                                 
*                                                                               
AC20A    DS    0H                                                               
         CLI   LOGSUBH+5,0                                                      
         BNE   *+12                                                             
         MVI   RSTFILT4,X'40'          ACCOUNT FILTER #4                        
         B     AC20A1                                                           
         CLC   RSTFILT4,LOGSUB         DID FILTER CHANGE                        
         BE    *+8                     NO                                       
         OI    LFMINDS,LFMIFLCH        FILTER HAS CHANGED                       
         LA    R2,LOGSUBH                                                       
         MVI   ERROR,INVALID                                                    
         CLI   LOGSUB,C'.'                                                      
         BE    XIT                                                              
         MVC   RSTFILT4,LOGSUB                                                  
*                                                                               
AC20A1   DS    0H                                                               
         CLI   LOGFLT5H+5,0                                                     
         BNE   *+12                                                             
         MVI   RSTFILT5,X'40'          ACCOUNT FILTER #5                        
         B     AC20B                                                            
         CLC   RSTFILT5,LOGFLT5        DID FILTER CHANGE                        
         BE    *+8                     NO                                       
         OI    LFMINDS,LFMIFLCH        FILTER HAS CHANGED                       
         LA    R2,LOGFLT5H                                                      
         MVI   ERROR,INVALID                                                    
         CLI   LOGFLT5,C'.'                                                     
         BE    XIT                                                              
         MVC   RSTFILT5,LOGFLT5                                                 
*                                                                               
AC20B    XC    RSTSECY,RSTSECY         SECURITY FIELD                           
         LA    R2,LOGSECH                                                       
         CLI   5(R2),0                                                          
         BE    AC20C                                                            
         GOTO1 NUMERIC                                                          
         GOTO1 PACK                                                             
         STH   R1,RSTSECY                                                       
         CHI   R1,256                                                           
         BL    AC20C                                                            
         MVI   ERROR,ONETO255                                                   
         B     XIT                                                              
AC20C    CLC   KEY+1(2),=C'1R'     PERSONNEL LEDGER?                            
         BNE   AC20C2              NO - CONTINUE                                
         TM    LOGPROFH+4,X'20'    ANY CHANGE?                                  
         BNO   AC20C1              YES - CHECK FOR PERSON RECORDS               
         TM    LOGPRO2H+4,X'20'                                                 
         BNO   AC20C1                                                           
         TM    LOGPRO3H+4,X'20'                                                 
         BO    AC20C2              NO - CONTINUE ON                             
AC20C1   BRAS  RE,CHKOVER             IS THIS AN OVH ACCOUNT?                   
         CLI   ERROR,X'FE'                                                      
         BE    AC20C2              YES - SKIP CHECK FOR PERSON                  
         BRAS  RE,CHKPRSN                                                       
         LA    R2,LOGPROFH                                                      
         CLI   ERROR,COSTPRGM      ARE THERE PERSON RECORDS SETUP?              
         BE    XIT                 YES - NO CHANGE TO STATUS HERE               
*                                                                               
AC20C2   CLC   KEY+1(2),=C'SI'     INCOME?                                      
         BNE   AC20D                                                            
         USING RSTELD,R4                                                        
         XC    RSTSYSME,RSTSYSME                                                
         USING RSTELD,R4                                                        
*                                                                               
AC20D    GOTO1 REMANEL,DMCB,(X'30',0)  DELETE ELEMENT X'30'                     
         GOTO1 ADDANEL                 ADD ELEMENT X'30'                        
         MVI   ERROR,INVALID                                                    
         LA    R3,BLOCK2                                                        
         LA    R7,20               MAX NUMBER                                   
         SR    R5,R5                                                            
         LA    R2,LOGPROFH            R2= PROFILE FIELD HEADER                  
         BAS   R6,SCAN                                                          
         LA    R2,LOGPRO2H                                                      
         BAS   R6,SCAN                                                          
         LA    R2,LOGPRO3H                                                      
         BAS   R6,SCAN                                                          
         LA    R2,LOGPROFH                                                      
                                                                                
         LR    R6,R5                                                            
         LTR   R5,R5                                                            
         BZ    AC22X                                                            
         LA    R3,BLOCK2                                                        
         B     AC21                                                             
                                                                                
SCAN     CLI   5(R2),0                                                          
         BE    0(R6)                                                            
         GOTO1 VSCANNER,DMCB,('LSCNDIVF',(R2)),((R7),(R3)),0                    
*        CLI   4(R1),0                                                          
*        BE    AC22MEM                                                          
         XC    HALF,HALF                                                        
         MVC   HALF+1(1),4(R1)                                                  
         AH    R5,HALF                                                          
         SH    R7,HALF                                                          
         ST    R5,FULL                                                          
         MH    R5,=AL2(LSCNLINE)   NUMBER X LENGTH OF SCAN LINE                 
         LA    R3,BLOCK2(R5)       TO NEXT BLOCK AREA                           
         L     R5,FULL                                                          
         BR    R6                                                               
                                                                                
AC21     DS    0H                                                               
*        CLI   1(R3),0                                                          
*        BE    AC22MEM                                                          
         CLC   12(8,R3),=C'ANALYSIS'                                            
         BNE   AC21A                                                            
         MVI   RSTCOSTG,C' '                                                    
         CLC   22(2,R3),=C'NO'                                                  
         BE    AC22                                                             
         TM    DUPSW,DUPANL        TEST DUPLICATE INPUT                         
         BO    XIT                                                              
         OI    DUPSW,DUPANL                                                     
         CLI   1(R3),1             MORE THAN 1 DO IT THE NEW WAY                
         BH    AC22                                                             
         CLI   22(R3),C'('                                                      
         BE    AC211                                                            
         CLI   22(R3),C')'                                                      
         BE    AC211                                                            
         CLI   22(R3),C'A'                                                      
         BL    XIT                                                              
         CLI   22(R3),C'9'                                                      
         BH    XIT                                                              
AC211    MVC   RSTCOSTG,22(R3)                                                  
         B     AC22                                                             
                                                                                
AC21A    CLC   12(6,R3),=C'LOCKED'                                              
         BNE   AC21B                                                            
         CLI   22(R3),C'N'                                                      
         BNE   *+12                                                             
         NI    RSTSTAT1,X'DF'                                                   
         B     AC22                                                             
         CLI   22(R3),C'Y'                                                      
         BNE   XIT                                                              
         OI    RSTSTAT1,X'20'                                                   
         CLI   DRFTITMS,C'N'                                                    
         BE    AC22                                                             
         MVI   ERROR,LOCDRFT       CANNOT LOCK - DRAFT ITEMS ATTACHED           
         B     XIT                                                              
                                                                                
AC21B    CLC   12(6,R3),=C'CLOSED'                                              
         BNE   AC21B1                                                           
         CLI   22(R3),C'N'                                                      
         BNE   XIT                                                              
         NI    RSTSTAT1,X'BF'                                                   
         B     AC22                                                             
                                                                                
AC21B1   CLC   12(3,R3),=C'PAY'      IS PAYMENT ON HOLD?                        
         BNE   AC21C                                                            
         CLI   22(R3),C'Y'           TURN BIT OFF?                              
         BNE   *+12                                                             
         NI    RSTSTAT2,X'FF'-X'20'  YES                                        
         B     AC22                                                             
         CLI   22(R3),C'N'           TURN BIT ON?                               
         BNE   XIT                                                              
         OI    RSTSTAT2,X'20'        YES                                        
         B     AC22                                                             
                                                                                
AC21C    CLC   12(5,R3),=C'STAFF'    ANALYZE BY STAFF?                          
         BNE   AC21D                                                            
         CLI   22(R3),C'N'                                                      
         BNE   *+12                                                             
         NI    RSTSTAT1,X'7F'        NO                                         
         B     AC22                                                             
         CLI   22(R3),C'Y'                                                      
         BNE   XIT                                                              
         OI    RSTSTAT1,X'80'        YES                                        
         B     AC22                                                             
                                                                                
AC21D    CLC   12(4,R3),=C'DEPT'     ANALYZE BY DEPT?                           
         BNE   AC21E                                                            
         CLI   22(R3),C'N'                                                      
         BNE   *+12                                                             
         NI    RSTSTAT1,X'EF'        NO                                         
         B     AC22                                                             
         CLI   22(R3),C'Y'                                                      
         BNE   XIT                                                              
         OI    RSTSTAT1,X'10'        YES                                        
         B     AC22                                                             
                                                                                
AC21E    CLC   12(7,R3),=C'OUTFILE'   OUTFILE?                                  
         BNE   AC21F                                                            
         CLI   22(R3),C'N'                                                      
         BNE   *+12                                                             
         NI    RSTSTAT1,X'F7'         NO                                        
         B     AC22                                                             
         CLI   22(R3),C'Y'                                                      
         BNE   XIT                                                              
         OI    RSTSTAT1,X'08'         YES                                       
         B     AC22                                                             
                                                                                
AC21F    DS    0H                                                               
*&&US*&& CLC   12(4,R3),=C'EXEC'                                                
*&&UK*&& CLC   12(7,R3),=C'PLUSVAT'                                             
         BNE   AC21G                                                            
         CLI   22(R3),C'N'                                                      
         BNE   *+12                                                             
         NI    RSTSTAT1,X'FB'                                                   
         B     AC22                                                             
         CLI   22(R3),C'Y'                                                      
         BNE   XIT                                                              
         OI    RSTSTAT1,X'04'                                                   
         B     AC22                                                             
                                                                                
*&&US                                                                           
AC21G    CLC   12(6,R3),=C'VEND2C'     1099 VENDOR?                             
         BNE   AC21H                                                            
         CLI   22(R3),C'N'                                                      
         BNE   *+12                                                             
         NI    RSTSTAT1,X'FD'          NO                                       
         B     AC22                                                             
         CLI   22(R3),C'Y'                                                      
         BNE   XIT                                                              
         OI    RSTSTAT1,X'02'          YES                                      
         B     AC22                                                             
*&&                                                                             
*&&UK                                                                           
AC21G    CLC   KEY+1(2),=C'SG'                                                  
         BNE   AC21H               VAT ACCOUNTS ONLY.                           
         CLC   12(6,R3),=C'VATTYP'                                              
         BNE   AC21H                                                            
         NI    RSTSTAT1,X'FD'      SET TYPE TO OUTPUT.                          
         CLI   22(R3),C'O'                                                      
         BE    AC22                THAT'S RIGHT.                                
         CLI   22(R3),C'I'                                                      
         BNE   XIT                 BAD ENTRY.                                   
         OI    RSTSTAT1,X'02'      IT'S INPUT.                                  
         B     AC22                                                             
*&&                                                                             
                                                                                
AC21H    CLC   12(6,R3),=C'ACTUAL'                                              
         BNE   AC21I                                                            
         CLI   22(R3),C'N'                                                      
         BNE   *+12                                                             
         NI    RSTSTAT1,X'FE'                                                   
         B     AC22                                                             
         CLI   22(R3),C'Y'                                                      
         BNE   XIT                                                              
         OI    RSTSTAT1,X'01'                                                   
         B     AC22                                                             
                                                                                
AC21I    CLC   12(2,R3),=C'CC'                                                  
         BNE   AC21J                                                            
         CLI   0(R3),3             MAX 3 CHARS. 'CC' + DIGIT                    
         BH    XIT                                                              
         MVI   RSTCCTRR,0                                                       
         CLC   22(6,R3),=C'DELETE'                                              
         BNE   *+14                                                             
         MVC   RSTCCTR,SPACES                                                   
         B     AC22                                                             
         CLC   KEY+1(2),=C'SJ'     OPTION INVALID FOR SJ                        
         BE    XIT                                                              
         CLI   14(R3),X'40'                                                     
         BE    *+30                                                             
         CLI   14(R3),C'1'         MUST BE 1-9                                  
         BL    XIT                                                              
         CLI   14(R3),C'9'                                                      
         BH    XIT                                                              
         MVC   RSTCCTRR,14(R3)                                                  
         NI    RSTCCTRR,X'0F'      MAKE IT BINARY                               
         CLI   1(R3),3             MAX 3 CHARS TO BE CHANGED                    
         BH    XIT                                                              
         MVC   RSTCCTR,22(R3)                                                   
         B     AC22                                                             
                                                                                
AC21J    CLC   12(6,R3),=C'VEND29'                                              
         BNE   AC21K                                                            
         CLI   22(R3),C'N'                                                      
         BNE   *+12                                                             
         NI    RSTSTAT3,X'7F'                                                   
         B     AC22                                                             
         CLI   22(R3),C'Y'                                                      
         BNE   XIT                                                              
         OI    RSTSTAT3,X'80'                                                   
         B     AC22                                                             
                                                                                
AC21K    CLC   KEY+1(2),=C'SC'                                                  
         BNE   AC21M                                                            
         CLC   12(5,R3),=C'RECCR'                                               
         BNE   AC21L                                                            
         CLI   22(R3),C'N'                                                      
         BNE   *+12                                                             
         NI    RSTSTAT3,X'BF'                                                   
         B     AC22                                                             
         CLI   22(R3),C'Y'                                                      
         BNE   XIT                                                              
         OI    RSTSTAT3,X'40'                                                   
         B     AC22                                                             
                                                                                
AC21L    CLC   12(5,R3),=C'RECDR'                                               
         BNE   AC21M                                                            
         CLI   22(R3),C'N'                                                      
         BNE   *+12                                                             
         NI    RSTSTAT3,X'DF'                                                   
         B     AC22                                                             
         CLI   22(R3),C'Y'                                                      
         BNE   XIT                                                              
         OI    RSTSTAT3,X'20'                                                   
         B     AC22                                                             
                                                                                
AC21M    CLC   12(2,R3),=C'PC'                                                  
         BNE   AC21N                                                            
         CLI   22(R3),C'N'                                                      
         BNE   *+12                                                             
         NI    RSTSTAT3,X'EF'                                                   
         B     AC22                                                             
         CLI   22(R3),C'Y'                                                      
         BNE   XIT                                                              
         OI    RSTSTAT3,X'10'                                                   
         B     AC22                                                             
*                                                                               
AC21N    CLC   12(3,R3),=C'IND'                                                 
         BNE   AC21O                                                            
         CLI   22(R3),C'N'                                                      
         BNE   *+12                                                             
         NI    RSTSTAT3,X'F7'                                                   
         B     AC22                                                             
         CLI   22(R3),C'Y'                                                      
         BNE   XIT                                                              
         OI    RSTSTAT3,X'08'                                                   
         B     AC22                                                             
*                                                                               
AC21O    CLC   12(5,R3),=C'CKDUP'                                               
         BNE   AC21P                                                            
         CLI   22(R3),C'Y'                                                      
         BNE   *+12                                                             
         NI    RSTSTAT2,X'FF'-X'08'                                             
         B     AC22                                                             
         CLI   22(R3),C'N'                                                      
         BNE   XIT                                                              
         OI    RSTSTAT2,X'08'       DISABLE DUPLICATE CHECKING                  
         B     AC22                 NO                                          
*                                                                               
AC21P    CLC   12(3,R3),=C'P/L'     P/L?                                        
         BNE   AC21R                                                            
         CLI   22(R3),C'N'                                                      
         BNE   *+12                                                             
         NI    RSTSTAT3,X'FB'       NO                                          
         B     AC22                                                             
         CLI   22(R3),C'Y'                                                      
         BNE   XIT                                                              
         TM    RSTSTAT3,X'02'       P/L AND BAL CANNOT BE ON AT 1 TIME          
         BO    XIT                                                              
         OI    RSTSTAT3,X'04'       YES                                         
         B     AC22                                                             
                                                                                
AC21R    CLC   12(3,R3),=C'BAL'     B/S?                                        
         BNE   AC21S                                                            
         CLI   22(R3),C'N'                                                      
         BNE   *+12                                                             
         NI    RSTSTAT3,X'FD'       NO                                          
         B     AC22                                                             
         CLI   22(R3),C'Y'                                                      
         BNE   XIT                                                              
         TM    RSTSTAT3,X'04'       P/L AND BAL CANNOT BE ON AT 1 TIME          
         BO    XIT                                                              
         OI    RSTSTAT3,X'02'       YES                                         
         B     AC22                                                             
                                                                                
AC21S    CLC   12(5,R3),=C'JCOST'                                               
         BNE   AC21T                                                            
         CLI   22(R3),C'N'                                                      
         BNE   *+12                                                             
         NI    RSTSTAT3,X'FE'                                                   
         B     AC22                                                             
         CLI   22(R3),C'Y'                                                      
         BNE   XIT                                                              
         OI    RSTSTAT3,X'01'                                                   
         B     AC22                                                             
                                                                                
AC21T    CLC   12(5,R3),=C'GLOFF'                                               
         BNE   AC21U                                                            
         CLC   22(3,R3),=C'DEL'    DELETE OFFICE                                
         BNE   *+14                                                             
         XC    RSTOFFC,RSTOFFC                                                  
         B     AC22                                                             
         CLI   1(R3),2                                                          
         BH    XIT                 NEVER MORE THAN 2                            
         TM    COMPSTA4,X'01'      NEW OFFICES                                  
         BO    *+12                                                             
         CLI   1(R3),1                                                          
         BH    XIT                 IF OLD OFFICES NOT MORE THAN 1               
         MVC   RSTOFFC,22(R3)                                                   
         B     AC22                                                             
                                                                                
AC21U    CLC   KEY+1(2),=C'SE'                                                  
         BNE   AC21V                                                            
         CLC   12(4,R3),=C'COST'                                                
         BNE   AC21V                                                            
         TM    DUPSW,DUPCST        TEST DUPLICATE INPUT                         
         BO    XIT                                                              
         OI    DUPSW,DUPCST                                                     
         NI    RSTSTAT2,X'FF'-X'03'                                             
         CLI   1(R3),6                                                          
         BNE   *+14                                                             
         CLC   22(6,R3),=C'DELETE'                                              
         BE    AC22                                                             
         CLI   22(R3),C'N'                                                      
         BNE   *+12                                                             
         OI    RSTSTAT2,X'01'                                                   
         B     AC22                                                             
         CLI   22(R3),C'Y'                                                      
         BNE   XIT                                                              
         OI    RSTSTAT2,X'02'                                                   
         B     AC22                                                             
                                                                                
AC21V    CLC   KEY+1(2),=C'1R'     DEFAULT TASK CODE FOR TMS                    
         BNE   AC21VA                                                           
         CLC   12(4,R3),=C'TASK'                                                
         BNE   AC21VA                                                           
         MVI   RSTLN,RSTLN3Q                                                    
         TM    DUPSW,DUPTASK       TEST DUPLICATE INPUT                         
         BO    XIT                                                              
         OI    DUPSW,DUPTASK                                                    
         MVC   RSTDFTSK,SPACES                                                  
         CLC   22(3,R3),=C'DEL'                                                 
         BE    AC22                                                             
         CLI   1(R3),2             MUST BE 2 CHARS                              
         BNE   XIT                                                              
         CLI   22(R3),C' '         MUST BE 2 CHARS                              
         BE    XIT                                                              
         MVC   RSTDFTSK,22(R3)                                                  
         B     AC22                                                             
*                                                                               
AC21VA   CLC   12(4,R3),=C'MAIL'                                                
         BNE   AC21W                                                            
         MVI   ELCODE,X'32'                                                     
         LA    R7,IO2                                                           
         BRAS  RE,GETEL                                                         
         BNE   XIT                                                              
         TM    DUPSW,DUPMAIL                                                    
         BO    XIT                                                              
         OI    DUPSW,DUPMAIL                                                    
         MVI   RSTLN,RSTLN3Q                                                    
         CLC   22(3,R3),=C'DEL'    DELETE MAIL SETTING                          
         BNE   *+14                                                             
         XC    RSTMAIL,RSTMAIL                                                  
         B     AC22                                                             
         CLC   22(2,R3),=C'OV'     OVERNIGHT                                    
         BNE   XIT                                                              
         OI    RSTMAIL,RSTMAIOV                                                 
         B     AC22                                                             
*                                                                               
*                                                                               
AC21W    CLC   12(4,R3),=C'1099'                                                
         BNE   AC21X                                                            
         TM    DUPSW,DUP1099                                                    
         BO    XIT                                                              
         OI    DUPSW,DUP1099                                                    
         MVI   RSTLN,RSTLN3Q                                                    
         CLC   KEY+1(2),=C'SV'                                                  
         BE    AC21WA                                                           
         CLC   KEY+1(2),=C'SW'                                                  
         BE    AC21WA                                                           
         CLC   KEY+1(2),=C'SX'                                                  
         BE    AC21WA                                                           
         CLC   KEY+1(2),=C'SY'                                                  
         BE    AC21WA                                                           
         CLC   KEY+1(2),=C'2C'                                                  
         BNE   XIT                                                              
AC21WA   CLC   22(3,R3),=C'RNT'                                                 
         BNE   AC21WB                                                           
         MVI   RSTX1099,RSTXRENT                                                
         B     AC22                                                             
AC21WB   CLC   22(3,R3),=C'ROY'                                                 
         BNE   AC21WC                                                           
         MVI   RSTX1099,RSTXROYL                                                
         B     AC22                                                             
AC21WC   CLC   22(3,R3),=C'PRZ'                                                 
         BNE   AC21WD                                                           
         MVI   RSTX1099,RSTXPRIZ                                                
         B     AC22                                                             
AC21WD   CLC   22(3,R3),=C'MED'                                                 
         BNE   AC21WE                                                           
         MVI   RSTX1099,RSTXMEDC                                                
         B     AC22                                                             
AC21WE   CLI   22(R3),C'N'                                                      
         BNE   XIT                                                              
         MVI   RSTX1099,RSTXNCMP                                                
         B     AC22                                                             
                                                                                
AC21X    CLC   12(6,R3),=C'NEWBIZ'                                              
         BNE   AC21Y                                                            
         CLC   KEY+1(2),=C'1C'                                                  
         BNE   XIT                                                              
         CLI   22(R3),C'N'                                                      
         BNE   *+12                                                             
         NI    RSTSTAT5,X'FF'-RSTSNBIZ                                          
         B     AC22                                                             
         CLI   22(R3),C'Y'                                                      
         BNE   XIT                                                              
         OI    RSTSTAT5,RSTSNBIZ                                                
         B     AC22                                                             
                                                                                
AC21Y    CLC   12(7,R3),=C'PROBONO'                                             
         BNE   AC21YA                                                           
         CLC   KEY+1(2),=C'1C'                                                  
         BNE   XIT                                                              
         CLI   22(R3),C'N'                                                      
         BNE   *+12                                                             
         NI    RSTSTAT5,X'FF'-RSTSBONO                                          
         B     AC22                                                             
         CLI   22(R3),C'Y'                                                      
         BNE   XIT                                                              
         OI    RSTSTAT5,RSTSBONO                                                
         B     AC22                                                             
                                                                                
AC21YA   CLC   12(5,R3),=C'HOUSE'                                               
         BNE   AC21Z                                                            
         CLC   KEY+1(2),=C'1C'                                                  
         BNE   XIT                                                              
         CLI   22(R3),C'N'                                                      
         BNE   *+12                                                             
         NI    RSTSTAT5,X'FF'-RSTSHOUS                                          
         B     AC22                                                             
         CLI   22(R3),C'Y'                                                      
         BNE   XIT                                                              
         OI    RSTSTAT5,RSTSHOUS                                                
         B     AC22                                                             
                                                                                
AC21Z    CLC   12(3,R3),=C'T/S'                                                 
         BNE   AC21ZD                                                           
         CLC   KEY+1(2),=C'1C'                                                  
         BE    AC21ZA                                                           
         CLC   KEY+1(2),=C'1R'                                                  
         BNE   XIT                                                              
AC21ZA   TM    DUPSW,DUPTIME                                                    
         BO    XIT                                                              
         OI    DUPSW,DUPTIME                                                    
         CLI   22(R3),C'N'                                                      
         BNE   AC21ZB                                                           
         NI    RSTSTAT5,X'FF'-RSTSPROD-RSTSPRJB                                 
         B     AC22                                                             
AC21ZB   CLI   22(R3),C'P'                                                      
         BNE   AC21ZC                                                           
         NI    RSTSTAT5,X'FF'-RSTSPRJB                                          
         OI    RSTSTAT5,RSTSPROD                                                
         B     AC22                                                             
AC21ZC   CLI   22(R3),C'J'                                                      
         BNE   XIT                                                              
         NI    RSTSTAT5,X'FF'-RSTSPROD                                          
         OI    RSTSTAT5,RSTSPRJB                                                
         B     AC22                                                             
                                                                                
         USING RSTELD,R4                                                        
AC21ZD   CLC   KEY+1(2),=C'SI'                                                  
         BNE   AC22                                                             
         CLC   12(3,R3),=C'SMC'    SYSTEM MEDIA                                 
         BNE   AC22                                                             
         MVC   RSTSYSME,22(R3)                                                  
         B     AC22                                                             
                                                                                
AC22     LA    R3,LSCNLINE(,R3)                                                 
         BCT   R5,AC21                                                          
         LR    R5,R6                                                            
         LA    R3,BLOCK2                                                        
         GOTO1 REMANEL,DMCB,(X'30',0)                                           
         GOTO1 ADDANEL                                                          
         LR    R5,R6                                                            
         LA    R3,BLOCK2                                                        
*                                                                               
*                                                                               
* CAN ENTER ILIST=AAA FOR ALL METHODS                                           
* OR ILISTN=AAA FOR METHOD N ONLY                                               
*                                                                               
         NI    BIT,X'FF'-DUPPROF                                                
AC221    XC    BYTE,BYTE           TO HOLD METHOD                               
         CLC   12(5,R3),=C'ILIST'                                               
         BNE   AC229                                                            
         CLC   KEY+1(2),=C'1N'       ONLY FOR 1N LEDGER                         
         BNE   ERRLEDG                                                          
         CLI   1(R3),3             ONLY ACCEPTS 3 CHARS OR LESS                 
         BH    ERRMNLST                                                         
         CLI   0(R3),5             ENTER 'ILIST' MEANS FOR ALL MTHDS            
         BE    AC222               'ILIST1' MEANS FOR MTH 1 ONLY                
         MVC   BYTE,17(R3)                                                      
         BRAS  RE,CHKMETH          VALIDATE METHOD                              
         BNE   ERRMETH                                                          
AC222    CLC   22(2,R3),=C'NO'     LOOKING TO DELETE                            
         BE    AC222A              NO NEED TO VALIDATE LIST                     
         BRAS  RE,CHKCLIST             VALIDATE THE CLIENT LIST                 
         BNE   ERRLSTCD                                                         
*                                                                               
AC222A   TM    BIT,DUPPROF         ALREADY DELETED ELEMENTS                     
         BO    AC222B                                                           
         GOTO1 REMANEL,DMCB,('FFTELQ',0)                                        
         OI    BIT,DUPPROF                                                      
*                                                                               
         USING FFTELD,R7                                                        
AC222B   LA    R7,IO2              CHECK FOR ELEMENT                            
         AH    R7,DATADISP                                                      
         SR    R1,R1                                                            
AC223    CLI   0(R7),0                                                          
         BE    AC225                                                            
         CLI   0(R7),FFTELQ        X'DB'                                        
         BNE   AC224                                                            
         CLI   2(R7),FFTTINDL      INDIR CLIST                                  
         BNE   AC224                                                            
         CLC   BYTE,FFTDATA        SAME METHOD                                  
         BE    ERRDUP              DUPLICATE                                    
AC224    IC    R1,1(R7)                                                         
         AR    R7,R1                                                            
         B     AC223                                                            
*                                                                               
AC225    CLC   22(2,R3),=C'NO'                                                  
         BE    AC229                                                            
         DROP  R7                                                               
*                                                                               
         USING FFTELD,R4                                                        
         LA    R4,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   FFTEL,FFTELQ        'DB'                                         
         LA    RE,FFTLN1Q          ELEM OVERHED                                 
         LA    RF,L'FFTDLEN        ACTUAL LEN OF TEXT                           
         AR    RE,RF                                                            
         LA    RF,7                1 FOR MTHD+5 FOR LST CDE+1 FOR               
         AR    RE,RF               STATUS BIT                                   
         STC   RE,FFTLN                                                         
         MVI   FFTTYPE,FFTTINDL    INDIR CLIENT LIST EQUATE                     
         MVI   FFTDLEN,7                                                        
         MVC   FFTDATA(1),BYTE                                                  
         MVC   FFTDATA+1(3),22(R3) ONLY ALLOWED 3 BYTES FOR NOW                 
         OC    FFTDATA+1(5),SPACES                                              
         XC    FFTDATA+6(1),FFTDATA+6    BIT FOR FUTURE USE                     
         GOTO1 ADDANEL                                                          
AC229    LA    R3,LSCNLINE(,R3)                                                 
         BCT   R5,AC221                                                         
         LR    R5,R6                                                            
         LA    R3,BLOCK2                                                        
*                                                                               
         LA    R4,ELEMENT                                                       
         USING RATELD,R4                                                        
         MVI   RATLN,X'04'                                                      
                                                                                
AC22A    CLC   12(4,R3),=CL4'VAT'                                               
         BNE   AC22B                                                            
         MVI   RATEL,X'36'                                                      
         B     AC22C                                                            
                                                                                
AC22B    CLC   12(8,R3),=C'DISCOUNT'                                            
         BNE   AC22D                                                            
         MVI   RATEL,X'38'                                                      
                                                                                
AC22C    GOTO1 REMANEL,DMCB,(RATEL,0)                                           
         CLI   22(R3),C'N'                                                      
         BE    AC22D                                                            
         XC    RATRATE,RATRATE                                                  
         CLC   22(4,R3),=C'ZERO'                                                
         BE    AC22CA                                                           
         SR    RF,RF                                                            
         IC    RF,1(R3)                                                         
         ST    RF,DMCB+4                                                        
         GOTO1 CASHVAL,DMCB,22(R3)                                              
         CLI   DMCB,X'FF'                                                       
         BE    XIT                                                              
         MVC   RATRATE,DMCB+6                                                   
AC22CA   GOTO1 ADDANEL                                                          
                                                                                
AC22D    LA    R3,LSCNLINE(,R3)                                                 
         BCT   R5,AC22A                                                         
         EJECT ,                                                                
*              NOW SCAN FOR NUMBER AND PROFILE                                  
         LR    R5,R6                                                            
         LA    R3,BLOCK2                                                        
         LA    R4,ELEMENT                                                       
         USING OTHELD,R4                                                        
         MVC   OTHEL(2),=X'230F'                                                
         MVC   OTHNUM(13),SPACES                                                
         MVI   ELCODE,X'23'                                                     
         LA    R7,IO2                                                           
         BRAS  RE,GETEL                                                         
         BNE   AC22E                                                            
         MVC   ELEMENT(15),0(R7)                                                
                                                                                
AC22E    CLC   12(6,R3),=C'NUMBER'                                              
         BE    AC22EA                                                           
         CLC   12(2,R3),=C'ID'                                                  
         BNE   *+12                                                             
         MVI   OTHPROF,C'I'                                                     
         B     AC22EA                                                           
         CLC   KEY+1(2),=C'1R'                                                  
         BNE   AC22F                                                            
         CLC   12(7,R3),=C'BENEFIT'                                             
         BNE   AC22F                                                            
AC22EA   MVC   OTHNUM,SPACES                                                    
         CLI   1(R3),1                                                          
         BH    *+12                                                             
         CLI   22(R3),C'N'                                                      
         BE    AC22G                                                            
         MVC   OTHNUM,22(R3)                                                    
         B     AC22G                                                            
                                                                                
AC22F    CLC   12(7,R3),=C'PROFILE'                                             
         BNE   AC22G                                                            
         MVC   OTHPROF,SPACES                                                   
         CLI   1(R3),1                                                          
         BH    *+12                                                             
         CLI   22(R3),C'N'                                                      
         BE    AC22G                                                            
         MVC   OTHPROF,22(R3)                                                   
                                                                                
AC22G    LA    R3,LSCNLINE(,R3)                                                 
         BCT   R5,AC22E                                                         
         GOTO1 REMANEL,DMCB,(X'23',0)                                           
         CLC   OTHNUM(13),SPACES                                                
         BE    AC22H                                                            
         GOTO1 ADDANEL                                                          
                                                                                
AC22H    DS    0H                                                               
         LR    R5,R6                                                            
         LA    R3,BLOCK2                                                        
         LA    R4,ELEMENT                                                       
         USING HOMELD,R4                                                        
         MVC   HOMEL(2),=X'6408'                                                
         ZAP   HOMENO,=P'0'                                                     
         MVI   ELCODE,X'64'                                                     
         LA    R7,IO2                                                           
         BRAS  RE,GETEL                                                         
         BNE   AC22J                                                            
         MVC   ELEMENT(8),0(R7)                                                 
                                                                                
AC22J    CLC   12(5,R3),=C'HOMES'                                               
         BNE   AC22K                                                            
         ZAP   HOMENO,=P'0'                                                     
         CLI   1(R3),1                                                          
         BNE   *+12                                                             
         CLI   22(R3),C'N'                                                      
         BE    AC22K                                                            
         TM    3(R3),X'80'         NUMERIC                                      
         BZ    XIT                                                              
         MVC   FULL,8(R3)                                                       
         L     RF,FULL                                                          
         CVD   RF,DUB                                                           
         ZAP   HOMENO,DUB                                                       
                                                                                
AC22K    LA    R3,LSCNLINE(,R3)                                                 
         BCT   R5,AC22J                                                         
         GOTO1 REMANEL,DMCB,(X'64',0)                                           
         CP    HOMENO,=P'0'        AMEND TO ZERO DELETES ELEMENT                
         BE    AC22L                                                            
         GOTO1 ADDANEL                                                          
                                                                                
AC22L    CLI   KEY+1,C'F'          FOR UNIT F DON'T HANDLE GENERAL              
         BE    AC22P                                                            
         LR    R5,R6               NOW SCAN FOR GENERAL LEDGER                  
         LA    R3,BLOCK2                                                        
         LA    R4,ELEMENT                                                       
         USING GLPELD,R4                                                        
         MVC   GLPEL(2),=X'151A'                                                
                                                                                
AC22M    CLC   12(7,R3),=C'GENERAL'                                             
         BE    AC22N                                                            
         LA    R3,LSCNLINE(,R3)                                                 
         BCT   R5,AC22M                                                         
         B     AC22P                                                            
                                                                                
AC22N    GOTO1 REMANEL,DMCB,(X'15',0)                                           
         CLC   22(2,R3),=C'NO'                                                  
         BE    AC22P                                                            
         CLI   KEY,X'AB'           DONT EDIT GL ACCOUNT FOR H AND K             
         BE    AC22N5                                                           
         CLI   22(R3),C'G'         HAS TO BE A GENERAL LEDGER ACCOUNT           
         BNE   AC22N1                                                           
         CLI   23(R3),C'B'                                                      
         BE    AC22N5                                                           
         CLI   23(R3),C'P'                                                      
         BE    AC22N5                                                           
*                                                                               
AC22N1   MVI   ERROR,ACCINVAL                                                   
         B     XIT                                                              
*                                                                               
AC22N5   MVC   GLPSUB,22(R3)                                                    
         MVC   GLPACC1,22(R3)                                                   
*                                                                               
         CLI   KEY,X'AB'           DONT EDIT GL ACCOUNT FOR H AND K             
         BE    AC22NG                                                           
*                                                                               
         MVC   KEY,SPACES          CHECK A/C EXISTS                             
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(14),22(R3)                                                 
         LA    RE,12                                                            
         LA    RF,KEY+3                                                         
AC22NA   CLI   0(RF),C'*'          LOOK FOR AN ASTERISK                         
         BE    AC22NC                                                           
         LA    RF,1(RF)                                                         
         BCT   RE,AC22NA                                                        
         B     AC22NE                                                           
AC22NC   MVI   0(RF),C' '          BLANK IT OUT                                 
         LA    RE,KEY                                                           
         SR    RF,RE                                                            
         BCTR  RF,0                                                             
         STC   RF,BYTE                                                          
         GOTO1 HIGH                                                             
         ZIC   RF,BYTE                                                          
         CLC   KEY(0),KEYSAVE                                                   
         EX    RF,*-6                                                           
         BNE   XIT                                                              
*        EX    RF,*+8                                                           
*        B     *+10                SEE IF WE CAN MATCH ON WHAT IS LEFT          
*        CLC   KEY(0),KEYSAVE                                                   
*        BNE   XIT                                                              
         B     AC22NG                                                           
AC22NE   GOTO1 READ                                                             
         MVI   ERROR,ACCINVAL                                                   
         MVI   ELCODE,ABLELQ       X'32' - GENRL ACCT MUST BE LOW LEVEL         
         LA    R7,IO                                                            
         BRAS  RE,GETEL                                                         
         BNE   XIT                                                              
AC22NG   GOTO1 ADDANEL                                                          
         MVC   KEY,HOLDKEY                                                      
                                                                                
AC22P    DS    0H                                                               
         LR    R5,R6               SCAN FOR COMMENTS                            
         LA    R3,BLOCK2                                                        
         LA    R4,ELEMENT                                                       
         USING SCMELD,R4                                                        
         MVI   BYTE,1                                                           
         MVC   SCMEL(2),=X'3E0A'                                                
                                                                                
AC22Q    MVI   SCMTYPE,X'44'                                                    
         CLC   12(3,R3),=C'EST'                                                 
         BE    AC22S                                                            
         MVI   SCMTYPE,X'84'                                                    
         CLC   12(3,R3),=C'BIL'                                                 
         BE    AC22S                                                            
         OI    SCMTYPE,X'40'                                                    
         CLC   12(3,R3),=C'B+E'                                                 
         BE    AC22S                                                            
AC22R    LA    R3,LSCNLINE(,R3)                                                 
         BCT   R5,AC22Q                                                         
         B     AC22U                                                            
                                                                                
AC22S    CLI   BYTE,1                                                           
         BNE   AC22T                                                            
         LA    R7,IO2                                                           
         AH    R7,DATADISP         REMOVE ALL STANDARD COMMENT ELEMENTS         
         SR    R1,R1                                                            
AC22SA   CLI   0(R7),0                                                          
         BE    AC22SE                                                           
         CLI   0(R7),SCMELQ        X'3E' - STANDARD COMMENTS ELEMENT            
         BNE   AC22SC                                                           
         CLI   3(R7),0                                                          
         BE    AC22SC                                                           
         MVI   0(R7),X'FF'                                                      
AC22SC   IC    R1,1(R7)                                                         
         AR    R7,R1                                                            
         B     AC22SA                                                           
                                                                                
AC22SE   GOTO1 =V(ACDELEL),DMCB,IO2,RR=RB                                       
                                                                                
AC22T    MVC   SCMSEQ,BYTE                                                      
         ZIC   RF,BYTE                                                          
         LA    RF,1(RF)                                                         
         STC   RF,BYTE                                                          
         CLC   22(6,R3),=CL6'N'    N REMOVES ELEMENT                            
         BE    AC22R                                                            
         MVC   SCMNARR(6),SPACES                                                
         IC    RF,1(R3)                                                         
         LTR   RF,RF                                                            
         BZ    AC22R                                                            
         LA    RE,6                                                             
         SR    RE,RF                                                            
         LA    RE,SCMNARR(RE)                                                   
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,RE),22(R3)                                                   
*                                                                               
         XC    KEY,KEY             SEE IF COMMENT EXISTS                        
         MVI   KEY,X'0C'                                                        
         MVC   KEY+1(1),COMPANY                                                 
         MVC   KEY+2(6),SCMNARR                                                 
         GOTO1 READ                                                             
         GOTO1 ADDANEL                                                          
         MVC   KEY,HOLDKEY         RESTORE KEY                                  
         B     AC22R                                                            
*                                                                               
AC22U    DS    0H                                                               
         LR    R5,R6               SCAN FOR EXTRA NUMBER                        
         LA    R3,BLOCK2                                                        
         LA    R4,ELEMENT                                                       
AC22UA   CLC   12(4,R3),=C'NUM2'                                                
         BE    AC22V                                                            
         CLC   KEY+1(2),=C'1R'                                                  
         BNE   AC22UX                                                           
         CLC   12(5,R3),=C'ADMIN'                                               
         BE    AC22V                                                            
AC22UX   LA    R3,LSCNLINE(,R3)                                                 
         BCT   R5,AC22UA                                                        
         B     AC22UY                                                           
                                                                                
         USING FFNELD,R4                                                        
AC22V    MVI   FFNEL,FFNELQ       X'25'                                         
         MVC   FFNUMBER,SPACES                                                  
         CLI   1(R3),1                                                          
         BH    *+12                                                             
         CLI   22(R3),C'N'         N REMOVES ELEMENT                            
         BE    AC22W                                                            
         ZIC   RF,1(R3)                                                         
         LA    RF,2(,RF)                                                        
         STC   RF,FFNLN                                                         
         MVC   FFNUMBER,22(R3)                                                  
                                                                                
AC22W    GOTO1 REMANEL,DMCB,(X'25',0)                                           
         CLC   FFNUMBER,SPACES                                                  
         BE    AC22UY                                                           
         GOTO1 ADDANEL                                                          
                                                                                
*        BUILD 27 ELEMENT IN HOLD AREA                                          
         USING ABIELD,R4                                                        
AC22UY   LR    R5,R6                                                            
         LA    R3,BLOCK2                                                        
         LA    R4,MYELEM                                                        
         MVI   ERROR,INVALID                                                    
         MVI   MYELEM,C' '        FIRST CLEAR 27 EL HOLD AREA                   
         MVC   MYELEM+1(L'MYELEM-1),MYELEM                                      
AC22UY1  CLC   12(2,R3),=C'EA'     EA NUMBER                                    
         BNE   AC22UYA                                                          
         MVC   ABIEANO,SPACES                                                   
         CLI   27(R3),C'-'         CHECK STRUCTURE                              
         BNE   AC22UY2             MIGHT BE 6-2-1                               
         CLI   30(R3),C'-'         IS IT 5-2-1                                  
         BNE   XIT                 NO                                           
         ZIC   RF,1(R3)            LENGTH FROM SCANNER                          
         CHI   RF,10               5-2-1 IS 10 POSITIONS                        
         BNE   XIT                                                              
         B     AC22UY3             INPUT IS OK                                  
AC22UY2  CLI   28(R3),C'-'         TRY FOR 6-2-1                                
         BNE   XIT                                                              
         CLI   31(R3),C'-'                                                      
         BNE   XIT                                                              
         ZIC   RF,1(R3)                                                         
         CHI   RF,11               6-2-1 IS 11 POSITIONS                        
         BNE   XIT                                                              
AC22UY3  BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   ABIEANO(0),22(R3)                                                
         B     AC22UYB                                                          
AC22UYA  LA    R3,LSCNLINE(,R3)                                                 
         BCT   R5,AC22UY1                                                       
                                                                                
AC22UYB  LR    R5,R6               CHECK FOR ACCT NO                            
         LA    R3,BLOCK2                                                        
         LA    R4,MYELEM                                                        
         MVI   ERROR,INVALID                                                    
AC22UYB1 CLC   12(2,R3),=C'AC'                                                  
         BNE   AC22UYC                                                          
         MVC   ABIACNO,SPACES                                                   
         ZIC   RF,1(R3)                                                         
         LTR   RF,RF               IS THIS A CLEAR REQUEST ?                    
         BZ    AC22VA              YES, BRANCH TO NEXT CHECK                    
         CHI   RF,13                                                            
         BL    TRY3                                                             
         CLI   26(R3),C'-'         STRUCTURE AND NUMERICS                       
         BNE   XIT                                                              
         CLI   31(R3),C'-'         SCHEME 1 (XXXX-XXXX-XXX)                     
         BNE   SCHEME2                                                          
         MVC   WORK(11),=15X'F0'                                                
         MVZ   WORK(4),22(R3)                                                   
         MVZ   WORK+4(4),27(R3)                                                 
         MVZ   WORK+8(3),32(R3)                                                 
         CLC   WORK(11),=15X'F0'                                                
         BNE   XIT                                                              
         B     ACSIZE                                                           
SCHEME2  CLI   30(R3),C'-'         SCHEME 2 (XXXX-XXX-XXXX)                     
         BNE   ACSIZE                                                           
         MVC   WORK(11),=15X'F0'                                                
         MVZ   WORK(4),22(R3)                                                   
         MVZ   WORK+4(3),27(R3)                                                 
         MVZ   WORK+7(4),31(R3)                                                 
         CLC   WORK(11),=15X'F0'                                                
         BNE   XIT                                                              
         B     ACSIZE                                                           
TRY3     CHI   RF,4                                                             
         BE    SCHEME3                                                          
         CHI   RF,5                                                             
         BNE   XIT                                                              
SCHEME3  MVC   WORK(4),=15X'F0'    SCHEME3 FOUR NUMERICS                        
         MVZ   WORK(4),22(R3)                                                   
         CLC   WORK(4),=15X'F0'                                                 
         BE    ACSIZE                                                           
         CLI   22(R3),C'P'         OR A P FOLLOWED BY FOUR NUMERICS             
         BNE   XIT                                                              
         MVC   WORK(4),=15X'F0'                                                 
         MVZ   WORK(4),23(R3)                                                   
         CLC   WORK(4),=15X'F0'                                                 
         BNE   XIT                                                              
ACSIZE   ZIC   RF,1(R3)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   ABIACNO(0),22(R3)                                                
         B     AC22VA                                                           
AC22UYC  LA    R3,LSCNLINE(,R3)                                                 
         BCT   R5,AC22UYB1                                                      
         B     AC22VA                                                           
                                                                                
AC22VA   DS    0H                                                               
                                                                                
         LR    R5,R6                                                            
         LA    R3,BLOCK2                                                        
         LA    R4,MYELEM                                                        
AC22VB   CLC   =C'ENO',12(R3)                                                   
         BNE   AC22VB1                                                          
         MVC   ABIESNO,SPACES     CLEAR ENO                                     
         ZIC   RF,1(R3)                                                         
         CHI   RF,12                                                            
         BH    XIT                                                              
         LTR   RF,RF               IS THIS A CLEAR REQUEST ?                    
         BZ    AC22VB2             YES, BRANCH TO NEXT CHECK                    
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   ABIESNO(0),22(R3)                                                
         B     AC22VB2                                                          
AC22VB1  LA    R3,LSCNLINE(,R3)                                                 
         BCT   R5,AC22VB                                                        
                                                                                
AC22VB2  LR    R5,R6                                                            
         LA    R3,BLOCK2                                                        
         LA    R4,MYELEM                                                        
         MVI   ERROR,INVALID                                                    
AC22VB3  CLC   =C'BUD',12(R3)                                                   
         BNE   AC22VB4                                                          
         MVC   ABIBUNO,SPACES     CLEAR BUD                                     
         ZIC   RF,1(R3)                                                         
         CHI   RF,15                                                            
         BH    XIT                                                              
         LTR   RF,RF               IS THIS A CLEAR REQUEST ?                    
         BZ    AC22VB5             YES, BRANCH TO NEXT CHECK                    
         BCTR  RF,0                                                             
         EX    RF,INST1                                                         
         EX    RF,INST2                                                         
         EX    RF,INST3                                                         
         B     INST4                                                            
INST1    MVC   WORK(0),=15X'F0'                                                 
INST2    MVZ   WORK(0),22(R3)                                                   
INST3    CLC   WORK(0),=15X'F0'                                                 
INST4    BNE   XIT                                                              
         EX    RF,*+4                                                           
         MVC   ABIBUNO(0),22(R3)                                                
         B     AC22VB5                                                          
AC22VB4  LA    R3,LSCNLINE(,R3)                                                 
         BCT   R5,AC22VB3                                                       
AC22VB5  DS    0H                                                               
         LR    R5,R6                                                            
         LA    R3,BLOCK2                                                        
         LA    R4,MYELEM                                                        
AC22WB   CLC   =C'BN',12(R3)                                                    
         BNE   AC22WB1                                                          
         MVC   ABIBINO,SPACES     CLEAR BN                                      
         MVI   ERROR,INVALID                                                    
         ZIC   RF,1(R3)                                                         
         CHI   RF,15                                                            
         BH    XIT                                                              
         LTR   RF,RF               IS THIS A CLEAR REQUEST ?                    
         BZ    AC22WB2             YES, BRANCH TO NEXT CHECK                    
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   ABIBINO(0),22(R3)                                                
         B     AC22WB2                                                          
AC22WB1  LA    R3,LSCNLINE(,R3)                                                 
         BCT   R5,AC22WB                                                        
                                                                                
AC22WB2  LR    R5,R6                                                            
         LA    R3,BLOCK2                                                        
         LA    R4,MYELEM                                                        
         MVI   ERROR,INVALID                                                    
AC22WB3  CLC   =C'BG',12(R3)                                                    
         BNE   AC22WB4                                                          
         ZIC   RF,1(R3)                                                         
         CHI   RF,15                                                            
         BH    XIT                                                              
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   ABIBMEM(0),22(R3)                                                
         B     AC22WB5                                                          
AC22WB4  LA    R3,LSCNLINE(,R3)                                                 
         BCT   R5,AC22WB3                                                       
                                                                                
AC22WB5  DS    0H                                                               
                                                                                
         LA    R4,MYELEM           IF THERE IS EA OR AC NOS BUILD AND           
*                                  ADD A 27 EL                                  
         CLC   ABIEANO(76),SPACES                                               
         BE    AC22VA1             NO GET RID OF OLD ONE IF ANY                 
         MVI   ABIEL,ABIELQ       X'27' - ACCOUNT BILLING INFO ELEMENT          
         MVI   ABILN,X'1E'                                                      
         MVC   ELEMENT(L'MYELEM),MYELEM                                         
         LA    R4,ELEMENT                                                       
AC22VA1  GOTO1 REMANEL,DMCB,(X'27',0)                                           
         CLC   ABIEANO(76),SPACES                                               
         BE    AC22X                                                            
         CLC   ABIESNO(48),SPACES                                               
         BE    *+8                                                              
         MVI   ABILN,X'39'                                                      
         CLC   ABIBINO,SPACES                                                   
         BE    *+8                                                              
         MVI   ABILN,X'48'                                                      
         CLC   ABIBMEM,SPACES                                                   
         BE    ADDELX                                                           
         MVI   ABILN,X'57'                                                      
ADDELX   GOTO1 ADDANEL                                                          
                                                                                
*              NOW SCAN FOR RETAIL BILLING- RECEIVABLE/COSTING CODES            
AC22X    DS    0H                                                               
         GOTO1 REMANEL,DMCB,(X'2B',0)     REMOVE OLD ELEMENT                    
         LTR   R5,R6                                                            
         BZ    AC22Y                                                            
         LA    R3,BLOCK2                                                        
         LA    R4,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         USING RBRELD,R4                                                        
                                                                                
AC22X1   CLC   12(4,R3),=C'RECV'                                                
         BNE   AC22X2                                                           
         MVC   KEY,SPACES          VALIDATE RECEIVABLE ACCOUNT                  
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),=C'SR'                                                  
         MVC   KEY+3(12),22(R3)                                                 
         GOTO1 READ                                                             
         MVI   ERROR,ACCINVAL                                                   
         MVI   ELCODE,ABLELQ       X'32'- GENRAL ACCT MUST BE LOW LEVEL         
         LA    R7,IO                                                            
         BRAS  RE,GETEL                                                         
         BNE   XIT                                                              
         MVI   RBREL,RBRELQ                                                     
         MVI   RBRLN,RBRLNQ                                                     
         MVC   RBRRECB,KEY+1       RECEIVABLE CODE TO ELEMENT                   
         MVC   KEY,HOLDKEY         RESTORE KEY                                  
         B     AC22X3                                                           
AC22X2   CLC   12(4,R3),=C'COST'                                                
         BNE   AC22X3                                                           
         CLC   KEY+1(2),=C'SE'     NOT ON SE                                    
         BE    AC22X3                                                           
         MVC   KEY,SPACES          VALIDATE COSTING ACCOUNT                     
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),=C'1C'                                                  
         MVC   KEY+3(12),22(R3)                                                 
         GOTO1 READ                                                             
         MVI   ERROR,ACCINVAL                                                   
         MVI   ELCODE,ABLELQ       X'32'- GENRAL ACCT MUST BE LOW LEVEL         
         LA    R7,IO                                                            
         BRAS  RE,GETEL                                                         
         BNE   XIT                                                              
         MVI   RBREL,RBRELQ                                                     
         MVI   RBRLN,RBRLNQ                                                     
         MVC   RBRCOST,KEY+1       COSTING CODE TO ELEMENT                      
         MVC   KEY,HOLDKEY         RESTORE KEY                                  
*                                                                               
AC22X3   LA    R3,LSCNLINE(,R3)                                                 
         BCT   R5,AC22X1                                                        
         CLI   RBREL,0            NO OVERRIDES                                  
         BE    AC22Y                                                            
         GOTO1 ADDANEL             ADD THE ELEMENT                              
         EJECT ,                                                                
*                                  FIND SPECIAL ACCOUNT ELEMENT DATA            
AC22Y    GOTO1 REMANEL,DMCB,(X'2C',0)                                           
         LTR   R5,R6                                                            
         BZ    AC22Z                                                            
         LA    R3,BLOCK2                                                        
AC22Y2   L     RF,=A(PNTACC)                                                    
         A     RF,PRELOC                                                        
         USING PNTD,RF                                                          
AC22Y3   CLC   PNTCODE,12(R3)                                                   
         BE    AC22Y4                                                           
         LA    RF,PNTLNQ(,RF)                                                   
         CLI   0(RF),X'FF'                                                      
         BNE   AC22Y3                                                           
         B     AC22Y30             NOT IN TABLE                                 
*                                                                               
AC22Y4   CLC   PNTCODE,=C'ANALYSIS' ANALYSIS OF                                 
         BNE   AC22Y4A                                                          
         CLI   1(R3),1             ONE IS THE OLD WAY                           
         BE    AC22Y30                                                          
         CLI   1(R3),2                                                          
         BNE   AC22Y4A                                                          
         CLC   22(2,R3),=C'NO'                                                  
         BE    AC22Y30                                                          
AC22Y4A  CLC   PNTCODE(5),=C'UNIT1'                                             
         BNE   AC22Y4B                                                          
         TM    DUPSW,DUPANL                                                     
         BO    AC22YER                                                          
         OI    DUPSW,DUPANL                                                     
AC22Y4B  LA    R4,ELEMENT                                                       
         USING SPAELD,R4                                                        
         XC    ELEMENT,ELEMENT                                                  
         MVI   SPAEL,SPAELQ      FOUND, BUILD AN ELEMENT                        
         MVC   SPATYPE,PNTTYP      TYPE                                         
         MVI   SPALN,SPALNQ    LENGTH                                           
         LA    RE,SPAAULA                                                       
         LA    R1,L'SPAAULA                                                     
         BCTR  R1,0                                                             
         CLC   PNTLDG,SPACES       IS THERE A DEFAULT LEDGER                    
         BE    AC22Y5                                                           
         MVC   SPAAULA(2),PNTLDG  MOVE UNIT/LEDGER                              
         LA    RE,2(RE)                                                         
         AHI   R1,-2                                                            
*                                                                               
AC22Y5   EX    R1,*+4                                                           
         MVC   0(0,RE),22(R3)      U/L ACCOUNT                                  
         CLC   1(1,R3),PNTMAX      CHECK MAX INPUT LENGTH                       
         BH    AC22YER             ACCOUNT IS TOO LONG                          
*                                                                               
         MVC   WORK(1),LOGUNIT     UNIT                                         
         MVC   WORK+1(1),LOGLEDG   LEDGER  OF ACCOUNT BEING CHANGED             
         SR    RE,RE                                                            
         ICM   RE,3,PNTFRUL        A(LIST OF VALID FROM U/L)                    
         BZ    AC22Y8              NO LIST                                      
         A     RE,SAVRB            RE TO THE LIST                               
*                                                                               
AC22Y6   CLI   0(RE),X'FF'                                                      
         BE    AC22YER             NOT IN LIST IT'S ERROR                       
         CLC   WORK(2),0(RE)       UL TO ITEM IN LIST                           
         BE    AC22Y8                                                           
         LA    RE,2(,RE)           NEXT ITEM IN LIST                            
         B     AC22Y6                                                           
*                                                                               
AC22Y8   SR    RE,RE                                                            
         ICM   RE,3,PNTTOUL        A(LIST OF VALID TO U/L)                      
         BZ    AC22Y12             NO LIST, ALL ARE VALID                       
         A     RE,SAVRB                                                         
*                                                                               
AC22Y10  CLI   0(RE),X'FF'                                                      
         BE    AC22YER             NOT IN LIST, ERROR                           
         CLC   SPAAULA(2),0(RE)                                                 
         BE    AC22Y12             FOUND A MATCH IT'S VALID POINTER             
         LA    RE,2(,RE)           NEXT TO U/L                                  
         B     AC22Y10                                                          
*                                                                               
AC22Y12  TM    PNTSTAT,PNTFLE      VERIFY POINTER IS ON FILE                    
         BNO   AC22Y15                                                          
         MVC   KEY,SPACES          YES, VALIDATE IT                             
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(L'SPAAULA),SPAAULA                                         
         LR    R7,RF               SAVE A(TABLE ENTRY)                          
         GOTO1 READ                                                             
         LR    RF,R7                                                            
         TM    PNTSTAT,PNTBAL      CHECK THE BALANCE ELEMENT ?                  
         BNO   AC22Y15                                                          
*                                                                               
         LA    R7,IO                                                            
         MVI   ELCODE,ABLELQ       X'32'- ACCOUNT BALANCE ELEMENT               
         BRAS  RE,GETEL                                                         
         BNE   AC22YER             NO BALANCE ELEMENT                           
*                                                                               
AC22Y15  GOTO1 ADDANEL             OK TO ADD THE ELEMENT                        
         MVC   KEY,HOLDKEY         RESTORE KEY                                  
*                                                                               
AC22Y30  LA    R3,LSCNLINE(,R3)                                                 
         BCT   R5,AC22Y2                                                        
         B     AC22Z                                                            
*                                                                               
AC22YER  MVI   ERROR,INVALID                                                    
         B     XIT                                                              
                                                                                
         EJECT ,                                                                
*                                  FIND SPECIAL VAT ELEMENT DATA                
AC22Z    GOTO1 REMANEL,DMCB,('ITCELQ',0)                                        
         LTR   R5,R6                                                            
         BZ    AC22AA              NOTHING IN PROFILE FIELD                     
                                                                                
         LA    R3,BLOCK2                                                        
AC22Z05  XC    TMPPROV,TMPPROV     ASSUME GST                                   
         CLC   12(3,R3),=C'GST'                                                 
         BE    AC22Z10                                                          
*                                                                               
         CLI   14(R3),C' '         PROVINCE ONLY 2 CHAR                         
         BNE   AC22Z09                                                          
         L     R1,=A(PRVTAB)       SEE IF PROVINCE IN TABLE                     
         A     R1,PRELOC                                                        
AC22Z05L CLI   0(R1),X'FF'         EOT?                                         
         BE    AC22Z09                                                          
         CLC   12(2,R3),0(R1)                                                   
         BE    AC22Z05X                                                         
         LA    R1,L'PRVTAB(,R1)                                                 
         B     AC22Z05L                                                         
*                                                                               
AC22Z05X MVC   TMPPROV,0(R1)                                                    
         B     AC22Z10                                                          
*                                                                               
AC22Z09  LA    R3,LSCNLINE(,R3)                                                 
         BCT   R5,AC22Z05                                                       
                                                                                
         B     AC22AA              NO GST DATA FOR THIS ACCOUNT                 
*                                  EDIT INPUT                                   
AC22Z10  CLI   23(R3),C'-'         FORMAT IS "X-DATE"                           
         BNE   AC22ZERR                                                         
                                                                                
         GOTO1 DATVAL,DMCB,(0,24(R3)),WORK  FIRST VALI-DATE                     
         OC    0(4,R1),0(R1)                                                    
         BZ    AC22ZERR                                                         
                                                                                
         GOTO1 DATCON,DMCB,(0,WORK),(1,SAVEDATE)                                
                                                                                
         USING VTCD,R1             CALL VATICAN TO VALIDATE CODE                
         LA    R1,VATBLOCK                                                      
*                                                                               
         XC    VTCD(VTCLNQ),VTCD                                                
         MVI   VTCACTN,VTCAIVAL    VALIDATE INPUT TAX TYPE                      
         MVC   VTCCPY,KEY          MOVE IN COMPANY                              
         MVC   VTCPRV,TMPPROV      THIS WILL TELL VATICAN, GST/PST              
*                                                                               
         USING FLDHDRD,RF                                                       
         LA    RF,VATFLDH                                                       
         XC    VATFLDH,VATFLDH     SET UP DUMMY FIELD HEADER                    
         MVI   FLDLEN,L'VATFLDH    HEADER+DATALEN                               
         MVI   FLDILEN,1           DATA LENGTH                                  
         MVC   FLDDATA(1),22(R3)   MOVE GST/PST CODE                            
         ST    RF,VTCAFLDH         PASS A(DUMMY TWA FIELD) TO VATICAN           
         DROP  RF                                                               
*                                                                               
         MVC   VTCCOMF,COMFACS                                                  
*                                                                               
         MVC   VTCINVD,SAVEDATE                                                 
*                                                                               
         GOTO1 =V(VATICAN),RR=PRELOC                                            
         BNE   AC22ZERR            CODE INVALID                                 
*                                                                               
         TM    VTCINDS,VTCINA      GST NOT APPLICABLE                           
         BO    AC22ZERR            YES, ERROR                                   
*                                                                               
                                                                                
         LA    R4,ELEMENT                                                       
         USING ITCELD,R4                                                        
         XC    ELEMENT,ELEMENT                                                  
         MVI   ITCEL,ITCELQ                                                     
         MVI   ITCLN,ITCLNQ        LENGTH                                       
         MVC   ITCTYPE,22(R3)                                                   
         MVC   ITCEFFD,SAVEDATE                                                 
         MVC   ITCPROV,TMPPROV     PROVINCE                                     
         GOTO1 ADDANEL                                                          
*                                                                               
         B     AC22Z09             LOOP THROUGH FOR PST TOO                     
*                                                                               
AC22ZERR MVI   ERROR,INVALID                                                    
         B     XIT                                                              
*------------------------------------------------------------------             
AC22AA   DS    0H                  PROCESS FAX,COMM OR EMAIL                    
         LA    R7,IO2                                                           
         AH    R7,DATADISP         REMOVE ALL STANDARD COMMENT ELEMENTS         
AC22AA01 CLI   0(R7),0                                                          
         BE    AC22AA03                                                         
         CLI   0(R7),FFTELQ                                                     
         BNE   AC22AA02                                                         
         CLI   2(R7),FFTTCONV                                                   
         BE    AC22AA02                                                         
         CLI   2(R7),FFTTINDL           INDIRECT CLIST                          
         BE    AC22AA02                                                         
         CLI   2(R7),FFTTEPTR           X'CA' ELEMENT TYPE 71                   
         BE    AC22AA02                                                         
         MVI   0(R7),X'FF'                                                      
AC22AA02 ZIC   R1,1(,R7)                                                        
         AR    R7,R1                                                            
         B     AC22AA01                                                         
*------------------------------------------------------------------             
AC22AA03 GOTO1 =V(ACDELEL),DMCB,IO2,RR=RB                                       
***      GOTO1 REMANEL,DMCB,('FFTELQ',0)                                        
*                                                                               
         LTR   R5,R6                                                            
         BZ    AC22AB                   CHECK NEXT PARAMETER                    
*                                                                               
         MVI   BYTE,0                   CLEAR SWITCHES                          
         LA    R3,BLOCK2                                                        
*                                                                               
AC22AA10 CLC   12(3,R3),=C'FAX'                                                 
         BE    AC22AA20                                                         
         CLC   12(4,R3),=C'COMM'                                                
         BE    AC22AA70                                                         
         CLC   12(2,R3),=C'EM'                                                  
         BE    AC22AA80                                                         
*                                                                               
AC22AA15 TM    BYTE,FNDFAX+FNDCOMM+FNDEMAIL  FAX,EMAIL,COMM PROCESSED?          
         BO    AC22AB                         YES, CONTINUE                     
         LA    R3,LSCNLINE(,R3)                                                 
         BCT   R5,AC22AA10                                                      
         B     AC22AB                   CHECK NEXT PARAMETER                    
*------------------------------------------------------------------             
AC22AA20 TM    BYTE,FNDFAX              DUPLICATE FAX PARM ?                    
         BO    AC22AA15                 YES, IGNORE THE DUPLICATE               
         OI    BYTE,FNDFAX              FOUND FAX                               
*        CLI   KEY+1,C'S'               SUBSIDARY ACCOUNTS ONLY                 
*        BNE   XIT                                                              
*        CLI   KEY+2,C'V'               SV, SW, SX, AND SY                      
*        BL    XIT                                                              
*        CLI   KEY+2,C'Y'                                                       
*        BH    XIT                                                              
*                                                                               
         LA    R4,ELEMENT                                                       
         USING FFTELD,R4                                                        
         XC    ELEMENT,ELEMENT                                                  
*                                       EDIT INPUT                              
         ZIC   RF,1(,R3)                AS VARIBLE LENGTH NUMERIC               
         CHI   RF,11                    MAX LEN 11                              
         BH    XIT                                                              
         CHI   RF,10                    MIN LEN 10                              
         BL    XIT                                                              
         BCTR  RF,0                                                             
         EX    RF,AC22AA30                                                      
         EX    RF,AC22AA40                                                      
         EX    RF,AC22AA50                                                      
         B     AC22AA60                                                         
*                                                                               
AC22AA30 MVC   WORK(0),=15X'F0'                                                 
AC22AA40 MVZ   WORK(0),22(R3)                                                   
AC22AA50 CLC   WORK(0),=15X'F0'                                                 
*                                                                               
AC22AA60 BNE   XIT                                                              
*                                       BUILD ELEMENT                           
         EXMVC RF,FFTDATA,22(R3)        SAVE THE DATA                           
*                                                                               
         LA    RF,1(,RF)                RESTORE RF TO DATA LENGTH               
         MVI   FFTTYPE,FFTTPFAX                                                 
         B     AC22AA90                 OUTPUT THE ELEMENT                      
*------------------------------------------------------------------             
AC22AA70 TM    BYTE,FNDCOMM             DUPLICATE COMM PARM?                    
         BO    AC22AA15                 YES, IGNORE THE DUPLICATE               
         OI    BYTE,FNDCOMM             FOUND COMM                              
         MVI   ERROR,INVALID                                                    
         CLI   KEY+1,C'3'               IS THIS A RETAIL RECORD?                
         BNE   XIT                      NO, ERROR                               
*                                       DATA MUST BE XX.XXXX%                   
         CLI   1(R3),1                  L(DATA) > ONE BYTE?                     
         BNH   XIT                      NO, ERROR                               
         CLI   1(R3),8                  L(DATA) > 8 BYTES?                      
         BH    XIT                      YES, ERROR                              
         ZIC   RF,1(,R3)                GET LENGTH OF DATA                      
         BCTR  RF,0                     MINUS ONE                               
         LA    RE,22(RF,R3)             -> LAST BYTE OF DATA                    
         CLI   0(RE),C'%'               PERCENT SIGN?                           
         BNE   XIT                      NO, ERROR                               
*                                                                               
*                                       VALIDATE THE PERCENTAGE                 
         GOTO1 CASHVAL,DMCB,(X'84',22(R3)),(RF)                                 
         CLI   DMCB,X'00'               VALID NUMBER?                           
         BNE   XIT                                                              
         CP    DMCB+4(8),=P'1000000'    >= 100.0000%                            
         BNL   XIT                      YES, ERROR                              
*                                                                               
         LA    R4,ELEMENT                                                       
         XC    ELEMENT,ELEMENT          INSERT THE NUMBER                       
         ZAP   FFTDATA(4),DMCB+4(8)                                             
         MVI   FFTTYPE,FFTTCOMR         COMMISSION RATE                         
         LA    RF,4                     LENGTH                                  
         B     AC22AA90                 OUTPUT THE ELEMENT                      
*--------------------------------------------------------------------           
AC22AA80 TM    BYTE,FNDEMAIL            DUPLICATE EAMIL PARM?                   
         BO    AC22AA15                 YES, IGNORE THE DUPLICATE               
         OI    BYTE,FNDEMAIL            FOUND EMAIL                             
*        CLI   KEY+1,C'S'               SUBSIDARY ACCOUNTS ONLY                 
*        BNE   XIT                                                              
*        CLI   KEY+2,C'V'               SV, SW, SX, AND SY                      
*        BL    XIT                                                              
*        CLI   KEY+2,C'Y'                                                       
*        BH    XIT                                                              
*                                                                               
         LA    R4,ELEMENT                                                       
         USING FFTELD,R4                                                        
         XC    ELEMENT,ELEMENT                                                  
*                                                                               
         BRAS  RE,EMVAL                 VALID EMAIL ADDRESS                     
         BNE   XIT                                                              
*                                                                               
         ZIC   RF,1(,R3)                AS VARIBLE LENGTH                       
         BCTR  RF,0                                                             
         EXMVC RF,FFTDATA,22(R3)        SAVE THE DATA                           
*                                                                               
         AHI   RF,1                     RESTORE RF TO DATA LENGTH               
         MVI   FFTTYPE,FFTTEML                                                  
*--------------------------------------------------------------------           
AC22AA90 MVI   FFTEL,FFTELQ             X'DB' FREE FORM ELEMENT                 
         STC   RF,FFTDLEN               SAVE LENGTH OF DATA                     
         LA    RF,FFTDATA-FFTELD(,RF)                                           
         STC   RF,FFTLN                 SAVE LENGTH OF ELEMENT                  
         GOTO1 ADDANEL                                                          
         MVI   ERROR,0                                                          
         B     AC22AA15                 GET NEXT PARAMETER                      
                                                                                
         DROP  R4                       KEEP IT CLEAN                           
*--------------------------------------------------------------------           
AC22AB   DS    0H                                                               
         MVI   BYTECNTR,1          MARK FIRST PASS THROUGH THIS LOOP            
         XC    SAVESTR,SAVESTR                                                  
         XC    SAVEEND,SAVEEND                                                  
                                                                                
         LTR   R5,R6               SET UP TO SCAN  FOR 'EFFECTIVE'              
         BZ    AC22ABEX            NOTHING IN PROFILE FIELD - SKIP              
         LA    R3,BLOCK2           START OF SCANNED FIELDS                      
                                                                                
AC22AB10 DS    0H                                                               
         MVI   BYTEB,GDATRTLS                                                   
         CLC   12(8,R3),=C'EFFSTART'                                            
         BE    AC22AB20            BRANCH IF FOUND EFFECTIVE                    
         MVI   BYTEB,GDATRTLE                                                   
         CLC   12(2,R3),=C'EFFEND'                                              
         BE    AC22AB20            BRANCH IF FOUND EFFECTIVE                    
         MVI   BYTEB,0                                                          
         LA    R3,LSCNLINE(,R3)    NO, BUMP TO NEXT SCAN LINE                   
         BCT   R5,AC22AB10         IF MORE LINES, CHECK NEXT LINE               
         B     AC22ABEX            NO MORE LINES, EXIT THESE TESTS              
                                                                                
AC22AB20 DS    0H                  FOUND 'EFFECTIVE'                            
         CLI   KEY+1,C'3'          IS THIS A RETAIL RECORD ?                    
         BNE   AC22ABER            NO, INVALID INPUT                            
         MVC   WORK,SPACES         CLEAR THE WORK AREA TO SPACES                
         XC    SAVEDATE,SAVEDATE   CLEAR THE 'FROM DATE'                        
         CLC   =C'NO ',22(R3)      CLEAR EFFECTIVE DATE REQUESTED ?             
         BE    AC22AB52            YES,  CLEAR EFFECTIVE DATE ELEMENT           
*                                  INSERT THE DATE(S)                           
         MVC   WORK(LSCNDIVF),22(R3)                                            
*                                  VALIDATE 'FROM DATE' -                       
*                                  MAKING SURE THAT THE NEXT CALL TO            
*                                  DATVAL WILL NOT USE ANY PART OF THE          
*                                  'FROM DATE'                                  
         GOTO1 DATVAL,DMCB,(2,WORK),WORK+LSCNDIVF+12                            
         OC    0(4,R1),0(R1)       TEST FOR VALID 'FROM DATE'                   
         BZ    AC22ABER            NO,  ERROR INVALID DATE                      
         MVC   FULL,0(R1)          SAVE THE LENGTH OF 'FROM DATE'               
*                                  PACK THE 'FROM DATE'                         
         CLC   WORK+LSCNDIVF+16(2),=C'00'                                       
         BNE   *+10                                                             
         MVC   WORK+LSCNDIVF+16(2),=C'01'                                       
         GOTO1 DATCON,DMCB,(0,WORK+LSCNDIVF+12),(1,SAVEDATE)                    
                                                                                
AC22AB50 DS    0H                                                               
         CLI   BYTEB,GDATRTLS                                                   
         BNE   AC22AB51                                                         
         OC    SAVESTR,SAVESTR                                                  
         BNZ   AC22ABER                                                         
         OC    SAVEEND,SAVEEND                                                  
         BZ    AC22AB52                                                         
         CLC   SAVESTR,SAVEDATE                                                 
         BH    AC22ABER                                                         
         B     AC22AB52                                                         
                                                                                
AC22AB51 CLI   BYTEB,GDATRTLE                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         OC    SAVEEND,SAVEEND                                                  
         BNZ   AC22ABER                                                         
         OC    SAVESTR,SAVESTR                                                  
         BZ    AC22AB52                                                         
         CLC   SAVESTR,SAVEDATE                                                 
         BH    AC22ABER                                                         
                                                                                
AC22AB52 LA    R7,IO2              FIND START OF ELEMENTS                       
         AH    R7,DATADISP                                                      
         USING GDAELD,R7           ADDRESSABILITY FOR 'E5' ELEMENT              
         MVI   BYTE,X'00'          SET 'DATE ELEMENT FOUND' TO NO               
                                                                                
AC22AB53 DS    0H                                                               
         CLI   GDAEL,X'00'         END  OF ELEMENTS ?                           
         BE    AC22AB60            YES, DONE                                    
         CLI   GDAEL,GDAELQ        IS   THIS A GENERAL DATE ELEMENT ?           
         BNE   AC22AB56            NO,  SET UP FOR NEXT ELEMENT                 
         CLC   GDATYPE,BYTEB       IS   THIS A RETAIL EFFECTIVE DATE            
         BNE   AC22AB56            NO,  SET UP FOR NEXT ELEMENT                 
         MVI   GDAEL,X'FF'         YES, KILL THE OLD ELEMENT                    
         MVI   BYTE,X'FF'          SET 'DATE ELEMENT FOUND' TO YES              
                                                                                
AC22AB56 DS    0H                  SET  UP FOR NEXT ELEMENT                     
         ZIC   R1,GDALN            GET  ELEMENT LENGTH                          
         AR    R7,R1               POINT TO NEXT ELEMENT                        
         B     AC22AB53            PROCESS  NEXT ELEMENT                        
                                                                                
         DROP  R7                  KEEP IT CLEAN                                
                                                                                
AC22AB60 DS    0H                                                               
         CLI   BYTE,X'00'          ANY  DATE ELEMENT FOUND ?                    
         BE    AC22AB70            NO,  SKIP DELETES                            
*                                  REMOVE DELETED ELEMENTS                      
         GOTO1 =V(ACDELEL),DMCB,IO2,RR=RB                                       
                                                                                
AC22AB70 DS    0H                  ADD  THE DATE ELEMENT                        
         OC    SAVEDATE,SAVEDATE   ANY  'FROM DATE' SPECIFIED ?                 
         BZ    AC22AB85            NO,  EXIT THIS ROUTINE                       
         LA    R4,ELEMENT          BUILD ELEMENT 'E5' HERE                      
         USING GDAELD,R4           ADDRESSABILITY FOR 'E5' ELEMENT              
         XC    ELEMENT,ELEMENT     CLEAR THE ELEMENT                            
         MVI   GDAEL,GDAELQ        ELEMENT CODE                                 
         MVC   GDATYPE,BYTEB       RETAIL EFFECTIVE START/END DATE              
         MVC   GDADATE,SAVEDATE    INSERT 'FROM DATE'                           
         MVI   GDALN,GDALNQ        ASSUME NO 'TO DATE'                          
*                                  TEST FOR 'TO DATE'                           
         CLI   BYTEB,GDATRTLS                                                   
         BNE   *+10                                                             
         MVC   SAVESTR,SAVEDATE                                                 
         CLI   BYTEB,GDATRTLE                                                   
         BNE   *+10                                                             
         MVC   SAVEEND,SAVEDATE                                                 
         DROP  R4                  KEEP IT CLEAN                                
                                                                                
                                                                                
AC22AB80 DS    0H                                                               
         GOTO1 ADDANEL             ADD  IT                                      
AC22AB85 ZIC   R4,BYTECNTR                                                      
         LA    R4,1(R4)                                                         
         STC   R4,BYTECNTR                                                      
         LA    R3,LSCNLINE(R3)                                                  
         CLI   BYTECNTR,2                                                       
         BE    AC22AB10                                                         
         B     AC22ABEX            EXIT THIS ROUTINE                            
                                                                                
AC22ABER MVI   ERROR,INVALID                                                    
         B     XIT                                                              
                                                                                
AC22ABEX DS    0H                  END  OF EFFECTIVE DATE PROCESSING            
*MN                                                                             
         LR    R5,R6                                                            
         LA    R3,BLOCK2                                                        
         GOTO1 REMANEL,DMCB,('DEXELQ',0)                                        
         LTR   R5,R6                                                            
         BZ    AC22ACX                                                          
                                                                                
         USING DEXELD,R4                                                        
         LA    R4,ELEMENT                                                       
         XC    DEXEL(DEXLNQ),DEXEL                                              
         MVI   DEXEL,DEXELQ                                                     
         MVI   DEXLN,DEXLNQ                                                     
AC22AC3  CLC   =C'DUEDATE',12(R3)                                               
         BE    AC22AC5                                                          
         LA    R3,38(R3)                                                        
         BCT   R5,AC22AC3                                                       
         B     AC22ACX                                                          
                                                                                
         USING CONBLKD,R1                                                       
AC22AC5  LA    R1,WORK                                                          
         XC    CONBLK(CONBLKL),CONBLK                                           
         MVI   CONACTN,CONAVALQ                                                 
         MVI   CONFLD,CONFIDUE                                                  
         LA    RF,22(R3)                                                        
         STCM  RF,15,CONIADD                                                    
         LR    R0,RF                                                            
         CLI   0(RF),C' '                                                       
         BNH   *+12                                                             
         LA    RF,1(RF)                                                         
         B     *-12                                                             
         SR    RF,R0                                                            
         STCM  RF,1,CONILEN                                                     
         MVI   CONOLEN,L'DEXVAL                                                 
         LA    RF,DEXVAL                                                        
         STCM  RF,15,CONOADD                                                    
         MVC   CONCOMF,COMFACS                                                  
         GOTO1 =V(CONVERT),WORK,RR=PRELOC                                       
         CLI   CONERR,0                                                         
         BNE   DUEERR                                                           
         CLI   DEXVAL,1                                                         
         BE    DUEERR                                                           
         GOTO1 ADDANEL                                                          
         B     AC22ACX                                                          
                                                                                
DUEERR   MVI   ERROR,DATERR                                                     
         B     XIT                                                              
                                                                                
AC22ACX  DS    0H                                                               
*MN                                                                             
         B     AC22MEM             NO FREE FORM TEXT FOR THIS ACCOUNT           
*                                  EDIT INPUT                                   
         EJECT ,                                                                
AC22MEM  GOTO1 REMANEL,DMCB,(X'3F',0)                                           
         CLI   LOGMEMH+5,0                                                      
         BE    AC23                                                             
         LA    R4,ELEMENT                                                       
         USING OMEELD,R4                                                        
         MVI   OMEEL,OMEELQ       X'3F' - ONLINE MEMO ELEMENT                   
         ZIC   RF,LOGMEMH+5                                                     
         AHI   RF,2                                                             
         STC   RF,OMELN                                                         
         AHI   RF,-3                                                            
         EX    RF,*+4                                                           
         MVC   OMEMO(0),LOGMEM                                                  
         GOTO1 ADDANEL                                                          
*                                                                               
AC23     GOTO1 =A(ACDSLY),DMCB,(RC),(R8),RR=PRELOC                              
*                                                                               
         GOTO1 =A(HEIRARC),DMCB,(RC),(R8),RR=PRELOC                             
*                                                                               
XIT      XIT1  REGS=(R2)                                                        
         EJECT ,                                                                
*-------------------------------------------------------------------*           
*              SPECIAL DELETE CODE                                              
*-------------------------------------------------------------------*           
*                                                                               
AC30     MVI   ERROR,INVALID                                                    
         CLI   LOGACT,C'A'                                                      
         BNE   XIT                                                              
         CLC   KEY+1(2),=C'SJ'     CAN'T DELETE SJ HERE                         
         BE    XIT                                                              
*                                                                               
         MVI   ERROR,67                                                         
         LA    R4,IO2                                                           
         AH    R4,DATADISP                                                      
*                                                                               
AC30A    CLI   0(R4),0                                                          
         BE    AC30X                                                            
         CLI   0(R4),X'26'         CAN'T DELETE IF NEW ESTIMATE JOB             
         BE    AC30B                                                            
*                                                                               
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     AC30A                                                            
*                                                                               
         USING JOBELD,R4                                                        
AC30B    TM    JOBSTA1,JOBSNEST                                                 
         BO    XIT                                                              
*                                                                               
AC30X    LA    R4,IO2                                                           
         AH    R4,DATADISP                                                      
*&&US                                                                           
                                                                                
AC31     CLI   0(R4),0                                                          
         BE    AC31X                                                            
         CLI   0(R4),DSCELQ        X'62'- CAN'T DELETE RECORDS WITH A           
         BNE   AC31B                      SCHEME CODE                           
         USING DSCELD,R4                                                        
         CP    DSCVAL,=P'0'                                                     
         BNE   XIT                 ERROR CAN'T DELETE                           
AC31B    ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     AC31                                                             
AC31X    LA    R4,IO2                                                           
         AH    R4,DATADISP                                                      
*&&                                                                             
AC32     CLI   0(R4),0                                                          
         BE    AC36                                                             
         CLI   0(R4),RSTELQ        X'30' - RECORD STATUS ELEMENT ?              
         BNE   AC33                                                             
         USING RSTELD,R4                                                        
         OC    RSTBDATE,RSTBDATE   OLD RECORDS                                  
         BZ    AC33                                                             
*&&US                                                                           
         LA    RF,IO2                                                           
         CLI   1(RF),C'3'          SKIP TEST BELOW IF RETAIL                    
         BE    AC33                                                             
*&&                                                                             
         CLC   RSTTDATE,RSTBDATE   THIS CHECKS FOR SELF-BALANCING DR/CR         
         BH    XIT                                                              
         B     AC34                                                             
                                                                                
AC33     CLI   0(R4),ABLELQ        X'32' - ACCOUNT BALANCE ELEMENT              
         BNE   AC34                                                             
         USING ABLELD,R4                                                        
         CP    ABLFRWD,=P'0'      ACCOUNT MUST HAVE BALANCE ELEMENT             
         BNE   XIT                 AND ALL BALANCES MUST BE ZERO                
         CP    ABLDR,=P'0'                                                      
         BNE   XIT                                                              
         CP    ABLCR,=P'0'                                                      
         BNE   XIT                                                              
         LA    RF,IO2                                                           
         OI    44(RF),X'80'                                                     
         MVI   ERROR,X'FF'                                                      
*                                                                               
         CLC   KEY+1(2),=C'1R'     DO FOR PERSON RECORDS ONLY                   
         BNE   ACX                                                              
*                                                                               
         GOTO1 =V(AC1RMNT),DMCB,(C'D',IO2),DATAMGR,RR=PRELOC                    
         CLI   DMCB+8,0                                                         
         BE    ACX                                                              
         DC    H'0'                                                             
                                                                                
         USING ASTELD,R4                                                        
AC34     CLI   ASTEL,ASTELQ        CHECK FOR DRAFT ITEMS                        
         BNE   AC35                                                             
         OC    ASTDRAFT,ASTDRAFT                                                
         BZ    AC35                NO DRAFT ITEMS - FINE                        
         MVI   ERROR,DELDRFT                                                    
         B     XIT                                                              
                                                                                
AC35     SR    R5,R5                                                            
         IC    R5,1(R4)                                                         
         AR    R4,R5                                                            
         B     AC32                                                             
                                                                                
*                                  GET ACCOUNT LEVEL                            
AC36     BRAS  RE,GETACTLV                                                      
*                                                                               
ACX      B     XIT                                                              
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*        ERROR ROUTINES                                               *         
*---------------------------------------------------------------------*         
ERRMETH  LA    RF,AE$IVMET         INVALID METHOD                               
         B     INVERR                                                           
ERRLEDG  LA    RF,AE$OPTNV         OPTION NOT VALID FOR U/L                     
         B     INVERR                                                           
ERRLSTCD LA    RF,AE$INVOF         INVALID OFFICE LIST                          
         B     INVERR                                                           
ERRDUP   LA    RF,AE$DUPMT         DUPLICATE METHOD                             
         B     INVERR                                                           
ERRMNLST LA    RF,AE$MNLST         LIST CDE MUST BE NO MORE THAN                
*                                                                               
INVERR   XC    WORK,WORK                                                        
         GOTO1 VGETTXT,WORK,(RF),(0,LOGHEADH),(C'E',0)                          
         LA    R2,LOGPROFH                                                      
         MVI   ERROR,X'FE'                                                      
         B     XIT                                                              
         EJECT ,                                                                
         LTORG                                                                  
*                                                                               
         GETEL R7,DATADISP,ELCODE                                               
*                                                                               
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*        EQUATES                                                      *         
*---------------------------------------------------------------------*         
*                                      LENGTH OF EQUATES                        
LSCNFIXD EQU   22                      .    FIXED PORTION OF SCAN               
LUNSCNFX EQU   10                      .    FIXED PORTION OF UNSCAN             
LSCNDIVF EQU   60                      .    SCAN DIVIDED FIELDS                 
LSCNLINE EQU   LSCNFIXD+LSCNDIVF       .    SCAN   LINE                         
LUNSCNLN EQU   LUNSCNFX+LSCNDIVF       .    UNSCAN LINE                         
                                                                                
*                                      MAXIMUMS                                 
MAXSCNLN EQU   20                      .    SCAN   LINES                        
                                                                                
         EJECT ,                                                                
                                                                                
       ++INCLUDE ACPRVTAB                                                       
         EJECT ,                                                                
*-------------------------------------------------------------------*           
* TABLE OF SPECIAL ACCOUNT POINTERS (2C ELEMENT)                                
*-------------------------------------------------------------------*           
PNTACC   DC    AL1(SPATINCO)        OVERRIDE INCOME                             
         DC    CL8'OIN'                                                         
         DC    CL2'  '                                                          
         DC    AL1(PNTFLE+PNTBAL)                                               
         DC    AL1(14)                                                          
         DC    AL2(OINFR-T60303)                                                
         DC    AL2(OINTO-T60303)                                                
*                                                                               
         DC    AL1(SPATWOFF)        OVERRIDE WRITE-OFF                          
         DC    CL8'OWO'                                                         
         DC    CL2'  '                                                          
         DC    AL1(PNTFLE+PNTBAL)                                               
         DC    AL1(14)                                                          
         DC    AL2(OWOFR-T60303)                                                
         DC    AL2(OWOTO-T60303)                                                
*                                                                               
         DC    AL1(SPATANAL)        ANALYSIS AND UNIT ARE BOTH VALID            
         DC    CL8'ANALYSIS'                                                    
         DC    CL2'  '                                                          
         DC    AL1(0)                                                           
         DC    AL1(12)                                                          
         DC    AL2(OANFR-T60303)                                                
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(SPATANAL)        FOR ANALYSIS IN UNIT 1                      
         DC    CL8'UNIT1'                                                       
         DC    CL2'  '                                                          
         DC    AL1(0)                                                           
         DC    AL1(12)                                                          
         DC    AL2(OANFR-T60303)                                                
         DC    AL2(0)                                                           
*                                                                               
         DC    X'FF'                                                            
*                                                                               
OINFR    DC    C'1R',X'FF'                                                      
OINTO    DC    C'SK',C'SI',X'FF'                                                
*                                                                               
OWOFR    DC    C'1R',X'FF'                                                      
OWOTO    DC    C'SI',X'FF'                                                      
*                                                                               
OANFR    DC    C'SI',C'SE',X'FF'                                                
         EJECT ,                                                                
*-----------------------------------------------------------------*             
* SETS FILTER 1 VALUE IF ANYTHING IS IN THE FILTER FIELD          *             
* R4=A(X'30' ELEMENT)                                             *             
*-----------------------------------------------------------------*             
         SPACE 1                                                                
         USING RSTELD,R4                                                        
SETFILT1 NTR1  BASE=*,LABEL=*                                                   
         LA    R4,ELEMENT                                                       
         CLI   LOGFILTH+5,0                                                     
         BNE   *+12                                                             
         MVI   RSTFILT1,X'40'                                                   
         B     SETF1NO                 SET CC NOT EQUAL                         
         CLC   RSTFILT1,LOGFILT        DID FILTER CHANGE                        
         BE    *+8                     NO                                       
         OI    LFMINDS,LFMIFLCH        FILTER HAS CHANGED                       
         LA    R2,LOGFILTH                                                      
         MVC   RSTFILT1,LOGFILT                                                 
         MVI   ERROR,INVALID                                                    
         CLI   LOGFILT,C'.'                                                     
         BE    SETF1X                                                           
SETF1NO  LTR   RB,RB                   CC NOT EQUAL                             
SETF1X   XIT1  REGS=(R2)                                                        
         DROP  R4                                                               
         EJECT                                                                  
*------------------------------------------------------------------*            
* EQUBLD ROUTINE - BUILD EQUIVLENT  ACCOUNT ELEM AND CHECK  RULES  *            
*------------------------------------------------------------------*            
*                                                                               
         USING APRELD,R4                                                        
EQUBLD   NTR1  BASE=*                                                           
*                                                                               
         MVC   KEY,SPACES          ZERO KEY                                     
         MVC   KEY(1),COMPANY      ADD COMPANY TO KEY                           
         MVC   KEY+1(1),LOGUNIT    ADD UNIT TO KEY                              
         MVC   KEY+2(1),LOGLEDG    LEDGER TO KEY                                
         GOTO1 READ                                                             
*                                                                               
         LHI   R5,0                SOME GLOBAL VAR FOR LOOP                     
*                                                                               
         LA    R4,IO               LEDGER RECORD IN IO                          
         AH    R4,DATADISP         POINT TO FIRST ELEMENT                       
EQUBL10  DS    0H                                                               
         CLI   0(R4),0                                                          
         BE    EQUBLX                                                           
         CLI   0(R4),APRELQ        IS IT X'CA' RULE ELEMENT                     
         BE    EQUBL30                                                          
EQUBL20  ZIC   R1,1(R4)            BUMP TO NEXT ELEMENT                         
         AR    R4,R1                                                            
         B     EQUBL10                                                          
*                                                                               
EQUBL30  DS    0H                                                               
         MVC   SVSEQ,APRSEQ        SAVE SEQUENCE NUMBER TO BUILD NEW            
         ST    R4,ACAELEM          STORE ADDRESS OF X'CA' ELEMENT               
*                                                                               
         ZIC   R3,EQUACNT          # OF ENTRIES THAT CAN GO ON A SCREEN         
         CR    R5,R3                                                            
         BE    EQUBLX                                                           
         ZIC   R6,SVSEQ                                                         
         CHI   R6,4                                                             
         BNE   *+8                                                              
         B     *+4                                                              
         MHI   R6,LOGEQU2H-LOGEQU1H                                             
         LA    R2,LOGEQU1H                                                      
         AR    R2,R6                                                            
*                                                                               
         ZIC   R1,7(R2)            GET LENGTH OF ACCOUNT DESC                   
         LTR   R1,R1               ZERO MEANS NOTHING ON SCREEN                 
         BZ    EQUBLX                                                           
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),APRDESC    DON'T SCREW UP R4                             
         BNE   EQUBL20             GET NEXT X'CA' ELEMENT AND CHECK             
         DROP  R4                                                               
*                                                                               
         SR    R1,R1                                                            
         IC    R1,0(R2)            GET LENGTH                                   
         AR    R2,R1               POINT TO NEXT FIELD ACC CODE                 
         CLI   5(R2),0             IS THERE ANYTHING IN THIS FIELD              
         BE    EQUBL40                                                          
         GOTO1 =A(CKCARULE),DMCB,ACAELEM,(R2),RR=PRELOC                         
         BNZ   EQUBLX                                                           
*                                                                               
         USING FFTELD,R4                                                        
EQUBL40  DS    0H                  FIND AND DELETE OR ADD AN ELEM               
         LA    R4,IO2                                                           
         AH    R4,DATADISP                                                      
EQUBL50  DS    0H                                                               
         CLI   0(R4),0                                                          
         BE    EQUBL80             EITHER ADD IF NEEDED ELSE SKIP               
         CLI   0(R4),FFTELQ        X'DB' FREE FORM TEXT                         
         BNE   EQUBL60                                                          
         CLI   FFTTYPE,FFTTEPTR    TYPE 71                                      
         BNE   EQUBL60                                                          
         CLC   SVSEQ,FFTSEQ        DOES SEQUENCE ON SCREEN MATCH                
         BE    EQUBL70                                                          
EQUBL60  ZIC   R3,1(R4)            GET LENGTH                                   
         AR    R4,R3               BUMP TO NEXT ELEMENT                         
         B     EQUBL50                                                          
*                                                                               
EQUBL70  MVI   0(R4),X'FF'         READY TO DELETE                              
         GOTO1 REMANEL,DMCB,(X'FF',0)  DELETE ELEMENT X'DB' TYPE 70             
*                                                                               
EQUBL80  CLI   5(R2),0             IS THERE ANYTHING IN THE FIELD               
         BE    EQUBL90             TO BUILD NEW                                 
*                                                                               
         LA    R4,ELEMENT          BUILD AND ADD NEW ELEMENT                    
         XC    ELEMENT,ELEMENT                                                  
         MVI   FFTEL,FFTELQ        MOVE X'DB'                                   
         MVI   FFTTYPE,FFTTEPTR    TYPE 70                                      
         MVC   FFTSEQ,SVSEQ                                                     
         ZIC   R1,5(R2)            GET LENGTH OF INPUT                          
         STC   R1,FFTDLEN          ADD DATA LENGTH                              
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   FFTDATA(0),8(R2)                                                 
         AHI   R1,FFTDATA-FFTELD+1 ADD OVRHEAD+1 THAT WAS SUBTRACTED            
         STC   R1,FFTLN            UPDATE ELEMENTS LENGTH                       
         GOTO1 ADDANEL                                                          
*                                                                               
EQUBL90  DS     0H                                                              
         AHI   R5,1                BUMP TO NEXT FIELD                           
         L     R4,ACAELEM          GET   ADDRESS OF X'CA' ELEMENT               
         B     EQUBL20                                                          
*                                                                               
EQUBLX   DS    0H                                                               
         XIT1  REGS=(R2)                                                        
*                                                                               
         DROP  R4                                                               
         EJECT ,                                                                
*--------------------------------------------------------------------*          
* THIS ROUTINE CHECKS WETHER EQUIVALENT ACCOUNT INPUT BY USER MATCHES*          
* AND CONFINE WITHIN THE X'CA' RULE ELEMENT FROM LEDGER RECORD       *          
* EQUIVALENT ACCOUNT CANNOT BE ADDED TO SJ JOB LEVEL. DISPLAY ERROR  *          
* IF SOMEONE TRIES TO DO SO                                          *          
* PARAMETER 1 = ADDRESS OF X'CA' ELEMENT                             *          
* PARAMETER 2 = ADDRESS OF INPUT FLD HEADER                          *          
* ON EXIT EITHER DISPLAY ERROR MESSAGE OR EXITS OK                   *          
*--------------------------------------------------------------------*          
         USING APRELD,R4                                                        
CKCARULE NTR1  BASE=*,LABEL=*                                                   
         L     R4,0(R1)            GET ADDRESS OF X'CA' ELEMENT                 
         L     R2,4(R1)                                                         
*                                                                               
         ZIC   R3,5(R2)            GET LENGTH OF INPUT FLD                      
         ST    R2,AHEADER          STORE ADD OF THIS CURRENT HEADER             
         LA    R2,8(R2)            R2 NOW POINTS TO INPUT                       
*                                                                               
         LA    R1,IO2              GET KEY                                      
         CLC   1(2,R1),=C'SJ'      SJ AT JOB LVL NOT ALLOW  EQUIV ACCS          
         BNE   CKCA10                                                           
         ZIC   R5,LEVB             GET DISPLACEMENT OF JOB IN ACCOUNT           
         LA    R1,3(R5,R1)         BUMP CPY/U/L                                 
         CLI   0(R1),C' '          IS ANYTHING AT JOB LEVEL                     
         BE    CKCA10            GOOD TO CONTINUE NOTHING AT JOB LEVEL          
*                                                                               
         LA    R2,LOGACCH                                                       
         ST    R2,AHEADER                                                       
         LR    R7,R5               POINT CURSOR THIS FAR IN FIELD               
         BAS   RE,SETCUR           SETCURSOR AT ACCOUNT FLD                     
*                                                                               
         USING GETTXTD,R1          BUILD PARAM LIST FOR GETTXT                  
         LA    R1,DMCB                                                          
         XC    DMCB(24),DMCB                                                    
         MVI   GTMTYP,GTMERR                                                    
         MVC   GTMSGNO,=AL2(1781)  SETTING INVALID AT THIS LEVEL                
         MVI   GTLTXT,5                                                         
         MVC   ERRBLK(2),=X'9686'  APPEND LOWERCASE C'OF' TO ERRMSG             
         MVC   ERRBLK+2(3),=C' SJ' APPEND 'OF SJ' IN FRONT                      
         LA    RE,ERRBLK                                                        
         STCM  RE,7,GTATXT         PASS NEW TEXT'S BLOCK'S ADDRESS              
         B     CKCAERX                                                          
         DROP  R1                                                               
*                                                                               
CKCA10   MVI   ERROR,AE$FLDTL      INPUT FIELD LEN TOO LONG                     
         ZIC   R5,APRTLEN          GET LEN OF RULE                              
         CR    R3,R5               R3 HAS LEN OF INPUT                          
         BH    CKCANO              NOT GOOD                                     
         MVI   ERROR,AE$FLDTS      INPUT FIELD LEN TOO SHORT                    
         BL    CKCANO                                                           
*                                                                               
         LA    R5,APRMLEN          POINT TO LENGTH OF FIRST LEVEL               
         SR    R7,R7                                                            
         ZIC   R1,APRNLEVS         TOTAL NUMBER OF LEVEL                        
CKCA20   DS    0H                                                               
         STC   R1,NLEVS            TOTAL LEVELS LEFT,OUTER LOOP CNTR            
         ZIC   R6,0(R5)            DON'T SCREW UP R6 IT IS A LOOP CNTR          
         LA    R3,1(R5)            POINT TO FIRST CHARACTER OF RULE             
*                                                                               
CKCA30   DS    0H                                                               
         AHI   R7,1                POSITION OF CHARACTER COUNTER                
         CLI   0(R3),X'83'         IS IT LOWER CASE 'C'                         
         BE    CKCA40              SKIP CHCKNG FOR LOWER 'A' AND '#'            
         CLI   0(R3),X'81'         IS IT LOWER CASE 'A'                         
         BNE   CKCA50              CHECK FOR NUMBER SIGN                        
CKCA40   CLI   0(R2),C'A'                                                       
         BL    CKCAERR                                                          
         CLI   0(R2),C'I'                                                       
         BNH   CKCA80                                                           
         CLI   0(R2),C'J'                                                       
         BL    CKCAERR                                                          
         CLI   0(R2),C'R'                                                       
         BNH   CKCA80                                                           
         CLI   0(R2),C'S'                                                       
         BL    CKCAERR                                                          
         CLI   0(R2),C'Z'                                                       
         BNH   CKCA80                                                           
         CLI   0(R3),X'83'         IF ALPHANUMERIC THEN CHECK FOR NUM           
         BE    CKCA60                                                           
CKCA50   CLI   0(R3),C'#'                                                       
         BNE   CKCA70                                                           
CKCA60   CLI   0(R2),X'F0'         IS IT BETWEEN 0 AND 9                        
         BL    CKCAERR                                                          
         CLI   0(R2),X'F9'                                                      
         BH    CKCAERR                                                          
         B     CKCA80                                                           
CKCA70   CLC   0(1,R3),0(R2)       HAS TO BE A CONSTANT                         
         BNE   CKCAERR                                                          
CKCA80   LA    R2,1(R2)            BUMP ACCOUNT BY 1 CHARACTER                  
         LA    R3,1(R3)            BUMP RULE BY 1 CHARACTER                     
         BCT   R6,CKCA30                                                        
*                                                                               
         IC    R6,0(R5)                                                         
         AHI   R6,1                ADD OVERHEAD                                 
         AR    R5,R6                                                            
         ZIC   R1,NLEVS            GET HOW MANY MORE LEVELS LEFT                
         BCT   R1,CKCA20           GET NEXT RULE ELEMENT                        
         B     CKCAYES                                                          
*                                                                               
CKCAERR  DS    0H                  BUILD GETTXT PARAM LIST AND DIS ERR          
         BCTR  R7,0                INDEX TO POINT CURSOR                        
         BAS   RE,SETCUR           SET CURSOR TO WHERE ERROR IS                 
         AHI   R7,1                RESTORE R7 FOR CORRECT POS NO                
*                                                                               
         USING GETTXTD,R1                                                       
         LA    R1,DMCB                                                          
         XC    DMCB(24),DMCB                                                    
         MVI   GTMTYP,GTMERR                                                    
         MVI   GTLTXT,2                                                         
         EDIT  (R7),(2,ERRBLK),ALIGN=LEFT,FILL=0  R1 CAN GET MESSED UP          
         LA    RE,ERRBLK                                                        
         STCM  RE,7,GTATXT                                                      
*                                                                               
         CLI   0(R3),X'83'         NOT ALPHANUMERIC ERROR                       
         BNE   *+14                                                             
         MVC   GTMSGNO,=AL2(AE$ALNUM)                                           
         B     CKCAERX                                                          
*                                                                               
         CLI   0(R3),X'81'         NOT AN ALPHABET ERROR                        
         BNE   *+14                                                             
         MVC   GTMSGNO,=AL2(AE$ALPHA)                                           
         B     CKCAERX                                                          
*                                                                               
         CLI   0(R3),C'#'          NOT NUMERIC ERROR                            
         BNE   *+14                                                             
         MVC   GTMSGNO,=AL2(AE$NUMER)                                           
         B     CKCAERX                                                          
*                                                                               
         MVC   GTMSGNO,=AL2(AE$NCONS)  NOT A VALID CONSTANT ERROR               
*                                                                               
CKCAERX  GOTO1 VGETTXT,DMCB                                                     
         DROP  R1                                                               
         MVI   ERROR,X'FE'                                                      
         B     CKCANO                                                           
*                                                                               
CKCAYES  SR    RC,RC               SET CONDITION CODE TO GOOD                   
CKCANO   LTR   RC,RC               SET CONDITION CODE TO NOT GOOD               
CKCAX    XIT1                                                                   
         EJECT                                                                  
*-----------------------------------------------------------------*             
* SETS CURSOR TO WHERE ERROR IS IN EQUIVALENT ACCOUNT CODE        *             
* R7=INDEX   WHERE TO POINT THE CURSOR                            *             
* AHEADER = A(ADDRESS OF FLD TO PUT THE CURSOR)                   *             
*-----------------------------------------------------------------*             
         SPACE 1                                                                
         USING TIOBD,RF                                                         
SETCUR   NTR1                                                                   
         L     RF,ASYSPARM                                                      
         L     R1,AHEADER          GET ADDRESS OF CURRENT FLD HDR               
         SR    R1,RA                                                            
         STCM  R1,3,TIOBCURD       SET DISPLACEMENT OF FIELD HDR                
         STC   R7,TIOBCURI         SET WHERE IN FIELD TO POINT CURSOR           
         OI    TIOBINDS,TIOBSETC   POSITION CURSOR                              
         B     CKCAX                                                            
         DROP  RF                                                               
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        VALIDATE METHOD FOR ILIST ACCOUNT PROFILE                              
*        BYTE CONTAINS METHOD NUMBER FOR READ                                   
*-------------------------------------------------------------------*           
CHKMETH  NTR1  BASE=*                                                           
         USING CAHRECD,R4                                                       
         LA    R4,KEY2                                                          
         MVC   CAHKEY,SPACES                                                    
         MVI   CAHKTYP,CAHKTYPQ    ALLOCATION METHOD RECORD TYPE BY NUM         
         MVI   CAHKSUB,CAHKSUBQ    METHOD BY NUM SUB TYPE                       
         MVC   CAHKCPY,COMPANY     COMPANY                                      
         MVC   CAHKMTHD,BYTE       METHOD NUMBER                                
         XC    CAHKOFC,CAHKOFC                                                  
         MVC   SAVEKEY,KEY2                                                     
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'ACCDIR',KEY2,KEY2                     
         CLC   SAVEKEY(L'SAVEKEY),KEY2                                          
         BE    CHKMYES                                                          
         B     CHKMNO                                                           
*                                                                               
CHKMYES  CR    RB,RB                                                            
         B     XIT                                                              
CHKMNO   LTR   RB,RB                                                            
         B     XIT                                                              
         DROP  R4                                                               
         EJECT ,                                                                
*-------------------------------------------------------------------*           
*        VALIDATE INDIRECT CLIENT LIST CODE                                     
*-------------------------------------------------------------------*           
CHKCLIST NTR1  BASE=*,LABEL=*                                                   
         USING LSTRECD,R4                                                       
         LA    R4,KEY2                                                          
         MVC   LSTKEY,SPACES                                                    
         MVI   LSTKTYP,LSTKTYPQ    X'1D'                                        
         MVC   LSTKCPY,COMPANY                                                  
         MVC   LSTKLST,22(R3)      LIST CODE                                    
         OC    LSTKLST,SPACES                                                   
         MVC   SAVEKEY,KEY2                                                     
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'ACCDIR',KEY2,KEY2                     
         CLC   SAVEKEY(L'SAVEKEY),KEY2                                          
         BE    CHKLYES                                                          
         B     CHKLNO                                                           
*                                                                               
CHKLYES  CR    RB,RB                                                            
         B     XIT                                                              
CHKLNO   LTR   RB,RB                                                            
         B     XIT                                                              
         DROP  R4                                                               
         EJECT ,                                                                
*-------------------------------------------------------------------*           
*        CHECK FOR LOW LEVEL OF 1R                                              
*-------------------------------------------------------------------*           
CHKPRSN  NTR1  BASE=*,LABEL=*                                                   
         MVI   ERROR,0                                                          
         LA    R4,SAVEHEIR             CHECK IF AMENDING/ADDING LOW             
         USING ACLELD,R4              LEVEL ACCOUNT                             
         LA    R2,LOGACCH                                                       
         CLC   5(1,R2),ACLVALS+(L'ACLVALS*2)                                    
         BNH   XIT                                                              
         MVC   SAVEKEY,KEY             DO ANY NEW PERSON RECS EXIST             
         MVC   KEY,SPACES              ON FILE FOR THIS COMPANY?                
         MVI   KEY,X'0F'                                                        
         MVC   KEY+1(1),COMPANY                                                 
         GOTO1 HIGH                                                             
         CLI   KEY,X'0F'               DID I GET A PERSON RECORD?               
         BNE   CHKP10                                                           
         CLC   KEY+1(1),COMPANY        - FOR THIS COMPANY?                      
         BNE   CHKP10                  IF YES PUT OUR ERR MSG AND EXIT          
         MVC   KEY,SAVEKEY                                                      
         CLI   LOGACT,C'N'                                                      
         BE    CHKP05                                                           
         GOTO1 READ                    RESET DATAMGR IF ACTION NOT NEW          
CHKP05   DS    0H                                                               
         MVI   ERROR,COSTPRGM                                                   
         B     XIT                                                              
*                                                                               
CHKP10   DS    0H                                                               
         MVC   KEY,SAVEKEY             ON ACTION CHANGE MUST RESTORE            
         CLI   LOGACT,C'N'             READ                                     
         BE    XIT                                                              
         GOTO1 READ                                                             
         B     XIT                                                              
         EJECT ,                                                                
*-------------------------------------------------------------------*           
*        CHECK FOR METHOD BUCKETS                                               
*-------------------------------------------------------------------*           
CHKBUCK  NTR1  BASE=*,LABEL=*                                                   
         USING CACRECD,R6                                                       
         MVI   ERROR,0                                                          
         MVC   SAVEKEY,KEY             DO ANY METHOD BUCKETS                    
         LA    R6,IO                                                            
         MVC   CACKEY,SPACES                                                    
         MVC   CACKCULA,SAVEKEY                                                 
         MVC   CACKCCPY,COMPANY                                                 
         MVC   CACKCUNT(2),=C'14'                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'ACCDIR',IO,IO                         
         B     CHKBUK04                                                         
CHKBUK02 GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'ACCDIR',IO,IO                         
CHKBUK04 CLC   CACKCULA,SAVEKEY                                                 
         BNE   CHKBUK06                                                         
         CLC   CACKCUNT(2),=C'14'                                               
         BNE   CHKBUK06                                                         
         CLC   CACKSPAC,SPACES                                                  
         BNE   CHKBUK02                                                         
         CLI   CACKBTYP,C' '                                                    
         BH    CHKBUK08                                                         
         B     CHKBUK02                                                         
CHKBUK06 MVC   KEY,SAVEKEY             RESET DATAMGR IF ACTION NOT NEW          
         CLI   LOGACT,C'N'             READ                                     
         BE    XIT                                                              
         GOTO1 READ                                                             
         B     XIT                                                              
CHKBUK08 DS    0H                                                               
         MVI   ERROR,67                                                         
         B     XIT                                                              
         DROP  R6                                                               
         EJECT ,                                                                
*-------------------------------------------------------------------*           
*        PROVISIONAL ACCOUNTS MAY NOT HAVE TRANSACTIONS                         
*        READ FOR DRAFT AND LIVE TRANSACTIONS. IF ANY FOUND ERROR               
*-------------------------------------------------------------------*           
         USING TRNRECD,R3                                                       
CHKPROV  NTR1  BASE=*,LABEL=*                                                   
         MVC   HOLDKEY,KEY                                                      
         LA    R3,IO                                                            
         GOTO1 HIGH                                                             
         B     CKPV11                                                           
CKPV10   GOTO1 SEQ                                                              
CKPV11   CLC   TRNKCULA,KEYSAVE    STILL SAME KEY ?                             
         BNE   CKPV20              NO, PROVISIONAL OK                           
*                                  X'44' - TRANSACTION ?                        
         CLI   TRNKEY+ACCORFST,TRNELQ                                           
         BNE   CKPV10                                                           
         MVI   ERROR,NOCHANGE      NO TRANS ALLOWED ON A PROVISIONAL            
         B     CKPVERR                                                          
*                                                                               
CKPV20   MVC   KEY,HOLDKEY                                                      
         GOTO1 READ                RESET DATAMGR                                
         B     XIT                                                              
*                                                                               
CKPVERR  CR    RB,RC                                                            
         B     XIT                                                              
         DROP  R3                                                               
         EJECT ,                                                                
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -             
*-------------------------------------------------------------------*           
*        CHECK IF VALID EMAIL ADDRESS                                           
*-------------------------------------------------------------------*           
EMVAL    NTR1  BASE=*,LABEL=*                                                   
         MVI   FLGEM,0             INIT FLAG                                    
         ZIC   R2,1(R3)            PICK UP THE LENGTH                           
         LA    R4,22(R3)                                                        
         LR    R3,R4               EXTRA POINTER TO EMAIL ADDRS                 
*                                                                               
         CHI   R2,60               IS E-MAIL MORE THAN 60 CHARS                 
         BH    EMVALERR            YES, IT'S AN ERROR                           
         CHI   R2,7                SHLD ATLST LOOK LIKE X@X.XXX                 
         BL    EMVALERR                                                         
*                                                                               
EMVAL10  DS    0H                                                               
         SHI   R2,4                WANT TO POINT TO .XXX                        
         AR    R4,R2                                                            
         CLI   0(R4),C'.'          IS IT DOT SOMETHING                          
         BNE   EMVALERR                                                         
         LHI   R0,3                VALIDATE .COM, .EDU ETC                      
EMVAL5   LA    R4,1(R4)                                                         
         CLI   0(R4),C'A'          IS IT HIGHER THAN A                          
         BL    EMVALERR                                                         
         CLI   0(R4),C'Z'          SHOULD BE Z OR LOWER                         
         BH    EMVALERR                                                         
         BCT   R0,EMVAL5                                                        
         AHI   R2,4                E-MAIL'S LENGTH                              
*                                                                               
         CLI   0(R3),C'.'          DOES USER NAME START WITH DOT                
         BE    EMVALERR            ERROR CAN'T START WITH DOT                   
EMVAL30  DS    0H                                                               
         CLI   0(R3),C'0'          IS IT LESS THAN F0                           
         BL    EMVAL40             YES CHK NEXT                                 
         CLI   0(R3),C'9'          IS IT HIGHER THAN F9                         
         BNH   EMVAL50             IT IS A NUMBER                               
         B     EMVALERR                                                         
EMVAL40  CLI   0(R3),C'A'          IS IT HIGHER THAN A                          
         BL    EMVAL60             CHECK FOR SPECIAL CHARS                      
         CLI   0(R3),C'Z'          SHOULD BE Z OR LOWER                         
         BH    EMVALERR                                                         
*                                                                               
EMVAL50  DS    0H                                                               
         LA    R3,1(R3)            GET NEXT CHAR                                
         BCT   R2,EMVAL30                                                       
         B     EMVALGD                                                          
*                                                                               
EMVAL60  DS    0H                                                               
         LA    R1,EXCTAB           POINT TO SPECIAL CHARS TABLE                 
EMVAL60A CLI   0(R1),X'FF'         DID WE FIND SPECIAL CHARS                    
         BE    EMVALERR            NO SPCL CHAR FND, ERROR.                     
         CLI   0(R3),C'@'          HAVE WE REACHED @ YET                        
         BNE   EMVAL6AA                                                         
         TM    FLGEM,ATFOUND       MAKE SURE NO MORE THAN ONE @ SIGN            
         BO    EMVALERR                                                         
         OI    FLGEM,ATFOUND       NOW WE HAVE ONE @ IN E-MAIL                  
EMVAL6AA CLC   0(1,R1),0(R3)       IS IT SPCL CHAR                              
         BE    EMVAL60C                                                         
EMVAL60B LA    R1,1(R1)            POINT TO NEXT TABLE ENTRY                    
         B     EMVAL60A                                                         
*                                                                               
EMVAL60C DS    0H                  MAKE SURE NO 2 SPCL CHRS APEAR TOGE          
         LA    R5,EXCTAB                                                        
EMVAL60D CLI   0(R5),X'FF'                                                      
         BE    EMVAL60E                                                         
         CLC   1(1,R3),0(R5)       ARE BOTH SPECIAL CHARS                       
         BE    EMVALERR                                                         
         LA    R5,1(R5)            POINT TO NEXT CHAR IN TAB                    
         B     EMVAL60D                                                         
*                                                                               
EMVAL60E B     EMVAL50                                                          
*                                                                               
EMVALGD  DS    0H                                                               
         TM    FLGEM,ATFOUND       SHOULD HAVE ONE @ IN E-MAIL                  
         BZ    EMVALERR            NO @ FOUND ERROR                             
         CR    RB,RB                                                            
         B     *+6                                                              
*                                                                               
EMVALERR CR    RB,RD                                                            
EMVALX   XIT1                                                                   
EXCTAB   DC    C'@'                @ SIGN                                       
         DC    C'_'                UNDERSCORE                                   
         DC    C'.'                DOT                                          
         DC    X'FF'               END OF TABLE                                 
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -             
*-------------------------------------------------------------------*           
*        CHECK IF OVERHEAD ACCOUNT                                              
*-------------------------------------------------------------------*           
CHKOVER  NTR1  BASE=*,LABEL=*                                                   
         MVI   ERROR,0                                                          
         LA    R4,SAVEHEIR         CHECK IF AMENDING/ADDING LOW                 
         USING ACLELD,R4           LEVEL ACCOUNT                                
         LA    R3,LOGACC           ACCOUNT FIELD FROM SCREEN                    
         LA    R2,LOGACCH                                                       
         SR    R6,R6               DISPLACEMENT INTO ACCT FOR COMP              
         ZIC   R5,ACLVALS          LENGTH FOR COMPARE                           
         SR    R5,R6               LENGTH FOR COMPARE                           
         BAS   RE,CHKO50                                                        
*                                                                               
         ZIC   R6,ACLVALS          DISPLACEMENT INTO ACCT FOR COMP              
*                                  LENGTH FOR COMPARE                           
         ZIC   R5,ACLVALS+(L'ACLVALS)                                           
         SR    R5,R6               LENGTH FOR COMPARE                           
         BAS   RE,CHKO50                                                        
*                                                                               
*                                  DISPLACEMENT INTO ACCT FOR COMP              
         ZIC   R6,ACLVALS+(L'ACLVALS)                                           
*                                  LENGTH FOR COMPARE                           
         ZIC   R5,ACLVALS+(L'ACLVALS*2)                                         
         SR    R5,R6               LENGTH FOR COMPARE                           
         BAS   RE,CHKO50                                                        
*                                                                               
*                                  MUST BE LEVEL D ACCOUNT                      
         ZIC   R6,ACLVALS+(L'ACLVALS*2)                                         
         LA    R5,3                ONLY COMPARE FOR MINIMUM OF 3                
         BAS   RE,CHKO50                                                        
         B     XIT                                                              
*                                                                               
CHKO50   NTR1                                                                   
         AR    R3,R6               BUMP DISPLACEMENT                            
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R3),=12CL1'9'                                                
         BNE   XIT                                                              
         MVI   ERROR,X'FE'         INDICATE OVERHEAD ACCOUNT                    
         B     XIT                                                              
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
*-------------------------------------------------------------------*           
*        GET ACCOUNT LEVEL                                                      
*-------------------------------------------------------------------*           
*                                                                               
         USING ACLELD,R4                                                        
*                                                                               
GETACTLV NTR1  BASE=*,LABEL=*                                                   
         MVC   KEY,IO2             IF    RECORD HAS NO BALANCE ELEMENT          
         LA    R4,SAVEHEIR               GET ACCOUNT LEVEL                      
         ZIC   R3,ACLVALS                                                       
         LA    R5,KEY+3(R3)                                                     
         CLI   ACLVALS,12                                                       
         BE    GA48                                                             
         CLI   0(R5),C' '                                                       
         BE    GA48                LEVEL B IS BLANK THIS IS LEVEL A             
         ZIC   R3,ACLVALS+(L'ACLVALS)                                           
         LA    R5,KEY+3(R3)                                                     
         CLI   ACLVALS+(L'ACLVALS),12                                           
         BE    GA48                                                             
         CLI   0(R5),C' '                                                       
         BE    GA48                LEVEL C IS BLANK THIS IS LEVEL B             
         ZIC   R3,ACLVALS+(L'ACLVALS*2)                                         
         LA    R5,KEY+3(R3)                                                     
         CLI   ACLVALS+(L'ACLVALS*2),12                                         
         BE    GA48                                                             
         CLI   0(R5),C' '                                                       
         BE    GA48                LEVEL D IS BLANK THIS IS LEVEL C             
         LA    R3,12               LOW   LEVEL ACCOUNT                          
         LA    R5,KEY+3(R3)                                                     
                                                                                
GA48     ZIC   R0,0(,R5)           READ  NEXT  UNDELETED RECORD                 
         AHI   R0,1                                                             
         STC   R0,0(,R5)                                                        
         GOTO1 HIGH                INTO  IO                                     
         MVI   ERROR,67                                                         
         LA    R4,IO                                                            
                                                                                
         LA    R3,2(,R3)           LENGTH FOR COMPARE                           
         EXCLC R3,0(R4),KEYSAVE                                                 
         BE    GAEX                                                             
*                                                                               
         MVC   KEY,IO2             NO    LOWER RECORDS                          
         LA    RF,IO2                                                           
         OI    44(RF),X'80'        SO,   OK TO DELETE THIS ONE                  
         MVI   ERROR,X'FF'                                                      
*                                                                               
GAEX     B     XIT                                                              
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*              SPECIAL ROUTINES FOR HEIRARCHY                                   
*--------------------------------------------------------------------*          
HEIRARC  DS    0D                                                               
         NMOD1 0,**HEIRA**,R9                                                   
         L     RC,0(R1)                                                         
         L     R8,4(R1)                                                         
                                                                                
         MVI   ERROR,X'FF'                                                      
         CLI   ACTION,C'N'                                                      
         BNE   XITA                                                             
                                                                                
         USING ACLELD,R4                                                        
         LA    R4,SAVEHEIR                                                      
         LA    R3,LOGACC           ACCOUNT FIELD FROM SCREEN                    
         LA    R2,LOGACCH                                                       
         SR    R6,R6               DISPLACEMENT INTO ACCT FOR COMP              
         BAS   RE,CHKACC                                                        
         CLI   ERROR,ACCINVAL                                                   
         BE    XITA                                                             
*                                                                               
         ZIC   R6,ACLVALS          DISPLACEMENT INTO ACCT FOR COMP              
         LTR   R6,R6               DOES THIS LEVEL EXIST?                       
         BZ    ACS23A                                                           
         BAS   RE,CHKACC                                                        
         CLI   ERROR,ACCINVAL                                                   
         BE    XITA                                                             
*                                                                               
         ZIC   R6,ACLVALS+(L'ACLVALS)                                           
         LTR   R6,R6                                                            
         BZ    ACS23A                                                           
         BAS   RE,CHKACC                                                        
         CLI   ERROR,ACCINVAL                                                   
         BE    XITA                                                             
*                                                                               
         ZIC   R6,ACLVALS+(L'ACLVALS*2)                                         
         LTR   R6,R6                                                            
         BZ    ACS23A                                                           
         BAS   RE,CHKACC                                                        
         CLI   ERROR,ACCINVAL                                                   
         BE    XITA                                                             
         B     ACS23A                                                           
*                                                                               
CHKACC   NTR1                                                                   
         AR    R3,R6               BUMP DISPLACEMENT                            
         CLI   0(R3),C' '          NO SPACE IN FIRST POSTITION                  
         BNE   XITA                                                             
         MVI   ERROR,ACCINVAL      INVALID ACCOUNT                              
         B     XITA                                                             
*                                                                               
ACS23A   SR    R5,R5                                                            
         IC    R5,ACLVALS                                                       
         CLC   5(1,R2),ACLVALS                                                  
         BH    AC24                                                             
         CLI   ACLVALS+(L'ACLVALS),0                                            
         BNE   XITA                                                             
         GOTO1 BALIN                                                            
         B     XITA                                                             
                                                                                
AC24     CLI   ACLVALS+(L'ACLVALS),0                                            
         BE    TOOLON                                                           
         CLC   5(1,R2),ACLVALS+(L'ACLVALS)                                      
         BH    AC26                                                             
         CLI   ACLVALS+(L'ACLVALS*2),0                                          
         BNE   CHECKHIR                                                         
         GOTO1 BALIN                                                            
         B     CHECKHIR                                                         
                                                                                
AC26     IC    R5,ACLVALS+(L'ACLVALS)                                           
         CLI   ACLVALS+(L'ACLVALS*2),0                                          
         BE    TOOLON                                                           
         CLC   5(1,R2),ACLVALS+(L'ACLVALS*2)                                    
         BH    AC28                                                             
         CLI   ACLVALS+(L'ACLVALS*3),0                                          
         BNE   CHECKHIR                                                         
         GOTO1 BALIN                                                            
         B     CHECKHIR                                                         
                                                                                
AC28     IC    R5,ACLVALS+(L'ACLVALS*2)                                         
         CLI   ACLVALS+(L'ACLVALS*3),0                                          
         BE    TOOLON                                                           
         GOTO1 BALIN                                                            
                                                                                
CHECKHIR MVC   KEY,SPACES          CHECK IF HIGHER LEVEL IS THERE               
         MVC   KEY(3),IO2                                                       
         LA    RF,IO2                                                           
         BCTR  R5,R0                                                            
         EX    R5,*+4                                                           
         MVC   KEY+3(0),3(RF)                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(15),KEYSAVE                                                  
         BE    XITA                                                             
         MVI   ERROR,NOHIGHER                                                   
         B     XITA                                                             
                                                                                
TOOLON   MVI   ERROR,ACTOOLNG                                                   
                                                                                
XITA     XIT1  REGS=(R2)                                                        
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*              DSECT TO COVER THE POINTER TABLE                                 
*---------------------------------------------------------------------*         
PNTD     DSECT                                                                  
PNTTYP   DS    CL1           EQUATE                                             
PNTCODE  DS    CL8           CODE                                               
PNTLDG   DS    CL2           DEFAULT UNIT/LEDGER                                
PNTSTAT  DS    XL1           STATUS                                             
PNTFLE   EQU   X'80'         ACCOUNT MUST BE ON FILE                            
PNTBAL   EQU   X'40'         MUST HAVE BALANCE ELEMENT                          
PNTMAX   DS    CL1           MAXIMUM INPUT LENGHT                               
PNTFRUL  DS    AL2           LIST OF VALID U/L WHERE IT CAN BE FOUND            
PNTTOUL  DS    AL2           LIST OF VALID U/L TO WHICH IT CAN POINT            
PNTLNQ   EQU   *-PNTD                                                           
         EJECT ,                                                                
*---------------------------------------------------------------------*         
* DESECT TO COVER EQUIVALENT ACCOUNTS TABLE EQUATAB                             
*---------------------------------------------------------------------*         
EQUTBD   DSECT                                                                  
EQUSEQ   DS    XL1           SEQUENCE NUMBER                                    
EQUCODE  DS    CL10          ACCOUNT/SYSTEM NAME EG HYPERION=                   
EQULNQ   EQU   *-EQUTBD                                                         
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*              DSECT FOR LOCAL W/S                                              
*---------------------------------------------------------------------*         
LWSD     DSECT                                                                  
VSCANNER DS    V                                                                
VUNSCAN  DS    V                                                                
VDICTAT  DS    V                                                                
VGETTXT  DS    V                                                                
PRELOC   DS    F                                                                
SAVRB    DS    F                   ENTRY POINT                                  
*                                                                               
DUPSW    DS    X                   CHECK DUPLICATE INPUT                        
DUPCST   EQU   X'80'               COST=                                        
DUPANL   EQU   X'40'               ANALYSIS=                                    
DUP1099  EQU   X'20'               1099= TAX FORMS                              
DUPTIME  EQU   X'10'               T/S=  TIME SHEETS                            
DUPMAIL  EQU   X'02'               MAIL=                                        
DUPTASK  EQU   X'01'               TASK=                                        
*                                                                               
ELCODE   DS    CL1                                                              
*                                                                               
BYTE     DS    CL1                                                              
FNDFAX   EQU   X'80'               FOUND FAX  PARAMETER                         
FNDCOMM  EQU   X'40'               FOUND COMM PARAMETER                         
FNDEMAIL EQU   X'20'               FOUND EMAIL PARAMETER                        
*                                                                               
BYTEB    DS    CL1                                                              
DRFTITMS DS    CL1                                                              
*                                  SCAN/UNSCAN BLOCK                            
*                                  LENGTH = MAX SCAN LINES *                    
*                                           LENGTH OF A SCAN LINE               
BLOCK2   DS    (MAXSCNLN*LSCNLINE)C                                             
VATBLOCK DS    (VTCLNQ)C           SPACE FOR VATICAN CONTROL BLOCK              
VATFLDH  DS    CL9                 DUMMY FIELD HDR TO PASS TO VATICAN           
SAVEDATE DS    CL3                 SAVE YMD DATE                                
TMPPROV  DS    CL2                 TEMPORARY PLACE FOR PROVINCE CODE            
SAVEKEY  DS    CL42                                                             
*                                                                               
DICO     DS    0C                                                               
AC@PST   DS    CL3                                                              
AC@QST   DS    CL3                                                              
AC@ONT   DS    CL3                                                              
AC@HST   DS    CL3                                                              
LWSX     DS    0C                                                               
         EJECT ,                                                                
*                                                                               
       ++INCLUDE ACLFMFFD                                                       
         ORG   LOGTABH                                                          
       ++INCLUDE ACLFMFCD                                                       
*                                                                               
LEVELS   DS    0XL1                                                             
LEVA     DS    XL1                 LEVEL A LENGTH                               
LEVB     DS    XL1                 LEVEL B LENGTH (A+B)                         
LEVC     DS    XL1                 LEVEL C LENGTH (A+B+C)                       
LEVD     DS    XL1                 LEVEL D LENGTH (A+B+C+D)                     
LEVLNQ   EQU   *-LEVELS                                                         
*                                                                               
LEVLNQS  DS    0XL1                                                             
LEVLNQA  DS    XL1                 LEVEL A INDIVIDUAL LENGTH                    
LEVLNQB  DS    XL1                 LEVEL B INDIVIDUAL LENGTH                    
LEVLNQC  DS    XL1                 LEVEL C INDIVIDUAL LENGTH                    
LEVLNQD  DS    XL1                 LEVEL D INDIVIDUAL LENGTH                    
*                                                                               
LEVNUM   DS    XL1                 NUMBER OF LEVELS IN HEIRARCHY                
LEVELQ   EQU   4                   MAXIMUM NUMBER OF LEVELS                     
*                                                                               
SAVEHEIR DS    CL250                                                            
SAVECOMP DS    CL64                                                             
KEY2     DS    CL56                                                             
HOLDKEY  DS    CL32                                                             
MYELEM   DS    CL87                                                             
BYTECNTR DS    CL1                 TIMES TO PASS THROUGH THIS LOOP              
SAVESTR  DS    CL3                 SAVE YMD DATE START                          
SAVEEND  DS    CL3                 SAVE YMD DATE END                            
BIT      DS    XL1                                                              
DUPPROF  EQU   X'80'               DUPLICATE ACCT PROFILE                       
*                                                                               
EQUACNT  DS    XL1                 EQUIVALENT ACC TAB ENTRY COUNT               
EQUATAB  DS    0CL(EQUMAX*EQULNQ)                                               
         DS    (EQUMAX)CL(EQULNQ)  1 BYTE SEQUENCE #, 10 BYTE ACC NAME          
EQUMAX   EQU   5                   MAX IS 5, ONLY 5 FIELDS ON SCREEN            
SVSEQ    DS    XL1                 SAVED SEQUENCE NUMBER                        
ACAELEM  DS    A                                                                
AHEADER  DS    A                   ADDRESS OF CURRENT EQV ACC HEADER            
NLEVS    DS    XL1                 TOTAL SAVED LEVELS LEFT X'CA' ELEM           
ERRBLK   DS    CL64                OPTION ERROR BLOCK                           
*                                                                               
MAXLSTLN EQU   7                   LENGTH OF LIST TEXT STRING                   
FLGEM    DS    XL1                 FLAG FOR EMAIL VALIDATION                    
ATFOUND  EQU   X'80'               @ SIGN FOUND                                 
*                                                                               
         EJECT ,                                                                
       ++INCLUDE ACLFMWORK                                                      
         EJECT ,                                                                
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* ACLFMEQU                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACLFMEQU                                                       
         PRINT ON                                                               
* DDFLDIND                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDFLDIND                                                       
         PRINT ON                                                               
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
* ACVATICAND                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACVATICAND                                                     
         PRINT ON                                                               
* DDFLDHDR                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDFLDHDR                                                       
         PRINT ON                                                               
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
*********** ++INCLUDE ACMSGEQUS REMOVED BY DEIS                                 
*********** IT IS ALREADY INCLUDED IN ACLFMWORK                                 
* ACMSGEQUS                                                                     
         PRINT OFF                                                              
****   ++INCLUDE ACMSGEQUS                                                      
         PRINT ON                                                               
* DDCONBLK                                                                      
         PRINT OFF                                                              
CONBLKD  DSECT                                                                  
       ++INCLUDE DDCONBLK                                                       
         PRINT ON                                                               
* FAGETTXTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
* FATIOB                                                                        
         PRINT OFF                                                              
       ++INCLUDE FATIOB                                                         
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'023ACLFM03   02/21/15'                                      
         END                                                                    
