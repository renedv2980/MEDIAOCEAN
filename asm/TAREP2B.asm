*          DATA SET TAREP2B    AT LEVEL 212 AS OF 03/12/15                      
*PHASE T7032BD,*                                                                
*INCLUDE TALIM                                                                  
*INCLUDE TA2BAP                                                                 
*INCLUDE TA2BUC                                                                 
*INCLUDE TA2BUR                                                                 
*INCLUDE TA2BNY                                                                 
*INCLUDE ADSCAN                                                                 
*INCLUDE DLFLD                                                                  
         TITLE 'T7032B - UNEMPLOYMENT HANDLER'                                  
T7032B   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 MYEND-MYSTART,T7032B,R7,R8,CLEAR=YES                             
         LR    RA,RC                                                            
         USING MYD,RA                                                           
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9                                                       
         L     R6,ATWA                                                          
         USING CONHEADH-64,R6                                                   
         ST    RB,MYRB                                                          
         ST    R7,MYR7                                                          
         ST    R8,MYR8                                                          
         GOTO1 INITIAL,DMCB,0                                                   
         SPACE 1                                                                
         CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
         BAS   RE,VREC                                                          
         BAS   RE,PREP                                                          
*                                                                               
         L     R1,ABUFFER                                                       
         LTR   R1,R1               LEAVE, IF NONE ALLOCATED                     
         BZ    XIT                                                              
         L     R0,LBUFFER                                                       
         FREEMAIN R,A=(1),LV=(0)                                                
*                                                                               
         B     XIT                                                              
         SPACE 1                                                                
*                                  PROGRAM IS HANDLING THE FOLLOWING            
UREPORT  EQU   37                  UNEMPLOYMENT REPORTS                         
UCTAPES  EQU   46                  UC TAPES                                     
UC941    EQU   47                  941 REPORTS                                  
AGYPROF  EQU   48                  AGENCY PROFITABILITY                         
NYDISK   EQU   65                  NY DISK                                      
UCDISK   EQU   72                  UC DISK                                      
         EJECT                                                                  
       ++INCLUDE TAREP1BE                                                       
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              HANDLE REPORT                                                    
         SPACE 3                                                                
PREP     NTR1                                                                   
         BAS   RE,MYINIT           INITIALIZE                                   
         BAS   RE,READCHEK         USE SYSIO FOR CHECKS/BILLS                   
         BAS   RE,DOREPS           THEN DO THE REPORTS                          
         B     XIT                                                              
         EJECT                                                                  
*              INITIAL FOR REPORTS                                              
         SPACE 3                                                                
MYINIT   NTR1                                                                   
         ZAP   TRACOUNT,=P'0'      SET COUNTERS                                 
         ZAP   RECCOUNT,=P'0'                                                   
         ZAP   REPCOUNT,=P'0'                                                   
         ZAP   CHKCOUNT,=P'0'                                                   
         ZAP   NTSCOUNT,=P'0'                                                   
         ZAP   OTSCOUNT,=P'0'                                                   
         ZAP   MOFCOUNT,=P'0'                                                   
         ZAP   INVCOUNT,=P'0'                                                   
         ZAP   SRTCOUNT,=P'0'                                                   
         ZAP   TAPCOUNT,=P'0'                                                   
*                                                                               
         MVC   THISSTR,TIQPSTR     ADJUST PERIODS                               
         MVC   THISEND,TIQPEND                                                  
         TM    STSTAT,STSTATIL                                                  
         BO    *+10                                                             
         MVC   TIQPSTR+1(2),=X'0101'                                            
         BAS   RE,MYCLEAR                                                       
         MVC   MYH6,MYSPACES                                                    
         MVC   MYH7,MYSPACES                                                    
         XC    BUFCOUNT,BUFCOUNT                                                
*                                                                               
         XC    STATEUC(216),STATEUC                                             
         XC    STATEUC+216(216),STATEUC+216                                     
*                                                                               
         L     R2,=A(MYBUFF)                                                    
         MVI   0(R2),0                                                          
*                                                                               
         L     R6,ATWA                                                          
         USING CONHEADH-64,R6                                                   
*                                                                               
         MVC   AMASTD,TWAMASTC     SAVE ADDRESS OF MASTER                       
*                                                                               
         L     R1,TWADCONS                                                      
         USING TWADCOND,R1                                                      
         MVC   DYNALLOC,TDYNALLO                                                
         MVC   ALOGOC,TLOGOC       SAVE A(LOGOC)                                
         DROP  R6                                                               
*                                                                               
         L     R1,AMASTD                                                        
         USING MASTD,R1                                                         
         MVC   ALOGO,MCVLOGO       SAVE A(LOGO)                                 
         MVC   AREMOT,MCVREMOT     SAVE A(REMOTE)                               
         DROP  R1                                                               
*                                                                               
         L     R5,ASPOOLD                                                       
         USING SPOOLD,R5                                                        
*        CLI   ACTEQU,ACTDOWN                                                   
*        BE    INIT05                                                           
         LA    R1,MYSPECS                                                       
         ST    R1,SPECS                                                         
         LA    R1,HOOK             INITIALIZE FOR REPORT                        
         ST    R1,HEADHOOK                                                      
INIT05   MVI   RCSUBPRG,1                                                       
         CLI   RECNUM,UC941                                                     
         BE    INIT10                                                           
         CLI   RECNUM,UCDISK                                                    
         BE    INIT10                                                           
         CLI   RECNUM,UCTAPES                                                   
         BNE   INIT20                                                           
         SPACE 1                                                                
INIT10   MVI   RCSUBPRG,2                                                       
         SPACE 1                                                                
INIT20   MVC   MYSORTER,SORTER                                                  
         DROP  R5                                                               
         MVC   MYTITLE,MYSPACES                                                 
         MVC   MYTITLE(12),=C'UNEMPLOYMENT'                                     
         SPACE 1                                                                
         XC    WEEKLIST,WEEKLIST   BUILD WEEK LIST                              
         GOTO1 DATCON,DMCB,(1,TIQPSTR),(20,WORK)                                
         MVC   RYEAR,WORK          DISPLAYABLE YEAR                             
         SPACE 1                                                                
         GOTO1 DATCON,DMCB,(1,TIQPSTR),(0,WORK)                                 
         MVC   WORK+2(4),=C'0101'                                               
         GOTO1 GETDAY,DMCB,WORK,WORK+6                                          
         CLI   DMCB,7              IS JAN01 DATE SUNDAY?                        
         BE    WKL2                                                             
         ZIC   R0,DMCB             NO - SO SKIP TO IT                           
         LA    R1,7                                                             
         SR    R1,R0                                                            
         ST    R1,DMCB+8                                                        
         GOTO1 ADDAY,DMCB,WORK,WORK+6                                           
         MVC   WORK(6),WORK+6                                                   
         SPACE 1                                                                
WKL2     LA    R2,WEEKLIST         NOW BUILD 53 SUNDAYS - PACKED                
         LA    R3,7                                                             
         LA    R0,53                                                            
         SPACE 1                                                                
WKL4     GOTO1 DATCON,DMCB,(0,WORK),(1,0(R2))                                   
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(R3)                                      
         MVC   WORK(6),WORK+6                                                   
         LA    R2,3(R2)                                                         
         BCT   R0,WKL4                                                          
         SPACE 1                                                                
         GOTO1 DATCON,DMCB,(1,THISEND),(3,WORK)                                 
         MVC   QMONTH,WORK+1       MONTH NUMBER FROM END DATE                   
         ZIC   R1,QMONTH                                                        
         BCTR  R1,0                                                             
         SR    R0,R0                                                            
         D     R0,=F'3'                                                         
         LA    R1,1(R1)                                                         
         STC   R1,QQUART           QUARTER # FROM MONTH                         
         SPACE 1                                                                
         XC    QMTH1(3),QMTH1      PRE CLEAR MONTH NUMBERS                      
         CLI   RECNUM,UCTAPES      FOR UCTAPE OR UCDISK                         
         BE    *+12                                                             
         CLI   RECNUM,UCDISK                                                    
         BNE   MYINITX                                                          
*                                                                               
         LA    R0,3                SAVE EACH MONTH IN QTR USING END DTE         
         LA    RE,QMTH3                                                         
         ZIC   R1,QMONTH                                                        
INIT30   STC   R1,0(RE)                                                         
         BCTR  RE,0                BUMP TO PREVIOUS MONTH                       
         BCT   R1,*+8              DECREMENT MONTH NUMBER                       
         B     MYINITX                                                          
         BCT   R0,INIT30                                                        
         SPACE 1                                                                
MYINITX  LA    R2,LIMAREA          SET UP TALIM BLOCK                           
         USING TMD,R2                                                           
         XCEF  (R2),'TMLNQ'                                                     
         ST    RC,TMRC                                                          
         GOTO1 DATCON,DMCB,(1,TIQPEND),(0,TMEFDTE0)                             
         MVC   TMEFDTE0+2(4),=C'1231'                                           
         CLI   TRACOPT,C'Y'        IF REQUESTING TRACE                          
         BNE   XIT                                                              
         MVI   TMTRACE,C'Y'        SET TRACE FOR TALIM TOO                      
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*              READ CHECK RECORDS FROM DISK                                     
         SPACE 3                                                                
READCHEK NTR1                                                                   
         BAS   RE,SETSKIP                                                       
         MVC   TIACOMFC,ACOMFACS   SET UP FOR TASYSIO                           
         LA    R1,CHEKHOOK                                                      
         ST    R1,TIHOOK                                                        
         XC    THISSSN,THISSSN                                                  
         SPACE 1                                                                
         MVI   TIREAD,TLCKCDQ      SET TO READ CHECKS                           
         CLI   RECNUM,NYDISK       FOR NEW YORK DISK                            
         BNE   READCK05                                                         
         MVI   TIREAD,TLCKYCDQ     SET TO READ PASSIVE PTRS                     
         MVI   TIFCUR,C'U'         US DOLLARS ONLY                              
         MVI   TIFTUNIT,TACWSTAX   FILTER ON TAXABLE STATE                      
         SPACE 1                                                                
READCK05 OI    TIFPDSN,TAPDSCAN    NO CANADIAN                                  
         OI    TIFPDSN,TAPDSCNL    NO CANCELS                                   
         NI    STSTAT,X'FF'-STSTATFL    TURN OFF FL STATUS                      
         CLI   RECNUM,UCDISK       UCDISK                                       
         BNE   READCK08                                                         
         CLC   TIFUNIT,=CL3'FL'    IF FLORIDA, READ ALL STATES                  
         BNE   READCK07                                                         
         CLI   STSTAT,0            TN STATUS SHOULD NOT BE SET!                 
         BE    *+6                                                              
         DC    H'00'                                                            
         XC    TIFUNIT,TIFUNIT                                                  
         OI    STSTAT,STSTATFL     TURN ON FLORIDA STATUS                       
         B     READCK08                                                         
READCK07 CLC   TIFUNIT,=CL3'TN'    IF TENNESSEE, CHECK FOR FL FMT OPT           
         BNE   READCK08                                                         
         TM    STSTAT,STSTATTN     IF FL FMT OPTION ON, READ ALL STATES         
         BZ    READCK08                                                         
         XC    TIFUNIT,TIFUNIT                                                  
READCK08 CLI   RECNUM,AGYPROF      IF NOT AGENCY PROFITABLITY                   
         BE    *+12                                                             
         OI    TIFPDPN,TAPDPBNP    NO BNP                                       
         B     *+8                                                              
         OI    TIQFLAGS,TIQFPBNP   ELSE, SET TO PASS BNP                        
         OI    TIFPDPN,TAPDPCRD    NO CREDITS                                   
         OI    TIFPO3N,TAPDODUM    NO DUMMYS                                    
         CLI   RECNUM,UCTAPES      FOR UCTAPES - ALWAYS READ GREY               
         BE    READCK10                                                         
         CLI   RECNUM,UCDISK       SAME FOR UCDISK                              
         BE    READCK10                                                         
         CLI   RECNUM,UC941                                                     
         BE    READCK10                                                         
         CLI   GREYOPT,C'Y'        IF GREY OPTION SET,                          
         BNE   *+8                                                              
READCK10 OI    TIQFLAG2,TIQFPGRY   TELL SYSIO TO PASS GREY RECORDS              
READCK12 GOTO1 TASYSIO,DMCB,TASYSIOD                                            
         CLI   RECNUM,NYDISK                                                    
         BNE   CKEOF2                                                           
         CLC   TIFUNIT,=C'NY '                                                  
         BNE   CKEOF2                                                           
         MVC   TIFUNIT,=C'NYC'                                                  
         B     READCK12                                                         
         SPACE 1                                                                
CHEKHOOK NTR1                                                                   
         CLI   TIMODE,PROCINV      CAN GET INVOICES FOR AGYPROF                 
         BE    BILLHOOK                                                         
         CLI   TIMODE,PROCREC      IF THERE IS SOMETHING INTERESTING            
         BNE   XIT                                                              
         AP    TAPCOUNT,=P'1'                                                   
         L     R6,TIAREC                                                        
         ST    R6,ACHECK                                                        
         BAS   RE,PROCCHEK                                                      
         B     XIT                                                              
         SPACE 1                                                                
BILLHOOK DS    0H                                                               
         CLI   RECNUM,AGYPROF      IF NOT AGENCY PROFITABLITY                   
         BNE   XIT                 EXIT                                         
         MVC   ABILL,TIAREC                                                     
         CLI   TIOFF,C'A'          NEED A VALID OFFICE                          
         BL    XIT                                                              
         AP    INVCOUNT,=P'1'                                                   
         GOTO1 =V(TA2BAP),DMCB,(3,(RC)),(RA)                                    
         B     XIT                                                              
         EJECT                                                                  
*              COUNTS AT CHECK EOF                                              
         SPACE 3                                                                
CKEOF2   CLI   TRACOPT,C'Y'                                                     
         BNE   CKEOF4                                                           
         MVC   MYP(24),=C'CHECK READING COMPLETE  '                             
         BAS   RE,SPLAT                                                         
         MVC   MYP(24),=C'----------------------  '                             
         BAS   RE,SPLAT                                                         
         BAS   RE,SPLAT                                                         
         EDIT  (P6,TAPCOUNT),(8,MYP)                                            
         MVC   MYP+9(12),=C'TAPE RECORDS'                                       
         BAS   RE,SPLAT                                                         
         EDIT  (P6,CHKCOUNT),(8,MYP)                                            
         MVC   MYP+9(16),=C'CHECKS PROCESSED'                                   
         BAS   RE,SPLAT                                                         
         EDIT  (P6,INVCOUNT),(8,MYP)                                            
         MVC   MYP+9(16),=C'BILLS PROCESSED '                                   
         BAS   RE,SPLAT                                                         
         EDIT  (P6,SRTCOUNT),(8,MYP)                                            
         MVC   MYP+9(16),=C'SORTS PROCESSED '                                   
         BAS   RE,SPLAT                                                         
         EDIT  (P6,NTSCOUNT),(8,MYP)                                            
         MVC   MYP+9(16),=C'NO TAXABLE STATE'                                   
         BAS   RE,SPLAT                                                         
         EDIT  (P6,OTSCOUNT),(8,MYP)                                            
         MVC   MYP+9(16),=C'OT TAXABLE STATE'                                   
         BAS   RE,SPLAT                                                         
         EDIT  (P6,MOFCOUNT),(8,MYP)                                            
         MVC   MYP+9(16),=C'MISSING OFFICE  '                                   
         BAS   RE,SPLAT                                                         
         SPACE 1                                                                
CKEOF4   ZAP   TAPCOUNT,=P'0'                                                   
         ZAP   CHKCOUNT,=P'0'                                                   
         ZAP   SRTCOUNT,=P'0'                                                   
         B     XIT                                                              
         EJECT                                                                  
*              PROCESS A CHECK RECORD                                           
         SPACE 3                                                                
*              INPUT               ACHECK=A(CHECK RECORD)                       
         SPACE 1                                                                
PROCCHEK NTR1                                                                   
         XC    EXTDATA,EXTDATA                                                  
         XC    THISTAX,THISTAX                                                  
         XC    THISEARN,THISEARN                                                
         XC    YTDSEARN,YTDSEARN                                                
         XC    YTDFEARN,YTDFEARN                                                
         MVC   EXTYEAR,RYEAR+2     DISPLAYABLE YEAR                             
         L     R4,ACHECK                                                        
         USING TLCKD,R4                                                         
*&&DO                                                                           
* TEST GH                                                                       
         CLI   RECNUM,UC941                                                     
         BE    *+8                                                              
         CLI   RECNUM,UCDISK       NOT FOR UK (UC DISK)                         
         BE    *+8                                                              
         CLI   RECNUM,UCTAPES      NOT FOR UC                                   
         BNE   PCCA                                                             
         CLC   TLCKSSN,=CL9'126369789'                                          
         BE    XIT                                                              
         CLC   TLCKSSN,=CL9'123369789'                                          
         BE    XIT                                                              
*        CLC   TLCKAGY,=CL6'999999'                                             
*        BE    XIT                                                              
* TEST GH                                                                       
*&&                                                                             
PCCA     LR    R6,R4                                                            
         MVI   ELCODE,TACDELQ      MUST BE CHECK DETAILS                        
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
**NO-OP**USING TACDD,R6                                                         
*        OC    TACDEARN,TACDEARN                                                
*        BNZ   PCCB                                                             
*        OC    TACDNTAX,TACDNTAX                                                
*        BNZ   PCCB                                                             
**NO-OP**B     XIT                                                              
         SPACE 1                                                                
PCCB     LR    R6,R4                                                            
         MVI   ELCODE,TAPDELQ      MUST BE PAY DETAILS                          
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING TAPDD,R6                                                         
         TM    TAPDPST1,TAPDPBNP   IGNORE BNP CHECKS                            
         BO    XIT                                                              
         CLI   RECNUM,AGYPROF      UNLESS AGENCY PROFITABILITY                  
         BE    PCC1                                                             
         CLI   TAPDW4TY,TAW4TYCO   NO CORPS                                     
         BE    XIT                                                              
         CLI   TAPDW4TY,TAW4TYCA   OR CANADIAN                                  
         BE    XIT                                                              
         CLI   TAPDW4TY,TAW4TYTR   OR TRUSTEE                                   
         BE    XIT                                                              
         CLI   TAPDW4TY,TAW4TYFO   OR FOREIGN                                   
         BE    XIT                                                              
         SPACE 1                                                                
PCC1     AP    CHKCOUNT,=P'1'                                                   
         MVC   EXTAGY,TIAGY                                                     
         MVC   EXTSSN,TISSN                                                     
         MVC   EXTEMP,TIEMP                                                     
         MVC   EXTSTATE,=C'999'                                                 
*                                                                               
         CLI   RECNUM,NYDISK                                                    
         BNE   PCC1A                                                            
         CLC   TIFUNIT,=C'NYC'                                                  
         BNE   *+10                                                             
         MVC   EXTSTATE,=C'NY '                                                 
*                                                                               
PCC1A    XC    TATUPTR,TATUPTR                                                  
         XC    SVTUUNIT,SVTUUNIT                                                
                                                                                
         USING TAPDD,R6                                                         
         MVI   TGBYTE,0                                                         
         LR    R6,R4                                                            
         MVI   ELCODE,TAPDELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   TGBYTE,TAPDADJS                                                  
                                                                                
         CLC   =C'P+',TIEMP                                                     
         BNE   PCC1TUX                                                          
* DRIVE THIS OFF TATU ELEMENTS                                                  
         LR    R6,R4                                                            
         MVI   ELCODE,TATUELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   PCC1TUX                                                          
         B     *+16                                                             
PCC1TU   L     R6,TATUPTR                                                       
         MVI   ELCODE,TATUELQ                                                   
PCC1TU0  BAS   RE,NEXTEL                                                        
         BE    *+16                                                             
         TM    TGBYTE,TAPDADST     SSN TRANSFER?                                
         BO    XIT                                                              
         B     PCCENDTU                                                         
         USING TATUD,R6                                                         
         MVC   SVTUUNIT,TATUUNIT                                                
                                                                                
         CLI   RECNUM,NYDISK                                                    
         BNE   PCC1TU1                                                          
         CLC   TIFUNIT,=C'NYC'     2ND TIME THROUGH, DON'T GET EARNINGS         
         BNE   PCC1TU1                                                          
         CLC   TATUUNIT,=C'NYC'                                                 
         BE    PCC1TUX                                                          
         B     PCC1TU0                                                          
                                                                                
****     MVC   THISEARN,TATUWAGE                                                
*                                                                               
*NEW REIM CODE PER JBAS                                                         
PCC1TU1  ZICM  RE,TATUTNAD,(15)                                                 
         ZICM  RF,TATUWAAD,(15)                                                 
         AR    RE,RF                                                            
         STCM  RE,15,THISEARN                                                   
         ZICM  RE,TATUNNAD,(15)                                                 
         ZICM  RF,TATUTNAD,(15)                                                 
         SR    RE,RF                                                            
         STCM  RE,15,THISNTAX                                                   
         CLC   TATUTNWA,TATUTNAD    INDIVIDUALS TAKE FROM TATUNNWA              
         BNE   *+10                                                             
         MVC   THISNTAX,TATUNNAD                                                
*                                                                               
         ST    R6,TATUPTR                                                       
         OC    TIFUNIT,TIFUNIT                                                  
         BZ    *+14                                                             
         CLC   TIFUNIT,TATUUNIT     FILTER ON TAXABLE STATE                     
         BNE   PCC1TU                                                           
         CLC   =C'FD',TATUUNIT      SKIP FD JUST LIKE TACW                      
         BE    PCC1TU                                                           
* NEWSTEST CODE ADDED 12/18/12                                                  
         CLI   TATUUNIT+2,C' '     LOOK FOR STATE                               
         BH    PCC1TU                                                           
*                                                                               
         CLC   =C'CN',TATUUNIT      SKIP CN JUST LIKE TACW                      
         BE    PCC1TUA                                                          
         MVC   TGCTRY,=C'CA'        AND CANADIAN PROVINCES                      
         GOTO1 TAXVAL,DMCB,(X'FF',TATUUNIT)                                     
         BNE   PCC1TUX                                                          
PCC1TUA  XC    THISEARN,THISEARN                                                
         XC    THISNTAX,THISNTAX                                                
         B     PCC1TU                                                           
*                                                                               
PCC1TUX  LR    R6,R4                                                            
         MVI   ELCODE,0                                                         
         BAS   RE,GETEL                                                         
         B     *+8                                                              
         SPACE 1                                                                
PCC2     BAS   RE,NEXTEL                                                        
         BNE   PCCEND                                                           
*                                                                               
         CLI   0(R6),TACAELQ                                                    
         BE    PCCCA                                                            
         CLI   0(R6),TACDELQ                                                    
         BE    PCCCD                                                            
         CLI   0(R6),TAPDELQ                                                    
         BE    PCCPD                                                            
         CLI   0(R6),TACWELQ                                                    
         BE    PCCCW                                                            
         CLI   0(R6),TACYELQ                                                    
         BE    PCCCY                                                            
         CLI   0(R6),TATIELQ                                                    
         BE    PCCTI                                                            
         CLI   0(R6),TAYEELQ                                                    
         BE    PCCYE                                                            
         B     PCC2                                                             
         SPACE 1                                                                
         SPACE 1                                                                
         USING TACAD,R6                                                         
PCCCA    MVC   EXTUN,TACAUN                                                     
         MVC   EXTONOFF,TACAONOF                                                
         MVC   EXTUNIT,TACAUNIT                                                 
         B     PCC2                                                             
         SPACE 1                                                                
         USING TACDD,R6                                                         
PCCCD    MVC   EXTCDTE,TACDDTE                                                  
         OC    SVTUUNIT,SVTUUNIT    FOR  P+ - WE TAKE EARNINGS FROM             
         BNZ   PCC2                 TATUWAGE                                    
         CLI   RECNUM,NYDISK                                                    
         BNE   *+14                                                             
         CLC   TIFUNIT,=C'NYC'     2ND TIME THROUGH, DON'T GET EARNINGS         
         BE    PCC2                                                             
         MVC   THISEARN,TACDEARN                                                
         MVC   THISNTAX,TACDNTAX                                                
         B     PCC2                                                             
         SPACE 1                                                                
         USING TAPDD,R6                                                         
PCCPD    OC    TAPDEOR,TAPDEOR                                                  
         BZ    *+10                                                             
         MVC   EXTEMP,TAPDEOR                                                   
         MVC   EXTW4TY,TAPDW4TY                                                 
         MVC   EXTADJS,TAPDADJS                                                 
         MVC   EXTOFF,TAPDOFF                                                   
         MVC   EXTUSE,TAPDUSE                                                   
         B     PCC2                                                             
         SPACE 1                                                                
         USING TACWD,R6                                                         
PCCCW    CLI   RECNUM,NYDISK                                                    
         BNE   PCCCW1                                                           
                                                                                
         CLC   TIFUNIT,=C'NYC'     IF 2ND TIME THROUGH, ONLY WANT NYC           
         BNE   *+14                THEN ALREADY PROCESSED THIS                  
         CLC   TACWUNIT,=C'NYC'                                                 
         BNE   PCC2                                                             
                                                                                
         CLC   TACWUNIT,=C'NYC'                                                 
         BNE   PCCCW1                                                           
         CLI   QQUART,4            IF NOT FOURTH QUARTER                        
         BNE   PCCCW0                                                           
         OC    TACWTAX,TACWTAX                                                  
         BZ    PCC2                                                             
         L     RF,THISTAX          ADD NYC TAX INTO NY TAX                      
         ICM   RE,15,TACWTAX                                                    
         AR    RF,RE                                                            
         ST    RF,THISTAX                                                       
PCCCW0   MVC   EXTSTATE,=C'NY '                                                 
         B     PCC2                                                             
                                                                                
PCCCW1   CLI   TACWUNIT+2,C' '     LOOK FOR STATE                               
         BH    PCC2                                                             
         CLC   TACWUNIT,=C'FD '                                                 
         BE    PCC2                                                             
         CLC   TACWUNIT,=C'CN '    ASSUME CANADA IS TAXABLE                     
         BNE   PCCCW2                                                           
         CLI   EXTSTATE,C'9'       UNLESS PREVIOUS TAXABLE FOUND                
         BNE   PCC2                                                             
         MVC   EXTSTATE,TACWUNIT                                                
*                                                                               
         B     PCC2                                                             
         SPACE 1                                                                
*  IF THERE IS A TATU - LOOK FOR TACW UNIT MATCHING THE TATU UNIT               
*  WE ARE CURRENTLY PROCESSING                                                  
*                                                                               
PCCCW2   DS    0C                                                               
                                                                                
         OC    SVTUUNIT,SVTUUNIT                                                
         BZ    *+14                                                             
         CLC   TACWUNIT,SVTUUNIT   ONLY LOOK FOR UNIT FOR THE TATU              
         BNE   PCC2                                                             
                                                                                
         TM    TACWSTAT,TACWSTAX   THAT IS TAXABLE                              
         BNO   PCC2                                                             
         MVC   EXTSTATE,TACWUNIT   PICK UP TAXABLE STATE                        
         MVC   THISTAX,TACWTAX                                                  
         B     PCC2                                                             
         SPACE 1                                                                
         USING TACYD,R6                                                         
PCCCY    CLC   =C'FD ',TACYUNIT    FEDERAL                                      
         BNE   PCCCY10                                                          
         MVC   YTDFEARN,TACYEARN   YES SO SAVE YTD FEDERAL EARNINGS             
         B     PCC2                                                             
         SPACE 1                                                                
PCCCY10  CLC   EXTSTATE,TACYUNIT   YTD - MATCH V TAXABLE STATE                  
         BNE   PCC2                                                             
         MVC   YTDSEARN,TACYEARN   YES SO SAVE YTD STATE EARNINGS               
         B     PCC2                                                             
         SPACE 1                                                                
         USING TATID,R6                                                         
PCCTI    CLI   TATITYPE,TATITYCO   MAY USE CORP SS#                             
         BNE   PCC2                                                             
         MVC   EXTSSN,TATIID                                                    
         B     PCC2                                                             
         SPACE 1                                                                
         USING TAYED,R6                                                         
PCCYE    CLI   TAYETYPE,TAYETCHK   GET AMOUNTS FROM CHECK TYPE                  
         BNE   PCC2                                                             
         MVC   EXTTXBL(EXTTXLNQ),TAYETXBL                                       
* ADJUST THE EARNINGS FOR P+ TO ONLY FOR THIS UNIT AND NOT ALL UNITS            
         OC    SVTUUNIT,SVTUUNIT                                                
         BZ    *+10                                                             
         MVC   EXTERN,THISEARN                                                  
*                                                                               
         CLC   TIFUNIT,=C'MN '     SPECIAL FOR MINNESOTA                        
         BE    PCCYEA                                                           
         CLC   EXTUNIT,=C'MN '                                                  
         BE    PCCYEA                                                           
         OC    TIFUNIT,TIFUNIT     IF RUNNING FOR ALL STATES,                   
         BNZ   PCC2                                                             
         CLC   EXTSTATE,=C'MN '    IS TAXABLE STATE MN?                         
         BNE   PCC2                                                             
PCCYEA   OC    EXTSUI,EXTSUI       WAS SUI WAGES FOR THIS CHECK ZERO?           
         BZ    PCCYE1                                                           
         CLC   EXTSUI,EXTERN       SUI WAS LESS THAN EARNED                     
         BNL   PCC2                NO, CONTINUE                                 
*                                                                               
PCCYE1   CLC   YTDSEARN,TAYESUI    DID TALENT GO OVER SUI LIMIT?                
         BNL   PCCYE2              YES, CONTINUE                                
         MVC   EXTSUI,EXTERN       NO, USE AMOUNT EARNED IN SUI WAGE            
         B     PCC2                                                             
*                                                                               
PCCYE2   XC    EXTSUI,EXTSUI       SEE IF STATE EARNING                         
         L     RF,YTDSEARN         SEE IF STATE EARNING                         
         S     RF,EXTERN           MINUS THIS CHECK IS OVER                     
         C     RF,TAYESUI          SUI LIMIT                                    
         BNL   PCC2                YES, DONE                                    
         L     RE,TAYESUI          NO, FIND REMAINDER AND                       
         SR    RE,RF                                                            
         ST    RE,EXTSUI           SAVE IT IN SUI WAGES                         
*                                                                               
         B     PCC2                                                             
         SPACE 1                                                                
PCCEND   CLI   RECNUM,AGYPROF      EXCEPT FOR AGENCY PROFITABLITY               
         BE    PCCEND1                                                          
         CLC   EXTSTATE,=C'999'    DROP IF DIDN'T FIND STATE                    
         BE    XIT                                                              
         SPACE 1                                                                
         TM    EXTADJS,TAPDADST          IF SSN TRANSFER                        
         BZ    PCCEND1                                                          
         OC    EXTTXBL(EXTTXLNQ),EXTTXBL AND NO TAXABLE AMTS FOR STATE          
         BNZ   PCCEND1                                                          
         LA    R1,EXTTXLNQ/L'EXTTXBL     R1=(N'AMOUNTS)                         
         LA    RE,EXTTXBL                RE=A(TAXABLE AMOUNTS)                  
PCCENDA  MVC   0(L'EXTTXBL,RE),THISEARN  USE TAXABLE EARNINGS FROM CHK          
         LA    RE,L'EXTTXBL(RE)          BUMP TO NEXT TAXABLE AMOUNT            
         BCT   R1,PCCENDA                LOOP                                   
         SPACE 1                                                                
PCCEND1  OC    THISEARN,THISEARN   IF NO GROSS OR NON TAX SKIP CHECK            
         BNZ   PCCEND1X                                                         
         OC    THISNTAX,THISNTAX                                                
         BNZ   PCCEND1X                                                         
         CLI   RECNUM,NYDISK       IF NYDISK, CHECK TAX TOO                     
         BE    PCCEND1B                                                         
*&&DO                                                                           
         CLI   RECNUM,UCTAPES      UNLESS THIS IS UCTAPE/UCDISK FOR CA          
         BE    *+12                                                             
         CLI   RECNUM,UCDISK       IN WHICH CASE NEED TO CHECK TAX TOO          
         BNE   XIT                                                              
         CLC   EXTSTATE(2),=C'CA'                                               
         BNE   XIT                                                              
*&&                                                                             
         CLI   RECNUM,UCTAPES      UNLESS THIS IS UCTAPE/UCDISK FOR CA          
         BE    PCCEND1A                                                         
         CLI   RECNUM,UCDISK       IN WHICH CASE NEED TO CHECK TAX TOO          
         BE    PCCEND1A                                                         
         OC    TATUPTR,TATUPTR     IF THERE IS TATU- GET NEXT TATU ELEM         
         BZ    XIT                 ELSE EXIT FOR NON P+                         
         B     PCCEND1Z                                                         
PCCEND1A CLC   EXTSTATE(2),=C'CA'                                               
         BE    PCCEND1B                                                         
         OC    TATUPTR,TATUPTR     IF THERE IS TATU- GET NEXT TATU ELEM         
         BZ    XIT                 ELSE EXIT FOR NON P+                         
         B     PCCEND1Z                                                         
PCCEND1B DS    0C                                                               
***      OC    THISTAX,THISTAX                                                  
***      BZ    XIT                                                              
         OC    THISTAX,THISTAX           IF NO TATU THEN CONTINUE               
         BNZ   PCCEND1X                  TO EXIT AS BEFORE                      
         OC    TATUPTR,TATUPTR           ELSE GO PROCESS NEXT TATU ELEM         
         BZ    XIT                                                              
         B     PCCEND1Z                                                         
*                                                                               
PCCEND1X BAS   RE,POSTSUI                                                       
         BRAS  RE,POSTDATE                                                      
         MVI   FTYPE,C'R'                                                       
         TM    STSTAT,STSTATFL+STSTATTN   IF FLORIDA OR TN, NOT SPECIAL         
         BNZ   PCCEND1Y                                                         
         BAS   RE,SPECFORM         FIND OUT IF THIS IS A SPECIAL FORM           
         BNE   *+8                                                              
         MVI   FTYPE,C'S'                                                       
PCCEND1Y BRAS  RE,POSTDETS                                                      
                                                                                
* NOT SURE IF THIS IS THE RIGHT PLACE TO BRANCH BACK UP                         
PCCEND1Z OC    TATUPTR,TATUPTR                                                  
         BNZ   PCC1TU                                                           
                                                                                
PCCENDTU DS    0C                                                               
*                                                                               
         CLI   RECNUM,AGYPROF      AP NOT INTERESTED IN STATE DRAMAS            
         BE    PCCEND4                                                          
         CLC   EXTSTATE(2),=C'CN'  REPORT IF TAXABLE STATE CANADA               
         BE    PCCEND3                                                          
         CLI   EXTSTATE,C'9'       REPORT IF TAXABLE STATE MISSING              
         BNE   PCCEND2                                                          
         AP    NTSCOUNT,=P'1'                                                   
         CP    NTSCOUNT,TRALIMIT                                                
         BH    PCCEND3                                                          
         MVC   RECTYPE,=CL16'NO TAXABLE STATE'                                  
         L     R6,ACHECK                                                        
         BAS   RE,TRACEREC                                                      
         SPACE 1                                                                
PCCEND2  CLC   EXTSTATE(2),=C'OT'  TRACE OT STATE                               
         BNE   PCCEND4                                                          
         AP    OTSCOUNT,=P'1'                                                   
         CP    OTSCOUNT,TRALIMIT                                                
         BH    PCCEND3                                                          
         MVC   RECTYPE,=CL16'OT TAXABLE STATE'                                  
         L     R6,ACHECK                                                        
         BAS   RE,TRACEREC                                                      
         SPACE 1                                                                
PCCEND3  MVC   MYP(13),=C'TAXABLE STATE'                                        
         MVC   MYP+14(2),EXTSTATE                                               
         MVC   MYP+20(7),=C'AGENCY='                                            
         MVC   MYP+27(6),EXTAGY                                                 
         MVC   MYP+35(8),=C'INVOICE='                                           
         L     R6,ACHECK                                                        
         MVI   ELCODE,TAPDELQ                                                   
         BAS   RE,GETEL                                                         
         USING TAPDD,R6                                                         
         GOTO1 TINVCON,DMCB,TAPDINV,MYP+43,DATCON                               
         MVC   MYP+50(4),=C'SS#='                                               
         MVC   MYP+54(9),TLCKSSN                                                
         MVC   MYP+64(6),=C'CHECK='                                             
         L     R6,ACHECK                                                        
         MVI   ELCODE,TACDELQ                                                   
         BAS   RE,GETEL                                                         
         USING TACDD,R6                                                         
         MVC   MYP+70(8),TACDCHK                                                
         GOTO1 DATCON,DMCB,(1,TACDDTE),(8,MYP+80)                               
         BAS   RE,SPLAT                                                         
         SPACE 1                                                                
PCCEND4  DS    0H                                                               
*&&DO                                                                           
         CLC   EXTSSN,=C'361201480'                                             
         BE    *+14                                                             
         CLC   EXTSSN,=C'154306593'                                             
         BNE   PCCEND6                                                          
         MVC   RECTYPE,=CL16'SYSIO RECORD    '                                  
         L     R6,ACHECK                                                        
         BAS   RE,TRACEREC                                                      
*&&                                                                             
         SPACE 1                                                                
PCCEND6  CLI   EXTOFF,C'A'         TRACE MISSING OFFICE                         
         BNL   XIT                                                              
         AP    MOFCOUNT,=P'1'                                                   
         CP    MOFCOUNT,TRALIMIT                                                
         BH    XIT                                                              
         MVC   RECTYPE,=CL16'MISSING OFFICE  '                                  
         L     R6,ACHECK                                                        
         BAS   RE,TRACEREC                                                      
         SPACE 1                                                                
         B     XIT                                                              
         EJECT                                                                  
*              POST SUI, SDI, FICA INTO EXTRACT DATA                            
         SPACE 3                                                                
POSTSUI  NTR1                                                                   
         CLI   RECNUM,NYDISK                                                    
         BNE   PSUI05                                                           
         CLC   TIFUNIT,=C'NYC'     2ND LOOP                                     
         BE    XIT                                                              
         B     PSUI10                                                           
*                                                                               
PSUI05   L     R1,YTDSEARN                                                      
         S     R1,THISEARN                                                      
         ST    R1,PRVSEARN                                                      
         L     R1,YTDFEARN                                                      
         S     R1,THISEARN                                                      
         ST    R1,PRVFEARN                                                      
         SPACE 1                                                                
PSUI10   LA    R2,LIMAREA                                                       
         USING TMD,R2                                                           
*                                  (SOME ALREADY SET UP IN INIT)                
         MVC   TMEMP,EXTEMP                                                     
         MVC   TMUNIT,EXTSTATE                                                  
         MVC   TMW4TYPE,EXTW4TY                                                 
         MVC   TMSSN,EXTSSN                                                     
         MVI   TMCURR,C'U'             NO CANADIAN                              
         OI    TMSTAT,TMSCHK+TMSPAMT   CALC BASED ON PAYROLL & PASSING          
*                                           TALIM AMOUNTS TO BE TAXED           
*                                                                               
         OI    TMSTAT2,TMSEFICA        NEEDS EMPLOYER FICA RATE                 
         MVC   TMTAXBLE(TMTNAMT*4),EXTTXBL  SET TAXABLE AMOUNTS AT              
         CLC   EXTCDTE,=X'B20401'  IF APR01/12 OR AFTER                         
         BL    PSUI15                    DON'T CLEAR                            
         CLC   TMUNIT,=C'HI'       IF STATE IS HAWAII                           
         BE    *+10                                                             
PSUI15   XC    TMTSDI,TMTSDI                    EACH LEVEL                      
                                                                                
         CLC   TMUNIT,=C'NJ'       IF STATE IS NEW JERSEY                       
         BNE   *+10                                                             
         MVC   TMTSDI,EXTSUI       SDI USES SAME RULES AS SUI                   
                                                                                
         XC    SUIMAX(16),SUIMAX   PRE-CLEAR SUI                                
         XC    SDIMAX(16),SDIMAX         AND SDI                                
                                                                                
* GET  SUI,FUI,FICA, MEDICARE TAXABLE WAGES FOR P+                              
         GOTOR =A(SUIBASE),DMCB,ACHECK,SVTUUNIT,TMTAXBLE,MEDOVER                
* READJUST NJ FOR P+ AFTER SUIBASE ROUTINE CALL                                 
         OC    TATUPTR,TATUPTR     IF THERE IS TATU- GET NEXT TATU ELEM         
         BZ    PSUI18              ELSE EXIT FOR NON P+                         
         MVC   EXTERN,THISEARN     UPDATED AFTER DUE COMP                       
         CLC   TMUNIT,=C'NJ'       IF STATE IS NEW JERSEY                       
         BNE   PSUI18                                                           
         MVC   TMTSDI,TMTSUI       SDI USES SAME RULES AS SUI                   
*                                                                               
         SPACE 1                                                                
PSUI18   GOTO1 MYTRACE,DMCB,TMD,256,=C'TO TALIM BLOCK'                          
         GOTO1 =V(TALIM),DMCB,(R2)                                              
         OC    TMBSDI,TMBSDI       IF THERE'S SDI FOR THIS STATE                
         BZ    PSUI20                                                           
         CLC   TMUNIT,=C'NJ'       AND STATE IS NOT NEW JERSEY                  
         BE    PSUI20                                                           
         CLC   TMUNIT,=C'HI'       HAWAII SKIP THIS ROUTINE - CHECKS            
         BE    PSUI20              FIGURES OUT SDI NOW                          
         BRAS  RE,SDITXBL          CALCULATE SDI TAXABLE MANUALLY               
         GOTO1 =V(TALIM),DMCB,(R2) AND GO BACK TO TALIM TO CALC. TAXES          
         SPACE 1                                                                
PSUI20   BRAS  RE,RETLIM           GET RETURNED INFO FROM TALIM                 
         GOTO1 MYTRACE,DMCB,TMD,256,=C'FROM TALIM BLOCK'                        
         SPACE 1                                                                
         CP    CHKCOUNT,TRALIMIT                                                
         BH    XIT                                                              
         MVC   MYP(17),=C'TALIM FOR TEXAS'                                      
         BAS   RE,SPLAT                                                         
         SPACE 1                                                                
         LA    R2,48                                                            
         LA    R6,EXTDATA                                                       
         BAS   RE,TRACEL                                                        
         MVC   MYP(8),=C'EARNINGS'                                              
         LA    R2,THISEARN                                                      
         LA    R4,6                                                             
         BAS   RE,COMPEDIT                                                      
         MVC   MYP(3),=C'SUI'                                                   
         LA    R2,SUIMAX                                                        
         LA    R4,4                                                             
         BAS   RE,COMPEDIT                                                      
         MVC   MYP(3),=C'SDI'                                                   
         LA    R2,SDIMAX                                                        
         BAS   RE,COMPEDIT                                                      
         MVC   MYP(4),=C'FICA'                                                  
         LA    R2,FCAMAX                                                        
         BAS   RE,COMPEDIT                                                      
         MVC   MYP(8),=C'MEDICARE'                                              
         LA    R2,MEDMAX                                                        
         BAS   RE,COMPEDIT                                                      
         B     XIT                                                              
         SPACE 1                                                                
COMPEDIT NTR1                                                                   
         LA    R3,MYP+10                                                        
         SPACE 1                                                                
COMPED2  EDIT  (4,0(R2)),(12,0(R3)),2,MINUS=YES                                 
         LA    R2,4(R2)                                                         
         LA    R3,13(R3)                                                        
         BCT   R4,COMPED2                                                       
         BAS   RE,SPLAT                                                         
         B     XIT                                                              
         EJECT                                                                  
*&&DO                                                                           
*              READ INVOICE RECORDS FOR BILLING DATA                            
         SPACE 3                                                                
READBILL NTR1                                                                   
         BAS   RE,SETSKIP                                                       
         MVC   TIACOMFC,ACOMFACS   SET UP FOR TASYSIO                           
         LA    R1,IOHOOK                                                        
         ST    R1,TIHOOK                                                        
         SPACE 1                                                                
PREP2    MVI   TIREAD,TLINCDQ      SET TO READ INVOICES                         
         OI    TIFPDSN,TAPDSCAN    NO CANADIAN                                  
         OI    TIFPDSN,TAPDSCNL    NO CANCELS                                   
         OI    TIFPDPN,TAPDPBNP    NO BNP                                       
         OI    TIFPDPN,TAPDPCRD    NO CREDITS                                   
         OI    TIFPO3N,TAPDODUM    NO DUMMYS                                    
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
         CLI   TRACOPT,C'Y'                                                     
         BNE   PREP4                                                            
         MVC   MYP(23),=C'BILL READING COMPLETE  '                              
         BAS   RE,SPLAT                                                         
         MVC   MYP(23),=C'---------------------  '                              
         BAS   RE,SPLAT                                                         
         BAS   RE,SPLAT                                                         
         EDIT  (P6,INVCOUNT),(8,MYP)                                            
         MVC   MYP+9(12),=C'BILL RECORDS'                                       
         BAS   RE,SPLAT                                                         
         EDIT  (P6,SRTCOUNT),(8,MYP)                                            
         MVC   MYP+9(16),=C'SORT OUTPUT RECS'                                   
         BAS   RE,SPLAT                                                         
         SPACE 1                                                                
PREP4    ZAP   INVCOUNT,=P'0'                                                   
         ZAP   SRTCOUNT,=P'0'                                                   
         B     XIT                                                              
         SPACE 1                                                                
IOHOOK   NTR1                                                                   
         CLI   TIMODE,PROCREC      IF THERE IS SOMETHING INTERESTING            
         BNE   XIT                                                              
         AP    INVCOUNT,=P'1'                                                   
         CP    INVCOUNT,TRALIMIT                                                
         BH    IOHOOK2                                                          
         MVC   RECTYPE,=CL16'INVOICE'                                           
         L     R6,TIAREC                                                        
         BAS   RE,TRACEREC                                                      
         SPACE 1                                                                
IOHOOK2  MVC   ABILL,TIAREC                                                     
         CLI   TIOFF,C'A'          MUST BE A VALID OFFICE                       
         BL    XIT                                                              
         GOTO1 =V(TA2BAP),DMCB,(3,(RC)),(RA)                                    
         B     XIT                                                              
*&&                                                                             
         EJECT                                                                  
*              CONTROL REPORTS ETC                                              
         SPACE 3                                                                
DOREPS   NTR1                                                                   
         BAS   RE,SETSKIP                                                       
         BAS   RE,SETPAGE                                                       
         CLI   RECNUM,AGYPROF                                                   
         BE    DOREPSAP                                                         
         CLI   RECNUM,UREPORT                                                   
         BE    DOREPSUR                                                         
         CLI   SORTFRST,C'Y'       IF NOTHING TO GET FROM SORT                  
         BE    XIT                 XIT NOW                                      
         BAS   RE,SORTRECS         FOR UC/NEWYORK, SORT THE DETAILS             
         BAS   RE,SORTWORK                 THEN SORT THE WORK RECORDS           
         B     XIT                                                              
         SPACE 1                                                                
DOREPSAP GOTO1 =V(TA2BAP),DMCB,(2,(RC)),(RA)                                    
         B     XIT                                                              
         SPACE 1                                                                
DOREPSUR GOTO1 =V(TA2BUR),DMCB,(2,(RC)),(RA)                                    
         B     XIT                                                              
         EJECT                                                                  
*              SORT THE UC CHECKS RECORDS AND                                   
*              COMBINE THOSE FOR SAME EMP/STATE/SS#                             
         SPACE 3                                                                
SORTRECS NTR1                                                                   
         BAS   RE,SETSKIP                                                       
         L     R2,=A(WRKFILE)      OPEN A WORK FILE                             
         OPEN  ((2),OUTPUT)                                                     
         ZAP   SRTCOUNT,=P'0'                                                   
         XC    WEEKNOTE,WEEKNOTE                                                
         XC    WEEKERNS,WEEKERNS                                                
*                                                                               
***      XC    SORTIO,SORTIO                                                    
         LA    RE,SORTIO           CLEAR SORTIO                                 
         LH    RF,=AL2(L'SORTIO)                                                
         XCEFL                                                                  
*                                                                               
         CLI   TRACOPT,C'Y'                                                     
         BNE   SORT1                                                            
         MVC   MYP(16),=C'STARTING TO SORT'                                     
         BAS   RE,SPLAT                                                         
         SPACE 1                                                                
SORT1    GOTO1 MYSORTER,DMCB,=C'GET'                                            
         L     R2,DMCB+4                                                        
         CLI   TRACOPT,C'Y'                                                     
         BNE   SORT2                                                            
         MVC   MYP(16),=C'GOT FIRST RECORD'                                     
         BAS   RE,SPLAT                                                         
         SPACE 1                                                                
SORT2    LA    R5,SORTIO                                                        
         USING UCRD,R5                                                          
         B     SORT4                                                            
         SPACE 1                                                                
SORT3    GOTO1 MYSORTER,DMCB,=C'GET'                                            
         L     R2,DMCB+4                                                        
         SPACE 1                                                                
SORT4    LTR   R2,R2                                                            
         BNZ   SORT7                                                            
         BAS   RE,SSNEND                                                        
         BAS   RE,STATEND                                                       
         CLI   TRACOPT,C'Y'                                                     
         BNE   SORT5                                                            
         MVC   MYP(23),=C'SORT READING COMPLETE  '                              
         BAS   RE,SPLAT                                                         
         BAS   RE,SPLAT                                                         
         EDIT  (P6,SRTCOUNT),(8,MYP)                                            
         MVC   MYP+9(12),=C'SORT RECORDS'                                       
         BAS   RE,SPLAT                                                         
         SPACE 1                                                                
SORT5    ZAP   SRTCOUNT,=P'0'                                                   
         L     R2,=A(WRKFILE)      CLOSE THE WORK FILE                          
         CLOSE ((2))                                                            
         B     XIT                                                              
         SPACE 1                                                                
SORT7    LR    R6,R2                                                            
         MVC   RECTYPE,=CL16'SORT INPUT'                                        
         AP    SRTCOUNT,=P'1'                                                   
         BAS   RE,SORTRACE                                                      
         OC    SORTIO(255),SORTIO                                               
         BZ    SORT15                                                           
*                                  R5=A(SAVED RECORD IN SORTIO)                 
*                                  R2=A(NEW RECORD FROM SORT)                   
         CLC   UCREMP,1(R2)        CHECK SAME EMP/STATE                         
         BNE   SORT8                                                            
         CLC   UCRSTATE,4(R2)                                                   
         BE    SORT11                                                           
         TM    STSTAT,STSTATFL+STSTATTN     IF FLORIDA OR TENNESSEE UC          
         BNZ   SORT11              NEED OUT OF STATE WAGES                      
*                                                                               
SORT8    BAS   RE,SSNEND                                                        
         BAS   RE,STATEND                                                       
         B     SORT15                                                           
         SPACE 1                                                                
SORT11   CLC   UCRSSN,UCRSSN-UCRD(R2)         AND SS#                           
         BE    SORT20                                                           
         BAS   RE,SSNEND           KEYS ARE DIFFERENT - WRAP UP LAST            
         SPACE 1                                                                
***T15   XC    SORTIO,SORTIO       NEW ENTRY                                    
SORT15   LA    RE,SORTIO           CLEAR SORTIO                                 
         LH    RF,=AL2(L'SORTIO)                                                
         XCEFL                                                                  
*                                                                               
         MVC   UCRKEY(UCRKLNQ),0(R2)                                            
*                                                                               
         TM    STSTAT,STSTATTN     IF TENNESSEE UC                              
         BZ    SORT15A                                                          
         CLC   UCRSTATE,MYSPACES                                                
         BNH   SORT16                                                           
         MVC   UCRSTATE,=CL3'TN'   SET STATE TO TENNESSEE                       
         B     SORT16                                                           
*                                                                               
SORT15A  TM    STSTAT,STSTATFL     IF FLORIDA UC                                
         BZ    SORT16                                                           
         CLC   UCRSTATE,MYSPACES                                                
         BNH   SORT16                                                           
         MVC   UCRSTATE,=CL3'FL'   SET STATE TO FLORIDA                         
*                                                                               
SORT16   ZAP   UCRGROSY,=P'0'                                                   
         ZAP   UCRGROSQ,=P'0'                                                   
         ZAP   UCRSUIWY,=P'0'                                                   
         ZAP   UCRSUIWQ,=P'0'                                                   
         ZAP   UCRSDIWY,=P'0'                                                   
         ZAP   UCRSDIWQ,=P'0'                                                   
         ZAP   UCRFUIWY,=P'0'                                                   
         ZAP   UCRFUIWQ,=P'0'                                                   
         ZAP   UCRFICWY,=P'0'                                                   
         ZAP   UCRFICWQ,=P'0'                                                   
         ZAP   UCRMEDWY,=P'0'                                                   
         ZAP   UCRMEDWQ,=P'0'                                                   
         ZAP   UCRTAXY,=P'0'                                                    
         ZAP   UCRTAXQ,=P'0'                                                    
         ZAP   UCROOSGW,=P'0'                                                   
         ZAP   UCROOSTW,=P'0'                                                   
         ZAP   EHTMAXP,=P'0'                                                    
         XC    EHTMAX,EHTMAX                                                    
         MVC   UCROOSST,MYSPACES                                                
         CLI   RECNUM,UCTAPES                                                   
         BNE   SORT17                                                           
         CLC   =C'MA ',UCRSTATE                                                 
         BNE   SORT17                                                           
         ZAP   EHTMAXP,=P'1400000'                                              
         MVC   EHTMAX,=AL4(1400000)                                             
SORT17   BAS   RE,W4DETS           (NEED SOME W4 STUFF)                         
         SPACE 1                                                                
SORT20   BRAS  RE,ADDETS                                                        
         MVC   FTYPE,0(R2)                                                      
         B     SORT3                                                            
         EJECT                                                                  
*              NEED SOME W4 INFO FOR THIS SS#                                   
         SPACE 3                                                                
*              INPUT               RECORD IS IN SORTIO                          
         SPACE 1                                                                
W4DETS   NTR1                                                                   
         LA    R5,SORTIO                                                        
         USING UCRD,R5                                                          
         LA    R2,UCRSSN                                                        
         MVC   UCRFRST,=CL16'UNKNOWN'                                           
         MVC   UCRLAST,=CL16'UNKNOWN'                                           
         MVC   UCRMID,MYSPACES                                                  
         MVI   UCRSEX,C'M'                                                      
         MVC   UCRADDR,MYSPACES                                                 
         MVC   UCRCITY,MYSPACES                                                 
         MVC   UCRASTAT,MYSPACES                                                
         MVC   UCRZIP,MYSPACES                                                  
         MVC   UCRZIPX,MYSPACES                                                 
         BAS   RE,NEEDW4                                                        
         CLI   NEEDHIT,C'N'                                                     
         BE    XIT                                                              
         L     R6,NEEDAREC                                                      
         MVI   ELCODE,TAW4ELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   W4DETS4                                                          
         USING TAW4D,R6                                                         
         MVC   UCRFRST,TAW4NAM1                                                 
         MVC   UCRLAST,TAW4NAM2                                                 
         MVC   UCRMID,TAW4MIDN                                                  
         CLI   TAW4SEX,C'A'                                                     
         BL    W4DETS4                                                          
         MVC   UCRSEX,TAW4SEX                                                   
*                                                                               
W4DETS4  L     R6,NEEDAREC                                                      
         MVI   ELCODE,TAA2ELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING TAA2D,R6                                                         
         MVC   UCRADDR,TAA2ADD1                                                 
         MVC   UCRCITY,TAA2CITY                                                 
         CLI   TAA2LEN,TAA2LNQ                                                  
         BL    W4DETS20                                                         
         CLC   TAA2CTRY,=C'US'                                                  
         BNE   XIT                                                              
W4DETS20 MVC   UCRASTAT,TAA2ST                                                  
         MVC   UCRZIP,TAA2ZIP                                                   
         MVC   UCRZIPX,TAA2ZIP+6                                                
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINES AT END OF THIS SSN                                      
         SPACE 3                                                                
SSNEND   NTR1                                                                   
         LA    R5,SORTIO                                                        
         USING UCRD,R5                                                          
         XC    WEEKNOTE,WEEKNOTE                                                
         XC    WEEKERNS,WEEKERNS                                                
         MVI   UCRSIGN,C' '                                                     
         MVI   ADDGROSQ,C'Y'       DEFAULT IS ADD GROSS TO GROSS TOTAL          
*                                                                               
         CLI   RECNUM,UCTAPES      IF UCTAPE OR UCDISK FOR CA                   
         BE    *+12                                                             
         CLI   RECNUM,UCDISK                                                    
         BNE   SSN2                                                             
         CLC   UCRSTATE(2),=C'CA'                                               
         BNE   SSN2                                                             
         BAS   RE,ADDCA            ADD TO TOTALS THE CA WAY                     
         B     XIT                                                              
*                                                                               
SSN2     CLI   RECNUM,NYDISK       IF NEW YORK DISK                             
         BNE   SSN4                                                             
         BAS   RE,ADDNY            ADD TO TOTALS THE NY WAY                     
         B     XIT                                                              
*                                                                               
SSN4     CP    UCRGROSQ,=P'0'      ELSE, IF THIS SSN HAS NEGATIVE GROSS         
         BNL   *+16                                                             
         MVI   UCRSIGN,C'N'        MARK SO SORTS HIGH/DON'T ADD TO TOT          
         BAS   RE,PUTWRKF          PUT RECORD TO SORT                           
         B     XIT                                                              
         BAS   RE,ADDENDT          ELSE, ADD TO TOTAL AND PUT TO SORT           
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO CHECK IF OKAY TO ADD THESE AMOUNTS TO                 
*              TOTALS FOR CA                                                    
         SPACE 1                                                                
ADDCA    NTR1                                                                   
         CP    UCRGROSQ,=P'0'      IF SSN HAS NEGATIVE GROSS                    
         BL    *+14                                                             
         CP    UCRTAXQ,=P'0'       OR NEGATIVE TAX WITHHELD                     
         BNL   ADDCA10                                                          
         MVI   UCRSIGN,C'N'        MARK SO SORTS HIGH/DON'T ADD TO TOT          
         BAS   RE,PUTWRKF          PUT RECORD TO SORT                           
         B     XIT                                                              
*                                                                               
ADDCA10  CP    UCRGROSQ,=P'0'      IF SSN HAS ZERO GROSS                        
         BNE   ADDCA20                                                          
         CP    UCRTAXQ,=P'0'       AND THERE IS TAX WITHHELD                    
         BE    ADDCA20                                                          
         MVI   UCRSIGN,C'N'        MARK SO SORTS HIGH/DON'T ADD TO TOT          
         BAS   RE,PUTWRKF          PUT RECORD TO SORT                           
         B     XIT                                                              
*                                                                               
ADDCA20  BAS   RE,ADDENDT          ELSE, ADD TO TOTAL AND PUT TO SORT           
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO CHECK IF OKAY TO ADD THESE AMOUNTS TO                 
*              TOTALS FOR NY                                                    
         SPACE 1                                                                
ADDNY    NTR1                                                                   
         CLI   QQUART,4            IF NOT FOURTH QUARTER                        
         BE    ADDNY10                                                          
         CP    UCRGROSQ,=P'0'      IF THIS SSN HAS NEGATIVE GROSS               
         BNL   ADDNY5                                                           
         MVI   UCRSIGN,C'N'        MARK SO SORTS HIGH                           
         BAS   RE,PUTWRKF          PUT RECORD TO SORT(NOT ADDED TO TOT)         
         B     XIT                                                              
ADDNY5   CP    UCRGROSQ,=P'0'      IF SSN HAS NO QUARTERLY GROSS                
         BE    XIT                 SKIP ALTOGETHER                              
         BAS   RE,ADDENDT          ELSE, ADD TO TOTAL AND PUT TO SORT           
         B     XIT                                                              
*                                                                               
ADDNY10  OC    TATUPTR,TATUPTR     DO NOT OVERRIDE WITH W2                      
***      BNZ   *+8                 ANNUAL GROSS FOR P+                          
         BRAS  RE,READW2           FOR FOURTH QUARTER, READ W2 RECORD           
         BAS   RE,CKEOYR           CHECK AMTS FOR END OF YR CONDITIONS          
         BNE   XIT                 NO AMTS - SKIP ALTOGETHER                    
         CLI   UCRSIGN,UCRERROR    IF ERROR CODE                                
         BE    ADDNY15             DON'T ADD TO TOTAL/PUT TO SORT HIGH          
         CLI   UCRSIGN,C'N'        IF NEGATIVE AMTS                             
         BE    ADDNY15             DON'T ADD TO TOTAL/PUT TO SORT HIGH          
         CLI   UCRSIGN,UCRFUDGE    IF FUDGING QUARTER GROSS                     
         BNE   *+8                                                              
         MVI   ADDGROSQ,C'N'       DON'T ADD TO QTR GRS TOT/PUT HIGH            
         BAS   RE,ADDENDT          ADD TO TOTAL AND PUT TO SORT                 
         B     XIT                                                              
*                                                                               
ADDNY15  BAS   RE,PUTWRKF          PUT RECORD TO SORT                           
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE CHECKS AMTS FOR SPECIAL YTD CONDITIONS                   
*              RETURNS CC CODE AND UCRSIGN                                      
         SPACE                                                                  
CKEOYR   NTR1                                                                   
         CP    UCRGROSQ,=P'0'                                                   
         BNE   CKEOYR5                                                          
         CP    UCRGROSY,=P'0'                                                   
         BNE   CKEOYR5                                                          
         CP    UCRTAXY,=P'0'                                                    
         BE    NO                                                               
*                                                                               
CKEOYR5  CP    UCRGROSQ,=P'0'                                                   
         BNL   CKEOYR10                                                         
         CP    UCRGROSY,=P'0'                                                   
         BNH   CKEOYR10                                                         
         MVI   UCRSIGN,UCRFUDGE    NEG QTR/YTD GROSS - FUDGE QTR                
         B     YES                                                              
*                                                                               
CKEOYR10 CP    UCRGROSQ,=P'0'                                                   
         BL    CKEOYR12                                                         
         CP    UCRGROSY,=P'0'                                                   
         BL    CKEOYR12                                                         
         CP    UCRTAXY,=P'0'                                                    
         BNL   CKEOYR15                                                         
CKEOYR12 MVI   UCRSIGN,C'N'        NEG QTR/YTD GROSS OR YTD TAXES               
         B     YES                                                              
*                                                                               
CKEOYR15 CP    UCRTAXY,=P'0'                                                    
         BE    CKEOYR20                                                         
         CP    UCRGROSY,=P'0'                                                   
         BH    CKEOYR20                                                         
         CLI   RECNUM,NYDISK       IF NYC DISK, THIS IS FINE                    
         BE    YES                                                              
         MVI   UCRSIGN,UCRERROR    YTD TAXES BUT ZERO/NEG YTD WAGES             
         B     YES                                                              
*                                                                               
CKEOYR20 CP    UCRGROSY,=P'0'                                                   
         BNE   YES                                                              
         CP    UCRGROSQ,=P'0'                                                   
         BE    YES                                                              
         MVI   UCRSIGN,UCRWARN     ZERO YTD WAGES BUT 4TH QTR WAGES             
         B     YES                                                              
         EJECT                                                                  
*              ROUTINE ADDS TO TOTALS FOR SSN END AND PUTS RECORD               
*              TO WORKER FILE                                                   
         SPACE 1                                                                
ADDENDT  NTR1                                                                   
         CLI   ADDGROSQ,C'Y'       IF OKAY TO ADD TO GROSS                      
         BNE   *+10                                                             
         AP    STAGROSQ,UCRGROSQ   ADD GROSS TO QUARTER GROSS TOTAL             
         AP    STAGROSY,UCRGROSY                                                
         AP    STASUIWQ,UCRSUIWQ                                                
         AP    STASUIWY,UCRSUIWY                                                
         OC    EHTMAX,EHTMAX       IF EMP HEALTH INSURANCE                      
         BZ    *+8                                                              
         BAS   RE,SETEHT           SET EHT VALUES IN SDI                        
         AP    STASDIWQ,UCRSDIWQ   ADD TO SDI OR EHT                            
         AP    STASDIWY,UCRSDIWY                                                
*                                                                               
         AP    STAFUIWQ,UCRFUIWQ                                                
         AP    STAFUIWY,UCRFUIWY                                                
         AP    STAFICWQ,UCRFICWQ                                                
         AP    STAFICWY,UCRFICWY                                                
         AP    STAMEDWQ,UCRMEDWQ                                                
         AP    STAMEDWY,UCRMEDWY                                                
         AP    STATAXQ,UCRTAXQ                                                  
         AP    STATAXY,UCRTAXY                                                  
*                                                                               
         CLI   RECNUM,NYDISK       IF NOT NEW YORK DISK                         
         BE    *+12                                                             
         BAS   RE,ANYAMTS          IF THIS PERSON HAS NO AMOUNTS                
         BE    XIT                 IGNORE                                       
*                                                                               
         CLI   UCRMTH1,0                                                        
         BE    AENDT10                                                          
         AP    NMTH1,=P'1'                                                      
         CLI   UCRSEX,C'F'         FEMALE?                                      
         BNE   AENDT10                                                          
         AP    NFMTH1,=P'1'        UPDATE MONTHLY FEMALE COUNTER                
AENDT10  CLI   UCRMTH2,0                                                        
         BE    AENDT20                                                          
         AP    NMTH2,=P'1'                                                      
         CLI   UCRSEX,C'F'         FEMALE?                                      
         BNE   AENDT20                                                          
         AP    NFMTH2,=P'1'        UPDATE MONTHLY FEMALE COUNTER                
AENDT20  CLI   UCRMTH3,0                                                        
         BE    AENDT30                                                          
         AP    NMTH3,=P'1'                                                      
         CLI   UCRSEX,C'F'         FEMALE?                                      
         BNE   AENDT30                                                          
         AP    NFMTH3,=P'1'        UPDATE MONTHLY FEMALE COUNTER                
*                                                                               
AENDT30  AP    NSSN,=P'1'          ADD TO STATE NUMBERS                         
         TM    UCRINDS,UCRISESS    IF PERSON HAS SESSION WAGES,                 
         BNO   *+10                                                             
         AP    NSESS,=P'1'         ADD TO TOTAL PEOPLE WITH SESS WAGES          
         BAS   RE,PUTWRKF          PUT RECORD TO WORKER FILE                    
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO CALCULATE E.H.T AND SET IN FIELDS                     
*              ORG'D OVER SDI FIELDS                                            
*                                                                               
SETEHT   NTR1                                                                   
         CP    UCRGROSY,EHTMAXP    IF YTD NOT GREATER THAN MAX                  
         BH    SETEHT10                                                         
         ZAP   UCREHTWQ,UCRGROSQ   THEN QTR WAGES ALL TAXABLE                   
         ZAP   UCREHTWY,UCRGROSY   AND YTD WAGES ALL TAXABLE                    
         B     SETEHTX                                                          
*                                                                               
SETEHT10 ZAP   DUB2,UCRGROSY       ELSE, IF YTD GREATER THAN MAX                
         SP    DUB2,UCRGROSQ                                                    
         CP    DUB2,EHTMAXP        AND WAS GREATER BEFORE QTR WAGES             
         BH    SETEHT20            ALL QTR DOLLARS NON-TAXABLE                  
         ZAP   DUB,EHTMAXP         ELSE, SOME QTR WAGES ARE TAXABLE             
         SP    DUB,DUB2                                                         
         AP    UCREHTWQ,DUB                                                     
*                                                                               
SETEHT20 AP    UCREHTWY,EHTMAXP    AND YTD WAGES TAXABLE UP TO MAX              
SETEHTX  B     XIT                                                              
         EJECT                                                                  
*              ROUTINES TO CHECK IF THIS PERSON HAS ANY AMOUNTS                 
         SPACE 2                                                                
ANYAMTS  DS    0H                                                               
         CP    UCRGROSQ,=P'0'                                                   
         BNER  RE                                                               
         CP    UCRSUIWQ,=P'0'                                                   
         BNER  RE                                                               
         CP    UCRSDIWQ,=P'0'                                                   
         BNER  RE                                                               
         CP    UCRFUIWQ,=P'0'                                                   
         BNER  RE                                                               
         CP    UCRFICWQ,=P'0'                                                   
         BNER  RE                                                               
         CP    UCRMEDWQ,=P'0'                                                   
         BNER  RE                                                               
         BR    RE                                                               
         SPACE 2                                                                
*              ROUTINE TO PUT A RECORD TO THE WORKER FILE                       
*                                                                               
PUTWRKF  NTR1                                                                   
         L     R1,=A(WRKFILE)                                                   
         LA    R0,SORTIO           PUT RECORD TO WORK FILE                      
         PUT   (1),(0)                                                          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINES AT END OF THIS EMPLOYER/STATE                           
         SPACE 3                                                                
STATEND  NTR1                                                                   
         LA    R5,SORTIO                                                        
         USING UCRD,R5                                                          
         MVI   UCRTYPE,UCRTHEAD    PREPARE TO WRITE OUT A HEADER                
         MVC   UCRFTYPE,FTYPE                                                   
         XC    UCRLAST,UCRLAST                                                  
         XC    UCRSSN,UCRSSN                                                    
         XC    UCRDATA,UCRDATA                                                  
         ZAP   UCRNSSN,NSSN                                                     
         ZAP   UCRNMTH1,=P'0'                                                   
         ZAP   UCRNMTH2,=P'0'                                                   
         ZAP   UCRNMTH3,=P'0'                                                   
*                                                                               
         CLC   UCRSTATE(2),=C'NM'  NEW MEXICO ONLY                              
         BNE   *+10                                                             
         ZAP   UCRNSESS,NSESS                                                   
*                                                                               
         CLC   UCRSTATE(2),=C'ME'  MAINE                                        
         BNE   STATEND1                                                         
         ZAP   UCRNFMT1,=P'0'                                                   
         ZAP   UCRNFMT2,=P'0'                                                   
         ZAP   UCRNFMT3,=P'0'                                                   
*                                                                               
STATEND1 CLI   RECNUM,UCTAPES      FOR UCTAPE OR UCDISK                         
         BE    *+12                                                             
         CLI   RECNUM,UCDISK                                                    
         BNE   STATEND5                                                         
****     LA    RE,MONTHTAB                                                      
         L     RE,=A(MONTHTAB)                                                  
STATENDA CLI   0(RE),X'FF'                                                      
         BE    STATEND5                                                         
         CLC   UCRSTATE(2),0(RE)                                                
         BE    STATEND2                                                         
         LA    RE,L'MONTHTAB(RE)                                                
         B     STATENDA                                                         
*&&DO                                                                           
         CLC   UCRSTATE(2),=C'PA'  FOR PA, IL, CT, CA, WA, FL                   
         BE    STATEND2                                                         
         CLC   UCRSTATE(2),=C'IL'                                               
         BE    STATEND2                                                         
         CLC   UCRSTATE(2),=C'CT'                                               
         BE    STATEND2                                                         
         CLC   UCRSTATE(2),=C'CA'                                               
         BE    STATEND2                                                         
         CLC   UCRSTATE(2),=C'WA'                                               
         BE    STATEND2                                                         
         CLC   UCRSTATE(2),=C'FL'                                               
         BE    STATEND2                                                         
         CLC   UCRSTATE(2),=C'KY'  AND KENTUCKY                                 
         BE    STATEND2                                                         
         CLC   UCRSTATE(2),=C'ME'  AND MAINE                                    
         BE    STATEND2                                                         
         CLC   UCRSTATE(2),=C'NM'  AND NEW MEXICO                               
         BNE   STATEND5                                                         
*&&                                                                             
STATEND2 ZAP   UCRNMTH1,NMTH1      SET TRUE COUNT OF MONTHLY EMPLOYEES          
         ZAP   UCRNMTH2,NMTH2                                                   
         ZAP   UCRNMTH3,NMTH3                                                   
         SPACE 1                                                                
         CLC   UCRSTATE(2),=C'ME'  MAINE                                        
         BNE   STATEND5                                                         
         ZAP   UCRNFMT1,NFMTH1     SET TRUE COUNT OF MONTHLY FEMALES            
         ZAP   UCRNFMT2,NFMTH2                                                  
         ZAP   UCRNFMT3,NFMTH3                                                  
         SPACE 1                                                                
STATEND5 MVC   UCRRYR,EXTYEAR      REPORTING YEAR - DISPLAYABLE                 
         MVC   UCRQUART,EXTQUART   REPORTING QUARTER                            
         ZAP   UCRGROSQ,STAGROSQ                                                
         ZAP   UCRGROSY,STAGROSY                                                
         ZAP   UCRSUIWQ,STASUIWQ                                                
         ZAP   UCRSUIWY,STASUIWY                                                
         ZAP   UCRSDIWQ,STASDIWQ                                                
         ZAP   UCRSDIWY,STASDIWY                                                
         ZAP   UCRFUIWQ,STAFUIWQ                                                
         ZAP   UCRFUIWY,STAFUIWY                                                
         ZAP   UCRFICWQ,STAFICWQ                                                
         ZAP   UCRFICWY,STAFICWY                                                
         ZAP   UCRMEDWQ,STAMEDWQ                                                
         ZAP   UCRMEDWY,STAMEDWY                                                
         ZAP   UCRTAXQ,STATAXQ                                                  
         ZAP   UCRTAXY,STATAXY                                                  
         SPACE 1                                                                
         L     R1,=A(WRKFILE)                                                   
         LA    R0,SORTIO           PUT A HEADER                                 
         PUT   (1),(0)                                                          
         SPACE 1                                                                
         MVI   UCRTYPE,UCRTRAIL                                                 
         L     R1,=A(WRKFILE)                                                   
         LA    R0,SORTIO           AND A TRAILER                                
         PUT   (1),(0)                                                          
         ZAP   NSSN,=P'0'                                                       
         ZAP   NMTH1,=P'0'                                                      
         ZAP   NMTH2,=P'0'                                                      
         ZAP   NMTH3,=P'0'                                                      
         ZAP   NSESS,=P'0'                                                      
         ZAP   STAGROSQ,=P'0'                                                   
         ZAP   STAGROSY,=P'0'                                                   
         ZAP   STASUIWQ,=P'0'                                                   
         ZAP   STASUIWY,=P'0'                                                   
         ZAP   STASDIWQ,=P'0'                                                   
         ZAP   STASDIWY,=P'0'                                                   
         ZAP   STAFUIWQ,=P'0'                                                   
         ZAP   STAFUIWY,=P'0'                                                   
         ZAP   STAFICWQ,=P'0'                                                   
         ZAP   STAFICWY,=P'0'                                                   
         ZAP   STAMEDWQ,=P'0'                                                   
         ZAP   STAMEDWY,=P'0'                                                   
         ZAP   STATAXQ,=P'0'                                                    
         ZAP   STATAXY,=P'0'                                                    
         B     XIT                                                              
         SPACE 1                                                                
STAGROSQ DC    PL8'0'                                                           
STAGROSY DC    PL8'0'                                                           
STASUIWQ DC    PL8'0'                                                           
STASUIWY DC    PL8'0'                                                           
STASDIWQ DC    PL8'0'                                                           
STASDIWY DC    PL8'0'                                                           
         ORG   STASDIWQ                                                         
STAEHTWQ DC    PL8'0'                                                           
STAEHTWY DC    PL8'0'                                                           
*                                                                               
STAFUIWQ DC    PL8'0'                                                           
STAFUIWY DC    PL8'0'                                                           
STAFICWQ DC    PL8'0'                                                           
STAFICWY DC    PL8'0'                                                           
STAMEDWQ DC    PL8'0'                                                           
STAMEDWY DC    PL8'0'                                                           
STATAXQ  DC    PL8'0'                                                           
STATAXY  DC    PL8'0'                                                           
NSSN     DC    PL4'0'                                                           
NMTH1    DC    PL4'0'                                                           
NMTH2    DC    PL4'0'                                                           
NMTH3    DC    PL4'0'                                                           
NFMTH1   DC    PL4'0'                                                           
NFMTH2   DC    PL4'0'                                                           
NFMTH3   DC    PL4'0'                                                           
NSESS    DC    PL4'0'                                                           
DUB2     DC    PL8'0'              MISC PACKED ACCUM                            
ADDGROSQ DC    CL1'0'              FLAG TO ADD GROSS TO QTR GROSS TOTAL         
         EJECT                                                                  
*              ROUTINE TO READ WRKFILE AND PUT TO SORT AGAIN                    
         SPACE                                                                  
SORTWORK NTR1                                                                   
         L     R2,=A(WRKFILE)                                                   
         LA    R5,SORTIO                                                        
         USING UCRD,R5                                                          
         OPEN  ((2),INPUT)                                                      
         GOTO1 MYSORTER,DMCB,=C'END'                                            
         MVI   SORTFRST,C'Y'                                                    
         SPACE 1                                                                
SW2      GET   (2),(5)                                                          
         BAS   RE,SORTPUT                                                       
         B     SW2                                                              
         SPACE 1                                                                
WORKEOF  DS    0H                                                               
*ORKEOF  CLOSE (2)                 CAUSED A BAD CLOSE, 7/6/01                   
*                                                                               
WORKEOF2 GOTO1 MYSORTER,DMCB,=C'GET'                                            
         L     R5,DMCB+4                                                        
         LTR   R5,R5                                                            
         BZ    XIT                                                              
         ST    R5,ASORTREC                                                      
         CLI   TRACOPT,C'Y'                                                     
         BNE   WE10                                                             
*                                                                               
         MVC   MYP+00(3),UCREMP                                                 
         MVC   MYP+4(1),UCRFTYPE                                                
         MVC   MYP+06(16),UCRLAST                                               
         CLI   UCRTYPE,UCRTHEAD                                                 
         BNE   *+10                                                             
         MVC   MYP+6(7),=C'HEADER '                                             
         CLI   UCRTYPE,UCRTRAIL                                                 
         BNE   *+10                                                             
         MVC   MYP+6(7),=C'TRAILER'                                             
         MVC   MYP+29(2),UCRRYR    DISPLAYABLE                                  
         MVC   MYP+33(9),UCRSSN                                                 
         MVC   MYP+43(16),UCRFRST                                               
         MVC   MYP+60(1),UCRSEX                                                 
         CLI   UCRTYPE,UCRTRAIL                                                 
         BNE   WE4                                                              
         EDIT  (P4,UCRNMTH1),(7,MYP+33)                                         
         EDIT  (P4,UCRNMTH2),(7,MYP+41)                                         
         EDIT  (P4,UCRNMTH3),(7,MYP+49)                                         
         SPACE 1                                                                
WE4      EDIT  (1,UCRWEEK),(2,MYP+62)                                           
         EDIT  (P8,UCRGROSQ),(12,MYP+65),2,MINUS=YES                            
         EDIT  (P8,UCRGROSY),(12,MYP+77),2,MINUS=YES                            
         EDIT  (P8,UCRSUIWQ),(12,MYP+89),2,MINUS=YES                            
         EDIT  (P8,UCRSUIWY),(12,MYP+101),2,MINUS=YES                           
         CLI   UCRTYPE,UCRTDET                                                  
         BNE   WE5                                                              
         EDIT  (1,UCRMTH1),(1,MYP+115)                                          
         EDIT  (1,UCRMTH2),(1,MYP+117)                                          
         EDIT  (1,UCRMTH3),(1,MYP+119)                                          
         SPACE 1                                                                
WE5      BAS   RE,SPLAT                                                         
         EDIT  (P8,UCRSDIWQ),(12,MYP+10),2,MINUS=YES                            
         EDIT  (P8,UCRSDIWY),(12,MYP+23),2,MINUS=YES                            
         EDIT  (P8,UCRFUIWQ),(12,MYP+36),2,MINUS=YES                            
         EDIT  (P8,UCRFUIWY),(12,MYP+49),2,MINUS=YES                            
         EDIT  (P8,UCRFICWQ),(12,MYP+62),2,MINUS=YES                            
         EDIT  (P8,UCRFICWY),(12,MYP+75),2,MINUS=YES                            
         EDIT  (P8,UCRMEDWQ),(12,MYP+88),2,MINUS=YES                            
         EDIT  (P8,UCRMEDWY),(12,MYP+101),2,MINUS=YES                           
         EDIT  (P8,UCRTAXQ),(12,MYP+114),2,MINUS=YES                            
         EDIT  (P8,UCRTAXY),(12,MYP+127),2,MINUS=YES                            
         BAS   RE,SPLAT                                                         
*                                                                               
WE10     CLI   RECNUM,NYDISK                                                    
         BE    WE20                                                             
         GOTO1 =V(TA2BUC),DMCB,(1,(RC)),(RA)                                    
         B     WORKEOF2                                                         
*                                                                               
WE20     GOTO1 =V(TA2BNY),DMCB,(1,(RC)),(RA)                                    
         B     WORKEOF2                                                         
         EJECT                                                                  
*              ROUTINES TO TEST IS A SPECIAL 941 FORM IS NEEDED                 
         SPACE 3                                                                
*              INPUT               EXTSTATE                                     
*                                  EXTEMP                                       
         SPACE 1                                                                
SPECFORM NTR1                                                                   
***      LA    R2,FORMTAB          SPECIAL FORM TABLES                          
         L     R2,=A(FORMTAB)      SPECIAL FORM TABLES                          
         USING FORMD,R2                                                         
*                                                                               
SF10     CLI   0(R2),X'FF'                                                      
         BE    NO                                                               
         CLC   EXTSTATE,FSTATE                                                  
         BNE   SF20                                                             
         CLC   EXTEMP,FEMP                                                      
         BE    YES                                                              
*                                                                               
SF20     LA    R2,FORMLNQ(R2)                                                   
         B     SF10                                                             
         EJECT                                                                  
*              ROUTINES TO ENSURE SUB RECORDS AROUND                            
         SPACE 3                                                                
*              INPUT               R2=A(EMP)                                    
         SPACE 1                                                                
NEEDEM   NTR1                                                                   
         XC    NEEDKEY,NEEDKEY     ENSURE EMPLOYER AROUND                       
         LA    R4,NEEDKEY                                                       
         USING TLEMD,R4                                                         
         MVI   TLEMCD,TLEMCDQ                                                   
         MVC   TLEMEMP,0(R2)                                                    
         CLC   TLEMEMP,=C'TP '                                                  
         BNE   NEEDEM3                                                          
         CLC   RYEAR,=C'2004'                                                   
         BNL   NEEDEM3                                                          
         MVC   TLEMEMP,=C'TP1'                                                  
NEEDEM3  BAS   RE,NEEDREC                                                       
         B     XIT                                                              
         SPACE 1                                                                
*              INPUT               R2=A(OFFICE CODE)                            
         SPACE 1                                                                
NEEDOF   NTR1                                                                   
         XC    NEEDKEY,NEEDKEY     ENSURE OFFICE AROUND                         
         LA    R4,NEEDKEY                                                       
         USING TLOFD,R4                                                         
         MVI   TLOFCD,TLOFCDQ                                                   
         MVC   TLOFOFF,0(R2)                                                    
         BAS   RE,NEEDREC                                                       
         L     R6,NEEDAREC         LOOK FOR ADDRESS                             
         MVC   THISOFNM,MYSPACES                                                
         MVI   ELCODE,TAADELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING TAADD,R6                                                         
         ZIC   R1,TAADLNES         GET TO LAST LINE                             
         BCTR  R1,0                                                             
         MH    R1,=H'30'                                                        
         LA    R1,TAADADD(R1)                                                   
         LA    R2,THISOFNM                                                      
         LA    R0,30                                                            
         SPACE 1                                                                
NEEDOF2  CLI   0(R1),C','          OUTPUT CITY (BEFORE COMMA)                   
         BE    XIT                                                              
         MVC   0(1,R2),0(R1)                                                    
         LA    R1,1(R1)                                                         
         LA    R2,1(R2)                                                         
         BCT   R0,NEEDOF2                                                       
         B     XIT                                                              
         SPACE 1                                                                
*              INPUT               R2=A(SS#)                                    
         SPACE 1                                                                
NEEDW4   NTR1                                                                   
         XC    NEEDKEY,NEEDKEY     ENSURE W4 AROUND                             
         LA    R4,NEEDKEY                                                       
         USING TLW4D,R4                                                         
         MVI   TLW4CD,TLW4CDQ                                                   
         MVC   TLW4SSN,0(R2)                                                    
         BAS   RE,NEEDREC                                                       
         B     XIT                                                              
         EJECT                                                                  
*              GENERAL BUFFER HANDLER                                           
         SPACE 3                                                                
NEEDREC  NTR1                                                                   
         L     R2,ABUFFER                                                       
         LTR   R2,R2                                                            
         BNZ   NREC2                                                            
         L     R0,LBUFFER                                                       
         GETMAIN RU,LV=(0)                                                      
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         ST    R1,ABUFFER                                                       
         L     R2,ABUFFER                                                       
         MVC   0(4,R2),=F'100'     SET UP FOR 100 RECORDS                       
         MVC   4(4,R2),=F'4000'    4000 BYTES EACH                              
         XC    8(4,R2),8(R2)                                                    
         LA    RF,100                                                           
         M     RE,=F'4000'                                                      
         LA    RE,12(R2)                                                        
*                                  CLEAR BUFFER FIRST TIME                      
         XCEF                                                                   
         B     NREC2                                                            
         SPACE 1                                                                
ABUFFER  DC    A(0)                                                             
LBUFFER  DC    F'400016'           (100*4000 + 16)                              
         SPACE 1                                                                
NREC2    DS    0H                  NOW R2 HAS A(BUFFER)                         
*                                  BYTES  1-4 N'ENTRIES                         
*                                  BYTES  5-8 L'ENTRY                           
*                                  BYTES 9-12 NUMBER OF LAST ENTRY              
         LA    R4,12(R2)           BYTES 13+  THE BUFFER!                       
         L     R0,0(R2)                                                         
         SPACE 1                                                                
NREC6    CLC   NEEDKEY,0(R4)       IS MY RECORD IN THE BUFFER?                  
         BE    NREC10                                                           
         A     R4,4(R2)                                                         
         BCT   R0,NREC6                                                         
         SPACE 1                                                                
         MVI   NEEDHIT,C'N'                                                     
         MVC   KEY,NEEDKEY         NO, NOW NEED THE RECORD                      
         GOTO1 HIGH                                                             
         CLC   NEEDKEY(32),KEY                                                  
         BNE   XIT                                                              
         SPACE 1                                                                
NREC8    L     R1,8(R2)            NO - PICK UP N'LAST ENTRY                    
         LA    R1,1(R1)                 ROUND ROBIN                             
         C     R1,0(R2)            HAVE WE GOT TO THE END OF BUFFER?            
         BNH   *+8                                                              
         LA    R1,1                YES, SO GO BACK TO THE BEGINNING             
         ST    R1,8(R2)                                                         
         BCTR  R1,0                                                             
         M     R0,4(R2)            DISPLACE INTO THE BUFFER                     
         LA    R4,12(R1,R2)                                                     
         GOTO1 GETREC                                                           
         L     R3,AIO                                                           
         L     R2,4(R2)                                                         
         MOVE  ((R4),(R2)),(R3)    MOVE INTO OUR AREA                           
         OC    TIKEY,TIKEY         IS SYSIO READING RECORDS                     
         BZ    NREC10                                                           
         TM    TISTAT,TISTRDCK     UNLESS READING CHECK FILE                    
         BO    NREC10                                                           
         MVC   KEY,TIKEY                                                        
         GOTO1 HIGH                REREAD TO ESTABLISH SEQUENCE                 
         SPACE 1                                                                
NREC10   ST    R4,NEEDAREC         PASS BACK A RECORD                           
         BAS    RE,GETNAME                                                      
         MVI   NEEDHIT,C'Y'                                                     
         B     ITSFINE                                                          
         SPACE 1                                                                
NEEDAREC DS    A                                                                
NEEDKEY  DC    XL32'00'                                                         
NEEDNAME DS    CL36                                                             
NEEDSHRT DS    CL16                                                             
NEEDHIT  DS    CL1                                                              
NEEDTYPE DS    CL1                                                              
         EJECT                                                                  
                                                                                
*              ODDMENTS                                                         
         SPACE 3                                                                
GETNAME  NTR1                                                                   
*                                  R4=A(RECORD) TO NEEDNAME                     
         MVC   SAVEEL,ELCODE       SAVE PRESENT ELCODE                          
         MVC   NEEDNAME,MYSPACES                                                
         CLI   0(R4),TLW4CDQ                                                    
         BE    GETW4NM                                                          
         MVI   ELCODE,TANAELQ                                                   
         LR    R6,R4                                                            
         BAS   RE,GETEL                                                         
         MVC   ELCODE,SAVEEL       RESTORE PRESENT ELCODE                       
         BNE   NOGOOD                                                           
         USING TANAD,R6                                                         
         ZIC   R1,TANALEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     ITSFINE                                                          
         MVC   NEEDNAME(0),TANANAME                                             
         SPACE 1                                                                
GETW4NM  MVI   ELCODE,TAW4ELQ                                                   
         LR    R6,R4                                                            
         BAS   RE,GETEL                                                         
         MVC   ELCODE,SAVEEL       RESTORE PRESENT ELCODE                       
         BNE   NOGOOD                                                           
         USING TAW4D,R6                                                         
         MVC   NEEDNAME(32),TAW4CRPN                                            
         MVC   NEEDTYPE,TAW4TYPE                                                
         B     ITSFINE                                                          
         SPACE 1                                                                
GETSHORT NTR1                                                                   
*                                  R4=A(RECORD) TO NEEDSHRT                     
         MVC   SAVEEL,ELCODE       SAVE PRESENT ELCODE                          
         MVI   ELCODE,TASNELQ      SHORT NAME                                   
         LR    R6,R4                                                            
         BAS   RE,GETEL                                                         
         MVC   NEEDSHRT,MYSPACES                                                
         MVC   ELCODE,SAVEEL                                                    
         BNE   XIT                                                              
         USING TASND,R6                                                         
         ZIC   R1,TASNLEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     XIT                                                              
         MVC   NEEDSHRT(0),TASNAME                                              
         SPACE 1                                                                
ITSFINE  SR    R1,R1                                                            
         B     *+8                                                              
         SPACE 1                                                                
NOGOOD   LA    R1,1                                                             
         LTR   R1,R1                                                            
         B     XIT                                                              
         SPACE 1                                                                
                                                                                
*              TRACING ROUTINES                                                 
         SPACE                                                                  
*        SET INFO FOR TRACE ROUTINE                                             
         SPACE                                                                  
MYTRACE  NTR1                                                                   
         CLI   TRACOPT,C'Y'                                                     
         BNE   XIT                                                              
         LM    R2,R4,0(R1)                                                      
         ZIC   R5,8(R1)                                                         
         GOTO1 TRACE,DMCB,(R2),(R3),(R4),(R5)                                   
         B     XIT                                                              
         SPACE                                                                  
TRACEREC NTR1                                                                   
         MVI   MYP,X'BF'           R6=A(INPUT RECORD)                           
         MVC   MYP+1(131),MYP                                                   
         MVC   MYP+60(16),RECTYPE                                               
         BAS   RE,SPLAT                                                         
         LA    R2,40                                                            
         BAS   RE,TRACEL                                                        
         MVI   ELCODE,0                                                         
         BAS   RE,GETEL                                                         
         B     *+8                                                              
         SPACE 1                                                                
TRACERC2 BAS   RE,NEXTEL                                                        
         BNE   XIT                                                              
         ZIC   R2,1(R6)                                                         
         BAS   RE,TRACEL                                                        
         B     TRACERC2                                                         
         SPACE 1                                                                
TRACEM   NTR1                                                                   
         CLI   TRACOPT,C'Y'                                                     
         BNE   XIT                                                              
         MVI   MYP,X'BF'                                                        
         MVC   MYP+1(131),MYP                                                   
         MVC   MYP+60(16),RECTYPE                                               
         BAS   RE,SPLAT                                                         
         LA    R2,TAPEIO                                                        
         SPACE 1                                                                
TRACE2   MVC   MYP(100),0(R2)                                                   
         BAS   RE,SPLAT                                                         
         LA    R2,100(R2)                                                       
         BAS   RE,SPLAT                                                         
         B     XIT                                                              
         SPACE 1                                                                
TRACEL   NTR1                                                                   
*                                  R2=LENGTH, R6=ADDRESS                        
         MVC   MYP,MYSPACES                                                     
         LR    R1,R2                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MYP(0),0(R6)                                                     
         OC    MYP,MYSPACES                                                     
         GOTO1 HEXOUT,DMCB,(R6),MYP3,132,=C'SEP'                                
         LR    R1,R2                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MYP2(0),MYP3                                                     
         MVC   MYP3,MYSPACES                                                    
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MYP3(0),MYP4                                                     
         MVC   MYP4,MYSPACES                                                    
         BAS   RE,SPLAT                                                         
         BAS   RE,SPLAT                                                         
         B     XIT                                                              
         SPACE 1                                                                
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
*              SORT UTILITIES                                                   
         SPACE 3                                                                
SORTPUT  NTR1                                                                   
         AP    SRTCOUNT,=P'1'                                                   
         LA    R6,SORTIO                                                        
         MVC   RECTYPE,=CL16'SORT OUTPUT'                                       
         BAS   RE,SORTRACE                                                      
         CLI   SORTFRST,C'Y'                                                    
         BNE   SORTPUT2                                                         
         GOTO1 MYSORTER,DMCB,SORTCARD,RECCARD                                   
         MVI   SORTFRST,C'N'                                                    
         SPACE 1                                                                
SORTPUT2 GOTO1 MYSORTER,DMCB,=C'PUT',SORTIO                                     
         B     XIT                                                              
         SPACE 1                                                                
SORTRACE NTR1                                                                   
*&&DO                                                                           
         CLC   UCRSSN-UCRD(9,R6),=C'361201480'                                  
         BE    STRAC10                                                          
         CLC   UCRSSN-UCRD(9,R6),=C'154306593'                                  
         BE    STRAC10                                                          
*&&                                                                             
         CLI   TRACOPT,C'Y'                                                     
         BNE   XIT                                                              
         CP    SRTCOUNT,TRALIMIT                                                
         BH    XIT                                                              
STRAC10  MVI   MYP,X'BF'                                                        
         MVC   MYP+1(131),MYP                                                   
         MVC   MYP+60(16),RECTYPE                                               
         BAS   RE,SPLAT                                                         
         LA    R2,UCRAMTS-UCRD     KEY + DATA                                   
         BAS   RE,TRACEL                                                        
         LA    R6,UCRAMTS                                                       
         LA    R2,UCRAMTSL         L'AMOUNTS                                    
         BAS   RE,TRACEL                                                        
         B     XIT                                                              
         SPACE 1                                                                
         DS    0F                                                               
SORTFRST DC    C'Y'                                                             
SORTCARD DC    CL80'SORT FIELDS=(1,36,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=(265)'                                 
         EJECT                                                                  
*              ODD ROUTINES                                                     
         SPACE 3                                                                
YES      SR    R1,R1                                                            
         B     *+8                                                              
         SPACE 1                                                                
NO       LA    R1,1                                                             
         LTR   R1,R1                                                            
         B     XIT                                                              
         SPACE 1                                                                
SETSKIP  NTR1                                                                   
         L     R5,ASPOOLD                                                       
         USING SPOOLD,R5                                                        
         MVI   FORCEHED,C'Y'                                                    
         DROP  R5                                                               
         B     XIT                                                              
         SPACE 1                                                                
SETPAGE  NTR1                                                                   
         L     R5,ASPOOLD                                                       
         USING SPOOLD,R5                                                        
         MVC   PAGE,=H'1'                                                       
         DROP  R5                                                               
         B     XIT                                                              
         SPACE 1                                                                
SPLAT    NTR1                                                                   
         L     R5,ASPOOLD                                                       
         USING SPOOLD,R5                                                        
         MVC   P,MYP                                                            
         MVC   P2,MYP2                                                          
         MVC   P3,MYP3                                                          
         MVC   P4,MYP4                                                          
         GOTO1 SPOOL,DMCB,(R5)                                                  
         DROP  R5                                                               
         BAS   RE,MYCLEAR                                                       
         B     XIT                                                              
         SPACE 1                                                                
MYCLEAR  NTR1                                                                   
         MVI   MYP,C' '                                                         
         MVC   MYP+1(131),MYP                                                   
         MVC   MYP2,MYP                                                         
         MVC   MYP3,MYP                                                         
         MVC   MYP4,MYP                                                         
         MVC   MYSPACES,MYP                                                     
         B     XIT                                                              
XIT      XIT1                                                                   
         EJECT                                                                  
*              HEADLINES ETC                                                    
         SPACE 3                                                                
         DS    0H                                                               
         USING *,RF                                                             
HOOK     NTR1                                                                   
         L     RB,MYRB                                                          
         L     R7,MYR7                                                          
         L     R8,MYR8                                                          
         DROP  RF                                                               
         USING T7032B,RB,R7,R8                                                  
         CLI   RECNUM,UC941                                                     
         BE    HKX                                                              
         CLI   RECNUM,UCDISK       NOT FOR UK (UC DISK)                         
         BE    HKX                                                              
         CLI   RECNUM,UCTAPES      NOT FOR UC                                   
         BE    HKX                                                              
         L     R5,ASPOOLD                                                       
         USING SPOOLD,R5                                                        
         CLI   RECNUM,NYDISK                                                    
         BNE   HOOK1                                                            
         MVC   MYTITLE,SPACES      CLEAR PREVIOUS TITLE                         
         MVC   MYTITLE(7),=C'NY DISK'                                           
         SPACE 1                                                                
HOOK1    MVC   H1+48(32),MYTITLE                                                
         GOTO1 CENTER,DMCB,H1+48,32                                             
         GOTO1 UNDERLIN,DMCB,(32,H1+48),(X'BF',H2+48)                           
         CLI   THISEMP,0                                                        
         BE    HOOK2                                                            
         LA    R3,H3                                                            
         CLI   RECNUM,AGYPROF                                                   
         BNE   *+8                                                              
         LA    R3,H2                                                            
         MVC   0(20,R3),MYSPACES                                                
         MVC   0(8,R3),=C'EMPLOYER'                                             
         MVC   9(3,R3),THISEMP                                                  
         LA    R2,THISEMP                                                       
         BAS   RE,NEEDEM                                                        
         MVC   13(30,R3),NEEDNAME                                               
         CLC   THISEMP,=C'TP '                                                  
         BNE   HOOK1B                                                           
         CLC   RYEAR,=C'2004'                                                   
         BL    HOOK1B                                                           
         MVC   13(30,R3),=CL30'TALENT PARTNERS COMMERCIAL SERVICES'             
*                                                                               
HOOK1B   CLI   RECNUM,AGYPROF                                                   
         BNE   HOOK2                                                            
         CLI   THISOFF,C'A'                                                     
         BL    HOOK2                                                            
         MVC   H3(20),=C'BILLING TYPE SUMMARY'                                  
         CLI   THISOFF,X'FE'                                                    
         BE    HOOK2                                                            
         MVC   H3(20),=C'TOP 100 AGENCIES    '                                  
         CLI   THISOFF,X'FF'                                                    
         BE    HOOK2                                                            
         MVC   H3(20),=C'OFFICE SUMMARY      '                                  
         CLI   THISOFF,X'FD'                                                    
         BE    HOOK2                                                            
         MVC   H3(20),MYSPACES                                                  
         MVC   H3(6),=C'OFFICE'                                                 
         MVC   H3+9(1),THISOFF                                                  
         LA    R2,THISOFF                                                       
         BAS   RE,NEEDOF                                                        
         MVC   H3+13(30),THISOFNM                                               
         SPACE 1                                                                
HOOK2    MVC   H3+52(6),=C'PERIOD'                                              
         L     R1,APERH                                                         
         MVC   H3+59(17),8(R1)                                                  
         MVC   H6,MYH6                                                          
         MVC   H7,MYH7                                                          
         L     R2,ABOX                                                          
         USING BOXD,R2                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXOFF,0                                                         
         MVI   BOXINIT,0                                                        
         MVC   BOXCOLS,MYCOLS                                                   
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+4,C'T'                                                   
         MVI   BOXROWS+7,C'M'                                                   
         MVI   BOXROWS+58,C'B'                                                  
         CLI   RECNUM,NYDISK                                                    
         BNE   HKX                                                              
         MVI   BOXROWS+6,C'M'                                                   
         MVI   BOXROWS+7,C' '                                                   
         MVI   BOXCOLS,C'L'                                                     
         MVI   BOXCOLS+10,C'C'                                                  
         MVI   BOXCOLS+41,C'C'                                                  
         MVI   BOXCOLS+56,C'C'                                                  
         MVI   BOXCOLS+71,C'C'                                                  
         MVI   BOXCOLS+86,C'R'                                                  
         MVC   H6+1(9),=C'SS NUMBER'                                            
         MVC   H6+12(7),=C'SS NAME'                                             
         MVC   H6+43(13),=C'QUARTER GROSS'                                      
         MVC   H6+58(12),=C'ANNUAL GROSS'                                       
         MVC   H6+74(10),=C'ANNUAL TAX'                                         
         SPACE 1                                                                
HKX      XIT1                                                                   
         DROP  R2                                                               
         DROP  R5                                                               
         SPACE 1                                                                
MYSPECS  SPROG 1                                                                
         SSPEC H1,1,RUN                                                         
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,99,AGYNAME                                                    
         SSPEC H2,99,AGYADD                                                     
         SSPEC H3,99,REPORT                                                     
         SSPEC H3,112,PAGE                                                      
         SPROG 2                                                                
         DC    H'0'                                                             
         EJECT                                                                  
*              LTORG ETC                                                        
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
         SPACE 1                                                                
LSORT    EQU   176                                                              
         SPACE 3                                                                
FORMTAB  DS    0CL(FORMLNQ)                                                     
         DC    C'UT TP '                                                        
         DC    C'WI TP '                                                        
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
MONTHTAB DS    0CL2                                                             
         DC    C'PA'                                                            
         DC    C'IL'                                                            
         DC    C'CT'                                                            
         DC    C'CA'                                                            
         DC    C'WA'                                                            
         DC    C'FL'                                                            
         DC    C'KY'                                                            
         DC    C'ME'                                                            
         DC    C'NM'                                                            
         DC    C'TN'                                                            
         DC    C'TX'                                                            
         DC    X'FF'                                                            
         EJECT                                                                  
WRKFILE  DCB   DDNAME=TALWRK,DSORG=PS,MACRF=(PM,GM),EODAD=WORKEOF,     X        
               RECFM=FB,LRECL=265,BUFNO=2,BLKSIZE=26500                         
         SPACE 1                                                                
         ENTRY MYBUFF                                                           
MYBUFF   DS    0D                                                               
         DC    50000X'00'                                                       
         EJECT                                                                  
***                                                                             
       ++INCLUDE TASUIBAS                                                       
*                                                                               
         SPACE 3                                                                
POSTDETS NTR1  BASE=*                                                           
         CLI   RECNUM,UREPORT      CHECK REPORT                                 
         BE    PDUR                                                             
         CLI   RECNUM,AGYPROF                                                   
         BE    PDAP                                                             
         LA    R5,SORTIO           FOR UC, PUT A SORT RECORD HERE               
****     XC    SORTIO,SORTIO                                                    
         LA    RE,SORTIO           CLEAR SORTIO                                 
         LH    RF,=AL2(L'SORTIO)                                                
         XCEFL                                                                  
         USING UCRD,R5                                                          
         MVC   UCRFTYPE,FTYPE                                                   
         MVC   UCREMP,EXTEMP                                                    
         MVC   UCRSTATE,EXTSTATE                                                
         MVI   UCRTYPE,UCRTDET                                                  
         MVC   UCRSSN,EXTSSN                                                    
         MVC   UCRRYR,EXTYEAR       REPORTING YEAR - DISPLAYABLE                
         MVC   UCRWEEK,EXTWEEK                                                  
         MVC   UCRQUART,EXTQUART                                                
         GOTO1 USEVAL,DMCB,EXTUSE,WORK                                          
         TM    TGUSSTA2,LIVEWORK   CHECK FOR LIVE WORK                          
         BNO   *+8                                                              
         OI    UCRINDS,UCRILIVE                                                 
         XC    UCRHRSWK,UCRHRSWK                                                
         CLC   EXTSTATE,=C'DC '    IF TAXABLE STATE IS DC,                      
         BNE   PDETS3                                                           
         CLC   EXTEMP,=C'P+ '      P+ GETS ZERO HOURS                           
         BE    PDETS5                                                           
         OC    EXTERN,EXTERN       HOURS ARE ONLY FOR TAXABLE EARNINGS          
         BZ    PDETS5                                                           
PDETS3   TM    TGUSSTAT,SESSION    CHECK FOR SESSION                            
         BZ    PDETS5                                                           
         CLC   UCRQUART,QQUART     IF THIS CHECK FOR REQUESTED QUARTER          
         BNE   PDETS5                                                           
         OI    UCRINDS,UCRISESS    SESSION WAGES                                
         MVI   UCRHRSWK+1,1        DEFAULT FOR MUSICIANS                        
         CLC   =C'AFM',EXTUN       UNION WAS AFM, MUSICIAN                      
         BE    PDETS5                                                           
         MVI   UCRHRSWK+1,8        DEFAULT FOR ON-CAMERAS                       
         CLC   =C'ON ',EXTONOFF                                                 
         BE    PDETS5                                                           
         MVI   UCRHRSWK+1,2        DEFAULT FOR OFF-CAMERA                       
*                                                                               
PDETS5   TM    STSTAT,STSTATFL    IF RUNNING FOR FLORIDA,                       
         BZ    PDETS10                                                          
         CLC   UCRSTATE,=CL3'FL ' NEED TO REPORT OUT OF STATE WAGES             
         BNE   PDETS20                                                          
         B     PDETS15                                                          
*                                                                               
PDETS10  TM    STSTAT,STSTATTN    IF RUNNING FOR TENNESSEE,                     
         BZ    PDETS15                                                          
         CLC   UCRSTATE,=CL3'TN ' NEED TO REPORT OUT OF STATE WAGES             
         BNE   PDETS20                                                          
*                                                                               
PDETS15  L     R1,THISEARN                                                      
         CVD   R1,DUB                                                           
         ZAP   UCRGROSY,DUB                                                     
         L     R1,SUIWAGES                                                      
         CVD   R1,DUB                                                           
         ZAP   UCRSUIWY,DUB                                                     
         L     R1,SDIWAGES                                                      
         CVD   R1,DUB                                                           
         ZAP   UCRSDIWY,DUB                                                     
         L     R1,FUIWAGES                                                      
         CVD   R1,DUB                                                           
         ZAP   UCRFUIWY,DUB                                                     
         L     R1,FCAWAGES                                                      
         CVD   R1,DUB                                                           
         ZAP   UCRFICWY,DUB                                                     
         L     R1,MEDWAGES                                                      
         CVD   R1,DUB                                                           
         ZAP   UCRMEDWY,DUB                                                     
         L     R1,THISTAX                                                       
         CVD   R1,DUB                                                           
         ZAP   UCRTAXY,DUB                                                      
         SPACE 1                                                                
         ZAP   UCRGROSQ,=P'0'                                                   
         ZAP   UCRSUIWQ,=P'0'                                                   
         ZAP   UCRSDIWQ,=P'0'                                                   
         ZAP   UCRFUIWQ,=P'0'                                                   
         ZAP   UCRFICWQ,=P'0'                                                   
         ZAP   UCRMEDWQ,=P'0'                                                   
         ZAP   UCRTAXQ,=P'0'                                                    
         ZAP   UCROOSGW,=P'0'                                                   
         ZAP   UCROOSTW,=P'0'                                                   
         MVC   UCROOSST,MYSPACES                                                
         CLC   EXTQUART,QQUART                                                  
         BNE   PDETSX                                                           
         L     R1,THISEARN                                                      
         CVD   R1,DUB                                                           
         ZAP   UCRGROSQ,DUB                                                     
         L     R1,SUIWAGES                                                      
         CVD   R1,DUB                                                           
         ZAP   UCRSUIWQ,DUB                                                     
         L     R1,SDIWAGES                                                      
         CVD   R1,DUB                                                           
         ZAP   UCRSDIWQ,DUB                                                     
         L     R1,FUIWAGES                                                      
         CVD   R1,DUB                                                           
         ZAP   UCRFUIWQ,DUB                                                     
         L     R1,FCAWAGES                                                      
         CVD   R1,DUB                                                           
         ZAP   UCRFICWQ,DUB                                                     
         L     R1,MEDWAGES                                                      
         CVD   R1,DUB                                                           
         ZAP   UCRMEDWQ,DUB                                                     
         L     R1,THISTAX                                                       
         CVD   R1,DUB                                                           
         ZAP   UCRTAXQ,DUB                                                      
         SPACE 1                                                                
         GOTO1 DATCON,DMCB,(1,EXTCDTE),(3,WORK)                                 
         CLC   WORK+1(1),QMTH1                                                  
         BNE   *+8                                                              
         MVI   UCRMTH1,1                                                        
*                                                                               
         CLC   WORK+1(1),QMTH2                                                  
         BNE   *+8                                                              
         MVI   UCRMTH2,1                                                        
*                                                                               
         CLC   WORK+1(1),QMTH3                                                  
         BNE   *+8                                                              
         MVI   UCRMTH3,1                                                        
         B     PDETSX                                                           
*                                 FLORIDA UC ONLY                               
PDETS20  MVC   UCROOSST,UCRSTATE  OUT OF STATE CODE                             
         L     R1,THISEARN                                                      
         CVD   R1,DUB                                                           
         ZAP   UCROOSGW,DUB       OUT OF STATE GROSS WAGES                      
         L     R1,SUIWAGES                                                      
         CVD   R1,DUB                                                           
         ZAP   UCROOSTW,DUB       OUT OF STATE TAXABLE WAGES                    
*                                                                               
         TM    STSTAT,STSTATFL    IF RUNNING FOR FLORIDA,                       
         BZ    PDETS25                                                          
         MVC   UCRSTATE,=CL3'FL'  SET STATE                                     
         B     PDETS30                                                          
PDETS25  TM    STSTAT,STSTATTN    IF RUNNING FOR TENNESSEE,                     
         BNZ   *+6                                                              
         DC    H'00'              (HAS TO BE FL OR TN)                          
         MVC   UCRSTATE,=CL3'TN'  SET STATE                                     
*                                                                               
PDETS30  ZAP   UCRGROSQ,=P'0'                                                   
         ZAP   UCRSUIWQ,=P'0'                                                   
         ZAP   UCRSDIWQ,=P'0'                                                   
         ZAP   UCRFUIWQ,=P'0'                                                   
         ZAP   UCRFICWQ,=P'0'                                                   
         ZAP   UCRMEDWQ,=P'0'                                                   
         ZAP   UCRTAXQ,=P'0'                                                    
*                                                                               
PDETSX   BAS   RE,SORTPUT                                                       
         J     XIT                                                              
         DROP  R5                                                               
         SPACE 1                                                                
PDAP     GOTO1 =V(TA2BAP),DMCB,(1,(RC)),(RA)                                    
         J     XIT                                                              
         SPACE 1                                                                
PDUR     GOTO1 =V(TA2BUR),DMCB,(1,(RC)),(RA)                                    
         J     XIT                                                              
         EJECT                                                                  
***                                                                             
*              ADD DETAILS FOR THIS SORT RECORD                                 
         SPACE 3                                                                
*              INPUT               R2=A(RECORD FROM SORT)                       
*                                  R5=A(RECORD WE ARE ADDING TO)                
         SPACE 1                                                                
ADDETS   NTR1  BASE=*,LABEL=*                                                   
         USING UCRD,R2                                                          
         TM    STSTAT,STSTATFL+STSTATTN  IF RUNNING FOR FLORIDA OR TN           
         BZ    ADDETS1                                                          
         CLC   UCROOSST,MYSPACES      IF OUT OF STATE FILLED IN,                
         BNH   ADDETS1                                                          
         BRAS  RE,UPDOUT              ONLY UPDATE OUT OF STATE TOTALS           
         B     ADDETSX                                                          
ADDETS1  CLC   UCRQUART,QQUART     IF THIS CHECK FOR REQUESTED QUARTER          
         BNE   ADDETS80                                                         
         CLC   =C'PA ',UCRSTATE       IF NOT PA                                 
         BE    *+12                                                             
         TM    UCRINDS,UCRILIVE       AND IT'S A 'LIVE' PAYMENT                 
         BNO   ADDETS80                                                         
         ZIC   R1,UCRWEEK             PICK UP WEEK NUMBER                       
         LTR   R1,R1                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         BCTR  R1,0                                                             
                                                                                
         CLC   =C'PA ',UCRSTATE       IF PA, ACCUM WEEKLY EARNINGS              
         BNE   ADDETS60                                                         
         LR    RF,R1                                                            
         SLL   RF,2                   MULTIPLY BY 4                             
         LA    RF,WEEKERNS(RF)                                                  
         ICM   RE,15,0(RF)                                                      
         MVC   DUB(8),UCRGROSQ                                                  
         CVB   R7,DUB                                                           
         AR    RE,R7                                                            
         STCM  RE,15,0(RF)                                                      
                                                                                
ADDETS60 LA    R1,WEEKNOTE(R1)                                                  
         CLI   0(R1),1                ANY EARNINGS IN THAT WEEK YET?            
         BE    ADDETS80                                                         
         CLC   =C'PA ',UCRSTATE       IF PA, ACCUM WEEKLY EARNINGS              
         BNE   ADDETS65                                                         
         CLC   0(4,RF),=F'10000'      MUST EXCEED $100 TO BE COUNTED            
         BL    ADDETS80                                                         
                                                                                
ADDETS65 MVI   0(R1),1                NO - SO NOTE EARNINGS                     
                                                                                
         DROP  R2                                                               
         USING UCRD,R5                                                          
         AI    UCRWEEK,1              AND BUMP N'ACTIVE WEEKS                   
         SPACE 1                                                                
         DROP  R5                                                               
                                                                                
         USING UCRD,R2                                                          
ADDETS80 LA    RE,UCRGROSQ         RE=FROM NUMBERS                              
         DROP  R2                                                               
         USING UCRD,R5                                                          
         LA    RF,UCRGROSQ         RF=TO NUMBERS                                
         LA    R0,UCRNAMT                                                       
         SPACE 1                                                                
ADDETS90 AP    0(8,RF),0(8,RE)     ADD FROM FROM TO TO!                         
         LA    RE,8(RE)                                                         
         LA    RF,8(RF)                                                         
         BCT   R0,ADDETS90                                                      
         SPACE 1                                                                
         DROP  R5                                                               
*                                                                               
         TM    UCRINDS-UCRD(R2),UCRISESS                                        
         BNO   *+8                                                              
         OI    UCRINDS-UCRD(R5),UCRISESS                                        
*                                                                               
         CLI   UCRMTH1-UCRD(R2),0                                               
         BE    *+8                                                              
         MVI   UCRMTH1-UCRD(R5),1                                               
*                                                                               
         CLI   UCRMTH2-UCRD(R2),0                                               
         BE    *+8                                                              
         MVI   UCRMTH2-UCRD(R5),1                                               
*                                                                               
         CLI   UCRMTH3-UCRD(R2),0                                               
         BE    *+8                                                              
         MVI   UCRMTH3-UCRD(R5),1                                               
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,UCRHRSWK-UCRD(R5)                                           
         SR    RE,RE                                                            
         ICM   RE,3,UCRHRSWK-UCRD(R2)                                           
         AR    RF,RE                                                            
         STCM  RF,3,UCRHRSWK-UCRD(R5)                                           
ADDETSX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*              SET UP INFORMATION RETURNED FROM TALIM                           
         SPACE 1                                                                
RETLIM   NTR1  BASE=*,LABEL=*                                                   
         LA    R2,LIMAREA                                                       
         USING TMD,R2                                                           
         MVC   FUIMAX,TMBFUI       SET WAGE BASES                               
         MVC   SUIMAX,TMBSUI                                                    
         MVC   FCAMAX,TMBFICA                                                   
         MVC   MEDMAX,TMBMED                                                    
         MVC   SDIMAX,TMBSDI                                                    
         SPACE 1                                                                
         MVC   FUIRATE,TMRFUI      SET RATES                                    
         MVC   SUIRATE,TMRSUI                                                   
         MVC   FCARATE,TMRFICA                                                  
         MVC   MEDRATE,TMRMED                                                   
         MVC   SDIRATE,TMRSDI                                                   
         SPACE 1                                                                
         MVC   FUI,TMXFUI          SET TAXES                                    
         MVC   SUI,TMXSUI                                                       
         MVC   FICA,TMXFICA                                                     
         MVC   MEDICARE,TMXMED                                                  
         MVC   SDI,TMOSDI                                                       
         SPACE 1                                                                
         MVC   FUIWAGES,TMTFUI     SET TAXABLE AMOUNTS                          
         MVC   SUIWAGES,TMTSUI     (TAKEN FROM TAYE ELEMENT)                    
         MVC   FCAWAGES,TMTFICA    ****                                         
         MVC   MEDWAGES,TMTMED                                                  
         MVC   SDIWAGES,TMTSDI                                                  
                                                                                
RETLIMX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*              POST DATE NUMBERS INTO EXTRACT DATA                              
         SPACE 3                                                                
POSTDATE NTR1  BASE=*,LABEL=*                                                   
         LA    R1,1                                                             
         LA    R2,WEEKLIST         LOOK UP WEEK NUMBER                          
         LA    R0,53                                                            
         SPACE 1                                                                
POSTD2   STC   R1,EXTWEEK                                                       
         CLC   EXTCDTE,0(R2)                                                    
         BNH   POSTD4                                                           
         LA    R1,1(R1)                                                         
         LA    R2,3(R2)                                                         
         BCT   R0,POSTD2                                                        
         SPACE 1                                                                
POSTD4   GOTO1 DATCON,DMCB,(1,EXTCDTE),(3,WORK)                                 
         MVC   EXTMONTH,WORK+1     MONTH NUMBER FROM DATE                       
         ZIC   R1,EXTMONTH                                                      
         BCTR  R1,0                                                             
         SR    R0,R0                                                            
         D     R0,=F'3'                                                         
         LA    R1,1(R1)                                                         
         STC   R1,EXTQUART         QUARTER # FROM MONTH                         
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE CALCS SDI TAXABLE AMOUNT BASED ON STATE EARNINGS         
*                                                                               
SDITXBL  NTR1  BASE=*,LABEL=*                                                   
         L     R3,EXTERN                                                        
         LPR   R3,R3               SET AMT TO (+) FOR CALCULATION               
*                                                                               
         L     R5,TMBSDI           MAX TAXABLE AMOUNT                           
         L     R4,YTDSEARN         YTD WITHOUT THIS CHECK                       
         S     R4,EXTERN                                                        
         TM    YTDFEARN,X'80'                                                   
         BZ    *+6                                                              
         XR    R4,R4                                                            
*                                                                               
         SR    R5,R4               R1 <= MAX - YTD                              
         BZ    SDI20               YTD = MAX, WE CAN USE CHECK                  
         BP    SDI10                                                            
         L     R3,EXTERN           ADJUST SDI WAGE FOR THIS CHECK               
         LCR   R5,R5               POSTIVE DIFFERENCE                           
         AR    R3,R5               CHECK + DIFF = SDI WAGE                      
         BP    SDIX                                                             
         B     SDI30                                                            
*                                                                               
SDI10    CR    R5,R3               IF MAX TAXABLE >= PAYMENT                    
         BNL   SDI20               USE ENTIRE AMOUNT                            
         TM    EXTERN,X'80'                                                     
         BO    SDI25                                                            
         LR    R3,R5                                                            
*                                                                               
SDI20    TM    EXTERN,X'80'        IF THIS IS A NEGATIVE AMOUNT                 
         BNO   *+6                                                              
SDI25    LCR   R3,R3               RESTORE AMT TO (-) FOR CALCULATION           
SDI30    ST    R3,TMTSDI           AMOUNT TAXED THIS INVOICE                    
         B     SDIX                                                             
*                                                                               
SDIX     XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO READ W2 RECORD FOR A SSN                              
*              FOR ANNUAL GROSS AND TAX                                         
         SPACE                                                                  
         USING UCRD,R5                                                          
READW2   NTR1  BASE=*,LABEL=*                                                   
         ZAP   UCRGROSY,=P'0'                                                   
         ZAP   UCRTAXY,=P'0'                                                    
*                                                                               
         GOTO1 DATCON,DMCB,(1,TIQPEND),(20,WORK)                                
         MVC   TGYEAR,WORK         CCYY                                         
         MVI   TGCUR,C'U'          US CURRENCY ONLY                             
*                                                                               
         MVC   SYSDIR,=CL8'CHKDIR' SET TO READ CHECK FILE                       
         MVC   SYSFIL,=CL8'CHKFIL'                                              
         GOTO1 RECVAL,DMCB,TLW2CDQ,(X'A4',UCRSSN)                               
         MVC   SYSDIR,=CL8'TALDIR' RESET BACK TO TALENT FILE                    
         MVC   SYSFIL,=CL8'TALFIL'                                              
         BNE   READW2X                                                          
*                                                                               
         MVI   ELCODE,TAW2ELQ      LOOK FOR NYC W2 DETAILS ELEMENT              
         GOTO1 GETL,DMCB,(3,=C'NYC')                                            
         BNE   READW210                                                         
         L     R4,TGELEM                                                        
         USING TAW2D,R4                                                         
*                                                                               
         L     R1,TAW2TAX          ADD NYC YTD TAXES                            
         CVD   R1,DUB                                                           
         AP    UCRTAXY,DUB                                                      
*                                                                               
READW210 MVI   ELCODE,TAW2ELQ      LOOK FOR NY W2 DETAILS ELEMENT               
         GOTO1 GETL,DMCB,(3,=C'NY ')                                            
         BNE   READW2X                                                          
         L     R4,TGELEM                                                        
         USING TAW2D,R4                                                         
*                                                                               
         L     R1,TAW2EARN         OVERRIDE YTD GROSS WAGES                     
         CVD   R1,DUB                                                           
         ZAP   UCRGROSY,DUB                                                     
         L     R1,TAW2TAX          OVERRIDE YTD TAXES                           
         CVD   R1,DUB                                                           
         AP    UCRTAXY,DUB                                                      
*                                                                               
READW2X  J     XIT                                                              
         DROP  R5                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*        UPDATE OUT OF STATE TOTALS (FLORIDA UCDISK)                            
*                                                                               
UPDOUT   NTR1  BASE=*,LABEL=*                                                   
         USING UCRD,R2                                                          
         MVC   FRMSTATE,UCROOSST   SAVE OUT OF STATE CODE                       
         LA    RE,UCROOSGW         RE=FROM NUMBERS                              
         DROP  R2                                                               
         USING UCRD,R5                                                          
         LA    RF,UCROOSGW         RF=TO NUMBERS                                
         LA    R0,2                                                             
UPOUT20  AP    0(8,RF),0(8,RE)     ADD FROM FROM TO TO!                         
         LA    RE,8(RE)                                                         
         LA    RF,8(RF)                                                         
         BCT   R0,UPOUT20                                                       
*                                                                               
         CLC   UCROOSST,=CL3'MU'   IF MULTIPLE STATES DONE                      
         BE    UPOUTX                                                           
         CLC   UCROOSST,MYSPACES   IF NO STATE YET,                             
         BH    UPOUT30                                                          
         MVC   UCROOSST,FRMSTATE   USE FROM STATE                               
         B     UPOUTX                                                           
UPOUT30  CLC   UCROOSST,FRMSTATE   IF SAME STATE, OKAY                          
         BE    UPOUTX                                                           
         MVC   UCROOSST,=CL3'MU'   MULTIPLE STATES                              
         DROP  R5                                                               
UPOUTX   XIT1                                                                   
FRMSTATE DS    CL3                                                              
         LTORG                                                                  
         EJECT                                                                  
FORMD    DSECT                                                                  
FSTATE   DS    CL3                                                              
FEMP     DS    CL3                                                              
FORMLNQ  EQU   *-FORMD                                                          
         EJECT                                                                  
       ++INCLUDE TAREP2BMY                                                      
       ++INCLUDE TAREP2BUCR                                                     
         EJECT                                                                  
         DSECT                                                                  
       ++INCLUDE TALIMD                                                         
         EJECT                                                                  
*              OTHER DSECTS ARE HIDDEN IN HERE                                  
         SPACE 3                                                                
*TAREPWORKD                                                                     
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*DDMASTD                                                                        
*DDLOGOD                                                                        
*DDREMOTED                                                                      
*TAGENFILE                                                                      
*TASYSDSECT                                                                     
*DDTWADCONS                                                                     
*FAFACTS                                                                        
*FATIOB                                                                         
*TAREPFFD                                                                       
         PRINT OFF                                                              
       ++INCLUDE TAREPWORKD                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDLOGOD                                                        
       ++INCLUDE DDREMOTED                                                      
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSDSECT                                                     
TWADCOND DSECT                                                                  
       ++INCLUDE DDTWADCONS                                                     
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FATIOB                                                         
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE TAREPFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE TAREPEBD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPCBD                                                       
         SPACE 1                                                                
       ++INCLUDE DDGENTWA                                                       
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'212TAREP2B   03/12/15'                                      
         END                                                                    
