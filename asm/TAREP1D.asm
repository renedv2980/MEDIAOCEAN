*          DATA SET TAREP1D    AT LEVEL 056 AS OF 08/13/14                      
*PHASE T7031DC,*                                                                
         TITLE 'T7031D - CKPURGE REPORT'                                        
T7031D   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 STTABEQU*2,T7031D,R6,R5                                          
         LR    R3,RC                                                            
         L     RC,0(R1)                                                         
         USING GEND,RC             RC=A(CONTROLLER W/S)                         
         L     RA,ATWA                                                          
         USING T703FFD,RA          RA=A(SCREEN)                                 
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          R9=A(SYSTEM W/S)                             
         LA    R7,BUFF                                                          
         LA    R7,8(R7)                                                         
         USING TPD,R7              R7=A(LOCAL W/S)                              
         ST    R3,AQTRSTTB         SAVE A(QTRSTTAB)                             
         AH    R3,=Y(STTABEQU)                                                  
         ST    R3,AYTDSTTB         SAVE A(YTDSTTAB)                             
*                                                                               
         L     R1,ASPOOLD                                                       
         USING SPOOLD,R1           R1=A(SPOOL DSECT)                            
         MVC   MYSORTER,SORTER     A(SORTER)                                    
         L     R2,ABOX                                                          
         ST    R2,MYABOX           A(BOXES)                                     
         USING BOXD,R2                                                          
         L     R8,BOXAWIDE                                                      
         USING WIDED,R8                                                         
         DROP  R1,R2                                                            
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
         GOTO1 MYSORTER,DMCB,SORTCARD,RECCARD  INITIALIZE                       
         BAS   RE,PREP                         PRINT REPORT                     
         GOTO1 MYSORTER,DMCB,=C'END'           CLOSE                            
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE SCREEN                                                  
         SPACE 1                                                                
VKEY     NTR1                                                                   
         LR    RE,R7               CLEAR LOCAL W/S                              
         LH    RF,=Y(TPLNQ)                                                     
         XCEFL                                                                  
*                                                                               
         XC    TIFILTS,TIFILTS     CLEAR SYSIO FILTERS                          
*                                                                               
         LA    R2,SCPPDH           R2=A(FIELD)                                  
         LA    R3,BLOCK                                                         
         USING PERVALD,R3                                                       
         GOTO1 PDVAL,DMCB,(R3)     GO TO SYSTEM PERIOD VAL. ROUTINE             
         MVC   TPPERIOD,PVALCPER   SAVE DISPLAYABLE PERIOD IN W/S               
         MVC   TIQPSTR,PVALPSTA    SET PWOS DATES FOR SYSIO                     
         MVC   TIQPEND,PVALPEND                                                 
         SPACE 1                                                                
         BAS   RE,VALOPT           VALIDATE OPTIONS                             
         SPACE 1                                                                
         TM    TPOPTS,TPSUMM       IF SUMMARY OPTION                            
         BZ    VKEYX                                                            
         LA    R2,CONOUTH          MUST HAVE OUTPUT                             
         GOTO1 ANY                 (HOPEFULLY 1PP)                              
         SPACE 1                                                                
VKEYX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE OPTIONS                                      
VALOPT   NTR1                                                                   
         LA    R2,SCPOPTH          VALIDATE OPTIONS                             
         CLI   5(R2),0                                                          
         BE    VOPTX                                                            
*                                                                               
         XC    BLOCK,BLOCK         CLEAR SCAN BLOCK                             
         LA    R3,BLOCK            R3=A(SCAN BLOCK)                             
         USING SCAND,R3                                                         
         GOTO1 SCANNER,DMCB,(R2),(R3)                                           
         CLI   4(R1),0                                                          
         BE    FLDINV                                                           
         ZIC   R0,4(R1)            R0=N'SCAN BLOCK ENTRIES                      
*                                                                               
VOPT4    CLC   =C'TRACE',SCDATA1  TRACE                                         
         BNE   VOPT5                                                            
         CLI   SCDATA2,C'Y'                                                     
         BNE   FLDINV                                                           
         OI    TPOPTS,TPTRACE      SET TRACE ON                                 
         B     VOPT10              (REMOVE WIDTH=166 JCL CARD TO WORK)          
*                                                                               
VOPT5    CLC   =C'NOBOX',SCDATA1   NO BOXES                                     
         BNE   VOPT6                                                            
         OI    TPOPTS,TPNOBOX      SET NO BOXES                                 
         B     VOPT10                                                           
*                                                                               
VOPT6    CLC   =C'EMP',SCDATA1     EMPLOYER FILTER                              
         BNE   VOPT6X                                                           
         MVC   TGEMP,SCDATA2                                                    
         GOTO1 RECVAL,DMCB,TLEMCDQ,0                                            
         BNE   FLDINV                                                           
         B     VOPT10                                                           
*                                                                               
VOPT6X   CLC   =C'AGY',SCDATA1     AGENCY FILTER - TESTING ONLY                 
         BNE   VOPT7                                                            
         MVC   TGAGY,SCDATA2                                                    
         GOTO1 RECVAL,DMCB,TLAYCDQ,0                                            
         BNE   FLDINV                                                           
         B     VOPT10                                                           
*                                                                               
VOPT7    CLC   =C'DELETE',SCDATA1  DELETE FILTER                                
         BNE   VOPT8                                                            
         OI    TPOPTS,TPDELETE     DELETE THE CHECK RECORD                      
         B     VOPT10                                                           
*                                                                               
VOPT8    CLC   =C'AUDIT',SCDATA1   AUDIT FILTER                                 
         BNE   VOPT9                                                            
         OI    TPOPTS,TPAUDIT      FIND THE DIFF BETWEEN GROSS/NET              
         B     VOPT10                                                           
*                                                                               
VOPT9    CLC   =C'SUMMARY',SCDATA1 SUMMARY OPTION                               
         BNE   VOPT9A                                                           
         OI    TPOPTS,TPSUMM       NO DETAIL/JUST HIGH LEVEL TOTALS             
         B     VOPT10                                                           
*                                                                               
VOPT9A   CLC   =C'MYCKRD',SCDATA1 SUMMARY OPTION                                
         BNE   FLDINV                                                           
         OI    TPOPTS,TPMYCKRD    READ CHECK RECORDS MYSELF                     
         B     VOPT10                                                           
*                                                                               
VOPT10   LA    R3,SCANNEXT         BUMP TO NEXT                                 
         BCT   R0,VOPT4            AND CONTINUE                                 
VOPTX    B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              ROUTINE CONTROLS REPORT GENERATION                               
         SPACE 1                                                                
PREP     NTR1                                                                   
         XC    KPCOUNT,KPCOUNT     CLEAR DELETED CHECK RECORD COUNTER           
         BAS   RE,CLRQTR           PRECLEAR QTRSTTAB                            
         BAS   RE,CLRYTD           PRECLEAR YTDSTTAB                            
         L     R2,ASPOOLD                                                       
         USING SPOOLD,R2                                                        
         LA    R1,MYSPECS          SET A(SPECS) FOR PRINTING                    
         ST    R1,SPECS                                                         
         MVI   RCSUBPRG,0                                                       
         TM    TPOPTS,TPNOBOX      IF NO BOXES REQUESTED                        
         BZ    *+8                                                              
         MVI   RCSUBPRG,1          PRINT DASHES UNDER HEADINGS                  
         LA    R1,HOOK             SET A(HEADLINE HOOK)                         
         ST    R1,HEADHOOK                                                      
         SPACE 1                                                                
         TM    TPOPTS,TPMYCKRD     IF READING CHECKS MYSELF                     
         BZ    PREP20                                                           
         XC    MYCDTE,MYCDTE                                                    
         XC    MYINV,MYINV                                                      
         BAS   RE,RDCHKS           READ CHECK FILE                              
         BAS   RE,RDCHK2           READ SOME EXTRA CHECKS                       
         B     PREP50                                                           
*                                                                               
PREP20   MVC   TIACOMFC,ACOMFACS   SET UP SYSIO BLOCK                           
         MVC   TIHOOK,=A(IOHOOK)   A(I/O HOOK)                                  
         MVC   TIACCS,TWAACCS      LIMIT ACCESS                                 
         MVC   TIAUTH,TWAAUTH      AUTHORIZATION                                
         MVC   TIUSERID,TWAORIG    REQUESTING ID                                
         MVI   TIREAD,TLCKCDQ      READ CHECK RECORDS                           
         MVI   TIQDTYPE,TIQDCHK    SET FILTERING ON CHECK DATE                  
         OI    TIQFLAGS,TIQFPDUM+TIQFPBNP        PASS DUMMY AND BNP             
         OI    TIQFLAG2,TIQFPGRY+TIQFSUB+TIQFPRI PASS GREY AND SPLITS           
         MVC   TIFEMP,TGEMP        EMPLOYER FILTER                              
         MVC   TIFAGY,TGAGY        AGENCY FILTER                                
         SPACE 1                                                                
         GOTO1 TASYSIO,DMCB,TASYSIOD  OFF TO SYSIO TO DO I/O                    
         SPACE 1                                                                
PREP50   BAS   RE,GETSORT          GET SORT RECS/DELETE/PRINT REPORT            
         SPACE 1                                                                
         XR    R1,R1               SET TOTAL NUMBER OF PAGES                    
         ICM   R1,3,PAGE                                                        
         BCTR  R1,0                                                             
         STCM  R1,3,KPPAGES                                                     
         TM    TPOPTS,TPSUMM       IF SUMMARY OPTION                            
         BZ    *+8                                                              
         BAS   RE,PRNTOT           PRINT SUMMARY TOTALS                         
         SPACE 1                                                                
         TM    TPOPTS,TPDELETE     IF DELETING                                  
         BZ    *+8                                                              
         BAS   RE,UPDTLSY          UPDATE SYSTEM RECORD                         
         SPACE 1                                                                
PREPX    B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*              ROUTINE TO UPDATE SYSTEM RECORD WITH DELETED COUNT               
         SPACE                                                                  
UPDTLSY  NTR1                                                                   
         MVC   AIO,AIO1                                                         
         GOTO1 RECVAL,DMCB,TLSYCDQ,(X'B0',0)                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    ELEMENT,ELEMENT     ADD DELETE COUNT ELEMENT                     
         LA    R4,ELEMENT                                                       
         USING TADCD,R4                                                         
         MVI   TADCEL,TADCELQ      SET ELEMENT CODE                             
         MVI   TADCLEN,TADCLNQ     SET ELEMENT LENGTH                           
         MVI   TADCTYPE,TADCTKP    SET FROM CKPURGE/REPORT                      
         MVC   TADCDATE,TGTODAY1   SET DATE DELETED FROM FILE                   
         MVC   TADCCNT,KPCOUNT     SET TOTAL RECORD COUNT                       
         MVC   TADCPGS,KPPAGES     SET TOTAL PAGE COUNT                         
         GOTO1 ADDELEM                                                          
*                                                                               
         GOTO1 PUTREC                                                           
         GOTO1 MYTRACE                                                          
         B     XIT                                                              
         DROP  R4                                                               
         SPACE 2                                                                
*              PRINT SUMMARY TOTALS                                             
*                                                                               
PRNTOT   NTR1                                                                   
         OC    KPCOUNT,KPCOUNT                                                  
         BZ    PRNTOT5                                                          
         MVC   XP+2(24),=CL24'CHECKS MARKED DELETED = '                         
         EDIT  KPCOUNT,(13,XP+27),ALIGN=LEFT,COMMAS=YES                         
         BAS   RE,PRNTIT                                                        
*                                                                               
PRNTOT5  OC    KPPAGES,KPPAGES                                                  
         BZ    PRNTOTX                                                          
         MVC   XP+2(24),=CL24'TOTAL NUMBER OF PAGES = '                         
         EDIT  KPPAGES,(13,XP+27),ALIGN=LEFT,COMMAS=YES                         
         BAS   RE,PRNTIT                                                        
PRNTOTX  B     XIT                                                              
         EJECT                                                                  
*              READ FOR CHECKS MYSELF                                           
RDCHKS   NTR1                                                                   
         MVC   AIO,AIO1                                                         
         BAS   RE,SETCHK           SET FILE TO CHECKS                           
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING TLCKD,R4                                                         
         MVI   TLCKCD,TLCKCDQ      SET RECORD TYPE                              
         MVC   TLCKAGY,TGAGY       SET AGENCY START IF AROUND                   
         GOTO1 HIGH                                                             
         B     RDCHK20                                                          
*                                                                               
RDCHK10  GOTO1 SEQ                                                              
         LA    R4,KEY                                                           
*                                                                               
RDCHK20  CLC   KEY(TLCKAGY-TLCKCD),KEYSAVE                                      
         BNE   RDCHKX                                                           
         OC    TGAGY,TGAGY                                                      
         BZ    *+14                                                             
         CLC   TLCKAGY,TGAGY                                                    
         BNE   RDCHKX                                                           
*                                                                               
         CLI   TLCKINV+5,0         IF NOT NEW TP INVOICE                        
         BE    RDCHK10                                                          
         MVC   TIAGY,TLCKAGY                                                    
         MVC   TISSN,TLCKSSN                                                    
         MVC   TIDSKADD,KEY+TLDRDA-TLDRD                                        
*                                                                               
         GOTO1 GETREC              GET THE RECORD                               
*                                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,TACDELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   RDCHK10                                                          
*                                                                               
         USING TACDD,R4                                                         
         CLC   TACDDTE,TIQPSTR     IF CHECK DT W/IN PERIOD                      
         BL    RDCHK10                                                          
         CLC   TACDDTE,TIQPEND                                                  
         BH    RDCHK10                                                          
*                                                                               
         XC    MYCID,MYCID         SAVE INTERNAL COMMERCIAL NUMBER              
         L     R4,AIO              FOR LATER USE                                
         MVI   ELCODE,TAPDELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAPDD,R4                                                         
         MVC   MYCID(4),TAPDCOM                                                 
         MVC   TIEMP,TAPDEMP                                                    
*                                                                               
         BAS   RE,PROCCHK          PROCESS THE CHECK RECORD                     
         B     RDCHK10             LOOK FOR NEXT                                
*                                                                               
RDCHKX   BAS   RE,SETTAL           SET FILE BACK TO TALENT                      
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO PICK UP SOME CHECKS FROM 999999                       
         SPACE 1                                                                
RDCHK2   NTR1                                                                   
         MVC   AIO,AIO1                                                         
         BAS   RE,SETCHK           SET FILE TO CHECKS                           
*                                                                               
         LA    R2,XTRCHKS          R2=A(EXTRA CHECK TABLE)                      
RDCHK4   CLI   0(R2),X'FF'                                                      
         BE    RDCHK2X                                                          
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING TLCKD,R4                                                         
         MVI   TLCKCD,TLCKCDQ      SET RECORD TYPE                              
         MVC   TLCKAGY,=C'999999'  SET AGENCY                                   
         MVC   TLCKINV,0(R2)       INVOICE NUMBER                               
         GOTO1 HIGH                                                             
         CLC   KEY(TLCKSORT-TLCKD),KEYSAVE                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   TISSN,TLCKSSN                                                    
         MVC   TIDSKADD,KEY+TLDRDA-TLDRD                                        
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         XC    MYCID,MYCID         SAVE INTERNAL COMMERCIAL NUMBER              
         L     R4,AIO              FOR LATER USE                                
         MVI   ELCODE,TAPDELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAPDD,R4                                                         
         MVC   MYCID(4),TAPDCOM                                                 
         MVC   TIEMP,TAPDEMP                                                    
*                                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,TAOKELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAOKD,R4                                                         
         MVC   MYCDTE,TAOKDTE      SET CHECK DATE                               
*                                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,TAOIELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAOID,R4                                                         
         MVC   TIAGY,TAOIAGY                                                    
         MVC   MYINV,TAOIINV                                                    
*                                                                               
         BAS   RE,PROCCHK          PROCESS THE CHECK                            
         LA    R2,L'XTRCHKS(R2)    BUMP TO NEXT IN TABLE                        
         B     RDCHK4                                                           
*                                                                               
RDCHK2X  BAS   RE,SETTAL           SET FILE BACK TO TALENT                      
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              PROCESS RECORDS FROM SYSIO AND WRITE TO SORT                     
IOHOOK   NTR1                                                                   
         CLI   TIMODE,PROCINV      IF PROCESSING INVOICES                       
         BNE   IOHK10                                                           
         MVC   MYCID,XSPACES       PRECLEAR COMMERCIAL ID                       
         L     R4,TIAREC           R4=A(INVOICE RECORD)                         
         MVI   ELCODE,TACOELQ                                                   
         USING TACOD,R4            R4=A(COMM DETAILS ELEMENT)                   
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   MYCID,TACOCID       SAVE COMMERCIAL ID                           
         B     IOHKX                                                            
*                                                                               
IOHK10   CLI   TIMODE,PROCREC      PROCESSING CHECK RECORDS                     
         BNE   IOHKX                                                            
         L     R4,TIAREC           R4=A(CHECK RECORD)                           
         ST    R4,AIO                                                           
         BAS   RE,PROCCHK          PROCESS THE CHECK RECORD                     
*                                                                               
IOHKX    MVC   AIO,AIO1                                                         
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO EXTRACT CHECK INFO AND ADD A SORT RECORD              
*              AIO = CHECK RECORD                                               
         SPACE 1                                                                
PROCCHK  NTR1                                                                   
         L     R4,AIO              R4=A(CHECK RECORD)                           
         USING TLCKD,R4                                                         
**NO-OP* TM    TLCKSTAT,TLCKSDEL   SKIP CHECKS MARKED DELETED                   
**NO-OP* BO    IOHKX               (AS PER DAVID HABER 8/16/93)                 
         SPACE 1                                                                
         LA    R2,SRTREC           R2=A(SORT RECORD)                            
         USING SORTD,R2                                                         
         MVC   SRTREC,XSPACES      INITIALIZE RECORD                            
         LA    R0,NAMTS                                                         
         LA    RE,SORTAMTS                                                      
PRCHK20  ZAP   0(L'SORTAMTS,RE),=P'0'                                           
         LA    RE,L'SORTAMTS(RE)                                                
         BCT   R0,PRCHK20                                                       
*                                                                               
         MVC   SORTCKDA,TIDSKADD   D/A OF CHECK RECORD                          
         MVC   SORTEMP,TIEMP       EMPLOYER                                     
         MVC   SORTAGY,TIAGY       AGENCY                                       
         MVC   SORTCID,MYCID       COMMERCIAL ID                                
*                                                                               
         MVC   SORTW4,TISSN        SET TO SORT BY S/S NUMBER                    
         MVI   ELCODE,TATIELQ                                                   
         MVI   BYTE,TATITYCO       LOOK FOR TAX ID ELEMENT FOR CORP.            
         GOTO1 GETL,DMCB,(1,BYTE)                                               
         BNE   PRCHK30                                                          
         MVC   SORTISSN,SORTW4     SAVE INDIVIDUAL SSN                          
         L     R1,TGELEM                                                        
         USING TATID,R1                                                         
         MVC   SORTW4,TATIID       SORT BY CORP ID                              
         DROP  R1                                                               
*                                                                               
PRCHK30  BAS   RE,XPDEL            EXTRACT DATA FROM PAY DETAILS EL.            
         BAS   RE,XCAEL                              CAST DETAILS EL.           
         BAS   RE,XCDEL                              CHECK DETAILS EL.          
         BAS   RE,XCWELS                             WITHHOLDING ELS.           
         BAS   RE,XDWELS                             DUE CO WITHOLD EL.         
         BAS   RE,XLWELS                             LIEN WITHOLD ELS.          
         BAS   RE,XODELS                             OTHER WITHHOLDING          
         BAS   RE,XCMEL                              CHECK COMMENT EL.          
*                                                                               
         GOTO1 MYSORTER,DMCB,=C'PUT',(R2)   WRITE OUT SORT RECORD               
         B     XIT                                                              
         DROP  R2,R4                                                            
         EJECT                                                                  
*              EXTRACT DATA FROM PAYMENT DETAILS ELEMENT                        
         SPACE 1                                                                
         USING SORTD,R2            R2=A(SORT RECORD)                            
         USING TLCKD,R4            R4=A(RECORD)                                 
XPDEL    NTR1                                                                   
         MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS ELEMENT                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAPDD,R4            R4=A(PAYMENT DETAILS EL.)                    
         SPACE 1                                                                
         MVI   SORTPRT,SORTNOP     DON'T PRINT - JUST DELETE                    
         TM    TAPDPSTS,TAPDPBNP   IF BILL NO PAYROLL CHECK                     
         BO    XPDEL5                                                           
         TM    TAPDOPT3,TAPDODUM   OR IF DUMMY CHECK                            
         BO    XPDEL5                                                           
         TM    TAPDSTA2,TAPDSSUB   OR IF SUBSIDIARY CHECK                       
         BO    XPDEL5                                                           
         NI    SORTPRT,X'FF'-SORTNOP ELSE OKAY TO PRINT                         
*                                                                               
XPDEL5   MVC   SORTINV,TAPDINV     INVOICE NUMBER                               
         OC    MYINV,MYINV         IF OVERRIDE INVOICE NUMBER                   
         BZ    *+10                                                             
         MVC   SORTINV,MYINV       USE IT                                       
         GOTO1 USEVAL,DMCB,TAPDUSE,TAPDTYPE                                     
         MVI   SORTPTYP,SORTREUS   ASSUME REUSE                                 
         TM    TGUSSTAT,SESSION    TEST FOR SESSION TYPE                        
         BZ    *+8                                                              
         MVI   SORTPTYP,SORTSESS                                                
         MVI   SORTCUR,C'U'        US$                                          
         TM    TAPDSTAT,TAPDSCAN   IF CANADIAN DOLLARS                          
         BZ    *+8                                                              
         MVI   SORTCUR,C'C'        SET CAN$                                     
*                                                                               
         L     R1,TAPDREXP         REIMBURSED EXPENSES                          
         CVD   R1,SORTREXP                                                      
         L     R1,TAPDMDED         MISC DEDUCTIONS                              
         CVD   R1,SORTMDED                                                      
         L     R1,TAPDDUES         UNION DUES                                   
         CVD   R1,SORTDUES                                                      
*                                                                               
         L     R1,TAPDPAYI         IF PAYI+PAYC+REXP <> SPNH                    
         A     R1,TAPDPAYC                                                      
         A     R1,TAPDREXP                                                      
         C     R1,TAPDSPNH                                                      
         BE    *+8                                                              
         OI    SORTPRT,SORTSUBP    PRINT SUBJECT TO PNH AT DETAIL LINE          
         L     R1,TAPDSPNH         SUBJECT TO PNH                               
         CVD   R1,SORTSPNH                                                      
XPDELX   B     XIT                                                              
         DROP  R2,R4                                                            
         EJECT                                                                  
*              EXTRACT DATA FROM CAST DETAILS ELEMENT                           
         SPACE 1                                                                
         USING SORTD,R2            R2=A(SORT RECORD)                            
         USING TLCKD,R4            R4=A(RECORD)                                 
XCAEL    NTR1                                                                   
         XC    SORTFRST,SORTFRST   CLEAR FIRST SERVICE DATE                     
         MVI   ELCODE,TACAELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   XCAELX                                                           
         USING TACAD,R4            R4=A(CAST DETAILS EL.)                       
         SPACE 1                                                                
         MVC   SORTUN,TACAUN       UNION                                        
         MVC   SORTNCDE,TACANCDE   AGENT CODE                                   
         MVC   SORTFRST,TACAFRST   FIRST SERVICE DATE                           
XCAELX   B     XIT                                                              
         DROP  R2,R4                                                            
         EJECT                                                                  
*              EXTRACT DATA FROM CHECK DETAILS ELEMENT                          
         SPACE 1                                                                
         USING SORTD,R2            R2=A(SORT RECORD)                            
         USING TLCKD,R4            R4=A(RECORD)                                 
XCDEL    NTR1                                                                   
         MVI   ELCODE,TACDELQ      GET CHECK DETAILS ELEMENT                    
         BAS   RE,GETEL            IF NONE FOUND                                
         BE    XCDEL5                                                           
         TM    SORTPRT,SORTNOP     OKAY IF NOT PRINTING                         
         BO    XCDELX                                                           
         DC    H'0'                ELSE DIE                                     
         SPACE 1                                                                
         USING TACDD,R4            R4=A(CHECK DETAILS EL.)                      
XCDEL5   MVC   SORTCHK,TACDCHK     CHECK NUMBER                                 
         OC    TACDCHK,TACDCHK     IF NO CHECK NUMBER                           
         BNZ   *+10                                                             
         MVC   SORTCHK,=8C'0'      FILL IT WITH ZEROS                           
         MVC   SORTDTE,TACDDTE     CHECK DATE                                   
         OC    MYCDTE,MYCDTE       IF OVERRIDE CHECK DATE                       
         BZ    *+10                                                             
         MVC   SORTDTE,MYCDTE      USE IT                                       
         BAS   RE,QTRIT                                                         
         MVC   SORTQTR,CURQTR      SET QUARTER OF CHECK DATE                    
*                                                                               
         L     R1,TACDNET          CHECK AMOUNT                                 
         CVD   R1,SORTNET                                                       
         L     R1,TACDEARN         GROSS EARNINGS                               
         CVD   R1,SORTEARN                                                      
         L     R1,TACDNTAX         NON-TAXABLE - REIMBURSED EXPENSES            
         CVD   R1,SORTNTAX                                                      
         SP    SORTNTAX,SORTREXP   IS EQUAL TO NON-TAXABLE EARNINGS             
*                                                                               
XCDELX   B     XIT                                                              
         DROP  R2,R4                                                            
         SPACE 2                                                                
*              ROUTINE TO CALCULATE QUARTER                                     
*                                  XIT - CURQTR SET                             
         USING SORTD,R2            R2=A(SORT RECORD)                            
         SPACE                                                                  
QTRIT    NTR1                                                                   
**NO-OP**LA    R1,SORTDTE          R1=A(PWOS CHECK DATE)                        
*        MVO   DUB,0(1,R1)                                                      
*        OI    DUB+7,X'0F'                                                      
*        CVB   RF,DUB                                                           
**NO-OP**STC   RF,CURBYR           BINARY YEAR                                  
*                                                                               
         MVO   DUB,1(1,R1)                                                      
         CVB   R1,DUB              R1=BINARY MONTH                              
         AH    R1,=H'2'            PLUS 2                                       
         XR    R0,R0                                                            
         D     R0,=F'3'            DIVIDE BY 3                                  
         STC   R1,CURQTR           QUARTER (1-4)                                
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*              EXTRACT DATA FROM CHECK WITHHOLDING ELEMENTS                     
         SPACE 1                                                                
         USING SORTD,R2            R2=A(SORT RECORD)                            
         USING TLCKD,R4            R4=A(RECORD)                                 
XCWELS   NTR1                                                                   
         MVI   ELCODE,TACWELQ      GET CHECK WITHHOLDING ELEMENTS               
         BAS   RE,GETEL                                                         
         BNE   XCW8X                                                            
         USING TACWD,R4            R4=A(CHECK WITHHOLDING EL.)                  
         SPACE 1                                                                
XCW2     CLC   TACWUNIT(2),=C'FD'  TEST FOR FEDERAL                             
         BNE   XCW4                                                             
         L     R1,TACWTAX          FEDERAL TAXES                                
         CVD   R1,SORTFTAX                                                      
         L     R1,TACWFICA         FICA                                         
         CVD   R1,SORTFICA                                                      
         B     XCW8                                                             
         SPACE 1                                                                
XCW4     CLC   TACWUNIT,=C'NYN'    IF UNIT IS NYN TREAT LIKE STATE              
         BE    *+12                                                             
         CLI   TACWUNIT+2,C' '     TEST FOR STATE                               
         BNE   XCW6                                                             
*                                                                               
         TM    TACWSTAT,TACWSTAX                                                
         BZ    *+10                                                             
         MVC   SORTTXST,TACWUNIT   SORT BY TAXABLE STATE                        
         TM    TACWSTAT,TACWSWRK                                                
         BZ    *+10                                                             
         MVC   SORTSOW,TACWUNIT    SET WORK STATE                               
*                                                                               
         CLC   TACWUNIT,=C'CN '    TEST FOR CANADA                              
         BNE   XCW4X                                                            
         L     R1,TACWTAX          SET CANADIAN TAX                             
         CVD   R1,SORTCTAX                                                      
         B     XCW5                                                             
*                                                                               
XCW4X    L     R1,TACWTAX          STATE TAXES                                  
         CVD   R1,DUB                                                           
         AP    SORTSTAX,DUB                                                     
*                                                                               
XCW5     L     R1,TACWSDI          STATE DISABILITY                             
         CVD   R1,DUB                                                           
         AP    SORTSDI,DUB                                                      
         L     R1,TACWSUI          STATE UNEMPLOYMENT                           
         CVD   R1,DUB                                                           
         AP    SORTSUI,DUB                                                      
         B     XCW8                                                             
         SPACE 1                                                                
XCW6     TM    TACWSTAT,TACWSWRK   SET WORK LOCAL                               
         BZ    *+10                                                             
         MVC   SORTLOW,TACWUNIT                                                 
         TM    TACWSTAT,TACWSTAX   SET TAXABLE LOCAL                            
         BZ    *+10                                                             
         MVC   SORTTXCY,TACWUNIT                                                
         L     R1,TACWTAX          LOCAL TAXES                                  
         CVD   R1,DUB                                                           
         AP    SORTLTAX,DUB                                                     
         SPACE 1                                                                
XCW8     BAS   RE,NEXTEL           GET NEXT ELEMENT                             
         BE    XCW2                                                             
         CLC   SORTTXST,XSPACES    IF NO TAXABLE STATE FOUND                    
         BNE   XCW8X                                                            
         MVC   SORTTXST,SORTSOW    USE WORK STATE                               
XCW8X    B     XIT                                                              
         DROP  R2,R4                                                            
         EJECT                                                                  
*              EXTRACT DATA FROM DUE COMPANY WITHOLDING ELEMENT                 
         SPACE 1                                                                
         USING SORTD,R2            R2=A(SORT RECORD)                            
         USING TLCKD,R4            R4=A(RECORD)                                 
XDWELS   NTR1                                                                   
         XR    R1,R1               R1=A(ACCUMULATED DUE COMPANY AMOUNT)         
         MVI   ELCODE,TADWELQ                                                   
         BAS   RE,GETEL            GET DUE CO WITHHOLDING ELEMENTS              
         B     *+8                                                              
*                                                                               
XDWELS5  BAS   RE,NEXTEL                                                        
         BNE   XDWELS10                                                         
*                                                                               
         USING TADWD,R4            R4=A(DUE CO WITHHOLDING EL.)                 
         A     R1,TADWREC          ADD AMOUNT RECOVERED                         
         B     XDWELS5                                                          
*                                                                               
XDWELS10 CVD   R1,SORTDUE          DUE COMPANY WITHHOLDING                      
         B     XIT                                                              
         DROP  R2,R4                                                            
         SPACE 2                                                                
*              EXTRACT DATA FROM LIEN WITHOLDING ELEMENT                        
         SPACE 1                                                                
         USING SORTD,R2            R2=A(SORT RECORD)                            
         USING TLCKD,R4            R4=A(RECORD)                                 
XLWELS   NTR1                                                                   
         ZAP   SORTLIEN,=P'0'                                                   
         MVI   ELCODE,TALWELQ      GET LIEN WITHHOLDING ELEMENTS                
         BAS   RE,GETEL                                                         
         BNE   XLWELSX                                                          
         USING TALWD,R4            R4=A(LIEN WITHHOLDING EL.)                   
         L     R1,TALWREC          AMOUNT RECOVERED                             
         CVD   R1,SORTLIEN                                                      
XLWELSX  B     XIT                                                              
         DROP  R2,R4                                                            
         EJECT                                                                  
*              EXTRACT DATA FROM OTHER WITHHOLDING ELEMENTS                     
         SPACE 1                                                                
         USING SORTD,R2            R2=A(SORT RECORD)                            
         USING TLCKD,R4            R4=A(RECORD)                                 
XODELS   NTR1                                                                   
         MVI   ELCODE,TAODELQ      GET OTHER WITHHOLDING ELEMENTS               
         BAS   RE,GETEL                                                         
         BNE   XODX                                                             
         USING TAODD,R4            R4=A(OTHER WITHHOLDING EL.)                  
         SPACE 1                                                                
XOD2     L     R1,TAODAMT          OTHER WITHOLDING AMOUNT                      
         CVD   R1,DUB                                                           
*                                                                               
*  need to code taodtypt for w4 trustee here                                    
*                                                                               
         CLI   TAODTYPE,TAODTYPF   FEDERAL TAX FOR FOREIGNERS                   
         BNE   XOD4                                                             
         AP    SORTFTAX,DUB        ADD TO REGULAR FEDERAL TAX                   
         B     XOD10                                                            
         SPACE 1                                                                
XOD4     CLI   TAODTYPE,TAODTYPM   MOTION PICTURE RELIEF FUND                   
         BNE   XOD6                                                             
         AP    SORTMPR,DUB         ADD TO MOTION PICTURE                        
         B     XOD10                                                            
         SPACE 1                                                                
XOD6     CLI   TAODTYPE,TAODTYPP   PERMANENT CHARITY                            
         BNE   XOD8                                                             
         AP    SORTCHAR,DUB        ADD TO CHARITY                               
         B     XOD10                                                            
         SPACE 1                                                                
XOD8     AP    SORTMDED,DUB        ELSE, ADD TO MISC. DEDUCTIONS                
         SPACE 1                                                                
XOD10    BAS   RE,NEXTEL           GET NEXT ELEMENT                             
         BE    XOD2                                                             
XODX     B     XIT                                                              
         DROP  R2,R4                                                            
         SPACE 2                                                                
*              EXTRACT DATA FROM CHECK COMMENT ELEMENT                          
         SPACE 1                                                                
         USING SORTD,R2            R2=A(SORT RECORD)                            
         USING TLCKD,R4            R4=A(RECORD)                                 
XCMEL    NTR1                                                                   
         GOTO1 CHAROUT,DMCB,TACMELQ,0,TACMTYPC  GET CHECK COMMENT               
         MVC   SORTCMNT,TGNAME                  SAVE FROM GLOBAL W/S            
         B     XIT                                                              
         DROP  R2,R4                                                            
         EJECT                                                                  
*              GET RECORDS FROM SORT / DELETE CHECKS / PRINT REPORT             
         SPACE 1                                                                
GETSORT  NTR1                                                                   
*                                                                               
GETS2    GOTO1 MYSORTER,DMCB,=C'GET'                                            
         ICM   R2,15,4(R1)                                                      
         BZ    GETSX                                                            
         USING SORTD,R2            R2=A(SORT RECORD)                            
*                                                                               
         TM    SORTPRT,SORTNOP     IF OKAY TO PRINT                             
         BO    GETS30                                                           
*                                                                               
         TM    TPOPTS,TPAUDIT      IF AUDITING                                  
         BZ    *+8                                                              
         BAS   RE,DOAUDIT          CHECK AMOUNTS                                
*                                                                               
         CLC   SVCUR,SORTCUR       IF CURRENCY CHANGED                          
         BE    *+8                                                              
         BAS   RE,NEWCUR           PRINT TOTALS/PROCESS NEW CURRENCY            
*                                                                               
         CLC   SVEMP,SORTEMP       IF EMPLOYER CHANGED                          
         BE    *+8                                                              
         BAS   RE,NEWEMP           PRINT TOTALS/PROCESS NEW EMPLOYER            
*                                                                               
         TM    TPOPTS,TPSUMM       IF SUMMARY OPTION                            
         BO    GETS30              NO MORE DETAIL NEEDED                        
*                                                                               
         CLC   SVW4,SORTW4         IF W4 CHANGED                                
         BE    *+8                                                              
         BAS   RE,NEWW4            PRINT TOTALS/PROCESS NEW W4                  
*                                                                               
         CLC   SVQTR,SORTQTR       ELSE, IF QUARTER CHANGED                     
         BE    *+8                                                              
         BAS   RE,NEWQTR           PRINT TOTALS/PROCESS NEW QUARTER             
*                                                                               
         ZAP   SVSPNH,=P'0'                                                     
         TM    SORTPRT,SORTSUBP    IF OKAY TO PRINT SPNH AT DETAIL              
         BO    GETS25                                                           
         ZAP   SVSPNH,SORTSPNH     SAVE SUBJECT TO PNH                          
         ZAP   SORTSPNH,=P'0'      AND CLEAR AT DETAIL LEVEL                    
*                                                                               
GETS25   TM    TPOPTS,TPMYCKRD     IF READ CHECKS MYSELF                        
         BZ    *+8                                                              
         BAS   RE,RDCID            READ COMMERCIAL FOR CID                      
*                                                                               
         BAS   RE,PLINE            PRINT LINE                                   
         CP    SVSPNH,=P'0'        IF SAVED SUBJECT TO PNH                      
         BE    GETS30                                                           
         ZAP   SORTSPNH,SVSPNH     RESET FOR ADDING TO TOTALS                   
*                                                                               
GETS30   TM    TPOPTS,TPDELETE     IF DELETING                                  
         BZ    *+8                                                              
         BAS   RE,DELCHECK         DELETE THE CHECK                             
*                                                                               
         L     R1,KPCOUNT          ADD TO DELETED CHECK RECORD COUNT            
         LA    R1,1(R1)                                                         
         ST    R1,KPCOUNT                                                       
*                                                                               
         TM    SORTPRT,SORTNOP     IF PRINTING                                  
         BO    *+8                                                              
         BAS   RE,ADDUP            ADD TO HIGH LEVEL TOTALS                     
         B     GETS2               GET NEXT RECORD FROM SORT                    
*                                                                               
GETSX    LA    R1,CURTOT           R1=A(CURRENCY TOTALS)                        
         BAS   RE,PRNTTOTS         PRINT LAST TOTALS                            
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*              ROUTINE TO READ COMMERCIAL RECORD FOR CID                        
         SPACE 1                                                                
         USING SORTD,R2            R2=A(SORT RECORD)                            
RDCID    NTR1                                                                   
         MVC   TGCOM,SORTCID       SET INTERNAL COMMERCIAL NUMBER               
         MVC   SORTCID,XSPACES     PRE - CLEAR COMMERCIAL ID                    
         OC    TGCOM,TGCOM         IF SPECIFIED                                 
         BZ    RDCIDX                                                           
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'A0',0)                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,AIO                                                           
         USING TLCOD,R4                                                         
         MVI   ELCODE,TACOELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACOD,R4                                                         
         MVC   SORTCID,TACOCID     SET COMMERCIAL ID                            
RDCIDX   B     XIT                                                              
         DROP  R2,R4                                                            
         EJECT                                                                  
*              CHECK SORT AMOUNTS                                               
*                                                                               
         USING SORTD,R2            R2=A(SORT RECORD)                            
DOAUDIT  NTR1                                                                   
         MVC   SRTREC,0(R2)        SAVE IN LOCAL STORAGE                        
*                                                                               
         ZAP   DUB,SORTEARN        GROSS + NON TAX + REIM EXP                   
         AP    DUB,SORTNTAX                                                     
         AP    DUB,SORTREXP                                                     
*                                                                               
         SP    DUB,SORTFTAX        LESS FEDERAL TAX                             
         SP    DUB,SORTFICA        LESS FICA                                    
         SP    DUB,SORTSTAX        LESS STATE TAX                               
         SP    DUB,SORTSDI         LESS STATE DISABILITY                        
         SP    DUB,SORTSUI         LESS STATE UNEMPLOYMENT                      
         SP    DUB,SORTLTAX        LESS LOCAL TAX                               
         SP    DUB,SORTLIEN        LESS LIENT WITHOLDING                        
         SP    DUB,SORTCTAX        LESS CAN TAX                                 
         SP    DUB,SORTMDED        LESS MISC DEDUCTION                          
         SP    DUB,SORTMPR         LESS MOTION PIC.                             
         SP    DUB,SORTCHAR        LESS PERM CHAR                               
         SP    DUB,SORTDUES        LESS UNION DUES                              
*                                                                               
         CP    DUB,SORTNET         SHOULD BE SAME AS NET                        
         BE    AUDITX                                                           
         CLI   TIQPEND,X'90'       DEAL WITH 1990 BUG IN CHECKS                 
         BNE   AUDIT50                                                          
         CLI   SORTINV+5,X'01'     IF OLD DPS US                                
         BE    AUDITX              SKIP AUDIT                                   
         CLI   SORTINV+5,X'02'     OR IF OLD DPS CANADA                         
         BE    AUDITX              SKIP AUDIT                                   
         CLI   SORTINV+5,X'03'     OR IF OLD T & R                              
         BE    AUDITX              SKIP AUDIT                                   
         CP    SORTMDED,=P'0'      IF THERE WAS MISC DED                        
         BE    AUDIT20                                                          
         AP    DUB,SORTMDED        ADD BACK MISC DED.                           
         CP    DUB,SORTNET         AND IF SAME NOW                              
         BE    AUDITX              THEN OKAY                                    
         B     AUDIT50                                                          
*                                                                               
AUDIT20  DS    0H                                                               
**NO-OP* CP    SORTDUE,=P'0'       OR IF THERE WAS DUE COMPANY                  
**NO-OP* BE    AUDIT50                                                          
**NO-OP* SP    DUB,SORTDUE         SUBTRACT IT                                  
**NO-OP* CP    DUB,SORTNET         AND IF SAME NOW                              
**NO-OP* BE    AUDITX              THEN OKAY                                    
*                                                                               
AUDIT50  BAS   RE,PLINE            ELSE PRINT OUT                               
*                                                                               
AUDITX   B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*              ROUTINE TO PROCESS NEW EMPLOYER                                  
         SPACE 1                                                                
         USING SORTD,R2            R2=A(SORT RECORD)                            
NEWCUR   NTR1                                                                   
         OC    SVCUR,SVCUR         IF PREVIOUS CURRENCY                         
         BZ    NEWCUR40                                                         
         LA    R1,CURTOT           R1=A(HIGHEST LEVEL AMOUNTS)                  
         BAS   RE,PRNTTOTS         PRINT/FLUSH TOTALS                           
*                                                                               
NEWCUR40 BAS   RE,NEWPAGE          SET NEW PAGE                                 
         MVC   SVCUR,SORTCUR       SET NEW CURRENCY                             
         XC    SVEMP,SVEMP         FORCE EMPLOYER CONTROL BREAK                 
         B     XIT                                                              
         DROP  R2                                                               
         SPACE 2                                                                
*              ROUTINE TO PROCESS NEW EMPLOYER                                  
         SPACE 1                                                                
         USING SORTD,R2            R2=A(SORT RECORD)                            
NEWEMP   NTR1                                                                   
         OC    SVEMP,SVEMP         IF PREVIOUS EMPLOYER                         
         BZ    NEWEMP40                                                         
         LA    R1,EMPTOT           R1=A(HIGHEST LEVEL AMOUNTS)                  
         BAS   RE,PRNTTOTS         PRINT/FLUSH TOTALS                           
*                                                                               
NEWEMP40 BAS   RE,NEWPAGE          SET NEW PAGE                                 
         MVC   SVEMP,SORTEMP       SAVE EMPLOYER                                
         GOTO1 RECVAL,DMCB,TLEMCDQ,(X'A0',SORTEMP)  READ/SET GLOBAL             
         GOTO1 MYTRACE                                                          
*                                                                               
         GOTO1 CHAROUT,DMCB,TANAELQ,0                                           
         MVC   SVEMPNM,TGNAME      SAVE EMPLOYER NAME                           
         XC    SVW4,SVW4           FORCE W4 CONTROL BREAK                       
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*              ROUTINE TO PROCESS NEW SSN                                       
         SPACE                                                                  
         USING SORTD,R2            R2=A(SORT RECORD)                            
NEWW4    NTR1                                                                   
         OC    SVW4,SVW4           IF PREVIOUS SSN                              
         BZ    NEWW440                                                          
*                                                                               
         LA    R1,W4TOT            R1=A(HIGHEST LEVEL AMOUNTS)                  
         BAS   RE,PRNTTOTS         PRINT/FLUSH TOTALS                           
         BAS   RE,BXMID            PRINT MIDDLE LINE                            
*                                                                               
NEWW440  MVC   SVW4,SORTW4         SAVE NEW SSN                                 
         MVC   SVW4NM,XSPACES                                                   
         MVC   SVW4NM(L'LTNOTFND),LTNOTFND                                      
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'A0',SORTW4)  READ/SET GLOBAL              
         BNE   NEWW4X                                                           
         GOTO1 MYTRACE                                                          
         GOTO1 CHAROUT,DMCB,TANAELQ,0                                           
         MVC   SVW4NM,TGNAME         SAVE W4 NAME                               
*                                                                               
         MVC   SVW4ADD(4*L'SVW4ADD),XSPACES                                     
         L     R4,AIO                                                           
         MVI   ELCODE,TAA2ELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   NEWW4X                                                           
         USING TAA2D,R4                                                         
*                                                                               
         LA    R2,SVW4ADD                                                       
         LA    R1,TAA2ADD1                                                      
         LA    R0,3                ASSUME 3 ADDRESS LINES                       
         CLI   TAA2LEN,TAA2LNQ     FOR OLD STYLE ADDRESSES                      
         BL    NEWW450                                                          
         CLC   TAA2CTRY,=C'US'     AND NEW STYLE US ADDRESSES                   
         BE    NEWW450                                                          
         LHI   R0,2                ELSE USE 2                                   
NEWW450  CLC   0(L'SVW4ADD,R1),XSPACES TEST ANY DATA LEFT                       
         BNH   NEWW460                                                          
         MVC   0(L'SVW4ADD,R2),0(R1)   SAVE MOST OF ADDRESS                     
         LA    R2,L'SVW4ADD(R2)                                                 
         LA    R1,L'TAA2ADD1(R1)                                                
         BCT   R0,NEWW450                                                       
*                                                                               
NEWW460  MVC   WORK,XSPACES                                                     
         MVC   WORK(25),TAA2CITY     CITY                                       
         MVC   WORK+26(2),TAA2ST     STATE                                      
         MVC   WORK+29(10),TAA2ZIP   ZIP                                        
         GOTO1 SQUASHER,DMCB,WORK,39 SQUASH IT                                  
         CLC   WORK(39),XSPACES                                                 
         BNH   *+10                                                             
         MVC   0(L'SVW4ADD,R2),WORK  SAVE IT                                    
*                                                                               
         CLI   TAA2LEN,TAA2LNQ        FOR OLD STYLE ADDRESSES                   
         BL    NEWW4X                                                           
         CLC   TAA2CTRY,=C'US'        AND NEW STYLE US ADDRESSES                
         BE    NEWW4X                                                           
         GOTO1 VALCTRY,DMCB,(X'80',TAA2CTRY)                                    
         BNE   NEWW4X                                                           
         USING CTRYTABD,R1                                                      
         L     R1,TGACTRY                                                       
         ZIC   RE,CTRYDSP                                                       
         LTR   RE,RE                                                            
         BNZ   *+8                                                              
         IC    RE,CTRYLEN                                                       
         SHI   RE,CTRYDESC-CTRYTABD+1                                           
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   39(0,R2),CTRYDESC                                                
         EX    RE,*+8                                                           
         B     *+10                                                             
         OC    39(0,R2),XSPACES                                                 
         DROP  R1                                                               
*                                                                               
NEWW4X   XC    SVQTR,SVQTR         FORCE QUARTER CONTROL BREAK                  
         B     XIT                                                              
         DROP  R2,R4                                                            
         EJECT                                                                  
*              ROUTINE TO PROCESS NEW QUARTER                                   
         SPACE                                                                  
         USING SORTD,R2            R2=A(SORT RECORD)                            
NEWQTR   NTR1                                                                   
         OC    SVQTR,SVQTR         IF PREVIOUS QUARTER                          
         BZ    NEWQTR40                                                         
*                                                                               
         L     R3,AQTRSTTB                                                      
         L     R0,QSTCNT                                                        
         GOTO1 XSORT,DMCB,(R3),(R0),STLNQ,L'STTAX,0                             
         BAS   RE,PRNTQTR          PRINT QTR/FLUSH TOTALS                       
*                                                                               
NEWQTR40 MVC   SVQTR,SORTQTR       SAVE NEW QUARTER                             
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*              ROUTINE TO PRINT/FLUSH ACCUMULATORS                              
         SPACE                                                                  
*                                  R1=A(HIGHEST ACCUM LEVEL)                    
PRNTTOTS NTR1                                                                   
         LA    R4,XP               R4=A(PRINT LINE)                             
         USING PRNTD,R4                                                         
*                                                                               
         TM    TPOPTS,TPSUMM       IF SUMMARY OPTION                            
         BO    PRNTTOT4            SKIP TO HIGHER LEVEL TOTALS                  
*                                                                               
         L     RE,YSTCNT           CHECK YTD=W4                                 
         C     RE,=F'1'            MEANING ONLY ONE STATE TOTAL                 
         BH    *+8                                                              
         BAS   RE,CLRW4TOT         CLEAR W4 AMOUNTS                             
         BAS   RE,CMPTABS          CHECK QTR=YTD (CLEAR YTD TABLE)              
         BAS   RE,PRNTQTR          PRINT QUARTER TOTALS                         
         BAS   RE,PRNTYTD          PRINT YTD TOTALS                             
*                                                                               
PRNTTOT4 LA    R3,TOTALS           R3=A(TOTAL TABLE)                            
         USING TOTALSD,R3                                                       
*                                                                               
PRNTTOT6 BAS   RE,CHKTAMTS         IF TOTAL                                     
         BNE   PRNTTOT8                                                         
         LA    R2,PRNTID           SET TAG FOR LITERAL                          
         LA    RF,L'TOTLIT         L'LITERAL                                    
         LA    RE,TOTLIT           LITERAL FOR TOTAL                            
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(RE)       LITERAL FOR TOTAL                            
*                                                                               
         BAS   RE,FORMAT           FORMAT ACCUMULATED AMOUNTS (R3)              
         BAS   RE,PRNTIT           PRINT THEM                                   
*                                                                               
PRNTTOT8 BAS   RE,CLRTAMT          FLUSH PREV LEVEL ACCUMULATORS                
         LA    R3,L'TOTALS(R3)     BUMP TO NEXT LEVEL                           
*                                                                               
         CR    R3,R1               IF NOT PASSED HIGHEST ACCUM LEVEL            
         BNH   PRNTTOT6            LOOP                                         
         B     XIT                                                              
         DROP  R3,R4                                                            
         EJECT                                                                  
*              ROUTINE TO CLEAR AMOUNTS AT W4 TOTAL LEVEL                       
         SPACE 1                                                                
CLRW4TOT NTR1                                                                   
         LA    R0,NAMTS                                                         
         LA    RE,W4TOT                                                         
CLRW4T5  ZAP   0(L'SORTAMTS,RE),=P'0'                                           
         LA    RE,L'SORTAMTS(RE)                                                
         BCT   R0,CLRW4T5                                                       
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE CHECKS IF ANY TOTALS                                     
*                                  CC = AMOUNT FOUND                            
         USING TOTALSD,R3          R3=A(TOTAL AMOUNTS)                          
         SPACE 1                                                                
CHKTAMTS NTR1                                                                   
         LA    R0,NAMTS                                                         
CHKTAMT5 CP    0(L'SORTAMTS,R3),=P'0'                                           
         BNE   YES                 AMOUNT FOUND                                 
         LA    R3,L'SORTAMTS(R3)                                                
         BCT   R0,CHKTAMT5                                                      
         B     NO                                                               
         DROP  R3                                                               
*              ROUTINE TO FLUSH PREV LEVEL ACCUMULATORS                         
         SPACE 1                                                                
         USING TOTALSD,R3          R3=A(TOTAL AMOUNTS)                          
CLRTAMT  NTR1                                                                   
         LA    R0,NAMTS                                                         
CLRTAMT5 ZAP   0(L'SORTAMTS,R3),=P'0'                                           
         LA    R3,L'SORTAMTS(R3)                                                
         BCT   R0,CLRTAMT5                                                      
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              ROUTINE TO CHECK IF YTD AMTS SAME AS QTR AMTS, IF SO             
*              FLUSH YTD AMOUNTS                                                
         SPACE                                                                  
CMPTABS  NTR1                                                                   
         L     R3,AQTRSTTB                                                      
         L     R0,QSTCNT                                                        
         GOTO1 XSORT,DMCB,(R3),(R0),STLNQ,L'STTAX,0                             
         L     R3,AYTDSTTB                                                      
         L     R0,YSTCNT                                                        
         GOTO1 XSORT,DMCB,(R3),(R0),STLNQ,L'STTAX,0                             
*                                                                               
         USING STTABD,R3                                                        
         L     R3,AQTRSTTB                                                      
         L     R2,AYTDSTTB                                                      
         LA    R0,MAXSTS           R0=(MAX NUMBER OF ENTRIES)                   
CMPTAB10 CLC   0(STLNQ,R3),0(R2)   MATCH TABLE ENTRIES                          
         BNE   CMPTABX                                                          
         LA    R3,STLNQ(R3)        BUMP TO NEXT QTRSTTAB ENTRY                  
         LA    R2,STLNQ(R2)        BUMP TO NEXT YTDSTTAB ENTRY                  
         BCT   R0,CMPTAB10         LOOP                                         
*                                                                               
         BAS   RE,CLRYTD           CLEAR YTD TABLE                              
*                                                                               
CMPTABX  B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              ROUTINE TO PRINT QUARTER TOTALS BY STATE                         
         SPACE                                                                  
PRNTQTR  NTR1                                                                   
         LA    R4,XP               R4=A(PRINT LINE)                             
         USING PRNTD,R4                                                         
         MVI   BYTE,4              FOUR W4 ADDRESS LINES                        
         BAS   RE,PRTW4ADD         MOVE TO PRINT LINE                           
         CLI   PRNTDA,C'Y'         IF MOVED TO PRINT LINE                       
         BNE   *+8                                                              
         BAS   RE,PRNTIT           PRINT IT                                     
*                                                                               
         OC    QSTCNT,QSTCNT       IF NO STATES                                 
         BZ    PRNTQTRX                                                         
         L     R0,QSTCNT           R0=(NUMBER OF STATES)                        
         L     R3,AQTRSTTB         R3=A(QUARTER STATE TABLE)                    
         USING STTABD,R3                                                        
*                                                                               
PRNTQ15  LA    R2,PRNTID           SET TAG FOR TOTAL LINE                       
         MVC   0(L'LTQTRTOT,R2),LTQTRTOT                                        
         ZIC   R1,SVQTR                                                         
         BCTR  R1,0                                                             
         MH    R1,=AL2(L'QTRTAB)                                                
         LA    RE,QTRTAB(R1)       RE=A(CURRENT QUARTER LITERAL)                
         MVC   0(L'QTRTAB,R2),0(RE)                                             
         LA    R2,L'LTQTRTOT+1(R2)                                              
*                                                                               
         CLC   0(L'STTAX,R3),XSPACES                                            
         BE    *+14                                                             
         MVC   0(L'STTAX,R2),0(R3)  SET STATE IN TOTAL LITERAL                  
         LA    R2,L'STTAX+1(R2)                                                 
         MVC   0(L'LTTOTAL,R2),LTTOTAL                                          
*                                                                               
         LA    R3,L'STTAX(R3)      R3=A(ACCUMULATED AMOUNTS)                    
         BAS   RE,FORMAT                                                        
         BAS   RE,PRNTIT           PRINT THE LINE                               
         LA    R3,L'STAMTS(R3)     BUMP TO NEXT STATE ENTRY                     
         BCT   R0,PRNTQ15                                                       
*                                                                               
PRNTQTRX BAS   RE,CLRQTR           CLEAR QTR TABLE AND COUNTER                  
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              ROUTINE TO PRINT YTD TOTALS BY STATE                             
         SPACE                                                                  
PRNTYTD  NTR1                                                                   
         LA    R4,XP               R4=A(PRINT LINE)                             
         USING PRNTD,R4                                                         
*                                                                               
         OC    YSTCNT,YSTCNT       IF NO STATES                                 
         BZ    PRNTYTDX                                                         
         L     R0,YSTCNT           R0=(NUMBER OF STATES)                        
         L     R3,AYTDSTTB         R3=A(YTD STATE TABLE)                        
         USING STTABD,R3                                                        
*                                                                               
PRNTY15  LA    R2,PRNTID           SET TAG FOR TOTAL LINE                       
         CLC   STTAX,XSPACES                                                    
         BNE   *+14                                                             
         MVC   0(L'LTTOTAL,R2),LTTOTAL                                          
         B     PRNTY20                                                          
         MVC   0(L'STTAX,R2),STTAX                                              
         LA    R2,L'STTAX+1(R2)                                                 
         MVC   0(L'LTYTDTOT,R2),LTYTDTOT                                        
*                                                                               
PRNTY20  LA    R3,L'STTAX(R3)      R3=A(ACCUMULATED AMOUNTS)                    
         BAS   RE,FORMAT                                                        
         BAS   RE,PRNTIT           PRINT THE LINE                               
         LA    R3,L'STAMTS(R3)     BUMP TO NEXT STATE ENTRY                     
         BCT   R0,PRNTY15                                                       
*                                                                               
PRNTYTDX BAS   RE,CLRYTD           CLEAR YTD TABLE AND COUNTER                  
         B     XIT                                                              
         DROP  R3,R4                                                            
         EJECT                                                                  
*              ROUTINE TO CLEAR QTRSTTAB AND COUNTER                            
         SPACE                                                                  
CLRQTR   NTR1                                                                   
         XC    QSTCNT,QSTCNT       CLEAR STATION COUNT                          
         L     R1,AQTRSTTB         FLUSH QUARTER TABLE                          
         B     CLRTAB                                                           
         SPACE 1                                                                
*              ROUTINE TO CLEAR YTDSTTAB AND COUNTER                            
         SPACE                                                                  
CLRYTD   NTR1                                                                   
         XC    YSTCNT,YSTCNT       CLEAR YTD STATION COUNT                      
         L     R1,AYTDSTTB         FLUSH YTD TABLE                              
         B     CLRTAB                                                           
         SPACE 1                                                                
         USING STTABD,R1                                                        
CLRTAB   LA    RF,MAXSTS           RF=MAX NUMBER OF ENTRIES                     
*                                                                               
CLRTAB2  XC    STTAX,STTAX         CLEAR CODE                                   
         LA    R0,NAMTS            CLEAR AMOUNTS                                
         LA    RE,STAMTS                                                        
CLRTAB5  ZAP   0(L'SORTAMTS,RE),=P'0'                                           
         LA    RE,L'SORTAMTS(RE)                                                
         BCT   R0,CLRTAB5                                                       
*                                                                               
         LA    R1,STLNQ(R1)        BUMP TO NEXT TABLE ENTRY                     
         BCT   RF,CLRTAB2          LOOP                                         
         B     XIT                                                              
         DROP  R1                                                               
         EJECT                                                                  
*              PRINT W4 ADDRESS                                                 
*                                  NTRY -BYTE =( # OF LINES TO PRINT)           
         USING PRNTD,R4            R4=A(PRINT LINE)                             
         SPACE 1                                                                
PRTW4ADD NTR1                                                                   
         MVI   PRNTDA,C'N'         W4 ADDRESS NOT PRINTED                       
         XR    R1,R1               COUNTER OF LINES PRINTED                     
         LA    R0,4                TOTAL # OF ADDRESS LINES                     
         LA    R2,SVW4ADD          R2=A(W4 ADDRESS)                             
PRTW4A10 CLC   0(L'SVW4ADD,R2),XSPACES                                          
         BNH   PRTW4A20                                                         
         MVC   PRNTID(L'SVW4ADD),0(R2)                                          
         MVC   0(L'SVW4ADD,R2),XSPACES FLUSH ADDRESS LINE                       
         MVI   PRNTDA,C'Y'             W4 ADDRESS PRINTED                       
         LA    R4,L'XP(R4)             BUMP TO NEXT PRINT LINE                  
         LA    R1,1(R1)                INCREMENT LINE PRINTED COUNTER           
         CLM   R1,1,BYTE               CHECK PRINTED # LINES REQUESTED          
         BE    PRTW4AX                                                          
*                                                                               
PRTW4A20 LA    R2,L'SVW4ADD(R2)    BUMP TO NEXT ADRESS LINE                     
         BCT   R0,PRTW4A10         LOOP                                         
*                                                                               
PRTW4AX  B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              PRINT SORT RECORD DETAILS                                        
         SPACE 1                                                                
         USING SORTD,R2            R2=A(SORT RECORD)                            
PLINE    NTR1                                                                   
         LA    R4,XP               R4=A(PRINT LINE)                             
         USING PRNTD,R4                                                         
*                                                                               
         LA    R3,SORTAMTS         R3=A(AMOUNTS)                                
         BAS   RE,FORMAT           FORMAT AMOUNTS                               
*                                                                               
         CLC   SVW4NM,XSPACES      IF SAME W4                                   
         BNE   PLINE20                                                          
         MVI   BYTE,2              PRINT TWO LINES OF ADDRESS                   
         CLI   PRNTD3,C'Y'         UNLESS THREE AMT LINES PRINTED               
         BNE   *+8                                                              
         MVI   BYTE,3              INWHICH CASE PRINT THREE ADDRESS LNS         
         BAS   RE,PRTW4ADD         IF NECESSARY                                 
         B     PLINE30                                                          
*                                                                               
PLINE20  MVC   PRNTID,LTID         PRINT ID#                                    
         MVC   PRNTSSN,SORTW4      PRINT SSN                                    
         MVC   PRNTSSNN,SVW4NM     PRINT SQUASHED NAME                          
         MVC   SVW4NM,XSPACES      ENSURE SAME W4 INFO NOT PRINTED              
         CLI   PRNTD3,C'Y'         IF THREE AMT LINES PRINTED                   
         BNE   PLINE30                                                          
         LR    R0,R4               SAVE A(PRINT LINE)                           
         LA    R4,396(R4)          R4=A(THIRD PRINT LINE)                       
         MVI   BYTE,1              PRINT FIRST ADDRESS LINE TOO                 
         BAS   RE,PRTW4ADD                                                      
         LR    R4,R0               RESTORE A(PRINT LINE)                        
*                                                                               
PLINE30  MVC   PRNTTXST,SORTTXST   TAXABLE STATE                                
         MVC   PRNTTXCY,SORTTXCY   TAXABLE CITY                                 
*                                                                               
         GOTO1 DATCON,DMCB,(1,SORTDTE),(5,PRNTDTE)    CHECK DATE                
         MVC   PRNTISSN,SORTISSN                      INDIV SSN (CORP)          
*                                                                               
         MVC   PRNTWKST,SORTSOW    STATE OF WORK                                
         MVC   PRNTWKCY,SORTLOW    LOCAL OF WORK                                
*                                                                               
         MVC   PRNTAGY,SORTAGY                       AGENCY                     
         GOTO1 TINVCON,DMCB,SORTINV,PRNTINV,DATCON   INVOICE NUMBER             
*                                                                               
         MVC   PRNTCHK,SORTCHK                        CHECK NUMBER              
         OC    SORTFRST,SORTFRST                                                
         BZ    PLINE35                                                          
         GOTO1 DATCON,DMCB,(1,SORTFRST),(5,PRNTFRST)  FIRST SERVICES            
*                                                                               
PLINE35  MVC   PRNTCID,SORTCID     COMMERCIAL ID                                
         MVC   PRNTTYPE,SORTPTYP   SESSION OR REUSE                             
         MVC   PRNTUN,SORTUN       UNION                                        
         OC    SORTNCDE,SORTNCDE   AGENT                                        
         BZ    PLINE40                                                          
         GOTO1 TRNSAGT,DMCB,(X'40',SORTNCDE),PRNTNCDE                           
*                                                                               
PLINE40  MVC   PRNTCOM,SORTCMNT    PRINT CHECK COMMENTS                         
*                                                                               
         LA    R4,XP                                                            
         LA    R1,L'XP*2           MAKE SURE NO NULLS IN THE TWO                
PLINE50  CLI   0(R4),0             PRINT LINES                                  
         BNE   *+8                                                              
         MVI   0(R4),C' '                                                       
         LA    R4,1(R4)                                                         
         BCT   R1,PLINE50                                                       
*                                                                               
         BAS   RE,PRNTIT           PRINT THE LINE                               
         B     XIT                                                              
         DROP  R2,R4                                                            
         EJECT                                                                  
*              ROUTINE FORMATS AMOUNTS TO PRINT LINE                            
*                                  R3=A(AMOUNTS)                                
         USING PRNTD,R4            R4=A(PRINT AREA)                             
FORMAT   NTR1                                                                   
         LA    RE,NAMTS            RE=N'ACCUMS                                  
         LA    R2,PRNTAMTS         R2=A(FIRST PRINT POSITION)                   
         MVI   PRNTD3,C'N'         THIRD LINE NOT PRINTED                       
         SPACE 1                                                                
FORM2    CH    RE,=Y((NAMTS/3)*2)  IF 2ND SET OF AMOUNTS                        
         BNE   *+12                                                             
         LA    R2,PRNTAMTS+198     PRINT ON SECOND PRINT LINE                   
         B     FORM5                                                            
         SPACE 1                                                                
         CH    RE,=Y(NAMTS/3)      IF 3RD SET OF AMOUNTS                        
         BNE   FORM5                                                            
         STC   RE,BYTE             SAVE NUMBER IN RE                            
         BAS   RE,CHKTHIRD         AND SOMETHING TO PRINT                       
         BNE   FORMX                                                            
         ZIC   RE,BYTE             RESTORE NUMBER IN RE                         
         LA    R2,PRNTAMTS+396     PRINT ON THIRD PRINT LINE                    
         MVI   PRNTD3,C'Y'         THIRD LINE PRINTED                           
         SPACE 1                                                                
FORM5    EDIT  (P8,0(R3)),(13,(R2)),2,FLOAT=-,ZERO=NOBLANK                      
         SPACE 1                                                                
         LA    R2,13(R2)           BUMP TO NEXTS                                
         LA    R3,L'SORTAMTS(R3)                                                
         BCT   RE,FORM2                                                         
         SPACE 1                                                                
FORMX    B     XIT                                                              
         DROP  R4                                                               
         SPACE 2                                                                
*              CHECK ANY AMOUNTS TO PRINT ON THIRD LINE                         
*                                  XIT CC = IF AMOUNTS                          
CHKTHIRD NTR1                                                                   
*                                  R0=NUMBER OF AMOUNTS ON LINE 3               
         LA    R0,(SORTAMTX-SORTSPNH)/L'SORTAMTS                                
CHKTHRD5 CP    0(L'SORTAMTS,R3),=P'0'                                           
         BNE   YES                                                              
         LA    R3,L'SORTAMTS(R3)                                                
         BCT   R0,CHKTHRD5                                                      
         B     NO                                                               
         EJECT                                                                  
*              ROUTINE TO DELETE THE CHECK RECORD                               
         SPACE 1                                                                
         USING SORTD,R2            R2=A(SORT RECORD)                            
DELCHECK NTR1                                                                   
         BAS   RE,SETCHK           SET TO READ CHECK FILE                       
*                                                                               
         XC    KEY,KEY             RE-READ CHECK RECORD                         
         LA    R4,KEY                                                           
         USING TLDRD,R4                                                         
         MVC   TLDRDA,SORTCKDA                                                  
         GOTO1 GETREC                                                           
*                                                                               
         L     R4,AIO                                                           
         USING TLCKD,R4                                                         
         OI    TLCKSTAT,TLCKSDEL   MARK CHECK FOR DELETION                      
         GOTO1 PUTREC                                                           
         GOTO1 MYTRACE                                                          
*                                                                               
         BAS   RE,SETTAL           SET TO READ TALENT FILE                      
         B     XIT                                                              
         DROP  R2,R4                                                            
         SPACE 2                                                                
SETCHK   NTR1                                                                   
         MVC   SYSFIL,=CL8'CHKFIL'                                              
         MVC   SYSDIR,=CL8'CHKDIR'                                              
         B     XIT                                                              
         SPACE 2                                                                
SETTAL   NTR1                                                                   
         MVC   SYSFIL,=CL8'TALFIL'                                              
         MVC   SYSDIR,=CL8'TALDIR'                                              
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO ADD ACCUMS TO HIGHER LEVELS                           
         SPACE 1                                                                
         USING SORTD,R2            R2=A(SORT RECORD)                            
ADDUP    NTR1                                                                   
         TM    TPOPTS,TPSUMM       IF SUMMARY OPTION                            
         BO    ADD1                SKIP TO HIGH LEVEL TOTALS                    
*                                                                               
         L     R3,AQTRSTTB         R3=A(QUARTER TABLE BY STATE)                 
         MVI   TFLAG,C'Q'                                                       
         BAS   RE,ADDTOST          ADD TO QUARTER TABLE                         
*                                                                               
         L     R3,AYTDSTTB         R3=A(YTD TABLE BY STATE)                     
         MVI   TFLAG,C'Y'                                                       
         BAS   RE,ADDTOST          ADD TO YTD TABLE                             
*                                                                               
ADD1     LA    RE,SORTAMTS         RE=A(LOWEST LEVEL OF ACCUMS)                 
         LA    R1,NAMTS            R1=N'ACCUMS AT EACH LEVEL                    
         LA    R2,TOTALS           R2=A(FIRST HIGH LEVEL ACCUMS)                
         TM    TPOPTS,TPSUMM       IF SUMMARY OPTION                            
         BZ    *+8                                                              
         LA    R2,EMPTOT           R2=A(FIRST LEVEL ACCUM)                      
         SPACE 1                                                                
ADD2     LR    RF,R2               RF=A(THIS ACCUM AT NEXT LEVEL)               
         LA    R0,NTOTS            R0=N'HIGHER LEVELS OF ACCUMS                 
         TM    TPOPTS,TPSUMM                                                    
         BZ    *+8                                                              
         LA    R0,NSUMTOTS                                                      
         SPACE 1                                                                
ADD4     DS    0H                  ADD SORT ACCUM TO HIGHER LEVEL               
         AP    0(L'SORTAMTS,RF),0(L'SORTAMTS,RE)                                
*                                                                               
         LA    RF,L'TOTALS(RF)     BUMP TO NEXT HIGHER LEVEL                    
         BCT   R0,ADD4                                                          
         SPACE 1                                                                
         LA    RE,L'SORTAMTS(RE)   BUMP TO NEXT ACCUM                           
         LA    R2,L'SORTAMTS(R2)                                                
         BCT   R1,ADD2                                                          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO ADD CHECK AMTS TO QTR/YTD TABLE BY STATE              
*                                  R2=A(SORT RECORD)                            
         USING STTABD,R3           R3=A(QTR/YTD TABLE)                          
         SPACE                                                                  
ADDTOST  NTR1                                                                   
*                                                                               
         LA    R0,MAXSTS           R0=(N'MAXIMUM ENTRIES)                       
ADDTOS5  OC    STTAX,STTAX         IF TABLE ENTRY                               
         BZ    ADDTOS10                                                         
         CLC   STTAX,SORTTXST      MATCH ON STATE                               
         BE    ADDTOS15                                                         
         LA    R3,STLNQ(R3)                                                     
         BCT   R0,ADDTOS5                                                       
         DC    H'0'                NOT ENOUGH ROOM                              
*                                                                               
ADDTOS10 MVC   STTAX,SORTTXST                                                   
         CLI   TFLAG,C'Q'          IF ADDING TO QUARTER STATE TABLE             
         BNE   *+12                                                             
         BAS   RE,INCQCNT          INCREMENT QUARTER STATE COUNT                
         B     *+8                                                              
         BAS   RE,INCYCNT          ELSE, INCREMENT YTD STATE COUNT              
*                                                                               
ADDTOS15 LA    RE,SORTAMTS         RE=A(AMTS TO ACCUM)                          
         LA    R3,L'STTAX(R3)      R3=A(ACCUMULATED TABLE)                      
         LA    R0,NAMTS            R1=N'ACCUMULATED AMOUNTS                     
*                                                                               
ADDTOS20 DS    0H                  ADD CHECK AMTS TO TABLE                      
         AP    0(L'SORTAMTS,R3),0(L'SORTAMTS,RE)                                
*                                                                               
         LA    R3,L'SORTAMTS(R3)   BUMP TO AMT                                  
         LA    RE,L'SORTAMTS(RE)                                                
         BCT   R0,ADDTOS20                                                      
         B     XIT                                                              
         DROP  R2,R3                                                            
         SPACE 2                                                                
*              ROUTINE ADDS TO COUNT OF STATES IN QTR TABLE                     
         SPACE                                                                  
INCQCNT  L     R1,QSTCNT                                                        
         LA    R1,1(R1)                                                         
         ST    R1,QSTCNT                                                        
         BR    RE                                                               
         SPACE 2                                                                
*              ROUTINE ADDS TO COUNT OF STATES IN YTD TABLE                     
         SPACE                                                                  
INCYCNT  L     R1,YSTCNT                                                        
         LA    R1,1(R1)                                                         
         ST    R1,YSTCNT                                                        
         BR    RE                                                               
         EJECT                                                                  
*              ROUTINE TO PRINT A LINE                                          
         SPACE 1                                                                
PRNTIT   NTR1                                                                   
         GOTO1 SPOOL,DMCB,ASPOOLD                                               
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO SKIP TO NEW PAGE                                      
         SPACE 1                                                                
NEWPAGE  NTR1                                                                   
         L     R1,ASPOOLD                                                       
         USING SPOOLD,R1                                                        
         MVI   FORCEHED,C'Y'       FORCE NEW PAGE                               
         B     XIT                                                              
         DROP  R1                                                               
         SPACE 2                                                                
*              ROUTINE TO PRINT OUT MIDDLE LINE                                 
         SPACE                                                                  
BXMID    NTR1                                                                   
         TM    TPOPTS,TPNOBOX      IF BOXES                                     
         BO    BXMIDX                                                           
         L     R4,MYABOX           R4=A(BOXES)                                  
         USING BOXD,R4                                                          
         L     R1,ASPOOLD                                                       
         USING SPOOLD,R1                                                        
         MVC   BOXROWS,XSPACES                                                  
         ZIC   RF,LINE                                                          
         LA    RF,BOXROWS-1(RF)                                                 
         MVI   0(RF),C'M'                                                       
         MVI   BOXINIT,0                                                        
         MVI   PRNTMID,C'Y'        PRINTING MIDLINE                             
         BAS   RE,PRNTIT                                                        
         MVI   PRNTMID,0           RESET                                        
BXMIDX   B     XIT                                                              
         DROP  R1,R4                                                            
         SPACE 2                                                                
*              ROUTINE HANDLES RECORD TRACES                                    
         SPACE 1                                                                
MYTRACE  NTR1                                                                   
         TM    TPOPTS,TPTRACE      TEST TRACE ENABLED                           
         BZ    MYTRACEX                                                         
         GOTO1 TRACE,DMCB,AIO,0,0,0                                             
MYTRACEX B     XIT                                                              
         EJECT                                                                  
*              HEADLINE HOOK (HEADHOOK)                                         
         SPACE 1                                                                
HOOK     NTR1                                                                   
         MVC   XHEAD3+11(4),=C'US$ '      US DOLLAR                             
         CLI   SVCUR,C'U'                                                       
         BE    *+10                                                             
         MVC   XHEAD3+11(4),=C'CAN$'      CANADIAN DOLLAR                       
         SPACE 1                                                                
         MVC   XHEAD4+80(17),TPPERIOD     REQUESTED PERIOD                      
         MVC   XHEAD4+11(L'SVEMP),SVEMP   EMPLOYER AND NAME                     
         MVC   XHEAD4+16(L'SVEMPNM),SVEMPNM                                     
         SPACE 1                                                                
         LA    R4,XP               R4=A(PRINT LINE)                             
         USING PRNTD,R4                                                         
         CLC   PRNTSSN,XSPACES     IF CONTINUATION                              
         BNE   HOOK20                                                           
         CLI   PRNTMID,C'Y'        AND NOT PRINTING MIDDLE BOX                  
         BE    HOOK20                                                           
         MVC   PRNTID(L'SVW4),SVW4 RESET SSN                                    
         MVC   PRNTSSNN,=CL21'(CONTINUED)'                                      
*                                                                               
HOOK20   TM    TPOPTS,TPNOBOX      IF BOXES                                     
         BO    HOOKX                                                            
         L     R4,MYABOX           SET UP BOXES                                 
         USING BOXD,R4                                                          
         MVC   BOXROWS,XSPACES     SET ROWS                                     
         MVI   BOXROWS+5,C'T'      TOP                                          
         MVI   BOXROWS+9,C'M'      MIDDLE                                       
         MVI   BOXROWS+60,C'B'     BOTTOM                                       
         SPACE 1                                                                
         MVC   BOXCOLS,XSPACES     SET COLUMNS                                  
         LA    R2,BOXCOLS                                                       
         USING PRNTD,R2            USE PRINT LINE DSECT                         
         MVI   BL,C'L'                                                          
         MVI   BC1,C'C'                                                         
         MVI   BC2,C'C'                                                         
         MVI   BC3,C'C'                                                         
         MVI   BC4,C'C'                                                         
         MVI   BC5,C'C'                                                         
         MVI   BC6,C'C'                                                         
         MVI   BC7,C'C'                                                         
         MVI   BC8,C'C'                                                         
         MVI   BC9,C'C'                                                         
         MVI   BC10,C'C'                                                        
         MVI   BC11,C'C'                                                        
         MVI   BC12,C'C'                                                        
         MVI   BR,C'R'                                                          
         SPACE 1                                                                
         MVI   BOXYORN,C'Y'        INITIALIZE REMAINING FLDS                    
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
HOOKX    B     XIT                                                              
         DROP  R2,R4                                                            
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
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
*                                                                               
XIT      XIT1                                                                   
         SPACE 3                                                                
         GETEL R4,DATADISP,ELCODE                                               
         SPACE 3                                                                
*              CONSTANTS, ETC.                                                  
         SPACE 1                                                                
SORTCARD DC    CL80'SORT FIELDS=(1,27,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=256'                                   
         SPACE 1                                                                
LTNOTFND DC    C'*NOT FOUND* '                                                  
LTID     DC    C'ID# '                                                          
LTQTRTOT DC    C'XXX Quarter'                                                   
LTTOTAL  DC    C'Totals'                                                        
LTYTDTOT DC    C'State Totals'                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              REPORT SPECS                                                     
         SPACE 1                                                                
MYSPECS  DS    0H                                                               
         SPROG 0,1,2                                                            
         WSPEC H1,2,RUN                                                         
         WSPEC H1,141,REPORT                                                    
         WSPEC H1,153,PAGE                                                      
         WSPEC H2,141,REQUESTOR                                                 
         WSPEC H1,83,C'Purged Checks'                                           
*                                                                               
         SPROG 1                                                                
         WSPEC H2,83,13C'-'                                                     
         SPROG 0,1,2                                                            
         WSPEC H2,83,13X'BF'                                                    
*                                                                               
         SPROG 0,1,2                                                            
         WSPEC H3,2,C'Currency'                                                 
         WSPEC H4,2,C'Employer'                                                 
*                                                                               
         WSPEC H7,02,C'S/S Number, Name    '                                    
         WSPEC H8,02,C'and Address         '                                    
         WSPEC H7,24,C'Check Dt  Tax Wrk Agency Check No'                       
         WSPEC H8,24,C'Indiv SSN         Inv No Frst Srv'                       
         WSPEC H7,058,C'Commercial   Type Union Agent'                          
         WSPEC H8,058,C'Check Comment          '                                
*                                                                               
         SPROG 1                                                                
         WSPEC H7,02,C'S/S Number, Name    '                                    
         WSPEC H8,02,C'and Address         '                                    
         WSPEC H9,02,C'----------------    '                                    
         WSPEC H7,24,C'Check Dt  Tax Wrk Agency Check No'                       
         WSPEC H8,24,C'Indiv SSN         Inv No Frst Srv'                       
         WSPEC H9,24,C'--------- --- --- ------ --------'                       
         WSPEC H7,058,C'Commercial   Type Union Agent'                          
         WSPEC H8,058,C'Check Comment---- ----- -----'                          
         WSPEC H9,058,C'-------------          '                                
*                                                                               
         SPROG 0,1,2                                                            
         WSPEC H7,088,C'    Gross     Non-Taxable    Reimb Exp'                 
         WSPEC H7,127,C' Due Company  Federal Tax       FICA  '                 
         WSPEC H8,088,C'   State Tax  State Disab  State Unemp'                 
         WSPEC H8,127,C'   Local Tax      Lien       Net Check'                 
         WSPEC H9,088,C'  Sub to PNH   Canada Tax    Misc Ded '                 
         WSPEC H9,127,C' Motion Pict  Perm Charity  Union Dues'                 
         DC    X'00'                                                            
         EJECT                                                                  
*              SOME TABLES                                                      
         SPACE                                                                  
*              TABLE OF INVOICES OF EXTRA CHECKS TO READ                        
         SPACE                                                                  
XTRCHKS  DS    0XL6                                                             
         DC    XL6'199007007300'                                                
         DC    XL6'199008004900'                                                
         DC    XL6'199009011600'                                                
         DC    XL6'199011008500'                                                
         DC    XL6'199011008600'                                                
         DC    XL6'199011008700'                                                
         DC    XL6'199011008800'                                                
         DC    XL6'199011008900'                                                
         DC    XL6'199011009000'                                                
         DC    XL6'199011009100'                                                
         DC    XL6'199011009200'                                                
         DC    XL6'199011009300'                                                
         DC    XL6'199011009400'                                                
         DC    XL6'199011009500'                                                
         DC    X'FF'                                                            
         SPACE 1                                                                
*              POSITIONAL QUARTER TABLE                                         
QTRTAB   DS    0CL3                                                             
         DC    CL3'1st'                                                         
         DC    CL3'2nd'                                                         
         DC    CL3'3rd'                                                         
         DC    CL3'4th'                                                         
         SPACE 1                                                                
*              HIGH LEVEL TOTALS                                                
TOTALS   DS    0CL(SORTAMLQ+L'PRNTSSNN)                                         
W4TOT    DC    (NAMTS)PL8'00',CL21'W4 Totals      '                             
EMPTOT   DC    (NAMTS)PL8'00',CL21'Employer Totals'                             
CURTOT   DC    (NAMTS)PL8'00',CL21'Currency Totals '                            
NTOTS    EQU   (*-TOTALS)/L'TOTALS        # OF ACCUMULATORS                     
NSUMTOTS EQU   (*-EMPTOT)/L'TOTALS        # OF ACCUMS (FOR SUMM OPT)            
         SPACE 1                                                                
MAXSTS   EQU   50                                                               
STTABEQU EQU   (MAXSTS*STLNQ)        LENGTH OF QTRSTTAB OR YTDSTTAB             
         SPACE 2                                                                
*              DSECT TO COVER TOTALS TABLE                                      
         SPACE                                                                  
TOTALSD  DSECT                                                                  
TOTAMTS  DS    CL(SORTAMLQ)                                                     
TOTLIT   DS    CL(L'PRNTSSNN)                                                   
         SPACE 2                                                                
*              DSECT TO COVER QTR/YTD STATE TABLE                               
         SPACE                                                                  
STTABD   DSECT                                                                  
STTAX    DS    CL2                                                              
STAMTS   DS    CL(SORTAMLQ)                                                     
STLNQ    EQU   *-STTABD                                                         
         EJECT                                                                  
*              DSECT TO COVER LOCAL WORKING STORAGE                             
         SPACE 1                                                                
TPD      DSECT                                                                  
         DS    0A                                                               
MYABOX   DS    A                   A(BOXES)                                     
MYSORTER DS    A                   A(SORTER)                                    
AQTRSTTB DS    A                   A(QTRSTTAB)                                  
AYTDSTTB DS    A                   A(YTDSTTAB)                                  
*                                                                               
QSTCNT   DS    F                   STATE COUNTER FOR QUARTER                    
YSTCNT   DS    F                   STATE COUNTER FOR YEAR                       
KPCOUNT  DS    F                   COUNT OF DELETED CHECK RECORDS               
KPPAGES  DS    H                   COUNT OF TOTAL NUMBER OF PAGES               
*                                                                               
TPOPTS   DS    XL1                 OPTIONS                                      
TPTRACE  EQU   X'80'               TRACE ACTIVE                                 
TPDELETE EQU   X'40'               DELETE THE CHECKS                            
TPNOBOX  EQU   X'20'               NO BOXES                                     
TPSUMM   EQU   X'10'               SUMMARY (PRINT HIGH LEVEL TOTS ONLY)         
TPAUDIT  EQU   X'08'               AUDIT (CHECK AMOUNTS)                        
TPMYCKRD EQU   X'04'               READ CHECK RECORDS MYSELF                    
*                                                                               
TFLAG    DS    XL1                                                              
*                                                                               
TPPERIOD DS    CL17                DISPLAYABLE REQUEST PERIOD                   
*                                                                               
SVVALS   DS    0C                                                               
SVQTR    DS    XL1                 QUARTER (1-4)                                
SVW4     DS    CL(L'TGSSN)         SAVED S/S NUMBER                             
SVEMP    DS    CL(L'TGEMP)         SAVED EMPLOYER                               
SVCUR    DS    CL1                 CURRENCY                                     
*                                                                               
SVEMPNM  DS    CL(L'TGNAME)        SAVED EMPLOYER NAME                          
SVW4NM   DS    CL(L'TGNAME)        SAVED W4 NAME                                
SVW4ADD  DS    4CL(L'PRNTSSNN)     THREE ADDRESS LINES/ONE CY,ST,ZIP            
*                                                                               
SVSPNH   DS    CL(L'SORTSPNH)      SAVED SUBJECT TO PNH                         
*                                                                               
PRNTD3   DS    CL1                 Y=THREE AMOUNT LINES PRINTED                 
PRNTDA   DS    CL1                 Y=W4 ADDRESS PRINTED                         
PRNTMID  DS    CL1                 Y=PRINTING MIDDLE LINE                       
CURQTR   DS    XL1                 CURRENT QUARTER                              
*CURBYR  DS    XL1                 CURRENT BINARY YEAR                          
MYCID    DS    CL12                COMMERCIAL ID                                
MYINV    DS    XL6                 OVERRIDE INVOICE                             
MYCDTE   DS    XL3                 OVERRIDE CHECK DATE                          
*                                                                               
SRTREC   DS    CL(SORTLNQ)         SORT RECORD AREA                             
*                                                                               
TPLNQ    EQU   *-TPD                                                            
         EJECT                                                                  
*              DSECT TO COVER SORT RECORD                                       
         SPACE 1                                                                
SORTD    DSECT                                                                  
SORTCUR  DS    CL1                 CURRENCY                                     
SORTEMP  DS    CL3                 EMPLOYER                                     
SORTW4   DS    CL9                 S/S NUMBER/CORP ID                           
SORTDTE  DS    PL3                 CHECK DATE                                   
SORTQTR  DS    XL1                 QUARTER (1-4)                                
SORTCHK  DS    CL8                 CHECK NUMBER                                 
SORTTXST DS    CL2                 TAXABLE STATE                                
*                                                                               
SORTPTYP DS    CL1                                                              
SORTSESS EQU   C'S'                SESSION                                      
SORTREUS EQU   C'R'                REUSE                                        
SORTTXCY DS    CL3                 TAXABLE CITY                                 
SORTSOW  DS    CL2                 STATE OF WORK                                
SORTLOW  DS    CL3                 LOCAL OF WORK                                
SORTISSN DS    CL9                 INDIVIDUAL SSN (FOR CORP)                    
SORTAGY  DS    CL6                 AGENCY                                       
SORTCID  DS    CL12                COMMERCIAL ID                                
SORTINV  DS    CL6                 INVOICE NUMBER                               
SORTUN   DS    CL3                 UNION                                        
SORTNCDE DS    CL2                 AGENT CODE                                   
SORTFRST DS    XL3                 FIRST SERVICE DATE                           
SORTCMNT DS    CL30                CHECK COMMENT                                
SORTPRT  DS    XL1                 PRINT STATUS                                 
SORTNOP  EQU   X'80'               DON'T PRINT-JUST DELETE                      
SORTSUBP EQU   X'40'               PRINT SPNH AT DETAIL LEVEL TOO               
SORTCKDA DS    XL4                 D/A OF CHECK RECORD                          
*                                                                               
SORTAMTS DS    0PL8                                                             
SORTEARN DS    PL8                 GROSS EARNINGS                               
SORTNTAX DS    PL8                 NON-TAXABLE EARNINGS                         
SORTREXP DS    PL8                 REIMBURSED EXPENSES                          
SORTDUE  DS    PL8                 DUE COMPANY WITHOLDING                       
SORTFTAX DS    PL8                 FEDERAL TAX                                  
SORTFICA DS    PL8                 FICA                                         
SORTSTAX DS    PL8                 STATE TAX                                    
SORTSDI  DS    PL8                 STATE DISABILITY                             
SORTSUI  DS    PL8                 STATE UNEMPLOYMENT                           
SORTLTAX DS    PL8                 LOCAL TAX                                    
SORTLIEN DS    PL8                 LIEN WITHOLDING                              
SORTNET  DS    PL8                 NET                                          
SORTSPNH DS    PL8                 SUBJECT TO PNH                               
SORTCTAX DS    PL8                 CANADA TAX                                   
SORTMDED DS    PL8                 MISC. DEDUCTIONS                             
SORTMPR  DS    PL8                 MOTION PICTURE                               
SORTCHAR DS    PL8                 PERM CHARITY                                 
SORTDUES DS    PL8                 UNION DUES                                   
SORTAMTX EQU   *                                                                
SORTAMLQ EQU   *-SORTAMTS                                                       
NAMTS    EQU   SORTAMLQ/L'SORTAMTS                                              
SORTLNQ  EQU   *-SORTD                                                          
         EJECT                                                                  
*              DSECT TO COVER PRINT LINE                                        
         SPACE 1                                                                
PRNTD    DSECT                                                                  
BL       DS    CL1                                                              
PRNTID   DS    CL4                 ID#                                          
PRNTSSN  DS    CL9                 S/S NUMBER                                   
         DS    CL8                 SPARE                                        
BC1      DS    CL1                                                              
PRNTDTE  DS    CL8                 CHECK DATE                                   
         DS    CL1                                                              
BC2      DS    CL1                                                              
PRNTTXST DS    CL2                 TAXABLE STATE                                
         DS    CL1                                                              
BC3      DS    CL1                                                              
PRNTWKST DS    CL2                 WORK STATE                                   
         DS    CL1                                                              
BC4      DS    CL1                                                              
PRNTAGY  DS    CL6                 AGENCY                                       
BC5      DS    CL1                                                              
PRNTCHK  DS    CL8                 CHECK NUMBER                                 
BC6      DS    CL1                                                              
PRNTCID  DS    CL12                COMMERCIAL ID                                
         DS    CL2                 SPACE                                        
PRNTTYPE DS    CL1                 SESSION OR REUSE                             
         DS    CL4                 SPACE                                        
PRNTUN   DS    CL3                 UNION                                        
         DS    CL2                 SPACE                                        
PRNTNCDE DS    CL4                 AGENT                                        
         DS    CL2                 SPARE                                        
*                                                                               
PRNTAMTS EQU   *                   ACCUMULATORS START HERE                      
BC7      DS    CL1                                                              
         DS    CL12                                                             
BC8      DS    CL1                                                              
         DS    CL12                                                             
BC9      DS    CL1                                                              
         DS    CL12                                                             
BC10     DS    CL1                                                              
         DS    CL12                                                             
BC11     DS    CL1                                                              
         DS    CL12                                                             
BC12     DS    CL1                                                              
         DS    CL12                                                             
BR       DS    CL1                                                              
         ORG   PRNTD+198                                                        
         DS    CL1                                                              
PRNTSSNN DS    CL21                PERFORMER NAME                               
         DS    CL1                                                              
PRNTISSN DS    CL9                 INDIVIDUAL SSN                               
         DS    CL1                                                              
PRNTTXCY DS    CL3                 TAXABLE CITY                                 
         DS    CL1                                                              
PRNTWKCY DS    CL3                 WORK CITY                                    
         DS    CL1                                                              
PRNTINV  DS    CL6                 INVOICE NUMBER                               
         DS    CL1                                                              
PRNTFRST DS    CL8                 FIRST SERVICE DATE                           
         DS    CL1                                                              
PRNTCOM  DS    CL30                CHECK COMMENT                                
         DS    CL1                                                              
         EJECT                                                                  
       ++INCLUDE TAREPFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPC5D                                                       
         EJECT                                                                  
* DDGENTWA   (MUST FOLLOW LAST SCREEN)                                          
* DDPERVAL                                                                      
* DDSPOOLD                                                                      
* DDWIDED                                                                       
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
       ++INCLUDE DDWIDED                                                        
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAREPWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'056TAREP1D   08/13/14'                                      
         END                                                                    
