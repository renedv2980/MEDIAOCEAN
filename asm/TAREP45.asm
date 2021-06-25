*          DATA SET TAREP45    AT LEVEL 073 AS OF 06/10/13                      
*PHASE T70345A                                                                  
         TITLE 'T70345 - COPURGE REPORT AND COHIST REPORT'                      
T70345   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70345,R6,R5                                                   
         L     RC,0(R1)                                                         
         USING GEND,RC             RC=A(CONTROLLER W/S)                         
         L     RA,ATWA                                                          
         USING T703FFD,RA          RA=A(SCREEN)                                 
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          R9=A(SYSTEM W/S)                             
         LA    R7,BUFF                                                          
         LA    R7,8(R7)                                                         
         USING WORKD,R7            R7=A(LOCAL W/S)                              
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
*                                                                               
* COHIST/REPORT - NEVER MARKS ANY RECORDS.  IT IS REALLY A SOFT                 
* VERSION OF THE COPURGE/REPORT WITH ONE MAJOR EXCEPTION.  A                    
* COMMERCIAL IS ELIGIBLE FOR DISPLAY ON COHIST REPORT IF THE LAST               
* PAY DATE FALLS WITHIN THE REQUESTED PERIOD.  THE COPURGE/REPORT               
* HAS STRICTER ELIGIBILITY RULES (SEE TSTDEL FOR DETAILS).                      
*                                                                               
*              MODE CONTROLLED ROUTINES                                         
         SPACE 1                                                                
         GOTO1 INITIAL,DMCB,0      INITIALIZE                                   
         SPACE 1                                                                
         CLI   MODE,VALKEY         VALIDATE SCREEN                              
         BNE   *+12                                                             
         BAS   RE,VKEY             VALIDATE SCREEN FIELDS                       
         B     XIT                                                              
         SPACE 3                                                                
         CLI   MODE,PRINTREP       PROCESS REPORT                               
         BNE   XIT                                                              
         BAS   RE,PREP             PRINT REPORT                                 
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE SCREEN                                                  
         SPACE 1                                                                
VKEY     NTR1                                                                   
         LR    RE,R7               CLEAR LOCAL W/S                              
         LA    RF,WORKLNQ                                                       
         XCEFL ,                                                                
*                                                                               
         LHI   RF,TIEND-TASYSIOD                                                
         XCEFL TASYSIOD,(RF)                                                    
*                                                                               
         XC    TIFILTS,TIFILTS     CLEAR SYSIO FILTERS                          
*                                                                               
         CLI   RECNUM,CP           IF CP                                        
         BNE   *+12                                                             
         BAS   RE,VKCP             VALIDATE COPURGE SCREEN                      
         B     *+8                                                              
         BAS   RE,VKCH             ELSE, VALIDATE COHIST SCREEN                 
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE COPURGE SCREEN                               
*                                                                               
VKCP     NTR1                                                                   
         LA    R2,SCPPDH           R2=A(PERIOD FIELD)                           
         BAS   RE,VALPD            VALIDATE PERIOD                              
         SPACE 1                                                                
         LA    R2,SCPOPTH          R2=A(OPTIONS FIELD)                          
         BAS   RE,VALOPT           VALIDATE OPTIONS                             
         SPACE 1                                                                
         TM    CPOPTS,CPSUMM       IF ASKING FOR SUMMARY OPTION                 
         BO    *+12                                                             
         TM    CPOPTS,CPDELETE     OR IF NOT DELETING                           
         BO    VKCPX                                                            
         LA    R2,CONOUTH          MUST HAVE OUTPUT                             
         GOTO1 ANY                 (HOPEFULLY 1PP)                              
         SPACE 1                                                                
VKCPX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE COHIST SCREEN                                
*                                                                               
VKCH     NTR1                                                                   
         XC    SCHCOMN,SCHCOMN     PRE CLEAR NAME FIELDS                        
         OI    SCHCOMNH+6,X'80'                                                 
         XC    SCHCLIN,SCHCLIN                                                  
         OI    SCHCLINH+6,X'80'                                                 
         XC    SCHPRDN,SCHPRDN                                                  
         OI    SCHPRDNH+6,X'80'                                                 
*                                                                               
         LA    R2,SCHAGYH          R2=A(AGENCY FIELD)                           
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'08',(R2)),SCHAGYNH                        
         MVC   TIFAGY,TGAGY                                                     
*                                                                               
         LA    R2,SCHPDH           R2=A(PERIOD FIELD)                           
         BAS   RE,VALPD            VALIDATE PERIOD                              
*                                                                               
         LA    R2,SCHCOMH          R2=A(COMMERCIAL FIELD)                       
         CLI   5(R2),0                                                          
         BE    VKCH10                                                           
         GOTO1 RECVAL,DMCB,TLCOICDQ,(X'08',(R2)),SCHCOMNH                       
         L     R4,AIO                                                           
         USING TLCOD,R4                                                         
         MVC   TIFCLI,TLCOCLI                                                   
         MVC   TIFPRD,TLCOPRD                                                   
         MVI   ELCODE,TACOELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACOD,R4                                                         
         MVC   TIQSTART(L'TACOCID),TACOCID                                      
         MVC   TIFCID,TACOCID                                                   
         DROP  R4                                                               
*                                                                               
VKCH10   LA    R2,SCHCLIH          R2=A(CLIENT FIELD)                           
         CLI   5(R2),0                                                          
         BE    VKCH20                                                           
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'08',(R2)),SCHCLINH                        
         CLI   SCHCOMH+5,0         IF COMMERCIAL INPUT                          
         BE    *+14                                                             
         CLC   TIFCLI,TGCLI        REQUESTED CLIENT MUST BE SAME                
         BNE   FLDINV                                                           
         MVC   TIFCLI,TGCLI                                                     
*                                                                               
VKCH20   LA    R2,SCHPRDH          R2=A(PRODUCT FIELD)                          
         CLI   5(R2),0             IF PRODUCT INPUT                             
         BE    VKCH30                                                           
         CLI   SCHCLIH+5,0         REQUIRE CLIENT                               
         BE    CLIMISS                                                          
         GOTO1 RECVAL,DMCB,TLPRCDQ,(X'08',(R2)),SCHPRDNH                        
         CLI   SCHCOMH+5,0         IF COMMERCIAL INPUT                          
         BE    *+14                                                             
         CLC   TIFPRD,TGPRD        REQUESTED PRODUCT MUST BE SAME               
         BNE   FLDINV                                                           
         MVC   TIFPRD,TGPRD                                                     
*                                                                               
VKCH30   LA    R2,SCHOPTH          R2=A(OPTIONS FIELD)                          
         BAS   RE,VALOPT           VALIDATE OPTIONS                             
*                                                                               
         TM    WHEN,X'20'          IF SOON                                      
         BZ    VKCHX                                                            
         OC    TIFCID,TIFCID       THEN COMMERCIAL INPUT REQUIRED               
         BNZ   VKCHX                                                            
         OC    TIFCLI,TIFCLI       OR CLIENT INPUT REQUIRED                     
         BNZ   VKCHX                                                            
         B     COMMISS                                                          
*                                                                               
VKCHX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE VALIDATES PERIOD FIELD                                   
*                                  NTRY (R2) = A(PERIOD FIELD)                  
         SPACE                                                                  
VALPD    NTR1                                                                   
         LA    R3,BLOCK            R3=A(BLOCK TO PASS)                          
         USING PERVALD,R3                                                       
         GOTO1 PDVAL,DMCB,(R3)     GO TO SYSTEM PERIOD VAL. ROUTINE             
*                                                                               
         MVC   CPPERIOD,PVALCPER   SAVE DISPLAYABLE PERIOD IN W/S               
         MVC   CPPSTA,PVALPSTA     SAVED PACKED START DATE                      
         MVC   CPPEND,PVALPEND     SAVED PACKED END DATE                        
*                                                                               
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              ROUTINE TO VALIDATE OPTIONS                                      
*                                  NTRY (R2) = A(OPTIONS FIELD)                 
         SPACE 1                                                                
VALOPT   NTR1                                                                   
         CLI   5(R2),0             IF OPTIONS FIELD INPUT                       
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
VOPT2    CLI   RECNUM,CH           IF COHIST/REPORT                             
         BNE   VOPT3                                                            
*                                                                               
         CLC   =C'PID',SCDATA1     PID OPTION                                   
         BNE   VOPT3                                                            
         OI    CPOPTS,CPPID        PRINT PID INSTEAD OF SSN                     
         B     VOPT8                                                            
*                                                                               
VOPT3    CLI   RECNUM,CP           IF COPURGE/REPORT                            
         BNE   VOPT6                                                            
*                                                                               
         CLC   =C'SUMMARY',SCDATA1   SUMMARY FILTER                             
         BNE   VOPT4                                                            
         OI    CPOPTS,CPSUMM       PRINT COMMERCIALS MARKED                     
         B     VOPT8                                                            
*                                                                               
VOPT4    CLC   =C'DELETE',SCDATA1  DELETE FILTER                                
         BNE   VOPT5                                                            
         OI    CPOPTS,CPDELETE     MARK THE RECORDS FOR DELETION                
         B     VOPT8                                                            
*                                                                               
VOPT5    CLC   =C'AGY',SCDATA1     AGENCY FILTER                                
         BNE   VOPT6                                                            
         MVC   TGAGY,SCDATA2                                                    
         MVC   TIFAGY,SCDATA2                                                   
         GOTO1 RECVAL,DMCB,TLAYCDQ,0                                            
         BNE   FLDINV                                                           
         B     VOPT8                                                            
*                                                                               
VOPT6    CLC   =C'NOBOX',SCDATA1    NO BOXES                                    
         BNE   VOPT7                                                            
         OI    CPOPTS,CPNOBOX      SET TO NOT PRINT BOXES                       
         B     VOPT8                                                            
*                                                                               
VOPT7    CLC   =C'TRACE',SCDATA1  TRACE                                         
         BNE   FLDINV                                                           
         CLI   SCDATA2,C'Y'                                                     
         BNE   FLDINV                                                           
         OI    CPOPTS,CPTRACE      SET TRACE ON                                 
         B     VOPT8               (REMOVE WIDTH=166 JCL CARD TO WORK)          
*                                                                               
VOPT8    LA    R3,SCANNEXT         BUMP TO NEXT                                 
         BCT   R0,VOPT2            AND CONTINUE                                 
*                                                                               
VOPTX    B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              ROUTINE CONTROLS REPORT GENERATION                               
         SPACE 1                                                                
PREP     NTR1                                                                   
         XC    CPCOUNT,CPCOUNT     CLEAR COMMERCIAL DELETE COUNT                
         XC    CPECNT,CPECNT       CLEAR COMMERCIAL ERROR COUNT                 
         L     R2,ASPOOLD                                                       
         USING SPOOLD,R2                                                        
         LA    R1,MYSPECS3         SET A(SPECS) FOR PRINTING                    
         TM    CPOPTS,CPPID                                                     
         BZ    PREP00                                                           
         LA    R1,MYSPECS4                                                      
         SPACE 1                                                                
PREP00   ST    R1,SPECS                                                         
         LA    R1,HOOK             SET A(HEADLINE HOOK)                         
         ST    R1,HEADHOOK                                                      
         LA    R1,MHOOK            SET A(MIDLINE HOOK)                          
         ST    R1,MIDHOOK                                                       
         SPACE 1                                                                
         MVC   TIACOMFC,ACOMFACS   SET UP SYSIO BLOCK                           
         MVC   TIHOOK,=A(IOHOOK)   A(I/O HOOK)                                  
         MVC   TIACCS,TWAACCS      LIMIT ACCESS                                 
         MVC   TIAUTH,TWAAUTH      AUTHORIZATION                                
         MVC   TIUSERID,TWAORIG    REQUESTING ID                                
         MVI   TIREAD,TLCOCDQ      READ COMMERCIAL RECORDS                      
         TM    CPOPTS,CPSUMM       IF DETAIL REPORT                             
         BO    PREP10                                                           
         MVI   TISUBRD,TLCACDQ     SUBREAD CAST RECORDS                         
         OI    TIQFLAG2,TIQFHINV   SUBREAD INVOICE HISTORY RECS TOO             
*                                                                               
PREP10   OI    TIQFLAG2,TIQFPGRY+TIQFSUB+TIQFPRI  PASS GREY,SUB & PRI           
         OI    TIQFLAGS,TIQFPDUM+TIQFPBNP         PASS DUMMY & BNP              
         MVI   FRST,C'Y'           FIRST TIME                                   
         SPACE 1                                                                
         GOTO1 TASYSIO,DMCB,TASYSIOD  OFF TO SYSIO TO DO I/O                    
         SPACE 1                                                                
         XR    R1,R1               SET TOTAL PAGES PRINTED/SPOOLED              
         ICM   R1,3,PAGE                                                        
         BCTR  R1,0                                                             
         STCM  R1,3,CPPAGES                                                     
         SPACE 1                                                                
         CLI   RECNUM,CP           IF CP                                        
         BNE   PREP20                                                           
**NO-OP* TM    CPOPTS,CPSUMM       IF ASKING FOR SUMMARY                        
**NO-OP* BZ    *+8                                                              
         BAS   RE,PRTTOT           PRINT TOTAL COUNTS                           
         SPACE 1                                                                
PREP20   TM    CPOPTS,CPDELETE     IF MARKING FOR DELETION                      
         BZ    *+8                                                              
         BAS   RE,UPDTLSY          UPDATE SYSTEM RECORD                         
         SPACE 1                                                                
PREPX    B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*              ROUTINE TO UPDATE THE SYSTEM RECORD                              
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
         MVI   TADCTYPE,TADCTCP    SET FROM COPURGE/REPORT                      
         MVC   TADCDATE,TGTODAY1   SET DATE DELETED FROM FILE                   
         MVC   TADCCNT,CPCOUNT     SET TOTAL RECORD COUNT                       
         MVC   TADCPGS,CPPAGES     SET TOTAL NUMBER OF PAGES                    
         GOTO1 ADDELEM                                                          
         GOTO1 PUTREC                                                           
         GOTO1 MYTRACE                                                          
         B     XIT                                                              
         DROP  R4                                                               
         SPACE 1                                                                
*              ROUTINE TO PRINT TOTALS                                          
PRTTOT   NTR1                                                                   
         BAS   RE,BXBOT            CLOSE ANY REMAINING OPEN BOXES               
         L     R1,ASPOOLD          R1=A(SPOOL DSECT)                            
         USING SPOOLD,R1                                                        
         XC    SPECS,SPECS         CLEAR A(SPECS)                               
         MVI   PROCSW,0            NOT PROCESSING                               
         BAS   RE,NEWPAGE          FORCE TO NEW PAGE                            
         MVI   FORCECLR,C'Y'       FORCE CLEARING OF ALL LINES                  
         BAS   RE,PRNTIT2                                                       
         OC    CPCOUNT,CPCOUNT                                                  
         BZ    PRTTOT5                                                          
         MVC   XP(27),=CL27'COMMERCIALS MARKED PURGED ='                        
         EDIT  CPCOUNT,(12,XP+29),ALIGN=LEFT,ZERO=NOBLANK                       
         BAS   RE,PRNTIT2                                                       
*                                                                               
PRTTOT5  DS    0H                                                               
**NO-OP* OC    CPECNT,CPECNT                                                    
**NO-OP* BZ    PRTTOT8                                                          
**NO-OP* MVC   XP(27),=CL27'COMMERCIALS MARKED ERROR  ='                        
**NO-OP* EDIT  CPECNT,(12,XP+29),ALIGN=LEFT,ZERO=NOBLANK                        
**NO-OP* BAS   RE,PRNTIT2                                                       
*                                                                               
PRTTOT8  OC    CPPAGES,CPPAGES                                                  
         BZ    PRTTOTX                                                          
         MVC   XP(27),=CL27'TOTAL NUMBER OF PAGES     ='                        
         EDIT  CPPAGES,(12,XP+29),ALIGN=LEFT,ZERO=NOBLANK                       
         BAS   RE,PRNTIT2                                                       
PRTTOTX  B     XIT                                                              
         DROP  R1                                                               
         EJECT                                                                  
*              PROCESS RECORDS FROM SYSIO AND PRINTS REPORT                     
IOHOOK   NTR1                                                                   
         MVC   AIO,TIAREC          SET IOAREA                                   
         L     R4,TIAREC           R4=A(FILE RECORD)                            
         USING TLDRD,R4                                                         
*                                                                               
         TM    CPOPTS,CPSUMM       IF ASKING FOR SUMMARY OPTION                 
         BO    *+12                ONLY GETTING COMMERCIAL HOOK                 
         CLI   TIMODE,PROCCOMM     IF PROCESSING COMMERCIAL RECORDS             
         BNE   IOHK10                                                           
*                                                                               
         CLI   TLDRCD,TLCOCDQ      IF COMMERCIAL RECORD                         
         BNE   IOHKX                                                            
         BAS   RE,PROCCOM          PROCESS COMMERCIAL RECORD                    
         B     IOHKX                                                            
*                                                                               
IOHK10   CLI   TIMODE,PROCREC      IF PROCESSING FILE RECORD                    
         BNE   IOHK20                                                           
*                                                                               
         CLI   TLDRCD,TLCACDQ      IF CAST RECORD                               
         BNE   IOHKX                                                            
         BAS   RE,PROCCAST         PROCESS CAST RECORD                          
         B     IOHKX                                                            
*                                                                               
IOHK20   CLI   TIMODE,PROCINV      IF PROCESSING INVOICE RECORDS                
         BNE   IOHKX                                                            
*                                                                               
         CLI   TLDRCD,TLINCDQ      IF HISTORY INVOICE RECORD                    
         BNE   *+8                                                              
         BAS   RE,PROCHINV         PROCESS HISTORY INVOICE RECORD               
         MVI   TIMODE,PROCNOCK     NEVER PROCESS CHECKS                         
*                                                                               
IOHKX    MVC   AIO,AIO1            RESET IOAREA                                 
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO PROCESS COMMERCIAL RECORDS                            
         SPACE                                                                  
         USING TLCOD,R4            R4=A(COMMERCIAL RECORD)                      
PROCCOM  NTR1                                                                   
         BAS   RE,TSTDEL           TEST COMML ELIGIBLE FOR DELETE               
         CLI   TIMODE,PROCNOCA                                                  
         BE    PROCCX                                                           
*                                                                               
         MVI   PROCSW,PRSWCOM      PROCESSING COMMERCIAL                        
         CLI   FRST,C'Y'           IF FIRST TIME IN                             
         BNE   PROCC5                                                           
         OI    FRSTIME,FRSTCST+FRSTINV SET FIRST CAST/INV FOR COMML             
         MVI   FRST,C'N'                                                        
         B     PROCC10                                                          
*                                                                               
PROCC5   BAS   RE,BXBOT            CLOSE ANY REMAINING OPEN BOXES               
         TM    FRSTIME,FRSTCST     IF NO CAST OR HISTORY FOR COMML              
         BZ    PROCC8                                                           
         TM    FRSTIME,FRSTINV                                                  
         BZ    PROCC8                                                           
         BAS   RE,PRNTIT           PRINT LINE (FOR HEADLINE)                    
*                                                                               
PROCC8   BAS   RE,NEWPAGE          FORCE NEW COMML TO NEW PAGE                  
         OI    FRSTIME,FRSTCST+FRSTINV RESET 1ST CAST/INV FOR COMML             
*                                                                               
PROCC10  BAS   RE,XTLCO            EXTRACT DATA FROM COMMERCIAL REC             
*                                                                               
         BAS   RE,ANYFROM          CHECK 'FROM' COMMERCIAL ON RECORD            
*                                                                               
         L     R1,CPCOUNT          ADD TO DELETE COUNT                          
         LA    R1,1(R1)                                                         
         ST    R1,CPCOUNT                                                       
*                                                                               
         L     R4,TIAREC           R4=A(COMMERCIAL RECORD)                      
         USING TLCOD,R4                                                         
         OI    TLCOSTAT,TLCOSDEL   MARK RECORD FOR DELETION                     
         GOTO1 MYTRACE             TRACE RECORD                                 
*                                                                               
         TM    CPOPTS,CPDELETE     IF MARKING FOR DELETION                      
         BZ    PROCCX                                                           
         CLI   TWAWRITE,C'N'       IF WRITE = NO THEN DON'T                     
         BE    *+8                                                              
         MVI   TIMODE,PROCPTRC     HAVE SYSIO WRITE BACK RECORD                 
*                                                                               
PROCCX   B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO TEST IF COMMERCIAL ELIGIBLE FOR DELETION              
         SPACE                                                                  
TSTDEL   NTR1                                                                   
         L     R4,AIO              R4=A(CHECK RECORD)                           
         USING TLCOD,R4                                                         
*                                                                               
**NO-OP* TM    TLCOSTAT,TLCOSDEL   SKIP COMMLS ALREADY MARKED DELETED           
**NO-OP* BO    TSTDELN             (AS PER DAVID HABER 8/17/93)                 
*                                                                               
         MVI   ELCODE,TACOELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACOD,R4            R4=A(COMMERCIAL DETAILS ELEMENT)             
         MVC   COMMTYPE,TACOTYPE   SAVE COMMERCIAL TYPE                         
*                                                                               
         CLI   RECNUM,CP           IF COHIST/REPORT                             
         BE    TSTDEL5                                                          
         CLC   TACOPDTE,CPPSTA     AND IF LAST PAY DATE WITHIN PERIOD           
         BL    TSTDELN                                                          
         CLC   TACOPDTE,CPPEND                                                  
         BH    TSTDELN                                                          
         B     TSTDELXX            THEN COMMERCIAL ELIGIBLE                     
*                                                                               
TSTDEL5  OC    TACOPDTE,TACOPDTE   IF HISTORY FOR COMML                         
         BZ    TSTDEL10                                                         
         CLC   TACOPDTE,CPPEND     AND IF LAST PAY DATE BEFORE END              
         BH    TSTDELN                                                          
         B     TSTDELX             THEN COMMERCIAL ELIGIBLE                     
*                                                                               
TSTDEL10 L     R4,AIO              ELSE, IF NO HISTORY                          
         MVI   ELCODE,TAACELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAACD,R4                                                         
         CLC   TAACCDTE,CPPEND     IF LAST CHANGED DATE BEFORE END              
         BH    TSTDELN                                                          
         B     TSTDELX             THEN COMMERCIAL ELIGIBLE                     
*                                                                               
TSTDELX  BAS   RE,ANYCKS           COMML ELIGIBLE - MUST BE NO CHECKS           
         CLI   CHECKS,C'Y'         IF CHECKS ATTACHED TO COMMERCIAL             
         BNE   TSTDELXX                                                         
         BAS   RE,ERRCOMML         PUT COMMERCIAL RECORD IN ERROR               
*                                                                               
TSTDELN  MVI   TIMODE,PROCNOCA     COMMERCIAL NOT ELIGIBLE, DON'T TOUCH         
TSTDELXX B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO SEE IF ANY CHECKS ATTACHED TO COMMERCIAL              
*                                  XIT CHECKS SET                               
         SPACE                                                                  
ANYCKS   NTR1                                                                   
         BAS   RE,SETCHK           SET TO CHKFIL                                
         MVI   CHECKS,C'N'         NO CHECKS ATTACHED TO COMMERCIAL             
*                                                                               
         L     R4,AIO              R4=A(COMMERCIAL RECORD)                      
         USING TLCOD,R4                                                         
         XC    KEY,KEY             READ FOR CAST PAYMENT HISTORY                
         LA    RE,KEY                                                           
         USING TLCKPD,RE                                                        
         MVI   TLCKPCD,TLCKHCDQ    SET RECORD TYPE                              
         MVC   TLCKHCOM,TLCOCOM    SET INTERNAL COMMERCIAL NUMBER               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(TLCKHSSN-TLCKPD),KEYSAVE                                     
         BNE   ANYCKS10                                                         
*                                                                               
**NO-OP* MVC   AIO,AIO1            SET IOAREA FOR GETREC                        
**NO-OP* GOTO1 GETREC                                                           
**NO-OP* MVC   AIO,TIAREC          RESET IOAREA                                 
**NO-OP* L     R4,AIO1                                                          
**NO-OP* MVI   ELCODE,TACDELQ                                                   
**NO-OP* USING TACDD,R4                                                         
**NO-OP* BAS   RE,GETEL                                                         
**NO-OP* BNE   ANYCKS5                                                          
**NO-OP* CLC   TACDDTE,CPPEND      IF CHECK DATE BEFORE END                     
**NO-OP* BH    ANYCKS10                                                         
*                                                                               
         MVI   CHECKS,C'Y'         SET CHECKS ATTACHED TO COMMERCIAL            
*                                                                               
ANYCKS10 XC    KEY,KEY                                                          
         MVC   KEY(L'TIKEY),TIKEY  RESTORE SYSIO'S KEY                          
         BAS   RE,SETTAL           SET TO TALFIL                                
         B     XIT                                                              
         DROP  R4,RE                                                            
         SPACE 2                                                                
SETCHK   NTR1                                                                   
         MVC   SYSFIL,=CL8'CHKFIL'                                              
         MVC   SYSDIR,=CL8'CHKDIR'                                              
         B     XIT                                                              
         SPACE                                                                  
SETTAL   NTR1                                                                   
         MVC   SYSFIL,=CL8'TALFIL'                                              
         MVC   SYSDIR,=CL8'TALDIR'                                              
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PUT COMMERCIAL RECORD IN ERROR                        
         SPACE                                                                  
ERRCOMML NTR1                                                                   
         MVC   AIO,AIO1            SET IOAREA FOR READ                          
         XC    KEY,KEY             GET THE COMMERCIAL RECORD                    
         LA    R4,KEY                                                           
         USING TLDRD,R4                                                         
         MVC   TLDRDA,TIDSKADD                                                  
         GOTO1 GETREC                                                           
*                                                                               
         L     R4,AIO              R4=A(COMMERCIAL RECORD)                      
         USING TLCOD,R4                                                         
         OI    TLCOSTAT,TLCOSERR   SET ERROR IN STATUS BYTE                     
*                                                                               
         L     R1,CPECNT           ADD TO ERROR COUNT                           
         LA    R1,1(R1)                                                         
         ST    R1,CPECNT                                                        
*                                                                               
**NO-OP* TM    CPOPTS,CPSUMM       IF SUMMARY OPTION                            
**NO-OP* BZ    ERRCOMM5                                                         
**NO-OP* L     R1,ASPOOLD          PRINT ANY ERRORS                             
**NO-OP* USING SPOOLD,R1                                                        
**NO-OP* MVI   FORCECLR,C'Y'       FORCE CLEARING OF ALL LINES                  
**NO-OP* DROP  R1                                                               
**NO-OP* BAS   RE,PRNTIT2                                                       
**NO-OP* GOTO1 HEXOUT,DMCB,TLCOKEY,XP,L'TLCOKEY,=C'TOG'                         
**NO-OP* BAS   RE,PRNTIT2                                                       
*                                                                               
ERRCOMM5 TM    CPOPTS,CPDELETE     IF MARKING FOR DELETION                      
         BZ    ERRCOMMX                                                         
         GOTO1 PUTREC              WRITE BACK COMMERCIAL RECORD                 
*                                                                               
ERRCOMMX GOTO1 MYTRACE                                                          
         MVC   AIO,TIAREC          RESET IOAREA                                 
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO PRINT COMMERCIAL INFO IN HEADLINES                    
         SPACE                                                                  
XTLCO    NTR1                                                                   
         L     R4,AIO              R4=A(COMMERCIAL RECORD)                      
         USING TLCOD,R4                                                         
         LA    R2,XHEAD5           R2=A(HEADLINE)                               
         USING HEADD,R2                                                         
*                                                                               
         MVI   RERDSW,C'N'                                                      
         CLC   SVAGY,TIAGY         IF CHANGE IN AGENCY                          
         BE    *+8                                                              
         BAS   RE,NEWAGY           SET NEW AGENCY INFO                          
         MVC   HAGENCY,SVAGY       PRINT AGENCY CODE AND NAME                   
         MVC   HAGYNAME,SVAGYNM                                                 
*                                                                               
         CLC   SVCLI,TICLI         IF CHANGE IN CLIENT                          
         BE    *+8                                                              
         BAS   RE,NEWCLI           SET NEW CLIENT INFO                          
         MVC   HCLIENT,SVCLI       PRINT CLIENT CODE AND NAME                   
         MVC   HCLINAME,SVCLINM                                                 
         MVC   HEMP,SVEMP          PRINT EMP CODE AND NAME                      
         MVC   HEMPNAME,SVEMPNM                                                 
*                                                                               
         OC    TIPRD,TIPRD         IF NO PRODUCT                                
         BZ    *+14                                                             
         CLC   SVPRD,TIPRD         OR CHANGE IN PRODUCT                         
         BE    *+8                                                              
         BAS   RE,NEWPRD           SET NEW PRODUCT INFO                         
         MVC   HPRODUCT,SVPRD      PRINT PRODUCT CODE AND NAME                  
         MVC   HPRDNAME,SVPRDNM                                                 
*                                                                               
         GOTO1 CHAROUT,DMCB,TANAELQ,0                                           
         BNE   *+10                                                             
         MVC   HTITLE,TGNAME                                                    
*                                                                               
         BAS   RE,XCOEL            EXTRACT DATA FROM TACOD ELEMENT              
         BAS   RE,XLFEL            EXTRACT DATA FROM TALFD ELEMENT              
         BAS   RE,XCSEL            EXTRACT DATA FROM TACSD ELEMENT              
         BAS   RE,XCCEL            EXTRACT DATA FROM TACCD ELEMENT              
         BAS   RE,XCMEL            EXTRACT DATA FROM TACMD ELEMENT              
*                                                                               
         LA    R1,L'XHEAD5*7       CHECK NO NULLS IN THE 7 HEADLINES            
         LA    R2,XHEAD5                                                        
XTLCO10  CLI   0(R2),0                                                          
         BNE   *+8                                                              
         MVI   0(R2),C' '                                                       
         LA    R2,1(R2)                                                         
         BCT   R1,XTLCO10                                                       
*                                                                               
         CLI   RERDSW,C'Y'         IF NEED TO RESTORE SYSIO'S READ SEQ          
         BNE   XTLCOX                                                           
         XC    KEY,KEY             SET KEY                                      
         MVC   KEY(L'TIKEY),TIKEY                                               
         GOTO1 HIGH                                                             
XTLCOX   B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO SET NEW AGENCY INFORMATION                            
         SPACE                                                                  
NEWAGY   NTR1                                                                   
         MVC   SVAGY,TIAGY         SET NEW AGENCY CODE                          
         MVC   AIO,AIO1                                                         
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'A8',SVAGY),0                              
         MVC   AIO,TIAREC                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVAGYNM,TGNAME                                                   
         MVC   CUREMP,TGTPEMP      SET DEFAULT EMPLOYER                         
         BAS   RE,GETRULES         SET EMPLOYER OF RECORD                       
         XC    SVCLI,SVCLI         SET TO REVALIDATE CLIENT                     
         MVI   RERDSW,C'Y'                                                      
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO SET NEW CLIENT INFORMATION                            
         SPACE                                                                  
NEWCLI   NTR1                                                                   
         MVC   SVCLI,TICLI         SET NEW CLIENT CODE                          
         MVC   HCLINAME,LTNOTFND                                                
         MVC   AIO,AIO1                                                         
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'A8',SVCLI),0                              
         MVC   AIO,TIAREC                                                       
         BNE   *+14                                                             
         MVC   SVCLINM,TGNAME                                                   
         BAS   RE,GETRULES         SET EMPLOYER OF RECORD                       
*                                                                               
         CLC   SVEMP,CUREMP        IF EMPLOYER CHANGED                          
         BE    NEWCLIX                                                          
         MVC   SVEMP,CUREMP                                                     
         MVC   AIO,AIO1                                                         
         GOTO1 RECVAL,DMCB,TLEMCDQ,(X'A8',SVEMP),0                              
         MVC   AIO,TIAREC                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVEMPNM,TGNAME                                                   
*                                                                               
NEWCLIX  XC    SVPRD,SVPRD         SET TO REVALIDATE PRODUCT                    
         MVI   RERDSW,C'Y'                                                      
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO SET PRODUCT INFORMATION                               
         SPACE                                                                  
NEWPRD   NTR1                                                                   
         OC    TIPRD,TIPRD         IF THERE IS A PRODUCT                        
         BNZ   NEWPRD10                                                         
         XC    SVPRD,SVPRD         CLEAR PRODUCT CODE                           
         MVC   SVPRDNM,XSPACES     AND PRODUCT NAME                             
         GOTO1 CHAROUT,DMCB,TAFNELQ,0,TAFNTPRD                                  
         BNE   *+10                                                             
         MVC   SVPRDNM,TGNAME      SET PRODUCT NAME                             
         B     NEWPRDX                                                          
*                                                                               
NEWPRD10 MVC   SVPRD,TIPRD         SAVE PRODUCT                                 
         MVC   SVPRDNM,LTNOTFND    SET PRODUCT NOT FOUND                        
         MVC   AIO,AIO1                                                         
         GOTO1 RECVAL,DMCB,TLPRCDQ,(X'A8',SVPRD),0                              
         MVC   AIO,TIAREC                                                       
         BNE   *+10                                                             
         MVC   SVPRDNM,TGNAME                                                   
*                                                                               
NEWPRDX  MVI   RERDSW,C'Y'                                                      
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO EXTRACT DATA FROM TACOD ELEMENT                       
         SPACE                                                                  
XCOEL    NTR1                                                                   
         MVI   ELCODE,TACOELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACOD,R4            R4=A(COMM DETAILS ELEMENT)                   
*                                                                               
         MVC   HCID,TACOCID              COMMERCIAL ID                          
         GOTO1 TAXVAL,DMCB,(2,TACOADST)                                         
         BNE   *+10                                                             
         MVC   HADDSTNM,TGTANAME   ADDENDUM STATE NAME                          
*                                                                               
         GOTO1 CTYPVAL,DMCB,TACOTYPE     COMMERCIAL TYPE                        
         BNE   XCOEL10                                                          
         MVC   HCTYPENM,TGCTNAME                                                
*                                                                               
XCOEL10  OC    TACOFCYC,TACOFCYC   FIRST FIXED CYCLE DATE                       
         BZ    XCOEL15                                                          
         GOTO1 DATCON,DMCB,(1,TACOFCYC),(8,HFCYCLE)                             
*                                                                               
XCOEL15  OC    TACOAIR,TACOAIR     FIRST AIR DATE                               
         BZ    XCOEL20                                                          
         GOTO1 DATCON,DMCB,(1,TACOAIR),(8,HFAIR)                                
*                                                                               
XCOEL20  OC    TACOEXP,TACOEXP     EXPIRATION DATE                              
         BZ    XCOEL25                                                          
         GOTO1 DATCON,DMCB,(1,TACOEXP),(8,HEXPIRE)                              
*                                                                               
XCOEL25  OC    TACOPDTE,TACOPDTE   LAST PAY DATE                                
         BZ    XCOEL28                                                          
         GOTO1 DATCON,DMCB,(1,TACOPDTE),(8,HPAYDATE)                            
*                                                                               
XCOEL28  GOTO1 MEDVAL,DMCB,TACOMED                                              
         BNE   XCOEL30                                                          
         MVC   HMEDIANM,TGMENAME   MEDIA NAME                                   
*                                                                               
XCOEL30  EDIT  TACOSEC,(3,HLEN),ALIGN=LEFT  LENGTH                              
*                                                                               
         MVC   HAFMRATE,TACOAFM    AFM RATE                                     
         OC    TACODUB,TACODUB     DUB RATE                                     
         BZ    XCOEL40                                                          
         GOTO1 DATCON,DMCB,(1,TACODUB),(8,HAFMDATE)                             
*                                  AFM TITLE                                    
XCOEL40  GOTO1 CHAROUT,DMCB,TAFNELQ,0,TAFNTMUS                                  
         BNE   *+10                                                             
         MVC   HAFMTTL,TGNAME                                                   
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO EXTRACT DATA FROM TALFD ELEMENT                       
         SPACE                                                                  
XLFEL    NTR1                                                                   
         MVI   ELCODE,TALFELQ      GET LIFT DETAILS ELEMENT                     
         BAS   RE,GETEL                                                         
         BNE   XLFELX                                                           
         USING TALFD,R4                                                         
         MVC   HLIFTID,TALFLID     LIFT ID                                      
         EDIT  TALFSEC,(3,HLFTLEN),ALIGN=LEFT                                   
XLFELX   B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO EXTRACT DATA FROM TACSD ELEMENT                       
         SPACE                                                                  
XCSEL    NTR1                                                                   
         MVI   ELCODE,TACSELQ      GET COMMERCIAL STUDIO ELEMENT                
         USING TACSD,R4                                                         
*                                                                               
         MVI   WORK,TACSTYPF       FILM ELEMENT                                 
         GOTO1 GETL,DMCB,(1,WORK)                                               
         BNE   XCSEL10                                                          
         L     R4,TGELEM                                                        
         GOTO1 DATCON,DMCB,(1,TACSDATE),(8,HFLMDT)                              
         MVC   HFLMSTUD,TACSSTUD                                                
         GOTO1 XCFRMST,DMCB,HFLMCITY                                            
*                                                                               
XCSEL10  MVI   WORK,TACSTYPR       RECORDING ELEMENT                            
         GOTO1 GETL,DMCB,(1,WORK)                                               
         BNE   XCSEL20                                                          
         L     R4,TGELEM                                                        
         GOTO1 DATCON,DMCB,(1,TACSDATE),(8,HRCDDT)                              
         MVC   HRCDSTUD,TACSSTUD                                                
         GOTO1 XCFRMST,DMCB,HRCDCITY                                            
*                                                                               
XCSEL20  MVI   WORK,TACSTYPM       MUSIC ELEMENT                                
         GOTO1 GETL,DMCB,(1,WORK)                                               
         BNE   XCSELX                                                           
         L     R4,TGELEM                                                        
         GOTO1 DATCON,DMCB,(1,TACSDATE),(8,HMSCDT)                              
         MVC   HMSCSTUD,TACSSTUD                                                
         GOTO1 XCFRMST,DMCB,HMSCCITY                                            
*                                                                               
XCSELX   B     XIT                                                              
                                                                                
*              ROUTINE TO EXTRACT STATE DATA FROM TACSD ELEMENT                 
*              ON ENTRY ... R4 = A(STUDIO ELEMENT)                              
                                                                                
XCFRMST  NTR1                                                                   
         L     R1,0(R1)                                                         
         MVC   0(L'HMSCCITY,R1),XSPACES                                         
         MVC   0(L'TACSCITY,R1),TACSCITY                                        
                                                                                
         CLI   TACSLEN,TACSLNQ     IF FILM/MUSIC/RECORD STATE IS SET            
         JL    XIT                                                              
         OC    TACSSTAT,XSPACES                                                 
         CLC   TACSSTAT,XSPACES                                                 
         BE    XIT                                                              
                                                                                
         LA    R1,L'TACSCITY+1(R1)                                              
XFRMS10  CLI   0(R1),C' '          FIND LAST SIGNIFICANT CHARACTER              
         BNE   XFRMS20                                                          
         SHI   R1,1                                                             
         B     XFRMS10                                                          
                                                                                
XFRMS20  MVI   1(R1),C','          THEN ADD COMMA AND STATE                     
         MVC   3(L'TACSSTAT,R1),TACSSTAT                                        
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO EXTRACT DATA FROM TACCD ELEMENT                       
         SPACE                                                                  
XCCEL    NTR1                                                                   
         GOTOR TRN2TACC,DMCB,AIO,AIO                                            
         JNE   *+8                                                              
         MVI   RERDSW,C'Y'                                                      
*                                                                               
         MVI   ELCODE,TACCELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   XCCELX                                                           
         USING TACCD,R4            R4=A(COMM CONTRACT ELEMENT)                  
*                                                                               
         ZIC   R3,TACCNCON         R3=(NUMBER OF CONTRACTS)                     
         CHI   R3,4                                                             
         BNH   *+8                                                              
         LHI   R3,4                                                             
         LA    R4,TACCCON          R4=A(FIRST CONTRACT)                         
         LA    R1,HCONTRCT         R1,=A(PRINT AREA)                            
         B     XCCEL15                                                          
*                                                                               
XCCEL10  MVI   0(R1),C','          NUMBER SEPERATOR                             
         LA    R1,1(R1)                                                         
*                                                                               
XCCEL15  MVC   0(L'TACCCON,R1),0(R4)                                            
         LA    R1,L'TACCCON(R1)                                                 
         LA    R4,L'TACCCON(R4)    PT TO NEXT CONTRACT NUMBER                   
         BCT   R3,XCCEL10                                                       
XCCELX   B     XIT                                                              
         DROP  R4                                                               
         SPACE 2                                                                
*              ROUTINE TO EXTRACT DATA FROM TACMD ELEMENT                       
         SPACE                                                                  
XCMEL    NTR1                                                                   
         XC    BLOCK,BLOCK         FAKE SCREEN HDR FOR COMMENT                  
         MVI   BLOCK,8+60                                                       
         GOTO1 CHAROUT,DMCB,TACMELQ,BLOCK,TACMTYPG                              
         BNE   *+10                                                             
         MVC   HCOMMENT,BLOCK+8                                                 
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO GET RULES (OVERRIDE EMPLOYER)                         
GETRULES NTR1                                                                   
         MVI   ELCODE,TABRELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   GETRULX                                                          
         USING TABRD,R4            R4=A(BILLING RULES ELEMENT)                  
         OC    TABROEOR,TABROEOR                                                
         BZ    *+10                                                             
         MVC   CUREMP,TABROEOR                                                  
GETRULX  B     XIT                                                              
         DROP  R4                                                               
         DROP  R2                                                               
         EJECT                                                                  
*              ROUTINE TO MARK ANY 'FROM' COMMERCIAL RECORDS                    
         SPACE                                                                  
ANYFROM  NTR1                                                                   
         L     R4,AIO              R4=A(COMMERCIAL RECORD)                      
         MVI   RERDSW,C'N'                                                      
*                                                                               
ANYFRM10 MVI   ELCODE,TAOCELQ      CHECK FOR OLD AGENCY/CID ELEMENT             
         BAS   RE,GETEL                                                         
         B     *+8                                                              
ANYFRM15 BAS   RE,NEXTEL                                                        
         BNE   ANYFRM30                                                         
*                                                                               
         USING TAOCD,R4            IF OLD STYLE CHANGE                          
         CLI   TAOCLEN,TAOCSTAT+L'TAOCSTAT-TAOCD                                
         BL    ANYFRM15                                                         
         TM    TAOCSTAT,TAOCSFRO                                                
         BZ    ANYFRM15                                                         
*                                                                               
         MVC   AIO,AIO1            SET IOAREA FOR READ                          
         MVI   RERDSW,C'Y'         SET TO RESTORE SYSIO'S KEY                   
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'B0',TAOCCOM)                             
         BNE   ANYFRM30                                                         
*                                                                               
         L     R4,AIO              MARK FROM COMMERCIAL DELETED TOO             
         USING TLCOD,R4                                                         
         OI    TLCOSTAT,TLCOSDEL                                                
         TM    CPOPTS,CPDELETE     IF MARKING FOR DELETION                      
         BZ    ANYFRM20                                                         
         GOTO1 PUTREC              WRITE BACK THE RECORD                        
ANYFRM20 GOTO1 MYTRACE                                                          
*                                                                               
         L     R1,CPCOUNT          ADD TO DELETE COUNT                          
         LA    R1,1(R1)                                                         
         ST    R1,CPCOUNT                                                       
         B     ANYFRM10                                                         
*                                                                               
ANYFRM30 CLI   RERDSW,C'Y'         IF NEED TO RESTORE SYSIO'S READ SEQ          
         BNE   ANYFRMX                                                          
         XC    KEY,KEY             SET KEY                                      
         MVC   KEY(L'TIKEY),TIKEY                                               
         GOTO1 HIGH                                                             
         MVC   AIO,TIAREC          AND IOAREA                                   
ANYFRMX  B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO PROCESS CAST RECORDS                                  
         SPACE                                                                  
         USING TLCAD,R4            R4=A(CAST RECORD)                            
PROCCAST NTR1                                                                   
         L     R1,ASPOOLD          R1=A(SPOOL DSECT)                            
         USING SPOOLD,R1                                                        
*                                                                               
         TM    FRSTIME,FRSTCST     IF FIRST CAST FOR COMML                      
         BZ    PROCST5                                                          
         NI    FRSTIME,X'FF'-FRSTCST TURN OFF FIRST TIME INDICTOR               
         MVI   PROCSW,PRSWCST        SET PROCESSING CAST                        
         B     PROCST8                                                          
*                                                                               
PROCST5  CLI   LINE,56             AND IF LINE IS 56 OR LATER                   
         BL    PROCST10                                                         
         BAS   RE,NEWPAGE          FORCE NEW PAGE                               
         OI    PROCSW,PRSWNOHD     PROCESSING WITH NO HEADINGS                  
*                                                                               
PROCST8  BAS   RE,NEWMID           FORCE MIDLINES TO PRINT                      
         DROP  R1                                                               
*                                                                               
PROCST10 LA    R2,XP+1             R2=A(PRINT LINE)                             
         USING PRINTD,R2                                                        
*                                                                               
         MVC   PCASSN,TLCASSN      S/S NUMBER                                   
         MVC   AIO,AIO1            READ W4 RECORD IN AIO1                       
         MVC   PCASNAME,LTNOTFND                                                
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'A8',PCASSN),0                             
                                                                                
         BRAS  RE,CVTPID           MAY NEED TO CONVERT SS# TO PID               
*                                  CONVERT SS# TO PID                           
         MVC   AIO,TIAREC                                                       
         BNE   *+10                                                             
         MVC   PCASNAME,TGNAME     S/S NAME                                     
*                                                                               
         MVC   PCACAT,TLCACAT      CATEGORY CODE                                
*                                                                               
         BAS   RE,XCAEL            EXTRACT DATA FROM TACAD                      
*                                                                               
         BAS   RE,PRNTIT           PRINT THE CAST MEMBER                        
*                                                                               
         XC    KEY,KEY             RESET SYSIO'S READ SEQUENCE                  
         MVC   KEY(L'TIKEY),TIKEY                                               
         GOTO1 HIGH                                                             
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              EXTRACT DATA FROM CAST DETAILS ELEMENT                           
*                                  NTRY   AIO1=A(W4 RECORD)                     
*                                         TIAREC=A(CAST RECORD)                 
         SPACE 1                                                                
XCAEL    NTR1                                                                   
         MVI   ELCODE,TACAELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   XCAELX                                                           
         USING TACAD,R4            R4=A(CAST DETAILS EL.)                       
         SPACE 1                                                                
         BAS   RE,SETCRP           SET CORP ID AND NAME                         
         MVC   PCACAM,TACAONOF     CAMERA (ON/OFF)                              
         MVC   PCATAX,TACAUNIT     TAX UNIT                                     
         BAS   RE,SETAGNT          AGENT CODE AND NAME                          
*                                                                               
         MVC   PCAUNI,TACAUN       UNION                                        
         MVC   PCALOCL,TACALOCL    LOCAL                                        
         MVC   PCAYEAR,TACAYEAR    YEAR                                         
         TM    TACASTAT,TACASTLO   IF ONLY ON LIFT                              
         BZ    *+12                                                             
         MVI   PCALIFT,C'O'                                                     
         B     XCAEL20                                                          
         TM    TACASTAT,TACASTLF   IF ON LIFT                                   
         BZ    *+8                                                              
         MVI   PCALIFT,C'Y'                                                     
*                                                                               
XCAEL20  MVC   PCAGUAR,TACAGUA     GUARANTEE CODE                               
*                                                                               
         BAS   RE,SETOV1           SET 1ST OVERSCALE PERCENT                    
         BAS   RE,SETOV2P          SET 2ND OVERSCALE PERCENT                    
*                                                                               
         CLI   COMMTYPE,CTYSOAP    IF SOAP COMMERCIAL                           
         BE    XCAEL40             (THIS IS WRITER PERCENTAGE)                  
         OC    TACAOV2,TACAOV2     2ND OVERSCALE PERCENT                        
         BZ    XCAEL40                                                          
         EDIT  TACAOV2,(6,PCAOV2),2,ALIGN=LEFT                                  
*                                                                               
XCAEL40  MVC   PCADBL,TACADBL      DOUBLES                                      
         OC    TACAFRST,TACAFRST   FIRST SERVICE DATE                           
         BZ    XCAELX                                                           
         GOTO1 DATCON,DMCB,(1,TACAFRST),(8,PCAFRST)                             
*                                                                               
XCAELX   B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO CORP ID AND NAME                                      
*                                  NTRY  AIO1=A(W4 RECORD)                      
         USING TACAD,R4            R4=A(CAST DETAILS ELEMENT)                   
SETCRP   NTR1                                                                   
         MVC   AIO,AIO1            SET A(W4 RECORD)                             
*                                                                               
         CLI   TACACORP,C'1'       IF CORP ID                                   
         BL    SETCRP10                                                         
         MVI   ELCODE,TATIELQ                                                   
         MVI   HALF,TATITYCO                                                    
         MVC   HALF+1(1),TACACORP                                               
         GOTO1 GETL,DMCB,(2,HALF)                                               
         BNE   SETCRPX                                                          
         L     R4,TGELEM           R4=A(TAX ID ELEMENT FOR CORP)                
         USING TATID,R4                                                         
         MVC   PCACORP,TATIID                                                   
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'8C',TATIID),0                             
         BNE   SETCRPX                                                          
         MVC   PCACNAME,TGNAME                                                  
         B     SETCRPX                                                          
*                                                                               
SETCRP10 L     R4,AIO                                                           
         MVI   ELCODE,TATIELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   SETCRPX                                                          
         MVC   PCACNAME,LTCRP      PERFORMER HAS CORP                           
*                                                                               
SETCRPX  MVC   AIO,TIAREC                                                       
         B     XIT                                                              
         DROP  R4                                                               
         SPACE 2                                                                
*              ROUTINE TO PRINT AGENT CODE AND NAME                             
         SPACE                                                                  
         USING TACAD,R4            R4=A(CAST DETAILS ELEMENT)                   
SETAGNT  NTR1                                                                   
         OC    TACANCDE,TACANCDE   AGENT CODE                                   
         BZ    SETAGNTX                                                         
         GOTO1 TRNSAGT,DMCB,(X'40',TACANCDE),PCAAGNT                            
*                                                                               
         MVC   AIO,AIO1                                                         
         GOTO1 RECVAL,DMCB,TLANCDQ,(X'8C',PCAAGNT),0                            
         MVC   AIO,TIAREC                                                       
         BNE   *+10                                                             
         MVC   PCAANAME,TGNAME     AGENT NAME                                   
*                                                                               
SETAGNTX B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO DISPLAY FIRST OVERSCALE PERCENTAGE                    
         SPACE                                                                  
SETOV1   NTR1                                                                   
         L     R4,AIO              R4=A(CAST RECORD)                            
         MVI   ELCODE,TAOAELQ      IF OVERSCALE AMOUNTS ELEMENT                 
         BAS   RE,GETEL                                                         
         BE    SETOV110                                                         
*                                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,TAOPELQ      OR IF OVERSCALE PERCENT ELEMENT              
         BAS   RE,GETEL                                                         
         BNE   SETOV1X                                                          
         USING TAOPD,R4                                                         
         CLI   TAOPNUM,1           AND NUMBER OF PERCENTAGES IS NOT 1           
         BNE   SETOV110                                                         
         CLC   TAOPUSE,=CL3' '     OR USE TYPE IS NOT ALL                       
         BE    *+14                                                             
SETOV110 MVC   PCAOV1(4),=C'****'  THEN DISPLAY ***                             
         B     SETOV1X                                                          
*                                                                               
         MVI   BYTE,0              FLAG FOR NOT % SCALE                         
         LA    RF,PCAOV1                                                        
         TM    TAOPPCT,X'80'       IS THIS A PERCENT SCALE?                     
         BNO   SETOV120                                                         
         NI    TAOPPCT,X'FF'-X'80' YES                                          
         MVI   0(RF),C'%'                                                       
         LA    RF,1(RF)                                                         
         MVI   BYTE,1              SET FLAG FOR % SCALE                         
SETOV120 OC    TAOPPCT,TAOPPCT                                                  
         BZ    SETOV1X                                                          
         CLI   BYTE,0                                                           
         BNE   SETOV130                                                         
         EDIT  TAOPPCT,(6,(RF)),2,ALIGN=LEFT                                    
         B     SETOV1X                                                          
SETOV130 EDIT  TAOPPCT,(5,(RF)),2,ALIGN=LEFT                                    
*                                                                               
SETOV1X  B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO DISPLAY SECOND OVERSCALE PERCENTAGE                   
         SPACE                                                                  
SETOV2P  NTR1                                                                   
         USING TAO2D,R4                                                         
         L     R4,AIO              R4=A(CAST RECORD)                            
         MVI   ELCODE,TAO2ELQ      IF OVERSCALE 2 PERCENT ELEMENT               
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         CLI   TAO2NUM,1           AND NUMBER OF PERCENTAGES IS NOT 1           
         BNE   SO2P10                                                           
         CLC   TAO2USE,=CL3' '     OR USE TYPE IS NOT ALL                       
         BE    SO2P20                                                           
SO2P10   MVC   PCAOV2(4),=C'****'  THEN DISPLAY ***                             
         B     XIT                                                              
                                                                                
SO2P20   MVI   BYTE,0              FLAG FOR NOT % SCALE                         
         LA    RF,PCAOV2                                                        
         TM    TAO2PCT,X'80'       IS THIS A PERCENT SCALE?                     
         BNO   SO2P30                                                           
         NI    TAO2PCT,X'FF'-X'80' YES                                          
         MVI   0(RF),C'%'                                                       
         LA    RF,1(RF)                                                         
         MVI   BYTE,1              SET FLAG FOR % SCALE                         
SO2P30   OC    TAO2PCT,TAO2PCT                                                  
         BZ    XIT                                                              
         CLI   BYTE,0                                                           
         BNE   SO2P40                                                           
         EDIT  TAO2PCT,(6,(RF)),2,ALIGN=LEFT                                    
         B     XIT                                                              
SO2P40   EDIT  TAO2PCT,(5,(RF)),2,ALIGN=LEFT                                    
         B     XIT                                                              
         DROP  R2,R4                                                            
         EJECT                                                                  
*              ROUTINE TO PROCESS HISTORY INVOICE RECORDS                       
         SPACE                                                                  
         USING TLIND,R4            R4=A(INVOICE RECORD)                         
PROCHINV NTR1                                                                   
         L     R1,ASPOOLD          R1=A(SPOOL DSECT)                            
         USING SPOOLD,R1                                                        
*                                                                               
         TM    FRSTIME,FRSTINV     IF FIRST INV FOR COMMERCIAL                  
         BZ    PROCHI5                                                          
         NI    FRSTIME,X'FF'-FRSTINV TURN OFF FIRST TIME INDICATOR              
         TM    FRSTIME,FRSTCST     IF CAST INFO PRINTED                         
         BO    *+8                                                              
         BAS   RE,BXBOT            CLOSE CAST BOX                               
         BAS   RE,PRNTIT           SKIP A LINE                                  
         MVI   PROCSW,PRSWINV      PROCESSING INVOICE HISTORY                   
         CLI   LINE,55             IF LINE IS 55 OR LATER                       
         BL    PROCHI8                                                          
         B     PROCHI7             FORCE NEW PAGE                               
*                                                                               
PROCHI5  CLI   LINE,55             IF LINE IS 55 OR LATER                       
         BL    PROCHI10                                                         
PROCHI7  BAS   RE,NEWPAGE          FORCE NEW PAGE                               
         OI    PROCSW,PRSWNOHD     PROCESSING WITH NO HEADLINE                  
*                                                                               
PROCHI8  BAS   RE,NEWMID           FORCE MIDLINES TO PRINT                      
         DROP  R1                                                               
*                                                                               
PROCHI10 LA    R2,XP+1             R2=A(PRINT LINE)                             
         USING PRINTD,R2                                                        
*                                                                               
         BAS   RE,XINEL            EXTRACT DATA FROM TAIND ELEMENT              
         BAS   RE,XBDEL            EXTRACT DATA FROM TABDD ELEMENT              
         BAS   RE,XPDEL            EXTRACT DATA FROM TAPDD ELEMENT              
*                                                                               
         MVC   PHCOM,XSPACES                                                    
         GOTO1 CHAROUT,DMCB,TACMELQ,0,TACMTYPH                                  
         BNE   *+10                                                             
         MVC   PHCOM,TGNAME        SET HISTORY COMMENT                          
*                                                                               
         CLI   PRTFLAG,C'Y'        IF OKAY TO PRINT (NOT SUB OR DUM)            
         BNE   *+12                                                             
         BAS   RE,PRNTIT           PRINT THE HISTORY LINE                       
         B     *+10                                                             
         MVC   XP,XSPACES          ELSE, CLEAR THE HISTORY LINE                 
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO EXTRACT DATA FROM TAIND ELEMENT                       
         SPACE                                                                  
XINEL    NTR1                                                                   
         MVI   ELCODE,TAINELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAIND,R4            R4=A(INVOICE DETAILS ELEMENT)                
*                                                                               
         OC    TAINPDTE,TAINPDTE   PAY DATE                                     
         BZ    XINEL10                                                          
         GOTO1 DATCON,DMCB,(1,TAINPDTE),(8,PHPDATE)                             
*                                                                               
XINEL10  MVI   BILLED,C'N'                                                      
         OC    TAINBDTE,TAINBDTE   IF BILLED DATE                               
         BZ    XINELX                                                           
         MVI   BILLED,C'Y'         SET BILLED INDICATOR                         
*                                                                               
XINELX   B     XIT                                                              
         DROP  R4                                                               
         SPACE 2                                                                
*              ROUTINE TO EXTRACT DATA FROM TABDD ELEMENT                       
         SPACE                                                                  
XBDEL    NTR1                                                                   
         MVI   ELCODE,TABDELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TABDD,R4            R4=A(BILLED DETAILS ELEMENT)                 
*                                                                               
         L     R3,TABDTAX          CALCULATE TNH                                
         A     R3,TABDHND                                                       
         A     R3,TABDHNDC                                                      
         A     R3,TABDFICR                                                      
         EDIT  (R3),(10,PHINVTNH),2,FLOAT=-,ZERO=NOBLANK                        
*                                                                               
         CLI   BILLED,C'Y'                                                      
         BNE   XBDELX                                                           
         EDIT  TABDTOT,(11,PHINVTOT),2,FLOAT=-,ZERO=NOBLANK                     
*                                                                               
XBDELX   B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO EXTRACT DATA FROM TAPDD ELEMENT                       
XPDEL    NTR1                                                                   
         MVI   ELCODE,TAPDELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAPDD,R4            R4=A(PAYMENT DETAILS ELEMENT)                
*                                                                               
         MVI   PRTFLAG,C'Y'        OKAY TO PRINT                                
         TM    TAPDOPT3,TAPDODUM   UNLESS DUMMY PAYMENTS                        
         BO    XPDEL5                                                           
         TM    TAPDSTA2,TAPDSSUB   UNLESS SUBSIDIARY PAYMENTS                   
         BZ    *+8                                                              
XPDEL5   MVI   PRTFLAG,C'N'                                                     
*                                                                               
         GOTO1 TINVCON,DMCB,TAPDINV,PHINV,DATCON    INVOICE NUMBER              
         OC    TAPDCYCS,TAPDCYCS   IF CYCLE DATES                               
         BZ    XPDEL10                                                          
         GOTO1 DATCON,DMCB,(X'11',TAPDCYCS),(8,PHCYC)                           
*                                                                               
XPDEL10  MVC   PHAPPLY,TAPDACDE    APPLY CODE                                   
         TM    TAPDSTAT,TAPDSLFT   LIFT PAYMENT                                 
         BZ    *+8                                                              
         MVI   PHLIFT,C'Y'                                                      
         TM    TAPDSTA2,TAPDSLFA   PAYMENT TO ALL ON COMML                      
         BZ    *+8                                                              
         MVI   PHLIFT,C'A'                                                      
         TM    TAPDPST1,TAPDPBNP   BILL-NO-PAYROLL PAYMENT                      
         BZ    *+8                                                              
         MVI   PHBNP,C'Y'                                                       
         TM    TAPDPST1,TAPDPCRD   CREDIT PAYMENT                               
         BZ    *+8                                                              
         MVI   PHCRD,C'Y'                                                       
*                                  PNH                                          
         EDIT  TAPDPNH,(9,PHINVPNH),2,FLOAT=-,ZERO=NOBLANK                      
*                                                                               
         L     R3,TAPDPAYI         WAGES                                        
         A     R3,TAPDPAYC                                                      
         A     R3,TAPDREXP                                                      
         EDIT  (R3),(11,PHINVPAY),2,FLOAT=-,ZERO=NOBLANK                        
*                                                                               
         GOTO1 USEVAL,DMCB,(X'20',TAPDUSE),TAPDTYPE                             
         BNE   *+12                                                             
         BAS   RE,SETDETS          SET PAYMENT TYPE                             
         B     XPDELX                                                           
*                                  VALIDATE USE W/O TYPE                        
         GOTO1 USEVAL,DMCB,(X'60',TAPDUSE)                                      
         BNE   *+10                                                             
         MVC   PHTYPE(L'TGUSNAME),TGUSNAME                                      
XPDELX   B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO DISPLAY THE PAYMENT USE DETAILS                       
         SPACE                                                                  
         USING TAPDD,R4            R4=A(PAYMENT DETAILS ELEMENT)                
SETDETS  NTR1                                                                   
         MVC   BLOCK(L'PHTYPE),XSPACES USE NAME AND TYPE                        
         MVC   BLOCK(L'TGUSNAME),TGUSNAME                                       
         OC    TAPDAREA(6),TAPDAREA    IF HAVE PRINT AREA AND USE               
         BZ    SETD2                                                            
         MVC   BLOCK+17(3),TAPDAREA    DISPLAY IT                               
         MVC   BLOCK+21(3),TAPDPUSE                                             
         B     SETD9                                                            
*                                                                               
SETD2    TM    TGUSTYST,USES                                                    
         BO    SETD8               BRANCH IF USES REQUIRED                      
         TM    TGUSTYST,MAJORS                                                  
         BO    SETD3               BRANCH IF MAJORS REQUIRED                    
         TM    TGUSTYST,UNITS                                                   
         BO    SETD5               BRANCH IF UNITS REQUIRED                     
         TM    TGUSTYST,INSERTS                                                 
         BO    SETD7               BRANCH IF INSERTS REQUIRED                   
         CLI   TGUSEQU,UTAG                                                     
         BE    SETD7C              BRANCH IF TAG PAYMENT                        
         B     SETDX10             ELSE GET OUT                                 
         SPACE                                                                  
SETD3    GOTO1 MAJVAL,DMCB,(X'80',TAPDMAJ) VALIDATE MAJORS                      
         BNE   *+10                                                             
         MVC   BLOCK+17(L'TGMACHAR),TGMACHAR                                    
SETD5    OC    TAPDUNIT,TAPDUNIT           IF THERE ARE UNITS                   
         BZ    SETD9                                                            
         LA    RF,BLOCK+17+L'TGMACHAR-1    FIND END OF MAJORS                   
         CLI   0(RF),X'40'                                                      
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         EDIT  TAPDUNIT,(5,2(RF)),ALIGN=LEFT NUMBER OF UNITS                    
         B     SETD9                                                            
         SPACE                                                                  
SETD7    OC    TAPDINS,TAPDINS   IF THERE ARE INSERTS                           
         BZ    SETD9                                                            
         LA    RF,BLOCK+17                                                      
         EDIT  TAPDINS,(5,2(RF)),ALIGN=LEFT NUMBER OF INSERTS                   
         B     SETD9                                                            
         SPACE                                                                  
SETD7C   OC    TAPDTAGS,TAPDTAGS IF THERE ARE TAGS                              
         BZ    SETD9                                                            
         LA    RF,BLOCK+17                                                      
         EDIT  TAPDTAGS,(3,2(RF)),ALIGN=LEFT NUMBER OF INSERTS                  
         B     SETD9                                                            
         SPACE                                                                  
         USING TANDD,R4                                                         
SETD8    MVI   ELCODE,TANDELQ      GET NETWORK/CLASS A DETAILS ELEMENT          
         L     R4,TIAREC                                                        
         BAS   RE,GETEL                                                         
         BNE   SETDX10                                                          
         SR    R3,R3                                                            
         ICM   R3,3,TANDSTUS       FIND TOTAL START USE NUMBER                  
         BNZ   *+8                                                              
         AHI   R3,1                + 1 IF 0 FROM CONVERSION                     
         SR    R1,R1                                                            
         ICM   R1,3,TANDSTUL                                                    
         BNZ   *+8                                                              
         AHI   R1,1                                                             
         AR    R3,R1                                                            
         BCTR  R3,0                R3=TOTAL START USE NUMBER                    
         LA    R1,BLOCK+17                                                      
         EDIT  (R3),(5,(R1)),ALIGN=LEFT                                         
         LA    RF,BLOCK+4+17       FIND END OF START USE NUMBER                 
         CLI   0(RF),X'40'                                                      
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'-'                                                       
         AH    R3,TANDUSES         FIND TOTAL END USE NUMBER                    
         AH    R3,TANDUSEL                                                      
         BCTR  R3,0                                                             
         EDIT  (R3),(5,2(RF)),ALIGN=LEFT                                        
         SPACE                                                                  
         OC    TANDUSEL,TANDUSEL   IF THERE ARE LIFT USES                       
         BZ    SETD9                                                            
         MVI   BLOCK+10+17,C'L'    SHOW LIFT USES                               
         LA    R1,BLOCK+11+17                                                   
         EDIT  (2,TANDSTUL),(5,(R1)),ALIGN=LEFT  LIFT START USE NUMBER          
         LA    RF,BLOCK+11+4+17      FIND END OF LIFT START USE NUMBER          
         CLI   0(RF),X'40'                                                      
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'-'                                                       
         LH    R3,TANDSTUL         FIND LIFT END USE NUMBER                     
         AH    R3,TANDUSEL                                                      
         BCTR  R3,0                                                             
         EDIT  (R3),(5,2(RF)),ALIGN=LEFT                                        
SETD9    GOTO1 SQUASHER,DMCB,BLOCK,L'BLOCK SQUASH IT                            
SETDX10  MVC   PHTYPE,BLOCK     MOVE PRINT LINE                                 
SETDXX   B     XIT                                                              
         DROP  R2,R4                                                            
         EJECT                                                                  
*              ROUTINE TO PRINT A LINE                                          
         SPACE 1                                                                
PRNTIT   NTR1                                                                   
         TM    CPOPTS,CPSUMM       IF SUMMARY OPTION                            
         BO    PRNTITX             DON'T PRINT                                  
*                                                                               
         LA    R2,XP               POINT TO LINE PRINTING                       
         LA    R1,L'XP                                                          
PRNTITA  CLI   0(R2),0             MAKE SURE NO NULLS                           
         BNE   *+8                                                              
         MVI   0(R2),C' '                                                       
         LA    R2,1(R2)                                                         
         BCT   R1,PRNTITA                                                       
*                                                                               
         L     R1,ASPOOLD          R1=A(SPOOL DSECT)                            
         USING SPOOLD,R1                                                        
         XR    R2,R2                                                            
         TM    PROCSW,PRSWCOM      IF PROCESSING COMMERCIALS                    
         BZ    *+8                                                              
         AHI   R2,8                ADD 8                                        
         TM    PROCSW,PRSWINV      IF PROCESSING HISTORY                        
         BZ    *+8                                                              
         AHI   R2,1                ADD ONE                                      
         TM    CPOPTS,CPNOBOX      IF NOBOX OPTION (USING DASHES)               
         BZ    *+8                                                              
         AHI   R2,2                ADD TWO                                      
         TM    PROCSW,PRSWNOHD     IF NO HEADINGS                               
         BZ    *+8                                                              
         AHI   R2,4                ADD FOUR                                     
         STC   R2,RCSUBPRG                                                      
         GOTO1 SPOOL,DMCB,ASPOOLD                                               
*                                                                               
PRNTITX  B     XIT                                                              
         DROP  R1                                                               
         SPACE 2                                                                
*              ROUTINE TO PRINT A LINE REGARDLESS                               
         SPACE 1                                                                
PRNTIT2  NTR1                                                                   
         GOTO1 SPOOL,DMCB,ASPOOLD                                               
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO SKIP TO NEW PAGE                                      
         SPACE 1                                                                
NEWPAGE  NTR1                                                                   
         L     R1,ASPOOLD          R1=A(SPOOL DSECT)                            
         USING SPOOLD,R1                                                        
         MVI   FORCEHED,C'Y'       FORCE NEW PAGE                               
         B     XIT                                                              
         DROP  R1                                                               
         SPACE 2                                                                
*              ROUTINE TO FORCE NEW MIDLINE                                     
         SPACE 1                                                                
NEWMID   NTR1                                                                   
         L     R1,ASPOOLD          R1=A(SPOOL DSECT)                            
         USING SPOOLD,R1                                                        
         MVI   FORCEMID,C'Y'       FORCE NEW MIDLINE                            
         B     XIT                                                              
         DROP  R1                                                               
         SPACE 2                                                                
*              ROUTINE TO TRACE RECORDS                                         
         SPACE 1                                                                
MYTRACE  NTR1                                                                   
         TM    CPOPTS,CPTRACE      TEST TRACE ENABLED                           
         BZ    MYTRACEX                                                         
         GOTO1 TRACE,DMCB,AIO,0,0,0                                             
*                                                                               
MYTRACEX B     XIT                                                              
         EJECT                                                                  
*              HEADLINE HOOK (HEADHOOK)                                         
         SPACE 1                                                                
HOOK     NTR1                                                                   
         L     R1,ASPOOLD          R1=A(SPOOL DSECT)                            
         USING SPOOLD,R1                                                        
*                                                                               
         CLI   RECNUM,CP           IF COHIST REPORT                             
         BE    *+10                                                             
         MVC   XHEAD1+73(18),=CL18'Commercial History'                          
*                                                                               
         MVC   XHEAD3+74(17),CPPERIOD     REQUESTED PERIOD                      
*                                                                               
         TM    CPOPTS,CPNOBOX      IF NOBOXES REQUESTED                         
         BZ    *+12                                                             
         NI    SPOOLIND,X'FF'-SPNSPACE RESET SPACING                            
         B     HOOKX                                                            
*                                                                               
         L     R3,MYABOX           R3=A(BOXES)                                  
         USING BOXD,R3                                                          
         MVI   BOXYORN,C'Y'        INTIALIZE REMAINING FIELDS                   
         MVI   BOXWT,1                                                          
         MVI   BOXOFF,0                                                         
         MVI   BOXINIT,0                                                        
*                                                                               
HOOKX    B     XIT                                                              
         DROP  R1,R3                                                            
         SPACE 2                                                                
*              MIDLINE HOOK (MIDHOOK)                                           
         SPACE 1                                                                
MHOOK    NTR1                                                                   
         L     R1,ASPOOLD          R1=A(SPOOL DSECT)                            
         USING SPOOLD,R1                                                        
*                                                                               
         TM    CPOPTS,CPNOBOX      IF NO BOXES REQUESTED                        
         BZ    *+12                                                             
         OI    SPOOLIND,SPNSPACE   SET NO BLANK LINES AFTER MIDLINES            
         B     MHKX                                                             
*                                                                               
         TM    PROCSW,PRSWCOM      IF PROCESSING COMMERCIALS                    
         BO    MHKX                                                             
         TM    PROCSW,PRSWCST      IF PROCESSING CAST                           
         BZ    *+12                                                             
         LA    R2,PCALNQ+1         LENGTH OF CAST BOX                           
         B     MHK40                                                            
*                                                                               
         TM    PROCSW,PRSWINV      IF PROCESSING INVOICE HISTORY                
         BZ    MHKX                                                             
         LA    R2,PHLNQ+1          LENGTH OF HISTORY BOX                        
*                                                                               
MHK40    GOTO1 BXTOP,DMCB,0,(R2),2  OPEN THE BOX                                
*                                                                               
MHKX     B     XIT                                                              
         DROP  R1                                                               
         EJECT                                                                  
*              ROUTINE TO POP IN A BOXES TOP                                    
*                                                                               
*              P1 = BOXCOLS + X LHS                                             
*              P2 = BOXCOLS + X RHS                                             
*              P3 = MIDLNS                                                      
*                                                                               
         SPACE 1                                                                
BXTOP    NTR1                                                                   
         L     R3,MYABOX           R3=A(BOXES)                                  
         USING BOXD,R3                                                          
         L     R4,0(R1)            LHS                                          
         L     RE,4(R1)            RHS                                          
         L     R2,8(R1)            MID LINES                                    
         L     R1,ASPOOLD          R1=A(SPOOL DSECT)                            
         USING SPOOLD,R1                                                        
         MVC   BOXROWS,XSPACES                                                  
         MVC   BOXCOLS,XSPACES                                                  
         MVC   BOXCOLSR,XSPACES                                                 
         ZIC   RF,LINE                                                          
         LA    RF,BOXROWS-1(RF)                                                 
         MVI   0(RF),C'T'                                                       
         LTR   R2,R2               DO WE WANT A MIDDLE LINE                     
         BZ    BXT10                                                            
         AR    RF,R2               NUMBER OF LINES TO MIDDLE LINE               
         MVI   0(RF),C'M'                                                       
*                                                                               
BXT10    LA    R4,BOXCOLS(R4)                                                   
         MVI   0(R4),C'L'          LEFT CORNER                                  
         LA    RE,BOXCOLS(RE)                                                   
         MVI   0(RE),C'R'          RIGHT CORNER                                 
*                                                                               
BXT30    BAS   RE,BXCOL                                                         
         MVI   BOXYORN,C'Y'        INTIALIZE REMAINING FIELDS                   
         MVI   BOXWT,1                                                          
         MVI   BOXOFF,0                                                         
         MVI   BOXINIT,0                                                        
         B     XIT                                                              
         DROP  R1                                                               
         EJECT                                                                  
*              ROUTINE TO DRAW CENTER BOX LINES                                 
         SPACE                                                                  
BXCOL    NTR1                                                                   
         LA    R2,BOXCOLS+1                                                     
         USING PRINTD,R2                                                        
         TM    PROCSW,PRSWCST      IF CAST BOX                                  
         BZ    BXCOL10                                                          
         MVI   PCA1,C'C'                                                        
         MVI   PCA2,C'C'                                                        
         MVI   PCA3,C'C'                                                        
         MVI   PCA4,C'C'                                                        
         MVI   PCA5,C'C'                                                        
         MVI   PCA6,C'C'                                                        
         MVI   PCA7,C'C'                                                        
         MVI   PCA8,C'C'                                                        
         MVI   PCA9,C'C'                                                        
         MVI   PCA10,C'C'                                                       
         MVI   PCA11,C'C'                                                       
         MVI   PCA12,C'C'                                                       
         MVI   PCA13,C'C'                                                       
         MVI   PCA14,C'C'                                                       
         MVI   PCA15,C'C'                                                       
         MVI   PCA16,C'C'                                                       
         B     BXCOLX                                                           
*                                                                               
BXCOL10  MVI   PH1,C'C'            ELSE HISTORY INVOICE BOX                     
         MVI   PH2,C'C'                                                         
         MVI   PH3,C'C'                                                         
         MVI   PH4,C'C'                                                         
         MVI   PH5,C'C'                                                         
         MVI   PH6,C'C'                                                         
         MVI   PH7,C'C'                                                         
         MVI   PH8,C'C'                                                         
         MVI   PH9,C'C'                                                         
         MVI   PH10,C'C'                                                        
         MVI   PH11,C'C'                                                        
         MVI   PH12,C'C'                                                        
BXCOLX   B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*              ROUTINE TO POP IN A BOTTOM AT END OF REPORT                      
         SPACE 1                                                                
BXBOT    NTR1                                                                   
         TM    CPOPTS,CPNOBOX      IF BOXES                                     
         BO    BXBOTX                                                           
*                                                                               
         L     R3,MYABOX           R3=A(BOXES)                                  
         USING BOXD,R3                                                          
         L     R1,ASPOOLD          R1=A(SPOOL DSECT)                            
         USING SPOOLD,R1                                                        
         MVC   BOXROWS,XSPACES                                                  
         ZIC   RF,LINE                                                          
         LA    RF,BOXROWS-1(RF)                                                 
         MVI   0(RF),C'B'                                                       
         MVI   BOXINIT,0                                                        
         BAS   RE,PRNTIT                                                        
*                                                                               
BXBOTX   B     XIT                                                              
         DROP  R3,R1                                                            
         EJECT                                                                  
*              ERRORS, EXITS                                                    
         SPACE 2                                                                
CLIMISS  LA    R2,SCHCLIH          R2=A(CLIENT FIELD)                           
         B     FLDMISS                                                          
COMMISS  LA    R2,SCHCOMH          R2=A(COMMERCIAL FIELD)                       
FLDMISS  MVI   ERROR,MISSING       MISSING INPUT FIELD                          
         B     THEEND                                                           
         SPACE 1                                                                
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
YES      SR    RC,RC               SET CONDITION CODES                          
NO       LTR   RC,RC                                                            
         SPACE 1                                                                
XIT      XIT1                                                                   
         SPACE 3                                                                
         GETEL R4,DATADISP,ELCODE                                               
         SPACE 3                                                                
*              CONSTANTS, ETC.                                                  
         SPACE 1                                                                
LTNOTFND DC    CL36'*NOT FOUND*'                                                
LTCRP    DC    CL36'*PERFORMER HAS CORP*'                                       
         SPACE 1                                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              REPORT SPECS                                                     
         SPACE 1                                                                
MYSPECS3 DS    0H                                                               
         SPROG 0,1,2,3,4,5,6,7,8,10                                             
         SSPEC H1,2,RUN                                                         
         WSPEC H1,138,REPORT                                                    
         WSPEC H1,150,PAGE                                                      
         WSPEC H2,138,REQUESTOR                                                 
*                                                                               
         WSPEC H1,74,C'Purged Commercials'                                      
*                                                                               
         SPROG 0,1,4,5,7                                                        
         WSPEC H2,74,18X'BF'                                                    
         SPROG 2,3,6,7,10                                                       
         WSPEC H2,74,18C'-'                                                     
*                                                                               
         SPROG 0,1,2,3,8,10                                                     
         WSPEC H5,2,C'Agency'                                                   
         WSPEC H5,58,C'Film Date'                                               
         WSPEC H5,83,C'Film Studio'                                             
         WSPEC H5,110,C'City/State'                                             
*                                                                               
         WSPEC H6,2,C'Client'                                                   
         WSPEC H6,58,C'Recording Date'                                          
         WSPEC H6,83,C'Recd Studio'                                             
         WSPEC H6,110,C'City/State'                                             
*                                                                               
         WSPEC H7,2,C'Prod'                                                     
         WSPEC H7,58,C'Music Date'                                              
         WSPEC H7,83,C'Music Studio'                                            
         WSPEC H7,110,C'City/State'                                             
*                                                                               
         WSPEC H8,2,C'CID'                                                      
         WSPEC H8,58,C'Addendum St'                                             
         WSPEC H8,83,C'Lst Pay Date'                                            
         WSPEC H8,110,C'Employer'                                               
*                                                                               
         WSPEC H9,02,C'Media'                                                   
         WSPEC H9,16,C'Len'                                                     
         WSPEC H9,26,C'Lift Id'                                                 
         WSPEC H9,47,C'Len'                                                     
         WSPEC H9,58,C'Comml Type'                                              
         WSPEC H9,83,C'AFM Rate'                                                
         WSPEC H9,110,C'Dub Date'                                               
*                                                                               
         WSPEC H10,2,C'1st Fixed Cyc'                                           
         WSPEC H10,26,C'1st Air'                                                
         WSPEC H10,58,C'Expires'                                                
         WSPEC H10,83,C'AFM Title'                                              
*                                                                               
         WSPEC H11,02,C'Comment'                                                
         WSPEC H11,83,C'Contrct Nums'                                           
*                                  BOXES W/ AND W/O HEADINGS                    
         SPROG 0,4                                                              
         WSPEC M2,2,C'S/S Numb  Name'                                           
         WSPEC M2,38,C'Cat Cam Tax Agnt Name'                                   
         WSPEC M2,81,C'Uni/Lcl Yr Lft Corp Id   Name'                           
         WSPEC M2,132,C'Ov1%   Ov2%   Guar Db 1st Serv'                         
*                                  DASHES W/ AND W/O HEADINGS                   
         SPROG 2,6                                                              
         WSPEC M1,2,C'S/S Numb  Name'                                           
         WSPEC M1,38,C'Cat Cam Tax Agnt Name'                                   
         WSPEC M1,81,C'Uni/Lcl Yr Lft Corp Id   Name'                           
         WSPEC M1,132,C'Ov1%   Ov2%   Guar Db 1st Serv'                         
*                                                                               
         SPROG 2,6                                                              
         WSPEC M2,2,C'--------  ----'                                           
         WSPEC M2,38,C'--- --- --- ---- ----'                                   
         WSPEC M2,81,C'------- -- --- -------   ----'                           
         WSPEC M2,132,C'----   ----   ---- -- --------'                         
*                                  BOXES W/ AND W/O HEADINGS                    
         SPROG 1,5                                                              
         WSPEC M2,2,C'Inv No Cycle Dates       Payment Type'                    
         WSPEC M2,63,C'Pay Date Ap Lft BNP Cr       Wages'                      
         WSPEC M2,98,C'      PNH        TNH'                                    
         WSPEC M2,119,C'Invoice Tot Comment'                                    
*                                  DASHES W/ AND W/O HEADINGS                   
         SPROG 3,7                                                              
         WSPEC M1,2,C'Inv No Cycle Dates       Payment Type'                    
         WSPEC M1,63,C'Pay Date Ap Lft BNP Cr       Wages'                      
         WSPEC M1,98,C'      PNH        TNH'                                    
         WSPEC M1,119,C'Invoice Tot Comment'                                    
*                                                                               
         SPROG 3,7                                                              
         WSPEC M2,2,C'------ -----------       ------------'                    
         WSPEC M2,63,C'-------- -- --- --- --       -----'                      
         WSPEC M2,98,C'      ---        ---'                                    
         WSPEC M2,119,C'----------- -------'                                    
         DC    X'00'                                                            
                                                                                
MYSPECS4 DS    0H                                                               
         SPROG 0,1,2,3,4,5,6,7,8,10                                             
         SSPEC H1,2,RUN                                                         
         WSPEC H1,138,REPORT                                                    
         WSPEC H1,150,PAGE                                                      
         WSPEC H2,138,REQUESTOR                                                 
*                                                                               
         WSPEC H1,74,C'Purged Commercials'                                      
*                                                                               
         SPROG 0,1,4,5,7                                                        
         WSPEC H2,74,18X'BF'                                                    
         SPROG 2,3,6,7,10                                                       
         WSPEC H2,74,18C'-'                                                     
*                                                                               
         SPROG 0,1,2,3,8,10                                                     
         WSPEC H5,2,C'Agency'                                                   
         WSPEC H5,58,C'Film Date'                                               
         WSPEC H5,83,C'Film Studio'                                             
         WSPEC H5,110,C'City/State'                                             
*                                                                               
         WSPEC H6,2,C'Client'                                                   
         WSPEC H6,58,C'Recording Date'                                          
         WSPEC H6,83,C'Recd Studio'                                             
         WSPEC H6,110,C'City/State'                                             
*                                                                               
         WSPEC H7,2,C'Prod'                                                     
         WSPEC H7,58,C'Music Date'                                              
         WSPEC H7,83,C'Music Studio'                                            
         WSPEC H7,110,C'City/State'                                             
*                                                                               
         WSPEC H8,2,C'CID'                                                      
         WSPEC H8,58,C'Addendum St'                                             
         WSPEC H8,83,C'Lst Pay Date'                                            
         WSPEC H8,110,C'Employer'                                               
*                                                                               
         WSPEC H9,02,C'Media'                                                   
         WSPEC H9,16,C'Len'                                                     
         WSPEC H9,26,C'Lift Id'                                                 
         WSPEC H9,47,C'Len'                                                     
         WSPEC H9,58,C'Comml Type'                                              
         WSPEC H9,83,C'AFM Rate'                                                
         WSPEC H9,110,C'Dub Date'                                               
*                                                                               
         WSPEC H10,2,C'1st Fixed Cyc'                                           
         WSPEC H10,26,C'1st Air'                                                
         WSPEC H10,58,C'Expires'                                                
         WSPEC H10,83,C'AFM Title'                                              
*                                                                               
         WSPEC H11,02,C'Comment'                                                
         WSPEC H11,83,C'Contrct Nums'                                           
*                                  BOXES W/ AND W/O HEADINGS                    
         SPROG 0,4                                                              
         WSPEC M2,2,C'PID       Name'                                           
         WSPEC M2,38,C'Cat Cam Tax Agnt Name'                                   
         WSPEC M2,81,C'Uni/Lcl Yr Lft Corp Id   Name'                           
         WSPEC M2,132,C'Ov1%   Ov2%   Guar Db 1st Serv'                         
*                                  DASHES W/ AND W/O HEADINGS                   
         SPROG 2,6                                                              
         WSPEC M1,2,C'PID       Name'                                           
         WSPEC M1,38,C'Cat Cam Tax Agnt Name'                                   
         WSPEC M1,81,C'Uni/Lcl Yr Lft Corp Id   Name'                           
         WSPEC M1,132,C'Ov1%   Ov2%   Guar Db 1st Serv'                         
*                                                                               
         SPROG 2,6                                                              
         WSPEC M2,2,C'--------  ----'                                           
         WSPEC M2,38,C'--- --- --- ---- ----'                                   
         WSPEC M2,81,C'------- -- --- -------   ----'                           
         WSPEC M2,132,C'----   ----   ---- -- --------'                         
*                                  BOXES W/ AND W/O HEADINGS                    
         SPROG 1,5                                                              
         WSPEC M2,2,C'Inv No Cycle Dates       Payment Type'                    
         WSPEC M2,63,C'Pay Date Ap Lft BNP Cr       Wages'                      
         WSPEC M2,98,C'      PNH        TNH'                                    
         WSPEC M2,119,C'Invoice Tot Comment'                                    
*                                  DASHES W/ AND W/O HEADINGS                   
         SPROG 3,7                                                              
         WSPEC M1,2,C'Inv No Cycle Dates       Payment Type'                    
         WSPEC M1,63,C'Pay Date Ap Lft BNP Cr       Wages'                      
         WSPEC M1,98,C'      PNH        TNH'                                    
         WSPEC M1,119,C'Invoice Tot Comment'                                    
*                                                                               
         SPROG 3,7                                                              
         WSPEC M2,2,C'------ -----------       ------------'                    
         WSPEC M2,63,C'-------- -- --- --- --       -----'                      
         WSPEC M2,98,C'      ---        ---'                                    
         WSPEC M2,119,C'----------- -------'                                    
         DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
*        ROUTINE CONVERTS SSN TO PID                                  *         
*        ON ENTRY ... R2=A(PRINT LINE)                                *         
***********************************************************************         
                                                                                
         USING PRINTD,R2                                                        
CVTPID   NTR1  BASE=*,LABEL=*                                                   
         TM    CPOPTS,CPPID        IF PRINTING PIDS                             
         JZ    XIT                                                              
         GOTO1 SSNPACK,DMCB,PCASSN,DUB                                          
         MVC   PCASSN,=9C' '                                                    
         MVC   PCASSN(6),DUB       COVERT SS# TO PID                            
         J     XIT                                                              
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAT2TACC                                                       
*              DSECT TO COVER LOCAL WORKING STORAGE                             
         SPACE 1                                                                
WORKD    DSECT                                                                  
         DS    0A                                                               
MYABOX   DS    A                   A(BOXES)                                     
MYSORTER DS    A                   A(SORTER)                                    
*                                                                               
CPCOUNT  DS    F                   COMMERCIAL DELETE COUNT                      
CPECNT   DS    F                   COMMERCIAL ERROR COUNT                       
CPPAGES  DS    H                   NUMBER OF PAGES PRINTED/SPOOLED              
*                                                                               
CPOPTS   DS    XL1                 OPTIONS                                      
CPTRACE  EQU   X'80'               TRACE ACTIVE                                 
CPDELETE EQU   X'40'               DELETE THE CHECKS                            
CPNOBOX  EQU   X'20'               NO BOXES                                     
CPSUMM   EQU   X'10'               PRINT COMMERCIALS MARKED TOTAL               
CPPID    EQU   X'08'               PRINT PID INSTEAD OF SSN                     
*                                                                               
SVAGY    DS    CL(L'TGAGY)         SAVED AGENCY                                 
SVAGYNM  DS    CL(L'TGNAME)        SAVED AGENCY NAME                            
SVCLI    DS    CL(L'TGCLI)         SAVED CLIENT                                 
SVCLINM  DS    CL(L'TGNAME)        SAVED CLIENT NAME                            
SVEMP    DS    CL(L'TGEMP)         SAVED EMPLOYER                               
SVEMPNM  DS    CL(L'TGNAME)        SAVED EMPLOYER NAME                          
SVPRD    DS    CL(L'TGPRD)         SAVED PRODUCT                                
SVPRDNM  DS    CL(L'TGNAME)        SAVED PRODUCT NAME                           
*                                                                               
CUREMP   DS    CL(L'TGEMP)         CURRENT EMPLOYER CODE                        
*                                                                               
COMMTYPE DS    CL1                 COMMERCIAL TYPE                              
PRTFLAG  DS    CL1                 N=DON'T PRINT                                
CHECKS   DS    CL1                 Y=CHECKS ATTACHED TO COMMERCIAL              
BILLED   DS    CL1                 Y=INVOICE BILLED                             
RERDSW   DS    CL1                 Y=REREAD SYSIOS KEY                          
FRST     DS    CL1                 Y=FIRST TIME IN                              
FRSTIME  DS    XL1                 FIRST TIME INDICATOR                         
FRSTCST  EQU   X'80'               FIRST CAST FOR COMML                         
FRSTINV  EQU   X'40'               FIRST HISTORY INVOICE FOR COMML              
PROCSW   DS    XL1                 PROCESSING SWITCH                            
PRSWCOM  EQU   X'80'               PROCESSING COMMERCIALS                       
PRSWCST  EQU   X'40'               PROCESSING CAST                              
PRSWINV  EQU   X'20'               PROCESSING INVOICE HISTORY                   
PRSWNOHD EQU   X'10'               PROCESSING WITHOUT HEADLINES                 
*                                                                               
CPPERIOD DS    CL17                DISPLAYABLE REQUEST PERIOD                   
CPPSTA   DS    PL3                 PACKED START DATE OF PERIOD                  
CPPEND   DS    PL3                 PACKED END DATE OF PERIOD                    
*                                                                               
WORKLNQ  EQU   *-WORKD                                                          
         EJECT                                                                  
*              DSECT TO COVER HEADLINE                                          
         SPACE 1                                                                
HEADD    DSECT                                                                  
*                                                                               
HCOMML   DS    0CL165              COMMERCIAL HEADLINE                          
         DS    CL8                 'AGENCY'                                     
HAGENCY  DS    CL6                                                              
         DS    CL7                                                              
HAGYNAME DS    CL36                                                             
         DS    CL15                'FILM DATE'                                  
HFLMDT   DS    CL8                                                              
         DS    CL15                'FILM STUDIO'                                
HFLMSTUD DS    CL12                                                             
         DS    CL13                'FILM CITY'                                  
HFLMCITY DS    CL16                                                             
*                                                                               
         ORG   HCOMML+198                                                       
H2LINE   DS    CL8                 'CLIENT'                                     
HCLIENT  DS    CL6                                                              
         DS    CL7                                                              
HCLINAME DS    CL36                                                             
         DS    CL15                'RECORDING DATE'                             
HRCDDT   DS    CL8                                                              
         DS    CL15                'RECD STUDIO'                                
HRCDSTUD DS    CL12                                                             
         DS    CL13                'RECD CITY'                                  
HRCDCITY DS    CL16                                                             
*                                                                               
         ORG   H2LINE+198                                                       
H3LINE   DS    CL8                 'PRODUCT'                                    
HPRODUCT DS    CL6                                                              
         DS    CL7                                                              
HPRDNAME DS    CL36                                                             
         DS    CL15                'MUSIC DATE'                                 
HMSCDT   DS    CL8                                                              
         DS    CL15                'MUSIC STUDIO'                               
HMSCSTUD DS    CL12                                                             
         DS    CL13                'MUSIC CITY'                                 
HMSCCITY DS    CL16                                                             
*                                                                               
         ORG   H3LINE+198                                                       
H4LINE   DS    CL8                 'CID'                                        
HCID     DS    CL12                                                             
         DS    CL1                                                              
HTITLE   DS    CL36                                                             
         DS    CL15                'ADDENDUM ST'                                
HADDSTNM DS    CL7                                                              
         DS    CL16                'LST PAY DATE'                               
HPAYDATE DS    CL8                                                              
         DS    CL17                'EMPLOYER'                                   
HEMP     DS    CL3                                                              
         DS    CL1                                                              
HEMPNAME DS    CL36                                                             
*                                                                               
         ORG   H4LINE+198                                                       
H5LINE   DS    CL8                 'MEDIA'                                      
HMEDIANM DS    CL5                                                              
         DS    CL6                 'LEN'                                        
HLEN     DS    CL3                                                              
         DS    CL11                'LIFT ID'                                    
HLIFTID  DS    CL12                                                             
         DS    CL6                 'LEN'                                        
HLFTLEN  DS    CL3                                                              
         DS    CL18                'COMML TYPE'                                 
HCTYPENM DS    CL7                                                              
         DS    CL16                'AFM RATE'                                   
HAFMRATE DS    CL1                                                              
         DS    CL24                'DUB DATE'                                   
HAFMDATE DS    CL8                                                              
*                                                                               
         ORG   H5LINE+198                                                       
H6LINE   DS    CL15                '1st FIXED CYCLE'                            
HFCYCLE  DS    CL8                                                              
         DS    CL10                'FIRST AIR'                                  
HFAIR    DS    CL8                                                              
         DS    CL31                'EXPIRES'                                    
HEXPIRE  DS    CL8                                                              
         DS    CL15                'AFM TITLE'                                  
HAFMTTL  DS    CL36                                                             
*                                                                               
         ORG   H6LINE+198                                                       
H7LINE   DS    0CL198                                                           
         DS    CL15                'COMMENT'                                    
HCOMMENT DS    CL60                                                             
         DS    CL20                'CONTRACT NO'S'                              
HCONTRCT DS    CL51                                                             
         EJECT                                                                  
*              DSECT TO COVER PRINT LINE                                        
         SPACE 1                                                                
PRINTD   DSECT                                                                  
*                                                                               
PCAST    DS    0CL160              PRINT CAST LINE                              
PCASSN   DS    CL9                 S/S NUMBER                                   
PCA1     DS    CL1                                                              
PCASNAME DS    CL25                S/S NAME                                     
PCA2     DS    CL1                                                              
PCACAT   DS    CL3                 CATEGORY                                     
PCA3     DS    CL1                                                              
PCACAM   DS    CL3                 CAMERA                                       
PCA4     DS    CL1                                                              
PCATAX   DS    CL3                 TAX UNIT                                     
PCA5     DS    CL1                                                              
PCAAGNT  DS    CL4                 AGENT                                        
PCA6     DS    CL1                                                              
PCAANAME DS    CL25                AGENT NAME                                   
PCA7     DS    CL1                                                              
PCAUNI   DS    CL3                 UNION                                        
         DS    CL1                                                              
PCALOCL  DS    CL3                 LOCAL                                        
PCA8     DS    CL1                                                              
PCAYEAR  DS    CL2                 YEAR                                         
PCA9     DS    CL1                                                              
         DS    CL1                                                              
PCALIFT  DS    CL1                 LIFT                                         
         DS    CL1                                                              
PCA10    DS    CL1                                                              
PCACORP  DS    CL9                 CORP ID NUMBER                               
PCA11    DS    CL1                                                              
PCACNAME DS    CL25                CORP NAME                                    
PCA12    DS    CL1                                                              
PCAOV1   DS    CL6                 OVERSCALE 1 PERCENTAGE                       
PCA13    DS    CL1                                                              
PCAOV2   DS    CL6                 OVERSCALE 2 PERCENTAGE                       
PCA14    DS    CL1                                                              
PCAGUAR  DS    CL4                 GUARANTEE                                    
PCA15    DS    CL1                                                              
PCADBL   DS    CL1                 DOUBLES                                      
         DS    CL1                                                              
PCA16    DS    CL1                                                              
PCAFRST  DS    CL8                 1ST SERVICE                                  
PCALNQ   EQU   *-PCAST             LENGTH OF CAST LINE                          
         EJECT                                                                  
         ORG   PRINTD                                                           
PHISTRY  DS    0CL160              HISTORY LINE                                 
PHINV    DS    CL6                 INVOICE NUMBER                               
PH1      DS    CL1                                                              
PHCYC    DS    CL17                CYCLE DATES                                  
PH2      DS    CL1                                                              
PHTYPE   DS    CL35                PAYMENT TYPE                                 
PH3      DS    CL1                                                              
PHPDATE  DS    CL8                 PAY DATE                                     
PH4      DS    CL1                                                              
PHAPPLY  DS    CL1                 APPLY                                        
         DS    CL1                                                              
PH5      DS    CL1                                                              
         DS    CL1                                                              
PHLIFT   DS    CL1                 LIFT                                         
         DS    CL1                                                              
PH6      DS    CL1                                                              
         DS    CL1                                                              
PHBNP    DS    CL1                 BNP                                          
         DS    CL1                                                              
PH7      DS    CL1                                                              
PHCRD    DS    CL1                 CREDIT OF INV#                               
         DS    CL1                                                              
PH8      DS    CL1                                                              
PHINVPAY DS    CL11                WAGE TOTAL                                   
PH9      DS    CL1                                                              
PHINVPNH DS    CL9                 PNH TOTAL                                    
PH10     DS    CL1                                                              
PHINVTNH DS    CL10                TNH TOTAL                                    
PH11     DS    CL1                                                              
PHINVTOT DS    CL11                INVOICE TOTAL                                
PH12     DS    CL1                                                              
PHCOM    DS    CL31                COMMENT                                      
PHLNQ    EQU   *-PHISTRY           LENGTH OF HISTORY INVOICE LINE               
         EJECT                                                                  
       ++INCLUDE TAREPFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPC5D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPC6D                                                       
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
**PAN#1  DC    CL21'073TAREP45   06/10/13'                                      
         END                                                                    
