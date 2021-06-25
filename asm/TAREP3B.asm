*          DATA SET TAREP3B    AT LEVEL 048 AS OF 08/13/14                      
*PHASE T7033BC                                                                  
         TITLE 'T7033B - NR4 REPORT'                                            
T7033B   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T7033B                                                         
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
         USING LOCALD,R7           R7=A(LOCAL W/S)                              
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
*                                                                               
         GOTO1 INITIAL,DMCB,0      INITIALIZE                                   
*                                                                               
         CLI   MODE,VALREC         VALIDATE SCREEN                              
         BE    *+12                                                             
         CLI   MODE,DISPREC                                                     
         BNE   *+12                                                             
         BAS   RE,VKEY                                                          
         B     XIT                                                              
*                                                                               
         CLI   MODE,PRINTREP       PROCESS REPORT                               
         BNE   XIT                                                              
         BAS   RE,PREP                                                          
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE SCREEN                                                  
*                                                                               
VKEY     NTR1                                                                   
         LA    R2,SNRYEARH         VALIDATE YEAR                                
         TM    4(R2),X'20'                                                      
         BO    VK10                                                             
         CLI   5(R2),0                                                          
         BE    FLDMISS                                                          
         CLC   8(2,R2),=C'19'                                                   
         BE    *+14                                                             
         CLC   8(2,R2),=C'20'                                                   
         BNE   FLDINV                                                           
         OI    4(R2),X'20'                                                      
         SPACE                                                                  
VK10     LA    R2,SNRSSNH          VALIDATE SSN                                 
         TM    4(R2),X'20'                                                      
         BO    VK20                                                             
         XC    TIFSSN,TIFSSN                                                    
         XC    SNRSSNN,SNRSSNN                                                  
         XC    PERFSSN,PERFSSN                                                  
         XC    TIQSTART,TIQSTART                                                
         CLI   5(R2),0             TEST FOR INPUT                               
         BE    VK19                                                             
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'08',SNRSSNH),SNRSSNNH                     
         MVC   TIFSSN,TGSSN                                                     
         MVC   TIQSTART,TGSSN                                                   
VK19     OI    4(R2),X'20'                                                      
         SPACE                                                                  
VK20     LA    R2,SNRAGYH          VALIDATE AGENCY FILTER                       
         TM    4(R2),X'20'         TEST NOT PREV VALIDATED                      
         BO    VK30                                                             
         NI    SNROPTH+4,X'FF'-X'20'  SET OPTIONS NOT VALIDATED                 
         XC    TIFAGY,TIFAGY                                                    
         CLI   5(R2),0             TEST FOR INPUT                               
         BE    VK29                                                             
         GOTO1 ANY                 GET INPUT IN WORK PADDED WITH SPACES         
         CLI   SNRAGY,C'-'         TEST NEGATIVE FILTER                         
         BNE   VK26                                                             
         CLI   SNRAGY+1,C'@'       TEST FLIST                                   
         BNE   VK24                                                             
         MVI   TGLTYP,TLGLTYPF     SET FILTER FOR READ                          
         GOTO1 RECVAL,DMCB,TLGLCDQ,(X'88',WORK+2),SNRAGYNH  GET NAME            
         BNE   THEEND                                                           
         MVC   TIFAGY,TGLST        SET FLIST FOR AGENCY FILTER                  
         NI    TIFAGY,X'FF'-X'C0'  TELL SYSIO IT'S NEGATIVE FLIST               
         B     VK29                                                             
         SPACE                                                                  
VK24     CLI   5(R2),7             MAX INPUT LENGTH IS 7                        
         BH    FLDINV                                                           
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'88',WORK+1),SNRAGYNH  GET NAME            
         BNE   THEEND                                                           
         MVC   TIFAGY,TGAGY        SET AGENCY FILTER                            
         NI    TIFAGY,X'FF'-X'40'  LET SYSIO KNOW IT'S NEGATIVE                 
         B     VK29                                                             
         SPACE                                                                  
VK26     MVI   BYTE,6              MAX INPUT LENGTH IS 6                        
         CLI   SNRAGY,C'@'         IF FLIST                                     
         BNE   *+8                                                              
         MVI   BYTE,7              MAX INPUT LENGTH IS 7                        
         CLC   5(1,R2),BYTE                                                     
         BH    FLDINV                                                           
         GOTO1 RECVAL,DMCB,(X'40',TLAYCDQ),(X'08',SNRAGYH),SNRAGYNH             
         MVC   TIFAGY,TGAGY                                                     
         CLI   SNRAGY,C'@'         IF FLIST                                     
         BNE   VK29                                                             
         MVC   TIFAGY,TGLST        SET FLIST FOR AGENCY FILTER                  
         NI    TIFAGY,X'FF'-X'80'  LET SYSIO KNOW IT'S FLIST                    
         SPACE                                                                  
VK29     OI    SNRAGYH+4,X'20'                                                  
*                                                                               
VK30     MVC   TIFEMP,=C'TP '      ONLY FOR EMPLOYER TP                         
         BAS   RE,VALOPT           VALIDATE OPTIONS                             
         MVI   FRSTFORM,C'Y'       SET FIRST FORM TO BE PRINTED                 
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE VALIDATES OPTIONS FIELD                                  
         SPACE                                                                  
         USING SCAND,R3                                                         
VALOPT   NTR1                                                                   
         LA    R2,SNROPTH          VALIDATE OPTIONS                             
         TM    4(R2),X'20'                                                      
         BO    VOPTX                                                            
         XC    OPTS,OPTS                                                        
         CLI   5(R2),0                                                          
         BE    VOPTX                                                            
         LA    R3,BLOCK            R3=A(SCAN BLOCK)                             
         GOTO1 SCANNER,DMCB,(R2),(X'80',(R3))                                   
         CLI   4(R1),0                                                          
         BE    FLDINV                                                           
         ZIC   R0,4(R1)            NUMBER OF SCANNER ENTRIES                    
         SPACE                                                                  
VOPT10   CLC   =C'W2',SCDATA1                                                   
         BNE   FLDINV                                                           
         CLI   SCDATA2,C'Y'        IF INPUT W2=Y                                
         BNE   VOPT15                                                           
         OC    TIFAGY,TIFAGY       INVALID IF FILTERING ON AGENCY               
         BNZ   FLDINV                                                           
         OI    OPTS,OPTW2          READ W2 RECORDS INSTEAD OF CHECKS            
         B     VOPTNEXT                                                         
VOPT15   CLI   SCDATA2,C'N'                                                     
         BNE   FLDINV                                                           
         B     VOPTNEXT                                                         
         SPACE                                                                  
VOPTNEXT LA    R3,SCANNEXT         BUMP TO NEXT                                 
         BCT   R0,VOPT10                                                        
         SPACE                                                                  
VOPTX    TM    OPTS,OPTW2          IF READING CHECK RECORDS                     
         BO    *+10                                                             
         XC    TIQSTART,TIQSTART   CLEAR TIQSTART                               
         OI    4(R2),X'20'                                                      
         B     XIT                                                              
         EJECT                                                                  
*              PRINT REPORT                                                     
*                                                                               
PREP     NTR1                                                                   
         XC    NEGTAB,NEGTAB                                                    
         LA    RF,NEGTAB                                                        
         ST    RF,ANEGPTR                                                       
*                                                                               
         MVC   TIACOMFC,ACOMFACS   SET UP FOR TASYSIO                           
         LA    R1,W2HOOK           SET HOOK FOR W2                              
         ST    R1,TIHOOK                                                        
**NO-OP  MVI   TIFCUR,C'U'         ONLY INTERESTED IN $US                       
         SPACE                                                                  
         TM    OPTS,OPTW2          IF OPTION SET TO READ W2 RECORDS             
         BZ    PR15                                                             
         MVI   TIREAD,TLW2CDQ        SET TO READ W2 RECORDS                     
         OI    TIQFLAGS,TIQFDIR      WE'RE FILTERING DIRECTORY                  
         MVC   TIFYEAR(2),SNRYEAR+2  SET YEAR                                   
         B     PRX                                                              
         SPACE                                                                  
PR15     MVI   TIREAD,TLCKCDQ        SET TO READ ACTIVE CHECK POINTER           
         OC    TIFSSN,TIFSSN                                                    
         BNZ   PR18                                                             
         MVI   TIREAD,TLCKYCDQ                                                  
         MVC   TIFUNIT,=C'CN '                                                  
*                                                                               
PR18     MVC   DUB(2),SNRYEAR+2      UPTO YEAR INPUT                            
         MVC   DUB+2(4),=C'1231'     12/31                                      
         GOTO1 DATCON,DMCB,(0,DUB),(1,TIQPEND)  GET PWOS FOR TIQPEND            
         SPACE                                                                  
         MVC   TIQPSTR(1),TIQPEND     SET START DATE FOR YEAR INPUT             
         MVC   TIQPSTR+1(2),=X'0101'                                            
         MVI   TIQDTYPE,TIQDCHK       SET DATES ARE CHECK DATES                 
         OI    TIQFLAGS,TIQFNOGR      SET TO JUST GET CHECKS FROM INV           
         OI    TIQFLAG3,TIQFADJ       SET TO PASS ADJUSTMENTS                   
         LA    R1,DTLHOOK             SET DIFFERENT HOOK FOR DETAILS            
         ST    R1,TIHOOK                                                        
         SPACE                                                                  
PRX      GOTO1 TASYSIO,DMCB,TASYSIOD                                            
         SPACE                                                                  
         TM    STAT,STSORT         IF SORT ACTIVE                               
         BZ    *+8                                                              
         BAS   RE,GETSORT          GET SORTER RECS AND PROCESS                  
*                                                                               
         BAS   RE,PRNTTOT          PRINT TOTALS AT THE END                      
*                                                                               
         BAS   RE,PRNTNEG          PRINT SSN W/ NEGATIVE WAGES                  
         B     XIT                                                              
         EJECT                                                                  
*              SYSIO HOOK FOR W2 RECS                                           
*                                                                               
W2HOOK   NTR1                                                                   
         CLC   TISSN,=C'952693630' SKIP THIS SSN                                
         BE    NO                                                               
         CLI   TIMODE,PROCREC      TEST FOR SYSIO MODE PROCREC                  
         BE    W2HK5                                                            
         CLI   TIMODE,PROCDIR      IF PROCDIR                                   
         BNE   XIT                                                              
W2HK2    CLC   TISSN,PERFSSN       ONLY WANT FIRST W2 REC FOR SSN               
         BE    NO                                                               
         B     YES                                                              
         SPACE                                                                  
W2HK5    CLI   FRSTFORM,C'Y'       IF FIRST FORM TO BE PRINTED                  
         BNE   *+12                                                             
         BAS   RE,LINEUP           PRINT LINE UP PATTERNS                       
         MVI   FRSTFORM,C'N'       SET NOT FIRST FORM                           
         SPACE                                                                  
         MVI   ELCODE,TAW2ELQ      GET W2 DETAILS ELEMENT                       
         MVC   FULL(3),=C'CN '     FOR CN                                       
         MVC   AIO,TIAREC          SET AIO TO SYSIO'S IO AREA                   
         GOTO1 GETL,DMCB,(3,FULL)                                               
         MVC   AIO,AIO1            RESET AIO                                    
         BNE   XIT                 SKIP IF NOT FOUND                            
         SPACE                                                                  
         USING TAW2D,R4                                                         
         L     R4,TGELEM           R4=A(ELEMENT FOUND)                          
         OC    TAW2TAX,TAW2TAX     SKIP IF TAX AMOUNT IS 0                      
         BZ    XIT                                                              
         TM    TAW2TAX,X'80'       TEST NEGATIVE NUMBER                         
         BZ    W2HK8                                                            
         L     RF,ANEGPTR          CURRENT EMPTY TABLE ENTRY                    
         PACK  DUB,TISSN           CONVERT SSN TO BINARY                        
         CVB   R3,DUB                                                           
         ST    R3,0(RF)            SAVE IT                                      
         LA    RF,4(RF)            BUMP TO NEXT EMPTY TABLE ENTRY               
         ST    RF,ANEGPTR          SAVE IT                                      
         B     XIT                                                              
*                                                                               
W2HK8    MVC   TAXAMT,TAW2TAX      TAX AMOUNT                                   
*                                                                               
         L     R1,TOTTAX           ADD TO TOTAL TAX AMOUNT                      
         A     R1,TAXAMT                                                        
         ST    R1,TOTTAX                                                        
         SPACE                                                                  
         BAS   RE,PROCEMP          PROCESS EMPLOYER INFO                        
         MVC   PERFSSN,TISSN       SAVE CURRENT PERFORMER OR CORP               
         BAS   RE,PROCW4           PROCESS W4 RECORD INFO                       
         SPACE                                                                  
W2HKX    BRAS  RE,PRNTFORM         PRINT FORM                                   
         B     XIT                                                              
         EJECT                                                                  
*              SYSIO HOOK FOR TLCK, NEED DETAILS                                
*                                                                               
DTLHOOK  NTR1                                                                   
         CLI   TIMODE,PROCREC      TEST FOR SYSIO MODE PROCREC                  
         BNE   XIT                                                              
         SPACE                                                                  
         MVI   SVCURR,0            SAVED CURRENCY                               
         MVI   SOWCAN,0            SAVED STATUS - SOW = CAN                     
         XC    PYMTAMT,PYMTAMT     PAYMENT AMOUNT                               
         XC    TAXAMT,TAXAMT       TAX AMOUNT                                   
*                                                                               
         MVI   ELCODE,TACWELQ      GET CHK WITHHOLDING ELEMENT                  
         MVC   FULL(3),=C'CN '     FOR CN                                       
         MVC   AIO,TIAREC          SET AIO TO SYSIO'S IO AREA                   
         GOTO1 GETL,DMCB,(3,FULL)                                               
         MVC   AIO,AIO1            RESET AIO                                    
         BNE   XIT                 SKIP IF NOT FOUND                            
*                                                                               
         USING TACWD,R4                                                         
         L     R4,TGELEM           R4=A(ELEMENT FOUND)                          
         OC    TACWTAX,TACWTAX     IF TAX AMOUNT IS NOT 0,                      
         BZ    DTLH10                                                           
         MVC   TAXAMT,TACWTAX      SAVE TAX AMOUNT                              
         BAS   RE,SAVPYMT          SAVE PAYMENT AMOUNT                          
         B     DTLH15                                                           
*                                                                               
DTLH10   BAS   RE,CHKSOW           IF STATE OF WORK GET PYMT AMT                
         BNE   XIT                                                              
*                                                                               
         USING TATID,R4                                                         
DTLH15   MVC   PERFSSN,TISSN                                                    
         L     R4,TIAREC           MAY HAVE CORP TAX ID                         
         MVI   ELCODE,TATIELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   PERFSSN,TATIID                                                   
         CLC   PERFSSN,=C'952693630' SKIP THIS SSN                              
         BE    XIT                                                              
         SPACE                                                                  
         TM    STAT,STSORT         TEST SORT ACTIVE YET                         
         BO    DTLH20                                                           
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD                                     
         OI    STAT,STSORT         SET SORT ACTIVE                              
         SPACE                                                                  
         USING SORTD,R2                                                         
DTLH20   LA    R2,SORTREC                R2=A(SORT RECORD)                      
         XC    SORTREC,SORTREC                                                  
         MVC   SORTSSN,PERFSSN           SSN                                    
         MVI   SORTCUR,C'U'              US DOLLARS                             
         CLI   SVCURR,C'C'               CURRENCY                               
         BNE   *+8                                                              
         MVI   SORTCUR,C'C'              CANADIAN DOLLARS                       
         MVC   SORTTAX,TAXAMT            TAX AMOUNT                             
         MVC   SORTPYMT,PYMTAMT          SAVE PAYMENT AMOUNT                    
         GOTO1 SORTER,DMCB,=C'PUT',(R2)  WRITE OUT SORT REC                     
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE READS SORTER RECORDS AND ACCUMULATES                     
*              WITHHOLDING FOR EACH SSN AND PRINTS ONE NR4 FORM PER SSN         
         SPACE                                                                  
GETSORT  NTR1                                                                   
         BAS   RE,LINEUP           PRINT LINE UP PATTERNS                       
         XC    PERFSSN,PERFSSN     CLEAR CURRENT SSN                            
         MVI   PERFCUR,0           CLEAR CURRENT CURRENCY                       
         LA    R2,SORTREC          R2=A(SORT RECORD)                            
         SPACE                                                                  
         USING SORTD,R2                                                         
GETS5    GOTO1 SORTER,DMCB,=C'GET' GET SORT RECORDS                             
         ICM   RF,15,4(R1)         TEST ANY RECORDS LEFT                        
         BZ    GETSX                                                            
         MVC   SORTREC,0(RF)                                                    
         OC    PERFSSN,PERFSSN     TEST NOT FIRST RECORD READ                   
         BZ    GETS9                                                            
         CLC   PERFCUR,SORTCUR     IF NEW CURR, PROCESS PREV SSN/CUR            
         BNE   GETS7                                                            
         CLC   PERFSSN,SORTSSN     IF NEW SSN, PROCESS PREV SSN FIRST           
         BE    GETS10                                                           
GETS7    OC    PYMTAMT,PYMTAMT     PRINT IF PYMTAMT FOR OLD SSN NOT 0           
         BNZ   GETS8                                                            
         OC    TAXAMT,TAXAMT       IF TAXAMT FOR OLD SSN=0, DON'T PRINT         
         BZ    GETS9               SKIP IT                                      
GETS8    TM    TAXAMT,X'80'        TEST NEGATIVE NUMBER                         
         BZ    GETS8A              NOPE, CONTINUE                               
         L     RF,ANEGPTR          CURRENT EMPTY TABLE ENTRY                    
         PACK  DUB,PERFSSN         CONVERT SSN TO BINARY                        
         CVB   R3,DUB                                                           
         ST    R3,0(RF)            SAVE IT                                      
         LA    RF,4(RF)            BUMP TO NEXT EMPTY TABLE ENTRY               
         ST    RF,ANEGPTR          SAVE IT                                      
*                                                                               
         L     RE,TOTTAX           REMOVE FROM TOTAL                            
         S     RE,TAXAMT                                                        
         ST    RE,TOTTAX                                                        
         B     GETS9                                                            
*                                                                               
GETS8A   BAS   RE,PROCEMP                                                       
         BAS   RE,PROCW4           PROCESS W4 RECORD INFO                       
         BRAS  RE,PRNTFORM         PRINT FORM                                   
         SPACE                                                                  
GETS9    XC    TAXAMT,TAXAMT       CLEAR TAX AMOUNT FOR THIS NEW SSN            
         XC    PYMTAMT,PYMTAMT     CLEAR PYMT AMOUNT FOR NEW SSN                
         MVC   PERFSSN,SORTSSN     SAVE CURRENT SSN                             
         MVC   PERFCUR,SORTCUR     SAVE CURRENT CURRENCY                        
         SPACE                                                                  
GETS10   ICM   R1,15,SORTTAX                                                    
         LR    RE,R1                                                            
         A     R1,TAXAMT           ADD TO TAX AMOUNT FOR THIS SSN               
         ST    R1,TAXAMT                                                        
         SPACE                                                                  
         A     RE,TOTTAX           ADD TO TOTAL TAX AMOUNT                      
         ST    RE,TOTTAX                                                        
         SPACE                                                                  
         ICM   R1,15,SORTPYMT                                                   
         LR    RE,R1                                                            
         A     R1,PYMTAMT          ADD TO PYMT AMOUNT FOR THIS SSN              
         ST    R1,PYMTAMT                                                       
*                                                                               
*        A     RE,TOTPYMT                                                       
*        ST    RE,TOTPYMT                                                       
         B     GETS5               GET NEXT SORT RECORD                         
         SPACE                                                                  
GETSX    OC    PERFSSN,PERFSSN     TEST HAVE SOMETHING TO PROCESS               
         BZ    GETSX2                                                           
         OC    PYMTAMT,PYMTAMT     PRINT IF PYMTAMT FOR OLD SSN NOT 0           
         BNZ   GETSX0                                                           
         OC    TAXAMT,TAXAMT       IF TAXAMT FOR OLD SSN=0, DON'T PRINT         
         BZ    GETSX2              SKIP IT                                      
GETSX0   TM    TAXAMT,X'80'        TEST NEGATIVE NUMBER                         
         BZ    GETSX1              NOPE, CONTINUE                               
         L     RF,ANEGPTR          CURRENT EMPTY TABLE ENTRY                    
         PACK  DUB,PERFSSN         CONVERT SSN TO BINARY                        
         CVB   R3,DUB                                                           
         ST    R3,0(RF)            SAVE IT                                      
         LA    RF,4(RF)            BUMP TO NEXT EMPTY TABLE ENTRY               
         ST    RF,ANEGPTR          SAVE IT                                      
*                                                                               
         L     RE,TOTTAX           REMOVE FROM TOTAL                            
         S     RE,TAXAMT                                                        
         ST    RE,TOTTAX                                                        
         B     GETSX2                                                           
*                                                                               
GETSX1   BAS   RE,PROCEMP                                                       
         BAS   RE,PROCW4           PROCESS W4 RECORD INFO                       
         BRAS  RE,PRNTFORM         PRINT FORM                                   
*                                                                               
GETSX2   GOTO1 SORTER,DMCB,=C'END'                                              
         XI    STAT,STSORT         SET SORT NO LONGER ACTIVE                    
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE PROCESSES EMPLOYER INFO                                  
         SPACE                                                                  
PROCEMP  NTR1                                                                   
         CLC   EMP,=C'TP '         IF DON'T ALREADY HAVE TP EMP INFO            
         BE    XIT                                                              
         MVC   EMP,=C'TP '         GET EMPLOYER INFO FOR TP                     
         GOTO1 RECVAL,DMCB,TLEMCDQ,(X'88',EMP),0                                
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   EMPNAME,TGNAME                                                   
         BAS   RE,GETADDR          GET EMPLOYER ADDRESS                         
         SPACE                                                                  
         BAS   RE,GETCNID          GET EMPLOYER TAX ID FROM EMP TAX REC         
         B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE GETS W4 RECORD FOR PERFSSN AND PROCESSES INFO            
         SPACE                                                                  
PROCW4   NTR1                                                                   
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'A0',PERFSSN)                              
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,GETW4NME         GETS W4 NAME IN FIRST-LAST ORDER             
         MVC   PERFNAME,TGNAME                                                  
         BAS   RE,GETW4ADD         GET ADDRESS                                  
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE GETS ADDRESS ELEMENT IN AIO AND SAVES THE                
*              ADDRESS AT EMPADDR                                               
         SPACE                                                                  
         USING TAADD,R4                                                         
GETADDR  NTR1                                                                   
         XC    EMPADDR,EMPADDR                                                  
         L     R4,AIO                                                           
         MVI   ELCODE,TAADELQ      GET ADDRESS ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         ZIC   R1,TAADLEN                                                       
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     XIT                                                              
         MVC   EMPADDR(0),TAADADD                                               
         SPACE 3                                                                
*              ROUTINE GETS W4 DETAILS ELEMENT IN AIO AND SAVES THE             
*              NAME (FIRST NAME FIRST) IN TGNAME                                
         SPACE                                                                  
         USING TAW4D,R4                                                         
GETW4NME NTR1                                                                   
         MVC   TGNAME,SPACES                                                    
         MVI   ELCODE,TAW4ELQ      GET W4 DETAILS ELEMENT                       
         L     R4,AIO                                                           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   TAW4TYPE,TAW4TYCO   FOR CORPS                                    
         BE    GETW4N5                                                          
         CLI   TAW4TYPE,TAW4TYTR   TRUSTEES                                     
         BE    GETW4N5                                                          
         CLI   TAW4TYPE,TAW4TYES   AND ESTATES, HANDLE DIFFERENTLY              
         BE    GETW4N5                                                          
         MVC   TGNAME(16),TAW4NAM1      WANT FIRST NAME FIRST                   
         MVC   TGNAME+17(16),TAW4NAM2   SKIP A SPACE BEFORE LAST NAME           
         GOTO1 SQUASHER,DMCB,TGNAME,33  SQUASH NAMES TOGETHER                   
         B     XIT                                                              
         SPACE                                                                  
GETW4N5  MVC   TGNAME(32),TAW4CRPN  NAME FOR CORPS AND ESTATES                  
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE GETS TAX ID FOR CN FROM EMPLOYER TAX REC                 
         SPACE                                                                  
GETCNID  NTR1                                                                   
         XC    EMPID,EMPID                                                      
         GOTO1 RECVAL,DMCB,TLEXCDQ,(X'A0',EMP)  READ TP TAX REC                 
         BNE   GETCN5                                                           
         MVI   ELCODE,TATIELQ      GET TAX ID ELEMENT                           
         MVI   FULL,TATITYTX                                                    
         MVC   FULL+1(3),=C'CN '   FOR CN                                       
         GOTO1 GETL,DMCB,(4,FULL)                                               
         BE    *+14                                                             
GETCN5   MVC   EMPID,=CL14'FAKE EMP ID'                                         
         B     XIT                                                              
         SPACE                                                                  
         L     R4,TGELEM           R4=A(ELEMENT FOUND)                          
         USING TATID,R4                                                         
         MVC   EMPID,TATIID        SAVE ID NUMBER                               
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE GETS ADDRESS FROM W4 RECORD AND SAVES                    
*              IT IN PERFADDR                                                   
         SPACE                                                                  
GETW4ADD NTR1                      GET ADDRESS FROM W4 RECORD                   
         XC    PERFADDR,PERFADDR                                                
         L     R4,AIO                                                           
         MVI   ELCODE,TAA2ELQ      SEE IF THERE'S NEW STYLE ADDRESS             
         BAS   RE,GETEL                                                         
         BNE   GETW4A10                                                         
         SPACE                                                                  
         USING TAA2D,R4                                                         
         LA    R2,PERFADDR         R2=WHERE TO SAVE IT IN PERFADDR              
         LA    R6,TAA2ADD1                                                      
         LHI   R0,3                ASSUME 3 ADDRESS LINES                       
         CLI   TAA2LEN,TAA2LNQ     FOR OLD STYLE ADDRESSES                      
         BL    GETW4A2                                                          
         CLC   TAA2CTRY,=C'US'     AND NEW STYLE US ADDRESSES                   
         BE    GETW4A2                                                          
         LHI   R0,2                ELSE USE 2                                   
GETW4A2  CLC   0(30,R6),SPACES     TEST FOR ANY DATA LEFT                       
         BNH   GETW4A5                                                          
         MVC   0(30,R2),0(R6)                                                   
         LA    R2,L'PERFADD1(R2)   BUMP IN PERFADDR                             
         LA    R6,30(R6)           BUMP TO NEXT ADDRESS LINE                    
         BCT   R0,GETW4A2                                                       
         SPACE                                                                  
GETW4A5  XC    WORK(33),WORK                                                    
         MVC   WORK(24),TAA2CITY      CITY (ONLY FITS L'24)                     
         MVC   WORK+25(2),TAA2ST      STATE                                     
         MVC   WORK+28(5),TAA2ZIP     ZIP (ONLY USE L'5)                        
         GOTO1 SQUASHER,DMCB,WORK,33  SQUASH IT                                 
         MVC   0(33,R2),WORK                                                    
         SPACE                                                                  
         BRAS  RE,GETCTRY                                                       
         B     XIT                                                              
         SPACE                                                                  
         USING TAADD,R4                                                         
GETW4A10 MVI   ELCODE,TAADELQ      GET OLD STYLE ADDRESS ELEMENT                
         L     R4,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         ZIC   R0,TAADLNES         R0=N'ADDRESS LINES                           
         LA    R4,TAADADD                                                       
         LA    R3,PERFADDR                                                      
         SPACE                                                                  
GETW4A15 MVC   0(L'TAADADD,R3),0(R4)                                            
         LA    R3,L'PERFADD1(R3)   BUMP TO NEXT SAVED ADDRESS LINE              
         LA    R4,L'TAADADD(R4)                                                 
         BCT   R0,GETW4A15         LOOP FOR NUMBER OF LINES                     
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO CHECK IF STATE OF WORK IS CANADA                      
*              AND STATE OF RESIDENCE IS NOT CANADA                             
*              RETURNS CCEQ IF SHOULD HAVE HAD CN TAX WITHHELD                  
*              SAVES PAYMENT AMOUNT                                             
         SPACE 2                                                                
CHKSOW   NTR1                                                                   
         L     R4,TIAREC                                                        
         MVI   ELCODE,TACAELQ      GET CAST DETAILS ELEMENT                     
         BAS   RE,GETEL                                                         
         BNE   NO                                                               
         USING TACAD,R4                                                         
         CLC   TACAUNIT,=C'CN '    IS CN STATE OF WORK?                         
         BE    CKSOW05                                                          
         MVC   TGCTRY,=C'CA'                                                    
         GOTO1 TAXVAL,DMCB,(X'FF',TACAUNIT)                                     
         BNE   NO                                                               
CKSOW05  CLC   =C'ACT',TACAUN      SKIP IF UNION ACTRA                          
         BE    NO                                                               
         CLC   =C'UDA',TACAUN      UDA                                          
         BE    NO                                                               
         CLC   =C'NON',TACAUN      OR NON CAN                                   
         BNE   CKSOW10                                                          
         CLC   =C'CAN',TACALOCL                                                 
         BE    NO                                                               
         DROP  R4                                                               
*                                                                               
CKSOW10  L     R4,TIAREC                                                        
         MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS ELEMENT                  
         USING TAPDD,R4                                                         
         BAS   RE,GETEL                                                         
         BNE   NO                                                               
         CLI   TAPDW4TY,TAW4TYCA   EXCLUDE IF CANADIAN                          
         BE    NO                                                               
         CLI   TAPDW4TY,TAW4TYCO   IF CORP, STATE OF RES MAY BE CANADA          
         BNE   CKSOW20                                                          
*                                                                               
         BAS   RE,READCRP          OKAY IF CORP HAS INDIV ATTACHED              
         BNE   NO                                                               
*                                                                               
CKSOW20  MVI   SOWCAN,C'Y'         SAVE STATUS, STATE OF WORK = CAN             
         L     R1,TAPDPAYI                                                      
         A     R1,TAPDPAYC                                                      
         CLI   TAPDLEN,TAPDLNQ                                                  
         BL    *+8                                                              
         A     R1,TAPDTXNW                                                      
         ST    R1,PYMTAMT          SAVE PAYMENT AMOUNT                          
         TM    TAPDSTAT,TAPDSCAN   CAN DOLLAR INVOICE?                          
         BNO   YES                                                              
         MVI   SVCURR,C'C'                                                      
         B     YES                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*              ROUTINE TO CHECK IF CORP HAS INDIVIDUAL ATTACHED                 
*              IF YES, CORP IS NOT A CANADIAN RESIDENT                          
*                                                                               
READCRP  NTR1                                                                   
         L     R4,TIAREC                                                        
         MVI   ELCODE,TATIELQ     LOOK FOR TAX ID ELEMENT                       
         USING TATID,R4                                                         
         BAS   RE,GETEL           OKAY IF INDIVIDUAL ATTACHED TO CORP           
         BE    YES                                                              
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING TLW4PD,R4                                                        
         MVI   TLW4PCD,TLW4CCDQ    LOOK FOR CORP POINTER                        
         MVC   TLW4CCRP,TISSN                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(TLW4CSSN-TLW4PD),KEYSAVE                                     
         BE    RDCRPYES                                                         
         MVC   KEY,TIKEY           RESTORE READ SEQUENCE FOR SYSIO              
         BAS   RE,SETCHK                                                        
         GOTO1 HIGH                                                             
         BAS   RE,SETTAL                                                        
         B     NO                                                               
*                                                                               
RDCRPYES MVC   KEY,TIKEY           RESTORE READ SEQUENCE FOR SYSIO              
         BAS   RE,SETCHK                                                        
         GOTO1 HIGH                                                             
         BAS   RE,SETTAL                                                        
         B     YES                                                              
*                                                                               
         DROP  R4                                                               
         EJECT                                                                  
*        SET SYSFIL/DIR TO CHECK FILE                                           
*                                                                               
         SPACE 1                                                                
SETCHK   NTR1                                                                   
         MVC   SYSFIL,=CL8'CHKFIL'                                              
         MVC   SYSDIR,=CL8'CHKDIR'                                              
         B     XIT                                                              
         SPACE 2                                                                
*                                                                               
*        SET SYSFIL/DIR TO TALENT FILE                                          
*                                                                               
         SPACE 1                                                                
SETTAL   NTR1                                                                   
         MVC   SYSFIL,=CL8'TALFIL'                                              
         MVC   SYSDIR,=CL8'TALDIR'                                              
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*              ROUTINE TO SAVE PAYMENT AMOUNT FROM TAPD ELEMENT                 
*              IN CHECK RECORD                                                  
         SPACE 2                                                                
SAVPYMT  NTR1                                                                   
         L     R4,TIAREC                                                        
         MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS ELEMENT                  
         USING TAPDD,R4                                                         
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         L     R1,TAPDPAYI                                                      
         A     R1,TAPDPAYC                                                      
         CLI   TAPDLEN,TAPDLNQ                                                  
         BL    *+8                                                              
         A     R1,TAPDTXNW                                                      
         ST    R1,PYMTAMT          SAVE PAYMENT AMOUNT                          
         TM    TAPDSTAT,TAPDSCAN   CAN DOLLAR INVOICE?                          
         BNO   XIT                                                              
         MVI   SVCURR,C'C'                                                      
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO PRINT A LINE UP PATTERN                               
         SPACE 2                                                                
LINEUP   NTR1                                                                   
         MVI   NUMPAGE,3           SET TO GET NEW PAGE                          
         MVC   TAXAMT,=F'1000'                                                  
         MVI   EMPNAME,C'E'                                                     
         MVC   EMPNAME+1(L'EMPNAME-1),EMPNAME                                   
         MVI   EMPADDR,C'E'                                                     
         MVC   EMPADDR+1(L'EMPADDR-1),EMPADDR                                   
         MVC   EMPID,EMPNAME                                                    
         MVI   PERFNAME,C'P'                                                    
         MVC   PERFNAME+1(L'PERFNAME-1),PERFNAME                                
         MVI   PERFADDR,C'P'                                                    
         MVC   PERFADDR+1(L'PERFADDR-1),PERFADDR                                
         SPACE                                                                  
         MVC   PERFSSN,=C'111111111'                                            
         BRAS  RE,PRNTFORM                                                      
         MVC   PERFSSN,=C'222222222'                                            
         BRAS  RE,PRNTFORM                                                      
         MVC   PERFSSN,=C'333333333'                                            
         BRAS  RE,PRNTFORM                                                      
         MVC   PERFSSN,=C'444444444'                                            
         BRAS  RE,PRNTFORM                                                      
         MVC   PERFSSN,=C'555555555'                                            
         BRAS  RE,PRNTFORM                                                      
         MVC   PERFSSN,=C'666666666'                                            
         BRAS  RE,PRNTFORM                                                      
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PRINT TOTALS                                          
         SPACE 2                                                                
PRNTTOT  NTR1                                                                   
         MVI   NUMPAGE,3           SET TO GET NEW PAGE                          
         MVC   TAXAMT,TOTTAX                                                    
         MVC   PYMTAMT,TOTPYMT                                                  
         MVC   PERFADD1,=CL39'TOTALS'                                           
         MVC   EMPNAME,PERFADD1                                                 
         MVC   PERFNAME,PERFADD1                                                
         MVC   PERFADD2,PERFADD1                                                
         MVC   PERFADD3,PERFADD1                                                
         MVC   PERFADD4,PERFADD1                                                
         MVC   EMPADD1,PERFADD1                                                 
         MVC   EMPADD2,PERFADD1                                                 
         MVC   EMPADD3,PERFADD1                                                 
         MVC   EMPADD4,PERFADD1                                                 
         XC    EMPID,EMPID                                                      
         XC    PERFSSN,PERFSSN                                                  
         MVI   PERFCUR,0                                                        
         BRAS  RE,PRNTFORM                                                      
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PRINT SSN W/ NEGATIVE WAGES, W2=Y OPTION ONLY         
         SPACE 2                                                                
PRNTNEG  NTR1                                                                   
         OC    NEGTAB,NEGTAB       ANY SSN IN TABLE?                            
         BZ    XIT                 NO, LEAVE                                    
*                                                                               
         BAS   RE,PRNTIT                                                        
         MVC   P(45),=C'=============================================='         
         BAS   RE,PRNTIT                                                        
         MVC   P(10),=CL10'Negatives'                                           
         BAS   RE,PRNTIT                                                        
         MVC   P(45),=C'=============================================='         
         BAS   RE,PRNTIT                                                        
*                                                                               
         LA    R2,NEGTAB                                                        
PNEG10   OC    0(4,R2),0(R2)       ARE WE DONE?                                 
         BZ    XIT                                                              
         EDIT  (B4,0(R2)),(9,P+2),0,FILL=0                                      
         BAS   RE,PRNTIT                                                        
         LA    R2,4(R2)                                                         
         B     PNEG10                                                           
*              ROUTINE TO PRINT A LINE                                          
         SPACE                                                                  
PRNTIT   NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE CONVERTS AMOUNT IN R5 TO CANADIAN $ AND RETURNS          
*              RESULT IN R1                                                     
         SPACE                                                                  
*&&UK                                                                           
CONVERT  NTR1                                                                   
         GOTO1 RECVAL,DMCB,TLSYCDQ,(X'20',0) GET SYSTEM RECORD                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,AIO                                                           
         MVI   ELCODE,TASYELQ      TO GET SYSTEM ELEMENT FOR RATE               
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         USING TASYD,R4                                                         
         ZAP   DUB,TASYCCVT        CONVERT RATE TO BINARY                       
         CVB   R2,DUB              R2=RATE                                      
         SPACE                                                                  
         L     R1,=F'100000'         100,000                                    
         MR    R0,R5               * R5=AMOUNT                                  
         DR    R0,R2               / R2=RATE                                    
         XR    R0,R0                                                            
         D     R0,=F'5'                                                         
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AH    R1,=H'1'            ROUND                                        
         SRA   R1,1                                                             
         XIT1  REGS=(R1)           RETURN R1                                    
*&&                                                                             
         EJECT                                                                  
*              ERROR/EXIT ROUTINES                                              
         SPACE 2                                                                
FLDINV   MVI   ERROR,INVALID                                                    
         B     THEEND                                                           
FLDMISS  MVI   ERROR,MISSING                                                    
         B     THEEND                                                           
THEEND   GOTO1 ERREX                                                            
         SPACE 2                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 3                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
SORTCARD DC    CL80'SORT FIELDS=(1,10,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=18'                                    
         SPACE                                                                  
MAXPAGE  EQU   3                   PRINT 3 FORMS ON A PAGE                      
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO PRINT A FORM                                          
         SPACE 2                                                                
PRNTFORM NTR1  BASE=*,LABEL=*                                                   
         ZIC   R2,NUMPAGE          R2=# ALREADY PRINTED ON THIS PAGE            
         LA    R1,MAXPAGE          PRINT MAXPAGE # OF FORMS PER PAGE            
* NO-OP  CR    R2,R1               TEST ALREADY PRINTED MAX #                   
* NO-OP  BL    PRNTF3                                                           
         XR    R2,R2               CLEAR TO RESET NUMPAGE LATER                 
*        MVI   FORCEHED,C'Y'       FORCE NEW PAGE                               
         MVI   SPACING,4           SKIP 4 LINES                                 
         B     *+8                                                              
PRNTF3   MVI   SPACING,9           SKIP 9 LINES                                 
         BAS   RE,PRNTIT2                                                       
         LA    R2,1(R2)            INCREMENT R2=# PRINTED ON THIS PAGE          
         STC   R2,NUMPAGE                                                       
         SPACE                                                                  
         USING LINED,R2                                                         
         LA    R2,P                                                             
         MVC   LINYEAR,SNRYEAR                                                  
         MVI   LINRTYPE,C'1'                                                    
         MVC   LINCCODE,=C'USA'                                                 
         MVC   LINEID,EMPID                                                     
         MVC   LINPSSN(3),PERFSSN                                               
         MVC   LINPSSN+4(2),PERFSSN+3                                           
         MVC   LINPSSN+7(4),PERFSSN+5                                           
         MVI   LINPSSN+3,C'-'                                                   
         MVI   LINPSSN+6,C'-'                                                   
         MVI   SPACING,3                                                        
         BAS   RE,PRNTIT2                                                       
         SPACE                                                                  
         MVC   LINICODE,=C'22'                                                  
         MVC   LINCURR,=C'USD'                                                  
         CLI   PERFCUR,C'C'          CANADIAN DOLLARS?                          
         BNE   *+10                                                             
         MVC   LINCURR,=C'CAD'                                                  
         EDIT  TAXAMT,(11,LINTAX),2  TAX AMOUNT                                 
* PER MAURA, RESTORE REVERSE CALCULATION OF GROSS (FEB05/2007)                  
         L     R5,TAXAMT             R5=TAX AMOUNT                              
         XR    R4,R4                                                            
         EDIT  (R5),(11,LINTAX),2    TAX AMOUNT                                 
*                                                                               
         CVD   R5,DUB                                                           
         ZAP   WORK(11),DUB                                                     
         MP    WORK(11),=P'1000000'                                             
         DP    WORK(11),=P'23000'    / TAX RATE                                 
         ZAP   DUB,WORK(8)                                                      
         SRP   DUB,63,5              ROUND IT                                   
         CVB   R5,DUB                                                           
*&&DO                                                                           
         M     R4,=F'1000000'                                                   
         D     R4,=F'23000'          / TAX RATE                                 
         XR    R4,R4                                                            
         D     R4,=F'5'              ROUND IT                                   
         LTR   R5,R5                                                            
         BM    *+8                                                              
         AH    R5,=H'1'                                                         
         SRA   R5,1                  R5=PAYMENT AMOUNT                          
*&&                                                                             
         ST    R5,PYMTAMT                                                       
         LR    RE,R5                                                            
         A     RE,TOTPYMT                                                       
         ST    RE,TOTPYMT                                                       
*                                                                               
*1/1/97  BAS   RE,CONVERT            CONVERT IT TO CANADIAN $                   
         EDIT  PYMTAMT,(11,LINGROSS),2    PAYMENT AMOUNT                        
         MVI   SPACING,7                                                        
         BAS   RE,PRNTIT2                                                       
         SPACE                                                                  
         MVI   SPACING,1                                                        
         MVC   LINPNAME,PERFNAME                                                
         MVC   LINENAME,EMPNAME                                                 
         BAS   RE,PRNTIT2                                                       
         MVC   LINPADDR,PERFADD1                                                
         MVC   LINEADDR,EMPADD1                                                 
         BAS   RE,PRNTIT2                                                       
         MVC   LINPADDR,PERFADD2                                                
         MVC   LINEADDR,EMPADD2                                                 
         BAS   RE,PRNTIT2                                                       
         MVC   LINPADDR,PERFADD3                                                
         MVC   LINEADDR,EMPADD3                                                 
         BAS   RE,PRNTIT2                                                       
         MVC   LINPADDR,PERFADD4                                                
         MVC   LINEADDR,EMPADD4                                                 
         BAS   RE,PRNTIT2                                                       
PXIT     XIT1                                                                   
         EJECT                                                                  
*              ROUTINE TO PRINT A LINE                                          
         SPACE                                                                  
PRNTIT2  NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     PXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO GET COUNTRY CODE                                      
*              ON ENTRY ... R2 = A(PRINT LINE)                                  
*                           R4 = A(ADDRESS ELEMENT)                             
         SPACE                                                                  
         USING TAA2D,R4                                                         
GETCTRY  NTR1  BASE=*,LABEL=*                                                   
         CLI   TAA2LEN,TAA2LNQ        FOR OLD STYLE ADDRESSES                   
         JL    XIT                                                              
         CLC   TAA2CTRY,=C'US'        AND NEW STYLE US ADDRESSES                
         JE    XIT                                                              
         GOTO1 VALCTRY,DMCB,(X'80',TAA2CTRY)                                    
         JNE   XIT                                                              
         DROP  R4                                                               
         SPACE 1                                                                
         USING CTRYTABD,R1                                                      
         L     R1,TGACTRY                                                       
         ZIC   RE,CTRYDSP                                                       
         LTR   RE,RE                                                            
         JNZ   *+8                                                              
         IC    RE,CTRYLEN                                                       
         SHI   RE,CTRYDESC-CTRYTABD+1                                           
         MVC   WORK(33),SPACES                                                  
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   WORK(0),CTRYDESC                                                 
         EX    RE,*+8                                                           
         J     *+10                                                             
         OC    WORK(0),SPACES                                                   
         MVC   34(33,R2),WORK                                                   
         J     XIT                                                              
         DROP  R1                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              DSECT TO COVER LOCAL WORKING STORAGE                             
         SPACE 1                                                                
LOCALD   DSECT                                                                  
TAXAMT   DS    F                   TAX AMOUNT                                   
TOTTAX   DS    F                   TOTAL TAX AMOUNT                             
PYMTAMT  DS    F                   PAYMENT AMOUNT                               
TOTPYMT  DS    F                   TOTAL PAYMENT AMOUNT                         
NUMPAGE  DS    X                   NUMBER ALREADY PRINTED ON A PAGE             
FRSTFORM DS    C                   Y=PRINTING 1ST FORM - MUST LINE UP           
         SPACE                                                                  
OPTS     DS    XL1                 OPTIONS                                      
OPTW2    EQU   X'80'               READ W2 RECORDS (NOT CHECKS)                 
         SPACE                                                                  
STAT     DS    XL1                 STATUS                                       
STSORT   EQU   X'80'               SORT IS ACTIVE                               
         SPACE                                                                  
EMP      DS    CL3                 EMPLOYER                                     
EMPNAME  DS    CL36                EMPLOYER NAME                                
EMPADDR  DS    0CL(4*30)                    ADDRESS                             
EMPADD1  DS    CL30                                                             
EMPADD2  DS    CL30                                                             
EMPADD3  DS    CL30                                                             
EMPADD4  DS    CL30                                                             
EMPID    DS    CL14                         ID NUMBER                           
         SPACE                                                                  
PERFNAME DS    CL33                PERFORMER OR CORP NAME                       
PERFADDR DS    0CL(4*33)                             ADDRESS                    
PERFADD1 DS    CL33                                                             
PERFADD2 DS    CL33                                                             
PERFADD3 DS    CL33                                                             
PERFADD4 DS    CL33                                                             
PERFSSN  DS    CL9                 PERFORMER OR CORP SSN                        
         SPACE                                                                  
PERFCUR  DS    CL1                 PERFORMER'S CURRENCY                         
SVCURR   DS    CL1                 SAVED CURRENCY                               
SOWCAN   DS    CL1                 STATE OF WORK = CN                           
SVKEY    DS    CL38                SAVED KEY                                    
ANEGPTR  DS    A                   ADDRESS OF EMPTY ENTRY                       
NEGTAB   DS    XL256               LIST OF 64 SSN W/ NEG TAX (BINARY)           
         SPACE                                                                  
SORTREC  DS    CL(SORTLNQ)         SORT RECORD AREA                             
         EJECT                                                                  
*              DSECT TO COVER PRINT LINE                                        
         SPACE 1                                                                
LINED    DSECT                                                                  
         DS    CL2                                                              
LINYEAR  DS    CL4                 YEAR                                         
         DS    CL4                                                              
LINRTYPE DS    CL1                 RESIDENT TYPE                                
         DS    CL4                                                              
LINCCODE DS    CL3                 COUNTRY CODE                                 
         DS    CL14                                                             
LINEID   DS    CL11                EMPLOYER ID                                  
         DS    CL8                                                              
LINPSSN  DS    CL11                SOCIAL SECURITY NUMBER                       
         ORG   LINED                                                            
         DS    CL6                                                              
LINICODE DS    CL2                 INCOME CODE                                  
         DS    CL6                                                              
LINCURR  DS    CL3                 CURRENCY CODE                                
         DS    CL5                                                              
LINGROSS DS    CL11                GROSS                                        
         DS    CL8                                                              
LINTAX   DS    CL11                TAX WITHHELD                                 
         ORG   LINED                                                            
         DS    CL2                                                              
LINPNAME DS    CL33                PERFORMER NAME                               
         DS    CL3                                                              
LINENAME DS    CL30                EMPLOYER NAME                                
         ORG   LINPNAME                                                         
LINPADDR DS    CL33                PERFORMER ADDRESS                            
         DS    CL3                                                              
LINEADDR DS    CL30                EMPLOYER ADDRESS                             
         EJECT                                                                  
*              DSECT FOR SORT RECORD                                            
         SPACE                                                                  
SORTD    DSECT                                                                  
SORTSSN  DS    CL9                 SSN                                          
SORTCUR  DS    CL1                 CURRENCY                                     
SORTTAX  DS    CL4                 TAX AMOUNT                                   
SORTPYMT DS    CL4                 PAYMENT AMT (SOW = CAN)                      
SORTLNQ  EQU   *-SORTD                                                          
         EJECT                                                                  
       ++INCLUDE TAREPFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPDBD                                                       
         EJECT                                                                  
* DDGENTWA   (MUST FOLLOW LAST SCREEN)                                          
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* TAGENFILE                                                                     
* TASYSDSECT                                                                    
* TASYSEQUS                                                                     
* TAREPWORKD                                                                    
* DDPERVALD                                                                     
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAREPWORKD                                                     
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'048TAREP3B   08/13/14'                                      
         END                                                                    
