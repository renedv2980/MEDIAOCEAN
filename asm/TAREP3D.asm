*          DATA SET TAREP3D    AT LEVEL 027 AS OF 11/12/15                      
*PHASE T7033DC                                                                  
*INCLUDE ADSCAN                                                                 
         TITLE 'T7033D - NR4 REPORT (XML)'                                      
T7033D   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T7033D,R6                                                      
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
         SPACE                                                                  
         L     R2,TWADCONS                                                      
         USING TWADCOND,R2                                                      
         MVC   DYNALLOC,TDYNALLO   A(DYNALLOC)                                  
         DROP  R2                                                               
         SPACE                                                                  
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
         SPACE                                                                  
VK30     MVC   TIFEMP,=C'TP '      ONLY FOR EMPLOYER TP                         
         BAS   RE,VALOPT           VALIDATE OPTIONS                             
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
         OI    OPTS,OPTNOTAP       DEFAULT NO TAPE                              
         CLI   5(R2),0                                                          
         BE    VOPTX                                                            
         XC    BLOCK(120),BLOCK                                                 
         LA    R3,BLOCK            R3=A(SCAN BLOCK)                             
         GOTO1 SCANNER,DMCB,(R2),(X'80',(R3))                                   
         CLI   4(R1),0                                                          
         BE    FLDINV                                                           
         ZIC   R0,4(R1)            NUMBER OF SCANNER ENTRIES                    
*                                                                               
VOPT10   CLC   =C'W2',SCDATA1                                                   
         BNE   VOPT20                                                           
         CLI   SCDATA2,C'Y'        IF INPUT W2=Y                                
         BNE   VOPT15                                                           
         OC    TIFAGY,TIFAGY       INVALID IF FILTERING ON AGENCY               
         BNZ   FLDINV                                                           
         OI    OPTS,OPTW2          READ W2 RECORDS INSTEAD OF CHECKS            
         B     VOPTNEXT                                                         
VOPT15   CLI   SCDATA2,C'N'                                                     
         BNE   FLDINV                                                           
         B     VOPTNEXT                                                         
*                                                                               
VOPT20   CLC   =C'TAPE',SCDATA1                                                 
         BNE   VOPT30                                                           
         CLI   SCDATA2,C'N'                                                     
         BE    VOPTNEXT                                                         
         CLI   SCDATA2,C'Y'                                                     
         BNE   FLDINV                                                           
         NI    OPTS,X'FF'-OPTNOTAP     GENERATE A TAPE                          
         B     VOPTNEXT                                                         
*                                                                               
VOPT30   CLC   =C'TRACE',SCDATA1                                                
         BNE   VOPT40                                                           
         CLI   SCDATA2,C'Y'                                                     
         BNE   FLDINV                                                           
         OI    OPTS,OPTTRACE     TRACE RECORDS                                  
         B     VOPTNEXT                                                         
*                                                                               
VOPT40   CLC   =C'AMEND',SCDATA1                                                
         BNE   FLDINV                                                           
         CLI   SCDATA2,C'Y'                                                     
         BNE   FLDINV                                                           
         OI    OPTS,OPTAMEND     AMEND FORMS                                    
         B     VOPTNEXT                                                         
*                                                                               
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
*                                                                               
         TM    OPTS,OPTW2          IF OPTION SET TO READ W2 RECORDS             
         BZ    PR15                                                             
         MVI   TIREAD,TLW2CDQ      SET TO READ W2 RECORDS                       
         OI    TIQFLAGS,TIQFDIR    WE'RE FILTERING DIRECTORY                    
         MVC   TIFYEAR(2),SNRYEAR+2    SET YEAR                                 
         BAS   RE,OPENTAPE         OPEN THE TAPE                                
         BAS   RE,PROCEMP                                                       
         BRAS  RE,PROCSUBM         PROCESS SUBMISSION RECORD                    
         B     PR20                                                             
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
         MVC   TIQPSTR(1),TIQPEND     SET START DATE FOR YEAR INPUT             
         MVC   TIQPSTR+1(2),=X'0101'                                            
         MVI   TIQDTYPE,TIQDCHK       SET DATES ARE CHECK DATES                 
         OI    TIQFLAGS,TIQFNOGR      SET TO JUST GET CHECKS FROM INV           
         OI    TIQFLAG3,TIQFADJ       SET TO PASS ADJUSTMENTS                   
         LA    R1,DTLHOOK             SET DIFFERENT HOOK FOR DETAILS            
         ST    R1,TIHOOK                                                        
*                                                                               
PR20     GOTO1 TASYSIO,DMCB,TASYSIOD                                            
*                                                                               
         TM    OPTS,OPTW2          W2 OPTION ON?                                
         BO    PR30                                                             
         TM    STAT,STSORT         IF SORT ACTIVE                               
         BZ    PRX                                                              
         BAS   RE,PROCEMP          PROCESS EMPLOYER RECORD                      
         BAS   RE,OPENTAPE         OPEN THE TAPE                                
         BRAS  RE,PROCSUBM         PROCESS TRANSMITTER RECORD                   
         BAS   RE,GETSORT          GET SORTER RECS AND PROCESS                  
*                                                                               
PR30     BRAS  RE,PRCSUMM          PROCESS SUMMARY RECORD                       
         BAS   RE,CLOSTAPE         CLOSE THE TAPE                               
         BAS   RE,PRNTNEG          PRINT SSN W/ NEGATIVE WAGES                  
PRX      B     XIT                                                              
         EJECT                                                                  
*              SYSIO HOOK FOR W2 RECS                                           
*                                                                               
W2HOOK   NTR1                                                                   
         CLC   TISSN,=C'952693630'  SKIP THIS SSN                               
         BE    NO                                                               
         CLI   TIMODE,PROCDIR      IF PROCDIR                                   
         BNE   W2HK2                                                            
         CLC   TISSN,PERFSSN       ONLY WANT FIRST W2 REC FOR SSN               
         BE    NO                                                               
         B     YES                                                              
         CLI   TIMODE,PROCREC      TEST FOR SYSIO MODE PROCREC                  
         BNE   XIT                                                              
         SPACE                                                                  
W2HK2    MVI   ELCODE,TAW2ELQ      GET W2 DETAILS ELEMENT                       
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
         BZ    W2HK5               NOPE, CONTINUE                               
         L     RF,ANEGPTR          CURRENT EMPTY TABLE ENTRY                    
         PACK  DUB,TISSN        CONVERT SSN TO BINARY                           
         CVB   R3,DUB                                                           
         ST    R3,0(RF)            SAVE IT                                      
         LA    RF,4(RF)            BUMP TO NEXT EMPTY TABLE ENTRY               
         ST    RF,ANEGPTR          SAVE IT                                      
         B     XIT                                                              
*                                                                               
W2HK5    MVC   TAXAMT,TAW2TAX      TAX AMOUNT                                   
         L     R1,TOTTAX           ADD TO TOTAL TAX AMOUNT                      
         A     R1,TAXAMT                                                        
         ST    R1,TOTTAX                                                        
         SPACE                                                                  
         MVC   PERFSSN,TISSN       SAVE CURRENT PERFORMER OR CORP               
         BAS   RE,PROCW4           PROCESS W4 RECORD INFO                       
         BRAS  RE,PRCSLIP                                                       
         B     XIT                                                              
         EJECT                                                                  
*              SYSIO HOOK FOR TLCK, NEED DETAILS                                
*                                                                               
DTLHOOK  NTR1                                                                   
         CLI   TIMODE,PROCREC      TEST FOR SYSIO MODE PROCREC                  
         BNE   XIT                                                              
*                                                                               
         MVI   SVCURR,0            SAVED CURRENCY                               
         MVI   SOWCAN,0            SAVED STATUS - SOW = CAN                     
         XC    PYMTAMT,PYMTAMT     PAYMENT AMOUNT                               
         XC    TAXAMT,TAXAMT       TAX AMOUNT                                   
*                                                                               
         LA    R2,SORTREC                                                       
         USING SORTD,R2                                                         
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
         BRAS  RE,SAVPYMT          SAVE PAYMENT AMOUNT                          
         B     DTLH15                                                           
*                                                                               
DTLH10   BRAS  RE,CHKSOW           IF STATE OF WORK GET PYMT AMT                
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
*                                                                               
         XC    SORTREC,SORTREC                                                  
         MVC   SORTSSN,PERFSSN           SSN                                    
         MVI   SORTCUR,C'U'              US DOLLARS                             
         CLI   SVCURR,C'C'               CURRENCY                               
         BNE   *+8                                                              
         MVI   SORTCUR,C'C'              CANADIAN DOLLARS                       
         MVC   SORTTAX,TAXAMT            TAX AMOUNT                             
         MVC   SORTPYMT,PYMTAMT          SAVE PAYMENT AMOUNT                    
         BAS   RE,PUTSORT                                                       
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE READS SORTER RECORDS AND ACCUMULATES                     
*              WITHHOLDING FOR EACH SSN AND PUTS ONE SLIP RECORD TO             
*              TAPE PER SSN                                                     
         SPACE                                                                  
GETSORT  NTR1                                                                   
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
         OC    TAXAMT,TAXAMT       IF TAXAMT FOR OLD SSN=0, DO NOT              
         BZ    GETS9               GENERATE SLIP RECORD, SKIP IT                
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
GETS8A   BAS   RE,PROCW4           PROCESS W4 RECORD INFO                       
         BRAS  RE,PRCSLIP                                                       
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
*                                                                               
GETSX    OC    PERFSSN,PERFSSN     TEST HAVE SOMETHING TO PROCESS               
         BZ    GETSX2                                                           
         OC    PYMTAMT,PYMTAMT     PRINT IF PYMTAMT FOR OLD SSN NOT 0           
         BNZ   GETSX0                                                           
         OC    TAXAMT,TAXAMT       IF TAXAMT FOR LAST SSN=0, DO NOT             
         BZ    GETSX2              GENERATE SLIP RECORD, SKIP IT                
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
GETSX1   BAS   RE,PROCW4           PROCESS W4 RECORD INFO                       
         BRAS  RE,PRCSLIP                                                       
         SPACE                                                                  
GETSX2   GOTO1 SORTER,DMCB,=C'END'                                              
         XI    STAT,STSORT         SET SORT NO LONGER ACTIVE                    
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE PROCESSES EMPLOYER INFO                                  
         SPACE                                                                  
PROCEMP  NTR1                                                                   
         CLC   EMP,=C'TP '         IF DON'T ALREADY HAVE TP EMP INFO            
         BE    XIT                                                              
         MVC   EMPINFO(106),SPACES CLEAR EMPLOYER INFO                          
         MVC   EMP,=C'TP '         GET EMPLOYER INFO FOR TP                     
         GOTO1 RECVAL,DMCB,TLEMCDQ,(X'88',EMP),0                                
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAADD,R4                                                         
         MVI   ELCODE,TAADELQ      GET OLD STYLE ADDRESS ELEMENT                
         L     R4,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         MVC   EMPNAME,TGNAME                                                   
         BAS   RE,GETADD           PARSE ADDRESS                                
         MVC   EMPADD,BLOCK        STREET ADDRESS                               
         MVC   EMPCITY,BLOCK+90    CITY                                         
         MVC   EMPSTATE,DUB        STATE                                        
         MVC   EMPZIP,WORK         ZIP                                          
         B     XIT                                                              
         DROP  R4                                                               
         SPACE 3                                                                
*              ROUTINE THAT GETS THE EMPLOYER ADDRESS                           
         SPACE                                                                  
*        GET ADDRESS                                                            
*        INPUT  R4           - A(ADDRESS ELEMENT)                               
*        RETURN BLOCK        - STREET ADDRESS                                   
*               BLOCK+90     - CITY                                             
*               DUB          - STATE                                            
*               WORK         - ZIP CODE                                         
*                                                                               
         USING TAADD,R4                                                         
GETADD   NTR1                                                                   
         MVC   BLOCK(120),SPACES                                                
         MVC   WORK,SPACES                                                      
         MVC   DUB,SPACES                                                       
         LA    R3,TAADADD          A(ADDRESS)                                   
         ZIC   R2,2(R4)            N'LINES                                      
         BCTR  R2,0                                                             
         MH    R2,=H'30'                                                        
*                                                                               
         LA    R1,BLOCK                                                         
         MVC   0(30,R1),0(R3)      STREET ADDRESS                               
         LA    R1,30(R1)                                                        
         ST    R1,TGFULL                                                        
*                                                                               
GA10     LA    R4,30(R3)           NEXT LINE                                    
         GOTO1 =V(ADSCAN),DMCB,((R2),0(R4)),(30,BLOCK+90),DUB,(9,WORK)          
         CLI   0(R1),0             2 ACCEPTABLE RETURN CODES                    
         BE    GA20                                                             
         CLI   0(R1),2                                                          
         BNE   FLDINV                                                           
*                                                                               
GA20     CLC   BLOCK+90(30),SPACES     DID WE FIND CITY                         
         BH    GA30                                                             
         SH    R2,=H'30'           LESS 1 LINE                                  
         BNP   GA30                                                             
         LA    R3,30(R3)                                                        
         L     R1,TGFULL                                                        
         MVC   0(30,R1),0(R3)      STREET ADDRESS                               
         LA    R1,30(R1)                                                        
         ST    R1,TGFULL                                                        
         B     GA10                                                             
*                                                                               
GA30     GOTO1 SQUASHER,DMCB,BLOCK,90                                           
*                                                                               
GETADDX  B     XIT                                                              
         EJECT                                                                  
*              ROUTINE GETS W4 RECORD FOR PERFSSN AND PROCESSES INFO            
         SPACE                                                                  
PROCW4   NTR1                                                                   
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'A0',PERFSSN)                              
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,GETW4NME         GETS W4 NAME IN FIRST-LAST ORDER             
         BAS   RE,GETW4ADD         GET ADDRESS                                  
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE GETS W4 DETAILS ELEMENT IN AIO AND SAVES THE             
*              NAME IN RPTLNAM AND RPTFNAM                                      
         SPACE                                                                  
         USING TAW4D,R4                                                         
GETW4NME NTR1                                                                   
         MVC   RPTINFO,SPACES      CLEAR RPTNAME,AND CORPNAME                   
         MVI   ELCODE,TAW4ELQ      GET W4 DETAILS ELEMENT                       
         L     R4,AIO                                                           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   TAW4LEN,TAW4LN2Q    DOES IT HAVE MIDDLE INITAL                   
         BL    *+10                                                             
         MVC   RPTMIDI,TAW4MIDN                                                 
*                                                                               
         CLI   TAW4TYPE,TAW4TYCO   FOR CORPS                                    
         BE    GETW4N5                                                          
         CLI   TAW4TYPE,TAW4TYTR   TRUSTEES                                     
         BE    GETW4N5                                                          
         CLI   TAW4TYPE,TAW4TYES   AND ESTATES, HANDLE DIFFERENTLY              
         BE    GETW4N5                                                          
         MVC   RPTLNAM(16),TAW4NAM2    RECIPIENT LAST NAME                      
         MVC   RPTFNAM,TAW4NAM1        RECIPIENT FIRST NAME                     
         B     XIT                                                              
         SPACE                                                                  
GETW4N5  MVC   CORPNAM(L'TAW4CRPN),TAW4CRPN   SAVE CORPORATION NAME             
         OC    CORPNAM,SPACES                                                   
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE GETS ADDRESS FROM W4 RECORD AND SAVES                    
*              IT IN WORKING STORAGE                                            
         SPACE                                                                  
GETW4ADD NTR1                      GET ADDRESS FROM W4 RECORD                   
         MVC   RPTINFO2,SPACES     CLEAR RECIPIENT ADDRESS                      
         L     R4,AIO                                                           
         MVI   ELCODE,TAA2ELQ      SEE IF THERE'S NEW STYLE ADDRESS             
         BAS   RE,GETEL                                                         
         BNE   GETW4A10                                                         
         SPACE                                                                  
         USING TAA2D,R4                                                         
         MVC   RPTADD1,TAA2ADD1       RECIPIENT ADDRESS LINE 1                  
         MVC   RPTADD2,TAA2ADD2       LINE 2                                    
         OC    RPTADD2,SPACES                                                   
         MVC   RPTCITY(25),TAA2CITY   CITY                                      
         CLI   TAA2LEN,TAA2LNQ        IF OLD STYLE ADDRESS                      
         BL    GW4A10                                                           
         CLC   TAA2CTRY,=C'US'        OR COUNTRY IS US                          
         BNE   XIT                                                              
GW4A10   MVC   RPTSTATE,TAA2ST        STATE                                     
         MVC   RPTZIP,TAA2ZIP         ZIP CODE                                  
         B     XIT                                                              
         SPACE                                                                  
         USING TAADD,R4                                                         
GETW4A10 MVI   ELCODE,TAADELQ      GET OLD STYLE ADDRESS ELEMENT                
         L     R4,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         BAS   RE,GETADD           PARSE ADDRESS                                
         MVC   RPTADD1,BLOCK       STREET ADDRESS                               
         MVC   RPTCITY,BLOCK+90    CITY                                         
         MVC   RPTSTATE,DUB        STATE                                        
         MVC   RPTZIP,WORK         ZIP                                          
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
         SPACE                                                                  
*              ROUTINE TO PRINT TOTALS                                          
         SPACE 2                                                                
PRNTTOT  NTR1                                                                   
         MVC   P(7),=C'TOTALS:'                                                 
         BAS   RE,PRNTIT                                                        
         BAS   RE,PRNTIT                                                        
         MVC   P(23),=C'NUMBER OF SLIP RECORDS:'                                
         EDIT  (2,SLIPTOT),(7,P+26)   NUMBER OF SLIP RECORDS                    
         BAS   RE,PRNTIT                                                        
         MVC   P(12),=C'GROSS TOTAL:'                                           
         EDIT  (4,TOTPYMT),(13,P+15),2   TOTAL GROSS INCOME                     
         BAS   RE,PRNTIT                                                        
         MVC   P(31),=C'TOTAL NON-RESIDENT TAX WITHHELD'                        
         EDIT  (4,TOTTAX),(13,P+34),2  TOTAL NON-RES TAX WITHHELD               
         BAS   RE,PRNTIT                                                        
         B     XIT                                                              
         SPACE 2                                                                
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
         EJECT                                                                  
*              ROUTINE TO PRINT A LINE                                          
         SPACE                                                                  
PRNTIT   NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO OPEN TAPE                                             
         SPACE                                                                  
OPENTAPE NTR1                                                                   
         TM    OPTS,OPTNOTAP                                                    
         BO    XIT                                                              
*                                                                               
         L     R2,=A(N4TAPE)       N4TAPE = REPORT                              
         OPEN  ((2),OUTPUT)                                                     
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
OPENTX   B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PUT RECORD TO TAPE                                    
         SPACE                                                                  
PUTTAPE  NTR1                                                                   
         LA    R3,TAPEREC                                                       
         ST    R3,FULL             ADDRESS OF RECORD TO TRACE                   
         MVC   HALF,=AL2(TPLNQ)    LENGTH OF RECORD                             
         GOTO1 MYTRACE,DMCB,=C'TAPE REC'                                        
         TM    OPTS,OPTNOTAP   AND DOING A TAPE                                 
         BO    XIT                                                              
         LA    R2,N4TAPE                                                        
         PUT   (R2),(R3)          PUT IT TO TAPE                                
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO CLOSE TAPE                                            
         SPACE                                                                  
CLOSTAPE NTR1                                                                   
         TM    OPTS,OPTNOTAP                                                    
         BO    XIT                                                              
*                                                                               
         TM    OPTS,OPTNOTAP     IF TAPE REQUESTED                              
         BO    CLOSTX                                                           
         LA    R2,N4TAPE           CLOSE IT                                     
         CLOSE ((2))                                                            
CLOSTX   B     XIT                                                              
         EJECT                                                                  
*              ROUTINE PUTS A RECORD TO SORT                                    
*              ON ENTRY R2 POINTS TO SORT RECORD                                
         SPACE                                                                  
PUTSORT  NTR1                                                                   
         TM    STAT,STSORT         TEST SORT ACTIVE YET                         
         BO    PSORT10                                                          
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD                                     
         OI    STAT,STSORT         SET SORT ACTIVE                              
         SPACE                                                                  
PSORT10  GOTO1 SORTER,DMCB,=C'PUT',(R2)  WRITE OUT SORT REC                     
         B     XIT                                                              
         EJECT                                                                  
         SPACE                                                                  
*              ROUTINE TO PRESET TAPE RECORD TO SPACES                          
         SPACE                                                                  
CLRIT    NTR1                                                                   
         MVC   0(132,R3),SPACES                                                 
         LA    R3,132(R3)                                                       
         MVC   0(132,R3),SPACES                                                 
         LA    R3,132(R3)                                                       
         MVC   0(88,R3),SPACES                                                  
         B     XIT                                                              
*              ROUTINE TO TRACE RECORD                                          
         SPACE                                                                  
MYTRACE  NTR1                                                                   
         TM    OPTS,OPTTRACE       IF TRACE ON                                  
         BZ    MYTRACEX                                                         
         L     R4,FULL             WHAT TO TRACE                                
         LH    R5,HALF             LENGTH TO TRACE                              
         L     R2,0(R1)            LITERAL                                      
         ZIC   R3,0(R1)            LENGTH OF LITERAL                            
         GOTO1 TRACE,DMCB,(R4),(R5),(R2),(R3)                                   
MYTRACEX B     XIT                                                              
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
N4TAPE   DCB   DDNAME=N4TAPE,DSORG=PS,RECFM=FB,LRECL=132,              X        
               BLKSIZE=26400,MACRF=PM                                           
SORTCARD DC    CL80'SORT FIELDS=(1,10,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=18'                                    
*                                                                               
         LTORG                                                                  
*                                                                               
CONTACT  DC    CL15'Julie Hegelmann' CONTACT NAME                               
CTAREA   DC    C'312'              CONTACT AREA CODE                            
CTPHONE  DC    C'9237900'          CONTACT PHONE NUMBER                         
ZEROS    DC    13X'F0'                                                          
*                                                                               
*                                                                               
         SPACE 2                                                                
         EJECT                                                                  
*              ROUTINE PUTS SUBM PART TO TAPE                                   
         SPACE                                                                  
PROCSUBM NTR1  BASE=*,LABEL=*                                                   
         LA    R2,P                                                             
         GOTO1 =A(TAG2),DMCB,XXMLHD        <?xml>                               
         BRAS  RE,SPLAT2                                                        
         LA    R2,P                                                             
         GOTO1 =A(TAG2),DMCB,XSUBST        <Submission>                         
         BRAS  RE,SPLAT2                                                        
         LA    R2,P                                                             
         GOTO1 =A(TAG2),DMCB,XSUBS2        <Submission>                         
         BRAS  RE,SPLAT2                                                        
         LA    R2,P                                                             
         GOTO1 =A(TAG2),DMCB,XT619S        <T619>                               
         BRAS  RE,SPLAT2                                                        
*                                                                               
         LA    R2,P+4                                                           
         GOTO1 =A(TAG2),DMCB,XSRIDS        <sbmt_ref_id>                        
         GOTO1 DATCON,DMCB,(5,0),(20,(R2))    (TODAY in yyyymmdd)               
         LA    R2,8(R2)                                                         
         GOTO1 =A(TAG2),DMCB,XSRIDE        </sbmt_ref_id>                       
         BRAS  RE,SPLAT2                                                        
*                                                                               
         LA    R2,P+4                                                           
         GOTO1 =A(TAG2),DMCB,XRTCDS        <rpt_tcd>                            
         MVI   0(R2),C'O'                  ORIGINAL FORM (DEFAULT)              
         TM    OPTS,OPTAMEND               AMENDED FORMS                        
         BZ    *+8                                                              
         MVI   0(R2),C'A'                  AMENDED FORM                         
         LA    R2,1(R2)                                                         
         GOTO1 =A(TAG2),DMCB,XRTCDE        </rpt_tcd>                           
         BRAS  RE,SPLAT2                                                        
*                                                                               
         LA    R2,P+4                                                           
         GOTO1 =A(TAG2),DMCB,XTNBRS        <trnmtr_nbr>                         
         MVC   0(8,R2),=CL8'MM066353'      TRANSMITTER NUMBER                   
         LA    R2,8(R2)                                                         
         GOTO1 =A(TAG2),DMCB,XTNBRE        </trnmtr_nbr>                        
         BRAS  RE,SPLAT2                                                        
*                                                                               
         LA    R2,P+4                                                           
         GOTO1 =A(TAG2),DMCB,XTTCDS        <trnmtr_tcd>                         
         MVI   0(R2),C'1'                  TRANSMITTING ON OUR BEHALF           
         LA    R2,1(R2)                                                         
         GOTO1 =A(TAG2),DMCB,XTTCDE        </trnmtr_tcd>                        
         BRAS  RE,SPLAT2                                                        
*                                                                               
         LA    R2,P+4                                                           
         GOTO1 =A(TAG2),DMCB,XSUMCS        <summ_cnt>                           
         MVC   0(6,R2),=C'000001'          TOTAL # OF SUMMARY RECS              
         LA    R2,6(R2)                                                         
         GOTO1 =A(TAG2),DMCB,XSUMCE        </summ_cnt>                          
         BRAS  RE,SPLAT2                                                        
*                                                                               
         LA    R2,P+4                                                           
         GOTO1 =A(TAG2),DMCB,XLNGCS        <lang_cd>                            
         MVI   0(R2),C'E'                  LANGUAGE = ENGLISH                   
         LA    R2,1(R2)                                                         
         GOTO1 =A(TAG2),DMCB,XLNGCE        </lang_cd>                           
         BRAS  RE,SPLAT2                                                        
*                                                                               
         LA    R2,P+4                                                           
         GOTO1 =A(TAG2),DMCB,XTNAMS        <TRNMTR_NM>                          
         BRAS  RE,SPLAT2                                                        
*                                                                               
         LA    R2,P+4                                                           
         GOTO1 =A(TAG2),DMCB,XL1NMS        <l1_nm>                              
         MVC   0(30,R2),=CL30'Talent Partners Com Serv, LLC'                    
         LA    R2,30(R2)                                                        
         GOTO1 =A(TAG2),DMCB,XL1NME        </l1_nm>                             
         BRAS  RE,SPLAT2                                                        
*                                                                               
         LA    R2,P+4                                                           
         GOTO1 =A(TAG2),DMCB,XTNAME        </TRNMTR_NM>                         
         BRAS  RE,SPLAT2                                                        
*                                                                               
         LA    R2,P+4                                                           
         GOTO1 =A(TAG2),DMCB,XTADDS        <TRNMTR_ADDR>                        
         BRAS  RE,SPLAT2                                                        
*                                                                               
         LA    R2,P+4                                                           
         GOTO1 =A(TAG2),DMCB,XALN1S        <addr_l1_txt>                        
         MVC   0(32,R2),=CL32'111 W. Jackson Blvd., Suite 1525'                 
         LA    R2,32(R2)                                                        
         GOTO1 =A(TAG2),DMCB,XALN1E        </addr_l1_txt>                       
         BRAS  RE,SPLAT2                                                        
*                                                                               
         LA    R2,P+4                                                           
         GOTO1 =A(TAG2),DMCB,XACTYS        <cty_nm>                             
         MVC   0(28,R2),=CL28'Chicago'                                          
         LA    R2,28(R2)                                                        
         GOTO1 =A(TAG2),DMCB,XACTYE        </cty_nm>                            
         BRAS  RE,SPLAT2                                                        
*                                                                               
         LA    R2,P+4                                                           
         GOTO1 =A(TAG2),DMCB,XAPRVS        <prov_cd>                            
         MVC   0(2,R2),=CL2'IL'                                                 
         LA    R2,2(R2)                                                         
         GOTO1 =A(TAG2),DMCB,XAPRVE        </prov_cd>                           
         BRAS  RE,SPLAT2                                                        
*                                                                               
         LA    R2,P+4                                                           
         GOTO1 =A(TAG2),DMCB,XACNTS        <cntry_cd>                           
         MVC   0(3,R2),=CL3'USA'                                                
         LA    R2,3(R2)                                                         
         GOTO1 =A(TAG2),DMCB,XACNTE        </cntry_cd>                          
         BRAS  RE,SPLAT2                                                        
*                                                                               
         LA    R2,P+4                                                           
         GOTO1 =A(TAG2),DMCB,XAPSTS        <pstl_cd>                            
         MVC   0(10,R2),=CL10'60604'                                            
         LA    R2,10(R2)                                                        
         GOTO1 =A(TAG2),DMCB,XAPSTE        </pstl_cd>                           
         BRAS  RE,SPLAT2                                                        
*                                                                               
         LA    R2,P+4                                                           
         GOTO1 =A(TAG2),DMCB,XTADDE        </TRNMTR_ADDR>                       
         BRAS  RE,SPLAT2                                                        
*                                                                               
         LA    R2,P+4                                                           
         GOTO1 =A(TAG2),DMCB,XCNTCS        <CNTC>                               
         BRAS  RE,SPLAT2                                                        
*                                                                               
         LA    R2,P+4                                                           
         GOTO1 =A(TAG2),DMCB,XCNTNS        <cntc_nm>                            
         MVC   0(15,R2),=CL15'Julie Hegelmann'                                  
         LA    R2,15(R2)                                                        
         GOTO1 =A(TAG2),DMCB,XCNTNE        </cntc_nm>                           
         BRAS  RE,SPLAT2                                                        
*                                                                               
         LA    R2,P+4                                                           
         GOTO1 =A(TAG2),DMCB,XCNTAS        <cntc_area_cd>                       
         MVC   0(3,R2),=CL3'312'                                                
         LA    R2,3(R2)                                                         
         GOTO1 =A(TAG2),DMCB,XCNTAE        </cntc_area_cd>                      
         BRAS  RE,SPLAT2                                                        
*                                                                               
         LA    R2,P+4                                                           
         GOTO1 =A(TAG2),DMCB,XCNTPS        <cntc_phn_nbr>                       
         MVC   0(8,R2),=CL8'923-7900'                                           
         LA    R2,8(R2)                                                         
         GOTO1 =A(TAG2),DMCB,XCNTPE        </cntc_phn_nbr>                      
         BRAS  RE,SPLAT2                                                        
*                                                                               
         LA    R2,P+4                                                           
         GOTO1 =A(TAG2),DMCB,XCNTCE        </CNTC>                              
         BRAS  RE,SPLAT2                                                        
*                                                                               
         LA    R2,P                                                             
         GOTO1 =A(TAG2),DMCB,XT619E        </T619>                              
         BRAS  RE,SPLAT2                                                        
*                                                                               
         LA    R2,P                                                             
         GOTO1 =A(TAG2),DMCB,XRTRNS        <Return>                             
         BRAS  RE,SPLAT2                                                        
*                                                                               
         LA    R2,P+1                                                           
         GOTO1 =A(TAG2),DMCB,XRNR4S        <NR4>                                
         BRAS  RE,SPLAT2                                                        
*                                                                               
PRCSUBMX XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
XXMLHD   DC    AL1(37),C'<?xml version="1.0" encoding="UTF-8"?>'                
XSUBST   DC    AL1(64),C'<Submission xmlns:xsi="http://www.w3.org/2001/X        
               XMLSchema-instance"'                                             
XSUBS2   DC    AL1(42),C'xsi:noNamespaceSchemaLocation="layout.xsd">'           
XRTRNS   DC    AL1(09),C'  <Return>'                                            
XRNR4S   DC    AL1(06),C'  <NR4>'                                               
XT619S   DC    AL1(07),C'  <T619>'                                              
XT619E   DC    AL1(08),C'  </T619>'                                             
XSRIDS   DC    AL1(12),C'<sbmt_ref_id>'                                         
XSRIDE   DC    AL1(13),C'</sbmt_ref_id>'                                        
XRTCDS   DC    AL1(08),C'<rpt_tcd>'                                             
XRTCDE   DC    AL1(09),C'</rpt_tcd>'                                            
XTNBRS   DC    AL1(11),C'<trnmtr_nbr>'                                          
XTNBRE   DC    AL1(12),C'</trnmtr_nbr>'                                         
XTTCDS   DC    AL1(11),C'<trnmtr_tcd>'                                          
XTTCDE   DC    AL1(12),C'</trnmtr_tcd>'                                         
XSUMCS   DC    AL1(09),C'<summ_cnt>'                                            
XSUMCE   DC    AL1(10),C'</summ_cnt>'                                           
XLNGCS   DC    AL1(08),C'<lang_cd>'                                             
XLNGCE   DC    AL1(09),C'</lang_cd>'                                            
*                                                                               
XTNAMS   DC    AL1(10),C'<TRNMTR_NM>'                                           
XTNAME   DC    AL1(11),C'</TRNMTR_NM>'                                          
XL1NMS   DC    AL1(08),C'  <l1_nm>'                                             
XL1NME   DC    AL1(07),C'</l1_nm>'                                              
XL2NMS   DC    AL1(08),C'  <l2_nm>'                                             
XL2NME   DC    AL1(07),C'</l2_nm>'                                              
XTADDS   DC    AL1(12),C'<TRNMTR_ADDR>'                                         
XTADDE   DC    AL1(13),C'</TRNMTR_ADDR>'                                        
XALN1S   DC    AL1(14),C'  <addr_l1_txt>'                                       
XALN1E   DC    AL1(13),C'</addr_l1_txt>'                                        
XALN2S   DC    AL1(14),C'  <addr_l2_txt>'                                       
XALN2E   DC    AL1(13),C'</addr_l2_txt>'                                        
XACTYS   DC    AL1(09),C'  <cty_nm>'                                            
XACTYE   DC    AL1(08),C'</cty_nm>'                                             
XAPRVS   DC    AL1(10),C'  <prov_cd>'                                           
XAPRVE   DC    AL1(09),C'</prov_cd>'                                            
XACNTS   DC    AL1(11),C'  <cntry_cd>'                                          
XACNTE   DC    AL1(10),C'</cntry_cd>'                                           
XAPSTS   DC    AL1(10),C'  <pstl_cd>'                                           
XAPSTE   DC    AL1(09),C'</pstl_cd>'                                            
*                                                                               
XCNTCS   DC    AL1(05),C'<CNTC>'                                                
XCNTCE   DC    AL1(06),C'</CNTC>'                                               
XCNTNS   DC    AL1(10),C'  <cntc_nm>'                                           
XCNTNE   DC    AL1(09),C'</cntc_nm>'                                            
XCNTAS   DC    AL1(15),C'  <cntc_area_cd>'                                      
XCNTAE   DC    AL1(14),C'</cntc_area_cd>'                                       
XCNTPS   DC    AL1(15),C'  <cntc_phn_nbr>'                                      
XCNTPE   DC    AL1(14),C'</cntc_phn_nbr>'                                       
XCNTXS   DC    AL1(16),C'  <cntc_extn_nbr>'                                     
XCNTXE   DC    AL1(15),C'</cntc_extn_nbr>'                                      
XCNTES   DC    AL1(18),C'  <cntc_email_area>'                                   
XCNTEE   DC    AL1(18),C'  <cntc_email_area>'                                   
         EJECT                                                                  
*======================================================================         
*              THIS ROUTINE PUTS THE XML TAGS WHERE R2 IS                       
*                   R2 IS ALSO UPDATED                                          
*======================================================================         
TAG2     NTR1  BASE=*,LABEL=*                                                   
         L     RF,DMCB             A(XML TAG)                                   
         SR    R1,R1                                                            
         IC    R1,0(RF)                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),1(RF)                                                    
         LA    R2,1(R1,R2)                                                      
         XIT1  REGS=(R2)                                                        
         EJECT                                                                  
*======================================================================         
*              THIS ROUTINE PUTS FIXES SPECIAL CHARS                            
*                   R2 IS ALSO UPDATED                                          
*======================================================================         
MARKUP   NTR1  BASE=*,LABEL=*                                                   
         L     RF,DMCB             A(TEXT)                                      
         L     RE,DMCB+4           LENGTH OF TEXT                               
*                                                                               
MRKUP3   MVC   0(0,R2),0(RF)       MOVE TEXT CHAR BY CHAR                       
         CLI   0(RF),X'50'                                                      
         BNE   MRKUP5                                                           
         MVC   1(4,R2),=C'amp;'                                                 
         LA    R2,4(R2)                                                         
*                                                                               
MRKUP5   LA    R2,1(R2)                                                         
         LA    RF,1(RF)                                                         
         BCT   RE,MRKUP3                                                        
*                                                                               
         XIT1  REGS=(R2)                                                        
         EJECT                                                                  
*======================================================================         
*              USER SUPPLIED PRINT ROUTINE - DOWNLOAD                           
*======================================================================         
SPLAT2   NTR1  BASE=*,LABEL=*                                                   
         TM    OPTS,OPTNOTAP       NO FILE GENERATED?                           
         BO    SPLAT2B                                                          
         LA    R5,P                                                             
         BAS   RE,PUTIT2                                                        
SPLAT2B  GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   LINE,1              PREVENT PAGE BREAK                           
SPLAT2X  XIT1                                                                   
         EJECT                                                                  
*======================================================================         
*              ROUTINE TO PUT A RECORD TO TAPE OR DATASET                       
*              IF WCODE OPTION - R3 = A(HEADER)                                 
*                                R3 = A(DETAIL)                                 
*              ELSE              R5 = A(TAPEREC)                                
*======================================================================         
PUTIT2   NTR1                                                                   
         L     R2,=A(N4TAPE)       N4TAPE = REPORT                              
         PUT   (R2),(R5)           PUT IT TO TAPE/DATASET                       
*                                                                               
         B     SPLAT2X                                                          
         EJECT                                                                  
*======================================================================         
*              ROUTINE PUTS SLIP PART TO TAPE                                   
*======================================================================         
         SPACE                                                                  
PRCSLIP  NTR1  BASE=*,LABEL=*                                                   
         LA    R2,P+4                                                           
         GOTO1 =A(TAG2),DMCB,XSLIPS        <NR4Slip>                            
         BRAS  RE,SPLAT2                                                        
*                                                                               
         CLC   CORPNAM,MYSPACES            IS RECIPIENT A CORP?                 
         BNH   PRCSLP05                    NO, INDIVIDUAL                       
         LA    R2,P+6                      YES                                  
         GOTO1 =A(TAG2),DMCB,XENTNS        <ENTPRS_NM>                          
         BRAS  RE,SPLAT2                                                        
*                                                                               
         LA    R2,P+6                                                           
         GOTO1 =A(TAG2),DMCB,XEL1NS        <l1_nm>                              
         GOTO1 =A(MARKUP),DMCB,CORPNAM,L'CORPNAM                                
         GOTO1 =A(TAG2),DMCB,XEL1NE        </l1_nm>                             
         BRAS  RE,SPLAT2                                                        
*                                                                               
         LA    R2,P+6                                                           
         GOTO1 =A(TAG2),DMCB,XENTNE        </ENTPRS_NM>                         
         BRAS  RE,SPLAT2                                                        
         B     PRCSLP7X                    SKIP INDIVIDUAL NAME                 
*                                                                               
PRCSLP05 LA    R2,P+6                                                           
         GOTO1 =A(TAG2),DMCB,XRCPNS        <RCPNT_NM>                           
         BRAS  RE,SPLAT2                                                        
*                                                                               
         LA    R2,P+6                                                           
         GOTO1 =A(TAG2),DMCB,XRSNMS        <snm>                                
         MVC   0(L'RPTLNAM,R2),RPTLNAM     SURNAME                              
         LA    R2,L'RPTLNAM(R2)                                                 
         GOTO1 =A(TAG2),DMCB,XRSNME        </snm>                               
         BRAS  RE,SPLAT2                                                        
*                                                                               
         LA    R2,P+6                                                           
         GOTO1 =A(TAG2),DMCB,XRGNMS        <gvn_nm>                             
         MVC   0(L'RPTFNAM,R2),RPTFNAM     GIVEN NAME                           
         LA    R2,L'RPTFNAM(R2)                                                 
         GOTO1 =A(TAG2),DMCB,XRGNME        </gvn_nm>                            
         BRAS  RE,SPLAT2                                                        
*                                                                               
         CLI   RPTMIDI,C' '                DO WE HAVE A MIDDLE INIT?            
         BNH   PRCSLP07                                                         
         LA    R2,P+6                                                           
         GOTO1 =A(TAG2),DMCB,XRINTS        <init>                               
         MVC   0(L'RPTMIDI,R2),RPTMIDI     INITIAL                              
         LA    R2,L'RPTMIDI(R2)                                                 
         GOTO1 =A(TAG2),DMCB,XRINTE        </init>                              
         BRAS  RE,SPLAT2                                                        
*                                                                               
PRCSLP07 LA    R2,P+6                                                           
         GOTO1 =A(TAG2),DMCB,XRCPNE        </RCPNT_NM>                          
         BRAS  RE,SPLAT2                                                        
*                                                                               
PRCSLP7X LA    R2,P+6                                                           
         GOTO1 =A(TAG2),DMCB,XRADDS        <RCPNT_ADDR>                         
         BRAS  RE,SPLAT2                                                        
*                                                                               
         LA    R2,P+6                                                           
         GOTO1 =A(TAG2),DMCB,XRAL1S        <addr_l1_txt>                        
         GOTO1 =A(MARKUP),DMCB,RPTADD1,L'RPTADD1                                
         GOTO1 =A(TAG2),DMCB,XRAL1E        </addr_l1_txt>                       
         BRAS  RE,SPLAT2                                                        
*                                                                               
         CLC   RPTADD2,MYSPACES                                                 
         BNH   PRCSLP08                                                         
         LA    R2,P+6                                                           
         GOTO1 =A(TAG2),DMCB,XRAL2S        <addr_l2_txt>                        
         GOTO1 =A(MARKUP),DMCB,RPTADD2,L'RPTADD2                                
         GOTO1 =A(TAG2),DMCB,XRAL2E        </addr_l2_txt>                       
         BRAS  RE,SPLAT2                                                        
*                                                                               
PRCSLP08 CLC   RPTCITY,MYSPACES                                                 
         BNH   PRCSLP09                                                         
         LA    R2,P+6                                                           
         GOTO1 =A(TAG2),DMCB,XRACTS        <cty_nm>                             
         MVC   0(L'RPTCITY,R2),RPTCITY     CITY                                 
         LA    R2,L'RPTCITY(R2)                                                 
         GOTO1 =A(TAG2),DMCB,XRACTE        </cty_nm>                            
         BRAS  RE,SPLAT2                                                        
*                                                                               
PRCSLP09 CLC   RPTSTATE,MYSPACES                                                
         BNH   PRCSLP10                                                         
         LA    R2,P+6                                                           
         GOTO1 =A(TAG2),DMCB,XRASTS        <ste_cd>                             
         MVC   0(L'RPTSTATE,R2),RPTSTATE   STATE                                
         LA    R2,L'RPTSTATE(R2)                                                
         GOTO1 =A(TAG2),DMCB,XRASTE        </ste_cd>                            
         BRAS  RE,SPLAT2                                                        
*                                                                               
PRCSLP10 LA    R2,P+6                                                           
         GOTO1 =A(TAG2),DMCB,XRACNS        <cntry_cd>                           
         MVC   0(3,R2),=C'USA'             COUNTRY CODE                         
         CLC   RPTSTATE,MYSPACES                                                
         BH    *+10                                                             
         MVC   0(3,R2),=C'ZZZ'                                                  
         LA    R2,3(R2)                                                         
         GOTO1 =A(TAG2),DMCB,XRACNE        </cntry_cd>                          
         BRAS  RE,SPLAT2                                                        
*                                                                               
         CLC   RPTZIP,MYSPACES                                                  
         BNH   PRCSLP20                                                         
         LA    R2,P+6                                                           
         GOTO1 =A(TAG2),DMCB,XRAFPS        <fgn_pstl_cd>                        
         MVC   0(L'RPTZIP,R2),RPTZIP       POSTAL CODE                          
         LA    R2,L'RPTZIP(R2)                                                  
         GOTO1 =A(TAG2),DMCB,XRAFPE        </fgn_pstl_cd>                       
         BRAS  RE,SPLAT2                                                        
*                                                                               
PRCSLP20 LA    R2,P+6                                                           
         GOTO1 =A(TAG2),DMCB,XRADDE        </RCPNT_ADDR>                        
         BRAS  RE,SPLAT2                                                        
*                                                                               
         LA    R2,P+6                                                           
         GOTO1 =A(TAG2),DMCB,XTCCDS        <tx_cntry_cd>                        
         MVC   0(3,R2),=C'USA'             TAX COUNTRY CODE                     
         LA    R2,3(R2)                                                         
         GOTO1 =A(TAG2),DMCB,XTCCDE        </tx_cntry_cd>                       
         BRAS  RE,SPLAT2                                                        
*                                                                               
         LA    R2,P+6                                                           
         GOTO1 =A(TAG2),DMCB,XFSSNS        <fssn_nbr>                           
         MVC   0(L'PERFSSN,R2),PERFSSN     SSN / SIN                            
         LA    R2,L'PERFSSN(R2)                                                 
         GOTO1 =A(TAG2),DMCB,XFSSNE        </fssn_nbr>                          
         BRAS  RE,SPLAT2                                                        
*                                                                               
         LA    R2,P+6                                                           
         GOTO1 =A(TAG2),DMCB,XNRANS        <nr_acct_nbr>                        
         MVC   0(9,R2),=C'NRJ692173'       NON RESIDENT ACCT NUMBER             
         LA    R2,9(R2)                                                         
         GOTO1 =A(TAG2),DMCB,XNRANE        </nr_acct_nbr>                       
         BRAS  RE,SPLAT2                                                        
*                                                                               
         LA    R2,P+6                                                           
         GOTO1 =A(TAG2),DMCB,XRCPCS        <rcpnt_tcd>                          
         MVI   0(R2),C'1'                  RECIPIENT CODE, INDIVIDUAL           
         CLI   CORPNAM,C' '                                                     
         BE    *+8                                                              
         MVI   0(R2),C'3'                  CORP                                 
         LA    R2,1(R2)                                                         
         GOTO1 =A(TAG2),DMCB,XRCPCE        </rcpnt_tcd>                         
         BRAS  RE,SPLAT2                                                        
*                                                                               
         LA    R2,P+6                                                           
         GOTO1 =A(TAG2),DMCB,XINC1CS       <inc_1_tcd>                          
         MVC   0(2,R2),=C'22'              INCOME CODE                          
         LA    R2,2(R2)                                                         
         GOTO1 =A(TAG2),DMCB,XINC1CE       </inc_1_tcd>                         
         BRAS  RE,SPLAT2                                                        
*                                                                               
         LA    R2,P+6                                                           
         GOTO1 =A(TAG2),DMCB,XCUR1CS       <crcy_1_cd>                          
         MVC   0(3,R2),=C'USD'             CURRENCY CODE                        
         CLI   PERFCUR,C'C'                CANADIAN DOLLARS?                    
         BNE   *+10                                                             
         MVC   0(3,R2),=C'CAD'             CURRENCY CODE                        
         LA    R2,3(R2)                                                         
         GOTO1 =A(TAG2),DMCB,XCUR1CE       </crcy_1_cd>                         
         BRAS  RE,SPLAT2                                                        
*                                                                               
         LA    R2,P+6                                                           
         GOTO1 =A(TAG2),DMCB,XNR4AS        <NR4_AMT>                            
         BRAS  RE,SPLAT2                                                        
* PER MAURA, RESTORE REVERSE CALCULATION OF GROSS (FEB05/2007)                  
         L     R5,TAXAMT           R5=TAX AMOUNT                                
         XR    R4,R4                                                            
         CVD   R5,DUB                                                           
         ZAP   WORK(11),DUB                                                     
         MP    WORK(11),=P'1000000'                                             
         DP    WORK(11),=P'23000'  / TAX RATE                                   
         ZAP   DUB,WORK(8)                                                      
         SRP   DUB,63,5            ROUND IT                                     
         CVB   R5,DUB                                                           
*&&DO                                                                           
         M     R4,=F'1000000'                                                   
         D     R4,=F'11265'        / TAX RATE                                   
         XR    R4,R4                                                            
         D     R4,=F'5'            ROUND IT                                     
         LTR   R5,R5                                                            
         BM    *+8                                                              
         AH    R5,=H'1'                                                         
         SRA   R5,1                R5=PAYMENT AMOUNT                            
*&&                                                                             
         ST    R5,PYMTAMT                                                       
         LR    RE,R5                                                            
         A     RE,TOTPYMT                                                       
         ST    RE,TOTPYMT                                                       
         LR    RE,R5                                                            
         A     RE,GROSSTOT         ADD TO GROSS TOTAL                           
         ST    RE,GROSSTOT                                                      
*                                                                               
         LA    R2,P+6                                                           
         GOTO1 =A(TAG2),DMCB,XAGR1AS       <gro_1_incamt>                       
         EDIT  PYMTAMT,(11,(R2)),2,FILL=0   GROSS AMOUNT                        
         LA    R2,11(R2)                                                        
         GOTO1 =A(TAG2),DMCB,XAGR1AE       </gro_1_incamt>                      
         BRAS  RE,SPLAT2                                                        
*                                                                               
         LA    R2,P+6                                                           
         GOTO1 =A(TAG2),DMCB,XANT1AS       <nr_tx_1_amt>                        
         EDIT  TAXAMT,(11,(R2)),2,FILL=0   TAX AMOUNT                           
         LA    R2,11(R2)                                                        
         GOTO1 =A(TAG2),DMCB,XANT1AE       </nr_tx_1_amt>                       
         BRAS  RE,SPLAT2                                                        
*                                                                               
         LA    R2,P+6                                                           
         GOTO1 =A(TAG2),DMCB,XNR4AE        </NR4_AMT>                           
         BRAS  RE,SPLAT2                                                        
*                                                                               
         LA    R2,P+6                                                           
         GOTO1 =A(TAG2),DMCB,XRPTCDS       <rpt_tcd>                            
         MVI   0(R2),C'O'                  ORIGINALS                            
         TM    OPTS,OPTAMEND               AMEND FORMS?                         
         BZ    *+8                                                              
         MVI   0(R2),C'A'                  AMENDED FORMS                        
         LA    R2,1(R2)                                                         
         GOTO1 =A(TAG2),DMCB,XRPTCDE       </rpt_tcd>                           
         BRAS  RE,SPLAT2                                                        
*                                                                               
         LA    R2,P+4                                                           
         GOTO1 =A(TAG2),DMCB,XSLIPE        </NR4Slip>                           
         BRAS  RE,SPLAT2                                                        
*                                                                               
         LH    RE,SLIPTOT          KEEP TRACK OF SLIP RECS                      
         AHI   RE,1                                                             
         STH   RE,SLIPTOT                                                       
*                                                                               
PRCSLIPX XIT1                                                                   
         LTORG                                                                  
MYSPACES DC    CL132' '                                                         
XSLIPS   DC    AL1(08),C'<NR4Slip>'                                             
XSLIPE   DC    AL1(09),C'</NR4Slip>'                                            
XRCPNS   DC    AL1(09),C'<RCPNT_NM>'                                            
XRCPNE   DC    AL1(10),C'</RCPNT_NM>'                                           
XRSNMS   DC    AL1(06),C'  <snm>'                                               
XRSNME   DC    AL1(05),C'</snm>'                                                
XRGNMS   DC    AL1(09),C'  <gvn_nm>'                                            
XRGNME   DC    AL1(08),C'</gvn_nm>'                                             
XRINTS   DC    AL1(07),C'  <init>'                                              
XRINTE   DC    AL1(06),C'</init>'                                               
X2RPNS   DC    AL1(13),C'<SEC_RCPNT_NM>'                                        
X2RPNE   DC    AL1(14),C'</SEC_RCPNT_NM>'                                       
XR2SNS   DC    AL1(10),C'  <sec_snm>'                                           
XR2SNE   DC    AL1(09),C'</sec_snm>'                                            
XR2GNS   DC    AL1(13),C'  <sec_gvn_nm>'                                        
XR2GNE   DC    AL1(12),C'</sec_gvn_nm>'                                         
XR2INS   DC    AL1(11),C'  <sec_init>'                                          
XR2INE   DC    AL1(10),C'</sec_init>'                                           
XENTNS   DC    AL1(10),C'<ENTPRS_NM>'                                           
XENTNE   DC    AL1(11),C'</ENTPRS_NM>'                                          
XEL1NS   DC    AL1(08),C'  <l1_nm>'                                             
XEL1NE   DC    AL1(07),C'</l1_nm>'                                              
XEL2NS   DC    AL1(08),C'  <l2_nm>'                                             
XEL2NE   DC    AL1(07),C'</l2_nm>'                                              
XRADDS   DC    AL1(11),C'<RCPNT_ADDR>'                                          
XRADDE   DC    AL1(12),C'</RCPNT_ADDR>'                                         
XRAL1S   DC    AL1(14),C'  <addr_l1_txt>'                                       
XRAL1E   DC    AL1(13),C'</addr_l1_txt>'                                        
XRAL2S   DC    AL1(14),C'  <addr_l2_txt>'                                       
XRAL2E   DC    AL1(13),C'</addr_l2_txt>'                                        
XRACTS   DC    AL1(09),C'  <cty_nm>'                                            
XRACTE   DC    AL1(08),C'</cty_nm>'                                             
XRASTS   DC    AL1(09),C'  <ste_cd>'                                            
XRASTE   DC    AL1(08),C'</ste_cd>'                                             
XRACNS   DC    AL1(11),C'  <cntry_cd>'                                          
XRACNE   DC    AL1(10),C'</cntry_cd>'                                           
XRAFPS   DC    AL1(14),C'  <fgn_pstl_cd>'                                       
XRAFPE   DC    AL1(13),C'</fgn_pstl_cd>'                                        
XTCCDS   DC    AL1(12),C'<tx_cntry_cd>'                                         
XTCCDE   DC    AL1(13),C'</tx_cntry_cd>'                                        
XFSSNS   DC    AL1(09),C'<fssn_nbr>'                                            
XFSSNE   DC    AL1(10),C'</fssn_nbr>'                                           
XNRANS   DC    AL1(12),C'<nr_acct_nbr>'                                         
XNRANE   DC    AL1(13),C'</nr_acct_nbr>'                                        
XRCPCS   DC    AL1(10),C'<rcpnt_tcd>'                                           
XRCPCE   DC    AL1(11),C'</rcpnt_tcd>'                                          
XPYRNS   DC    AL1(09),C'<payr_nbr>'                                            
XPYRNE   DC    AL1(10),C'</payr_nbr>'                                           
XINC1CS  DC    AL1(10),C'<inc_1_tcd>'                                           
XINC1CE  DC    AL1(11),C'</inc_1_tcd>'                                          
XCUR1CS  DC    AL1(10),C'<crcy_1_cd>'                                           
XCUR1CE  DC    AL1(11),C'</crcy_1_cd>'                                          
XNR4AS   DC    AL1(08),C'<NR4_AMT>'                                             
XNR4AE   DC    AL1(09),C'</NR4_AMT>'                                            
XAGR1AS  DC    AL1(15),C'  <gro_1_incamt>'                                      
XAGR1AE  DC    AL1(14),C'</gro_1_incamt>'                                       
XANT1AS  DC    AL1(14),C'  <nr_tx_1_amt>'                                       
XANT1AE  DC    AL1(13),C'</nr_tx_1_amt>'                                        
XAGR2AS  DC    AL1(15),C'  <gro_2_incamt>'                                      
XAGR2AE  DC    AL1(14),C'</gro_2_incamt>'                                       
XANT2AS  DC    AL1(14),C'  <nr_tx_2_amt>'                                       
XANT2AE  DC    AL1(13),C'</nr_tx_2_amt>'                                        
XTXM1CS  DC    AL1(13),C'<tx_xmpt_1_cd>'                                        
XTXM1CE  DC    AL1(13),C'</tx_xmpt_1_cd>'                                       
XINC2CS  DC    AL1(10),C'<inc_2_tcd>'                                           
XINC2CE  DC    AL1(11),C'</inc_2_tcd>'                                          
XCUR2CS  DC    AL1(10),C'<crcy_2_cd>'                                           
XCUR2CE  DC    AL1(11),C'</crcy_2_cd>'                                          
XTXM2CS  DC    AL1(13),C'<tx_xmpt_2_cd>'                                        
XTXM2CE  DC    AL1(13),C'</tx_xmpt_2_cd>'                                       
XRPTCDS  DC    AL1(08),C'<rpt_tcd>'                                             
XRPTCDE  DC    AL1(09),C'</rpt_tcd>'                                            
         EJECT                                                                  
*======================================================================         
*              ROUTINE PUTS SUMMARY PART TO TAPE                                
*======================================================================         
         SPACE                                                                  
PRCSUMM  NTR1  BASE=*,LABEL=*                                                   
         LA    R2,P+4                                                           
         GOTO1 =A(TAG2),DMCB,XSUMMS        <NR4Summary>                         
         BRAS  RE,SPLAT2                                                        
*                                                                               
         LA    R2,P+6                                                           
         GOTO1 =A(TAG2),DMCB,XSANBRS       <nr_acct_nbr>                        
         MVC   0(9,R2),=CL9'NRJ692173'                                          
         LA    R2,9(R2)                                                         
         GOTO1 =A(TAG2),DMCB,XSANBRE       </nr_acct_nbr>                       
         BRAS  RE,SPLAT2                                                        
*                                                                               
         LA    R2,P+6                                                           
         GOTO1 =A(TAG2),DMCB,XSPYRNS       <PAYR_NM>                            
         BRAS  RE,SPLAT2                                                        
*                                                                               
         LA    R2,P+6                                                           
         GOTO1 =A(TAG2),DMCB,XSPL1NS       <l1_nm>                              
         MVC   0(30,R2),=CL30'Talent Partners Com Serv, LLC'                    
         LA    R2,30(R2)                                                        
         GOTO1 =A(TAG2),DMCB,XSPL1NE       </l1_nm>                             
         BRAS  RE,SPLAT2                                                        
*                                                                               
         LA    R2,P+6                                                           
         GOTO1 =A(TAG2),DMCB,XSPYRNE       </PAYR_NM>                           
         BRAS  RE,SPLAT2                                                        
*                                                                               
         LA    R2,P+6                                                           
         GOTO1 =A(TAG2),DMCB,XSPYRAS       <PAYR_ADDR>                          
         BRAS  RE,SPLAT2                                                        
*                                                                               
         LA    R2,P+6                                                           
         GOTO1 =A(TAG2),DMCB,XSPAL1S       <addr_l1_txt>                        
         MVC   0(L'EMPADD,R2),EMPADD                                            
         LA    R2,L'EMPADD(R2)                                                  
         GOTO1 =A(TAG2),DMCB,XSPAL1E       </addr_l1_txt>                       
         BRAS  RE,SPLAT2                                                        
*                                                                               
         LA    R2,P+6                                                           
         GOTO1 =A(TAG2),DMCB,XSPACYS       <cty_nm>                             
         MVC   0(L'EMPCITY,R2),EMPCITY                                          
         LA    R2,L'EMPCITY(R2)                                                 
         GOTO1 =A(TAG2),DMCB,XSPACYE       </cty_nm>                            
         BRAS  RE,SPLAT2                                                        
*                                                                               
         LA    R2,P+6                                                           
         GOTO1 =A(TAG2),DMCB,XSPAPVS       <prov_cd>                            
         MVC   0(L'EMPSTATE,R2),EMPSTATE                                        
         LA    R2,L'EMPSTATE(R2)                                                
         GOTO1 =A(TAG2),DMCB,XSPAPVE       </prov_cd>                           
         BRAS  RE,SPLAT2                                                        
*                                                                               
         LA    R2,P+6                                                           
         GOTO1 =A(TAG2),DMCB,XSPACNS       <cntry_cd>                           
         MVC   0(3,R2),=C'USA'                                                  
         LA    R2,3(R2)                                                         
         GOTO1 =A(TAG2),DMCB,XSPACNE       </cntry_cd>                          
         BRAS  RE,SPLAT2                                                        
*                                                                               
         LA    R2,P+6                                                           
         GOTO1 =A(TAG2),DMCB,XSPAPSS       <pstl_cd>                            
         MVC   0(L'EMPZIP,R2),EMPZIP                                            
         LA    R2,L'EMPZIP(R2)                                                  
         GOTO1 =A(TAG2),DMCB,XSPAPSE       </pstl_cd>                           
         BRAS  RE,SPLAT2                                                        
*                                                                               
         LA    R2,P+6                                                           
         GOTO1 =A(TAG2),DMCB,XSPYRAE       </PAYR_ADDR>                         
         BRAS  RE,SPLAT2                                                        
*                                                                               
         LA    R2,P+6                                                           
         GOTO1 =A(TAG2),DMCB,XSCNTCS       <CNTC>                               
         BRAS  RE,SPLAT2                                                        
*                                                                               
         LA    R2,P+6                                                           
         GOTO1 =A(TAG2),DMCB,XSCNMS        <cntc_nm>                            
         MVC   0(L'XCONTACT,R2),XCONTACT                                        
         LA    R2,L'XCONTACT(R2)                                                
         GOTO1 =A(TAG2),DMCB,XSCNME        </cntc_nm>                           
         BRAS  RE,SPLAT2                                                        
*                                                                               
         LA    R2,P+6                                                           
         GOTO1 =A(TAG2),DMCB,XSCACDS       <cntc_area_cd>                       
         MVC   0(L'XAREA,R2),XAREA                                              
         LA    R2,L'XAREA(R2)                                                   
         GOTO1 =A(TAG2),DMCB,XSCACDE       </cntc_area_cd>                      
         BRAS  RE,SPLAT2                                                        
*                                                                               
         LA    R2,P+6                                                           
         GOTO1 =A(TAG2),DMCB,XSCPHNS       <cntc_phn_nbr>                       
         MVC   0(3,R2),XPHONE                                                   
         MVI   3(R2),C'-'                                                       
         MVC   4(4,R2),XPHONE+3                                                 
         LA    R2,L'XPHONE+1(R2)                                                
         GOTO1 =A(TAG2),DMCB,XSCPHNE       </cntc_phn_nbr>                      
         BRAS  RE,SPLAT2                                                        
*                                                                               
         LA    R2,P+6                                                           
         GOTO1 =A(TAG2),DMCB,XSCNTCE       </CNTC>                              
         BRAS  RE,SPLAT2                                                        
*                                                                               
         LA    R2,P+6                                                           
         GOTO1 =A(TAG2),DMCB,XSTXYRS       <tx_yr>                              
         MVC   0(L'SNRYEAR,R2),SNRYEAR                                          
         LA    R2,L'SNRYEAR(R2)                                                 
         GOTO1 =A(TAG2),DMCB,XSTXYRE       </tx_yr>                             
         BRAS  RE,SPLAT2                                                        
*                                                                               
         LA    R2,P+6                                                           
         GOTO1 =A(TAG2),DMCB,XSSLPCS       <slp_cnt>                            
         EDIT  SLIPTOT,(7,(R2)),FILL=0                                          
         LA    R2,7(R2)                                                         
         GOTO1 =A(TAG2),DMCB,XSSLPCE       </slp_cnt>                           
         BRAS  RE,SPLAT2                                                        
*&&DO                                                                           
         LA    R2,P+6                                                           
         GOTO1 =A(TAG2),DMCB,XSRMTCS       <rmt_tcd>                            
         MVI   0(R2),C'1'                  PAYER                                
         LA    R2,1(R2)                                                         
         GOTO1 =A(TAG2),DMCB,XSRMTCE       </rmt_tcd>                           
         BRAS  RE,SPLAT2                                                        
*&&                                                                             
         LA    R2,P+6                                                           
         GOTO1 =A(TAG2),DMCB,XSRPTCS       <rpt_tcd>                            
         MVI   0(R2),C'O'                                                       
         TM    OPTS,OPTAMEND               AMENDED FORMS                        
         BZ    *+8                                                              
         MVI   0(R2),C'A'                  AMENDED FORM                         
         LA    R2,1(R2)                                                         
         GOTO1 =A(TAG2),DMCB,XSRPTCE       </rpt_tcd>                           
         BRAS  RE,SPLAT2                                                        
*                                                                               
         LA    R2,P+6                                                           
         GOTO1 =A(TAG2),DMCB,XSAMTS        <NR4_TAMT>                           
         BRAS  RE,SPLAT2                                                        
*                                                                               
         LA    R2,P+6                                                           
         GOTO1 =A(TAG2),DMCB,XSAGR1AS      <tot_gro_1_incamt>                   
         EDIT  TOTPYMT,(13,(R2)),2,FILL=0                                       
         LA    R2,13(R2)                                                        
         GOTO1 =A(TAG2),DMCB,XSAGR1AE      </tot_gro_1_incamt>                  
         BRAS  RE,SPLAT2                                                        
*                                                                               
         LA    R2,P+6                                                           
         GOTO1 =A(TAG2),DMCB,XSANT1AS      <tot_nr_tx_1_amt>                    
         EDIT  TOTTAX,(13,(R2)),2,FILL=0                                        
         LA    R2,13(R2)                                                        
         GOTO1 =A(TAG2),DMCB,XSANT1AE      </tot_nr_tx_1_amt>                   
         BRAS  RE,SPLAT2                                                        
*                                                                               
         LA    R2,P+6                                                           
         GOTO1 =A(TAG2),DMCB,XSAMTE        </NR4_TAMT>                          
         BRAS  RE,SPLAT2                                                        
*                                                                               
         LA    R2,P+4                                                           
         GOTO1 =A(TAG2),DMCB,XSUMME        </NR4Summary>                        
         BRAS  RE,SPLAT2                                                        
*                                                                               
         LA    R2,P+3                                                           
         GOTO1 =A(TAG2),DMCB,XSNR4E        </NR4>                               
         BRAS  RE,SPLAT2                                                        
*                                                                               
         LA    R2,P+2                                                           
         GOTO1 =A(TAG2),DMCB,XRTRNE        </Return>                            
         BRAS  RE,SPLAT2                                                        
*                                                                               
         LA    R2,P                                                             
         GOTO1 =A(TAG2),DMCB,XSUBME        </Submission>                        
         BRAS  RE,SPLAT2                                                        
*                                                                               
PRCSUMMX XIT1                                                                   
         LTORG                                                                  
XCONTACT DC    CL15'Julie Hegelmann' CONTACT NAME                               
XAREA    DC    C'312'              CONTACT AREA CODE                            
XPHONE   DC    C'9237900'          CONTACT PHONE NUMBER                         
*                                                                               
XSUMMS   DC    AL1(11),C'<NR4Summary>'                                          
XSUMME   DC    AL1(12),C'</NR4Summary>'                                         
XSNR4E   DC    AL1(05),C'</NR4>'                                                
XSANBRS  DC    AL1(12),C'<nr_acct_nbr>'                                         
XSANBRE  DC    AL1(13),C'</nr_acct_nbr>'                                        
XSPYRNS  DC    AL1(08),C'<PAYR_NM>'                                             
XSPYRNE  DC    AL1(09),C'</PAYR_NM>'                                            
XSPL1NS  DC    AL1(08),C'  <l1_nm>'                                             
XSPL1NE  DC    AL1(07),C'</l1_nm>'                                              
XSPL2NS  DC    AL1(08),C'  <l2_nm>'                                             
XSPL2NE  DC    AL1(07),C'</l2_nm>'                                              
XSPL3NS  DC    AL1(08),C'  <l3_nm>'                                             
XSPL3NE  DC    AL1(07),C'</l3_nm>'                                              
XSPYRAS  DC    AL1(10),C'<PAYR_ADDR>'                                           
XSPYRAE  DC    AL1(11),C'</PAYR_ADDR>'                                          
XSPAL1S  DC    AL1(14),C'  <addr_l1_txt>'                                       
XSPAL1E  DC    AL1(13),C'</addr_l1_txt>'                                        
XSPAL2S  DC    AL1(14),C'  <addr_l2_txt>'                                       
XSPAL2E  DC    AL1(13),C'<addr_l2_txt>'                                         
XSPACYS  DC    AL1(09),C'  <cty_nm>'                                            
XSPACYE  DC    AL1(08),C'</cty_nm>'                                             
XSPAPVS  DC    AL1(10),C'  <prov_cd>'                                           
XSPAPVE  DC    AL1(09),C'</prov_cd>'                                            
XSPACNS  DC    AL1(11),C'  <cntry_cd>'                                          
XSPACNE  DC    AL1(10),C'</cntry_cd>'                                           
XSPAPSS  DC    AL1(10),C'  <pstl_cd>'                                           
XSPAPSE  DC    AL1(09),C'</pstl_cd>'                                            
XSCNTCS  DC    AL1(05),C'<CNTC>'                                                
XSCNTCE  DC    AL1(06),C'</CNTC>'                                               
XSCNMS   DC    AL1(10),C'  <cntc_nm>'                                           
XSCNME   DC    AL1(09),C'</cntc_nm>'                                            
XSCACDS  DC    AL1(15),C'  <cntc_area_cd>'                                      
XSCACDE  DC    AL1(14),C'</cntc_area_cd>'                                       
XSCPHNS  DC    AL1(15),C'  <cntc_phn_nbr>'                                      
XSCPHNE  DC    AL1(14),C'</cntc_phn_nbr>'                                       
XSCEXNS  DC    AL1(16),C'  <cntc_extn_nbr>'                                     
XSCEXNE  DC    AL1(15),C'</cntc_extn_nbr>'                                      
XSTXYRS  DC    AL1(06),C'<tx_yr>'                                               
XSTXYRE  DC    AL1(07),C'</tx_yr>'                                              
XSSLPCS  DC    AL1(08),C'<slp_cnt>'                                             
XSSLPCE  DC    AL1(09),C'</slp_cnt>'                                            
XSRMTCS  DC    AL1(08),C'<rmt_tcd>'                                             
XSRMTCE  DC    AL1(09),C'</rmt_tcd>'                                            
XSRPTCS  DC    AL1(08),C'<rpt_tcd>'                                             
XSRPTCE  DC    AL1(09),C'</rpt_tcd>'                                            
XSAMTS   DC    AL1(09),C'<NR4_TAMT>'                                            
XSAMTE   DC    AL1(10),C'</NR4_TAMT>'                                           
XSAGR1AS DC    AL1(19),C'  <tot_gro_1_incamt>'                                  
XSAGR1AE DC    AL1(18),C'</tot_gro_1_incamt>'                                   
XSANT1AS DC    AL1(18),C'  <tot_nr_tx_1_amt>'                                   
XSANT1AE DC    AL1(17),C'</tot_nr_tx_1_amt>'                                    
XSAGR2AS DC    AL1(19),C'  <tot_gro_2_incamt>'                                  
XSAGR2AE DC    AL1(18),C'</tot_gro_2_incamt>'                                   
XSANT2AS DC    AL1(18),C'  <tot_nr_tx_2_amt>'                                   
XSANT2AE DC    AL1(17),C'</tot_nr_tx_2_amt>'                                    
XSARPTAS DC    AL1(18),C'  <tot_nrpt_incamt>'                                   
XSARPTAE DC    AL1(17),C'</tot_nrpt_incamt>'                                    
XSATAXAS DC    AL1(21),C'  <tot_nr_nrpt_tx_amt>'                                
XSATAXAE DC    AL1(20),C'</tot_nr_nrpt_tx_amt>'                                 
XRTRNE   DC    AL1(08),C'</Return>'                                             
XSUBME   DC    AL1(12),C'</Submission>'                                         
*                        0....+....+....+....+                                  
         EJECT                                                                  
*              ROUTINE TO CHECK IF STATE OF WORK IS CANADA                      
*              AND STATE OF RESIDENCE IS NOT CANADA                             
*              RETURNS CCEQ IF SHOULD HAVE HAD CN TAX WITHHELD                  
*              SAVES PAYMENT AMOUNT                                             
         SPACE 2                                                                
CHKSOW   NTR1  BASE=*,LABEL=*                                                   
         L     R4,TIAREC                                                        
         MVI   ELCODE,TACAELQ      GET CAST DETAILS ELEMENT                     
         BRAS  RE,GETEL                                                         
         BNE   CKSOWNO                                                          
         USING TACAD,R4                                                         
         CLC   TACAUNIT,=C'CN '    IS CN STATE OF WORK?                         
         BE    CKSOW05                                                          
         MVC   TGCTRY,=C'CA'                                                    
         GOTO1 TAXVAL,DMCB,(X'FF',TACAUNIT)                                     
         BNE   CKSOWNO                                                          
CKSOW05  CLC   =C'ACT',TACAUN      SKIP IF UNION ACTRA                          
         BE    CKSOWNO                                                          
         CLC   =C'UDA',TACAUN      UDA                                          
         BE    CKSOWNO                                                          
         CLC   =C'NON',TACAUN      OR NON CAN                                   
         BNE   CKSOW10                                                          
         CLC   =C'CAN',TACALOCL                                                 
         BE    CKSOWNO                                                          
         DROP  R4                                                               
*                                                                               
CKSOW10  L     R4,TIAREC                                                        
         MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS ELEMENT                  
         USING TAPDD,R4                                                         
         BRAS  RE,GETEL                                                         
         BNE   CKSOWNO                                                          
         CLI   TAPDW4TY,TAW4TYCA   EXCLUDE IF CANADIAN                          
         BE    CKSOWNO                                                          
         CLI   TAPDW4TY,TAW4TYCO   IF CORP, STATE OF RES MAY BE CANADA          
         BNE   CKSOW20                                                          
*                                                                               
         BRAS  RE,READCRP          OKAY IF CORP HAS INDIV ATTACHED              
         BNE   CKSOWNO                                                          
*                                                                               
CKSOW20  MVI   SOWCAN,C'Y'         SAVE STATUS, STATE OF WORK = CAN             
         L     R1,TAPDPAYI                                                      
         A     R1,TAPDPAYC                                                      
         CLI   TAPDLEN,TAPDLNQ                                                  
         BL    CKSOW30                                                          
         ICM   RF,15,TAPDTXNW                                                   
         AR    R1,RF                                                            
CKSOW30  ST    R1,PYMTAMT          SAVE PAYMENT AMOUNT                          
         TM    TAPDSTAT,TAPDSCAN   CAN DOLLAR INVOICE?                          
         BNO   CKSOWYES                                                         
         MVI   SVCURR,C'C'                                                      
         B     CKSOWYES                                                         
CKSOWYES XR    RC,RC                                                            
CKSOWNO  LTR   RC,RC                                                            
         XIT1                                                                   
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*              ROUTINE TO CHECK IF CORP HAS INDIVIDUAL ATTACHED                 
*              IF YES, CORP IS NOT A CANADIAN RESIDENT                          
*                                                                               
READCRP  NTR1  BASE=*,LABEL=*                                                   
         L     R4,TIAREC                                                        
         MVI   ELCODE,TATIELQ     LOOK FOR TAX ID ELEMENT                       
         USING TATID,R4                                                         
         BRAS  RE,GETEL           OKAY IF INDIVIDUAL ATTACHED TO CORP           
         BE    RDCRPY                                                           
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
         BRAS  RE,SETCHK                                                        
         GOTO1 HIGH                                                             
         BRAS  RE,SETTAL                                                        
         B     RDCRPN                                                           
*                                                                               
RDCRPYES MVC   KEY,TIKEY           RESTORE READ SEQUENCE FOR SYSIO              
         BRAS  RE,SETCHK                                                        
         GOTO1 HIGH                                                             
         BRAS  RE,SETTAL                                                        
         B     RDCRPY                                                           
*                                                                               
RDCRPY   XR    RC,RC                                                            
RDCRPN   LTR   RC,RC                                                            
         XIT1                                                                   
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*        SET SYSFIL/DIR TO CHECK FILE                                           
*                                                                               
         SPACE 1                                                                
SETCHK   NTR1  BASE=*,LABEL=*                                                   
         MVC   SYSFIL,=CL8'CHKFIL'                                              
         MVC   SYSDIR,=CL8'CHKDIR'                                              
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*        SET SYSFIL/DIR TO TALENT FILE                                          
*                                                                               
         SPACE 1                                                                
SETTAL   NTR1  BASE=*,LABEL=*                                                   
         MVC   SYSFIL,=CL8'TALFIL'                                              
         MVC   SYSDIR,=CL8'TALDIR'                                              
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*              ROUTINE TO SAVE PAYMENT AMOUNT FROM TAPD ELEMENT                 
*              IN CHECK RECORD                                                  
         SPACE 2                                                                
SAVPYMT  NTR1  BASE=*,LABEL=*                                                   
         L     R4,TIAREC                                                        
         MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS ELEMENT                  
         USING TAPDD,R4                                                         
         BRAS  RE,GETEL                                                         
         BNE   SVPYMTX                                                          
         L     R1,TAPDPAYI                                                      
         A     R1,TAPDPAYC                                                      
         CLI   TAPDLEN,TAPDLNQ                                                  
         BL    SAVPYMT5                                                         
         ICM   RF,15,TAPDTXNW                                                   
         AR    R1,RF                                                            
SAVPYMT5 ST    R1,PYMTAMT          SAVE PAYMENT AMOUNT                          
         TM    TAPDSTAT,TAPDSCAN   CAN DOLLAR INVOICE?                          
         BNO   SVPYMTX                                                          
         MVI   SVCURR,C'C'                                                      
         B     SVPYMTX                                                          
SVPYMTX  XIT1                                                                   
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*              DSECT TO COVER LOCAL WORKING STORAGE                             
         SPACE 1                                                                
LOCALD   DSECT                                                                  
DYNALLOC DS    A                   A(DYNALLOC)                                  
         SPACE                                                                  
TAXAMT   DS    F                   TAX AMOUNT                                   
TOTTAX   DS    F                   TOTAL TAX AMOUNT                             
PYMTAMT  DS    F                   PYMTAMT AMOUNT                               
TOTPYMT  DS    F                   TOTAL PAYMENT AMOUNT                         
GROSSTOT DS    F                                                                
SLIPTOT  DS    H                                                                
         SPACE                                                                  
OPTS     DS    XL1                 OPTIONS                                      
OPTW2    EQU   X'80'               READ W2 RECORDS (NOT CHECKS)                 
OPTNOTAP EQU   X'40'               DON'T GENERATE A TAPE                        
OPTTRACE EQU   X'20'               TRACE RECORDS                                
OPTAMEND EQU   X'10'               AMENDED FORMS                                
         SPACE                                                                  
STAT     DS    XL1                 STATUS                                       
STSORT   EQU   X'80'               SORT IS ACTIVE                               
         SPACE                                                                  
EMP      DS    CL3                 EMPLOYER                                     
EMPINFO  DS    0CL106              EMPLOYER INFO                                
EMPNAME  DS    CL36                EMPLOYER NAME                                
EMPADD   DS    CL30                EMPLOYER STREET ADDRESS                      
EMPCITY  DS    CL28                CITY                                         
EMPSTATE DS    CL2                 STATE                                        
EMPZIP   DS    CL10                ZIP                                          
         SPACE                                                                  
RPTINFO  DS    0CL68                                                            
RPTLNAM  DS    CL20                RECIPIENT LAST NAME                          
RPTFNAM  DS    CL12                RECIPIENT FIRST NAME                         
RPTMIDI  DS    CL1                 RECIPIENT MIDDLE INITIAL                     
CORPNAM  DS    CL35                CORPORATION NAME                             
PERFSSN  DS    CL9                 PERFORMER OR CORP SSN                        
RPTINFO2 DS    0CL100                                                           
RPTADD1  DS    CL30                RECIPIENT ADDRESS                            
RPTADD2  DS    CL30                                                             
RPTCITY  DS    CL28                RECIPIENT CITY                               
RPTSTATE DS    CL2                 RECIPIENT STATE                              
RPTZIP   DS    CL10                RECIPIENT ZIP CODE                           
         SPACE                                                                  
PERFCUR  DS    CL1                 PERFORMER'S CURRENCY                         
SVCURR   DS    CL1                 SAVED CURRENCY                               
SOWCAN   DS    CL1                 STATE OF WORK = CN                           
SVADJ    DS    CL1                 SAVED ADJUSTMENT PAY                         
SVKEY    DS    CL38                SAVED KEY                                    
         SPACE                                                                  
SORTREC  DS    CL(SORTLNQ)         SORT RECORD AREA                             
TAPEREC  DS    CL(TPLNQ)           TAPE RECORD AREA                             
ANEGPTR  DS    A                   ADDRESS OF EMPTY ENTRY                       
NEGTAB   DS    XL256               LIST OF 64 SSN W/ NEG TAX (BINARY)           
         EJECT                                                                  
         SPACE                                                                  
*              DSECT TO COVER TAPE RECORD                                       
TAPED    DSECT                                                                  
*                                  TRANSMITTER RECORD                           
TPTYP    DS    CL3                 TYPE CODE                                    
TPDYP    DS    CL1                 DATA TYPE CODE                               
TPTRANS  DS    CL8                 TRANSMITTER NUMBER                           
TPTRATYP DS    CL1                 TRANSMITTER TYPE INDICATOR                   
TPTOTSUM DS    CL6                 TOTAL NUMBER OF SUMMARY RECORDS              
TPTRNAM1 DS    CL30                TRANSMITTER NAME - LINE 1                    
TPTRNAM2 DS    CL30                TRANSMITTER NAME - LINE 2                    
TPTRADD1 DS    CL30                TRANSMITTER ADDRESS - LINE 1                 
TPTRADD2 DS    CL30                TRANSMITTER ADDRESS - LINE 2                 
TPTRCITY DS    CL28                TRANSMITTER CITY                             
TPTRPRTR DS    CL2                 TRANSMITTER PROVINCE OR TERRITORY            
TPTRCTRY DS    CL3                 TRANSMITTER COUNTRY CODE                     
TPTRPOST DS    CL10                TRANSMITTER POSTAL CODE                      
TPTCNAM  DS    CL22                TECHNICAL CONTACT NAME                       
TPTCARCD DS    CL3                 TECHNICAL CONTACT AREA CODE                  
TPTCPNUM DS    CL7                 TECHNICAL CONTACT PHONE NUMBER               
TPLANG   DS    CL1                 LANGUAGE OF CUMMUNICATION INDICATOR          
         DS    CL137               SPARE                                        
TPLNQ    EQU   *-TAPED                                                          
*                                                                               
         ORG   TPDYP               SLIP RECORD                                  
TPIRSNAM DS    CL20                INDIVIDUAL RECIPIENT SURNAME                 
TPIRFNAM DS    CL12                INDIVIDUAL RECIPIENT FIRST NAME              
TPIRINIT DS    CL1                 INDIVIDUAL RECIPIENT INITIAL                 
TPSRSNAM DS    CL20                2ND INDIVIDUAL RECIPIENT SURNAME             
TPSRFNAM DS    CL12                2ND INDIVIDUAL RECIPIENT FIRST NAME          
TPSRINIT DS    CL1                 2ND INDIVIDUAL RECIPIENT INITIAL             
TPCORP1  DS    CL30                CORPORATION, ORGANIZATION, ETC               
TPCORP2  DS    CL30                CORPORATION, ORGANIZTION, ETC LINE 2         
TPRADD1  DS    CL30                RECIPIENT ADDRESS - LINE 1                   
TPRADD2  DS    CL30                RECIPIENT ADDRESS - LINE 2                   
TPRCITY  DS    CL28                RECIPIENT CITY                               
TPRPRTE  DS    CL2                 RECIPIENT PROVINCE OR TERRITORY              
TPRCTRY  DS    CL3                 RECIPIENT COUNTRY CODE                       
TPRPOST  DS    CL10                RECIPIENT POSTAL CODE                        
TPTXCTY  DS    CL3                 TAX COUNTRY CODE                             
TPRFSSN  DS    CL20                RECIPIENT FSSN OR SIN                        
TPNRACC  DS    CL15                NON-RESIDENT ACCOUNT NUMBER                  
TPRTYPI  DS    CL1                 RECIPIENT TYPE INDICATOR                     
TPPRID   DS    CL20                PAYER OR REMITTER ID NUMBER                  
TPINCOME DS    CL2                 INCOME CODE                                  
TPCURCOD DS    CL3                 CURRENCY CODE                                
TPGROSS  DS    CL11                GROSS INCOME                                 
TPNONRES DS    CL11                NON-RESIDENT TAX WITHHELD                    
TPEXEMPT DS    CL1                 EXEMPTION CODE                               
TPINCOM2 DS    CL2                 INCOME CODE 2                                
TPCURCD2 DS    CL3                 CURRENCY CODE 2                              
TPGROSS2 DS    CL11                GROSS INCOME 2                               
TPNONRE2 DS    CL11                NON-RESIDENT TAX WITHHELD 2                  
TPEXEMP2 DS    CL1                 EXEMPTION CODE 2                             
         DS    CL5                 SPARE                                        
*                                                                               
         ORG   TPDYP               SUMMARY RECORD                               
TPNRANUM DS    CL15                NON-RESIDENT ACCOUNT NUMBER                  
TPPNAME1 DS    CL30                PAYER NAME - LINE 1                          
TPPNAME2 DS    CL30                PAYER NAME - LINE 2                          
TPPNAME3 DS    CL30                PAYER NAME - LINE 3                          
TPPADD1  DS    CL30                PAYER ADDRESS - LINE 1                       
TPPADD2  DS    CL30                PAYER ADDRESS - LINE 2                       
TPPCITY  DS    CL28                PAYER CITY                                   
TPPPRTR  DS    CL2                 PAYER PROVINCE OR TERRITORY                  
TPPCTRY  DS    CL3                 PAYER COUNTRY CODE                           
TPPOSTAL DS    CL10                PAYER POSTAL CODE                            
TPACCNAM DS    CL22                ACCOUNTING CONTACT NAME                      
TPACCAC  DS    CL3                 ACCOUNTING CONTACT AREA CODE                 
TPACNUM  DS    CL7                 ACCOUNTING CONTACT PHONE NUMBER              
TPTAXYR  DS    CL4                 TAXATION YEAR                                
TPNUMSLP DS    CL7                 TOTAL NUMBER OF NR4 SLIP RECORDS             
TPREMTR  DS    CL1                 REMITTER CODE                                
TPGRINC  DS    CL13                TOTAL GROSS INCOME                           
TPNRTW   DS    CL13                TOTAL NON-RESIDENT TAX WITHHELD              
TPGRINC2 DS    CL13                TOTAL GROSS INCOME 2                         
TPNRTW2  DS    CL13                TOTAL NON-RESIDENT TAX WITHHELD 2            
TPGNOREP DS    CL13                TOT GROSS INC NOT REPORTED ON SLIPS          
TPTNOREP DS    CL13                TOT NON-RES TAX W/H NOT REP ON SLIPS         
         DS    CL19                SPARE                                        
*                                                                               
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
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE DDCOMFACS                                                      
TWADCOND DSECT                                                                  
       ++INCLUDE DDTWADCONS                                                     
       ++INCLUDE TAREPWORKD                                                     
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'027TAREP3D   11/12/15'                                      
         END                                                                    
