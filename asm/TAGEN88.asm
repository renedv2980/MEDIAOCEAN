*          DATA SET TAGEN88    AT LEVEL 113 AS OF 11/03/11                      
*PHASE T70288A                                                                  
         TITLE 'T70288 - GRNTEE, ECAST, AND FIXED CYCLE TRCKING REPORT'         
T70288   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 TMPLNQ,T70288,R5,R6,CLEAR=YES                                    
         LR    RE,RC               RE=A(TEMP STOARGE FOR SPOOLING)              
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         LA    R7,TWAHOLE          R7=A(PROGRAM SAVED STORAGE)                  
         USING LOCALD,R7                                                        
         ST    RE,ATMPSTOR         SAVE RE IN SAVED STORAGE                     
         SPACE 3                                                                
         LA    R3,GTPFTAB          R3=A(PFKEY TABLE)                            
         CLI   RECNUM,GT           BASE ON RECORD TYPE                          
         BE    GT2                                                              
         LA    R3,FTPFTAB                                                       
         CLI   RECNUM,FT                                                        
         BE    GT2                                                              
         LA    R3,ETPFTAB                                                       
GT2      GOTO1 INITIAL,DMCB,(R3)                                                
*                                                                               
         CLI   RECNUM,FT           FTRACK                                       
         BE    GT05                                                             
         CLI   RECNUM,GT           GTRACK                                       
         BE    GT06                                                             
         CLI   RECNUM,ETR          ETRACK                                       
         BE    GT07                                                             
         BNE   GT08                                                             
GT05     MVC   SFTSHED(7),=C'Pid Num'                                           
         OI    SFTSHEDH+6,X'80'                                                 
         B     GT08                                                             
GT06     MVC   SGTSHED(7),=C'Pid Num'                                           
         OI    SGTSHEDH+6,X'80'                                                 
         B     GT08                                                             
GT07     MVC   SETSHED(7),=C'Pid Num'                                           
         OI    SETSHEDH+6,X'80'                                                 
*                                                                               
GT08     GOTO1 FLDVAL,DMCB,(2,SFTUSHDH),SFTUSEH                                 
         CLI   RECNUM,GT                                                        
         BE    *+12                                                             
         CLI   RECNUM,GX                                                        
         BNE   GT09                                                             
         GOTO1 FLDVAL,DMCB,(2,SGTAGYH),SGTCLINH                                 
         GOTO1 FLDVAL,DMCB,SGTCLHDH,(8,SGTCLINH)                                
                                                                                
GT09     CLI   RECNUM,GT           BASED ON RECORD TYPE                         
         BE    *+12                                                             
         CLI   RECNUM,GX                                                        
         BNE   GT10                                                             
         LA    R1,SGTFSTH          SET A(FIRST DATA LINE)                       
         ST    R1,AFSTH                                                         
         LA    R1,SGTLSTH          A(LAST DATA LINE)                            
         ST    R1,ALSTH                                                         
         LA    R1,GTVKEY           A(VALKEY ROUTINE)                            
         ST    R1,AVKEY                                                         
         MVI   RCSUBPRG,0          OFFLINE SPROG                                
         MVI   TIREAD,TLGTCDQ      RECORD TO READ FOR SYSIO                     
         B     GT20                                                             
         SPACE 1                                                                
GT10     CLI   RECNUM,FT                                                        
         BNE   GT15                                                             
         LA    R1,SFTFSTH          SET A(FIRST DATA LINE)                       
         ST    R1,AFSTH                                                         
         LA    R1,SFTLSTH          A(LAST DATA LINE)                            
         ST    R1,ALSTH                                                         
         LA    R1,FTVKEY           A(VALKEY ROUTINE)                            
         ST    R1,AVKEY                                                         
         MVI   RCSUBPRG,2          OFFLINE SPROG                                
         MVI   TIREAD,TLFTCDQ      RECORD TO READ FOR SYSIO                     
         B     GT20                                                             
         SPACE 1                                                                
GT15     LA    R1,SETFSTH          SET A(FIRST DATA LINE)                       
         ST    R1,AFSTH                                                         
         LA    R1,SETLSTH          A(LAST DATA LINE)                            
         ST    R1,ALSTH                                                         
         LA    R1,ETVKEY           A(VALKEY ROUTINE)                            
         ST    R1,AVKEY                                                         
         MVI   RCSUBPRG,4          OFFLINE SPROG                                
         MVI   TIREAD,TLETCDQ      RECORD TO READ FOR SYSIO                     
GT20     EQU   *                                                                
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
         SPACE 1                                                                
         CLI   MODE,VALKEY         VALIDATE KEY FIELDS                          
         BNE   GT30                                                             
         MVI   PGSW,0              INIT. PAGING SWITCH                          
         SPACE 1                                                                
         CLI   PFAID,7             IF PF7 PRESSED                               
         BNE   *+8                                                              
         MVI   PGSW,C'L'           SET TO DISPLAY LAST (PREV) PAGE              
         SPACE 1                                                                
         CLI   PFAID,8             IF PF8 PRESSED                               
         BNE   *+8                                                              
         MVI   PGSW,C'F'           SET TO START AT BEGINNING                    
         SPACE 1                                                                
         GOTO1 AVKEY               VALIDATE KEY FIELDS                          
         B     XIT                                                              
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES, CONT'D.                                
         SPACE 1                                                                
GT30     CLI   MODE,PRINTREP       PRINT REPORT                                 
         BNE   XIT                                                              
         L     R1,AFSTH                                                         
         L     RF,ALSTH                                                         
         TWAXC (R1),(RF),PROT=Y    CLEAR SCREEN                                 
         SPACE 1                                                                
         TM    WHEN,X'80'          IF WE'RE ON SCREEN                           
         BZ    GT34                                                             
         XC    DSPLINE,DSPLINE     SET TO START AT FIRST LINE                   
         LA    R2,SCHOOK           SET HOOK TO SYSIO                            
         B     GT40                                                             
         SPACE 1                                                                
GT34     MVI   PGSW,C'F'           SPOOLING - INSURE WE START AT BEG.           
         MVI   FRSTLINE,13         SET FIRST PRINT LINE                         
         LA    R2,MYSPECS                                                       
         ST    R2,SPECS            SAVE A(SPECS)                                
         LA    R2,HOOK                                                          
         ST    R2,HEADHOOK         AND A(HEADLINE HOOK)                         
         LA    R2,PRHOOK           USE SEPARATE I/O HOOK FOR SYSIO              
         SPACE 1                                                                
         L     R0,ATMPSTOR         INIT NEXT SLOT IN D/A TABLE                  
         ST    R0,ANXTDA                                                        
         SPACE 1                                                                
         CLI   OFFLINE,C'Y'        NEED HEADINGS UNDERLINED IF ONLINE           
         BE    GT40                                                             
         ZIC   R1,RCSUBPRG                                                      
         LA    R1,1(R1)            BUMP SPROG                                   
         STC   R1,RCSUBPRG                                                      
         ZIC   R1,MAXLINES                                                      
         LA    R1,1(R1)            BUMP MAXLINES (SO PGS MATCH OFFLINE)         
         STC   R1,MAXLINES                                                      
         MVI   FRSTLINE,14         FIRST PRINT LINE IS ONE LATER                
         SPACE 1                                                                
GT40     ST    R2,TIHOOK           SET A(SYSIO HOOK)                            
         ZIC   RE,MAXLINES         DETERMINE WHICH RECORD TO START WITH         
         ZIC   RF,FRSTLINE                                                      
         SR    RE,RF               RE=N'PRINT LINES PER PAGE                    
         STC   RE,NUMLINES         SAVE IT                                      
         MVC   TIACOMFC,ACOMFACS                                                
         SPACE 1                                                                
         MVC   LISTSW,PGSW         SET PAGE CONTROL SWITCH                      
         GOTO1 PGCNTL,DMCB,PGTBL,TIKEY,TIQSKEY  SET KEY                         
         SPACE 1                                                                
         CLI   RECNUM,GT           IF GTRACK/REPORT                             
         BE    *+12                                                             
         CLI   RECNUM,GX                                                        
         BNE   GT50                                                             
         OI    TIQFLAG2,TIQFNLIM   SKIP LIMIT CHECKS                            
         SPACE 1                                                                
GT50     GOTO1 TASYSIO,DMCB,TASYSIOD  OFF TO SYSIO TO READ RECORDS              
         SPACE 1                                                                
         TM    WHEN,X'80'          IF ON SCREEN, GIVE FINAL MESSAGE             
         BO    TRKDISP                                                          
         SPACE 1                                                                
         BAS   RE,PROCESS          PROCESS D/A TABLE                            
         SPACE 1                                                                
         TM    WHEN,X'40'          IF NOW                                       
         BZ    GTX                                                              
         XC    CONSERV,CONSERV     SET FOR AUTO $DQU                            
         MVC   CONSERV(4),=C'$DQU'                                              
         SPACE 1                                                                
GTX      B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE KEY FIELDS (GTRACK)                          
         SPACE 1                                                                
GTVKEY   NTR1                                                                   
         CLI   RECNUM,GX           IF AUTOMATIC GTX REQUESTS, THEN MOVE         
         BNE   GTVK2                OUT DATA FROM SPECIAL LONG FIELD            
         MVI   SGTSSNH+5,9                                                      
         MVC   SGTSSN,SGTLONG+16   SSN                                          
         MVI   SGTCODEH+5,4                                                     
         MVC   SGTCODE,SGTLONG+25  GUARANTEE CODE                               
         MVI   SGTPDH+5,9                                                       
         MVI   SGTPD,C'*'          CYCLE START DATE                             
         MVC   SGTPD+1(8),SGTLONG+29                                            
*                                                                               
*                                                                               
GTVK2    MVC   AIO,AIO2            INSURE S/S NUMBER ALWAYS IN I/O 2            
*                                                                               
         LA    R2,SGTSSNH          S/S NUM                                      
         CLI   SGTSSNH+5,0                                                      
         BE    GTVK2B                                                           
         CLI   SGTSSNH+5,9         MUST CHECK FOR LENGTH HERE SINCE             
         BE    GTVK2C              RECVAL CALL DOES NOT CHECK FOR               
         CLI   SGTSSNH+5,6         INVALID ENTRIES DUE TO X'40' PARAM           
         BNE   FLDINV                                                           
         MVC   TGPID,SGTSSN                                                     
GTVK2B   OC    TGPID,TGPID                                                      
         BZ    FLDMISS                                                          
         GOTO1 SSNUNPK,DMCB,TGPID,TGSSN                                         
         BNE   GTVK2C                                                           
         MVC   SGTSSN,TGSSN                                                     
         MVI   SGTSSNH+5,9                                                      
*                                                                               
GTVK2C   GOTO1 RECVAL,DMCB,TLW4CDQ,(X'0A',SGTSSNH),SGTSSNNH                     
         MVC   AIO,AIO1                                                         
*                                                                               
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   SGTSSN,SPACES                                                    
         MVC   SGTSSN(L'TGPID),TGPID                                            
         MVI   SGTSSNH+5,6                                                      
         OI    SGTSSNH+6,X'80'                                                  
*                                                                               
         GOTO1 FLDVAL,DMCB,(X'40',SGTSSNH),SGTPDH                               
         BE    XIT                                                              
         LH    RF,=AL2(TIEND-TASYSIOD)                                          
         XCEFL TASYSIOD,(RF)                                                    
         MVC   TIFSSN,TGSSN        SET FILTER FOR SYSIO                         
         MVC   SSNNAME,TGNAME      SAVE FULL NAME FOR PRINTED REPORT            
*                                                                               
         LA    R2,SGTCODEH         VALIDATE GUARANTEE CODE                      
         CLI   5(R2),0             TEST SOMETHING INPUT                         
         BNE   GTVK4                                                            
         OC    TGGUA,TGGUA         NO - SOMETHING IN GLOBAL                     
         BZ    GTVK3                                                            
         MVC   8(4,R2),TGGUA       YES - MOVE TO FIELD                          
         XC    8(4,R2),HEXFFS      AND UNCOMPLEMENT IT                          
         OI    6(R2),X'80'                                                      
         B     GTVK5                                                            
*                                                                               
GTVK3    GOTO1 ANY                 REQUIRE INPUT                                
*                                                                               
GTVK4    MVC   TGGUA,8(R2)         CODE INPUT - MOVE TO GLOBAL AND              
         XC    TGGUA,HEXFFS        COMPLEMENT IT                                
*                                                                               
GTVK5    GOTO1 RECVAL,DMCB,TLGUCDQ,(X'20',0)  VALIDATE IT                       
         BNE   THEEND                                                           
         MVC   TIFGUA,TGGUA        GUAR CODE IN TRACKING KEY                    
         XC    TIFGUA,HEXFFS       IS NOT COMPLEMENTED                          
*                                                                               
         USING TAGUD,R4            R4=A(GUARANTEE DETAILS EL.)                  
         L     R4,AIO                                                           
         MVI   ELCODE,TAGUELQ      SET TO GET GUAR. DETAILS EL.                 
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,CHKACC           CHECK IF STAFF HAS ACCESS TO GUAR            
*                                                                               
GTVK5B   MVC   AIO,AIO1            RESET IO AREA                                
*                                                                               
GTVK6    LA    R2,SGTPDH           R2=A(PERIOD FIELD)                           
*                                                                               
         OC    TAGUCOM,TAGUCOM     IF PRIMARY COMMERCIAL DEFINED                
         BZ    GTVK7                                                            
         MVI   ELCODE,TAGCELQ      SET TO LOOK UP GUAR CYCLE EL.                
         BAS   RE,VALPD            RECOGNIZE INPUT TO PERIOD FIELD              
         BNE   NOTRK                                                            
         L     R3,TGELEM           R3=A(CORRESPONDING GUAR CYCLE EL.)           
         USING TAGCD,R3                                                         
         MVC   TAGUPD,TAGCPD       SET PERIOD IN GUAR DETAILS EL.               
         MVC   TAGUAMT,TAGCAMT         AMOUNT                                   
         MVC   TAGUBAL,TAGCBAL         BALANCE                                  
*                                                                               
GTVK7    BAS   RE,GUDTLS           DISPLAY GUARANTEE DETAILS                    
*                                                                               
GTVK8    MVI   ADDSTAT,0                                                        
*                                                                               
         OC    TAGUINV,TAGUINV                                                  
         BNZ   GTVK9                                                            
         OI    ADDSTAT,ADDMANUL                                                 
*                                                                               
GTVK9    LA    R2,SGTUSEH          VALIDATE USE TYPE                            
         CLI   5(R2),0             TEST SOMETHING INPUT                         
         BE    GTVK10                                                           
         GOTO1 USEVAL,DMCB,(X'40',8(R2))                                        
         BNE   FLDINV                                                           
         MVC   TIFUSE,TGUSCDE                                                   
*                                                                               
GTVK10   XC    SGTAGYN,SGTAGYN     INITIALIZE AGENCY NAME FIELD                 
         OI    SGTAGYNH+6,X'80'                                                 
*                                                                               
         MVC   AGYNAME,SPACES      CLEAR AGENCY VARIABLES                       
         XC    AGYSTAT2,AGYSTAT2                                                
*                                                                               
         CLI   SGTAGYH+5,0         IF AGENCY FILTER IS NOT DEFINED              
         BNE   GTVK15                                                           
         MVC   AGYNAME,SPACES                                                   
         OC    TAGUAGY,TAGUAGY     AND GUARANTEE NOT SET UP FOR                 
         BZ    GTVK20                                                           
         CLI   SGTAGY,0            ALL AGENCIES                                 
         BE    GTVK20                                                           
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'A0',TAGUAGY)                              
         BNE   GTVK20                                                           
         BAS   RE,AGYDTLS          EXTRACT AGENCY DETAILS                       
         B     GTVK20                                                           
*                                                                               
GTVK15   CLI   SGTAGY+5,C'*'                                                    
         BNE   *+8                                                              
         MVI   SGTAGY+5,C' '                                                    
         GOTO1 RECVAL,DMCB,(X'80',TLAYCDQ),(8,SGTAGYH),SGTAGYNH                 
         BAS   RE,AGYDTLS          EXTRACT AGENCY DETAILS                       
*                                                                               
GTVK20   BAS   RE,INIT             RE-INITIALIZE LIST                           
*                                                                               
GTVKX    GOTO1 FLDVAL,DMCB,(X'20',SGTSSNH),SGTPDH                               
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO EXTRACT GUARANTEE RECORD DETAILS                      
         SPACE 1                                                                
*                                  AIO = A(GUARANTEE RECORD)                    
         USING TAGUD,R4            R4=A(GUARANTEE DETAILS EL.)                  
GUDTLS   NTR1                                                                   
         MVC   AIO,AIO3            SET ALT. I/O AREA FOR READS                  
*&&DO                                                                           
* Removed as per bug from June 20, 2008                                         
         MVC   SGTAGY,TAGUAGY      AGENCY                                       
         OI    SGTAGYH+6,X'80'                                                  
         XC    SGTAGYN,SGTAGYN     PRE-CLEAR NAME                               
         OI    SGTAGYNH+6,X'80'                                                 
         MVC   AGYNAME,SPACES                                                   
         XC    AGYSTAT2,AGYSTAT2   AND STATUS                                   
*&&                                                                             
         CLI   SGTAGY,0            IT MAY NOT BE AROUND                         
         BE    GUD2                                                             
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'88',TAGUAGY),SGTAGYNH                     
         LA    R2,SGTAGYH                                                       
         BNE   FLDINV                                                           
         BAS   RE,AGYDTLS          EXTRACT AGENCY DETAILS                       
         SPACE 1                                                                
GUD2     MVC   SGTCLI,TAGUCLI      CLIENT                                       
         OI    SGTCLIH+6,X'80'                                                  
         XC    SGTCLIN,SGTCLIN     PRE-CLEAR NAME                               
         OI    SGTCLINH+6,X'80'                                                 
         MVC   CLINAME,SPACES                                                   
         CLI   SGTCLI,0            IT MAY NOT BE AROUND                         
         BE    GUD4                                                             
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'88',SGTCLI),SGTCLINH                      
         GOTO1 CHAROUT,DMCB,TANAELQ,0                                           
         MVC   CLINAME,TGNAME                                                   
         SPACE 1                                                                
GUD4     MVC   CRPNAME,SPACES      PRE-CLEAR CORP INFO                          
         XC    CRPCODE,CRPCODE                                                  
         CLI   TAGUCRP,0           IF WE HAVE CORP CODE                         
         BE    GUD6                                                             
         MVC   AIO,AIO2            SET I/O AREA WITH W4 RECORD                  
         MVI   ELCODE,TATIELQ      SET TO LOOK UP CORP ID ELEMENT               
         MVI   HALF,TATITYCO       SCAN FOR CORP ID ELEMENT TYPE                
         MVC   HALF+1(1),TAGUCRP   AND FOR THIS CORP CODE                       
         GOTO1 GETL,DMCB,(2,HALF)                                               
         MVC   AIO,AIO3            RESTORE ALT. I/O AREA                        
         BNE   GUD6                                                             
         L     R2,TGELEM           R2=A(TAX ID EL. FOR CORP)                    
         USING TATID,R2                                                         
         MVC   CRPCODE,TATIID                                                   
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'A0',TATIID)  READ CORP ID RECORD          
         MVC   TGSSN,TIFSSN        (RESTORE ACTUAL SSN TO GLOBAL)               
         BNE   GUD6                                                             
         GOTO1 CHAROUT,DMCB,TANAELQ,0              EXTRACT NAME                 
         MVC   CRPNAME,TGNAME                      SAVE IN LOCAL W/S            
         SPACE 1                                                                
GUD6     MVI   DBAL,C'N'           SET STATUS SWITCHES FOR HEADLINES            
         TM    TAGUSTAT,TAGUSDES                                                
         BZ    *+8                                                              
         MVI   DBAL,C'Y'           DESCENDING BALANCE                           
         SPACE 1                                                                
         MVI   PAYPNH,C'N'                                                      
         TM    TAGUSTAT,TAGUSPNH                                                
         BZ    *+8                                                              
         MVI   PAYPNH,C'Y'         PAY P&H                                      
         SPACE 1                                                                
         MVI   PAYOVER,C'N'                                                     
         TM    TAGUSTAT,TAGUSOVR                                                
         BZ    *+8                                                              
         MVI   PAYOVER,C'Y'        PAY OVERAGE                                  
         SPACE 1                                                                
         GOTO1 DATCON,DMCB,(X'11',TAGUPD),(8,SGTPD)  PERIOD                     
         OI    SGTPDH+6,X'80'                                                   
         MVC   DPERIOD,SGTPD       SAVE FOR PRINTING                            
         SPACE 1                                                                
         EDIT  (4,TAGUAMT),(12,DAMOUNT),2,MINUS=YES            AMOUNT           
         EDIT  (4,TAGUAMT),(12,SGTAMT),2,MINUS=YES,ALIGN=LEFT                   
         OI    SGTAMTH+6,X'80'                                                  
         SPACE 1                                                                
         EDIT  (4,TAGUBAL),(12,DBALANCE),2,MINUS=YES           BALANCE          
         EDIT  (4,TAGUBAL),(12,SGTBAL),2,MINUS=YES,ALIGN=LEFT                   
         OI    SGTBALH+6,X'80'                                                  
         NI    SGTBALH+1,X'F3'     SET TO NORMAL INTENSITY                      
         TM    TAGUBAL,X'80'       IF BALANCE IS NEGATIVE                       
         BZ    *+8                                                              
         OI    SGTBALH+1,X'08'     SET TO HIGH INTENSITY                        
         SPACE 1                                                                
         MVC   SAVEAGY,TGAGY                                                    
         MVC   GUARCMT1,SPACES                                                  
         MVC   GUARCMT2,SPACES                                                  
         SPACE 1                                                                
         XC    TGAGY,TGAGY         READ FOR ATTACHED COMMENT                    
         MVI   TGTYPE,TLCMTGUA                                                  
         GOTO1 RECVAL,DMCB,TLCMCDQ,(X'A4',TGTYPE)                               
         BNE   GUD7                                                             
         SPACE 1                                                                
         USING TAXCD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAXCELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   GUD7                                                             
         ZIC   RE,TAXCLEN         IF FOUND, SAVE FIRST LINE                     
         SHI   RE,5                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   GUARCMT1(0),TAXCCMNT                                             
         BRAS  RE,NEXTEL                                                        
         BNE   GUD7                                                             
         ZIC   RE,TAXCLEN         AND SECOND LINE                               
         SHI   RE,5                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   GUARCMT2(0),TAXCCMNT                                             
         DROP  R4                                                               
         SPACE 1                                                                
GUD7     MVC   TGAGY,SAVEAGY       RESTORE GLOBAL AGENCY                        
         MVC   AIO,AIO1            AND GUAR. RECORD I/O AREA                    
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE KEY FIELDS (FTRACK)                          
         SPACE 1                                                                
FTVKEY   NTR1                                                                   
         CLI   OFFLINE,C'Y'        IF RUNNING OFFLINE                           
         BNE   FTVK2                                                            
         CLI   SFTAGYH+5,0         AND AGENCY FIELD EMPTY                       
         BNE   FTVK2                                                            
         BAS   RE,SCRNGEN          TRANSLATE REQUEST TO SCREEN                  
         SPACE 1                                                                
FTVK2    CLI   SCRSTAT,0           IF SCREEN CHANGED                            
         BNE   FTVK2A                                                           
         TM    SFTSSNH+4,X'20'     OR SSN HAS CHANGED                           
         BZ    FTVK2A                                                           
         TM    SFTUSEH+4,X'20'     OR USE HAS CHANGED                           
         BO    FTVK4                                                            
*                                                                               
FTVK2A   LA    R2,SFTSSNH          S/S NUM                                      
         CLI   SFTSSNH+5,0                                                      
         BE    FLDMISS                                                          
         CLI   SFTSSNH+5,9         MUST CHECK FOR LENGTH HERE SINCE             
         BE    FTVK3               RECVAL CALL DOES NOT CHECK FOR               
         CLI   SFTSSNH+5,6         INVALID ENTRIES DUE TO X'40' PARAM           
         BNE   FLDINV                                                           
         MVC   TGPID,SFTSSN                                                     
         GOTO1 SSNUNPK,DMCB,TGPID,TGSSN                                         
         BNE   FTVK3                                                            
         MVC   SFTSSN,TGSSN                                                     
         MVI   SFTSSNH+5,9                                                      
*                                                                               
FTVK3    GOTO1 RECVAL,DMCB,TLW4CDQ,(X'08',SFTSSNH),SFTSSNNH                     
         MVC   TIFSSN,TGSSN        SET FILTER FOR SYSIO                         
         MVC   SSNNAME,TGNAME      SAVE FULL NAME FOR PRINTED REPORT            
*                                                                               
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   SFTSSN,SPACES                                                    
         MVC   SFTSSN(L'TGPID),TGPID                                            
         MVI   SFTSSNH+5,6                                                      
         OI    SFTSSNH+6,X'80'                                                  
*                                                                               
FTVK3A   NI    SFTAGYH+4,X'DF'     FORCE AGENCY RE-VALIDATION                   
         SPACE 1                                                                
FTVK4    TM    SFTAGYH+4,X'20'     TEST AGENCY CHANGED                          
         BO    FTVK6                                                            
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'08',SFTAGYH),SFTAGYNH                     
         BAS   RE,AGYDTLS          EXTRACT AGENCY DETAILS                       
         NI    SFTCIDH+4,X'DF'     FORCE CID RE-VALIDATION                      
         SPACE 1                                                                
FTVK6    TM    SFTCIDH+4,X'20'     TEST COMMERCIAL ID CHANGED                   
         BO    FTVK8                                                            
         GOTO1 RECVAL,DMCB,TLCOICDQ,(X'08',SFTCIDH),SFTCIDNH                    
         L     R3,AIO                                                           
         USING TLCOD,R3                                                         
         MVC   TIFCOM,TLCOCOM      SET FILTER FOR SYSIO                         
         GOTO1 CHAROUT,DMCB,TANAELQ,0                                           
         MVC   COMNAME,TGNAME      SAVE FULL NAME FOR PRINTED REPORT            
         SPACE 1                                                                
         MVC   TGCLI,TLCOCLI       SET GLOBAL CLIENT                            
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'20',0)                                    
         GOTO1 CHAROUT,DMCB,TANAELQ,0                                           
         MVC   CLINAME,TGNAME      SAVE FULL NAME FOR PRINTED REPORT            
         NI    SFTCATH+4,X'DF'     FORCE CATEGORY RE-VALIDATION                 
         SPACE 1                                                                
FTVK8    BAS   RE,VALCAT           VALIDATE CATEGORY                            
         SPACE 1                                                                
         USING TACAD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TACAELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   FTVK8B                                                           
         TM    TACASTA2,TACASPUS   IF SET UP TO RECEIVE GRR                     
         BZ    FTVK8A              AMOUNT ONCE PER CYCLE PER USE                
         NI    SFTUSHDH+1,X'F3'    GIVE USE FILTER OPTION                       
         NI    SFTUSEH+1,X'D3'                                                  
         B     FTVK8B                                                           
         DROP  R4                                                               
         SPACE 1                                                                
FTVK8A   OI    SFTUSHDH+1,X'0C'    ELSE HIDE THE FILTER OPTION                  
         OI    SFTUSEH+1,X'2C'                                                  
         CLI   SFTUSEH+5,0         AND CLEAR ANY PRIOR INPUT                    
         BE    FTVK8B                                                           
         NI    SFTUSEH+4,X'DF'                                                  
         MVI   SFTUSEH+5,0                                                      
         XC    SFTUSE,SFTUSE                                                    
         SPACE 1                                                                
FTVK8B   LA    R2,SFTUSEH                                                       
         TM    4(R2),X'20'         TEST USE CHANGED                             
         BO    FTVK9                                                            
         NI    SFTPDH+4,X'DF'      RE-INITIALIZE LIST                           
         XC    FLTUSECD,FLTUSECD   AND VALIDATE USE                             
         XC    FLTUSEQU,FLTUSEQU                                                
         CLI   5(R2),0                                                          
         BE    FTVK9                                                            
         GOTO1 USEVAL,DMCB,(X'40',8(R2))                                        
         MVC   FLTUSECD,TGUSCDE                                                 
         MVC   FLTUSEQU,TGUSEQU                                                 
FTVK9    OI    4(R2),X'20'                                                      
         SPACE 1                                                                
         LA    R2,SFTPDH                                                        
         TM    4(R2),X'20'         TEST PERIOD CHANGED                          
         BO    FTVKX                                                            
         OI    SFTWARNH+1,X'0C'    TURN OFF WARNING                             
         OI    SFTWARNH+6,X'80'                                                 
         MVI   ELCODE,TACRELQ      SET TO LOOK UP APPLIED CREDIT EL.            
         BAS   RE,VALPD            VALIDATE PERIOD                              
         BE    FTVK10                                                           
         NI    SFTWARNH+1,X'FB'    NO TACR EL FOUND - TURN ON WARNING           
         XC    TIFCSEQ,TIFCSEQ     CLEAR CAST SEQUENCE FILTER                   
         LA    R4,ELEMENT          POINT TO DUMMY ELEMENT                       
         USING TACRD,R4                                                         
         XC    TACREL(TACRLNQ),TACREL                                           
         B     *+8                                                              
FTVK10   L     R4,TGELEM           R4=A(CORRES. TACR EL.)                       
         EDIT  (4,TACRAPPL),(12,DAMOUNT),2,MINUS=YES  SAVE APPLIED AMT          
         EDIT  (4,TACRAPPL),(12,SFTAMT),2,MINUS=YES,ALIGN=LEFT                  
         OI    SFTAMTH+6,X'80'                                                  
         SPACE 1                                                                
         EDIT  (4,TACRBAL),(12,DBALANCE),2,MINUS=YES  AND BALANCE               
         EDIT  (4,TACRBAL),(12,SFTBAL),2,MINUS=YES,ALIGN=LEFT                   
         OI    SFTBALH+6,X'80'                                                  
         SPACE 1                                                                
         MVI   SFTGUAR,C'N'                                                     
         TM    TACRSTAT,TACRSGUA   TEST THIS IS GUARANTEE                       
         BZ    *+8                                                              
         MVI   SFTGUAR,C'Y'                                                     
         OI    SFTGUARH+6,X'80'                                                 
         SPACE 1                                                                
         BAS   RE,INIT             RE-INITIALIZE LIST                           
         SPACE 1                                                                
FTVKX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE KEY FIELDS (ETRACK)                          
         SPACE 1                                                                
ETVKEY   NTR1                                                                   
*&&UK                                                                           
         CLI   OFFLINE,C'Y'        IF RUNNING OFFLINE                           
         BNE   ETVK2                                                            
         CLI   SETAGYH+5,0         AND AGENCY FIELD EMPTY                       
         BNE   ETVK2                                                            
         BAS   RE,SCRNGEN          TRANSLATE REQUEST TO SCREEN                  
*&&                                                                             
         SPACE 1                                                                
ETVK2    CLI   SCRSTAT,0           IF SCREEN CHANGED                            
         BNE   *+12                                                             
         TM    SETSSNH+4,X'20'     OR SSN HAS CHANGED                           
         BO    ETVK4                                                            
*                                                                               
         LA    R2,SETSSNH          S/S NUM                                      
         CLI   SETSSNH+5,0                                                      
         BE    FLDMISS                                                          
         CLI   SETSSNH+5,9         MUST CHECK FOR LENGTH HERE SINCE             
         BE    ETVK3               RECVAL CALL DOES NOT CHECK FOR               
         CLI   SETSSNH+5,6         INVALID ENTRIES DUE TO X'40' PARAM           
         BNE   FLDINV                                                           
         MVC   TGPID,SETSSN                                                     
         GOTO1 SSNUNPK,DMCB,TGPID,TGSSN                                         
         BNE   ETVK3                                                            
         MVC   SETSSN,TGSSN                                                     
         MVI   SETSSNH+5,9                                                      
*                                                                               
ETVK3    GOTO1 RECVAL,DMCB,TLW4CDQ,(X'08',SETSSNH),SETSSNNH                     
         MVC   TIFSSN,TGSSN        SET FILTER FOR SYSIO                         
         MVC   SSNNAME,TGNAME      SAVE FULL NAME FOR PRINTED REPORT            
*                                                                               
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   SETSSN,SPACES                                                    
         MVC   SETSSN(L'TGPID),TGPID                                            
         MVI   SETSSNH+5,6                                                      
         OI    SETSSNH+6,X'80'                                                  
*                                                                               
ETVK3A   NI    SETAGYH+4,X'DF'     FORCE AGENCY RE-VALIDATION                   
         SPACE 1                                                                
ETVK4    TM    SETAGYH+4,X'20'     TEST AGENCY CHANGED                          
         BO    ETVK6                                                            
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'08',SETAGYH),SETAGYNH                     
         BAS   RE,AGYDTLS          EXTRACT AGENCY DETAILS                       
         NI    SETCIDH+4,X'DF'     FORCE CID RE-VALIDATION                      
         SPACE 1                                                                
ETVK6    TM    SETCIDH+4,X'20'     TEST COMMERCIAL ID CHANGED                   
         BO    ETVK8                                                            
         LA    R2,SETCIDH                                                       
         GOTO1 RECVAL,DMCB,TLCOICDQ,(X'08',SETCIDH),SETCIDNH                    
         L     R4,AIO                                                           
         MVI   ELCODE,TACOELQ      GET COMMERCIAL DETAILS EL                    
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACOD,R4                                                         
         CLI   TACOTYPE,CTYSOAP    COMMERCIAL TYPE MUST BE SOAP                 
         BNE   INVTYPE                                                          
         SPACE                                                                  
         L     R3,AIO                                                           
         USING TLCOD,R3                                                         
         MVC   TIFCOM,TLCOCOM      SET FILTER FOR SYSIO                         
         GOTO1 CHAROUT,DMCB,TANAELQ,0                                           
         MVC   COMNAME,TGNAME      SAVE FULL NAME FOR PRINTED REPORT            
         SPACE 1                                                                
         MVC   TGCLI,TLCOCLI       SET GLOBAL CLIENT                            
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'20',0)                                    
         GOTO1 CHAROUT,DMCB,TANAELQ,0                                           
         MVC   CLINAME,TGNAME      SAVE FULL NAME FOR PRINTED REPORT            
         NI    SETCATH+4,X'DF'     FORCE CATEGORY RE-VALIDATION                 
         SPACE 1                                                                
ETVK8    BAS   RE,VALECAT          VALIDATE CAT & EPI & GETS ECAST REC          
         BE    ETVKX               DONE IF PREV VALIDATED                       
         SPACE 1                                                                
         USING TACRD,R4                                                         
         LA    R2,SETEPIH                                                       
         L     R4,AIO                                                           
         MVI   ELCODE,TACRELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   NOTRK                                                            
         EDIT  (4,TACRAPPL),(12,DAMOUNT),2,MINUS=YES  SAVE APPLIED AMT          
         EDIT  (4,TACRAPPL),(12,SETAMT),2,MINUS=YES,ALIGN=LEFT                  
         OI    SETAMTH+6,X'80'                                                  
         SPACE 1                                                                
         EDIT  (4,TACRBAL),(12,DBALANCE),2,MINUS=YES  AND BALANCE               
         EDIT  (4,TACRBAL),(12,SETBAL),2,MINUS=YES,ALIGN=LEFT                   
         OI    SETBALH+6,X'80'                                                  
         SPACE 1                                                                
         BAS   RE,INIT             RE-INITIALIZE LIST                           
ETVKX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TRANSLATES TURNAROUND REQUESTS TO SCREEN                 
         SPACE 1                                                                
SCRNGEN  NTR1                                                                   
         GOTO1 HEXIN,DMCB,SFTCID,TGCOM,8  CVT INTERNAL COMML TO HEX             
         SPACE 1                                                                
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'20',0)  READ COMMERCIAL RECORD           
         SPACE 1                                                                
         L     R3,AIO                                                           
         USING TLCOD,R3                                                         
         MVC   SFTAGY,TLCOAGY      MOVE AGENCY TO SCREEN                        
         MVI   SFTAGYH+5,L'TGAGY                                                
         LR    R4,R3                                                            
         MVI   ELCODE,TACOELQ      GET COMML DETAILS EL.                        
         BAS   RE,GETEL                                                         
         USING TACOD,R4                                                         
         MVC   SFTCID,TACOCID      COMMERCIAL ID TO SCREEN                      
         MVI   SFTCIDH+5,L'TACOCID                                              
         SPACE 1                                                                
         GOTO1 DATCON,DMCB,(0,SFTPD),(8,SFTPD)  START DATE TO MMMDD/YY          
         MVI   SFTPDH+5,8                                                       
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE EXTRACTS AGENCY DETAILS FROM AGENCY RECORD               
         SPACE 1                                                                
*                                  AIO=A(AGENCY RECORD)                         
AGYDTLS  NTR1                                                                   
         GOTO1 CHAROUT,DMCB,TANAELQ,0                                           
         MVC   AGYNAME,TGNAME      FULL NAME FOR PRINTED REPORT                 
         SPACE 1                                                                
         L     R4,AIO                                                           
         MVI   ELCODE,TAAYELQ      GET AGENCY DETAILS EL.                       
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAAYD,R4                                                         
         MVC   AGYSTAT2,TAAYSTA2   SAVE 2ND AGENCY STATUS BYTE                  
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE CATEGORY FIELD                               
         SPACE 1                                                                
VALCAT   NTR1                                                                   
         LA    R2,SFTCATH                                                       
         TM    SFTPDH+4,X'20'      IF PERIOD FIELD CHANGED                      
         BZ    *+12                                                             
         TM    4(R2),X'20'         OR CATEGORY CHANGED                          
         BO    VCX                                                              
         XC    TGCAT,TGCAT                                                      
         CLI   5(R2),0             OK TO LEAVE BLANK - I'LL LOOK UP             
         BE    VC10                                                             
         OC    8(3,R2),SPACES                                                   
         GOTO1 CATVAL,DMCB,8(R2)   VALIDATE CATEGORY CODE                       
         BE    VC10                                                             
         SPACE 1                                                                
         MVC   FULL(3),=3C'0'      IF NOT VALID TEST IF VALID HEX               
         ZIC   R1,5(R2)                                                         
         LA    RF,L'FULL                                                        
         SR    RF,R1                                                            
         LA    RF,FULL(RF)         SET TO RIGHT-ALIGN IN FULL                   
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),8(R2)                                                    
         SPACE 1                                                                
         GOTO1 HEXIN,DMCB,FULL,TGFULL,4                                         
         OC    12(4,R1),12(R1)                                                  
         BZ    FLDINV                                                           
         SPACE 1                                                                
VC10     XC    KEY,KEY             BUILD CAST KEY                               
         LA    R3,KEY                                                           
         USING TLCAPD,R3                                                        
         MVI   TLCAPCD,TLCACCDQ                                                 
         MVC   TLCACSSN,TIFSSN     SOCIAL SECURITY NUMBER                       
         MVC   TLCACCOM,TIFCOM     INTERNAL COMMERCIAL NUMBER                   
         MVC   TLCACCAT,TGCAT      CATEGORY                                     
         GOTO1 HIGH                                                             
         SPACE 1                                                                
VC20     CLC   TLCAPKEY(TLCACCAT-TLCAPD),KEYSAVE  STILL SAME COMML              
         BNE   FLDINV                                                           
         OC    TGCAT,TGCAT         IF CATEGORY INPUT                            
         BZ    VC30                                                             
         CLC   TLCACCAT,TGCAT      TAKE FIRST MATCH                             
         BE    VC50                                                             
         B     VC40                                                             
         SPACE 1                                                                
VC30     CLI   5(R2),0             IF NO INPUT TAKE FIRST ONE FOUND             
         BE    VC50                                                             
         CLC   TLCACSEQ,TGFULL     ELSE SCAN FOR MATCHING SEQUENCE NO.          
         BE    VC50                                                             
         SPACE 1                                                                
VC40     GOTO1 SEQ                 GET NEXT CAST RECORD                         
         B     VC20                AND KEEP ON TRYING                           
         SPACE 1                                                                
VC50     GOTO1 GETREC              GET CAST RECORD FOR PD VAL.                  
         SPACE 1                                                                
         GOTO1 HEXOUT,DMCB,TLCACSEQ,FULL,2,0  CVT SEQ TO CHAR.                  
         MVC   CASTSEQ,FULL+1                 SAVE 3 LOBS                       
         SPACE 1                                                                
         MVC   TIFCSEQ,TLCACSEQ    SET FILTER FOR SYSIO                         
         MVC   8(3,R2),TLCACCAT    DISPLAY ACTUAL CATEGORY                      
         OI    6(R2),X'80'                                                      
         OI    4(R2),X'20'         SET VALIDATED                                
         NI    SFTPDH+4,X'DF'      FORCE PERIOD RE-VALIDATION                   
VCX      B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE CATEGORY AND EPISODE FLDS FOR ETRACK         
*              RETURNS CC EQUAL IF BOTH FIELDS PREV VALIDATED                   
*              ELSE RETURNS CC NOT EQUAL                                        
         SPACE 1                                                                
VALECAT  NTR1                                                                   
         LA    R2,SETCATH                                                       
         TM    SETEPIH+4,X'20'     TEST EPISODE FIELD CHANGED                   
         BZ    *+12                                                             
         TM    4(R2),X'20'         OR CATEGORY CHANGED                          
         BO    YES                                                              
         XC    TGCAT,TGCAT                                                      
         CLI   5(R2),0             OK TO LEAVE BLANK - I'LL LOOK UP             
         BE    VEC5                                                             
         OC    8(3,R2),SPACES                                                   
         GOTO1 CATVAL,DMCB,8(R2)   VALIDATE CATEGORY CODE                       
         BNE   FLDINV                                                           
VEC5     LA    R2,SETEPIH                                                       
         GOTO1 RECVAL,DMCB,TLEPCDQ,SETEPIH  VALIDATE EPISODE FIELD              
         MVC   TIFEPI,TGEPI                 SET SYSIO FILTER                    
         SPACE 1                                                                
VEC10    XC    KEY,KEY             BUILD ECAST KEY                              
         LA    R3,KEY                                                           
         USING TLECPD,R3                                                        
         MVI   TLECPCD,TLECCCDQ                                                 
         MVC   TLECCSSN,TIFSSN     SOCIAL SECURITY NUMBER                       
         MVC   TLECCCOM,TIFCOM     INTERNAL COMMERCIAL NUMBER                   
         MVC   TLECCEPI,TGEPI      EPISODE NUMBER                               
         XC    TLECCEPI,HEXFFS     (COMPLEMENTED)                               
         MVC   TLECCCAT,TGCAT      CATEGORY                                     
         GOTO1 HIGH                                                             
         SPACE 1                                                                
VEC20    CLC   TLECPKEY(TLECCCAT-TLECPD),KEYSAVE  STILL SAME COMML              
         BNE   FLDINV                                                           
         OC    TGCAT,TGCAT         IF CATEGORY INPUT                            
         BZ    VEC50                                                            
         CLC   TLECCCAT,TGCAT      FIND MATCH                                   
         BE    VEC50                                                            
         SPACE 1                                                                
         GOTO1 SEQ                 GET NEXT ECAST RECORD                        
         B     VEC20               AND KEEP ON TRYING                           
         SPACE 1                                                                
VEC50    GOTO1 GETREC              GET RECORD                                   
         SPACE 1                                                                
         MVC   TIFCAT,TLECCCAT     SET FILTER FOR SYSIO                         
         MVC   TGCAT,TLECCCAT      SET GLOBAL IN CASE NOT INPUT                 
         MVC   SETCAT,TLECCCAT     DISPLAY ACTUAL CATEGORY                      
         OI    SETCATH+6,X'80'                                                  
         OI    SETCATH+4,X'20'     SET VALIDATED                                
         B     NO                                                               
         EJECT                                                                  
*              ROUTINE TO VALIDATE PERIOD FIELD                                 
         SPACE 1                                                                
*                                  R2=A(FIELD)                                  
*                                  ELCODE=ELEMENT TO LOOK UP IN AIO             
*                                  CYCLE START ASSUMED 1ST FLD IN EL.           
VALPD    NTR1                                                                   
         XR    R0,R0                                                            
         CLI   5(R2),0             TEST SOMETHING INPUT                         
         BE    VP2                                                              
         CLI   OFFLINE,C'Y'        IF RUNNING OFFLINE                           
         BNE   VP1                                                              
         CLI   8(R2),C'*'          AND FIELD BEGINS WITH AN ASTERISK            
         BNE   VP1                                                              
         LA    R0,1                SET INDICATOR USING R0                       
         MVC   DUB,9(R2)                                                        
         MVC   8(8,R2),DUB         SLIDE DATE OVER ONE                          
         MVI   16(R2),C' '                                                      
         MVI   5(R2),8             AND RESET L'INPUT                            
VP1      LA    R3,BLOCK                                                         
         GOTO1 PDVAL,DMCB,(R3)     VALIDATE PERIOD                              
         USING PERVALD,R3                                                       
         MVC   DPERIOD,PVALCPER    SAVE DEFAULT DISPLAY PERIOD                  
         MVC   TIQSTART(6),PVALPSTA AND DEFAULT SYSIO PERIOD FILTER             
         XC    TIQSTART(6),HEXFFS                                               
         SPACE 1                                                                
VP2      L     R4,AIO              R4=A(RECORD)                                 
         XR    R1,R1                                                            
         BAS   RE,GETEL                                                         
         BNE   NO                                                               
         CLI   ELCODE,TACRELQ      IF TACR ELEMENT                              
         BNE   VP4                                                              
         BAS   RE,GETTACR          USE SPECIAL ROUTINE TO FIND IT               
         BNE   NO                                                               
         B     VP8                                                              
         SPACE 1                                                                
VP4      LR    R1,R4               SAVE A(LAST ONE FOUND)                       
         CLI   5(R2),0             IF NO INPUT SCAN UNTIL END                   
         BE    VP6                                                              
         CLC   PVALPSTA,2(R4)      ELSE MATCH ON START DATE                     
         BE    VP8                                                              
         BL    VP6                 CAN'T BE GOOD IF INPUT < START               
         LTR   R0,R0               IF SCANNING FOR OVERLAP                      
         BZ    VP6                                                              
         CLC   PVALPEND,2+3(R4)    MUST BE ON/BEFORE END DATE                   
         BNH   VP8                                                              
VP6      BAS   RE,NEXTEL           TRY NEXT ELEMENT                             
         BE    VP4                                                              
         CLI   5(R2),0             NO MORE - OK IF NO PD INPUT                  
         BNE   NO                                                               
         LTR   R4,R1               USE LAST ELEMENT FOUND                       
         BZ    NO                                                               
         SPACE 1                                                                
VP8      GOTO1 DATCON,DMCB,(X'11',2(R4)),(8,8(R2))  DISPLAY DATES               
         MVI   5(R2),17                                                         
         OI    6(R2),X'80'                                                      
         MVC   DPERIOD,8(R2)       SAVE FOR PRINTING                            
         SPACE 1                                                                
         MVC   TIQSTART(6),2(R4)   SAVE PWOS DATES FOR SYSIO                    
         XC    TIQSTART(6),HEXFFS       (COMPLEMENTED)                          
         SPACE 1                                                                
         ST    R4,TGELEM           RETURN A(ELEMENT)                            
         SPACE 1                                                                
         OI    4(R2),X'20'         SET VALIDATED                                
         B     YES                                                              
         EJECT                                                                  
*              ROUTINE TO GET THE CORRECT TACR ELEMENT                          
*              RETURNS ITS ADDRESS IN R4 AND SETS CC EQ IF FINDS IT             
*              ELSE RETURNS CC NOT EQ                                           
         SPACE 1                                                                
*                                  R0=INDICATOR WHETHER OVERLAP ALLOWED         
*                                  R2=A(FIELD)                                  
*                                  R4=A(FIRST TACR EL)                          
*                                  R3=A(BLOCK FILLED BY PERVAL)                 
         SPACE 1                                                                
         USING TACRD,R4                                                         
         USING PERVALD,R3                                                       
GETTACR  NTR1                                                                   
         XC    TGELEM,TGELEM                                                    
         XC    TGDUB,TGDUB                                                      
         XC    DUB,DUB                                                          
         SPACE 1                                                                
GTCR4    CLI   FLTUSEQU,0          IF FILTERING ON USE                          
         BZ    GTCR4A                                                           
         CLC   FLTUSECD,TACRUSE    ACCEPT FTRACKS FOR THE USE                   
         BE    GTCR4A                                                           
         CLC   TACRUSE,=C'GRR'     AND RADIO GUARANTEES THAT COVER              
         BNE   GTCR6               THE USE                                      
         CLC   FLTUSEQU,TACRTYPE                                                
         BNE   GTCR6                                                            
         SPACE 1                                                                
GTCR4A   OC    TACRINV,TACRINV     IF THERE'S AN INVOICE NUMBER                 
         BZ    *+14                                                             
         CLC   TACRINV,DUB         TEST THIS INVOICE LATER THAN SAVED           
         BNH   GTCR5                                                            
         LR    R1,R4                   SAVE A(LAST ONE FOUND)                   
         MVC   DUB(L'TACRINV),TACRINV  AND ITS INV NUM                          
GTCR5    CLI   5(R2),0             IF PD INPUT                                  
         BE    GTCR6                                                            
         CLC   PVALPSTA,TACRSTRT   MATCH ON START DATE                          
         BE    GTCR8                                                            
         BL    GTCR6               CAN'T BE GOOD IF INPUT < START               
         LTR   R0,R0               IF SCANNING FOR OVERLAP                      
         BZ    GTCR6                                                            
         CLC   PVALPEND,TACREND    MUST BE ON/BEFORE END DATE                   
         BNH   GTCR8                                                            
GTCR6    BAS   RE,NEXTEL           TRY NEXT ELEMENT                             
         BE    GTCR4                                                            
         CLI   5(R2),0             NO MORE - IF NO PD INPUT                     
         BNE   GTCR10                                                           
         LR    R4,R1               RETURN VERY LAST ELEMENT FOUND               
         B     GTCRX                                                            
         SPACE 1                                                                
GTCR8    OC    TACRINV,TACRINV     IF HAVE INVOICE NUM                          
         BZ    GTCR9                                                            
         CLC   TACRINV,TGDUB       TEST THIS INV LATER THAN SAVED               
         BNH   GTCR6                                                            
         SPACE 1                                                                
GTCR9    STCM  R4,7,TGELEM+1           SAVE A(LAST EL WITH RIGHT DATES)         
         MVC   TGDUB(L'TACRINV),TACRINV  AND ITS INV NUM                        
         B     GTCR6                                                            
         SPACE 1                                                                
GTCR10   L     R4,TGELEM           HAVE PD INPUT                                
         LTR   R4,R4               TEST IF FOUND EL WITH CORRECT DATES          
         BZ    NO                                                               
GTCRX    XR    RC,RC                                                            
         LTR   RC,RC               SET CC EQUAL                                 
         XIT1  REGS=(R4)           RETURN R4                                    
         EJECT                                                                  
*              INITIALIZATION ROUTINE                                           
         SPACE 1                                                                
INIT     NTR1                                                                   
         MVI   PGSW,C'F'           TELLS PGCNTL TO RESTART LIST                 
         MVC   TIUSERID,TWAORIG    SET UP SOME FIELDS FOR SYSIO                 
         MVC   TIQSTAFF,TGCTSTAF                                                
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PROCESS RECORDS FROM SYSIO ON SCREEN                  
         SPACE 1                                                                
SCHOOK   NTR1                                                                   
         CLI   TIMODE,PROCREC      PROCESS RECORDS                              
         BNE   XIT                                                              
         L     R2,AFSTH                                                         
         AH    R2,DSPLINE          R2=A(THIS DISPLAY LINE)                      
         USING LINED,R2                                                         
         L     RF,ALSTH                                                         
         CR    R2,RF               TEST WHETHER WE'RE PAST END                  
         BH    TRKMORE             YES - RETURN TO USER                         
         SPACE 1                                                                
         LA    R2,8(R2)            BUMP PAST HEADER                             
         BAS   RE,GETTRK           GET TRACKING ELEMENT - EXTRACT DATA          
         BNE   XIT                                                              
         MVC   LINCMNT,BLOCK+8     COMMENT TO DISPLAY                           
         SPACE 1                                                                
         CLI   TGCTSTTY,TASTTYPP   IF THIS IS PROGRAMMER                        
         BNE   SCH10                                                            
         LA    R3,LINCMNT+L'LINCMNT-10  SET TO DISPLAY D/A                      
         MVI   0(R3),C'('                                                       
         GOTO1 HEXOUT,DMCB,TIDSKADD,1(R3),4,0                                   
         MVI   9(R3),C')'                                                       
         SPACE 1                                                                
SCH10    MVC   LINUSE,TRKUS        USE DETAILS                                  
         SPACE 1                                                                
         ICM   R4,15,TRKELEM       DID WE GET TRACKING EL.                      
         BZ    SCHX                                                             
         USING TAGTD,R4            R4=A(GUARANTEE TRACKING EL.)                 
         SPACE 1                                                                
         MVC   LININV,TRKINV       INVOICE NUMBER                               
         MVC   LINAGY,TRKAGY       AGENCY                                       
         MVC   LINCYC,TRKCYC       CYCLE DATES                                  
         MVC   LINCAT,TAGTCAT      CATEGORY                                     
         MVC   LINCAM,TAGTCAM      CAMERA                                       
         MVC   LINOV1,TRKOV1       OVERSCALE RATE 1                             
         MVC   LINOV2,TRKOV2       OVERSCALE RATE 2                             
         MVC   LINCID,TRKCID       COMMERCIAL ID                                
         SPACE 1                                                                
         CLC   ADJLITBY,ADJDSCRP                                                
         BNE   SCH20                                                            
         MVC   LINUSE,SPACES                                                    
         MVC   LINCID(L'ADJLITBY+L'TAACSTAF),ADJDSCRP                           
         SPACE 1                                                                
SCH20    CLC   ADJLIT2,ADJDSCRP                                                 
         BNE   SCH30                                                            
         MVC   LINUSE,SPACES                                                    
         MVC   LINCID(L'ADJLIT2),ADJLIT2                                        
         MVC   LINUSE(15),ADJDSCRP+L'ADJLIT2                                    
         SPACE 1                                                                
SCH30    EDIT  (4,TRKAMT),(10,LINCRD),2,MINUS=YES   APPLIED AMOUNT              
         EDIT  (4,TAGTPNH),(10,LINPNH),2,MINUS=YES  PENSION AND HEALTH          
         EDIT  (4,TAGTBAL),(12,LINBAL),2,MINUS=YES  BALANCE                     
         SPACE 1                                                                
         OC    TRKPAY,TRKPAY       IF THERE'S SEPARATE PAYMENT AMT              
         BZ    SCHX                                                             
         MVC   LINCMNT,SPACES                      CLEAR COMMENT AND            
         EDIT  (4,TRKPAY),(12,LINPAY),2,MINUS=YES  DISPLAY ON 2ND LINE          
         LA    R1,LINPAY+L'LINPAY-2                                             
         BAS   RE,MVCPAID          MOVE IN PD: LITERAL                          
         SPACE 1                                                                
SCHX     LA    R2,LINNEXT          BUMP TO NEXT LINE                            
         L     RF,AFSTH                                                         
         LA    RF,8(RF)                                                         
         SR    R2,RF               CALC. DISP. TO NEXT LINE                     
         STH   R2,DSPLINE          SAVE IT FOR NEXT TIME IN                     
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PROCESS RECORDS FROM SYSIO WHEN SPOOLING              
         SPACE 1                                                                
PRHOOK   NTR1                                                                   
         CLI   TIMODE,PROCREC      PROCESS RECORD                               
         BNE   PRHX                                                             
         SPACE 1                                                                
         BAS   RE,PRNTREC          SET UP PRINT LINES                           
         BNE   PRHX                                                             
         SPACE 1                                                                
         L     R2,ANXTDA           R2=A(NEXT SLOT IN D/A TABLE)                 
         L     RE,ATMPSTOR                                                      
         AH    RE,=Y(TMPLNQ-5)                                                  
         CR    R2,RE                                                            
         BNH   PRH5                                                             
         MVI   NOROOM,C'Y'         SET NO ROOM IN TEMP STORAGE FLAG             
         B     PRHX                                                             
         SPACE 1                                                                
PRH5     MVC   0(4,R2),TIDSKADD    SAVE D/A                                     
         SPACE 1                                                                
         MVI   4(R2),1             SAVE N'PRINT LINES USED                      
         CLC   P2,SPACES                                                        
         BE    *+8                                                              
         MVI   4(R2),2                                                          
         CLC   P3,SPACES                                                        
         BE    *+8                                                              
         MVI   4(R2),3                                                          
         SPACE 1                                                                
         CLC   TILADATE,TGTODAY1   IF LAST ACTIVE DATE ON/AFTER TODAY           
         BL    PRH40                                                            
         OC    TRKINV,TRKINV       (IF MANUAL ADJ. SKIP NEXT TEST)              
         BZ    *+14                                                             
         OC    TILACT,TILACT       AND STAFF NOT DEFINED (IMPLIES ADD)          
         BNZ   PRH40                                                            
         OI    4(R2),X'80'         FORCE PRINT                                  
         SPACE 1                                                                
PRH40    LA    R2,5(R2)            BUMP TO NEXT POSITION IN TABLE               
         ST    R2,ANXTDA           AND SAVE                                     
         SPACE 1                                                                
PRH45    MVC   P,SPACES            CLEAR PRINT LINES                            
         MVC   P2,SPACES                                                        
         MVC   P3,SPACES                                                        
         SPACE 1                                                                
PRHX     B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PROCESS RECORDS FROM DISK ADDRESS TABLE               
         SPACE 1                                                                
PROCESS  NTR1                                                                   
         BAS   RE,SETCOMP          SET COMPLETE PAGE INDICATORS IN TBL.         
         SPACE 1                                                                
         XC    FULL,FULL           CLEAR PREVIOUS PAGE VALUES                   
         ZIC   RE,NUMLINES         RE=N'PRINT LINES PER PAGE                    
         LA    R0,99               SET TO START NEW PAGE                        
         L     R2,ANXTDA           R2=A(NEXT SLOT IN TABLE)                     
         XR    R3,R3               R3=A(FIRST RECORD ON PAGE)                   
         XR    R1,R1               R1=PAGE NUMBER                               
         SPACE 1                                                                
PROC10   SH    R2,=H'5'            BUMP TO PREVIOUS ENTRY IN TABLE              
         C     R2,ATMPSTOR         HAVE WE GONE PAST THE BEGINNING              
         BL    PROC20                                                           
         ZIC   RF,4(R2)            RF=N'LINES REQUIRED FOR THIS REC.            
         SLL   RF,26               (SQUEEZE OUT HOBS)                           
         SRL   RF,26                                                            
         AR    R0,RF               R0=RUNNING TOTAL OF LINES USED               
         CR    R0,RE               IF WE EXCEEDED PAGE                          
         BNH   PROC14                                                           
         ST    R3,FULL             SAVE A(START OF CURRENT PAGE)                
         SR    R0,RF               BACK OUT N'LINES FOR THIS RECORD AND         
         STC   R0,FULL             SAVE N'LINES USED ON CURRENT PAGE            
         LR    R3,R2               SAVE A(START OF NEW PAGE)                    
         LR    R0,RF               RESET LINES USED COUNTER                     
         LA    R1,1(R1)            INCREMENT PAGE NUMBER                        
         SPACE 1                                                                
PROC14   TM    4(R2),X'80'         IF FORCE PRINT SET FOR THIS ENTRY            
         BZ    PROC16                                                           
         CR    R2,R3               AND THIS IS FIRST ENTRY ON THIS PAGE         
         BNE   PROC15                                                           
         ICM   R4,7,FULL+1         AND WE HAVE A(BEG. OF PREV. PAGE)            
         BZ    PROC15                                                           
         CLC   FULL(1),NUMLINES    AND IF DIDN'T USE EVERY LINE OF              
         BNL   PROC15              OF PREVIOUS PAGE                             
         LR    R3,R4               THEN SET TO START FROM PREV. PAGE            
         BCTR  R1,0                REDUCE PAGE NUMBER ACCORDINGLY               
PROC15   B     PROC20              START PRINTING FROM HERE                     
         SPACE 1                                                                
PROC16   TM    WHEN,X'40'          ELSE IF NOT PRINTING NOW                     
         BO    PROC20                                                           
         TM    AGYSTAT2,TAAYSTRK   AND NOT PRINTING ALL PAGES                   
         BZ    PROC10              GET NEXT RECORD                              
         SPACE 1                                                                
PROC20   LTR   R2,R3               R2=A(FIRST RECORD TO PRINT)                  
         BZ    PROCX                                                            
         STH   R1,PAGE             FORCE CORRECT PAGE NUMBER                    
         L     R3,TIAREC                                                        
         ST    R3,AIO              R3=A(I/O AREA)                               
         USING TLGTD,R3                                                         
         LA    R4,KEY              R4=A(KEY)                                    
         USING TLDRD,R4                                                         
         SPACE 1                                                                
PROC30   MVC   TLDRDA,0(R2)        SET D/A IN KEY                               
         GOTO1 GETREC              GET THE RECORD                               
         MVC   TIKEY,TLGTKEY       MOVE KEY TO SYSIO KEY                        
         SPACE 1                                                                
         MVI   COMPLETE,C'N'                                                    
         TM    4(R2),X'40'         TEST COMPLETE PAGE INDICATOR                 
         BZ    *+8                                                              
         MVI   COMPLETE,C'Y'                                                    
         BAS   RE,PRNTREC          SET UP PRINT LINES                           
**NO-OP* GOTO1 HEXOUT,DMCB,4(R2),P+7,1,0                                        
         GOTO1 SPOOL,DMCB,(R8)     PRINT THEM                                   
         SPACE 1                                                                
         SH    R2,=H'5'            BUMP TO PREVIOUS RECORD                      
         C     R2,ATMPSTOR         TEST WE'RE BEFORE BEG. OF TABLE              
         BNL   PROC30                                                           
         MVC   AIO,AIO1            YES - RESET I/O AREA                         
         SPACE 1                                                                
PROCX    CLI   NOROOM,C'Y'         IF NO ROOM IN STORGE FOR FULL REPORT         
         BNE   XIT                                                              
         MVC   P(46),=C'*** UNABLE TO DISPLAY THE REST OF TRACKING ***'         
         GOTO1 SPOOL,DMCB,(R8)     PRINT MESSAGE                                
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE SETS COMPLETE PAGE FLAGS IN D/A TABLE                    
         SPACE 1                                                                
SETCOMP  NTR1                                                                   
         L     R2,ANXTDA           R2=A(NEXT SLOT IN TABLE)                     
         SH    R2,=H'5'            BUMP TO PREVIOUS ENTRY IN TABLE              
         C     R2,ATMPSTOR         HAVE WE GONE PAST THE BEGINNING              
         BL    SETCX               YES                                          
         LR    R3,R2               R3=R2=A(FIRST RECORD ON FIRST PAGE)          
         ZIC   RE,NUMLINES         RE=N'PRINT LINES PER PAGE                    
         XR    R0,R0               SET WE'RE STARTING NEW PAGE                  
         SPACE 1                                                                
SETC10   ZIC   RF,4(R2)            RF=N'LINES REQUIRED FOR THIS REC.            
         SLL   RF,25               (SQUEEZE OUT HOB)                            
         SRL   RF,25                                                            
         AR    R0,RF               R0=RUNNING TOTAL OF LINES USED               
         CR    R0,RE               IF WE REACHED END OF PAGE                    
         BL    SETC20                                                           
         OI    4(R3),X'40'         SET X'40' BIT IN 1ST ENTRY FOR PAGE          
         SPACE 1                                                                
         CR    R0,RE               IF THIS LINE WILL FIT THEN SKIP              
         BE    SETC20                                                           
         LR    R3,R2               ELSE SAVE A(START OF NEW PAGE)               
         LR    R0,RF               RESET LINES USED COUNTER                     
         SPACE 1                                                                
SETC20   SH    R2,=H'5'            BUMP TO PREVIOUS ENTRY IN TABLE              
         C     R2,ATMPSTOR         HAVE WE GONE PAST THE BEGINNING              
         BNL   SETC10              YES                                          
         SPACE 1                                                                
SETCX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PROCESS RECORDS WHEN SPOOLING                         
         SPACE 1                                                                
PRNTREC  NTR1                                                                   
         BAS   RE,GETTRK           GET TRACKING ELEMENT - EXTRACT DATA          
         BNE   NO                                                               
         LA    R2,P                SET R2=A(PRINT LINE)                         
         USING PRNTD,R2                                                         
         ICM   R4,15,TRKELEM       DID WE GET TRACKING EL.                      
         BZ    PREC5                                                            
         USING TAGTD,R4            R4=A(TRACKING EL)                            
         SPACE 1                                                                
         EDIT  (4,TRKAMT),(12,PRNTCRD),2,MINUS=YES  APPLIED AMOUNT              
         EDIT  (4,TAGTPNH),(11,PRNTPNH),2,MINUS=YES PENSION AND HEALTH          
         EDIT  (4,TAGTBAL),(12,PRNTBAL),2,MINUS=YES BALANCE                     
         SPACE 1                                                                
         OC    TRKPAY,TRKPAY       IF THERE'S SEPARATE PAYMENT AMT              
         BZ    PREC5                                                            
         EDIT  (4,TRKPAY),(12,PRNTPAY),2,MINUS=YES  DISPLAY ON 2ND LINE         
         LA    R1,PRNTPAY+L'PRNTPAY-2                                           
         BAS   RE,MVCPAID          MOVE IN PD: LITERAL                          
         SPACE 1                                                                
PREC5    CLI   RECNUM,FT           IF THIS IS FTRACK REPORT                     
         BE    PREC6                                                            
         CLI   RECNUM,ETR          OR ETRACK REPORT                             
         BNE   PREC7                                                            
PREC6    SH    R2,=H'7'            SKIP OVER AGENCY COLUMN                      
         LA    R0,40+L'FCYCLIT                                                  
         GOTO1 CHOPPER,DMCB,((R0),BLOCK+8),(32,PRNTCMNT),(C'P',2)               
         SPACE 1                                                                
         MVC   PRNTUSE(32),TRKUS   USE DETAILS (HAVE ROOM FOR L'32)             
         B     PREC9                                                            
         SPACE 1                                                                
PREC7    CLI   TRKUSLEN,25         IF L'USE DETAILS > L'PRNTUSE                 
         BNH   PREC8                                                            
         MVC   WORK,BLOCK+8            SAVE COMMENT                             
         MVC   BLOCK+8(L'TRKUS),TRKUS  PUT USE DETAILS BEFORE IT                
         MVI   BLOCK+8+L'TRKUS,C' '    AND CHOP IT INTO 3 LINES                 
         MVC   BLOCK+9+L'TRKUS(L'WORK),WORK                                     
         LA    R0,40+L'FCYCLIT+L'TRKUS                                          
         GOTO1 SQUASHER,DMCB,BLOCK+8,(R0)                                       
         GOTO1 CHOPPER,DMCB,((R0),BLOCK+8),(25,PRNTUSE),(C'P',3)                
         B     PREC9                                                            
         SPACE                                                                  
PREC8    LA    R0,40+L'FCYCLIT                                                  
         GOTO1 CHOPPER,DMCB,((R0),BLOCK+8),(25,PRNTCMNT),(C'P',2)               
         SPACE 1                                                                
         MVC   PRNTUSE,TRKUS       USE DETAILS                                  
         SPACE 1                                                                
PREC9    LTR   R4,R4               DID WE GET TRACKING EL.                      
         BZ    PRECX                                                            
         SPACE 1                                                                
         MVC   PRNTINV,TRKINV      INVOICE NUMBER                               
         TM    TAGTPST1,TAPDPCRD   IF THIS IS CREDIT INVOICE                    
         BZ    *+10                                                             
         MVC   PRNTCR,CRLIT        INDICATE WITH CR LITERAL                     
         SPACE 1                                                                
         CLI   RECNUM,FT           IF THIS IS NOT FTRACK REPORT                 
         BE    PREC10                                                           
         CLI   RECNUM,ETR          AND NOT ETRACK REPORT                        
         BE    *+10                                                             
         MVC   PRNTAGY,TRKAGY      DISPLAY AGENCY                               
         SPACE 1                                                                
PREC10   MVC   PRNTCID,TAGTCID     COMMERCIAL ID                                
         MVC   PRNTCAT,TAGTCAT     CATEGORY                                     
         MVC   PRNTCAM,TAGTCAM     CAMERA                                       
         MVC   PRNTYR,TAGTYEAR     CONTRACT YEAR                                
         MVC   PRNTOV1,TRKOV1      OVERSCALE RATE 1                             
         MVC   PRNTOV2,TRKOV2      OVERSCALE RATE 2                             
         MVC   PRNTCYC,TRKCYC      CYCLE DATES                                  
         SPACE 1                                                                
PRECX    B     YES                                                              
         SPACE 3                                                                
MVCPAID  DS    0H                                                               
         CLI   0(R1),C' '                                                       
         BE    *+8                                                              
         BCT   R1,*-8                                                           
         BCTR  R1,0                                                             
         BCTR  R1,0                                                             
         MVC   0(3,R1),=C'PD:'                                                  
         BR    RE                                                               
         EJECT                                                                  
*              ROUTINE EXTRACTS SOME DETAILS FROM TRACKING RECORD               
         SPACE 1                                                                
*                                  RETURNS COMMENT IN BLOCK+8                   
GETTRK   NTR1                                                                   
         XC    TRKDETS(TRKLNQ),TRKDETS  PRE-CLEAR DETAILS                       
         MVC   TRKUS(L'ADJLIT),ADJLIT   ASSUME THIS IS AN ADJUSTMENT            
         MVC   ADJDSCRP,SPACES                                                  
         SPACE 1                                                                
         BAS   RE,WHOADJ           DISPLAY WHO DID MANUAL ADJUSTMENT            
         SPACE 1                                                                
         BAS   RE,FILTUSE          IF FILTERING BY USE                          
         BNE   XIT                 FILTER NOW                                   
         SPACE 1                                                                
         L     R4,TIAREC                                                        
         CLI   RECNUM,FT           IF THIS IS FTRACK REPORT                     
         BNE   GGT1                                                             
         TM    SFTWARNH+1,X'0C'    AND WARNING IS NOT ZERO INTENSITY            
         BO    GGT1                                                             
         TM    SFTPDH+4,X'20'      AND HAVEN'T ALREADY DONE SO                  
         BO    GGT1                                                             
         USING TLFTD,R4                                                         
         MVC   WORK(6),TLFTSTRT    DISPLAY ACTUAL PERIOD (NECESSARY             
         XC    WORK(6),HEXFFS          BECAUSE TACREL DOESN'T EXIST)            
         GOTO1 DATCON,DMCB,(X'11',WORK),(8,SFTPD)                               
         OI    SFTPDH+4,X'20'      SET WE'VE TAKEN CARE OF PERIOD               
         OI    SFTPDH+6,X'80'                                                   
         SPACE 1                                                                
GGT1     MVI   ELCODE,TAGTELQ      LOOK FOR GUARANTEE TRACKING EL.              
         BAS   RE,GETEL                                                         
         BE    GGT2                                                             
         OC    TIFUSE,TIFUSE       NOT FOUND - IF USE FILTER DEFINED            
         BZ    *+14                                                             
         CLC   TIFUSE,=C'OTH'      ONLY TAKE IF FILTERING ON 'OTH'              
         BNE   NO                                                               
         XR    R4,R4                                                            
         B     GGT14               ELSE LOOK FOR COMMENT                        
         SPACE 1                                                                
GGT2     ST    R4,TRKELEM          R4=A(GUARANTEE TRACKING EL.)                 
         USING TAGTD,R4                                                         
         OC    TIFUSE,TIFUSE       IF USE FILTER DEFINED                        
         BZ    GGT3                                                             
         OC    TAGTUSE,TAGTUSE     AND NO USE CODE IN ELEMENT                   
         BNZ   GGT3                                                             
         CLC   TIFUSE,=C'OTH'      ONLY TAKE IF FILTERING ON 'OTH'              
         BNE   NO                                                               
         SPACE 1                                                                
GGT3     OC    TAGTINV,TAGTINV                                                  
         BZ    GGT4                                                             
         GOTO1 TINVCON,DMCB,TAGTINV,TRKINV,DATCON  INVOICE NUMBER               
         SPACE 1                                                                
GGT4     CLI   RECNUM,ETR          NO OV1 AND OV2 IF ETRACK REPORT              
         BE    GGT4B                                                            
         TM    TAGTOV,X'80'        PERCENT SCALE USED?                          
         BZ    GGT4A1                                                           
         MVI   TRKOV1,C'%'                                                      
         EDIT  (4,TAGTOV),(5,TRKOV1+1),2   % SCALE RATE 1                       
         B     GGT4A9                                                           
*                                                                               
GGT4A1   EDIT  (4,TAGTOV),(6,TRKOV1),2   OVERSCALE RATE 1                       
*                                                                               
GGT4A9   EDIT  (4,TAGTOV2),(6,TRKOV2),2  OVERSCALE RATE 2                       
         SPACE                                                                  
         CLI   RECNUM,FT           IF THIS IS ETRACK OR FTRACK REPORT           
         BNE   *+14                                                             
GGT4B    CLC   TGCID,TAGTCID       IF CID ON TRACKING DIFFERENT FROM            
         BE    *+10                                                             
         MVC   TRKCID,TAGTCID      REC, THEN SAVE FOR DISPLAY                   
         SPACE 1                                                                
         CLI   RECNUM,GX                                                        
         BE    *+14                                                             
         CLC   TGAGY,TAGTAGY       IF AGENCY ON TRACKING DIFFERENT FROM         
         BE    *+10                                                             
         MVC   TRKAGY,TAGTAGY      REC, THEN SAVE FOR DISPLAY                   
         SPACE 1                                                                
         CLI   RECNUM,GT           IF THIS IS GTRACK REPORT                     
         BE    *+12                                                             
         CLI   RECNUM,GX                                                        
         BNE   GGT4D                                                            
         CLI   SGTAGYH+5,0         AND FILTERING ON AGENCY                      
         BE    GGT4C                                                            
         OC    SGTAGY,SPACES                                                    
         CLC   SGTAGY,TAGTAGY      AGENCY MUST MATCH                            
         BNE   NO                                                               
GGT4C    MVC   TRKAGY,TAGTAGY      IF IT DOES, ALWAYS DISPLAY AGENCY            
         SPACE 1                                                                
GGT4D    MVC   TRKAMT,TAGTCRD      DISPLAY CREDIT AMOUNT                        
         CLI   TAGTINV+5,3         IF THIS ISN'T A T&R CONVERTED INV            
         BE    GGT6                                                             
         LA    RE,TIKEY+TLFTTRK-TLFTD                                           
         CLI   RECNUM,FT                                                        
         BE    GGT5                                                             
         LA    RE,TIKEY+TLETTRK-TLETD                                           
         CLI   RECNUM,ETR                                                       
         BE    GGT5                                                             
         LA    RE,TIKEY+TLGTTRK-TLGTD                                           
GGT5     TM    ADDSTAT,ADDMANUL                                                 
         BO    GGT5A                                                            
         CLC   =X'FFFE',0(RE)      AND THIS ISN'T INITIAL RECORD                
         BE    GGT6                                                             
         CLC   TAGTUSE,=C'GRR'     (TREAT ALL GRR'S COVERING                    
         BNE   GGT5A               SPECIFIC USE AS INITIAL RECORD)              
         CLI   TAGTTYPE,0                                                       
         BNE   GGT6                                                             
GGT5A    ICM   R1,15,TAGTPAY       AND THERE'S A PAYMENT AMOUNT                 
         BZ    GGT6                                                             
         OC    TRKAMT,TRKAMT       THEN IF THERE'S BOTH CREDITS & PYMT          
         BZ    *+12                                                             
         ST    R1,TRKPAY           SET TO DISPLAY PAYMENT SEPARATELY            
         B     *+8                                                              
         ST    R1,TRKAMT           ELSE DISPLAY PAYMENT IN CREDIT AMT           
         SPACE 1                                                                
GGT6     CLI   DBAL,C'N'           IF BALANCE IS ASCENDING                      
         BNE   GGT7                                                             
         XC    TRKAMT,=X'FFFFFFFF' DISPLAY APPLIED AMOUNT AS OPPOSITE           
         L     RE,TRKAMT                                                        
         AHI   RE,1                                                             
         ST    RE,TRKAMT                                                        
         SPACE 1                                                                
GGT7     OC    TAGTSTRT,TAGTSTRT   CYCLE DATES                                  
         BZ    GGT8                                                             
         GOTO1 DATCON,DMCB,(X'11',TAGTSTRT),(8,TRKCYC)                          
         SPACE 1                                                                
GGT8     OC    TAGTUSE,TAGTUSE     USE CODE                                     
         BZ    GGT14                                                            
         MVC   TRKUSNME,SPACES     PRE-CLEAR USE DETAILS                        
         MVC   TRKUSDTL,SPACES                                                  
         GOTO1 USEVAL,DMCB,TAGTUSE,TAGTTYPE                                     
         BNE   GGT14                                                            
         SPACE                                                                  
         CLI   TAGTTYPE,0           IF FTRACK FOR SPECIFIC TYPE                 
         BE    GGT8A                GRR, DISPLAY FULL USE NAME                  
         CLI   TGUSEQU,UGRR                                                     
         BE    GGT9                                                             
         SPACE 1                                                                
GGT8A    CLI   RECNUM,ETR           IF ETRACK, DISPLAY FULL USE NAME            
         BE    GGT9                                                             
         MVC   TRKUSNME(3),TGUSCDE  ELSE ONLY DISPLAY USE CODE                  
         TM    WHEN,X'80'           IF ONLINE                                   
         BO    *+10                                                             
GGT9     MVC   TRKUSNME(L'TGUSNAME),TGUSNAME                                    
         SPACE 1                                                                
         LA    R2,TRKUSDTL         R2=OUTPUT AREA                               
         SPACE 1                                                                
         CLI   TAGTINV+5,3         DON'T BOTHER IF T&R CONVERTED INV            
         BE    GGT14                                                            
         TM    TGUSTYST,USES       USE NUMBERS PRESENT                          
         BZ    GGT10                                                            
         LH    RF,TAGTSTUS         RF=STARTING USE NUMBER                       
         BAS   RE,EDITIT                                                        
         CLC   TAGTUSES,=H'1'      FINISHED IF ONE USE                          
         BE    GGT10                                                            
         BCTR  R2,0                                                             
         MVI   0(R2),C'-'                                                       
         LA    R2,1(R2)                                                         
         AH    RF,TAGTUSES         + N'USES PAID                                
         BCTR  RF,0                - 1                                          
         BAS   RE,EDITIT           = ENDING USE NUMBER                          
         SPACE 1                                                                
GGT10    TM    TGUSTYST,MAJORS     MAJORS                                       
         BZ    GGT12                                                            
         GOTO1 MAJVAL,DMCB,(X'80',TAGTMAJ)                                      
         MVC   0(L'TGMACHAR,R2),TGMACHAR                                        
         LA    R2,L'TGMACHAR+1(R2)                                              
         SPACE 1                                                                
GGT12    TM    TGUSTYST,UNITS      N'UNITS                                      
         BZ    GGT14                                                            
         LH    RF,TAGTUNIT                                                      
         BAS   RE,EDITIT                                                        
         SPACE 1                                                                
GGT14    XC    BLOCK(100),BLOCK    INITIALIZE BLOCK FOR COMMENT                 
         MVI   BLOCK,L'LINCMNT+8                                                
         SPACE 1                                                                
         LTR   R4,R4               IF DON'T HAVE A(TRACKING ELEMENT)            
         BZ    GGT17                                                            
         OC    TAGTINV,TAGTINV     OR NO INVOICE NUMBER (ALSO ADJ.)             
         BZ    GGT17               SKIP TO COMMENT HANDLING                     
         SPACE 1                                                                
GGT16    TM    TAGTINV+5,X'0F'     IF THIS IS NOT A CONVERTED INV               
         BNZ   GGT17                                                            
         LA    RF,TACMTYPG         LOOK FOR GENERAL COMMENT                     
         B     *+8                                                              
*                                  ENTER HERE FOR ADJUSTMENTS                   
GGT17    LA    RF,TACMTYPH         ELSE LOOK FOR HISTORY COMMENT                
         L     R0,AIO                                                           
         MVC   AIO,TIAREC                                                       
         GOTO1 CHAROUT,DMCB,TACMELQ,BLOCK,(RF)  RETURN IN BLOCK+8               
         ST    R0,AIO                                                           
         SPACE 1                                                                
         BAS   RE,GRRMADDS         SETUP GRR MANUAL ADDS                        
         BAS   RE,GRRMAPPS         SETUP GRR MANUAL APPLICATIONS                
         SPACE 1                                                                
         GOTO1 SQUASHER,DMCB,TRKUS,L'TRKUS  SQUASH USE DETAILS                  
         MVC   TRKUSLEN,DMCB+7     SAVE ACTUAL LENGTH OF USE DETAILS            
         SPACE 1                                                                
         CLI   RECNUM,GT           IF THIS IS GUARANTEE TRACKING                
         BE    *+12                                                             
         CLI   RECNUM,GX                                                        
         BNE   GGTX                                                             
         LTR   R4,R4               AND WE HAVE A(TRACKING ELEMENT)              
         BZ    GGTX                                                             
         OC    TAGTAPPL,TAGTAPPL   AND APPLIED CREDITS TAKEN                    
         BZ    GGTX                                                             
         MVC   WORK,BLOCK+8                    SAVE COMMENT                     
         MVC   BLOCK+8(L'FCYCLIT),FCYCLIT      TOOK FIXED CYCLE CREDITS         
         MVC   BLOCK+8+L'FCYCLIT(L'WORK),WORK  RESTORE COMMENT                  
         SPACE 1                                                                
GGTX     OC    BLOCK+8(100),SPACES                                              
         B     YES                                                              
         SPACE 3                                                                
EDITIT   DS    0H                  DISPLAY BINARY AMOUNT                        
         EDIT  (RF),(4,(R2)),ALIGN=LEFT,ZERO=NOBLANK                            
         LR    R1,R0                                                            
         LA    R2,1(R1,R2)                                                      
         BR    RE                                                               
         EJECT                                                                  
*              ROUTINE TO DISPLAY STAFF MEMBER WHO MADE THE ADJUSTMENT          
         SPACE 2                                                                
WHOADJ   NTR1                                                                   
         CLI   RECNUM,GT           IF RECORD/ACTION IS GTRACK/REPORT            
         BNE   XIT                                                              
         SPACE 1                                                                
         USING TAGTD,R4                                                         
         L     R4,TIAREC           R4=A(GUARANTEE TRACKING RECORD)              
         MVI   ELCODE,TAGTELQ      GET GUARANTEE TRACKING ELEMENT               
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         OC    TAGTINV,TAGTINV     IF ADDED MANUALLY ...                        
         BNZ   XIT                                                              
         DROP  R4                                                               
         SPACE 1                                                                
         USING TAACD,R4                                                         
         L     R4,TIAREC           R4=A(GUARANTEE TRACKING RECORD)              
         MVI   ELCODE,TAACELQ      GET ACTIVITY ELEMENT                         
         BAS   RE,GETEL            AND DISPLAY THE STAFF ID                     
         BNE   XIT                                                              
         MVC   ADJDSCRP(L'ADJLITBY),ADJLITBY                                    
         MVC   ADJDSCRP+L'ADJLITBY(L'TAACSTAF),TAACSTAF                         
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO DISPLAY DETAILS FOR A MANUALLY ADDED                  
*              GRR FTRACK                                                       
         SPACE 2                                                                
GRRMADDS NTR1                                                                   
         CLI   RECNUM,FT           IF RECORD/ACTION IS FTRACK/REPORT            
         BNE   XIT                                                              
         SPACE 1                                                                
         USING TAGTD,R4                                                         
         L     R4,TIAREC           R4=A(FIXED CYCLE TRACKING RECORD)            
         MVI   ELCODE,TAGTELQ      GET GUARANTEE TRACKING ELEMENT               
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         CLC   TAGTUSE,=C'GRR'     REJECT IF USE IS NOT GRR                     
         BNE   XIT                                                              
         CLI   TAGTTYPE,0          REJECT IF NOT COVERING SPECIFIC USE          
         BE    XIT                                                              
         OC    TAGTINV,TAGTINV     REJECT IF ADDED VIA PAY                      
         BNZ   XIT                                                              
         DROP  R4                                                               
         SPACE 1                                                                
         MVC   ADJDSCRP(L'ADJLIT2),ADJLIT2                                      
         MVC   ADJDSCRP+L'ADJLIT2(15),TRKUS                                     
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO DISPLAY DETAILS FOR A MANUALLY ADDED                  
*              GRR APPLICATION                                                  
         SPACE 2                                                                
GRRMAPPS NTR1                                                                   
         CLI   RECNUM,FT           IF RECORD/ACTION IS FTRACK/REPORT            
         BNE   XIT                                                              
         SPACE 1                                                                
         USING TAGTD,R4                                                         
         L     R4,TIAREC           R4=A(FIXED CYCLE TRACKING RECORD)            
         MVI   ELCODE,TAGTELQ      GET GUARANTEE TRACKING ELEMENT               
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         TM    TAGTSTAT,TAGTSGRR   REJECT IF NOT AN APPLICATION TO              
         BZ    XIT                 A GRR CYCLE                                  
         DROP  R4                                                               
         SPACE 1                                                                
         MVC   ADJDSCRP(L'ADJLIT2),ADJLIT2                                      
         MVC   ADJDSCRP+L'ADJLIT2(L'TGUSCDE),TGUSCDE                            
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO FILTER TRACKING BY USE                                
         SPACE 2                                                                
FILTUSE  NTR1                                                                   
         CLI   RECNUM,FT           IF RUNNING FTRACK REPORT                     
         BNE   YES                                                              
         CLI   FLTUSEQU,0          AND USING THE USE FILTER ...                 
         BE    YES                                                              
         SPACE 1                                                                
         USING TAGTD,R4                                                         
         L     R4,TIAREC                                                        
         MVI   ELCODE,TAGTELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   NO                                                               
         CLC   FLTUSECD,TAGTUSE    ONLY ACCEPT FTRACKS FOR THE USE              
         BE    YES                                                              
         CLC   TAGTUSE,=C'GRR'     OR RADIO GUARANTEES THAT COVER               
         BNE   NO                  THE USE                                      
         CLC   FLTUSEQU,TAGTTYPE                                                
         BE    YES                                                              
         B     NO                                                               
         DROP  R4                                                               
         EJECT                                                                  
*              HEADLINE HOOK                                                    
         SPACE 2                                                                
HOOK     NTR1                                                                   
         CLI   COMPLETE,C'Y'       IF FULL PAGE WILL BE PRINTED                 
         BNE   *+10                                                             
         MVC   HEAD1+124(L'CMPLIT),CMPLIT DISPLAY COMPLETE NEXT TO PAGE         
         SPACE 1                                                                
         MVC   HEAD4+14(9),TGSSN     SOCIAL SECURITY NUMBER                     
         MVC   HEAD4+28(32),SSNNAME     AND NAME                                
         SPACE 1                                                                
         MVC   HEAD4+14(9),SPACES                                               
         GOTO1 SSNPACK,DMCB,TGSSN,HEAD4+14                                      
         SPACE 1                                                                
         LA    RF,HEAD5            FLOAT REMAINDER OF LHS                       
         CLI   CRPCODE,0                IF AROUND, PRINT CORP DETAILS           
         BE    HK4                                                              
         MVC   1(L'CORPLIT,RF),CORPLIT  CORPORATION LITERAL                     
         MVC   14(L'CRPCODE,RF),CRPCODE CORPORATION FID NUMBER                  
         MVC   28(L'CRPNAME,RF),CRPNAME         AND NAME                        
         SPACE 1                                                                
         LR    R2,RF                                                            
         MVC   14(L'CRPCODE,R2),SPACES                                          
         GOTO1 SSNPACK,DMCB,CRPCODE,14(R2)                                      
         LR    RF,R2                                                            
         SPACE 1                                                                
HK3      LA    RF,132(RF)                                                       
         SPACE 1                                                                
HK4      CLC   AGYNAME,SPACES           IF AROUND, PRINT AGENCY DETAILS         
         BNH   HK5                                                              
         MVC   1(L'AGYLIT,RF),AGYLIT    AGENCY LITERAL                          
         MVC   14(L'TGAGY,RF),TGAGY     AGENCY CODE                             
         MVC   28(L'AGYNAME,RF),AGYNAME    AND NAME                             
         LA    RF,132(RF)                                                       
         SPACE 1                                                                
HK5      CLC   CLINAME,SPACES           IF AROUND, PRINT CLIENT DETAILS         
         BNH   HK6                                                              
         MVC   1(L'CLILIT,RF),CLILIT    CLIENT LITERAL                          
         MVC   14(L'TGCLI,RF),TGCLI     CLIENT CODE                             
         MVC   28(L'CLINAME,RF),CLINAME    AND NAME                             
         SPACE 1                                                                
HK6      CLI   RECNUM,GT           IF THIS IS GUARANTEE TRACKING                
         BE    *+12                                                             
         CLI   RECNUM,GX                                                        
         BNE   HK12                                                             
         MVC   HEAD8+14(80),GUARCMT1    FIRST COMMENT LINE                      
         MVC   HEAD9+14(80),GUARCMT2    SECOND COMMENT LINE                     
         SPACE 1                                                                
         MVC   HEAD4+111(4),SGTCODE   GUARANTEE CODE                            
         MVC   HEAD5+108(1),DBAL         DESC. BAL                              
         MVC   HEAD5+119(1),PAYPNH       PAY P&H                                
         MVC   HEAD5+131(1),PAYOVER      PAY OVERAGE                            
         SPACE 1                                                                
         OC    TIFUSE,TIFUSE          IF USE FILTER DEFINED                     
         BZ    *+16                                                             
         MVC   HEAD7+99(8),USELIT     DISPLAY IT                                
         MVC   HEAD7+111(3),TIFUSE                                              
         SPACE 1                                                                
         MVC   HEAD6+111(17),DPERIOD  PERIOD                                    
         MVC   HEAD8+111(12),DAMOUNT  AMOUNT                                    
         MVC   HEAD9+111(12),DBALANCE BALANCE                                   
         B     HK20                                                             
         SPACE 1                                                                
HK12     MVC   HEAD7+14(12),TGCID     COMMERCIAL ID                             
         MVC   HEAD7+28(32),COMNAME          AND NAME                           
         MVC   HEAD8+14(3),SFTCAT     CATEGORY                                  
         SPACE 1                                                                
         MVC   HEAD7+111(12),DAMOUNT  AMOUNT                                    
         MVC   HEAD8+111(12),DBALANCE BALANCE                                   
         CLI   RECNUM,FT                                                        
         BNE   *+14                                                             
         MVC   HEAD5+111(17),DPERIOD  IF FTRACK, DISPLAY PERIOD                 
         B     HK20                                                             
         MVC   HEAD5+111(5),SETEPI    ELSE DISPLAY EPISODE                      
         SPACE 1                                                                
HK20     CLI   OFFLINE,C'Y'        IF OFFLINE                                   
         BNE   HKX                                                              
         L     R4,ABOX             SET UP BOXES                                 
         USING BOXD,R4                                                          
         MVC   BOXROWS,SPACES      SET ROWS                                     
         MVI   BOXROWS+9,C'T'      TOP                                          
         MVI   BOXROWS+11,C'M'     MIDDLE                                       
         MVI   BOXROWS+66,C'B'     BOTTOM                                       
         SPACE 1                                                                
         MVC   BOXCOLS,SPACES      SET COLUMNS                                  
         LA    R2,BOXCOLS                                                       
         USING PRNTD,R2            USE PRINT LINE DSECT                         
         MVI   BXL,C'L'                                                         
         MVI   BC9,C'C'                                                         
         MVI   BC10,C'C'                                                        
         MVI   BC11,C'C'                                                        
         MVI   BXR,C'R'                                                         
         SPACE                                                                  
         CLI   RECNUM,FT           IF THIS IS FTRACK REPORT                     
         BE    HK22                                                             
         CLI   RECNUM,ETR          OR ETRACK REPORT                             
         BNE   *+12                                                             
HK22     SH    R2,=H'7'            SKIP OVER AGENCY COLUMN                      
         B     *+8                                                              
         MVI   BC0,C'C'            ELSE SET FOR AGENCY COLUMN                   
         SPACE                                                                  
         MVI   BC1,C'C'                                                         
         MVI   BC2,C'C'                                                         
         MVI   BC3,C'C'                                                         
         MVI   BC4,C'C'                                                         
         MVI   BC5,C'C'                                                         
         MVI   BC6,C'C'                                                         
         MVI   BC7,C'C'                                                         
         MVI   BC8,C'C'                                                         
         SPACE 1                                                                
         MVI   BOXYORN,C'Y'        INITIALIZE REMAINING FLDS                    
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
HKX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        ROUTINE CHECKS IF STAFF HAS ACCESS TO THIS GUARANTEE         *         
*        ON ENTRY ... R4=A(GUARANTEE DETAILS ELEMENT)                 *         
***********************************************************************         
                                                                                
         USING TAGUD,R4                                                         
CHKACC   NTR1                                                                   
         OC    TAGUAGY(L'TAGUAGY+L'TAGUCLI),TAGUAGY                             
         JNZ   CACC10                                                           
         TM    TGCTSTST,TGCTSCLI                                                
         JO    RECNTFND                                                         
*                                                                               
CACC10   MVC   SAVEKEY,KEY       SAVE GUARANTEE KEY                             
         MVC   AIO,AIO2          AND PREPARE TO USE AIO2                        
         XR    R0,R0                                                            
*                                                                               
         OC    TAGUAGY,TAGUAGY   IF AGENCY DEFINED                              
         BZ    CACC20            TEST ACCESS                                    
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'80',TAGUAGY)                              
         BE    CACC20                                                           
         LHI   R0,1                                                             
*                                                                               
CACC20   OC    TAGUCLI,TAGUCLI   IF CLIENT DEFINED                              
         BZ    CACC30            TEST ACCESS                                    
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'80',TAGUCLI)                              
         BE    CACC30                                                           
         LHI   R0,1                                                             
*                                                                               
CACC30   LTR   R0,R0             IF USING NEW GUARANTEE SYSTEM                  
         BZ    CACC60            AND AGY/CLI MATCH NOT YET FOUND                
*                                                                               
         USING TAVAD,R4                                                         
         L     R4,AIO1           R4=A(GUARANTEE RECORD)                         
         MVI   ELCODE,TAVAELQ    TRY TO GET NEW SYSTEM AGENCY/CLIENT            
         BAS   RE,GETEL          ELEMENT                                        
         B     *+8                                                              
CACC40   BRAS  RE,NEXTEL                                                        
         BNE   THEEND                                                           
*                                                                               
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'80',TAVAAGY)                              
         BNE   CACC40                                                           
*                                                                               
         CLI   TAVALEN,TAVALNQ   IF NO CLIENT LIMITS ARE DEFINED                
         BE    CACC60            ACCESS IS GRANTED                              
*                                                                               
         ZIC   R2,TAVALEN                                                       
         SHI   R2,TAVALNQ                                                       
         LA    R3,TAVACLI                                                       
CACC50   GOTO1 RECVAL,DMCB,TLCLCDQ,(X'80',(R3))                                 
         BE    CACC60                                                           
         LA    R3,L'TAVACLI(R3)                                                 
         SHI   R2,L'TAVACLI                                                     
         LTR   R2,R2                                                            
         JNZ   CACC50                                                           
         B     CACC40                                                           
         DROP  R4                                                               
*                                                                               
CACC60   MVC   KEY,SAVEKEY         RESET KEY                                    
         B     XIT                                                              
         EJECT                                                                  
*              EXITS, ETC.                                                      
         SPACE 2                                                                
FLDINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     THEEND                                                           
         SPACE 1                                                                
FLDMISS  MVI   ERROR,MISSING       MISSING INPUT FIELD                          
         B     THEEND                                                           
         SPACE 1                                                                
RECNTFND MVI   ERROR,NOTFOUND      RECORD NOT FOUND                             
         B     THEEND                                                           
         SPACE 1                                                                
NOTRK    MVI   ERROR,ERNOTRK       NO TRACKING RECS FOUND                       
         B     THEEND                                                           
         SPACE 1                                                                
INVTYPE  MVI   ERROR,ERRECCTY      INVALID REC FOR THIS COMMERCIAL TYPE         
         B     THEEND                                                           
         SPACE 1                                                                
TRKMORE  MVI   MYMSGNO1,33         TRACKING DISP. - HIT ENTER FOR NEXT          
         B     *+8                                                              
TRKDISP  MVI   MYMSGNO1,34         TRACKING DISP.                               
         OI    GENSTAT2,USGETTXT                                                
         L     R2,AFRSTKEY                                                      
         B     THEEND                                                           
         SPACE 1                                                                
THEEND   GOTO1 EXIT,DMCB,0                                                      
         SPACE 2                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 1                                                                
GTPFTAB  DS    0C                                                               
         DC    AL1(PF13X-*,13,0,0,0)                                            
         DC    CL3' ',CL8'GRT     ',CL8'LIST    '                               
PF13X    EQU   *                                                                
         DC    AL1(PF14X-*,14,0,0,0)                                            
         DC    CL3' ',CL8'GRT     ',CL8'DISPLAY '                               
PF14X    EQU   *                                                                
         DC    AL1(PF15X-*,15,0,(PF15X-PF15)/KEYLNQ,0)                          
         DC    CL3' ',CL8'GTRACK  ',CL8'ADD     '                               
PF15     DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYTWA,8-1),AL2(SGTPD-T702FFD)                             
PF15X    EQU   *                                                                
         DC    AL1(PF16X-*,16,0,0,0)                                            
         DC    CL3' ',CL8'GCAST   ',CL8'LIST    '                               
PF16X    EQU   *                                                                
         DC    X'FF'                                                            
         SPACE 2                                                                
FTPFTAB  DS    0C                                                               
         DC    AL1(FPF13X-*,13,0,(FPF13X-FPF13)/KEYLNQ,0)                       
         DC    CL3' ',CL8'CHECK   ',CL8'LIST    '                               
FPF13    DC    AL1(KEYTYGLB,L'TGSSN-1),AL2(TGSSN-TGD)                           
         DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYGLB,L'TGCID-1),AL2(TGCID-TGD)                           
FPF13X   EQU   *                                                                
         DC    AL1(FPF14X-*,14,0,(FPF14X-FPF14)/KEYLNQ,0)                       
         DC    CL3' ',CL8'FTRACK  ',CL8'ADD     '                               
FPF14    DC    AL1(KEYTYGLB,L'TGSSN-1),AL2(TGSSN-TGD)                           
         DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYGLB,L'TGCID-1),AL2(TGCID-TGD)                           
         DC    AL1(KEYTYWS,L'CASTSEQ-1),AL2(CASTSEQ-LOCALD)                     
         DC    AL1(KEYTYTWA,8-1),AL2(SFTPD-T702FFD)                             
FPF14X   EQU   *                                                                
         DC    AL1(FPF15X-*,15,0,(FPF15X-FPF15)/KEYLNQ,0)                       
         DC    CL3' ',CL8'FTRACK  ',CL8'LIST    '                               
FPF15    DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYGLB,L'TGCID-1),AL2(TGCID-TGD)                           
FPF15X   EQU   *                                                                
         DC    AL1(FPF16X-*,16,0,(FPF16X-FPF16)/KEYLNQ,0)                       
         DC    CL3' ',CL8'CERROR  ',CL8'DISPLAY '                               
FPF16    DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYWS,L'CASTSEQ-1),AL2(CASTSEQ-LOCALD)                     
FPF16X   EQU   *                                                                
         DC    X'FF'                                                            
         SPACE 2                                                                
ETPFTAB  DS    0C                                                               
         DC    AL1(EPF13X-*,13,0,(EPF13X-EPF13)/KEYLNQ,0)                       
         DC    CL3' ',CL8'CHECK   ',CL8'LIST    '                               
EPF13    DC    AL1(KEYTYGLB,L'TGSSN-1),AL2(TGSSN-TGD)                           
         DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYGLB,L'TGCID-1),AL2(TGCID-TGD)                           
EPF13X   EQU   *                                                                
         DC    AL1(EPF14X-*,14,0,(EPF14X-EPF14)/KEYLNQ,0)                       
         DC    CL3' ',CL8'ETRACK  ',CL8'ADD     '                               
EPF14    DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYWS,L'CASTSEQ-1),AL2(CASTSEQ-LOCALD)                     
         DC    AL1(KEYTYTWA,8-1),AL2(SFTPD-T702FFD)                             
EPF14X   EQU   *                                                                
         DC    AL1(EPF15X-*,15,0,(EPF15X-EPF15)/KEYLNQ,0)                       
         DC    CL3' ',CL8'ECAST   ',CL8'DISPLAY '                               
EPF15    DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYGLB,L'TGCID-1),AL2(TGCID-TGD)                           
         DC    AL1(KEYTYGLB,L'TGEPI-1),AL2(TGEPI-TGD)                           
         DC    AL1(KEYTYGLB,L'TGSSN-1),AL2(TGSSN-TGD)                           
         DC    AL1(KEYTYGLB,L'TGCAT-1),AL2(TGCAT-TGD)                           
EPF15X   EQU   *                                                                
         DC    AL1(EPF16X-*,16,0,(EPF16X-EPF16)/KEYLNQ,0)                       
         DC    CL3' ',CL8'ECAST   ',CL8'LIST    '                               
EPF16    DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYGLB,L'TGCID-1),AL2(TGCID-TGD)                           
         DC    AL1(KEYTYGLB,L'TGEPI-1),AL2(TGEPI-TGD)                           
EPF16X   EQU   *                                                                
         DC    X'FF'                                                            
         SPACE 1                                                                
HEXFFS   DC    6X'FF'                                                           
ADJLIT   DC    C'MANUAL ADJUSTMENT'                                             
ADJLIT2  DC    C'MANUAL ADJ. '                                                  
ADJLITBY DC    C'MANUAL ADJUSTMENT by '                                         
CORPLIT  DC    C'Corporation'                                                   
AGYLIT   DC    C'Agency'                                                        
CLILIT   DC    C'Client'                                                        
USELIT   DC    C'Use Code'                                                      
FCYCLIT  DC    C'(-BSS/HLD) '                                                   
CRLIT    DC    C'cr'                                                            
CMPLIT   DC    C'Complete'                                                      
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              REPORT SPECS                                                     
         SPACE 1                                                                
*                                  SPROG 0,2,4=BOXES                            
MYSPECS  DS    0H                        1,3,5=NO BOXES                         
         SPROG 0,1,2,3,4,5                                                      
         SSPEC H1,2,RUN                                                         
         SSPEC H1,100,REPORT                                                    
         SSPEC H1,117,PAGE                                                      
         SSPEC H2,100,REQUESTOR                                                 
*                                                                               
         SSPEC H4,2,C'S/S Number'                                               
*                                                                               
         SSPEC H11,95,CL37'    Applied         P&&H      Balance'               
*                                                                               
         SPROG 0,1                                                              
         SSPEC H1,54,C'Guarantee Tracking Report'                               
         SSPEC H8,2,C'Comment'                                                  
         SSPEC H4,100,C'Guar Code'                                              
         SSPEC H5,100,CL32'Desc Bal=   Pay P&&H=   Pay Over='                   
         SSPEC H6,100,C'Period'                                                 
         SSPEC H8,100,C'Amount'                                                 
         SSPEC H9,100,C'Balance'                                                
         SSPEC H11,2,C'Agency Invoice  Commercial   Cat Cam Yr  Ov1'            
         SSPEC H11,47,C'Ov2 Cycle Dates       Use Details'                      
*                                                                               
         SPROG 2,3                                                              
         SSPEC H1,53,C'Fixed Cycle Tracking Report'                             
         SSPEC H5,100,C'Period'                                                 
         SSPEC H11,2,C'Invoice  Commercial   Cat Cam Yr  Ov1 Ov2'               
*                                                                               
         SPROG 4,5                                                              
         SSPEC H1,50,C'Ecast Tracking Report'                                   
         SSPEC H5,100,C'Episode'                                                
         SSPEC H11,2,C'Invoice  Commercial   Cat Cam Yr'                        
*                                                                               
         SPROG 2,3,4,5                                                          
         SSPEC H7,2,C'Commercial'                                               
         SSPEC H8,2,C'Category'                                                 
         SSPEC H7,100,C'Amount'                                                 
         SSPEC H8,100,C'Balance'                                                
         SSPEC H11,44,C'Cycle Dates       Use Details'                          
*                                                                               
         SPROG 0                                                                
         SSPEC H2,54,25X'BF'                                                    
*                                                                               
         SPROG 1                                                                
         SSPEC H2,54,25C'-'                                                     
         SSPEC H12,2,C'------ -------  ----------   --- --- -- ----'            
         SSPEC H12,47,C'--- -----------       -----------'                      
*                                                                               
         SPROG 2                                                                
         SSPEC H2,53,27X'BF'                                                    
*                                                                               
         SPROG 3                                                                
         SSPEC H2,53,27C'-'                                                     
         SSPEC H12,2,C'-------  ----------   --- --- --- --- ---'               
*                                                                               
         SPROG 4                                                                
         SSPEC H2,50,21X'BF'                                                    
*                                                                               
         SPROG 5                                                                
         SSPEC H2,50,21C'-'                                                     
         SSPEC H12,2,C'-------  ----------   --- --- ---'                       
*                                                                               
         SPROG 3,5                                                              
         SSPEC H12,44,C'-----------       -----------'                          
*                                                                               
         SPROG 1,3,5                                                            
         SSPEC H12,95,CL37'    -------         ---      -------'                
         DC    X'00'                                                            
         EJECT                                                                  
*              DSECT TO COVER PROGRAM SAVED STORAGE                             
         SPACE 1                                                                
LOCALD   DSECT                                                                  
AFSTH    DS    A                   A(FIRST DATA LINE)                           
ALSTH    DS    A                   A(LAST DATA LINE)                            
AVKEY    DS    A                   A(VALKEY ROUTINE)                            
ANXTDA   DS    A                   A(NEXT D/A TO PROCESS)                       
ATMPSTOR DS    A                   A(TEMPORARY STORAGE)                         
TMPLNQ   EQU   32000               L'TEMP STORAGE                               
*                                                                               
PGSW     DS    CL1                 PAGING SWITCH TO BE USED BY PGCNTL           
FRSTLINE DS    XL1                 FIRST PRINT LINE                             
NUMLINES DS    XL1                 N'PRINT LINES ON A PAGE                      
CASTSEQ  DS    CL3                 CAST INPUT SEQ NUMBER (EBCDIC)               
AGYSTAT2 DS    XL1                 AGENCY STATUS BYTE 2                         
COMPLETE DS    CL1                 Y=FULL PAGE IS PRINTED                       
*                                                                               
DPERIOD  DS    CL17                DISPLAYABLE PERIOD                           
DAMOUNT  DS    CL12                RIGHT-ALIGNED FOR PRINTED REPORT             
DBALANCE DS    CL12                RIGHT-ALIGNED FOR PRINTED REPORT             
*                                                                               
SSNNAME  DS    CL32                S/S NAME                                     
CRPCODE  DS    CL9                 CORP ID NUMBER                               
CRPNAME  DS    CL32                CORP NAME                                    
AGYNAME  DS    CL36                AGENCY NAME                                  
CLINAME  DS    CL36                CLIENT NAME                                  
COMNAME  DS    CL36                COMMERCIAL NAME                              
*                                                                               
FLTUSECD DS    CL3                 USE FILTER (CODE)                            
FLTUSEQU DS    XL1                 USE FILTER (EQUATE)                          
*                                                                               
ADJDSCRP DS    CL37                ADJUSTEMENT DESCRIPTION                      
*                                                                               
DBAL     DS    CL1                 GUAR STATUS SWITCHES - DESC BALANCE          
PAYPNH   DS    CL1                                        PAY P&H               
PAYOVER  DS    CL1                                        PAY OVERAGE           
*                                                                               
ADDSTAT  DS    X                   GUARANTEE ADD STATUS                         
ADDMANUL EQU   X'80'               GUARANTEE ADDED MANUALLY                     
*                                                                               
SAVEKEY  DS    CL(L'KEY)           SAVED KEY                                    
SAVEAGY  DS    CL(L'TAGUAGY)       SAVED AGENCY                                 
*                                                                               
DSPLINE  DS    H                   DISP. TO NEXT DISPLAY LINE                   
GUARCMT1 DS    CL80                GUARANTEE COMMENT 1                          
GUARCMT2 DS    CL80                GUARANTEE COMMENT 2                          
*                                                                               
TRKDETS  DS    0A                  CURRENT TRACKING RECORD DETAILS              
TRKELEM  DS    A                   A(TRACKING ELEMENT)                          
TRKINV   DS    CL6                 INVOICE NUMBER                               
TRKAGY   DS    CL6                 AGENCY                                       
TRKCID   DS    CL12                COMM'L ID                                    
TRKCYC   DS    CL17                CYCLE DATES                                  
*                                                                               
TRKUS    DS    0CL37                                                            
TRKUSNME DS    CL17                USE NAME                                     
TRKUSDTL DS    CL20                USE DETAILS                                  
*                                                                               
TRKOV1   DS    CL6                 OVERSCALE RATE 1                             
TRKOV2   DS    CL6                 OVERSCALE RATE 2                             
TRKAMT   DS    F                   AMOUNT TO BE DISP. IN APPLIED COLUMN         
TRKPAY   DS    F                   PAY AMOUNT IF THERE WERE CREDITS TOO         
TRKLNQ   EQU   *-TRKDETS                                                        
*                                                                               
NOROOM   DS    CL1                 Y=NOT ENOUGH ROOM IN TEMP STORAGE            
*                                    FOR RECORDS                                
TRKUSLEN DS    XL1                 ACTUAL LENGTH OF TRKUS AFTER SQUASH          
*                                                                               
PGTBL    DS    CL(16*L'TLRCKEY)    SAVED KEYS FOR PAGE CONTROL RTN.             
         EJECT                                                                  
*              DSECT TO COVER SCREEN DISPLAY LINES                              
         SPACE 2                                                                
LINED    DSECT                                                                  
LININV   DS    CL6                 INVOICE NUMBER                               
         DS    CL1                                                              
LINAGY   DS    CL5                 AGENCY                                       
         DS    CL1                                                              
LINCYC   DS    CL17                CYCLE DATES                                  
         DS    CL1                                                              
LINCAT   DS    CL3                 CATEGORY                                     
         DS    CL1                                                              
LINCAM   DS    CL3                 ON/OFF CAMERA                                
         DS    CL1                                                              
LINOV1   DS    CL3                 OVERSCALE RATE 1                             
         DS    CL1                                                              
LINOV2   DS    CL3                 OVERSCALE RATE 2                             
         DS    CL1                                                              
LINCRD   DS    CL10                APPLIED AMOUNT                               
LINPNH   DS    CL10                PENSION AND HEALTH                           
LINBAL   DS    CL12                GUARANTEE BALANCE                            
         DS    CL8                                                              
LINCID   DS    CL12                COMMERCIAL ID                                
         DS    CL1                                                              
LINUSE   DS    CL21                USE DETAILS                                  
         DS    CL1                                                              
LINCMNT  DS    CL40                COMMENT                                      
         ORG   LINCMNT                                                          
         DS    CL10                                                             
LINPAY   DS    CL10                PAYMENT WHEN THERE ARE CREDITS TOO           
         ORG                                                                    
         DS    CL4                                                              
         DS    CL8                                                              
LINNEXT  EQU   *                   START OF NEXT LINE                           
         EJECT                                                                  
*              DSECT TO COVER PRINT LINE                                        
         SPACE 2                                                                
PRNTD    DSECT                                                                  
BXL      DS    CL1                                                              
PRNTAGY  DS    CL6                 AGENCY                                       
BC0      DS    CL1                                                              
PRNTINV  DS    CL6                 INVOICE NUMBER                               
PRNTCR   DS    CL2                 CREDIT INVOICE INDICATOR                     
BC1      DS    CL1                                                              
PRNTCID  DS    CL12                COMMERCIAL ID                                
BC2      DS    CL1                                                              
PRNTCAT  DS    CL3                 CATEGORY                                     
BC3      DS    CL1                                                              
PRNTCAM  DS    CL3                 ON/OFF CAMERA                                
BC4      DS    CL1                                                              
PRNTYR   DS    CL3                 CONTRACT YEAR                                
BC5      DS    CL1                                                              
PRNTOV1  DS    CL3                 OVERSCALE RATE 1                             
BC6      DS    CL1                                                              
PRNTOV2  DS    CL3                 OVERSCALE RATE 2                             
BC7      DS    CL1                                                              
PRNTCYC  DS    CL17                CYCLE DATES                                  
BC8      DS    CL1                                                              
PRNTUSE  DS    CL25                USE DETAILS                                  
BC9      DS    CL1                                                              
PRNTCRD  DS    CL12                APPLIED AMOUNT                               
BC10     DS    CL1                                                              
PRNTPNH  DS    CL11                PENSION AND HEALTH                           
BC11     DS    CL1                                                              
PRNTBAL  DS    CL12                GUARANTEE BALANCE                            
BXR      DS    CL1                                                              
*                                                                               
         ORG   PRNTUSE+132         SET NEXT FIELD UNDER USE DTLS.               
PRNTCMNT DS    CL25                COMMENT                                      
*                                                                               
         ORG   PRNTCRD+132         SET NEXT FIELD UNDER CREDIT AMOUNT           
PRNTPAY  DS    CL12                PAYMENT WHEN THERE ARE CREDITS TOO           
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR88D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR41D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR8AD                                                       
         EJECT                                                                  
       ++INCLUDE TASYSIOD                                                       
         EJECT                                                                  
* DDGENTWA      (MUST FOLLOW LAST SCREEN)                                       
* TAGENWORKD                                                                    
* TASYSDSECT                                                                    
* TAGENFILE                                                                     
* TAGENEQUS                                                                     
* TASYSEQUS                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* DDBIGBOX                                                                      
* DDPERVALD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TAGENEQUS                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDPERVALD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'113TAGEN88   11/03/11'                                      
         END                                                                    
