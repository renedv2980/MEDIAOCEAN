*          DATA SET BUFIL30    AT LEVEL 009 AS OF 05/01/02                      
*PHASE T50230A                                                                  
*INCLUDE BINSRCH2                                                               
         TITLE 'T50230 - BUDGET CONTROL LFM - SNAPSHOT UTILITY'                 
T50230   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**FISN**,RA,RR=R2                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R5,ANODBLK          R5=A(NODIO BLOCK)                            
         USING NODBLKD,R5                                                       
         ST    R2,RELO             SAVE PROGRAM RELOCATION FACTOR               
         ST    RB,MYBASE                                                        
*                                                                               
SNAP1    L     RF,=V(BINSRCH)      RELOCATE INCLUDED MODULES                    
         AR    RF,R2                                                            
         ST    RF,VBINSRCH                                                      
*                                                                               
         GOTO1 VSETADD                                                          
*                                                                               
SNAP2    CLI   MODE,VALKEY         VALIDATE KEY FIELDS                          
         BE    SNAP4                                                            
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    SNAP6                                                            
         B     SNAPX                                                            
*                                                                               
SNAP4    MVI   TERMSW,NO                                                        
         BAS   RE,VALHED           VALIDATE HEADLINE FIELDS                     
         B     SNAPX                                                            
*                                                                               
SNAP6    BAS   RE,INITREP          INITIALIZE FOR REPORT                        
         CLI   TERMSW,YES          TEST IF REPORT TERMINATED                    
         BE    SNAP8                                                            
*                                                                               
         BAS   RE,CLRTAB           INITIALIZE BINSRCH TABLE                     
         BAS   RE,RDPL             READ THE PLAN/PRINT REPORT                   
         CLI   TEST,YES                                                         
         BE    *+8                                                              
         BAS   RE,UPPLAN           UPDATE PLAN RECORD                           
         BAS   RE,HIST             PRINT PLAN SNAPSHOT HISTORY                  
         B     SNAPX                                                            
*                                                                               
SNAP8    LA    R2,P                TERMINATED REPORT PROCESSING                 
         USING PRTD,R2                                                          
         MVC   PRTNAME(L'SVERRMSG),SVERRMSG PRINT SAVED ERROR MESSAGE           
         GOTO1 SPOOL,PARAS,(R8)                                                 
         GOTO1 (RF),(R1),(R8)                                                   
         MVC   PRTNAME(L'TERMMSG),TERMMSG                                       
         GOTO1 (RF),(R1),(R8)                                                   
*                                                                               
SNAPX    B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
* SUB-ROUTINE TO VALIDATE HEADLINE KEY FIELDS                                   
*                                                                               
VALHED   NTR1                                                                   
         GOTO1 VVALCLT,PARAS,SNACLTH,0                                          
         MVC   SNACLN,CLTNAM                                                    
         OI    SNACLNH+6,X'80'     XMIT                                         
*                                                                               
VALHED2  GOTO1 VVALPRD,PARAS,SNAPRDH,0                                          
         MVC   SNAPRN,PRDNAM                                                    
         OI    SNAPRNH+6,X'80'     XMIT                                         
*                                                                               
VALHED4  GOTO1 VVALPLAN,PARAS,SNAPLAH,0                                         
         MVC   SNAPLN,PLANNAM                                                   
         OI    SNAPLNH+6,X'80'                                                  
         L     R3,NDLEVPTR                                                      
         USING NDLVTABD,R3                                                      
         MVC   SVPLNKEY,NDLVKEY                                                 
         GOTO1 GETCOL,PARAS,NDLVKEY                                             
         BNE   VALHED6             FOUND AT LEAST 1 COLUMN                      
*                                                                               
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'NODTMSG),NODTMSG                                          
         MVI   ERROR,SUPPLIED                                                   
         CLI   OFFLINE,YES         TEST IF OFFLINE                              
         BNE   SPERR               NO                                           
         MVC   SVERRMSG,WORK       SAVE ERROR MESSAGE TEXT                      
         MVI   TERMSW,YES          SET TERMINATED REPORT FLAG                   
         B     VALHEDX             LET GENCON OPEN PQ AND DO SCREEN             
*                                                                               
VALHED6  GOTO1 VGETFLD,PARAS,SNAMODEH                                           
         MVI   ERROR,MISSING                                                    
         CLI   FLDH+5,0                                                         
         BE    SPERR                                                            
         MVC   SNAPMODE,FLD                                                     
         MVI   ERROR,INVALID                                                    
         CLI   SNAPMODE,C'S'                                                    
         BE    VALHED8                                                          
         CLI   SNAPMODE,C'D'                                                    
         BNE   SPERR                                                            
*                                                                               
VALHED8  GOTO1 VGETFLD,PARAS,SNADDH                                             
         CLI   FLDH+5,0            TEST FOR INPUT                               
         BNE   VALHED10                                                         
         MVI   ERROR,MISSING                                                    
         CLI   SNAPMODE,C'D'       TEST FOR SNAPSHOT DELETE                     
         BE    SPERR               YES-INPUT IS COMPULSORY                      
         B     VALHED12                                                         
*                                                                               
VALHED10 MVI   ERROR,INVALID                                                    
         CLI   SNAPMODE,C'S'       TEST FOR SNAPSHOT                            
         BE    SPERR               YES-ANY INPUT IS WRONG                       
         MVI   ERROR,INVDATE                                                    
         GOTO1 DATVAL,DMCB,FLD,DUB                                              
         OC    0(4,R1),0(R1)                                                    
         BZ    SPERR                                                            
         CLC   FLDH+5(1),3(R1)     TEST THAT DATE MAKES UP WHOLE FIELD          
         BNE   SPERR                                                            
         GOTO1 DATCON,DMCB,DUB,(3,DELDATE)                                      
*                                                                               
         L     R4,NDIOA                                                         
         MVI   ELCODE,BUSNELQ      GET PLAN SNAPSHOT ELEMENT                    
         BAS   RE,GETEL                                                         
         BNE   VALHED11            NONE FOUND-ERROR                             
         USING BUSND,R6                                                         
         ZIC   R1,BUSNLEN                                                       
         SH    R1,=Y(BUSNAPS-BUSND)                                             
         SR    R0,R0                                                            
         LA    RE,L'BUSNAPS                                                     
         DR    R0,RE               R1=N'SNAPSHOTS                               
         LA    RE,BUSNAPS          RE=SNAPSHOT DATE POINTER                     
*                                                                               
         CLC   DELDATE,0(RE)       TEST IF DATE IS IN LIST                      
         BE    VALHED12            YES                                          
         LA    RE,L'BUSNAPS(RE)                                                 
         BCT   R1,*-14                                                          
*                                                                               
VALHED11 MVC   WORK,SPACES                                                      
         MVC   WORK(L'SNAPMSG),SNAPMSG                                          
         MVI   ERROR,SUPPLIED                                                   
         CLI   OFFLINE,NO                                                       
         BE    SPERR                                                            
         MVC   SVERRMSG,WORK       SAVE ERROR MESSAGE TEXT                      
         MVI   TERMSW,YES                                                       
         B     VALHEDX                                                          
*                                                                               
VALHED12 MVI   TEST,NO                                                          
         GOTO1 VGETFLD,PARAS,SNATESTH                                           
         CLI   FLDH+5,0                                                         
         BE    VALHED14                                                         
         MVI   ERROR,INVALID                                                    
         MVC   TEST,FLD                                                         
         CLI   TEST,YES                                                         
         BE    VALHED14                                                         
         CLI   TEST,NO                                                          
         BNE   SPERR                                                            
*                                                                               
VALHED14 MVI   DETAILS,YES                                                      
         GOTO1 VGETFLD,PARAS,SNADETH                                            
         CLI   FLDH+5,0                                                         
         BE    VALHEDX                                                          
         MVC   DETAILS,FLD                                                      
         CLI   DETAILS,YES                                                      
         BE    VALHEDX                                                          
         CLI   DETAILS,NO                                                       
         BNE   SPERR                                                            
*                                                                               
VALHEDX  B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO READ DATA TYPE RECORDS AND BUILD A LIST OF COLUMNS             
* ON REPORT                                                                     
*                                                                               
* AT ENTRY, P1 = A(PLAN KEY)                                                    
* ON EXIT, CC=EQ FOR NO COLUMNS, CC=NEQ FOR 1 OR MORE COLUMNS                   
*                                                                               
GETCOL   NTR1                                                                   
         LA    R2,COLLIST          R2=A(COLUMN LIST ENTRY)                      
         SR    R3,R3               R3=N'COLUMNS                                 
         L     RE,0(R1)            RE=A(PLAN KEY)                               
         MVC   AIO,AIO2            READ DATA TYPE RECORDS INTO IO2              
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING BURECD,R4                                                        
         MVC   BUKEY,0(RE)         SET PLAN KEY                                 
         MVI   BUDSUB,BUDSUBQ                                                   
         GOTO1 HIGH                                                             
         B     GETCOL2                                                          
*                                                                               
GETCOL1  LA    R4,KEY                                                           
         GOTO1 SEQ                                                              
*                                                                               
GETCOL2  CLC   BUKEY(BUDTYP-BUKEY),KEYSAVE                                      
         BNE   GETCOL4                                                          
         GOTO1 GETREC                                                           
         MVI   ELCODE,BUPOLELQ                                                  
         L     R4,AIO              LOOK FOR POLISH FORMULA ELEMENT              
         BAS   RE,GETEL                                                         
         BE    GETCOL1             SKIP FORMULA DATA TYPES                      
         BAS   RE,GETDT            EXTRACT VALUES FROM RECORD                   
*                                                                               
         USING COLD,R2                                                          
         MVC   COLCODE,DTCODE                                                   
         MVC   COLHED1,DTHEAD1                                                  
         MVC   COLHED2,DTHEAD2                                                  
         MVC   COLSC,DTSC                                                       
         MVC   COLDEC,DTDEC                                                     
         MVC   COLFORM,DTFORM                                                   
         LA    R2,COLNTRL(R2)      NEXT COLUMN LIST ENTRY                       
         LA    R3,1(R3)            INCREMENT COLUMN COUNT                       
         CH    R3,=Y(MAXCOL)       TEST FOR COLUMN LIMIT                        
         BL    GETCOL1                                                          
*                                                                               
GETCOL4  MVC   AIO,AIO1            RESTORE IO AREA POINTER                      
         STC   R3,NCOLS                                                         
         LTR   R3,R3               SET CC ON EXIT                               
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
* SUB-ROUTINE TO GET VALUES FROM DATA TYPE RECORD                               
*                                                                               
GETDT    NTR1                                                                   
         L     R4,AIO                                                           
         XC    DTVALS(DTVALLN),DTVALS CLEAR DATA TYPE VALUES                    
         MVC   DTCODE,BUDTYP       DATA TYPE CODE                               
         MVC   DTHEAD1,SPACES                                                   
         MVC   DTHEAD2,SPACES                                                   
         MVI   ELCODE,BUDHELQ      FIRST HEADING                                
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING BUDHD,R6                                                         
         ZIC   R1,BUDHLEN                                                       
         SH    R1,=Y(BUDHEAD-BUDHD+1)                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DTHEAD1(0),BUDHEAD                                               
*                                                                               
         SR    R0,R0                                                            
*                                                                               
GETDT2   IC    R0,1(R6)            BUMP TO NEXT ELEMENT                         
         AR    R6,R0                                                            
         CLI   0(R6),0             TEST FOR EOR                                 
         BE    GETDT3              YES                                          
         CLI   0(R6),BUDHELQ       TEST FOR HEADING ELEMENT                     
         BNE   GETDT2                                                           
*                                                                               
         ZIC   R1,BUDHLEN                                                       
         SH    R1,=Y(BUDHEAD-BUDHD+1)                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DTHEAD2(0),BUDHEAD                                               
*                                                                               
GETDT3   MVI   ELCODE,BUDTELQ      GET DATA TYPE ELEMENT                        
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING BUDTD,R6                                                         
*                                                                               
GETDT4   MVC   DTEX,BUDTEX                                                      
         MVC   DTCOL,BUDTCOL                                                    
         MVC   DTSC,BUDTSC                                                      
         MVC   DTDEC,BUDTDEC                                                    
         MVC   DTFORM,BUDTFORM     EXTRACT FORMAT OPTIONS                       
*                                                                               
GETDTX   B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO INITIALIZE REPORT                                              
*                                                                               
* BINSRCH TABLE KEPT AT IO3 FOR FIXED PLAN - STORAGE ACQUIRED FOR               
* AN OPEN-ENDED PLAN (MUST BE OFF-LINE)                                         
*                                                                               
INITREP  ST    RE,SAVERE                                                        
         LA    R1,HEDSPECS         SET UP FOR PRINTING                          
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         CLI   PLANST,0            TEST FOR OPEN-ENDED PLAN                     
         BE    INITREP4            YES                                          
*                                                                               
INITREP2 MVC   ADATATAB,AIO3       IO3 IS BINSRCH TABLE FOR REG PLAN            
         LA    RE,1*13             1 YEAR LIMIT * 13 PERIODS                    
         MH    RE,=Y(MAXCOL)       * MAXIMUM N'COLUMNS                          
         ST    RE,BINMAX           MAXIMUM NUMBER OF RECORDS IN TABLE           
         MH    RE,=Y(BINRECL)      RE=TABLE LENGTH                              
         ST    RE,LENTAB                                                        
         B     INITREP6                                                         
*                                                                               
INITREP4 LA    RE,100*13           100 YEAR LIMIT * 13 PERIODS                  
         MH    RE,=Y(MAXCOL)                                                    
         ST    RE,BINMAX                                                        
         MH    RE,=Y(BINRECL)                                                   
         ST    RE,LENTAB                                                        
         LA    R0,0(RE)            HOB IS LV PARM SUB-POOL                      
         GETMAIN R,LV=(0)                                                       
         ST    R1,ADATATAB         SAVE A(DATA TABLE AREA)                      
*                                                                               
INITREP6 MVC   BINATAB,ADATATAB                                                 
         LA    R1,BINRECL                                                       
         ST    R1,BINLREC          SET BINSRCH RECORD LEN PARM                  
         LA    R1,BINKEYL                                                       
         ST    R1,BINLENK                                                       
         LA    R1,BINKEY-BINRECD                                                
         STC   R1,BINDISPK         SET DISPLACEMENT TO KEY                      
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO READ THE PLAN AND PRINT REPORT                                 
*                                                                               
* AT ENTRY, ASSUMES THAT PLAN HAS JUST BEEN READ THROUGH NODIO                  
*                                                                               
RDPL     NTR1                                                                   
         MVI   NDSUBRSW,YES        SET TO RETURN SUB-RECORDS                    
         XC    NDSUBRLO,NDSUBRLO   SET TO FILTER ON DATA VALUE RECS             
         MVI   NDSUBRLO,BUVSUBQ                                                 
         MVC   NDSUBRHI,EFFS                                                    
         MVI   NDSUBRHI,BUVSUBQ                                                 
         LA    R1,RDPL4                                                         
         ST    R1,NDHOOK                                                        
         GOTO1 VNODIO,DMCB,NODBLKD,=C'LSEQ',NODKEY,0                            
*                                                                               
RDPL2    CLI   PENDSW,YES          TEST FOR PENDING PRINT                       
         BNE   *+8                                                              
         BAS   RE,PRTTAB                                                        
         MVI   NDSUBRSW,0          TURN OFF SUB-RECORD READ                     
         XC    NDSUBRLO,NDSUBRLO                                                
         XC    NDSUBRHI,NDSUBRHI                                                
*                                                                               
RDPLX    B     XIT                                                              
         SPACE 2                                                                
* NODIO HOOK ROUTINE FOR READING OUTLINES AND SUB-RECORDS                       
*                                                                               
RDPL4    NTR1                                                                   
         CLI   NDMODE,NDPROC                                                    
         BNE   RDPL6                                                            
*                                                                               
         CLI   PENDSW,YES          TEST FOR PENDING PRINT                       
         BNE   *+8                                                              
         BAS   RE,PRTTAB           PRINT BINSRCH TABLE FOR LAST OUTLINE         
         GOTO1 VGETVAL                                                          
*                                                                               
         MVI   PARENTSW,NO         FIND OUT IF OUTLINE IS PARENT                
         CLI   OUTLEV,MAXOUTS      TEST IF AT LOWEST LEVEL                      
         BE    RDPL5               YES-CANNOT BE A PARENT                       
         L     R3,NDLEVPTR                                                      
         USING NDLVTABD,R3                                                      
         LA    R3,NDLVTABL(R3)     POINT TO NEXT LOWER OUTLINE                  
         OC    NDLVNOD,NDLVNOD                                                  
         BZ    RDPL5                                                            
         MVI   PARENTSW,YES                                                     
*                                                                               
RDPL5    LA    R2,P                                                             
         USING PRTD,R2                                                          
         ZIC   R1,OUTLEV                                                        
         BCTR  R1,0                DEVELOP INDEX FOR OUTLINE LEVEL              
         SLL   R1,1                X 2 FOR INDENTATION                          
         LA    RE,PRTCODE(R1)                                                   
         MVC   0(L'OUTCODE,RE),OUTCODE                                          
         OC    PRTCODE,SPACES                                                   
         LA    RE,PRTNAME(R1)                                                   
         MVC   0(L'OUTNAME,RE),OUTNAME                                          
         GOTO1 SPOOL,PARAS,(R8)                                                 
         B     RDPL8                                                            
*                                                                               
RDPL6    CLI   NDMODE,NDPROCSR     TEST FOR PROCESS SUB-RECORD                  
         BNE   RDPL8                                                            
         BAS   RE,PERSNAP          PERFORM SNAPSHOT                             
         B     RDPL8                                                            
*                                                                               
RDPL8    B     RDPLX                                                            
         EJECT                                                                  
* HOOK ROUTINE FOR HEADLINE PRINTING                                            
*                                                                               
HOOK     NTR1                                                                   
         MVC   H4+10(3),CLTCODE                                                 
         OC    H4+10(3),SPACES                                                  
         MVC   H4+15(L'CLTNAM),CLTNAM                                           
*                                                                               
         MVC   H5+10(3),PRDCODE                                                 
         OC    H5+10(3),SPACES                                                  
         MVC   H5+15(L'PRDNAM),PRDNAM                                           
*                                                                               
         MVC   H6+10(3),PLANCODE                                                
         OC    H6+10(3),SPACES                                                  
         MVC   H6+15(L'PLANNAM),PLANNAM                                         
*                                                                               
HOOK1    MVC   H6+74(8),=C'SNAPSHOT'                                            
         CLI   SNAPMODE,C'S'       TEST FOR SNAPSHOT                            
         BNE   HOOK2               NO                                           
         LA    R4,H6+88                                                         
         MVC   H6+83(4),=C'TEST'                                                
         CLI   TEST,YES                                                         
         BE    *+14                                                             
         LA    R4,H6+89                                                         
         MVC   H6+83(5),=C'TAKEN'                                               
*                                                                               
         MVC   0(3,R4),=C'FOR'                                                  
         LA    R4,4(R4)            POINT TO POSITION FOR DATE                   
         GOTO1 DATCON,DMCB,(3,BTODAY),(8,(R4))                                  
         B     HOOK3                                                            
*                                                                               
HOOK2    MVC   H6+83(2),=C'OF'     SNAPSHOT DELETION                            
         GOTO1 DATCON,DMCB,(3,DELDATE),(8,H6+86)                                
         MVC   H6+95(8),=C'DELETION'                                            
         CLI   TEST,YES                                                         
         BNE   *+10                                                             
         MVC   H6+104(4),=C'TEST'                                               
*                                                                               
HOOK3    CLI   RCSUBPRG,1          TEST FOR SNAPSHOT HISTORY                    
         BE    HOOK6               YES                                          
         ZIC   R0,NCOLS            R0=LOOP COUNTER                              
         LA    R4,COLLIST          R4=A(COLUMN LIST ENTRY)                      
         USING COLD,R4                                                          
         LA    R2,H9+(PFSTCOL-PRTD)  R2=A(COLUMN POSITION)                      
*                                                                               
HOOK4    MVC   1(L'COLHED1,R2),COLHED1 FIRST HEADING                            
         MVC   L'P+1(L'COLHED2,R2),COLHED2 SECOND HEADING                       
         LA    R4,COLNTRL(R4)      NEXT COLUMN ENTRY                            
         LA    R2,COLLEN+1(R2)     NEXT COLUMN                                  
         BCT   R0,HOOK4                                                         
*                                                                               
HOOK6    ICM   R3,15,ABOX                                                       
         BZ    HOOKX                                                            
         USING BOXD,R3                                                          
*                                                                               
         CLI   RCSUBPRG,1          TEST FOR SNAPSHOT HISTORY                    
         BNE   *+16                                                             
         XC    BOXROWS,BOXROWS     YES-CLEAR PRIOR ROW/COLUMN SETTINGS          
         XC    BOXCOLS,BOXCOLS                                                  
*                                                                               
         MVI   BOXROWS+7,C'T'      SET UP FOR BOXES                             
         MVI   BOXROWS+58,C'B'                                                  
         CLI   RCSUBPRG,1          TEST FOR SNAPSHOT HISTORY                    
         BNE   *+12                                                             
         MVI   BOXROWS+9,C'M'                                                   
         B     HOOK8                                                            
*                                                                               
         MVI   BOXROWS+10,C'M'                                                  
         LA    R2,BOXCOLS                                                       
         MVI   PRTLBOX-PRTD(R2),C'L'                                            
         MVI   PRTBOX1-PRTD(R2),C'C'                                            
*                                                                               
         LA    R2,PFSTCOL-PRTD(R2) R2=A(COLUMN POSITION)                        
         ZIC   R0,NCOLS            R0=LOOP COUNTER                              
*                                                                               
HOOK7    MVI   0(R2),C'C'                                                       
         LA    R2,COLLEN+1(R2)                                                  
         BCT   R0,HOOK7                                                         
         MVI   0(R2),C'R'                                                       
         B     HOOK10                                                           
*                                                                               
HOOK8    LA    R2,BOXCOLS          SNAPSHOT HISTORY                             
         MVI   43(R2),C'L'                                                      
         MVI   51(R2),C'C'                                                      
         MVI   65(R2),C'R'                                                      
         B     HOOK10                                                           
*                                                                               
HOOK10   MVI   BOXWT,1                                                          
         MVI   BOXYORN,YES                                                      
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
*                                                                               
HOOKX    B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
* SUB-ROUTINE TO PERFORM SNAPSHOT                                               
*                                                                               
* AT ENTRY,  NDIOA (AIO1) =A(DATA VALUE RECORD)                                 
* ON EXIT,   AIO2 = A(BUPPER RECORD)                                            
*                                                                               
PERSNAP  NTR1                                                                   
         L     R4,AIO2             R4=A(BUPPER RECORD)                          
         LR    RE,R4                                                            
         L     RF,SIZEIO                                                        
         SR    R1,R1                                                            
         MVCL  RE,R0               CLEAR IO2                                    
         BAS   RE,FINDATA                                                       
         BNE   PERSNAPX            NOTHING TO DO                                
*                                                                               
         USING BURECD,R4                                                        
PERSNAP2 L     RE,NDIOA                                                         
         MVC   BUKEY,0(RE)         EXTRACT KEY                                  
         MVC   BUFRSTEL(BUDALNQ),0(R6) ATTACH SNAPSHOT ELEMENT                  
         LA    R1,(BUFRSTEL-BUKEY)+BUDALNQ+1                                    
         STH   R1,BURLEN                                                        
         LA    R6,BUFRSTEL         ADDRESS NEW DATA ELEMENT                     
         USING BUDAD,R6                                                         
         MVC   DUB,BUDAPREC        SAVE VALUE                                   
         OC    BUDADATE,BUDADATE   TEST FOR DELETING SNAPSHOT                   
         BNZ   PERSNAP3            YES                                          
*                                                                               
         MVC   BUDADATE,BTODAY                                                  
         XC    BUDADATE,EFFS       COMPLEMENT DATE                              
         B     PERSNAP4                                                         
*                                                                               
PERSNAP3 MVI   BUDAPREC,X'FF'      SET TO DELETE ELEMENT                        
*                                                                               
PERSNAP4 CLI   DETAILS,NO          TEST IF PRINTING DETAILS                     
         BE    PERSNAP5                                                         
         CLI   PARENTSW,YES        TEST IF OUTLINE IS A PARENT NOW              
         BE    PERSNAP5            YES-SKIP DETAIL LINE POSTING                 
         BAS   RE,POST                                                          
*                                                                               
PERSNAP5 CLI   TEST,YES            TEST MODE                                    
         BE    PERSNAPX            YES-SKIP UPDATE                              
*                                                                               
PERSNAP6 XC    BUPBLOCK,BUPBLOCK                                                
         LA    RE,BUPBLOCK                                                      
         USING BUPBLKD,RE                                                       
         MVC   BUPADMGR,DATAMGR                                                 
         MVC   BUPAHELO,HELLO                                                   
         LR    R1,R4               BUPPER ASSUMES RECORD HEADER                 
         SH    R1,=H'4'                                                         
         ST    R1,BUPAREC                                                       
         MVI   BUPORIG,BUACTSNP                                                 
         CLI   SNAPMODE,C'S'       TEST FOR SNAPSHOT                            
         BE    *+8                                                              
         MVI   BUPORIG,BUACTSND    NO-SNAPSHOT DELETE                           
         MVC   BUPBDATE,BTODAY                                                  
         MVI   BYTE,BUPPUT                                                      
         GOTO1 VBUPPER,DMCB,(BYTE,BUPBLOCK)                                     
         BE    *+6                                                              
         DC    H'0'                DATAMGR ERROR                                
*                                                                               
PERSNAPX B     XIT                                                              
         DROP  R6,RE                                                            
         EJECT                                                                  
* SUB-ROUTINE TO POST A VALUE TO BINSRCH TABLE FOR AN OUTLINE                   
*        CALLED FROM PERSNAP                                                    
* AT ENTRY,  AIO2 POINTS TO DATA VALUE RECORD                                   
*            DUB CONTAINS VALUE TO POST                                         
*                                                                               
POST     NTR1                                                                   
         L     R4,AIO2             R4=A(DATA VALUE RECORD)                      
         USING BURECD,R4                                                        
         LA    R2,COLLIST          R2=A(COLUMN ENTRY LIST)                      
         USING COLD,R2                                                          
         SR    R1,R1               R1=COLUMN INDEX                              
         ZIC   R0,NCOLS            R0=LOOP COUNTER                              
*                                                                               
POST2    CLC   BUVDTYP,COLCODE     TEST FOR MATCH ON CODE                       
         BE    POST4                                                            
         LA    R1,1(R1)                                                         
         LA    R2,COLNTRL(R2)      NEXT LIST ENTRY                              
         BCT   R0,POST2                                                         
         B     POSTX               NOT A PRINTABLE DATA TYPE                    
*                                                                               
POST4    XC    BINREC,BINREC                                                    
         LA    R3,BINREC                                                        
         USING BINRECD,R3          R3=A(BINSRCH RECORD)                         
         MVC   BINMONTH,BUVPER     MONTH                                        
         STC   R1,BINCOL           COLUMN INDEX                                 
         MVC   BINVAL,DUB          VALUE                                        
         ST    R3,BINAREC                                                       
         MVI   BINOP,BININS        INSERT RECORD INTO TABLE                     
         GOTO1 VBINSRCH,BINPARM                                                 
         OC    BINAREC,BINAREC                                                  
         BNZ   *+6                                                              
         DC    H'0'                BLOW UP IF TABLE FULL                        
         MVI   PENDSW,YES          SET PRINT PENDING SWITCH                     
*                                                                               
POSTX    B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
* SUB-ROUTINE TO FIND DATA ELEMENT TO BE SNAPSHOT OR DELETED                    
* CALLED FROM PERSNAP.                                                          
* ON EXIT, CC=EQ IF FOUND AND R6 POINTS TO ELEMENT                              
*                                                                               
FINDATA  ST    RE,SAVERE                                                        
         L     R6,NDIOA                                                         
         LA    R6,BUFRSTEL-BUKEY(R6)                                            
         SR    R1,R1                                                            
         USING BUDAD,R6                                                         
*                                                                               
FINDATA2 CLI   0(R6),0             TEST FOR EOR                                 
         BE    FINDATAN            ELEMENT NOT FOUND                            
         CLI   0(R6),BUDAELQ       TEST FOR DATA ELEMENT                        
         BNE   *+14                                                             
         OC    BUDADATE,BUDADATE                                                
         BZ    FINDATA4                                                         
         IC    R1,BUDALEN                                                       
         AR    R6,R1                                                            
         B     FINDATA2                                                         
*                                                                               
FINDATA4 OC    DELDATE,DELDATE     TEST FOR DELETE OPTION                       
         BZ    FINDATAY            NO                                           
         MVC   WORK(3),DELDATE                                                  
         XC    WORK(3),EFFS        COMPLEMENT DELETE DATE                       
*                                                                               
FINDATA6 IC    R1,BUDALEN                                                       
         AR    R6,R1                                                            
         CLI   0(R6),0             TEST FOR EOR                                 
         BE    FINDATAN            NO ELEMENT THERE                             
         CLC   BUDADATE,WORK                                                    
         BE    FINDATAY                                                         
         B     FINDATA6                                                         
*                                                                               
FINDATAY CR    RB,RB                                                            
         B     FINDATAX                                                         
*                                                                               
FINDATAN LTR   RB,RB                                                            
*                                                                               
FINDATAX L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO PRINT DETAIL LINES FOR AN OUTLINE FROM THE                     
* BINSRCH TABLE.  CALLED FROM RDPL ON BREAK IN OUTLINE.                         
*                                                                               
PRTTAB   NTR1                                                                   
         CLI   DETAILS,NO                                                       
         BE    PRTTABX                                                          
         LA    R2,P                R2=A(PRINT LINE)                             
         USING PRTD,R2                                                          
         ICM   R3,15,BINRECS       R3=N'BINSRCH TABLE ENTRIES                   
         BZ    PRTTABX             NOTHING TO PRINT                             
         L     R4,BINATAB          R4=A(BINSRCH TABLE ENTRY)                    
         USING BINRECD,R4                                                       
         XC    LASTMON,LASTMON     CLEAR LAST MONTH CONTROL                     
*                                                                               
PRTTAB2  CLC   BINMONTH,LASTMON    TEST SAME MONTH                              
         BE    PRTTAB4             YES                                          
         OC    LASTMON,LASTMON     TEST FIRST RECORD                            
         BZ    PRTTAB3             YES                                          
         GOTO1 SPOOL,PARAS,(R8)                                                 
*                                                                               
PRTTAB3  MVC   LASTMON,BINMONTH                                                 
         GOTO1 VPEROUT,PARAS,BINMONTH,WORK                                      
         MVC   PRTPER,WORK                                                      
*                                                                               
PRTTAB4  ZIC   R7,BINCOL           GET COLUMN INDEX                             
         MH    R7,=Y(COLNTRL)      DISPLACEMENT INTO COLUMN LIST                
         LA    R7,COLLIST(R7)                                                   
         USING COLD,R7             R7=A(COLUMN LIST ENTRY)                      
         LA    RF,PFSTCOL-PRTD(R2) RF=A(FIRST COLUMN ON REPORT)                 
         LA    R0,COLLEN+1         R0=LENGTH OF PRINTED COLUMN                  
         ZIC   R1,BINCOL           COLUMN NUMBER                                
         MR    R0,R0               DISPLACEMENT TO COLUMN                       
         LA    RF,1(R1,RF)         RF=A(PRINT POSITION)                         
*                                                                               
PRTTAB6  XC    EBLOCK,EBLOCK                                                    
         MVI   EBLIN,L'BINVAL                                                   
         MVI   EBTIN,C'S'          SCALED NUMBER                                
         MVI   EBOPT,EMINUS+EZERO                                               
         MVC   EBDECS,COLDEC                                                    
         MVC   EBSCOUT,COLSC                                                    
         LA    RE,BINVAL                                                        
         ST    RE,EBAIN                                                         
         MVC   EBSCIN,BINPREC      EXTRACT SCALE BYTE FROM VALUE                
         ST    RF,EBAOUT                                                        
         MVI   EBLOUT,COLLEN                                                    
         GOTO1 EDITOR,PARAS,EBLOCK                                              
*                                                                               
PRTTAB8  LA    R4,BINRECL(R4)      NEXT RECORD IN TABLE                         
         BCT   R3,PRTTAB2                                                       
*                                                                               
PRTTAB10 GOTO1 SPOOL,PARAS,(R8)    PRINT LAST LINE                              
         MVI   PENDSW,NO           TURN OFF PENDING PRINT SWITCH                
         BAS   RE,CLRTAB           CLEAR BINSRCH TABLE                          
         GOTO1 SPOOL,PARAS,(R8)    SKIP A LINE                                  
*                                                                               
PRTTABX  B     XIT                                                              
         DROP  R2,R4,R7                                                         
         SPACE 2                                                                
* SUB-ROUTINE TO CLEAR BINSRCH TABLE                                            
*                                                                               
CLRTAB   ST    RE,SAVERE                                                        
         L     RE,BINATAB          RE=A(TABLE)                                  
         L     RF,LENTAB                                                        
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         XC    BINRECS,BINRECS     CLEAR COUNT OF RECORDS SO FAR                
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO UPDATE PLAN RECORD                                             
*                                                                               
UPPLAN   NTR1                                                                   
         GOTO1 VSETKEY             BUILD PLAN KEY                               
         XC    NDHOOK,NDHOOK                                                    
         GOTO1 VNODIO,DMCB,NODBLKD,=C'READ',NODKEY,0                            
         CLI   NDERR,0                                                          
         BNE   UPPLANX                                                          
         L     R4,NDIOA                                                         
         MVI   ELCODE,BUSNELQ      SEARCH FOR SNAPSHOT ELEMENT                  
         MVI   PREVSNAP,YES        SET PREVIOUS SNAPSHOTS ON PLAN               
         BAS   RE,GETEL                                                         
         BE    *+8                                                              
         MVI   PREVSNAP,NO         NO PREVIOUS SNAPSHOTS IF ELEM NF             
*                                                                               
UPPLAN1  XC    ELEM,ELEM                                                        
         LA    R4,ELEM             R4=A(NEW SNAPSHOT ELEMENT)                   
         USING BUSND,R4                                                         
         MVI   BUSNEL,BUSNELQ                                                   
         CLI   SNAPMODE,C'D'       TEST FOR SNAPSHOT DELETE                     
         BE    UPPLAN4                                                          
*                                                                               
UPPLAN2  MVC   BUSNAPS,BTODAY      SET TODAY IN PLACE                           
         LA    R2,BUSNAPS-BUSND    INITIALIZE NEW ELEMENT LENGTH                
         CLI   PREVSNAP,NO         TEST FOR FIRST SNAPSHOT                      
         BE    UPPLAN3             YES                                          
*                                                                               
         CLC   BUSNAPS,BUSNAPS-BUSND(R6) TEST IF SNAPSHOT ALREADY               
         BE    UPPLANX                   YES-SKIP UPDATE                        
         ZIC   R1,1(R6)            GET ELEMENT LENGTH                           
         LR    R2,R1               SAVE OLD ELEMENT LENGTH                      
         SH    R1,=Y(BUSNAPS-BUSND+1)                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BUSNAPS+3(0),BUSNAPS-BUSND(R6)  ATTACH REST OF OLD ELEM          
*                                                                               
UPPLAN3  LA    R2,L'BUSNAPS(R2)    UPDATE ELEMENT LENGTH                        
         STC   R2,BUSNLEN                                                       
         B     UPPLAN7                                                          
*                                                                               
UPPLAN4  CLI   PREVSNAP,NO         TEST FOR NO SNAPSHOTS                        
         BE    UPPLANX             YES-SO NOTHING TO DELETE                     
*                                                                               
         ZIC   R1,1(R6)            GET OLD ELEMENT LENGTH                       
         SH    R1,=Y(BUSNAPS-BUSND)                                             
         SR    R0,R0                                                            
         LA    RE,L'BUSNAPS                                                     
         DR    R0,RE               R1=SNAPSHOT COUNTER                          
         LA    RF,BUSNAPS-BUSND    RF=NEW ELEMENT LENGTH                        
         LA    R2,BUSNAPS          R2=NEW SNAPSHOT LIST POINTER                 
         LA    R3,BUSNAPS-BUSND(R6) R3=OLD SNAPSHOT LIST POINTER                
*                                                                               
UPPLAN5  CLC   0(L'BUSNAPS,R3),DELDATE TEST FOR DELETED SNAPSHOT                
         BE    UPPLAN6                                                          
         MVC   0(L'BUSNAPS,R2),0(R3) COPY DATE FROM OLD TO NEW                  
         LA    R2,L'BUSNAPS(R2)                                                 
         LA    RF,L'BUSNAPS(RF)    UPDATE NEW ELEMENT LENGTH                    
*                                                                               
UPPLAN6  LA    R3,L'BUSNAPS(R3)                                                 
         BCT   R1,UPPLAN5                                                       
         STC   RF,BUSNLEN                                                       
*                                                                               
UPPLAN7  GOTO1 HELLO,DMCB,(C'D',SYSFIL),(ELCODE,NDIOA),0                        
         CLI   BUSNLEN,BUSNAPS-BUSND TEST FOR NO SNAPSHOTS LEFT                 
         BE    UPPLAN8             YES-SKIP ELEMENT PUT                         
         GOTO1 (RF),DMCB,(C'P',SYSFIL),NDIOA,ELEM                               
*                                                                               
UPPLAN8  GOTO1 VNODIO,DMCB,NODBLKD,=C'PUT',NODKEY,0                             
*                                                                               
UPPLANX  B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO PRINT PLAN SNAPSHOT HISTORY                                    
*                                                                               
HIST     NTR1                                                                   
         GOTO1 VSETKEY                                                          
         XC    NDHOOK,NDHOOK                                                    
         GOTO1 VNODIO,DMCB,NODBLKD,=C'READ',NODKEY,0                            
         CLI   NDERR,0                                                          
         BNE   HISTX                                                            
         L     R4,NDIOA                                                         
         MVI   ELCODE,BUSNELQ      FIND SNAPSHOT ELEMENT                        
         BAS   RE,GETEL                                                         
         BNE   HISTX                                                            
         USING BUSND,R6                                                         
*                                                                               
HIST2    MVI   FORCEHED,YES        FORCE A PAGE BREAK                           
         MVI   RCSUBPRG,1                                                       
         LA    R2,L'BUSNAPS                                                     
         LNR   R2,R2               R2=INCREMENT REGISTER FOR BXH                
         LA    R3,BUSNAPS-1        R3=LIMIT REGISTER FOR BXH                    
         LR    R4,R6               R4=INDEX REGISTER FOR BXH                    
         ZIC   R0,BUSNLEN                                                       
         AR    R4,R0               POINT TO END OF ELEMENT                      
         AR    R4,R2               BACK UP TO LAST SNAPSHOT DATE                
         XC    SNAPNUM,SNAPNUM                                                  
         LA    R6,P                R6=A(PRINT LINE)                             
         USING PRTD,R6                                                          
*                                                                               
HIST4    L     R0,SNAPNUM                                                       
         AH    R0,=H'1'            NEXT SNAPSHOT NUMBER                         
         ST    R0,SNAPNUM                                                       
         EDIT  (R0),(3,PRTNUM),ALIGN=LEFT                                       
         LA    RE,PRTNUM                                                        
         AR    RE,R0               POINT RIGHT AFTER NUMBER                     
         MVI   0(RE),C'.'          PUT A PERIOD AFTER NUMBER                    
         GOTO1 DATCON,DMCB,(3,(R4)),(8,PRTDATE)                                 
         GOTO1 SPOOL,PARAS,(R8)                                                 
         BXH   R4,R2,HIST4                                                      
*                                                                               
HISTX    B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
* COMMON ROUTINES                                                               
*                                                                               
XIT      XIT1                                                                   
         SPACE 1                                                                
TRAPERR  GOTO1 ERREX                                                            
         SPACE 1                                                                
SPERR    GOTO1 VCURSERR,PARAS,0                                                 
         SPACE 1                                                                
GETEL    LR    R0,RE                                                            
         GOTO1 HELLO,PARAS,(C'G',SYSFIL),(ELCODE,(R4)),0,0                      
         L     R6,12(R1)                                                        
         LR    RE,R0                                                            
         CLI   12(R1),0                                                         
         BR    RE                                                               
         SPACE 1                                                                
RELO     DS    A                                                                
         SPACE 2                                                                
* CONSTANTS                                                                     
*                                                                               
EFFS     DC    12X'FF'                                                          
NODTMSG  DC    C'** PLAN HAS NO EXTRACTABLE OR INPUT DATA TYPES **'             
SNAPMSG  DC    C'** NO SNAPSHOT TAKEN ON DATE INPUT **'                         
TERMMSG  DC    C'** REPORT ENDED DUE TO ERROR **'                               
         SPACE 2                                                                
* PATCH AREA                                                                    
*                                                                               
PATCH    DS    0H                                                               
         DC    XL32'00'                                                         
         SPACE 2                                                                
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* REPORT HEADLINE SPECS                                                         
*                                                                               
HEDSPECS DS    0H                                                               
*                                                                               
         SPROG 0,1                                                              
*&&US*&& SSPEC H1,2,C'ABC - ACCOUNT BUDGET SYSTEM'                              
*&&UK*&& SSPEC H1,2,C'ABS - ACCOUNT BUDGET SYSTEM'                              
         SSPEC H2,2,C'---------------------------'                              
         SSPEC H1,75,AGYNAME                                                    
         SSPEC H2,75,AGYADD                                                     
         SSPEC H4,2,C'CLIENT'                                                   
         SSPEC H5,2,C'PRODUCT'                                                  
         SSPEC H6,2,C'PLAN'                                                     
         SSPEC H4,75,REPORT                                                     
         SSPEC H4,87,RUN                                                        
         SSPEC H5,75,REQUESTOR                                                  
         SSPEC H5,93,PAGE                                                       
*                                                                               
         SPROG 0                                                                
         SSPEC H1,48,C'SNAPSHOT REPORT'                                         
         SSPEC H2,48,C'---------------'                                         
         SSPEC H10,2,C'CODES'                                                   
         SSPEC H10,22,C'DESCRIPTION'                                            
*                                                                               
         SPROG 1                                                                
         SSPEC H1,47,C'SNAPSHOT HISTORY'                                        
         SSPEC H2,47,C'----------------'                                        
         SSPEC H9,45,C'NUMBER'                                                  
         SSPEC H9,53,C'SNAPSHOT DATE'                                           
*                                                                               
         DC    X'00'                                                            
         EJECT                                                                  
*                                                                               
       ++INCLUDE BUFILWORKD                                                     
         EJECT                                                                  
* DSECT TO COVER REPORT REQUEST SCREEN                                          
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE BUFILB1D                                                       
         EJECT                                                                  
* WORKING STORAGE VALUES                                                        
*                                                                               
SYSD     DSECT                                                                  
         ORG   OVWORK              LOCAL WORKING STORAGE                        
MYBASE   DS    A                                                                
VBINSRCH DS    V                   V(BINSRCH2)                                  
*                                                                               
ADATATAB DS    A                   A(DATA TABLE)                                
LENTAB   DS    A                   L'DATA TABLE                                 
*                                                                               
BINPARM  DS    6F                                                               
         ORG   BINPARM                                                          
BINOP    DS    0X                  BINSRCH OPERATION                            
BINAREC  DS    A                   A(RECORD TO BE ADDED TO TABLE)               
BINATAB  DS    A                   A(TABLE)                                     
BINRECS  DS    F                   N'RECORDS SO FAR                             
BINLREC  DS    F                   L'RECORDS IN TABLE                           
BINDISPK DS    0X                  DISPLACEMENT TO KEY FIELD                    
BINLENK  DS    F                   L'KEY                                        
BINMAX   DS    F                   MAXIMUM RECORDS IN TABLE                     
*                                                                               
BINREC   DS    XL(BINRECL)         BINSRCH DATA RECORD                          
*                                                                               
SNAPMODE DS    C                   S=SNAPSHOT,D=DELETE                          
TERMSW   DS    C                   Y=TERMINATE REPORT                           
DELDATE  DS    XL3                 SNAPSHOT DELETE DATE                         
TEST     DS    C                   TEST OPTION (Y/N)                            
DETAILS  DS    C                   PRINT DETAILS OPTION (Y/N)                   
*                                                                               
PENDSW   DS    C                   PRINT PENDING (Y)                            
LASTMON  DS    XL(L'BINMONTH)                                                   
PARENTSW DS    C                   OUTLINE IS A PARENT (Y/N)                    
PREVSNAP DS    C                   PREVIOUS SNAPSHOTS ON PLAN (Y/N)             
SNAPNUM  DS    F                                                                
*                                                                               
DTVALS   DS    0C                  DATA TYPE RECORD VALUES                      
DTCODE   DS    CL(L'BUDTYP)                                                     
DTHEAD1  DS    CL20                HEADING 1+2 (SPACE PADDED)                   
DTHEAD2  DS    CL20                                                             
DTEX     DS    X                   EXTRACT TYPE                                 
DTCOL    DS    X                   COLUMN WIDTH                                 
DTSC     DS    X                   SCALE                                        
DTDEC    DS    X                   N'DECIMAL PLACES                             
DTFORM   DS    XL2                 FORMAT OPTIONS                               
DTVALLN  EQU   *-DTVALS            L'DATA TYPE RECORD VALUES                    
*                                                                               
NCOLS    DS    X                   N'COLUMN ENTRIES IN COLLIST                  
COLLIST  DS    (MAXCOL)CL(COLNTRL)                                              
*                                                                               
       ++INCLUDE DDEBLOCK                                                       
*                                                                               
         DS    0D                                                               
BUPBLOCK DS    CL(BUPBLKL)         BUPPER BLOCK AREA                            
         DS    CL(L'OVWORK-(*-OVWORK)) SPARE                                    
*                                                                               
         ORG   TWA1USER            SAVE AREA                                    
SVPLNKEY DS    CL(L'BUKEY)         PLAN KEY                                     
SVERRMSG DS    CL(L'WORK)          SAVED ERROR MESSAGE OFFLINE                  
*                                                                               
         SPACE 2                                                                
* DSECT TO COVER COLUMN LIST                                                    
*                                                                               
COLD     DSECT                                                                  
COLCODE  DS    CL8                 DATA TYPE CODE                               
COLHED1  DS    CL10                                                             
COLHED2  DS    CL10                                                             
COLSC    DS    X                                                                
COLDEC   DS    X                                                                
COLFORM  DS    X                                                                
COLNTRL  EQU   *-COLD              LENGTH OF COLUMN LIST ENTRY                  
         SPACE 2                                                                
* DSECT TO COVER BINSRCH TABLE ENTRY                                            
*                                                                               
BINRECD  DSECT                                                                  
BINKEY   DS    0X                                                               
BINMONTH DS    XL2                 YEAR/MONTH                                   
BINCOL   DS    X                   COLUMN NUMBER                                
BINKEYL  EQU   *-BINKEY            BINSRCH KEY LENGTH                           
BINVAL   DS    CL7                 SCALED NUMBER                                
         ORG   BINVAL                                                           
BINPREC  DS    X                   EXPONENT                                     
BINDATA  DS    PL6                 INTEGER                                      
BINRECL  EQU   *-BINRECD                                                        
         SPACE 2                                                                
* DSECT TO COVER PRINT LINE                                                     
*                                                                               
PRTD     DSECT                                                                  
PRTLBOX  DS    C                                                                
PRTCODE  DS    CL18                                                             
         DS    CL1                                                              
PRTBOX1  DS    CL1                                                              
PRTNAME  DS    CL30                                                             
         DS    CL2                                                              
PFSTCOL  DS    0C                  POSITION FOR FIRST DATA COLUMN               
         ORG   PRTNAME+6                                                        
PRTPER   DS    CL6                 PERIOD EXPRESSION                            
         ORG   PRTD                                                             
         DS    CL44                SPARE                                        
PRTNUM   DS    CL4                 SNAPSHOT NUMBER                              
         DS    CL4                                                              
PRTDATE  DS    CL8                                                              
         SPACE 2                                                                
* EQUATES                                                                       
*                                                                               
BININS   EQU   X'01'               INSERT INTO BINSRCH TABLE                    
COLLEN   EQU   10                                                               
COMMA    EQU   C','                                                             
EMINUS   EQU   X'40'               MINUS=YES                                    
EZERO    EQU   X'20'               ZERO=NOBLANK                                 
MAXCOL   EQU   (L'P-(PFSTCOL-PRTD))/(COLLEN+1) MAX DATA TYPE COLUMNS            
         SPACE 2                                                                
* BUPPERD                                                                       
         PRINT OFF                                                              
       ++INCLUDE BUPPERD                                                        
         PRINT ON                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009BUFIL30   05/01/02'                                      
         END                                                                    
