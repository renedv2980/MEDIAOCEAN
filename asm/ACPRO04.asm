*          DATA SET ACPRO04    AT LEVEL 048 AS OF 02/11/21                      
*PHASE T60B04A                                                                  
         TITLE 'T60B04 - PRODUCTION OPTION MAINT'                               
*----------------------------------------------------------------------         
*GHOA 045 14FEB18 DSRD-18081 NEW OPT/MAIN 'EMAIL IF PERSN ASSIGN 2 JOB'         
*GHOA 046 2019APR22 DSRD-22283 JBT - REMOVE DDS ONLY RESTRICTION RELINK         
*ASAX 047 26MAY20 DSRD-25987 NEW OPT/MAIN 'DATE EST IN LAST FISCAL YR'          
*GHOA 048 11FEB21 DSRD-22448 FNT: remove DDS only                               
*----------------------------------------------------------------------         
                                                                                
T60B04   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T60B04**,RA,RR=R2                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9                                                       
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
         ST    R2,RELO                                                          
         SPACE 1                                                                
OPT2     CLI   MODE,VALKEY                                                      
         BNE   OPT4                                                             
         LA    RE,LOCAL                                                         
         LA    RF,LOCALLN                                                       
         SR    R1,R1                                                            
         MVCL  RE,R0               CLEAR LOCAL WORKING STORAGE                  
         BAS   RE,VALHED                                                        
         MVI   INTMODE,EDTSCR      SET INTERNAL MODE TO EDIT SCREEN             
         CLI   RACHANGE,C'Y'       TEST RECORD/ACTION CHANGED                   
         BNE   *+8                                                              
         MVI   INTMODE,FSTSCR      YES-FORCE FIRST SCREEN                       
         CLI   KEYCHG,C'Y'         TEST KEY FIELDS HAVE CHANGED                 
         BNE   *+8                                                              
         MVI   INTMODE,FSTSCR      YES-FORCE FIRST SCREEN                       
         B     OPTX                                                             
         SPACE 1                                                                
OPT4     CLI   MODE,VALREC                                                      
         BNE   OPTX                                                             
         CLI   INTMODE,FSTSCR      TEST FIRST SCREEN FORCED                     
         BE    OPT6                YES                                          
         CLI   PFKEY,PF7           TEST PF7=SCROLL BACKWARDS                    
         BE    OPT15               YES                                          
         CLI   PFKEY,PF8           TEST PF8=SCROLL FORWARDS                     
         BE    OPT8                                                             
         CLI   CONACT,C'D'                                                      
         BE    *+12                                                             
         BAS   RE,TSTEDT           TEST ANYTHING TO EDIT                        
         BE    OPT25               YES                                          
         MVI   INTMODE,NEXTSCR     SET INTERNAL MODE TO NEXT SCREEN             
         B     OPT8                NEXT SCREEN=SCROLL FORWARDS                  
         SPACE 1                                                                
* FIRST SCREEN - DISPLAY                                                        
*                                                                               
OPT6     LA    R3,1                                                             
         LA    R4,MAXOPTS-1(R3)    LAST ENTRY                                   
         CH    R4,NOPTIONS         TEST BEYOND LAST ENTRY                       
         BNH   *+8                 NO                                           
         LH    R4,NOPTIONS                                                      
         STC   R3,SVFIRST                                                       
         STC   R4,SVLAST                                                        
         LA    R4,1(R4)                                                         
         SR    R4,R3               COMPUTE N'ENTRIES ON SCREEN                  
         ST    R4,SVNOPTS                                                       
         B     OPT20                                                            
         SPACE 1                                                                
* SCROLL FORWARDS                                                               
*                                                                               
OPT8     ZIC   R3,SVLAST           PICK UP LAST ENTRY ON PREV SCRN              
         LA    R3,1(R3)            BUMP AHEAD TO NEXT ENTRY                     
         CLI   SCROLL,0                                                         
         BNE   OPT9                YES                                          
         CH    R3,NOPTIONS         TEST START ABOVE HIGHEST ENTRY               
         BNH   *+8                                                              
         LA    R3,1                YES-FORCE WRAP-AROUND TO FIRST PAGE          
         B     OPT10                                                            
         SPACE 1                                                                
OPT9     ZIC   RE,SCROLL                                                        
         CH    R3,NOPTIONS         TEST IF LAST PAGE                            
         BNH   OPT9A               NO                                           
         IC    R3,SVFIRST          YES-GET FIRST ENTRY ON PAGE                  
         AR    R3,RE               ADD IN SCROLL AMOUNT                         
         B     OPT9B                                                            
*                                                                               
OPT9A    LA    RF,MAXOPTS                                                       
         AR    R3,RE               ADD SCROLL AMOUNT                            
         SR    R3,RF               LESS 1 PAGE                                  
         BP    *+8                                                              
         LA    R3,1                                                             
*                                                                               
OPT9B    CH    R3,NOPTIONS         TEST IF PAST HIGHEST ENTRY                   
         BNH   *+8                                                              
         LH    R3,NOPTIONS         YES-FORCE DISPLAY OF LAST ENTRY              
         SPACE 1                                                                
OPT10    LA    R4,MAXOPTS-1(R3)    R4=LAST ENTRY                                
         CH    R4,NOPTIONS         TEST BEYOND HIGHEST ENTRY                    
         BNH   *+8                                                              
         LH    R4,NOPTIONS         YES-ADJUST LAST ENTRY                        
         STC   R3,SVFIRST                                                       
         STC   R4,SVLAST                                                        
         LA    R4,1(R4)                                                         
         SR    R4,R3               N'OPTIONS ON SCREEN                          
         ST    R4,SVNOPTS                                                       
         B     OPT20                                                            
         SPACE 1                                                                
* SCROLL BACKWARDS                                                              
*                                                                               
OPT15    ZIC   R3,SVFIRST          R3=FIRST ENTRY ON SCREEN                     
         SR    R4,R4                                                            
         ICM   R4,1,SCROLL                                                      
         BNZ   *+8                 BACK UP BY SCROLL AMOUNT                     
         LA    R4,MAXOPTS          USE MAXIMUM OPTIONS ON PAGE                  
         SR    R3,R4               R3=FIRST ENTRY ON NEW PAGE                   
         BP    *+8                                                              
         LA    R3,1                CANNOT GO BACK PAST 1ST ENTRY                
         LA    R4,MAXOPTS-1(R3)    SET LAST ENTRY                               
         CH    R4,NOPTIONS         TEST BEYOND OPTION LIST                      
         BNH   *+8                                                              
         LH    R4,NOPTIONS         YES-ADJUST LAST ENTRY                        
         STC   R3,SVFIRST                                                       
         STC   R4,SVLAST                                                        
         LA    R4,1(R4)            COMPUTE N'OPTIONS ON PAGE                    
         SR    R4,R3                                                            
         ST    R4,SVNOPTS          SET N'OPTIONS ON SCREEN                      
         B     OPT20                                                            
         SPACE 1                                                                
*                                                                               
* DISPLAY LOGIC                                                                 
*                                                                               
OPT20    GOTO1 VCLEARF,DMCB,PROFSETH,PROPFH                                     
         GOTO1 (RF),(R1),(1,PROFOPTH),PROPFH                                    
         BAS   RE,FORM             FORMAT SCREEN                                
         BAS   RE,DIS              DISPLAY OPTION RECORD VALUES                 
*                                                                               
* EXIT FROM DISPLAY LOGIC - SET MESSAGE AND CURSOR                              
*                                                                               
         MVC   CONHEAD(9),=C'** OPTION'                                         
         LA    R4,CONHEAD+9        R4=OUTPUT POINTER                            
         ZIC   R0,SVFIRST                                                       
         CLC   SVFIRST,SVLAST      TEST ONLY 1 LINE ON SCREEN                   
         BE    *+12                YES                                          
         MVI   0(R4),C'S'                                                       
         LA    R4,1(R4)                                                         
*                                                                               
         LA    R4,1(R4)            POSITION FOR LINE NUMBER                     
         EDIT  (R0),(3,0(R4)),ALIGN=LEFT                                        
         AR    R4,R0               UPDATE OUTPUT POINTER                        
         CLC   SVFIRST,SVLAST      TEST ONLY 1 LINE                             
         BE    OPT21               YES                                          
*                                                                               
         MVI   0(R4),C'-'                                                       
         LA    R4,1(R4)                                                         
         ZIC   R0,SVLAST                                                        
         EDIT  (R0),(3,0(R4)),ALIGN=LEFT                                        
         AR    R4,R0                                                            
*                                                                               
OPT21    LA    R4,1(R4)                                                         
         MVC   0(2,R4),=C'OF'                                                   
         LA    R4,3(R4)                                                         
         LH    R0,NOPTIONS                                                      
         EDIT  (R0),(3,0(R4)),ALIGN=LEFT                                        
         AR    R4,R0                                                            
         MVC   1(L'DISMSG,R4),DISMSG                                            
*                                                                               
OPT22    BAS   RE,SETCUR           SET CURSOR POSITION                          
         ST    R2,ACURFORC                                                      
         B     OPTX                                                             
         SPACE 1                                                                
* EDIT LOGIC                                                                    
*                                                                               
OPT25    BAS   RE,EDT                                                           
         CLI   UPDATE,C'Y'         TEST FOR ANY SUBSTANTIVE CHANGES             
         BE    OPT28               YES                                          
         MVC   CONHEAD(L'NOCHGMSG),NOCHGMSG                                     
         B     OPT30                                                            
         SPACE 1                                                                
OPT28    GOTO1 VCLEARF,DMCB,PROFSETH,PROPFH                                     
         GOTO1 (RF),(R1),(1,PROFOPTH),PROPFH                                    
         BAS   RE,FORM             FORMAT SCREEN                                
         BAS   RE,DIS              RE-DISPLAY OPTION RECORD VALUES              
         MVC   CONHEAD(L'CHAMSG),CHAMSG                                         
         SPACE 1                                                                
OPT30    BAS   RE,SETCUR                                                        
         ST    R2,ACURFORC                                                      
         SPACE 1                                                                
OPTX     B     XIT                                                              
         SPACE 2                                                                
* SUB-ROUTINE TO DETERMINE CURSOR POSITION                                      
*                                                                               
SETCUR   ST    RE,FULL                                                          
         LA    R2,PROFSETH         R2=A(VALUE FIELD)                            
         LA    R6,SVTABLE          R6=A(SAVED OPTION LIST)                      
         L     R0,SVNOPTS          R0=LOOP COUNTER                              
*                                                                               
SETCUR2  CLI   1(R6),C'U'          FIND FIRST UNPROTECTED FIELD                 
         BE    SETCURX                                                          
         LA    R2,DISPLIN(R2)                                                   
         LA    R6,OPTLISTL(R6)                                                  
         BCT   R0,SETCUR2                                                       
         LA    R2,PROFSETH         DEFAULT TO FIRST VALUE                       
*                                                                               
SETCURX  L     RE,FULL                                                          
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO VALIDATE HEADLINE FIELDS                                       
*                                                                               
VALHED   NTR1                                                                   
         LA    R4,OPTKEY           R4=A(EXACT OPTION KEY)                       
         USING ACOPKEY,R4                                                       
         MVI   ACOPRTYP,ACOPEQU                                                 
         MVI   ACOPSREC,ACOPSEQU                                                
         MVC   ACOPCUL,CUL                                                      
         MVI   OPTION,0                                                         
         MVI   KEYLEVEL,0                                                       
         MVI   KEYCHG,C'N'                                                      
         SPACE 1                                                                
* GENCON REQUIRES THE ENTRY OF AT LEAST ONE KEY FIELD BEFORE                    
* IT WILL GIVE A VALREC CALL-SO 'ALL' MUST BE INPUT IN OFFICE                   
* GROUP TO SEE COMPANY LEVEL OPTIONS                                            
*                                                                               
VALHED1  LA    R2,PROOGRH         OFFICE GROUP                                  
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VALHED2                                                          
         CLC   =C'ALL',8(R2)       TEST FOR 'ALL'                               
         BE    VALHED2                                                          
         MVI   ERROR,INVALID                                                    
         CLI   5(R2),1                                                          
         BNE   ERREND                                                           
         GOTO1 VALOG                                                            
         MVC   ACOPOG,EFFOFG                                                    
         MVC   OPTOG,EFFOFG                                                     
         MVI   KEYLEVEL,1                                                       
         SPACE 1                                                                
VALHED2  LA    R2,PROOFFH          OFFICE                                       
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VALHED4                                                          
         MVI   ERROR,NOTOFNOG      NOT COMPATIBLE WITH OFFICE GROUP             
         CLI   PROOGRH+5,0                                                      
         BNE   ERREND                                                           
         GOTO1 VALOFF                                                           
         MVC   ACOPOFC,EFFOFFC                                                  
         MVC   OPTOFF,EFFOFFC                                                   
         MVC   OPTOG,EFFOFG                                                     
         MVI   KEYLEVEL,2                                                       
         SPACE 1                                                                
VALHED4  LA    R2,PROCLIH          CLIENT                                       
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VALHED6                                                          
         MVI   ERROR,NOTCLNOG      NOT COMPATIBLE WITH OFFICE GROUP             
         CLI   PROOGRH+5,0                                                      
         BNE   ERREND                                                           
         MVI   ERROR,NOTCLNOF      NOT COMPATIBLE WITH OFFICE                   
         CLI   PROOFFH+5,0                                                      
         BNE   ERREND                                                           
         GOTO1 VALCLI                                                           
         MVC   ACOPCLI,CLICODE                                                  
         MVC   OPTCLI,CLICODE                                                   
         MVC   OPTOFF,EFFOFFC                                                   
         MVC   OPTOG,EFFOFG                                                     
         MVI   KEYLEVEL,3                                                       
         SPACE 1                                                                
VALHED6  LA    R2,PROPROH          PRODUCT                                      
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VALHED8                                                          
         MVI   ERROR,NEEDCLI       IF INPUT, NEED CLIENT AS WELL                
         CLI   PROCLIH+5,0                                                      
         BE    ERREND                                                           
         GOTO1 VALPROD                                                          
         MVC   ACOPPRO,PRODCODE                                                 
         MVC   OPTPROD,PRODCODE                                                 
         MVC   OPTOFF,EFFOFFC                                                   
         MVC   OPTOG,EFFOFG                                                     
         MVI   KEYLEVEL,4                                                       
         SPACE 1                                                                
VALHED8  LA    R2,PROJOBH          JOB                                          
         BAS   RE,TSTKEY                                                        
         MVI   MEDLEVEL,0                                                       
         CLI   5(R2),0                                                          
         BE    VALHED10                                                         
         MVI   MEDLEVEL,2                                                       
         MVI   ERROR,NEEDPRO       IF INPUT, NEED PRODUCT AS WELL               
         CLI   PROPROH+5,0                                                      
         BE    ERREND                                                           
         GOTO1 VALJOB                                                           
         MVC   ACOPJOB,JOBNUM                                                   
         MVC   OPTJOB,JOBNUM                                                    
         MVC   OPTMED,MEDIA                                                     
         MVC   OPTMG,MGROUP                                                     
         MVI   KEYLEVEL,5                                                       
         SPACE 1                                                                
VALHED10 LA    R2,PROMGRH          MEDIA GROUP                                  
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VALHED12                                                         
         MVI   ERROR,NOTJBNME      NOT COMPATIBLE WITH JOB                      
         CLI   PROJOBH+5,0                                                      
         BNE   ERREND                                                           
         GOTO1 VALMG                                                            
         MVC   ACOPMG,MGROUP                                                    
         MVC   OPTMG,MGROUP                                                     
         MVI   MEDLEVEL,1                                                       
         SPACE 1                                                                
VALHED12 LA    R2,PROMEDH          MEDIA                                        
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VALHED14                                                         
         MVI   ERROR,NOTMENMG      NOT COMPATIBLE WITH MEDIA GROUP              
         CLI   PROMGRH+5,0                                                      
         BNE   ERREND                                                           
         MVI   ERROR,NOTJBNME      NOT COMPATIBLE WITH JOB                      
         CLI   PROJOBH+5,0                                                      
         BNE   ERREND                                                           
         GOTO1 VALMED                                                           
         MVI   ACOPMG,X'FF'                                                     
         MVC   ACOPMED,MEDIA                                                    
         MVC   OPTMED,MEDIA                                                     
         MVC   OPTMG,MGROUP                                                     
         MVI   MEDLEVEL,2                                                       
         SPACE 1                                                                
VALHED14 LA    R2,PROWGRH          WORK GROUP                                   
         BAS   RE,TSTKEY                                                        
         MVI   WRKLEVEL,0                                                       
         CLI   5(R2),0                                                          
         BE    VALHED16                                                         
         GOTO1 VALWG                                                            
         MVC   ACOPWG,WGROUP                                                    
         MVC   OPTWG,WGROUP                                                     
         MVI   WRKLEVEL,1                                                       
         SPACE 1                                                                
VALHED16 LA    R2,PROWRKH          WORK CODE                                    
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VALHED18                                                         
         MVI   ERROR,NOTWKNWG      NOT COMPATIBLE WITH WORK GROUP               
         CLI   PROWGRH+5,0                                                      
         BNE   ERREND                                                           
         GOTO1 VALWORK                                                          
         MVI   ACOPWG,X'FF'                                                     
         MVC   ACOPWORK,WORKCODE                                                
         MVC   OPTWORK,WORKCODE                                                 
         MVC   OPTWG,WGROUP                                                     
         MVI   WRKLEVEL,2                                                       
         SPACE 1                                                                
VALHED18 LA    R2,PROOPTH                                                       
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    *+8                                                              
         BAS   RE,VALOPT           VALIDATE OPTIONS FIELD                       
*                                                                               
         LA    R2,PROSCRLH                                                      
         CLI   5(R2),0                                                          
         BE    VALHED20                                                         
         GOTO1 VALINUM                                                          
         MVC   SCROLL,ACTUAL                                                    
         SPACE 1                                                                
VALHED20 MVC   WORK(1),KEYLEVEL                                                 
         MVC   WORK+1(1),WRKLEVEL                                               
         L     R1,=A(LEVTAB)       LOOK UP LEVEL TABLE                          
         A     R1,RELO                                                          
         SPACE 1                                                                
VALHED21 CLI   0(R1),X'FF'         TEST FOR EOT                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   WORK(2),0(R1)       MATCH ON KEY LEVEL/WC LEVEL                  
         BE    VALHED22                                                         
         LA    R1,L'LEVTAB(R1)                                                  
         B     VALHED21                                                         
         SPACE 1                                                                
VALHED22 L     RF,4(R1)                                                         
         A     RF,RELO                                                          
         BCTR  RF,0                                                             
         BCTR  RF,0                BACK UP 2 BYTES                              
         MVC   NOPTIONS,0(RF)      SAVE N'OPTION ENTRIES IN LIST                
         LA    RF,2(RF)                                                         
         ST    RF,AOPTLIST         SAVE A(OPTION LIST FOR SCREEN)               
         BAS   RE,SETLIST          SET RUN LIST                                 
         OC    NOPTIONS,NOPTIONS   TEST ANY OPTIONS TO DISPLAY                  
         BNZ   VALHED24                                                         
         LA    R2,PROOGRH                                                       
         MVC   CONHEAD(L'NOOPT),NOOPT                                           
         B     MYEND               TAKE AN EARLY EXIT                           
         SPACE 1                                                                
VALHED24 OI    PROOGRH+4,X'20'     SET PREV VALID BITS ON IN KEY                
         OI    PROOFFH+4,X'20'                                                  
         OI    PROCLIH+4,X'20'                                                  
         OI    PROPROH+4,X'20'                                                  
         OI    PROJOBH+4,X'20'                                                  
         OI    PROMGRH+4,X'20'                                                  
         OI    PROMEDH+4,X'20'                                                  
         OI    PROWGRH+4,X'20'                                                  
         OI    PROWRKH+4,X'20'                                                  
         OI    PROOPTH+4,X'20'                                                  
         SPACE 1                                                                
VALHEDX  B     XIT                                                              
         SPACE 2                                                                
TSTKEY   TM    4(R2),X'20'         TEST IF KEY FIELD HAS CHANGED                
         BOR   RE                  NO                                           
         MVI   KEYCHG,C'Y'         YES                                          
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO VALIDATE OPTIONS FIELD                                         
*                                                                               
VALOPT   NTR1                                                                   
         LA    R0,L'PROOPT                                                      
         GOTO1 SCANNER,DMCB,((R0),(R2)),(2,AIO2),0                              
         MVI   ERROR,INVALID                                                    
         CLI   4(R1),0                                                          
         BE    ERREND                                                           
         ZIC   R6,4(R1)            R6=N'FIELDS                                  
         L     R4,AIO2             R4=A(SCANNNER BLOCK)                         
*                                                                               
VALOPT1  CLI   0(R4),0                                                          
         BE    ERREND                                                           
         CLI   0(R4),8                                                          
         BH    ERREND                                                           
         CLI   1(R4),0             TEST FOR PARAMETER                           
         BNE   ERREND              YES                                          
         ZIC   R1,0(R4)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8              ACTIVITY                                     
         B     *+10                                                             
         CLC   12(0,R4),=C'ACTIVITY'                                            
         BNE   VALOPT2                                                          
         MVI   OPTACT,C'A'         SHOW ACTIVITY IN PROTECTED FIELD             
         B     VALOPT10                                                         
*                                                                               
VALOPT2  EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R4),=C'ESTIMATE' TEST FOR ESTIMATE OPTIONS                  
         BNE   VALOPT4                                                          
         MVI   OPTEST,C'Y'                                                      
         B     VALOPT10                                                         
*                                                                               
VALOPT4  EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R4),=C'BILLING'                                             
         BNE   VALOPT5                                                          
         MVI   OPTBILL,C'Y'                                                     
         B     VALOPT10                                                         
*                                                                               
VALOPT5  EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R4),=C'STUDIO'                                              
         BNE   VALOPT6                                                          
         MVI   OPTSTUD,C'Y'                                                     
         B     VALOPT10                                                         
*                                                                               
VALOPT6  EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R4),=C'CBILL'                                               
         BNE   VALOPT7                                                          
         MVI   OPTNBIL,C'Y'                                                     
         B     VALOPT10                                                         
*                                                                               
VALOPT7  EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R4),=C'ORDER'                                               
         BNE   VALOPT8                                                          
         MVI   OPTORD,C'Y'                                                      
         B     VALOPT10                                                         
*                                                                               
VALOPT8  EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R4),=C'FILE'                                                
         BE    VALOPT9                                                          
         CLI   0(R4),2                                                          
         BNE   ERREND                                                           
         CLC   =C'FM',12(R4)                                                    
         BNE   ERREND                                                           
*                                                                               
VALOPT9  MVI   OPTFILE,C'Y'                                                     
*                                                                               
VALOPT10 LA    R4,22+L'PROOPT(R4)                                               
         BCT   R6,VALOPT1                                                       
         B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO SET THE RUN-TIME OPTION LIST                                   
*                                                                               
SETLIST  NTR1  ,                                                                
         LA    R4,OPTLIST          R4=A(RUN TIME LIST)                          
         SR    R5,R5               R5=OPTION COUNTER                            
         L     R3,AOPTLIST         R3=A(OPTION TABLE)                           
         USING OPTLISTD,R3                                                      
         LH    R2,NOPTIONS         R2=LOOP COUNTER                              
*                                                                               
*                                                                               
SETL02   CLI   AUTHOR,0            TEST FOR SECURITY IN FORCE                   
         BE    SETL04              NO                                           
         CLI   OPTLSEC,0           TEST FOR SECURITY TYPE                       
         BE    SETL04              NONE-SO SKIP SECURITY CHECK                  
*                                                                               
         LA    R0,TYPES            R0=LOOP COUNTER                              
         LA    RE,TYPETAB          RE=A(SECURITY TYPE TABLE)                    
         CLC   OPTLSEC,0(RE)       MATCH ON SECURITY TYPE                       
         BE    *+16                YES                                          
         LA    RE,L'TYPETAB(RE)                                                 
         BCT   R0,*-14                                                          
         B     SETL04                                                           
*                                                                               
         MVC   BYTE,AUTHOR                                                      
         NC    BYTE,1(RE)          TEST TO DISPLAY OPTION                       
         BZ    SETL14              NO                                           
*                                                                               
         CLI   OPTLTYPE,C'U'       TEST FOR UNPROTECTED FIELD                   
         BNE   SETL04              NO                                           
*                                                                               
         MVC   BYTE,AUTHOR                                                      
         NC    BYTE,2(RE)          TEST IF OK FOR UNPROTECTED                   
         BNZ   *+8                 YES                                          
         MVI   OPTLTYPE,C'P'       NO-FORCE PROTECTED FIELD                     
*                                                                               
SETL04   OC    OPTFILTS,OPTFILTS   TEST FOR ANY FILTERS                         
         BZ    SETL06              NO-TAKE THE OPTIONS                          
*                                                                               
         CLI   OPTEST,C'Y'         TEST ESTIMATE FILTER                         
         BNE   *+12                                                             
         TM    OPTLIND,OPTLEST                                                  
         BO    SETL06              PASSES ESTIMATE FILTER                       
*                                                                               
         CLI   OPTBILL,C'Y'                                                     
         BNE   *+12                                                             
         TM    OPTLIND,OPTLBILL                                                 
         BO    SETL06              PASSES BILLING FILTER                        
*                                                                               
         CLI   OPTSTUD,C'Y'                                                     
         BNE   *+12                                                             
         TM    OPTLIND,OPTLSTUD                                                 
         BO    SETL06              PASSES STUDIO FILTER                         
*                                                                               
         CLI   OPTORD,C'Y'                                                      
         BNE   *+12                                                             
         TM    OPTLIND,OPTLORD                                                  
         BO    SETL06              PASSES ORDER FILTER                          
*                                                                               
         CLI   OPTFILE,C'Y'                                                     
         BNE   SETL14                                                           
         TM    OPTLIND,OPTLFILE                                                 
         BZ    SETL14              FLUNKS FILE MAINT FILTER                     
*                                                                               
SETL06   TM    OPTLIND,OPTLEMU     TEST EMULATED FILE ONLY OPTION               
         BZ    *+12                                                             
         CLI   EMULATE,C'Y'                                                     
         BNE   SETL14              DON'T DISPLAY OPTION                         
*                                                                               
         TM    OPTLIND,OPTLDDS     TEST DDS ONLY OPTION                         
         BZ    *+12                                                             
         CLI   DDS,C'Y'            TEST DDS TERMINAL                            
         BNE   SETL14              NO-SKIP OPTION                               
*                                                                               
         OC    OPTLAGY,OPTLAGY     ANY SPECIFIC AGENCY?                         
         BZ    SETL08              NO                                           
         CLC   OPTLAGY,CUL         YES, IS THIS THE AGENCY?                     
         BNE   SETL14              NO                                           
*                                                                               
SETL08   CLI   OPTLCTRY,CTRYANY    TEST FOR COUNTRY FILTER                      
         BE    SETL12              NO                                           
         TM    OPTLCTRY,CTRYNOT    TEST FOR NEGATIVE FILTER                     
         BO    SETL10              YES                                          
         CLC   OPTLCTRY,AGYCTRY    APPLY COUNTRY FILTER                         
         BE    SETL12                                                           
         B     SETL14              REJECT OPTION                                
*                                                                               
SETL10   MVC   BYTE,OPTLCTRY                                                    
         NI    BYTE,X'FF'-CTRYNOT                                               
         CLC   AGYCTRY,BYTE        NEGATIVE FILTER                              
         BE    SETL14              SKIP OPTION                                  
*                                                                               
SETL12   MVC   0(OPTLISTL,R4),0(R3) COPY OPTION ENTRY                           
         LA    R5,1(R5)            INCREMENT COUNTER                            
         LA    R4,OPTLISTL(R4)     BUMP RUN-TIME LIST POINTER                   
*                                                                               
SETL14   LA    R3,OPTLISTL(R3)                                                  
         BCT   R2,SETL02                                                        
*                                                                               
SETLISTX STH   R5,NOPTIONS         UPDATE OPTION LIST COUNTER                   
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
* SUB-ROUTINE TO TEST FOR CHANGES TO OPTION VALUE FIELDS                        
* ON EXIT, CC=EQ TO EDIT SCREEN, CC=NEQ IF NOTHING TO EDIT                      
*                                                                               
TSTEDT   ST    RE,FULL                                                          
         LA    R2,PROFSETH                                                      
         ICM   R3,15,SVNOPTS                                                    
         BZ    TSTEDTN             NOTHING TO EDIT                              
         LA    R6,SVTABLE          R6=A(OPTION SAVE LIST)                       
         SPACE 1                                                                
TSTEDT2  CLI   1(R6),C'P'          TEST FOR PROTECTED FIELD                     
         BE    *+12                                                             
         TM    4(R2),X'20'         TEST FOR CHANGE IN FIELD                     
         BZ    TSTEDTY             YES                                          
         LA    R2,DISPLIN(R2)                                                   
         LA    R6,OPTLISTL(R6)                                                  
         BCT   R3,TSTEDT2                                                       
         SPACE 1                                                                
TSTEDTN  LTR   RB,RB               SET CC=NEQ                                   
         B     TSTEDTX                                                          
         SPACE 1                                                                
TSTEDTY  CR    RB,RB               SET CC=EQ                                    
         SPACE 1                                                                
TSTEDTX  L     RE,FULL                                                          
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO FORMAT THE SCREEN                                              
*                                                                               
FORM     NTR1                                                                   
         ZIC   R3,SVFIRST                                                       
         LA    R2,PROFOPTH         R2=A(OPTION DESCRIPTION FIELD)               
         LA    R4,OPTLIST          R4=A(OPTION TABLE)                           
         USING OPTLISTD,R4                                                      
         LR    R1,R3                                                            
         BCTR  R1,0                                                             
         MH    R1,=Y(OPTLISTL)                                                  
         LA    R4,0(R1,R4)         POINT TO FIRST ENTRY                         
         XC    SVTABLE,SVTABLE                                                  
         LA    R6,SVTABLE          R6=A(TABLE)                                  
         L     R0,SVNOPTS          R0=N'OPTIONS ON SCREEN                       
*                                                                               
FORM2    L     R5,AOPTTAB                                                       
         USING OPTBD,R5                                                         
FORM3    CLI   OPTBOPN,0                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   OPTBOPN,0(R4)       MATCH ON OPTION NUMBER                       
         BE    FORM4                                                            
         LA    R5,OPTBL(R5)                                                     
         B     FORM3                                                            
*                                                                               
FORM4    MVC   0(OPTLISTL,R6),0(R4)  SAVE OPTION LIST ENTRY                     
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'OPTBSHRT),OPTBSHRT SHORT KEYWORD                          
         LA    RE,WORK+L'OPTBSHRT                                               
         MVI   0(RE),C'-'          SEPARATES KEYWORD AND DESCRIPTION            
         MVC   1(L'OPTBDESC,RE),OPTBDESC                                        
         MVC   8(L'PROFOPT,R2),WORK MOVE OUT NAME                               
         BAS   RE,BUMP                                                          
         MVI   BYTE,X'88'          SET MASK FOR UNPROTECTED FIELD               
         CLI   OPTLTYPE,C'U'                                                    
         BE    *+8                                                              
         MVI   BYTE,X'A0'                                                       
         OC    6(1,R2),BYTE                                                     
         OI    4(R2),X'20'         MARK FIELD AS PREV VALID                     
         LA    R1,PROFSETX-PROFSETH(R2)                                         
         MVC   0(1,R1),OPTBOPN     MOVE OUT OPTION NUMBER FOR HELP              
         LA    R6,OPTLISTL(R6)     NEXT SAVE TABLE ENTRY                        
         LA    R4,OPTLISTL(R4)     NEXT OPTION LIST ENTRY                       
         BAS   RE,BUMP                                                          
         BAS   RE,BUMP                                                          
         BCT   R0,FORM2                                                         
*                                                                               
FORMX    B     XIT                                                              
         DROP  R4,R5                                                            
         EJECT                                                                  
* SUB-ROUTINE TO DISPLAY OPTIONS VALUES ON THE SCREEN                           
*                                                                               
DIS      NTR1                                                                   
         MVC   SVELEMSL,=H'3'      INITIALIZE SAVED ELEMENT TABLE               
         LA    RE,SVELEMS                                                       
         LA    RF,L'SVELEMS                                                     
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         OI    PRODHEDH+6,X'80'    FORMAT RIGHT HEADING                         
         MVC   PRODHED(21),=C'FROM  LAST ACTIVITY  '                            
         CLI   OPTACT,C'A'                                                      
         BNE   DIS2                                                             
         MVC   PRODHED,SPACES                                                   
         ZIC   R1,KEYLEVEL                                                      
         LA    R1,2(R1)                                                         
         MH    R1,=H'3'                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     DIS2                                                             
         MVC   PRODHED(0),=C'DF AG OG OF CL PR JB '                             
         SPACE 1                                                                
DIS2     L     R2,AIO                                                           
         L     R1,=A(DEFREC)       POINT TO DEFAULT RECORD FIRST                
         A     R1,RELO                                                          
         ST    R1,AIO                                                           
         MVC   FROMLEV,=C'DF'                                                   
         MVC   FROMMW,=C'**'                                                    
         XC    FROMDISP,FROMDISP                                                
         BAS   RE,DISREC           AND FORMAT THAT                              
         ST    R2,AIO                                                           
         SPACE 1                                                                
         LA    R2,FROMLIST         NOW SET UP TO HANDLE MULT LEVELS             
         LA    R3,3                                                             
         SR    R4,R4                                                            
         ZIC   R0,KEYLEVEL                                                      
         AH    R0,=H'1'                                                         
         SPACE 1                                                                
DIS4     MVC   FROMLEV,0(R2)       PASS 2 CHARACTER FROM CODE                   
         STH   R3,FROMDISP         AND DISPLACEMENT INTO DISPLAY                
         STC   R4,FROMLEVN         AND THE LEVEL NUMBER                         
         BAS   RE,OPTIO            DEAL WITH OPTIONS AT THAT LEVEL              
         LA    R2,2(R2)                                                         
         LA    R3,3(R3)                                                         
         LA    R4,1(R4)                                                         
         BCT   R0,DIS4                                                          
         B     XIT                                                              
         SPACE 1                                                                
FROMLIST DC    C'AGOGOFCLPRJB'                                                  
         B     XIT                                                              
         EJECT                                                                  
*              OPTION I/O CONTROL AT A LEVEL                                    
         SPACE 3                                                                
*                                  FROMLEVN IS 0-5                              
*                                  MEDLEVEL & WRKLEVEL 0-2                      
*                                  SETS FROMMW                                  
         SPACE 1                                                                
OPTIO    NTR1                                                                   
         LA    R4,KEY                                                           
         USING ACOPKEY,R4                                                       
         XC    ACOPKEY,ACOPKEY                                                  
         MVI   ACOPRTYP,ACOPEQU                                                 
         MVI   ACOPSREC,ACOPSEQU                                                
         MVC   ACOPCUL,CUL                                                      
         SPACE 1                                                                
*                                  REST OF KEY DEPENDS ON FROMLEVN              
         CLI   FROMLEVN,1          AGENCY LEVEL 0                               
         BL    OPTIO2                                                           
         MVC   ACOPOG,OPTOG        OFFICE GROUP LEVEL 1                         
         BNE   OPTIO1                                                           
         CLI   OPTOG,0             (MAY NOT BE AN OFFICE GROUP)                 
         BE    XIT                                                              
         B     OPTIO2                                                           
         SPACE 1                                                                
OPTIO1   MVI   ACOPOG,0                                                         
         MVC   ACOPOFC,OPTOFF      OFFICE LEVEL 2                               
         CLI   FROMLEVN,3                                                       
         BL    OPTIO2                                                           
         XC    ACOPOFC,ACOPOFC                                                  
         MVC   ACOPCLI,OPTCLI      CLIENT LEVEL 3                               
         CLI   FROMLEVN,3                                                       
         BE    OPTIO2                                                           
         MVC   ACOPPRO,OPTPROD     PRODUCT LEVEL 4                              
         CLI   FROMLEVN,5                                                       
         BL    OPTIO2                                                           
         MVC   ACOPJOB,OPTJOB      JOB NUMBER 5                                 
         SPACE 1                                                                
OPTIO2   MVI   FROMMW,C'*'         TRY THIS AT ALL MEDIA LEVEL                  
         BAS   RE,OPTIO4                                                        
         CLI   MEDLEVEL,0                                                       
         BE    XIT                                                              
         SPACE 1                                                                
         MVI   FROMMW,C'G'         THEN AT MEDIA GROUP                          
         MVC   ACOPMG,OPTMG                                                     
         CLI   OPTMG,0             (IF RELEVANT)                                
         BE    *+8                                                              
         BAS   RE,OPTIO4                                                        
         CLI   MEDLEVEL,1                                                       
         BE    XIT                                                              
         SPACE 1                                                                
         MVI   FROMMW,C'M'         FINALLY SPECIFIC MEDIA                       
         MVI   ACOPMG,X'FF'                                                     
         MVC   ACOPMED,OPTMED                                                   
         BAS   RE,OPTIO4                                                        
         B     XIT                                                              
         SPACE 1                                                                
OPTIO4   NTR1                                                                   
         MVC   MEDKEY,KEY          SAVE ENTRY VERSION OF KEY                    
         MVI   FROMMW+1,C'*'       ALL WORK GROUP LEVEL                         
         GOTO1 HIGH                                                             
         CLC   ACOPKEY,KEYSAVE                                                  
         BNE   *+8                                                              
         BAS   RE,DISREC           DISPLAY IT                                   
         MVC   KEY,MEDKEY          RESTORE ENTRY VERSION                        
         CLI   WRKLEVEL,0                                                       
         BE    XIT                                                              
         SPACE 1                                                                
         CLI   OPTWG,0             TEST FOR A WORK GROUP                        
         BE    OPTIO4A             NO-MUST BE WC WHICH HAS NO WG                
         MVI   FROMMW+1,C'G'       WORK GROUP LEVEL                             
         MVC   ACOPWG,OPTWG                                                     
         GOTO1 HIGH                                                             
         CLC   ACOPKEY,KEYSAVE                                                  
         BNE   *+8                                                              
         BAS   RE,DISREC                                                        
         MVC   KEY,MEDKEY          RESTORE ENTRY VERSION                        
         CLI   WRKLEVEL,1                                                       
         BE    XIT                                                              
         SPACE 1                                                                
OPTIO4A  MVI   FROMMW+1,C'W'       WORK LEVEL                                   
         MVI   ACOPWG,X'FF'                                                     
         MVC   ACOPWORK,OPTWORK                                                 
         GOTO1 HIGH                                                             
         CLC   ACOPKEY,KEYSAVE                                                  
         BNE   *+8                                                              
         BAS   RE,DISREC                                                        
         MVC   KEY,MEDKEY          RESTORE ENTRY VERSION                        
         B     XIT                                                              
         SPACE 1                                                                
         EJECT                                                                  
* SUB-ROUTINE TO DISPLAY A RECORD'S OPTIONS VALUES ON SCREEN                    
* AT ENTRY, THE FOLLOWING APPLIES                                               
*                                  FROMLEV  HAS AG OG ETC                       
* AIO=A(OPTION RECORD)             FROMLEVN IS 0-5                              
*                                  FROMDISP DISPLACEMENT INTO DISPLAY           
*                                  FROMMW IS SET TO MEDIA/WORK                  
DISREC   NTR1                                                                   
         LA    R2,PROFSETH         R2=A(OPTION VALUE FIELD HEADER)              
         L     R3,SVNOPTS          R3=LOOP COUNTER                              
         LA    R5,SVTABLE          R5=A(OPTION NUMBER TABLE)                    
*                                                                               
DISREC2  GOTO1 HELLO,DMCB,(C'g',SYSFIL),('ACOPELQ',AIO),(1,(R5)),0,2600         
         CLI   12(R1),0            TEST IF ELEMENT FOUND                        
         BE    DISREC4             YES                                          
         BAS   RE,BUMP             ADVANCE TO ACTIVITY FIELD                    
         B     DISREC10                                                         
*                                                                               
DISREC4  L     R6,12(R1)                                                        
         USING ACOPD,R6                                                         
         MVC   8(L'PROFSET,R2),SPACES                                           
         GOTO1 VDISOPT,DMCB,(R6),(R2)                                           
*                                                                               
DISREC6  XC    ELEMENT,ELEMENT     BUILD NEW SAVE ELEMENT                       
         LA    R4,ELEMENT                                                       
         USING SVELD,R4                                                         
         MVC   SVELCODE,ACOPEL                                                  
         MVC   SVELOPN,ACOPNUM                                                  
         ZIC   R1,ACOPLEN                                                       
         SH    R1,=Y(ACOPDATA+1-ACOPD)                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SVELDATA(0),ACOPDATA EXTRACT DATA                                
         LA    R1,SVELDATA+1-SVELD(R1) SET ELEMENT LENGTH                       
         STC   R1,SVELLEN                                                       
*                                                                               
         GOTO1 HELLO,DMCB,(C'D',=C'CORETAB'),('ACOPELQ',SVELEMSL),     X        
               (1,SVELOPN)                                                      
         GOTO1 HELLO,DMCB,(C'P',=C'CORETAB'),SVELEMSL,ELEMENT                   
*                                                                               
DISREC8  BAS   RE,BUMP                                                          
         CLI   OPTACT,C'A'                                                      
         BNE   DISREC9                                                          
         LH    R1,FROMDISP         DISPLACE INTO DISPLAY AREA                   
         LA    R1,8(R1,R2)                                                      
         MVC   0(2,R1),FROMMW      AND SHOW MEDIA/WORK INDICATOR                
         B     DISREC10                                                         
         SPACE 1                                                                
DISREC9  LA    R4,8(R2)            DISPLAY OPTION A                             
         MVC   0(L'PROFDEF,R4),SPACES                                           
         MVC   0(2,R4),FROMLEV     SHOW 2 CHARACTER LEVEL                       
         MVC   3(2,R4),FROMMW      AND MEDIA/WORK INDICATOR                     
         LA    R4,6(R4)                                                         
         MVC   0(8,R4),ACOPPERS                                                 
         GOTO1 DATCON,DMCB,(1,ACOPLAST),(8,9(R4))                               
         GOTO1 SQUASHER,DMCB,(R4),17                                            
*                                                                               
DISREC10 BAS   RE,BUMP                                                          
         BAS   RE,BUMP                                                          
         LA    R5,OPTLISTL(R5)     NEXT OPTION NUMBER                           
         BCT   R3,DISREC2                                                       
*                                                                               
DISRECX  B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
* SUB-ROUTINE TO EDIT THE OPTION VALUES                                         
*                                                                               
EDT      NTR1  WORK=(R4,POINTLN)                                                
         ST    R4,APOINT                                                        
         MVI   ADDSW,C'Y'          SET ADDING OPTION RECORD                     
         MVI   UPDATE,C'N'         SET UPDATE NEEDED TO NO                      
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING ACKEYD,R4                                                        
         MVC   ACOPKEY,OPTKEY      CHECK IF OPTIONS RECORD EXISTS               
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 HIGH                                                             
         CLC   ACOPKEY,KEYSAVE                                                  
         BNE   EDT1                                                             
*                                                                               
         MVI   ADDSW,C'N'          FOUND RECORD SO NOT ADDING IT                
         GOTO1 VSAVPTRS,DMCB,(X'80',AIO),APOINT                                 
         TM    ACSTATUS,X'80'      TEST FOR DELETED RECORD                      
         BO    EDT1                YES-BUILD SKELETAL RECORD                    
         B     EDT2                                                             
*                                                                               
EDT1     L     R4,AIO              R4=A(IO AREA)                                
         LR    RE,R4                                                            
         L     RF,SIZEIO                                                        
         SR    R1,R1                                                            
         MVCL  RE,R0               CLEAR IO AREA                                
         MVC   ACOPKEY,OPTKEY      INITIALIZE KEY                               
         MVC   ACLENGTH,=Y(ACRECORD+1-ACKEYD) AND RECORD LENGTH                 
         CLI   ADDSW,C'Y'          TEST FOR ADDING OPTION KEY                   
         BNE   EDT2                NO-HAVE SAVED POINTERS ALREADY               
         GOTO1 VSAVPTRS,DMCB,(X'80',0),APOINT                                   
*                                                                               
EDT2     LA    R2,PROFSETH         R2=A(OPTION SETTING FIELD)                   
         LA    R5,SVTABLE          R5=A(OPTION NUMBER TABLE)                    
         L     R3,SVNOPTS          R3=LOOP COUNTER                              
*                                                                               
EDT4     CLI   1(R5),C'P'          TEST FOR PROTECTED FIELD                     
         BE    EDT10               YES                                          
         TM    4(R2),X'20'         TEST IF FIELD CHANGED                        
         BO    EDT10               NO                                           
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT          BUILD SKELETAL OPTION ELEMENT                
         USING ACOPD,R6                                                         
         MVI   ACOPEL,ACOPELQ                                                   
         MVC   ACOPNUM,0(R5)       SET OPTION NUMBER                            
         CLI   5(R2),0             TEST IF FIELD ERASED                         
         BNE   EDT6                NO                                           
*                                                                               
         GOTO1 HELLO,DMCB,(C'G',SYSFIL),('ACOPELQ',AIO),(1,ACOPNUM)             
         CLI   12(R1),0            TEST IF OPTION EXISTS FOR RECORD             
         BNE   EDT10               NO-NOTHING TO CHANGE                         
         GOTO1 HELLO,DMCB,(C'D',SYSFIL),('ACOPELQ',AIO),(1,ACOPNUM)             
         MVI   UPDATE,C'Y'         NOTE TO UPDATE RECORD                        
         B     EDT10               ALL DONE                                     
*                                                                               
EDT6     MVI   ERROR,INVALID                                                    
         GOTO1 VVALOPT,DMCB,(R2),(R6)                                           
         BNE   ERREND              ERROR IN INPUT                               
*                                                                               
         GOTO1 HELLO,DMCB,(C'G',=C'CORETAB'),('ACOPELQ',SVELEMSL),     X        
               (1,ACOPNUM)                                                      
         CLI   12(R1),0            TEST IF SAVE ELEMENT FOUND                   
         BNE   EDT8                NO-FIELD DEFINITELY CHANGED                  
         L     RE,12(R1)                                                        
         USING SVELD,RE                                                         
         ZIC   RF,SVELLEN                                                       
         SH    RF,=Y(SVELDATA-SVELD) LENGTH OF SAVED OPTION DATA                
         ZIC   R1,ACOPLEN                                                       
         SH    R1,=Y(ACOPDATA-ACOPD)                                            
         CR    R1,RF               TEST IF SAME LENGTH                          
         BNE   EDT8                NO-FIELD DEFINITELY CHANGED                  
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   ACOPDATA(0),SVELDATA TEST FOR DATA CHANGE                        
         BE    EDT10               NO SIGNIFICANT CHANGE IN DATA                
*                                                                               
EDT8     MVI   UPDATE,C'Y'                                                      
         GOTO1 HELLO,DMCB,(C'D',SYSFIL),('ACOPELQ',AIO),(1,ACOPNUM)             
         GOTO1 ADDELEM                                                          
         BAS   RE,DISACT           RE-DISPLAY ACTIVITY FIELD                    
*                                                                               
EDT10    LA    R2,DISPLIN(R2)      NEXT OPTION SETTING FIELD                    
         LA    R5,OPTLISTL(R5)     NEXT OPTION NUMBER                           
         BCT   R3,EDT4                                                          
*                                                                               
EDT15    CLI   UPDATE,C'N'         TEST ANYTHING TO UPDATE                      
         BE    EDTX                NO                                           
*                                                                               
         GOTO1 PERSIN                                                           
         CLI   ADDSW,C'Y'          TEST ADDING A RECORD                         
         BNE   EDT20               NO                                           
         GOTO1 ADD                                                              
         GOTO1 VCHGPTRS,DMCB,(X'80',AIO),APOINT                                 
         B     EDTX                                                             
*                                                                               
EDT20    LA    R4,KEY                                                           
         MVC   ACOPKEY,OPTKEY      RE-READ RECORD BEFORE WRITE                  
         MVC   AIO,AIO2                                                         
         OI    DMINBTS,X'08'                                                    
         GOTO1 READ                                                             
         MVC   AIO,AIO1            RESTORE IO POINTER                           
*                                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,ACOPELQ      TEST IF ANY OPTIONS ELEMS ON REC             
         BAS   RE,GETELIO                                                       
         BE    *+8                 YES                                          
         OI    ACSTATUS,X'80'      MARK RECORD AS DELETED                       
         GOTO1 WRITE                                                            
         GOTO1 VCHGPTRS,DMCB,(X'80',AIO),APOINT                                 
*                                                                               
EDTX     B     XIT                                                              
         DROP  RE                                                               
         EJECT                                                                  
*              DISPLAY ACTIVITY AFTER FIELD INPUT                               
         SPACE 3                                                                
DISACT   NTR1                                                                   
         BAS   RE,BUMP             GET TO THAT FIELD                            
         OI    6(R2),X'80'         ENSURE OUTPUT                                
         SPACE 1                                                                
         ZIC   R1,MEDLEVEL         SET MEDIA/WORK MASK                          
         LA    R1,MEDLTAB(R1)                                                   
         MVC   FROMMW(1),0(R1)                                                  
         ZIC   R1,WRKLEVEL                                                      
         LA    R1,WRKLTAB(R1)                                                   
         MVC   FROMMW+1(1),0(R1)                                                
         CLI   OPTACT,C'A'                                                      
         BNE   DISACT2                                                          
         ZIC   R1,KEYLEVEL                                                      
         LA    R1,1(R1)                                                         
         MH    R1,=H'3'                                                         
         LA    R1,8(R1,R2)                                                      
         MVC   0(2,R1),FROMMW                                                   
         B     XIT                                                              
         SPACE 1                                                                
DISACT2  LA    R3,8(R2)            DISPLAY OPTION A                             
         MVC   0(L'PROFDEF,R3),SPACES PRE-CLEAR FIELD                           
         ZIC   R1,KEYLEVEL                                                      
         SLL   R1,1                                                             
         LA    R1,KEYLTAB(R1)                                                   
         MVC   0(2,R3),0(R1)       SHOW 2 CHARACTER LEVEL                       
         MVC   3(2,R3),FROMMW      AND MEDIA/WORK INDICATOR                     
         LA    R3,6(R3)                                                         
         MVC   0(8,R3),ACOPPERS                                                 
         GOTO1 DATCON,DMCB,(1,ACOPLAST),(8,9(R3))                               
         GOTO1 SQUASHER,DMCB,(R3),17                                            
         B     XIT                                                              
         SPACE 1                                                                
KEYLTAB  DC    C'AGOGOFCLPRJB'                                                  
MEDLTAB  DC    C'*GM'                                                           
WRKLTAB  DC    C'*GW'                                                           
         EJECT                                                                  
*              SUPPORTING SUBROUTINES                                           
         SPACE 3                                                                
BUMP     ZIC   RF,0(R2)            BUMP TO NEXT HEADER                          
         AR    R2,RF                                                            
         BR    RE                                                               
         SPACE 1                                                                
BUMPTOUN ZIC   RF,0(R2)            BUMP TO NEXT UNPROTECTED                     
         AR    R2,RF                                                            
         CLI   0(R2),0                                                          
         BER   RE                                                               
         TM    1(R2),X'20'                                                      
         BO    BUMPTOUN                                                         
         BR    RE                                                               
         SPACE 1                                                                
GETELIO  L     R6,AIO                                                           
         GETEL (R6),DATADISP,ELCODE                                             
         SPACE 1                                                                
INVEND   MVI   ERROR,INVALID       INVALID EXIT                                 
         B     ERREND                                                           
         SPACE 1                                                                
MYEND    MVI   ERROR,SUPPLIED      USING MY OWN ERROR MSG                       
         SPACE 1                                                                
ERREND   GOTO1 VERRCUR                                                          
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              LTORG FOR THIS PHASE                                             
         SPACE 3                                                                
RELO     DS    A                                                                
         SPACE 1                                                                
PATCH    DS    0H                                                               
         DC    XL32'00'                                                         
         SPACE 2                                                                
DISMSG   DC    C'DISPLAYED - ENTER CHANGES **'                                  
CHAMSG   DC    C'** CHANGES COMPLETED **'                                       
NOCHGMSG DC    C'** NOTHING TO CHANGE FOR THIS SCREEN **'                       
NOOPT    DC    C'** THERE ARE NO OPTIONS TO DISPLAY  **'                        
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE ACPRODFREC                                                     
         EJECT                                                                  
* TABLE OF LEVELS AND THEIR CORRESPONDING OPTIONS LISTS                         
*                                                                               
         DS    0F                                                               
LEVTAB   DS    0CL8                                                             
         DC    AL1(0,0,0,0),A(AGYTAB)  AGENCY  NO WC                            
         DC    AL1(0,1,0,0),A(WGTAB)        WGRP                                
         DC    AL1(0,2,0,0),A(WCTAB)        WORK                                
         DC    AL1(1,0,0,0),A(OGTAB)   OGROUP  NO WC                            
         DC    AL1(1,1,0,0),A(WGTAB)        WGRP                                
         DC    AL1(1,2,0,0),A(WCTAB)        WORK                                
         DC    AL1(2,0,0,0),A(OFFTAB)  OFFICE  NO WC                            
         DC    AL1(2,1,0,0),A(WGTAB)        WGRP                                
         DC    AL1(2,2,0,0),A(WCTAB)        WORK                                
         DC    AL1(3,0,0,0),A(CLITAB)  CLIENT NO WC                             
         DC    AL1(3,1,0,0),A(WGTAB)        WGRP                                
         DC    AL1(3,2,0,0),A(WCTAB)        WORK                                
         DC    AL1(4,0,0,0),A(PROTAB)  PRODUCT NO WC                            
         DC    AL1(4,1,0,0),A(WGTAB)        WGRP                                
         DC    AL1(4,2,0,0),A(WCTAB)        WORK                                
         DC    AL1(5,0,0,0),A(JOBTAB)  JOB   NO WC                              
         DC    AL1(5,1,0,0),A(JWGTAB)       WGRP                                
         DC    AL1(5,2,0,0),A(JWCTAB)       WORK                                
         DC    X'FF'                                                            
         SPACE 2                                                                
* TABLE OF SECURITY TYPES AND THEIR RESPECTIVE AUTHORIZATION MASKS              
*                                                                               
TYPETAB  DS    0CL3                                                             
         DC    C'A',AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q)                    
         DC    AL1(CAT1Q)                                                       
*                                                                               
         DC    C'B',AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q)                    
         DC    AL1(CAT1Q+CAT3Q+CAT5Q)                                           
*                                                                               
         DC    C'C',AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q)                    
         DC    AL1(CAT1Q)                                                       
*                                                                               
         DC    C'D',AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q)                    
         DC    AL1(CAT1Q+CAT3Q+CAT5Q)                                           
*                                                                               
         DC    C'E',AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q)                    
         DC    AL1(CAT2Q)                                                       
*                                                                               
         DC    C'F',AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q)                    
         DC    AL1(CAT2Q+CAT4Q)                                                 
*                                                                               
TYPES    EQU   (*-TYPETAB)/L'TYPETAB                                            
         EJECT                                                                  
       ++INCLUDE ACOPTLIST                                                      
         EJECT                                                                  
*ACPROWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACPROWORKD                                                     
         PRINT ON                                                               
*DDSPOOLD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
*DDSPLWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
*ACGENBOTH                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
         EJECT                                                                  
* DSECT TO COVER LOCAL WORKING STORAGE                                          
*                                                                               
SUBSYSD  DSECT                                                                  
         ORG   DRGEN               RE-USE DRONEBLK                              
LOCAL    DS    0C                  LOCAL WORKING STORAGE                        
SAVERE   DS    A                                                                
INTMODE  DS    X                   INTERNAL MODE                                
OPTKEY   DS    CL(L'ACOPKEY)                                                    
OPTOG    DS    CL1                                                              
OPTOFF   DS    CL2                                                              
OPTCLI   DS    CL6                                                              
OPTPROD  DS    CL6                                                              
OPTJOB   DS    CL6                                                              
OPTMG    DS    CL1                                                              
OPTMED   DS    CL1                                                              
OPTWG    DS    CL1                                                              
OPTWORK  DS    CL2                                                              
OPTACT   DS    C                   ACTIVITY OPTION                              
OPTFILTS DS    0CL8                                                             
OPTEST   DS    C                   Y=ESTIMATE OPTIONS                           
OPTBILL  DS    C                   Y=BILLING OPTIONS                            
OPTFILE  DS    C                   Y=FILE MAINTENANCE OPTIONS                   
OPTSTUD  DS    C                   Y=STUDIO OPTIONS                             
OPTNBIL  DS    C                   Y=NEW BILLING OPTIONS                        
OPTORD   DS    C                   Y=ORDER OPTIONS                              
         DS    CL(L'OPTFILTS-(*-OPTFILTS)) SPARE                                
SCROLL   DS    X                                                                
         SPACE 1                                                                
NOPTIONS DS    H                   N'OPTION ENTRIES IN LIST                     
AOPTLIST DS    A                   A(OPTION LIST FOR LEVEL)                     
APOINT   DS    A                                                                
         SPACE 1                                                                
ADDSW    DS    C                                                                
UPDATE   DS    C                                                                
         SPACE 1                                                                
MEDKEY   DS    CL42                                                             
KEYLEVEL DS    CL1                                                              
KEYCHG   DS    C                                                                
MEDLEVEL DS    CL1                                                              
WRKLEVEL DS    CL1                                                              
FROMLEV  DS    CL2                                                              
FROMDISP DS    H                                                                
FROMMW   DS    CL2                                                              
FROMLEVN DS    XL1                                                              
         SPACE 1                                                                
OPTLIST  DS    100CL(OPTLISTL)     OPTION LIST                                  
LOCALLN  EQU   *-LOCAL                                                          
         EJECT                                                                  
T60BFFD  DSECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE ACPROF4D                                                       
         SPACE 2                                                                
SVSTART  DS    0F                                                               
SVNOPTS  DS    F                   N'OPTIONS ON SCREEN                          
SVFIRST  DS    X                   FIRST ENTRY ON SCREEN                        
SVLAST   DS    X                   LAST ENTRY ON SCREEN                         
SVTABLE  DS    XL(MAXOPTS*OPTLISTL)                                             
         SPACE 1                                                                
SVELEMSL DS    H                                                                
SVELEMS  DS    CL512                                                            
         DS    CL((SAVAREA-SVSTART)-(*-SVSTART))  SPARE                         
         SPACE 2                                                                
* DSECT TO COVER SAVE ELEMENTS                                                  
*                                                                               
SVELD    DSECT                                                                  
SVELCODE DS    X                   ELEMENT CODE = X'A4'                         
SVELLEN  DS    X                   ELEMENT LENGTH = L'SVELDATA+3                
SVELOPN  DS    X                   OPTION NUMBER                                
SVELDATA DS    0C                  OPTION DATA                                  
         SPACE 2                                                                
* EQUATES                                                                       
*                                                                               
DISPLIN  EQU   PROFOP2H-PROFOPTH                                                
MAXOPTS  EQU   (PROPFH-PROFOPTH)/DISPLIN                                        
POINTLN  EQU   ((8*54)+1)                                                       
FSTSCR   EQU   1                   FIRST SCREEN                                 
NEXTSCR  EQU   2                   NEXT SCREEN                                  
EDTSCR   EQU   3                   EDIT SCREEN                                  
CTRYANY  EQU   0                                                                
CTRYNOT  EQU   X'80'                                                            
         SPACE 2                                                                
       ++INCLUDE DDCTRYEQUS                                                     
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'048ACPRO04   02/11/21'                                      
         END                                                                    
