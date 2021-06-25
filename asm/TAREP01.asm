*          DATA SET TAREP01    AT LEVEL 123 AS OF 09/16/14                      
*PHASE T70301C                                                                  
         TITLE 'T70301 - TALENT WRITER APPLICATION'                             
T70301   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 750,T70301                                                       
         ST    RC,AGRABUFF         GRAB A 6K BUFFER IN STORAGE                  
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          SYSTEM SPECIFIC WORK                         
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         LA    R7,BUFF                                                          
         LA    R7,8(R7)                                                         
         USING TRWD,R7             R7=A(LOCAL W/S)                              
*                                                                               
         GOTO1 INITIAL,DMCB,0                                                   
*                                                                               
MODE1    CLI   MODE,DISPREC                                                     
         BNE   MODE2                                                            
         BAS   RE,SETTWA                                                        
         CLI   ACTEQU,19           LIMIT ACTION?                                
         BNE   XIT                 NO, LEAVE                                    
         LA    R2,WLMUIDH                                                       
         BRAS  RE,DSPLMUI          DISPLAY LIMIT USER ID                        
         OI    WLMUIDH+4,X'20'     VALIDATED PREVIOUSLY                         
         B     WRLMDIS                                                          
*                                                                               
MODE2    CLI   MODE,VALREC                                                      
         BNE   MODE4                                                            
         BAS   RE,VREC                                                          
         B     XIT                                                              
         SPACE 1                                                                
MODE4    CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
         BAS   RE,PREP                                                          
         B     XIT                                                              
         EJECT                                                                  
*              SET INPUT BITS IN TWA ON DISPREC                                 
         SPACE 3                                                                
SETTWA   NTR1                                                                   
         LA    R2,SPLREADH                                                      
         LA    RF,SPLPERH                                                       
*                                                                               
SETTWA2  CLI   0(R2),0             END OF TWA?                                  
         BE    XIT                                                              
         USING FLDHDRD,R2                                                       
         CLI   FLDILEN,0           IF ANY INPUT LENGTH                          
         BE    SETTWA3                                                          
         CR    R2,RF               AT PERIOD                                    
         BE    SETTWA3             DON'T MARK PREV VALIDATED                    
         OI    FLDIIND,X'20'          SET PREVIOUSLY VALIDATED                  
SETTWA3  ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         B     SETTWA2                                                          
         EJECT                                                                  
*              VALIDATE RECORD                                                  
         SPACE 3                                                                
VREC     NTR1                                                                   
         NI    GENSTAT3,X'FF'-DIEONERR  DON'T DIE ON ERRORS OFFLINE             
                                                                                
         CLI   ACTEQU,19           LIMIT                                        
         BE    VREC500                                                          
                                                                                
         XC    ACODEFLD,ACODEFLD                                                
         LA    R2,SPLCODEH                                                      
         CLI   5(R2),0             IF CODE FIELD INPUT                          
         BE    *+8                                                              
         ST    R2,ACODEFLD         SAVE ADDRESS OF FIELD                        
         SPACE 1                                                                
         LA    R2,SPLOPTH          VALIDATE OPTIONS BEFORE OTHERS               
         GOTO1 VALOPTS                                                          
         CLI   TIQDTYPE,0          IF OPTION PER= HAS BEEN USED                 
         BE    VREC1                                                            
         LA    R2,SPLPERH                                                       
         CLI   5(R2),0             AND IF THERE IS NO PERIOD INPUT              
         BE    MISSERR             EXIT WITH ERROR MESSAGE                      
         SPACE 1                                                                
VREC1    TM    DOWNOPT,GLDLACTV    IF WE ARE DOWNLOADING                        
         BNO   VREC2                                                            
         CLI   CONOUT,C' '            AND OUTPUT TYPE NOT REQUESTED             
         BH    VREC2                                                            
         MVC   CONOUT(8),=CL8'DOWN'   DEFAULT TO OUTPUT OF 'DOWN'               
         OI    CONOUTH+6,X'80'                                                  
         MVC   TWAOUT,CONOUT                                                    
         SPACE 1                                                                
VREC2    GOTO1 INITDRON            INITIALIZE DRONE                             
         LAY   R1,READTAB                                                       
         LA    R2,SPLREADH                                                      
         GOTO1 ANY                                                              
         ZIC   R3,5(R2)                                                         
         BCTR  R3,0                                                             
         SPACE 1                                                                
VREC4    CLI   0(R1),X'FF'         LOOK UP RECORDS TO BE READ                   
         BE    INVERR                                                           
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R1),8(R2)                                                    
         BE    VREC6                                                            
         LA    R1,10(R1)                                                        
         B     VREC4                                                            
         SPACE 1                                                                
VREC6    MVC   READRECS,8(R1)                                                   
         MVC   READSUB,9(R1)                                                    
         MVC   8(8,R2),0(R1)       EXPAND                                       
         OI    6(R2),X'80'                                                      
         MVC   TIREAD,READRECS     SET SYSIO'S READ                             
*                                  (NEED BEFORE VALFILT CALL)                   
*                                                                               
         CLC   =CL8'T4',8(R2)                                                   
         BNE   *+8                                                              
         MVI   TIRDSUBT,TLT4SCDQ                                                
                                                                                
         CLC   =CL8'RL1',8(R2)                                                  
         BNE   *+8                                                              
         MVI   TIRDSUBT,TLR1SCDQ                                                
                                                                                
         CLI   TIREAD,TLCKCDQ      READING CHECKS?                              
         BNE   *+16                                                             
         CLI   SPLPERF,C'@'        YES,IS THIS AN FLIST?                        
         BNE   *+8                                                              
         MVI   TIREAD,TLCKECDQ     YES, READ EMPLOYER CHECKS                    
         EJECT                                                                  
*              SPECIFIC FILTER FIELDS                                           
         SPACE 3                                                                
*                                  OPTIONAL FIELDS                              
         LA    R2,SPLOFFH          OFFICE                                       
         LA    R3,TIFOFF                                                        
         LA    R4,L'TIFOFF                                                      
         LA    R5,TLOFCDQ                                                       
         BAS   RE,SPECFILT                                                      
         SPACE 1                                                                
         LA    R2,SPLEMPH          EMPLOYER                                     
         LA    R3,TIFEMP                                                        
         LA    R4,L'TIFEMP                                                      
         LA    R5,TLEMCDQ                                                       
         BAS   RE,SPECFILT                                                      
         SPACE 1                                                                
         LA    R2,SPLAGYH          AGENCY                                       
         LA    R3,TIFAGY                                                        
         LA    R4,L'TIFAGY                                                      
         LA    R5,TLAYCDQ                                                       
         BAS   RE,SPECFILT                                                      
         SPACE 1                                                                
         LA    R2,SPLCLIH          CLIENT                                       
         LA    R3,TIFCLI                                                        
         LA    R4,L'TIFCLI                                                      
         LA    R5,TLCLCDQ                                                       
         BAS   RE,SPECFILT                                                      
         SPACE 1                                                                
         LA    R2,SPLPROH          PRODUCT                                      
         LA    R3,TIFPRD                                                        
         LA    R4,L'TIFPRD                                                      
         LA    R5,TLPRCDQ                                                       
         BAS   RE,SPECFILT                                                      
         SPACE 1                                                                
         LA    R2,SPLISCIH         COMMERCIAL ID                                
         LA    R3,TIFCID                                                        
         LA    R4,L'TIFCID                                                      
         LA    R5,TLCOICDQ                                                      
         BAS   RE,SPECFILT                                                      
         CLI   5(R2),0             IF FILTER INPUT                              
         BE    FILTCOMX                                                         
         TM    0(R3),X'C0'         AND NOT NEGATIVE AND/OR FLIST                
         BNO   FILTCOMX                                                         
         MVC   TIFCOM,KEY+TLCOICOM-TLCOPD  SET INT. COMM AS WELL                
         CLC   =C'CKFRCINV',SPLREAD                                             
         BNE   FILTCOMX                                                         
         OI    TISTAT,TISTSPCI                                                  
FILTCOMX DS    0H                                                               
         SPACE 1                                                                
         LA    R2,SPLAGGH          AGENCY GROUP                                 
         LA    R3,TIFAGG                                                        
         LA    R4,L'TIFAGG                                                      
         LA    R5,TLAGCDQ                                                       
         BAS   RE,SPECFILT                                                      
         SPACE 1                                                                
         LA    R2,SPLUNH           UNION                                        
         LA    R3,TIFUN                                                         
         LA    R4,L'TIFUN                                                       
         LA    R5,1                                                             
         BAS   RE,SPECFILT                                                      
         OI    TIFLTLVL,TIFLVLCA+TIFLVLCK  MATCH AT CAST & CHECK REC            
         SPACE 1                                                                
         LA    R2,SPLUSEH          USE CODE                                     
         LA    R3,TIFUSE                                                        
         LA    R4,L'TIFUSE                                                      
         LA    R5,2                                                             
         BAS   RE,SPECFILT                                                      
*                                                                               
         LA    R2,SPLPERFH         PERFORMER                                    
         LA    R3,TIFSSN                                                        
         LA    R4,L'TIFSSN                                                      
         LA    R5,TLW4CDQ                                                       
         CLI   8(R2),C'@'          MAKE SURE 1ST CHAR FOR PID / SSN             
         BNH   VREC9Z              ELSE IT'S FOR FLIST                          
*                                                                               
         TM    TGSYSTAT,TASYSPID                                                
         BZ    VREC9Z                                                           
         CLI   5(R2),0                                                          
         BE    VREC10                                                           
         CLI   5(R2),6             USER ENTERED MORE THAN PID, SSN              
         BH    VREC8                                                            
*                                                                               
         MVC   TGPID,8(R2)                                                      
         GOTO1 SSNUNPK,DMCB,TGPID,TGSSN                                         
         B     VREC9                                                            
*                                                                               
VREC8    CLI   5(R2),9                      CONVERT SSN TO PID                  
         BNE   BADFILT                                                          
         MVC   TGSSN,8(R2)                  CONVERT SSN TO PID                  
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
*                                                                               
VREC9    MVC   8(L'SPLPERF,R2),SPACES                                           
         MVC   8(L'TGPID,R2),TGPID                                              
         MVI   5(R2),6                                                          
         OI    6(R2),X'80'                                                      
*                                                                               
         GOTO1 RECVAL,DMCB,(R5),(X'84',TGSSN),0                                 
         BNE   BADFILT                                                          
         MVC   TIFSSN,TGSSN                                                     
*                                                                               
         B     VREC10                                                           
*                                                                               
VREC9Z   BAS   RE,SPECFILT                                                      
         B     VREC10                                                           
         EJECT                                                                  
*              VALIDATE A FILTER EXPRESSION                                     
         SPACE 3                                                                
*              INPUT               R2=A(HEADER)                                 
*                                  R3=A(SYSIO FILTER AREA)                      
*                                  R4=L'ABOVE                                   
*                                  R5=RECORD TYPE CODE                          
         SPACE 1                                                                
SPECFILT NTR1                                                                   
         CLI   5(R2),0             ANY DATA                                     
         BE    XIT                                                              
         OI    6(R2),X'80'                                                      
         GOTO1 ANY                 PUT INTO WORK                                
         BCTR  R4,0                (L'FILTER - 1)                               
         CLC   WORK(2),=C'-@'      CHECK FOR NEGATIVE LIST                      
         BE    NEGLIST                                                          
         CLC   WORK(2),=C'@-'                                                   
         BE    NEGLIST                                                          
         CLI   WORK,C'-'           CHECK FOR NEGATIVE FILTER                    
         BE    NEGFILT                                                          
         CLI   WORK,C'@'           CHECK FOR POSITIVE LIST                      
         BE    POSLIST                                                          
         SPACE 1                                                                
POSFILT  EX    R4,*+8              POSITIVE FILTER                              
         B     *+10                                                             
         MVC   0(0,R3),WORK                                                     
         LA    R4,WORK                                                          
         B     ALLFILT                                                          
         SPACE 1                                                                
NEGFILT  EX    R4,*+8              NEGATIVE FILTER                              
         B     *+10                                                             
         MVC   0(0,R3),WORK+1                                                   
         NI    0(R3),X'FF'-X'40'                                                
         LA    R4,WORK+1                                                        
         SPACE 1                                                                
ALLFILT  LTR   R5,R5                                                            
         BZ    XIT                                                              
         CH    R5,=H'1'                                                         
         BE    ALLUNVL                                                          
         CH    R5,=H'2'                                                         
         BE    ALLUSEVL                                                         
         GOTO1 RECVAL,DMCB,(R5),(X'80',(R4)),0                                  
         BNE   BADFILT                                                          
         B     XIT                                                              
         SPACE 1                                                                
ALLUNVL  GOTO1 UNIVAL,DMCB,(R4)                                                 
         BNE   BADFILT                                                          
         B     XIT                                                              
         SPACE 1                                                                
ALLUSEVL GOTO1 USEVAL,DMCB,(X'40',(R4))                                         
         BNE   BADFILT                                                          
         B     XIT                                                              
         SPACE 1                                                                
POSLIST  EX    R4,*+8              POSITIVE LIST                                
         B     *+10                                                             
         MVC   0(0,R3),WORK+1                                                   
         NI    0(R3),X'FF'-X'80'                                                
         LA    R5,WORK+1                                                        
         B     VALLIST                                                          
         SPACE 1                                                                
NEGLIST  EX    R4,*+8              NEGATIVE LIST                                
         B     *+10                                                             
         MVC   0(0,R3),WORK+2                                                   
         NI    0(R3),X'FF'-X'80'-X'40'                                          
         LA    R5,WORK+2                                                        
         SPACE 1                                                                
VALLIST  LA    R1,1(R5,R4)         CHECK CODE IS NOT TOO LONG                   
         CLI   0(R1),C' '                                                       
         BH    BADLONG                                                          
         XC    KEY,KEY             CHECK LIST EXISTS                            
         LA    R3,KEY                                                           
         USING TLGLD,R3                                                         
         MVI   TLGLCD,TLGLCDQ                                                   
         MVI   TLGLTYPE,TLGLTYPF                                                
         MVC   TLGLLST,0(R5)                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(32),KEYSAVE                                                  
         BNE   BADLIST                                                          
         B     XIT                                                              
         DROP  R3                                                               
         SPACE 1                                                                
BADFILT  LAY   RF,FLTERR                                                        
         MVC   CONHEAD(L'FLTERR),0(RF)                                          
         B     BADXIT                                                           
         SPACE 1                                                                
BADLIST  LAY   RF,LSTERR                                                        
         MVC   CONHEAD(L'LSTERR),0(RF)                                          
         B     BADXIT                                                           
         SPACE 1                                                                
BADLONG  LAY   RF,LNGERR                                                        
         MVC   CONHEAD(L'LNGERR),0(RF)                                          
         B     BADXIT                                                           
         EJECT                                                                  
*              VALIDATE REST OF SCREEN                                          
         SPACE 3                                                                
VREC10   LA    R2,SPLFILTH         OPTIONAL FILTERS                             
         GOTO1 VALFILT                                                          
                                                                                
         LA    R2,SPLPERH          PERIOD EXPRESSION                            
         BRAS  RE,VSFTDAT          VALIDATE USING SOFDAT                        
         BE    VREC200             IF NOT GOOD,                                 
         GOTO1 VALPERD             DO IT THE OLD WAY                            
*                                                                               
VREC200  TM    TWAWHEN,X'04'       OVERNIGHT?                                   
         BO    VREC300             YES, NO LIMIT TESTS                          
*                                                                               
         BRAS  RE,SOONOK           SEE IF SOON REQUEST OK                       
         BNE   BADXIT                                                           
*                                                                               
VREC300  LA    R2,SPLLEFTH         LEFT HEADERS                                 
         MVI   MAX,4                                                            
         GOTO1 VALLEFT                                                          
         LA    R2,SPLRGHTH         RIGHT HEADERS                                
         MVI   MAX,3                                                            
         GOTO1 VALRIGHT                                                         
         LA    R2,SPLMIDH          MID LINE                                     
         MVI   MAX,1                                                            
         GOTO1 VALMID                                                           
         LA    R2,SPLROWSH         ROWS                                         
         MVI   MAX,8                                                            
         GOTO1 VALROWS                                                          
         LA    R2,SPLCOLSH         COLUMNS                                      
         MVI   MAX,16                                                           
         GOTO1 VALCOLS                                                          
         LA    R2,SPLTITLH         USER TITLES                                  
         GOTO1 VALTITS                                                          
         GOTO1 WRAPDRON                                                         
         SPACE 1                                                                
         B     XIT                                                              
         EJECT                                                                  
VREC500  DS    0H                                                               
         LA    R2,WLMUIDH                                                       
         TM    WLMUIDH+4,X'20'     VALIDATED PREVIOUSLY?                        
         BO    WRLMDIS             YES, LEAVE                                   
         CLI   WLMUIDH+5,0         ANY USERIDS?                                 
         BNE   VREC600             YES, GO VALIDATE THEM                        
         XC    LIMUSR,LIMUSR                                                    
         BRAS  RE,UPDLMUI          UPDATE LIMIT USER ID ELEMENT                 
         B     WRLMUPD             GOOD ONE, CONTINUE                           
                                                                                
VREC600  MVC   AIO,AIO3                                                         
         XC    LIMUSRID,LIMUSRID                                                
         ZIC   R3,WLMUIDH+5                                                     
         AHI   R3,-1                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   LIMUSRID(0),WLMUID                                               
         OC    LIMUSRID,SPACES                                                  
                                                                                
         GOTO1 USERVAL,DMCB,(X'A0',LIMUSRID),0                                  
         MVC   AIO,AIO1                                                         
         BNE   USRIERR             INVALID USERID                               
         OI    WLMUIDH+4,X'20'     VALIDATED PREVIOUSLY                         
         MVC   LIMUSR,TGUSER                                                    
         BRAS  RE,UPDLMUI          UPDATE LIMIT USER ID ELEMENT                 
         B     WRLMUPD                                                          
                                                                                
WRLMDIS  DS    0H                                                               
         MVC   CONHEAD(22),=C'** WRITER DISPLAYED **'                           
         B     MESSXIT                                                          
                                                                                
WRLMUPD  DS    0H                                                               
         MVC   CONHEAD(20),=C'** WRITER UPDATED **'                             
         B     MESSXIT                                                          
                                                                                
USRIERR  MVC   CONHEAD(26),=C'** ERROR ** INVALID USERID'                       
MESSXIT  MVI   ERROR,X'FE'                                                      
         GOTO1 ERRXIT                                                           
                                                                                
INVERR   MVI   ERROR,INVALID                                                    
         GOTO1 ERRXIT                                                           
MISSERR  MVI   ERROR,MISSING                                                    
         GOTO1 ERRXIT                                                           
         EJECT                                                                  
*              PRINT REPORT                                                     
         SPACE 1                                                                
PREP     NTR1                                                                   
         GOTO1 INITDRIV            INITIALIZE DRIVER                            
         L     R4,AGLOBAL                                                       
         LTR   R4,R4               (HAPPENS SOMETIMES ON LINE)                  
         BZ    XIT                 (WE SHOULDN'T REALLY BE HERE)                
         USING GLOBALD,R4                                                       
         GOTO1 INITDRIV            RE-INITIALIZE DRIVER                         
         LA    R1,HOOK             APPLICATION HOOK                             
         ST    R1,GLAHOOK                                                       
         MVI   GLMODE,GLINIT                                                    
         GOTO1 DRIVER,DMCB,(R4)                                                 
         SPACE 1                                                                
         MVC   TIACOMFC,ACOMFACS   SET UP FOR TASYSIO                           
         LA    R1,IOHOOK                                                        
         ST    R1,TIHOOK                                                        
         MVC   TIACCS,TWAACCS      PASS THROUGH LIMIT ACCESS                    
         MVC   TIAUTH,TWAAUTH                   AND AUTHORIZATION               
         MVC   TIUSERID,TWAORIG                 AND REQUESTING ID#              
         CLI   TGCTSTTY,TASTTYPC   IF STAFF TYPES C OR D                        
         BE    *+12                                                             
         CLI   TGCTSTTY,TASTTYPD                                                
         BNE   *+10                                                             
         MVC   TIQSTAFF,TGSTAF     SET STAFF CODE FOR AGY/CLI LIMIT CHK         
         MVC   TIABUFF,AGRABUFF                 A(OPTIMIZATION BUFFER)          
         MVC   TILBUFF,=F'6000'                 AND ITS LENGTH                  
         MVC   TIACOLFL,ACOLFILT                A(COLFILT)                      
         LA    R1,TGD                                                           
         ST    R1,TIATGLOB                      A(TALENT GLOBALS)               
         MVC   TISUBRD,READSUB                  POSSIBLE SUBREAD                
         SPACE 1                                                                
         NI    TISTAT,X'FF'-TISTSPSC                                            
         CLC   SPLREAD,=C'INFRCCOM' IF WE ARE FORCING INV VIA COMML             
         BE    PREP6                                                            
         CLC   SPLREAD,=C'CKFRCCOM' OR WE ARE FORCING CKS VIA COMML             
         BE    PREP6                                                            
         CLC   SPLREAD,=C'CKFRCAST' OR WE ARE FORCING CKS VIA CAST              
         BE    PREP6                                                            
         CLC   SPLREAD(4),=C'DEAL'  OR READING DEALS                            
         BNE   PREP7                SET UP FOR SPECIAL                          
PREP6    OI    TISTAT,TISTSPSC      HANDLING FOR COMM'L SUBREAD                 
         SPACE 1                                                                
PREP7    CLC   SPLREAD,=C'INFRCCOM' IF WE ARE FORCING INV VIA COMML             
*        BE    *+10                                                             
*        CLC   SPLREAD,=C'CKFRCCOM' OR WE ARE FORCING CKS VIA COMML             
         BNE   PREP8                                                            
         OI    TIQFLAG3,TIQFFCOM   FORCE COMML INFO FROM COMML LVL ONLY         
         OC    TIFCLG,TIFCLG       AND IF CLIENT GROUP SPECIFIED                
         BZ    PREP8                                                            
         CLC   SPLREAD,=C'INFRCCOM' IF WE ARE FORCING INV VIA COMML             
         BE    PREP8                                                            
         MVI   TIREAD,TLCOGCDQ     READ CLIENT GROUP COMML PASSIVE PTR.         
PREP8    XC    LASTW2KY,LASTW2KY   INITIALIZE W2 CONTROL                        
         OC    TIFCOM,TIFCOM       IF INT. COMML NUMBER                         
         BZ    PREP10                                                           
         OC    TIFAGY,TIFAGY       AND AGENCY CODE SPECIFIED                    
         BZ    PREP10                                                           
         TM    TIFAGY,X'C0'        (NEGATIVE AND/OR FLIST)                      
         BO    PREP10                                                           
         XC    TIFAGY,TIFAGY       CLEAR TO GET TRANSFERS                       
         SPACE 1                                                                
PREP10   BRAS  RE,BLDUFLST         BUILD UNIT FLIST                             
                                                                                
PREP30   BRAS  RE,GETFFLST         IF FLIST AND MULTIPLE REQS BETTER            
         BE    *+12                REPLACES FLIST W/1ST CODE OF FLIST           
*                                  ELSE IF 1ST CODE IS INVALID                  
PREP40   BAS   RE,GETNFLST         GETS NEXT CODE IN FLIST                      
         BNE   PREP50                                                           
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
         B     PREP40                                                           
         SPACE 1                                                                
PREP50   MVI   GLMODE,GLOUTPUT     THEN PRINT THE REPORTS                       
         GOTO1 DRIVER,DMCB,(R4)                                                 
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO CHECK IF ANOTHER REQUEST IS NECESSARY                 
*                                  YES - NECESSARY 'TIF' FIELD IS SET           
         SPACE 1                                                                
         USING TAGLD,R6            R6=A(CURRENT TAGLD ELEMENT)                  
GETNFLST NTR1                                                                   
         LTR   R6,R6               IF R6 POINTING TO AN ELEMENT                 
         BZ    NO                                                               
         MVI   ELCODE,TAGLELQ      RESET ELEMENT CODE                           
GETNF10  BAS   RE,NEXTEL           GET NEXT ONE                                 
         BNE   NO                                                               
         L     R3,FLSTNTRY         R3=A(THIS FLIST ENTRY)                       
         USING FLISTABD,R3                                                      
         ZIC   R1,FLSTVALL         R1=(LENGTH OF CODE-1)                        
         ICM   R2,15,FLSTVAL       R2=A(CODE)                                   
         LA    R2,TIFILTS(R2)      R2=A(CODE)                                   
*                                                                               
         BAS   RE,CLCCODE          IF CODE IS EQUAL TO PREVIOUS                 
         BE    GETNF10             SKIP IT                                      
         BAS   RE,CLRCODE          ELSE, CLEAR FIELD                            
         BAS   RE,SETCODE          SET FIELD WITH NEXT CODE IN FLIST            
         BNE   GETNF10             IF INVALID, SKIP IT                          
         B     YESR6               XIT WITH CC EQUAL AND R6 SET                 
         DROP  R6,R3                                                            
         EJECT                                                                  
*              ROUTINE TO CHECK CODE PROCESSED SAME AS NEXT IN FLIST            
         USING TAGLD,R6                                                         
CLCCODE  DS    0H                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),TAGLDATA                                                 
         BR    RE                                                               
         SPACE 2                                                                
*              ROUTINE TO CLEAR FLIST CODE                                      
CLRCODE  DS    0H                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    0(0,R2),0(R2)                                                    
         BR    RE                                                               
         SPACE 2                                                                
*              ROUTINE TO SET CODE AND RETURNS CC NOT EQ IF INVALID             
*              ELSE SETS CC EQ                                                  
         SPACE                                                                  
         USING TAGLD,R6                                                         
SETCODE  NTR1                                                                   
         ZIC   R1,TAGLLEN                                                       
         SH    R1,=Y(TAGLLNQ+1)                                                 
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),TAGLDATA                                                 
         LA    R1,TIFINVD          IF HANDLING TIFINVD                          
         CR    R1,R2                                                            
         BNE   YES                                                              
         GOTO1 WILDINV             HANDLE WILDCARDS & CONV TO INTERNAL          
         BNE   NO                                                               
         B     YES                                                              
         EJECT                                                                  
IOHOOK   NTR1                                                                   
         L     RF,TIAREC                                                        
         CLI   0(RF),TLCKCDQ       PROCESSING CHECK?                            
         BNE   *+8                                                              
         BRAS  RE,TATUDUE          UPDATE TATU'S W/ DUE COMPANY (P+)            
*                                                                               
         CLI   TIMODE,PROCREC      IF THERE IS SOMETHING INTERESTING            
         BE    IOHOOK2                                                          
         CLI   TIMODE,PROCINV                                                   
         BE    IOHOOK2                                                          
         CLI   TIMODE,PROCCOMM                                                  
         BE    IOHOOK2                                                          
         B     XIT                                                              
         SPACE 1                                                                
IOHOOK2  BAS   RE,SUBCON           MAY CONTROL AT SUB RECORD LEVEL              
         B     XIT                                                              
         EJECT                                                                  
*=====================================================================          
*              SUB RECORD CONTROL                                               
*=====================================================================          
SUBCON   NTR1                                                                   
         CLI   TIMODE,PROCINV      IF INVOICE HOOK                              
         BNE   SUBCON1                                                          
         TM    TIQFLAG2,TIQFEXCT   AND IF EXACT FILTERING REQUESTED             
         BZ    SUBCON4                                                          
         LA    R1,GLAINTD                                                       
         L     RE,0(R1)            RE=A(FIRST INTERNAL DETAILS REC)             
         USING GLINTD,RE                                                        
         TM    GLRECIND,GLADINRC   IF NO ADDITIVE FIELDS (LISTING RPT)          
         BO    SUBCON4                                                          
         B     XIT                 DON'T HOOK WITH INVOICE                      
         DROP  RE                                                               
         SPACE 1                                                                
SUBCON1  L     R6,TIAREC           FOR DETAIL, MUST MATCH ON RECORD             
         CLC   READRECS,0(R6)                                                   
         BE    SUBCON2                                                          
         CLC   READSUB,0(R6)                                                    
         BE    SUBCON2                                                          
         CLC   READRECS,TIKEY      OR KEY                                       
         BE    SUBCON2                                                          
         CLC   SPLREAD,=C'INFRCCOM' IF WE ARE FORCING INV VIA COMML             
         BNE   SUBCON1A                                                         
         CLI   0(R6),TLINCDQ        TAKE ANY INVOICE RECORDS                    
         BNE   XIT                                                              
         B     SUBCON2                                                          
*                                                                               
SUBCON1A CLC   SPLREAD,=C'CKFRCCOM' IF WE ARE FORCING INV VIA COMML             
         BNE   XIT                                                              
*                                                                               
SUBCON2  CLI   0(R6),TLCKCDQ       CHECKS NEED ELEMENT CONTROL                  
         BE    ELCON                                                            
         CLI   0(R6),TLCACDQ       SO DO CASTS FOR TACRS                        
         BE    ELCON                                                            
         CLI   0(R6),TLGUCDQ       SO DO GUARANTEES                             
         BE    ELCON                                                            
         CLI   0(R6),TLT4CDQ       SO DO T4S                                    
         BE    ELCON                                                            
         CLI   0(R6),TLW2CDQ       SO DO W2S                                    
         BE    SUBCON6                                                          
         SPACE 1                                                                
SUBCON4  MVI   GLMODE,GLINPUT                                                   
         XC    ATHISEL,ATHISEL     (NO SUB CONTROL)                             
         BRAS  RE,RECTRACE                                                      
         CLI   READRECS,TLCKCDQ                                                 
         BNE   SUBCON4A                                                         
         CLI   READSUB,0                                                        
         BNE   SUBCON4A                                                         
         L     R6,TIAREC                                                        
         CLI   0(R6),TLCKCDQ                                                    
         BNE   XIT                                                              
SUBCON4A GOTO1 DRIVER,DMCB,(R4)                                                 
         B     XIT                                                              
         SPACE 1                                                                
SUBCON6  CLC   LASTW2KY,0(R6)      ONLY PROCESS FIRST (LATEST) W2               
         BE    XIT                                                              
         MVC   LASTW2KY,0(R6)                                                   
         B     ELCON                                                            
         SPACE 1                                                                
*ASTW2KY DS    CL29                W2 KEY WITHOUT ACTIVITY DATE                 
         EJECT                                                                  
*              ELEMENT LEVEL CONTROLLER                                         
         SPACE 3                                                                
ELCON    MVI   ELCODE,0                                                         
         MVI   ANYFED,C'N'                                                      
         MVI   ANYTATU,C'N'                                                     
         BRAS  RE,GETEL                                                         
         B     *+12                                                             
ELCON2   MVI   ELCODE,0                                                         
         BRAS  RE,NEXTEL                                                        
         BNE   ELCONX                                                           
         SPACE 1                                                                
         CLC   SPLREAD(4),=C'DEAL' IF WE ARE READING DEALS                      
         BNE   ELCON4                                                           
         CLI   0(R6),TADLELQ       THEN ONLY INTERESTED IN DEALS ELS            
         BNE   ELCON2                                                           
         B     ELCON6                                                           
         SPACE 1                                                                
ELCON4   CLC   SPLREAD(3),=C'ECH'  IF WE ARE READING ECHECKS                    
         BNE   ELCON6                                                           
         CLI   0(R6),TASOELQ       THEN ONLY INTERESTED IN SOAP ELS             
         BNE   ELCON2                                                           
         SPACE 1                                                                
ELCON6   CLI   0(R6),TACWELQ                                                    
         BE    ELCONCW                                                          
         CLI   0(R6),TACYELQ                                                    
         BE    ELCONCY                                                          
         CLI   0(R6),TADLELQ                                                    
         BE    ELCONDL                                                          
         CLI   0(R6),TAPDELQ                                                    
         BE    ELCONPD                                                          
         CLI   0(R6),TAW2ELQ                                                    
         BE    ELCONW2                                                          
         CLI   0(R6),TASOELQ                                                    
         BE    ELCONSO                                                          
         CLI   0(R6),TATUELQ                                                    
         BE    ELCONTU                                                          
         CLI   0(R6),TAT4ELQ                                                    
         BE    ELCONT4                                                          
         CLI   0(R6),TAR1ELQ                                                    
         BE    ELCONR1                                                          
         CLI   0(R6),TAATELQ                                                    
         BE    ELCONAT                                                          
         B     ELCONGO                                                          
         SPACE 1                                                                
         USING TACWD,R6                                                         
ELCONCW  MVC   TIUNIT,TACWUNIT                                                  
         BRAS  RE,SRCUFLST         SEE IF TIUNIT IN FLIST                       
         BNE   ELCON2              NO, LEAVE                                    
ELCONCW0 CLC   TIUNIT(2),=C'FD'                                                 
         BNE   *+8                                                              
         MVI   ANYFED,C'Y'                                                      
         CLI   TIFUNIT,C'A'        MAY FILTER TAX UNIT                          
         BL    ELCONGO                                                          
*                                                                               
         CLC   TIFUNIT,TIUNIT      IF STATE/CITY FILTER MATCH                   
         BE    ELCONGO                PROCESS ELEMENT                           
*                                                                               
         CLI   TIFUNIT+2,C' '      IF FILTERING ON STATE                        
         BH    ELCONCW1                                                         
*                                                                               
         CLI   TIUNIT+2,C' '       AND ELEMENT IS FOR A CITY                    
         BNH   ELCONCW9                                                         
*                                                                               
         GOTOR FNDST,DMCB,TIUNIT,DUB    FIND CITY'S STATE                       
*                                                                               
         CLC   TIFUNIT,DUB         IF STATE FILTER MATCHES                      
         BE    ELCONGO                PROCESS ELEMENT                           
         B     ELCONCW9                                                         
*                                                                               
ELCONCW1 DS    0H                  FILTERING ON CITY                            
*                                                                               
         CLI   TIFUNIT+2,C' '      IF FILTERING ON CITY                         
         BNH   ELCONCW2                                                         
*                                                                               
         CLI   TIUNIT+2,C' '       AND ELEMENT IS FOR A STATE                   
         BH    ELCONCW9                                                         
*                                                                               
         GOTOR FNDST,DMCB,TIFUNIT,DUB    FIND CITY'S STATE                      
*                                                                               
         CLC   TIUNIT,DUB          IF STATE FILTER MATCHES                      
         BE    ELCONGO                PROCESS ELEMENT                           
         B     ELCONCW9                                                         
*                                                                               
ELCONCW2 DS    0H                                                               
*                                                                               
ELCONCW9 DS    0H                                                               
         B     ELCON2              SKIP ELEMENT                                 
*                                                                               
         USING TATUD,R6                                                         
ELCONTU  DS    0H                                                               
         TM    TIFTUNIT,TIFTTAX    FILTERING BY TAX UNIT                        
         JO    ELCONCW0                                                         
         MVC   TIUNIT,TATUUNIT                                                  
         BRAS  RE,SRCUFLST         SEE IF TIUNIT IN FLIST                       
         BNE   ELCON2              NO, LEAVE                                    
         MVI   ANYTATU,C'Y'                                                     
         B     ELCONCW0                                                         
*                                                                               
         USING TAT4D,R6                                                         
ELCONT4  DS    0H                                                               
         TM    TIFTUNIT,TIFTTAX    FILTERING BY TAX UNIT                        
         JO    ELCONCW0                                                         
         MVC   TIUNIT,TAT4UNIT                                                  
         BRAS  RE,SRCUFLST         SEE IF TIUNIT IN FLIST                       
         BNE   ELCON2              NO, LEAVE                                    
         B     ELCONCW0                                                         
*                                                                               
         USING TAR1D,R6                                                         
ELCONR1  DS    0H                                                               
         TM    TIFTUNIT,TIFTTAX    FILTERING BY TAX UNIT                        
         JO    ELCONCW0                                                         
         MVC   TIUNIT,TAR1UNIT                                                  
         BRAS  RE,SRCUFLST         SEE IF TIUNIT IN FLIST                       
         BNE   ELCON2              NO, LEAVE                                    
         B     ELCONCW0                                                         
*                                                                               
         USING TAATD,R6                                                         
ELCONAT  DS    0H                                                               
         TM    TIFTUNIT,TIFTWRK    FILTERING BY WORK UNIT                       
         BO    ELCONCW0                                                         
         MVC   TIUNIT,TAATUNIT                                                  
         BRAS  RE,SRCUFLST         SEE IF TIUNIT IN FLIST                       
         BNE   ELCON2              NO, LEAVE                                    
         B     ELCONCW0                                                         
                                                                                
         USING TADLD,R6                                                         
ELCONDL  CLI   TIFAREA,0           IF FILTERING ON AREA                         
         BE    ELCONDL2                                                         
         CLC   TADLAREA,TIFAREA       DO IT NOW                                 
         BNE   ELCON2                                                           
         SPACE 1                                                                
ELCONDL2 CLI   TIFPUSE,0           IF FILTERING ON PRINT USE                    
         BE    ELCONDL4                                                         
         CLC   TADLUSE,TIFPUSE        DO IT NOW                                 
         BNE   ELCON2                                                           
         SPACE 1                                                                
ELCONDL4 CLI   TIQDTYPE,TIQDEXP    IF FILTERING ON EXPIRY DATE                  
         BNE   ELCONGO                                                          
         CLC   TADLEXP,TIQPSTR        DO IT NOW                                 
         BL    ELCON2                                                           
         CLC   TADLEXP,TIQPEND                                                  
         BH    ELCON2                                                           
         B     ELCONGO                                                          
         SPACE 1                                                                
         USING TAPDD,R6                                                         
ELCONPD  CLC   TIFUNIT,SPACES                                                   
         BNH   ELCONPD3                                                         
         CLI   ANYTATU,C'Y'                                                     
         BNE   ELCONPD3                                                         
         MVC   TIUNIT,TIFUNIT                                                   
                                                                                
ELCONPD3 CLI   TAPDW4TY,TAW4TYCO   UNLESS CANADIAN                              
         BE    ELCONPD5                                                         
         CLI   TAPDW4TY,TAW4TYCO   OR CORP                                      
         BE    ELCONPD5                                                         
         CLI   TAPDW4TY,TAW4TYTR   OR TRUSTEE                                   
         BNE   ELCONGO                                                          
ELCONPD5 MVI   ANYFED,C'Y'         PRETEND THAT WE SAW A FED ELEMENT            
         B     ELCONGO                                                          
         SPACE 1                                                                
         USING TACYD,R6                                                         
ELCONCY  MVC   TIUNIT,TACYUNIT     YTD - SET UNIT                               
         BRAS  RE,SRCUFLST         SEE IF TIUNIT IN FLIST                       
         BNE   ELCON2              NO, LEAVE                                    
         CLI   TIFUNIT,C'A'        MAY FILTER TAX UNIT                          
         BL    ELCONCY2                                                         
         CLC   TIFUNIT,TIUNIT                                                   
         BNE   ELCON2                                                           
         SPACE 1                                                                
ELCONCY2 LA    R1,TIKEY                                                         
         CLI   0(R1),TLCKYCDQ      IF WE ARE READING FROM YTD POINTERS          
         BNE   ELCONGO                                                          
         USING TLCKPKEY,R1                                                      
         CLC   TLCKYTXU,TACYUNIT   MATCH ON UNIT IN KEY                         
         BNE   ELCON2              TO UNIT IN THE KEY                           
         B     ELCONGO                                                          
         DROP  R1                                                               
         SPACE 1                                                                
         USING TAW2D,R6                                                         
ELCONW2  MVC   TIUNIT,TAW2UNIT     W2 - SET UNIT                                
         BRAS  RE,SRCUFLST         SEE IF TIUNIT IN FLIST                       
         BNE   ELCON2              NO, LEAVE                                    
         CLI   TIFUNIT,C'A'        MAY FILTER TAX UNIT                          
         BL    ELCONGO                                                          
         CLC   TIFUNIT,TIUNIT                                                   
         BNE   ELCON2                                                           
         B     ELCONGO                                                          
         SPACE 1                                                                
         USING TASOD,R6                                                         
ELCONSO  ZIC   R2,TASONUM          R2 = N'SUB'ELEMENTS                          
         LTR   R2,R2                                                            
         BZ    ELCON2                                                           
         LA    R3,TASOSEPI         R3 = A(SUB-ELEMENTS)                         
         USING TASOSEPI,R3                                                      
         SPACE 1                                                                
ELCONSO2 LH    R1,TASOEPI          EPISODE                                      
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  TIEPI,DUB+5(3)                                                   
         CLI   TIFEPI,C' '         MAY FILTER ON EPISODE                        
         BNH   *+14                                                             
         CLC   TIFEPI,TIEPI                                                     
         BNE   ELCONSO6                                                         
         SPACE 1                                                                
         LAY   RF,DUMTASO          BUILD DUMMY TASO EL. FOR SYSDRIVE            
         XC    0(L'DUMTASO,RF),0(RF)                                            
         MVI   TASOEL-TASOD(RF),TASOELQ                                         
         MVI   TASOLEN-TASOD(RF),L'DUMTASO                                      
         MVI   TASONUM-TASOD(RF),1                                              
         MVC   TASOSTAT-TASOD(L'TASOSTAT,RF),TASOSTAT                           
         MVC   TASOEPI-TASOD(L'TASOEPI,RF),TASOEPI                              
         MVC   TASOPNH-TASOD(L'TASOPNH,RF),TASOPNH                              
         MVC   TASOPAY-TASOD(L'TASOPAY,RF),TASOPAY                              
         MVC   TASOAPPL-TASOD(L'TASOAPPL,RF),TASOAPPL                           
         ST    RF,ATHISEL                                                       
         MVI   GLMODE,GLINPUT                                                   
         GOTO1 DRIVER,DMCB,(R4)                                                 
ELCONSO6 LA    R3,L'TASOSEPI(R3)   BUMP TO NEXT SUB-ELEMENT                     
         BCT   R2,ELCONSO2                                                      
         B     ELCON2                                                           
*                                                                               
ELCONGO  MVI   GLMODE,GLINPUT      YES SO OFF TO DRIVER                         
         ST    R6,ATHISEL                                                       
         GOTO1 DRIVER,DMCB,(R4)                                                 
         B     ELCON2                                                           
         SPACE 1                                                                
ELCONX   CLI   ANYFED,C'Y'         NEED TO DUMMY UP FED IF NONE                 
         BE    XIT                                                              
         L     R6,TIAREC           AND                                          
         CLI   0(R6),TLCKCDQ       IF WE ARE HANDLING CHECKS                    
         BNE   XIT                                                              
         LA    R6,DUMFED                                                        
         USING TACWD,R6                                                         
         MVC   TIUNIT,TACWUNIT                                                  
         CLI   TIFUNIT,C'A'        MAY FILTER TAX UNIT                          
         BL    ELCONXGO                                                         
         CLC   TIFUNIT,TIUNIT                                                   
         BNE   XIT                                                              
         SPACE 1                                                                
ELCONXGO MVI   GLMODE,GLINPUT                                                   
         ST    R6,ATHISEL                                                       
         GOTO1 DRIVER,DMCB,(R4)                                                 
         B     XIT                                                              
                                                                                
         SPACE 1                                                                
ANYFED   DC    C'N'                                                             
ANYTATU  DC    C'N'                                                             
DUMFED   DC    X'C2'               DUMMY FED WITHHOLDING ELEMENT                
         DC    AL1(32)                                                          
         DC    C'FD '                                                           
         DC    32X'00'                                                          
         EJECT                                                                  
         DROP  R4                                                               
*              HEADLINE HOOK (HEADHOOK)                                         
         SPACE 1                                                                
HOOK     NTR1                                                                   
         L     R2,AGLOBAL                                                       
         USING GLOBALD,R2                                                       
         CLI   GLHOOK,GLPRINT      ABOUT TO PRINT A LINE                        
         BNE   HK4                                                              
         CLI   NOPRINT,C'Y'        IF SWITCH IS SET                             
         BNE   HKX                                                              
         MVI   GLHOOK,GLDONT       SET TO SUPPRESS THIS LINE                    
         MVI   NOPRINT,C'N'        AND RESET SWITCH                             
         B     HKX                                                              
         SPACE 1                                                                
HK4      CLI   GLHOOK,GLHEAD       PROCESSING HEADINGS                          
         BNE   HKX                                                              
         GOTO1 GENHEAD             TAREPGEN WILL HANDLE                         
         SPACE 1                                                                
HKX      B     XIT                                                              
         DROP  R2                                                               
         SPACE 1                                                                
BADXIT   GOTO1 ERREX2                                                           
         SPACE 1                                                                
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 1                                                                
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 1                                                                
YESR6    SR    RC,RC                                                            
NOR6     LTR   RC,RC                                                            
XITR6    XIT1  REGS=(R6)                                                        
         EJECT                                                                  
*              LTORG AND TABLES                                                 
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 1                                                                
AGRABUFF DS    A                                                                
         SPACE 1                                                                
DUMTASO  DS    CL(TASOXSUB-TASOD)  DUMMY TASO EL.                               
         SPACE 1                                                                
FLTERR   DC    C'** ERROR ** RECORD NOT FOUND'                                  
LSTERR   DC    C'** ERROR ** MISSING FILTER LIST'                               
LNGERR   DC    C'** ERROR ** CODE TOO LONG'                                     
         SPACE 1                                                                
*              FLIST TABLE FOR MULTIPLE REQUESTS                                
         SPACE 1                                                                
FLISTAB  DS    0CL7                                                             
         DC    AL1(TLCKECDQ),AL1(0),AL1(L'TIFSSN-1),AL4(TIFSSN-TIFILTS)         
FLISTW4  DC    AL1(TLW4CDQ),AL1(0),AL1(L'TIFSSN-1),AL4(TIFSSN-TIFILTS)          
         DC    AL1(TLINCDQ),AL1(0),AL1(L'TIFINVD-1)                             
         DC    AL4(TIFINVD-TIFILTS)                                             
         DC    AL1(TLCKCDQ),AL1(0),AL1(L'TIFINVD-1)                             
         DC    AL4(TIFINVD-TIFILTS)                                             
         DC    AL1(TLCKHCDQ),AL1(0),AL1(L'TIFINVD-1)                            
         DC    AL4(TIFINVD-TIFILTS)                                             
         DC    X'FF'                                                            
         SPACE 1                                                                
READTAB  DS    0H                                                               
         DC    C'STAFF   ',AL1(TLSTCDQ),AL1(0)                                  
         DC    C'ST      ',AL1(TLSTCDQ),AL1(0)                                  
         DC    C'AGENCY  ',AL1(TLAYCDQ),AL1(0)                                  
         DC    C'AY      ',AL1(TLAYCDQ),AL1(0)                                  
         DC    C'AGROUP  ',AL1(TLAGCDQ),AL1(0)                                  
         DC    C'AG      ',AL1(TLAGCDQ),AL1(0)                                  
         DC    C'ATTN.   ',AL1(TLATCDQ),AL1(0)                                  
         DC    C'AT      ',AL1(TLATCDQ),AL1(0)                                  
         DC    C'CLIENT  ',AL1(TLCLCDQ),AL1(0)                                  
         DC    C'CL      ',AL1(TLCLCDQ),AL1(0)                                  
         DC    C'CGROUP  ',AL1(TLCGCDQ),AL1(0)                                  
         DC    C'CG      ',AL1(TLCGCDQ),AL1(0)                                  
         DC    C'PRODUCT ',AL1(TLPRCDQ),AL1(0)                                  
         DC    C'PR      ',AL1(TLPRCDQ),AL1(0)                                  
         DC    C'PGROUP  ',AL1(TLPGCDQ),AL1(0)                                  
         DC    C'PG      ',AL1(TLPGCDQ),AL1(0)                                  
         DC    C'COMM.   ',AL1(TLCOCDQ),AL1(0)                                  
         DC    C'CO      ',AL1(TLCOCDQ),AL1(0)                                  
         DC    C'COMUSIC ',AL1(TLCOMCDQ),AL1(0)                                 
         DC    C'MUSIC   ',AL1(TLMUCDQ),AL1(0)                                  
         DC    C'MU      ',AL1(TLMUCDQ),AL1(0)                                  
         DC    C'AGENT   ',AL1(TLANCDQ),AL1(0)                                  
         DC    C'AN      ',AL1(TLANCDQ),AL1(0)                                  
         DC    C'INTER   ',AL1(TLIFCDQ),AL1(0)                                  
         DC    C'IF      ',AL1(TLIFCDQ),AL1(0)                                  
         DC    C'EMPLOYER',AL1(TLEMCDQ),AL1(0)                                  
         DC    C'EM      ',AL1(TLEMCDQ),AL1(0)                                  
         DC    C'W4      ',AL1(TLW4CDQ),AL1(0)                                  
         DC    C'GUA.    ',AL1(TLGUCDQ),AL1(0)                                  
         DC    C'GU      ',AL1(TLGUCDQ),AL1(0)                                  
         DC    C'GTRACK  ',AL1(TLGTCDQ),AL1(0)                                  
         DC    C'GT      ',AL1(TLGTCDQ),AL1(0)                                  
         DC    C'DUECOMP ',AL1(TLDUCDQ),AL1(0)                                  
         DC    C'DU      ',AL1(TLDUCDQ),AL1(0)                                  
         DC    C'LIEN    ',AL1(TLLNCDQ),AL1(0)                                  
         DC    C'LIENS   ',AL1(TLLNCDQ),AL1(0)                                  
         DC    C'LN      ',AL1(TLLNCDQ),AL1(0)                                  
         DC    C'CAST    ',AL1(TLCACDQ),AL1(0)                                  
         DC    C'CA      ',AL1(TLCACDQ),AL1(0)                                  
         DC    C'INVOICE ',AL1(TLINCDQ),AL1(0)                                  
         DC    C'IN      ',AL1(TLINCDQ),AL1(0)                                  
         DC    C'INVOICES',AL1(TLINCDQ),AL1(0)                                  
         DC    C'INVDUE  ',AL1(TLINCCDQ),AL1(0)                                 
         DC    C'INFRCCOM',AL1(TLCOCDQ),AL1(TLINHCDQ) FORCE INV VIA COM         
         DC    C'INVSTAT ',AL1(TLINBCDQ),AL1(0)                                 
         DC    C'CKFRCCOM',AL1(TLCOCDQ),AL1(TLCKHCDQ) FORCE CKS VIA COM         
         DC    C'CKFRCINV',AL1(TLINDCDQ),AL1(TLCKCDQ) FORCE CKS VIA INV         
         DC    C'CKFRCAST',AL1(TLCACDQ),AL1(TLCKECDQ) FORCE CKS VIA CAS         
         DC    C'CHECK   ',AL1(TLCKCDQ),AL1(0)                                  
         DC    C'CH      ',AL1(TLCKCDQ),AL1(0)                                  
         DC    C'CHECKS  ',AL1(TLCKCDQ),AL1(0)                                  
         DC    C'CKY     ',AL1(TLCKYCDQ),AL1(0)                                 
         DC    C'CKE     ',AL1(TLCKECDQ),AL1(0)                                 
         DC    C'CKB     ',AL1(TLCKBCDQ),AL1(0)                                 
         DC    C'EST     ',AL1(TLESCDQ),AL1(0)                                  
         DC    C'ES      ',AL1(TLESCDQ),AL1(0)                                  
         DC    C'ADVICE  ',AL1(TLDVCDQ),AL1(0)                                  
         DC    C'FTRACK  ',AL1(TLFTCDQ),AL1(0)                                  
         DC    C'FT      ',AL1(TLFTCDQ),AL1(0)                                  
         DC    C'W2      ',AL1(TLW2CDQ),AL1(0)                                  
         DC    C'PCOMM   ',AL1(TLCOPCDQ),AL1(0)                                 
         DC    C'PCAST   ',AL1(TLCOPCDQ),AL1(TLCACDQ)                           
         DC    C'DEAL    ',AL1(TLCOPCDQ),AL1(TLCACDQ)                           
         DC    C'EC      ',AL1(TLECCDQ),AL1(0)                                  
         DC    C'ECAST   ',AL1(TLECCDQ),AL1(0)                                  
         DC    C'EP      ',AL1(TLEPCDQ),AL1(0)                                  
         DC    C'EPISODE ',AL1(TLEPCDQ),AL1(0)                                  
         DC    C'ECHECK  ',AL1(TLCKCDQ),AL1(0)                                  
         DC    C'HOLD    ',AL1(TLNXCDQ),AL1(0)                                  
         DC    C'T4      ',AL1(TLT4CDQ),AL1(0)                                  
         DC    C'RL1     ',AL1(TLR1CDQ),AL1(0)                                  
         DC    X'FF'                                                            
         EJECT                                                                  
*              ROUTINE CHECKS FOR FLIST AND IF NECESSARY TO BREAK               
*              REQUEST UP INTO MULTIPLE SYSIO REQUESTS.  SETS CC EQ IF          
*              1ST CODE IN FLIST IS INVALID, ELSE SETS CC EQ                    
         SPACE 1                                                                
GETFFLST NTR1  BASE=*,LABEL=*                                                   
         XR    R6,R6               SET MULTIPLE REQUESTS NOT NECESSARY          
         LAY   R3,FLISTAB                                                       
         USING FLISTABD,R3                                                      
GETFF5   CLI   0(R3),X'FF'         IF NOT END OF TABLE                          
         JE    GETFFX                                                           
         CLC   TIREAD,FLSTRD       AND READS MATCH                              
         JNE   GETFF7                                                           
         CLC   TISUBRD,FLSTSRD                                                  
         JE    GETFF8                                                           
GETFF7   LA    R3,L'FLISTAB(R3)                                                 
         J     GETFF5                                                           
*                                                                               
GETFF8   ST    R3,FLSTNTRY         SAVE A(THIS FLIST ENTRY)                     
*                                                                               
         ICM   R2,15,FLSTVAL                                                    
         LA    R2,TIFILTS(R2)      R2=A(CODE)                                   
         CLI   0(R2),C'*'          IF POSITIVE FLIST IS REQUESTED               
         JE    GETFFX                                                           
         CLI   0(R2),X'1C'                                                      
         JE    GETFFX                                                           
         TM    0(R2),X'80'                                                      
         JO    GETFFX                                                           
         TM    0(R2),X'40'                                                      
         JZ    GETFFX                                                           
         OI    0(R2),X'80'         TURN OFF FLIST BIT                           
         LA    R6,FLISTIO          SET IOAREA AND READ FLIST                    
         ST    R6,AIO                                                           
         MVC   TGLST,SPACES                                                     
         ZIC   R1,FLSTVALL                                                      
         EX    R1,*+8                                                           
         J     *+10                                                             
         MVC   TGLST(0),0(R2)                                                   
         MVI   TGLTYP,TLGLTYPF                                                  
         GOTO1 RECVAL,DMCB,TLGLCDQ,(X'A0',0)                                    
         MVC   AIO,AIO1                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R1,TIFINVD          SKIP CHKWILD IF FLIST IS FOR TIFINVD         
         CR    R1,R2                                                            
         JE    GETFF10                                                          
         BRAS  RE,CHKWILD          IF WILD CARDS IN FLIST                       
         JNE   GETFF10                                                          
         NI    0(R2),X'FF'-X'80'   RESET FLIST BIT                              
         XR    R6,R6               SET MULTIPLE REQUESTS NOT NECESSARY          
         J     GETFFX                                                           
*                                                                               
GETFF10  LAY   RE,FLISTW4          IF W4 READ                                   
         CR    R3,RE                                                            
         JNE   *+8                                                              
         OI    TIQFLAG3,TIQFLTW4   TELL SYSIO FILTERING W4 BY TIFSSN            
*                                                                               
         LA    R6,FLISTIO                                                       
         MVI   ELCODE,TAGLELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         USING TAGLD,R6                                                         
         ZIC   R1,FLSTVALL         R1=(LENGTH OF CODE-1)                        
         BAS   RE,CLRCOD3          CLEAR FIELD OF REQUESTED FLIST               
         BAS   RE,SETCOD3          SET FIELD WITH FIRST CODE IN FLIST           
         JNE   NOR6                IF INVALID, RETURN CC NOT EQ WITH R6         
GETFFX   J     YESR6               SET CC EQ AND RETURN WITH R6                 
*              ROUTINE TO CHECK CODE PROCESSED SAME AS NEXT IN FLIST            
         USING TAGLD,R6                                                         
CLCCOD3  DS    0H                                                               
         EX    R1,*+8                                                           
         J     *+10                                                             
         CLC   0(0,R2),TAGLDATA                                                 
         BR    RE                                                               
         SPACE 2                                                                
*              ROUTINE TO CLEAR FLIST CODE                                      
CLRCOD3  DS    0H                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    0(0,R2),0(R2)                                                    
         BR    RE                                                               
         SPACE 2                                                                
         DROP  R6,R3                                                            
         EJECT                                                                  
*              ROUTINE TO SET CODE AND RETURNS CC NOT EQ IF INVALID             
*              ELSE SETS CC EQ                                                  
         SPACE                                                                  
         USING TAGLD,R6                                                         
SETCOD3  NTR1                                                                   
         ZIC   R1,TAGLLEN                                                       
         SH    R1,=Y(TAGLLNQ+1)                                                 
         EX    R1,*+8                                                           
         J     *+10                                                             
         MVC   0(0,R2),TAGLDATA                                                 
         LA    R1,TIFINVD          IF HANDLING TIFINVD                          
         CR    R1,R2                                                            
         JNE   YES                                                              
         GOTO1 WILDINV             HANDLE WILDCARDS & CONV TO INTERNAL          
         JNE   NO                                                               
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE CHECKS NO WILD CHARDS IN FLIST ENTRIES                   
*                                  XIT CC NEQ - NO WILD CARDS                   
         SPACE 1                                                                
CHKWILD  NTR1  BASE=*,LABEL=*                                                   
         LA    R6,FLISTIO                                                       
         MVI   ELCODE,TAGLELQ                                                   
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
CHKWILD2 BRAS  RE,NEXTEL                                                        
         JNE   NO                                                               
*                                                                               
         USING TAGLD,R6                                                         
         ZIC   R1,TAGLLEN                                                       
         SH    R1,=Y(TAGLLNQ)                                                   
         LA    R2,TAGLDATA                                                      
*                                                                               
CHKWILD5 CLI   0(R2),C'*'          IF WILDCARD                                  
         JE    YES                                                              
         CLI   0(R2),X'1C'         OR NEGATIVE WILDCARD                         
         JE    YES                                                              
         LA    R2,1(R2)                                                         
         BCT   R1,CHKWILD5                                                      
         J     CHKWILD2                                                         
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================          
*              VALIDATE USING SOFT DATES                                        
*              R2 = FIELD HEADER                                                
*=====================================================================          
         USING SOFDATD,R1                                                       
VSFTDAT  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,SDBLOCK                                                       
         XC    SOFDATD(SOFDATL),SOFDATD                                         
         ST    R2,SOFAINP          A(INPUT)                                     
         LA    R3,OUTDATE                                                       
         ST    R3,SOFAOUT          A(OUTPUT)                                    
         MVC   SOFACOM,ACOMFACS    A(COMFACS)                                   
         MVI   SOFITYPE,SOFITYMD   VALIDATE FOR YEAR, MONTH, DAY                
         MVI   SOFOTYPE,SOFOTSD2   12 BYTE EBCIDIC (YYMMDDYYMMDD)               
         MVI   SOFIINDS,SOFIISFT   VALIDATE ONLY SOFT DATES                     
*                                                                               
         MVC   SOFTODAY,TGTODAY0   TODAY'S DATE                                 
         MVC   SOFCTRY,CTRY        COUNTRY CODE                                 
         MVC   SOFLANG,LANG        LANGUAGE CODE                                
         MVI   SOFSYSN,7           TALENT SYSTEM                                
         GOTO1 SOFTDATE,SOFDATD                                                 
         JZ    VSFTDAT3                                                         
* CHECK FOR YEAR, MONTH VALIDATION                                              
         MVI   SOFITYPE,SOFITYM    VALIDATE FOR YEAR, MONTH THEN                
         GOTO1 SOFTDATE,SOFDATD                                                 
         JZ    VSFTDAT3                                                         
         MVC   ERROR,SOFERROR                                                   
         J     NO                                                               
*                                                                               
VSFTDAT3 CLI   OFFLINE,C'Y'                                                     
         JE    VSFTDAT5                                                         
         CLI   CONOUTH+4,0         RLP ONLINE, IF GROUP NAME IN OUTPUT          
         JE    VSFTDAT5                                                         
         CLC   =C'FILE',CONDEST    AND FILE IN DESTINATION                      
         JE    YES                 DON'T RESOLVE DATES                          
*                                                                               
VSFTDAT5 OI    SOFIINDS,SOFIIRES   RESOLVE THE DATES TO ACTUAL                  
         GOTO1 SOFTDATE,SOFDATD                                                 
         GOTO1 DATCON,DMCB,(0,OUTDATE),(1,TIQPSTR)                              
         GOTO1 DATCON,DMCB,(0,OUTDATE+6),(1,TIQPEND)                            
         J     YES                                                              
*                                                                               
         DROP  R1                                                               
         EJECT                                                                  
*=====================================================================          
*              DISPLAY LIMIT USER ID ELEMENT                                    
*=====================================================================          
         USING CT01ORGD,R6                                                      
DSPLMUI  NTR1  BASE=*,LABEL=*                                                   
         L     R6,AIO                                                           
         MVI   ELCODE,CT01UCDQ     X'05'                                        
         MVC   DATADISP,=H'28'     CONTROL'S DATADISP                           
         BRAS  RE,GETEL                                                         
         JNE   XIT                 NO USER ID TO DISPLAY                        
                                                                                
         XC    WORK(10),WORK       CALL USERVAL TO GET ALPHA USER ID            
         MVC   WORK+8(2),CT01UORG                                               
         MVC   AIO,AIO3                                                         
         GOTO1 USERVAL,DMCB,(X'80',WORK)                                        
         MVC   AIO,AIO1                                                         
         MVC   WLMUID(6),TGUSERID                                               
         OI    WLMUIDH+6,X'80'                                                  
         MVC   DATADISP,=H'40'     RESTORE TALENT'S DATADISP                    
         J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================          
*              UPDATE LIMIT USER ID ELEMENT                                     
*=====================================================================          
UPDLMUI  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         GOTO1 SWITCH,DMCB,(X'FF',X'FFFFFFFF'),0                                
         L     R1,0(R1)                                                         
         XR    RF,RF                                                            
         IC    RF,TSYS-UTLD(R1)                                                 
         SRA   RF,4                                                             
         STC   RF,TGTALNUM         SAVE TALENT SYSTEM NUMBER (TAL?)             
                                                                                
         MVI   DMCB,X'0A'          CONTROL SYSTEM                               
         MVC   DMCB+1(3),=X'FFFFFF'                                             
         GOTO1 SWITCH,DMCB,,0                                                   
         CLI   4(R1),0             RETURN CC                                    
         BE    *+6                                                              
         DC    H'0'                DIE ON ERRORS                                
                                                                                
         MVC   SYSFIL,=CL8'CTFILE'                                              
         XC    ELEMENT,ELEMENT                                                  
         XC    KEY,KEY                                                          
                                                                                
         L     R6,AIO                                                           
         MVC   KEY(28),0(R6)       KEY OF THE WRITER REPORT                     
         GOTOR DATAMGR,DMCB,=C'DMREAD',=C'CTFILE',KEY,AIO1                      
                                                                                
         MVI   ELCODE,CT01UCDQ     X'05'                                        
         MVC   DATADISP,=H'28'     CONTROL'S DATADISP                           
         BRAS  RE,GETEL                                                         
         JNE   ULMUI30             NO LIMIT USER ID ELEMENT                     
         MVI   CT01UCD,X'FF'       REMOVE OLD ONE                               
                                                                                
         L     R6,AIO                                                           
         MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM                                                          
                                                                                
ULMUI30  OC    LIMUSR,LIMUSR       NO LIMIT USER ID                             
         BZ    ULMUI50             WRITE RECORD BACK                            
                                                                                
         LA    R6,ELEMENT                                                       
         MVI   CT01UCD,CT01UCDQ    X'05'                                        
         MVI   CT01ULEN,CT01ULNQ                                                
         MVC   CT01UORG,LIMUSR     USER ID NUMBER                               
                                                                                
         L     R6,AIO                                                           
         GOTO1 ADDELEM             ADD ELEMENT TO RECORD                        
                                                                                
ULMUI50  GOTOR DATAMGR,DMCB,=C'DMWRT',=C'CTFILE',KEY,AIO1                       
                                                                                
         ZIC   RF,TGTALNUM         TALENT SYSTEM NUMBER (TAL?)                  
         SLA   RF,4                                                             
                                                                                
         STC   RF,DMCB             SWITCH BACK TO TALENT                        
         MVC   DMCB+1(3),=X'FFFFFF'                                             
         GOTO1 SWITCH,DMCB,,0                                                   
         CLI   4(R1),0             RETURN CC                                    
         BE    *+6                                                              
         DC    H'0'                DIE ON ERRORS                                
                                                                                
         MVC   DATADISP,=H'40'     RESTORE TALENT'S DATADISP                    
*        MVC   FILENAME,=CL8'TALFILE'                                           
         MVC   SYSFIL,=CL8'TALFILE'                                             
         J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================          
*              UPDATE P+ TATU'S W/ DUE COMPANY                                  
*=====================================================================          
TATUDUE  NTR1  BASE=*,LABEL=*                                                   
         USING TADWD,R6                                                         
         L     R6,TIAREC                                                        
         MVI   ELCODE,TADWELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   TATUDUEX                                                         
*                                                                               
         USING TATUD,R6                                                         
         L     R6,TIAREC                                                        
         MVI   ELCODE,TATUELQ                                                   
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
TDUE10   BRAS  RE,NEXTEL                                                        
         JNE   TATUDUEX                                                         
         MVC   TATUWAGE,TATUWAAD   UPDATE WAGES WITH WAGES AFTER DC             
         J     TDUE10                                                           
*                                                                               
TATUDUEX J     XIT                                                              
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================          
*              BUILD UNIT FLIST?                                                
*=====================================================================          
BLDUFLST NTR1  BASE=*,LABEL=*                                                   
         OC    TIFUNIT,TIFUNIT     IF NO UNIT FILTER, LEAVE                     
         JZ    XIT                                                              
         TM    TIFUNIT,X'80'       OR NOT FLIST, LEAVE                          
         JO    XIT                                                              
                                                                                
         XC    UNITFLST,UNITFLST                                                
                                                                                
         L     R4,AIO              SAVE A(AIO)                                  
         MVC   AIO,AIO3                                                         
                                                                                
         LA    R6,KEY              NOW GO AND READ THIS LIST                    
         XC    KEY,KEY                                                          
         USING TLGLD,R6                                                         
         MVI   TLGLCD,TLGLCDQ                                                   
         MVI   TLGLTYPE,TLGLTYPF   (TYPE=FILTER)                                
         MVC   TLGLLST,TIFUNIT                                                  
         OI    TLGLLST,X'80'                                                    
         OC    TLGLLST,SPACES                                                   
                                                                                
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(32),KEYSAVE                                                  
         JNE   BLDUXIT                                                          
         GOTO1 GETREC                                                           
*                                                                               
         USING TAGLD,R6                                                         
         LA    R5,UNITFLST                                                      
         L     R6,AIO                                                           
         MVI   ELCODE,TAGLELQ      FIND ALL THE FLIST DATA                      
         BRAS  RE,GETEL                                                         
         B     BLDUF55                                                          
                                                                                
BLDUF50  BRAS  RE,NEXTEL                                                        
BLDUF55  BNE   BLDUXIT                                                          
         MVC   0(3,R5),SPACES      SPACE PADDED                                 
         SR    R1,R1                                                            
         IC    R1,TAGLLEN                                                       
         AHI   R1,-4                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),TAGLDATA                                                 
         AHI   R5,3                                                             
         B     BLDUF50             NEXT ONE                                     
*                                                                               
BLDUXIT  ST    R4,AIO              RESTORE A(AIO)                               
         J     XIT                                                              
                                                                                
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================          
*              SEARCH UNIT FLIST                                                
*=====================================================================          
SRCUFLST NTR1  BASE=*,LABEL=*                                                   
         OC    TIFUNIT,TIFUNIT     IF NO UNIT FILTER, LEAVE                     
         JZ    YES                                                              
         TM    TIFUNIT,X'80'       OR NOT FLIST, LEAVE                          
         JO    YES                                                              
                                                                                
         LA    R5,UNITFLST         SAVE A(AIO)                                  
SRCUF10  CLI   0(R5),0             EOT?                                         
         JE    NO                                                               
         CLC   TIUNIT,0(R5)                                                     
         JE    YES                                                              
         CLI   TIUNIT+2,C' '                                                    
         BNH   SRCUF20                                                          
         GOTOR FNDST,DMCB,TIUNIT,DUB    FIND CITY'S STATE                       
         CLC   DUB(3),0(R5)                                                     
         JE    YES                                                              
SRCUF20  AHI   R5,3                                                             
         J     SRCUF10                                                          
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================          
*              TRACE OPTIONS                                                    
*=====================================================================          
         USING GLOBALD,R4                                                       
RECTRACE NTR1  BASE=*,LABEL=*                                                   
                                                                                
         CLI   GLTRACE,C'Y'                                                     
         JNE   XIT                                                              
         L     R6,TIAREC                                                        
         MVC   P(32),0(R6)                                                      
         OC    P(32),SPACES                                                     
         GOTO1 HEXOUT,DMCB,(R6),P+33,36,=C'TOG'  KEY/STATUS/LEN                 
         MVC   P+107(2),=C'DA'                                                  
         GOTO1 HEXOUT,DMCB,TIDSKADD,P+110,4,=C'TOG'    D/A                      
         GOTO1 SPOOL,DMCB,(R8)                                                  
         J     XIT                                                              
                                                                                
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================          
* SOON OK: TEST IF SOON REQUEST IS OK                                           
*=====================================================================          
SOONOK   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   SPLPERH+5,0        PERIOD?                                       
         BZ    SOONOK10                                                         
         CLC   SPLPER(3),=C'ANY'  THIS CAN SLOW SYSTEM DOWN TOO                 
         BNE   SOONOK40                                                         
*                                                                               
SOONOK10 ZIC   R3,SPLREADH+5                                                    
         BCTR  R3,0                                                             
         LA    RF,PERREADS        SEE IF READ IS PERIOD DEPENDENT               
SOONOK20 CLI   0(RF),X'FF'        EOT?                                          
         BE    SOONOK40           YES, OK TO CONTINUE                           
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RF),SPLREAD    READ IS PERIOD DEPENDENT                      
         BE    SOONOK30           ERROR                                         
         AHI   RF,L'PERREADS                                                    
         B     SOONOK20                                                         
*                                                                               
SOONOK30 LA    R2,SPLPERH         POINT TO PERIOD                               
         MVC   CONHEAD(L'SERRPERI),SERRPERI                                     
         B     SOONOKNO                                                         
*                                                                               
SOONOK40 GOTO1 DATCON,DMCB,(1,TIQPSTR),(0,OUTDATE)                              
         GOTO1 DATCON,DMCB,(1,TIQPEND),(0,OUTDATE+6)                            
*                                                                               
         CLI   SPLPERH+5,0        PERIOD?                                       
         BZ    SOONOK50           NO, CATCH ERROR LATER                         
*                                                                               
         GOTO1 ADDAY,DMCB,(C'M',OUTDATE),MO1DATE,1                              
         GOTO1 ADDAY,DMCB,(C'M',OUTDATE),MO3DATE,3                              
         GOTO1 ADDAY,DMCB,(C'M',OUTDATE),MO6DATE,6                              
         GOTO1 ADDAY,DMCB,(C'Y',OUTDATE),YR1DATE,1                              
SOONOK50 CLI   TIREAD,TLCKCDQ      ARE WE READING CHECKS?                       
         BE    SOONOK60                                                         
         CLI   TISUBRD,TLCKCDQ                                                  
         BE    SOONOK60                                                         
         CLI   TIREAD,TLINCDQ      ARE WE READING INVOICES?                     
         BE    SOONOK80                                                         
         B     SOONOKYS            NO, THE REST IS OK                           
*                                                                               
SOONOK60 CLI   SPLPERH+5,0        PERIOD?                                       
         BNE   SOONOK70           YES, CHECK RANGE OF DATES                     
         CLI   SPLPERFH+5,0       PERFORMER?                                    
         BNE   SOONOKYS           YES, OK TO PROCEED WITH SOON                  
         LA    R2,SPLPERH         POINT TO PERIOD                               
         MVC   CONHEAD(L'SERRPERD),SERRPERD                                     
         B     SOONOKNO                                                         
*                                                                               
SOONOK70 LA    R2,SPLPERH                                                       
         CLC   SPLPER(3),=C'ANY'  THIS CAN SLOW SYSTEM DOWN TOO                 
         BE    SOONOK75                                                         
*                                                                               
         CLC   OUTDATE+6(6),MO3DATE                 <  3 MONTH, OK              
         BNH   SOONOK95                                                         
SOONOK75 CLI   SPLPERFH+5,0       PERFORMER?                                    
         BNE   SOONOKYS           YES, OK TO PROCEED WITH SOON                  
         MVC   CONHEAD(L'SERR3MOS),SERR3MOS                                     
         B     SOONOKNO                                                         
*                                                                               
SOONOK80 CLI   SPLPERH+5,0        PERIOD?                                       
         BE    SOONLONG           NO, MAKE IT LONG                              
         B     SOONOK90           YES, CHECK RANGE OF DATES                     
*                                                                               
SOONLONG MVI   REQSML,C'L'        LONG JOB                                      
         B     SOONOKYS                                                         
*                                                                               
SOONOK90 LA    R2,SPLPERH                                                       
         CLC   OUTDATE+6(6),MO6DATE     < 6 MOS, MEDIUM                         
         BNH   SOONOKYS                                                         
         CLC   OUTDATE+6(6),MO6DATE     > 1 YEAR, LONG                          
         BNL   SOONLONG                                                         
SOONOK95 CLI   SPLAGYH+5,0        6MOS - 1 YEAR, ANY FILTERS?  THEN OK          
         BNE   SOONOKYS                                                         
         CLI   SPLCLIH+5,0                                                      
         BNE   SOONOKYS                                                         
         CLI   SPLPROH+5,0                                                      
         BNE   SOONOKYS                                                         
         CLI   SPLISCIH+5,0                                                     
         BNE   SOONOKYS                                                         
         CLI   SPLPERFH+5,0                                                     
         BNE   SOONOKYS                                                         
*                                                                               
         B     SOONLONG           NO FILTERS, LONG JOB                          
*                                                                               
SOONOKNO LTR   RB,RB                                                            
         B     SOONOKX                                                          
SOONOKYS CR    RB,RB                                                            
SOONOKX  XIT1  REGS=(R2)                                                        
*                                                                               
SERRPERD DC    C'** SOON ERROR ** NO PERIOD, ENTER PERFORMER'                   
SERRPERI DC    C'** SOON ERROR ** PERIOD INVALID, ENTER VALID DATE RANGX        
               E'                                                               
SERR3MOS DC    C'** SOON ERROR ** PERIOD > 3 MONTHS, ENTER PERFORMER'           
                                                                                
PERREADS DS    0CL8            READS THAT REQUIRE PERIOD FOR SOON               
         DC    C'INVOICE '                                                      
         DC    C'IN      '                                                      
         DC    C'INVOICES'                                                      
         DC    C'INVDUE  '                                                      
         DC    C'INFRCCOM' FORCE INV VIA COM                                    
         DC    C'INVSTAT '                                                      
         DC    C'CKFRCCOM' FORCE CKS VIA COM                                    
         DC    C'CKFRCINV' FORCE CKS VIA INV                                    
         DC    C'CKFRCAST' FORCE CKS VIA CAS                                    
         DC    C'CHECK   '                                                      
         DC    C'CH      '                                                      
         DC    C'CHECKS  '                                                      
         DC    C'CKY     '                                                      
         DC    C'CKE     '                                                      
         DC    C'CKB     '                                                      
         DC    X'FF'                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        FIND STATE FOR CITY                                          *         
*                                                                     *         
*                                                                     *         
*NTRY    PARM0 ==>  CITY CODE                                         *         
*        PARM1 ==>  OUTPUT - CL3 FOR STATE CODE                       *         
*                   X'000000' IF STATE NOT FOUND                      *         
*                                                                     *         
***********************************************************************         
*                                                                               
FNDST    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R2,0(R1)            POINT TO CITY CODE                           
         L     R3,4(R1)            POINT TO OUTPUT AREA                         
*                                                                               
         XC    0(L'CTYSTST,R3),0(R3)       INIT OUTPUT                          
*                                                                               
         LA    R4,CTYSTTAB         POINT TO CITY/STATE TABLE                    
         USING CTYSTTAB,R4         ESTABLISH TABLE ENTRY                        
*                                                                               
FSTLOOP  DS    0H                                                               
*                                                                               
         CLC   CTYSTCTY,=X'FFFFFF' DONE AT END OF TABLE                         
         BE    FSTDONE                                                          
*                                                                               
         CLC   CTYSTCTY,0(R2)      MATCH CITY TO TABLE ENTRY                    
         BE    FSTFND                MATCH FOUND                                
*                                                                               
FSTCONT  DS    0H                                                               
         LA    R4,CTYSTCTY+CTYSTLNQ   BUMP TO NEXT TABLE ENTRY                  
         B     FSTLOOP                                                          
*                                                                               
FSTFND   DS    0H                                                               
*                                                                               
         MVC   0(L'CTYSTST,R3),CTYSTST   RETURN STATE CODE                      
*                                                                               
FSTDONE  DS    0H                                                               
*                                                                               
FNDSTX   DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R4                                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        TABLE LISTING CITY AND STATE                                 *         
*                                                                     *         
***********************************************************************         
*                                                                               
CTYSTTAB DS    0D                  TABLE OF CITIES AND THEIR STATES             
CTYSTCTY DC    CL3'NYC'            NEW YORK                                     
CTYSTST  DC    CL3'NY '            NY                                           
CTYSTLNQ EQU   *-CTYSTTAB          LENGTH OF ENTRY IN TABLE                     
*                                                                               
         DC    CL3'YON',CL3'NY '   YONKERS      NY                              
         DC    CL3'PHL',CL3'PA '   PHILADELPIA  PA                              
         DC    CL3'PIT',CL3'PA '   PITTSBUR     PA                              
         DC    CL3'CIN',CL3'OH '   CINCINNATI   OH                              
         DC    CL3'CLV',CL3'OH '   CLEVLAND     OH                              
         DC    CL3'DNV',CL3'CO '   DENVER       CO                              
         DC    CL3'LJC',CL3'KY '   LEXINGTON    KY                              
         DC    CL3'DET',CL3'MI '   DETROIT      MI                              
         DC    CL3'LAX',CL3'CA '   LOS ANGELES  CA                              
*                                                                               
         DC    X'FFFFFF'           EOT                                          
*                                                                               
         EJECT                                                                  
*                                                                               
         SPACE 1                                                                
TRWD     DSECT                                                                  
         DS    0A                                                               
         DS    XL50                                                             
MYDUB    DS    D                                                                
SDBLOCK  DS    CL(SOFXTNL)         SOFTDATE BLOCK                               
OUTDATE  DS    CL12                                                             
CLCDATE  DS    CL6                                                              
MO1DATE  DS    CL6                                                              
MO3DATE  DS    CL6                                                              
MO6DATE  DS    CL6                                                              
YR1DATE  DS    CL6                                                              
UNITFLST DS    CL200               AREA FOR UNIT FLIST EXPANDED                 
LASTW2KY DS    CL29                W2 KEY WITHOUT ACTIVITY DATE                 
LIMUSRID DS    CL10                LIMIT USER ID                                
LIMUSR   DS    XL2                 LIMIT USER NUMBER                            
FLSTNTRY DS    A                   A(CURRENT FLIST ENTRY)                       
*                                                                               
FLISTIO  DS    CL4000              AREA FOR FLIST RECORD                        
         SPACE 2                                                                
*              DSECT FOR FLISTAB                                                
         SPACE 1                                                                
FLISTABD DSECT                                                                  
FLSTRD   DS    XL1                 SYSIO READ                                   
FLSTSRD  DS    XL1                 SYSIO SUBREAD                                
FLSTVALL DS    XL1                 FLIST VALUE LENGTH                           
FLSTVAL  DS    XL4                 DISPLACEMENT TO FLIST CODE                   
         EJECT                                                                  
       ++INCLUDE TAREPWORKD                                                     
         EJECT                                                                  
*              OTHER DSECTS ARE HIDDEN IN HERE                                  
         SPACE 3                                                                
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*TAGENFILE                                                                      
*CTGENFILE                                                                      
*FLDHDRD                                                                        
*DRGLOBAL                                                                       
*DRINTRECD                                                                      
*FAFACTS                                                                        
*FATIOB                                                                         
*TAREPFFD                                                                       
*TASOFDATD                                                                      
*FAGETTXTD                                                                      
*FAUTL                                                                          
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE FLDHDRD                                                        
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE DRINTRECD                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FATIOB                                                         
       ++INCLUDE DDSOFDATD                                                      
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE FAUTL                                                          
       ++INCLUDE TAREPFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE TAREPF1D                                                       
         SPACE 1                                                                
READRECS DS    XL1                                                              
READSUB  DS    XL1                                                              
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE TAREPC8D                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'123TAREP01   09/16/14'                                      
         END                                                                    
