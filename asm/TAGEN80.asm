*          DATA SET TAGEN80    AT LEVEL 045 AS OF 05/29/15                      
*PHASE T70280E,*                                                                
         TITLE 'T70280 - CAST COPY'                                             
T70280   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70280,R6                                                      
         L     RC,0(R1)            RC=CONTROLLER STORAGE AREA                   
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL STORAGE AREA                        
         USING SPOOLD,R8                                                        
         LA    R7,TWAHOLE          R7=WORKING STORAGE                           
         USING LWSD,R7                                                          
         EJECT                                                                  
*              MODE CONTROL                                                     
         SPACE 2                                                                
         GOTO1 INITIAL,DMCB,0      INITIALIZE                                   
         SPACE 2                                                                
         CLI   SCASCST,C'Y'        IF SELECTING CAST - CONTINUE                 
         BE    M100                                                             
         CLI   MODE,VALKEY         VALIDATE SCREEN                              
         BNE   M20                                                              
         BAS   RE,VKEY             VALIDATE KEY FIELDS                          
         B     XIT                                                              
         SPACE 1                                                                
M20      CLI   MODE,VALREC         COPY RECORDS                                 
         BNE   XIT                                                              
         BAS   RE,DIRSCAN          SCAN CAST DIRECTORY OF NEW COMML             
         BAS   RE,COPY                                                          
         B     XIT                                                              
         SPACE 1                                                                
M100     GOTO1 FLDVAL,DMCB,(X'40',AFRSTKEY),SCAFFC                              
         BE    M110                                                             
         SPACE 1                                                                
         BAS   RE,VKEY             VALIDATE KEY FIELDS                          
         GOTO1 FLDVAL,DMCB,(X'20',AFRSTKEY),SCAFFC                              
         BAS   RE,DIRSCAN          SCAN CAST DIRECTORY OF NEW COMML             
         SPACE 1                                                                
M110     CLI   LSTPAGE,C'Y'        IF LAST PAGE DISPLAYED                       
         BE    M130                                                             
         CLI   LISTED,C'Y'         IF A PAGE WAS ALREADY LISTED                 
         BNE   M120                                                             
         BAS   RE,SELECTC          THEN SELECT FROM IT                          
         BAS   RE,OUTSEL           OUTPUT N'CAST MEMBERS SELECTED               
         SPACE 1                                                                
M120     BAS   RE,COPY             LIST CAST                                    
         OC    TIQSKEY,TIQSKEY     IF END OF LIST                               
         BNZ   SELINF                                                           
         B     LSTINF              GIVE LAST PAGE MESSAGE                       
         SPACE 1                                                                
M130     BAS   RE,SELECTC          THEN SELECT FROM IT                          
         BAS   RE,ADDSEL           COPY SELECTED MEMBERS TO NEW COMML           
         BRAS  RE,CLRSCR           CLEAR BOTTOM OF SCREEN                       
         BAS   RE,OUTSEL           OUTPUT N'CAST MEMBERS SELECTED               
         BRAS  RE,UPDCOMML         UPDATE NEW COMMERCIAL                        
         NI    SCASCSTH+4,X'DF'    UNVALIDATE A FIELD                           
         B     COPIED                                                           
         SPACE 1                                                                
MX       B     XIT                                                              
         EJECT                                                                  
*              VALIDATE KEY                                                     
         SPACE 1                                                                
VKEY     NTR1                                                                   
         LA    RE,LOCALD           CLEAR STORAGE                                
         LHI   RF,LOCALLNQ                                                      
         XCEFL                                                                  
         XC    NCOPIED,NCOPIED                                                  
         XC    NDUPS,NDUPS                                                      
*                                                                               
         BRAS  RE,CLRSCR                                                        
         XC    NSEL,NSEL           CLEAR SCREEN & STORAGE                       
         MVI   LSTPAGE,C'N'                                                     
         MVI   LISTED,C'N'                                                      
         SPACE 1                                                                
         XC    TIQSKEY,TIQSKEY     CLEAR CONTINUE KEY TO START OVER             
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'20',SCAFAYH)        FROM AGY              
         MVC   TIAGY,TGAGY         SAVE FOR SYSIO                               
         GOTO1 RAVPPLSA,DMCB,0     SEE IF REC / ACT INVALID FOR P+              
         JNE   ERPPLSI                                                          
*                                                                               
*** GET TAAYSTA6 VIA GETEL CALL                                                 
                                                                                
         L     R4,AIO                                                           
         USING TAAYD,R4                                                         
         MVI   ELCODE,TAAYELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   FAYSTA6,TAAYSTA6                                                 
                                                                                
                                                                                
         GOTO1 RECVAL,DMCB,TLCOICDQ,(X'08',SCAFCOH),SCAFCONH FROM COMML         
         MVC   FROMCID,TGCID       SAVE FOR DUP. CHECK                          
         SPACE 1                                                                
         USING TLCOPD,R3                                                        
         LA    R3,KEY                                                           
         MVC   TIFCOM,TLCOICOM     SAVE INTERNAL COMML NO. FOR SYSIO            
         MVC   TGCOM,TLCOICOM      AND RECVAL                                   
         DROP  R3                                                               
         SPACE 1                                                                
         BRAS  RE,GETMAIN                                                       
                                                                                
         L     R3,AIO                                                           
         USING TLCOD,R3                                                         
         MVC   AIO,AIO2                                                         
         MVI   FCISTA2,0                                                        
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'A0',TLCOCLI)                              
         L     R4,AIO                                                           
         MVI   ELCODE,TACIELQ                                                   
         USING TACID,R4                                                         
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   FCISTA2,TACISTA2                                                 
         MVC   AIO,AIO1                                                         
         DROP  R3                                                               
                                                                                
         SPACE 1                                                                
         USING TACOD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TACOELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         SPACE 1                                                                
         TM    TGSYSTAT,TASYSMUS   IF NEW MUSIC RULES ARE ENABLED               
         BZ    VK10                                                             
         CLI   TACOTYPE,CTYMUS     CANNOT COPY FROM MUSIC COMMERCIAL            
         BNE   VK10                                                             
         LA    R2,SCAFCOH                                                       
         B     FLDINV                                                           
         SPACE 1                                                                
VK10     MVC   FROMMED,TACOMED     SAVE "FROM" MEDIA                            
         MVC   FROMTYPE,TACOTYPE   AND TYPE                                     
         DROP  R4                                                               
         SPACE 1                                                                
         GOTOR HANDVER,DMCB,SCAFVERH                                            
         SPACE 1                                                                
         GOTOR VVER,DMCB,SCAFVERH,FROMVER   VALIDATE FROM VERSION               
         GOTOR VTRK,DMCB,SCAFTRKH           VALIDATE FROM TRACK                 
                                                                                
         LA    R2,SCATAYH                                                       
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'20',SCATAYH)         TO AGENCY            
         GOTO1 RAVPPLSA,DMCB,1     SEE IF REC / ACT INVALID FOR P+              
         JNE   ERPPLSI2                                                         
*                                                                               
*** GET TAAYSTA6 VIA GETEL CALL                                                 
                                                                                
         L     R4,AIO                                                           
         USING TAAYD,R4                                                         
         MVI   ELCODE,TAAYELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   TAYSTA6,TAAYSTA6                                                 
*                                                                               
         SPACE 1                                                                
         LA    R2,SCATCOH          REQUIRE INPUT IN TARGET COMMERCIAL           
         GOTO1 ANY                                                              
         GOTO1 RECVAL,DMCB,TLCOICDQ,(X'08',(R2)),SCATCONH    TO COMML           
         MVC   COMDA,DMDSKADD                                                   
         SPACE 1                                                                
         CLC   TIAGY,TGAGY         TARGET CAN'T BE SAME AS SOURCE               
         BNE   *+14                                                             
         CLC   TGCID,FROMCID                                                    
         BE    FLDINV                                                           
         SPACE 1                                                                
         USING TLCOPD,R3                                                        
         LA    R3,KEY                                                           
         MVC   TGCOM,TLCOICOM      SAVE NEW INTERNAL COMMERCIAL NUMBER          
         MVC   TOCOM,TLCOICOM                                                   
         DROP  R3                                                               
         SPACE 1                                                                
*                                                                               
         L     R3,AIO                                                           
         USING TLCOD,R3                                                         
         MVC   AIO,AIO2                                                         
         MVI   TCISTA2,0                                                        
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'A0',TLCOCLI)                              
         L     R4,AIO                                                           
         MVI   ELCODE,TACIELQ                                                   
         USING TACID,R4                                                         
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   TCISTA2,TACISTA2                                                 
         MVC   AIO,AIO1                                                         
         DROP  R3                                                               
*                                                                               
         TM    FAYSTA6,TAAYSREG                                                 
         BO    *+12                                                             
         TM    FCISTA2,TACISREG                                                 
         BNO   VK16                                                             
* FROM AGY/CLIENT REG STATUS ON.  MAKE SURE TO AGY/CLIENT ALSO ON               
         TM    TAYSTA6,TAAYSREG                                                 
         BO    VK18                                                             
         TM    TCISTA2,TACISREG                                                 
         BNO   ERREGON                                                          
         B     VK18                                                             
* FROM AGY/CLIENT REG STATUS OFF.  MAKE SURE TO AGY/CLIENT ALSO OFF             
*                                                                               
VK16     TM    TAYSTA6,TAAYSREG                                                 
         BO    ERREGOFF                                                         
         TM    TCISTA2,TACISREG                                                 
         BO    ERREGOFF                                                         
*                                                                               
         USING TACOD,R4                                                         
VK18     L     R4,AIO              R4=A("TO" COMMERCIAL RECORD)                 
         MVI   ELCODE,TACOELQ      GET COMMERCIAL DETAILS ELEMENT               
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         TM    TACOSTA2,TACOPCYC   CANNOT BE A PER CYCLE COMMERCIAL             
         BO    ERRTOPC                                                          
         MVC   TOMED,TACOMED       SAVE "TO" MEDIA                              
         MVC   TOTYPE,TACOTYPE     AND TYPE                                     
         SPACE 1                                                                
         TM    TGSYSTAT,TASYSMUS   IF NEW MUSIC RULES ARE ENABLED               
         BZ    VK20                                                             
         CLI   TACOTYPE,CTYMUS     CANNOT COPY TO MUSIC COMMERCIAL              
         BNE   VK20                                                             
         LA    R2,SCATCOH                                                       
         B     FLDINV                                                           
         DROP  R4                                                               
         SPACE 1                                                                
VK20     BRAS  RE,GETMAIN                                                       
         GOTOR HANDVER,DMCB,SCATVERH                                            
         SPACE 1                                                                
         GOTOR VVER,DMCB,SCATVERH,TOVER     VALIDATE TO VERSION                 
         GOTOR VTRK,DMCB,SCATTRKH           VALIDATE TO TRACK                   
         SPACE 1                                                                
         LA    R2,SCASCSTH         VALIDATE SELECT CAST                         
         GOTO1 ANY                                                              
         CLI   8(R2),C'N'          DON'T SELECT CAST                            
         BE    VK30                                                             
         CLI   8(R2),C'Y'          YES                                          
         BNE   FLDINV                                                           
         OI    STATUS,SELCAST      SET SELECT CAST                              
         NI    SCATTL1H+1,X'FB'    TURN HEADINGS TO HIGH INTENSITY              
         NI    SCATTL2H+1,X'FB'                                                 
         OI    SCATTL1H+6,X'80'                                                 
         OI    SCATTL2H+6,X'80'                                                 
         SPACE 1                                                                
VK30     LA    R2,SCADUPH          VALIDATE COPY DUPLICATES                     
         GOTO1 ANY                                                              
         CLI   8(R2),C'Y'          YES                                          
         BNE   *+12                                                             
         OI    STATUS,DUPOK        SET DUPLICATES OK                            
         B     VK40                                                             
         CLI   8(R2),C'O'          ONLY                                         
         BNE   *+12                                                             
         OI    STATUS,DUPONLY      SET ONLY DUPLICATES OK                       
         B     VK40                                                             
         CLI   8(R2),C'N'          DON'T COPY DUPLICATES                        
         BNE   FLDINV                                                           
         SPACE 1                                                                
VK40     BAS   RE,VFILTS           VALIDATE FILTERS                             
         SPACE 1                                                                
         BAS   RE,VOVERS           VALIDATE OVERRIDES                           
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE FILTERS                                      
         SPACE 1                                                                
VFILTS   NTR1                                                                   
         GOTO1 FLTSCAN,DMCB,SCAUNISH,UNIVAL,FLTUNIS UNIONS                      
         SPACE 1                                                                
         GOTO1 (RF),(R1),SCACATSH,CATVAL,FLTCATS    CATEGORIES                  
         SPACE 1                                                                
         LA    R2,SCAONOFH         VALIDATE CAMERA STATUS                       
         CLI   5(R2),0                                                          
         BE    VFI4                                                             
         MVC   FLTONOF,8(R2)                                                    
         OC    FLTONOF,SPACES                                                   
         CLC   FLTONOF,=C'ON '                                                  
         BE    *+14                                                             
         CLC   FLTONOF,=C'OFF'                                                  
         BNE   FLDINV                                                           
         SPACE 1                                                                
VFI4     DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE SCANNED FILTERS                              
         SPACE 1                                                                
*                                  P1=A(FIELD)                                  
*                                  P2=A(DATA VAL. ROUTINE)                      
*                                  P3=A(W/S FOR SAVED DATA)                     
FLTSCAN  NTR1                                                                   
         LM    R2,R4,0(R1)                                                      
         SPACE 1                                                                
         CLI   5(R2),0             GET OUT IF NOTHING INPUT                     
         BE    FLTSX                                                            
         GOTO1 SCANNER,DMCB,(R2),(X'80',BLOCK)                                  
         CLI   4(R1),0                                                          
         BE    FLDINV                                                           
         LR    RF,R3               RF=A(DATA VALIDATION ROUTINE)                
         SPACE 1                                                                
         LA    R3,BLOCK            R3=A(SCAN BLOCK)                             
         USING SCAND,R3                                                         
         ZIC   R0,4(R1)            R0=N'SCAN ENTRIES                            
         SPACE 1                                                                
FLTS2    MVC   ERRDISP,SCDISP1     SET DISP. INTO FLD OF LHS FOR ERRORS         
         SPACE 1                                                                
         CLI   SCLEN1,0            MUST HAVE LHS                                
         BE    FLDINV                                                           
         SPACE 1                                                                
         GOTO1 (RF),DMCB,SCDATA1   VALIDATE INPUT                               
         BNE   FLDINV                                                           
         MVC   0(3,R4),SCDATA1     IT'S GOOD - SAVE IT                          
         LA    R4,3(R4)            AND BUMP TO NEXT LOCATION                    
         SPACE 1                                                                
         MVC   ERRDISP,SCDISP2     SET DISP. INTO FLD OF RHS                    
         CLI   SCLEN2,0            CANNOT HAVE RHS                              
         BNE   FLDINV                                                           
         SPACE 1                                                                
         LA    R3,SCANNEXT         BUMP TO NEXT SCAN FIELD                      
         BCT   R0,FLTS2            AND VALIDATE IT                              
         SPACE 1                                                                
FLTSX    MVI   ERRDISP,0                                                        
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE OVERRIDES                                    
         SPACE 1                                                                
VOVERS   NTR1                                                                   
         LA    R2,SCAYRH           VALIDATE CONTRACT YEAR                       
         MVC   TGYRCDE,SPACES                                                   
         CLI   5(R2),0                                                          
         BE    VOV4                                                             
         OC    SCAYR,SPACES                                                     
         GOTO1 YRVAL,DMCB,8(R2)    VALIDATE - SET IN GLOBAL STORAGE             
         BNE   FLDINV                                                           
         SPACE 1                                                                
VOV4     LA    R2,SCA1STH          VALIDATE FIRST SERVICES DATE                 
         XC    FRSTSERV,FRSTSERV                                                
         CLI   5(R2),0                                                          
         BE    VOV6                                                             
         GOTO1 DTVAL,DMCB,FRSTSERV                                              
         XC    ELEMENT,ELEMENT                                                  
         GOTO1 EXPIRE,DMCB,ELEMENT,FRSTSERV,EXPDATE                             
         SPACE 1                                                                
VOV6     LA    R2,SCAFFCH          VALIDATE FIRST FIXED CYCLE DATE              
         XC    FRSTFCYC,FRSTFCYC                                                
         CLI   5(R2),0                                                          
         BE    VOVX                                                             
         GOTO1 DTVAL,DMCB,FRSTFCYC                                              
         SPACE 1                                                                
VOVX     B     XIT                                                              
         EJECT                                                                  
*              OUTPUT N'CAST MEMBERS SELECTED                                   
         SPACE 1                                                                
OUTSEL   NTR1                                                                   
         EDIT  NSEL,(3,SCANSEL),ZERO=NOBLANK                                    
         NI    SCANSELH+1,X'F3'    TURN TO HIGH INTENSITY                       
         OI    SCANSELH+6,X'80'                                                 
         B     XIT                                                              
         EJECT                                                                  
*        LIST CAST RECORD                                                       
*                                                                               
LISTCAST NTR1                                                                   
         MVC   TIQSKEY,TIKEY       SET CONTINUATION KEY                         
         CLI   COUNTER,9                                                        
         BNE   LC10                IF END OF PAGE                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'90'           FORCE END TO SYSIO READ                      
         GOTO1 HIGH                                                             
         MVI   COUNTER,0           AND START OVER                               
         B     LCX                                                              
*                                                                               
LC10     L     R2,APLINE           A(HEADER OF NEXT LINE TO PRINT ON)           
         LA    R2,8(R2)            PAST HEADER                                  
         USING LLINED,R2                                                        
         L     R3,TIAREC                                                        
         USING TLCAD,R3                                                         
*                                                                               
         MVC   CASSN,TLCASSN       SSN                                          
         XC    CASSN,CASSN                                                      
         GOTO1 SSNPACK,DMCB,TLCASSN,CASSN                                       
*                                                                               
LC11     MVC   CACAT,TLCACAT       CATEGORY                                     
         MVC   CACAM,TIONOF        ON/OFF CAMERA                                
         MVC   CATAX,TIUNIT        TAX UNIT                                     
         MVC   CAUNI,TIUN          UNION                                        
         MVC   CALCL,TILOCL        LOCAL                                        
         MVC   CAYR,TIYEAR         CONTRACT YEAR                                
         SPACE                                                                  
         L     R4,TIAREC                                                        
         USING TACAD,R4                                                         
         MVI   ELCODE,TACAELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   LC15                                                             
         MVC   CATAX,TACAUNIT      TAX UNIT                                     
         GOTO1 TRNSAGT,DMCB,(X'40',TACANCDE),CAAGNT                             
         MVC   CACORP,TACACORP     CORP CODE                                    
         TM    TACASTAT,TACASTLF                                                
         BNO   LC12                                                             
         MVI   CALIFT,C'Y'         ON LIFT                                      
         SPACE 1                                                                
LC12     TM    TACASTAT,TACASTLO                                                
         BNO   LC15                                                             
         MVI   CALIFT,C'O'         ONLY ON LIFT                                 
         SPACE 1                                                                
LC15     ZIC   R1,COUNTER                                                       
         LA    R4,DATABLE          A(START OF D/A TABLE)                        
         LTR   R1,R1                                                            
         BZ    LC30                                                             
         SPACE 1                                                                
LC20     LA    R4,4(R4)            BUMP TO CORRECT ENTRY IN TABLE               
         BCT   R1,LC20                                                          
         SPACE 1                                                                
LC30     MVC   0(4,R4),TIDSKADD    SET D/A                                      
         MVI   MYNAMEH,24          SET FIELD LENGTH                             
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'88',TLCASSN),MYNAMEH                      
         MVC   CALNAME(16),MYNAME                                               
         OI    STATUS,REREAD       SET WE NEED TO RE-READ SYSIO'S KEY           
         SPACE 1                                                                
         L     R2,APLINE           A(HEADER OF NEXT LINE TO PRINT ON)           
         OI    6(R2),X'80'         TRASNMIT                                     
         ZIC   R1,0(R2)            BUMP PAST LINE FIELD                         
         AR    R2,R1                                                            
         ZIC   R1,0(R2)            BUMP PAST SEL FIELD                          
         AR    R2,R1                                                            
         ZIC   R1,0(R2)            BUMP PAST AUTO SKIP FIELD                    
         AR    R2,R1                                                            
         ST    R2,APLINE           NEXT LINE TO PRINT ON                        
         SPACE 1                                                                
         ZIC   R1,COUNTER                                                       
         LA    R1,1(R1)                                                         
         STC   R1,COUNTER                                                       
         SPACE 1                                                                
LCX      B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO SELECT CAST MEMBERS                                   
         SPACE 1                                                                
SELECTC  NTR1                                                                   
         MVI   LISTED,C'N'                                                      
         MVI   LSTPAGE,C'N'                                                     
         LA    R2,SCASELH          START AT FIRST SEL FIELD                     
         LA    R4,DATABLE          D/A TABLE                                    
         LA    R5,SCALSTH                                                       
         LA    R3,SELTAB                                                        
         LA    R1,STABLNQ(R3)      END OF TABLE                                 
         SPACE 1                                                                
SC10     OC    0(4,R3),0(R3)       FIND LAST ENTRY IN TABLE                     
         BZ    SC20                                                             
         LA    R3,4(R3)                                                         
         CR    R3,R1                                                            
         BNH   SC10                                                             
         DC    H'0'                TABLE TOO SMALL                              
         SPACE 1                                                                
SC20     CR    R2,R5               IF END OF SCREEN - EXIT                      
         BNL   SCX                                                              
         CLI   5(R2),0             ACCEPT ANY INPUT                             
         BE    SC30                                                             
         OC    0(4,R4),0(R4)       IS THERE A D/A TO SELECT                     
         BZ    SC30                                                             
         MVC   8(3,R2),SPACES      CLEAR FIELD FROM INPUT                       
         OI    6(R2),X'80'                                                      
         MVC   0(4,R3),0(R4)       SAVE SELECTED D/A                            
         LA    R3,4(R3)            BUMP SELECTED D/A TABLE                      
         ZIC   R1,NSEL             INCREMENT NUMBER SELECTED                    
         LA    R1,1(R1)                                                         
         STC   R1,NSEL                                                          
         BRAS  RE,SETCMP           SET SSN & CATEGORY FOR COMPARE               
         MVI   BYTE,COUNT          COUNT DUPLICATES                             
         BAS   RE,CHKDUP           CHECK IF THIS IS A DUPLICATE                 
         SPACE 1                                                                
SC30     LA    R4,4(R4)            BUMP D/A TABLE                               
         ZIC   R1,0(R2)            BUMP PAST SEL,DISPLAY & AUTO SKIP            
         AR    R2,R1                                                            
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         B     SC20                                                             
         SPACE 1                                                                
SCX      B     XIT                                                              
         EJECT                                                                  
*              ROUTINE GOES THROUGH SELECTED RECS TO ADD THEM TO FILE           
         SPACE 1                                                                
ADDSEL   NTR1                                                                   
         LA    R4,SELTAB           SELECTED D/A'S                               
         LA    R2,STABLNQ(R4)      END OF TABLE                                 
         SPACE 1                                                                
AS10     OC    0(4,R4),0(R4)       ANY D/A'S LEFT                               
         BZ    ASX                                                              
         CR    R4,R2               REACHED END OF TABLE                         
         BH    ASX                                                              
         LA    R3,KEY                                                           
         USING TLDRD,R3                                                         
         XC    KEY,KEY                                                          
         MVC   TLDRDA,0(R4)        SET D/A                                      
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC              & GET SELECTED CAST RECORD                   
         SPACE 1                                                                
         L     R3,AIO                                                           
         BAS   RE,NEWCAST          ADD A NEW CAST RECORD TO THE FILE            
         LA    R4,4(R4)            BUMP TO NEXT D/A                             
         B     AS10                                                             
         SPACE 1                                                                
ASX      B     XIT                                                              
         EJECT                                                                  
                                                                                
*              ROUTINE TO CONTROL RECORD COPYING                                
         SPACE 1                                                                
COPY     NTR1                                                                   
         LA    R2,IOHOOK           SET A(I/O HOOK) FOR SYSIO                    
         ST    R2,TIHOOK                                                        
         MVC   TIUSERID,TWAORIG    SET UP SOME FIELDS FOR SYSIO                 
         MVC   TIQSTAFF,TGCTSTAF                                                
         MVC   TIACOMFC,ACOMFACS                                                
         MVI   TIREAD,TLCACDQ      SET TO READ CAST RECORDS                     
         SPACE 1                                                                
         MVI   ACTNUM,ACTADD       TRICK ADDPTRS TO ASSUME ADD                  
         SPACE 1                                                                
         TM    STATUS,SELCAST      IF SELECTING CAST                            
         BNO   CP10                                                             
         LA    R1,SCALINEH         SET LIST LINE                                
         ST    R1,APLINE                                                        
         XC    DATABLE(36),DATABLE    CLEAR D/A TABLE                           
         TWAXC SCASELH,SCALSTH,PROT=Y      CLEAR SCREEN                         
         SPACE 1                                                                
CP10     GOTO1 TASYSIO,DMCB,TASYSIOD  OFF TO SYSIO TO READ RECORDS              
         SPACE 1                                                                
         MVI   COUNTER,0                                                        
         MVI   LISTED,C'Y'                                                      
         MVI   ACTNUM,ACTCOPY      RESTORE CORRECT ACTION                       
         SPACE 1                                                                
         TM    STATUS,SELCAST      IF NOT SELECTING CAST                        
         BO    CP20                                                             
         BRAS  RE,UPDCOMML         UPDATE NEW COMMERCIAL                        
         B     COPIED                                                           
         SPACE 1                                                                
CP20     CLI   KEY,X'90'           ELSE IF SYSIO STOPPED (NOT FORCED)           
         BE    CPX                                                              
         XC    TIQSKEY,TIQSKEY     CLEAR CONTINUE KEY TO START OVER             
         SPACE 1                                                                
CPX      B     XIT                                                              
         EJECT                                                                  
                                                                                
*              ROUTINE SCANS ENTIRE CAST DIRECTORY OF NEW COMMERCIAL            
         SPACE 1                                                                
DIRSCAN  NTR1                                                                   
         CLC   TGCOM,TOCOM                                                      
         BE    *+6                                                              
         DC    H'00'                                                            
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'B4',0)                                   
         BE    *+6                                                              
         DC    H'00'                                                            
         SPACE 1                                                                
         USING TANUD,R4                                                         
         MVI   ELCODE,TANUELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TANUTSEQ))                                     
         BE    *+6                                                              
         DC    H'00'                                                            
         L     R4,TGELEM                                                        
         MVC   CASTSEQ,TANUNXTC                                                 
         DROP  R4                                                               
         SPACE 1                                                                
         LA    R4,EXTTAB                                                        
         USING CASTD,R4            R4=A(TABLE OF EXISTING CAST MEMBERS)         
         XC    KEY,KEY                                                          
         LA    R3,KEY              R3=A(KEY)                                    
         USING TLCAD,R3                                                         
         MVI   TLCACD,TLCACDQ      BUILD INITIAL KEY                            
         MVC   TLCACOM,TGCOM       INTERNAL COMMERCIAL NUMBER                   
         GOTO1 HIGH                                                             
         B     DIR4                                                             
         SPACE 1                                                                
DIR2     GOTO1 SEQ                 GET NEXT DIRECTORY RECORD                    
         SPACE 1                                                                
DIR4     CLC   TLCAKEY(TLCASORT-TLCAD),KEYSAVE  STILL THIS COMMERCIAL           
         BNE   DIR6                                                             
         SPACE 1                                                                
         PACK  DUB,TLCASSN         CVT SSN TO BINARY                            
         CVB   R1,DUB                                                           
         ST    R1,CASTSSN          AND SAVE IN TABLE                            
         SPACE 1                                                                
         MVC   CASTCAT,TLCACAT     SAVE CATEGORY AS WELL                        
         SPACE 1                                                                
         BRAS  RE,TESTANN          TEST IF ANNOUNCER                            
         LR    R1,R3                                                            
         USING TLDRD,R1                                                         
         MVC   MYSTAT,TLDRSTAT     SET DIRECTORY STATUS BYTES                   
         DROP  R1                                                               
         BAS   RE,TESTLIFT         TEST IF PERFORMER ON LIFT                    
         SPACE 1                                                                
         LA    R4,CASTNEXT         BUMP TO NEXT ENTRY IN CAST TABLE             
         B     DIR2                                                             
         SPACE 1                                                                
DIR6     XC    CASTD(CASTLNQ),CASTD  MARK END OF CAST TABLE                     
         B     XIT                                                              
         EJECT                                                                  
                                                                                
*              ROUTINE TO PROCESS RECORDS FROM SYSIO                            
         SPACE 1                                                                
IOHOOK   NTR1                                                                   
         CLI   TIMODE,PROCREC      PROCESS RECORDS                              
         BNE   XIT                                                              
         L     R3,TIAREC           R3=A(CAST RECORD)                            
         ST    R3,AIO                                                           
         BAS   RE,FILTER           FILTER THIS CAST RECORD                      
         BNE   IOH20                                                            
         TM    STATUS,SELCAST      IF WE ARE SELECTING FROM THE CAST            
         BNO   IOH10                                                            
         BAS   RE,LISTCAST         LIST THE CAST MEMBER                         
         B     IOH20                                                            
         SPACE 1                                                                
IOH10    BAS   RE,NEWCAST          ELSE ADD A NEW CAST REC TO THE FILE          
         SPACE 1                                                                
IOH20    MVC   AIO,AIO1            RESTORE DEFAULT I/O AREA                     
         TM    STATUS,REREAD       TEST WE NEED TO RE-READ SYSIO'S KEY          
         BZ    IOHX                                                             
         XI    STATUS,REREAD                                                    
         MVC   KEY,TIKEY           SET SYSIO'S KEY                              
         GOTO1 HIGH                                                             
         SPACE 1                                                                
IOHX     B     XIT                                                              
         EJECT                                                                  
                                                                                
*              ROUTINE TO FILTER CAST RECORDS FROM SYSIO                        
         SPACE 1                                                                
         USING TLCAD,R3            R3=A(CAST RECORD)                            
FILTER   NTR1                                                                   
         OC    FLTONOF,FLTONOF     ON/OFF CAMERA FILTER                         
         BZ    *+14                                                             
         CLC   TIONOF,FLTONOF                                                   
         BNE   NO                                                               
         SPACE 1                                                                
         LA    R2,FLTUNIS          UNION FILTERS                                
         CLI   0(R2),0                                                          
         BE    FLT6                                                             
         LA    R0,NUNIS                                                         
FLT4     CLC   TIUN,0(R2)                                                       
         BE    FLT6                                                             
         LA    R2,3(R2)                                                         
         BCT   R0,FLT4                                                          
         B     NO                                                               
         SPACE 1                                                                
FLT6     LA    R2,FLTCATS          CATEGORY FILTERS                             
         CLI   0(R2),0                                                          
         BE    FLT10                                                            
         LA    R0,NCATS                                                         
FLT8     CLC   TICAT,0(R2)                                                      
         BE    FLT10                                                            
         LA    R2,3(R2)                                                         
         BCT   R0,FLT8                                                          
         B     NO                                                               
         SPACE 1                                                                
FLT10    GOTOR VTFILT,DMCB,=AL1(TAFNTVER),FROMVER VERSION FILTER                
         BNE   NO                                                               
         SPACE 1                                                                
         GOTOR VTFILT,DMCB,=AL1(TAFNTTRK),SCAFTRK TRACK FILTER                  
         BNE   NO                                                               
         SPACE 1                                                                
         BRAS  RE,CHKREL           CHECK IF CAST WAS RELEASED                   
         BE    NO                                                               
         SPACE 1                                                                
         PACK  DUB,TLCASSN         CVT SSN TO BINARY                            
         CVB   R1,DUB              SET BSSN = BINARY SSN                        
         ST    R1,BSSN                                                          
         MVC   CAT,TLCACAT         AND CAT = CATEGORY                           
         MVI   BYTE,COUNT          COUNT DUPLICATES                             
         TM    STATUS,SELCAST      IF SELECTING CAST                            
         BNO   *+8                                                              
         MVI   BYTE,DONTCNT        DON'T COUNT DUPS UNTIL SELECTED              
         BAS   RE,CHKDUP                                                        
         BNE   NO                                                               
         B     YES                                                              
         SPACE 1                                                                
         EJECT                                                                  
*        CHECK FOR DUPLICATES                                                   
*              BYTE - COUNT/DONTCNT DUPLICATES                                  
*              BSSN - BINARY SS NUMBER                                          
*              CAT  - CATEGORY                                                  
         SPACE 1                                                                
CHKDUP   NTR1                                                                   
         LA    R4,EXTTAB           R4=A(TABLE OF EXISTING CAST MEMBERS)         
         USING CASTD,R4                                                         
         SPACE 1                                                                
CDUP10   OC    CASTCAT,CASTCAT     END OF TABLE                                 
         BZ    CDUP20                                                           
         CLC   BSSN,CASTSSN        LOOK FOR MATCH ON SSN                        
         BNE   *+14                                                             
         CLC   CASTCAT,CAT         AND CATEGORY                                 
         BE    *+12                                                             
         LA    R4,CASTNEXT         BUMP TO NEXT ENTRY IN CAST TABLE             
         B     CDUP10                                                           
         SPACE 1                                                                
         TM    STATUS,DUPOK+DUPONLY  FOUND DUPLICATE - TEST OK TO COPY          
         BZ    NO                                                               
         CLI   BYTE,COUNT          COUNT DUPLICATES OR NOT                      
         BNE   YES                                                              
         LH    RF,NDUPS            ELSE ADD TO DUPLICATE COUNTER                
         AHI   RF,1                                                             
         STH   RF,NDUPS                                                         
         B     YES                                                              
         SPACE 1                                                                
CDUP20   TM    STATUS,DUPONLY      ENTRY NOT FOUND - TEST DUPS. ONLY            
         BO    NO                                                               
         B     YES                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE ADDS A NEW CAST RECORD TO THE FILE                       
         SPACE 1                                                                
         USING TLCAD,R3            R3=A(CAST RECORD)                            
NEWCAST  NTR1                                                                   
         MVC   TLCACOM,TGCOM       SET NEW INTERNAL COMMERCIAL NUMBER           
         MVC   TLCASEQ,CASTSEQ     SET NEW INPUT SEQUENCE NUMBER                
         LH    R1,CASTSEQ                                                       
         LA    R1,1(R1)            BUMP INPUT SEQUENCE NUMBER                   
         STH   R1,CASTSEQ                                                       
         SPACE 1                                                                
         MVI   ELCODE,TAHFELQ      DELETE HOLDING FEE NOTICE EL.                
         GOTO1 REMELEM                                                          
         XC    ELEMENT,ELEMENT                                                  
         LA    R4,ELEMENT                                                       
         USING TAHFD,R4                                                         
         MVI   TAHFEL,TAHFELQ      BUILD AND                                    
         MVI   TAHFLEN,TAHFLNQ                                                  
         GOTO1 ADDELEM             ADD NEW ONE                                  
         SPACE 1                                                                
         MVI   ELCODE,TACRELQ      DELETE APPLIED CREDIT HISTORY ELS.           
         GOTO1 REMELEM                                                          
         SPACE 1                                                                
         ST    R3,AIO                                                           
         MVI   ELCODE,TAFNELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTWEB))                                     
         BNE   NC10                                                             
         L     RE,TGELEM           REMOVE WEB APPLICATION ID ELEMENT            
         MVI   0(RE),X'FF'                                                      
         SPACE 1                                                                
NC10     GOTO1 GETL,DMCB,(1,=AL1(TAFNTOWB))                                     
         BNE   NC20                                                             
         L     RE,TGELEM           REMOVE ORIGINAL WEB APPLICATION              
         MVI   0(RE),X'FF'         ID ELEMENT                                   
         SPACE 1                                                                
NC20     BRAS  RE,PROOAP           PROCESS OVERSCALE AMOUNT/PERCENTS            
         SPACE 1                                                                
         MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM                                                          
         SPACE 1                                                                
         BRAS  RE,ADDVEREL         ADD "TO" VERSION ELEMENT                     
         GOTO1 NAMIN,DMCB,TAFNELQ,(X'80',SCATTRKH),TAFNTTRK                     
         SPACE 1                                                                
         MVI   ELCODE,TACAELQ      GET CAST DETAILS EL.                         
         LR    R4,R3                                                            
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACAD,R4                                                         
         CLC   TGYRCDE,SPACES      CONTRACT YEAR OVERRIDE                       
         BE    *+10                                                             
         MVC   TACAYEAR,TGYRCDE                                                 
         SPACE 1                                                                
         OC    FRSTSERV,FRSTSERV   FIRST SERVICES OVERRIDE                      
         BZ    NC30                                                             
         MVC   TACAFRST,FRSTSERV                                                
         MVC   TACAEXP,EXPDATE     SET NEW EXPIRATION DATE                      
         SPACE 1                                                                
NC30     OC    FRSTFCYC,FRSTFCYC   1ST FIXED CYCLE OVERRIDE                     
         BZ    *+10                                                             
         MVC   TACAFCYC,FRSTFCYC                                                
         SPACE 1                                                                
         GOTO1 ACTVIN,DMCB,0       ADD NEW ACTIVITY ELEMENT                     
         SPACE 1                                                                
         BRAS  RE,MYADDREC         ADD THE RECORD TO THE FILE                   
         SPACE 1                                                                
         BRAS  RE,TESTANN          TEST IF ANNOUNCER                            
         MVC   MYSTAT,TLCASTAT     SET STATUS BYTES FROM KEY IN AIO             
         BAS   RE,TESTLIFT         TEST IF PERFORMER ON LIFT                    
         SPACE 1                                                                
         GOTO1 ADDPTRS,DMCB,PTRBLK ADD PASSIVE POINTERS                         
*                                                                               
         LH    RF,NCOPIED          ADD TO COPY COUNT                            
         AHI   RF,1                                                             
         STH   RF,NCOPIED                                                       
         OI    STATUS,REREAD       SET WE NEED TO RE-READ SYSIO'S KEY           
         B     XIT                                                              
         EJECT                                                                  
*        THIS ROUTINE INSPECTS THE ACTIVE KEY AND DETERMINES IF THE             
*        CAST MEMBER IS ON THE LIFT OR NOT.                                     
*        MYSTAT - STATUS BYTE OF KEY                                            
*                                                                               
TESTLIFT NTR1                                                                   
         TM    MYSTAT,TLCASLFT     IF CAST MEMBER IS ON LIFT                    
         BNO   TLX                                                              
         L     R1,NUMLIFT                                                       
         LA    R1,1(R1)            INCREMENT N'PERFORMERS ON LIFT               
         ST    R1,NUMLIFT                                                       
*                                                                               
TLX      B     XIT                                                              
         EJECT                                                                  
*              EXITS, ETC.                                                      
         SPACE 1                                                                
SELINF   MVI   MYMSGNO1,3          CAST DISPLAYED - SELECT AS DESIRED           
         LA    R2,SCASELH                                                       
         B     INFEND                                                           
         SPACE 1                                                                
LSTINF   MVI   MYMSGNO1,10         NO MORE CAST TO DISPLAY - SELECT             
         MVI   LSTPAGE,C'Y'                                                     
         LA    R2,SCASELH                                                       
         B     INFEND                                                           
FLDINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     THEEND                                                           
         SPACE 1                                                                
ERRTOPC  MVC   MYMSGNO,=Y(ERRNCPCY) CANNOT COPY TO PER CYCLE COMML              
         J     ERREND                                                           
ERPPLSI  MVC   MYMSGNO,=Y(ERRIAPPA)   RECORD / ACTION INVALID FOR P+            
         J     ERREND                                                           
                                                                                
ERPPLSI2 MVC   MYMSGNO,=Y(ERRIAPP2)   RECORD / ACTION INVALID FOR P+            
         J     ERREND                                                           
*                                                                               
ERREGON  MVC   MYMSGNO,=Y(534)        FROM AGY/CLIENT REG STATUS ON             
         J     ERREND                                                           
*                                                                               
ERREGOFF MVC   MYMSGNO,=Y(535)        FROM AGY/CLIENT REG STATUS OFF            
         J     ERREND                                                           
                                                                                
ERREND   MVI   BLOCK,0                                                          
         MVI   MYMTYP,GTMERR                                                    
         OI    GENSTAT2,USGETTXT                                                
         J     THEEND                                                           
                                                                                
COPIED   MVI   MYMSGNO1,37         SET COMPLETION MESSAGE                       
         SPACE 1                                                                
         LA    R2,NCOPIED          BUILD SUBSTITUTION BLOCK FOR GETTXT          
         LA    R3,BLOCK                                                         
         BAS   RE,CNTOUT                                                        
         LA    R2,NDUPS                                                         
         BAS   RE,CNTOUT                                                        
         OI    GENSTAT2,USGETTXT                                                
         LA    R2,CONRECH                                                       
         B     THEEND                                                           
*                                                                               
CNTOUT   DS    0H                                                               
         OC    0(2,R2),0(R2)       IF COUNT IS ZERO                             
         BNZ   CNT2                                                             
         MVC   1(2,R3),=C'NO'      DISPLAY 'NO'                                 
         LA    R1,2                                                             
         B     CNT4                                                             
CNT2     EDIT  (B2,0(R2)),(3,1(R3)),ALIGN=LEFT                                  
         LR    R1,R0                                                            
CNT4     LA    R1,1(R1)                                                         
         STC   R1,0(R3)                                                         
         AR    R3,R1                                                            
         MVI   0(R3),0                                                          
         BR    RE                                                               
*                                                                               
INFEND   OI    GENSTAT2,USGETTXT                                                
THEEND   GOTO1 EXIT,DMCB,0                                                      
         SPACE 1                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 3                                                                
         GETEL (R4),DATADISP,ELCODE                                             
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 1                                                                
COUNT    EQU   0                   COUNT DUPLICATES                             
DONTCNT  EQU   1                   DON'T COUNT DUPLICATES                       
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*        SET BSSN = BINARY SSN & CAT = CATEGORY                                 
         SPACE 1                                                                
SETCMP   NTR1  BASE=*,LABEL=*                                                   
         USING LLINED,R2                                                        
         ZIC   R1,0(R2)            BUMP PAST SELECT FIELD                       
         AR    R2,R1                                                            
         ZIC   R1,0(R2)            AUTOSKIP                                     
         AR    R2,R1                                                            
         LA    R2,8(R2)            & 8 BYTE HEADER                              
         MVC   TGSSN,CASSN                                                      
         GOTO1 SSNUNPK,DMCB,CASSN,TGSSN                                         
SETCMP10 PACK  DUB,TGSSN           CVT SSN TO BINARY                            
         CVB   R1,DUB              R1=BINARY SSN                                
         ST    R1,BSSN             BINARY SSN                                   
         MVC   CAT,CACAT           CATEGORY                                     
         XIT1                                                                   
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
*              ROUTINE CONTROLS ADDING NEW CAST RECORD TO FILE                  
         SPACE 1                                                                
         USING TLCAD,R3            R3=A(CAST RECORD)                            
MYADDREC NTR1  BASE=*,LABEL=*                                                   
         MVC   KEY,TLCAKEY         SEE IF RECORD ON FILE MARKED DELETED         
         OI    DMINBTS,X'08'       SET READ FOR DELETED                         
         MVI   RDUPDATE,C'Y'       AND FOR UPDATE                               
         GOTO1 HIGH                LOOK FOR RECORD ALREADY ON FILE              
         SPACE 1                                                                
         LA    R4,KEY              R4=A(DIRECTORY KEY)                          
         USING TLDRD,R4                                                         
         CLC   TLDRKEY,KEYSAVE     TEST WE FOUND RECORD                         
         BNE   MYADR4                                                           
         TM    TLDRSTAT,X'80'      IT HAD BETTER BE DELETED                     
         BO    *+6                                                              
         DC    H'0'                                                             
         NI    TLDRSTAT,X'3F'      TURN OFF CAST DELETED BITS                   
         GOTO1 WRITE               AND WRITE IT BACK                            
         SPACE 1                                                                
         MVC   AIO,AIO3            NOW SET TO GET THE RECORD                    
         MVI   RDUPDATE,C'Y'       READ FOR UPDATE                              
         GOTO1 GETREC              GET THE RECORD SO THAT WE CAN ...            
         ST    R3,AIO                                                           
         GOTO1 PUTREC              WRITE NEW ONE BACK OVER DELETED ONE          
         B     MYADRX                                                           
         SPACE 1                                                                
MYADR4   MVC   TLDRKEY,KEYSAVE                                                  
         GOTO1 ADDREC              OK TO ADD THE RECORD                         
         SPACE 1                                                                
MYADRX   NI    DMINBTS,X'F7'                                                    
         XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
                                                                                
*              ROUTINE TO GET MAIN COMMERCIAL RECORD INTO AIO                   
*              ON ENTRY ... R3=A(KEY)                                           
         SPACE 1                                                                
         USING TLCOPD,R3                                                        
GETMAIN  NTR1  BASE=*,LABEL=*                                                   
         CLI   TLCOIVER,26         IF VERSION IS GREATER THAN 26                
         BNH   GMAINX              READ MAIN COMMERCIAL RECORD                  
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'A4',TGCOM)                               
         BE    GMAINX                                                           
         DC    H'00'                                                            
GMAINX   XIT1                                                                   
         DROP R3                                                                
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO HANDLE INPUTTED A VERSION                             
         SPACE 1                                                                
HANDVER  NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)           R2=A(VERSION SCREEN FIELD)                    
         SPACE 1                                                                
         CLI   5(R2),0            EXIT IF VERSION LETTER INPUT                  
         BNE   HVX                                                              
         SPACE 1                                                                
         L     R4,AIO             R4=A(COMMERCIAL RECORD)                       
         MVI   ELCODE,TAVRELQ                                                   
         BRAS  RE,GETEL           IF NO VERSION ELEMENTS EXIST                  
         BNE   HVX                THIS IS NOT A VERSION, EXIT                   
         SPACE 1                                                                
         OI    4(R2),X'08'                                                      
         MVI   5(R2),1                                                          
         OI    6(R2),X'80'                                                      
         MVI   8(R2),C'1'                                                       
HVX      XIT1                                                                   
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO VALIDATE VERSION INPUT                                
         SPACE 1                                                                
VVER     NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)            R2=A(VERSION SCREEN FIELD)                   
         L     R3,4(R1)            R3=A(WHERE TO SAVE VERSION CODE)             
         SPACE 1                                                                
         MVI   0(R3),0                                                          
         SPACE 1                                                                
         CLI   5(R2),0                                                          
         BE    VVX                                                              
         SPACE 1                                                                
         L     R4,AIO              R4=A(COMMERCIAL RECORD)                      
         SPACE 1                                                                
         GOTO1 VALINUM             VALIDATE VERSION CODE                        
         MVC   0(1,R3),ACTUAL                                                   
         SPACE 1                                                                
         CLI   ACTUAL,26           IF VERSION CODE IS GREATER THAN 26           
         BNH   VV40                                                             
         SPACE 1                                                                
         USING VINDEXD,RE                                                       
         LA    RE,VERINDEX         FIND RECORD EQUATE FOR THIS                  
VV10     CLI   0(RE),X'FF'         VERSION NUMBER                               
         BNE   *+6                                                              
         DC    H'00'                                                            
         CLC   ACTUAL,VINDUPLM                                                  
         BNH   VV20                                                             
         LA    RE,VINDLNQ(RE)                                                   
         B     VV10                                                             
         SPACE 1                                                                
VV20     XC    KEY,KEY              GET COMMERCIAL RECORD FOR                   
         MVC   KEY(L'TLCOKEY),0(R4) THAT VERSION                                
         MVC   KEY+TLCOVER-TLCOD(L'TLCOVER),VINDEQUT                            
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLCOKEY),KEYSAVE                                           
         BNE   VVFLDINV                                                         
         GOTO1 GETREC                                                           
         B     VV40                                                             
         DROP  RE                                                               
         SPACE 1                                                                
         USING TAVRD,R4                                                         
VV40     MVI   ELCODE,TAVRELQ                                                   
         BRAS  RE,GETEL            R4=A(VERSION ELEMENT)                        
         B     *+8                                                              
VV50     BRAS  RE,NEXTEL                                                        
         BNE   VVFLDINV                                                         
         CLC   TAVRVERS,0(R3)      MATCHING VERSION ELEMENT                     
         BNE   VV50                MUST BE FOUND OR ERROR                       
         DROP  R4                                                               
         SPACE 1                                                                
         USING TLCOD,R4                                                         
         L     R4,AIO              IF VERSION WAS NOT ON MAIN                   
         CLI   TLCOVER,TLCOV026    COMMERCIAL RECORD, RESTORE                   
         BE    VVX                 MAIN COMMERCIAL IN AIO                       
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'A4',0)                                   
         BE    VVX                                                              
         DC    H'00'                                                            
         DROP  R4                                                               
         SPACE 1                                                                
VVX      XIT1                                                                   
         SPACE 2                                                                
*              TABLE TO DETERMINE WHICH COMMERCIAL RECORD THE                   
*              VERSION CODE IS ON                                               
         SPACE 1                                                                
VERINDEX DC    X'1A',AL1(TLCOV026)                                              
         DC    X'78',AL1(TLCOV120)                                              
         DC    X'D2',AL1(TLCOV210)                                              
         DC    X'FA',AL1(TLCOV250)                                              
         DC    X'FF'                                                            
         SPACE 2                                                                
VVFLDINV MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         GOTO1 EXIT,DMCB,0                                                      
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO VALIDATE TRACK INPUT                                  
         SPACE 1                                                                
VTRK     NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)            R2=A(TRACK SCREEN FIELD)                     
                                                                                
         CLI   5(R2),0                                                          
         JE    XIT                                                              
                                                                                
         USING TAMCD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAMCELQ                                                   
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
VT10     BRAS  RE,NEXTEL                                                        
         JNE   VTFLDINV                                                         
         CLC   TAMCTRK,8(R2)                                                    
         JNE   VT10                                                             
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
VTFLDINV MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         GOTO1 EXIT,DMCB,0                                                      
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO PROCESS VERSION AND TRACK FILTERS                     
         SPACE 1                                                                
VTFILT   NTR1  BASE=*,LABEL=*                                                   
         L     RE,0(R1)            RE=A(TAFNTYPE TO GET)                        
         L     R2,4(R1)            R2=A(VERSION/TRACK FILTER)                   
         SPACE 1                                                                
         CLI   0(R2),0             EXIT IF NO FILTER ENTERED                    
         BE    VTYES                                                            
         CLI   0(R2),C' '                                                       
         BE    VTYES                                                            
         SPACE 1                                                                
         USING TAFND,R4                                                         
         MVI   ELCODE,TAFNELQ                                                   
         GOTO1 GETL,DMCB,(1,(RE))  ERROR IF TAFN ELEMENT DOES NOT               
         BNE   VTNO                EXIST                                        
         L     R4,TGELEM                                                        
         LA    RF,TAFNNAME                                                      
         ZIC   RE,TAFNLEN                                                       
         SHI   RE,3                                                             
VTF10    CLC   0(1,R2),0(RF)       SKIP CAST MEMBER IF NOT ON                   
         BE    VTYES                 VERSION/TRACK                              
         SPACE 1                                                                
         LA    R3,FROMVER          IF PROCESSING VERSIONS                       
         CR    R3,R2                                                            
         BNE   VTF20                                                            
         CLI   0(RF),251           251 MEANS CAST IS ON ALL VERSIONS            
         BNE   VTF30                                                            
         B     VTYES                                                            
         SPACE 1                                                                
VTF20    CLI   0(RF),C'*'          IF PROCESSING TRACKS                         
         BE    VTYES               * MEANS ALL TRACKS                           
         SPACE 1                                                                
VTF30    LA    RF,1(RF)                                                         
         BCT   RE,VTF10                                                         
         B     VTNO                                                             
         DROP  R4                                                               
         SPACE 1                                                                
VTYES    XR    RC,RC                                                            
VTNO     LTR   RC,RC                                                            
VTXIT    XIT1                                                                   
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO PROCESS VERSION ELEMENT                               
         SPACE 1                                                                
PROVER   NTR1  BASE=*,LABEL=*                                                   
         MVI   ELCODE,TAVRELQ      DELETE VERSIONS ELEMENTS                     
         GOTO1 REMELEM                                                          
         SPACE 1                                                                
         BRAS  RE,ADDVEREL         ADD "TO" VERSION ELEMENT                     
         B     PVERX                                                            
         SPACE 2                                                                
*              ROUTINE TO ADD "TO" VERSION ELEMENT                              
         SPACE 1                                                                
ADDVEREL NTR1  BASE=*,LABEL=*                                                   
         CLI   TOVER,0                                                          
         BE    PVERX                                                            
         SPACE 1                                                                
         USING TAFND,R4                                                         
         LA    R4,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   TAFNEL,TAFNELQ                                                   
         MVI   TAFNLEN,TAFNLNQ+1                                                
         MVI   TAFNTYPE,TAFNTVER                                                
         MVC   TAFNNAME(1),TOVER                                                
         GOTO1 ADDELEM                                                          
PVERX    XIT1                                                                   
         DROP  R4                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE UNVERIFIES TARGET COMMERCIAL AFTER COPY                  
*              AND SET ANNOUNCER ONLY & LIFT BITS IN STATUS BYTE                
         SPACE 1                                                                
UPDCOMML NTR1  BASE=*,LABEL=*                                                   
         OC    NCOPIED,NCOPIED     DON'T BOTHER IF NOTHING COPIED               
         BZ    UCX                                                              
*                                                                               
         USING TLCAPD,R4                                                        
         MVI   CHGHLD,C'N'                                                      
         LA    R4,KEY              READ ALL HOLDING FEE KEYS FOR                
         XC    KEY,KEY             COMMERCIAL'S CAST TO SEE IF NEW              
         MVI   TLCAPCD,TLCAHCDQ    HOLDING FEE HAS TO BE GENERATED              
         MVC   TLCAHCOM,TGCOM                                                   
         GOTO1 HIGH                (SKIP COMMERCIAL POINTER)                    
UC10     GOTO1 SEQ                                                              
         CLC   KEY(TLCAHSRT-TLCAPD),KEYSAVE                                     
         BNE   UC20                                                             
         OC    TLCAHNXT,TLCAHNXT   IF NO CAST HAS RECEIVED HOLDING              
         BZ    UC10                FEE YET                                      
         CLC   TLCAHDTE,TLCAHNXT   OR LATEST NOTICE WAS PAID                    
         BH    UC10                DO NOT TURN ON STATUS                        
         MVI   CHGHLD,C'Y'         ELSE, TURN ON STATUS                         
         DROP  R4                                                               
         SPACE 1                                                                
UC20     MVC   KEY+TLDRDA-TLDRD(4),COMDA  SET D/A OF NEW COMMERCIAL             
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC              READ RECORD FOR UPDATE                       
         SPACE 1                                                                
         MVI   ELCODE,TACOELQ      GET COMML DETAILS EL.                        
         L     R4,AIO                                                           
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACOD,R4                                                         
         XC    TACOVDTE,TACOVDTE   CLEAR VERIFIED DATE                          
         XC    TACOVTIM,TACOVTIM         VERIFIED TIME                          
         XC    TACOVSTU,TACOVSTU         USER ID                                
         XC    TACOVST,TACOVST           STAFF                                  
         MVI   TACOUVST,0                UNVERIFICATION STATUS                  
         SPACE 1                                                                
         CLC   NUMANN,=F'1'        IF ANNOUNCER IS ONLY NON-MUSICIAN            
         BNE   UC30                                                             
         CLC   NUMNONM,=F'1'                                                    
         BNE   UC30                                                             
         OI    TACOSTA2,TACOSANO   THEN SET BIT                                 
         B     UC40                                                             
         SPACE 1                                                                
UC30     NI    TACOSTA2,ALL-TACOSANO                                            
         SPACE 1                                                                
UC40     OC    NUMLIFT,NUMLIFT     ARE THERE CAST MEMBERS ON LIFT               
         BZ    UC50                                                             
         OI    TACOSTA2,TACOSLFT   THEN SET BIT                                 
         B     UC60                                                             
         SPACE 1                                                                
UC50     NI    TACOSTA2,ALL-TACOSLFT                                            
         SPACE 1                                                                
UC60     CLI   CHGHLD,C'Y'         IF NEED TO GENERATE UPDATED                  
         BNE   UC70                HOLDING FEE                                  
         OI    TACOSTA2,TACOCHHF   TURN ON STATUS                               
         GOTOR SNDMQHFR,DMCB,(PRGSTAT,TGCOM),(TGSYSTA2,HEXOUT),MQIO             
         DROP  R4                                                               
         SPACE 1                                                                
         USING TANUD,R4                                                         
UC70     MVI   ELCODE,TANUELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TANUTSEQ))                                     
         BE    *+6                                                              
         DC    H'00'                                                            
         L     R4,TGELEM                                                        
         MVC   TANUNXTC,CASTSEQ                                                 
         DROP  R4                                                               
         SPACE 1                                                                
         GOTO1 ACTVIN,DMCB,0       LAST CHANGED                                 
         SPACE 1                                                                
         GOTO1 PUTREC              WRITE BACK COMMERCIAL                        
         SPACE 1                                                                
UCX      XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE PROCESSES OVERSCALE AMOUNT/PERCENTAGE ELEMENTS       *         
*        ON ENTRY ... R3 = A(CAST RECORD)                             *         
***********************************************************************         
                                                                                
PROOAP   NTR1  BASE=*,LABEL=*                                                   
         GOTO1 MEDVAL,DMCB,TOMED                                                
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   TGCTEQU,TOTYPE                                                   
                                                                                
         GOTO1 UPDOAP,DMCB,('TAOAELQ',(R3))                                     
         GOTO1 (RF),(R1),('TAOPELQ',(R3))                                       
         GOTO1 (RF),(R1),('TAO2ELQ',(R3))                                       
         J     XIT                                                              
                                                                                
       ++INCLUDE TAUPDOAP                                                       
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        CLEAR BOTTOM SCREEN - COMMERCIAL COPY - CAST SELECT          *         
***********************************************************************         
                                                                                
CLRSCR   NTR1  BASE=*,LABEL=*                                                   
         TWAXC SCASELH,SCALSTH,PROT=Y      CLEAR SCREEN                         
         OI    SCATTL1H+1,X'0C'    TURN HEADINGS TO LOW INTENSITY               
         OI    SCATTL1H+6,X'80'                                                 
         OI    SCATTL2H+1,X'0C'                                                 
         OI    SCATTL2H+6,X'80'                                                 
         OI    SCANSELH+1,X'0C'                                                 
         OI    SCANSELH+6,X'80'                                                 
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO CHECK IF THE CAST IS MUSICIAN                         
         SPACE 1                                                                
CHKMUS   NTR1  BASE=*,LABEL=*                                                   
         USING TLCAD,R4                                                         
         L     R4,TIAREC                                                        
         TM    TLCASTAT,X'80'    CAST RELEASED STATUS ELEMENT                   
         JO    YES                                                              
         J     NO                                                               
         DROP  R4                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO CHECK IF THE CAST WAS RELEASED                        
         SPACE 1                                                                
CHKREL   NTR1  BASE=*,LABEL=*                                                   
         USING TARLD,R4                                                         
         L     R4,TIAREC                                                        
         MVI   ELCODE,TARLELQ    CAST RELEASED STATUS ELEMENT                   
         BRAS  RE,GETEL                                                         
         JNE   NO                                                               
         CLI   TARLSTAT,C' '     RELEASED, IF STATUS = A,B,C,D                  
         JH    YES                                                              
         J     NO                                                               
         DROP  R4                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
* THIS ROUTINE INSPECTS THE ACTIVE KEY AND DETERMINES HOW ITS                   
* CATEGORY WILL EFFECT THE ANNOUNCER-ONLY-NON-MUSICIAN STATUS FOR               
* THE COMMERCIAL.                                                               
*                                                                               
TESTANN  NTR1  BASE=*,LABEL=*                                                   
         LA    R3,KEY              R3 = A(CAST KEY)                             
         USING TLCAD,R3                                                         
         GOTO1 CATVAL,DMCB,TLCACAT                                              
*                                                                               
         CLI   TGCAEQU,CTANN       IF CAST MEMBER IS AN ANNOUNCER               
         JNE   *+16                                                             
         L     R1,NUMANN                                                        
         LA    R1,1(R1)            INCREMENT NUMBER OF ANNOUNCERS               
         ST    R1,NUMANN                                                        
*                                                                               
         GOTO1 UNITEST,DMCB,TGCAUNIS,AFM,0,0,0                                  
         JO    *+16                                                             
         L     R1,NUMNONM          THEN INC NUMBER OF NON-MUSICIANS             
         LA    R1,1(R1)                                                         
         ST    R1,NUMNONM                                                       
         J     XIT                                                              
                                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAMQHFR                                                        
*              DSECT TO COVER TABLE THAT DETERMINES WHICH COMMERCIAL            
*              RECORD THE VERSION CODE IS ON                                    
         SPACE 1                                                                
VINDEXD  DSECT                                                                  
VINDUPLM DS    X                 RECORD'S UPPER LIMIT                           
VINDEQUT DS    X                 RECORD'S EQUATE                                
VINDLNQ  EQU   *-VINDEXD                                                        
         EJECT                                                                  
*              DSECT TO COVER CAST TABLE                                        
         SPACE 1                                                                
CASTD    DSECT                                                                  
CASTSSN  DS    F                   SOCIAL SECURITY NUMBER                       
CASTCAT  DS    CL3                 CATEGORY                                     
CASTLNQ  EQU   *-CASTD                                                          
CASTNEXT EQU   *                                                                
         SPACE 2                                                                
*        DSECT TO COVER LIST LINE                                               
LLINED   DSECT                                                                  
CASSN    DS    CL9                 SS NUMBER                                    
         DS    CL1                                                              
CALNAME  DS    CL16                PERFORMER NAME                               
         DS    CL1                                                              
CACAT    DS    CL3                 CATEGORY                                     
         DS    CL1                                                              
CACAM    DS    CL3                 ON/OFF CAMERA                                
         DS    CL1                                                              
CATAX    DS    CL3                 TAX UNIT                                     
         DS    CL1                                                              
CAAGNT   DS    CL4                 AGENT                                        
         DS    CL1                                                              
CAUNI    DS    CL3                 UNION                                        
         DS    CL1                                                              
CALCL    DS    CL3                 LOCAL                                        
         DS    CL1                                                              
CAYR     DS    CL2                 CONTRACT YEAR                                
         DS    CL1                                                              
CALIFT   DS    CL1                 LIFT                                         
         DS    CL2                                                              
CACORP   DS    CL1                 CORP CODE                                    
         EJECT                                                                  
*              DSECT TO COVER LOCAL W/S                                         
         SPACE 1                                                                
LWSD     DSECT                                                                  
LOCALD   EQU   *                                                                
STATUS   DS    XL1                 STATUS - SEE EQUATES                         
DUPOK    EQU   X'80'               OK TO COPY DUPLICATES                        
DUPONLY  EQU   X'40'               COPY DUPLICATES ONLY                         
REREAD   EQU   X'20'               NEED TO RE-READ SYSIO'S KEY                  
SELCAST  EQU   X'10'               SELECT CAST                                  
*                                                                               
CHGHLD   DS    XL1                 TURN ON CHANGED SINCE LAST HF STAT?          
*                                                                               
FROMCID  DS    CL(L'TGCID)         'FROM' COMMERCIAL ID                         
COMDA    DS    XL4                 D/A OF 'TO' COMMERCIAL                       
CASTSEQ  DS    H                   CAST INPUT SEQUENCE NUMBER                   
PTRBLK   DS    CL(L'TLDRREC+1)     CAST PTRS (REQ MINIMUM SINCE ADDING)         
*                                                                               
FROMVER  DS    X                   'FROM' VERSION                               
TOVER    DS    X                   'TO' VERSION                                 
*                                                                               
FROMMED  DS    X                   'FROM' MEDIA                                 
TOMED    DS    X                   'TO' MEDIA                                   
*                                                                               
FROMTYPE DS    X                   'FROM' TYPE                                  
TOTYPE   DS    X                   'TO' TYPE                                    
*                                                                               
ACASTREC DS    A                   A(CAST RECORD)                               
OLENGTH  DS    X                   ORIGINAL ELEMENT LENGTH                      
*                                                                               
NCOPIED  DS    H                   N'RECORDS COPIED                             
NDUPS    DS    H                   N'DUPLICATES COPIED                          
*                                                                               
FILTERS  DS    0CL147                                                           
FLTUNIS  DS    (NUNIS)CL3          UNIONS                                       
NUNIS    EQU   8                                                                
FLTCATS  DS    (NCATS)CL3          CATEGORIES                                   
NCATS    EQU   40                                                               
FLTONOF  DS    CL3                 ON/OFF CAMERA FILTER                         
*                                                                               
FRSTSERV DS    PL3                 NEW FIRST SERVICE DATE                       
FRSTFCYC DS    PL3                 NEW FIRST FIXED CYCLE DATE                   
*                                                                               
EXPDATE  DS    PL3                 NEW EXPIRATION DATE                          
*                                                                               
         SPACE 1                                                                
COUNTER  DS    X                   COUNTER - LINES PER PAGE                     
MYNAMEH  DS    CL8                 FAKE HEADER                                  
MYNAME   DS    CL16                                                             
MYSTAT   DS    XL2                 STATUS BYTES OF KEY                          
APLINE   DS    A                   A(NEXT LINE TO LIST)                         
DATABLE  DS    9F                  D/A TABLE                                    
NUMANN   DS    F                   N'ANNOUNCERS ON CAST                         
NUMNONM  DS    F                   N'NON MUSICIANS ON CAST                      
NUMLIFT  DS    F                   N'PERFORMENRS ON LIFT                        
         SPACE 1                                                                
BSSN     DS    F                   BINARY SSN                                   
CAT      DS    CL3                 CATEGORY                                     
         SPACE 1                                                                
NSEL     DS    X                   NUMBER OF CAST MEMBERS SELECTED              
LISTED   DS    C                   CAST LISTED ALREADY                          
LSTPAGE  DS    C                   SELECT FROM LAST PAGE                        
SELTAB   DS    200F                SELECTED D/A TABLE                           
STABLNQ  EQU   *-SELTAB                                                         
         SPACE 1                                                                
EXTTAB   DS    300CL(CASTLNQ)      TABLE OF EXISTING CAST MEMBERS               
EXTTLNQ  EQU   *-EXTTAB                                                         
FAYSTA6  DS    X                                                                
FCISTA2  DS    X                                                                
TAYSTA6  DS    X                                                                
TCISTA2  DS    X                                                                
TOCOM    DS    XL(L'TGCOM)                                                      
         SPACE 1                                                                
LOCALLNQ EQU   *-LOCALD                                                         
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR80D                                                       
         EJECT                                                                  
* TASYSIOD     (MUST FOLLOW LAST SCREEN)                                        
* DDGENTWA     (MUST FOLLOW LAST SCREEN)                                        
* TAGENWORKD                                                                    
* TASYSDSECT                                                                    
* TASYSEQUS                                                                     
* TAGENEQUS                                                                     
* DDSPLWORKD                                                                    
* DDSPOOLD                                                                      
* TAGENFILE                                                                     
* FAGETTXTD                                                                     
       ++INCLUDE TASYSIOD                                                       
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAGENEQUS                                                      
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'045TAGEN80   05/29/15'                                      
         END                                                                    
