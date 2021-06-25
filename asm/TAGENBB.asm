*          DATA SET TAGENBB    AT LEVEL 051 AS OF 04/28/15                      
*PHASE T702BBA                                                                  
         TITLE 'T702BB - PRINT CAST MAINTENANCE'                                
T702BB   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T702BB,R6                                                      
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         LA    R7,TWAHOLE          R7=PROGRAM SAVED STORAGE                     
         USING CASTD,R7                                                         
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
*                                                                               
         GOTO1 INITIAL,DMCB,PFTAB  INITIALIZE                                   
*                                                                               
         MVC   SCASHED(7),=C'Pid Num'                                           
         OI    SCASHEDH+6,X'80'                                                 
         LA    RE,DLIO                                                          
         AHI   RE,L'DLIO                                                        
         ST    RE,AW4IO                                                         
*                                                                               
         CLI   MODE,SETFILE        GUAR. TO GET THIS MODE DURING SELECT         
         BNE   CST10                                                            
         CLI   ACTNUM,ACTSEL       IF ACTION IS SELECT                          
         BNE   CSTX                                                             
         CLI   THISLSEL,CHASELQ    AND SELECTED FOR CHANGE                      
         BNE   CSTX                                                             
         L     RE,SYSPARMS                                                      
         L     RE,0(RE)                                                         
         USING TIOBD,RE            RE = A(TRANSLATOR I/O BLOCK)                 
         LA    R1,SCAAGTH                                                       
         SR    R1,RA                                                            
         STH   R1,TIOBLAST         FORCE GENCON TO THINK A FLD WAS I/P          
         B     CSTX                                                             
*                                                                               
CST10    CLI   MODE,VALKEY         VALIDATE KEY                                 
         BNE   *+12                                                             
         BAS   RE,VKEY                                                          
         B     CSTX                                                             
*                                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BNE   *+12                                                             
         BAS   RE,DKEY                                                          
         B     CSTX                                                             
*                                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    CST20                                                            
         CLI   MODE,RECDEL         DELETE RECORD                                
         BE    CST20                                                            
         CLI   MODE,RECREST        OR RESTORE RECORD                            
         BNE   CST30                                                            
CST20    GOTO1 SAVPTRS,DMCB,PTRBLK SAVE PASSIVE PTRS                            
*                                                                               
         CLI   MODE,VALREC         IF VALIDATE RECORD                           
         BNE   *+8                                                              
         BAS   RE,BLDREC           BUILD THE RECORD                             
         B     CSTX                                                             
*                                                                               
CST30    CLI   MODE,XRECADD        IF RECORD ADDED                              
         BE    CST40                                                            
         CLI   MODE,XRECPUT        OR RECORD CHANGED                            
         BE    CST40                                                            
         CLI   MODE,XRECREST       OR RECORD RESTORED                           
         BE    CST40                                                            
         CLI   MODE,XRECDEL        OR RECORD DELETED                            
         BNE   CST50                                                            
CST40    GOTO1 ADDPTRS,DMCB,PTRBLK UPDATE PASSIVE POINTERS                      
*                                                                               
         CLI   MODE,XRECPUT        IF RECORD CHANGED                            
         BNE   CST44                                                            
         CLI   ACTNUM,ACTSEL       AND ACTION IS SELECT                         
         BE    *+12                                                             
         CLI   TWALACT,ACTSEL      OR LAST ACTION WAS SELECT                    
         BNE   CST44                                                            
         CLC   THISPG,NPAGES       AND THIS WAS LAST PAGE                       
         BNE   CST44                                                            
         GOTO1 FLDVAL,DMCB,(X'40',AFRSTREC),999  AND NOTHING CHANGED            
         BE    CSTX                THEN DON'T RE-DISPLAY                        
*                                                                               
CST44    BAS   RE,DISPLAY          RE-DISPLAY THE RECORD                        
*                                                                               
         CLI   MODE,XRECADD        IF RECORD ADDED                              
         BNE   CST45                                                            
         BRAS  RE,UPNXTSEQ         UPDATE COMML'S NEXT CAST SEQ NUMBER          
*                                                                               
CST45    CLI   MODE,XRECPUT        TEST RECORD CHANGED                          
         BNE   CSTX                                                             
         CLI   ACTNUM,ACTCHA       IF ACTION IS CHANGE                          
         BNE   CST46                                                            
         CLI   TWALACT,ACTSEL      AND LAST ACTION WAS SELECT                   
         BNE   CST46                                                            
         MVC   CONACT(6),=C'SELECT'  RETURN TO SELECT NEXT TIME                 
CST46    B     PGDSPMS2            GIVE MY OWN MESSAGE                          
*                                                                               
CST50    CLI   MODE,DISPREC        IF MODE IS DISPLAY                           
         BNE   CSTX                                                             
         CLI   THISLSEL,C'D'       AND SELECTED FOR DELETE                      
         BE    CSTX                THEN DON'T DISPLAY YET                       
         BAS   RE,DISPLAY          ELSE DISPLAY THE RECORD                      
*                                                                               
         CLI   ACTNUM,ACTDIS       IF ACTION IS DISPLAY                         
         BE    PGDSPMSG            GIVE MY OWN MESSAGE                          
         CLI   ACTNUM,ACTCHA       IF ACTION IS CHANGE                          
         BE    PGDSPMSG            GIVE MY OWN MESSAGE                          
         CLI   ACTNUM,ACTSEL       IF ACTION IS SELECT                          
         BNE   CSTX                                                             
         CLC   THISPG,NPAGES       AND THIS IS NOT LAST PAGE                    
         BNE   PGDSPMSG            GIVE MY OWN MESSAGE                          
*                                                                               
CSTX     B     XIT                                                              
         SPACE 3                                                                
         GETEL R3,DATADISP,ELCODE                                               
         EJECT                                                                  
*              ROUTINE TO VALIDATE THE KEY                                      
*                                                                               
VKEY     NTR1                                                                   
         CLI   SCRSTAT,0           IF SCREEN CHANGED                            
         BE    *+8                                                              
         NI    SCAAGYH+4,X'DF'     SET AGENCY FIELD CHANGED                     
*                                                                               
         TM    SCAAGYH+4,X'20'     IF AGENCY CHANGED                            
         BO    VK10                                                             
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'08',SCAAGYH),SCAAGYNH  AGENCY             
*                                                                               
VK10     LA    R2,SCACIDH                                                       
         TM    4(R2),X'20'         IF COMMERCIAL CHANGED                        
         BO    *+8                                                              
         BAS   RE,VALCID           VALIDATE COMMERCIAL ID                       
*                                                                               
         LA    R2,SCASSNH                                                       
         TM    4(R2),X'20'         IF S/S NUMBER CHANGED                        
         BO    VK30                                                             
         L     R1,AW4IO                                                         
         ST    R1,AIO                                                           
         CLI   SCASSNH+5,6                                                      
         BH    VK15                                                             
         MVC   TGPID,SCASSN                                                     
         GOTO1 SSNUNPK,DMCB,TGPID,TGSSN                                         
         BNE   VK15                                                             
         MVC   SCASSN,TGSSN                                                     
         MVI   SCASSNH+5,9                                                      
VK15     GOTO1 RECVAL,DMCB,TLW4CDQ,(X'08',(R2)),SCASSNNH  S/S NUMBER            
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   SCASSN,SPACES                                                    
         MVC   SCASSN(L'TGPID),TGPID                                            
         MVI   SCASSNH+5,6                                                      
         OI    SCASSNH+6,X'80'                                                  
*                                                                               
VK18     MVC   AIO,AIO1                                                         
         L     R3,AW4IO                                                         
         USING TAW4D,R3                                                         
         MVI   ELCODE,TAW4ELQ                                                   
         BAS   RE,GETEL            GET W4 ELEMENT                               
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    TAW4STA2,TAW4SLCK   IF RECORD IS LOCKED                          
         BNO   VK30                                                             
         CLI   ACTNUM,ACTADD       & ADDING                                     
         BNE   VK20                                                             
         NI    4(R2),X'DF'         UNVALIDATE                                   
         B     ERW4LCK             EXIT WITH ERROR MESSAGE                      
*                                                                               
VK20     MVC   SCASSNN(16),=C'** W4 LOCKED ** '                                 
*                                                                               
VK30     CLI   TAW4TYPE,TAW4TYTR   IF THIS W4 RECORD IS TRUSTEE                 
         BNE   VK31                                                             
         CLI   ACTNUM,ACTADD       & ADDING                                     
         BNE   VK31                                                             
         NI    4(R2),X'DF'         UNVALIDATE                                   
         B     ERNOTRS             EXIT WITH ERROR MESSAGE                      
*                                                                               
VK31     LA    R2,SCACATH                                                       
         TM    4(R2),X'20'         IF CATEGORY CHANGED                          
         BO    VK40                                                             
         OC    8(3,R2),SPACES                                                   
         CLI   5(R2),0             AND NOTHING INPUT                            
         BNE   VK34                                                             
         OC    TGCAT,TGCAT         AND THERE'S NO GLOBAL CATEGORY               
         BZ    FLDMISS             THEN REQUIRE INPUT                           
         MVC   8(3,R2),TGCAT       ELSE USE GLOBAL CATEGORY                     
         OI    6(R2),X'80'                                                      
VK34     GOTO1 CATVAL,DMCB,8(R2)   VALIDATE CATEGORY CODE                       
         BNE   FLDINV                                                           
         CLI   TGCAEQU,CTZZ        ALLOW ZZ AND                                 
         BE    VK35                                                             
         CLI   TGCAEQU,CTZZZ       ALLOW ZZZ                                    
         BE    VK35                                                             
         OC    TGCAUNIS,TGCAUNIS                                                
         BNZ   FLDINV              ONLY VALID IF NO UNIONS DEFINED              
VK35     OI    4(R2),X'20'         SET PREV. VALIDATED                          
*                                                                               
         XC    DLS,DLS             CLEAR LAST EL. KEYS                          
*                                                                               
VK40     XC    TGCSORT,TGCSORT                CLEAR CAST SORT KEY               
         GOTO1 RECVAL,DMCB,TLCACDQ,(X'40',0)  BUILD KEY FOR CAST                
*                                                                               
         CLI   ACTNUM,ACTADD       ONLY FOR ACTION ADD                          
         BNE   XIT                                                              
         LA    R2,SCASSNH                                                       
         BRAS  RE,CHKMODL          CHK MODEL/CAT NOT IN COMML ALREADY           
         BNE   ERMDLXST            MODEL/CAST ALREADY EXIST                     
         BRAS  RE,STNXTSEQ         SET NEXT CAST SEQ NUMBER IN KEY              
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE VALIDATES COMMERCIAL ID                                  
*                                                                               
VALCID   NTR1                                                                   
         LA    R2,SCACIDH                                                       
         GOTO1 RECVAL,DMCB,TLCOICDQ,(X'08',(R2)),SCACIDNH  READ RECORD          
*                                                                               
         TM    TGMEEQU,PRINT       ENSURE THIS IS PRINT COMMERCIAL              
         BZ    BADCTYPE                                                         
*                                                                               
         L     R4,AIO              R4=A(COMMERCIAL RECORD)                      
         USING TLCOD,R4                                                         
         MVC   TGCLI,TLCOCLI       SAVE GLOBAL CLIENT                           
         MVC   TGCOM,TLCOCOM       SAVE GLOBAL INTERNAL COMMERCIAL NO.          
*                                                                               
         GOTO1 CHAROUT,DMCB,TACMELQ,SCACCMTH,TACMTYPG  DSPLY COMML CMNT         
*                                                                               
         XC    COMLSDTE,COMLSDTE   CLEAR COMMERCIAL SHOOT DATE                  
         MVI   ELCODE,TACSELQ      GET COMMERCIAL STUDIO ELEMENT                
         GOTO1 GETL,DMCB,(1,=AL1(TACSTYPS))                                     
         BNE   VCID20                                                           
         L     R3,TGELEM                                                        
         USING TACSD,R3                                                         
         MVC   COMLSDTE,TACSDATE   SAVE COMMERCIAL SHOOT DATE                   
*                                                                               
VCID20   GOTO1 CHAROUT,DMCB,TANUELQ,0,TANUTPHO  GET PHOTOGRAPHER SSN            
         MVC   COMLPHOT,TGNAME                                                  
*                                                                               
         L     R0,AIO                                                           
         LA    R1,DLIO             READ DEAL RECORD INTO LOCAL STORAGE          
         ST    R1,AIO                                                           
         GOTO1 RECVAL,DMCB,TLDLCDQ,(X'20',0)                                    
         ST    R0,AIO                                                           
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO DISPLAY THE KEY                                       
*                                                                               
DKEY     NTR1                                                                   
         CLC   SVKEY,KEY           IF KEY CHANGED                               
         BE    *+10                                                             
         XC    DLS,DLS             CLEAR LAST EL. KEYS                          
*                                                                               
         MVC   SVKEY,KEY           SAVE KEY                                     
         MVC   AIO,AIO2            AND I/O AREA                                 
*                                                                               
         MVC   SCAAGY,TGAGY        AGENCY IS IN GLOBAL W/S                      
         OI    SCAAGYH+6,X'80'                                                  
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'08',0),SCAAGYNH   AGENCY NAME             
*                                                                               
         MVC   SCACID,TGCID        COMML ID IS IN GLOBAL W/S                    
         MVI   SCACIDH+5,L'TGCID                                                
         OI    SCACIDH+6,X'80'                                                  
         BAS   RE,VALCID           VALIDATE COMMERCIAL ID                       
*                                                                               
         L     R4,AIO1             R4=A(CAST RECORD)                            
         USING TLCAD,R4                                                         
*                                                                               
         MVC   SCASSN,TLCASSN      S/S NUMBER IS IN CAST RECORD                 
         MVI   SCASSNH+5,L'TLCASSN                                              
         OI    SCASSNH+6,X'80'                                                  
         L     R1,AW4IO                                                         
         ST    R1,AIO                                                           
         LA    R2,SCASSNH                                                       
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'08',(R2)),SCASSNNH  SSN NAME              
         GOTO1 SSNPACK,DMCB,TLCASSN,TGPID                                       
         MVC   SCASSN,SPACES                                                    
         MVC   SCASSN(L'TGPID),TGPID                                            
         MVI   SCASSNH+5,6                                                      
         OI    SCASSNH+6,X'80'                                                  
*                                                                               
DK05     MVC   AIO,AIO2                                                         
         L     R3,AW4IO                                                         
         USING TAW4D,R3                                                         
         MVI   ELCODE,TAW4ELQ                                                   
         BAS   RE,GETEL            GET W4 ELEMENT                               
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    TAW4STA2,TAW4SLCK   IF RECORD IS LOCKED                          
         BNO   DK10                                                             
         MVC   SCASSNN(16),=C'** W4 LOCKED ** '                                 
*                                                                               
DK10     MVC   SCACAT,TLCACAT      CATEGORY IS IN CAST RECORD                   
         OI    SCACATH+6,X'80'                                                  
         OI    SCACATH+4,X'20'                                                  
*                                                                               
         MVC   KEY,SVKEY           RESTORE KEY                                  
         MVC   AIO,AIO1            AND I/O AREA                                 
         B     XIT                                                              
         EJECT                                                                  
*              BUILD THE RECORD                                                 
*                                                                               
BLDREC   NTR1                                                                   
         MVC   SVKEY,KEY                                                        
*                                                                               
BLD10    MVI   ELCODE,TACAELQ      REMOVE EXISTING CAST DETAILS EL.             
         GOTO1 REMELEM                                                          
*                                                                               
         USING TACAD,R3                                                         
         XC    ELEMENT,ELEMENT     BUILD NEW CAST DETAILS ELEMENT               
         LA    R3,ELEMENT                                                       
         MVI   TACAEL,TACAELQ      ELEMENT CODE                                 
         MVI   TACALEN,TACALNQ     ELEMENT LENGTH                               
*                                                                               
         USING TAAND,R3                                                         
         CLI   SCAAGTH+5,0         AGENT CODE (OPTIONAL)                        
         BE    BLD20                                                            
         LA    R2,SCAAGTH                                                       
         MVC   AIO,AIO2            DON'T CREAM RECORD AT AIO                    
         GOTO1 RECVAL,DMCB,TLANCDQ,(X'20',(R2))                                 
         L     R3,AIO                                                           
         MVC   AIO,AIO1            RESTORE AIO                                  
         CLI   SCAAGTFH+5,0        TEST HAVE AGENT FEE                          
         BE    BLD15                                                            
         SPACE                                                                  
         MVI   ELCODE,TAANELQ      GET AGENT ELEMENT                            
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         OC    TAANSSN,TAANSSN     MUST HAVE SSN                                
         BZ    NOSSN                                                            
         SPACE                                                                  
         USING TACAD,R3                                                         
BLD15    LA    R3,ELEMENT                                                       
         GOTO1 TRNSAGT,DMCB,(X'80',TGAGT),TACANCDE                              
*                                                                               
BLD20    LA    R2,SCATAXH          TAX UNIT                                     
         GOTO1 ANY                                                              
         GOTO1 TAXVAL,DMCB,(3,WORK)                                             
         BE    BLD20A                                                           
         MVC   TGCTRY,=C'CA'                                                    
         GOTO1 TAXVAL,DMCB,(X'FF',WORK)                                         
         BNE   FLDINV                                                           
BLD20A   TM    TGTASTAT,TASUINGF   ENSURE TAX UNIT IS VALID GOING               
         BO    FLDINV                                                           
         MVC   TACAUNIT,WORK                                                    
*                                                                               
         BAS   RE,VALCORP          VALIDATE CORP FIELD                          
*                                                                               
         LA    R2,SCAPAYEH         PAYEE CODE                                   
         GOTO1 ANY                                                              
         CLI   8(R2),TACAPMOD      ALLOW MODEL                                  
         BE    BLD30                                                            
         CLI   8(R2),TACAPAGT      IF AGENT                                     
         BE    *+12                                                             
         CLI   8(R2),TACAPCOM      OR COMMISSION ONLY TO AGENT                  
         BNE   *+16                                                             
         CLI   SCAAGTH+5,0         THEN AGENT MUST BE INPUT                     
         BE    AGTMISS                                                          
         B     BLD30                                                            
         CLI   8(R2),TACAPPHO      IF PHOTOGRAPHER                              
         BNE   FLDINV                                                           
         CLC   COMLPHOT,SPACES     THEN PHOTOGRAPHER MUST BE DEFINED            
         BE    ERRPHO                                                           
BLD30    MVC   TACAPAYE,8(R2)                                                   
*                                                                               
         LA    R2,SCAEBASH         EXPIRATION DATE BASIS                        
         MVI   TACAEBAS,TACAESHT   DEFAULT TO SHOOT DATE                        
         CLI   5(R2),0                                                          
         BE    BLD40                                                            
         CLI   8(R2),TACAESHT      ALLOW SHOOT DATE                             
         BE    BLD40                                                            
         CLI   8(R2),TACAEPUB            PUBLICATION DATE                       
         BNE   FLDINV                                                           
         MVC   TACAEBAS,8(R2)                                                   
*                                                                               
BLD40    LA    R2,SCAAGTFH         AGENT FEE RATE                               
         CLI   5(R2),0             IF HAVE INPUT                                
         BE    BLD50                                                            
         BAS   RE,AMTVAL           VALIDATE IT                                  
         CLC   FULL,=F'9999'                                                    
         BH    FLDINV                                                           
         MVC   TACARATE,FULL+2                                                  
         CLI   SCAAGTH+5,0         THEN AGENT MUST BE INPUT                     
         BE    AGTMISS                                                          
*                                                                               
BLD50    LA    R2,SCASDTEH         SHOOT DATE                                   
         XC    CASTSDTE,CASTSDTE                                                
         CLI   5(R2),0             IF FIELD IS EMPTY                            
         BNE   *+14                                                             
         OC    COMLSDTE,COMLSDTE   AND COMML SHOOT DATE DEFINED                 
         BNZ   BLD60               THEN SKIP VALIDATION                         
         GOTO1 DTVAL,DMCB,TACASDTE                                              
         MVC   CASTSDTE,TACASDTE   SAVE CAST SHOOT DATE                         
*                                                                               
         BAS   RE,VALSTAT          VALIDATE STATUS                              
*                                                                               
BLD60    GOTO1 ADDELEM             ADD CAST DETAILS ELEMENT                     
*                                                                               
         GOTO1 NAMIN,DMCB,TACMELQ,(X'80',SCACMNTH),TACMTYPG  COMMENT            
*                                                                               
         BAS   RE,VALDEAL          VALIDATE DEAL LINES                          
*                                                                               
         BAS   RE,ALLDEAL          INSURE ALL DEALS ACCOUNTED FOR               
*                                                                               
         GOTO1 ACTVIN,DMCB,0       LAST CHANGED                                 
*                                                                               
         MVC   KEY,SVKEY           RESTORE CAST KEY                             
*                                                                               
         CLI   ACTNUM,ACTADD       IF NOT ADDING                                
         BE    BLDX                                                             
         CLI   SCAAGTH+5,0         IF THERE WAS AGENT INPUT                     
         BNE   BLD70                                                            
         CLI   SCACRPH+5,0         OR CORP INPUT                                
         BE    BLDX                                                             
BLD70    MVC   AIO,AIO2            DON'T CREAM CAST REC IN AIO1                 
         GOTO1 GETREC              GET CAST REC BECAUSE READ AGENT REC          
         MVC   AIO,AIO1            RESTORE AIO                                  
BLDX     B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE CORP FIELD                                   
*                                                                               
         USING TACAD,R3            R3=A(CAST ELEMENT)                           
VALCORP  NTR1                                                                   
         LA    R2,SCACRPH          R2=A(CORPORATION FIELD)                      
         CLI   5(R2),0                                                          
         BE    VCRPX                                                            
         MVC   TACACORP,8(R2)      SAVE IN ELEMENT                              
*                                                                               
         CLI   TACACORP,C'Y'       IF FIELD HAD 'Y'                             
         BNE   *+8                                                              
         MVI   TACACORP,C'1'       THEN SET TO '1' IN ELEMENT                   
*                                                                               
         L     R1,AW4IO            SEARCH W4 RECORD FOR TAX UNIT EL             
         ST    R1,AIO                                                           
         MVI   ELCODE,TATIELQ                                                   
         MVI   HALF,TATITYCO       BUILD GETL SEARCH ARGUMENT                   
         MVC   HALF+1(1),TACACORP                                               
         GOTO1 GETL,DMCB,(2,HALF)                                               
         BNE   FLDINV              ERROR IF NOT FOUND                           
*                                                                               
         MVC   AIO,AIO1            RESTORE AIO TO AIO1                          
*                                                                               
         CLI   8(R2),C'Y'          IF 'Y' WAS INPUT                             
         BNE   VCRPX                                                            
         MVI   ELCODE,TATIELQ                                                   
         L     R3,TGELEM                                                        
         BAS   RE,NEXTEL           SEARCH FOR ADDL CORPS ON W4 REC.             
         BE    ERRCRP              NEED PRECISE CORP CODE                       
*                                                                               
VCRPX    CLI   5(R2),0                                                          
         BE    XIT                                                              
         L     RF,TGELEM           SAVE VALUE OF TGELEM                         
         GOTOR W4LCKCRP,DMCB,TATIID-TATID(RF),AIO3,AIO1                         
         BE    XIT                                                              
         NI    4(R2),X'DF'         UNVALIDATE                                   
         B     ERCRPLK             EXIT WITH ERROR MESSAGE                      
         EJECT                                                                  
*        VALIDATE STATUS CODES FIELD                                            
*                                                                               
         USING TACAD,R3            R3=A(CAST ELEMENT)                           
VALSTAT  NTR1                                                                   
         NI    TACASTA2,X'FF'-TACASEUR                                          
         CLI   SCASTAH+5,0                                                      
         BE    XIT                                                              
*                                                                               
         LA    R2,SCASTAH          ELSE SCAN FIELD INTO SCANNER BLOCK           
         GOTO1 SCANNER,DMCB,(R2),(10,BLOCK),0                                   
         CLI   4(R1),0                                                          
         BE    FLDINV                                                           
*                                                                               
         USING SCAND,R4                                                         
         LA    R4,BLOCK            R4 = A(SCANNER BLOCK)                        
         ZIC   R5,4(R1)            R5 = NUMBER OF SCAN BLOCK ENTRIES            
         XR    R0,R0                                                            
*                                                                               
VSTAT10  CLI   SCLEN2,0            ERROR IF RHS EXISTS                          
         BNE   FLDINV                                                           
*                                                                               
         ZIC   RE,SCLEN1           IF MATCH THEN SET BIT                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   SCDATA1(0),EURO     IF PAYING PERFORMER IN EUROS                 
         BNE   FLDINV                                                           
         OI    TACASTA2,TACASEUR   SET STATUS BIT                               
*                                                                               
VRS40    ZIC   RF,SCLEN1           BUMP R0 TO NEXT STATUS CODE                  
         LA    RF,1(RF)                                                         
         AR    R0,RF                                                            
*                                                                               
         LA    R4,SCANNEXT         BUMP R4 TO NEXT SCANNER ENTRY                
         BCT   R5,VSTAT10                                                       
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              ROUTINE TO VALIDATE DEAL LINES                                   
*                                                                               
VALDEAL  NTR1                                                                   
         OC    DLS,DLS             IF SOMETHING ON SCREEN                       
         BZ    VDL30                                                            
         L     R3,AIO              SET TO DELETE CORRES. DEAL ELEMENTS          
         MVI   ELCODE,TADLELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TADLD,R3            R3=A(DEAL ELEMENT)                           
*                                                                               
VDL10    CLC   TADLAREA(6),DLFRST  IF ELEMENT IS WITHIN DISPLAYED RANGE         
         BL    VDL20                                                            
         CLC   TADLAREA(6),DLLAST                                               
         BH    VDL20                                                            
         MVI   TADLEL,X'FF'        SET TO DELETE                                
*                                                                               
VDL20    BAS   RE,NEXTEL                                                        
         BE    VDL10                                                            
         MVI   ELCODE,X'FF'        REMOVE MARKED ELEMENTS                       
         GOTO1 REMELEM                                                          
*                                                                               
VDL30    XC    TGAREA,TGAREA       CLEAR GLOBAL AREA CODE                       
*                                                                               
         LA    R4,SCAL1H           R4=A(1ST LINE)                               
         USING LINED,R4                                                         
         LA    R0,MXLINES          R0=N'LINES                                   
         LA    R3,ELEMENT                                                       
         USING TADLD,R3            R3=A(DEAL ELEMENT)                           
*                                                                               
VDL40    GOTO1 FLDVAL,DMCB,(X'80',LINAREAH),(X'80',LINEXPH)  IF BLANK           
         BE    VDL90                                         SKIP LINE          
*                                                                               
         XC    ELEMENT,ELEMENT     INITIALIZE NEW DEAL ELEMENT                  
         MVI   TADLEL,TADLELQ                                                   
         MVI   TADLLEN,TADLLNQ                                                  
*                                                                               
         LA    R2,LINAREAH         AREA                                         
         CLI   5(R2),0                                                          
         BNE   *+14                                                             
         OC    TGAREA,TGAREA                                                    
         BNZ   VDL50                                                            
         GOTO1 RECVAL,DMCB,TLARCDQ,LINAREAH                                     
VDL50    MVC   TADLAREA,TGAREA                                                  
*                                                                               
         LA    R2,LINUSEH          USE                                          
         GOTO1 ANY                                                              
         GOTO1 RECVAL,DMCB,TLUSCDQ,(R2)                                         
         MVC   TADLUSE,TGUSE                                                    
*                                                                               
         MVI   ELCODE,TADLELQ                                                   
         GOTO1 GETL,DMCB,(6,TADLAREA)  TEST IF THIS KEY ON REC ALREADY          
         BE    DUPDEAL                                                          
*                                                                               
         LA    R2,LINTERMH         TERM                                         
         CLC   8(2,R2),=C'UN'      IF UNLIMITED                                 
         BNE   VDL52                                                            
         MVI   TADLTERM,TADLTUNL   SET SPECIAL TERM                             
         B     VDL60                                                            
VDL52    CLC   8(2,R2),=C'1X'      IF ONE TIME                                  
         BNE   VDL54                                                            
         MVI   TADLTERM,TADLT1X    SET SPECIAL TERM                             
         B     VDL60                                                            
VDL54    CLI   8(R2),C' '                                                       
         BH    *+8                                                              
         MVI   8(R2),C'0'                                                       
         GOTO1 VALINUM                                                          
         MVC   TADLTERM,ACTUAL     SAVE N'MONTHS                                
*                                                                               
VDL60    LA    R2,LINAMTH          AMOUNT                                       
         BAS   RE,AMTVAL                                                        
         MVC   TADLAMT,FULL                                                     
*                                                                               
         LA    R2,LINRATEH         COMMISSION RATE                              
         CLI   5(R2),0                                                          
         BE    VDL70                                                            
         BAS   RE,AMTVAL                                                        
         CLC   FULL,=F'9999'                                                    
         BH    FLDINV                                                           
         MVC   TADLRATE,FULL+2     SAVE RATE                                    
*                                                                               
VDL70    LA    R2,LINPUBH          PUBLICATION DATE                             
         GOTO1 DTVAL,DMCB,(X'80',TADLPUB)                                       
*                                                                               
         BAS   RE,VALEXP           EXPIRATION DATE                              
*                                                                               
         GOTO1 ADDELEM             ADD ELEMENT TO RECORD                        
*                                                                               
VDL90    LA    R4,LINNEXT          BUMP TO NEXT LINE                            
         BCT   R0,VDL40            AND CONTINUE IF MORE                         
*                                                                               
         BAS   RE,BASISCHG         IF EXPIRATION CALC. BASIS CHANGED            
         BNE   *+8                                                              
         BAS   RE,CHGEXP           CHANGE ALL EXPIRATION DATES                  
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE VALIDATES AN AMOUNT                                      
*                                                                               
*                                  R2=A(FIELD HEADER)                           
AMTVAL   NTR1                                                                   
         ZIC   RF,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,8(R2),(RF)  VALIDATE FOR 2 DEC. PLACES              
         CLI   0(R1),X'FF'                                                      
         BE    FLDINV                                                           
         TM    4(R1),X'80'         DON'T ALLOW NEGATIVE                         
         BO    FLDINV                                                           
         MVC   FULL,4(R1)          RETURN AMOUNT IN FULL                        
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE VALIDATES EXPIRATION DATE FIELD                          
*                                                                               
         USING TADLD,R3            R3=A(DEAL ELEMENT)                           
         USING LINED,R4            R4=A(DEAL LINE)                              
VALEXP   NTR1                                                                   
         LA    R2,LINEXPH          R2=A(EXPIRATION DATE FIELD)                  
*                                                                               
         BAS   RE,BASISCHG         IF EXPIRATION CALC. BASIS CHANGED            
         BE    VEXPX               IGNORE INPUT - WILL PROCESS LATER            
*                                                                               
         CLI   5(R2),0             ELSE IF NOTHING INPUT                        
         BNE   *+12                                                             
         BAS   RE,CALCEXP          CALCULATE NEW EXPIRATION DATE                
         B     VEXPX                                                            
*                                                                               
         GOTO1 DTVAL,DMCB,TADLEXP  ELSE VALIDATE DATE INPUT                     
*                                                                               
VEXPX    B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE DETERMINES IF WE HAVE TO CHANGE ALL EXPIRY DATES         
*                                                                               
BASISCHG NTR1                                                                   
         TM    SCAEBASH+4,X'20'    IF EXPIRATION CALC. BASIS CHANGED            
         BZ    YES                 RETURN YES                                   
*                                                                               
         CLI   SCAEBAS,TACAEPUB    IF BASIS ISN'T PUBLICATION DATE              
         BE    NO                                                               
         TM    SCASDTEH+4,X'20'    AND SHOOT DATE CHANGED                       
         BO    NO                                                               
         B     YES                 RETURN YES                                   
         SPACE 3                                                                
*              ROUTINE CHANGES EXPIRATION DATE IN ALL DEAL ELEMENTS             
*                                                                               
CHGEXP   NTR1                                                                   
         L     R3,AIO              SET TO LOOP THROUGH ALL DEAL ELS.            
         MVI   ELCODE,TADLELQ                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
         BAS   RE,NEXTEL                                                        
         BNE   XIT                                                              
         BAS   RE,CALCEXP          CALCULATE NEW EXPIRATION DATE                
         B     *-12                GET NEXT ELEMENT                             
         EJECT                                                                  
*              ROUTINE CALCULATES EXPIRATION DATE                               
*                                                                               
         USING TADLD,R3            R3=A(DEAL ELEMENT)                           
CALCEXP  NTR1                                                                   
         XC    TADLEXP,TADLEXP     CLEAR EXPIRATION DATE                        
*                                                                               
         LA    R2,TADLPUB          SET PUBLICATION DATE AS BASIS                
         CLI   SCAEBAS,TACAEPUB                                                 
         BE    CEXP10                                                           
         LA    R2,CASTSDTE         ELSE SET CAST SHOOT DATE                     
         CLI   SCASDTEH+5,0                                                     
         BNE   *+8                                                              
         LA    R2,COMLSDTE         ELSE SET COMML SHOOT DATE                    
*                                                                               
CEXP10   OC    0(3,R2),0(R2)       IF BASIS DATE NOT DEFINED                    
         BZ    CEXPX               THEN DON'T BOTHER CALCULATING EXPIRY         
*                                                                               
         CLI   TADLTERM,TADLTUNL   IF UNLIMITED, DON'T SET EXPIRY DATE          
         BE    CEXPX                                                            
         CLI   TADLTERM,TADLT1X    IF ONE TIME                                  
         BNE   CEXP12                                                           
         MVC   TADLEXP,0(R2)       SET EXPIRY = BASIS DATE                      
         B     CEXPX                                                            
*                                                                               
CEXP12   GOTO1 DATCON,DMCB,(1,(R2)),(8,WORK)  MOVE 'MMMDD/YY' TO WORK           
         MVI   WORK+8,C'-'                                                      
         MVI   WORK+9,C'('                                                      
         EDIT  (1,TADLTERM),(2,WORK+10),ALIGN=LEFT,WRK=WORK+30                  
         LR    RF,R0                                                            
         LA    RF,WORK+10(RF)                                                   
         MVI   0(RF),C'M'                                                       
         MVI   1(RF),C')'                                                       
         AH    R0,=H'12'                                                        
*                                                                               
         LA    R4,BLOCK            R4=A(PERVAL BLOCK)                           
         USING PERVALD,R4                                                       
         GOTO1 PDVAL,DMCB,(X'40',(R4)),((R0),WORK)                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   TADLEXP,PVALPEND    SAVE EXPIRATION DATE                         
*                                                                               
CEXPX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE INSURES ALL DEALS ACCOUNTED FOR                          
*                                                                               
ALLDEAL  NTR1                                                                   
         LA    R3,DLIO             SET TO LOOP THROUGH DEAL RECORD              
         MVI   ELCODE,TADLELQ                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
ALLD10   BAS   RE,NEXTEL                                                        
         BNE   XIT                                                              
         USING TADLD,R3                                                         
         GOTO1 GETL,DMCB,(6,TADLAREA)  IF THIS AREA/USE NOT ON CAST REC         
         BE    ALLD20                                                           
         MVC   ELEMENT(TADLLNQ),TADLEL ADD IT NOW                               
         GOTO1 ADDELEM                                                          
ALLD20   B     ALLD10                                                           
         EJECT                                                                  
*              DISPLAY THE RECORD                                               
*                                                                               
DISPLAY  NTR1                                                                   
         GOTO1 FLDVAL,DMCB,(X'01',SCAAGTH),(X'80',999)  CLEAR SCREEN            
         GOTO1 (RF),(R1),(X'01',SCAL1H),SCALSTH                                 
         XC    SCAAGTN,SCAAGTN     AGENT NAME                                   
         OI    SCAAGTNH+6,X'80'                                                 
         XC    SCACRPN,SCACRPN     CORP NAME                                    
         OI    SCACRPNH+6,X'80'                                                 
*                                                                               
         L     R3,AIO              GET CAST DETAILS ELEMENT                     
         MVI   ELCODE,TACAELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACAD,R3            R3=A(CAST DETAILS EL.)                       
*                                                                               
         OC    TACANCDE,TACANCDE   AGENT                                        
         BZ    DISP20                                                           
         GOTO1 TRNSAGT,DMCB,(X'40',TACANCDE),SCAAGT                             
         MVC   SCAAGTN(L'LTMISS),LTMISS                                         
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLANCDQ,(X'8C',SCAAGT),SCAAGTNH  GET NAME            
         MVC   AIO,AIO1                                                         
*                                                                               
DISP20   MVC   SCATAX,TACAUNIT     TAX UNIT                                     
*                                                                               
         BAS   RE,DISCORP          CORP CODE AND NAME                           
*                                                                               
         MVC   SCAPAYE,TACAPAYE    PAYEE                                        
         MVC   SCAEBAS,TACAEBAS    EXPIRATION DATE CALC. BASIS                  
*                                                                               
*                                  AGENT FEE RATE                               
         EDIT  (2,TACARATE),(5,SCAAGTF),2,ALIGN=LEFT,ZERO=BLANK                 
*                                                                               
         OC    TACASDTE,TACASDTE   SHOOT DATE                                   
         BZ    DISP50                                                           
         GOTO1 DATCON,DMCB,(1,TACASDTE),(8,SCASDTE)                             
*                                                                               
DISP50   BRAS  RE,DISSTAT                                                       
                                                                                
         GOTO1 CHAROUT,DMCB,TACMELQ,SCACMNTH,TACMTYPG  COMMENT                  
*                                                                               
         BAS   RE,DISDEAL          DISPLAY DEAL LINES                           
*                                                                               
         GOTO1 ACTVOUT,DMCB,SCALCHGH  LAST CHANGED INFO                         
*                                                                               
         GOTO1 FLDVAL,DMCB,(X'20',AFRSTREC),999  MAKE ALL FLDS VALID            
*                                                                               
         MVC   TGSSN,SCASSN        INSURE CAST SSN IN GLOBAL                    
         CLI   SCASSNH+5,6                                                      
         BH    DISPXX                                                           
         MVC   TGSSN,SPACES                                                     
         MVC   TGPID,SCASSN                                                     
         GOTO1 SSNUNPK,DMCB,TGPID,TGSSN                                         
DISPXX   B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO DISPLAY CORP INFO                                     
*                                                                               
DISCORP  NTR1                                                                   
         MVC   SCACRP,TACACORP     CORP CODE                                    
*                                                                               
         CLI   SCACRP,C'1'         IF THERE'S A CODE                            
         BL    DCRP20                                                           
         MVC   SCACRPN(L'LTMISS),LTMISS  DISPLAY MISSING LIT                    
*                                                                               
         L     R1,AW4IO            SEARCH W4 RECORD FOR TAX UNIT EL             
         ST    R1,AIO                                                           
         MVI   ELCODE,TATIELQ                                                   
         MVI   HALF,TATITYCO       BUILD GETL SEARCH ARGUMENT                   
         MVC   HALF+1(1),SCACRP                                                 
         GOTO1 GETL,DMCB,(2,HALF)                                               
         MVC   AIO,AIO1                                                         
         BNE   DCRPX               SKIP IF NOT FOUND                            
*                                                                               
         L     R4,TGELEM           R4=A(CORP ID ELEMENT)                        
         USING TATID,R4                                                         
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'8C',TATIID),SCACRPNH  GET NAME            
         BNE   DCRP19                                                           
         L     R3,AIO2                                                          
         USING TAW4D,R3                                                         
         MVI   ELCODE,TAW4ELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    TAW4STA2,TAW4SLCK   IF THIS CRP IS LOCKED                        
         BNO   DCRP19                                                           
         MVC   SCACRPN(L'LTCRPL),LTCRPL    DISPLAY 'CRP LOCKED'                 
                                                                                
DCRP19   MVC   AIO,AIO1                                                         
         B     DCRPX                                                            
*                                                                               
DCRP20   L     R3,AW4IO            NO CORP CODE ON CAST RECORD                  
         MVI   ELCODE,TATIELQ      IF THERE'S A CORP ID IN W4 REC.              
         BAS   RE,GETEL                                                         
         BNE   DCRPX                                                            
         MVC   SCACRPN(L'LTCRP),LTCRP  DISPLAY 'PERF HAS CORP'                  
*                                                                               
DCRPX    B     XIT                                                              
         EJECT                                                                  
*        ROUTINE DISPLAYS STATUS CODES FIELD                                    
*                                                                               
         USING TACAD,R3            R3=A(CAST ELEMENT)                           
DISSTAT  NTR1                                                                   
         TM    TACASTA2,TACASEUR                                                
         BZ    XIT                                                              
         MVC   SCASTA(L'EURO),EURO                                              
         MVI   SCASTAH+5,L'EURO                                                 
         OI    SCASTAH+6,X'80'                                                  
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE DISPLAYS DEAL LINES                                      
*                                                                               
DISDEAL  NTR1                                                                   
         CLI   MODE,XRECPUT        IF JUST CHANGED RECORD                       
         BNE   DDL10                                                            
         CLI   PFAID,14            AND USER WANTS ROOM FOR MORE                 
         BE    DDL20               CLEAR SAVED KEYS & LEAVE SCRN EMPTY          
*                                                                               
DDL10    L     R3,AIO                                                           
         MVI   ELCODE,TADLELQ      LOOK FOR DEAL ELEMENTS                       
         BAS   RE,GETEL                                                         
         BE    *+14                                                             
DDL20    XC    DLS,DLS             NONE FOUND - CLEAR SAVED EL. KEYS            
         B     DDLX                AND GET OUT                                  
*                                                                               
         OC    DLS,DLS             IF WE HAVEN'T DISPLAYED ANYTHING YET         
         BZ    DDL50               THEN START WITH FIRST                        
*                                                                               
         USING TADLD,R3            R3=A(DEAL ELEMENT)                           
*                                                                               
         XR    R0,R0               INITIALIZE START SWITCH                      
         CLI   MODE,XRECPUT        IF JUST CHANGED RECORD                       
         BE    *+12                                                             
         CLI   MODE,XRECADD        OR JUST ADDED IT                             
         BNE   DDL30                                                            
         GOTO1 FLDVAL,DMCB,(X'40',SCAL1H),SCALSTH  AND SCREEN CHANGED           
         BNE   *+12                                                             
         BAS   RE,BASISCHG         OR EXPIRATION CALC. BASIS CHANGED            
         BNE   DDL30                                                            
         LA    R0,1                SET START SWITCH TO RE-DISPLAY               
*                                                                               
DDL30    LTR   R0,R0               IF NEED TO DISPLAY CURRENT PAGE              
         BZ    DDL40                                                            
         CLC   TADLAREA(6),DLFRST  SCAN FOR FIRST EL DISPLAYED                  
         BNL   DDL50                                                            
         B     *+14                                                             
DDL40    CLC   TADLAREA(6),DLLAST  ELSE SCAN FOR 1ST EL AFTER LAST DISP         
         BH    DDL50                                                            
         BAS   RE,NEXTEL                                                        
         BE    DDL30                                                            
         XC    DLS,DLS             NONE LEFT - START FROM BEGINNING             
         B     DDL10                                                            
*                                                                               
DDL50    LA    R4,SCAL1H           R4=A(1ST LINE)                               
         USING LINED,R4                                                         
         LA    R2,MXLINES          R2=N'LINES                                   
*                                                                               
         XC    TGAREA,TGAREA       CLEAR GLOBAL AREA CODE                       
         MVC   DLFRST,TADLAREA     SAVE KEY OF FIRST EL. DISPLAYED              
*                                                                               
DDL60    MVC   DLLAST,TADLAREA     SAVE KEY OF LAST EL. DISPLAYED               
*                                                                               
         MVC   LINAREA2,TADLAREA   AREA IN NOP FIELD FOR PFTAB                  
*                                                                               
         CLC   TADLAREA,TGAREA     IF THIS AREA IS SAME AS PREVIOUS             
         BE    DDL70               DON'T BOTHER DISPLAYING AGAIN                
*                                                                               
         MVC   LINAREA,TADLAREA    AREA                                         
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLARCDQ,(X'8C',LINAREA),LINARENH  GET NAME           
         MVC   AIO,AIO1                                                         
*                                                                               
DDL70    MVC   LINUSE,TADLUSE      USE                                          
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLUSCDQ,(X'8C',LINUSE),LINUSENH   GET NAME           
         MVC   AIO,AIO1                                                         
*                                                                               
         CLI   TADLTERM,TADLTUNL   IF UNLIMITED                                 
         BNE   *+14                                                             
         MVC   LINTERM,=C'UN'      DISPLAY "UN"                                 
         B     DDL74                                                            
         CLI   TADLTERM,TADLT1X    IF ONE TIME                                  
         BNE   *+14                                                             
         MVC   LINTERM,=C'1X'      DISPLAY "1X"                                 
         B     DDL74                                                            
         EDIT  (1,TADLTERM),(2,LINTERM),ZERO=BLANK     TERM                     
*                                                                               
DDL74    EDIT  (4,TADLAMT),(10,LINAMT),2,ZERO=NOBLANK  AMOUNT                   
*                                                                               
         EDIT  (2,TADLRATE),(5,LINRATE),2,ZERO=BLANK   COMMISSION RATE          
*                                                                               
         OC    TADLPUB,TADLPUB     PUBLICATION DATE                             
         BZ    DDL80                                                            
         GOTO1 DATCON,DMCB,(1,TADLPUB),(8,LINPUB)                               
*                                                                               
DDL80    OC    TADLEXP,TADLEXP     EXPIRATION DATE                              
         BZ    DDL90                                                            
         GOTO1 DATCON,DMCB,(1,TADLEXP),(8,LINEXP)                               
*                                                                               
DDL90    MVI   ELCODE,TADLELQ      RESET FOR DEAL ELEMENTS                      
         BAS   RE,NEXTEL           GET NEXT ELEMENT                             
         BNE   DDLX                                                             
         LA    R4,LINNEXT          BUMP TO NEXT LINE                            
         BCT   R2,DDL60                                                         
*                                                                               
DDLX     XR    R4,R4               SET TO COUNT TOTAL N'PAGES                   
         BAS   RE,PGIT                                                          
         MVC   NPAGES,BYTE                                                      
*                                                                               
         LA    R4,DLLAST           SET TO CALC THIS PAGE NUMBER BASED           
         BAS   RE,PGIT             ON LAST DEAL ELEMENT DISPLAYED               
         MVC   THISPG,BYTE                                                      
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE CALCULATES CURRENT PAGE NUMBER                           
*                                                                               
*                                  R4=0 (COUNT ALL) OR A(LAST DSPLY'D)          
PGIT     NTR1                                                                   
         XR    R2,R2               R2=COUNT                                     
         L     R3,AIO                                                           
         MVI   ELCODE,TADLELQ      LOOK FOR DEAL ELEMENTS                       
         BAS   RE,GETEL                                                         
         B     *+8                                                              
PG10     BAS   RE,NEXTEL                                                        
         BNE   PG20                                                             
         AH    R2,=H'1'            ADD 1 TO COUNT                               
         LTR   R4,R4               IF SCANNING FOR LAST EL.                     
         BZ    PG10                                                             
         CLC   DLLAST,TADLAREA     TEST IF WE'VE REACHED IT                     
         BNE   PG10                                                             
*                                                                               
PG20     LA    R1,MXLINES-1                                                     
         AR    R1,R2               ADD N'LINES-1 FOR DIVIDE                     
         XR    R0,R0                                                            
         LA    R2,MXLINES                                                       
         DR    R0,R2               DIVIDE BY N'LINES/SCREEN                     
         LTR   R1,R1                                                            
         BNZ   *+8                                                              
         LA    R1,1                MUST HAVE AT LEAST 1 PAGE                    
         STC   R1,BYTE             RETURN BYTE=PAGE NUMBER                      
         B     XIT                                                              
         EJECT                                                                  
*              ERRORS, EXITS                                                    
*                                                                               
ERRFLD   STC   R0,ERRDISP                                                       
FLDINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     THEEND                                                           
*                                                                               
AGTMISS  LA    R2,SCAAGTH          CURSOR TO AGENT FIELD                        
FLDMISS  MVI   ERROR,MISSING       MISSING INPUT FIELD                          
         B     THEEND                                                           
*                                                                               
ERRCRP   MVI   ERROR,ERGT1CRP      GT 1 CORP - NEED EXACT CODE                  
         B     THEEND                                                           
*                                                                               
NOSSN    MVI   ERROR,ERNOSSN       MISSING SSN FOR THIS AGENT                   
         B     THEEND                                                           
*                                                                               
BADCTYPE MVI   ERROR,ERRECCTY      BAD COMMERCIAL TYPE (NOT PRINT)              
         L     R2,EFHREC                                                        
         B     THEEND                                                           
*                                                                               
ERRPHO   MVI   ERROR,ERMISPHO      PHOTOGRAPHER NOT DEFINED ON COML REC         
         B     THEEND                                                           
*                                                                               
DUPDEAL  MVI   ERROR,ERDUPDL       DEAL ALREADY DEF. ON THIS CAST REC           
         B     THEEND                                                           
*                                                                               
ERW4LCK  MVI   ERROR,ERW4SLCK      W4 RECORD LOCKED                             
         B     THEEND                                                           
*                                                                               
ERNOTRS  MVC   MYMSGNO,=Y(ERNOTRST) TRUSTEE NOT ALLOWED ON CAST                 
         B     EXTEXIT                                                          
*                                                                               
ERCRPLK  MVC   MYMSGNO,=Y(ERCPSLCK)    CORP RECORD LOCKED - NON-PAY             
EXTEXIT  OI    GENSTAT2,USGETTXT   NEW THEEND FOR TWO BYTE ERROR MSGS           
         MVI   BLOCK,0                                                          
         MVI   MYMTYP,GTMERR                                                    
         B     THEEND                                                           
*                                                                               
ERMDLXST MVI   ERROR,RECEXIST      RECORD ALREADY EXISTS                        
         B     THEEND                                                           
*                                                                               
PGDSPMS2 CLI   PFAID,14            TEST DISPLAYED NEW PAGE                      
         BE    PLSENTER                                                         
PGDSPMSG MVI   MYMSGNO1,54         PAGE X OF Y DISPLAYED                        
         L     R2,AFRSTKEY                                                      
         CLI   ACTNUM,ACTDIS                                                    
         BE    PGDSP2                                                           
         CLI   ACTNUM,ACTSEL                                                    
         BNE   *+12                                                             
         CLI   THISLSEL,C'S'                                                    
         BE    PGDSP2                                                           
         MVI   MYMSGNO1,55          ... - ENTER CHANGES AS DESIRED              
         L     R2,AFRSTREC                                                      
PGDSP2   MVI   BLOCK,2                                                          
         EDIT  THISPG,(1,BLOCK+1)                                               
         MVI   BLOCK+2,2                                                        
         EDIT  NPAGES,(1,BLOCK+3)                                               
         MVI   BLOCK+4,0                                                        
         B     INFEND                                                           
*                                                                               
PLSENTER MVI   MYMSGNO1,2          PLEASE ENTER REQUIRED FIELDS                 
         MVI   MYMSYS,X'FF'                                                     
         LA    R2,SCAL1H           CURSOR TO FIRST DEAL LINE                    
         B     INFEND                                                           
*                                                                               
INFEND   OI    GENSTAT2,USGETTXT   SET INFO MESSAGE                             
         B     THEEND                                                           
CIDEND   LA    R2,SCACIDH          CURSOR TO COMMERCIAL ID                      
THEEND   GOTO1 EXIT,DMCB,0                                                      
*                                                                               
YES      XR    RC,RC               SET CONDITION CODE                           
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
*                                                                               
PFTAB    DS    0C                                                               
         DC    AL1(PF13X-*,13,0,(PF13X-PF13)/KEYLNQ,0)                          
         DC    CL3'   ',CL8'CHECK   ',CL8'LIST'                                 
PF13     DC    AL1(KEYTYGLB,L'TGSSN-1),AL2(TGSSN-TGD)                           
         DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYGLB,L'TGCID-1),AL2(TGCID-TGD)                           
PF13X    EQU   *                                                                
         DC    AL1(PF14X-*,14,0,0,PFTRETRN)                                     
         DC    CL3' ',CL8'        ',CL8'        '                               
PF14X    EQU   *                                                                
         DC    AL1(PF15X-*,15,0,(PF15X-PF15)/KEYLNQ,PFTNOSEL)                   
         DC    CL3'   ',CL8'PRS     ',CL8'PAY'                                  
PF15     DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYGLB,L'TGCID-1),AL2(TGCID-TGD)                           
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYCUR,L'LINAREA2-1),AL2(LINAREA2-LINCURST)                
         DC    AL1(KEYTYCUR,L'LINUSE-1),AL2(LINUSE-LINCURST)                    
PF15X    EQU   *                                                                
         DC    AL1(PF16X-*,16,0,(PF16X-PF16)/KEYLNQ,PFTNOSEL)                   
         DC    CL3'   ',CL8'PRT     ',CL8'PAY'                                  
PF16     DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYGLB,L'TGCID-1),AL2(TGCID-TGD)                           
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYCUR,L'LINAREA2-1),AL2(LINAREA2-LINCURST)                
         DC    AL1(KEYTYCUR,L'LINUSE-1),AL2(LINUSE-LINCURST)                    
PF16X    EQU   *                                                                
         DC    X'FF'                                                            
*                                                                               
MXLINES  EQU   (SCACCMTH-SCAL1H)/LINLNQ  MAX N'DEAL LINES/SCREEN                
*                                                                               
LTMISS   DC    C'*Not Found*'                                                   
LTCRP    DC    C'*Perf has Corp*'                                               
LTCRPL   DC    C'*Corp is Locked*'                                              
                                                                                
EURO     DC    C'EURO'                                                          
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAPCSTLCK                                                      
       ++INCLUDE TAW4LCRP                                                       
*              LOCAL SAVED STORAGE                                              
*                                                                               
CASTD    DSECT                                                                  
COMLPHOT DS    CL9                 PHOTOGRAPHER SSN FROM COMMERCIAL             
COMLSDTE DS    XL3                 COMMERCIAL SHOOT DATE                        
CASTSDTE DS    XL3                 CAST SHOOT DATE                              
DLS      DS    0CL12                                                            
DLFRST   DS    CL6                 KEY OF FIRST EL DISPLAYED ON SCREEN          
DLLAST   DS    CL6                 KEY OF LAST EL DISPLAYED ON SCREEN           
THISPG   DS    XL1                 CURRENT PAGE NUMBER                          
NPAGES   DS    XL1                 TOTAL N'PAGES                                
SVKEY    DS    CL(L'TLDRREC)       SAVED KEY                                    
PTRBLK   DS    CL(4*L'TLDRREC+1)   PASSIVE + ACTIVE PTRS                        
W4CRP    DS    CL9                 W4 CRP ID                                    
AW4IO    DS    A                   A(W4 RECORD)                                 
DLIO     DS    CL4000              DEAL RECORD                                  
W4IO     DS    CL4000              W4 RECORD                                    
         SPACE 3                                                                
*              DSECT TO COVER SCREEN DEAL LINES                                 
*                                                                               
LINED    DSECT                                                                  
LINAREAH DS    CL8                                                              
LINCURST EQU   *                   START OF DSECT FOR KEYTYCUR ENTRIES          
LINAREA  DS    CL3                 AREA                                         
         DS    CL8                                                              
         DS    CL8                                                              
LINAREA2 DS    CL3                 AREA IN NOP FIELD FOR PFTAB                  
LINARENH DS    CL8                                                              
LINAREN  DS    CL16                AREA NAME                                    
LINUSEH  DS    CL8                                                              
LINUSE   DS    CL3                 USE                                          
         DS    CL8                                                              
LINUSENH DS    CL8                                                              
LINUSEN  DS    CL16                USE NAME                                     
LINTERMH DS    CL8                                                              
LINTERM  DS    CL2                 TERM (MONTHS)                                
         DS    CL8                                                              
LINAMTH  DS    CL8                                                              
LINAMT   DS    CL10                AMOUNT                                       
         DS    CL8                                                              
LINRATEH DS    CL8                                                              
LINRATE  DS    CL5                 AGENT COMMISSION RATE                        
         DS    CL8                                                              
LINPUBH  DS    CL8                                                              
LINPUB   DS    CL8                 PUBLICATION DATE                             
         DS    CL8                                                              
LINEXPH  DS    CL8                                                              
LINEXP   DS    CL8                 EXPIRATION DATE                              
         DS    CL8                                                              
LINLNQ   EQU   *-LINED                                                          
LINNEXT  EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRBBD                                                       
         EJECT                                                                  
* DDGENTWA   (MUST FOLLOW LAST SCREEN)                                          
* TASYSDSECT                                                                    
* TAGENEQUS                                                                     
* TASYSEQUS                                                                     
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSDSECT                                                                    
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* DDPERVALD                                                                     
* FATIOB                                                                        
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TAGENEQUS                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE FATIOB                                                         
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'051TAGENBB   04/28/15'                                      
         END                                                                    
