*          DATA SET TAGENC9    AT LEVEL 038 AS OF 04/29/15                      
*PHASE T702C9A,*                                                                
         TITLE 'T702C9 - ECAST MAINTENANCE'                                     
T702C9   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T702C9,R5                                                      
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         LA    R7,TWAHOLE          R7=PROGRAM SAVED STORAGE                     
         USING ECASTD,R7                                                        
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
         SPACE 1                                                                
         GOTO1 INITIAL,DMCB,PFTAB  INITIALIZE                                   
         MVC   SECSHED(7),=C'Pid Num'                                           
         OI    SECSHEDH+6,X'80'                                                 
         SPACE 1                                                                
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BNE   *+12                                                             
         BAS   RE,VKEY                                                          
         B     MAINX                                                            
         SPACE                                                                  
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BNE   *+12                                                             
         BAS   RE,DKEY                                                          
         B     MAINX                                                            
         SPACE                                                                  
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    MAIN20                                                           
         CLI   MODE,RECDEL         DELETE RECORD                                
         BE    MAIN20                                                           
         CLI   MODE,RECREST        OR RESTORE RECORD                            
         BNE   MAIN30                                                           
MAIN20   GOTO1 SAVPTRS,DMCB,PTRBLK  SAVE PASSIVE PTRS                           
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BNE   *+8                                                              
         BAS   RE,BLDREC           BUILD THE RECORD                             
         B     MAINX                                                            
         SPACE                                                                  
MAIN30   CLI   MODE,DISPREC        IF DISPLAY RECORD                            
         BE    MAIN60                                                           
         CLI   MODE,XRECDEL        OR DELETED                                   
         BE    MAIN40                                                           
         CLI   MODE,XRECREST       OR RESTORED                                  
         BE    MAIN40                                                           
         CLI   MODE,XRECPUT        OR CHANGED                                   
         BE    MAIN40                                                           
         CLI   MODE,XRECADD        IF RECORD ADDED                              
         BNE   MAINX                                                            
         XC    PTRBLK,PTRBLK                                                    
*                                                                               
MAIN40   GOTO1 ADDPTRS,DMCB,PTRBLK UPDATE PASSIVE POINTERS                      
*                                                                               
MAIN60   BAS   RE,DISPLAY          (RE-)DISPLAY THE RECORD                      
*                                                                               
MAINX    B     XIT                                                              
         SPACE 2                                                                
YES      XR    RC,RC               SET CONDITION CODE                           
NO       LTR   RC,RC                                                            
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*              VALIDATE THE KEY                                                 
         SPACE 1                                                                
VKEY     NTR1                                                                   
         CLI   SCRSTAT,0           IF SCREEN CHANGED                            
         BE    *+8                                                              
         NI    SECAGYH+4,X'DF'     SET AGENCY FIELD CHANGED                     
*                                                                               
         TM    SECAGYH+4,X'20'     IF AGENCY CHANGED                            
         BO    VK10                                                             
         NI    SECCIDH+4,X'DF'                                                  
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'20',SECAGYH)   AGENCY                     
         GOTO1 RAVPPLSA,DMCB,0     SEE IF REC / ACT INVALID FOR P+              
         JNE   ERPPLSI                                                          
*                                                                               
VK10     LA    R2,SECCIDH                                                       
         TM    4(R2),X'20'         IF COMMERCIAL CHANGED                        
         BO    VK20                                                             
         NI    SECEPIH+4,X'DF'                                                  
         GOTO1 RECVAL,DMCB,TLCOICDQ,(X'08',(R2)),0 COMMERCIAL ID                
         LA    R3,KEY                                                           
         USING TLCOPD,R3                                                        
         MVC   TGCOM,TLCOICOM      INTERNAL COMMERCIAL NUMBER                   
         DROP  R3                                                               
         L     R4,AIO                                                           
         USING TLCOPD,R4                                                        
         MVI   ELCODE,TACOELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACOD,R4                                                         
         CLI   TACOTYPE,CTYSOAP    INSURE SOAP COMM TYPE                        
         BNE   BADCTYPE                                                         
         DROP  R4                                                               
*                                                                               
VK20     LA    R2,SECEPIH                                                       
         TM    4(R2),X'20'         IF EPISODE CHANGED                           
         BO    VK30                                                             
         NI    SECSSNH+4,X'DF'                                                  
         GOTO1 RECVAL,DMCB,TLEPCDQ,(R2)  EPISODE                                
*                                                                               
VK30     LA    R2,SECSSNH          IF S/S CHANGED                               
         TM    4(R2),X'20'                                                      
         BO    VK40                                                             
         NI    SECCATH+4,X'DF'                                                  
         LA    R1,W4IO                                                          
         ST    R1,AIO                                                           
         CLI   SECSSNH+5,6                                                      
         BH    VK32                                                             
         MVC   TGPID,SECSSN                                                     
         GOTO1 SSNUNPK,DMCB,TGPID,TGSSN                                         
         BNE   VK32                                                             
         MVC   SECSSN,TGSSN                                                     
         MVI   SECSSNH+5,9                                                      
VK32     GOTO1 RECVAL,DMCB,TLW4CDQ,(X'08',(R2)),SECSSNNH                        
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   SECSSN,SPACES                                                    
         MVC   SECSSN(L'TGPID),TGPID                                            
         MVI   SECSSNH+5,6                                                      
         OI    SECSSNH+6,X'80'                                                  
*                                                                               
VK34     MVC   AIO,AIO1                                                         
         LA    R4,W4IO                                                          
         USING TAW4D,R4                                                         
         MVI   ELCODE,TAW4ELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    TAW4STA2,TAW4SLCK   IF RECORD IS LOCKED                          
         BO    ERW4LCK             ERROR MSG W4 IS LOCKED                       
         CLI   TAW4TYPE,TAW4TYTR   IF THIS W4 RECORD IS TRUSTEE                 
         BE    ERNOTRS             EXIT WITH ERROR MESSAGE                      
         DROP  R4                                                               
*                                                                               
VK40     LA    R2,SECCATH                                                       
         TM    4(R2),X'20'         IF CATEGORY CHANGED                          
         BO    VK50                                                             
         OC    8(3,R2),SPACES                                                   
         CLI   5(R2),0             AND IF NO INPUT                              
         BNE   VK44                                                             
         OC    TGCAT,TGCAT         AND NO GLOBAL                                
         BZ    FLDMISS             THEN REQUIRE INPUT                           
         MVC   8(3,R2),TGCAT       ELSE USE GLOBAL                              
         OI    6(R2),X'80'                                                      
VK44     GOTO1 CATVAL,DMCB,8(R2)   VALIDATE CATEGORY                            
         BNE   FLDINV                                                           
         MVC   TGCSORT(1),TGCASORT SET CATEGORY SORT CODE FOR KEY               
         OI    4(R2),X'20'         SET PREVIOUSLY VALIDATED                     
*                                                                               
VK50     CLI   ACTNUM,ACTREST      IF RESTORE                                   
         BNE   *+8                                                              
         OI    DMINBTS,X'08'       READ FOR DELETED                             
         XC    TGINV,TGINV         OTHERWISE CHECK NO DUP UP TO INV             
         GOTO1 RECVAL,DMCB,TLECCDQ,0                                            
         NI    DMINBTS,X'F7'       RESET                                        
         CLC   KEY(TLECINV-TLECKEY),KEYSAVE                                     
         BE    *+10                KEY DUPLICATE                                
         MVC   KEY,KEYSAVE         KEY OKAY TO ADD                              
         B     XIT                                                              
         EJECT                                                                  
*              BUILD THE RECORD                                                 
         SPACE 1                                                                
DKEY     NTR1                                                                   
         MVC   SVKEY,KEY           SAVE KEY                                     
         L     R6,AIO                                                           
         USING TLECD,R6                                                         
         MVC   SECAGY,TGAGY        AGENCY IS IN GLOBAL                          
         OI    SECAGYH+6,X'80'                                                  
         MVC   SECCID,TGCID        COMMERCIAL ID IS IN GLOBAL                   
         OI    SECCIDH+6,X'80'                                                  
         MVC   SECEPI,TGEPI        EPISODE NUMBER IS IN GLOBAL                  
         OI    SECEPIH+6,X'80'                                                  
*                                                                               
         MVC   SECSSN,TLECSSN      S/S NUMBER                                   
         MVI   SECSSNH+5,L'TLECSSN                                              
         OI    SECSSNH+6,X'80'                                                  
         LA    R1,W4IO                                                          
         ST    R1,AIO                                                           
         LA    R2,SECSSNH                                                       
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'08',(R2)),SECSSNNH                        
         GOTO1 SSNPACK,DMCB,SECSSN,TGPID                                        
         MVC   SECSSN,SPACES                                                    
         MVC   SECSSN(L'TGPID),TGPID                                            
         MVI   SECSSNH+5,6                                                      
         OI    SECSSNH+6,X'80'                                                  
*                                                                               
DK10     MVC   AIO,AIO1                                                         
         LA    R4,W4IO                                                          
         USING TAW4D,R4                                                         
         MVI   ELCODE,TAW4ELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    TAW4STA2,TAW4SLCK   IF RECORD IS LOCKED                          
         BO    ERW4LCK             ERROR MSG W4 IS LOCKED                       
         CLI   TAW4TYPE,TAW4TYTR   IF THIS W4 RECORD IS TRUSTEE                 
         BE    ERNOTRS             EXIT WITH ERROR MESSAGE                      
         DROP  R4                                                               
*                                                                               
         MVC   SECCAT,TLECCAT      CATEGORY                                     
         OI    SECCATH+6,X'80'                                                  
         MVC   KEY,SVKEY           RESTORE KEY                                  
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*              BUILD THE RECORD                                                 
         SPACE 1                                                                
BLDREC   NTR1                                                                   
         MVC   SVKEY,KEY           SAVE KEY                                     
         L     R6,AIO                                                           
         USING TLECD,R6                                                         
         LR    R4,R6                                                            
         MVI   ELCODE,TACAELQ      REMOVE EXISTING CAST DETAILS ELEMENT         
         GOTO1 REMELEM                                                          
*                                                                               
         XC    CAELEM,CAELEM       BUILD NEW CAST ELEMENT                       
         LA    R4,CAELEM                                                        
         USING TACAD,R4                                                         
         MVI   TACAEL,TACAELQ      ELEMENT CODE                                 
         MVI   TACALEN,TACALNQ     ELEMENT LENGTH                               
         BAS   RE,VRONOF           VALIDATE CAMERA (ON/OFF)                     
         BAS   RE,VRTAX            VALIDATE TAX UNIT                            
         BAS   RE,VRUNLO           VALIDATE UNION/LOCAL                         
         BAS   RE,VRYEAR           VALIDATE CONTRACT YEAR                       
         BAS   RE,VRCORP           VALIDATE CORP NUMBER                         
         BAS   RE,VRAGCD           VALIDATE AGENT CODE                          
*                                                                               
         BAS   RE,VRROLE           VALIDATE WRITER ROLE                         
*                                                                               
         BAS   RE,VRPCT            VALIDATE WRITER PERCENTAGE                   
         MVC   ELEMENT,CAELEM                                                   
         GOTO1 ADDELEM             ADD CAST DETAILS ELEMENT                     
*                                                                               
         BAS   RE,VRAMTS           VALIDATE PERF RATE, CREDIT AMT & BAL         
         XC    ELEMENT,ELEMENT                                                  
         LR    R4,R6                                                            
         MVI   ELCODE,TACRELQ                                                   
         USING TACRD,R4                                                         
         BAS   RE,GETEL            IF ELEMENT EXISTS- CHANGE IT                 
         BE    *+8                                                              
         LA    R4,ELEMENT          OTHERWISE ADD NEW ELEMENT                    
         MVI   TACREL,TACRELQ      ELEMENT CODE                                 
         MVI   TACRLEN,TACRLNQ     ELEMENT LENGTH                               
         MVC   TACRSCAL,TPERF      PERFORMANCE RATE                             
         MVC   TACRAPPL,TCAMT      CREDIT AMOUNT                                
         MVC   TACRBAL,TCBAL       CREDIT BALANCE                               
         CLI   ELEMENT,0                                                        
         BE    BLDREC30                                                         
         GOTO1 ADDELEM             ADD NEW ELEMENT                              
*                                                                               
BLDREC30 GOTO1 NAMIN,DMCB,TACMELQ,(X'80',SECCMH),TACMTYPG  COMMENT              
*                                                                               
         GOTO1 ACTVIN,DMCB,0       LAST CHANGED                                 
*                                                                               
         MVC   KEY,SVKEY           RESTORE KEY                                  
*                                                                               
         L     RE,TCBAL              IF BALANCE ON RECORD                       
         LTR   RE,RE                                                            
         BNP   *+12                                                             
         OI    TLECSTAT,TLECSBAL     TURN ON STATUS IN RECORD                   
         B     *+8                                                              
         NI    TLECSTAT,ALL-TLECSBAL OTHERWISE TURN IT OFF                      
         CLI   ACTNUM,ACTADD         ON CHANGE                                  
         BE    BLDRECX                                                          
         CLC   TLECSTAT,KEY+TLDRSTAT-TLDRD SET DIRECTORY IF NECESSARY           
         BE    BLDRECX                                                          
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLECKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY+TLDRSTAT-TLDRD(1),TLECSTAT                                   
         GOTO1 WRITE                                                            
*                                                                               
BLDRECX  B     XIT                                                              
         EJECT                                                                  
*                                                                               
*              ROUTINE TO VALIDATE ON/OFF CAMERA                                
         SPACE 1                                                                
         USING TACAD,R4                                                         
VRONOF   NTR1                                                                   
         GOTO1 CATVAL,DMCB,TLECCAT SET TGCASTAT                                 
         LA    R2,SECONOFH         R2=A(ON/OFF FIELD)                           
         CLI   5(R2),0             IF FIELD IS EMPTY                            
         BNE   VRONOF50                                                         
         TM    TGCASTAT,OKON       THEN IF OK ON CAMERA ONLY                    
         BZ    VRONOF10                                                         
         TM    TGCASTAT,OKOFF                                                   
         BO    VRONOF20                                                         
         MVC   8(3,R2),=C'ON '     THEN MOVE 'ON' TO FIELD                      
         B     VRONOF40                                                         
*                                                                               
VRONOF10 TM    TGCASTAT,OKON       ELSE IF OK OFF CAMERA ONLY                   
         BO    VRONOF20                                                         
         MVC   8(3,R2),=C'OFF'     THEN MOVE 'OFF' TO FIELD                     
         B     VRONOF40                                                         
*                                                                               
*RONOF20 TM    TGCAUNI,AFM         ELSE IF CATEGORY IS MUSICIAN                 
VRONOF20 GOTO1 UNITEST,DMCB,TGCAUNIS,AFM,0,0,0                                  
         BZ    VRONOF50                                                         
         MVC   8(3,R2),=C'OFF'     THEN MOVE 'OFF' TO FIELD                     
VRONOF40 MVI   5(R2),L'TACAONOF    AND SET ITS LENGTH                           
*                                                                               
VRONOF50 GOTO1 ANY                 MOVE AND SPACE PAD FIELD TO WORK             
         CLC   =C'ON ',WORK        IF FIELD CONTAINS 'ON'                       
         BNE   VRONOF60                                                         
         TM    TGCASTAT,OKON       THEN MUST BE OK FOR CATEGORY                 
         BZ    FLDINV                                                           
         B     VRONOF90                                                         
*                                                                               
VRONOF60 CLC   =C'OFF',WORK        ELSE FIELD MUST CONTAIN 'OFF'                
         BNE   FLDINV                                                           
         TM    TGCASTAT,OKOFF      AND MUST BE VALID FOR CATEGORY               
         BZ    FLDINV                                                           
*                                                                               
VRONOF90 MVC   TACAONOF,WORK       SAVE IN ELEMENT                              
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO TAX UNIT FIELD                                        
         SPACE 1                                                                
VRTAX    NTR1                                                                   
         LA    R2,SECUNITH        R2=A(TAX UNIT)                                
         GOTO1 ANY                                                              
         CLI   5(R2),2            MUST BE AT LEAST 2 CHARS                      
         BL    FLDINV                                                           
         GOTO1 TAXVAL,DMCB,(3,WORK)                                             
         BNE   FLDINV                                                           
         TM    TGTASTAT,TASUINGF  ENSURE TAX UNIT IS VALID GOING                
         BO    FLDINV             GOING FORWARD                                 
         CLC   =C'FD',8(R2)       IF UNIT STARTS W/ FD                          
         BE    FLDINV                                                           
         CLI   WORK+2,C'0'        OR ENDS WITH 0 THEN ERROR                     
         BE    FLDINV                                                           
*                                                                               
         MVC   TACAUNIT,WORK      SAVE IN ELEMENT                               
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO TAX UNIT FIELD (OPTIONAL)                             
         SPACE 1                                                                
VRAGCD   NTR1                                                                   
         XC    TACANCDE,TACANCDE                                                
         CLI   SECNCDEH+5,0       AGENT CODE (OPTIONAL)                         
         BE    XIT                                                              
         GOTO1 RECVAL,DMCB,TLANCDQ,SECNCDEH                                     
         GOTO1 TRNSAGT,DMCB,(X'80',TGAGT),TACANCDE                              
         B     XIT                                                              
         EJECT                                                                  
* VALIDATE UNION/LOCAL FIELD                                                    
*                                                                               
VRUNLO   NTR1                                                                   
         LA    R2,SECUNLOH                                                      
         GOTO1 ANY                                                              
         CLI   5(R2),3             FIELD MUST BE LONGER THEN 3 BYTES            
         BNH   FLDINV                                                           
         OC    WORK(7),SPACES      SET NULLS TO SPACES                          
         GOTO1 UNIVAL,DMCB,WORK    VALIDATE UNION                               
         BNE   FLDINV                                                           
*                                                                               
*        ZIC   RF,TGUNEQU          TEST UNION VALID FOR THIS CATEGORY           
*        EX    RF,*+8                                                           
*        B     *+8                                                              
*        TM    TGCAUNI,0                                                        
         GOTO1 UNITEST,DMCB,(X'80',TGUNEQUS),TGCAUNIS                           
         BZ    FLDINV              TEST UNION VALID FOR THIS CATEGORY           
         MVC   TACAUN,WORK         SAVE UNION IN ELEMENT                        
*                                                                               
         LA    R3,WORK+4           R3 = A(LOCAL CODE WITHIN FIELD)              
         CLI   WORK+3,C' '                                                      
         BE    VRUNLO20                                                         
         CLI   WORK+3,C'/'                                                      
         BE    VRUNLO20                                                         
         LA    R3,WORK+3                                                        
*                                                                               
VRUNLO20 MVC   TGLCL,0(R3)         VALIDATE LOCAL CODE                          
         MVC   TGUNI,TACAUN                                                     
         GOTO1 RECVAL,DMCB,TLLOCDQ,0                                            
         BNE   THEEND                                                           
         MVC   TACALOCL,TGLCL      SAVE LOCAL IN ELEMENT                        
         B     XIT                                                              
         EJECT                                                                  
* VALIDATE CONTRACT YEAR FIELD                                                  
*                                                                               
VRYEAR   NTR1                                                                   
         LA    R2,SECYEARH                                                      
         GOTO1 ANY                 IF FIELD IS EMPTY                            
         GOTO1 YRVAL,DMCB,WORK     VALIDATE YEAR                                
         BNE   FLDINV                                                           
*                                                                               
         LA    R3,TGUNYR           R3 = A(VALID UNION YEARS)                    
VRYEAR10 CLI   0(R3),0             IF END OF TABLE THEN ERROR                   
         BE    FLDINV                                                           
         CLC   0(1,R3),TGYREQU     IF MATCH THEN FOUND                          
         BE    VRYEAR20                                                         
         LA    R3,1(R3)            ELSE TRY NEXT TABLE ENTRY                    
         B     VRYEAR10                                                         
*                                                                               
VRYEAR20 MVC   TACAYEAR,WORK       SAVE YEAR IN ELEMENT                         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE CORP FIELD (OPTIONAL)                        
         SPACE 1                                                                
VRCORP   NTR1                                                                   
         MVI   TACACORP,0                                                       
         LA    R2,SECCORPH         R2=A(CORPORATION FIELD)                      
         CLI   5(R2),0                                                          
         BE    VCRPX                                                            
         MVC   TACACORP,8(R2)      SAVE IN ELEMENT                              
         CLI   TACACORP,C'Y'       IF FIELD HAD 'Y'                             
         BNE   *+8                                                              
         MVI   TACACORP,C'1'       THEN SET TO '1' IN ELEMENT                   
*                                                                               
         LA    R1,W4IO             SEARCH W4 RECORD FOR TAX ID EL.              
         ST    R1,AIO                                                           
         MVI   ELCODE,TATIELQ                                                   
         MVI   HALF,TATITYCO       BUILD GETL SEARCH ARGUMENT                   
         MVC   HALF+1(1),TACACORP                                               
         GOTO1 GETL,DMCB,(2,HALF)                                               
         MVC   AIO,AIO1            RESTORE AIO                                  
         BNE   FLDINV              ERROR IF NOT FOUND                           
*                                                                               
         CLI   8(R2),C'Y'          IF 'Y' WAS INPUT                             
         BNE   VCRPX                                                            
         L     R4,TGELEM                                                        
         BAS   RE,NEXTEL           SEARCH FOR ADDL CORPS ON W4 REC.             
         BE    ERRCRP              NEED PRECISE CORP CODE                       
*                                                                               
VCRPX    CLI   5(R2),0                                                          
         BE    XIT                                                              
         L     RF,TGELEM                                                        
         GOTOR W4LCKCRP,DMCB,TATIID-TATID(RF),AIO3,AIO1                         
         BE    VCRPXX                                                           
         NI    4(R2),X'DF'         UNVALIDATE                                   
         B     ERCRPLK             EXIT WITH ERROR MESSAGE                      
*                                                                               
         USING TLDRD,R3                                                         
VCRPXX   LA    R3,KEY                                                           
         MVC   TLDRDA,GLOBDA                                                    
         MVC   AIO,AIO3                                                         
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
         J     XIT                                                              
         DROP  R3,R4                                                            
         EJECT                                                                  
*              VALIDATE WRITER ROLE                                             
VRROLE   NTR1                                                                   
         L     R4,AIO              DELETE OLD TASOD ELEMENT                     
         MVI   ELCODE,TASOELQ                                                   
         GOTO1 REMELEM                                                          
*                                                                               
         MVI   SVSOSTAT,0          SAVED WRITER ROLE EQUATE                     
         LA    R2,SECWRIRH                                                      
         CLI   5(R2),0             IF INPUT                                     
         BE    VRROLE10                                                         
         TM    TGCATYPE,WRITER     MUST BE WRITER CATEGORY                      
         BNO   FLDINV                                                           
         CLI   TGCAEQU,CTW         IF CATEGORY IS NOT W, W2, OR W3              
         BE    VRROLE15                                                         
         CLI   TGCAEQU,CTW2                                                     
         BE    VRROLE15                                                         
         CLI   TGCAEQU,CTW3                                                     
         BE    VRROLE15                                                         
*                                                                               
VRROLE10 TM    TGCATYPE,WRITER                                                  
         BNO   VRROLEX                                                          
         BAS   RE,SETROLE          TRY TO DEDUCE WRITER ROLE                    
*                                                                               
VRROLE15 MVC   HALF,8(R2)                                                       
         OC    HALF,SPACES                                                      
*                                                                               
         XC    ELEMENT,ELEMENT     ADD NEW TASOD ELEMENT                        
         LA    R4,ELEMENT                                                       
         USING TASOD,R4                                                         
         MVI   TASOEL,TASOELQ      ELEMENT CODE                                 
         MVI   TASOLEN,TASOLNQ+L'TASOSEPI ELEMENT LENGTH                        
         MVI   TASONUM,1           NUMBER OF EPISODES                           
         PACK  DUB,TGEPI           SET BINARY EPISODE NUMBER                    
         CVB   RE,DUB                                                           
         STH   RE,TASOEPI                                                       
         LA    R3,WROLTAB          TABLE OF VALID WRITER ROLES                  
         USING WROLTABD,R3                                                      
VRROLE20 CLC   WROLCHAR,HALF       CHECK INPUT ROLE                             
         BE    VRROLE30                                                         
         LA    R3,WROLTABL(R3)                                                  
         CLI   0(R3),X'FF'         IF END OF TABLE                              
         BNE   VRROLE20                                                         
         B     FLDINV              INVALID WRITER ROLE INPUT                    
*                                                                               
VRROLE30 MVC   TASOSTAT,WROLEQU    SET WRITER ROLE EQUATE IN ELEMENT            
         GOTO1 ADDELEM                                                          
         MVC   SVSOSTAT,TASOSTAT   SAVE WRITER EQUATE                           
VRROLEX  B     XIT                                                              
         DROP  R3,R4                                                            
         EJECT                                                                  
SETROLE  NTR1                                                                   
         LA    RE,WEQUTAB          TRY TO DEDUCE WRITER ROLE                    
         USING WEQUTABD,RE                                                      
SETROL5  CLC   WEQUCAT,TGCAEQU                                                  
         BE    SETROL8                                                          
         LA    RE,WEQUTABL(RE)     BUMP TO NEXT TABLE ENTRY                     
         CLI   0(RE),X'FF'                                                      
         BNE   SETROL5             LOOP                                         
         B     FLDMISS             MISSING INPUT FIELD                          
*                                                                               
SETROL8  XC    8(2,R2),8(R2)                CLEAR ROLE FIELD                    
         MVC   8(L'WEQUROLE,R2),WEQUROLE    SET ROLE TO SCREEN                  
         OI    6(R2),X'80'                  TRANSMIT                            
         B     XIT                                                              
         DROP  RE                                                               
         EJECT                                                                  
*              THIS ROUTINE VALIDATES PERCENTAGE                                
VRPCT    NTR1                                                                   
         LA    R2,SECPCTH          IF WRITER PERCENT INPUT                      
         SR    RF,RF                                                            
         ICM   RF,1,5(R2)                                                       
         BZ    VRPCTX                                                           
         CLI   SVSOSTAT,0          MUST HAVE WRITER ROLE INPUT                  
         BE    FLDINV                                                           
         CLI   SVSOSTAT,TASOSH     AND ROLE MUST BE H,S,OR B                    
         BE    VRPCT10                                                          
         CLI   SVSOSTAT,TASOSS                                                  
         BE    VRPCT10                                                          
         CLI   SVSOSTAT,TASOSB                                                  
         BNE   FLDINV              NO COMBINATIONS ALLOWED                      
*                                                                               
VRPCT10  GOTO1 CASHVAL,DMCB,(3,8(R2)),(RF)                                      
         CLI   0(R1),0                                                          
         BNE   FLDINV                                                           
         LA    R4,CAELEM                                                        
         USING TACAD,R4                                                         
         L     RE,4(R1)            SET WRITER PERCENTAGE                        
         C     RE,=F'99999'                                                     
         BH    FLDINV              CAN'T BE MORE THAN 99.999                    
         STCM  RE,15,TACAWPCT                                                   
VRPCTX   B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              VALIDATE PERFORMANCE RATE                                        
VRAMTS   NTR1                                                                   
         LA    R2,SECPERFH                                                      
         BAS   RE,VALAMT           VALIDATE PERFORMANCE RATE                    
         MVC   TPERF,FULL                                                       
*                                                                               
         LA    R2,SECCAMTH                                                      
         BAS   RE,VALAMT           VALIDATE CREDIT AMOUNT                       
         MVC   TCAMT,FULL                                                       
*                                                                               
         LA    R2,SECCBALH         VALIDATE CREDIT BALANCE                      
         CLI   5(R2),0             IF NO CREDIT BALANCE                         
         BNE   VRAMT20                                                          
         MVC   TCBAL,TCAMT         SET TO CREDIT AMOUNT                         
         B     VRAMTX                                                           
*                                                                               
VRAMT20  BAS   RE,VALAMT           VALIDATE CREDIT BALANCE                      
         MVC   TCBAL,FULL                                                       
*                                                                               
VRAMTX   B     XIT                                                              
         SPACE 2                                                                
*              VALIDATE AN AMOUNT POINTED TO BY R2                              
*              RESULT IN FULL                                                   
VALAMT   NTR1                                                                   
         XC    FULL,FULL                                                        
         SR    RF,RF                                                            
         ICM   RF,1,5(R2)                                                       
         BZ    VALAMTX                                                          
         GOTO1 CASHVAL,DMCB,8(R2),(RF) VALIDATE IT                              
         CLI   0(R1),0                                                          
         BNE   FLDINV                                                           
         TM    4(R1),X'80'         NEGATIVE AMOUNT NOT VALID                    
         BO    FLDINV                                                           
         MVC   FULL,4(R1)                                                       
VALAMTX  B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*              DISPLAY THE RECORD                                               
         SPACE 1                                                                
DISPLAY  NTR1                                                                   
         MVC   SVKEY,KEY           SAVE KEY                                     
         GOTO1 FLDVAL,DMCB,(X'01',SECONOFH),(X'80',999)    CLEAR SCREEN         
         XC    SECNCDN,SECNCDN     AGENT NAME                                   
         OI    SECNCDNH+6,X'80'                                                 
         XC    SECCORN,SECCORN     CORP NAME                                    
         OI    SECNCDNH+6,X'80'                                                 
         XC    SECINV,SECINV       INVOICE NUMBER                               
         OI    SECINVH+6,X'80'                                                  
         XC    SECCAMT,SECCAMT     CREDIT APPL AMOUNT                           
         OI    SECCAMTH+6,X'80'                                                 
         XC    SECCBAL,SECCBAL     CREDIT BALANCE AMOUNT                        
         OI    SECCBALH+6,X'80'                                                 
         L     R6,AIO                                                           
         USING TLECD,R6                                                         
         LR    R4,R6                                                            
         MVI   ELCODE,TACAELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   DISP20                                                           
         USING TACAD,R4                                                         
         MVC   SECONOF,TACAONOF    ON/OFF CAMARA                                
         MVC   SECUNIT,TACAUNIT    TAX UNIT CODE                                
         MVC   SECUNLO(L'TACAUN),TACAUN                                         
         MVC   SECUNLO+L'TACAUN+1(L'TACALOCL),TACALOCL                          
         MVC   SECYEAR,TACAYEAR                                                 
         OC    TACANCDE,TACANCDE   DISPLAY AGENT                                
         BZ    DISP5                                                            
         GOTO1 TRNSAGT,DMCB,(X'40',TACANCDE),SECNCDE                            
DISP5    BAS   RE,DRAGNT           DISPLAY AGENT NAME                           
         MVC   SECCORP,TACACORP    DISPLAY CORP NUMBER                          
         BAS   RE,DRCORP           DISPLAY CORP NAME                            
         OC    TACAWPCT,TACAWPCT   DISPLAY WRITER PERCENTAGE                    
         BZ    DISP10                                                           
         EDIT  TACAWPCT,(6,SECPCT),3,ALIGN=RIGHT                                
*                                                                               
DISP10   OC    TLECINV,TLECINV                                                  
         BZ    DISP20                                                           
         GOTO1 TINVCON,DMCB,TLECINV,SECINV,DATCON                               
*                                                                               
DISP20   BAS   RE,DRROLE           DISPLAY WRITER ROLE                          
*                                                                               
         BAS   RE,DRAMTS           DISPLAY PERFRATE, CREDIT AMT & BAL           
*                                                                               
         GOTO1 CHAROUT,DMCB,TACMELQ,SECCMH,TACMTYPG  COMMENT                    
         GOTO1 ACTVOUT,DMCB,SECLCHGH                 LAST CHANGED               
         GOTO1 FLDVAL,DMCB,(X'20',AFRSTREC),999      ALL FLDS VALID             
DISPX    MVC   KEY,SVKEY           RESTORE KEY                                  
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*              DISPLAY AGENT NAME                                               
         SPACE 1                                                                
DRAGNT   NTR1                                                                   
         OC    TACANCDE,TACANCDE   ANY AGENT CODE?                              
         BZ    DRAGNT5                                                          
         MVC   SECNCDN(L'LTMISS),LTMISS                                         
         MVC   AIO,AIO2            YES -READ AGENT REC FOR NAME                 
         GOTO1 RECVAL,DMCB,TLANCDQ,(X'8C',SECNCDE),SECNCDNH                     
         MVC   AIO,AIO1                                                         
         B     DRAGNTX                                                          
*                                                                               
DRAGNT5  LA    R4,W4IO             NO AGENT CODE -                              
         MVI   ELCODE,TAPEELQ      SO LOOK FOR PAYEE ELEMENT ON W4              
         BAS   RE,GETEL                                                         
         BNE   DRAGNTX                                                          
         MVC   SECNCDN,=CL16'* PAYEE ON W4 *'                                   
*                                                                               
DRAGNTX  B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              DISPLAY CORPORATION NAME                                         
         SPACE 1                                                                
DRCORP   NTR1                                                                   
         CLI   SECCORP,C'1'        IF THERE IS A CODE                           
         BL    DCRP20                                                           
         MVC   SECCORN,LTMISS                                                   
*                                                                               
         LA    R1,W4IO                                                          
         ST    R1,AIO                                                           
         MVI   ELCODE,TATIELQ                                                   
         MVI   HALF,TATITYCO                                                    
         MVC   HALF+1(1),SECCORP                                                
         GOTO1 GETL,DMCB,(2,HALF)                                               
         MVC   AIO,AIO1                                                         
         BNE   DCRPX                                                            
*                                                                               
         L     R2,TGELEM           R2=A(TAX ID ELEMENT FOR CORP)                
         USING TATID,R2                                                         
         MVC   AIO,AIO2                                                         
         MVC   SVSSN,TGSSN                                                      
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'8C',TATIID),SECCORNH                      
         MVC   AIO,AIO1                                                         
         MVC   TGSSN,SVSSN                                                      
         B     DCRPX                                                            
*                                                                               
DCRP20   LA    R4,W4IO             NO CORP CODE ON CAST RECORD                  
         MVI   ELCODE,TATIELQ      CHECK ANY CORP ID ON W4 RECORD               
         BAS   RE,GETEL                                                         
         BNE   DCRPX                                                            
         MVC   SECCORN,LTCRP       DISPLAY 'PERF HAS CORP'                      
*                                                                               
DCRPX    B     XIT                                                              
         EJECT                                                                  
*              DISPLAY WRITER ROLE                                              
         SPACE                                                                  
DRROLE   NTR1                                                                   
         LR    R4,R6                                                            
         MVI   ELCODE,TASOELQ                                                   
         BAS   RE,GETEL            GET TASOD ELEMENT                            
         BNE   DRROLEX                                                          
         USING TASOD,R4                                                         
         CLI   TASOSTAT,0          NO WRITER ROLE EQUATE                        
         BE    DRROLEX                                                          
         LA    R3,WROLTAB          LOOK UP CHARACTER CODE FOR DISPLAY           
         USING WROLTABD,R3                                                      
DRROLE10 CLC   WROLEQU,TASOSTAT                                                 
         BE    DRROLE20                                                         
         LA    R3,WROLTABL(R3)                                                  
         CLI   0(R3),X'FF'         IF END OF TABLE                              
         BNE   DRROLE10                                                         
         DC    H'0'                DIE - MUST FIND WRITER ROLE                  
*                                                                               
DRROLE20 MVC   SECWRIR,WROLCHAR    DISPLAY WRITER ROLE TO SCREEN                
DRROLEX  B     XIT                                                              
         DROP  R3,R4                                                            
         EJECT                                                                  
*              DISPLAY PERFORMANCE RATE                                         
         SPACE                                                                  
DRAMTS   NTR1                                                                   
         LR    R4,R6                                                            
         MVI   ELCODE,TACRELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   DRAMTX                                                           
         USING TACRD,R4                                                         
         OC    TACRSCAL,TACRSCAL                                                
         BZ    DRAMT10                                                          
         EDIT  TACRSCAL,(9,SECPERF),2,ALIGN=LEFT                                
*                                                                               
DRAMT10  EDIT  TACRAPPL,(9,SECCAMT),2,ALIGN=LEFT                                
*                                                                               
         EDIT  TACRBAL,(9,SECCBAL),2,ALIGN=LEFT                                 
DRAMTX   B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              EXITS                                                            
FLDINV   MVI   ERROR,INVALID                                                    
         B     ERRXIT                                                           
*                                                                               
FLDMISS  MVI   ERROR,MISSING                                                    
         B     ERRXIT                                                           
*                                                                               
ERRCRP   MVI   ERROR,ERGT1CRP      GT 1 CORP - NEED EXACT CODE                  
         B     ERRXIT                                                           
*                                                                               
ERW4LCK  MVI   ERROR,ERW4SLCK      W4 RECORD LOCKED                             
         B     ERRXIT                                                           
*                                                                               
ERNOTRS  MVC   MYMSGNO,=Y(ERNOTRST) TRUSTEE NOT ALLOWED ON CAST                 
         B     ERREND                                                           
*                                                                               
BADCTYPE MVI   ERROR,ERRECCTY      WRONG COMML TYPE FOR THIS SCREEN             
         B     ERRXIT                                                           
*                                                                               
ERPPLSI  MVC   MYMSGNO,=Y(ERRIAPPA)   RECORD / ACTION INVALID FOR P+            
         J     ERREND                                                           
*                                                                               
ERCRPLK  MVC   MYMSGNO,=Y(ERCPSLCK)    CORP RECORD LOCKED - NON-PAY             
         J     ERREND                                                           
*                                                                               
ERREND   MVI   MYMTYP,GTMERR       ERROR MESSAGE EXIT                           
         OI    GENSTAT2,USGETTXT                                                
         J     THEEND                                                           
*                                                                               
ERRXIT   XC    DMCB,DMCB                                                        
THEEND   GOTO1 EXIT,DMCB,0                                                      
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         SPACE 2                                                                
*              CONSTANTS, ETC.                                                  
         SPACE 2                                                                
PFTAB    DS    0C                                                               
         DC    AL1(PF13X-*,13,0,(PF13X-PF13)/KEYLNQ,0)                          
         DC    CL3'   ',CL8'CHECK   ',CL8'LIST'                                 
PF13     DC    AL1(KEYTYGLB,L'TGSSN-1),AL2(TGSSN-TGD)                           
         DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYGLB,L'TGCID-1),AL2(TGCID-TGD)                           
PF13X    EQU   *                                                                
         SPACE                                                                  
         DC    AL1(PF16X-*,16,0,(PF16X-PF16)/KEYLNQ,0)                          
         DC    CL3'   ',CL8'HISTORY ',CL8'DISPLAY'                              
PF16     DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYTWA,L'SECINV-1),AL2(SECINV-T702FFD)                     
PF16X    EQU   *                                                                
         DC    X'FF'                                                            
         SPACE 1                                                                
LTMISS   DC    C' * NOT FOUND *  '                                              
LTCRP    DC    C'*PERF HAS CORP* '                                              
         SPACE 1                                                                
*          TABLE OF WRITER ROLE EQUATES                                         
*                                                                               
WEQUTAB  DS    0CL2                                                             
         DC    AL1(CTWB),C'B'                                                   
         DC    AL1(CTWS),C'S'                                                   
         DC    AL1(CTWH),C'H'                                                   
         DC    AL1(CTW2B),C'B'                                                  
         DC    AL1(CTW2S),C'S'                                                  
         DC    AL1(CTW2H),C'H'                                                  
         DC    AL1(CTW3B),C'B'                                                  
         DC    AL1(CTW3S),C'S'                                                  
         DC    AL1(CTW3H),C'H'                                                  
         DC    X'FF'                                                            
         SPACE 1                                                                
*          TABLE OF VALID WRITER ROLES                                          
*                                                                               
WROLTAB  DS    0CL3                                                             
         DC    AL1(TASOSH+TASOSS+TASOSB),CL2'A '                                
         DC    AL1(TASOSH+TASOSS),CL2'HS'                                       
         DC    AL1(TASOSH+TASOSB),CL2'HB'                                       
         DC    AL1(TASOSS+TASOSB),CL2'SB'                                       
         DC    AL1(TASOSH),CL2'H '                                              
         DC    AL1(TASOSS),CL2'S '                                              
         DC    AL1(TASOSB),CL2'B '                                              
         DC    X'FF'                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAW4LCRP                                                       
*                LOCAL SAVED STORAGE                                            
ECASTD   DSECT                                                                  
TPERF    DS    XL4                 TEMPORARY PERFORMANCE RATE                   
TCAMT    DS    XL4                 TEMPORARY CREDIT AMOUNT                      
TCBAL    DS    XL4                 TEMPORARY CREDIT BALANCE                     
SVSOSTAT DS    XL1                 SAVED WRITER ROLE EQUATE                     
SVSSN    DS    CL(L'TGSSN)         TEMPORARY SAVED SSN NUMBER                   
CAELEM   DS    CL(L'ELEMENT)       CAST DETAILS ELEMENT                         
SVKEY    DS    CL(L'TLDRREC)       SAVED KEY                                    
PTRBLK   DS    CL(2*L'TLDRREC+1)   PASSIVE + ACTIVE PTRS                        
W4IO     DS    CL4000              W4 RECORD                                    
         SPACE                                                                  
*                WRITER ROLE TABLE DSECT                                        
WROLTABD DSECT                                                                  
WROLEQU  DS    XL1                 EQUATE FOR THIS WRITER ROLE                  
WROLCHAR DS    CL2                 CHAR WRITER ROLE                             
WROLTABL EQU   (*-WROLTABD)                                                     
         SPACE                                                                  
*                WRITER EQUATE TABLE DSECT                                      
WEQUTABD DSECT                                                                  
WEQUCAT  DS    XL1                 CATEGORY EQUATE                              
WEQUROLE DS    CL1                 CHAR WRITER ROLE                             
WEQUTABL EQU   (*-WEQUTABD)                                                     
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRC9D                                                       
         EJECT                                                                  
* DDGENTWA                                                                      
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSEQUS                                                                     
* TASYSDSECT                                                                    
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'038TAGENC9   04/29/15'                                      
         END                                                                    
