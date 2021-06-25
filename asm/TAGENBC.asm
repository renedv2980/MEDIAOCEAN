*          DATA SET TAGENBC    AT LEVEL 039 AS OF 04/28/15                      
*PHASE T702BCA,*                                                                
         TITLE 'T702BC - PRINT CAST LIST'                                       
T702BC   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T702BC                                                         
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
*                                                                               
         GOTO1 INITIAL,DMCB,PFTAB                                               
         MVC   SCATAG(8),=C'Sel ^Pid'                                           
         OI    SCATAGH+6,X'80'                                                  
*                                                                               
         CLI   MODE,VALKEY                                                      
         BNE   *+12                                                             
         BAS   RE,VKEY                                                          
         B     CAX                                                              
*                                                                               
         CLI   MODE,LVALKEY        VALIDATE KEY OF LISTED RECORD                
         BNE   *+12                                                             
         BAS   RE,LVKEY                                                         
         B     CAX                                                              
*                                                                               
         CLI   MODE,LVALREC        VALIDATE LISTED RECORD                       
         BNE   *+12                                                             
         BRAS  RE,LVREC                                                         
         B     CAX                                                              
*                                                                               
         CLI   MODE,XRECADD        IF JUST ADDED RECORD                         
         BNE   CA15                                                             
         XC    PTRBLK,PTRBLK       CLEAR SAVED POINTER BLOCK                    
         L     R3,ATHISLST         R3=A(THIS DATA LINE)                         
         USING LINED,R3                                                         
         OI    LINSSNH+1,X'20'     PROTECT IT                                   
         OI    LINSSNH+6,X'80'                                                  
         B     *+12                                                             
CA15     CLI   MODE,XRECPUT        IF JUST CHANGED RECORD                       
         BNE   CA20                                                             
         GOTO1 AADDPTRS,DMCB,PTRBLK  UPDATE POINTERS                            
*                                                                               
         CLI   MODE,XRECADD        IF JUST ADDED RECORD                         
         BNE   CAX                                                              
         BRAS  RE,UPNXTSEQ         UPDATE COMML'S NEXT CAST SEQ NUMBER          
         B     CAX                                                              
*                                                                               
CA20     CLI   MODE,LISTRECS       LIST RECORDS                                 
         BNE   CA30                                                             
         LA    R0,LRHOOK           SET A(SYSIO HOOK)                            
         BAS   RE,LREC                                                          
         B     CAX                                                              
*                                                                               
CA30     CLI   MODE,PRINTREP                                                    
         BNE   CAX                                                              
         XC    KEY,KEY             START REPORT FROM BEGINING                   
         ZAP   COUNTER,=P'0'       LINE COUNTER                                 
         LA    R0,MYSPECS          SET A(SPECS)                                 
         ST    R0,SPECS                                                         
         LA    R0,HDHOOK           SET A(HEADLINE HOOK)                         
         ST    R0,HEADHOOK                                                      
         LA    R0,PRHOOK           SET A(SYSIO HOOK)                            
         BAS   RE,LREC                                                          
*                                                                               
CAX      B     XIT                                                              
         SPACE 2                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
* VALIDATE THE KEY                                                              
*                                                                               
VKEY     NTR1                                                                   
         CLI   SCRSTAT,0           IF SCREEN CHANGED                            
         BE    *+8                                                              
         NI    SCAAGYH+4,X'DF'     SET TO RE-VALIDATE                           
*                                                                               
         LA    R2,SCAAGYH          AGENCY                                       
         TM    4(R2),X'20'                                                      
         BO    VK10                                                             
         NI    SCACIDH+4,X'DF'                                                  
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'28',(R2)),0                               
         MVC   AGYNAME,TGNAME                                                   
                                                                                
         L     R4,AIO                                                           
         USING TAAYD,R4            KEEP 2ND STATUS BYTE TO                      
         MVI   ELCODE,TAAYELQ                                                   
         BAS   RE,GETEL            CHECK LOCKED STATUS OF AGENCY                
         BNE   *+10                                                             
         MVC   SVAYSTA6,TAAYSTA6                                                
                                                                                
         GOTO1 RAVPPLSA,DMCB,0     SEE IF REC / ACT INVALID FOR P+              
         JNE   ERPPLSI                                                          
*                                                                               
VK10     LA    R2,SCACIDH          COMMERCIAL ID                                
         TM    4(R2),X'20'                                                      
         BO    VK100                                                            
         GOTO1 RECVAL,DMCB,TLCOICDQ,(X'0A',(R2)),SCACIDNH                       
         L     R4,AIO                                                           
         USING TLCOD,R4                                                         
         MVC   TGCLI,TLCOCLI       SAVE CLIENT CODE                             
         MVC   TGCOM,TLCOCOM       SAVE INTERNAL COMMERCIAL NUMBER              
         SPACE 1                                                                
         TM    TGMEEQU,PRINT       ENSURE THIS IS PRINT COMMERCIAL              
         BZ    BADCTYPE                                                         
         SPACE 1                                                                
         OI    4(R2),X'20'         MANUALLY SET VALIDATION BIT                  
         XC    COMLSDTE,COMLSDTE   CLEAR COMMERCIAL SHOOT DATE                  
         MVI   ELCODE,TACSELQ      GET COMMERCIAL STUDIO ELEMENT                
         GOTO1 GETL,DMCB,(1,=AL1(TACSTYPS))                                     
         BNE   VK30                                                             
         L     R4,TGELEM                                                        
         USING TACSD,R4                                                         
         MVC   COMLSDTE,TACSDATE   SAVE COMMERCIAL SHOOT DATE                   
*                                                                               
VK30     GOTO1 CHAROUT,DMCB,TANUELQ,0,TANUTPHO  GET PHOTOGRAPHER SSN            
         MVC   COMLPHOT,TGNAME                                                  
*                                                                               
         GOTO1 CHAROUT,DMCB,TACMELQ,SCACCMTH,TACMTYPG  DISP COMML CMNT          
                                                                                
*                                  READ CLIENT RECORD FOR STA2                  
         MVI   SVCISTA2,0                                                       
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'A4',TGCLI)                                
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,AIO                                                           
         MVI   ELCODE,TACIELQ                                                   
         USING TACID,R4                                                         
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   SVCISTA2,TACISTA2                                                
                                                                                
*                                                                               
         XC    KEY,KEY             RESTART LIST - CLEAR KEY                     
*                                                                               
VK100    DS    0H                                                               
*              SET I DISPLAY/RETURN EXTRA/NEED VALIDATION/OK TO ADD             
         OI    GLSTSTAT,APPLCDSP+RETEXTRA+CHNGLIST+OKTOADD                      
         MVC   LLIST,=Y(LINLNQ)            SET L'DATA LINE                      
         MVI   NLISTS,4                    AND N'LINES                          
*                                                                               
         LA    R0,TASYSIOD         CLEAR SYSIO BLOCK                            
         LHI   R1,TIEND-TASYSIOD                                                
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
VKX      B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO CONTROL LISTING RECORDS                               
         SPACE 1                                                                
LREC     NTR1                                                                   
         GOTO1 FLDVAL,DMCB,(X'23',SCAL1H),SCALSTH  CLEAR/SET VER SCREEN         
         NI    SCASSN1H+1,X'DF'    UNPROTECT SSN FIELDS                         
         NI    SCASSN2H+1,X'DF'                                                 
         NI    SCASSN3H+1,X'DF'                                                 
         NI    SCASSN4H+1,X'DF'                                                 
*                                                                               
         ST    R0,TIHOOK           SET HOOK TO SYSIO                            
         MVC   TIACOMFC,ACOMFACS                                                
         MVC   TIUSERID,TWAORIG    SET UP NECCESSARY DATA FROM                  
         MVC   TIQSTAFF,TGCTSTAF       GLOBAL STORAGE                           
         MVI   TIREAD,TLCACDQ      READ CAST RECORDS                            
         MVC   TIFCOM,TGCOM        INTERNAL COMMERCIAL NUMBER                   
         MVC   TIKHOOK,SETLSTK     A(KEY SETTING ROUTINE)                       
         MVI   TIQFLAGS,TIQFDIR                                                 
*                                                                               
         CLI   MODE,LISTRECS       IF LISTING ON SCREEN                         
         BNE   *+12                                                             
         CLI   PFAID,14            AND USER WANTS A CLEAN SCREEN                
         BE    LR10                SKIP                                         
*                                                                               
         GOTO1 TASYSIO,DMCB,TASYSIOD  OFF TO SYSIO TO DO I/O                    
*                                                                               
LR10     XC    TIQSKEY,TIQSKEY     END OF LIST - CLEAR CONTINUE KEY             
*                                                                               
         CLI   MODE,LISTRECS       IF LISTING ON SCREEN                         
         BNE   LR20                                                             
         CLI   PFAID,14            AND USER PRESSED 'ADD' PFKEY                 
         BE    PLSENTER            DISPLAY PLEASE ENTER REQUIRED FIELDS         
         B     LRX                 ELSE RETURN                                  
*                                                                               
LR20     CP    COUNTER,=P'0'       THIS MUST BE PRINTREP                        
         BE    LRX                                                              
******** GOTO1 SPOOL,DMCB,(R8)             **************************           
******** EDIT  COUNTER,(8,P),ALIGN=LEFT    ** DON'T DISPLAY TOTAL  **           
******** LR    R1,R0                       ** RECORDS, FOR NOW.    **           
******** LA    R1,P+1(R1)                  **                      **           
******** MVC   0(12,R1),=C'CAST RECORDS'   ** LEAVE CODE, IN CASE. **           
******** GOTO1 SPOOL,DMCB,(R8)             **************************           
*                                                                               
         TM    WHEN,X'40'          IF SPOOLING AND NOW                          
         BZ    LRX                                                              
         XC    CONSERV,CONSERV     AUTO $DQU                                    
         MVC   CONSERV(4),=C'$DQU'                                              
*                                                                               
LRX      B     XIT                                                              
         EJECT                                                                  
*              PROCESS SYSIO RECORDS ON SCREEN                                  
         SPACE 1                                                                
LRHOOK   NTR1                                                                   
         CLI   TIMODE,PROCDIR      PROCESS DIRECTORY                            
         BNE   LRH10                                                            
         BAS   RE,FILTKEY          FILTER KEY                                   
         BE    YES                                                              
         B     NO                                                               
*                                                                               
LRH10    CLI   TIMODE,PROCREC      PROCESS RECORDS                              
         BNE   LRHX                                                             
         CLC   LISTNUM,NLISTS      IF ALREADY DISPLAYED MAX                     
         BE    LRH60                  GO BACK TO LISTMON                        
*                                                                               
         BAS   RE,DISPLAY          GO DISPLAY IT                                
*                                                                               
         MVC   KEY,TIKEY           NEED TO RESTORE READ SEQUENCE                
         GOTO1 HIGH                                                             
*                                                                               
         MVC   DMDSKADD,TIDSKADD   SET D/A FOR LISTMON                          
*                                                                               
LRH60    GOTO1 LISTMON             CALL LISTMON                                 
*                                                                               
LRHX     B     XIT                                                              
         EJECT                                                                  
*              FILTER KEY                                                       
         SPACE 1                                                                
         USING TLCAD,R3                                                         
FILTKEY  NTR1                                                                   
         LA    R3,TIKEY                                                         
         CLC   TLCACOM,TIFCOM                                                   
         BE    YES                                                              
         B     NO                                                               
         DROP  R3                                                               
         EJECT                                                                  
*              DISPLAY RECORD IN TIAREC ON SCREEN                               
         SPACE 1                                                                
DISPLAY  NTR1                                                                   
         L     R3,ATHISLST         R3=A(THIS LINE)                              
         USING LINED,R3                                                         
         L     R4,TIAREC           R4=A(RECORD)                                 
         USING TLCAD,R4                                                         
*                                                                               
         MVC   LINSSN,TLCASSN      S/S NUMBER                                   
         OI    LINSSNH+1,X'20'     PROTECT IT                                   
         MVC   LINCAT,TLCACAT      CATEGORY                                     
*                                                                               
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'8C',LINSSN),0  GET W4 REC                 
         GOTO1 SSNPACK,DMCB,TLCASSN,TGPID                                       
         MVC   LINSSN,SPACES                                                    
         MVC   LINSSN(L'TGPID),TGPID                                            
         MVI   LINSSNH+5,6                                                      
         OI    LINSSNH+6,X'80'                                                  
*                                                                               
DIS10    L     R4,AIO                                                           
         USING TAW4D,R4                                                         
         MVI   ELCODE,TAW4ELQ      GET W4 ELEMENT                               
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    TAW4STA2,TAW4SLCK   IF RECORD IS LOCKED                          
         BNO   DIS20                                                            
         MVC   LINSSNN(16),=C'** W4 LOCKED ** ' PUT OUT MESSAGE                 
         B     *+10                                                             
*                                                                               
DIS20    MVC   LINSSNN,TGNAME      ELSE DISPLAY NAME                            
*                                                                               
         L     R4,TIAREC                                                        
         MVI   ELCODE,TACAELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   DIS70                                                            
         USING TACAD,R4                                                         
*                                                                               
         BAS   RE,DISCORP          CORP CODE AND NAME                           
*                                                                               
         MVC   LINTAX,TACAUNIT     TAX UNIT                                     
*                                                                               
         OC    TACANCDE,TACANCDE   AGENT                                        
         BZ    DIS30                                                            
         GOTO1 TRNSAGT,DMCB,(X'40',TACANCDE),LINAGT                             
         MVC   LINAGTN(L'LTMISS),LTMISS              DISPLAY MISSING            
         GOTO1 RECVAL,DMCB,TLANCDQ,(X'8C',LINAGT),0  GET AGENT REC              
         BNE   DIS30                                                            
         MVC   LINAGTN,TGNAME                        DISPLAY NAME               
*                                                                               
DIS30    OC    TACASDTE,TACASDTE                                                
         BZ    DIS40                                                            
         GOTO1 DATCON,DMCB,(1,TACASDTE),(8,LINSDTE)  SHOOT DATE                 
*                                                                               
DIS40    MVC   LINPAYE,TACAPAYE    PAYEE CODE                                   
*                                                                               
         EDIT  (2,TACARATE),(5,LINRATE),2,ZERO=BLANK  AGENCY FEE RATE           
*                                                                               
         MVC   LINEBAS,TACAEBAS    EXPIRATION DATE CALC. BASIS                  
*                                                                               
DIS70    MVC   LINCMNTT,LTCMNT     DISPLAY COMMENT LITERAL                      
         MVC   AIO,TIAREC                                                       
         GOTO1 CHAROUT,DMCB,TACMELQ,LINCMNTH,TACMTYPG  COMMENT                  
         MVC   AIO,AIO1                                                         
*                                                                               
         MVC   LINLCHGT,LTLCHG     DISPLAY LAST CHANGED LITERAL                 
         MVC   AIO,TIAREC                                                       
         GOTO1 ACTVOUT,DMCB,LINLCHGH                    INFO                    
         MVC   AIO,AIO1                                                         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE DISPLAYS CORP INFO                                       
         SPACE 1                                                                
DISCORP  NTR1                                                                   
         MVC   LINCRP,TACACORP     CORP CODE                                    
         SPACE 1                                                                
         CLI   LINCRP,C'1'         IF THERE'S A CODE                            
         BL    DCRP20                                                           
         MVC   LINCRPN(L'LTMISS),LTMISS  DISPLAY MISSING LIT                    
         SPACE 1                                                                
         MVI   ELCODE,TATIELQ      W4 RECORD IS IN AIO                          
         MVI   HALF,TATITYCO       BUILD GETL SEARCH ARGUMENT                   
         MVC   HALF+1(1),LINCRP                                                 
         GOTO1 GETL,DMCB,(2,HALF)                                               
         BNE   DCRPX               SKIP IF NOT FOUND                            
         SPACE 1                                                                
         L     R2,TGELEM           R2=A(TAX ID ELEMENT FOR CORP)                
         USING TATID,R2                                                         
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'8C',TATIID),0  GET CORP W4 REC            
         MVC   LINCRPN,TGNAME                        DISPLAY NAME               
         L     R4,AIO                                                           
         MVI   ELCODE,TAW4ELQ      SEE IF CORP IS LOCKED                        
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         USING TAW4D,R4                                                         
         TM    TAW4STA2,TAW4SLCK   IF THIS CRP IS LOCKED                        
         BZ    DCRPX                                                            
         MVC   LINCRPN(L'LTCRPL),LTCRPL  DISPLAY 'CORP IS LOCKED'               
                                                                                
         B     DCRPX                                                            
         SPACE 1                                                                
DCRP20   L     R4,AIO              NO CORP CODE ON CAST RECORD                  
         MVI   ELCODE,TATIELQ      IF THERE'S A CORP ID IN W4 REC.              
         BAS   RE,GETEL                                                         
         BNE   DCRPX                                                            
         MVC   LINCRPN(L'LTCRP),LTCRP  DISPLAY 'PERF HAS CORP'                  
         SPACE 1                                                                
DCRPX    B     XIT                                                              
         EJECT                                                                  
*              PROCESS SYSIO RECORDS WHEN PRINTING                              
         SPACE 1                                                                
PRHOOK   NTR1                                                                   
         CLI   TIMODE,PROCREC      PROCESS RECORDS                              
         BNE   PRHX                                                             
         LA    R3,SCASSN1H         SET A(CURRENT LINE TO 1ST LINE)              
         ST    R3,ATHISLST                                                      
         USING LINED,R3                                                         
         BAS   RE,DISPLAY          DISPLAY RECORD TO FIRST SCREEN LINE          
         SPACE 1                                                                
         MVI   FORCEHED,C'Y'       START NEW PAGE FOR EACH CAST MEMBER          
         LA    R2,P                                                             
         USING PRNTD,R2            R2=A(PRINT LINE)                             
         SPACE 1                                                                
         XC    TGAREA,TGAREA       CLEAR GLOBAL AREA CODE                       
         L     R4,TIAREC                                                        
         MVI   ELCODE,TADLELQ      SET TO LOOP THROUGH DEAL ELEMENTS            
         BAS   RE,GETEL                                                         
         BNE   PRH90                                                            
         USING TADLD,R4            R4=A(DEAL ELEMENT)                           
         SPACE 1                                                                
PRH20    CLC   TADLAREA,TGAREA     IF THIS AREA IS SAME AS PREVIOUS             
         BE    PRH30               DON'T BOTHER DISPLAYING AGAIN                
         SPACE 1                                                                
         MVC   PRNTAREA,TADLAREA   AREA                                         
         GOTO1 RECVAL,DMCB,TLARCDQ,(X'8C',PRNTAREA),0  GET NAME                 
         MVC   PRNTAREN,TGNAME                                                  
         SPACE 1                                                                
PRH30    MVC   PRNTUSE,TADLUSE     USE                                          
         GOTO1 RECVAL,DMCB,TLUSCDQ,(X'8C',PRNTUSE),0   GET NAME                 
         MVC   PRNTUSEN,TGNAME                                                  
         SPACE 1                                                                
         EDIT  (1,TADLTERM),(2,PRNTTERM),ZERO=BLANK    TERM                     
         SPACE 1                                                                
         EDIT  (4,TADLAMT),(10,PRNTAMT),2,ZERO=NOBLANK AMOUNT                   
         SPACE 1                                                                
         EDIT  (2,TADLRATE),(5,PRNTRATE),2,ZERO=BLANK  AGENCY COMM RATE         
         SPACE 1                                                                
         OC    TADLPUB,TADLPUB     PUBLICATION DATE                             
         BZ    PRH50                                                            
         GOTO1 DATCON,DMCB,(1,TADLPUB),(8,PRNTPUB)                              
         SPACE 1                                                                
PRH50    OC    TADLEXP,TADLEXP     EXPIRATION DATE                              
         BZ    PRH60                                                            
         GOTO1 DATCON,DMCB,(1,TADLEXP),(8,PRNTEXP)                              
         SPACE 1                                                                
PRH60    GOTO1 SPOOL,DMCB,(R8)     PRINT THE LINE                               
         SPACE 1                                                                
         MVI   ELCODE,TADLELQ      RESET FOR DEAL ELEMENTS                      
         BAS   RE,NEXTEL           GET NEXT ELEMENT                             
         BE    PRH20                                                            
         SPACE 1                                                                
PRH90    AP    COUNTER,=P'1'       ADD TO CAST COUNTER                          
         SPACE 1                                                                
         GOTO1 FLDVAL,DMCB,(X'01',LINSSNH),LINLCHGH  CLEAR SCREEN FLDS          
         SPACE 1                                                                
         MVC   KEY,TIKEY           NEED TO RESTORE READ SEQUENCE                
         GOTO1 HIGH                                                             
         SPACE 1                                                                
PRHX     B     XIT                                                              
         EJECT                                                                  
*              ROUTINE VALIDATES KEY OF LISTED RECORDS                          
         SPACE 2                                                                
LVKEY    NTR1                                                                   
         L     R3,ATHISLST         R3=A(THIS SCREEN LINE)                       
         USING LINED,R3                                                         
         SPACE 1                                                                
         LA    R2,LINSSNH                                                       
         CLI   LINSSNH+5,6                                                      
         BH    LVK05                                                            
         MVC   TGPID,LINSSN                                                     
         GOTO1 SSNUNPK,DMCB,TGPID,TGSSN                                         
         BNE   LVK05                                                            
         MVC   LINSSN,TGSSN                                                     
         MVI   LINSSNH+5,9                                                      
LVK05    GOTO1 RECVAL,DMCB,TLW4CDQ,(X'08',(R2)),0   S/S NUMBER                  
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   LINSSN,SPACES                                                    
         MVC   LINSSN(L'TGPID),TGPID                                            
         MVI   LINSSNH+5,6                                                      
         OI    LINSSNH+6,X'80'                                                  
         SPACE 1                                                                
LVK10    MVI   ELCODE,TAW4ELQ      GET W4 ELEMENT                               
         L     R4,AIO                                                           
         USING TAW4D,R4                                                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    TAW4STA2,TAW4SLCK   IF THIS W4 IS LOCKED                         
         BO    ERW4LCK             EXIT WITH ERROR MESSAGE                      
         CLI   TAW4TYPE,TAW4TYTR   IF THIS W4 RECORD IS TRUSTEE                 
         BE    ERNOTRS             EXIT WITH ERROR MESSAGE                      
                                                                                
         TM    TAW4STA3,TAW4SREG   IF W4 IS FOR REGRESSION TESTING              
         JZ    LVK14                                                            
*                                                                               
         TM    SVAYSTA6,TAAYSREG   ENSURE AGENCY                                
         JO    LVK18                                                            
         TM    SVCISTA2,TACISREG   OR CLIENT IS FOR REGRESSION TESTING          
         JO    LVK18                                                            
         J     ERW4REG                                                          
                                                                                
LVK14    TM    SVAYSTA6,TAAYSREG   IF W4 IS NOT FOR REGRESSION TESTING          
         JO    ERW4NREG            ENSURE AGENCY                                
         TM    SVCISTA2,TACISREG   AND CLIENT ARE NOT FOR REGRESSION            
         JO    ERW4NREG            TESTING                                      
                                                                                
         SPACE 1                                                                
LVK18    MVC   LINSSNN,TGNAME                                                   
         SPACE 1                                                                
         LA    R2,LINCATH          CATEGORY                                     
         OC    8(3,R2),SPACES                                                   
         CLI   5(R2),0             IF NOTHING INPUT                             
         BNE   LVK20                                                            
         OC    TGCAT,TGCAT         AND THERE'S NO GLOBAL CATEGORY               
         BZ    FLDMISS             THEN REQUIRE INPUT                           
         MVC   8(3,R2),TGCAT       ELSE USE GLOBAL CATEGORY                     
         OI    6(R2),X'80'                                                      
LVK20    GOTO1 CATVAL,DMCB,8(R2)   VALIDATE CATEGORY CODE                       
         BNE   FLDINV                                                           
         CLI   TGCAEQU,CTZZ        ALLOW DUMMY CATEGORY ZZ                      
         BE    LVK30                                                            
         CLI   TGCAEQU,CTZZZ       ALLOW DUMMY CATEGORY                         
         BE    LVK30                                                            
         OC    TGCAUNIS,TGCAUNIS   VALID ONLY IF NO UNIONS DEFINED              
         BNZ   FLDINV                                                           
LVK30    OI    4(R2),X'20'         SET PREV. VALIDATED                          
         SPACE 1                                                                
         XC    TGCSORT,TGCSORT                CLEAR CAST SORT KEY               
         GOTO1 RECVAL,DMCB,TLCACDQ,(X'40',0)  BUILD KEY FOR CAST                
*                                                                               
         TM    LINSSNH+1,X'20'     NOT ADD IF SS# IS PROTECTED                  
         BO    XIT                                                              
         LA    R2,LINSSNH                                                       
         BRAS  RE,CHKMODL          CHK MODEL/CAT NOT IN COMML ALREADY           
         BNE   ERMDLXST            MODEL/CAST ALREADY EXIST                     
         BRAS  RE,STNXTSEQ         SET NEXT CAST SEQ NUMBER IN KEY              
         B     XIT                                                              
         EJECT                                                                  
*              HEADLINE HOOK FOR REPORT PRINTING                                
         SPACE 1                                                                
HDHOOK   NTR1                                                                   
         MVC   HEAD4+12(6),SCAAGY  AGENCY CODE                                  
         MVC   HEAD4+26(16),AGYNAME       NAME                                  
         SPACE 1                                                                
         MVC   HEAD5+12(12),SCACID COMMERCIAL ID                                
         MVC   HEAD5+26(16),SCACIDN           NAME                              
         SPACE 1                                                                
         L     R3,ATHISLST         R3=A(CURRENT REC DISPLAYED ON SCRN)          
         USING LINED,R3                                                         
         SPACE 1                                                                
         MVC   HEAD6+12(9),LINSSN  S/S NUMBER                                   
         MVC   HEAD6+26(16),LINSSNN    NAME                                     
         SPACE 1                                                                
         MVC   HEAD7+12(3),LINCAT  CATEGORY                                     
         SPACE 1                                                                
         MVC   HEAD9+12(8),LINSDTE    SHOOT DATE                                
         MVC   HEAD10+12(1),LINEBAS   EXPIRATION CALC. BASIS                    
         MVC   HEAD11+12(60),LINCMNT  COMMENT                                   
         SPACE 1                                                                
         MVC   HEAD4+55(1),LINCRP  CORP CODE                                    
         MVC   HEAD4+61(16),LINCRPN     NAME                                    
         SPACE 1                                                                
         MVC   HEAD5+55(4),LINAGT  AGENT CODE                                   
         MVC   HEAD5+61(16),LINAGTN      NAME                                   
         SPACE 1                                                                
         MVC   HEAD7+61(1),LINPAYE  PAYEE CODE                                  
         MVC   HEAD8+61(3),LINTAX   TAX UNIT                                    
         MVC   HEAD9+61(5),LINRATE  AGENCY COMMISSION RATE                      
         B     XIT                                                              
         EJECT                                                                  
*              ERRORS, EXITS                                                    
*                                                                               
FLDINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         J     THEEND                                                           
*                                                                               
         USING LINED,R3                                                         
AGTMISS  LA    R2,LINAGTH                                                       
FLDMISS  MVI   ERROR,MISSING       MISSING INPUT FIELD                          
         J     THEEND                                                           
*                                                                               
BADCTYPE MVI   ERROR,ERRECCTY      WRONG COMML TYPE FOR THIS SCREEN             
         L     R2,EFHREC                                                        
         J     THEEND                                                           
*                                                                               
ERRPHO   MVI   ERROR,ERMISPHO      PHOTOGRAPHER NOT DEFINED ON COML REC         
         J     THEEND                                                           
*                                                                               
NOSSN    MVI   ERROR,ERNOSSN       MISSING SSN FOR THIS AGENT                   
         J     THEEND                                                           
*                                                                               
AMTINV   MVI   ERROR,ERINVAMT      INVALID AMOUNT                               
         J     THEEND                                                           
*                                                                               
ERW4LCK  MVI   ERROR,ERW4SLCK      W4 RECORD LOCKED                             
         J     THEEND                                                           
*                                                                               
ERNOTRS  LHI   RE,ERNOTRST         TRUSTEE NOT ALLOWED ON CAST                  
         STH   RE,MYMSGNO                                                       
         J     ERREND                                                           
*                                                                               
ERMDLXST MVI   ERROR,RECEXIST      RECORD ALREADY EXISTS                        
         J     THEEND                                                           
*                                                                               
ERW4REG  LHI   RE,536              W4 IS FOR REGRESSION TESTING                 
         STH   RE,MYMSGNO                                                       
         J     ERREND                                                           
*                                                                               
ERW4NREG LHI   RE,537              W4 IS NOT FOR REGRESSION TESTING             
         STH   RE,MYMSGNO                                                       
         J     ERREND                                                           
*                                                                               
PLSENTER MVI   MYMSGNO1,2          PLEASE ENTER REQUIRED FIELDS                 
         MVI   MYMSYS,X'FF'                                                     
         LA    R2,SCASSN1H         CURSOR TO FIRST SSN FIELD                    
         J     INFEND                                                           
*                                                                               
ERPPLSI  LHI   RE,ERRIAPPA                                                      
         STH   RE,MYMSGNO          RECORD / ACTION INVALID FOR P+               
         J     ERREND                                                           
                                                                                
ERREND   MVI   MYMTYP,GTMERR       ERROR MESSAGE EXIT                           
INFEND   OI    GENSTAT2,USGETTXT   SET INFO MESSAGE                             
THEEND   GOTO1 EXIT,DMCB,0                                                      
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
*                                                                               
PFTAB    DS    0C                                                               
         DC    AL1(PF3X-*,3,0,(PF3X-PF3)/KEYLNQ,0)                              
         DC    CL3'W4 ',CL8'W4      ',CL8'DISPLAY'                              
PF3      DC    AL1(KEYTYCUR,L'LINSSN-1),AL2(LINSSN-LINSSN)                      
PF3X     EQU   *                                                                
*                                                                               
         DC    AL1(PF13X-*,13,0,(PF13X-PF13)/KEYLNQ,0)                          
         DC    CL3'   ',CL8'CHECK   ',CL8'LIST'                                 
PF13     DC    AL1(KEYTYCUR,L'LINSSN-1),AL2(LINSSN-LINSSN)                      
         DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYGLB,L'TGCID-1),AL2(TGCID-TGD)                           
PF13X    EQU   *                                                                
         DC    AL1(PF14X-*,14,0,0,PFTRETRN)                                     
         DC    CL3' ',CL8'        ',CL8'        '                               
PF14X    EQU   *                                                                
         DC    X'FF'                                                            
*                                                                               
LTMISS   DC    C'*Not Found*'                                                   
LTCRP    DC    C'*Perf has Corp*'                                               
LTCRPL   DC    C'*Corp is Locked*'                                              
LTCMNT   DC    C'Cmnt'                                                          
LTLCHG   DC    C'Last Changed:'                                                 
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*              SPECS FOR REPORT                                                 
         SPACE 1                                                                
MYSPECS  DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,56,REPORT                                                     
         SSPEC H1,73,PAGE                                                       
         SSPEC H2,56,REQUESTOR                                                  
         SPACE 1                                                                
         SSPEC H1,34,C'Print Ad Cast'                                           
         SSPEC H2,34,C'-------------'                                           
         SPACE 1                                                                
         SSPEC H4,1,C'Agency'                                                   
         SSPEC H5,1,C'Comml ID'                                                 
         SSPEC H6,1,C'S/S Number'                                               
         SSPEC H7,1,C'Category'                                                 
         SPACE 1                                                                
         SSPEC H9,1,C'Shoot Date'                                               
         SSPEC H10,1,C'Exp Basis'                                               
         SSPEC H11,1,C'Comment'                                                 
         SPACE 1                                                                
         SSPEC H4,49,C'Corp'                                                    
         SSPEC H5,49,C'Agent'                                                   
         SPACE 1                                                                
         SSPEC H7,49,C'Payee'                                                   
         SSPEC H8,49,C'Tax Unit'                                                
         SSPEC H9,49,C'Agy Comm%'                                               
         SPACE 1                                                                
         SSPEC H13,1,C'Area'                                                    
         SSPEC H14,1,C'----'                                                    
         SSPEC H13,22,C'Use'                                                    
         SSPEC H14,22,C'---'                                                    
         SSPEC H13,43,C'Mo     Amount AgyC% Pub-Date Exp-Date'                  
         SSPEC H14,43,C'--     ------ ----- -------- --------'                  
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
       ++INCLUDE TAPCSTLCK                                                      
         EJECT                                                                  
*              ROUTINE VALIDATES LISTED RECORDS                                 
         SPACE 1                                                                
LVREC    NTR1  BASE=*,LABEL=*                                                   
         MVC   SVKEY,KEY             SAVE KEY                                   
         GOTO1 ASAVPTRS,DMCB,PTRBLK  SAVE POINTERS                              
                                                                                
         L     R3,ATHISLST         R3=A(THIS DATA LINE)                         
         USING LINED,R3                                                         
                                                                                
         MVI   ELCODE,TACAELQ      REMOVE EXISTING CAST DETAILS EL.             
         GOTO1 REMELEM                                                          
                                                                                
         USING TACAD,R4                                                         
         XC    ELEMENT,ELEMENT     BUILD NEW CAST DETAILS ELEMENT               
         LA    R4,ELEMENT                                                       
         MVI   TACAEL,TACAELQ      ELEMENT CODE                                 
         MVI   TACALEN,TACALNQ     ELEMENT LENGTH                               
*                                                                               
         LA    R2,LINTAXH          TAX UNIT                                     
         GOTO1 ANY                                                              
         GOTO1 TAXVAL,DMCB,(3,WORK)                                             
         JE    LVR5                                                             
         MVC   TGCTRY,=C'CA'                                                    
         GOTO1 TAXVAL,DMCB,(X'FF',WORK)                                         
         JNE   FLDINV                                                           
*                                                                               
LVR5     TM    TGTASTAT,TASUINGF   ENSURE TAX UNIT IS VALID GOING               
         JO    FLDINV              FORWARD                                      
         MVC   TACAUNIT,WORK                                                    
*                                                                               
         USING TAAND,R4                                                         
         CLI   LINAGTH+5,0         AGENT CODE (OPTIONAL)                        
         JE    LVR10                                                            
         LA    R2,LINAGTH                                                       
         MVC   AIO,AIO2            DON'T CREAM RECORD AT AIO                    
         GOTO1 RECVAL,DMCB,TLANCDQ,(X'20',(R2))                                 
         L     R4,AIO                                                           
         MVC   AIO,AIO1            RESTORE AIO                                  
         CLI   LINRATEH+5,0        TEST HAVE AGENT FEE INPUT                    
         JE    LVR8                                                             
                                                                                
         MVI   ELCODE,TAANELQ      GET AGENT ELEMENT                            
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         OC    TAANSSN,TAANSSN     MUST HAVE SSN                                
         JZ    NOSSN                                                            
                                                                                
         USING TACAD,R4                                                         
LVR8     LA    R4,ELEMENT                                                       
         GOTO1 TRNSAGT,DMCB,(X'80',TGAGT),TACANCDE                              
                                                                                
LVR10    LA    R2,LINSDTEH         SHOOT DATE                                   
         CLI   5(R2),0             IF FIELD IS EMPTY                            
         JNE   *+14                                                             
         OC    COMLSDTE,COMLSDTE   AND COMML SHOOT DATE DEFINED                 
         JNZ   LVR20               THEN SKIP VALIDATION                         
         GOTO1 DTVAL,DMCB,TACASDTE                                              
*                                                                               
LVR20    BRAS  RE,VALCORP          VALIDATE CORP FIELD                          
*                                                                               
         LA    R2,LINPAYEH         PAYEE CODE                                   
         GOTO1 ANY                                                              
         CLI   8(R2),TACAPMOD      ALLOW MODEL                                  
         JE    LVR30                                                            
         CLI   8(R2),TACAPAGT      IF AGENT                                     
         JNE   *+16                                                             
         CLI   LINAGTH+5,0         THEN AGENT MUST BE INPUT                     
         JE    AGTMISS                                                          
         J     LVR30                                                            
         CLI   8(R2),TACAPPHO      IF PHOTOGRAPHER                              
         JNE   FLDINV                                                           
         CLC   COMLPHOT,SPACES     THEN PHOTOGRAPHER MUST BE DEFINED            
         JE    ERRPHO                                                           
LVR30    MVC   TACAPAYE,8(R2)                                                   
                                                                                
         LA    R2,LINRATEH         AGENCY FEE RATE                              
         CLI   5(R2),0             IF HAVE INPUT                                
         JE    LVR40                                                            
         BAS   RE,AMTVAL           VALIDATE IT                                  
         CLC   FULL,=F'9999'                                                    
         JH    AMTINV                                                           
         MVC   TACARATE,FULL+2                                                  
         CLI   LINAGTH+5,0         THEN AGENT MUST BE INPUT                     
         JE    AGTMISS                                                          
                                                                                
LVR40    LA    R2,LINEBASH         EXPIRATION DATE BASIS                        
         MVI   TACAEBAS,TACAESHT   DEFAULT TO SHOOT DATE                        
         CLI   5(R2),0                                                          
         JE    LVR50                                                            
         CLI   8(R2),TACAESHT      ALLOW SHOOT DATE                             
         JE    LVR50                                                            
         CLI   8(R2),TACAEPUB            PUBLICATION DATE                       
         JNE   FLDINV                                                           
         MVC   TACAEBAS,8(R2)                                                   
                                                                                
LVR50    GOTO1 ADDELEM             ADD CAST DETAILS ELEMENT                     
                                                                                
         GOTO1 NAMIN,DMCB,TACMELQ,(X'80',LINCMNTH),TACMTYPG  COMMENT            
                                                                                
         BAS   RE,ALLDEAL          INSURE ALL DEALS ACCOUNTED FOR               
                                                                                
         GOTO1 ACTVIN,DMCB,0       LAST CHANGED                                 
                                                                                
         GOTO1 FLDVAL,DMCB,(X'20',LINSSNH),LINCMNTH  SET ALL FLDS VER.          
                                                                                
         MVC   KEY,SVKEY           RESTORE CAST KEY                             
                                                                                
         TM    LINSSNH+1,X'20'     IF S/S NUMBER PROTECTED (REC CHANGE)         
         JZ    LVRX                                                             
         MVC   AIO,AIO2            SET TO GET CAST RECORD SINCE WE READ         
         GOTO1 GETREC              DEAL REC AND POSSIBLY W4 REC FOR CRP         
         MVC   AIO,AIO1                                                         
         SPACE 1                                                                
LVRX     J     XIT                                                              
         EJECT                                                                  
*              ROUTINE VALIDATES AN AMOUNT                                      
         SPACE 1                                                                
*                                  R2=A(FIELD HEADER)                           
AMTVAL   NTR1                                                                   
         ZIC   RF,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,8(R2),(RF)  VALIDATE FOR 2 DEC. PLACES              
         CLI   0(R1),X'FF'                                                      
         JE    AMTINV                                                           
         TM    4(R1),X'80'         DON'T ALLOW NEGATIVE                         
         JO    AMTINV                                                           
         MVC   FULL,4(R1)          RETURN AMOUNT IN FULL                        
         J     XIT                                                              
         EJECT                                                                  
*              ROUTINE INSURES ALL DEALS ACCOUNTED FOR                          
         SPACE 1                                                                
ALLDEAL  NTR1                                                                   
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLDLCDQ,(X'20',0)  GET DEAL RECORD                   
         MVC   AIO,AIO1                                                         
         SPACE 1                                                                
         L     R4,AIO2             SET TO LOOP THROUGH DEAL RECORD              
         MVI   ELCODE,TADLELQ                                                   
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
ALLD10   BRAS  RE,NEXTEL                                                        
         JNE   XIT                                                              
         USING TADLD,R4                                                         
         GOTO1 GETL,DMCB,(6,TADLAREA)  IF THIS AREA/USE NOT ON CAST REC         
         JE    ALLD20                                                           
         MVC   ELEMENT(TADLLNQ),TADLEL ADD IT NOW                               
         GOTO1 ADDELEM                                                          
ALLD20   J     ALLD10                                                           
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO VALIDATE CORP FIELD                                   
                                                                                
         USING LINED,R3            R3=A(SCREEN LINE)                            
         USING TACAD,R4            R4=A(CAST ELEMENT)                           
VALCORP  NTR1  BASE=*,LABEL=*                                                   
         LA    R2,LINCRPH          R2=A(CORPORATION FIELD)                      
         CLI   5(R2),0                                                          
         JE    XIT                                                              
         MVC   TACACORP,8(R2)      SAVE IN ELEMENT                              
                                                                                
         CLI   TACACORP,C'Y'       IF FIELD HAD 'Y'                             
         JNE   *+8                                                              
         MVI   TACACORP,C'1'       THEN SET TO '1' IN ELEMENT                   
                                                                                
         MVC   AIO,AIO2            SEARCH W4 RECORD FOR TAX ID EL.              
         CLI   LINSSNH+5,6                                                      
         JH    VCRP10                                                           
         MVC   TGPID,LINSSN                                                     
         GOTO1 SSNUNPK,DMCB,TGPID,TGSSN                                         
         JNE   VCRP10                                                           
         MVC   LINSSN,TGSSN                                                     
         MVI   LINSSNH+5,9                                                      
VCRP10   GOTO1 RECVAL,DMCB,TLW4CDQ,(X'A0',LINSSN)   GET W4 REC                  
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   LINSSN,SPACES                                                    
         MVC   LINSSN(L'TGPID),TGPID                                            
         MVI   LINSSNH+5,6                                                      
         OI    LINSSNH+6,X'80'                                                  
                                                                                
VCRP20   MVI   ELCODE,TATIELQ                                                   
         MVI   HALF,TATITYCO       BUILD GETL SEARCH ARGUMENT                   
         MVC   HALF+1(1),TACACORP                                               
         GOTO1 GETL,DMCB,(2,HALF)                                               
         JNE   VCEINV              ERROR IF NOT FOUND                           
                                                                                
         MVC   AIO,AIO1            RESTORE AIO                                  
                                                                                
         CLI   8(R2),C'Y'          IF 'Y' WAS INPUT                             
         JNE   VCRPX                                                            
         MVI   ELCODE,TATIELQ                                                   
         L     R4,TGELEM                                                        
         BRAS  RE,NEXTEL           SEARCH FOR ADDL CORPS ON W4 REC.             
         JE    VCECRP              NEED PRECISE CORP CODE                       
                                                                                
VCRPX    CLI   5(R2),0                                                          
         JE    XIT                                                              
         L     RF,TGELEM                                                        
         GOTOR W4LCKCRP,DMCB,TATIID-TATID(RF),AIO3,AIO1                         
         JE    XIT                                                              
         NI    4(R2),X'DF'         UNVALIDATE                                   
         J     ERCRPLK             EXIT WITH ERROR MESSAGE                      
                                                                                
VCEINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         J     VCEND                                                            
                                                                                
VCECRP   MVI   ERROR,ERGT1CRP      GT 1 CORP - NEED EXACT CODE                  
         J     VCEND                                                            
                                                                                
ERCRPLK  MVC   MYMSGNO,=Y(ERCPSLCK)    CORP RECORD LOCKED - NON-PAY             
         OI    GENSTAT2,USGETTXT   NEW THEEND FOR TWO BYTE ERROR MSGS           
         MVI   BLOCK,0                                                          
         MVI   MYMTYP,GTMERR                                                    
         B     VCEND                                                            
                                                                                
VCEND    GOTO1 EXIT,DMCB,0                                                      
                                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAW4LCRP                                                       
*              DSECT TO COVER SCREEN LINE                                       
         SPACE 1                                                                
LINED    DSECT                                                                  
LINSSNH  DS    CL8                 S/S NUMBER                                   
LINSSN   DS    CL9                                                              
         DS    CL8                                                              
LINCATH  DS    CL8                 CATEGORY                                     
LINCAT   DS    CL3                                                              
         DS    CL8                                                              
LINTAXH  DS    CL8                 TAX UNIT                                     
LINTAX   DS    CL3                                                              
         DS    CL8                                                              
LINAGTH  DS    CL8                 AGENT                                        
LINAGT   DS    CL4                                                              
         DS    CL8                                                              
LINSDTEH DS    CL8                 SHOOT DATE                                   
LINSDTE  DS    CL8                                                              
         DS    CL8                                                              
LINCRPH  DS    CL8                 CORP NUMBER                                  
LINCRP   DS    CL1                                                              
         DS    CL8                                                              
LINPAYEH DS    CL8                 PAYEE CODE                                   
LINPAYE  DS    CL1                                                              
         DS    CL8                                                              
LINRATEH DS    CL8                 AGENCY COMMISSION RATE                       
LINRATE  DS    CL5                                                              
         DS    CL8                                                              
LINEBASH DS    CL8                 EXPIRATION CALC. BASIS                       
LINEBAS  DS    CL1                                                              
         DS    CL8                                                              
         DS    CL8                                                              
LINLITS  DS    CL60                                                             
         ORG   LINLITS                                                          
LINSSNN  DS    CL16                W4 NAME                                      
         ORG   LINLITS+22                                                       
LINAGTN  DS    CL16                AGENT NAME                                   
         ORG   LINLITS+42                                                       
LINCRPN  DS    CL16                CORP. NAME                                   
         ORG                                                                    
         DS    CL8                                                              
LINCMNTT DS    CL4                                                              
LINCMNTH DS    CL8                 COMMENT                                      
LINCMNT  DS    CL60                                                             
         DS    CL8                                                              
         DS    CL8                                                              
LINLCHGT DS    CL13                                                             
LINLCHGH DS    CL8                 LAST CHANGED INFO                            
LINLCHG  DS    CL17                                                             
LINLNQ   EQU   *-LINED                                                          
*                                                                               
LINSELH  DS    CL8                 A(SELECT FIELD ON NEXT LINE)                 
LINSEL   DS    CL3                                                              
LINSELX  DS    CL8                                                              
LINNEXT  EQU   *                   A(NEXT DATA LINE)                            
         EJECT                                                                  
*              DSECT TO COVER PRINT LINE FOR REPORTS                            
         SPACE 1                                                                
PRNTD    DSECT                                                                  
PRNTAREA DS    CL3                 AREA                                         
         DS    CL1                                                              
PRNTAREN DS    CL16                AREA NAME                                    
         DS    CL1                                                              
PRNTUSE  DS    CL3                 USE                                          
         DS    CL1                                                              
PRNTUSEN DS    CL16                USE NAME                                     
         DS    CL1                                                              
PRNTTERM DS    CL2                 TERM (MONTHS)                                
         DS    CL1                                                              
PRNTAMT  DS    CL10                AMOUNT                                       
         DS    CL1                                                              
PRNTRATE DS    CL5                 AGY COMMISSION RATE                          
         DS    CL1                                                              
PRNTPUB  DS    CL8                 PUBLICATION DATE                             
         DS    CL1                                                              
PRNTEXP  DS    CL8                 EXPIRATION DATE                              
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRBCD                                                       
         EJECT                                                                  
*              LOCAL SAVED STORAGE AT END OF TWA0                               
         SPACE 1                                                                
AGYNAME  DS    CL16                AGENCY NAME FOR PRINTING                     
COUNTER  DS    PL4                 LINE COUNTER                                 
NEXTSEQ  DS    H                   NEXT AVAILABLE SEQUENCE NUMBER               
SVKEY    DS    CL(L'TLDRREC)       SAVED KEY                                    
COMLSDTE DS    XL3                 COMMERCIAL SHOOT DATE                        
COMLPHOT DS    CL9                 PHOTOGRAPHER FROM COMML RECORD               
PTRBLK   DS    CL(5*L'TLDRREC+1)   BLOCK FOR POINTER MAINTENANCE                
SVAYSTA6 DS    X                                                                
SVCISTA2 DS    X                                                                
         SPACE 2                                                                
       ++INCLUDE TASYSIOD                                                       
         EJECT                                                                  
* DDGENTWA  (MUST FOLLOW LAST SCREEN)                                           
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSDSECT                                                                    
* TASYSEQUS                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* DDPERVALD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'039TAGENBC   04/28/15'                                      
         END                                                                    
