*          DATA SET ACINT12    AT LEVEL 030 AS OF 04/14/05                      
*PHASE T61912A,*                                                                
*INCLUDE KHDUMMY                                                                
         TITLE 'T61912 - CHECK HEADER'                                          
T61912   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T61912**,R7,RR=R2                                              
         L     RC,0(,R1)                                                        
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          SYSTEM SPECIFIC WORK                         
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
*                                                                               
         CLI   MODE,VALKEY                                                      
         BE    HED2                                                             
         CLI   MODE,VALREC                                                      
         BE    HED10                                                            
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* VALKEY LOGIC                                                       *          
**********************************************************************          
         SPACE 1                                                                
HED2     LA    RE,LOCAL            CLEAR LOCAL STORAGE                          
         LA    RF,LOCALLEN                                                      
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         ST    R2,RELO                                                          
*                                                                               
         ST    RB,ABASE1           SAVE MAIN CSECTS' BASE REGISTERS             
         ST    R7,ABASE2                                                        
         ST    RC,AGEND            AND RC FOR COMMON SUBS                       
         LA    RE,VALPRD                                                        
         ST    RE,AVALPRD                                                       
         LA    RE,VALMEDF                                                       
         ST    RE,AVALMED                                                       
         LA    RE,VALESTF                                                       
         ST    RE,AVALEST                                                       
         LA    RE,FILTER                                                        
         ST    RE,AFILTER                                                       
         L     RE,AIO6             CLIENT TABLE SHARES IO6                      
         LA    RE,100(RE)          RESOLVES TO 100 BYTES INTO IO6               
         ST    RE,ACLITAB          CLIENT CODE TABLE                            
*                                                                               
HED3     CLI   ACTNUM,ACTNHED                                                   
         BE    HED6                                                             
         BAS   RE,GETHED           RETRIEVE HEADER SCREEN/SAVE DATA             
         CLI   ACTNUM,ACTNLIST     TEST FOR ACTION=LIST                         
         BE    HED4                                                             
         CLI   ACTNUM,ACTNEST      TEST FOR ACTION ESTIMATE                     
         BNE   HEDX                ALL DONE FOR OTHER ACTIONS                   
*                                                                               
HED4     MVI   FILTSW,C'N'                                                      
         BAS   RE,INIT             INITIALIZE TSAR                              
         LA    R1,LSTMOD           SET TO LOAD LIST MODULE                      
         CLI   ACTNUM,ACTNLIST                                                  
         BE    *+8                                                              
         LA    R1,ESTMOD           ITS ACTION=ESTIMATE                          
         STC   R1,OVERLAY          SET FOR CORRECT CALL/RETURN                  
         GOTO1 LOADOV,(R1)                                                      
         GOTO1 (RF),DMCB,(RC)                                                   
         CLI   FILTSW,C'N'         TEST IF ANY FILTERS CHANGED                  
         BE    HEDX                NO                                           
*                                                                               
HED5     MVC   LISTAR,SPACES       MERGE THE NEW FILTERS INTO HEADER            
         L     R3,ATIA             R3=A(SAVED HEADER SCREEN)                    
         LA    R2,HEDPROH-CONRECH(R3) R2=A(PRODUCT FILTER FIELD)                
         MVC   LISTAR(L'PRODCHAR),PRODCHAR                                      
         BAS   RE,MOVEFLD                                                       
         LA    R2,HEDMEDH-CONRECH(R3)                                           
         MVC   LISTAR(L'MEDCHAR),MEDCHAR                                        
         BAS   RE,MOVEFLD                                                       
         LA    R2,HEDESTH-CONRECH(R3)                                           
         MVC   LISTAR(L'ESTCHAR),ESTCHAR                                        
         BAS   RE,MOVEFLD                                                       
         BAS   RE,SAVHED           YES-UPDATE THE SAVED HEADER                  
         B     HEDX                                                             
*                                                                               
* VALKEY LOGIC - CHECK HEADER                                                   
*                                                                               
HED6     BAS   RE,VALHED                                                        
         B     HEDX                                                             
*                                                                               
* VALREC LOGIC-CHECK HEADER                                                     
*                                                                               
HED10    CLI   ACTNUM,ACTNHED      TEST FOR ACTION=HEADER                       
         BE    HED20               YES                                          
         CLI   ACTNUM,ACTNREV      TEST ACTION=REVIEW                           
         BE    HED30                                                            
         CLI   ACTNUM,ACTNQUIT     TEST ACTION=QUIT                             
         BE    HED40                                                            
         CLI   ACTNUM,ACTNDFT      TEST ACTION=DRAFT                            
         BE    HED60                                                            
         CLI   ACTNUM,ACTNFILT     TEST ACTION=FILTER                           
         BE    HED60                                                            
         CLI   ACTNUM,ACTNUPD      TEST ACTION=UPDATE                           
         BE    HED50                                                            
         LA    R1,LSTMOD           SET MODULE TO LOAD                           
         CLI   ACTNUM,ACTNLIST                                                  
         BE    *+8                                                              
         LA    R1,ESTMOD           ITS ACTION=ESTIMATE                          
         STC   R1,OVERLAY                                                       
         GOTO1 LOADOV,(R1)                                                      
         GOTO1 (RF),DMCB,(RC)                                                   
         B     HEDX                                                             
*                                                                               
HED20    NI    CHKMODE,X'FF'-TSARINI FORCE RE-INITIALIZE OF FILE                
         BAS   RE,INIT             INITIALIZE TSAR FILE                         
*                                                                               
HED21    ZAP   TOTPAID,=P'0'       CLEAR TOTAL PAID BUCKET                      
         BAS   RE,FILE             BUILD THE TSAR FILE                          
         BE    HED22               SOMETHING IN FILE                            
         LA    R2,HEDRCVH                                                       
         MVI   ERROR,X'FE'                                                      
         MVC   CONHEAD(L'NONEMSG),NONEMSG                                       
         B     ERREND                                                           
*                                                                               
HED22    BAS   RE,OPEN             OPEN BATCH HEADER                            
         LA    RE,SAVEVALS                                                      
         LA    RF,SAVELNQ                                                       
         LA    R0,SAVEST           PUT HEADER VALUES BACK INTO SCREEN           
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                                                               
         MVI   CALLSP,0            FORCE NEW HEADER TO START OF                 
         XC    CALLSTK,CALLSTK     SAVE CHAIN                                   
         BAS   RE,SAVTSAR          SAVE THE TSAR BLOCK                          
*                                                                               
         GOTO1 VCALL,WORK,=C'CHECK',=C'LIST',=C',',0                            
*                                                                               
* VALREC LOGIC - CHECK REVIEW                                                   
*                                                                               
HED30    CLI   CALLER,0            TEST JUST CALLED                             
         BE    HED35               NO                                           
*                                                                               
         BAS   RE,REVHED           DISPLAY THE REVIEW SCREEN                    
         MVC   CONHEAD(L'REVMSG),REVMSG                                         
         B     HED38                                                            
*                                                                               
HED35    BAS   RE,VALHED                                                        
         L     RE,ATIA             MOVE BACK SCREEN VALUES TO TIA               
         LA    RE,HEDREPH-CONRECH(RE) RE=DESTINATION                            
         LA    RF,HEDTOT1H-HEDREPH FROM REPORT TO FIRST TOTAL LINE              
         LA    R0,HEDREPH          R0=SOURCE                                    
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                                                               
         BAS   RE,SAVHED                                                        
         MVC   CONHEAD(L'REVCHMSG),REVCHMSG                                     
         LA    R2,HEDREPH                                                       
         SR    R0,R0                                                            
         LA    R1,HEDLAST-1        R1=BXLE LIMIT                                
*                                                                               
HED36    TM    1(R2),X'20'         TEST FOR UNPROTECTED FIELD                   
         BO    *+12                                                             
         TM    4(R2),X'80'         TEST INPUT THIS TIME                         
         BO    HED38               YES-SOME CHANGE WAS MADE                     
         IC    R0,0(R2)                                                         
         BXLE  R2,R0,HED36                                                      
*                                                                               
HED37    MVC   CONHEAD,SPACES                                                   
         MVC   CONHEAD(L'NOCHGMSG),NOCHGMSG                                     
*                                                                               
HED38    LA    R2,HEDREPH                                                       
         ST    R2,ACURFORC                                                      
         B     HEDX                                                             
*                                                                               
* VALREC LOGIC - CHECK QUIT                                                     
*                                                                               
HED40    CLI   CALLER,0            TEST IF JUST CALLED                          
         BE    HED45               NO                                           
*                                                                               
         BAS   RE,QUITHED          PREPARE QUIT SCREEN                          
         MVC   CONHEAD(L'QUITDMSG),QUITDMSG                                     
         LA    R2,QUIINPH                                                       
         ST    R2,ACURFORC                                                      
         B     HEDX                                                             
*                                                                               
HED45    BAS   RE,INIT                                                          
         BAS   RE,QUIT                                                          
         CLI   ACTNUM,ACTNQUIT     TEST ACTION=QUIT                             
         BE    HED47               YES                                          
*                                                                               
         MVC   CONACT,=CL8'QUIT'   RESTORE ACTION AFTER REPORT                  
         MVI   ACTNUM,ACTNQUIT                                                  
         LA    R2,QUIINPH                                                       
         OI    6(R2),X'80'                                                      
         ST    R2,ACURFORC                                                      
         B     HEDX                                                             
*                                                                               
HED47    LA    R2,HEDREPH          ONLY HERE IF THEY ARE QUITING                
         ST    R2,ACURFORC                                                      
         MVC   CONHEAD(L'QUITMSG),QUITMSG                                       
         B     HEDX                                                             
*                                                                               
* VALREC LOGIC - CHECK UPDATE                                                   
*                                                                               
HED50    CLI   CALLER,0            TEST IF JUST CALLED                          
         BE    HED55               NO                                           
*                                                                               
         BAS   RE,UPHED            PREPARE UPDATE SCREEN                        
         MVC   CONHEAD(L'UPCMSG),UPCMSG                                         
         LA    R2,UPDINPH                                                       
         ST    R2,ACURFORC                                                      
         B     HEDX                                                             
*                                                                               
HED55    BAS   RE,INIT             INITIALIZE TSAR FILE                         
         BAS   RE,UP                                                            
         CLI   ACTNUM,ACTNUPD      TEST ACTION=UPDATE                           
         BE    HED57               YES                                          
*                                                                               
         MVC   CONACT,=CL8'UPDATE' RESTORE ACTION AFTER REPORT                  
         MVI   ACTNUM,ACTNUPD                                                   
         LA    R2,UPDINPH                                                       
         OI    6(R2),X'80'                                                      
         ST    R2,ACURFORC                                                      
         B     HEDX                                                             
*                                                                               
HED57    LA    R2,HEDREPH          ONLY HERE IF THEY DID UPDATE                 
         ST    R2,ACURFORC                                                      
         B     HEDX                                                             
*                                                                               
* VALREC LOGIC - CHECK DRAFT OR CHECK FILTER                                    
*                                                                               
HED60    BAS   RE,INIT                                                          
         LA    R0,HEDREPH          MOVE BACK THE HEADER SCREEN INPUT            
         L     RE,ATIA                                                          
         LA    RE,HEDREPH-CONRECH(RE)                                           
         LA    R1,HEDLAST-HEDREPH                                               
         MVCL  R0,RE                                                            
         GOTO1 LOADOV,UPDMOD       LOAD IN THE UPDATE MODULE                    
         GOTO1 (RF),DMCB,(RC)                                                   
         BAS   RE,SAVTSAR                                                       
         MVI   PFKEY,0             RETURN TO CALLER                             
         GOTO1 VRETURN                                                          
*                                                                               
HEDX     XMOD1 1                                                                
         EJECT                                                                  
**********************************************************************          
* SUB-ROUTINE TO VALIDATE THE HEADLINE FIELDS                        *          
**********************************************************************          
         SPACE 1                                                                
VALHED   NTR1                                                                   
         MVI   OPTION,0            NO NAMES TO DISPLAY                          
*                                                                               
* EDIT THE REPORT FIELD                                                         
*                                                                               
VALREP   LA    R2,HEDREPH                                                       
         TM    1(R2),X'20'         TEST PROTECTED                               
         BO    VALREPX                                                          
         GOTO1 ANY                                                              
         MVC   PERSON,WORK                                                      
VALREPX  DS    0H                                                               
*                                                                               
* EDIT BATCH NAME FIELD                                                         
*                                                                               
VALNAME  LA    R2,HEDNAMEH                                                      
         TM    1(R2),X'20'                                                      
         BO    VALNAMEX                                                         
         GOTO1 ANY                                                              
         MVC   BATNAME,WORK                                                     
VALNAMEX DS    0H                                                               
*                                                                               
* EDIT BATCH REFERENCE FIELD                                                    
*                                                                               
VALBAT   LA    R2,HEDREFH                                                       
         TM    1(R2),X'20'                                                      
         BO    VALBATX                                                          
         GOTO1 ANY                                                              
         MVI   ERROR,INVALID                                                    
         ZIC   R0,5(R2)                                                         
         LA    R1,WORK                                                          
*                                                                               
VALBAT2  CLI   0(R1),C' '                                                       
         BE    *+12                                                             
         CLI   0(R1),C'A'          TEST CAPITAL LETTERS/NUMBERS                 
         BL    ERREND                                                           
         LA    R1,1(R1)                                                         
         BCT   R0,VALBAT2                                                       
         MVC   BATREF,WORK                                                      
VALBATX  DS    0H                                                               
*                                                                               
* EDIT BATCH HEADER MONTH                                                       
*                                                                               
VALMON   LA    R2,HEDMONH                                                       
         TM    1(R2),X'20'                                                      
         BO    VALMONX                                                          
         XC    BATMON,BATMON                                                    
*&&DO                                                                           
         SR    RE,RE                                                            
         ICM   RE,1,BTODAY                                                      
         MH    RE,=H'12'                                                        
         SR    RF,RF                                                            
         IC    RF,BTODAY+1                                                      
         AR    RF,RE               RF=(CURRENT YEAR*12)+MONTH                   
         LA    RE,1(RF)            RE=RELATIVE NEXT MONTH                       
         STH   RE,DUB+2            DUB+2(2)=1 RELATIVE MONTH FORWARD            
         BCTR  RF,0                                                             
         BCTR  RF,0                                                             
         STH   RF,DUB+0            DUB+0(2)=2 RELATIVE MONTHS BACK              
         SPACE 2                                                                
*&&                                                                             
         CLI   5(R2),0             TEST ANY INPUT                               
         BNE   VALMON2                                                          
         GOTO1 DATCON,DMCB,(1,TODAYP),(18,8(R2))                                
         MVI   5(R2),6             SET INPUT LENGTH                             
         OI    6(R2),X'80'         XMIT                                         
*                                                                               
         USING BMONVALD,R1                                                      
VALMON2  GOTO1 ANY                                                              
         XC    BMONWRK,BMONWRK                                                  
         GOTO1 VBMONVAL,DMCB,HEDMONH,(30,ACOMFACS),(0,BMONWRK),        X        
               (COMPANY,0)                                                      
         LA    R1,BMONWRK                                                       
         MVI   ERROR,INVDATE                                                    
         TM    BMOERR,BMOEINVQ+BMOERNGQ   OUT OF RANGE OR INVALID DATE          
         BNZ   ERREND                     YES                                   
         MVI   ERROR,MOALOCK                                                    
         TM    BMOERR,BMOELOKQ            LOCKED MOA ?                          
         BO    ERREND                     YES                                   
*        GOTO1 DATVAL,DMCB,(0,EPOSTD),WORK                                      
*        GOTO1 DATCON,DMCB,(0,WORK),(1,MOSP)                                    
*&&DO                                                                           
         ZIC   R0,5(R2)            GET I/P LEN                                  
         GOTO1 PERVAL,DMCB,((R0),WORK),ELEMENT                                  
         TM    4(R1),X'01'         START INVALID?                               
         BO    ERREND                                                           
         LA    R1,ELEMENT                                                       
         USING PERVALD,R1                                                       
         CLC   PVALNMNS,=H'1'      ALLOW ONE MONTH ONLY                         
         BNE   ERREND                                                           
         MVC   FULL(L'TODAYP),PVALPSTA                                          
         TM    COMPSTA3,X'10'      TEST ANY MONTH ALLOWED                       
         BNZ   VALMON4                                                          
         SR    RE,RE                                                            
         ICM   RE,1,PVALBSTA                                                    
         MH    RE,=H'12'                                                        
         SR    RF,RF                                                            
         IC    RF,PVALBSTA+1                                                    
         AR    RF,RE               RF=INPUT MOS RELATIVE MONTH                  
         CH    RF,DUB+0            TEST MORE THAN 2 MONTHS BACK                 
         BL    ERREND                                                           
         CH    RF,DUB+2            TEST MORE THAN 1 MONTH FORWARD               
         BH    ERREND                                                           
         MVI   ERROR,MOALOCK                                                    
         CLC   CKMOSL,PVALPSTA     TEST LOCKED MONTH                            
         BNL   ERREND                                                           
*                                                                               
VALMON4  MVC   BATMONP,FULL        SET (PWOS) BATCH MONTH                       
         MVC   BATMON+0(1),FULL+0  YEAR 'DIGIT' C'0'-C'9'                       
         OI    BATMON,X'F0'                                                     
         MVC   BATMON+1(1),FULL+1  MONTH PACKED                                 
         TR    BATMON+1(1),MONTAB                                               
*&&                                                                             
VALMON4  MVC   BATMONP,BMOMOSP                                                  
         MVC   BATMON,BMOMOSC                                                   
         MVC   FULL(2),BATMONP     Move in packed YYMM                          
         MVI   FULL+2,X'01'        Move in packed DD                            
         XC    HEDMON,HEDMON                                                    
         OI    6(R2),X'80'         XMIT IT BACK                                 
         GOTO1 DATCON,DMCB,(1,FULL),(9,8(R2))                                   
*                                                                               
VALMONX  DS    0H                                                               
         DROP  R1                                                               
*                                                                               
* EDIT BANK ACCOUNT                                                             
*                                                                               
VALBANK  LA    R2,HEDBANKH                                                      
         GOTO1 ANY                                                              
         MVC   UNIT(2),BANKLEDG                                                 
         MVC   KEY,SPACES                                                       
         MVC   KEY(L'CUL),CUL                                                   
         MVC   KEY+L'CUL(L'ACKEYACC-L'CUL),WORK ATTACH REST OF KEY              
         GOTO1 GETLOW                                                           
         BNE   ERREND                                                           
         MVC   BANK,KEY                                                         
         MVC   BANKNAME,RECNAME                                                 
         MVC   HEDBNKN,BANKNAME                                                 
         OI    HEDBNKNH+6,X'80'                                                 
*                                                                               
* EDIT CHECK NUMBER                                                             
*                                                                               
VALCHK   LA    R2,HEDCKNH                                                       
         GOTO1 ANY                                                              
         MVC   CHKNUM,WORK                                                      
*                                                                               
* EDIT DEPOSIT DATE                                                             
*                                                                               
VALDEP   XC    BANKDATE,BANKDATE                                                
         LA    R2,HEDDEPH                                                       
         GOTO1 ANY                                                              
         ZIC   R0,5(R2)                                                         
         MVC   LISTAR(1),AGYLANG   SET LANGUAGE                                 
         OI    LISTAR,X'60'        SINGLE DATE ONLY, RETURN AS SINGLE           
         GOTO1 PERVAL,DMCB,((R0),WORK),(LISTAR,LISTAR)                          
         MVI   ERROR,INVDATE                                                    
         TM    4(R1),X'03'         CHECK VALIDITY                               
         BNZ   ERREND                                                           
         LA    R3,LISTAR                                                        
         USING PERVALD,R3                                                       
         TM    PVALASSM,PVALASD+PVALASM+PVALASY  TEST ALL ASSUMED               
         BO    *+12                                                             
         TM    PVALASSM,PVALASD+PVALASM          TEST DAY/MON ASSUMED           
         BO    ERREND              ERROR IF ONLY YEAR INPUT                     
         CLC   PVALPSTA,TODAYP     CAN'T BE HIGHER THAN TODAY                   
         BH    ERREND                                                           
         TM    COMPSTA4,X'40'      TEST ALLOW OVER 12 MONTHS BACK DATE          
         BNZ   VALDEP2                                                          
         SR    RE,RE                                                            
         ICM   RE,1,BTODAY                                                      
         MH    RE,=H'12'                                                        
         SR    RF,RF                                                            
         IC    RF,BTODAY+1                                                      
         AR    RE,RF               RE=(CURRENT YEAR*12)+MONTH                   
         SH    RE,=H'12'                                                        
         STH   RE,DUB                                                           
         SR    RE,RE                                                            
         ICM   RE,1,PVALBSTA                                                    
         MH    RE,=H'12'                                                        
         IC    RF,PVALBSTA+1                                                    
         AR    RE,RF               RE=CURRENT RELATIVE MONTH                    
         CH    RE,DUB              TEST MORE THAN NN MONTHS AGO                 
         BL    ERREND                                                           
*                                                                               
VALDEP2  MVC   BANKDATE,PVALESTA   EBCDIC START DATE                            
         MVC   HEDDEP,SPACES                                                    
         OI    HEDDEPH+6,X'80'                                                  
         GOTO1 DATCON,DMCB,(1,PVALPSTA),(17,8(R2))                              
*                                                                               
VALDEPX  DS    0H                                                               
         DROP  R3                                                               
*                                                                               
* EDIT CHECK AMOUNT (CONTROL AMOUNT)                                            
*                                                                               
VALAMT   LA    R2,HEDAMTH                                                       
         GOTO1 ANY                                                              
         MVI   ERROR,INVALID                                                    
         ZIC   R0,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,(X'82',8(R2)),(R0)                                  
         CLI   0(R1),0             TEST FOR ERROR                               
         BNE   ERREND                                                           
         ZAP   CHKAMT,4(8,R1)                                                   
         CURED CHKAMT,(L'HEDAMT,HEDAMT),2,ALIGN=LEFT,FLOAT=-                    
         OI    6(R2),X'80'         XMIT                                         
         EJECT                                                                  
*                                                                               
* EDIT CHECK DATE                                                               
*                                                                               
         SPACE 1                                                                
VALCDT   XC    CHKDATE,CHKDATE                                                  
         LA    R2,HEDCKDH                                                       
         GOTO1 ANY                                                              
         MVC   LISTAR(1),AGYLANG   SET LANGUAGE                                 
         OI    LISTAR,X'60'        SINGLE DATE ONLY, RETURN AS SINGLE           
         ZIC   R0,5(R2)                                                         
         GOTO1 PERVAL,DMCB,((R0),WORK),(LISTAR,LISTAR)                          
         MVI   ERROR,INVDATE                                                    
         TM    4(R1),X'03'         CHECK VALIDITY                               
         BNZ   ERREND                                                           
         LA    R3,LISTAR                                                        
         USING PERVALD,R3                                                       
         TM    PVALASSM,PVALASD+PVALASM+PVALASY                                 
         BO    *+12                ALL ASSUMED                                  
         TM    PVALASSM,PVALASD+PVALASM TEST DAY/MON ASSUMED                    
         BO    ERREND              ERROR IF ONLY YEAR INPUT                     
         MVC   CHKDATE,PVALESTA    EBCDIC START DATE                            
         MVC   HEDCKD,SPACES                                                    
         OI    6(R2),X'80'                                                      
         GOTO1 DATCON,DMCB,(1,PVALPSTA),(17,8(R2))                              
         DROP  R3                                                               
         SPACE 2                                                                
*                                                                               
* EDIT RECEIVABLE ACCOUNT                                                       
*                                                                               
VALRCV   LA    R2,HEDRCVH                                                       
         TM    1(R2),X'20'         TEST PROTECTED                               
         BO    VALRCVX             YES                                          
*                                                                               
         GOTO1 ANY                                                              
         MVC   UNIT(2),RECVLEDG                                                 
         MVC   KEY,SPACES                                                       
         MVC   KEY(L'CUL),CUL                                                   
         MVC   KEY+L'CUL(L'ACKEYACC-L'CUL),WORK ATTACH REST OF KEY              
         GOTO1 GETLOW                                                           
         BNE   ERREND                                                           
*                                                                               
         MVC   RECEIVE,KEY                                                      
         MVC   RECENAME,RECNAME                                                 
         MVC   HEDRCVN,RECENAME                                                 
         OI    HEDRCVNH+6,X'80'    XMIT BACK NAME                               
*                                                                               
VALRCVX  DS    0H                                                               
*                                                                               
* EDIT CLIENT                                                                   
*                                                                               
VALCLT   LA    R2,HEDCLIH                                                       
         TM    1(R2),X'20'         TEST PROTECTED                               
         BO    VALCLTX                                                          
*                                                                               
         BAS   RE,BLDCLT           BUILD CLIENT TABLE FROM SCREEN               
         MVI   OPTION,C'Y'         DISPLAY THE NAME                             
         MVC   UNIT(2),PRODLEDG    PRODUCTION LEDGER                            
*                                                                               
         MVC   SVCLIFLD,HEDCLI     SAVE ORIGINAL CLIENT CODES                   
         SR    R1,R1                                                            
         IC    R1,5(R2)            LENGTH OF CLIENT FIELD                       
         STC   R1,SVCLILEN         SAVE ORIGINAL LENGTH                         
*                                                                               
         MVI   ERROR,INVALID       SET ERROR TO INVALID                         
         SR    R0,R0                                                            
         ICM   R0,1,CLINUM         NUMBER OF ENTRIES IN TABLE                   
         BZ    ERREND                                                           
         USING CLITABD,R3                                                       
         L     R3,ACLITAB                                                       
         MVC   HEDCLI,SPACES       SPACE OUT ORIGINAL FIELD                     
VALCLT10 SR    R1,R1               R1=CLIENT CODE LENGTH                        
         IC    R1,CLILEN                                                        
         STC   R1,5(R2)            MAKE FIELD LENGTH CLICDE LEN                 
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   HEDCLI(0),CLICDE    PUT CLIENT FROM TABLE TO SCR FIELD           
         GOTO1 VALCLI                                                           
         MVC   CLINM(L'HEDCLIN),HEDCLIN    PUT CLIENT NAME INTO TABLE           
         MVC   HEDCLIN,SPACES                                                   
         LA    R1,HEDCLINH                                                      
         OI    6(R1),X'80'         CLEAR FIELD AND TRANSMIT                     
         LA    R3,CLILNQ(R3)                                                    
         BCT   R0,VALCLT10                                                      
*                                                                               
         MVC   HEDCLI,SVCLIFLD     RESTORE ORIGINAL CODES                       
         SR    R1,R1                                                            
         IC    R1,SVCLILEN                                                      
         STC   R1,5(R2)            RESTORE ORIGINAL LENGTH                      
*                                                                               
VALCLTX  DS    0H                                                               
         DROP  R3                                                               
*                                                                               
* EDIT ADVERTISING PERIOD (MONTH OR MONTH-MONTH)                                
*                                                                               
VALADV   LA    R2,HEDPERH                                                       
         TM    1(R2),X'20'                                                      
         BO    VALADVX                                                          
*                                                                               
VALADV2  GOTO1 ANY                                                              
         MVI   ERROR,INVDATE                                                    
         GOTO1 DATVAL,DMCB,(2,8(R2)),WORK                                       
         ICM   R3,15,0(R1)         GET LENGTH OF EXPRESSION                     
         BZ    ERREND                                                           
         GOTO1 DATCON,DMCB,WORK,(1,PERIODS)                                     
         MVC   WORK+6(6),WORK                                                   
         MVC   PERIODE,PERIODS                                                  
         CLM   R3,1,5(R2)          TEST FIRST DATE LENGTH VS. INPUT LEN         
         BE    VALADV4                                                          
         LA    R3,8(R3,R2)                                                      
         CLI   0(R3),C'-'          TEST FOR A HYPHEN                            
         BNE   ERREND                                                           
         LA    R3,1(R3)                                                         
         GOTO1 DATVAL,DMCB,(2,(R3)),WORK+6                                      
         OC    0(4,R1),0(R1)                                                    
         BZ    ERREND                                                           
         GOTO1 DATCON,DMCB,WORK+6,(1,PERIODE)                                   
*                                                                               
VALADV4  CLC   PERIODS,PERIODE                                                  
         BNH   *+12                                                             
         MVI   ERROR,BADDATES                                                   
         B     ERREND                                                           
         CLC   PERIODE(2),NEXTP    TEST ENDS WITHIN A YEAR                      
         BH    ERREND                                                           
         GOTO1 PERVERT,DMCB,WORK,WORK+6                                         
         ZIC   R3,15(R1)                                                        
         STH   R3,PERINUM                                                       
         MVI   ERROR,ONEYEAR                                                    
         CH    R3,=H'12'           PERIOD NO MORE THAN 12 MONTHS                
         BH    ERREND                                                           
         MVC   ADVST,PERIODS                                                    
         MVC   ADVEND,PERIODE                                                   
         MVC   ADVNUM,PERINUM+1                                                 
         BAS   RE,BLDADV           BUILD ADV PERIOD TABLE                       
*                                                                               
VALADVX  DS    0H                                                               
*                                                                               
* EDIT OPTIONAL FILTER FIELDS                                                   
*                                                                               
VALFILT  LA    R2,HEDPROH          PRODUCT                                      
         GOTO1 AVALPRD                                                          
*                                                                               
         LA    R2,HEDMEDH          MEDIA                                        
         GOTO1 AVALMED                                                          
*                                                                               
         LA    R2,HEDESTH          ESTIMATE                                     
         GOTO1 AVALEST                                                          
*                                                                               
VALHEDX  B     EXIT                                                             
         EJECT                                                                  
* SUB-ROUTINE TO GET THE HEADER SCREEN AND TO EXTRACT THE SAVED                 
* BATCH VALUES                                                                  
*                                                                               
GETHED   ST    RE,SAVERE                                                        
         XC    DMCB+8(4),DMCB                                                   
         MVI   DMCB+8,3            ITS ON TWA 3                                 
         MVC   DMCB+10(2),TWATRM                                                
         GOTO1 DATAMGR,DMCB,DMREAD,TEMPSTR,,ATIA                                
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
GETHED2  LA    R0,SAVEST                                                        
         LA    R1,SAVELNQ                                                       
         L     RE,ATIA                                                          
         LA    RE,SAVEVALS-CONRECH(RE)                                          
         LR    RF,R1                                                            
         MVCL  R0,RE               GET THE SAVED VALUES                         
*                                                                               
GETHEDX  L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO SAVE THE HEADER SCREEN AND SAVE DATA                           
*                                                                               
SAVHED   ST    RE,SAVERE                                                        
         LA    R0,SAVEST           R0=A(SOURCE)                                 
         LA    R1,SAVELNQ          R1=L'SAVE VALUES                             
         L     RE,ATIA                                                          
         LA    RE,SAVEVALS-CONRECH(RE)                                          
         LR    RF,R1                                                            
         MVCL  RE,R0               MOVE SAVE VALUES BACK INTO TWA               
*                                                                               
SAVHED2  XC    DMCB+8(4),DMCB+8                                                 
         MVI   DMCB+8,3            TWA PAGE 3                                   
         MVC   DMCB+10(2),TWATRM                                                
         GOTO1 DATAMGR,DMCB,DMWRT,TEMPSTR,,ATIA                                 
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
SAVHEDX  L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
**********************************************************************          
* SUB ROUTINE TO BUILD A TABLE OF CLIENT CODES FROM CLIENT FIELD     *          
**********************************************************************          
         SPACE 1                                                                
BLDCLT   NTR1                                                                   
         XC    CLINUM,CLINUM       NUMBER OF ENTRIES IN CLIENT TABLE            
         L     RE,ACLITAB                                                       
         LA    RF,CLITBLNQ         CLIENT TABLE LENGTH                          
         SR    R1,R1                                                            
         ICM   R1,8,SPACES                                                      
         MVCL  RE,R0               CLEAR TABLE                                  
*                                                                               
         USING CLITABD,R1                                                       
         L     R1,ACLITAB          R1=A(CLIENT TABLE)                           
         LA    R2,HEDCLIH          R2=A(CLIENT FIELD HEADER)                    
         LR    R6,R2               R6=A(CLIENT FIELD HEADER)                    
         SR    R0,R0                                                            
         ICM   R0,1,5(R6)          R0=LENGTH OF FIELD                           
         BZ    BLDCX               NOTHING IN FIELD - EXIT                      
*                                                                               
         LA    R6,8(R6)            R6=A(CLIENT CODES FIELD)                     
         SR    R3,R3               R3=COUNTER                                   
         SR    R4,R4               R4=LOOP COUNTER                              
         MVC   CLITMP,SPACES       SAVED AREA FOR TEMPORY STORAGE               
         LA    R5,CLITMP           R5=A(SAVED AREA FOR CLI CDE IN LOOP)         
*                                                                               
BLDC10   CH    R4,=H'5'            MAX NUMBER OF ENTRIES IN TABLE - 5           
         BL    *+12                NOT VALID - EXIT WITH 0 IN CLINUM            
         MVI   ERROR,CLI2BIG       TOO MANY CLIENTS ENTERED                     
         B     ERREND                                                           
*                                                                               
         CH    R3,=Y(L'CLITMP)     IF GREATER THAN CLIENT LENGTH                
         BNH   *+12                                                             
         MVI   CLINUM,0            CLEAR CLINUM AND EXIT                        
         B     BLDCX               FOR INVALID ERROR                            
*                                                                               
         CLI   0(R6),C' '          SPACE MEANS END OF DATA                      
         BE    BLDC30                                                           
         CLI   0(R6),C','          COMMAS SEPARATE CLIENT CODES                 
         BNE   BLDC20                                                           
*                                                                               
* UPDATE TABLE                                                                  
*                                                                               
         STC   R3,CLILEN           SAVE LENGTH OF CLIENT CODE IN TABLE          
         BCTR  R3,0                                                             
         EX    R3,*+4                                                           
         MVC   CLICDE(0),CLITMP    PUT CLIENT CODE INTO TABLE                   
*                                                                               
* RESET ALL POINTERS AND COUNTERS AND ACCUMULATE TOTALS                         
*                                                                               
         SR    R3,R3               CLEAR R3 FOR COMPARE-STARTING OVER           
         IC    R4,CLINUM                                                        
         LA    R4,1(R4)                                                         
         STC   R4,CLINUM           BUMP CLINUM                                  
         LA    R5,CLITMP           POINT R5 BACK TO START OF FIELD              
         MVC   CLITMP,SPACES                                                    
         LA    R6,1(R6)            BUMP PAST COMMA                              
         LA    R1,CLILNQ(R1)       BUMP TO NEXT ENTRY IN TABLE                  
         BCTR  R0,0                DEC BY 1 BUT DON'T DO A BCT LOOP             
         B     BLDC10              I DON'T WANT TO FALL THROUGH                 
*                                                                               
* MOVE IN 1 CHARACTER AT A TIME AND BUMP POINTERS                               
*                                                                               
BLDC20   MVC   0(1,R5),0(R6)       MOVE LETTER TO TABLE                         
         LA    R3,1(R3)            ADD ONE TO LENGTH COUNTER                    
         LA    R6,1(R6)            BUMP TO NEXT POSITION IN HEDCLI              
         LA    R5,1(R5)            BUMP TO NEXT POSITION IN CLIENT              
         BCT   R0,BLDC10                                                        
*                                                                               
BLDC30   STC   R3,CLILEN           SAVE LENGTH OF CLIENT CODE IN TABLE          
         BCTR  R3,0                                                             
         EX    R3,*+4                                                           
         MVC   CLICDE(0),CLITMP    PUT CLIENT CODE INTO TABLE                   
         IC    R4,CLINUM                                                        
         LA    R4,1(R4)                                                         
         STC   R4,CLINUM           BUMP CLINUM                                  
*                                                                               
BLDCX    B     EXIT                                                             
         DROP  R1                                                               
         EJECT                                                                  
**********************************************************************          
* SUB-ROUTINE TO BUILD A TABLE OF MONTHS IN THE ADVERTISING PERIOD   *          
**********************************************************************          
         SPACE 1                                                                
BLDADV   NTR1  ,                                                                
         ZIC   R3,ADVNUM                                                        
         LA    R5,ADVTAB                                                        
         MVC   FULL(2),ADVST                                                    
         MVI   FULL+2,1            SET DAY                                      
*                                                                               
BLDADV2  MVC   0(L'ADVTAB,R5),FULL SET MONTH                                    
         GOTO1 DATCON,DMCB,(1,FULL),(3,DUB)                                     
         ZIC   RF,DUB+1            RF=MONTH (BINARY)                            
         ZIC   RE,DUB              RE=YEAR (BINARY)                             
         LA    RF,1(RF)            ADD ONE MONTH                                
         CH    RF,=H'12'           TEST NEXT YEAR                               
         BH    BLDADV3             YES                                          
         STC   RF,DUB+1                                                         
         B     BLDADV4                                                          
*                                                                               
BLDADV3  MVI   DUB+1,1             MONTH IS ONE                                 
         LA    RE,1(RE)            ADD 1 TO YEAR                                
         STC   RE,DUB                                                           
*                                                                               
BLDADV4  GOTO1 DATCON,DMCB,(3,DUB),(1,FULL)                                     
*                                                                               
         LA    R5,L'ADVTAB(R5)                                                  
         BCT   R3,BLDADV2                                                       
*                                                                               
BLDADVX  B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* SUB-ROUTINE TO INITIALIZE TSAR FILE                                *          
**********************************************************************          
         SPACE 1                                                                
INIT     NTR1  ,                                                                
         L     R1,ATSARBLK                                                      
         USING TSARD,R1                                                         
         LA    RE,BUFF                                                          
         ST    RE,TSABUF                                                        
         LA    R0,TSARREC                                                       
         ST    R0,TSAREC                                                        
         MVC   TSACOM,ACOMFACS                                                  
         MVI   TSKEYL,TSARKEYL                                                  
         ZIC   RE,ADVNUM           RE=N'ADVERTISING PERIOD MONTHS               
         MH    RE,=Y(L'TSARPAID)   COMPUTE LENGTH OF PAID BUCKETS               
         LA    RE,TSARMINL(RE)     COMPUTE RECORD LENGTH                        
         STH   RE,TSARRECL                                                      
         STH   RE,TSRECL                                                        
         MVI   TSINDS,TSIALLOC     ALLOCATE TEMPEST STORAGE                     
         MVI   TSNBUF,1                                                         
*        MVI   TSPAGN,TSPAGNCI+1   1 CYLINDER INTERVAL                          
         MVI   TSPAGN,15           USE 15 PAGES                                 
         TM    CHKMODE,TEMPRES     TEST IF STORAGE RESERVED                     
         BZ    INIT2               NO                                           
*                                                                               
         MVC   TSINDS,CHKTSARI     RESTORE INDICATORS                           
         OI    TSINDS,TSIREUSE     RE-USE TEMPEST STORAGE                       
         MVC   TSPAGL,CHKLOWPG     LOW PAGE                                     
         MVC   TSPAGN,CHKNUMPG                                                  
*                                                                               
INIT2    MVI   TSACTN,TSAINI                                                    
         TM    CHKMODE,TSARINI     TEST IF TSAR FILE INITIALIZED                
         BZ    *+8                                                              
         MVI   TSACTN,TSARES       RESTORE THE FILE                             
         GOTO1 TSAR                                                             
         BE    INIT4               OK                                           
*                                                                               
         TM    CHKMODE,TEMPRES     TEST FIRST TIME                              
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         NI    TSINDS,X'FF'-TSIALLOC TRY REGULAR TEMPSTR INSTEAD                
         MVI   TSPAGL,2            USE TWA2                                     
         MVI   TSPAGN,1                                                         
         GOTO1 TSAR                                                             
         BE    INIT4                                                            
         DC    H'0'                REAL TROUBLE                                 
*                                                                               
INIT4    MVC   CHKLOWPG,TSPAGL                                                  
         MVC   CHKNUMPG,TSPAGN                                                  
         MVC   CHKTSARI,TSINDS                                                  
         NI    CHKTSARI,TSIALLOC                                                
         OI    CHKMODE,TSARINI+TEMPRES                                          
*                                                                               
INITX    B     EXIT                                                             
         DROP  R1                                                               
         EJECT                                                                  
**********************************************************************          
* SUB-ROUTINE TO OPEN A BATCH HEADER FOR THE CHECK                   *          
**********************************************************************          
         SPACE 1                                                                
OPEN     NTR1  ,                                                                
         MVI   DELSW,C'N'          SET DELETED RECORD FOUND TO NO               
         LA    R4,KEY                                                           
         USING ACBKEYD,R4                                                       
         XC    ACBKEY(ACBKEYL),ACBKEY                                           
         MVC   ACBKEY+ACBKEYL(L'ACBKEY-ACBKEYL),SPACES                          
         MVI   ACBKCODE,X'0B'                                                   
         MVC   ACBKCOMP,COMPANY                                                 
         MVC   ACBKOFF,TWAORIG                                                  
         MVI   ACBKGRUP,C'G'       GENERAL ACCOUNTING                           
         MVI   ACBKTYPE,30         BATCH TYPE 30                                
         MVC   ACBKDATE,TODAYP                                                  
         MVC   ACBKREF,BATMON                                                   
         MVI   RDUPDATE,C'Y'                                                    
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 HIGH                                                             
         CLC   ACBKEY,KEYSAVE      TEST IF RECORD FOUND                         
         BNE   OPEN2               NO-GO AHEAD AND ADD                          
         L     R4,AIO                                                           
         USING ACKEYD,R4                                                        
         MVI   DELSW,C'Y'          NOTE DELETED RECORD                          
         TM    ACSTATUS,X'80'      TEST FOR DELETE                              
         BO    OPEN2               YES-OK TO RE-ADD                             
         MVI   ERROR,RECEXIST                                                   
         LA    R2,HEDREFH                                                       
         B     ERREND                                                           
*                                                                               
OPEN2    NI    DMINBTS,X'FF'-X'08'                                              
         L     R4,AIO2             BUILD NEW RECORD IN IO2                      
         ST    R4,AIO              RESET IO POINTER                             
         LR    RE,R4                                                            
         L     RF,SIZEIO                                                        
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         USING ACBKEYD,R4                                                       
         MVC   ACBKEY,KEYSAVE                                                   
         USING ACKEYD,R4                                                        
         MVC   ACLENGTH,DATADISP                                                
         MVI   ACSTATUS,X'50'                                                   
*                                                                               
OPEN4    XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING ACBTCHD,R6                                                       
         MVI   ACBHEL,ACBHELQ                                                   
         MVI   ACBHLEN,ACBHLNQ                                                  
         MVC   ACBHNAME,BATNAME                                                 
         ZAP   ACBHCASH,CHKAMT                                                  
         ZAP   ACBHITEM,=P'1'                                                   
         GOTO1 ADDELEM                                                          
         CLI   DELSW,C'Y'          TEST IF DELETED RECORD FOUND                 
         BE    OPEN6                                                            
*                                                                               
         GOTO1 ADD                                                              
         B     OPENX                                                            
*                                                                               
OPEN6    GOTO1 WRITE                                                            
*                                                                               
OPENX    MVC   AIO,AIO1            RESTORE IO POINTER                           
         B     EXIT                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
* SUB-ROUTINE TO BUILD A TSAR FILE FOR THE RECEIVABLE ACCOUNT AND     *         
*     CLIENT.  ON EXIT, CC=EQ IF OK, CC=NEQ IF NO RECORDS FOUND       *         
***********************************************************************         
         SPACE 1                                                                
         USING CLITABD,R3                                                       
FILE     NTR1                                                                   
         L     R3,ACLITAB          R3=A(CLIENT CODE TABLE)                      
         SR    R5,R5                                                            
         ICM   R5,1,CLINUM         NUMBER OF ENTRIES IN TABLE                   
         BZ    FILEX                                                            
*                                                                               
FILE10   LA    R4,KEY                                                           
         USING ACINKEY,R4                                                       
         XC    ACINKEY,ACINKEY                                                  
         MVI   ACINCOD,ACINEQU                                                  
         MVI   ACINSREC,ACINSEQU                                                
         MVC   ACINCUL(L'ACINCUL+L'ACINACC),RECEIVE                             
         MVC   ACINCLT,CLICDE                  CLIENT CODE FROM TABLE           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         B     FILE30                                                           
*                                                                               
FILE20   LA    R4,KEY                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
*                                                                               
FILE30   CLC   ACINKEY(ACINPRD-ACINKEY),KEYSAVE                                 
         BNE   FILE60              END OF FILE                                  
*                                                                               
         MVI   ELCODE,ACIPFEQU     PROFILE ELEMENT                              
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACINPRFD,R6                                                      
         CLC   ACIPFPRE,ADVST      TEST EST. ENDS BEFORE ADV ST                 
         BL    FILE20                                                           
         CLC   ACIPFPRS,ADVEND                                                  
         BH    FILE20                                                           
*                                                                               
         CLC   ACINPRD,LASTPRD     TEST FOR CHANGE IN PRODUCT                   
         BE    FILE50              NO                                           
*                                                                               
         MVC   LASTPRD,ACINPRD     UPDATE LAST PRODUCT                          
         BAS   RE,TSTPRD                                                        
         BE    FILE40              PRODUCT IS OK                                
*                                                                               
         LA    R4,KEY                                                           
         MVI   ACINMED,X'FF'       FORCE NEXT PRODUCT                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         B     FILE30                                                           
*                                                                               
FILE40   MVI   RDUPDATE,C'N'       RE-READ ESTIMATE                             
         GOTO1 READ                                                             
*                                                                               
FILE50   MVC   TSARCLT,ACINCLT     BUILD A TSAR RECORD                          
         MVC   TSARPROD,ACINPRD    BUILD A TSAR RECORD                          
         MVC   TSARMED,ACINMED                                                  
         MVC   TSAREST,ACINEST                                                  
         MVC   TSAROFFC,EFFOFFC    OFFICE CODE                                  
         ZIC   R0,ADVNUM           NUMBER OF PERIODS                            
         LA    R1,TSARPAID                                                      
         ZAP   0(L'TSARPAID,R1),=P'0'                                           
         LA    R1,L'TSARPAID(R1)                                                
         BCT   R0,*-10                                                          
*                                                                               
         L     R1,ATSARBLK                                                      
         USING TSARD,R1                                                         
         MVI   TSACTN,TSAADD                                                    
         LA    RE,TSARREC                                                       
         ST    RE,TSAREC                                                        
         GOTO1 TSAR                                                             
         BNE   FILE60                                                           
*                                                                               
         LH    R1,TSARNUM          INCREMENT RECORD COUNT                       
         LA    R1,1(R1)                                                         
         STH   R1,TSARNUM                                                       
         B     FILE20              NEXT RECORD                                  
*                                                                               
FILE60   LA    R3,CLILNQ(R3)       BUMP TABLE                                   
         BCT   R5,FILE10                                                        
*                                                                               
FILEX    OC    TSARNUM,TSARNUM                                                  
         BNZ   YESXIT              FILE IS BUILT                                
         B     NOXIT                                                            
         DROP  R3,R4,R6                                                         
         EJECT                                                                  
***********************************************************************         
* SUB-ROUTINE TO READ A NEW PRODUCT AND TEST ITS SECURITY ACCESS      *         
*     CALLED FROM FILE, AT ENTRY KEY CONTAINS ESTIMATE KEY            *         
*     ON EXIT, CC=EQ IF PRODUCT OK AND EFFOFFC=OFFICE, CC=NEQ IF BAD  *         
*     R3 - CLIENT CODE TABLE (ACLITAB)                                *         
***********************************************************************         
         SPACE 1                                                                
         USING CLITABD,R3                                                       
TSTPRD   NTR1                                                                   
         L     R5,APROLEDG         R5=A(PRODUCTION LEDGER TABLE ENTRY)          
         USING LDGTABD,R5                                                       
*                                                                               
         MVC   SAVEKEY1,KEY        SAVE ESTIMATE KEY                            
         MVC   AIO,AIO2            RESET IO POINTER                             
         LA    R4,KEY                                                           
         USING ACKEYD,R4                                                        
         MVC   KEY,SPACES                                                       
         MVC   ACKEYACC(3),CUL                                                  
         MVC   ACKEYACC+3(L'CLICDE),CLICDE      CLIENT CODE FROM TABLE          
         ZIC   R1,LDGTLVA                       GET LENGTH OF CLIENT            
         LA    R1,ACKEYACC+3(R1)                                                
         L     RE,AIO1                          RE=A(ESTIMATE RECORD)           
         MVC   0(L'ACINPRD,R1),ACINPRD-ACINKEY(RE)                              
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(42),KEYSAVE                                                  
         BNE   TSTPNO              COULD NOT FIND PRODUCT                       
*                                                                               
         MVI   ELCODE,ACPRELQ      GET PROFILE ELEMENT                          
         MVC   EFFOFFC,CLIOFFC     SET OFFICE=CLIENT OFFICE                     
         BAS   RE,GETELIO                                                       
         BNE   TSTP10              NONE                                         
         USING ACPROFD,R6                                                       
         CLC   ACPROFFC,SPACES     TEST FOR PRODUCT LEVEL OFFICE                
         BNH   *+10                                                             
         MVC   EFFOFFC,ACPROFFC                                                 
*                                                                               
TSTP10   L     R1,AOFFBLK                                                       
         USING OFFALD,R1                                                        
         MVI   OFFAACT,OFFATST                                                  
         MVC   OFFAOPOS,LDGTOFFP                                                
         MVC   OFFAOFFC,EFFOFFC    OFFICE CODE                                  
         MVC   OFFAREC,AIO                                                      
                                                                                
         MVC   KEY(L'SAVEKEY1),SAVEKEY1                                         
         MVC   AIO,AIO1                                                         
         GOTO1 OFFAL                                                            
         BE    YESXIT                                                           
                                                                                
TSTPNO   MVC   KEY(L'SAVEKEY1),SAVEKEY1                                         
         MVC   AIO,AIO1                                                         
         B     NOXIT                                                            
         DROP  R1,R3,R4,R5,R6                                                   
         EJECT                                                                  
***********************************************************************         
* SUB-ROUTINE TO BUILD THE REVIEW SCREEN                              *         
*     ONLY THE REPORT, BANK ACCOUNT, AND CHECK FIELDS WILL BE         *         
*     AVAILABLE FOR CHANGE                                            *         
***********************************************************************         
         SPACE 1                                                                
REVHED   NTR1  ,                                                                
         LA    R0,HEDREPH          R0=DESTINATION                               
         L     RE,ATIA                                                          
         LA    RE,HEDREPH-CONRECH(RE)  RE=SOURCE                                
         LA    R1,HEDLAST-HEDREPH  MOVE AMOUNT                                  
         LR    RF,R1                                                            
         MVCL  R0,RE               SLOT IN THE HEADER DATA                      
         MVC   HEDLAST(3),=X'000101'  SEND BACK THE WHOLE SCREEN                
*                                                                               
REVHED2  OI    HEDNAMEH+1,X'20'    NOW PROTECT THE NECESSARY FIELDS             
         OI    HEDREFH+1,X'20'                                                  
         OI    HEDMONH+1,X'20'                                                  
         OI    HEDRCVH+1,X'20'                                                  
         OI    HEDCLIH+1,X'20'                                                  
         OI    HEDPERH+1,X'20'                                                  
         OI    HEDPROH+1,X'20'                                                  
         OI    HEDMEDH+1,X'20'                                                  
         OI    HEDESTH+1,X'20'                                                  
*                                                                               
REVHED4  XC    HEDPF,HEDPF                                                      
         MVC   HEDPF(L'REVPFS),REVPFS DISPLAY A NEW PFKEY LINE                  
         SR    R0,R0               R0=BXLE INCREMENT REGISTER                   
         LA    R1,HEDLAST-1        R1=BXLE LIMIT                                
         LA    R2,CONRECH          TRANSMIT ALL FIELDS                          
         OI    6(R2),X'80'                                                      
         ICM   R0,1,0(R2)                                                       
         BZ    REVHEDX                                                          
         BXLE  R2,R0,*-12                                                       
*                                                                               
REVHEDX  B     EXIT                                                             
         EJECT                                                                  
* SUB-ROUTINE TO PREPARE THE QUIT SCREEN                                        
*                                                                               
QUITHED  NTR1  ,                                                                
         LA    R0,HEDREPH          RESTORE THE HEADER DATA                      
         L     RE,ATIA                                                          
         LA    RE,HEDREPH-CONRECH(RE)                                           
         LA    R1,HEDTOT1H-HEDREPH UP TO FIRST TOTALS LINE                      
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
QUITHED2 MVC   DMCB+4(3),SYSPHASE                                               
         MVI   DMCB+7,X'E2'        LOAD IN QUIT SCREEN AT BOTTOM                
         GOTO1 CALLOV,DMCB,HEDTOT1H,,,0                                         
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
QUITHED4 LA    R2,HEDREPH          PROTECT REPORT TO ESTIMATE                   
         LA    R1,HEDEST                                                        
         SR    R0,R0                                                            
         OI    1(R2),X'20'                                                      
         IC    R0,0(R2)                                                         
         BXLE  R2,R0,*-8                                                        
*                                                                               
QUITHED6 LA    R2,QUIINPH                                                       
         BAS   RE,BUMP                                                          
         CLI   0(R2),0             TEST FOR EOS                                 
         BNE   *-8                                                              
         MVC   1(2,R2),=X'0101'                                                 
*                                                                               
         LR    R1,R2               EOS                                          
         BCTR  R1,0                SET BXLE LIMIT                               
         LA    R2,CONRECH          XMIT BACK ALL FIELDS                         
         SR    R0,R0                                                            
         OI    6(R2),X'80'                                                      
         IC    R0,0(R2)                                                         
         BXLE  R2,R0,*-8                                                        
*                                                                               
QUITHEDX B     EXIT                                                             
         EJECT                                                                  
* SUB-ROUTINE TO EDIT THE QUIT SCREEN                                           
*                                                                               
QUIT     NTR1  ,                                                                
         CLI   PFKEY,PF2           TEST PF2=DRAFT                               
         BE    QUIT2               YES                                          
         CLI   PFKEY,PF3           TEST PF3=FILTER                              
         BNE   QUIT10                                                           
*                                                                               
         MVC   CONACT,=CL8'FILTER'                                              
         MVI   ACTNUM,ACTNFILT     RESET ACTION FOR OVERLAY                     
         B     QUIT4                                                            
*                                                                               
QUIT2    MVC   CONACT,=CL8'DRAFT'                                               
         MVI   ACTNUM,ACTNDFT                                                   
*                                                                               
QUIT4    MVI   PFKEY,0                                                          
         GOTO1 LOADOV,UPDMOD                                                    
         GOTO1 (RF),DMCB,(RC)                                                   
         BAS   RE,SAVTSAR          SAVE IT                                      
         B     QUITX                                                            
*                                                                               
QUIT10   LA    R2,QUIINPH                                                       
         MVI   ERROR,MISSING                                                    
         CLI   5(R2),0             INPUT FIELD IS REQUIRED                      
         BE    ERREND                                                           
*                                                                               
         MVI   ERROR,INVALID                                                    
         CLI   8(R2),C'Y'          TEST USER WANTS TO QUIT                      
         BE    QUIT14                                                           
         CLI   8(R2),C'N'          TEST USER DOESN'T WANT TO QUIT               
         BNE   ERREND                                                           
*                                                                               
QUIT12   MVI   PFKEY,0             FORCE A RETURN TO EXIT QUIT                  
         GOTO1 VRETURN                                                          
*                                                                               
QUIT14   LA    R4,KEY              MARK BATCH HEADER DELETED                    
         USING ACBKEYD,R4                                                       
         XC    ACBKEY(ACBKEYL),ACBKEY                                           
         MVC   ACBKEY+ACBKEYL(L'ACBKEY-ACBKEYL),SPACES                          
         MVI   ACBKCODE,X'0B'                                                   
         MVC   ACBKCOMP,COMPANY                                                 
         MVC   ACBKOFF,TWAORIG                                                  
         MVI   ACBKGRUP,C'G'       GENERAL ACCOUNTING                           
         MVI   ACBKTYPE,30         BATCH TYPE 30                                
         MVC   ACBKDATE,TODAYP                                                  
         MVC   ACBKREF,BATMON                                                   
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         CLC   ACBKEY,KEYSAVE      TEST IF RECORD FOUND                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,AIO                                                           
         USING ACKEYD,R4                                                        
         MVI   ACSTATUS,X'90'      DELETE + RECORDS ON RECOVERY                 
         GOTO1 WRITE                                                            
*                                                                               
QUIT16   NI    CHKMODE,X'FF'-TSARINI TURN OFF TSAR INIT BIT                     
         LA    R0,CONRECH                                                       
         L     RE,ATIA                                                          
         LA    R1,HEDLAST-CONRECH  RESTORE HEADER SCREEN                        
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         NI    CONACTH+4,X'FF'-X'20'                                            
*                                                                               
QUIT18   NI    HEDREPH+1,X'FF'-X'20'   MAKE SURE FIELDS ARE UNPROTECTED         
         NI    HEDNAMEH+1,X'FF'-X'20'                                           
         NI    HEDREFH+1,X'FF'-X'20'                                            
         NI    HEDMONH+1,X'FF'-X'20'                                            
         NI    HEDBANKH+1,X'FF'-X'20'                                           
         NI    HEDCKNH+1,X'FF'-X'20'                                            
         NI    HEDDEPH+1,X'FF'-X'20'                                            
         NI    HEDAMTH+1,X'FF'-X'20'                                            
         NI    HEDCKDH+1,X'FF'-X'20'                                            
         NI    HEDRCVH+1,X'FF'-X'20'                                            
         NI    HEDCLIH+1,X'FF'-X'20'                                            
         NI    HEDPERH+1,X'FF'-X'20'                                            
         NI    HEDPROH+1,X'FF'-X'20'                                            
         NI    HEDMEDH+1,X'FF'-X'20'                                            
         NI    HEDESTH+1,X'FF'-X'20'                                            
         MVC   HEDLAST(3),=X'000101'                                            
*                                                                               
QUIT20   LA    R2,CONRECH          XMIT BACK ALL FIELDS                         
         LA    R1,HEDLAST-1                                                     
         SR    R0,R0                                                            
         OI    6(R2),X'80'                                                      
         IC    R0,0(R2)                                                         
         BXLE  R2,R0,*-8                                                        
*                                                                               
QUITX    B     EXIT                                                             
         EJECT                                                                  
* SUB-ROUTINE TO PREPARE THE UPDATE SCREEN                                      
*                                                                               
UPHED    NTR1  ,                                                                
         LA    R0,HEDREPH          RESTORE THE HEADER DATA                      
         L     RE,ATIA                                                          
         LA    RE,HEDREPH-CONRECH(RE)                                           
         LA    R1,HEDTOT1H-HEDREPH UP TO FIRST TOTALS LINE                      
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
UPHED2   MVC   DMCB+4(3),SYSPHASE                                               
         MVI   DMCB+7,X'E3'        LOAD IN UPDATE SCREEN AT BOTTOM              
         GOTO1 CALLOV,DMCB,HEDTOT1H,,,0                                         
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
UPHED4   LA    R2,HEDREPH          PROTECT REPORT TO ESTIMATE                   
         LA    R1,HEDEST                                                        
         SR    R0,R0                                                            
         OI    1(R2),X'20'                                                      
         IC    R0,0(R2)                                                         
         BXLE  R2,R0,*-8                                                        
*                                                                               
UPHED6   LA    R2,UPDINPH                                                       
         BAS   RE,BUMP                                                          
         CLI   0(R2),0             TEST FOR EOS                                 
         BNE   *-8                                                              
         MVC   1(2,R2),=X'0101'                                                 
*                                                                               
         LR    R1,R2               EOS                                          
         BCTR  R1,0                SET BXLE LIMIT                               
         LA    R2,CONRECH          XMIT BACK ALL FIELDS                         
         SR    R0,R0                                                            
         OI    6(R2),X'80'                                                      
         IC    R0,0(R2)                                                         
         BXLE  R2,R0,*-8                                                        
*                                                                               
UPHEDX   B     EXIT                                                             
         EJECT                                                                  
* SUB-ROUTINE TO HANDLE THE UPDATE ACTION                                       
*                                                                               
UP       NTR1  ,                                                                
         CLI   PFKEY,PF2           TEST PF2=DRAFT                               
         BE    UP2                 YES                                          
         CLI   PFKEY,PF3           TEST PF3=FILTER                              
         BNE   UP10                NO                                           
*                                                                               
         MVC   CONACT,=CL8'FILTER'                                              
         MVI   ACTNUM,ACTNFILT     SET ACTION=FILTER                            
         B     UP4                                                              
*                                                                               
UP2      MVC   CONACT,=CL8'DRAFT'                                               
         MVI   ACTNUM,ACTNDFT                                                   
*                                                                               
UP4      MVI   PFKEY,0                                                          
         GOTO1 LOADOV,UPDMOD                                                    
         GOTO1 (RF),DMCB,(RC)                                                   
         BAS   RE,SAVTSAR                                                       
         B     UPX                 ALL DONE                                     
*                                                                               
UP10     LA    R2,UPDINPH                                                       
         MVI   ERROR,MISSING                                                    
         CLI   5(R2),0                                                          
         BE    ERREND                                                           
*                                                                               
         MVI   ERROR,INVALID                                                    
         CLI   8(R2),C'Y'          TEST FOR 'Y'=UPDATE                          
         BE    UP14                                                             
         CLI   8(R2),C'N'                                                       
         BNE   ERREND                                                           
*                                                                               
UP12     MVI   PFKEY,0             RETURN IF USER DOES NOT WANT UPDATE          
         GOTO1 VRETURN                                                          
*                                                                               
UP14     CP    TOTPAID,CHKAMT      TEST TOTAL PAID=CHECK AMOUNT                 
         BE    UP16                                                             
*                                                                               
         MVC   LISTAR,SPACES                                                    
         LA    R4,LISTAR                                                        
         MVC   0(L'PAIDLAB,R4),PAIDLAB                                          
         LA    R4,L'PAIDLAB+1(R4)                                               
         CURED TOTPAID,(12,(R4)),2,ALIGN=LEFT,MINUS=YES                         
         AR    R4,R0                                                            
         LA    R4,1(R4)                                                         
         MVC   0(L'NEQLAB,R4),NEQLAB                                            
         LA    R4,L'NEQLAB+1(R4)                                                
         MVC   0(L'CHKLAB,R4),CHKLAB                                            
         LA    R4,L'CHKLAB+1(R4)                                                
         CURED (P8,CHKAMT),(12,(R4)),2,ALIGN=LEFT,MINUS=YES                     
         LA    R2,CONHEADH                                                      
         BAS   RE,MOVEFLD                                                       
         MVI   ERROR,X'FE'         SPECIAL ERROR MESSAGE                        
         LA    R2,UPDINPH                                                       
         B     ERREND                                                           
*                                                                               
UP16     GOTO1 LOADOV,UPDMOD                                                    
         GOTO1 (RF),DMCB,(RC)                                                   
         NI    CHKMODE,X'FF'-TSARINI FORCE TSAR RE-INIT                         
         ZAP   TOTPAID,=P'0'                                                    
         MVC   DMCB+4(3),SYSPHASE  RE-LOAD HEADER SCREEN                        
         MVI   DMCB+7,X'F2'                                                     
         GOTO1 CALLOV,DMCB,CONTAGH,,0                                           
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   CONACT,=CL8'HEADER'                                              
         MVC   HEDREP,PERSON       SEND BACK THE PERSON                         
         MVC   HEDLAST(3),=X'000101'                                            
         LA    R2,CONRECH          XMIT NEW SCREEN BACK                         
         LA    R1,HEDLAST-1                                                     
         SR    R0,R0                                                            
         OI    6(R2),X'80'                                                      
         IC    R0,0(R2)                                                         
         BXLE  R2,R0,*-8                                                        
*                                                                               
UPX      B     EXIT                                                             
         EJECT                                                                  
* SUB-ROUTINE TO VALIDATE THE PRODUCT FILTERS                                   
* AT ENTRY, P1=A(FIELD HEADER)                                                  
*                                                                               
VALPRD   NTR1  BASE=ABASE1                                                      
         L     RC,AGEND                                                         
         L     R7,ABASE2                                                        
         MVI   NPRDS,0             CLEAR OUT PRODUCT VALUES                     
         XC    PRDLIST,PRDLIST                                                  
         MVC   PRODCHAR,SPACES                                                  
         CLI   5(R2),0             TEST FOR ANY INPUT                           
         BE    VALPRDX                                                          
*                                                                               
         GOTO1 ANY                                                              
         MVC   PRODCHAR,WORK       SAVE CHARACTER INPUT                         
         GOTO1 SCANNER,DMCB,(0,(R2)),(X'80',BLOCK),0                            
         MVI   ERROR,INVALID                                                    
         CLI   4(R1),0             TEST FOR A REAL DISASTER                     
         BE    ERREND                                                           
*                                                                               
         MVC   NPRDS,4(R1)                                                      
         ZIC   R3,NPRDS            R3=LOOP COUNTER                              
         LA    R4,BLOCK            R4=A(SCANNER BLOCK)                          
         USING SCANBLKD,R4                                                      
         LA    R5,PRDLIST                                                       
         MVI   NEGSW,C'N'          INITIALIZE NEGATIVE FILTER SWITCH            
*                                                                               
VALPRD2  CLI   SCONEFLD,C'*'       TEST FOR NEGATIVE FILTER                     
         BNE   VALPRD4             NO                                           
         MVI   NEGSW,C'Y'                                                       
         ZIC   R1,SC1STLEN                                                      
         BCTR  R1,0                                                             
         STC   R1,SC1STLEN                                                      
         MVC   SCONEFLD(L'SCONEFLD-1),SCONEFLD+1 ERASE THE STAR                 
*                                                                               
VALPRD4  MVC   ERRNDX,SC2NDNUM                                                  
         CLI   SC2NDLEN,0          TEST FOR DIVIDED FIELD                       
         BNE   VALPRDR             YES                                          
         MVC   ERRNDX,SC1STNUM                                                  
         CLI   SC1STLEN,0                                                       
         BE    VALPRDR                                                          
         CLI   SC1STLEN,L'ACINPRD  TEST PRODUCT CODE IS TOO BIG                 
         BH    VALPRDR                                                          
         MVC   0(L'ACINPRD,R5),SCONEFLD                                         
         CLI   NEGSW,C'Y'                                                       
         BNE   *+8                                                              
         NI    0(R5),X'FF'-X'40'                                                
*                                                                               
VALPRD6  LA    R5,L'ACINPRD(R5)                                                 
         LA    R4,L'SCLINE(R4)                                                  
         BCT   R3,VALPRD4                                                       
*                                                                               
VALPRDX  MVI   ERRNDX,0                                                         
         B     EXIT                                                             
*                                                                               
VALPRDR  MVI   ERROR,INVALID                                                    
         B     ERREND                                                           
         EJECT                                                                  
* SUB-ROUTINE TO VALIDATE THE MEDIA FILTERS                                     
* AT ENTRY, R2=A(FIELD HEADER)                                                  
*                                                                               
VALMEDF  NTR1  BASE=ABASE1                                                      
         L     RC,AGEND                                                         
         L     R7,ABASE2                                                        
         MVC   MEDCHAR,SPACES                                                   
         MVI   NMEDS,0                                                          
         XC    MEDLIST,MEDLIST                                                  
         CLI   5(R2),0                                                          
         BE    VALMEDX                                                          
*                                                                               
         GOTO1 ANY                                                              
         MVC   MEDCHAR,WORK        SAVE CHARACTER INPUT                         
         GOTO1 SCANNER,DMCB,(0,(R2)),(X'80',BLOCK),0                            
         MVI   ERROR,INVALID                                                    
         CLI   4(R1),0             TEST FOR A REAL DISASTER                     
         BE    ERREND                                                           
*                                                                               
         MVC   NMEDS,4(R1)                                                      
         ZIC   R3,NMEDS            R3=LOOP COUNTER                              
         LA    R4,BLOCK            R4=A(SCANNER BLOCK)                          
         USING SCANBLKD,R4                                                      
         LA    R5,MEDLIST                                                       
         MVI   NEGSW,C'N'          INITIALIZE NEGATIVE FILTER SWITCH            
*                                                                               
VALMED2  CLI   SCONEFLD,C'*'       TEST FOR NEGATIVE FILTER                     
         BNE   VALMED4             NO                                           
         MVI   NEGSW,C'Y'                                                       
         ZIC   R1,SC1STLEN                                                      
         BCTR  R1,0                                                             
         STC   R1,SC1STLEN                                                      
         MVC   SCONEFLD(L'SCONEFLD-1),SCONEFLD+1 ERASE THE STAR                 
*                                                                               
VALMED4  MVC   ERRNDX,SC2NDNUM                                                  
         CLI   SC2NDLEN,0          TEST FOR DIVIDED FIELD                       
         BNE   VALMEDR             YES                                          
         MVC   ERRNDX,SC1STNUM                                                  
         CLI   SC1STLEN,0                                                       
         BE    VALMEDR                                                          
         CLI   SC1STLEN,L'ACINMED  TEST MEDIA CODE IS TOO BIG                   
         BH    VALMEDR                                                          
         MVC   0(L'ACINMED,R5),SCONEFLD                                         
         CLI   NEGSW,C'Y'                                                       
         BNE   *+8                                                              
         NI    0(R5),X'FF'-X'40'                                                
*                                                                               
VALMED6  LA    R5,L'ACINMED(R5)                                                 
         LA    R4,L'SCLINE(R4)                                                  
         BCT   R3,VALMED4                                                       
*                                                                               
VALMEDX  MVI   ERRNDX,0                                                         
         B     EXIT                                                             
*                                                                               
VALMEDR  MVI   ERROR,INVALID                                                    
         B     ERREND                                                           
         DROP  R4                                                               
         EJECT                                                                  
* SUB-ROUTINE TO VALIDATE THE ESTIMATE FILTERS                                  
* AT ENTRY, R2=A(FIELD HEADER)                                                  
*                                                                               
VALESTF  NTR1  BASE=ABASE1                                                      
         L     RC,AGEND                                                         
         L     R7,ABASE2                                                        
         MVC   ESTCHAR,SPACES                                                   
         MVI   NESTS,0                                                          
         XC    ESTLIST,ESTLIST                                                  
         CLI   5(R2),0                                                          
         BE    VALESTX                                                          
*                                                                               
         GOTO1 ANY                                                              
         MVC   ESTCHAR,WORK        SAVE CHARACTER INPUT                         
         GOTO1 SCANNER,DMCB,(0,(R2)),(X'80',BLOCK),0                            
         MVI   ERROR,INVALID                                                    
         CLI   4(R1),0             TEST FOR A REAL DISASTER                     
         BE    ERREND                                                           
*                                                                               
         MVC   NESTS,4(R1)                                                      
         ZIC   R3,NESTS            R3=LOOP COUNTER                              
         LA    R4,BLOCK            R4=A(SCANNER BLOCK)                          
         USING SCANBLKD,R4                                                      
         LA    R5,ESTLIST                                                       
         MVI   NEGSW,C'N'          INITIALIZE NEGATIVE FILTER SWITCH            
*                                                                               
VALEST2  CLI   SCONEFLD,C'*'       TEST FOR NEGATIVE FILTER                     
         BNE   VALEST4             NO                                           
         MVI   NEGSW,C'Y'                                                       
         ZIC   R1,SC1STLEN                                                      
         BCTR  R1,0                                                             
         STC   R1,SC1STLEN                                                      
         MVC   SCONEFLD(L'SCONEFLD-1),SCONEFLD+1 ERASE THE STAR                 
*                                                                               
VALEST4  MVC   ERRNDX,SC2NDNUM                                                  
         CLI   SC2NDLEN,0          TEST FOR DIVIDED FIELD                       
         BNE   VALESTR             YES                                          
         MVC   ERRNDX,SC1STNUM                                                  
         CLI   SC1STLEN,0                                                       
         BE    VALESTR                                                          
         CLI   SC1STLEN,L'ACINEST  TEST ESTIMATE CODE IS TOO BIG                
         BH    VALESTR                                                          
         MVC   0(L'ACINEST,R5),SCONEFLD                                         
         CLI   NEGSW,C'Y'                                                       
         BNE   *+8                                                              
         NI    0(R5),X'FF'-X'40'                                                
*                                                                               
VALEST6  LA    R5,L'ACINEST(R5)                                                 
         LA    R4,L'SCLINE(R4)                                                  
         BCT   R3,VALEST4                                                       
*                                                                               
VALESTX  MVI   ERRNDX,0                                                         
         B     EXIT                                                             
*                                                                               
VALESTR  MVI   ERROR,INVALID                                                    
         B     ERREND                                                           
         DROP  R4                                                               
         EJECT                                                                  
********************************************************************            
* SUB-ROUTINE TO APPLY GLOBAL FILTERS TO A TSAR RECORD             *            
* AT ENTRY, TSARREC=A(RECORD)                                      *            
* ON EXIT, CC=EQ IF RECORD OK, CC=NEQ IF RECORD FAILS FILTERS      *            
********************************************************************            
         SPACE 1                                                                
FILTER   NTR1  BASE=ABASE1                                                      
         L     RC,AGEND                                                         
         L     R7,ABASE2                                                        
         CLI   NPRDS,0                                                          
         BE    FILTER4                                                          
*                                                                               
         ZIC   R2,NPRDS                                                         
         LA    R5,PRDLIST                                                       
         TM    PRDLIST,X'40'       TEST NEGATIVE FILTER                         
         BZ    FILTER2                                                          
*                                                                               
FILTER1  CLC   TSARPROD,0(R5)                                                   
         BE    FILTER4                                                          
         LA    R5,L'TSARPROD(R5)                                                
         BCT   R2,FILTER1                                                       
         B     NOXIT               DID NOT PASS ANY PRODUCT TESTS               
*                                                                               
FILTER2  MVC   DUB,0(R5)                                                        
         OI    DUB,X'40'           RESTORE UPPER CASE BIT                       
         CLC   TSARPROD,DUB                                                     
         BE    NOXIT                                                            
         LA    R5,L'TSARPROD(R5)                                                
         BCT   R2,FILTER2                                                       
*                                                                               
FILTER4  CLI   NMEDS,0                                                          
         BE    FILTER8                                                          
*                                                                               
         ZIC   R2,NMEDS                                                         
         LA    R5,MEDLIST                                                       
         TM    MEDLIST,X'40'       TEST NEGATIVE FILTER                         
         BZ    FILTER6                                                          
*                                                                               
FILTER5  CLC   TSARMED,0(R5)                                                    
         BE    FILTER8                                                          
         LA    R5,L'TSARMED(R5)                                                 
         BCT   R2,FILTER5                                                       
         B     NOXIT               DID NOT PASS ANY PRODUCT TESTS               
*                                                                               
FILTER6  MVC   DUB,0(R5)                                                        
         OI    DUB,X'40'           RESTORE UPPER CASE BIT                       
         CLC   TSARMED,DUB                                                      
         BE    NOXIT                                                            
         LA    R5,L'TSARMED(R5)                                                 
         BCT   R2,FILTER6                                                       
*                                                                               
FILTER8  CLI   NESTS,0                                                          
         BE    FILTERX                                                          
*                                                                               
         ZIC   R2,NESTS                                                         
         LA    R5,ESTLIST                                                       
         TM    ESTLIST,X'40'       TEST NEGATIVE FILTER                         
         BZ    FILTER10                                                         
*                                                                               
FILTER9  CLC   TSAREST,0(R5)                                                    
         BE    FILTERX                                                          
         LA    R5,L'TSAREST(R5)                                                 
         BCT   R2,FILTER9                                                       
         B     NOXIT               DID NOT PASS ANY PRODUCT TESTS               
*                                                                               
FILTER10 MVC   DUB,0(R5)                                                        
         OI    DUB,X'40'           RESTORE UPPER CASE BIT                       
         CLC   TSAREST,DUB                                                      
         BE    NOXIT                                                            
         LA    R5,L'TSAREST(R5)                                                 
         BCT   R2,FILTER10                                                      
*                                                                               
FILTERX  B     YESXIT                                                           
         EJECT                                                                  
* SUB-ROUTINE TO MOVE OUT DISPLAY DATA                                          
*                                                                               
MOVEFLD  ST    RE,SAVERE                                                        
         ZIC   R1,0(R2)                                                         
         LA    RE,8+1                                                           
         TM    1(R2),X'02'         TEST FOR EXTENDED FIELD                      
         BZ    *+8                                                              
         LA    RE,8+8+1                                                         
         SR    R1,RE                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),LISTAR                                                   
*                                                                               
MOVEFLDX L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO LOAD IN AN OVERLAY                                             
* AT ENTRY, R1=MODULE NUMBER.  ON EXIT, RF=A(MODULE)                            
*                                                                               
LOADOV   ST    RE,FULL                                                          
         L     RE,=V(DUMMY)                                                     
         A     RE,RELO                                                          
         ST    RE,DMCB             SET LOAD POINT                               
         MVC   DMCB+4(3),SYSPHASE                                               
         STC   R1,DMCB+7           SET MODULE NUMBER                            
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'         TEST FOR ERROR IN LOAD                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB             RF=A(MODULE)                                 
         L     RE,FULL                                                          
         BR    RE                                                               
         SPACE 2                                                                
         EJECT                                                                  
*              SUPPORTING SUBROUTINES                                           
*                                                                               
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
SAVTSAR  ST    RE,SAVERE                                                        
         L     R1,ATSARBLK                                                      
         USING TSARD,R1                                                         
         MVI   TSACTN,TSASAV                                                    
         GOTO1 TSAR                                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
**********************************************************************          
* GETEL                                                              *          
**********************************************************************          
         SPACE 1                                                                
GETELIO  L     R6,AIO                                                           
         GETEL (R6),DATADISP,ELCODE                                             
         EJECT                                                                  
**********************************************************************          
* EXITS WITH CONDITIONS                                              *          
**********************************************************************          
         SPACE 1                                                                
ERREND   GOTO1 VERRCUR                                                          
*                                                                               
YESXIT   CR    RB,RB               SET CC=EQ AND EXIT                           
         B     *+6                                                              
*                                                                               
NOXIT    LTR   RB,RB               SET CC=NEQ AND EXIT                          
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* PATCH AREA                                                         *          
**********************************************************************          
         SPACE 1                                                                
PATCH    DS    0H                                                               
         DC    XL32'00'                                                         
         EJECT                                                                  
**********************************************************************          
* CONSTANTS                                                          *          
**********************************************************************          
         SPACE 1                                                                
DMREAD   DC    C'DMREAD'                                                        
DMWRT    DC    C'DMWRT '                                                        
TEMPSTR  DC    C'TEMPSTR'                                                       
NONEMSG  DC    C'** No estimates found for account/client/period **'            
REVPFS   DC    C'PF12=Return'                                                   
REVMSG   DC    C'Header data shown for your review - Enter any changes'         
REVCHMSG DC    C'Header data changed'                                           
QUITDMSG DC    C'**Please confirm your decision to Quit**'                      
QUITMSG  DC    C'**Quit completed - enter a new header**'                       
UPCMSG   DC    C'**Please confirm your instruction to Update**'                 
NOCHGMSG DC    C'You did not make any changes'                                  
PAIDLAB  DC    C'Total Paid'                                                    
NEQLAB   DC    C'Does Not Equal'                                                
CHKLAB   DC    C'Check Amount'                                                  
MONTAB   DC    C'.123456789......ABC'                                           
         EJECT                                                                  
**********************************************************************          
* EQUATES                                                            *          
**********************************************************************          
         SPACE 1                                                                
LSTMOD   EQU   X'15'               LIST MODULE                                  
ESTMOD   EQU   X'13'               ESTIMATE MODULE                              
UPDMOD   EQU   X'22'               UPDATE/REPORT MODULE                         
*                                                                               
CLI2BIG  EQU   172                 TOO MANY CLIS ENTERED-5 IS MAX ERROR         
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* ++INCLUDES                                                         *          
**********************************************************************          
         SPACE 1                                                                
*  DDSPOOLD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
*  DDSPLWORKD                                                                   
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
*  ACGENBOTH                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
*  ACBMONVALD                                                                   
         PRINT OFF                                                              
       ++INCLUDE ACBMONVALD                                                     
         PRINT ON                                                               
*  ACINTWORKD                                                                   
         PRINT OFF                                                              
       ++INCLUDE ACINTWORKD                                                     
         PRINT ON                                                               
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE ACINTF2D                                                       
         SPACE 2                                                                
         DS    0D                                                               
SAVEVALS DS    CL(SAVELNQ)         SAVED BATCH VALUES                           
         EJECT                                                                  
* QUIT SCREEN                                                                   
*                                                                               
         ORG   HEDTOT1H                                                         
       ++INCLUDE ACINTE2D                                                       
         EJECT                                                                  
* UPDATE SCREEN                                                                 
*                                                                               
         ORG   HEDTOT1H                                                         
       ++INCLUDE ACINTE3D                                                       
         EJECT                                                                  
       ++INCLUDE ACINT12COM                                                     
         SPACE 2                                                                
* SUB-CONTROLLER STORAGE                                                        
*                                                                               
         ORG   SUBCWRK                                                          
LASTPRD  DS    CL(L'ACINPRD)                                                    
         EJECT                                                                  
       ++INCLUDE DDSCANBLKD                                                     
         EJECT                                                                  
       ++INCLUDE DDPERVALD                                                      
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'030ACINT12   04/14/05'                                      
         END                                                                    
