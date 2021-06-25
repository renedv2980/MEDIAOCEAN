*          DATA SET TAGENEA    AT LEVEL 007 AS OF 03/10/10                      
*PHASE T702EAA,*                                                                
         TITLE 'T702EA - CHECK PULL'                                            
T702EA   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T702EA,R7                                                      
         SPACE 1                                                                
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         EJECT                                                                  
* VALIDATE THE CHECK KEY AND READ THE CHECK RECORD INTO AIO1                    
         SPACE                                                                  
         MVC   PULSHED(7),=C'PID NUM'                                           
         OI    PULSHEDH+6,X'80'                                                 
MODE05   CLI   MODE,VALKEY                                                      
         BNE   MODE10                                                           
         BAS   RE,VKEY                                                          
         SPACE                                                                  
* TURN ON PROSDPF BIT FIRST TIME INTO CHECK PULL SCREEN SO                      
* CHECK INFORMATION DISPLAYED BEFORE ASKING TO CHANGE                           
* (IF AGENCY IS PRESENT WE KNOW IT HAS BEEN DISPLAYED)                          
         SPACE                                                                  
MODE10   NI    PROSTAT,X'FF'-PROSDPF                                            
         TM    PULAGYNH+4,X'20'                                                 
         BO    MODE20                                                           
         OI    PROSTAT,PROSDPF                                                  
         BAS   RE,DREC                                                          
         SPACE                                                                  
* TURN ON PROWARN IF USER HAS CHANGED SOMETHING ON SCREEN                       
* AND HIT PFKEY13 BEFORE SAVING  (ONLY GIVE WARNING ONCE)                       
         SPACE                                                                  
MODE20   CLI   PFAID,13                                                         
         BNE   MODE30                                                           
         TM    PROSTAT,PROWARN                                                  
         BO    MODE30                                                           
         GOTO1 FLDVAL,DMCB,(X'40',PULSSNH),(X'80',999)                          
         BE    MODE30                                                           
         OI    PROSTAT,PROWARN                                                  
         MVI   MODE,VALREC                                                      
         B     MODE50                                                           
         SPACE                                                                  
* IF NO WARNING NECESSARY AND IF USER CAME FROM SOMEWHERE                       
* OTHER THAN CHECK PLIST  ...  SEE IF USER WANTS TO GO TO                       
* CHECK DISPLAY                                                                 
         SPACE                                                                  
MODE30   NI    PROSTAT,X'FF'-PROWARN                                            
         CLI   THISLSEL,PULLL2M                                                 
         BE    MODE40                                                           
         OI    TRNSTAT,OKINTPFK                                                 
         GOTO1 INITIAL,DMCB,(X'40',PFTAB)                                       
         B     MODE50                                                           
         SPACE                                                                  
* IF NO WARNING NECESSARY, CHECK TO SEE IF USER CAME FROM                       
* CHECK PLIST, IF DONE HERE AND NOT REQUESTING CHECK DISPLAY                    
* GO BACK TO CHECK PLIST                                                        
         SPACE                                                                  
MODE40   GOTO1 FLDVAL,DMCB,(X'40',PULSSNH),(X'80',999)                          
         BNE   MODE45                                                           
         CLI   PFAID,0                                                          
         BNE   MODE45                                                           
         MVI   PFAID,17                                                         
         MVI   THISLSEL,0                                                       
         OI    TRNSTAT,OKINTPFK                                                 
         SPACE                                                                  
* IF NO WARNING NECESSARY, CHECK TO SEE IF USER CAME FROM                       
* CHECK PLIST, IF DONE HERE AND REQUESTING CHECK DISPLAY                        
* GO BACK TO CHECK DISPLAY                                                      
         SPACE                                                                  
MODE45   CLI   PFAID,13                                                         
         BNE   *+12                                                             
         MVI   THISLSEL,PULLC2M                                                 
         OI    TRNSTAT,OKINTPFK                                                 
         GOTO1 INITIAL,DMCB,(X'40',LISTTAB)                                     
         SPACE                                                                  
* VALIDATE AND DISPLAY THE CHECK RECORD                                         
         SPACE                                                                  
MODE50   CLI   MODE,VALREC                                                      
         BNE   *+12                                                             
         BAS   RE,VREC                                                          
         BAS   RE,DREC                                                          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO HANDLE MODE VALKEY.                                   
         SPACE                                                                  
VKEY     NTR1                                                                   
         SPACE                                                                  
         BAS   RE,PFINVIS          MAKE ALL PF KEYS INVISIBLE                   
         SPACE                                                                  
         LA    R2,PULCHKH          IF KEY ALREADY VALIDATED                     
         TM    4(R2),X'20'         JUST READ CHECK RECORD AND                   
         BO    VK40                EXIT                                         
         SPACE                                                                  
         GOTO1 HEXIN,DMCB,PULCHK,DSKADD,L'PULCHK   IF COMING FROM               
         BAS   RE,CHKNOIN          CHECK PLIST OR NO CHECK NUMBER               
         BE    VK50                YET, READ FOR REC BY DISK ADDR               
         SPACE                                                                  
         CLI   5(R2),0             IF INPUTTED, MOVE INTO TGCHK                 
         BE    *+14                AND SKIP AHEAD                               
         MVC   TGCHK,8(R2)                                                      
         B     VK25                                                             
         SPACE                                                                  
         CLI   KEY,TLCKCCDQ                                                     
         BNE   VK10                                                             
         GOTO1 READCK,DMCB,(C'A',0)                                             
         XC    TGCHK,TGCHK                                                      
         USING TACDD,R4                                                         
         L     R4,AIO              IF NO CHECK INPUT                            
         MVI   ELCODE,TACDELQ      AND COMING FROM CHECK DISPLAY                
         BAS   RE,GETEL            READ THE CHECK RECORD                        
         BNE   *+10                                                             
         MVC   TGCHK,TACDCHK       USE CHECK NUMBER IF PRESENT                  
         MVC   PULCHK,=CL8'N/A'    OTHERWISE, N/A                               
         DROP  R4                                                               
         SPACE                                                                  
VK10     OC    TGCHK,TGCHK         CHECK FOR GLOBAL NUMBER                      
         BZ    VK20                                                             
         MVC   PULCHK,TGCHK        AND USE IT                                   
         OI    6(R2),X'80'                                                      
         MVI   5(R2),L'TGCHK                                                    
         SPACE                                                                  
VK20     BAS   RE,CHKNOIN                                                       
         BE    VK30                                                             
VK25     GOTO1 READCK,DMCB,(C'Y',0) READ THE CHECK RECORD                       
         SPACE                                                                  
VK30     BAS   RE,LIMITCHK         CHECK LIMIT ACCESS                           
         BNE   ERNOFND                                                          
         SPACE                                                                  
         NI    PULAGYNH+4,X'FF'-X'20'                                           
         B     VK50                                                             
         SPACE                                                                  
VK40     BAS   RE,CHKNOIN           READ CHECK RECORD                           
         BE    VK50                 (EITHER BY DISK ADDRESS                     
         GOTO1 READCK,DMCB,(C'Y',0)  OR KEY)                                    
VK50     BAS   RE,PFVIS             MARK AND CHECK VALID PF KEYS                
         SPACE                                                                  
         USING TLDRD,R4                                                         
VKX      LA    R4,KEY               MOVE THE DISK ADDRESS TO SCR                
         GOTO1 HEXOUT,DMCB,TLDRDA,PULDKAD,L'TLDRDA                              
         DROP  R4                                                               
         SPACE                                                                  
         OI    PULCHKH+4,X'20'       AND SET VALIDATED                          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO CHECK AGENCY AND CLIENT LIMIT ACCESS                  
         SPACE 1                                                                
LIMITCHK NTR1                                                                   
         LHI   R2,1                                                             
*                                                                               
         USING TLCKD,R4                                                         
         L     R4,AIO                                                           
         MVC   CHKAGY,TLCKAGY      INITIALIZE CHECK'S AGENCY                    
         XC    CHKCLI,CHKCLI       AND CLIENT                                   
         DROP  R4                                                               
*                                                                               
         USING TAOID,R4                                                         
         MVI   ELCODE,TAOIELQ      SAVE CHECK'S AGENCY                          
         BRAS  RE,GETEL                                                         
         BNE   LIMCHK10                                                         
         MVC   CHKAGY,TAOIAGY                                                   
         DROP  R4                                                               
*                                                                               
         USING TAPDD,R4                                                         
LIMCHK10 L     R4,AIO              CHECK CLIENT LIMIT ACCESS                    
         MVI   ELCODE,TAPDELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   LIMCHK20                                                         
         MVC   CHKCLI,TAPDCLI                                                   
         DROP  R4                                                               
*                                                                               
         USING FAWSSVRD,R1                                                      
LIMCHK20 LA    R1,LIMBLK                                                        
         MVC   FAWSTOKN(3),=C'STF'                                              
         STC   R2,FAWSTOKN+3                                                    
         MVI   FAWSACTN,FAWSARST   RECALL STAFF2 INFORMATION VIA WWSVR          
         XC    FAWSLEN,FAWSLEN                                                  
         MVC   FAWSADR,TGAS2ACC                                                 
         GOTO1 WSSVR,(R1)                                                       
         CLI   FAWSRTN,0           IF NOT FOUND, STAFF HAS NO ACCESS            
         BNE   LIMCHKNO                                                         
         DROP  R1                                                               
*                                                                               
         AHI   R2,1                                                             
*                                                                               
         USING TAVAD,R1                                                         
         L     R1,TGAS2ACC                                                      
         OC    TAVAAGY,TAVAAGY     IF STAFF HAS NO AGENCY LIMITS,               
         BZ    YES                 STAFF HAS ACCESS TO ALL RECORDS              
*                                                                               
LIMCHK30 CLI   0(R1),0             RECALL NEXT RECORD FROM WSSVR                
         BE    LIMCHK20                                                         
*                                                                               
         CLC   CHKAGY,TAVAAGY      IF AGENCY IS FOUND IN STAFF LIMITS           
         BNE   LIMCHK50                                                         
*                                                                               
         CLI   TAVALEN,TAVALNQ     IF NO CLIENT LIMITS ARE DEFINED              
         BE    YES                 ACCESS IS GRANTED                            
*                                                                               
         ZIC   RE,TAVALEN                                                       
         SHI   RE,TAVALNQ                                                       
         LA    RF,TAVACLI                                                       
LIMCHK40 CLC   CHKCLI,0(RF)        IF CLIENT IS FOUND IN STAFF LIMITS           
         BE    YES                 ACCESS IS GRANTED                            
         LA    RF,L'TAVACLI(RF)                                                 
         SHI   RE,L'TAVACLI                                                     
         LTR   RE,RE                                                            
         BNZ   LIMCHK40                                                         
*                                                                               
LIMCHK50 ZIC   RE,TAVALEN          BUMP TO NEXT VALID AGENCY/CLIENT             
         AR    R1,RE               ELEMENT                                      
         B     LIMCHK30                                                         
         DROP  R1                                                               
*                                                                               
LIMCHKNO NI    PROSTAT,X'FF'-PROSDPF                                            
         B     NO                                                               
*                                                                               
CHKAGY   DS    CL6                 CHECK'S AGENCY                               
CHKCLI   DS    CL6                 CHECK'S CLIENT                               
LIMBLK   DS    XL100               AGENCY/CLIENT LIMIT WORK BLOCK               
         EJECT                                                                  
*              MAKE ALL PF KEYS INVISIBLE AND TRANSMITTED                       
         SPACE 1                                                                
PFINVIS  NTR1                                                                   
         NI    PULPF14H+1,X'FF'-X'08'                                           
         NI    PULPF15H+1,X'FF'-X'08'                                           
         NI    PULPF16H+1,X'FF'-X'08'                                           
         OI    PULPF14H+1,X'0C'                                                 
         OI    PULPF15H+1,X'0C'                                                 
         OI    PULPF16H+1,X'0C'                                                 
         OI    PULPF14H+6,X'80'                                                 
         OI    PULPF15H+6,X'80'                                                 
         OI    PULPF16H+6,X'80'                                                 
         B     XIT                                                              
         EJECT                                                                  
*              MAKE PF KEYS VISIBLE BASED ON CHECK DATA                         
         SPACE 1                                                                
         USING TAKLD,R4                                                         
PFVIS    NTR1                                                                   
         OI    PULPF13H+6,X'80'                                                 
         MVC   PULPF13,=CL18'PF13=Check Display'                                
         SPACE                                                                  
         L     R4,AIO                                                           
         MVI   ELCODE,TAKLELQ      TRY TO GET CHECK PULL ELEMENT                
         BAS   RE,GETEL                                                         
         BE    PFV10                                                            
         SPACE                                                                  
         NI    PULPF14H+1,X'FF'-X'0C'                                           
         OI    PULPF14H+1,X'08'                                                 
         CLI   PFAID,0             IF CHECK PULL ELEMENT NOT                    
         BE    PFVX                YET ADDED                                    
         CLI   PFAID,13            ONLY VALID PF KEYS ARE 13                    
         BE    *+12                AND 14                                       
         CLI   PFAID,14                                                         
         BNE   ERRPFK                                                           
         B     PFVX                                                             
         SPACE                                                                  
PFV10    OC    TAKLFXBY,TAKLFXBY                                                
         BNZ   PFV20                                                            
         NI    PULPF15H+1,X'FF'-X'0C'                                           
         OI    PULPF15H+1,X'08'                                                 
         NI    PULPF16H+1,X'FF'-X'0C'                                           
         OI    PULPF16H+1,X'08'                                                 
         CLI   PFAID,0             IF CHECK PULL ELEMENT IS                     
         BE    PFVX                FAXED PF KEY 14 IS INVALID                   
         CLI   PFAID,14                                                         
         BE    ERRPFK                                                           
         B     PFVX                                                             
         SPACE                                                                  
PFV20    NI    PULPF15H+1,X'FF'-X'0C'                                           
         OI    PULPF15H+1,X'08'                                                 
         CLI   PFAID,13                                                         
         BE    XIT                                                              
         CLI   PFAID,15            IF CHECK PULL ELEMENT IS                     
         BE    XIT                 ADDED AND FAXED                              
         CLI   PFAID,0             ONLY PF 13 AND 15 ARE VALID                  
         BNE   ERRPFK                                                           
PFVX     B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO GET CHECK RECORD IF NOT INPUTTED                      
         SPACE                                                                  
CHKNOIN  NTR1                                                                   
         LA    R2,PULCHKH                                                       
         OC    DSKADD,DSKADD       IF NOTHING IN DISK ADDRESS                   
         BNZ   CNI10               FIELD                                        
         MVI   THISLSEL,0                                                       
         MVC   CONKEY,=20C' '      RETURN MISSING CHECK NUMBER                  
         OI    CONKEYH+6,X'80'     ERROR                                        
         B     ERRMIS                                                           
         SPACE                                                                  
CNI10    CLI   THISLSEL,PULLL2M    IF COMING FROM CHECK PLIST                   
         BE    *+12                                                             
         CLI   THISLSEL,PULLC2M    OR CHECK DIPLAY                              
         BNE   NO                                                               
         XC    KEY,KEY             PUT DISK ADDRESS INTO KEY                    
         LA    R4,KEY                                                           
         USING TLDRD,R4                                                         
         MVC   TLDRDA,DSKADD                                                    
         MVC   SYSDIR,=CL8'CHKDIR'                                              
         MVC   SYSFIL,=CL8'CHKFIL'                                              
         MVI   RDUPDATE,C'Y'                                                    
         OI    DMINBTS,X'80'                                                    
         GOTO1 GETREC              AND GET CHECK RECORD                         
         SPACE                                                                  
         L     R4,AIO                                                           
         CLI   0(R4),TLCKCDQ                                                    
         BNE   NO                                                               
         SPACE                                                                  
         MVC   PULCHK,=CL8'N/A'    DEFAULT CHECK NUMBER TO                      
         MVI   PULCHKH+5,3         N/A                                          
         OI    PULCHKH+6,X'80'                                                  
         SPACE                                                                  
         USING TACDD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TACDELQ      IF CHECK DETAILS ELEMENT                     
         BAS   RE,GETEL            EXISTS                                       
         BNE   YES                                                              
         OC    TACDCHK,TACDCHK     AND CHECK NUMBER EXISTS                      
         BZ    YES                 IN IT                                        
         MVC   PULCHK,TACDCHK      OUTPUT CHECK NUMBER                          
         B     YES                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO DISPLAY CHECK RECORD                                  
*                                                                               
DREC     NTR1                                                                   
         GOTO1 FLDVAL,DMCB,(X'01',PULSSNH),(X'80',999)                          
         SPACE                                                                  
         USING TLCKD,R4                                                         
         L     R4,AIO                                                           
         MVC   PULAGY,TLCKAGY    COPY AGENCY,SS# AND INVOICE#                   
         MVC   PULSSN,TLCKSSN    FROM KEY TO SCREEN                             
         GOTO1 TINVCON,DMCB,TLCKINV,PULINV,DATCON                               
         DROP  R4                                                               
         SPACE                                                                  
         USING TAOID,R4                                                         
         L     R4,AIO            IF OLD AGENCY/INVOICE ELEMENT                  
         MVI   ELCODE,TAOIELQ    EXISTS                                         
         BAS   RE,GETEL          USE THIS AGENCY AND INVOICE                    
         BNE   DREC05            INSTEAD                                        
         MVC   PULAGY,TAOIAGY                                                   
         GOTO1 TINVCON,DMCB,TAOIINV,PULINV,DATCON                               
         DROP  R4                                                               
         SPACE                                                                  
DREC05   BAS   RE,SETTAL         SET TO READ FROM TAL FILE                      
         MVC   AIO,AIO2          DISPLAY AGENCY AND SS# NAMES                   
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'8C',PULAGY),PULAGYNH                      
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'8C',PULSSN),PULSSNNH                      
*                                                                               
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   PULSSN,SPACES                                                    
         MVC   PULSSN(L'TGPID),TGPID                                            
         MVI   PULSSNH+5,6                                                      
         OI    PULSSNH+6,X'80'                                                  
*                                                                               
DREC06   MVC   AIO,AIO1                                                         
         SPACE                                                                  
         USING TACDD,R4                                                         
         L     R4,AIO            R4=A(CHECK DETAILS ELEMENT)                    
         MVI   ELCODE,TACDELQ                                                   
         BAS   RE,GETEL          COPY CHECK DATE                                
         BNE   DREC10                                                           
         GOTO1 DATCON,DMCB,(1,TACDDTE),(8,PULCHKD)                              
         OC    TACDNET,TACDNET   AND NET AMOUNT TO SCREEN                       
         BZ    DREC10                                                           
         EDIT  TACDNET,(11,PULNET),2,MINUS=YES,ALIGN=LEFT                       
         DROP  R4                                                               
         SPACE                                                                  
         USING TATID,R4                                                         
DREC10   L     R4,AIO            R4=A(TAX ID ELEMENT)                           
         MVI   ELCODE,TATIELQ                                                   
         BAS   RE,GETEL          DISPLAY CORP ID AND NAME                       
         BNE   DREC15                                                           
         MVC   PULCRP,TATIID                                                    
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'AC',PULCRP),PULCRPNH                      
         MVC   AIO,AIO1                                                         
         DROP  R4                                                               
         SPACE                                                                  
         USING TACAD,R4                                                         
DREC15   L     R4,AIO             R4=A(CAST DETAIL ELEMENT)                     
         MVI   ELCODE,TACAELQ                                                   
         BAS   RE,GETEL           DISPLAY AGENT CODE AND NAME                   
         BNE   DREC16                                                           
         OC    TACANCDE,TACANCDE                                                
         BZ    DREC16                                                           
         GOTO1 TRNSAGT,DMCB,(X'40',TACANCDE),PULNCDE                            
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLANCCDQ,(X'AC',PULNCDE),PULNCDNH                    
         MVC   AIO,AIO1                                                         
         DROP  R4                                                               
         SPACE                                                                  
         USING TAPDD,R4                                                         
DREC16   L     R4,AIO                                                           
         MVI   ELCODE,TAPDELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   DREC20                                                           
         MVC   PULEMP,TAPDEMP                                                   
         MVI   PULCUR,C'U'                                                      
         TM    TAPDSTAT,TAPDSCAN                                                
         BZ    DREC20                                                           
         MVI   PULCUR,C'C'                                                      
         DROP  R4                                                               
         SPACE                                                                  
         USING TLDRD,R4                                                         
DREC20   LA    R4,KEY                                                           
         GOTO1 HEXOUT,DMCB,TLDRDA,PULDKAD,L'TLDRDA                              
         DROP  R4                                                               
         SPACE                                                                  
         USING TAKLD,R4                                                         
         MVC   AIO,AIO1          GET CHECK PULL ELEMENT                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAKLELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   DREC30                                                           
         SPACE                                                                  
         MVC   PULAGYC,TAKLCONT  AGENCY CONTACT                                 
         MVC   PULFAXN,TAKLFAX#  AGENCY FAX NUMBER                              
         SPACE                                                                  
         GOTO1 DATCON,DMCB,(1,TAKLPLDT),(8,PULPLDT)  PULL DATE                  
         MVI   PULPLDTH+5,8                                                     
         SPACE                                                                  
         TM    TAKLSTA2,TAKLSCUR INDICATE IF CHECK BEING PULLED                 
         BZ    *+12              IS URGENT                                      
         MVI   PULURGT,C'X'                                                     
         MVI   PULURGTH+5,1                                                     
         SPACE                                                                  
         TM    TAKLSTA2,TAKLSCOV INDICATE IF CHECK BEING PULLED                 
         BZ    *+12              IS OVERNIGHT                                   
         MVI   PULONVT,C'X'                                                     
         MVI   PULONVTH+5,1                                                     
         SPACE                                                                  
         TM    TAKLSTAT,TALKSG50 INDICATE IF REASON FOR CHECK PULL              
         BZ    *+12              IS GUARANTEE OVER 50K                          
         MVI   PULGRT,C'X'                                                      
         MVI   PULGRTH+5,1                                                      
         SPACE                                                                  
         TM    TAKLSTAT,TAKLSVOD INDICATE IF REASON FOR CHECK PULL              
         BZ    *+12              IS VOID                                        
         MVI   PULVOID,C'X'                                                     
         MVI   PULVOIDH+5,1                                                     
         SPACE                                                                  
         TM    TAKLSTAT,TAKLSHLD INDICATE IF REASON FOR CHECK PULL              
         BZ    *+12              IS HOLD                                        
         MVI   PULHOLD,C'X'                                                     
         MVI   PULHOLDH+5,1                                                     
         SPACE                                                                  
         TM    TAKLSTAT,TAKLSWIR INDICATE IF REASON FOR CHECK PULL              
         BZ    *+12              IS WIRE                                        
         MVI   PULWIRE,C'X'                                                     
         MVI   PULWIREH+5,1                                                     
         SPACE                                                                  
         TM    TAKLSTAT,TAKLSDRF INDICATE IF REASON FOR CHECK PULL              
         BZ    *+12              IS DRAFT                                       
         MVI   PULDRFT,C'X'                                                     
         MVI   PULDRFTH+5,1                                                     
         SPACE                                                                  
         TM    TAKLSTAT,TAKLSOVN INDICATE IF REASON FOR CHECK PULL              
         BZ    *+12              IS OVERNIGHT                                   
         MVI   PULOVNG,C'X'                                                     
         MVI   PULOVNGH+5,1                                                     
         SPACE                                                                  
         TM    TAKLSTAT,TAKLSOTH INDICATE IF REASON FOR CHECK PULL              
         BZ    *+12              IS OTHER                                       
         MVI   PULOTHR,C'X'                                                     
         MVI   PULOTHRH+5,1                                                     
         SPACE                                                                  
         GOTO1 CHAROUT,DMCB,TAADELQ,(6,PULDAD1H)  ADDRESS                       
         SPACE                                                                  
         GOTO1 DATCON,DMCB,(1,TAKLGCDD),(8,PULGRCD)  GUAR CHK DUE               
         SPACE                                                                  
         TM    TAKLSTA3,TAKLSTAY INDICATE IF TO AGENCY                          
         BZ    *+12                                                             
         MVI   PULTAGY,C'X'                                                     
         MVI   PULTAGYH+5,1                                                     
         SPACE                                                                  
         TM    TAKLSTA3,TAKLSTTA INDICATE IF TO TALENT                          
         BZ    *+12                                                             
         MVI   PULTTAL,C'X'                                                     
         MVI   PULTTALH+5,1                                                     
         SPACE                                                                  
         TM    TAKLSTA3,TAKLSTAG INDICATE IF TO AGENT                           
         BZ    *+12                                                             
         MVI   PULTAGT,C'X'                                                     
         MVI   PULTAGTH+5,1                                                     
         SPACE                                                                  
         TM    TAKLSTA3,TAKLSTOT INDICATE IF TO OTHER                           
         BZ    *+12                                                             
         MVI   PULTOTH,C'X'                                                     
         MVI   PULTOTHH+5,1                                                     
         SPACE                                                                  
         TM    TAKLSTA2,TAKLSVUP INDICATE IF VIA USPS                           
         BZ    *+12                                                             
         MVI   PULVUPS,C'X'                                                     
         MVI   PULVUPSH+5,1                                                     
         SPACE                                                                  
         TM    TAKLSTA2,TAKLOVCR INDICATE IF VIA OVERNIGHT CARRIER              
         BZ    *+12                                                             
         MVI   PULVOVC,C'X'                                                     
         MVI   PULVOVCH+5,1                                                     
         SPACE                                                                  
         TM    TAKLSTA2,TAKLSVPC INDICATE IF VIA POUCH                          
         BZ    *+12                                                             
         MVI   PULVPCH,C'X'                                                     
         MVI   PULVPCHH+5,1                                                     
         SPACE                                                                  
         TM    TAKLSTA2,TAKLSVLM INDICATE IF VIA LOCAL MESSENGER                
         BZ    *+12                                                             
         MVI   PULVLMS,C'X'                                                     
         MVI   PULVLMSH+5,1                                                     
         SPACE                                                                  
         MVC   PULRQMB,TAKLRQBY  COPY PULL REQUEST MADE BY INFO                 
         GOTO1 DATCON,DMCB,(1,TAKLRQDT),(8,PULRQMD)   TO SCREEN                 
         SPACE                                                                  
         MVC   PULINFB,TAKLFXBY      COPY INVOICE FAXED INFO TO                 
         GOTO1 DATCON,DMCB,(1,TAKLFXDT),(8,PULINFD)      SCREEN                 
         SPACE                                                                  
         GOTO1 DATCON,DMCB,(1,TAKLDTRC),(8,PULDTRC)                             
         GOTO1 DATCON,DMCB,(1,TAKLDTRT),(8,PULDTCR)                             
         DROP  R4                                                               
         SPACE                                                                  
         MVI   LSTCHGF,25                                                       
         GOTO1 ACTVOUT,DMCB,(X'80',LSTCHGF)   DISP LAST CHANGED                 
         MVC   PULLCHP,LSTCHGF+8                                                
         MVC   PULLCHD,LSTCHGF+17                                               
         SPACE                                                                  
         USING TAXCD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAXCELQ     SPECIAL INSTRUCTIONS                          
         BAS   RE,GETEL                                                         
         B     *+8                                                              
DREC30   BAS   RE,NEXTEL                                                        
         BNE   DRECX                                                            
         CLI   TAXCSEQ,6                                                        
         BNE   DREC30                                                           
         ZIC   RE,TAXCLEN                                                       
         SHI   RE,4                                                             
         STC   RE,PULSPINH+5                                                    
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   PULSPIN(0),TAXCCMNT                                              
         DROP  R4                                                               
         SPACE                                                                  
DRECX    GOTO1 FLDVAL,DMCB,(X'22',PULSSNH),999                                  
         SPACE                                                                  
         TM    PROSTAT,PROSDPF     IF FIRST TIME INTO CHECK STOP                
         BO    ENTCHG              SCREEN GIVE PROPER MESSAGE                   
         SPACE                                                                  
         CLI   MODE,VALREC         IF RECORD JUST CHANGED GIVE                  
         BE    CHKCHG              PROPER MESSAGE                               
         B     XIT                                                              
         SPACE                                                                  
         EJECT                                                                  
*              ROUTINE TO VALIDATE CHECK RECORD                                 
         SPACE                                                                  
VREC     NTR1                                                                   
         MVI   TEMPSTA1,0        CLEAR TEMPORARY VARIABLES                      
         MVI   TEMPSTA2,0                                                       
         XC    PLDTE,PLDTE                                                      
         XC    AGYCONT,AGYCONT                                                  
         XC    AGYFAX#,AGYFAX#                                                  
         SPACE                                                                  
         LA    R2,PULCHKH                                                       
         SPACE                                                                  
         GOTO1 SAVPTRS,DMCB,PBLOCK                                              
         SPACE                                                                  
         USING TACDD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TACDELQ    CANNOT STOP PAYMENT                            
         BAS   RE,GETEL                                                         
         BNE   VREC10                                                           
         OC    TACDCSH,TACDCSH   IF CHECK ALREADY CASHED                        
         BNZ   ERRINV                                                           
         OC    TACDDTE,TACDDTE   OR CHECK DATE EARLIER THAN TODAY               
         BZ    VREC10                                                           
         CLC   TACDDTE,TGTODAY1                                                 
         BL    ERRDATE                                                          
         DROP  R4                                                               
         SPACE                                                                  
VREC10   XC    TEMPREQ,TEMPREQ                                                  
         XC    TEMPFAX,TEMPFAX                                                  
         SPACE                                                                  
         USING TAKLD,R4                                                         
         L     R4,AIO            GET CHECK PULL ELEMENT                         
         MVI   ELCODE,TAKLELQ    IF IT ALREADY EXISTS                           
         BAS   RE,GETEL                                                         
         BNE   VREC16                                                           
         SPACE                                                                  
         MVC   TEMPREQ,TAKLRQBY                                                 
         MVC   TEMPFAX,TAKLFXBY                                                 
         SPACE                                                                  
         CLI   PFAID,14          IF ADDING STOP PAY REQUEST                     
         BE    ERRPFK            CANNOT ALREADY BE ADDED                        
         SPACE                                                                  
         CLI   PFAID,16                                                         
         BNE   VREC17                                                           
         OC    TAKLFXBY,TAKLFXBY                                                
         BNZ   ERRPFK                                                           
         B     VREC17                                                           
         SPACE                                                                  
VREC16   CLI   PFAID,16          IF FAXING                                      
         BE    ERRINV            REQUEST MUST ALREADY BE ADDED                  
         DROP  R4                                                               
         SPACE                                                                  
VREC17   MVC   AGYCONT,PULAGYC   SAVE AGENCY CONTACT                            
         MVC   AGYFAX#,PULFAXN   SAVE AGENCY FAX NUMBER                         
         SPACE                                                                  
         LA    R2,PULPLDTH        VALIDATE PULL DATE                            
         CLI   5(R2),0                                                          
         BE    ERRMIS                                                           
         GOTO1 DTVAL,DMCB,PLDTE                                                 
         SPACE                                                                  
VREC18   LA    R2,PULURGTH       MUST CHECK OFF ONE OF THE URGENT               
         CLI   5(R2),0           OR OVERNIGHT OPTIONS                           
         BE    VREC20                                                           
         OI    TEMPSTA2,TAKLSCUR URGENT                                         
         SPACE                                                                  
VREC20   LA    R2,PULONVTH                                                      
         CLI   5(R2),0           OR OVERNIGHT                                   
         BE    VREC50                                                           
         CLI   TEMPSTA2,0                                                       
         BNE   ERRINV                                                           
         OI    TEMPSTA2,TAKLSCOV                                                
         SPACE                                                                  
VREC50   LA    R2,PULURGTH                                                      
         CLI   TEMPSTA2,0                                                       
         BE    ERRMIS                                                           
         SPACE                                                                  
         LA    R2,PULVOIDH       MUST CHECK OFF ONE OF THE REASON               
         CLI   5(R2),0           CHECK PULL OPTIONS                             
         BE    VREC70                                                           
         OI    TEMPSTA1,TAKLSVOD VOID                                           
         SPACE                                                                  
VREC70   LA    R2,PULHOLDH                                                      
         CLI   5(R2),0           OR HOLD                                        
         BE    VREC80                                                           
         CLI   TEMPSTA1,0                                                       
         BNE   ERRINV                                                           
         OI    TEMPSTA1,TAKLSHLD                                                
         SPACE                                                                  
VREC80   LA    R2,PULWIREH                                                      
         CLI   5(R2),0           OR WIRE                                        
         BE    VREC90                                                           
         CLI   TEMPSTA1,0                                                       
         BNE   ERRINV                                                           
         OI    TEMPSTA1,TAKLSWIR                                                
         SPACE                                                                  
VREC90   LA    R2,PULDRFTH                                                      
         CLI   5(R2),0           OR VOID                                        
         BE    VREC100                                                          
         CLI   TEMPSTA1,0                                                       
         BNE   ERRINV                                                           
         OI    TEMPSTA1,TAKLSDRF                                                
         SPACE                                                                  
VREC100  LA    R2,PULOVNGH                                                      
         CLI   5(R2),0           OR OVERNIGHT                                   
         BE    VREC110                                                          
         CLI   TEMPSTA1,0                                                       
         BNE   ERRINV                                                           
         OI    TEMPSTA1,TAKLSOVN                                                
         SPACE                                                                  
VREC110  LA    R2,PULGRTH                                                       
         CLI   5(R2),0           OR GUARANTEE OVER 50K                          
         BE    VREC115                                                          
         CLI   TEMPSTA1,0                                                       
         BNE   ERRINV                                                           
         OI    TEMPSTA1,TALKSG50                                                
         SPACE                                                                  
VREC115  LA    R2,PULOTHRH                                                      
         CLI   5(R2),0           OR OTHER                                       
         BE    VREC120                                                          
         CLI   TEMPSTA1,0                                                       
         BNE   ERRINV                                                           
         OI    TEMPSTA1,TAKLSOTH                                                
         SPACE                                                                  
VREC120  LA    R2,PULVOIDH                                                      
         CLI   TEMPSTA1,0                                                       
         BE    ERRMIS                                                           
         SPACE                                                                  
         MVI   BYTE,6              IF REASON IS GUARANTEE OVER                  
         TM    TEMPSTA1,TALKSG50   50K DESTINATION ADDRESS IS                   
         BO    *+8                 REQUIRED, OTHERWISE OPTIONAL                 
         OI    BYTE,X'80'                                                       
         GOTO1 ADDRIN,DMCB,(BYTE,PULDAD1H)                                      
         SPACE                                                                  
         USING TAXCD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAXCELQ      REMOVE PREVIOUS SPECIAL                      
         BAS   RE,GETEL            INSTRUCTION ELEMENT                          
         B     *+8                                                              
VREC130  BAS   RE,NEXTEL                                                        
         BNE   VREC140                                                          
         CLI   TAXCSEQ,6                                                        
         BNE   VREC130                                                          
         MVI   TAXCEL,X'FF'                                                     
         B     VREC130                                                          
VREC140  MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM                                                          
         SPACE                                                                  
         LA    R2,PULSPINH                                                      
         CLI   5(R2),0                                                          
         BE    VREC150                                                          
         XC    ELEMENT,ELEMENT                                                  
         LA    R4,ELEMENT                                                       
         MVI   TAXCEL,TAXCELQ                                                   
         ZIC   RE,5(R2)                                                         
         AHI   RE,4                                                             
         STC   RE,TAXCLEN         ADD TANX ELEMENT FOR                          
         MVI   TAXCSEQ,6          SPECIAL INSTRUCTIONS FIELD                    
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   TAXCCMNT(0),8(R2)                                                
         GOTO1 ADDELEM                                                          
         DROP  R4                                                               
         SPACE                                                                  
VREC150  MVI   ELCODE,TAKLELQ    DELETE ALREADY EXISTING PULL                   
         GOTO1 REMELEM           CHECK ELEMENT                                  
         SPACE                                                                  
         XC    ELEMENT,ELEMENT   BEGIN BUILDING NEW ONE                         
         LA    R4,ELEMENT                                                       
         USING TAKLD,R4                                                         
         MVI   TAKLEL,TAKLELQ                                                   
         MVI   TAKLLEN,TAKLLNQ                                                  
         MVC   TAKLCONT,AGYCONT                                                 
         MVC   TAKLFAX#,AGYFAX#                                                 
         MVC   TAKLPLDT,PLDTE                                                   
         MVC   TAKLSTAT,TEMPSTA1                                                
         MVC   TAKLSTA2,TEMPSTA2                                                
         MVC   TAKLRQBY(11),TEMPREQ                                             
         MVC   TAKLFXBY(11),TEMPFAX                                             
         SPACE                                                                  
         LA    R2,PULGRCDH                                                      
         TM    TAKLSTAT,TALKSG50   GUARANTEE CHECK DUE DATE                     
         BO    *+12                REQUIRED IF REASON IS GUARANTEE              
         CLI   5(R2),0             OVER 50K, OTHERWISE OPTIONAL                 
         BE    VREC160                                                          
         GOTO1 DTVAL,DMCB,TAKLGCDD                                              
         SPACE                                                                  
VREC160  LA    R2,PULTAGYH       MUST CHECK OFF ONE OF THE TO                   
         CLI   5(R2),0           OPTIONS                                        
         BE    VREC170                                                          
         OI    TAKLSTA3,TAKLSTAY TO AGENCY                                      
         SPACE                                                                  
VREC170  LA    R2,PULTTALH                                                      
         CLI   5(R2),0           OR TO TALENT                                   
         BE    VREC190                                                          
         CLI   TAKLSTA3,0                                                       
         BNE   ERRINV                                                           
         OI    TAKLSTA3,TAKLSTTA                                                
         SPACE                                                                  
VREC190  LA    R2,PULTAGTH                                                      
         CLI   5(R2),0           OR TO AGENT                                    
         BE    VREC200                                                          
         CLI   TAKLSTA3,0                                                       
         BNE   ERRINV                                                           
         OI    TAKLSTA3,TAKLSTAG                                                
         SPACE                                                                  
VREC200  LA    R2,PULTOTHH                                                      
         CLI   5(R2),0           OR OTHER                                       
         BE    VREC210                                                          
         CLI   TAKLSTA3,0                                                       
         BNE   ERRINV                                                           
         OI    TAKLSTA3,TAKLSTOT                                                
         SPACE                                                                  
VREC210  TM    TAKLSTAT,TAKLSHLD                                                
         BO    VREC215                                                          
         TM    TAKLSTAT,TAKLSVOD                                                
         BO    VREC215                                                          
         LA    R2,PULTAGYH                                                      
         CLI   TAKLSTA3,0                                                       
         BE    ERRMIS                                                           
         SPACE                                                                  
VREC215  MVI   TEMPSTA1,0                                                       
         LA    R2,PULVUPSH       MUST CHECK OFF ONE OF THE VIA                  
         CLI   5(R2),0           OPTIONS                                        
         BE    VREC216                                                          
         OI    TAKLSTA2,TAKLSVUP VIA UPS                                        
         OI    TEMPSTA1,TAKLSVUP                                                
         SPACE                                                                  
VREC216  LA    R2,PULVOVCH                                                      
         CLI   5(R2),0           OR VIA OVERNIGHT CARRIER                       
         BE    VREC220                                                          
         CLI   TEMPSTA1,0                                                       
         BNE   ERRINV                                                           
         OI    TAKLSTA2,TAKLOVCR                                                
         OI    TEMPSTA1,TAKLOVCR                                                
         SPACE                                                                  
VREC220  LA    R2,PULVPCHH                                                      
         CLI   5(R2),0           OR VIA POUCH                                   
         BE    VREC230                                                          
         CLI   TEMPSTA1,0                                                       
         BNE   ERRINV                                                           
         OI    TAKLSTA2,TAKLSVPC                                                
         OI    TEMPSTA1,TAKLSVPC                                                
         SPACE                                                                  
VREC230  LA    R2,PULVLMSH                                                      
         CLI   5(R2),0           OR VIA LOCAL MESSENGER                         
         BE    VREC240                                                          
         CLI   TEMPSTA1,0                                                       
         BNE   ERRINV                                                           
         OI    TAKLSTA2,TAKLSVLM                                                
         OI    TEMPSTA1,TAKLSVLM                                                
         SPACE                                                                  
VREC240  TM    TAKLSTAT,TAKLSHLD                                                
         BO    VREC241                                                          
         TM    TAKLSTAT,TAKLSVOD                                                
         BO    VREC241                                                          
         LA    R2,PULVUPSH                                                      
         CLI   TEMPSTA1,0                                                       
         BE    ERRMIS                                                           
         SPACE                                                                  
VREC241  OC    TAKLFXBY,TAKLFXBY  IF INVOICE ALREADY FAXED                      
         BZ    VREC245                                                          
         LA    R2,PULINFBH        OK TO CHANGE FAXER                            
         CLI   5(R2),0                                                          
         BE    ERRMIS                                                           
         MVC   TAKLFXBY(8),PULINFB                                              
         LA    R2,PULINFDH        OR FAX DATE                                   
         CLI   5(R2),0                                                          
         BE    ERRMIS             BUT NOT OK TO DELETE THEM                     
         GOTO1 DTVAL,DMCB,TAKLFXBY+8                                            
         SPACE                                                                  
VREC245  LA    R2,PULDTRCH                                                      
         CLI   5(R2),0           DATE RECEIVED BY CLIENT                        
         BE    VREC250                                                          
         GOTO1 DTVAL,DMCB,TAKLDTRC                                              
         SPACE                                                                  
VREC250  LA    R2,PULDTCRH                                                      
         CLI   5(R2),0           DATE CHECK RELEASED                            
         BE    VREC255                                                          
         GOTO1 DTVAL,DMCB,TAKLDTRT                                              
         SPACE                                                                  
VREC255  CLI   PFAID,14          IF ADDING REQUEST                              
         BNE   VREC260                                                          
         MVC   TAKLRQBY,TGCTSTAF SAVE REQUESTER                                 
         MVC   TAKLRQDT,TGTODAY1 AND DATE                                       
         SPACE                                                                  
VREC260  CLI   PFAID,16          IF APPROVING REISSUE                           
         BNE   VREC270                                                          
         MVC   TAKLFXBY,TGCTSTAF SAVE APPROVER                                  
         MVC   TAKLFXDT,TGTODAY1 AND DATE                                       
VREC270  GOTO1 ADDELEM                                                          
         SPACE                                                                  
         GOTO1 ACTVIN,DMCB,(X'80',LSTCHGF)   ADD ACTIVITY ELEM                  
         SPACE                                                                  
         CLI   PFAID,14          ONLY PUT THE CHECK RECORD                      
         BE    VREC280           IF PF KEY 14, 15 OR 16 IS HIT                  
         CLI   PFAID,15                                                         
         BE    VREC280                                                          
         CLI   PFAID,16                                                         
         BNE   PRESSPF                                                          
         SPACE                                                                  
VREC280  MVC   AIO,AIO2                                                         
         BAS   RE,CHKNOIN                                                       
         BE    VREC290                                                          
         GOTO1 READCK,DMCB,(C'Y',0)                                             
VREC290  MVC   AIO,AIO1                                                         
         GOTO1 PUTREC                                                           
         GOTO1 ADDPTRS,DMCB,(X'22',PBLOCK)                                      
         SPACE                                                                  
         MVI   PFAID,0                                                          
         BAS   RE,PFINVIS                                                       
         BAS   RE,PFVIS                                                         
         B     XIT                                                              
         EJECT                                                                  
READCK   NTR1                                                                   
         MVC   DOREAD,0(R1)                                                     
         SPACE                                                                  
         MVC   SYSDIR,=CL8'CHKDIR'                                              
         MVC   SYSFIL,=CL8'CHKFIL'                                              
         SPACE                                                                  
         CLI   READCK,C'A'                                                      
         BE    READCK10                                                         
         SPACE                                                                  
         LA    R4,KEY              R4 = A(CHECK KEY)                            
         USING TLCKPD,R4                                                        
         XC    KEY,KEY                                                          
         MVI   TLCKPCD,TLCKCCDQ    RECORD CODE                                  
         MVC   TLCKCCHK,TGCHK      CHECK NUMBER                                 
         XC    TLCKCCHK,ALLFF      COMPLEMENTED                                 
         SPACE                                                                  
         CLI   DOREAD,C'N'                                                      
         BNE   *+10                                                             
         MVC   TGCHK,SVCHK                                                      
         SPACE                                                                  
READCK10 GOTO1 HIGH                READ FOR KEY                                 
         CLC   KEY(TLCKCBNK-TLCKPD),KEYSAVE                                     
         BNE   ERNOFND                                                          
         SPACE                                                                  
         CLI   DOREAD,C'N'                                                      
         BE    XIT                                                              
         SPACE                                                                  
         MVI   RDUPDATE,C'Y'                                                    
         OI    DMINBTS,X'80'                                                    
         GOTO1 GETREC              READ CHECK RECORD                            
         DROP  R4                                                               
         B     XIT                                                              
         SPACE 2                                                                
SETTAL   DS    0H                                                               
         MVC   SYSDIR,=CL8'TALDIR'                                              
         MVC   SYSFIL,=CL8'TALFIL'                                              
         BR    RE                                                               
         SPACE 2                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
*              ERROR MESSAGES                                                   
         SPACE                                                                  
ERRINV   MVI   ERROR,INVALID                                                    
         B     ERREND                                                           
         SPACE                                                                  
ERRDATE  MVC   MYMSGNO,=Y(ERCDETT)                                              
         OI    GENSTAT2,USGETTXT                                                
         MVI   BLOCK,0                                                          
         MVI   MYMTYP,GTMERR                                                    
         B     ERREND                                                           
         SPACE                                                                  
ERRMIS   MVI   ERROR,MISSING                                                    
         B     ERREND                                                           
         SPACE                                                                  
ERNOFND  MVI   ERROR,NOTFOUND                                                   
         B     ERREND                                                           
         SPACE                                                                  
ERRPFK   MVI   ERROR,ERINVPFK                                                   
         B     ERREND                                                           
         SPACE                                                                  
ERREND   NI    PROSTAT,X'FF'-PROWARN                                            
         SPACE                                                                  
         TM    PROSTAT,PROSDPF                                                  
         BZ    MESSEND                                                          
         SPACE                                                                  
ENTCHG   MVI   MYMSGNO1,107                                                     
         B     SPECMEND                                                         
         SPACE                                                                  
CHKCHG   MVI   MYMSGNO1,108                                                     
         NI    PROSTAT,X'FF'-PROWARN                                            
         B     SPECMEND                                                         
         SPACE                                                                  
PRESSPF  MVI   MYMSGNO1,106                                                     
         B     SPECMEND                                                         
         SPACE                                                                  
SPECMEND L     R2,EFHREC                                                        
         OI    GENSTAT2,USGETTXT                                                
         B     MESSEND                                                          
         SPACE                                                                  
MESSEND  GOTO1 EXIT,DMCB,0                                                      
         EJECT                                                                  
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
ALLFF    DC    8X'FF'                                                           
         SPACE                                                                  
         LTORG                                                                  
         SPACE 2                                                                
*              PF KEY TABLE                                                     
         SPACE                                                                  
PFTAB    DS    0C                  PF TABLE                                     
         DC    AL1(PF13X-*,13,PFTINT,(PF13X-PF13)/KEYLNQ,0)                     
         DC    CL3'U',CL8'CHECK  ',CL8'DISPLAY'                                 
PF13     DC    AL1(KEYTYTWA,L'PULDKAD-1),AL2(PULDKAD-T702FFD)                   
PF13X    EQU   *                                                                
         DC    AL1(PF14X-*,14,0,0,PFTRETRN)                                     
         DC    CL3' ',CL8'        ',CL8'        '                               
PF14X    EQU   *                                                                
         DC    AL1(PF15X-*,15,0,0,PFTRETRN)                                     
         DC    CL3' ',CL8'        ',CL8'        '                               
PF15X    EQU   *                                                                
         DC    AL1(PF16X-*,16,0,0,PFTRETRN)                                     
         DC    CL3' ',CL8'        ',CL8'        '                               
PF16X    EQU   *                                                                
         DC    X'FF'                                                            
         EJECT                                                                  
LISTTAB  DS    0H                                                               
         DC    AL1(LT13X-*,13,PFTINT,(LT13X-LT13)/KEYLNQ,0)                     
         DC    CL3'U',CL8'CHECK  ',CL8'DISPLAY'                                 
LT13     DC    AL1(KEYTYTWA,L'PULDKAD-1),AL2(PULDKAD-T702FFD)                   
LT13X    EQU   *                                                                
         DC    AL1(LT14X-*,14,0,0,PFTRETRN)                                     
         DC    CL3' ',CL8'        ',CL8'        '                               
LT14X    EQU   *                                                                
         DC    AL1(LT15X-*,15,0,0,PFTRETRN)                                     
         DC    CL3' ',CL8'        ',CL8'        '                               
LT15X    EQU   *                                                                
         DC    AL1(LT16X-*,16,0,0,PFTRETRN)                                     
         DC    CL3' ',CL8'        ',CL8'        '                               
LT16X    EQU   *                                                                
         DC    AL1(LT17X-*,17,PFTRPROG+PFTINT,0,0)                              
         DC    CL3' ',CL8' ',CL8' '                                             
LT17X    EQU   *                                                                
         DC    X'FF'                                                            
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE TASCREAD                                                       
         EJECT                                                                  
         ORG   PULWORK                                                          
DOREAD   DS    C                                                                
TEMPSTA1 DS    X                                                                
TEMPSTA2 DS    X                                                                
DSKADD   DS    XL4                                                              
AGYCONT  DS    CL(L'TAKLCONT)                                                   
AGYFAX#  DS    CL(L'TAKLFAX#)                                                   
TEMPREQ  DS    XL11                                                             
TEMPFAX  DS    XL11                                                             
SVCHK    DS    CL8                                                              
PLDTE    DS    XL3                                                              
LSTCHGF  DS    XL25                                                             
PROSTAT  DC    X'00'                                                            
PROWARN  EQU   X'80'                                                            
PROSDPF  EQU   X'40'                                                            
PBLOCK   DS    CL(L'TLDRREC*22+1)                                               
         EJECT                                                                  
* TAGENWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE TAGENWORKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
* TAGENFILE                                                                     
* TASYSEQUS                                                                     
* TASYSDSECT                                                                    
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* TAGENEQUS                                                                     
* FAWSSVRD                                                                      
*        PRINT OFF                                                              
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE TAGENEQUS                                                      
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE FAWSSVRD                                                       
*        PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007TAGENEA   03/10/10'                                      
         END                                                                    
