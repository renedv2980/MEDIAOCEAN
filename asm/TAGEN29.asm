*          DATA SET TAGEN29    AT LEVEL 161 AS OF 12/30/14                      
*PHASE T70229E,*                                                                
         TITLE 'T70229 - CHECK MAINTENANCE'                                     
T70229   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 TMPLNQ,T70229,R8,R7,R6                                           
         LR    R0,RC                                                            
         SPACE 1                                                                
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         EJECT                                                                  
* INITALIZE OVERLAY AND PASS CONTROL TO CORRECT MODE CONTROL ROUTINE.           
*                                                                               
         GOTOR FROMPULL            IF COMING FROM CHECK PULL                    
         BE    DISPKY              SET UP THE CHECK KEY/RECORD                  
*                                                                               
         CLI   PFAID,16            IF GOING TO CHECK PULL                       
         BNE   *+8                                                              
         MVI   THISLSEL,PULLC2M    SET THE GOING TO CHK PULL INDICATOR          
*                                                                               
         GOTO1 INITIAL,DMCB,PFTAB  INITIALIZE OVERLAY                           
         ST    R0,AEMPIO                                                        
         AHI   R0,EMPIOLNQ                                                      
         ST    R0,AW4IO                                                         
         AHI   R0,W4IOLNQ                                                       
         ST    R0,AUPPBLK                                                       
*                                                                               
         CLI   RECNUM,CH           CHECK RECORD                                 
         BE    INIT10                                                           
         CLI   RECNUM,DT           DTRACK RECORD                                
         BE    INIT10                                                           
         CLI   RECNUM,LT           LTRACK RECORD                                
         BNE   INIT20                                                           
INIT10   MVC   SCKSHED(7),=C'Pid Num'                                           
         OI    SCKSHEDH+6,X'80'                                                 
         MVC   SCKPHED(9),=C'Payer Pid'                                         
         OI    SCKPHEDH+6,X'80'                                                 
         B     INIT50                                                           
*                                                                               
INIT20   CLI   RECNUM,SS           SSN/TRANSFER RECORD                          
         BNE   INIT30                                                           
         MVC   SSTFHED(7),=C'Pid Num'                                           
         OI    SSTFHEDH+6,X'80'                                                 
         MVC   SSTTHED(7),=C'Pid Num'                                           
         OI    SSTTHEDH+6,X'80'                                                 
         B     INIT50                                                           
*                                                                               
INIT30   MVC   STRSHED(7),=C'Pid Num'    MUST BE TAX RECORD                     
         OI    STRSHEDH+6,X'80'                                                 
         MVC   STTSHED(7),=C'Pid Num'                                           
         OI    STTSHEDH+6,X'80'                                                 
*                                  SET A(SAVED PASSIVE POINTER BLOCK)           
INIT50   LH    RF,=AL2(YTDTAB-SYSD)                                             
         AR    RF,R9                                                            
         ST    RF,AYTDTAB                                                       
*                                                                               
         LH    RF,=AL2(ADJKEY-SYSD)                                             
         AR    RF,R9                                                            
         ST    RF,AADJKEY                                                       
*                                                                               
         LH    RF,=AL2(SVPBLK-SYSD)                                             
         AR    RF,R9                                                            
         ST    RF,ASVPBLK                                                       
*                                                                               
         ST    RD,SAVERD           SAVE RD FOR RETURNING TO GENCON              
*                                                                               
         BRAS  RE,SETCHK           SET SYSDIR/SYSFIL FOR CHECKS                 
*                                                                               
         CLI   MODE,DISPKEY        MODE DISPKEY                                 
         BE    DISPKY                                                           
         CLI   MODE,VALKEY         MODE VALKEY                                  
         BE    VALKY                                                            
         CLI   MODE,DISPREC        MODE DISPREC                                 
         BE    DISPRC                                                           
         CLI   MODE,VALREC         MODE VALREC                                  
         BE    VALRC                                                            
         CLI   MODE,PRINTREP       MODE PRINTREP (MULT REISSUE ONLY)            
         BE    MREISSUE                                                         
         CLI   MODE,XRECPUT        MODE XRECPUT (CHECK CHANGE ONLY)             
         BE    XRECPT                                                           
         B     XIT                                                              
         EJECT                                                                  
* ROUTINE TO HANDLE MODE DISPKEY.                                               
*                                                                               
DISPKY   DS    0H                                                               
         OI    SCKCHKH+6,X'80'     TRANSMIT CHECK NUMBER FIELD                  
         XC    SCKCHK,SCKCHK       PRE-CLEAR                                    
*                                                                               
         MVC   SCKCHK(3),=C'N/A'   INIT CHECK NUMBER TO N/A                     
*                                                                               
         L     R4,AIO              R4 = A(CHECK DETAILS ELEMENT)                
         MVI   ELCODE,TACDELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   DKX                                                              
         USING TACDD,R4                                                         
*                                                                               
         OC    TACDCHK,TACDCHK     IF THERE IS A CHECK NUMBER                   
         BZ    DKX                                                              
         MVC   SCKCHK,TACDCHK      DISPLAY IT                                   
         MVC   TGCHK,TACDCHK       AND SAVE IN GLOBAL STORAGE                   
*                                                                               
DKX      B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
* ROUTINE TO HANDLE MODE VALKEY.                                                
*                                                                               
VALKY    DS    0H                                                               
         CLI   ACTNUM,ACTDIS       ACTION DISPLAY                               
         BE    VKCHK                                                            
         CLI   ACTNUM,ACTCHA       ACTION CHANGE                                
         BE    VKCHK                                                            
         CLI   ACTNUM,ACTVOID      ACTION VOID                                  
         BE    VKCHK                                                            
         CLI   ACTNUM,ACTREIS      ACTION REISSUE                               
         BE    VKCHK                                                            
         CLI   ACTNUM,ACTISS       ACTION ISSUE                                 
         BE    VKCHK                                                            
         CLI   ACTNUM,ACTMREI      ACTION MULTIPLE REISSUE                      
         BE    VKMR                                                             
         CLI   ACTNUM,ACTREF       ACTION REFUND                                
         BE    VKTR                                                             
*                                                                               
         CLI   ACTNUM,ACTTRAN      ACTION TRANSFER                              
         BNE   VALKYX                                                           
         CLI   RECNUM,TA           RECORD TAX                                   
         BE    VKTT                                                             
         CLI   RECNUM,SS           RECORD SSN                                   
         BE    VKST                                                             
*                                                                               
VALKYX   DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
* ROUTINE TO HANDLE THE ACTIONS DISPLAY, SELECT, VOID, REISSUE, AND             
* ISSUE UNDER MODE VALKEY                                                       
*                                                                               
VKCHK    DS    0H                                                               
         BRAS  RE,SETTAL           SET SYSDIR/SYSFIL FOR NON-CHECKS             
         GOTO1 RECVAL,DMCB,TLSYCDQ,(X'20',0)  GET THE RECORD                    
         L     R4,AIO                                                           
         MVI   ELCODE,TASYELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TASYD,R4                                                         
         ZAP   SVECVT,TASYECVT     SAVE EURO CONVERSION RATE                    
         ZAP   SVCCVT,TASYCCVT     SAVE CANADIAN COVERSION RATE                 
         BRAS  RE,SETCHK           SET SYSDIR/SYSFIL FOR CHECKS                 
         DROP  R4                                                               
*                                                                               
         LA    R2,SCKCHKH          VALIDATE CHECK NUMBER                        
*                                                                               
         CLC   ACTNUM,TWALACT      IF ACTION HAS CHANGED                        
         BE    *+8                                                              
         NI    SCKCHKH+4,X'DF'     THEN SET CHECK NUMBER FIELD INVALID          
*                                                                               
         TM    SCKCHKH+4,X'20'     IF FIELD ALREADY VALID                       
         BO    VKX                 THEN RETURN                                  
*                                                                               
         MVI   ASKED,C'N'          SET USER HAS NOT BEEN ASKED TO PF19          
         XC    SCKMSG,SCKMSG       CLEAR RETURNED CHECK MESSAGE                 
         OI    SCKMSGH+6,X'80'                                                  
*                                                                               
         CLI   ACTNUM,ACTDIS       IF ACTION IS DISPLAY                         
         BE    *+12                                                             
         CLI   ACTNUM,ACTCHA       OR CHANGE                                    
         BNE   VK10                                                             
         CLI   5(R2),0             AND NOTHING INPUT                            
         BNE   VK10                                                             
         OC    TGCHK,TGCHK         AND THERE'S A GLOBAL NUMBER                  
         BZ    VK10                                                             
         MVC   8(L'TLCKCCHK,R2),TGCHK  USE IT                                   
         OI    6(R2),X'80'                                                      
         MVI   5(R2),L'TLCKCCHK                                                 
*                                                                               
VK10     GOTO1 ANY                 INPUT REQUIRED                               
         CLI   5(R2),L'TLCKCCHK    INSURE FULL CHECK NUMBER INPUT               
         BNE   ERRINV                                                           
         MVC   TGCHK,WORK          SAVE UNCOMPLEMENTED IN GLOBAL                
*                                                                               
         LA    R3,KEY              R3 = A(CHECK KEY)                            
         USING TLCKPD,R3                                                        
         XC    KEY,KEY                                                          
         MVI   TLCKPCD,TLCKCCDQ    RECORD CODE                                  
         MVC   TLCKCCHK,TGCHK      CHECK NUMBER                                 
         XC    TLCKCCHK,ALLFF      COMPLEMENTED                                 
*                                                                               
         GOTO1 HIGH                READ FOR KEY                                 
*                                                                               
VK20     L     RE,AADJKEY          SAVE KEY FOR ADJUSTMENTS                     
         MVC   0(L'TLDRREC,RE),KEY                                              
*                                                                               
         CLI   ACTNUM,ACTISS       IF ACTION ISSUE                              
         BNE   VK30                                                             
         CLC   KEY(TLCKCBNK-TLCKPD),KEYSAVE                                     
         BE    ERREXIST            THEN MUST NOT EXIST                          
         B     VK90                                                             
*                                  ELSE MUST EXIST                              
VK30     CLC   KEY(TLCKCBNK-TLCKPD),KEYSAVE                                     
         BNE   ERRNOFND                                                         
*                                                                               
         GOTO1 GETREC              READ RECORD                                  
*                                                                               
         BRAS  RE,LIMITCHK         CHECK LIMIT ACCESS                           
         BNE   ERRNOFND                                                         
*                                                                               
         BAS   RE,GETPDEL          R4 = A(PAYMENT DETAILS ELEMENT)              
         USING TAPDD,R4                                                         
*                                                                               
         TM    TAPDADJS,TAPDADVD   IF CHECK IS A VOID CHECK                     
         BZ    VK90                                                             
         CLI   ACTNUM,ACTDIS       AND ACTION IS DISPLAY                        
         BE    VK40                                                             
         CLI   ACTNUM,ACTCHA       OR CHANGE                                    
         BE    VK40                                                             
         CLI   ACTNUM,ACTVOID      OR VOID                                      
         BNE   VK90                                                             
VK40     GOTO1 SEQ                 THEN BUMP TO NORMAL CHECK                    
         CLC   KEY(TLCKCBNK-TLCKPD),KEYSAVE                                     
         BE    VK80                                                             
         MVC   KEY,KEYSAVE         IF CAN'T FIND (I.E. ORIGINAL PURGED)         
         GOTO1 HIGH                SHOW VOID                                    
         B     VK90                                                             
*                                                                               
VK80     CLI   ACTNUM,ACTVOID      IF FOUND AND ACTION IS VOID                  
         BE    VK20                GO BACK AND PROCESS                          
*                                                                               
VK90     OI    SCKCHKH+4,X'20'     SET CHECK NUMBER FIELD VALID                 
*                                                                               
         XC    TGSEQ,TGSEQ                                                      
VKX      B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
* ROUTINE TO HANDLE ACTION MULTIPLE REISSUE UNDER MODE VALKEY.                  
*                                                                               
VKMR     DS    0H                                                               
         LA    R2,SMRCHKH          R2 = A(CHECK NUMBERS FIELD)                  
         GOTO1 ANY                                                              
*                                                                               
         CLI   SCRSTAT,0           IF NOT FIRST TIME FOR SCREEN                 
         BNE   VKMR10                                                           
         TM    4(R2),X'20'         AND CHECK NUMBER FIELD VALID                 
         BZ    VKMR10                                                           
         TM    SMROPTH+4,X'20'     AND OPTIONS FIELD VALID                      
         BO    VKMR100             THEN SKIP TO PF19 TEST                       
*                                                                               
VKMR10   MVI   ASKED,C'N'          SET USER NOT YET ASKED TO PF19               
*                                                                               
         LA    R3,KEY              R3 = A(CHECK KEY)                            
         USING TLCKPD,R3                                                        
         XC    KEY,KEY                                                          
         MVI   TLCKPCD,TLCKCCDQ    RECORD CODE                                  
         MVC   TLCKCCHK,8(R2)      CHECK NUMBER                                 
         XC    TLCKCCHK,ALLFF      COMPLEMENTED                                 
*                                                                               
         GOTO1 HIGH                ERROR IF KEY NOT FOUND                       
         CLC   KEY(TLCKCBNK-TLCKPD),KEYSAVE                                     
         BNE   ERRNOFND                                                         
*                                                                               
         BAS   RE,VALVOID          MAKE SURE OK TO VOID                         
*                                                                               
         CLI   16(R2),C'-'         MUST HAVE '-' BETWEEN CHECK NUMBERS          
         BNE   ERRINV                                                           
*                                  BUILD SECOND CHECK NUMBER POINTER            
         XC    KEY,KEY                                                          
         MVI   TLCKPCD,TLCKCCDQ    RECORD CODE                                  
         MVC   TLCKCCHK,17(R2)     CHECK NUMBER                                 
         XC    TLCKCCHK,ALLFF      COMPLEMENTED                                 
*                                                                               
         GOTO1 HIGH                ERROR IF KEY NOT FOUND                       
         CLC   KEY(TLCKCBNK-TLCKPD),KEYSAVE                                     
         BNE   ERRNOFND                                                         
*                                                                               
         BAS   RE,VALVOID          MAKE SURE OK TO VOID                         
*                                                                               
         PACK  DUB,8(8,R2)         CONVERT STARTING CHECK NUMBER                
         CVB   R0,DUB                  TO BINARY                                
         ST    R0,STARTCHK                                                      
         PACK  DUB,17(8,R2)        CONVERT ENDING CHECK NUMBER                  
         CVB   R0,DUB                  TO BINARY                                
         ST    R0,ENDCHK                                                        
*                                                                               
         CLC   ENDCHK,STARTCHK     ENDING CHECK NUMBER MUST BE GREATER          
         BNH   ERRINV                  THAN STARTING CHECK NUMBER               
*                                                                               
         OI    4(R2),X'20'         SET CHECK NUMBERS FIELD VALID                
*                                                                               
         LA    R2,SMROPTH          R2 = A(OPTIONS FIELD)                        
*                                                                               
         MVI   OPTIONS,0           IF NO INPUT THEN CLEAR OPTIONS               
         CLI   5(R2),0                                                          
         BE    VKMR20                                                           
*                                                                               
         CLC   =C'URG',8(R2)       IF REQUST TO MARK URGENT                     
         BNE   VKMR15                                                           
         OI    OPTIONS,OPURGNT     SET URGENT OPTION                            
         B     VKMR20                                                           
                                                                                
VKMR15   OI    OPTIONS,OPTRACE     IF REQUEST TO TRACE                          
         CLC   =C'TRACE',8(R2)                                                  
         BNE   ERRINV              SET TRACE OPTION                             
*                                                                               
VKMR20   OI    4(R2),X'20'         SET TRACE FIELD VALID                        
*                                                                               
VKMR100  CLI   OFFLINE,C'Y'        GET OUT IF OFFLINE                           
         BE    VKMRX                                                            
*                                                                               
         LA    R2,SMRCHKH          R2 = A(CHECK NUMBERS FIELD)                  
*                                                                               
         CLI   ASKED,C'N'          IF USER HAS NOT BEEN ASKED TO PF19           
         BE    MSGPFMR             THEN GIVE HIT PFKEY MESSAGE                  
*                                                                               
         CLI   PFAID,PFADJ         ELSE IF CORRECT PFKEY NOT HIT                
         BNE   MSGPFMR             THEN GIVE HIT PFKEY MESSAGE                  
*                                                                               
VKMRX    B     XIT                                                              
         EJECT                                                                  
* ROUTINE TO HANDLE ACTION REFUND UNDER MODE VALKEY.                            
*                                                                               
VKTR     DS    0H                                                               
         CLI   SCRSTAT,0           IF NOT FIRST TIME FOR SCREEN                 
         BNE   VKTR10                                                           
*                                  AND ALL FIELDS VALID THEN RETURN             
         GOTO1 FLDVAL,DMCB,(X'40',STREMPH),(X'80',STRKENDH)                     
         BE    VKTRX                                                            
*                                                                               
VKTR10   MVI   ASKED,C'N'          SET USER NOT YET ASKED TO PF19               
*                                                                               
         LA    R2,STRCURRH         VALIDATE CURRENCY FIELD                      
         BAS   RE,VKCURR                                                        
*                                                                               
         BAS   RE,INITCHK          INITIALIZE CHECK RECORD                      
*                                                                               
         LA    R2,STREMPH          VALIDATE EMP                                 
         ST    R2,AEMPFLD                                                       
         BRAS  RE,VALEMP                                                        
*                                                                               
         LA    R2,STRSSNH          VALIDATE SSN                                 
         ST    R2,ASSNFLD                                                       
         LA    R2,STRSSNNH                                                      
         ST    R2,ASSNNFLD                                                      
         BRAS  RE,VALSSN                                                        
*                                                                               
         LA    R2,STRSOTH          VALIDATE STATE OF TAX                        
         BRAS  RE,VKSOT                                                         
*                                                                               
         LA    R2,STRCOTH          VALIDATE CITY OF TAX (OPTIONAL)              
         BRAS  RE,VKCOT                                                         
*                                                                               
         LA    R2,STRFRCH          VALIDATE FORCE TO LAST YEAR OPTION           
         CLI   5(R2),0                                                          
         BE    VKTR30                                                           
         CLI   8(R2),C'N'                                                       
         BE    VKTR30                                                           
         CLI   8(R2),C'Y'                                                       
         BNE   ERRINV                                                           
         TM    TGSYSTAT,TASYSLYR   TEST WHETHER FEATURE ENABLED                 
         BZ    ERRINV                                                           
         CLI   TGCUR,C'U'                                                       
         BNE   ERRCANL                                                          
VKTR30   MVC   LASTYEAR,8(R2)                                                   
*                                                                               
         GOTOR GETYTD              GET YTD AMOUNTS                              
*                                                                               
*                                  TRANSMIT ALL FIELDS                          
         GOTO1 FLDVAL,DMCB,(X'02',STRCFTAH),999                                 
*                                                                               
*                                  CLEAR AMOUNTS FIELDS                         
         GOTO1 FLDVAL,DMCB,(X'01',STRFTAXH),(X'80',999)                         
*                                                                               
         LA    RF,STRCFTAH         RF=A(CURRENT YTD AMT FIELDS)                 
         LA    RE,AFTAXFLD         RE=A(ADDRS FOR CURR YTD AMTS)                
         LA    R0,NAMTS1           R0=N'CURRENT YTD AMOUNT FIELDS               
         LA    R1,STRCFICH-STRCFTAH R1=LENGTH OF LINE                           
VKTR60   XC    8(L'STRCFIC,RF),8(RF) CLEAR THE FIELD                            
         ST    RF,0(RE)            AND SAVE THE ADDRESS                         
         AR    RF,R1               BUMP TO NEXT SCREEN FIELD                    
         LA    RE,4(RE)            BUMP TO NEXT SAVE ADDRESS                    
         BCT   R0,VKTR60           LOOP TO NEXT                                 
*                                                                               
         BAS   RE,DISPCURR         DISPLAY CURRENT YTD AMOUNTS                  
*                                                                               
*                                  SET VALIDITY BITS FOR ALL KEY FIELDS         
         GOTO1 FLDVAL,DMCB,(X'20',STREMPH),(X'80',STRKENDH)                     
*                                                                               
         LA    R2,STRFTAXH         GIVE PLEASE ENTER MESSAGE                    
         MVI   MYMSGNO1,2                                                       
         B     GENEND                                                           
*                                                                               
VKTRX    B     XIT                                                              
         EJECT                                                                  
* ROUTINE TO HANDLE RECORD TAX, ACTION TRANSFER, UNDER MODE VALKEY.             
*                                                                               
VKTT     DS    0H                                                               
         BRAS  RE,SETTAL           SET SYSDIR/SYSFIL FOR NON-CHECKS             
         GOTO1 RECVAL,DMCB,TLSYCDQ,(X'20',0)  GET THE RECORD                    
         L     R4,AIO                                                           
         MVI   ELCODE,TASYELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TASYD,R4                                                         
         ZAP   SVECVT,TASYECVT     SAVE EURO CONVERSION RATE                    
         ZAP   SVCCVT,TASYCCVT     SAVE CANADIAN COVERSION RATE                 
         BRAS  RE,SETCHK           SET SYSDIR/SYSFIL FOR CHECKS                 
         DROP  R4                                                               
*                                                                               
         CLI   SCRSTAT,0           IF NOT FIRST TIME FOR SCREEN                 
         BNE   VKTT10                                                           
*                                  IF ALL FIELDS VALID THEN RETURN              
         GOTO1 FLDVAL,DMCB,(X'40',STTEMPH),(X'80',STTKENDH)                     
         BE    VKTTX                                                            
*                                                                               
VKTT10   MVI   ASKED,C'N'          SET USER NOT YET ASKED TO PF19               
*                                                                               
         LA    R2,STTFRCH          VALIDATE FORCE TO LAST YEAR OPTION           
         CLI   5(R2),0                                                          
         BE    VKTT14                                                           
         CLI   8(R2),C'N'                                                       
         BE    VKTT14                                                           
         CLI   8(R2),C'Y'                                                       
         BNE   ERRINV                                                           
         TM    TGSYSTAT,TASYSLYR   TEST WHETHER FEATURE ENABLED                 
         BZ    ERRINV                                                           
VKTT14   MVC   LASTYEAR,8(R2)                                                   
*                                                                               
         MVI   CHKTRAN,C'N'        SET SINGLE CHECK TRANSFER TO FALSE           
*                                                                               
         LA    R2,STTCHKH          IF CHECK NUMBER SPECIFIED THEN SET           
         CLI   5(R2),0                 YTD AMOUNTS TO THE CHECK AMOUNTS         
         BE    VKTT20                                                           
         BAS   RE,VKNEWCHK                                                      
*                                                                               
         MVI   CHKTRAN,C'Y'        SET SINGLE CHECK TRANSFER TO TRUE            
         BRAS  RE,CHKCANL          ENSURE NOT CANADIAN ADJ TO LASTYEAR          
*                                                                               
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   STTSSN,SSNSPACE                                                  
         MVC   STTSSN(L'TGPID),TGPID                                            
         MVI   STTSSNH+5,6                                                      
VKTT16   OI    STTSSNH+6,X'80'                                                  
*                                                                               
         OI    STTSSNH+6,X'80'                                                  
         MVC   STTEMP,TGEMP                                                     
         OI    STTEMPH+6,X'80'                                                  
*                                                                               
         BRAS  RE,SETTAL           SET SYSDIR/SYSFIL FOR NON-CHECKS             
         MVC   AIO,AIO3            SET AIO TO AIO3                              
*                                                                               
*                                  DISPLAY SSN NAME                             
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'8A',TGSSN),STTSSNNH                       
         OI    STTSSNNH+6,X'80'                                                 
*                                                                               
         BRAS  RE,SETCHK           RESET SYSDIR/SYSFIL FOR CHECKS               
         MVC   AIO,AIO1            RESET AIO TO AIO1                            
*                                                                               
         MVI   STTCURR,C'N'                                                     
         CLI   TGCUR,C'C'          IF CANADA THEN DISPLAY 'Y'                   
         BNE   *+8                                                              
         MVI   STTCURR,C'Y'                                                     
         OI    STTCURRH+6,X'80'                                                 
*                                                                               
         MVC   OLDSOT,SOT          SET OLD SOT AND COT TO                       
         MVC   OLDSOTST,SOTST                                                   
         MVC   OLDCOT,COT              CHECK SOT AND COT                        
*                                                                               
         MVC   STTOSOT,SOT         DISPLAY OLD SOT AND COT                      
         OI    STTOSOTH+6,X'80'        ON SCREEN                                
         MVC   STTOCOT,COT                                                      
         OI    STTOCOTH+6,X'80'                                                 
         B     VKTT50                                                           
*                                                                               
VKTT20   BAS   RE,INITCHK          ELSE INITIALIZE CHECK RECORD                 
*                                                                               
         LA    R2,STTEMPH          VALIDATE EMPLOYER                            
         ST    R2,AEMPFLD                                                       
         BRAS  RE,VALEMP                                                        
*                                                                               
         LA    R2,STTSSNH          VALIDATE SSN                                 
         ST    R2,ASSNFLD                                                       
         LA    R2,STTSSNNH                                                      
         ST    R2,ASSNNFLD                                                      
         BRAS  RE,VALSSN                                                        
*                                                                               
         LA    R2,STTCURRH         VALIDATE CURRENCY FIELD                      
         BAS   RE,VKCURR                                                        
         BRAS  RE,CHKCANL          ENSURE NOT CANADIAN ADJ TO LASTYEAR          
*                                                                               
         LA    R2,STTONLYH         VALIDATE EARNINGS ONLY OPTION                
         CLI   5(R2),0                                                          
         BE    VKTT45                                                           
         CLI   8(R2),C'N'                                                       
         BE    VKTT45                                                           
         CLI   8(R2),C'Y'                                                       
         BNE   ERRINV                                                           
VKTT45   MVC   EARNONLY,8(R2)                                                   
*                                                                               
         LA    R2,STTSSNH                                                       
         CLI   EARNONLY,C'Y'       IF NOT TRANSFERRING EARNINGS ONLY            
         BE    VKTT48                                                           
         CLI   W4TYPE,TAW4TYIN     THEN W4 TYPE MUST BE INDIVIDUAL              
         BE    VKTT48                                                           
         CLI   W4TYPE,TAW4TYES                       OR ESTATE                  
         BNE   ERRINV                                                           
*                                                                               
VKTT48   LA    R2,STTOSOTH         VALIDATE OLD STATE OF TAX                    
         BRAS  RE,VKSOT                                                         
*                                                                               
         MVC   OLDSOT,SOT          SAVE OLD SOT IN LOCAL STORAGE                
         MVC   OLDSOTST,SOTST                                                   
*                                                                               
         LA    R2,STTOCOTH         VALIDATE OLD CITY OF TAX (OPTIONAL)          
         BRAS  RE,VKCOT                                                         
*                                                                               
         MVC   OLDCOT,COT          SAVE OLD COT IN LOCAL STORAGE                
*                                                                               
         GOTOR GETYTD              YTD AMTS FOR OLD SOT/COT                     
*                                                                               
VKTT50   LA    R2,STTNSOTH         VALIDATE NEW STATE OF TAX                    
         BRAS  RE,VKSOT                                                         
*                                                                               
         MVC   NEWSOT,SOT          SAVE NEW SOT IN LOCAL STORAGE                
         MVC   NEWSOTST,SOTST                                                   
*                                                                               
         LA    R2,STTNCOTH         VALIDATE NEW CITY OF TAX (OPTIONAL)          
         BRAS  RE,VKCOT                                                         
*                                                                               
         MVC   NEWCOT,COT          SAVE NEW COT IN LOCAL STORAGE                
*                                                                               
*                                  TRANSMIT ALL FIELDS                          
         GOTO1 FLDVAL,DMCB,(X'02',STTCERNH),999                                 
*                                                                               
*                                  CLEAR AMOUNTS FIELDS                         
         GOTO1 FLDVAL,DMCB,(X'01',STTOERNH),(X'80',999)                         
*                                                                               
         LA    RF,STTCERNH         RF=A(CURRENT YTD AMT FIELDS)                 
         LA    RE,AEARNFLD         RE=A(ADDRS FOR CURR YTD AMTS)                
         LA    R0,NAMTS3           R0=N'CURRENT YTD AMT FIELDS                  
         LA    R1,STTCNTXH-STTCERNH R1=LENGTH OF LINE                           
VKTT60   XC    8(L'STTCERN,RF),8(RF) CLEAR THE FIELD                            
         ST    RF,0(RE)            AND SAVE THE ADDRESS                         
         AR    RF,R1               BUMP TO NEXT SCREEN FIELD                    
         LA    RE,4(RE)            BUMP TO NEXT SAVE ADDRESS                    
         BCT   R0,VKTT60           LOOP TO NEXT                                 
*                                                                               
         XC    STTNERN,STTNERN     CLEAR NEW EARNINGS, OLD AND NEW NET          
         XC    STTNNTX,STTNNTX                                                  
         XC    STTONET,STTONET                                                  
         XC    STTNNET,STTNNET                                                  
*                                                                               
         MVC   SOT,OLDSOT          SET SOT AND COT FOR DISPCURR                 
         MVC   SOTST,OLDSOTST                                                   
         MVC   COT,OLDCOT                                                       
*                                                                               
         BAS   RE,DISPCURR         DISPLAY CURRENT YTD AMOUNTS                  
*                                                                               
*                                  SET VALIDITY BITS FOR ALL KEY FIELDS         
         GOTO1 FLDVAL,DMCB,(X'20',STTEMPH),(X'80',STTKENDH)                     
*                                                                               
         LA    R2,STTOERNH         GIVE PLEASE ENTER MESSAGE                    
         MVI   MYMSGNO1,2                                                       
         B     GENEND                                                           
*                                                                               
VKTTX    B     XIT                                                              
         EJECT                                                                  
* ROUTINE TO HANDLE RECORD SSN, ACTION TRANSFER, UNDER MODE VALKEY.             
*                                                                               
VKST     DS    0H                                                               
         CLI   SCRSTAT,0           IF NOT FIRST TIME FOR SCREEN                 
         BNE   VKST10                                                           
*                                  IF ALL FIELDS VALID THEN RETURN              
         GOTO1 FLDVAL,DMCB,(X'40',SSTFEMPH),(X'80',SSTKENDH)                    
         BE    VKSTX                                                            
*                                                                               
VKST10   MVI   ASKED,C'N'          SET USER NOT YET ASKED TO PF19               
*                                                                               
         LA    R2,SSTFRCH          VALIDATE FORCE TO LAST YEAR OPTION           
         CLI   5(R2),0                                                          
         BE    VKST14                                                           
         CLI   8(R2),C'N'                                                       
         BE    VKST14                                                           
         CLI   8(R2),C'Y'                                                       
         BNE   ERRINV                                                           
         TM    TGSYSTAT,TASYSLYR   TEST WHETHER FEATURE ENABLED                 
         BZ    ERRINV                                                           
VKST14   MVC   LASTYEAR,8(R2)                                                   
*                                                                               
         MVI   CHKTRAN,C'N'        SET SINGLE CHECK TRANSFER TO FALSE           
*                                                                               
         LA    R2,SSTCHKH          CHECK NUMBER FIELD MUST BE NON-EMPTY         
         CLI   5(R2),0                                                          
         BE    ERRMISS                                                          
*                                                                               
         CLC   =C'ALL',8(R2)       IF NOT 'ALL' CHECKS                          
         BE    VKST20                                                           
*                                                                               
         BAS   RE,VKNEWCHK         THEN SET YTD TABLE FROM CHECK RECORD         
*                                                                               
         MVI   CHKTRAN,C'Y'        SET SINGLE CHECK TRANSFER TO TRUE            
         BRAS  RE,CHKCANL          ENSURE NOT CANADIAN ADJ TO LASTYEAR          
*                                                                               
         MVC   SSTFSSN,TGSSN       DISPLAY 'FROM' SSN AND EMP ON SCREEN         
         MVC   SSTFEMP,TGEMP                                                    
         OI    SSTFEMPH+6,X'80'                                                 
*                                                                               
         BRAS  RE,SETTAL           SET SYSDIR/SYSFIL FOR NON-CHECKS             
         MVC   AIO,AIO3            SET AIO TO AIO3                              
*                                                                               
*                                  DISPLAY SSN NAME                             
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'8A',TGSSN),SSTFSNNH                       
         OI    SSTFSNNH+6,X'80'                                                 
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   SSTFSSN,SSNSPACE                                                 
         MVC   SSTFSSN(L'TGPID),TGPID                                           
         MVI   SSTFSSNH+5,6                                                     
VKST16   OI    SSTFSSNH+6,X'80'                                                 
*                                                                               
         BRAS  RE,SETCHK           RESET SYSDIR/SYSFIL FOR CHECKS               
         MVC   AIO,AIO1            RESET AIO TO AIO1                            
*                                                                               
         MVI   SSTCURR,C'N'        CURRENCY                                     
         CLI   TGCUR,C'C'          IF CANADA THEN DISPLAY 'Y'                   
         BNE   *+8                                                              
         MVI   SSTCURR,C'Y'                                                     
         OI    SSTCURRH+6,X'80'                                                 
         B     VKST50                                                           
*                                                                               
VKST20   BAS   RE,INITCHK          ELSE INITIALIZE CHECK RECORD                 
*                                                                               
         LA    RF,SSTFEMPH         VALIDATE 'FROM' EMPLOYER                     
         ST    RF,AEMPFLD                                                       
         BRAS  RE,VALEMP                                                        
*                                                                               
         LA    R2,SSTFSSNH         VALIDATE 'FROM' SSN                          
         ST    R2,ASSNFLD                                                       
         LA    R2,SSTFSNNH                                                      
         ST    R2,ASSNNFLD                                                      
         BRAS  RE,VALSSN                                                        
*                                                                               
         LA    R2,SSTCURRH         VALIDATE CURRENCY FIELD                      
         BAS   RE,VKCURR                                                        
         BRAS  RE,CHKCANL          ENSURE NOT CANADIAN ADJ TO LASTYEAR          
*                                                                               
         GOTOR GETYTD              THEN GET YTD TABLE                           
*                                                                               
VKST50   XC    TGEMP,TGEMP         CLEAR GLOBAL EMP AND SSN                     
         XC    TGSSN,TGSSN                                                      
*                                                                               
         LA    RF,SSTTEMPH         VALIDATE 'TO' EMPLOYER                       
         ST    RF,AEMPFLD                                                       
         BRAS  RE,VALEMP                                                        
*                                                                               
         LA    R2,SSTTEMPH                                                      
         CLC   =C'P+',SSTFEMP      IF P+, BOTH MUST BE P+                       
         BNE   VKST55                                                           
         CLC   =C'P+',SSTTEMP                                                   
         BNE   ERRINV                                                           
         B     VKST60                                                           
*                                                                               
VKST55   CLC   =C'P+',SSTTEMP                                                   
         BE    ERRINV                                                           
*                                                                               
VKST60   LA    R2,SSTTSSNH         VALIDATE 'TO' SSN                            
         ST    R2,ASSNFLD                                                       
         LA    R2,SSTTSNNH                                                      
         ST    R2,ASSNNFLD                                                      
         BRAS  RE,VALSSN                                                        
*                                                                               
         BAS   RE,DISPTAB          DISPLAY YTD TABLE                            
*                                                                               
*                                  TRANSMIT ALL FIELDS                          
         GOTO1 FLDVAL,DMCB,(X'02',SSTAMTSH),999                                 
*                                                                               
*                                  SET VALIDITY BITS FOR ALL KEY FIELDS         
         GOTO1 FLDVAL,DMCB,(X'20',SSTFEMPH),(X'80',SSTKENDH)                    
*                                                                               
VKSTX    B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE VALIDATES THE CURRENCY FIELD POINTED TO BY R2 AND SAVES          
* THE CURRENCY SETTING (U/C) IN LOCAL STORAGE.                                  
*                                                                               
VKCURR   DS   0H                                                                
         MVI   TGCUR,C'U'          IF FIELD BLANK OR HAS A 'N' OR 'U'           
         CLI   5(R2),0                 IN IT THEN SET CURRENCY TO 'U'           
         BE    VKCRX                                                            
         CLI   8(R2),C'N'                                                       
         BE    VKCRX                                                            
         CLI   8(R2),C'U'                                                       
         BE    VKCRX                                                            
*                                                                               
         MVI   TGCUR,C'E'          ELSE IF FIELD HAS AN 'E'                     
         CLI   8(R2),C'E'              IN IT THEN SET CURRENCY TO 'E'           
         BE    VKCRX                                                            
*                                                                               
         MVI   TGCUR,C'C'          ELSE FIELD MUST HAVE A 'Y' OR 'C'            
         CLI   8(R2),C'Y'              IN IT AND SET CURRENCY TO 'C'            
         BE    VKCRX                                                            
         CLI   8(R2),C'C'                                                       
         BNE   ERRINV                                                           
*                                                                               
VKCRX    BR    RE                                                               
         SPACE 2                                                                
* THIS ROUTINE VALIDATES THE CHECK NUMBER FIELD POINTED TO BY R2.               
* FIRST, THE ROUTINE DETERMINES IF THE FIELD HAS A VALID CHECK NUMBER.          
* IT THEN READS IN THE CHECK RECORD AND EXTRACTS ITS SOT AND COT.               
* LASTLY IT EXTRACTS THE EARNINGS AND TAX AMOUNTS INTO LOCAL STORAGE.           
* THESE AMOUNTS WILL BE CONSIDERED YTD AMOUNTS AND WILL BE DISPLAYED            
* UNDER THE HEADING OF 'CURRENT'.  ADDITIONALLY, IT BUILDS A MOCK               
* YTDTAB WITH ENTRIES FOR THE TAXABLE UNITS.  SSN TRANSFERS WILL USE            
* THIS TABLE TO TRANSFER THE WITHHOLDINGS.                                      
*                                                                               
VKNEWCHK NTR1                                                                   
*                                  PRE-CLEAR YTD AMOUNTS                        
         XC    MYDAMNTS(MYDAMNTL),MYDAMNTS                                      
*                                                                               
         XC    TGCHK,TGCHK         CLEAR CHECK NUMBER IN GLOBAL STORAGE         
*                                                                               
         L     R5,AYTDTAB           R5 = A(YTD TABLE)                           
         USING YTDD,R5                                                          
*                                                                               
         CLI   5(R2),L'TLCKCCHK    INSURE FULL CHECK NUMBER INPUT               
         BNE   ERRINV                                                           
*                                                                               
         MVC   TGCHK,8(R2)         SAVE UNCOMPLEMENTED IN GLOBAL                
*                                                                               
         LA    R3,KEY              BUILD CHECK KEY                              
         USING TLCKPD,R3                                                        
         XC    KEY,KEY                                                          
         MVI   TLCKPCD,TLCKCCDQ                                                 
         MVC   TLCKCCHK,8(R2)                                                   
         XC    TLCKCCHK,ALLFF                                                   
*                                                                               
         GOTO1 HIGH                READ FOR KEY                                 
*                                                                               
*                                  ERROR IF CHECK KEY NOT FOUND                 
         CLC   KEY(TLCKCBNK-TLCKPD),KEYSAVE                                     
         BNE   ERRNOFND                                                         
*                                                                               
         L     RE,AADJKEY          SAVE KEY FOR ADJUSTMENTS                     
         MVC   0(L'TLDRREC,RE),KEY                                              
*                                                                               
         GOTO1 GETREC              READ RECORD                                  
*                                                                               
         BAS   RE,GETCDEL          IF CHECK IS BEFORE 1991                      
         USING TACDD,R4                                                         
         CLC   TACDDTE,=X'901231'                                               
         BH    VKNC10                                                           
*                                                                               
         BRAS  RE,EXTRSC           EXTRACT SOW, COW, SOR, AND COR               
         BRAS  RE,TAXABLE          AND SET TAXABLE BITS IN CW ELEMENTS          
*                                                                               
VKNC10   L     R3,AIO              SAVE SSN IN GLOBAL STORAGE                   
         USING TLCKD,R3                                                         
         MVC   TGSSN,TLCKSSN                                                    
*                                                                               
         LR    R4,R3               IF TAX ID ELEMENT EXISTS                     
         MVI   ELCODE,TATIELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   *+10                                                             
         USING TATID,R4                                                         
         MVC   TGSSN,TATIID        SAVE CORPORATION ID AS SSN                   
*                                                                               
         BAS   RE,GETPDEL          R4 = A(PAYMENT DETAILS ELEMENT)              
         USING TAPDD,R4                                                         
*                                                                               
         TM    TAPDADJS,TAPDADVD   ERROR IF CHECK IS A VOID CHECK               
         BO    ERRVOI                                                           
*                                                                               
         MVC   MYDMDED,TAPDMDED    SET YTD MISC DEDUCTIONS                      
         MVC   MYDDUES,TAPDDUES            UNION DUES                           
         MVC   MYDREXP,TAPDREXP            REIMBURSED EXPENSES                  
*                                                                               
         MVC   TGEMP,TAPDEMP       SAVE EMPLOYER IN GLOBAL STORAGE              
         MVC   W4TYPE,TAPDW4TY          PAYMENT TYPE IN W4TYPE                  
*                                                                               
         MVI   TGCUR,C'C'          IF CANDIAN DOLLAR BIT SET THEN               
         TM    TAPDSTAT,TAPDSCAN       SET CURRENCY TO 'C'                      
         BO    VKNC15                                                           
         MVI   TGCUR,C'E'          IF EUROS BIT SET THEN                        
         TM    TAPDPST2,TAPDPEUR       SET CURRENCY TO 'E'                      
         BO    VKNC15                                                           
         MVI   TGCUR,C'U'          ELSE SET CURRENCY TO 'U'                     
*                                                                               
VKNC15   BAS   RE,GETCDEL          R4 = A(CHECK DETAILS ELEMENT)                
         USING TACDD,R4                                                         
*                                                                               
         MVC   MYDEARN,TACDEARN    MYDEARN = TACDEARN                           
*                                                                               
         L     RF,TACDNTAX         MYDNTAX = TACDNTAX - MYDREXP                 
         S     RF,MYDREXP                                                       
         ST    RF,MYDNTAX                                                       
*                                                                               
         MVC   MYDNET,TACDNET      MYDNET = TACDNET                             
*                                                                               
         USING TAODD,R4                                                         
         MVI   ELCODE,TAODELQ      LOOK FOR OTHER DEDUCT ELEMENTS               
         GOTO1 GETL,DMCB,(1,=AL1(TAODTYPF))                                     
         BNE   *+14                                                             
         L     R4,TGELEM           THEN MYDFTAX = TAODAMT                       
         MVC   MYDFTAX,TAODAMT                                                  
*                                                                               
         GOTO1 GETL,DMCB,(1,=AL1(TAODTYPM))                                     
         BNE   *+14                                                             
         L     R4,TGELEM           THEN MYDMPR = TAODAMT                        
         MVC   MYDMPR,TAODAMT                                                   
*                                                                               
         GOTO1 GETL,DMCB,(1,=AL1(TAODTYPP))                                     
         BNE   VKNC20                                                           
         L     R4,TGELEM           THEN MYDMPR = MPR + PERM CHAR                
         L     RF,MYDMPR                                                        
         L     RE,TAODAMT                                                       
         AR    RF,RE                                                            
         ST    RF,MYDMPR                                                        
*                                                                               
VKNC20   XC    SOT,SOT                                                          
         XC    COT,COT                                                          
*                                                                               
         USING TACWD,R4                                                         
         L     R4,AIO              R4 = A(FIRST CHECK WITHHOLD ELEMENT)         
         MVI   ELCODE,TACWELQ                                                   
         BRAS  RE,GETEL                                                         
         BE    VKNC50                                                           
         OC    MYDFTAX,MYDFTAX     DID'T FIND ANY TACW ELEMENTS                 
         BNZ   VKNC30                                                           
         OC    MYDMPR,MYDMPR                                                    
         BZ    VKNCX                                                            
VKNC30   XC    ELEM,ELEM           BUILD DUMMY TACW ELEMENT                     
         LA    R4,ELEM                                                          
         MVI   TACWEL,TACWELQ                                                   
         MVI   TACWLEN,TACWLN2Q                                                 
         MVC   TACWUNIT,=C'FD '                                                 
         MVC   TACWTAX,MYDFTAX                                                  
*                                                                               
         BRAS  RE,ADDTAB           ADD YTDTAB ENTRY FOR THIS UNIT               
         LA    R5,YTDLNQ(R5)       BUMP R5 TO NEXT ENTRY TO ADD                 
         B     VKNCX               DONE IF NONE FOUND                           
*                                                                               
VKNC50   CLC   TACWUNIT,=C'FD '    IF UNIT IS FEDERAL                           
         BNE   VKNC60                                                           
         AF    MYDFTAX,TACWTAX     THEN ADD TACWTAX TO MYDFTAX                  
         MVC   MYDFICA,TACWFICA    AND SET MYDFICA = TACWFICA                   
         B     VKNC100                                                          
*                                                                               
VKNC60   CLC   TACWUNIT,=C'CN '    ELSE IF UNIT IS CANADA                       
         BNE   *+14                                                             
         MVC   MYDCTX,TACWTAX      SET MYDCTX = TACWTAX                         
         B     VKNC100                                                          
*                                                                               
         TM    TACWSTAT,TACWSTAX   ELSE SKIP UNIT IF NOT TAXABLE                
         BZ    VKNC110                                                          
*                                                                               
         CLI   TACWUNIT+2,C' '     IF UNIT IS A STATE                           
         BH    VKNC70                                                           
         L     RE,MYDSTAX                                                       
         LTR   RE,RE                                                            
         BZ    *+10                                                             
         MVC   SOT,=C'MLT'         SET MULTI-STATE                              
         A     RE,TACWTAX                                                       
         ST    RE,MYDSTAX          SET STATE TAX                                
         L     RE,MYDSUI                                                        
         LTR   RE,RE                                                            
         BZ    *+10                                                             
         MVC   SOT,=C'MLT'         SET MULTI-STATE                              
         A     RE,TACWSUI                                                       
         ST    RE,MYDSUI           SET SUI                                      
         L     RE,MYDSDI                                                        
         LTR   RE,RE                                                            
         BZ    *+10                                                             
         MVC   SOT,=C'MLT'         SET MULTI-STATE                              
         A     RE,TACWSDI                                                       
         ST    RE,MYDSDI           SET SDI                                      
         CLI   TACWLEN,TACWLN2Q                                                 
         BL    VKNC61                                                           
         L     RE,MYDFLI                                                        
         LTR   RE,RE                                                            
         BZ    *+10                                                             
         MVC   SOT,=C'MLT'         SET MULTI-STATE                              
         A     RE,TACWSFLI                                                      
         ST    RE,MYDFLI           SET FLI                                      
*                                                                               
VKNC61   CLC   SOT,=C'MLT'         IF THIS IS FIRST STATE TAXED                 
         BE    VKNC100                                                          
         MVC   SOT,TACWUNIT        EXTRACT SOT                                  
         B     VKNC100                                                          
*                                                                               
VKNC70   L     RE,MYDLTAX                                                       
         LTR   RE,RE                                                            
         BZ    *+10                                                             
         MVC   COT,=C'MLT'         SET MULTI-STATE                              
         A     RE,TACWTAX                                                       
         ST    RE,MYDLTAX          SET LOCAL TAX                                
*                                                                               
         CLC   COT,=C'MLT'         IF THIS IS FIRST CITY TAXED                  
         BE    VKNC100                                                          
         MVC   COT,TACWUNIT        EXTRACT COT                                  
*                                                                               
VKNC100  BRAS  RE,ADDTAB           ADD YTDTAB ENTRY FOR THIS UNIT               
         LA    R5,YTDLNQ(R5)       BUMP R5 TO NEXT ENTRY TO ADD                 
*                                                                               
VKNC110  MVI   ELCODE,TACWELQ                                                   
         BRAS  RE,NEXTEL           GET NEXT CW ELEM AND LOOP BACK               
         BE    VKNC50                                                           
*                                                                               
VKNCX    MVI   0(R5),0             MARK END OF YTD TABLE                        
*                                                                               
         LR    R3,R5               R3 = NUMBER OF YTD TABLE ENTRIES             
         L     RF,AYTDTAB                                                       
         SR    R3,RF                                                            
         XR    R2,R2                                                            
         D     R2,=A(YTDLNQ)                                                    
*                                  SORT YTD TABLE                               
         GOTO1 XSORT,DMCB,AYTDTAB,(R3),YTDLNQ,YTDSRTLQ,YTDSORT-YTDD             
         B     XIT                                                              
         EJECT                                                                  
* ROUTINE TO DISPLAY THE CURRENT YTD AMOUNTS TO THE SCREEN.                     
*                                                                               
DISPCURR NTR1                                                                   
         CLI   ACTNUM,ACTTRAN      IF ACTION TRANSFER                           
         BNE   DC10                                                             
         L     R2,AEARNFLD         THEN DISPLAY YTD TAXABLE EARNINGS            
         MVC   FULL,MYDEARN                                                     
         BAS   RE,EDIT10F                                                       
*                                                                               
         L     R2,ANTAXFLD         YTD NON-TAXABLE EARNINGS                     
         MVC   FULL,MYDNTAX                                                     
         OC    MYDEARN,MYDEARN     IF TAXABLE EARNINGS, DON'T DISPLAY           
         BNZ   *+8                 NON-TAXABLE EARNINGS                         
         BAS   RE,EDIT10F                                                       
*                                                                               
         L     R2,AREXPFLD         YTD REIMBURSED EXPENSES                      
         MVC   FULL,MYDREXP                                                     
         BAS   RE,EDIT10                                                        
*                                                                               
         CLI   EARNONLY,C'Y'       IF TRANSFERRING EARNINGS ONLY                
         BE    DC100               THEN SKIP TO NET                             
*                                                                               
DC10     MVI   ELCODE,TACWELQ      IF FEDERAL ELEMENT EXISTS                    
         GOTO1 GETL,DMCB,(3,=C'FD ')                                            
         BNE   DC20                                                             
         L     R2,AFTAXFLD         DISPLAY YTD FEDERAL TAX                      
         MVC   FULL,MYDFTAX                                                     
         BAS   RE,EDIT10                                                        
         L     R2,AFICAFLD         DISPLAY YTD FICA                             
         MVC   FULL,MYDFICA                                                     
         BAS   RE,EDIT10                                                        
*                                                                               
DC20     L     R2,ASTAXFLD         DISPLAY YTD STATE TAX                        
         MVC   FULL,MYDSTAX                                                     
         BAS   RE,EDIT10                                                        
         L     R2,ASDIFLD          DISPLAY YTD SDI                              
         MVC   FULL,MYDSDI                                                      
         BAS   RE,EDIT10                                                        
*                                                                               
         CLC   SOT,=C'CN '         IF STATE OF TAX NOT CANADA                   
         BE    DC30                                                             
         L     R2,ASUIFLD          DISPLAY YTD SUI                              
         MVC   FULL,MYDSUI                                                      
         BAS   RE,EDIT10                                                        
*                                                                               
DC30     OC    COT,COT             IF CITY OF TAX SPECIFIED                     
         BZ    DC40                                                             
         L     R2,ALTAXFLD         DISPLAY YTD LOCAL TAX                        
         MVC   FULL,MYDLTAX                                                     
         BAS   RE,EDIT10                                                        
*                                                                               
DC40     L     R2,AMDEDFLD         DISPLAY YTD MISCELLANEOUS DEDUCTIONS         
         MVC   FULL,MYDMDED                                                     
         BAS   RE,EDIT10                                                        
         L     R2,ADUESFLD         DISPLAY YTD UNION DUES                       
         MVC   FULL,MYDDUES                                                     
         BAS   RE,EDIT10                                                        
         L     R2,AMPRFLD          DISPLAY YTD MPR FUND                         
         MVC   FULL,MYDMPR                                                      
         BAS   RE,EDIT10                                                        
         L     R2,AFLIFLD          DISPLAY YTD FLI                              
         MVC   FULL,MYDFLI                                                      
         BAS   RE,EDIT10                                                        
         L     R2,ACTXFLD          DISPLAY YTD CANADA TAX                       
         MVC   FULL,MYDCTX                                                      
         BAS   RE,EDIT10                                                        
*                                                                               
DC100    CLI   ACTNUM,ACTTRAN      IF ACTION TRANSFER                           
         BNE   DCX                                                              
         CLI   CHKTRAN,C'Y'        AND SINGLE CHECK                             
         BNE   DCX                                                              
         L     R2,ANETFLD          DISPLAY NET                                  
         MVC   FULL,MYDNET                                                      
         BAS   RE,EDIT10F                                                       
*                                                                               
DCX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE DISPLAYS THE YTDTAB ENTRIES TO THE SSN TRANSFER SCREEN.          
*                                                                               
DISPTAB  NTR1                                                                   
*                                  CLEAR ALL FIELDS                             
         GOTO1 FLDVAL,DMCB,(X'01',SSTAMTSH),SSTAMTXH                            
*                                                                               
         LA    R2,SSTNTAXH         DISPLAY NON-TAXABLE EARNINGS                 
         MVC   FULL,MYDNTAX                                                     
         BAS   RE,EDIT10                                                        
*                                                                               
         LA    R2,SSTAMTSH         R2 = A(FIRST DISPLAY LINE)                   
         L     R5,AYTDTAB          R5 = A(FIRST YTDTAB ENTRY)                   
         USING YTDD,R5                                                          
*                                                                               
DTB10    CLI   0(R5),0             WHILE NOT END OF YTDTAB                      
         BE    DTBX                                                             
*                                                                               
         MVC   8(3,R2),YTDUNIT     DISPLAY UNIT                                 
         BAS   RE,BUMP                                                          
*                                                                               
         MVC   FULL,YTDEARN        DISPLAY YTD EARNINGS                         
         CLC   TGEMP,=C'P+ '                                                    
         BNE   DTB12                                                            
         L     RF,YTDEARN                                                       
         A     RF,YTDTXRE                                                       
         ST    RF,FULL                                                          
DTB12    BAS   RE,EDIT10F                                                       
*                                                                               
         BAS   RE,BUMP                                                          
         MVC   FULL,YTDTAX         DISPLAY YTD TAX                              
         BAS   RE,EDIT10                                                        
         BAS   RE,BUMP                                                          
*                                                                               
         MVC   FULL,YTDFICA        DISPLAY YTD FICA/SUI                         
         BAS   RE,EDIT10                                                        
         BAS   RE,BUMP                                                          
*                                                                               
         MVC   FULL,YTDREXP        DISPLAY YTD REXP/SDI                         
         CLC   TGEMP,=C'P+ '                                                    
         BNE   DTB14                                                            
         CLC   YTDUNIT,=C'FD '                                                  
         BNE   DTB14                                                            
         MVC   FULL,YTDNTRE                                                     
DTB14    BAS   RE,EDIT10                                                        
         BAS   RE,BUMP             BUMP R2 TO NEXT LINE                         
*                                                                               
         LA    RE,SSTNTAXH         IF WE'RE POINTING TO NON-TAXABLE FLD         
         CR    R2,RE                                                            
         BNE   *+8                                                              
         BAS   RE,BUMP             NEED TO BUMP R2 ANOTHER TIME                 
*                                                                               
         LA    R5,YTDLNQ(R5)       BUMP R5 TO NEXT YTDTAB ENTRY                 
         B     DTB10               LOOP BACK                                    
*                                                                               
DTBX     B     XIT                                                              
         EJECT                                                                  
* ROUTINE TO HANDLE MODE DISPREC.                                               
*                                                                               
DISPRC   DS    0H                                                               
         GOTOR DISPLAY            DISPLAY RECORD IN AIO1                        
         B     XIT                                                              
         SPACE 3                                                                
* ROUTINE TO HANDLE MODE VALREC.                                                
*                                                                               
VALRC    DS    0H                                                               
         CLI   ACTNUM,ACTVOID      ACTION VOID                                  
         BE    VOID                                                             
         CLI   ACTNUM,ACTREIS      ACTION REISSUE                               
         BE    REISSUE                                                          
         CLI   ACTNUM,ACTISS       ACTION ISSUE                                 
         BE    ISSUE                                                            
         CLI   ACTNUM,ACTREF       ACTION REFUND                                
         BE    REFUND                                                           
*                                                                               
         CLI   ACTNUM,ACTTRAN      ACTION TRANSFER                              
         BNE   VRC10                                                            
         CLI   RECNUM,TA           RECORD TAX                                   
         BE    TAXTRAN                                                          
         CLI   RECNUM,SS           RECORD SSN                                   
         BE    SSNTRAN                                                          
*                                                                               
VRC10    CLI   ACTNUM,ACTCHA       IF ACTION CHANGE                             
         BE    *+12                                                             
         CLI   ACTNUM,ACTSEL       OR SELECT                                    
         BNE   VRCX                                                             
         CLI   RECNUM,DT           AND RECORD IS DTRACK                         
         BE    ERRACT              THEN DON'T ALLOW                             
         L     R3,ASVPBLK                                                       
         XC    0(255,R3),0(R3)                                                  
         GOTO1 SAVPTRS,DMCB,(R3)   ELSE SAVE OLD PASSIVE POINTERS               
*                                                                               
         BAS   RE,CHANGE           AND CHANGE RECORD                            
VRCX     B     XIT                                                              
         EJECT                                                                  
* ROUTINE TO HANDLE MODE XRECPUT.                                               
*                                                                               
XRECPT   DS    0H                                                               
         CLI   ACTNUM,ACTCHA       IF NOT ACTION CHANGE                         
         BE    *+12                                                             
         CLI   ACTNUM,ACTSEL       OR SELECT THEN GET OUT                       
         BNE   XRPX                                                             
*                                  ELSE ADD NEW PASSIVE POINTERS                
         GOTO1 ADDPTRS,DMCB,(8,ASVPBLK),AUPPBLK                                 
*                                                                               
         CLI   ACTNUM,ACTCHA       IF ACTION CHANGE                             
         BNE   XRPX                                                             
         GOTOR DISPLAY             THEN RE-DISPLAY RECORD                       
XRPX     B     XIT                                                              
         EJECT                                                                  
* ROUTINE TO HANDLE ACTION VOID UNDER MODE VALREC.                              
*                                                                               
VOID     DS    0H                                                               
         LA    R2,SCKCHKH          POINT TO CHECK NUMBER FOR MESSAGES           
*                                                                               
         L     RE,AADJKEY          RESTORE KEY FOR ADJUSTMENTS                  
         MVC   KEY,0(RE)                                                        
*                                                                               
         BAS   RE,VALVOID          MAKE SURE OK TO VOID                         
*                                                                               
         OI    SCKCHKH+4,X'20'     SET CHECK NUMBER FIELD TO VALID              
*                                                                               
         CLI   ASKED,C'N'          IF USER HAS NOT BEEN ASKED TO PF19           
         BE    VOID10                                                           
         CLI   PFAID,PFADJ         OR CORRECT PFKEY NOT HIT                     
         BE    VOID20                                                           
*                                                                               
VOID10   GOTOR DISPLAY             THEN DISPLAY REC                             
         B     MSGPFVR             AND ASK TO HIT PFKEY                         
*                                                                               
VOID20   MVI   LASTYEAR,C'N'       TURN OFF FORCE TO LAST YEAR SWITCH           
*                                                                               
         BAS   RE,GETCDEL          R4 = A(CHECK DETAILS ELEMENT)                
         USING TACDD,R4                                                         
         CLC   =C'LASTYEAR',SCKCHKD  IF SPEC. KEYWORD IN CHECK DATE FLD         
         BNE   VOID30                                                           
         CLC   TACDDTE(1),TGTODAY1 AND CHECK DATE FROM PREVIOUS YEAR            
         BNL   ERRCHKD                                                          
         TM    TGSYSTAT,TASYSLYR   TEST WHETHER FEATURE ENABLED                 
         BZ    ERRCHKD                                                          
         MVI   LASTYEAR,C'Y'       SET TO FORCE TO LAST YEAR                    
*                                                                               
VOID30   BAS   RE,GETPDEL          R4=A(PAYMENT DETAILS ELEMENT)                
         USING TAPDD,R4                                                         
         TM    TAPDSTAT,TAPDSCAN                                                
         BZ    VOID40                                                           
         CLI   LASTYEAR,C'Y'                                                    
         BNE   VOID40                                                           
         LA    R2,SCKCHKDH         CANADIAN ADJ TO LASTYEAR NOT ALLOWED         
         B     ERRCANL                                                          
*                                                                               
VOID40   BAS   RE,BLDVR            GO BUILD VOID/REISSUE CHECKS                 
*                                                                               
         GOTOR DISPLAY             DISPLAY RECORD TO USER                       
*                                                                               
         NI    SCKCHKH+4,X'DF'     FORCE VALKEY NEXT TRANSACTION                
*                                                                               
         B     THEEND              RETURN TO GENCON WITH SUCCESS                
         DROP  R4                                                               
         EJECT                                                                  
* ROUTINE TO HANDLE ACTION REISSUE UNDER MODE VALREC.                           
*                                                                               
REISSUE  DS    0H                                                               
         MVI   LASTYEAR,C'N'       TURN OFF FORCE TO LAST YEAR SWITCH           
*                                                                               
         LA    R2,SCKCHKH          POINT TO CHECK NUMBER FOR MESSAGES           
*                                                                               
         L     RE,AADJKEY          RESTORE KEY FOR ADJUSTMENTS                  
         MVC   KEY,0(RE)                                                        
*                                                                               
         BAS   RE,VALVOID          MAKE SURE OK TO VOID                         
*                                                                               
         OI    SCKCHKH+4,X'20'     SET CHECK NUMBER FIELD TO VALID              
*                                                                               
         CLI   ASKED,C'N'          IF USER HAS NOT BEEN ASKED TO PF19           
         BNE   REIS10                                                           
         GOTOR DISPLAY             THEN DISPLAY RECORD,                         
*                                  SET ALL VALIDITY BITS,                       
         GOTO1 FLDVAL,DMCB,(X'20',SCKSSNH),(X'80',999)                          
         B     MSGPFVR             AND ASK TO HIT PFKEY                         
*                                                                               
REIS10   GOTO1 FLDVAL,DMCB,(X'40',SCKSSNH),(X'80',999)                          
         BE    REIS20              NO FIELDS HAVE CHANGED                       
         TM    SCKNCDEH+4,X'20'    IF AGENT FIELD CHANGED                       
         BO    REIS20                                                           
         XC    TGAGT,TGAGT         THEN RE-VALIDATE                             
         BAS   RE,VALNCDE                                                       
         OI    SCKNCDEH+4,X'20'                                                 
         B     MSGPFVR                                                          
*                                                                               
REIS20   CLI   PFAID,PFADJ         IF CORRECT PFKEY WAS NOT HIT                 
         BNE   MSGPFVR             THEN ASK AGAIN                               
*                                                                               
         BAS   RE,BLDVR            ELSE GO BUILD VOID/REISSUE CHECKS            
*                                                                               
         GOTOR DISPLAY             DISPLAY RECORD TO USER                       
*                                                                               
         NI    SCKCHKH+4,X'DF'     FORCE VALKEY NEXT TRANSACTION                
         B     THEEND              RETURN TO GENCON WITH SUCCESS                
         EJECT                                                                  
* ROUTINE TO HANDLE ACTION ISSUE UNDER MODE VALREC.                             
*                                                                               
ISSUE    DS    0H                                                               
         MVI   LASTYEAR,C'N'       TURN OFF FORCE TO LAST YEAR SWITCH           
*                                                                               
*                                  IF ANY FIELD HAS CHANGED                     
         GOTO1 FLDVAL,DMCB,(X'40',SCKSSNH),(X'80',999)                          
         BE    *+8                                                              
         MVI   ASKED,C'N'          THEN SET TO ASK USER TO PF19                 
*                                                                               
         BAS   RE,BLDIS            BUILD ISSUE CHECK RECORD                     
*                                                                               
*                                  SET ALL FIELDS VALID                         
         GOTO1 FLDVAL,DMCB,(X'20',SCKSSNH),(X'80',999)                          
*                                                                               
         CLI   ASKED,C'N'          IF USER HAS NOT BEEN ASKED TO PF19           
         BE    MSGPFIS             THEN GIVE HIT PFKEY MESSAGE                  
*                                                                               
         CLI   PFAID,PFADJ         ELSE IF CORRECT PFKEY NOT HIT                
         BNE   MSGPFIS             THEN GIVE HIT PFKEY MESSAGE                  
*                                                                               
         BRAS  RE,GETINV           GET NEXT ADJUSTMENT INVOICE NUMBER           
*                                                                               
         BAS   RE,ADDCHK           ADD ISSUE CHECK                              
         BAS   RE,ADDINV           ADD INVOICE RECORD FOR ISSUE CHECK           
         GOTOR UPDW4               UPDATE W4 W/TRUSTEE AMT DEDUCTED             
         GOTOR DISPLAY             DISPLAY RECORD BACK                          
*                                                                               
         NI    SCKCHKH+4,X'DF'     SET TO RE-VAL CHECK NUMBER NEXT TIME         
         B     THEEND              RETURN TO GENCON WITH SUCCESS                
         EJECT                                                                  
* ROUTINE TO HANDLE ACTION MULTIPLE REISSUE UNDER MODE VALREC.                  
*                                                                               
MREISSUE DS    0H                                                               
         MVI   LASTYEAR,C'N'       TURN OFF FORCE TO LAST YEAR SWITCH           
*                                                                               
         L     R5,ASPOOLD          R5=SPOOL DSECT                               
         USING SPOOLD,R5                                                        
*                                                                               
         LA    R3,KEY              R3 = A(CHECK KEY)                            
         USING TLCKPD,R3                                                        
*                                                                               
         XR    R2,R2               CLEAR ERROR COUNTER                          
*                                                                               
         MVC   CHKCNT,STARTCHK     CHKCNT = FIRST CHECK NUMBER                  
*                                                                               
*                                  BUILD CHECK NUMBER POINTER                   
MR10     XC    KEY,KEY                                                          
         MVI   TLCKPCD,TLCKCCDQ             RECORD CODE                         
         EDIT  (4,CHKCNT),(8,TGCHK),FILL=0  CHECK NUMBER (SAVE UNCOMP)          
         MVC   TLCKCCHK,TGCHK                                                   
         XC    TLCKCCHK,ALLFF               COMPLEMENTED                        
*                                                                               
         GOTO1 HIGH                IF KEY NOT FOUND                             
         CLC   KEY(TLCKCBNK-TLCKPD),KEYSAVE                                     
         BNE   MR50                THEN PRINT NOT FOUND ERROR MESSAGE           
*                                                                               
         GOTO1 GETREC              GET CHECK RECORD INTO AIO1                   
*                                                                               
         BAS   RE,GETCDEL          R4 = A(CHECK DETAILS ELEMENT)                
         USING TACDD,R4                                                         
         OC    TACDCSH,TACDCSH     ERROR IF CHECK HAS BEEN CASHED               
         BNZ   MR60                                                             
         TM    TACDSTAT,TACDSVOI   ERROR IF CHECK HAS BEEN VOIDED               
         BO    MR70                                                             
*                                                                               
         BAS   RE,GETPDEL          R4 = A(PAYMENT DETAILS ELEMENT)              
         USING TAPDD,R4                                                         
         TM    TAPDADJS,TAPDADVD   ERROR IF CHECK IS A VOID CHECK               
         BO    MR70                                                             
*                                                                               
         BAS   RE,GETCAEL          R4 = A(CAST DETAILS ELEMENT)                 
         USING TACAD,R4                                                         
         XC    TGAGT,TGAGT         SAVE AGENT CODE IN GLOBAL STORAGE            
         OC    TACANCDE,TACANCDE                                                
         BZ    MR40                                                             
         GOTO1 TRNSAGT,DMCB,(X'40',TACANCDE),TGAGT                              
*                                                                               
MR40     L     RE,AADJKEY          SAVE KEY FOR ADJUSTMENTS                     
         MVC   0(L'TLDRREC,RE),KEY                                              
*                                                                               
         BAS   RE,BLDVR            MAKE VOID AND REISSUE                        
         B     MR90                                                             
*                                  SET ERROR TO 'NOT FOUND'                     
MR50     MVC   P+22(9),=C'NOT FOUND'                                            
         B     MR80                                                             
*                                  SET ERROR TO 'ALREADY CASHED'                
MR60     MVC   P+22(14),=C'ALREADY CASHED'                                      
         B     MR80                                                             
*                                  SET ERROR TO 'ALREADY VOIDED'                
MR70     MVC   P+22(14),=C'ALREADY VOIDED'                                      
*                                                                               
*                                  PRINT CHECK NUMBER AND ERROR                 
MR80     MVC   P(12),=C'CHECK NUMBER'                                           
         MVC   P+13(8),TGCHK                                                    
         BRAS  RE,CALLSPL                                                       
*                                                                               
         LA    R2,1(R2)            INCREMENT ERROR COUNTER                      
*                                                                               
MR90     L     RF,CHKCNT           INCREMENT CHECK NUMBER                       
         AHI   RF,1                                                             
         ST    RF,CHKCNT                                                        
*                                                                               
         CLC   CHKCNT,ENDCHK       IF NOT DONE WITH LAST CHECK                  
         BNH   MR10                THEN LOOP BACK                               
*                                                                               
         L     R4,CHKCNT           R4 = NUMBER OF CHECKS MREISSUED              
         S     R4,STARTCHK                                                      
         SR    R4,R2                                                            
*                                  PRINT 'NNN CHECKS VOIDED...'                 
         EDIT  (R4),(10,P),ALIGN=LEFT,ZERO=NOBLANK                              
         LR    R1,R0                                                            
         LA    RF,P+1(R1)                                                       
         MVC   0(26,RF),=C'CHECKS VOIDED AND REISSUED'                          
         BRAS  RE,CALLSPL                                                       
*                                  PRINT 'NNN CHECKS IN ERROR'                  
         EDIT  (R2),(10,P),ALIGN=LEFT,ZERO=NOBLANK                              
         LR    R1,R0                                                            
         LA    RF,P+1(R1)                                                       
         MVC   0(15,RF),=C'CHECKS IN ERROR'                                     
         BRAS  RE,CALLSPL                                                       
*                                                                               
MRX      B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
* ROUTINE TO HANDLE ACTION REFUND UNDER MODE VALREC.                            
*                                                                               
REFUND   DS    0H                                                               
*                                  IF ANY FIELD HAS CHANGED                     
         GOTO1 FLDVAL,DMCB,(X'40',STRFTAXH),(X'80',999)                         
         BE    *+8                                                              
         MVI   ASKED,C'N'          THEN SET TO ASK USER TO PF19                 
*                                                                               
         BAS   RE,BLDTR            BUILD TAX REFUND CHECK                       
*                                                                               
*                                  SET ALL FIELDS VALID                         
         GOTO1 FLDVAL,DMCB,(X'20',STRFTAXH),(X'80',999)                         
*                                                                               
         CLI   ASKED,C'N'          IF USER HAS NOT BEEN ASKED TO PF19           
         BE    MSGPFTR             THEN GIVE HIT PFKEY MESSAGE                  
*                                                                               
         CLI   PFAID,PFADJ         ELSE IF CORRECT PFKEY NOT HIT                
         BNE   MSGPFTR             THEN GIVE HIT PFKEY MESSAGE                  
*                                                                               
         BRAS  RE,GETINV           GET NEXT ADJUSTMENT INVOICE NUMBER           
*                                                                               
         BAS   RE,ADDCHK           ADD TAX REFUND CHECK                         
*                                                                               
         BRAS  RE,INITINV          INITIALIZE INVOICE RECORD                    
*                                                                               
         BAS   RE,ADDINV           ADD INVOICE RECORD FOR REFUND CHECK          
*                                                                               
TRX      B     THEEND              RETURN TO GENCON WITH SUCCESS                
         EJECT                                                                  
* ROUTINE TO HANDLE RECORD TAX, ACTION TRANSFER, UNDER MODE VALREC.             
*                                                                               
TAXTRAN  DS    0H                                                               
*                                  IF ANY FIELD HAS CHANGED                     
         GOTO1 FLDVAL,DMCB,(X'40',STTOERNH),(X'80',999)                         
         BE    *+8                                                              
         MVI   ASKED,C'N'          THEN SET TO ASK USER TO PF19                 
*                                                                               
         BAS   RE,BLDTT            BUILD TAX TRANSFER BLOCK                     
*                                                                               
*                                  SET ALL FIELDS VALID                         
         GOTO1 FLDVAL,DMCB,(X'20',STTOERNH),(X'80',999)                         
*                                                                               
         CLI   ASKED,C'N'          IF USER HAS NOT BEEN ASKED TO PF19           
         BE    MSGPFTT             THEN GIVE HIT PFKEY MESSAGE                  
*                                                                               
         CLC   NEWSOT,=C'RET'      IF RETURN, DON'T ADD DEBIT CHECK             
         BE    TTX10                                                            
*                                                                               
         CLI   PFAID,PFADJ         ELSE IF CORRECT PFKEY NOT HIT                
         BNE   MSGPFTT             THEN GIVE HIT PFKEY MESSAGE                  
*                                                                               
         BAS   RE,ADJW4TY          ADJUST W4 TYPE IF NECESSARY                  
*                                                                               
         BRAS  RE,GETINV           GET NEXT ADJUSTMENT INVOICE NUMBER           
*                                                                               
         MVC   WORK,NEWSOT                                                      
         BRAS  RE,CHGCAUNT         CHANGE CAST UNIT IN CHECK                    
                                                                                
         BAS   RE,ADDCHK           ADD DEBIT CHECK                              
*                                                                               
         BRAS  RE,INITINV          INITIALIZE INVOICE RECORD                    
*                                                                               
         BAS   RE,ADDINV           ADD INVOICE RECORD FOR DEBIT CHECK           
*                                                                               
TTX10    L     RE,AIO              MOVE CREDIT CHECK INTO AIO                   
         LHI   RF,4000                                                          
         L     R0,ATIA                                                          
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                                                               
         BAS   RE,ADJW4TY          ADJUST W4 TYPE IF NECESSARY                  
*                                                                               
         BRAS  RE,GETINV           GET NEXT ADJUSTMENT INVOICE NUMBER           
*                                                                               
         MVC   WORK,OLDSOT                                                      
         BRAS  RE,CHGCAUNT         CHANGE CAST UNIT IN CHECK                    
                                                                                
         BAS   RE,ADDCHK           ADD CREDIT CHECK                             
*                                                                               
         BRAS  RE,INITINV          INITIALIZE INVOICE RECORD                    
*                                                                               
         BAS   RE,ADDINV           ADD INVOICE RECORD FOR CREDIT CHECK          
*                                                                               
TTX      B     THEEND              RETURN TO GENCON WITH SUCCESS                
         EJECT                                                                  
* ROUTINE TO HANDLE RECORD SSN, ACTION TRANSFER, UNDER MODE VALREC.             
*                                                                               
SSNTRAN  DS    0H                                                               
         CLI   ASKED,C'N'          IF USER HAS NOT BEEN ASKED TO PF19           
         BE    MSGPFTT             THEN GIVE HIT PFKEY MESSAGE                  
*                                                                               
         CLI   PFAID,PFADJ         ELSE IF CORRECT PFKEY NOT HIT                
         BNE   MSGPFTT             THEN GIVE HIT PFKEY MESSAGE                  
*                                                                               
         BAS   RE,BLDST            ADD ALL SSN TRANFER RECORDS                  
*                                                                               
STX      B     THEEND              RETURN TO GENCON WITH SUCCESS                
         EJECT                                                                  
* THIS ROUTINE BUILDS AND ADDS THE VOID AND, IF APPLICABLE, THE REISSUE         
* CHECK RECORDS ALONG WITH THE ADJUSTMENT INVOICES.                             
*                                                                               
BLDVR    NTR1                                                                   
         L     RE,AADJKEY          RESTORE KEY FOR ADJUSTMENTS                  
         MVC   KEY,0(RE)                                                        
*                                                                               
         MVI   RDUPDATE,C'Y'       READ CHECK RECORD FOR UPDATE                 
         GOTO1 GETREC                                                           
*                                                                               
         GOTOR DOTRACE,DMCB,AIO,0,=C'OLD CK',6                                  
*                                                                               
         BAS   RE,GETCDEL          R4 = A(CHECK DETAILS ELEMENT)                
         USING TACDD,R4                                                         
         OI    TACDSTAT,TACDSVOI     MARK CHECK HAS BEEN VOIDED                 
         NI    TACDSTAT,ALL-TACDSSTA TURN OFF STALE INDICATOR                   
*                                                                               
         MVC   ORIGDTE,TACDDTE     SAVE ORIGINAL CHECK DATE/SEQNUM              
         MVC   ORIGSEQ,TACDSEQ                                                  
*                                                                               
         GOTO1 PUTREC              WRITE CHECK RECORD BACK                      
*                                                                               
         GOTOR DOTRACE,DMCB,AIO,0,=C'VOIDED CK',9                               
*                                                                               
*                                  CLEAR VOIDED,STALE,TRUST, & LIEN             
         NI    TACDSTAT,ALL-TACDSVOI                                            
         NI    TACDSTAT,ALL-TACDSSTA                                            
         NI    TACDSTAT,ALL-TACDSTRS                                            
         NI    TACDSTAT,ALL-TACDSLIN                                            
                                                                                
         BRAS  RE,DELRCHK                                                       
                                                                                
         L     R3,AIO              R3 = A(CHECK KEY)                            
         USING TLCKD,R3                                                         
*                                                                               
         L     R4,AIO              IF ORIGINAL AGY/INV ELEMENT EXISTS           
         MVI   ELCODE,TAOIELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   BVR10                                                            
         USING TAOID,R4                                                         
*                                                                               
         MVC   OIELEM,0(R4)        THEN SAVE IT IN OIELEM                       
*                                                                               
         OC    TAOIAGY,TAOIAGY     IF ORIGINAL AGENCY IS NOT ZEROS              
         BZ    BVR20                                                            
*                                                                               
         MVC   TLCKAGY,TAOIAGY     SAVE ORIGINAL AGY/INV IN CHECK KEY           
         MVC   TLCKINV,TAOIINV                                                  
         B     BVR20                                                            
*                                                                               
BVR10    LA    R4,OIELEM           ELSE BUILD ORIGINAL AGENCY/INVOICE           
         USING TAOID,R4                ELEMENT IN OIELEM                        
         XC    OIELEM,OIELEM                                                    
         MVI   TAOIEL,TAOIELQ                                                   
         MVI   TAOILEN,TAOILNQ                                                  
         MVC   TAOIAGY,TLCKAGY     GET ORIGINAL AGY/INV FROM CHECK KEY          
         MVC   TAOIINV,TLCKINV                                                  
*                                                                               
BVR20    LA    R4,OKELEM           BUILD ORIGINAL CHECK DATE/SEQUENCE           
         USING TAOKD,R4                NUMBER ELEMENT IN OKELEM                 
         XC    OKELEM,OKELEM                                                    
         MVI   TAOKEL,TAOKELQ                                                   
         MVI   TAOKLEN,TAOKLNQ                                                  
         MVC   TAOKDTE,ORIGDTE     CHECK DATE AND SEQUENCE NUMBER FROM          
         MVC   TAOKSEQ,ORIGSEQ         ORIGINAL CHECK                           
*                                                                               
         MVI   ELCODE,TAOIELQ      REMOVE OLD ORIGINAL AGY/INV ELEMENT          
         GOTO1 REMELEM                                                          
*                                  ADD OI ELEM TO CHECK RECORD                  
         MVC   ELEM(TAOILNQ),OIELEM                                             
         GOTO1 ADDELEM                                                          
*                                                                               
         MVI   ELCODE,TAOKELQ      REMOVE OLD ORIGINAL CHK DTE ELEMENT          
         GOTO1 REMELEM                                                          
*                                  ADD OK ELEM TO CHECK RECORD                  
         MVC   ELEM(TAOKLNQ),OKELEM                                             
         GOTO1 ADDELEM                                                          
*                                                                               
         MVI   ELCODE,TADWELQ      REMOVE ANY DUE COMPANY WITHHOLDING           
         GOTO1 REMELEM                 ELEMENTS                                 
*                                                                               
         XC    FULL,FULL                                                        
         L     R4,AIO              IF LIEN WITHHOLDING ELEMENT EXISTS           
         MVI   ELCODE,TALWELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   BVR24                                                            
         USING TALWD,R4                                                         
         MVC   FULL,TALWREC        SAVE AMOUNT RECOVERED                        
         GOTO1 REMELEM             NOW REMOVE THE ELEMENT                       
*                                                                               
BVR24    MVI   ELCODE,TAODELQ      IF W4 TRUSTEE DEDUCTION ELEMENT              
         GOTO1 GETL,DMCB,(1,=AL1(TAODTYPT))                                     
         BNE   BVR26                                                            
         L     R4,TGELEM                                                        
         USING TAODD,R4                                                         
         L     RF,FULL                                                          
         A     RF,TAODAMT                                                       
         ST    RF,FULL             SAVE AMOUNT OF DEDUCTION                     
         GOTO1 DELL,DMCB,(1,=AL1(TAODTYPT))                                     
*                                                                               
BVR26    BAS   RE,GETPDEL          R4 = A(PAYMENT DETAILS ELEMENT)              
         USING TAPDD,R4                                                         
         XC    TAPDPNH,TAPDPNH     CLEAR P&H, SUBJECT TO P&H, AND H&W           
         XC    TAPDSPNH,TAPDSPNH                                                
         XC    TAPDHNW,TAPDHNW                                                  
         L     R1,TAPDMDED                                                      
         A     R1,FULL             ADD LIEN&W4 TRUSTEE AMT TO MISC DED          
         ST    R1,TAPDMDED                                                      
*                                                                               
         BRAS  RE,SETTAL           SET SYSDIR/SYSFIL FOR NON-CHECKS             
         MVC   AIO,AIO2            READ ORIGINAL INVOICE REC INTO AIO2          
         MVC   TGAGY,TLCKAGY                                                    
         MVC   TGINV,TLCKINV                                                    
         XC    TGINV,ALLFF                                                      
         GOTO1 RECVAL,DMCB,TLINCDQ,(X'A0',TGINV)                                
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO1                                                         
         BRAS  RE,SETCHK           RESET SYSDIR/SYSFIL FOR CHECKS               
*                                                                               
         BAS   RE,GETCDEL          IF CHECK IS BEFORE 1991                      
         USING TACDD,R4                                                         
         CLC   TACDDTE,=X'901231'                                               
         BH    BVR30                                                            
*                                                                               
         BRAS  RE,EXTRSC           EXTRACT SOW, COW, SOR, AND COR               
         BRAS  RE,TAXABLE          AND SET TAXABLE BITS IN CW ELEMENTS          
***********************************************************************         
* THIS SECTION HANDLES STUFF THAT IS SPECIFIC TO THE VOID CHECK.      *         
***********************************************************************         
BVR30    MVI   ADJTYPE,ACTVOID     SET ADJUSTMENT TYPE TO VOID                  
*                                                                               
         BRAS  RE,GETINV           GET NEXT INVOICE NUMBER FOR ADJ              
*                                                                               
         GOTO1 REVERSE             REVERSE AMOUNTS IN CHECK RECORD              
*                                                                               
         BAS   RE,GETCDEL          R4 = A(CHECK DETAILS ELEMENT)                
         USING TACDD,R4                                                         
         XC    TACDDTE,TACDDTE     CLEAR CHECK AND RUN DATES                    
         XC    TACDRUN,TACDRUN                                                  
         CLI   LASTYEAR,C'Y'       IF FORCE TO LAST YEAR IS SET                 
         BNE   *+8                                                              
         OI    TACDSTAT,TACDSFRC   SET CORRES. BIT IN CHECK DTLS. EL.           
*                                                                               
*                                  ADD CHECK COMMENT IF ANY                     
         GOTO1 NAMIN,DMCB,TACMELQ,(X'80',SCKCCOMH),TACMTYPC                     
*                                                                               
         BAS   RE,ADDCHK           ADD VOID CHECK                               
*                                                                               
         GOTOR DOTRACE,DMCB,AIO,0,=C'VOID CK',7                                 
*                                                                               
         BAS   RE,ADDINV           ADD INVOICE RECORD FOR VOID CHECK            
*                                                                               
         GOTOR DOTRACE,DMCB,AIO2,0,=C'VOID INV',8                               
***********************************************************************         
* THE REST OF THIS ROUTINE HANDLES THE REISSUE CHECK THAT FOLLOWS THE *         
* VOID CHECK IF THE ACTION IS REISSUE.                                *         
***********************************************************************         
         CLI   ACTNUM,ACTVOID      IF ACTION NOT VOID                           
         BE    BVRX                                                             
*                                                                               
         MVI   ADJTYPE,ACTREIS     SET ADJUSTMENT TYPE TO REISSUE               
*                                                                               
         BRAS  RE,GETINV           GET NEXT INVOICE NUMBER FOR ADJ              
*                                                                               
         GOTO1 REVERSE             REVERSE AMOUNTS IN CHECK RECORD BACK         
*                                                                               
         BAS   RE,GETCDEL          R4 = A(CHECK DETAILS ELEMENT)                
         USING TACDD,R4                                                         
         XC    TACDBNK,TACDBNK     CLEAR BANK AND CHECK NUMBER                  
         XC    TACDCHK,TACDCHK                                                  
*                                                                               
         MVI   ELCODE,TAKPELQ      REMOVE CHECK STOP ELEMENT                    
         GOTO1 REMELEM                                                          
         MVI   ELCODE,TAKLELQ      REMOVE CHECK PULL ELEMENT                    
         GOTO1 REMELEM                                                          
*                                                                               
         BAS   RE,GETCAEL          R4 = A(CAST DETAILS ELEMENT)                 
         USING TACAD,R4                                                         
         XC    TACANCDE,TACANCDE   ASSUME THERE'S NO AGENT                      
         OC    TGAGT,TGAGT                                                      
         BZ    BVR40                                                            
         GOTO1 TRNSAGT,DMCB,(X'80',TGAGT),TACANCDE                              
*                                                                               
BVR40    BAS   RE,ADDCHK           ADD REISSUE CHECK                            
*                                                                               
         GOTOR DOTRACE,DMCB,AIO,0,=C'REISSUE CK',10                             
*                                                                               
         BAS   RE,ADDINV           ADD INVOICE RECORD FOR REISSUE CHECK         
*                                                                               
         GOTOR DOTRACE,DMCB,AIO2,0,=C'REISSUE INV',11                           
*                                                                               
BVRX     B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
* THIS ROUTINE VALIDATES THE SCREEN DATA AND BUILDS THE ISSUE (MANUAL)          
* CHECK RECORD WITH EVERYTHING EXCEPT THE ADJUSMENT AGY/INV.                    
*                                                                               
BLDIS    NTR1                                                                   
         BAS   RE,INITCHK          INITIALIZE CHECK RECORD                      
*                                                                               
         XC    TOTDED,TOTDED       CLEAR TOTAL DEDUCTIONS                       
         MVI   STAXFLAG,C'N'       INIT STATE TAX FLAG TO NO                    
*                                                                               
         LA    R2,SCKSSNH          SET A(SSN AND SSN NAME FIELDS)               
         ST    R2,ASSNFLD              FOR CALL TO VALSSN                       
         LA    R2,SCKSSNNH                                                      
         ST    R2,ASSNNFLD                                                      
*                                                                               
         BAS   RE,VALAI            VALIDATE AGENCY/INVOICE                      
         BRAS  RE,VALSSN                    SSN                                 
*                                                                               
         MVC   CHKSSN,TGSSN        SAVE CHECK ISSUE SSN IN LOCAL                
*                                                                               
         BAS   RE,VALPTYP          VALIDATE PAYMENT TYPE                        
         BAS   RE,VALCD                     CHECK DETAILS                       
         BRAS  RE,VALCORP                   CORPORATION ID                      
         BAS   RE,VALNCDE                   AGENT                               
         BAS   RE,VALCAST                   CATEGORY AND CAST SORT KEY          
         BAS   RE,VALAMNT                   CHECK AMOUNTS                       
         BAS   RE,VALMDED                   MISCELLANEOUS DEDUCTIONS            
         BAS   RE,VALDUES                   UNION DUES                          
         BAS   RE,VALWIRE                   WIRE TRANSFER                       
         BAS   RE,VALDIRCT                  DIRECT DEPOSIT                      
         BAS   RE,SETUNLO          SET UNION/LOCAL INFO                         
*                                                                               
         CLI   PAYTYPE,TAW4TYCO    IF PAYMENT TYPE IS CORPORATION               
         BE    BIS1                                                             
         CLI   PAYTYPE,TAW4TYCA    OR CANADIAN                                  
         BE    BIS1                                                             
         CLI   PAYTYPE,TAW4TYTR    OR TRUSTEE                                   
         BE    BIS1                                                             
         CLI   PAYTYPE,TAW4TYFO    OR FOREIGNER                                 
         BNE   BIS5                                                             
BIS1     MVI   ELCODE,TACWELQ      THEN REMOVE ANY WITHHOLDING ELEMENTS         
         GOTO1 REMELEM                                                          
*                                                                               
         CLI   PAYTYPE,TAW4TYTR    IF TRUSTEE                                   
         BE    BIS10               NO TAXES CAN BE TAKEN                        
*                                                                               
         CLI   PAYTYPE,TAW4TYCA    IF CANADIAN                                  
         BE    BIS2                                                             
         CLI   PAYTYPE,TAW4TYCO    OR CORP                                      
         BNE   BIS3                                                             
BIS2     BAS   RE,VALCAN           THEN VALIDATE CANADIAN AND CALIF TAX         
         B     BIS10                                                            
BIS3     BAS   RE,VALFOR           ELSE VALIDATE FOREIGN FEDERAL TAX            
BIS4     CLC   =C'CA',SCKSOW       CALIFORNIA?                                  
         BE    *+14                                                             
         CLC   =C'NC',SCKSOW       NORTH CAROLINA?                              
         BNE   BIS10                                                            
         BAS   RE,VALSOW                    STATE OF WORK                       
         B     BIS10                                                            
*                                                                               
BIS5     L     R4,AIO              IF WITHHOLDING ELEMENT EXISTS                
         MVI   ELCODE,TACWELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   BIS10                                                            
*                                                                               
         BAS   RE,VALFED           VALIDATE FEDERAL WITHHOLDINGS                
*                                  KEEP VALCOR CALL BEFORE VALCOW               
         BAS   RE,VALSOR                    STATE OF RESIDENCE                  
         BRAS  RE,VALCOR                    CITY OF RESIDENCE                   
*                                                                               
         BAS   RE,VALSOW                    STATE OF WORK                       
         BAS   RE,VALCOW                    CITY OF WORK                        
         BAS   RE,VALOTH                    OTHER STATE                         
*                                                                               
BIS10    BAS   RE,VALDUE           VALIDATE DUE COMPANY WITHHOLDINGS            
         GOTOR VALLORT             VALIDATE LIEN OR TRUSTEE                     
*                                                                               
         CLI   RECNUM,CH           CHECK RECORD                                 
         BNE   BIS15                                                            
         CLC   =C'CA',SCKSOW       CALIFORNIA?                                  
         BNE   BIS15                                                            
         CLI   STAXFLAG,C'Y'       DID WE ALREADY ADD THE STATE TAX?            
         BE    BIS15                                                            
         LA    R2,SCKSWTXH                                                      
         CLI   5(R2),0                                                          
         BE    BIS15                                                            
         GOTO1 VALIDEC                                                          
         OC    FULL,FULL                                                        
         BZ    BIS15                                                            
         L     RF,TOTDED                                                        
         A     RF,FULL                                                          
         ST    RF,TOTDED                                                        
*                                                                               
BIS15    LA    R2,SCKNETH          IF AMOUNTS DON'T ADD UP TO THE NET           
         L     RF,EARNINGS             CHECK AMOUNT THEN ERROR                  
         S     RF,TOTDED                                                        
         A     RF,REXP                                                          
         C     RF,NETAMNT                                                       
         BNE   ERRADD                                                           
*                                                                               
         BAS   RE,GETCDEL          R4 = A(CHECK DETAILS ELEMENT)                
         USING TACDD,R4                                                         
*                                                                               
         CLI   PAYTYPE,TAW4TYIN    IF PAYMENT TYPE IS INDIVIDUAL                
         BE    *+12                                                             
         CLI   PAYTYPE,TAW4TYES    OR ESTATE                                    
         BNE   BIS20                                                            
         MVC   TACDEARN,EARNINGS   THEN SAVE TAXABLE EARNINGS IN ELEM           
         MVC   TACDNTAX,REXP                 REIMBURSED EXPENSES                
         B     BIS30                                                            
*                                                                               
BIS20    L     RF,EARNINGS         ELSE SAVE NON-TAXABLE EARNINGS =             
         A     RF,REXP                 CORP EARNINGS + REIMB EXP                
         ST    RF,TACDNTAX             IN ELEMENT                               
*                                                                               
BIS30    MVC   TACDNET,NETAMNT     SAVE NET CHECK AMOUNT IN ELEMENT             
*                                                                               
         CLI   PAYTYPE,TAW4TYIN    IF PAYMENT TYPE IS INDIVIDUAL                
         BE    *+12                                                             
         CLI   PAYTYPE,TAW4TYES    OR ESTATE                                    
         BNE   BIS40                                                            
*                                                                               
         CLI   TACDFREQ,0          THEN IF FREQENCY ALREADY SET                 
         BNE   BIS40               THEN TAKE IT AS IS                           
*                                                                               
         MVI   TACDFREQ,C'W'       ELSE IF TAXABLE EARNINGS <= $1000            
         CLC   EARNINGS,=F'100000'                                              
         BNH   BIS40               THEN SET FREQUENCY TO WEEKLY                 
*                                                                               
         MVI   TACDFREQ,C'M'       ELSE IF TAXABLE EARNINGS <= $3500            
         CLC   EARNINGS,=F'350000'                                              
         BNH   BIS40               THEN SET FREQUENCY TO MONTHLY                
*                                                                               
         MVI   TACDFREQ,C'Q'       ELSE SET FREQUENCY TO QUATERLY               
*                                                                               
BIS40    OC    DIRCT,DIRCT         IF DIRECT AMOUNT                             
         BZ    BIS45                                                            
         MVI   BYTE,TAODTYPD                                                    
         BRAS  RE,BLDISOD                                                       
*                                                                               
BIS45    OC    WIRE,WIRE           IF WIRE AMOUNT                               
         BZ    BIS48                                                            
         MVI   BYTE,TAODTYPW                                                    
         BRAS  RE,BLDISOD                                                       
*                                                                               
BIS48    BAS   RE,GETPDEL          R4 = A(PAYMENT DETAILS ELEMENT)              
         USING TAPDD,R4                                                         
*                                  PRE-CLEAR AMOUNTS                            
         XC    TAPDAMTS(TAPDINR-TAPDAMTS+4),TAPDAMTS                            
         MVC   TAPDNTNW,REXP                                                    
*                                                                               
         L     RF,EARNINGS         RF = PAYMENT AMOUNT BEFORE DUECOMP           
         A     RF,DUETOT                                                        
*                                                                               
         CLI   PAYTYPE,TAW4TYIN    IF PAYMENT TYPE IS INDIVIDUAL                
         BE    *+12                                                             
         CLI   PAYTYPE,TAW4TYES    OR ESTATE                                    
         BNE   BIS50                                                            
         STCM  RF,15,TAPDPAYI      THEN SAVE PAYMENT AMOUNT FOR INDIV           
         B     BIS60                                                            
*                                                                               
BIS50    STCM  RF,15,TAPDPAYC      ELSE SAVE PAYMENT AMOUNT FOR CORP            
*                                                                               
BIS60    MVC   TAPDREXP,REXP       SAVE REIMBURSED EXPENSES                     
         MVC   TAPDMDED,MDED            MISCELLANEOUS DEDUCTIONS                
         MVC   TAPDDUES,DUES            UNION DUES                              
*                                                                               
         BRAS  RE,TAXABLE          SET TAXABLE INDICATORS IN CW ELEMS           
*                                                                               
*                                  ADD CHECK COMMENT IF ANY                     
         GOTO1 NAMIN,DMCB,TACMELQ,(X'80',SCKCCOMH),TACMTYPC                     
*                                                                               
         BRAS  RE,BLDISTU          BUILD TATU                                   
*                                                                               
         L     R3,AIO              BUILD ALL OF CHECK KEY EXCEPT FOR            
         USING TLCKD,R3                THE ADJUSTMENT AGY/INV AND               
         MVI   TLCKCD,TLCKCDQ          SEQUENCE NUMBER                          
         MVC   TLCKSORT,TGCSORT                                                 
         MVC   TLCKSSN,CHKSSN                                                   
         MVC   TLCKCAT,TGCAT                                                    
*                                                                               
BISX     B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE VALIDATES THE SCREEN DATA AND BUILDS THE TAX REFUND              
* CHECK RECORD WITH EVERYTHING EXCEPT THE ADJUSMENT AGY/INV.                    
*                                                                               
BLDTR    NTR1                                                                   
         LA    R2,STREMPH          SAVE ADDRESSES OF EMP AND SSN FIELDS         
         ST    R2,AEMPFLD                                                       
         LA    R2,STRSSNH                                                       
         ST    R2,ASSNFLD                                                       
         LA    R2,STRSSNNH                                                      
         ST    R2,ASSNNFLD                                                      
*                                                                               
         BAS   RE,INITCHK          INITIALIZE CHECK RECORD                      
*                                                                               
         BAS   RE,GETCAEL          R4 = A(CAST DETAILS ELEMENT)                 
         USING TACAD,R4                                                         
*                                                                               
         XC    STRNET,STRNET       CLEAR NET FIELD                              
         OI    STRNETH+6,X'80'                                                  
*                                                                               
         LA    RF,STRFTAXH         RF=A(DEDUCTION AMT FIELDS)                   
         LA    RE,AFTAXFLD         RE=A(ADDRS FOR DEDUCT AMTS)                  
         LA    R0,NAMTS1           R0=N'DEDUCTION AMT FIELDS                    
         LA    R1,STRFICAH-STRFTAXH R1=LENGTH OF LINE                           
BTT60    ST    RF,0(RE)            SAVE THE ADDRESS                             
         AR    RF,R1               BUMP TO NEXT SCREEN FIELD                    
         LA    RE,4(RE)            BUMP TO NEXT SAVE ADDRESS                    
         BCT   R0,BTT60            LOOP TO NEXT                                 
*                                                                               
         MVI   DRCRFLAG,C'C'       SET FLAG TO CREDIT AMOUNTS                   
         BAS   RE,BLDAMNTS         VALIDATE DEDUCTION AMOUNTS                   
*                                                                               
         GOTO1 REVERSE             REVERSE TAX AMOUNTS                          
*                                                                               
         BAS   RE,GETCDEL          R4 = A(CHECK DETAILS ELEMENT)                
         USING TACDD,R4                                                         
         CLI   LASTYEAR,C'Y'       IF FORCE TO LAST YEAR IS SET                 
         BNE   *+8                                                              
         OI    TACDSTAT,TACDSFRC   SET CORRES. BIT IN CHECK DTLS. EL.           
*                                                                               
         MVC   TACDNET,TOTDED      SAVE TOTAL DEDUCTIONS IN NET                 
*                                                                               
         LA    R2,STRNETH          DISPLAY NET BACK                             
         MVC   FULL,TOTDED                                                      
         BAS   RE,EDIT10F                                                       
*                                  ADD CHECK COMMENT IF ANY                     
         GOTO1 NAMIN,DMCB,TACMELQ,(X'80',STRCOMMH),TACMTYPC                     
*                                                                               
BTRX     B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE VALIDATES THE SCREEN DATA AND BUILDS THE TAX TRANSFER            
* CHECK RECORDS WITH EVERYTHING EXCEPT THE ADJUSMENT AGY/INV.  THE              
* FIRST CHECK BUILT IS THE CREDIT CHECK, AND IT IS SAVED IN TIA.  THE           
* SECOND CHECK IS THE DEBIT CHECK WHICH IS SAVED IN AIO.                        
*                                                                               
BLDTT    NTR1                                                                   
         LA    R2,STTEMPH          SAVE ADDRESSES OF EMP AND SSN FIELDS         
         ST    R2,AEMPFLD                                                       
         LA    R2,STTSSNH                                                       
         ST    R2,ASSNFLD                                                       
         LA    R2,STTSSNNH                                                      
         ST    R2,ASSNNFLD                                                      
*                                                                               
         CLC   STTOSOT,=C'MLT'                                                  
         BNE   BTT05                                                            
         LA    R2,STTOSTAH                                                      
         CLI   5(R2),0                                                          
         BNE   ERTTMLT                                                          
         LA    R2,STTNSTAH                                                      
         CLI   5(R2),0                                                          
         BNE   ERTTMLT                                                          
         LA    R2,STTOSUIH                                                      
         CLI   5(R2),0                                                          
         BNE   ERTTMLT                                                          
         LA    R2,STTNSUIH                                                      
         CLI   5(R2),0                                                          
         BNE   ERTTMLT                                                          
         LA    R2,STTOSDIH                                                      
         CLI   5(R2),0                                                          
         BNE   ERTTMLT                                                          
         LA    R2,STTNSDIH                                                      
         CLI   5(R2),0                                                          
         BNE   ERTTMLT                                                          
         LA    R2,STTOFLIH                                                      
         CLI   5(R2),0                                                          
         BNE   ERTTMLT                                                          
         LA    R2,STTNFLIH                                                      
         CLI   5(R2),0                                                          
         BNE   ERTTMLT                                                          
*                                                                               
BTT05    CLC   STTOCOT,=C'MLT'                                                  
         BNE   BTT06                                                            
         LA    R2,STTOLTAH                                                      
         CLI   5(R2),0                                                          
         BNE   ERTTMLT                                                          
         LA    R2,STTNLTAH                                                      
         CLI   5(R2),0                                                          
         BNE   ERTTMLT                                                          
                                                                                
BTT06    BAS   RE,INITCHK          INITIALIZE CHECK RECORD                      
*                                                                               
         BAS   RE,GETCAEL          R4 = A(CAST DETAILS ELEMENT)                 
         USING TACAD,R4                                                         
*                                                                               
         XC    STTONET,STTONET     CLEAR OLD AND NEW NET FIELDS                 
         OI    STTONETH+6,X'80'                                                 
         XC    STTNNET,STTNNET                                                  
         OI    STTNNETH+6,X'80'                                                 
*                                                                               
         LA    RF,STTOERNH         RF=A(CRED DEDUCT AMT FIELDS)                 
         LA    RE,AEARNFLD         RE=A(ADDRS FOR CRED DED AMTS)                
         LA    R0,NAMTS2           R0=N'CREDIT DEDUCTION AMT FIELDS             
         LA    R1,STTONTXH-STTOERNH R1=LENGTH OF LINE                           
BTT10    ST    RF,0(RE)            SAVE THE ADDRESS                             
         AR    RF,R1               BUMP TO NEXT SCREEN FIELD                    
         LA    RE,4(RE)            BUMP TO NEXT SAVE ADDRESS                    
         BCT   R0,BTT10            LOOP TO NEXT                                 
*                                                                               
         MVC   SOT,OLDSOT          SET CREDIT SOT AND COT                       
         MVC   SOTST,OLDSOTST                                                   
         MVC   COT,OLDCOT                                                       
*                                                                               
         MVI   DRCRFLAG,C'C'       SET FLAG TO CREDIT AMOUNTS                   
         BAS   RE,BLDAMNTS         VALIDATE CREDIT DEDUCTION AMOUNTS            
*                                                                               
         MVC   OLDDED,TOTDED       SAVE TOTAL CREDIT DEDUCTIONS                 
         MVC   OLDEARN,EARNINGS         CREDIT TAXABLE EARNINGS                 
         MVC   OLDNTAX,NONTAX           CREDIT NON-TAXABLE EARNINGS             
         MVC   OLDREXP,REXP             CREDIT REIMBURSED EXPENSES              
*                                                                               
         L     RF,OLDEARN          COMPUTE CREDIT NET = CREDIT EARNINGS         
         A     RF,OLDNTAX              + CREDIT NON-TAXABLE EARNINGS            
         A     RF,OLDREXP              + CREDIT REIMBURSED EXPENSES             
         S     RF,OLDDED               - TOTAL CREDIT DEDUCTIONS                
         ST    RF,OLDNET                                                        
*                                                                               
         CLI   CHKTRAN,C'N'        IF NOT SINGLE CHECK TRANSFER                 
         BNE   BTT30                                                            
         CLI   EARNONLY,C'Y'       AND NOT TRANSFERRING ONLY EARNINGS           
         BE    BTT30                                                            
         LA    R2,STTOERNH         THEN DON'T ALLOW DEDS GT EARNINGS            
         LTR   RF,RF                                                            
         BM    ERRDED                                                           
*                                                                               
BTT30    BAS   RE,GETCDEL          R4 = A(CHECK DETAILS ELEMENT)                
         USING TACDD,R4                                                         
         CLI   LASTYEAR,C'Y'       IF FORCE TO LAST YEAR IS SET                 
         BNE   *+8                                                              
         OI    TACDSTAT,TACDSFRC   SET CORRES. BIT IN CHECK DTLS. EL.           
*                                                                               
         MVC   TACDNET,OLDNET      SAVE CREDIT NET                              
         MVC   TACDEARN,OLDEARN    SAVE CREDIT EARNINGS IN ELEMENT              
         L     R0,OLDNTAX                                                       
         A     R0,OLDREXP                                                       
         ST    R0,TACDNTAX         SAVE CREDIT NON-TAXABLE EARNINGS             
*                                                                               
         BAS   RE,GETPDEL          R4 = A(PAYMENT DETAILS ELEMENT)              
         USING TAPDD,R4                                                         
         MVC   TAPDREXP,OLDREXP    SAVE CREDIT REXP IN ELEMENT                  
*                                                                               
         GOTO1 REVERSE             REVERSE CREDIT CHECK AMOUNTS                 
*                                                                               
*                                  ADD CHECK COMMENT IF ANY                     
         GOTO1 NAMIN,DMCB,TACMELQ,(X'80',STTCOMMH),TACMTYPC                     
*                                                                               
         L     RE,ATIA             MOVE CREDIT CHECK INTO TIA                   
         LHI   RF,4000                                                          
         L     R0,AIO                                                           
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                                                               
         CLC   NEWSOT,=C'RET'      IF RETURN, DON'T PROCESS DEBIT               
         BE    BTTX                                                             
*                                                                               
         BRAS  RE,EXTRW4           RESET CHECK WITHHOLDING BLOCK                
*                                                                               
         BAS   RE,GETCAEL          R4 = A(CAST DETAILS ELEMENT)                 
         USING TACAD,R4                                                         
*                                                                               
         LA    RF,STTNERNH         RF=A(DEBIT DEDUCT AMT FIELDS)                
         LA    RE,AEARNFLD         RE=A(ADDRS FOR DEB DED AMTS)                 
         LA    R0,NAMTS2           R0=N'DEBIT DEDUCTION AMT FIELDS              
         LA    R1,STTNNTXH-STTNERNH R1=LENGTH OF LINE                           
BTT40    ST    RF,0(RE)            SAVE THE ADDRESS                             
         AR    RF,R1               BUMP TO NEXT SCREEN FIELD                    
         LA    RE,4(RE)            BUMP TO NEXT SAVE ADDRESS                    
         BCT   R0,BTT40            LOOP TO NEXT                                 
*                                                                               
         MVC   SOT,NEWSOT          SET DEBIT SOT AND COT                        
         MVC   SOTST,NEWSOTST                                                   
         MVC   COT,NEWCOT                                                       
*                                                                               
         MVI   DRCRFLAG,C'D'       SET FLAG TO DEBIT AMOUNTS                    
         BAS   RE,BLDAMNTS         VALIDATE DEBIT DEDUCTION AMOUNTS             
*                                                                               
         L     RF,EARNINGS         COMPUTE DEBIT NET = DEBIT EARNINGS           
         A     RF,NONTAX               + DEBIT NON-TAXABLE                      
         A     RF,REXP                 + DEBIT REIMBURSED EXPENSES              
         S     RF,TOTDED               - TOTAL DEBIT DEDUCTIONS                 
         ST    RF,NETAMNT                                                       
*                                                                               
         CLI   EARNONLY,C'Y'       IF EARNINGS ONLY NOT REQUESTED               
         BE    BTT46                                                            
         OC    EARNINGS,EARNINGS   THEN DON'T ALLOW EARNINGS TRANSFER           
         BZ    *+14                                                             
         OC    OLDEARN,OLDEARN                                                  
         BZ    ERREARN                                                          
         OC    NONTAX,NONTAX                                                    
         BZ    *+14                                                             
         OC    OLDNTAX,OLDNTAX                                                  
         BZ    ERREARN                                                          
*                                                                               
BTT46    CLI   CHKTRAN,C'N'        IF NOT SINGLE CHECK TRANSFER                 
         BNE   BTT50                                                            
         CLI   EARNONLY,C'Y'       AND NOT TRANSFERRING ONLY EARNINGS           
         BE    *+14                                                             
         LA    R2,STTNERNH         THEN DON'T ALLOW NEGATIVE NET                
         LTR   RF,RF                                                            
         BM    ERRDED                                                           
*                                                                               
         LA    R2,STTOERNH         ERROR IF CREDIT NET DOES NOT                 
         CLC   OLDNET,NETAMNT          EQUAL DEBIT NET                          
         BNE   ERRDRCR                                                          
*                                                                               
BTT50    BAS   RE,GETCDEL          R4 = A(CHECK DETAILS ELEMENT)                
         USING TACDD,R4                                                         
*                                                                               
         MVC   TACDNET,NETAMNT     SAVE DEBIT NET                               
*                                                                               
         MVC   TACDEARN,EARNINGS   SAVE DEBIT EARNINGS IN ELEMENT               
*                                                                               
         L     R0,NONTAX                                                        
         A     R0,REXP                                                          
         ST    R0,TACDNTAX         SAVE DEBIT NON-TAXABLE EARNINGS              
*                                                                               
         BAS   RE,GETPDEL          R4 = A(PAYMENT DETAILS ELEMENT)              
         USING TAPDD,R4                                                         
         MVC   TAPDREXP,REXP       SAVE REXP IN ELEMENT                         
*                                                                               
         LA    R2,STTONETH         DISPLAY OLD NET BACK                         
         MVC   FULL,OLDNET                                                      
         BAS   RE,EDIT10F                                                       
         LA    R2,STTNNETH         DISPLAY NEW NET BACK                         
         MVC   FULL,NETAMNT                                                     
         BAS   RE,EDIT10F                                                       
*                                                                               
BTTX     B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE ADJUSTS THE W4 TYPE IN THE PAYMENT DETAILS ELEMENT OF            
* THE CURRENT CHECK RECORD IF TRANSFERRING EARNINGS ONLY AND THE TYPE           
* IS NOT IN SYNC WITH THE TYPE OF EARNINGS (TAXABLE VS. NON-TAXABLE)            
* BEING TRANSFERRED.  IF W4 TYPE ENDS UP BEING CORP, OR FOREIGNER               
* TRUSTEE THEN ANY WITHHOLDING ELEMENTS ARE REMOVED FROM THE RECORD.            
*                                                                               
ADJW4TY  NTR1                                                                   
         CLI   EARNONLY,C'Y'       IF TRANSFERRING EARNINGS ONLY                
         BNE   ATYX                                                             
         BAS   RE,GETCDEL          R4 = A(CHECK DETAILS ELEMENT)                
         USING TACDD,R4                                                         
         OC    TACDEARN,TACDEARN   IF TAXABLE EARNINGS INPUT                    
         BZ    ATY10                                                            
         BAS   RE,GETPDEL          R4 = A(PAYMENT DETAILS ELEMENT)              
         USING TAPDD,R4                                                         
         CLI   TAPDW4TY,TAW4TYIN   AND W4 TYPE NOT INDIVIDUAL                   
         BE    ATYX                                                             
         CLI   TAPDW4TY,TAW4TYES                OR ESTATE                       
         BE    ATYX                                                             
         MVI   TAPDW4TY,TAW4TYIN   THEN SET TO INDIVIDUAL                       
         B     ATYX                                                             
*                                                                               
         USING TACDD,R4                                                         
ATY10    OC    TACDNTAX,TACDNTAX   ELSE IF NON-TAXABLE EARNINGS INPUT           
         BZ    ATYX                                                             
         MVI   ELCODE,TACWELQ      DELETE WITHHOLDING ELEMENTS                  
         GOTO1 REMELEM                                                          
*                                                                               
         BAS   RE,GETPDEL          R4 = A(PAYMENT DETAILS ELEMENT)              
         USING TAPDD,R4                                                         
         CLI   TAPDW4TY,TAW4TYCO   IF W4 TYPE NOT CORPORATION                   
         BE    ATYX                                                             
         CLI   TAPDW4TY,TAW4TYCA               OR CANADIAN                      
         BE    ATYX                                                             
         CLI   TAPDW4TY,TAW4TYFO               OR FOREIGNER                     
         BE    ATYX                                                             
         CLI   TAPDW4TY,TAW4TYTR               OR TRUSTEE                       
         BE    ATYX                                                             
         MVI   TAPDW4TY,TAW4TYCO   THEN SET TO CORPORATION                      
*                                                                               
ATYX     B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE ADDS THE CHECK RECORDS FOR THE SSN TRANSFER.  IF THE             
* TRANSFER IS FOR A SPECIFIC CHECK, THEN SSNCHK IS CALLED.  IF NOT              
* THEN YTDLOOP IS CALLED FOR BOTH EMPLOYEES, ONE FOR THE CREDIT                 
* EMPLOYEE AND ONE FOR THE DEBIT EMPLOYEE.                                      
*                                                                               
BLDST    NTR1                                                                   
         CLI   CHKTRAN,C'Y'        IF SINGLE CHECK TRANSFER                     
         BNE   BST10                                                            
         BAS   RE,SSNCHK           THEN SSNCHK WILL GENERATE CHECKS             
         B     BSTX                                                             
*                                                                               
BST10    LA    R2,SSTFEMPH         ELSE SAVE ADDRESSES OF 'FROM' EMP            
         ST    R2,AEMPFLD              AND SSN FIELDS                           
         LA    R2,SSTFSSNH                                                      
         ST    R2,ASSNFLD                                                       
         LA    R2,SSTFSNNH                                                      
         ST    R2,ASSNNFLD                                                      
*                                                                               
         BAS   RE,INITCHK          INITIALIZE CHECK RECORD                      
*                                                                               
*                                  ADD CHECK COMMENT IF ANY                     
         GOTO1 NAMIN,DMCB,TACMELQ,(X'80',SSTCCOMH),TACMTYPC                     
*                                                                               
         MVI   DRCRFLAG,C'C'       SET FLAG TO CREDIT AMOUNTS                   
*                                                                               
         BAS   RE,YTDNTAX          ADD NON-TAXABLE CHECK IF NEC.                
*                                                                               
         BAS   RE,YTDLOOP          ADD CHECKS FOR 'FROM' EMP/SSN                
*                                                                               
         LA    R2,SSTTEMPH         SAVE ADDRESSES OF 'TO' EMP AND SSN           
         ST    R2,AEMPFLD              FIELDS                                   
         LA    R2,SSTTSSNH                                                      
         ST    R2,ASSNFLD                                                       
         LA    R2,SSTTSNNH                                                      
         ST    R2,ASSNNFLD                                                      
*                                                                               
         BAS   RE,INITCHK          INITIALIZE CHECK RECORD                      
*                                                                               
*                                  ADD CHECK COMMENT IF ANY                     
         GOTO1 NAMIN,DMCB,TACMELQ,(X'80',SSTCCOMH),TACMTYPC                     
*                                                                               
         MVI   DRCRFLAG,C'D'       SET FLAG TO DEBIT AMOUNTS                    
*                                                                               
         BAS   RE,YTDNTAX          ADD NON-TAXABLE CHECK IF NEC.                
*                                                                               
         BAS   RE,YTDLOOP          ADD CHECKS FOR 'TO' EMP/SSN                  
*                                                                               
BSTX     B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE HANDLES THE SSN TRANSFER CHECKS FOR A SINGLE CHECK               
* TRANSFER.  IT ADDS AN ADJUSTMENT CREDIT CHECK FOR THE OLD EMPLOYEE            
* AND A DEBIT CHECK FOR THE NEW EMPLOYEE.  IT USES THE ORIGINAL CHECK           
* FOR THE AMOUNTS, AND JUST CHANGES THE AGENCY, INVOICE, AND EMPLOYEE           
* INFORMATION WHERE APPROPRIATE.                                                
*                                                                               
SSNCHK   NTR1                                                                   
         LA    R3,KEY              BUILD CHECK NUMBER KEY                       
         USING TLCKPD,R3                                                        
         XC    KEY,KEY                                                          
         MVI   TLCKPCD,TLCKCCDQ                                                 
         MVC   TLCKCCHK,TGCHK                                                   
         XC    TLCKCCHK,ALLFF                                                   
*                                                                               
         GOTO1 HIGH                READ FOR KEY                                 
*                                                                               
         LA    R2,SSTCHKH          ERROR IF CHECK KEY NOT FOUND                 
         CLC   KEY(TLCKCBNK-TLCKPD),KEYSAVE                                     
         BNE   ERRNOFND                                                         
*                                                                               
         GOTO1 GETREC              READ RECORD                                  
*                                                                               
         BAS   RE,GETCDEL          R4 = A(CHECK DETAILS ELEMENT)                
         USING TACDD,R4                                                         
*                                                                               
         MVC   ORIGDTE,TACDDTE     SAVE ORIGINAL CHECK DATE/SEQNUM              
         MVC   ORIGSEQ,TACDSEQ                                                  
*                                                                               
         XC    TACDDTE,TACDDTE     CLEAR CHECK DATE, SEQUENCE NUMBER,           
         XC    TACDSEQ,TACDSEQ         BANK, CHECK NUMBER, AND RUN DATE         
         XC    TACDCHK,TACDCHK                                                  
         XC    TACDBNK,TACDBNK                                                  
         XC    TACDRUN,TACDRUN                                                  
*                                                                               
         CLI   LASTYEAR,C'Y'       IF FORCE TO LAST YEAR IS SET                 
         BNE   *+8                                                              
         OI    TACDSTAT,TACDSFRC   SET CORRES. BIT IN CHECK DTLS. EL.           
*                                                                               
         L     R3,AIO              R3 = A(CHECK KEY)                            
         USING TLCKD,R3                                                         
*                                                                               
         LA    R4,OIELEM           BUILD ORIGINAL AGENCY/INVOICE                
         USING TAOID,R4                ELEMENT IN OIELEM                        
         XC    OIELEM,OIELEM                                                    
         MVI   TAOIEL,TAOIELQ                                                   
         MVI   TAOILEN,TAOILNQ                                                  
         MVC   TAOIAGY,TLCKAGY     GET ORIGINAL AGY/INV FROM CHECK KEY          
         MVC   TAOIINV,TLCKINV                                                  
*                                                                               
         LA    R4,OKELEM           BUILD ORIGINAL CHECK DATE/SEQUENCE           
         USING TAOKD,R4                NUMBER ELEMENT IN OKELEM                 
         XC    OKELEM,OKELEM                                                    
         MVI   TAOKEL,TAOKELQ                                                   
         MVI   TAOKLEN,TAOKLNQ                                                  
         MVC   TAOKDTE,ORIGDTE     CHECK DATE AND SEQUENCE NUMBER FROM          
         MVC   TAOKSEQ,ORIGSEQ         ORIGINAL CHECK                           
*                                                                               
         CLI   RECNUM,SS           IF RECORD/ACTION SSN/TRANSFER                
         BNE   SSNCHK05                                                         
         MVI   ELCODE,TADWELQ      REMOVE ANY DUE COMPANY WITHHOLDING           
         GOTO1 REMELEM                 ELEMENTS                                 
*                                                                               
SSNCHK05 MVI   ELCODE,TAOIELQ      REMOVE OLD ORIGINAL AGY/INV ELEMENT          
         GOTO1 REMELEM                                                          
*                                  ADD OI ELEM TO CHECK RECORD                  
         MVC   ELEM(TAOILNQ),OIELEM                                             
         GOTO1 ADDELEM                                                          
*                                                                               
         MVI   ELCODE,TAOKELQ      REMOVE OLD ORIGINAL CHK DTE ELEMENT          
         GOTO1 REMELEM                                                          
*                                  ADD OK ELEM TO CHECK RECORD                  
         MVC   ELEM(TAOKLNQ),OKELEM                                             
         GOTO1 ADDELEM                                                          
*                                  ADD CHECK COMMENT IF ANY                     
         GOTO1 NAMIN,DMCB,TACMELQ,(X'80',SSTCCOMH),TACMTYPC                     
*                                                                               
         GOTO1 REVERSE             REVERSE AMOUNTS IN CHECK RECORD              
*                                                                               
         BRAS  RE,GETINV           GET NEXT INVOICE NUMBER FOR ADJ              
*                                                                               
         BAS   RE,ADDCHK           ADD SSN TRANSFER CHECK                       
*                                                                               
         BRAS  RE,INITINV          INITIALIZE INVOICE RECORD                    
*                                                                               
         BAS   RE,ADDINV           ADD INVOICE RECORD FOR SSN TRANSFER          
*                                                                               
         LA    R2,SSTTEMPH         SAVE 'TO' EMP AND SSN IN GLOBAL              
         GOTO1 ANY                                                              
         MVC   TGEMP,WORK                                                       
         LA    R2,SSTTSSNH                                                      
         GOTO1 ANY                                                              
         MVC   TGSSN,WORK                                                       
         CLI   SSTTSSNH+5,6                                                     
         BH    SSNCHK10                                                         
         MVC   TGSSN,SSNSPACE                                                   
         MVC   TGPID,SSTTSSN                                                    
         GOTO1 SSNUNPK,DMCB,TGPID,TGSSN                                         
*                                                                               
SSNCHK10 L     R3,AIO              R3 = A(CHECK KEY)                            
         USING TLCKD,R3                                                         
         MVC   TLCKSSN,TGSSN       SAVE 'TO' SSN IN KEY                         
*                                                                               
         BAS   RE,GETPDEL          R4 = A(PAYMENT DETAILS ELEMENT)              
         USING TAPDD,R4                                                         
         MVC   TAPDEMP,TGEMP       SAVE EMPLOYER IN ELEMENT                     
*                                                                               
         GOTO1 REVERSE             REVERSE AMOUNTS IN CHECK RECORD              
*                                                                               
         BRAS  RE,GETINV           GET NEXT INVOICE NUMBER FOR ADJ              
*                                                                               
         BAS   RE,ADDCHK           ADD SSN TRANSFER CHECK                       
*                                                                               
         BRAS  RE,INITINV          INITIALIZE INVOICE RECORD                    
*                                                                               
         BAS   RE,ADDINV           ADD INVOICE RECORD FOR SSN TRANSFER          
*                                                                               
SCX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE CREATES A CHECK RECORD BASED ON NON-TAXABLE EARINGS              
* RETURNED BY YTD ROUTINE.                                                      
*                                                                               
YTDNTAX  NTR1                                                                   
         BRAS  RE,TSTPCAN          P+ CANADIAN?                                 
         JE    YNX                                                              
*                                                                               
         OC    MYDNTAX,MYDNTAX     TEST ANYTHING TO ADD                         
         BZ    YNX                                                              
*                                                                               
         BAS   RE,GETCDEL          R4 = A(CHECK DETAILS ELEMENT)                
         USING TACDD,R4                                                         
         MVC   TACDNTAX,MYDNTAX    SET NON-TAXABLE EARNINGS                     
         MVC   TACDNET,MYDNTAX     NET AMOUNT IS SAME                           
*                                                                               
         BAS   RE,GETPDEL          R4 = A(PAYMENT DETAILS ELEMENT)              
         USING TAPDD,R4                                                         
         CLI   TAPDW4TY,TAW4TYCO   IF W4 TYPE IS NOT CORPORATION                
         BE    YN10                                                             
         CLI   TAPDW4TY,TAW4TYCA   OR CANADIAN                                  
         BE    YN10                                                             
         CLI   TAPDW4TY,TAW4TYTR   OR TRUSTEE                                   
         BE    YN10                                                             
         CLI   TAPDW4TY,TAW4TYFO   OR FOREIGNER                                 
         BE    YN10                                                             
         MVI   TAPDW4TY,TAW4TYCO   THEN FORCE CORPORATION                       
*                                                                               
YN10     CLI   DRCRFLAG,C'C'       IF ADDING CREDIT CHECKS                      
         BNE   YN20                                                             
         GOTO1 REVERSE             THEN REVERSE CHECK AMOUNTS                   
*                                                                               
YN20     BRAS  RE,GETINV           GET NEXT ADJUSTMENT INVOICE NUMBER           
*                                                                               
         BAS   RE,ADDCHK           ADD CHECK                                    
*                                                                               
         BRAS  RE,INITINV          INITIALIZE INVOICE RECORD                    
*                                                                               
         BAS   RE,ADDINV           ADD INVOICE RECORD FOR CHECK                 
*                                                                               
YNX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE LOOPS THROUGH THE YTDTAB MAKING CALLS TO YTDCHK FOR EACH         
* ENTRY TO GENERATE THE TRANSFER CHECKS FOR THE CURRENT EMPLOYER AND            
* SSN.                                                                          
*                                                                               
YTDLOOP  NTR1                                                                   
         XC    SVCNUTAX,SVCNUTAX                                                
         XC    SVCNCTAX,SVCNCTAX                                                
*                                                                               
         L     R5,AYTDTAB          R5 = A(FIRST YTDTAB ENTRY)                   
         USING YTDD,R5                                                          
*                                                                               
YL10     CLI   0(R5),0             WHILE NOT END OF YTDTAB                      
         BE    YLX                                                              
*                                                                               
         OC    YTDAMTS(YTDAMTLQ),YTDAMTS  IF NO AMOUNTS HERE                    
         BZ    YL30                       THEN SKIP TO NEXT UNIT                
*                                                                               
         BRAS  RE,TSTPCAN          P+ CANADIAN                                  
         JNE   YL15                                                             
         CLC   YTDUNIT,=C'FD '                                                  
         JE    YL30                                                             
         CLC   YTDUNIT,=C'CN '                                                  
         JNE   YL15                                                             
*                                                                               
         CLC   SVCNUTAX,=X'FFFFFFFF'   ALREADY ADDED TAAT FOR IT?               
         JE    *+16                                                             
         MVC   SVCNUTAX,YTDUTAX    SAVE P+ CANADIAN FEDERAL TAX                 
         MVC   SVCNCTAX,YTDCTAX    SAVE P+ CANADIAN FEDERAL TAX (CAD)           
         J     YL30                                                             
*                                                                               
YL15     BAS   RE,YTDCHK           BUILD CHECK AMOUNTS FOR THIS UNIT            
*                                                                               
         CLI   DRCRFLAG,C'C'       IF ADDING CREDIT CHECKS                      
         BNE   YL20                                                             
         GOTO1 REVERSE             THEN REVERSE CHECK AMOUNTS                   
*                                                                               
YL20     BRAS  RE,GETINV           GET NEXT ADJUSTMENT INVOICE NUMBER           
*                                                                               
         BAS   RE,ADDCHK           ADD CHECK                                    
*                                                                               
         BRAS  RE,INITINV          INITIALIZE INVOICE RECORD                    
*                                                                               
         BAS   RE,ADDINV           ADD INVOICE RECORD FOR CHECK                 
*                                                                               
YL30     LA    R5,YTDLNQ(R5)       BUMP R5 TO NEXT YTDTAB ENTRY                 
         B     YL10                LOOP BACK                                    
*                                                                               
YLX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE MOVES THE TAX INFO FOUND IN THE CURRENT YTD TABLE ENTRY          
* INTO THE CW ELEMENT BLOCK OF THE CHECK RECORD.  ADDITIONALLY IT SAVES         
* THE EARNINGS AND REIMBURSED EXPENSE AMOUNT IN THE CD AND PD ELEMENTS.         
* IT FINALLY MAKES SURE THE APPROPRIATE 'PARENT' CW ELEMENTS EXIST,             
* I.E., FEDERAL FOR EVERY UNIT AND STATE OF CITY FOR CITIES.                    
*                                                                               
         USING YTDD,R5             R5 = A(YTDTAB ENTRY)                         
YTDCHK   NTR1                                                                   
         L     RF,YTDEARN          TO COMPUTE NET FIRST TAKE EARNINGS           
         S     RF,YTDTAX               - TAXES - FICA/SUI                       
         S     RF,YTDFICA                                                       
*                                                                               
         L     RE,YTDREXP          THEN IF UNIT IS FEDERAL ADD IN REXP          
         CLC   YTDUNIT,=C'FD '                                                  
         BE    *+6                                                              
         LCR   RE,RE               ELSE SUBTRACT SDI                            
         AR    RF,RE                                                            
*                                                                               
         ST    RF,NETAMNT          SAVE NET IN LOCAL STORAGE                    
*                                                                               
         BAS   RE,GETCDEL          R4 = A(CHECK DETAILS ELEMENT)                
         USING TACDD,R4                                                         
         CLI   LASTYEAR,C'Y'       IF FORCE TO LAST YEAR IS SET                 
         BNE   *+8                                                              
         OI    TACDSTAT,TACDSFRC   SET CORRES. BIT IN CHECK DTLS. EL.           
*                                                                               
         BRAS  RE,TSTPCAN          P+ CANADIAN?                                 
         JNE   YC01                                                             
         L     RF,YTDEARN                                                       
         A     RF,YTDTXRE                                                       
         A     RF,YTDNTRE                                                       
         STCM  RF,15,TACDNTAX      SAVE EARNINGS IN NON TAX                     
         S     RF,YTDUTAX                                                       
         S     RF,YTDUPP                                                        
         S     RF,YTDUEI                                                        
         S     RF,YTDUPIP                                                       
         CLC   SVCNUTAX,=X'FFFFFFFF'  ALREADY USED?                             
         JE    *+8                                                              
         S     RF,SVCNUTAX         CANADIAN FEDERAL TAX                         
         ST    RF,NETAMNT                                                       
         MVC   TACDNET,NETAMNT     NET AMOUNT                                   
         J     YC02                                                             
*                                                                               
YC01     MVC   TACDEARN,YTDEARN    SAVE EARNINGS IN ELEMENT                     
         MVC   TACDNET,NETAMNT          NET                                     
*                                                                               
         MVC   TACDNTAX,YTDREXP    IF UNIT IS FEDERAL THEN SAVE REXP            
         CLC   =C'P+',TGEMP        P+?                                          
         BNE   *+10                                                             
         MVC   TACDNTAX,YTDNTRE    NON-TAXABLE REIMBURSEMENTS                   
*                                                                               
         CLC   YTDUNIT,=C'FD '         IN ELEMENT                               
         BE    *+10                                                             
         XC    TACDNTAX,TACDNTAX   ELSE CLEAR NON-TAXABLE EARNINGS              
*                                                                               
YC02     BAS   RE,GETPDEL          R4 = A(PAYMENT DETAILS ELEMENT)              
         USING TAPDD,R4                                                         
*                                                                               
         BRAS  RE,TSTPCAN          P+ CANADIAN?                                 
         JNE   YC04                                                             
         MVI   TAPDW4TY,C'A'                                                    
         MVC   TAPDGRS,YTDEARN                                                  
         MVC   TAPDPAYC,YTDEARN                                                 
         L     RF,YTDTXRE                                                       
         A     RF,YTDNTRE                                                       
         STCM  RF,15,TAPDREXP                                                   
         STCM  RF,15,TAPDNTNW                                                   
         J     YC20                                                             
*                                                                               
YC04     MVC   TAPDTXNW,YTDTXRE    TAXABLE NON WAGES                            
         MVC   TAPDNTNW,YTDNTRE    NON-TAXABLE NON WAGES                        
*                                                                               
         MVC   TAPDREXP,YTDREXP    IF UNIT IS FEDERAL THEN SAVE REXP            
         CLC   YTDUNIT,=C'FD '         IN ELEMENT                               
         BE    YC05                                                             
         XC    TAPDREXP,TAPDREXP   ELSE CLEAR REIMBURSED EXPENSES               
         XC    TAPDTXNW,TAPDTXNW                                                
         XC    TAPDNTNW,TAPDTXNW                                                
*                                                                               
         CLI   TAPDW4TY,TAW4TYIN   IF W4 TYPE IS NOT INDIVIDUAL                 
YC05     BE    *+16                                                             
         CLI   TAPDW4TY,TAW4TYES   OR ESTATE                                    
         BE    *+8                                                              
         MVI   TAPDW4TY,TAW4TYIN   THEN FORCE INDIVIDUAL                        
*                                                                               
         BRAS  RE,EXTRW4           INITIALIZE CW ELEMENT BLOCK                  
*                                                                               
*                                  SET TAXABLE INDICATOR FOR UNIT               
         GOTOR WITHUNIT,DMCB,('TACWSTAX',YTDUNIT)                               
*                                                                               
         MVI   ELCODE,TACWELQ      R4 = A(UNIT CW ELEMENT)                      
         GOTO1 GETL,DMCB,(3,YTDUNIT)                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,TGELEM                                                        
         USING TACWD,R4                                                         
*                                                                               
         MVC   TACWTAX,YTDTAX      SAVE TAX IN ELEMENT                          
*                                                                               
         CLC   YTDUNIT,=C'FD '     IF UNIT IS FEDERAL                           
         BNE   YC10                                                             
         MVC   TACWFICA,YTDFICA    THEN SAVE FICA IN ELEMENT                    
         B     YC20                                                             
*                                                                               
YC10     MVC   TACWSUI,YTDSUI      ELSE SAVE SUI IN ELEMENT                     
         MVC   TACWSDI,YTDSDI                SDI                                
         MVC   TACWSFLI,YTDSFLI              FLI                                
*                                                                               
*                                  MAKE SURE FEDERAL CW ELEM EXISTS             
         GOTOR WITHUNIT,DMCB,(0,=C'FD ')                                        
*                                                                               
         CLI   YTDUNIT+2,C' '      IF UNIT IS A CITY                            
         BNH   YC20                                                             
*                                  MAKE SURE STATE CW ELEM EXISTS               
         GOTO1 TAXVAL,DMCB,(3,YTDUNIT)                                          
         GOTOR WITHUNIT,DMCB,(0,TGTASTCY)                                       
*                                                                               
YC20     BRAS  RE,BLDSTTU          ADD YTD TATU ELEMENT FOR SSNTRAN             
         BRAS  RE,BLDSTAT          ADD CANADIAN TAXES ELEMENT                   
*                                                                               
YCX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE VALIDATES THE DEDUCTION AMOUNTS AND SAVES THEM IN THE            
* CHECK WITHHOLDING ELEMENTS.                                                   
*                                                                               
BLDAMNTS NTR1                                                                   
         XC    TOTDED,TOTDED       CLEAR TOTAL DEDUCTIONS                       
*                                                                               
         CLI   TWASCR,SCR09        IF THIS IS NOT TAX TRANSFER                  
         BNE   BA15                THEN SKIP TO WITHHOLDING VALIDATION          
*                                                                               
         L     R2,AEARNFLD         VALIDATE TAXABLE EARNINGS                    
         CLI   DRCRFLAG,C'D'       IF VALIDATING DEBIT AMOUNTS                  
         BNE   *+12                                                             
         CLI   5(R2),0             AND IF FIELD IS EMPTY                        
         BE    BA5                 THEN DEFAULT EARNINGS TO OLD                 
*                                                                               
         GOTO1 VALIDEC                                                          
         MVC   EARNINGS,FULL       SAVE IN LOCAL STORAGE                        
*                                                                               
BA5      MVC   FULL,EARNINGS       DISPLAY TAXABLE EARNINGS BACK                
         BAS   RE,EDIT10F                                                       
*                                                                               
         CLI   DRCRFLAG,C'C'       IF VALIDATING CREDIT AMOUNTS                 
         BNE   *+16                                                             
         L     R0,EARNINGS         MUST NOT BE MORE THAN YTD                    
         C     R0,MYDEARN                                                       
         BH    ERRINV                                                           
*                                                                               
         L     R2,ANTAXFLD         VALIDATE NON-TAXABLE EARNINGS                
         OC    STTCNTX,STTCNTX     IF NO NON-TAXABLE EARNINGS                   
         JZ    *+14                                                             
         CLC   STTCNTX,=CL10' '    IF NO NON-TAXABLE EARNINGS                   
         BNE   *+16                                                             
         CLI   5(R2),0             THEN NOTHING IS REQUIRED                     
         BE    BA10                                                             
*        B     ERRINV                                                           
*                                                                               
         CLI   DRCRFLAG,C'D'       IF VALIDATING DEBIT AMOUNTS                  
         BNE   *+12                                                             
         CLI   5(R2),0             AND IF FIELD IS EMPTY                        
         BE    BA10                THEN DEFAULT EARNINGS TO OLD                 
*                                                                               
         GOTO1 VALIDEC                                                          
         MVC   NONTAX,FULL         SAVE IN LOCAL STORAGE                        
*                                                                               
BA10     OC    STTCNTX,STTCNTX     IF NO NON-TAXABLE EARNINGS                   
         JZ    BA12                                                             
         CLC   STTCNTX,=CL10' '    IF NO NON-TAXABLE EARNINGS                   
         BE    *+14                                                             
         MVC   FULL,NONTAX         DISPLAY NON-TAXABLE EARNINGS BACK            
         BAS   RE,EDIT10F                                                       
*                                                                               
BA12     CLI   DRCRFLAG,C'C'       IF VALIDATING CREDIT AMOUNTS                 
         BNE   *+16                                                             
         L     R0,NONTAX           MUST NOT BE MORE THAN YTD                    
         C     R0,MYDNTAX                                                       
         BH    ERRINV                                                           
*                                                                               
         OC    EARNINGS,EARNINGS   IF TAXABLE EARNINGS INPUT                    
         BZ    *+14                                                             
         OC    NONTAX,NONTAX       THEN CAN'T ALSO HAVE NON-TAXABLE             
         BNZ   ERRINV                                                           
*                                                                               
         L     R2,AREXPFLD         VALIDATE REXP (OPTIONAL)                     
         L     R1,MYDREXP                                                       
         BAS   RE,OVERYTD                                                       
         MVC   REXP,FULL           SAVE IN LOCAL STORAGE                        
         BAS   RE,EDIT10F          DISPLAY REIMBURSED EXPENSES BACK             
*                                                                               
         CLI   EARNONLY,C'Y'       IF TRANSFERRING EARNINGS ONLY                
         BNE   BA15                                                             
         GOTO1 FLDVAL,DMCB,(X'80',AFTAXFLD),ACTXFLD  THEN NO MORE I/P           
         BE    BA15                                                             
         L     R2,8(R1)            FOUND INPUT - POINT TO IT                    
         B     ERRNOINP            AND GIVE ERROR                               
*                                                                               
BA15     CLC   SOT,=C'CN '         IF NOT TAXED IN CANADA                       
         BE    BA20                                                             
*                                  THEN SET WORK & TAX BIT IN FED ELEM          
         GOTOR WITHUNIT,DMCB,('TACWSWRK+TACWSTAX',=C'FD ')                      
*                                                                               
BA20     MVI   ELCODE,TACWELQ      IF FEDERAL ELEMENT EXISTS                    
         GOTO1 GETL,DMCB,(3,=C'FD ')                                            
         BNE   BA40                                                             
*                                                                               
         L     R4,TGELEM           THEN R4 = A(FEDERAL WITHHOLD ELEM)           
         USING TACWD,R4                                                         
*                                                                               
         L     R2,AFTAXFLD         VALIDATE FEDERAL TAX (OPTIONAL)              
         L     R1,MYDFTAX                                                       
         BAS   RE,OVERYTD                                                       
         MVC   TACWTAX,FULL        SAVE FEDERAL TAX IN ELEMENT                  
*                                                                               
         BAS   RE,EDIT10           DISPLAY FEDERAL TAX BACK                     
         BAS   RE,ADDDED           ADD TO TOTAL DEDUCTIONS                      
*                                                                               
         L     R2,AFICAFLD         VALIDATE FICA (OPTIONAL)                     
         L     R1,MYDFICA                                                       
         BAS   RE,OVERYTD                                                       
         MVC   TACWFICA,FULL       SAVE FICA IN ELEMENT                         
*                                                                               
         BAS   RE,EDIT10           DISPLAY FICA BACK                            
         BAS   RE,ADDDED           ADD TO TOTAL DEDUCTIONS                      
         B     BA50                                                             
*                                                                               
BA40     L     R2,AFTAXFLD         ELSE ERROR IF ANY INPUT IN FEDERAL           
         CLI   5(R2),0                 TAX OR FICA FIELDS                       
         BNE   ERRNOINP                                                         
         L     R2,AFICAFLD                                                      
         CLI   5(R2),0                                                          
         BNE   ERRNOINP                                                         
*                                  SET WORK & TAX BIT IN SOT ELEM               
BA50     GOTOR WITHUNIT,DMCB,('TACWSWRK+TACWSTAX',SOT)                          
*                                                                               
         MVI   ELCODE,TACWELQ      R4 = A(SOT WITHHOLD ELEMENT)                 
         GOTO1 GETL,DMCB,(3,SOT)                                                
         BNE   BA80                                                             
         L     R4,TGELEM                                                        
*                                                                               
         L     R2,ASTAXFLD         VALIDATE STATE TAX (OPTIONAL)                
         L     R1,MYDSTAX                                                       
         BAS   RE,OVERYTD                                                       
         MVC   TACWTAX,FULL        SAVE STATE TAX IN ELEMENT                    
*                                                                               
         BAS   RE,EDIT10           DISPLAY STATE TAX BACK                       
         BAS   RE,ADDDED           ADD TO TOTAL DEDUCTIONS                      
*                                                                               
         L     R2,ASUIFLD          VALIDATE UNEMPLOYMENT (OPTIONAL)             
         L     R1,MYDSUI                                                        
         BAS   RE,OVERYTD                                                       
         MVC   TACWSUI,FULL        SAVE SUI IN ELEMENT                          
*                                                                               
         BAS   RE,EDIT10           DISPLAY SUI BACK                             
         BAS   RE,ADDDED           ADD TO TOTAL DEDUCTIONS                      
*                                                                               
         L     R2,ASDIFLD          VALIDATE DISABILITY (OPTIONAL)               
         L     R1,MYDSDI                                                        
         BAS   RE,OVERYTD                                                       
         MVC   TACWSDI,FULL        SAVE SDI IN ELEMENT                          
*                                                                               
         BAS   RE,EDIT10           DISPLAY SDI BACK                             
         BAS   RE,ADDDED           ADD TO TOTAL DEDUCTIONS                      
*                                                                               
         L     R2,AFLIFLD          VALIDATE FLI (OPTIONAL)                      
         L     R1,MYDFLI                                                        
         BAS   RE,OVERYTD                                                       
         MVC   TACWSFLI,FULL       SAVE FLI IN ELEMENT                          
*                                                                               
         BAS   RE,EDIT10           DISPLAY FLI BACK                             
         BAS   RE,ADDDED           ADD TO TOTAL DEDUCTIONS                      
*                                                                               
BA80     L     R2,ALTAXFLD         VALIDATE LOCAL TAX (OPTIONAL)                
*                                                                               
         OC    COT,COT             IF COT DOESN'T EXIST                         
         BNZ   *+16                                                             
         CLI   5(R2),0             THEN DON'T ALLOW INPUT                       
         BNE   ERRNOINP                                                         
         B     BA100                                                            
*                                  ELSE SET WORK & TAX BIT IN COT ELEM          
         GOTOR WITHUNIT,DMCB,('TACWSWRK+TACWSTAX',COT)                          
         MVI   ELCODE,TACWELQ      R4 = A(COT WITHHOLD ELEMENT)                 
         GOTO1 GETL,DMCB,(3,COT)                                                
         BNE   BA100                                                            
         L     R4,TGELEM                                                        
*                                                                               
         L     R1,MYDLTAX                                                       
         BAS   RE,OVERYTD                                                       
         MVC   TACWTAX,FULL        SAVE LOCAL TAX IN ELEMENT                    
*                                                                               
         BAS   RE,EDIT10           DISPLAY LOCAL TAX BACK                       
         BAS   RE,ADDDED           ADD TO TOTAL DEDUCTIONS                      
*                                                                               
BA100    BAS   RE,GETPDEL          R4 = A(PAYMENT DETAILS ELEMENT)              
         USING TAPDD,R4                                                         
*                                                                               
         L     R2,AMDEDFLD         VALIDATE MISC DED (OPTIONAL)                 
         XC    FULL,FULL                                                        
         CLI   5(R2),0                                                          
         BE    BA110                                                            
         GOTO1 VALIDEC                                                          
BA110    MVC   TAPDMDED,FULL       SAVE LOCAL TAX IN ELEMENT                    
         BAS   RE,EDIT10           DISPLAY MISC DED BACK                        
         BAS   RE,ADDDED           ADD TO TOTAL DEDUCTIONS                      
*                                                                               
         L     R2,ADUESFLD         VALIDATE UNION DUES (OPTIONAL)               
         XC    FULL,FULL                                                        
         CLI   5(R2),0                                                          
         BE    BA120                                                            
         GOTO1 VALIDEC                                                          
BA120    MVC   TAPDDUES,FULL       SAVE IN ELEMENT                              
         BAS   RE,EDIT10           DISPLAY DUES BACK                            
         BAS   RE,ADDDED           ADD TO TOTAL DEDUCTIONS                      
*                                                                               
         MVI   ELCODE,TAODELQ      REMOVE OLD TAOD ELEMENTS                     
         GOTO1 REMELEM                                                          
         L     R2,AMPRFLD          VALIDATE MPR FUND (OPTIONAL)                 
         MVI   BYTE,TAODTYPM       MPD FUND                                     
         BAS   RE,BLDOD            BUILD TAOD ELEMENT                           
*                                                                               
         L     R2,ACTXFLD          VALIDATE CANADA TAX                          
         CLI   5(R2),0                                                          
         BE    BAX                                                              
         GOTOR WITHUNIT,DMCB,(0,=C'CN ')  INSURE ELEMENT EXISTS                 
         MVI   ELCODE,TACWELQ                                                   
         GOTO1 GETL,DMCB,=C'CN '                                                
         BNE   BAX                                                              
         L     R4,TGELEM           THEN R4 = A(CAN WITHHELD ELEMENT)            
         USING TACWD,R4                                                         
         L     R1,MYDCTX                                                        
         BAS   RE,OVERYTD                                                       
         MVC   TACWTAX,FULL        SAVE CANADIAN TAX IN ELEMENT                 
*                                                                               
         BAS   RE,EDIT10           DISPLAY CANADIAN TAX BACK                    
         BAS   RE,ADDDED           ADD TO TOTAL DEDUCTIONS                      
*                                                                               
BAX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
* IF VALIDATING CREDIT AMOUNTS - INSURE AMOUNT IS NOT OVER YTD                  
*   R1 = AMOUNT CHECKING                                                        
*   R2 = FIELD HEADER                                                           
*   RETURN - FULL - AMOUNT FROM FIELD                                           
*                                                                               
OVERYTD  NTR1                                                                   
         XC    FULL,FULL                                                        
         CLI   5(R2),0                                                          
         BE    NO                                                               
         GOTO1 VALIDEC                                                          
*                                                                               
         TM    SOTST,TALUSRND      IF STATE REQUIRES ROUNDING                   
         BZ    OVERYTD5                                                         
         L     RE,ASTAXFLD         AND THIS IS STATE TAXES                      
         CR    R2,RE                                                            
         BNE   OVERYTD5                                                         
         BRAS  RE,VALRND           MAKE SURE WHOLE DOLLARS                      
*                                                                               
OVERYTD5 CLI   DRCRFLAG,C'C'       IF VALIDATING CREDIT AMOUNTS                 
         BNE   OYX                                                              
         L     R0,FULL                                                          
         CR    R0,R1               THEN MUST NOT BE MORE THAN YTD               
         BH    ERRINV                                                           
*                                                                               
OYX      B     YES                                                              
         SPACE 2                                                                
*        BUILD TAOD ELEMENT & ADD IT TO CHECK                                   
*        ADD THE TAOD AMOUNT TO THE TOTAL DEDUCTIONS                            
*        R2 - FIELD HEADER                                                      
*        BYTE - TAOD TYPE                                                       
*                                                                               
BLDOD    NTR1                      BUILD TAOD ELEMENT                           
         XC    FULL,FULL                                                        
         CLI   5(R2),0                                                          
         BE    BOD10                                                            
         GOTO1 VALIDEC                                                          
         OC    FULL,FULL                                                        
         BZ    BOD10                                                            
*                                                                               
         LA    R4,ELEM             BUILD TAOD ELEMENT                           
         USING TAODD,R4                                                         
         XC    ELEM,ELEM                                                        
         MVI   TAODEL,TAODELQ                                                   
         MVI   TAODLEN,TAODLNQ                                                  
         MVC   TAODTYPE,BYTE       TYPE                                         
         MVC   TAODAMT,FULL                                                     
         GOTO1 ADDELEM             ADD ELEMENT TO CHECK RECORD                  
         BAS   RE,ADDDED           ADD TO TOTAL DEDUCTIONS                      
*                                                                               
BOD10    BAS   RE,EDIT10           DISPLAY IT BACK                              
         B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE READS THE CHECK RECORD TO BE VOIDED AND MAKES SURE THE           
* THE CHECK HAS NOT BEEN CASHED OR VOIDED OR THAT THE CHECK ITSELF              
* IS A VOID CHECK.  IT BRANCHES TO ERROR ROUTINES IF THE CHECK FAILS            
* ANY OF THESE TESTS.                                                           
*                                                                               
VALVOID  NTR1                                                                   
         GOTO1 GETREC              GET CHECK RECORD INTO AIO1                   
*                                                                               
         BAS   RE,GETCDEL          R4 = A(CHECK DETAILS ELEMENT)                
         USING TACDD,R4                                                         
         TM    TACDSTAT,TACDSVOI   ERROR IF CHECK HAS BEEN VOIDED               
         BO    ERRVOI                                                           
         OC    TACDCSH,TACDCSH     ERROR IF CHECK HAS BEEN CASHED               
         BZ    VLVOID50                                                         
                                                                                
         MVI   ELCODE,TAODELQ      OK IF WIRED CHECK                            
         GOTO1 GETL,DMCB,(1,=AL1(TAODTYPW))                                     
         BNE   ERRCASH                                                          
*                                                                               
VLVOID50 BAS   RE,GETPDEL          R4 = A(PAYMENT DETAILS ELEMENT)              
         USING TAPDD,R4                                                         
         TM    TAPDADJS,TAPDADVD   ERROR IF CHECK IS A VOID CHECK               
         BO    ERRVOI                                                           
                                                                                
         L     R3,AIO              INSURE ORIGINAL INVOICE ON FILE              
         USING TLCKD,R3                                                         
         MVC   TGAGY,TLCKAGY                                                    
         MVC   TGINV,TLCKINV                                                    
         XC    TGINV,ALLFF                                                      
         MVC   FILENAME,SVSYSDIR                                                
         GOTO1 RECVAL,DMCB,TLINCDQ,0                                            
         BNE   ERRNOINV                                                         
         XC    FILENAME,FILENAME                                                
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE INITIALIZES THE CHECK RECORD BEING BUILT BY EITHER               
* CHECK/ISSUE, TAX/REFUND, OR TAX/TRANSFER.  FOR MODE VALKEY, IT                
* ONLY INITIALIZES THE KEY OF THE RECORD.  FOR CHECK ISSUE, IT ONLY             
* ADDS CHECK DETAILS AND CAST DETAILS ELEMENTS.  FOR ACTIONS REFUND             
* ADD TRANSFER, IT GOES ON TO VALIDATE THE EMPLOYER AND SSN AND THEN TO         
* ADD PAYMENT DETAILS AND DUMMY ORIGINAL INVOICE ELEMENTS.  FINALLY, IT         
* SETS THE CODE AND SSN IN THE RECORD'S KEY.                                    
*                                                                               
INITCHK  NTR1                                                                   
         L     RF,AIO              INITIALIZE IOAREA WITH NULL KEY              
         LH    RE,DATADISP                                                      
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    0(0,RF),0(RF)                                                    
*                                                                               
         CLI   MODE,VALKEY         IF MODE VALKEY THEN DONE                     
         BE    ICX                                                              
*                                                                               
         LA    R4,ELEM             BUILD CHECK DETAILS ELEMENT                  
         USING TACDD,R4                                                         
         XC    ELEM,ELEM                                                        
         MVI   TACDEL,TACDELQ                                                   
         MVI   TACDLEN,TACDLNQ                                                  
*                                                                               
         GOTO1 ADDELEM             ADD ELEMENT TO CHECK RECORD                  
*                                                                               
         LA    R4,ELEM             BUILD CAST DETAILS ELEMENT                   
         USING TACAD,R4                                                         
         XC    ELEM,ELEM                                                        
         MVI   TACAEL,TACAELQ                                                   
         MVI   TACALEN,TACALNQ                                                  
*                                                                               
         GOTO1 ADDELEM             ADD ELEMENT TO CHECK RECORD                  
*                                                                               
         CLI   ACTNUM,ACTISS       IF ACTION ISSUE THEN DONE                    
         BE    ICX                                                              
*                                                                               
         BRAS  RE,VALEMP           READ EMP RECORD FOR TAXABLE                  
         BRAS  RE,VALSSN           BUILD CW ELEMENT BLOCK IN CHECK              
*                                                                               
         LA    R4,ELEM             BUILD PAYMENT DETAILS ELEMENT                
         USING TAPDD,R4                                                         
         XC    ELEM,ELEM                                                        
         MVI   TAPDEL,TAPDELQ                                                   
         MVI   TAPDLEN,TAPDLNQ                                                  
*                                                                               
         MVC   TAPDEMP,TGEMP       SAVE EMPLOYER IN ELEMENT                     
         MVC   TAPDW4TY,W4TYPE     SET PAYMENT TYPE TO W4TYPE                   
         CLI   TGCUR,C'C'          IF CANADIAN DOLLARS                          
         BNE   *+8                                                              
         OI    TAPDSTAT,TAPDSCAN   SET BIT                                      
         CLI   TGCUR,C'E'          IF EUROS                                     
         BNE   *+8                                                              
         OI    TAPDPST2,TAPDPEUR   SET BIT                                      
         MVI   TAPDOFF,C'1'        DEFAULT TO OFFICE 1                          
         CLC   =C'PP ',TGEMP       IF EMPLOYER IS PP                            
         BNE   *+8                                                              
         MVI   TAPDOFF,C'A'        DEFAULT TO OFFICE A                          
         CLC   =C'P+ ',TGEMP       IF EMPLOYER IS P+                            
         BNE   *+8                                                              
         MVI   TAPDOFF,C'V'        DEFAULT TO OFFICE V                          
         GOTO1 ADDELEM             ADD ELEMENT TO CHECK RECORD                  
*                                                                               
         CLI   TGCUR,C'E'          IF EUROS                                     
         BNE   IC10                                                             
         MVI   TAPDEL,TAEUELQ      EURO ELEMENT                                 
         GOTO1 ADDELEM             ADD ELEMENT TO CHECK RECORD                  
*                                                                               
IC10     L     R3,AIO              SET CODE AND SSN IN CHECK KEY                
         USING TLCKD,R3                                                         
         MVI   TLCKCD,TLCKCDQ                                                   
         MVC   TLCKSSN,TGSSN                                                    
*                                                                               
ICX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE VALIDATES THE AGENCY AND INVOICE, ADDS AN ORIGINAL               
* AGY/INV ELEMENT TO THE CHECK RECORD, AND READS THE ORIGINAL INVOICE           
* RECORD INTO AIO2 FOR LATER.                                                   
*                                                                               
VALAI    NTR1                                                                   
         BRAS  RE,SETTAL           SET SYSDIR/SYSFIL FOR NON-CHECKS             
*                                                                               
         MVC   AIO,AIO3            VALIDATE AGENCY                              
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'0A',SCKAGYH),SCKAGYNH                     
         MVC   AIO,AIO1                                                         
*                                                                               
         LA    R2,SCKINVH          VALIDATE INVOICE NUMBER                      
         GOTO1 ANY                                                              
         GOTO1 TINVCON,DMCB,SCKINV,TGINV,DATCON                                 
         CLI   0(R1),X'FF'                                                      
         BE    ERRINV                                                           
*                                                                               
         MVC   AIO,AIO2            READ INVOICE RECORD INTO AIO2                
         XC    TGINV,ALLFF                                                      
         GOTO1 RECVAL,DMCB,TLINCDQ,(X'A0',TGINV)                                
         BNE   ERREND              ERROR IF NOT FOUND                           
*                                                                               
         XC    TGINV,ALLFF         RESET GLOBAL INVOICE NUMBER                  
*                                                                               
         BAS   RE,GETINEL          R4 = A(INVOICE STATUS ELEMENT)               
         USING TAIND,R4                                                         
*                                                                               
         TM    TAINSTAT,TAINSPAY   ERROR IF NOT PAID                            
         BZ    ERRPAY                                                           
*                                                                               
         BAS   RE,GETPDEL          GET PD ELEMENT                               
         MVC   PDELEM,0(R4)        COPY PD ELEMENT TO LOCAL STORAGE             
*                                                                               
         LA    R4,PDELEM           R4 = A(PDELEM)                               
         USING TAPDD,R4                                                         
         XC    TAPDAMTS(TAPDAMTL),TAPDAMTS                                      
         XC    TAPDTXNW(L'TAPDTXNW*2),TAPDTXNW                                  
*                                                                               
         XC    TAPDACDE,TAPDACDE   CLEAR APPLIED CODE                           
         XC    TAPDICDE,TAPDICDE         INCLUDE CODE                           
         NI    TAPDSTAT,TAPDSCAN         ALL STATUS EXCEPT CAN$                 
         XC    TAPDPSTS,TAPDPSTS         PAYMENT STATUS                         
         XC    TAPDOPTS,TAPDOPTS         PAYMENT OPTIONS                        
         MVI   TAPDOPT4,0          CLEAR 4TH PAYMENT OPTION                     
*                                                                               
         MVI   TGCUR,C'C'          IF CANDIAN DOLLAR BIT SET THEN               
         TM    TAPDSTAT,TAPDSCAN       SET CURRENCY TO 'C'                      
         BO    VAI10                                                            
         MVI   TGCUR,C'E'          IF EUROS BIT SET THEN                        
         TM    TAPDPST2,TAPDPEUR       SET CURRENCY TO 'E'                      
         BO    VAI10                                                            
         MVI   TGCUR,C'U'          ELSE SET CURRENCY TO 'U'                     
*                                                                               
VAI10    MVC   TAPDW4TY,PAYTYPE    SAVE PAYMENT TYPE IN PDELEM                  
*                                                                               
         MVC   TGEMP,TAPDEMP       SAVE EMPLOYER IN GLOBAL STORAGE              
         MVC   TGCOM,TAPDCOM            INTERNAL COMMERCIAL NUMBER              
*                                                                               
         MVC   AIO,AEMPIO          READ EMPLOYER RECORD                         
         GOTO1 RECVAL,DMCB,TLEMCDQ,(X'A0',TGEMP)                                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BRAS  RE,SETCHK           RESET SYSDIR/SYSFIL FOR CHECKS               
         MVC   AIO,AIO1            RESET AIO TO AIO1                            
*                                                                               
         MVC   ELEM(TAPDLNQ),PDELEM ADD PD ELEMENT TO CHECK RECORD              
         GOTO1 ADDELEM                                                          
*                                                                               
         LA    R4,OIELEM           BUILD ORIGINAL AGENCY/INVOICE ELEM           
         USING TAOID,R4                IN OIELEM                                
         XC    OIELEM,OIELEM                                                    
         MVI   TAOIEL,TAOIELQ                                                   
         MVI   TAOILEN,TAOILNQ                                                  
         MVC   TAOIAGY,TGAGY                                                    
         MVC   TAOIINV,TGINV                                                    
*                                                                               
         MVI   ELCODE,TAOIELQ      REMOVE OLD ORIGINAL AGY/INV ELEMENT          
         GOTO1 REMELEM                                                          
*                                  ADD OI ELEMENT TO CHECK RECORD               
         MVC   ELEM(TAOILNQ),OIELEM                                             
         GOTO1 ADDELEM                                                          
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE VALIDATES THE PAYMENT TYPE AND SAVES IT IN LOCAL STORAGE         
* FOR LATER.                                                                    
*                                                                               
VALPTYP  NTR1                                                                   
         LA    R2,SCKTYPEH         VALIDATE PAYMENT TYPE                        
         GOTO1 ANY                                                              
*                                                                               
         MVI   PAYTYPE,TAW4TYIN    IF 'I' ENTERED THEN SET TO                   
         CLI   8(R2),C'I'              INDIVIDUAL                               
         BE    VPTYP10                                                          
*                                                                               
         MVI   PAYTYPE,TAW4TYCO    ELSE IF 'C' ENTERED THEN SET TO              
         CLI   8(R2),C'C'              CORPORATION                              
         BE    VPTYP10                                                          
*                                                                               
         MVI   PAYTYPE,TAW4TYCA    ELSE IF 'A' ENTERED THEN SET TO              
         CLI   8(R2),C'A'              CANADIAN                                 
         BE    VPTYP10                                                          
*                                                                               
         MVI   PAYTYPE,TAW4TYTR    ELSE IF 'T' ENTERED THEN SET TO              
         CLI   8(R2),C'T'              CORPORATION                              
         BE    VPTYP10                                                          
*                                                                               
         MVI   PAYTYPE,TAW4TYES    ELSE IF 'E' ENTERED THEN SET TO              
         CLI   8(R2),C'E'              ESTATE                                   
         BE    VPTYP10                                                          
*                                                                               
         MVI   PAYTYPE,TAW4TYFO    ELSE IF 'F' ENTERED THEN SET TO              
         CLI   8(R2),C'F'              FOREIGNER                                
*                                                                               
         BNE   ERRINV              ELSE INVALID INPUT FIELD                     
*                                                                               
VPTYP10  CLI   PAYTYPE,TAW4TYCO    IF PAYMENT TYPE IS NOT CORPORATION           
         BE    VPTYPX                                                           
         CLI   PAYTYPE,TAW4TYCA    OR CANADIAN                                  
         BE    VPTYPX                                                           
         CLI   PAYTYPE,TAW4TYTR    OR TRUSTEE                                   
         BE    VPTYPX                                                           
         CLC   PAYTYPE,W4TYPE      THEN MUST BE SAME AS W4 TYPE                 
         BNE   ERRINV                                                           
*                                                                               
VPTYPX   B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE VALIDATES CHECK DETAILS AND ADDS A CHECK DETAILS ELEMEMT         
* TO THE CHECK RECORD.                                                          
*                                                                               
VALCD    NTR1                                                                   
         BAS   RE,GETCDEL          R4 = A(CHECK DETAILS ELEMENT)                
         USING TACDD,R4                                                         
*                                                                               
         MVC   TACDCHK,TGCHK       SAVE CHECK NUMBER IN ELEMENT                 
*                                                                               
         LA    R2,SCKFREQH         VALIDATE PAYMENT FREQUENCY                   
*                                                                               
         MVI   TACDFREQ,0          IF NOTHING ENTERED THEN SET TO ZERO          
         CLI   5(R2),0                                                          
         BE    VCD20                                                            
*                                                                               
         CLI   8(R2),C'W'          ELSE MUST BE 'W', 'M', OR 'Q'                
         BE    VCD10                                                            
         CLI   8(R2),C'M'                                                       
         BE    VCD10                                                            
         CLI   8(R2),C'Q'                                                       
         BNE   ERRINV                                                           
*                                                                               
VCD10    MVC   TACDFREQ,8(R2)      SAVE FREQUENCY IN ELEMENT                    
*                                                                               
VCD20    LA    R2,SCKBNKH          VALIDATE BANK CODE                           
*                                                                               
         CLC   TGEMP,=C'DM '       IGNORE IF EMPLOYER DM                        
         BE    VCD30                                                            
         MVC   TACDBNK,TGUSBNK     DEFAULT TO U.S. BANK                         
         CLI   TGCUR,C'C'          UNLESS CAN$                                  
         BNE   *+10                                                             
         MVC   TACDBNK,TGCNBNK     DEFAULT TO CAN. BANK                         
         CLI   TGCUR,C'E'          UNLESS EUROS                                 
         BNE   *+10                                                             
         MVC   TACDBNK,TGEUBNK     DEFAULT TO EURO BANK                         
         CLC   TGEMP,=C'P+ '                                                    
         BNE   *+10                                                             
         MVC   TACDBNK,TGLCBNK     DEFAULT TO PAYROL PLUS BANK                  
*                                                                               
         CLI   5(R2),0                                                          
         BE    VCD30                                                            
*                                                                               
         GOTO1 ANY                                                              
         CLI   TGCUR,C'U'          US CURRENCY?                                 
         BNE   VCD25                                                            
         CLC   TGUSBNK,WORK        CHECK AGAINST US BANK                        
         BNE   ERRINV                                                           
         B     VCD29                                                            
*                                                                               
VCD25    CLI   TGCUR,C'C'          CANADIAN CURRENCY?                           
         BNE   VCD26                                                            
         CLC   TGCNBNK,WORK        CHECK AGAINST CAN BANK                       
         BNE   ERRINV                                                           
         B     VCD29                                                            
*                                                                               
VCD26    CLC   TGEUBNK,WORK        ELSE CHECK AGAINST EURO BANK                 
         BNE   ERRINV                                                           
*                                                                               
VCD29    MVC   TACDBNK,WORK        SAVE IN ELEMENT                              
*                                                                               
VCD30    DS    0H                                                               
*                                                                               
VCDX     B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE VALIDATES THE AGENT IF THE USER INPUT ONE.                       
*                                                                               
VALNCDE  NTR1                                                                   
         CLI   SCKNCDEH+5,0        IF NO INPUT THEN RETURN                      
         BE    VNCDEX                                                           
*                                                                               
         MVC   FILENAME,SVSYSDIR   SET TO READ TALDIR                           
*                                                                               
*                                  VALIDATE AGENT CODE                          
         GOTO1 RECVAL,DMCB,TLANCDQ,SCKNCDEH                                     
*                                                                               
         XC    FILENAME,FILENAME   RESET TO READ CHKDIR                         
*                                                                               
         BAS   RE,GETCAEL          R4 = A(CAST DETAILS ELEMENT)                 
         USING TACAD,R4                                                         
*                                                                               
         GOTO1 TRNSAGT,DMCB,(X'80',TGAGT),TACANCDE                              
*                                                                               
VNCDEX   B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE SEARCHES THE CAST LIST FOR THE FIRST MEMBER WITH THE             
* SAME SSN AS ENTERED FOR THIS CHECK.  IF IT FINDS NONE, THEN IT GIVES          
* AND ERROR MESSAGE.  OTHERWISE, IT SAVES THE PERFORMER'S CATEGORY AND          
* CAST SORT KEY IN LOCAL STORAGE.                                               
*                                                                               
VALCAST  NTR1                                                                   
         LA    R2,SCKSSNH          R2 = A(PERFORMER SSN FIELD)                  
*                                                                               
         BRAS  RE,SETTAL           SET SYSDIR/SYSFIL FOR NON-CHECKS             
         MVC   TGCSORT,ALLFF       SET DEFAULT CAST SORT KEY IN GLOBAL          
         MVC   TGCAT,=C'ZZZ'       SET DEFAULT CATEGORY IN GLOBAL               
         XC    SVCADTL,SVCADTL     CLEAR CAST DETAILS UNION/LOCAL/YEAR          
*                                                                               
         LA    R3,KEY              BUILD KEY OF FIRST CAST MEMBER               
         USING TLCAD,R3                                                         
         XC    KEY,KEY                                                          
         MVI   TLCACD,TLCACDQ                                                   
         MVC   TLCACOM,TGCOM                                                    
*                                                                               
         GOTO1 HIGH                READ FOR FIRST CAST MEMBER                   
*                                                                               
*                                  IF NO LONGER SAME CAST                       
VCAST10  CLC   KEY(TLCASORT-TLCAD),KEYSAVE                                      
         BNE   VCAST30             THEN EXIT                                    
*                                                                               
         CLC   TLCASSN,CHKSSN      IF SAME SSN AS CHECK THEN FOUND IT           
         BE    VCAST20                                                          
*                                                                               
         GOTO1 SEQ                 READ NEXT CAST MEMBER                        
         B     VCAST10             LOOP BACK                                    
*                                                                               
VCAST20  MVC   TGCSORT,TLCASORT    SAVE CAST SORT KEY IN GLOBAL                 
         MVC   TGCAT,TLCACAT            CATEGORY                                
*                                                                               
         MVC   AIO,AIO3            USE AIO3 FOR RECORD READS                    
         GOTO1 GETREC              GET CAST RECORD                              
         BAS   RE,GETCAEL          R4 = A(CAST DETAILS ELEMENT)                 
         MVC   AIO,AIO1            RESET IOAREA                                 
         USING TACAD,R4                                                         
         MVC   SVCAUN,TACAUN       SAVE UNION                                   
         MVC   SVCALOCL,TACALOCL   SAVE LOCAL                                   
         MVC   SVCAYEAR,TACAYEAR   SAVE YEAR                                    
*                                                                               
VCAST30  BRAS  RE,SETCHK           RESET SYSDIR/SYSFIL FOR CHECKS               
         B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE VALIDATES THE CHECK AMOUNTS AND SAVES THEM IN THE CHECK          
* DETAILS ELEMENT IN THE CHECK RECORD.                                          
*                                                                               
VALAMNT  NTR1                                                                   
         BAS   RE,GETCDEL          R4 = A(CHECK DETAILS ELEMENT)                
         USING TACDD,R4                                                         
*                                                                               
         LA    R2,SCKGRSH          IF PAYMENT TYPE IS INDIVIDUAL THEN           
         CLI   PAYTYPE,TAW4TYIN        GET EARNINGS FROM TAXABLE GROSS          
         BE    *+16                                                             
         CLI   PAYTYPE,TAW4TYES    OR ESTATE                                    
         BE    *+8                                                              
         LA    R2,SCKNTAXH         ELSE GET EARNINGS FROM NON-TAXABLE           
*                                                                               
         GOTO1 VALIDEC             VALIDATE EARNINGS                            
         MVC   EARNINGS,FULL       SAVE IN LOCAL STORAGE                        
*                                                                               
         LA    R2,SCKREXPH         VALIDATE REIMBURSED EXP (OPTIONAL)           
         XC    REXP,REXP                                                        
         CLI   5(R2),0                                                          
         BE    VAMNT10                                                          
         GOTO1 VALIDEC                                                          
         MVC   REXP,FULL           SAVE IN LOCAL STORAGE                        
*                                                                               
VAMNT10  LA    R2,SCKNETH          VALIDATE NET CHECK AMOUNT                    
         GOTO1 VALIDEC                                                          
         MVC   NETAMNT,FULL        SAVE IN LOCAL STORAGE                        
*                                                                               
VAMNTX   B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE VALIDATES THE MISCELLANEOUS DEDUCTIONS FIELD AND SAVES           
* THE AMOUNT IN LOCAL STORAGE.                                                  
*                                                                               
VALMDED  NTR1                                                                   
         LA    R2,SCKMDEDH         VALIDATE MISC DED (OPTIONAL)                 
         XC    MDED,MDED                                                        
         CLI   5(R2),0                                                          
         BE    VMDEDX                                                           
         GOTO1 VALIDEC                                                          
         MVC   MDED,FULL           SAVE IN LOCAL STORAGE                        
*                                                                               
         BAS   RE,ADDDED           ADD MISC DED TO TOTAL DEDUCTIONS             
VMDEDX   B     XIT                                                              
         SPACE 2                                                                
* THIS ROUTINE VALIDATES THE UNION DUES FIELD AND SAVES                         
* THE AMOUNT IN LOCAL STORAGE.                                                  
*                                                                               
VALDUES  NTR1                                                                   
         LA    R2,SCKDUEH          VALIDATE UNION DUES (OPTIONAL)               
         XC    DUES,DUES                                                        
         CLI   5(R2),0                                                          
         BE    VDUESX                                                           
         CLC   =C'ACT',SVCAUN      INPUT ALLOWED ONLY FOR ACTRA                 
         BNE   ERRINV                                                           
         GOTO1 VALIDEC                                                          
         MVC   DUES,FULL           SAVE IN LOCAL STORAGE                        
*                                                                               
         BAS   RE,ADDDED           ADD UNION DUES TO TOTAL DEDUCTIONS           
VDUESX   B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE VALIDATES THE WIRE TRANSFER FIELD AND SAVES                      
* THE AMOUNT IN LOCAL STORAGE.                                                  
*                                                                               
VALWIRE  NTR1                                                                   
         LA    R2,SCKWIREH         VALIDATE WIRE (OPTIONAL)                     
         XC    WIRE,WIRE                                                        
         CLI   5(R2),0                                                          
         BE    VWIREX                                                           
         TM    W4STA2,TAW4SWIR     MUST HAVE W4 WIRE STATUS ON                  
         BZ    ERRNOINP                                                         
         OC    NETAMNT,NETAMNT     AND THERE MUST BE NO NET INPUT               
         BNZ   ERRNOINP                                                         
         GOTO1 VALIDEC                                                          
         MVC   WIRE,FULL           SAVE IN LOCAL STORAGE                        
*                                                                               
         BAS   RE,ADDDED           ADD WIRE TO TOTAL DEDUCTIONS                 
VWIREX   B     XIT                                                              
         SPACE 2                                                                
* THIS ROUTINE VALIDATES THE DIRECT DEPOSIT FIELD AND SAVES                     
* THE AMOUNT IN LOCAL STORAGE.                                                  
*                                                                               
VALDIRCT NTR1                                                                   
         LA    R2,SCKDIRH          VALIDATE DIRECT DEP (OPTIONAL)               
         XC    DIRCT,DIRCT                                                      
         CLI   5(R2),0                                                          
         BE    VDIRCTX                                                          
         TM    W4STA2,TAW4SDD      MUST HAVE W4 DIRECT STATUS ON                
         BZ    ERRNOINP                                                         
         OC    NETAMNT,NETAMNT     AND THERE MUST BE NO NET INPUT               
         BNZ   ERRNOINP                                                         
         GOTO1 VALIDEC                                                          
         MVC   DIRCT,FULL          SAVE IN LOCAL STORAGE                        
*                                                                               
         BAS   RE,ADDDED           ADD DIRECT DEP TO TOTAL DEDUCTIONS           
VDIRCTX  B     XIT                                                              
         EJECT                                                                  
*                                                                               
* THIS ROUTINE SETS ORIGINAL LOCAL/UNION FROM INVOICE ON ISSUE CHECK            
*                                                                               
SETUNLO  NTR1                                                                   
         BAS   RE,GETCAEL          R4 = A(CAST DETAILS ELEMENT)                 
         USING TACAD,R4                                                         
*                                                                               
         MVC   TACAUN,SVCAUN       SET ORIGINAL UNION                           
         MVC   TACALOCL,SVCALOCL   SET ORIGINAL LOCAL                           
         MVC   TACAYEAR,SVCAYEAR   SET ORIGINAL YEAR                            
*                                                                               
         B     XIT                                                              
         SPACE 2                                                                
*                                                                               
* THIS ROUTINE VALIDATES CANADIAN TAX FIELD FOR CHECKS TO CORPORATIONS          
* AND IT ALSO VALIDATES CALIF WITHHOLDING FOR CORPS                             
*                                                                               
VALCAN   NTR1                                                                   
         LA    R2,SCKOSTH          R2 = A(OTHER STATE FIELD)                    
         CLI   5(R2),0             IF HAVE INPUT                                
         BE    VCAN5                                                            
         GOTO1 ANY                                                              
         CLC   =C'CN ',WORK        ALLOW ONLY UNIT CANADA                       
         BNE   ERRINV                                                           
         BAS   RE,VALOTH           GO AHEAD AND VALIDATE OTHER STATE            
         BRAS  RE,BLDPCAN                                                       
         B     VCANX                                                            
VCAN5    LA    R2,SCKSOWH          R2 = A(SOW FIELD)                            
         CLI   5(R2),0                                                          
         BE    VCANX                                                            
         XC    SOW,SOW                                                          
         GOTO1 ANY                                                              
         CLC   =C'NC ',WORK         ALLOW NORTH CAROLINA                        
         BE    *+14                                                             
         CLC   =C'CA ',WORK         AND CALIFORNIA                              
         BNE   ERRINV                                                           
         GOTO1 TAXVAL,DMCB,(3,WORK) VALIDATE TAX UNIT                           
         BNE   ERRINV                                                           
         MVC   SOW,TGTACODE         SAVE SOW IN LOCAL STORAGE                   
*                              SET WORK & TAX BIT IN WORK STATE ELEMENT         
         GOTOR WITHUNIT,DMCB,('TACWSWRK+TACWSTAX',SOW)                          
*                                                                               
         LA    R2,SCKSWTXH         R2 = A(STATE TAX FIELD)                      
         MVI   ELCODE,TACWELQ      R4 = A(STATE WITHHOLD ELEMENT)               
         GOTO1 GETL,DMCB,(3,TGTACODE)                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,TGELEM                                                        
         USING TACWD,R4                                                         
*                                                                               
         CLI   5(R2),0             VALIDATE STATE TAX (OPTIONAL)                
         BE    VCANX                                                            
         GOTO1 VALIDEC                                                          
         MVC   TACWTAX,FULL        SAVE STATE TAX IN ELEMENT                    
*                                                                               
         BAS   RE,ADDDED           ADD STATE TAX TO TOTAL DEDUCTIONS            
         MVI   STAXFLAG,C'Y'                                                    
*                                                                               
VCANX    B     XIT                                                              
         SPACE 3                                                                
* THIS ROUTINE VALIDATES THE FEDERAL TAX FIELD FOR CHECKS TO FOREIGNERS         
*                                                                               
VALFOR   NTR1                                                                   
         LA    R2,SCKFDTXH         VALIDATE FEDERAL TAX (OPTIONAL)              
*                                                                               
         MVI   ELCODE,TAODELQ      DELETE FEDERAL OTHER DED ELEMENT             
         MVI   BYTE,TAODTYPF                                                    
         GOTO1 DELL,DMCB,(1,BYTE)                                               
*                                                                               
         BAS   RE,BLDOD            BUILD OTHER DEDUCTION ELEMENT                
         B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE VALIDATES THE FEDERAL WITHHOLDINGS AND ADDS THE AMOUNTS          
* TO THE FEDERAL WITHHOLDING ELEMENT ON THE CHECK RECORD.                       
*                                                                               
VALFED   NTR1                                                                   
         LA    R2,SCKFRESH         VALIDATE 'RES?' FIELD                        
*                                                                               
         MVI   ELCODE,TACWELQ      IF FEDERAL WITHHOLD ELEMENT EXISTS           
         GOTO1 GETL,DMCB,(3,=C'FD ')                                            
         BNE   VFED20                                                           
*                                                                               
         CLI   5(R2),0             THEN IF 'RES?' FIELD IS EMPTY                
         BNE   VFED10                                                           
         MVI   SCKFRES,C'Y'        THEN PUT 'Y' IN FIELD                        
         B     VFED50                                                           
*                                                                               
VFED10   CLI   SCKFRES,C'Y'        ELSE FIELD MUST CONTAIN 'Y'                  
         BNE   ERRINV                                                           
         B     VFED50                                                           
*                                                                               
VFED20   CLI   5(R2),0             ELSE IF 'RES?' FIELD IS EMPTY                
         BNE   VFED30                                                           
         MVI   SCKFRES,C'N'        THEN PUT 'N' IN FIELD                        
         B     VFED50                                                           
*                                                                               
VFED30   MVI   SCKFRES,C'N'        ELSE FIELD MUST CONTAIN 'N'                  
         BNE   ERRINV                                                           
*                                                                               
VFED50   CLI   SCKFRES,C'N'        IF 'RES?' FIELD CONTAINS 'N'                 
         BNE   *+12                                                             
         CLI   SCKFWRK,C'N'        AND 'WORK?' FIELD CONTAINS 'N'               
         BE    VFEDX               THEN RETURN                                  
*                                                                               
         LA    R2,SCKFWRKH         VALIDATE 'WORK?' FIELD                       
         GOTO1 ANY                                                              
*                                                                               
         CLI   SCKFWRK,C'N'        IF FIELD CONTAINS 'N' THEN SKIP              
         BE    VFED60                                                           
*                                                                               
         CLI   SCKFWRK,C'Y'        ELSE FIELD MUST CONTAIN 'Y'                  
         BNE   ERRINV                                                           
*                                  GET TAX UNIT ENTRY FOR FEDERAL               
         GOTO1 TAXVAL,DMCB,(3,=C'FD ')                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                  SET WORK BIT IN FEDERAL ELEMENT              
         GOTOR WITHUNIT,DMCB,('TACWSWRK',=C'FD ')                               
*                                                                               
VFED60   MVI   ELCODE,TACWELQ      R4 = A(FEDERAL WITHHOLD ELEMENT)             
         GOTO1 GETL,DMCB,(3,=C'FD ')                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,TGELEM                                                        
         USING TACWD,R4                                                         
*                                                                               
         LA    R2,SCKFDTXH         VALIDATE FEDERAL TAX (OPTIONAL)              
         CLI   5(R2),0                                                          
         BE    VFED70                                                           
         GOTO1 VALIDEC                                                          
         MVC   TACWTAX,FULL        SAVE FEDERAL TAX IN ELEMENT                  
*                                                                               
         BAS   RE,ADDDED           ADD TAX TO TOTAL DEDUCTIONS                  
*                                                                               
VFED70   LA    R2,SCKFDFIH         VALIDATE FICA (OPTIONAL)                     
         CLI   5(R2),0                                                          
         BE    VFEDX                                                            
         GOTO1 VALIDEC                                                          
         MVC   TACWFICA,FULL       SAVE FICA IN ELEMENT                         
*                                                                               
         BAS   RE,ADDDED           ADD FICA TO TOTAL DEDUCTIONS                 
*                                                                               
VFEDX    B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE VALIDATES THE WORK STATE WITHHOLDINGS.                           
*                                                                               
VALSOW   NTR1                                                                   
         XC    SOW,SOW             PRE-CLEAR SAVED SOW                          
*                                                                               
         LA    R2,SCKSOWH          R2 = A(SOW FIELD)                            
         GOTO1 ANY                                                              
*                                  VALIDATE TAX UNIT FOR STATE OF WORK          
         GOTO1 TAXVAL,DMCB,(3,WORK)                                             
         BNE   ERRINV                                                           
         MVC   SOW,TGTACODE        SAVE SOW IN LOCAL STORAGE                    
*                                                                               
*                                  SET WORK BIT IN WORK STATE ELEMENT           
         GOTOR WITHUNIT,DMCB,('TACWSWRK',SOW)                                   
*                                                                               
         LA    R2,SCKSWTXH         R2 = A(STATE TAX FIELD)                      
         BAS   RE,VALSTATE         VALIDATE STATE WITHHOLDING FIELDS            
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE VALIDATES THE WORK CITY WITHHOLDINGS.                            
*                                                                               
VALCOW   NTR1                                                                   
         XC    COW,COW                                                          
         LA    R2,SCKCOWH          R2 = A(COW FIELD)                            
         CLI   5(R2),0                                                          
         BE    VCOWX               RETURN IF EMPTY                              
*                                                                               
         GOTO1 ANY                 VALIDATE TAX UNIT FOR CITY OF WORK           
         CLI   WORK+2,C' '         MAKE SURE IT IS REALLY A CITY!!!             
         BNH   ERRINV                                                           
         GOTO1 TAXVAL,DMCB,(3,WORK)                                             
         BNE   ERRINV                                                           
*                                                                               
         CLC   TGTASTCY,SOW        STATE OF CITY MUST = STATE OF WORK           
         BNE   ERRINV                                                           
         MVC   COW,TGTACODE        SAVE CITY OF WORK                            
*                                                                               
*                                  SET WORK BIT IN WORK CITY ELEMENT            
         GOTOR WITHUNIT,DMCB,('TACWSWRK',TGTACODE)                              
*                                                                               
         MVI   ELCODE,TACWELQ      R4 = A(WORK CITY WITHHOLD ELEMENT)           
         GOTO1 GETL,DMCB,(3,TGTACODE)                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,TGELEM                                                        
         USING TACWD,R4                                                         
*                                                                               
         LA    R2,SCKCWTXH         VALIDATE CITY TAX (OPTIONAL)                 
         CLI   5(R2),0                                                          
         BE    VCOWX                                                            
         GOTO1 VALIDEC                                                          
         MVC   TACWTAX,FULL        SAVE CITY TAX IN ELEMENT                     
*                                                                               
         BAS   RE,ADDDED           ADD CITY TAX TO TOTAL DEDUCTIONS             
*                                                                               
VCOWX    B     XIT                                                              
         EJECT                                                                  
                                                                                
* THIS ROUTINE VALIDATES THE RESIDENT STATE WITHHOLDINGS.  CURRENTLY,           
* NOTHING IS WITHHELD FOR NON-WORK STATES.                                      
*                                                                               
VALSOR   NTR1                                                                   
         XC    SOR,SOR                                                          
         LA    R2,SCKSORH          R2 = A(SOW FIELD)                            
         CLI   SCKSORH+5,0                                                      
         BNE   VALSOR10                                                         
         MVC   SCKSOR(2),W4STATE                                                
         MVI   SCKSORH+5,2                                                      
         OI    SCKSORH+6,X'80'                                                  
*                                                                               
VALSOR10 GOTO1 ANY                                                              
*                                  VALIDATE TAX UNIT FOR STATE OF WORK          
VALSOR20 GOTO1 TAXVAL,DMCB,(3,WORK)                                             
         BNE   ERRINV                                                           
         MVC   SOR,TGTACODE                                                     
*                                                                               
VSORX    B     XIT                                                              
         SPACE 3                                                                
* THIS ROUTINE VALIDATES THE OTHER STATE WITHHOLDINGS.                          
*                                                                               
VALOTH   NTR1                                                                   
         LA    R2,SCKOSTH          R2 = A(OTHER STATE FIELD)                    
         CLI   5(R2),0                                                          
         BE    VOTHX                                                            
         GOTO1 ANY                                                              
*                                  VALIDATE TAX UNIT FOR OTHER STATE            
         GOTO1 TAXVAL,DMCB,(3,WORK)                                             
         BNE   ERRINV                                                           
*                                  IF WITHHOLDING ELEM ALREADY EXISTS           
         GOTOR WITHUNIT,DMCB,(0,TGTACODE)                                       
         BE    ERRINV              GIVE ERROR                                   
*                                                                               
         LA    R2,SCKOTXH          R2 = A(STATE TAX FIELD)                      
         BAS   RE,VALSTATE         VALIDATE STATE WITHHOLDING FIELDS            
*                                                                               
VOTHX    B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE VALIDATES STATE TAX, UNEMP, DISABILITY & FLI FIELDS              
* ON ENTRY, R2 IS POINTING TO STATE TAX FIELD.                                  
*                                                                               
VALSTATE NTR1                                                                   
         MVI   ELCODE,TACWELQ      R4 = A(STATE WITHHOLD ELEMENT)               
         GOTO1 GETL,DMCB,(3,TGTACODE)                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,TGELEM                                                        
         USING TACWD,R4                                                         
*                                                                               
         CLI   5(R2),0             VALIDATE STATE TAX (OPTIONAL)                
         BE    VSTA10                                                           
         GOTO1 VALIDEC                                                          
*                                                                               
         TM    TGTASTAT,TALUSRND   IF STATE REQUIRES ROUNDING                   
         BZ    *+8                                                              
         BRAS  RE,VALRND           VALIDATE WHOLE DOLLARS                       
         MVC   TACWTAX,FULL        SAVE STATE TAX IN ELEMENT                    
         BAS   RE,ADDDED           ADD STATE TAX TO TOTAL DEDUCTIONS            
         LA    RE,SCKSWTXH         R2 = A(STATE TAX FIELD)                      
         CR    RE,R2                                                            
         BNE   *+8                                                              
         MVI   STAXFLAG,C'Y'                                                    
*                                                                               
VSTA10   CLC   TGTACODE,=C'CN '    IF STATE IS CANADA THEN DONE                 
         BE    VSTAX                                                            
*                                                                               
         BAS   RE,BUMP2            VALIDATE UNEMPLOYMENT (OPTIONAL)             
         CLI   5(R2),0                                                          
         BE    VSTA20                                                           
         GOTO1 VALIDEC                                                          
         MVC   TACWSUI,FULL        SAVE UNEMPLOYMENT IN ELEMENT                 
*                                                                               
         BAS   RE,ADDDED           ADD UNEMPLOYMENT TO TOTAL DEDUCTIONS         
*                                                                               
VSTA20   BAS   RE,BUMP2            VALIDATE DISABILITY (OPTIONAL)               
         CLI   5(R2),0                                                          
         BE    VSTA30                                                           
         GOTO1 VALIDEC                                                          
         MVC   TACWSDI,FULL        SAVE DISABILITY IN ELEMENT                   
*                                                                               
         BAS   RE,ADDDED           ADD DISABILITY TO TOTAL DEDUCTIONS           
*                                                                               
VSTA30   BAS   RE,BUMP2            VALIDATE FLI (OPTIONAL)                      
         BAS   RE,BUMP2                                                         
         BAS   RE,BUMP2                                                         
         CLI   5(R2),0                                                          
         BE    VSTAX                                                            
         GOTO1 VALIDEC                                                          
         MVC   TACWSFLI,FULL       SAVE FLI IN ELEMENT                          
*                                                                               
         BAS   RE,ADDDED           ADD DISABILITY TO TOTAL DEDUCTIONS           
*                                                                               
VSTAX    B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE VALIDATES THE DUE COMPANY WITHHOLDINGS.                          
*                                                                               
VALDUE   NTR1                                                                   
         BRAS  RE,SETTAL           SET SYSDIR/SYSFIL FOR NON-CHECKS             
*                                                                               
         XC    DUETOT,DUETOT       CLEAR TOTAL DUE COMPANY AMOUNT               
*                                                                               
         LA    R4,ELEM             BUILD SKELETON DUE COMPANY WITHHOLD          
         USING TADWD,R4                ELEMENT                                  
         XC    ELEM,ELEM                                                        
         MVI   TADWEL,TADWELQ                                                   
         MVI   TADWLEN,TADWLNQ                                                  
*                                                                               
         LA    R2,SCKDUE1H         R2 = A(FIRST DUE COMPANY CODE FIELD)         
         LA    R5,1                R5 = DUECOMP COUNTER                         
*                                                                               
VDUE10   CLI   5(R2),0             IF NOTHING ENTERED THEN DONE                 
         BE    VDUEX                                                            
         GOTO1 ANY                 GET INPUT INTO WORK                          
         CLI   5(R2),6             TEST LENGTH IS 6                             
         BNE   VK14                                                             
         MVC   DUB(2),WORK+2       TEST IF VALID DATE                           
         MVC   DUB+2(2),=C'01'     CHANGE TO M/D/Y FORMAT                       
         MVC   DUB+4(2),WORK                                                    
         GOTO1 DATVAL,DMCB,(0,DUB),WORK+6                                       
         CLI   3(R1),0                                                          
         BE    VK14                NOT VALID DATE                               
         MVC   WORK(4),WORK+6      YES - SO USE INTERNAL FORMAT                 
*                                                                               
VK14     GOTO1 RECVAL,DMCB,TLDUCDQ,(X'80',WORK)  NON-SCRN DATA                  
         BNE   ERRNOFND                                                         
         OI    4(R2),X'20'         SET DUE COMP CODE FIELD VALID                
         MVC   TADWDUC,TGDUC       SAVE DUE COMPANY CODE IN DW ELEMENT          
*                                                                               
         ZIC   R0,0(R2)            BUMP R2 TO DUE COMPANY AMOUNT FIELD          
         AR    R2,R0                                                            
*                                                                               
         CHI   R5,3                BUMP ONE MORE IF NOT THIRD AMOUNT            
         BE    VDUE20                                                           
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
VDUE20   GOTO1 VALIDEC             VALIDATE AMOUNT                              
*                                                                               
         MVC   TADWREC,FULL        SAVE AMOUNT IN DW ELEMENT                    
*                                                                               
         L     RF,DUETOT           ADD AMOUNT TO TOTAL DUE COMPANY AMNT         
         A     RF,FULL                                                          
         ST    RF,DUETOT                                                        
*                                                                               
         GOTO1 ADDELEM             ADD DW ELEMENT TO CHECK RECORD               
*                                                                               
         LA    R5,1(R5)            BUMP DUECOMP COUNTER                         
*                                                                               
         CHI   R5,4                ABORT LOOP IF ALREADY DID 3                  
         BNL   VDUEX                                                            
*                                                                               
         ZIC   R0,0(R2)            BUMP R2 TO NEXT DUECOMP CODE FIELD           
         AR    R2,R0                                                            
*                                                                               
         CHI   R5,3                BUMP ONE MORE IF NOT THIRD AMOUNT            
         BE    VDUE10                                                           
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     VDUE10              LOOP BACK                                    
*                                                                               
VDUEX    BRAS  RE,SETCHK           RESET SYSDIR/SYSFIL FOR CHECKS               
         B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE ADDS THE CHECK RECORD IN AIO1 TO THE FILE COPYING THE            
* AGENCY/INVOICE FROM GLOBAL STORAGE INTO THE KEY OF THE RECORD.  IT            
* WILL CALL ADDPTRS TO ADD THE PASSIVE POINTERS FOR THE CHECK.                  
*                                                                               
ADDCHK   NTR1                                                                   
         L     R3,AIO              R3 = A(CHECK KEY)                            
         USING TLCKD,R3                                                         
*                                                                               
         MVC   TLCKAGY,TGAGY       CHANGE AGENCY IN KEY TO ADJ AGENCY           
*                                                                               
         CLC   TLCKINV(3),=X'200703'  IF ORIGINAL CHECK BEFORE MAR 2007         
         BNL   AC05                                                             
         MVI   ELCODE,TABKELQ      REMOVE BREAKOUT DETAIL ELEMENT               
         GOTO1 REMELEM                                                          
*                                                                               
AC05     MVC   TLCKINV,TGINV       CHANGE INVOICE IN KEY TO ADJ INV             
         MVI   ELCODE,TABYELQ      REMOVE BILLING YTD ELEMENT                   
         GOTO1 REMELEM                                                          
*                                                                               
         MVI   ELCODE,TAKPELQ      REMOVE CHECK STOP ELEMENT                    
         GOTO1 REMELEM                                                          
         MVI   ELCODE,TAKLELQ      REMOVE CHECK PULL ELEMENT                    
         GOTO1 REMELEM                                                          
*                                                                               
         MVI   ELCODE,TACNELQ                                                   
         GOTO1 REMELEM                                                          
*                                                                               
         BAS   RE,GETPDEL          R4 = A(PAYMENT DETAILS ELEMENT)              
         USING TAPDD,R4                                                         
*                                                                               
         MVI   MYBYTE,0                                                         
         TM    TAPDADJS,TAPDADTR+TAPDADTT  IF TAX XFER OR REFUND                
         BZ    *+8                                                              
         MVI   MYBYTE,X'80'        SET MYBYTE FOR TAX TRANSFER/REFUND           
*                                                                               
         GOTOR GETSEQ              GET SEQ # AND SAVE IN KEY                    
*                                                                               
         MVI   TAPDADJS,0          PRE-CLEAR ADJ INDICATOR IN PAY ELEM          
*                                                                               
         CLI   ACTNUM,ACTVOID      IF ACTION VOID                               
         BNE   AC10                                                             
         OI    TAPDADJS,TAPDADVD   SET VOID INDICATOR IN PAY ELEM               
         B     AC50                                                             
*                                                                               
AC10     CLI   ACTNUM,ACTREIS      ELSE IF ACTION REISSUE                       
         BE    *+12                                                             
         CLI   ACTNUM,ACTMREI      OR IF ACTION MREISSUE                        
         BNE   AC20                                                             
         CLI   ADJTYPE,ACTVOID     THEN IF ADJUSTMENT TYPE VOID                 
         BNE   *+12                                                             
         OI    TAPDADJS,TAPDADVD   THEN SET VOID INDICATOR IN PAY ELEM          
         B     AC50                                                             
         OI    TAPDADJS,TAPDADRI   ELSE SET REIS INDICATOR IN PAY ELEM          
         B     AC50                                                             
*                                                                               
AC20     CLI   ACTNUM,ACTISS       ELSE IF ACTION ISSUE                         
         BNE   AC30                                                             
         OI    TAPDADJS,TAPDADIS   SET ISSUE INDICATOR IN PAY ELEM              
         B     AC50                                                             
*                                                                               
AC30     CLI   ACTNUM,ACTREF       ELSE IF ACTION REFUND                        
         BNE   AC40                                                             
         OI    TAPDADJS,TAPDADTR   SET TAX REFUND INDICATOR IN PAY ELEM         
         B     AC50                                                             
*                                                                               
AC40     CLI   ACTNUM,ACTTRAN      ELSE IF ACTION TRANSFER                      
         BNE   AC50                                                             
         CLI   RECNUM,TA           THEN IF RECORD TAX                           
         BNE   *+12                                                             
         OI    TAPDADJS,TAPDADTT   THEN SET TAX TRAN IND IN PAY ELEM            
         B     AC50                                                             
         OI    TAPDADJS,TAPDADST   ELSE SET SSN TRAN IND IN PAY ELEM            
*                                                                               
AC50     MVC   PDELEM,0(R4)        SAVE PAY ELEM IN PDELEM FOR INVOICE          
*                                                                               
         CLI   ACTNUM,ACTTRAN      ACTION TRANSFER                              
         BNE   AC100                                                            
         CLI   RECNUM,TA           RECORD TAX                                   
         BNE   AC100                                                            
         CLI   TGCUR,C'E'          EURO?                                        
         BNE   AC100                                                            
         BAS   RE,GETPDEUL                                                      
         MVC   1(TAPDLNQ-1,R4),PDELEM+1  UPDATE EURO PAID ELEMENT               
         MVC   PDEUELEM,0(R4)      SAVE EURO PAID ELEMENT                       
*                                                                               
         BAS   RE,GETPDEL          UPDATE US PAID ELEMENT                       
         BRAS  RE,CALCUS           CONVERT USING EURO CONV. RATE                
         MVC   PDELEM,0(R4)                                                     
*                                                                               
AC100    GOTO1 ADDREC              ADD NEW CHECK RECORD                         
*                                                                               
         L     R3,ASVPBLK          ADD PASSIVE POINTERS                         
         XC    0(255,R3),0(R3)                                                  
         GOTO1 ADDPTRS,DMCB,(8,ASVPBLK),AUPPBLK                                 
*                                                                               
ACX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE ADDS A NEW INVOICE RECORD FOR THE PAYROLL ADJUSTMENT BY          
* TAKING THE ORIGINAL INVOICE, FOUND IN AIO2, AND CHANGING ITS KEY,             
* INVOICE STATUS ELEMENT, PAY DETAILS ELEMENT, AND DUE DATE ELEMENT.            
*                                                                               
ADDINV   NTR1                                                                   
         BRAS  RE,SETTAL           SET SYSDIR/SYSFIL FOR NON-CHECKS             
         MVC   AIO,AIO2            SET AIO TO AIO2                              
*                                                                               
         L     R3,AIO              R3 = A(INVOICE KEY)                          
         USING TLIND,R3                                                         
*                                                                               
         MVC   TLINAGY,TGAGY       SAVE ADJUSTMENT AGY/INV IN KEY               
         MVC   TLININV,TGINV                                                    
         XC    TLININV,ALLFF                                                    
*                                                                               
         BAS   RE,GETINEL          R4 = A(INVOICE STATUS ELEMENT)               
         USING TAIND,R4                                                         
*                                                                               
         MVC   TAINIID,TWAORIG     SAVE USER ID IN ASSIGNMENT INFO              
         MVC   TAINIST,TGCTSTAF         STAFF ID                                
         MVC   TAINIDTE,TGTODAY1        DATE                                    
         TIME  DEC                      TIME                                    
         STCM  R0,14,TAINITIM                                                   
*                                                                               
         XC    TAINPINF,TAINPINF   CLEAR PAYMENT INFO                           
         XC    TAINQINF,TAINQINF         QC INFO                                
         XC    TAINBDTE,TAINBDTE         BILL DATE                              
         XC    TAINCDTE,TAINCDTE         CHECK DATE                             
         MVI   TAINSTAT,0                STATUS BYTE                            
*                                                                               
         MVI   TAINSTA2,TAINSADJ   SET INVOICE FOR PAYROLL ADJUSTMENT           
         CLI   LASTYEAR,C'Y'       IF FORCE TO LAST YEAR IS SET                 
         BNE   *+8                                                              
         OI    TAINSTA2,TAINSFRC   SET CORRES. BIT IN INV. STATUS EL.           
*                                                                               
         TM    OPTIONS,OPURGNT     IF USING URGENT OPTION                       
         BZ    *+8                                                              
         OI    TAINSTA2,TAINSURG   TURN ON URGENT INVOICE STATUS                
         DROP  R4                                                               
*                                                                               
         L     R4,AIO              R4 = A(DUE DATE ELEMENT)                     
         MVI   ELCODE,TADDELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   *+14                                                             
         USING TADDD,R4                                                         
         MVC   TADDDATE,TGTODAY1   CHANGE DATE TO TODAY'S DATE                  
         B     AI6                                                              
*                                                                               
         LA    R4,ELEM             ELSE BUILD DUE DATE ELEMENT                  
         XC    ELEM,ELEM                                                        
         MVI   TADDEL,TADDELQ                                                   
         MVI   TADDLEN,TADDLNQ                                                  
         MVC   TADDDATE,TGTODAY1   SET DATE TO TODAY'S DATE                     
         GOTO1 ADDELEM             ADD ELEMENT TO RECORD                        
*                                                                               
AI6      MVI   ELCODE,TAPDELQ      REMOVE OLD PAY ELEMENT                       
         GOTO1 REMELEM                                                          
*                                  ADD PAY ELEMENT FROM CHECK RECORD            
         MVC   ELEM(TAPDLNQ),PDELEM                                             
         TM    OPTIONS,OPURGNT     IF USING URGENT OPTION                       
         BZ    *+8                 TURN ON URGENT PAYMENT STATUS                
         OI    ELEM+TAPDOPT2-TAPDD,TAPDOURG                                     
         GOTO1 ADDELEM                                                          
*                                                                               
         CLI   ACTNUM,ACTTRAN      ACTION TRANSFER                              
         BNE   AI8                                                              
         CLI   RECNUM,TA           RECORD TAX                                   
         BNE   AI8                                                              
         CLI   TGCUR,C'E'          EURO?                                        
         BNE   AI8                                                              
         MVI   ELCODE,TAEUELQ      REMOVE OLD PAY ELEMENT                       
         GOTO1 REMELEM                                                          
*                                  ADD PAY ELEMENT FROM CHECK RECORD            
         MVC   ELEM(TAPDLNQ),PDEUELEM                                           
         GOTO1 ADDELEM                                                          
*                                                                               
AI8      MVI   ELCODE,TAOIELQ      REMOVE OLD ORIGINAL AGY/INV ELEMENT          
         GOTO1 REMELEM                                                          
*                                  ADD ORIGINAL AGY/INV ELEMENT                 
         MVC   ELEM(TAOILNQ),OIELEM                                             
         GOTO1 ADDELEM                                                          
*                                                                               
         GOTO1 ADDREC              ADD NEW INVOICE RECORD                       
*                                                                               
         L     R3,ASVPBLK          ADD PASSIVE POINTERS                         
         XC    0(255,R3),0(R3)                                                  
         GOTO1 ADDPTRS,DMCB,(8,ASVPBLK),AUPPBLK                                 
*                                                                               
         MVC   AIO,AIO1            RESTORE AIO TO AIO1                          
         BRAS  RE,SETCHK           RESET SYSDIR/SYSFIL FOR CHECKS               
         B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE HANDLES MODE VALREC FOR ACTION CHECK/CHANGE.                     
*                                                                               
CHANGE   NTR1                                                                   
         BAS   RE,GETCDEL          R4 = A(CHECK DETAILS ELEMENT)                
         USING TACDD,R4                                                         
*                                                                               
         LA    R2,SCKCSHDH         VALIDATE CASH DATE                           
         TM    TACDSTAT,TACDSVOI   IF VOID CHECK                                
         BO    CH20                CHANGE NOT ALLOWED                           
         TM    TACDSTA2,TACDSELN   OR IF EFT LIEN CHECK                         
         BO    CH20                CHANGE NOT ALLOWED                           
*                                                                               
         NI    TACDSTAT,ALL-TACDSSTA TURN OFF STALE DATED INDICATOR             
         CLC   8(5,R2),=C'STALE'   IF STALE ENTERED                             
         BNE   CH5                                                              
         OI    TACDSTAT,TACDSSTA   SET SWITCH ON                                
         XC    TACDCSH,TACDCSH     CLEAR CASHED DATE                            
         B     CH20                                                             
*                                                                               
CH5      CLC   8(3,R2),=C'N/A'     IF N/A ENTERED                               
         BNE   *+14                                                             
         XC    TACDCSH,TACDCSH     THEN CLEAR CASH DATE                         
         B     CH20                                                             
*                                                                               
         GOTO1 DTVAL,DMCB,TACDCSH  ELSE VALIDATE DATE                           
*                                                                               
CH20     OC    TACDDTE,TACDDTE     IF CHECK HASN'T BEEN WRITTEN YET             
         BNZ   CH40                                                             
         BAS   RE,GETCAEL          R4 = A(CAST DETAILS ELEMENT)                 
         USING TACAD,R4                                                         
         XC    TACANCDE,TACANCDE   CLEAR AGENT CODE                             
         BAS   RE,VALNCDE          VALIDATE AGENT CODE FIELD                    
*                                                                               
CH40     CLI   TGCTSTTY,TASTTYPP   IF PROGRAMMER                                
         BNE   CHX                                                              
*                                  ALLOW CHANGES TO CHECK COMMENT               
         GOTO1 NAMIN,DMCB,TACMELQ,(X'80',SCKCCOMH),TACMTYPC                     
CHX      GOTO1 ACTVIN,DMCB,0                                                    
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*              ROUTINE TO SET SYSDIR/SYSFIL FOR NON-CHECKS                      
         SPACE                                                                  
SETTAL   MVC   SYSDIR,=CL8'TALDIR'                                              
         MVC   SYSFIL,=CL8'TALFIL'                                              
         BR    RE                                                               
         SPACE 2                                                                
*              ROUTINE TO SET SYSDIR/SYSFIL TO CHECKS                           
         SPACE                                                                  
SETCHK   MVC   SYSDIR,=CL8'CHKDIR'                                              
         MVC   SYSFIL,=CL8'CHKFIL'                                              
         BR    RE                                                               
         EJECT                                                                  
* THIS ROUTINE EDITS THE AMOUNT IN FULL FOR 10 TO THE FIELD POINTED TO          
* BY R2 AND THEN TRANSMITS IT.  ITS TWO ENTRY POINTS DIFFER IN THAT THE         
* FIRST WILL DISPLAY ZERO AMOUNTS AS BLANKS RATHER THAN .00.                    
*                                                                               
EDIT10   DS    0H                                                               
         XC    8(10,R2),8(R2)                                                   
         OC    FULL,FULL                                                        
         BZ    EDIT10X                                                          
EDIT10F  DS    0H                                                               
         EDIT  (4,FULL),(10,8(R2)),2,FLOAT=-                                    
EDIT10X  OI    6(R2),X'80'                                                      
         BR    RE                                                               
         SPACE 2                                                                
* THIS ROUTINE ADDS THE DEDUCTION AMOUNT IN FULL TO THE TOTDED.                 
*                                                                               
ADDDED   L     RF,TOTDED           ADD FULL TO TOTDED                           
         A     RF,FULL                                                          
         ST    RF,TOTDED                                                        
         BR    RE                                                               
         SPACE 2                                                                
* THIS ROUTINE POINTS R4 TO THE CHECK DETAILS ELEMENT IN AIO.                   
*                                                                               
GETCDEL  LR    R1,RE               USE R1 TO RETURN                             
*                                                                               
         L     R4,AIO              R4 = A(CHECK DETAILS ELEMENT)                
         MVI   ELCODE,TACDELQ                                                   
         B     ALLELS                                                           
         SPACE 2                                                                
* THIS ROUTINE POINTS R4 TO THE PAYMENT DETAILS ELEMENT IN AIO.                 
*                                                                               
GETPDEL  LR    R1,RE               USE R1 TO RETURN                             
*                                                                               
         L     R4,AIO              R4 = A(PAYMENT DETAILS ELEMENT)              
         MVI   ELCODE,TAPDELQ                                                   
         B     ALLELS                                                           
         SPACE 2                                                                
* THIS ROUTINE POINTS R4 TO THE PAYMENT DETAILS ELEMENT IN AIO (EURO).          
*                                                                               
GETPDEUL LR    R1,RE               USE R1 TO RETURN                             
*                                                                               
         L     R4,AIO              R4 = A(PAYMENT DETAILS ELEMENT)              
         MVI   ELCODE,TAEUELQ                                                   
         B     ALLELS                                                           
         SPACE 2                                                                
* THIS ROUTINE POINTS R4 TO THE INVOICE STATUS ELEMENT IN AIO.                  
*                                                                               
GETINEL  LR    R1,RE               USE R1 TO RETURN                             
*                                                                               
         L     R4,AIO              R4 = A(INVOICE STATUS ELEMENT)               
         MVI   ELCODE,TAINELQ                                                   
         B     ALLELS                                                           
         SPACE 2                                                                
* THIS ROUTINE POINTS R4 TO THE CAST DETAILS ELEMENT IN AIO.                    
*                                                                               
GETCAEL  LR    R1,RE               USE R1 TO RETURN                             
*                                                                               
         L     R4,AIO              R4 = A(CAST DETAILS ELEMENT)                 
         MVI   ELCODE,TACAELQ                                                   
         B     ALLELS                                                           
         SPACE 2                                                                
* ALL ELEMENT FINDING ROUTINES END UP HERE TO GET ELEMENT AND DIE IF            
* NOT FOUND.                                                                    
*                                                                               
ALLELS   BRAS  RE,GETEL                                                         
         BER   R1                                                               
         DC    H'0'                                                             
         EJECT                                                                  
* LOCAL ERROR/EXIT ROUTINES                                                     
*                                                                               
ERRAGY   MVI   ERROR,ERAGYERR      CAN'T ASSIGN NEW NUMBER - AGY ERROR          
         B     ERREND                                                           
*                                                                               
ERREXIST MVI   ERROR,RECEXIST      RECORD ALREADY EXISTS                        
         B     ERREND                                                           
*                                                                               
ERRMISS  MVI   ERROR,MISSING       MISSING INPUT FIELD                          
         B     ERREND                                                           
*                                                                               
ERRNUM   MVI   ERROR,NOTNUM        INVALID NUMERIC INPUT                        
         B     ERREND                                                           
*                                                                               
ERRCHKD  LA    R2,SCKCHKDH         CURSOR TO CHECK DATE FIELD                   
ERRINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     ERREND                                                           
*                                                                               
ERRCANL  MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     ERREND                                                           
*                                                                               
ERRNOFND MVI   ERROR,NOTFOUND      RECORD NOT FOUND                             
         B     ERREND                                                           
*                                                                               
ERRNOINP MVI   ERROR,ERNOINP       INPUT NOT ALLOWED                            
         B     ERREND                                                           
*                                                                               
ERRCASH  MVI   ERROR,ERCASHED      CHECK HAS ALREADY BEEN CASHED                
         B     ERREND                                                           
*                                                                               
ERRVOI   MVI   ERROR,ERVOIDED      CHECK HAS ALREADY BEEN VOIDED                
         B     ERREND                                                           
*                                                                               
ERRADD   MVI   ERROR,ERADDUP       CHECK AMOUNTS DON'T ADD UP                   
         B     ERREND                                                           
*                                                                               
ERRPAY   MVI   ERROR,ERNOPAY       INVOICE HAS NOT BEEN PAID                    
         B     ERREND                                                           
*                                                                               
ERRDED   MVI   ERROR,ERDEDERN      DEDUCTIONS EXCEED EARNINGS                   
         B     ERREND                                                           
*                                                                               
ERRDRCR  MVI   ERROR,ERDRCR        DEBITS DO NOT EQUAL CREDITS                  
         B     ERREND                                                           
*                                                                               
ERRNOINV MVI   ERROR,ERNOINV       ORIGINAL INVOICE NOT ON FILE                 
         B     ERREND                                                           
*                                                                               
ERREARN  MVI   ERROR,ERXEARN       ERROR TRANSFERRING EARNINGS                  
         LA    R2,STTONLYH                                                      
         B     ERREND                                                           
*                                                                               
ERRACT   MVI   ERROR,INVACT        INVALID ACTION                               
         LA    R2,CONACTH                                                       
         B     ERREND                                                           
*                                                                               
ERTTMLT  MVC   MYMSGNO,=Y(ERRTTMLT)                                             
         MVI   BLOCK,0                                                          
         MVI   MYMTYP,GTMERR       TAX TRAN N/A FOR MULTI-STATE/CITY            
         B     MSGEND                                                           
*                                                                               
ERREND   MVI   ASKED,C'N'          MAKE USER HIT PFKEY NEXT TIME                
         B     CALLEXIT                                                         
*                                                                               
MSGPFVR  CLI   ACTNUM,ACTREIS      IF ACTION REISSUE                            
         BNE   MSGPFVD                                                          
         MVI   MYMSGNO1,42         THEN SAY 'HIT PF19 TO VOID & REISS'          
         B     MSGPF                                                            
*                                                                               
MSGPFVD  MVI   MYMSGNO1,43         ELSE SAY 'HIT PF19 TO VOID'                  
         B     MSGPF                                                            
*                                                                               
MSGPFIS  MVI   MYMSGNO1,45         SAY 'HIT PF19 TO ISSUE'                      
         B     MSGPF                                                            
*                                                                               
MSGPFTR  MVI   MYMSGNO1,46         SAY 'HIT PF19 TO REFUND'                     
         B     MSGPF                                                            
*                                                                               
MSGPFTT  MVI   MYMSGNO1,47         IF RECORD SSN                                
         CLI   RECNUM,SS                                                        
         BE    MSGPF               OR OLDNET = NEWNET                           
         CLC   OLDNET,NETAMNT      THEN SAY 'INPUT ACCEPTED - ...'              
         BE    MSGPF                                                            
         MVI   MYMSGNO1,64         ELSE SAY 'OUT OF BALANCE - ....'             
         B     MSGPF                                                            
*                                                                               
MSGPFMR  MVI   MYMSGNO1,49         SAY 'NNNNN CHECKS WILL BE REISSUED'          
*                                                                               
         LA    R3,BLOCK            BUILD SUBSTITUTION BLOCK FOR GETTXT          
*                                                                               
         L     RF,ENDCHK           RF = NUMBER OF CHECKS TO MREISSUE            
         S     RF,STARTCHK                                                      
         LA    RF,1(RF)                                                         
*                                  EDIT NUMBER INTO SUBSTITUTION BLOCK          
         EDIT  (RF),(10,1(R3)),ALIGN=LEFT                                       
*                                                                               
         LR    R1,R0               SAVE LENGTH IN BLOCK AND BUMP TO END         
         LA    R1,1(R1)                                                         
         STC   R1,0(R3)                                                         
         AR    R3,R1                                                            
*                                                                               
         MVI   0(R3),0             MARK END OF BLOCK                            
         B     MSGPF                                                            
*                                                                               
MSGPF    MVI   ASKED,C'Y'          SET USER HAS BEEN ASKED TO HIT PF19          
         B     MSGEND                                                           
*                                                                               
GENEND   MVI   MYMSYS,X'FF'        USE GENERAL MESSAGES                         
*                                                                               
MSGEND   OI    GENSTAT2,USGETTXT   TELL GENCON TO USE GETTEXT                   
*                                                                               
CALLEXIT GOTO1 EXIT,DMCB,0         GO TO CONTROLLER ERROR ROUTINE               
*                                                                               
THEEND   MVI   ASKED,C'N'          MAKE USER HIT PFKEY NEXT TIME                
         L     RD,SAVERD           RETURN WITHOUT AN ERROR                      
         B     XIT                                                              
*                                                                               
BUMP2    ZIC   R0,0(R2)            BUMP R2 TO NEXT SCREEN FIELD                 
         AR    R2,R0                                                            
BUMP     ZIC   R0,0(R2)            BUMP R2 TO NEXT SCREEN FIELD                 
         AR    R2,R0                                                            
         BR    RE                                                               
*                                                                               
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
*                                                                               
EMPIOLNQ EQU   4000                                                             
W4IOLNQ  EQU   4000                                                             
UPBLNQ   EQU   L'TLDRREC*22+1       SAVED PASSIVE POINTER BLOCK                 
TMPLNQ   EQU   EMPIOLNQ+W4IOLNQ+UPBLNQ                                          
*                                                                               
ALLFF    DC    8X'FF'              SHOULD BE SAME LENGTH AS LONGEST             
SSNSPACE DC    CL9' '              SPACES FIELD FOR SS#                         
*                                                                               
PFTAB    DS    0C                  PF TABLE                                     
         DC    AL1(PF13X-*,13,0,0,0)                                            
         DC    CL3' ',CL8'HISTORY',CL8'DISPLAY'                                 
PF13X    EQU   *                                                                
*                                                                               
         DC    AL1(PF14X-*,14,0,0,0)                                            
         DC    CL3' ',CL8'RCHECK ',CL8'DISPLAY'                                 
PF14X    EQU   *                                                                
*                                                                               
         DC    AL1(PF15X-*,15,0,0,0)                                            
         DC    CL3' ',CL8'CHECK  ',CL8'STOP   '                                 
PF15X    EQU   *                                                                
*                                                                               
         DC    AL1(PF16X-*,16,0,(PF16X-PF16)/KEYLNQ,0)                          
         DC    CL3' ',CL8'CHECK  ',CL8'PULL   '                                 
PF16     DC    AL1(KEYTYTWA,L'SCKDADD-1),AL2(SCKDADD-T702FFD)                   
PF16X    EQU   *                                                                
*                                                                               
         DC    AL1(PF17X-*,17,0,0,0)                                            
         DC    CL3' ',CL8'CHECK2 ',CL8'DISPLAY'                                 
PF17X    EQU   *                                                                
*                                                                               
         DC    AL1(PF19X-*,19,0,0,PFTRETRN)                                     
         DC    CL3' ',CL8' ',CL8' '                                             
PF19X    EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
         SPACE 2                                                                
         LTORG                                                                  
*                                                                               
         DROP  R6,R7,R8            DROP SECONDARY BASE REGISTERS                
         EJECT                                                                  
*              ROUTINE TO VALIDATE CITY OF RESIDENCE                            
VALCOR   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*                                                                               
         XC    COR,COR                                                          
         LA    R2,SCKCORH          R2 = A(COR FIELD)                            
         CLI   5(R2),0                                                          
         BE    VCORX               RETURN IF EMPTY                              
*                                                                               
         GOTO1 ANY                 VALIDATE TAX UNIT FOR CITY OF WORK           
         CLI   WORK+2,C' '         MAKE SURE IT IS REALLY A CITY!!!             
         JNH   ERRINV                                                           
         GOTO1 TAXVAL,DMCB,(3,WORK)                                             
         JNE   ERRINV                                                           
         MVC   COR,TGTACODE                                                     
*                                                                               
         CLC   TGTASTCY,SOR        STATE OF CITY MUST = STATE OF RES            
         JNE   ERRINV                                                           
         CLC   W4CITY,COR          STATE OF CITY MUST = W4 CITY                 
         JNE   ERRINV                                                           
         MVC   COR,TGTACODE        SAVE CITY OF RES                             
*  ADDITIONAL UNIT RULES FOR RESIDENCE                                          
         CLC   =C'NYC',COR         NYC RESIDENCE TAX ONLY IF WORKED             
         BNE   *+14                OUTSIDE NYC                                  
         CLC   =C'NYC',SCKCOW                                                   
         JNE   VALCOR10                                                         
*                                                                               
         CLC   =C'PHL',COR         PHL RESIDENCE TAX ONLY IF WORKED             
         BNE   *+14                OUTSIDE PHL                                  
         CLC   =C'PHL',SCKCOW                                                   
         JNE   VALCOR10                                                         
*                                                                               
***      LA    R2,SCKCRTXH                                                      
         LA    R2,SCKCORH                                                       
         J     ERRINV                                                           
*                                  SET WORK BIT IN WORK CITY ELEMENT            
VALCOR10 GOTOR WITHUNIT,DMCB,('TACWSRES',TGTACODE)                              
*                                                                               
         MVI   ELCODE,TACWELQ      R4 = A(WORK CITY WITHHOLD ELEMENT)           
         GOTO1 GETL,DMCB,(3,TGTACODE)                                           
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R4,TGELEM                                                        
         USING TACWD,R4                                                         
*                                                                               
         LA    R2,SCKCRTXH         VALIDATE CITY TAX (OPTIONAL)                 
         CLI   5(R2),0                                                          
         JE    VCORX                                                            
         GOTO1 VALIDEC                                                          
         MVC   TACWTAX,FULL        SAVE CITY TAX IN ELEMENT                     
*                                                                               
         L     RF,TOTDED           ADD FULL TO TOTDED                           
         A     RF,FULL                                                          
         ST    RF,TOTDED                                                        
VCORX    J     XIT                                                              
                                                                                
*              ROUTINE ADDS TAODD ELEMENT TO CHECK                              
*                                 NTRY BYTE=D/W                                 
         SPACE 1                                                                
BLDISOD  NTR1  BASE=*,LABEL=*                                                   
         LA    R4,ELEM             BUILD TAOD ELEMENT                           
         USING TAODD,R4                                                         
         XC    ELEM,ELEM                                                        
         MVI   TAODEL,TAODELQ                                                   
         MVI   TAODLEN,TAODLNQ                                                  
         MVC   TAODTYPE,BYTE       TYPE                                         
         CLI   BYTE,TAODTYPD                                                    
         BNE   *+14                                                             
         MVC   TAODAMT,DIRCT                                                    
         B     *+10                                                             
         MVC   TAODAMT,WIRE                                                     
         GOTO1 ADDELEM             ADD ELEMENT TO CHECK RECORD                  
         J     XIT                                                              
         EJECT                                                                  
*              CANADIAN/EURO ADJUSTMENTS TO LASTYEAR NOT ALLOWED                
* THIS ROUTINE VALIDATES THE STATE OF TAX FIELD POINTED TO BY R2                
* AND SAVES THE SOT IN LOCAL STORAGE.                                           
*                                                                               
VKSOT    NTR1  BASE=*,LABEL=*                                                   
         GOTO1 ANY                 VALIDATE STATE OF TAX                        
*                                                                               
         CLC   =C'RET',WORK        RETURN?                                      
         JNE   VKSOT10                                                          
         CLI   CHKTRAN,C'Y'        SINGLE CHECK TRANSFER?                       
         JNE   ERRINV                                                           
         MVC   SOT,=C'RET'                                                      
         J     VKSOTX                                                           
*                                                                               
VKSOT10  GOTO1 TAXVAL,DMCB,(3,WORK)                                             
         JNE   ERRINV                                                           
*                                                                               
         CLC   TGTACODE,=C'FD '    FEDERAL NOT VALID SOT                        
         JE    ERRINV                                                           
         CLI   TGTACODE+2,C' '     CITY NOT VALID SOT                           
         JH    ERRINV                                                           
*                                                                               
         MVC   SOT,TGTACODE        SAVE SOT IN LOCAL STORAGE                    
         MVC   SOTST,TGTASTAT      SAVE SOTST IN LOCAL STORAGE                  
*                                                                               
VKSOTX   J     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE VALIDATES THE EMPLOYER FIELD POINTED TO BY AEMPFLD AND           
* AND READS THE EMPLOYER RECORD INTO TWAHOLE FOR LATER USE BY TAXABLE.          
*                                                                               
VALEMP   NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,SETTAL           SET SYSDIR/SYSFIL FOR NON-CHECKS             
*                                                                               
         MVC   AIO,AEMPIO          VALIDATE EMPLOYER FIELD                      
         GOTO1 RECVAL,DMCB,TLEMCDQ,(X'20',AEMPFLD)                              
*                                                                               
         BRAS  RE,SETCHK           RESET SYSDIR/SYSFIL FOR CHECKS               
         MVC   AIO,AIO1            RESTORE AIO TO AIO1                          
         J     XIT                                                              
         EJECT                                                                  
*                                                                               
* TEST IF P+ CANADIAN                                                           
*                                                                               
TSTPCAN  NTR1  BASE=*,LABEL=*                                                   
         CLC   TGEMP,=C'P+ '                                                    
         JNE   NO                                                               
         CLI   W4TYPE,TAW4TYCA     P+ CANADIAN?                                 
         JE    YES                                                              
         CLI   PAYTYPE,TAW4TYCA    P+ CANADIAN?                                 
         JE    YES                                                              
         J     NO                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
         USING TACAD,R4                                                         
CHGCAUNT NTR1  BASE=*,LABEL=*                                                   
                                                                                
         L     R4,AIO                                                           
         MVI   ELCODE,TACAELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   XIT                                                              
         MVC   TACAUNIT,WORK       CHANGE IT IN CAST UNIT                       
                                                                                
         J     XIT                                                              
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*        ROUTINE MAKES SURE AMOUNT IS IN WHOLE DOLLARS                          
*                                                                               
VALRND   NTR1  BASE=*,LABEL=*                                                   
         EDIT  FULL,(12,WORK),2,ZERO=NOBLANK,ALIGN=LEFT                         
         AHI   R0,-3                                                            
         LA    RE,WORK                                                          
         AR    RE,R0                                                            
         CLC   0(3,RE),=C'.00'     PENNIES MUST BE ZERO                         
         JNE   ERRNUM                                                           
         J     XIT                                                              
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* THIS ROUTINE CALLS TRACE IF THE PROGRAM IS BEING RUN OFFLINE AND              
* THE TRACE OPTION IS SET, PASSING THE PARAMETERS ALONG TO THE TRACE            
* ROUTINE.                                                                      
*                                                                               
DOTRACE  NTR1  BASE=*,LABEL=*                                                   
         CLI   OFFLINE,C'Y'        IF RUNNING OFFLINE                           
         BNE   DTX                                                              
         TM    OPTIONS,OPTRACE     AND TRACE OPTION = 'Y'                       
         BZ    DTX                                                              
*                                                                               
         GOTO1 TRACE               THEN CALL TRACE ROUTINE                      
*                                                                               
DTX      XIT1                                                                   
         LTORG                                                                  
         SPACE 2                                                                
*                                                                               
* BUILD TATU ELEMENT FOR CHECK ISSUE                                            
*                                                                               
BLDISTU  NTR1  BASE=*,LABEL=*                                                   
         CLC   TGEMP,=C'P+ '                                                    
         JNE   BLDISTUX                                                         
*                                                                               
         MVI   ELCODE,TATUELQ                                                   
         GOTO1 REMELEM                                                          
*                                                                               
         XC    ELEM,ELEM           ADD TATU                                     
         LA    RF,ELEM                                                          
         USING TATUD,RF                                                         
         MVI   TATUEL,TATUELQ                                                   
         MVI   TATULEN,TATULNQ                                                  
         MVC   TATUUNIT,=C'FD '                                                 
         MVC   TATUWAGE,EARNINGS                                                
         MVC   TATUNNWA,REXP                                                    
         GOTO1 ADDELEM                                                          
*                                                                               
         OC    SOW,SOW             ANY STATE OF WORK?                           
         JZ    BISTU10                                                          
*                                                                               
         XC    ELEM,ELEM           ADD TATU                                     
         LA    RF,ELEM                                                          
         USING TATUD,RF                                                         
         MVI   TATUEL,TATUELQ                                                   
         MVI   TATULEN,TATULNQ                                                  
         MVC   TATUUNIT,SOW                                                     
         MVC   TATUWAGE,EARNINGS                                                
         MVC   TATUNNWA,REXP                                                    
         GOTO1 ADDELEM                                                          
*                                                                               
BISTU10  OC    COW,COW             ANY CITY OF WORK?                            
         JZ    BLDISTUX                                                         
*                                                                               
         XC    ELEM,ELEM           ADD TATU                                     
         LA    RF,ELEM                                                          
         USING TATUD,RF                                                         
         MVI   TATUEL,TATUELQ                                                   
         MVI   TATULEN,TATULNQ                                                  
         MVC   TATUUNIT,COW                                                     
         MVC   TATUWAGE,EARNINGS                                                
         MVC   TATUNNWA,REXP                                                    
         GOTO1 ADDELEM                                                          
*                                                                               
BLDISTUX J     XIT                                                              
         DROP  RF                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* BUILD TATU ELEMENTS FOR SSN TRANSFER                                          
*                                                                               
         USING YTDD,R5             R5 = A(YTDTAB ENTRY)                         
BLDSTTU  NTR1  BASE=*,LABEL=*                                                   
         CLC   TGEMP,=C'P+ '                                                    
         JNE   BLDSTTUX                                                         
*                                                                               
         MVI   ELCODE,TATUELQ                                                   
         GOTO1 REMELEM                                                          
*                                                                               
         BRAS  RE,TSTPCAN          P+ CANADIAN?                                 
         JE    BSTTU10                                                          
*                                                                               
         XC    ELEM,ELEM           ADD YTD TATU ELEMENT FOR SSNTRAN             
         LA    RF,ELEM                                                          
         USING TATUD,RF                                                         
         MVI   TATUEL,TATUELQ                                                   
         MVI   TATULEN,TATULNQ                                                  
         MVC   TATUUNIT,YTDUNIT                                                 
         MVC   TATUWAGE,YTDEARN                                                 
         MVC   TATUTNWA,YTDTXRE                                                 
         MVC   TATUNNWA,YTDNTRE                                                 
         GOTO1 ADDELEM                                                          
         J     BLDSTTUX                                                         
*                                                                               
BSTTU10  XC    ELEM,ELEM           ADD CN TATU ELEMENT FOR SSNTRAN              
         LA    RF,ELEM                                                          
         USING TATUD,RF                                                         
         MVI   TATUEL,TATUELQ                                                   
         MVI   TATULEN,TATULNQ                                                  
         MVC   TATUUNIT,YTDUNIT                                                 
         MVC   TATUWAGE,YTDEARN                                                 
         L     R1,YTDTXRE                                                       
         A     R1,YTDNTRE                                                       
         STCM  R1,15,TATUNNWA                                                   
         MVC   TATUSTRE,YTDTXRE                                                 
         GOTO1 ADDELEM                                                          
*                                                                               
         LA    RF,ELEM                                                          
         USING TATUD,RF                                                         
         MVI   TATUEL,TATUELQ                                                   
         MVI   TATULEN,TATULNQ                                                  
         MVC   TATUUNIT,=C'CN '                                                 
         MVC   TATUWAGE,YTDEARN                                                 
         L     R1,YTDTXRE                                                       
         A     R1,YTDNTRE                                                       
         STCM  R1,15,TATUNNWA                                                   
         MVC   TATUSTRE,YTDTXRE                                                 
         GOTO1 ADDELEM                                                          
*                                                                               
         XC    ELEM,ELEM           ADD FD TATU ELEMENT FOR SSNTRAN              
         LA    RF,ELEM                                                          
         USING TATUD,RF                                                         
         MVI   TATUEL,TATUELQ                                                   
         MVI   TATULEN,TATULNQ                                                  
         MVC   TATUUNIT,=C'FD '                                                 
         MVC   TATUWAGE,YTDEARN                                                 
         L     R1,YTDTXRE                                                       
         A     R1,YTDNTRE                                                       
         STCM  R1,15,TATUNNWA                                                   
         MVC   TATUSTRE,YTDTXRE                                                 
         GOTO1 ADDELEM                                                          
*                                                                               
BLDSTTUX J     XIT                                                              
         DROP  RF                                                               
         LTORG                                                                  
*                                                                               
* BUILD TAAT ELEMENTS FOR SSN TRANSFER                                          
*                                                                               
         USING YTDD,R5             R5 = A(YTDTAB ENTRY)                         
BLDSTAT  NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,TSTPCAN          P+ CANADIAN?                                 
         JNE   BLDSTATX                                                         
*                                                                               
         MVI   ELCODE,TAATELQ                                                   
         GOTO1 REMELEM                                                          
*                                                                               
         XC    ELEM,ELEM           ADD YTD TAAT ELEMENT FOR SSNTRAN             
         LA    RF,ELEM                                                          
         USING TAATD,RF                                                         
         MVI   TAATEL,TAATELQ                                                   
         MVI   TAATLEN,TAATLN2Q                                                 
         MVC   TAATUNIT,YTDUNIT                                                 
         MVC   TAATTAX,YTDUTAX                                                  
         MVC   TAATPP,YTDUPP                                                    
         MVC   TAATEI,YTDUEI                                                    
         MVC   TAATPIP,YTDUPIP                                                  
         MVC   TAATCTAX,YTDCTAX                                                 
         MVC   TAATCPP,YTDCPP                                                   
         MVC   TAATCEI,YTDCEI                                                   
         MVC   TAATCPIP,YTDCPIP                                                 
*                                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
         XC    ELEM,ELEM           ADD YTD CN TAAT ELEMENT FOR SSNTRAN          
         LA    RF,ELEM                                                          
         USING TAATD,RF                                                         
         MVI   TAATEL,TAATELQ                                                   
         MVI   TAATLEN,TAATLN2Q                                                 
         MVC   TAATUNIT,=C'CN '                                                 
         MVC   TAATTAX,SVCNUTAX                                                 
         MVC   SVCNUTAX,=X'FFFFFFFF'      SET ALREADY ADDED                     
         MVC   TAATPP,YTDUPP                                                    
         MVC   TAATEI,YTDUEI                                                    
         MVC   TAATPIP,YTDUPIP                                                  
         MVC   TAATCTAX,YTDCTAX                                                 
         MVC   TAATCTAX,SVCNCTAX                                                
         MVC   SVCNCTAX,=X'FFFFFFFF'      SET ALREADY ADDED                     
         MVC   TAATCPP,YTDCPP                                                   
         MVC   TAATCEI,YTDCEI                                                   
         MVC   TAATCPIP,YTDCPIP                                                 
*                                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
         MVC   WORK(3),YTDUNIT                                                  
         BRAS  RE,CHGCAUNT         UPDATE TACAUNIT                              
*                                                                               
BLDSTATX J     XIT                                                              
         DROP  RF                                                               
         LTORG                                                                  
*                                                                               
* THIS ROUTINE ADDS AN ENTRY TO THE YTDTAB FOR THE UNIT IN THE CW ELEM          
* IF THAT UNIT IS TAXABLE.  UPON ENTRY R4 SHOULD POINT TO THE CURRENT           
* CW ELEMENT AND R5 TO THE YTDTAB ENTRY TO BE BUILT.                            
*                                                                               
ADDTAB   NTR1  BASE=*,LABEL=*                                                   
         USING TACWD,R4                                                         
         USING YTDD,R5                                                          
*                                                                               
         XC    0(YTDLNQ,R5),0(R5)  PRE-CLEAR YTD TABLE ENTRY                    
*                                                                               
         MVI   YTDSORT,1           AVOID END OF TABLE TEST = TRUE               
*                                                                               
         MVC   YTDUNIT,TACWUNIT    SAVE UNIT                                    
         MVC   YTDTAX,TACWTAX           TAX                                     
*                                                                               
         MVC   YTDEARN,MYDEARN          EARNINGS                                
         CLC   TACWUNIT,=C'FD '    IF UNIT IS 'FD'                              
         BNE   AT10                                                             
         MVC   YTDFICA,TACWFICA    SAVE FICA                                    
         MVC   YTDREXP,REXP             REIMBURSED EXPENSES                     
         B     AT20                                                             
*                                                                               
AT10     MVC   YTDSUI,TACWSUI      SAVE SUI                                     
         MVC   YTDSDI,TACWSDI           SDI                                     
         CLI   TACWLEN,TACWLN2Q                                                 
         BL    *+10                                                             
         MVC   YTDSFLI,TACWSFLI         FLI                                     
*                                                                               
         MVI   YTDSORT,2           MAKE SURE FEDERAL COMES FIRST                
*                                                                               
AT20     MVI   ELCODE,TATUELQ                                                   
         GOTO1 GETL,DMCB,(3,YTDUNIT)                                            
         BNE   ATX                                                              
*                                                                               
         L     RF,TGELEM                                                        
         USING TATUD,RF                                                         
         ICM   R0,15,TATUWAGE                                                   
         ICM   R1,15,TATUTNWA                                                   
         AR    R0,R1                                                            
         STCM  R0,15,YTDEARN                                                    
         MVC   YTDTXRE,TATUTNWA                                                 
         MVC   YTDNTRE,TATUNNWA                                                 
*                                                                               
         CLC   TATUUNIT,=C'FD '                                                 
         BNE   ATX                                                              
         MVC   MYDNTAX,TATUNNWA                                                 
*                                                                               
         CLI   W4TYPE,TAW4TYIN     INDIVIDUAL?                                  
         BE    ATX                                                              
         STCM  R0,15,MYDNTAX                                                    
*                                                                               
         DROP  RF                                                               
*                                                                               
ATX      J     XIT                                                              
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
* THIS ROUTINE VALIDATES THE CORPORATION ID IF THE PERFORMER IS AN              
* INDIVIDUAL AND THE PAYMENT TYPE IS NOT INDIVIDUAL AND ADDS A TAX ID           
* ELEMENT TO THE CHECK RECORD.                                                  
*                                                                               
VALCORP  NTR1  BASE=*,LABEL=*                                                   
         LA    R2,SCKCRPH          R2 = A(CORP ID FIELD)                        
         CLI   5(R2),0             IF NOTHING ENTERED                           
         BNE   VCORP10                                                          
*                                                                               
         CLI   W4TYPE,TAW4TYCO     THEN INPUT IS REQUIRED IF W4 TYPE            
         BE    VCORPX              IS NOT CORPORATION                           
         CLI   W4TYPE,TAW4TYCA     OR CANADIAN                                  
         BE    VCORPX                                                           
         CLI   W4TYPE,TAW4TYTR     OR TRUSTEE                                   
         BE    VCORPX                                                           
         CLI   PAYTYPE,TAW4TYCO    BUT PAYMENT TYPE IS CORPORATION              
         JE    ERRMISS                                                          
         CLI   PAYTYPE,TAW4TYCA    OR CANADIAN                                  
         JE    ERRMISS                                                          
         CLI   PAYTYPE,TAW4TYTR    OR TRUSTEE                                   
         JE    ERRMISS                                                          
         B     VCORPX                                                           
*                                                                               
VCORP10  BRAS  RE,SETTAL           SET SYSDIR/SYSFIL FOR NON-CHECKS             
         MVC   AIO,AIO3            USE AIO3 FOR RECORD READS                    
*                                                                               
         CLI   W4TYPE,TAW4TYIN     PERFORMER TYPE MUST BE INDIVIDUAL            
         JNE   ERRINV                                                           
         CLI   PAYTYPE,TAW4TYIN    PAYMENT TYPE MUST NOT BE INDIVIDUAL          
         JE    ERRINV                                                           
*                                                                               
         CLI   5(R2),L'TGPID                                                    
         BH    VCORP20                                                          
         MVC   MYPID,8(R2)         ENTERED PID                                  
         GOTO1 SSNUNPK,DMCB,MYPID,MYSSN                                         
         MVC   8(L'TGSSN,R2),MYSSN                                              
         MVI   5(R2),L'TGSSN                                                    
         B     VCORP30                                                          
*                                                                               
VCORP20  MVC   MYSSN,8(R2)         ENTERED SSN                                  
         GOTO1 SSNPACK,DMCB,MYSSN,MYPID                                         
         JNE   ERRINV                                                           
*                                  THEN VALIDATE CORP ID                        
VCORP30  GOTO1 RECVAL,DMCB,TLW4CDQ,(X'0A',SCKCRPH),SCKCRPNH                     
         MVC   8(9,R2),=C'         '                                            
         MVC   8(L'TGPID,R2),MYPID                                              
         MVI   5(R2),L'TGPID                                                    
         OI    6(R2),X'80'                                                      
*                                                                               
         L     R4,AIO              R4 = A(W4 DETAILS ELEMENT)                   
         MVI   ELCODE,TAW4ELQ                                                   
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAW4D,R4                                                         
*                                                                               
         CLI   TAW4TYPE,TAW4TYIN   W4 TYPE MUST NOT BE INDIVIDUAL               
         JE    ERRINV                                                           
*                                                                               
         BRAS  RE,SETCHK           RESET SYSDIR/SYSFIL FOR CHECKS               
         MVC   AIO,AIO1            RESET AIO TO AIO1                            
*                                                                               
         LA    R4,ELEM             BUILD TAX ID ELEMENT                         
         USING TATID,R4                                                         
         XC    ELEM,ELEM                                                        
         MVI   TATIEL,TATIELQ                                                   
         MVI   TATILEN,TATILNQ                                                  
         MVI   TATITYPE,TATITYCO                                                
         MVC   TATIID,TGSSN        SAVE CORP ID IN ELEMENT                      
         GOTO1 ADDELEM             ADD ELEMENT TO CHECK RECORD                  
*                                                                               
VCORPX   J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
                                                                                
* THIS ROUTINE UPDATES THE TAPDELQ ELEM TAPDAMTS WITH EURO CONV RATE            
*                                                                               
         USING TAPDD,R4                                                         
CALCUS   NTR1  BASE=*,LABEL=*                                                   
         LA    R4,TAPDAMTS                                                      
         LA    R3,12                                                            
*                                                                               
CALCUS10 OC    0(4,R4),0(R4)       ANY VALUE?                                   
         BZ    CALCUS20                                                         
         ICM   RF,15,0(R4)                                                      
         CVD   RF,DUB                                                           
         MP    DUB,SVECVT                                                       
         DP    DUB,=PL3'10000'                                                  
         ZAP   WORK(8),DUB(5)                                                   
         CVB   RF,WORK                                                          
         ST    RF,0(R4)                                                         
*                                                                               
CALCUS20 AHI   R4,4                                                             
         BCT   R3,CALCUS10                                                      
*                                                                               
CALCUSX  XIT1                                                                   
         EJECT                                                                  
* THIS ROUTINE SETS THE TAXABLE INDICATOR IN THE CHECK'S CHECK                  
* WITHHOLDING ELEMENTS FOR THE TAXABLE STATE AND CITY.  THE SOT AND             
* COT ARE EITHER THE SOW AND COW OR THE SOR AND COR DEPENDING ON                
* WHICH UNITS HAVE TAX IS ELEMENTS IN THE EMPLOYER RECORD.                      
*                                                                               
TAXABLE  NTR1  BASE=*,LABEL=*                                                   
         MVC   SOT,SOW             SET SOT TO SOW                               
         MVC   COT,COW                 COT TO COW                               
*                                                                               
         LA    R1,SOW                                                           
         ST    R1,DMCB                                                          
         BAS   RE,TESTSOT          IF ID ELEMENT EXISTS FOR SOW                 
         BE    TB10                                                             
*                                                                               
         LA    R1,SOR                                                           
         ST    R1,DMCB                                                          
         BAS   RE,TESTSOT          OR DOESN'T EXIST FOR SOR                     
         BNE   TB10                THEN SOT/COT ARE CORRECT                     
*                                                                               
         MVC   SOT,SOR             ELSE SET SOT TO SOR                          
         MVC   COT,COR                      COT TO COR                          
*                                                                               
TB10     L     R4,AIO              R4 = A(FIRST WITHHOLDING ELEMENT)            
         MVI   ELCODE,TACWELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   TBX                                                              
         USING TACWD,R4                                                         
*                                                                               
TB30     CLC   TACWUNIT,SOT        IF UNIT IS SOT                               
         BE    TB40                                                             
         CLC   TACWUNIT,COT        OR COT                                       
         BE    TB40                                                             
*                                                                               
         CLC   TACWUNIT,=C'FD '    OR IF UNIT IS FEDERAL                        
         BNE   TB50                                                             
         CLC   SOT,=C'CN '         AND SOT IS NOT CANADA                        
         BE    TB50                                                             
         CLC   SOT,=C'OT '         OR OTHER                                     
         BE    TB50                                                             
*                                                                               
TB40     OI    TACWSTAT,TACWSTAX   THEN SET TAX INDICATOR IN ELEMENT            
*                                                                               
TB50     BRAS  RE,NEXTEL           GET NEXT CW ELEMENT AND LOOP BACK            
         BE    TB30                                                             
*                                                                               
TBX      XIT1                                                                   
         EJECT                                                                  
* THIS ROUTINE LOOKS FOR AN UNEMPLOYMENT ID OR TAX ID ELEMENT FOR THE           
* STATE TAX UNIT PASSED IN PARAMETER 1.  IF IT FINDS EITHER, IT RETURNS         
* 'YES'.  OTHERWISE, IT RETURNS 'NO'.                                           
*                                                                               
TESTSOT  NTR1                                                                   
         L     R2,DMCB             R2 = A(STATE UNIT CODE)                      
*                                                                               
         MVC   AIO,AEMPIO          IF EMPLOYER RECORD HAS AN UNEMP              
         MVI   ELCODE,TATIELQ          ELEMENT FOR STATE                        
         MVI   FULL,TATITYUN                                                    
         MVC   FULL+1(3),0(R2)                                                  
         GOTO1 GETL,DMCB,(4,FULL)                                               
         BE    TSYES               THEN RETURN 'YES'                            
*                                                                               
         MVI   FULL,TATITYTX       ELSE IF IT HAS A TAX ID ELEMENT              
         GOTO1 GETL,DMCB,(4,FULL)                                               
         BE    TSYES               THEN RETURN 'YES'                            
*                                                                               
         MVC   AIO,AIO1                                                         
         LTR   RB,RB               ELSE RETURN 'NO'                             
         B     TSXIT                                                            
*                                                                               
TSYES    MVC   AIO,AIO1                                                         
         CR    RB,RB                                                            
*                                                                               
TSXIT    XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
* THIS ROUTINE CALLS SPOOL AND RETURNS.                                         
*                                                                               
CALLSPL  NTR1  BASE=*,LABEL=*                                                   
         GOTO1 SPOOL,DMCB,ASPOOLD                                               
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
* THIS ROUTINE INITIALIZES THE CHECK RECORD FOUND IN AIO1 WITH THE              
* APPROPRIATE CW ELEMENTS FOR THE PERFORMER WHOSE W4 RECORD IS FOUND            
* AT AW4IO                                                                      
*                                                                               
EXTRW4   NTR1  BASE=*,LABEL=*                                                   
         MVI   ELCODE,TACWELQ      RESET CHECK WITHHOLDING BLOCK                
         GOTO1 REMELEM                                                          
*                                                                               
         L     R4,AW4IO            R4 = A(FIRST W4 WITHHOLDING ELEMENT)         
         MVI   ELCODE,TAWHELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   EWX                 DONE IF NO ELEMENTS                          
         USING TAWHD,R4                                                         
*                                                                               
         LA    R5,ELEM             BUILD SKELETON CHECK WITHHOLD ELEM           
         USING TACWD,R5                                                         
         XC    ELEM,ELEM                                                        
         MVI   TACWEL,TACWELQ                                                   
         MVI   TACWLEN,TACWLN2Q                                                 
*                                                                               
EW10     CLC   TAWHEMP,TGTPEMP     ONLY USING RULES FOR EMPLOYER TP             
         BNE   EW90                                                             
*                                                                               
         MVC   TACWUNIT,TAWHUNIT   MOVE TAX UNIT TO CW ELEMENT                  
         MVC   TACWMST,TAWHSTAT         MARITAL STATUS                          
*                                                                               
*                                  MOVE EXEMPTIONS TO CW ELEMENT                
         EDIT  (1,TAWHEXS),(2,TACWEXS),ALIGN=RIGHT,FILL=0                       
*                                                                               
         OI    TACWSTAT,TACWSRES   SET RESIDENT BIT IN STATUS                   
*                                                                               
         GOTO1 ADDELEM             ADD CW ELEMENT TO CHECK RECORD               
*                                                                               
         CLC   TACWUNIT,=C'FD '    IF UNIT IS A STATE OR A CITY                 
         BE    EW90                                                             
         CLC   TACWUNIT,=C'CN '                                                 
         BE    EW90                                                             
*                                                                               
         CLI   TACWUNIT+2,C' '     THEN IF UNIT IS A STATE                      
         BH    EW20                                                             
*                                                                               
         MVC   SOR,TACWUNIT        THEN SET SOR                                 
         B     EW90                                                             
*                                                                               
EW20     MVC   COR,TACWUNIT        ELSE SET COR                                 
*                                                                               
EW90     BRAS  RE,NEXTEL           GET NEXT WH ELEMENT                          
         BE    EW10                REPEAT UNTIL NO MORE                         
*                                                                               
EWX      XIT1                                                                   
         DROP  R5                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* THIS ROUTINE EXTRACT SOW, COW, SOR, AND COR FROM THE CHECK RECORD.            
*                                                                               
EXTRSC   NTR1  BASE=*,LABEL=*                                                   
         XC    SOW,SOW             PRE-CLEAR SOW, COW, SOR, AND COR             
         XC    COW,COW                                                          
         XC    SOR,SOR                                                          
         XC    COR,COR                                                          
*                                                                               
         L     R4,AIO              R4 = A(FIRST CHECK WITHHOLD ELEMENT)         
         MVI   ELCODE,TACWELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   ESCX                DONE IF NONE FOUND                           
         USING TACWD,R4                                                         
*                                                                               
ESC50    CLC   TACWUNIT,=C'FD '    IF UNIT IS FEDERAL                           
         BE    ESC100              THEN SKIP                                    
*                                                                               
         CLI   TACWUNIT+2,C' '     ELSE IF UNIT IS A STATE                      
         BH    ESC70                                                            
*                                                                               
         TM    TACWSTAT,TACWSWRK   IF SOW                                       
         BZ    *+10                                                             
         MVC   SOW,TACWUNIT        THEN EXTRACT SOW                             
*                                                                               
         TM    TACWSTAT,TACWSRES   IF SOR                                       
         BZ    *+10                                                             
         MVC   SOR,TACWUNIT        THEN EXTRACT SOR                             
         B     ESC100                                                           
*                                                                               
ESC70    TM    TACWSTAT,TACWSWRK   IF COW                                       
         BZ    *+10                                                             
         MVC   COW,TACWUNIT        THEN EXTRACT COW                             
*                                                                               
         TM    TACWSTAT,TACWSRES   IF COR                                       
         BZ    *+10                                                             
         MVC   COR,TACWUNIT        THEN EXTRACT COR                             
*                                                                               
ESC100   BRAS  RE,NEXTEL           GET NEXT CW ELEM AND LOOP BACK               
         BE    ESC50                                                            
*                                                                               
ESCX     XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* WHEN COMING FROM CHECK PULL, SET CHECK KEY AND CHECK RECORD IN                
* AIO BASED ON THE DISK ADDRESS BEING PASSED IN REC/USE FIELD                   
*                                                                               
         DS    0D                                                               
FROMPULL NTR1  BASE=*,LABEL=*                                                   
         CLI   THISLSEL,PULLC2M    IF COMING FROM CHECK PULL                    
         BNE   FPNO                FIRST TURN OFF THIS                          
         MVI   THISLSEL,0          COMING FROM CHECK INDICATOR                  
*                                                                               
         LA    R2,SCKCHKH          GET THE DISK ADDRESS                         
         GOTO1 HEXIN,DMCB,SCKCHK,DSKADD,L'SCKCHK                                
         OC    DSKADD,DSKADD                                                    
         BZ    FPMISS                                                           
*                                                                               
         XC    KEY,KEY             PUT DISK ADDRESS INTO KEY                    
         LA    R4,KEY                                                           
         USING TLDRD,R4                                                         
         MVC   TLDRDA,DSKADD                                                    
         MVC   SYSDIR,=CL8'CHKDIR'                                              
         MVC   SYSFIL,=CL8'CHKFIL'                                              
         MVI   RDUPDATE,C'Y'                                                    
         OI    DMINBTS,X'80'                                                    
         GOTO1 GETREC              AND GET CHECK RECORD                         
*                                                                               
         L     R4,AIO                                                           
         MVC   KEY(32),0(R4)       AND SET CHECK KEY                            
*                                                                               
         XR    RC,RC                                                            
FPNO     LTR   RC,RC                                                            
         XIT1                                                                   
*                                                                               
FPMISS   MVI   ERROR,MISSING                                                    
         GOTO1 EXIT,DMCB,0                                                      
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* THIS ROUTINE INITIALIZES THE INVOICE RECORD IN AIO2 WHICH WILL BE             
* UPDATED AND ADDED BY ADDINV.                                                  
*                                                                               
         DS    0D                                                               
INITINV  NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,SETTAL           SET SYSDIR/SYSFIL FOR NON-CHECKS             
         MVC   AIO,AIO2            SET AIO TO AIO2                              
*                                                                               
         L     RF,AIO              INITIALIZE IOAREA WITH NULL KEY              
         LH    RE,DATADISP                                                      
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    0(0,RF),0(RF)                                                    
*                                                                               
         MVI   0(RF),TLINCDQ       SAVE RECORD CODE IN KEY                      
*                                                                               
         LA    R4,ELEM             BUILD SKELETON INVOICE STATUS ELEM           
         USING TAIND,R4                                                         
         XC    ELEM,ELEM                                                        
         MVI   TAINEL,TAINELQ                                                   
         MVI   TAINLEN,TAINLNQ                                                  
*                                                                               
         GOTO1 ADDELEM             ADD ELEMENT TO RECORD                        
*                                                                               
         LA    R4,ELEM             BUILD SKELETON DUE DATE ELEMENT              
         USING TADDD,R4                                                         
         XC    ELEM,ELEM                                                        
         MVI   TADDEL,TADDELQ                                                   
         MVI   TADDLEN,TADDLNQ                                                  
*                                                                               
         GOTO1 ADDELEM             ADD ELEMENT TO RECORD                        
*                                                                               
         BRAS  RE,SETCHK           RESET SYSDIR/SYSFIL FOR CHECKS               
         MVC   AIO,AIO1            RESET AIO TO AIO1                            
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
* THIS ROUTINE DETERMINES THE SEQUENCE NUMBER FOR THE CHECK RECORD.             
* IT GETS THE FIRST CAST PAYMENT HISTORY POINTER FOR THIS RECORD AND            
* SUBTRACTS TWO FROM ITS SEQUENCE NUMBER TO DETERMINE THE NEXT                  
* AVAILABLE ONE.  FOR TAX TRANSFER OR REFUND CHECKS (MYBYTE=X'80'),             
* USES CHECK NUMBER POINTER INSTEAD BECAUSE NO CAST PAYMENT HISTORY             
* PTR IS GENERATED.  THE SEQUENCE NUMBER IS SAVED IN THE CHECK KEY.             
*                                                                               
         DS    0D                                                               
GETSEQ   NTR1  BASE=*,LABEL=*                                                   
         LA    R4,X'FF'            IF NONE FOUND THEN SEQ = X'FF'               
*                                                                               
         L     R3,ASVPBLK          GET PASSIVE POINTERS                         
         XC    0(255,R3),0(R3)                                                  
         GOTO1 AGENPTRS,DMCB,(R3)                                               
*                                                                               
GS10     CLI   0(R3),0             IF END OF BLOCK THEN NONE FOUND              
         BE    GS30                                                             
*                                                                               
         CLI   0(R3),TLCKHCDQ      IF CAST PAYMENT HISTORY POINTER              
         BE    GS20                    THEN DONE                                
*                                                                               
         CLI   MYBYTE,X'80'        FOR TAX XFER OR REFUND CHECK                 
         BNE   GS12                                                             
         CLI   0(R3),TLCKCCDQ      IF CHECK NUMBER POINTER                      
         BE    GS15                    THEN DONE                                
*                                                                               
GS12     LA    R3,L'TLDRREC(R3)    ELSE BUMP R3 TO NEXT POINTER                 
         B     GS10                LOOP BACK                                    
*                                                                               
GS15     MVC   KEY(L'TLDRREC),0(R3)                                             
*                                                                               
         LA    R3,KEY              CLEAR SEQUENCE NUMBER AND READ HIGH          
         USING TLCKPD,R3               TO GET FIRST CHECK NUMBER                
         MVI   TLCKCSEQ,0              KEY ON FILE                              
         GOTO1 HIGH                                                             
*                                  IF NO MATCHING CHECK NUM POINTER             
GS16     CLC   KEY(TLCKCSEQ-TLCKPD),KEYSAVE                                     
         BNE   GS30                THEN NOT FOUND                               
*                                                                               
         ZIC   R4,TLCKCSEQ         R4 = LAST USED SEQ                           
*                                                                               
         CLI   TLCKCSEQ,40         PROTECT AGAINST CONVERTED RECORDS            
         BH    GS18                (SEQ MAY HAVE BEEN CONV AS NOT FF)           
         AHI   R4,2                SEQ = LAST USED + 2                          
         GOTO1 SEQ                 AND WE NEED TO CHECK FOR ANOTHER             
         B     GS16                                                             
*                                                                               
GS18     AHI   R4,-2               ELSE SEQ = LAST USED - 2                     
         B     GS30                                                             
*                                                                               
*                                  BUILD KEY WITH CAST PAY KEY DETAILS          
GS20     MVC   KEY(L'TLDRREC),0(R3)                                             
*                                                                               
         LA    R3,KEY              CLEAR SEQUENCE NUMBER AND READ HIGH          
         USING TLCKPD,R3               TO GET FIRST CAST PAYMENT                
         MVI   TLCKHCSQ,0              HISTORY KEY ON FILE                      
         GOTO1 HIGH                                                             
*                                  IF NO MATCHING CAST PAY POINTER              
GS25     CLC   KEY(TLCKHCSQ-TLCKPD),KEYSAVE                                     
         BNE   GS30                THEN NOT FOUND                               
*                                                                               
         ZIC   R4,TLCKHCSQ         R4 = LAST USED SEQ                           
*                                                                               
         CLI   TLCKHCSQ,40         PROTECT AGAINST CONVERTED RECORDS            
         BH    GS28                (SEQ MAY HAVE BEEN CONV AS NOT FF)           
         AHI   R4,2                SEQ = LAST USED + 2                          
         GOTO1 SEQ                 AND WE NEED TO CHECK FOR ANOTHER             
         B     GS25                                                             
*                                                                               
GS28     AHI   R4,-2               ELSE SEQ = LAST USED - 2                     
*                                                                               
GS30     L     R3,AIO              SAVE SEQUENCE NUMBER IN CHECK KEY            
         USING TLCKD,R3                                                         
         STC   R4,TLCKSEQ                                                       
*                                                                               
GSX      XIT1                                                                   
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
* THIS ROUTINE VALIDATES THE LIEN WITHHOLDINGS OR THE W4 TRUSTEE                
* DEDUCTIONS                                                                    
*                                                                               
         DS    0D                                                               
VALLORT  NTR1  BASE=*,LABEL=*                                                   
         MVC   SYSDIR,SVSYSDIR                                                  
         MVC   SYSFIL,SVSYSFIL                                                  
*                                                                               
         LA    R2,SCKLIENH         R2 = A(LIEN CODE FIELD)                      
         CLI   5(R2),0             IF NOTHING ENTERED THEN RETURN               
         BE    VLORTX                                                           
         GOTO1 ANY                                                              
         CLC   =C'TRUST ',WORK     IF TRUST NOT INPUT                           
         BE    VLORT10                                                          
*                                                                               
         LA    R4,ELEM             BUILD LIEN WITHHOLDING ELEMENT               
         USING TALWD,R4                                                         
         XC    ELEM,ELEM                                                        
         MVI   TALWEL,TALWELQ                                                   
         MVI   TALWLEN,TALWLNQ                                                  
*                                                                               
         GOTO1 RECVAL,DMCB,TLLNCDQ,(R2) VALIDATE AS LIEN CODE                   
         MVC   TALWLIN,TGLIN       SAVE LIEN CODE IN LW ELEMENT                 
*                                                                               
         LA    R2,SCKLAMTH         VALIDATE LIEN AMOUNT                         
         GOTO1 VALIDEC                                                          
         MVC   TALWREC,FULL        SAVE AMOUNT IN LW ELEMENT                    
         L     RF,TOTDED           ADD FULL TO TOTDED                           
         A     RF,FULL                                                          
         ST    RF,TOTDED                                                        
         GOTO1 ADDELEM             ADD LW ELEMENT TO CHECK RECORD               
         B     VLORTX                                                           
*                                                                               
VLORT10  LA    R4,ELEM             BUILD W4 TRUSTEE DEDUCTION ELEMENT           
         USING TAODD,R4                                                         
         XC    ELEM,ELEM                                                        
         MVI   TAODEL,TAODELQ                                                   
         MVI   TAODLEN,TAODLNQ                                                  
         MVI   TAODTYPE,TAODTYPT                                                
*                                                                               
         LA    R2,SCKLAMTH                                                      
         GOTO1 VALIDEC                                                          
         MVC   TAODAMT,FULL                                                     
         L     RF,TOTDED           ADD FULL TO TOTDED                           
         A     RF,FULL                                                          
         ST    RF,TOTDED                                                        
         GOTO1 ADDELEM                                                          
*                                                                               
VLORTX   MVC   SYSDIR,=CL8'CHKDIR'                                              
         MVC   SYSFIL,=CL8'CHKFIL'                                              
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
* THIS ROUTINE LOOKS FOR A WITHHOLDING ELEMENT WITH THE UNIT FOUND IN           
* PARM 1.  IF IT DOESN'T FIND IT, IT ADDS ONE AND RETURNS CC NOT EQ.            
* IF IT DOES FIND IT, IT OR'S THE STATUS BIT OF THE ELEMENT WITH THE            
* CONTENTS OF BYTE 0 OF PARM 1 AND RETURNS CC EQ.                               
*                                                                               
         DS    0D                                                               
WITHUNIT NTR1 BASE=*,LABEL=*                                                    
         LA    R4,ELEM             BUILD NEW CW ELEMENT WITH UNIT AND           
         USING TACWD,R4                STATUS                                   
         XC    ELEM(TACWLN2Q),ELEM                                              
         MVI   TACWEL,TACWELQ                                                   
         MVI   TACWLEN,TACWLN2Q                                                 
         L     R3,0(R1)                                                         
         MVC   TACWUNIT,0(R3)                                                   
         MVC   TACWSTAT,0(R1)                                                   
*                                                                               
* ONLY SET THE NON RESIDENT FOREIGNER/CORPORATION BITS                          
* IF THE STATE IN W4 IS DIFFERENT THAN THE STATE FOR THE TAX REFUND             
*                                                                               
         CLC   W4STATE,TACWUNIT                                                 
         BE    WU1                                                              
         CLI   W4TYPE,C'C'                                                      
         BNE   *+8                                                              
         OI    TACWSTAT,TACWNCRP   NON RESIDENT CORPORTATION                    
         CLI   W4TYPE,C'F'                                                      
         BNE   *+8                                                              
         OI    TACWSTAT,TACWNFOR   FOREIGNER                                    
*                                                                               
WU1      MVI   ELCODE,TACWELQ      IF ELEMENT FOR UNIT DOES NOT EXIST           
         GOTO1 GETL,DMCB,(3,0(R3))                                              
         BE    WU10                                                             
*                                                                               
         CLI   RECNUM,SS           IF NOT SSN TRANSFER                          
         BE    WU5                                                              
         CLI   EARNONLY,C'Y'       AND NOT TRANSFERRING ONLY EARNINGS           
         BE    WU5                                                              
         CLI   W4TYPE,TAW4TYFO     FOREIGNERS DON'T HAVE TXBLE EARNINGS         
         BE    WU3                                                              
         CLI   W4TYPE,TAW4TYTR     NEITHER DO TRUSTEES                          
         BE    WU4                                                              
         CLI   W4TYPE,TAW4TYCA     NEITHER TO CANADIANS                         
         BE    WU3                                                              
         CLI   W4TYPE,TAW4TYCO     NEITHER TO CORPS                             
         BNE   WU5                                                              
WU3      CLC   TACWUNIT,=C'CA '    EXCEPT FOR CALIFORNIA                        
         BE    WU5                                                              
         CLC   TACWUNIT,=C'NC '    EXCEPT FOR NORTH CAROLINA                    
         BE    WU5                                                              
WU4      CLC   TACWUNIT,=C'CN '    THEY CAN HAVE CANADIAN TAXES                 
         BE    WU4B                                                             
         OC    TACWWHLD,TACWWHLD   IF NO WITHHOLDINGS                           
         BZ    WU6                 DON'T BOTHER ADDING                          
WU4B     NI    TACWSTAT,ALL-TACWSTAX  ELSE TURN OFF TAXABLE BIT                 
*                                                                               
WU5      GOTO1 ADDELEM             THEN ADD NEW ELEMENT FOR UNIT                
WU6      B     WUNO                AND SET CC NOT EQ                            
*                                                                               
WU10     MVC   BYTE,TACWSTAT       ELSE SET REQUESTED STATUS BITS               
         L     R4,TGELEM                                                        
         OC    TACWSTAT,BYTE                                                    
*                                                                               
         CLI   EARNONLY,C'Y'       IF NOT TRANSFERRING ONLY EARNINGS            
         BE    WUX                                                              
         CLI   W4TYPE,TAW4TYFO     FOREIGNERS DON'T HAVE TXBLE EARNINGS         
         BE    WU15                                                             
         CLI   W4TYPE,TAW4TYTR     NEITHER DO TRUSTEES                          
         BE    WU15                                                             
         CLI   W4TYPE,TAW4TYCA     NEITHER DO CANADIANS                         
         BE    WU14                                                             
         CLI   W4TYPE,TAW4TYCO     NEITHER DO CORPS                             
         BNE   WUX                                                              
WU14     CLC   TACWUNIT,=C'CA '    EXCEPT FOR CALIFORNIA                        
         BE    *+8                                                              
WU15     NI    TACWSTAT,ALL-TACWSTAX                                            
*                                                                               
WUYES    XR    RC,RC                                                            
WUNO     LTR   RC,RC                                                            
WUX      XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*        ROUTINE UPDATES W4 RECORD WITH W4 TRUSTEE AMOUNT W/HELD                
         SPACE 1                                                                
         DS    0D                                                               
UPDW4    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         BRAS  RE,SETTAL           SET SYSDIR/SYSFIL FOR NON-CHECKS             
         MVI   ELCODE,TAODELQ      IF W4 TRUSTEE AMOUNT DEDUCTED                
         GOTO1 GETL,DMCB,(1,=AL1(TAODTYPT))                                     
         BNE   UPDW4X                                                           
*                                                                               
         L     R4,TGELEM                                                        
         USING TAODD,R4                                                         
         ICM   R0,15,TAODAMT       R0=AMOUNT DEDUCTED                           
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'34',0)                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,AIO                                                           
         MVI   ELCODE,TAWXELQ                                                   
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAWXD,R4                                                         
         OC    TAWXTSSN,TAWXTSSN   IF NO TRUSTEE SPECIFIED                      
         BNZ   UPDW4X                                                           
         CLC   TGEMP,=C'TP '       ADD TO AMOUNT HELD BY EMPLOYER               
         BNE   *+14                                                             
         ICM   R1,15,TAWXHTP                                                    
         AR    R1,R0                                                            
         STCM  R1,15,TAWXHTP                                                    
         CLC   TGEMP,=C'P+ '                                                    
         BNE   *+14                                                             
         ICM   R1,15,TAWXHPPL                                                   
         AR    R1,R0                                                            
         STCM  R1,15,TAWXHPPL                                                   
         CLC   TGEMP,=C'PP '                                                    
         BNE   *+14                                                             
         ICM   R1,15,TAWXHPP                                                    
         AR    R1,R0                                                            
         STCM  R1,15,TAWXHPP                                                    
         GOTO1 PUTREC                                                           
*                                                                               
UPDW4X   MVC   AIO,AIO1            RESTORE AIO TO AIO1                          
         BRAS  RE,SETCHK           RESET SYSDIR/SYSFIL FOR CHECKS               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
* THIS ROUTINE DISPLAYS THE CHECK RECORD FOUND IN AIO1 TO THE SCREEN.           
*                                                                               
         DS    0D                                                               
DISPLAY  NTR1  BASE=*,LABEL=*                                                   
         TWAXC SCKSSNH             CLEAR ALL UNPROTECTED FIELDS                 
*                                                                               
         XC    SCKMSG,SCKMSG       CLEAR RETURNED CHECK MESSAGE                 
         OI    SCKMSGH+6,X'80'                                                  
         XC    SCKSSNN,SCKSSNN     CLEAR PERFORMER NAME                         
         OI    SCKSSNNH+6,X'80'                                                 
         XC    SCKCRPN,SCKCRPN     CLEAR CORP ID NAME                           
         OI    SCKCRPNH+6,X'80'                                                 
         XC    SCKAGYN,SCKAGYN     CLEAR AGENCY NAME                            
         OI    SCKAGYNH+6,X'80'                                                 
         XC    SCKNCDN,SCKNCDN     CLEAR AGENT NAME                             
         OI    SCKNCDNH+6,X'80'                                                 
         XC    SCKAINV,SCKAINV     CLEAR ADJUSTMENT INVOICE NUMBER              
         OI    SCKAINVH+6,X'80'                                                 
         MVC   SCKOTUT,SCKSWUT     SET UNEMP. TAG IN OTH SECTION                
         OI    SCKOTUTH+6,X'80'                                                 
         MVC   SCKOTDT,SCKSWDT     SET DISAB. TAG IN OTH SECTION                
         OI    SCKOTDTH+6,X'80'                                                 
         NI    SCKFTAGH+1,X'F3'    SET TAGS TO NORMAL INTENSITY                 
         OI    SCKFTAGH+6,X'80'                                                 
         NI    SCKSWTGH+1,X'F3'                                                 
         OI    SCKSWTGH+6,X'80'                                                 
         NI    SCKSRTGH+1,X'F3'                                                 
         OI    SCKSRTGH+6,X'80'                                                 
         NI    SCKCWTGH+1,X'F3'                                                 
         OI    SCKCWTGH+6,X'80'                                                 
         NI    SCKCRTGH+1,X'F3'                                                 
         OI    SCKCRTGH+6,X'80'                                                 
         NI    SCKOTGH+1,X'F3'                                                  
         OI    SCKOTGH+6,X'80'                                                  
*                                                                               
         BRAS  RE,SETTAL           SET SYSDIR/SYSFIL FOR NON-CHECKS             
*                                                                               
         LA    R3,KEY                                                           
         USING TLDRD,R3                                                         
         GOTO1 HEXOUT,DMCB,TLDRDA,SCKDADD,L'TLDRDA                              
*                                                                               
         L     R3,AIO                                                           
         CLI   0(R3),TLDTCDQ       IF THIS IS DUE COMP TRACKING REC             
         BNE   *+14                                                             
         USING TLDTD,R3                                                         
         MVC   SCKSSN,TLDTSSN      GET SS NUMBER FROM ITS KEY                   
         B     DISP0                                                            
*                                                                               
         USING TLCKD,R3                                                         
         MVC   TGSEQ,TLCKSEQ                                                    
         MVC   SCKSSN,TLCKSSN      ELSE GET SS NUMBER FROM CHECK KEY            
*                                                                               
DISP0    MVC   AIO,AIO2            DISPLAY PERF NAME                            
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'8C',SCKSSN),SCKSSNNH                      
         GOTO1 SSNPACK,DMCB,SCKSSN,TGPID                                        
         MVC   SCKSSN,=CL9' '                                                   
         MVC   SCKSSN(L'TGPID),TGPID                                            
         MVI   SCKSSNH+5,6                                                      
*                                                                               
DISP0A   MVC   AIO,AIO1                                                         
         L     R4,AIO                                                           
         CLI   0(R4),TLDTCDQ       IF THIS IS DUE COMP TRACKING REC             
         BE    DISP26              SKIP MOST DISPLAY ITEMS                      
*                                                                               
         MVI   ELCODE,TAPDELQ      R4 = A(PAYMENT DETAILS ELEMENT)              
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAPDD,R4                                                         
*                                                                               
         CLI   TAPDADJS,0          IF THIS IS AN ADJUSTMENT                     
         BE    DISP0D                                                           
         CLI   TGCTSTTY,TASTTYPP   THEN IF PROGRAMMER                           
         BNE   DISP0B                                                           
         GOTO1 TINVCON,DMCB,TLCKINV,SCKAINV,DATCON  DISP. ADJ. INVOICE          
*                                                                               
DISP0B   GOTO1 ADJOUT,DMCB,(TAPDADJS,SCKAINV+10)  DISP. ADJUSTMENT NAME         
*                                                                               
*                                  IF TAX REFUND OR TRANSFER                    
         TM    TAPDADJS,TAPDADTR+TAPDADTT                                       
         BNZ   DISP2F              THEN SKIP TO AMOUNTS                         
*                                                                               
DISP0D   L     R4,AIO              IF TAX ID ELEMENT EXISTS                     
         MVI   ELCODE,TATIELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   DISP1                                                            
         USING TATID,R4                                                         
         MVC   SCKCRP,TATIID       THEN DISPLAY CORP ID                         
         GOTO1 SSNPACK,DMCB,SCKCRP,TGPID                                        
         MVC   SCKCRP,=CL9' '                                                   
         MVC   SCKCRP(L'TGPID),TGPID                                            
         MVI   SCKCRPH+5,6                                                      
*                                                                               
DISP1    MVC   TGAGY,TLCKAGY       SAVE AGENCY IN GLOBAL                        
         MVC   TGINV,TLCKINV            INVOICE                                 
*                                                                               
         L     R4,AIO              IF ORIGINAL AGY/INV ELEMENT EXISTS           
         MVI   ELCODE,TAOIELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   DISP2                                                            
         USING TAOID,R4                                                         
*                                                                               
         MVC   TGAGY,TAOIAGY       SAVE ORIGINAL AGY/INV IN GLOBAL              
         MVC   TGINV,TAOIINV                                                    
*                                                                               
DISP2    MVC   SCKAGY,TGAGY        DISPLAY AGENCY/INVOICE                       
         GOTO1 TINVCON,DMCB,TGINV,SCKINV,DATCON                                 
*                                                                               
         L     R4,AIO              R4 = A(CAST DETAILS ELEMENT)                 
         MVI   ELCODE,TACAELQ                                                   
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACAD,R4                                                         
*                                                                               
         OC    TACANCDE,TACANCDE   DISPLAY AGENT IF NOT ZERO                    
         BZ    DISP2D                                                           
         GOTO1 TRNSAGT,DMCB,(X'40',TACANCDE),SCKNCDE                            
*                                                                               
DISP2D   XC    TGINV,=6X'FF'       COMPLEMENT INVOICE FOR INVOICE KEY           
*                                                                               
         MVC   AIO,AIO2            MAKE SURE DON'T CREAM RECORD                 
*                                                                               
*                                  DISPLAY CORP/AGY/AGENT NAMES                 
**NO-OP  CLI   SCKCRP,C'0'                                                      
         CLI   SCKCRP,X'40'        PID/SSN                                      
         BL    DISP2E                                                           
         GOTO1 SSNUNPK,DMCB,TGPID,MYSSN                                         
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'8C',MYSSN),SCKCRPNH                       
*                                                                               
DISP2E   GOTO1 RECVAL,DMCB,TLAYCDQ,(X'8C',SCKAGY),SCKAGYNH                      
         GOTO1 RECVAL,DMCB,TLANCCDQ,(X'8C',SCKNCDE),SCKNCDNH                    
*                                                                               
*                                  GET INVOICE RECORD FOR COMMENT               
         GOTO1 RECVAL,DMCB,TLINCDQ,(X'20',0)                                    
         BNE   DISP2F                                                           
*                                  DISPLAY INVOICE COMMENT                      
         GOTO1 CHAROUT,DMCB,TACMELQ,SCKGCOMH,TACMTYPG                           
*                                                                               
DISP2F   BRAS  RE,SETCHK           SET SYSDIR/SYSFIL FOR CHECKS                 
         MVC   AIO,AIO1                                                         
*                                                                               
         XC    DUETOT,DUETOT                                                    
         XC    DUETOTTR,DUETOTTR                                                
         XC    DUETOTNR,DUETOTNR                                                
*                                                                               
         USING TADWD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TADWELQ                                                   
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
DISP2G   BRAS  RE,NEXTEL                                                        
         BNE   DISP2I                                                           
         L     RF,DUETOT           ACCUMULATED DUE COMPANY                      
         ICM   RE,15,TADWREC                                                    
         AR    RF,RE                                                            
         ST    RF,DUETOT                                                        
         TM    TADWSTAT,TADWSTR                                                 
         BZ    DISP2H                                                           
         L     RF,DUETOTTR         TAXABLE REIMBURSEMENTS                       
         AR    RF,RE                                                            
         ST    RF,DUETOTTR                                                      
DISP2H   TM    TADWSTAT,TADWSNR                                                 
         BZ    DISP2G                                                           
         L     RF,DUETOTNR         NON TAXABLE REIMBURSEMENTS                   
         AR    RF,RE                                                            
         ST    RF,DUETOTNR                                                      
         B     DISP2G                                                           
*                                                                               
DISP2I   L     R4,AIO              R4 = A(PAYMENT DETAILS ELEMENT)              
         MVI   ELCODE,TAPDELQ                                                   
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAPDD,R4                                                         
*                                                                               
         MVC   SCKTYPE,TAPDW4TY    W4 TYPE                                      
*                                                                               
         MVC   SCKEMP,TAPDEMP      EMPLOYER                                     
*                                                                               
         MVI   SCKCUR,C'U'         CURRENCY                                     
         TM    TAPDSTAT,TAPDSCAN                                                
         BZ    *+8                                                              
         MVI   SCKCUR,C'C'                                                      
         TM    TAPDPST2,TAPDPEUR                                                
         BZ    *+8                                                              
         MVI   SCKCUR,C'E'                                                      
*                                                                               
         L     R3,TAPDREXP         REIMBURSED EXPENSES                          
         S     R3,DUETOTTR                                                      
         S     R3,DUETOTNR                                                      
*                                                                               
         LA    R5,SCKREXP                                                       
         BAS   RE,EDITAMT                                                       
*                                                                               
         MVC   REXP,TAPDREXP       SAVE REIMBURSED EXPENSES FOR LATER           
*                                                                               
         L     R3,TAPDMDED         MISCELLANEOUS DEDUCTIONS                     
         LA    R5,SCKMDED                                                       
         BAS   RE,EDITAMT                                                       
*                                                                               
         L     R3,TAPDDUES         UNION DUES                                   
         LA    R5,SCKDUE                                                        
         BAS   RE,EDITAMT                                                       
*                                                                               
         USING TACDD,R4                                                         
DISP3    L     R4,AIO                                                           
         MVI   ELCODE,TACDELQ      GET CHECK DETAILS ELEMENT                    
         BRAS  RE,GETEL                                                         
         BNE   DISP5                                                            
         MVC   SCKBNK,TACDBNK      BANK CODE                                    
         MVC   SCKFREQ,TACDFREQ    FREQUENCY                                    
*                                                                               
         GOTO1 DATCON,DMCB,(1,TACDDTE),(8,SCKCHKD) CHECK DATE                   
*                                                                               
         OC    TACDDTE,TACDDTE     IF CHECK DATE NOT DEFINED                    
         BNZ   DISP3B                                                           
         TM    TACDSTAT,TACDSFRC   AND BIT IS SET TO FORCE TO LAST YEAR         
         BZ    DISP3B                                                           
         MVC   SCKCHKD,=C'LASTYEAR' DISPLAY SPECIAL KEYWORD                     
*                                                                               
DISP3B   GOTO1 DATCON,DMCB,(1,TACDRUN),(8,SCKRUND) RUN DATE                     
*                                                                               
         MVC   SCKCSHD(3),=C'N/A'  INIT CASHED DATE TO N/A                      
*                                                                               
         TM    TACDSTAT,TACDSVOI   IF CHECK WAS VOIDED                          
         BZ    DISP3C                                                           
         MVC   SCKCSHD(8),=CL8'VOIDED'   INDICATE IT                            
         B     DISP4                                                            
*                                                                               
DISP3C   OC    TACDCSH,TACDCSH     IF THERE IS A CASHED DATE                    
         BZ    DISP3D                                                           
         GOTO1 (RF),(R1),(1,TACDCSH),(8,SCKCSHD) DISPLAY IT                     
         B     DISP4                                                            
*                                                                               
DISP3D   TM    TACDSTAT,TACDSSTA   ELSE IF CHECK IS STALE                       
         BZ    *+10                                                             
         MVC   SCKCSHD(8),=CL8'STALE'   INDICATE IT                             
*                                                                               
         TM    TACDSTA2,TACDSELN   ELSE IF CHECK IS EFT LIEN                    
         BZ    *+10                                                             
         MVC   SCKCSHD(8),=CL8'EFT LIEN'   INDICATE IT                          
*                                                                               
DISP4    OC    TACDRUN,TACDRUN     IF CHECK RUN ALREADY                         
         BNZ   *+14                                                             
         OC    TACDNET,TACDNET     OR CHECK ALREADY HAS NET AMOUNT              
         BZ    DISP5                                                            
*                                                                               
         L     R3,TACDEARN         DISPLAY GROSS TAXABLE EARNINGS               
         LA    R5,SCKGRS                                                        
         BAS   RE,EDITAMTZ         ALSO PRINT IF 0                              
*                                                                               
         L     R3,TACDNET          NET CHECK AMOUNT                             
         LA    R5,SCKNET                                                        
         BAS   RE,EDITAMTZ         ALSO PRINT IF 0                              
*                                                                               
         L     R3,TACDNTAX         ELSE DISPLAY NON-TAXABLE EARNINGS            
         S     R3,REXP             (MINUS REIMBURSED EXPENSES)                  
         A     R3,DUETOTTR         ADD BACK DUE COMPANY REMOVED                 
         A     R3,DUETOTNR         ADD BACK DUE COMPANY REMOVED                 
*                                                                               
         USING TAPDD,R4                                                         
         TM    TGAYSTA7,TAAYSPPL   MUST BE PAYROLL PLUS AGENCY                  
         JO    *+14                                                             
         CLC   =C'P+',TGEMP                                                     
         JNE   DISP4A                                                           
         L     R4,AIO              CHECK NOT WRITTEN YET                        
         MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS ELEMENT                  
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   TGFULL,TAPDTXNW                                                  
         A     R3,TGFULL           ADD BACK TAXABLE REIMBURSEMENTS              
*                                                                               
DISP4A   LA    R5,SCKNTAX                                                       
         BAS   RE,EDITAMTZ         ALSO PRINT IF ZERO                           
*                                                                               
         B     DISP6                                                            
*                                                                               
         USING TAPDD,R4                                                         
DISP5    L     R4,AIO              CHECK NOT WRITTEN YET                        
         MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS ELEMENT                  
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   TAPDW4TY,TAW4TYIN   IF CHECK IS FOR INDIVIDUAL                   
         BNE   DISP5A                                                           
         L     R3,TAPDPAYI         SHOW INDIV PAYMENT                           
         LA    R5,SCKGRS           DISPLAY IN GROSS                             
         BAS   RE,EDITAMTZ         ALSO PRINT IF 0                              
         L     R3,TAPDPAYI         SHOW REIMBURSED EXPENSES                     
         LA    R5,SCKGRS           DISPLAY IN NON-TAXABLE                       
         BAS   RE,EDITAMT          DON'T PRINT IF 0                             
         B     DISP6                                                            
*                                                                               
DISP5A   L     R3,TAPDPAYC         SHOW CORP PAYMENT                            
         LA    R5,SCKNTAX          DISPLAY IN NON-TAXABLE                       
         BAS   RE,EDITAMTZ         ALSO PRINT IF 0                              
*                                                                               
         CLI   TAPDW4TY,TAW4TYES   IF CHECK IS FOR estate                       
         BNE   DISP6                                                            
         L     R3,TAPDPAYI         SHOW INDIV PAYMENT                           
         LA    R5,SCKGRS           DISPLAY IN GROSS                             
         BAS   RE,EDITAMTZ         ALSO PRINT IF 0                              
*                                                                               
DISP6    BAS   RE,DISPRTN          GET CHECK RETURNED DETAILS ELEMENT           
         GOTO1 CHAROUT,DMCB,TANUELQ,SCKMEMBH,TANUTMEM  ACTRA MEMBER #           
*                                                                               
         XC    PSTAMT,PSTAMT                                                    
         MVI   ELCODE,TACXELQ      GET CHECK EXTRA DETAILS ELEMENT              
         L     R4,AIO                                                           
         BRAS  RE,GETEL                                                         
         BNE   DISP8                                                            
         USING TACXD,R4                                                         
         MVC   PSTAMT,TACXPST                                                   
         L     R3,TACXGST          DISPLAY HST DUE ON SCREEN                    
         A     R3,TACXPST                                                       
         LA    R5,SCKOTDI                                                       
         BAS   RE,EDITAMT                                                       
         XC    SCKOTDT,SCKOTDT                                                  
         MVC   SCKOTDT(7),=C'HST Due' SET MORE APPROPRIATE TAG                  
*                                                                               
DISP8    MVI   MSBSTAT,0                                                        
         XC    MSBUCKTS(MSBLNQ),MSBUCKTS                                        
*                                                                               
         XC    OTHTAX,OTHTAX                                                    
*                                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,TACWELQ      GET CHECK WITHHOLDING ELEMENT                
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
DISP10   BRAS  RE,NEXTEL                                                        
         BNE   DISP22                                                           
*                                                                               
         USING TACWD,R4                                                         
         CLC   TACWUNIT(2),=C'FD'  IF UNIT CODE IS FOR FEDERAL                  
         BNE   DISP12                                                           
         BAS   RE,PROCFED          BRANCH TO PROCFED                            
         B     DISP20                                                           
*                                                                               
DISP12   TM    TACWSTAT,TACWSRES+TACWSWRK                                       
         BNZ   DISP13              IF NEITHER RESIDENCE NOR WORK                
         BAS   RE,PROCOTH          THEN PROCESS OTHER                           
         B     DISP20                                                           
*                                                                               
DISP13   CLI   TACWUNIT+2,X'40'    IF UNIT CODE IS FOR A STATE                  
         BH    DISP16                                                           
         TM    TACWSTAT,TACWSWRK   IF WORK BIT ON                               
         BZ    DISP14                                                           
         BAS   RE,PROCSOW          PROCESS STATE OF WORK                        
         B     DISP20                                                           
DISP14   BAS   RE,PROCSOR          ELSE PROCESS STATE OF RESIDENCE              
         B     DISP20                                                           
*                                                                               
DISP16   TM    TACWSTAT,TACWSWRK   IF UNIT CODE IS FOR CITY                     
         BZ    DISP18              CHECK WORK BIT                               
         BAS   RE,PROCCOW          IF ON, PROCESS CITY OF WORK                  
         B     DISP20                                                           
DISP18   BAS   RE,PROCCOR          ELSE PROCESS CITY OF RESIDENCE               
DISP20   B     DISP10              KEEP LOOPING TILL NO MORE ELEMENTS           
*                                                                               
         USING TAODD,R4                                                         
DISP22   MVI   ELCODE,TAODELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAODTYPF))  GET OTHER DEDUCTION ELEM           
         BNE   DISP23                        FOR FEDERAL TAX TYPE               
         L     R4,TGELEM                                                        
         L     R3,TAODAMT          DISPLAY AMOUNT ON SCREEN                     
         LA    R5,SCKFDTX                                                       
         BAS   RE,EDITAMT                                                       
*                                                                               
DISP23   GOTO1 GETL,DMCB,(1,=AL1(TAODTYPD))  GET OTHER DEDUCTION ELEM           
         BNE   DISP23M                       FOR DIRECT DEPOSIT TYPE            
         L     R4,TGELEM                                                        
         L     R3,TAODAMT          DISPLAY AMOUNT ON SCREEN                     
         LA    R5,SCKDIR                                                        
         BAS   RE,EDITAMT                                                       
*                                                                               
DISP23M  GOTO1 GETL,DMCB,(1,=AL1(TAODTYPM))  GET OTHER DEDUCTION ELEM           
         BNE   DISP23W                       FOR MPTRF TYPE                     
         L     R4,TGELEM                                                        
         L     R3,TAODAMT          DISPLAY AMOUNT ON SCREEN                     
         LA    R5,SCKMPTR                                                       
         BAS   RE,EDITAMT                                                       
*                                                                               
DISP23W  GOTO1 GETL,DMCB,(1,=AL1(TAODTYPW))  GET OTHER DEDUCTION ELEM           
         BNE   DISP24                        FOR WIRE TYPE                      
         L     R4,TGELEM                                                        
         L     R3,TAODAMT          DISPLAY AMOUNT ON SCREEN                     
         LA    R5,SCKWIRE                                                       
         BAS   RE,EDITAMT                                                       
*                                                                               
DISP24   GOTO1 GETL,DMCB,(1,=AL1(TAODTYPP))  GET OTHER DEDUCTION ELEM           
         BNE   DISP26                        FOR PERM CHARITIES TYPE            
         L     R4,TGELEM                                                        
         L     R3,TAODAMT          DISPLAY AMOUNT ON SCREEN                     
         LA    R5,SCKCHAR                                                       
         BAS   RE,EDITAMT                                                       
*                                                                               
         USING TADWD,R4                                                         
DISP26   L     R4,AIO              R4 = A(FIRST DUECOMP WITHHOLD ELEM)          
         MVI   ELCODE,TADWELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   DISP35              DONE IF NONE FOUND                           
*                                                                               
         MVC   TGDUC,TADWDUC       SAVE IN GLOBAL                               
*                                                                               
         MVC   SCKDUE1,TADWDUC     DISPLAY REFERENCE NUMBER                     
         CLI   TADWDUC,X'FA'       IS THIS A YEAR 2000 DATE?                    
         BL    DISP26A             NO                                           
         MVC   WORK(4),TADWDUC     YES, CONVERT TO DISPLAY                      
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(X'20',WORK)                                
         MVC   SCKDUE1(4),WORK                                                  
         MVC   SCKDUE1+4(2),TADWDUC+4                                           
*                                                                               
DISP26A  L     R3,TADWREC          DISPLAY AMOUNT RECOVERED                     
         LA    R5,SCKDAM1                                                       
         BAS   RE,EDITAMT                                                       
*                                                                               
         BRAS  RE,NEXTEL           GET NEXT DW ELEMENT                          
         BNE   DISP35              DONE IF NO MORE                              
*                                                                               
         MVC   SCKDUE2,TADWDUC     DISPLAY REFERENCE NUMBER                     
         CLI   TADWDUC,X'FA'       IS THIS A YEAR 2000 DATE?                    
         BL    DISP26B             NO                                           
         MVC   WORK(4),TADWDUC     YES, CONVERT TO DISPLAY                      
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(X'20',WORK)                                
         MVC   SCKDUE2(4),WORK                                                  
         MVC   SCKDUE2+4(2),TADWDUC+4                                           
*                                                                               
DISP26B  L     R3,TADWREC          DISPLAY AMOUNT RECOVERED                     
         LA    R5,SCKDAM2                                                       
         BAS   RE,EDITAMT                                                       
*                                                                               
         XC    DUETOT,DUETOT       CLEAR DUE COMP TOTALS REST OF ELEMS          
*                                                                               
DISP27   BRAS  RE,NEXTEL           GET EXTRA ELEMENTS                           
         BNE   DISP30                                                           
         OC    SCKDUE3,SCKDUE3     IF MORE THAN 1 EXTRA ELEMENT                 
         BZ    DISP28                                                           
         MVC   SCKDUE3,=C'*MORE*'  MOVE *MORE* TO REFERENCE NUMBER              
         B     DISP29                                                           
*                                                                               
DISP28   MVC   SCKDUE3,TADWDUC     ELSE MOVE ACTUAL                             
         CLI   TADWDUC,X'FA'       IS THIS A YEAR 2000 DATE?                    
         BL    DISP29              NO                                           
         MVC   WORK(4),TADWDUC     YES, CONVERT TO DISPLAY                      
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(X'20',WORK)                                
         MVC   SCKDUE3(4),WORK                                                  
         MVC   SCKDUE3+4(2),TADWDUC+4                                           
*                                                                               
DISP29   AF    DUETOT,TADWREC      KEEP TRACK OF DUE COMP TOTAL                 
         B     DISP27              KEEP LOOPING TILL NO MORE ELEMENTS           
*                                                                               
DISP30   L     R3,DUETOT           DISPLAY EXTRA DUE COMP AMOUNTS               
         LA    R5,SCKDAM3                                                       
         BAS   RE,EDITAMT                                                       
*                                                                               
DISP35   MVI   ELCODE,TAODELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAODTYPT))                                     
         BNE   DISP38                                                           
         L     R4,TGELEM                                                        
         USING TAODD,R4                                                         
         L     R3,TAODAMT                                                       
         LA    R5,SCKLAMT                                                       
         BAS   RE,EDITAMT                                                       
         MVC   SCKLIEN,=CL6'TRUST'                                              
         GOTO1 CHAROUT,DMCB,TANUELQ,SCKLSSNH,TANUTRST                           
         OC    SCKLSSN,SCKLSSN                                                  
         BZ    DISP45                                                           
         GOTO1 SSNPACK,DMCB,SCKLSSN,MYPID                                       
         MVC   SCKLSSN,=CL9' '                                                  
         MVC   SCKLSSN(L'MYPID),MYPID                                           
         MVI   SCKLSSNH+5,6                                                     
         OI    SCKLSSNH+6,X'80'                                                 
         B     DISP45                                                           
*                                                                               
DISP38   L     R4,AIO                                                           
         MVI   ELCODE,TALWELQ      GET LIEN WITHHOLDING ELEMENT                 
         BRAS  RE,GETEL                                                         
         BNE   DISP40                                                           
*                                                                               
         USING TALWD,R4                                                         
         MVC   SCKLIEN,TALWLIN     REFERENCE NUMBER                             
         MVC   TGLIN,TALWLIN       SAVE IN GLOBAL                               
         L     R3,TALWREC          DISPLAY AMOUNT RECOVERED                     
         LA    R5,SCKLAMT                                                       
         BAS   RE,EDITAMT                                                       
DISP40   GOTO1 CHAROUT,DMCB,TANUELQ,SCKLSSNH,TANUTLIN  LIEN PAYER               
         OC    SCKLSSN,SCKLSSN                                                  
         BZ    DISP45                                                           
         GOTO1 SSNPACK,DMCB,SCKLSSN,MYPID                                       
         MVC   SCKLSSN,=CL9' '                                                  
         MVC   SCKLSSN(L'MYPID),MYPID                                           
         MVI   SCKLSSNH+5,6                                                     
         OI    SCKLSSNH+6,X'80'                                                 
*                                                                               
DISP45   GOTO1 CHAROUT,DMCB,TACMELQ,SCKCCOMH,TACMTYPC  CHECK COMMENT            
*                                                                               
         USING TACND,R4                                                         
         MVI   SCKSTOP,C'N'         CHECK STOP                                  
         L     R4,AIO                                                           
         MVI   ELCODE,TACNELQ                                                   
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
DISP46   BRAS  RE,NEXTEL                                                        
         BNE   DISP47                                                           
         CLI   TACNTYPE,TACNTYPS                                                
         BE    DISP50                                                           
         B     DISP46                                                           
DISP47   L     R4,AIO                                                           
         MVI   ELCODE,TAKPELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   DISP50                                                           
         MVI   SCKSTOP,C'Y'                                                     
         DROP  R4                                                               
*                                                                               
         USING TACND,R4                                                         
DISP50   MVI   SCKPULL,C'N'         CHECK PULL                                  
         L     R4,AIO                                                           
         MVI   ELCODE,TACNELQ                                                   
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
DISP51   BRAS  RE,NEXTEL                                                        
         BNE   DISP52                                                           
         CLI   TACNTYPE,TACNTYPP                                                
         BE    DISP60                                                           
         B     DISP51                                                           
DISP52   L     R4,AIO                                                           
         MVI   ELCODE,TAKLELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   DISP60                                                           
         MVI   SCKPULL,C'Y'                                                     
         DROP  R4                                                               
*                                                                               
         USING TANUD,R4                                                         
DISP60   MVI   ELCODE,TANUELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TANUTPAC))                                     
         BNE   DISP70                                                           
         L     R4,TGELEM                                                        
         ICM   R3,15,TANUOVAM                                                   
         LCR   R3,R3                                                            
         LA    R5,SCKACOM                                                       
         BAS   RE,EDITAMT                                                       
         DROP  R4                                                               
*                                                                               
DISP70   BRAS  RE,GETCTAX          DISPLAY CANADIAN TAXES                       
         JNE   DISP80                                                           
*                                                                               
         OC    SCKOST,SCKOST                                                    
         JZ    DISP80                                                           
*                                                                               
         L     R3,TGFULL           DISPLAY AMOUNT IN SCREEN                     
         LA    R5,SCKOTX                                                        
         BAS   RE,EDITAMT                                                       
*                                                                               
DISP80   GOTO1 ACTVOUT,DMCB,SCKLCHGH  DISPLAY LAST CHANGED INFO                 
*                                                                               
         L     R4,AIO              RESTORE KEY BEFORE EXITING                   
         MVC   KEY(L'TLDRKEY),0(R4)                                             
DISPX    B     XIT1                                                             
         DROP  R3                                                               
         EJECT                                                                  
*        ROUTINE TO DISPLAY ROUTINED CHECK INFORMATION                          
*                                                                               
DISPRTN  NTR1                                                                   
         L     R4,AIO                                                           
         MVI   ELCODE,TARNELQ      GET CHECK RETURNED DETAILS ELEMENT           
         BRAS  RE,GETEL                                                         
         BNE   DISPRTNX                                                         
*                                                                               
         USING TARND,R4                                                         
         LA    R3,SCKMSG               DISPLAY                                  
         MVC   0(L'LTRCHK,R3),LTRCHK   'THIS CHECK WAS'                         
         LA    R3,L'LTRCHK+1(R3)                                                
         TM    TARNSTAT,TARNMAIL                                                
         BZ    DISPRTN4                                                         
         MVC   0(6,R3),=C'MAILED'                                               
         LA    R3,7(R3)                                                         
         B     DISPRTN5                                                         
DISPRTN4 TM    TARNSTAT,TARNFILE                                                
         BZ    DISPRTN7                                                         
         MVC   0(5,R3),=C'FILED'                                                
         LA    R3,6(R3)                                                         
DISPRTN5 MVC   0(2,R3),=C'ON'                                                   
         GOTO1 DATCON,DMCB,(1,TARNDDTE),(8,3(R3))                               
         B     DISPRTNX                                                         
*                                                                               
DISPRTN7 MVC   0(11,R3),=CL11'RETURNED ON'                                      
         GOTO1 DATCON,DMCB,(1,TARNRDTE),(8,12(R3))                              
*                                                                               
DISPRTNX B     DISPX                                                            
         DROP  R4                                                               
         EJECT                                                                  
* THIS ROUTINE DISPLAYS THE CONTENTS OF A FEDERAL WITHHOLDING ELEMENT           
* TO THE SCREEN.                                                                
*                                                                               
         USING TACWD,R4                                                         
PROCFED  NTR1                                                                   
         MVI   SCKFRES,C'Y'        INIT RESIDENCE INDICATOR TO Y                
         TM    TACWSTAT,TACWSRES   IF RESIDENCE BIT IS OFF                      
         BO    *+8                                                              
         MVI   SCKFRES,C'N'        SET RESIDENCE INDICATOR TO N                 
*                                                                               
         MVC   SCKFMST,TACWMST                                                  
         MVC   SCKFEXM,TACWEXS                                                  
*                                                                               
         MVI   SCKFWRK,C'Y'        INIT WORK INDICATOR TO Y                     
         TM    TACWSTAT,TACWSWRK   IF WORK BIT IS OFF                           
         BO    *+8                                                              
         MVI   SCKFWRK,C'N'        SET WORK INDICATOR TO N                      
*                                                                               
         TM    TACWSTAT,TACWSTAX   IF TAXABLE BIT IS ON                         
         BZ    *+8                                                              
         OI    SCKFTAGH+1,X'08'    SET TAG TO HIGH INTENSITY                    
*                                                                               
         L     R3,TACWTAX          DISPLAY AMOUNTS IN SCREEN                    
         LA    R5,SCKFDTX                                                       
         BAS   RE,EDITAMT                                                       
         L     R3,TACWFICA                                                      
         LA    R5,SCKFDFI                                                       
         BAS   RE,EDITAMT                                                       
         B     XIT1                                                             
         EJECT                                                                  
* THIS ROUTINE DISPLAYS THE CONTENTS OF A WITHHOLDING ELEMENT FOR THE           
* THE STATE OF RESIDENCE TO THE SCREEN.                                         
*                                                                               
         USING TACWD,R4                                                         
PROCSOR  NTR1                                                                   
         MVC   SCKSOR,TACWUNIT     DISPLAY UNIT IN STATE OF RESIDENCE           
         MVC   SCKSMST,TACWMST                                                  
         MVC   SCKSEXM,TACWEXS                                                  
*                                                                               
         TM    TACWSTAT,TACWSTAX   IF TAXABLE BIT IS ON                         
         BZ    *+8                                                              
         OI    SCKSRTGH+1,X'08'    SET TAG TO HIGH INTENSITY                    
*                                                                               
         L     R3,TACWTAX          DISPLAY AMOUNTS IN SCREEN                    
         LA    R5,SCKSRTX                                                       
         BAS   RE,EDITAMT                                                       
         L     R3,TACWSUI                                                       
         LA    R5,SCKSRUN                                                       
         BAS   RE,EDITAMT                                                       
         L     R3,TACWSDI                                                       
         LA    R5,SCKSRDI                                                       
         BAS   RE,EDITAMT                                                       
         CLI   TACWLEN,TACWLN2Q                                                 
         BL    XIT1                                                             
         L     R3,TACWSFLI                                                      
         LA    R5,SCKSRFL                                                       
         BAS   RE,EDITAMT                                                       
         B     XIT1                                                             
         SPACE 3                                                                
* THIS ROUTINE DISPLAYS THE CONTENTS OF A WITHHOLDING ELEMENT FOR THE           
* THE STATE OF WORK TO THE SCREEN.                                              
*                                                                               
         USING TACWD,R4                                                         
PROCSOW  NTR1                                                                   
         MVC   SCKSOW,TACWUNIT     DISPLAY UNIT IN STATE OF WORK                
         TM    MSBSTAT,MSBSSE                                                   
         BZ    PSOW10                                                           
         MVC   SCKSOW,=C'MLT'                                                   
PSOW10   OI    MSBSTAT,MSBSSE                                                   
         MVC   SCKSWMS,TACWMST                                                  
         MVC   SCKSWEX,TACWEXS                                                  
*                                                                               
         TM    TACWSTAT,TACWSTAX   IF TAXABLE BIT IS ON                         
         BZ    *+8                                                              
         OI    SCKSWTGH+1,X'08'    SET TAG TO HIGH INTENSITY                    
*                                                                               
         TM    TACWSTAT,TACWSRES   IF RESIDENCE BIT IS ON                       
         BZ    PSOW20                                                           
         MVC   SCKSOR,TACWUNIT                                                  
         CLC   =C'MLT',SCKSOW                                                   
         BE    PSOW20                                                           
         MVC   SCKSOR,SCKSOW       DISPLAY UNIT IN STATE OF RES. ALSO           
*                                                                               
PSOW20   L     R3,TACWTAX          DISPLAY AMOUNTS IN SCREEN                    
         A     R3,MSSTAX                                                        
         ST    R3,MSSTAX                                                        
         LA    R5,SCKSWTX                                                       
         BAS   RE,EDITAMT                                                       
*                                                                               
         L     R3,TACWSUI                                                       
         A     R3,MSSSUI                                                        
         ST    R3,MSSSUI                                                        
         LA    R5,SCKSWUN                                                       
         BAS   RE,EDITAMT                                                       
*                                                                               
         L     R3,TACWSDI                                                       
         A     R3,MSSSDI                                                        
         ST    R3,MSSSDI                                                        
         LA    R5,SCKSWDI                                                       
         BAS   RE,EDITAMT                                                       
*                                                                               
         CLI   TACWLEN,TACWLN2Q                                                 
         BL    XIT1                                                             
         L     R3,TACWSFLI                                                      
         A     R3,MSSFLI                                                        
         ST    R3,MSSFLI                                                        
         LA    R5,SCKSWFL                                                       
         BAS   RE,EDITAMT                                                       
         B     XIT1                                                             
         EJECT                                                                  
* THIS ROUTINE DISPLAYS THE CONTENTS OF A WITHHOLDING ELEMENT FOR THE           
* THE CITY OF RESIDENCE TO THE SCREEN.                                          
*                                                                               
         USING TACWD,R4                                                         
PROCCOR  NTR1                                                                   
         MVC   SCKCOR,TACWUNIT     DISPLAY UNIT IN CITY OF RESIDENCE            
         MVC   SCKCMST,TACWMST                                                  
         MVC   SCKCEXM,TACWEXS                                                  
*                                                                               
         TM    TACWSTAT,TACWSTAX   IF TAXABLE BIT IS ON                         
         BZ    *+8                                                              
         OI    SCKCRTGH+1,X'08'    SET TAG TO HIGH INTENSITY                    
*                                                                               
         L     R3,TACWTAX          DISPLAY AMOUNTS IN SCREEN                    
         LA    R5,SCKCRTX                                                       
         BAS   RE,EDITAMT                                                       
         B     XIT1                                                             
         SPACE 3                                                                
* THIS ROUTINE DISPLAYS THE CONTENTS OF A WITHHOLDING ELEMENT FOR THE           
* THE CITY OF WORK TO THE SCREEN.                                               
*                                                                               
         USING TACWD,R4                                                         
PROCCOW  NTR1                                                                   
         TM    MSBSTAT,MSBSCE                                                   
         BZ    PCOW10                                                           
         OC    TACWTAX,TACWTAX                                                  
         BZ    PCOW20                                                           
         MVC   SCKCOW,=C'MLT'                                                   
         B     PCOW20                                                           
*                                                                               
PCOW10   MVC   SCKCOW,TACWUNIT     DISPLAY UNIT IN STATE OF WORK                
         OC    TACWTAX,TACWTAX                                                  
         BZ    PCOW20                                                           
         OI    MSBSTAT,MSBSCE                                                   
*                                                                               
PCOW20   TM    TACWSTAT,TACWSTAX   IF TAXABLE BIT IS ON                         
         BZ    *+8                                                              
         OI    SCKCWTGH+1,X'08'    SET TAG TO HIGH INTENSITY                    
*                                                                               
         TM    TACWSTAT,TACWSRES   IF RESIDENCE BIT IS ON                       
         BZ    *+10                                                             
         MVC   SCKCOR,TACWUNIT     DISPLAY UNIT IN CITY OF RES. ALSO            
*                                                                               
         L     R3,TACWTAX          DISPLAY AMOUNTS IN SCREEN                    
         A     R3,MSCTAX                                                        
         ST    R3,MSCTAX                                                        
         LA    R5,SCKCWTX                                                       
         BAS   RE,EDITAMT                                                       
         B     XIT1                                                             
         DROP  R4                                                               
         EJECT                                                                  
* THIS ROUTINE DISPLAYS THE CONTENTS OF A WITHHOLDING ELEMENT FOR               
* 'OTHER' TO THE SCREEN.                                                        
*                                                                               
         USING TACWD,R4                                                         
PROCOTH  NTR1                                                                   
         TM    TACWSTAT,TACWSTAX   IF TAXABLE BIT IS ON                         
         BZ    *+8                                                              
         OI    SCKOTGH+1,X'08'     SET TAG TO HIGH INTENSITY                    
*                                                                               
         CLC   TACWUNIT,=C'CN '    IF UNIT IS CANADA                            
         BNE   POTH10                                                           
         CLI   SCKCUR,C'C'         AND THIS IS CAN$ CHECK                       
         BE    *+14                                                             
         CLC   SCKOTDT(7),=C'HST Due'  OR US$ GST DUE                           
         BNE   POTH10                                                           
         MVC   SCKOTUT(6),=C'HST Pd'  SET MORE APPROPRIATE TAG                  
*                                                                               
POTH10   OC    SCKOST,SCKOST                                                    
         JZ    POTH12                                                           
         MVC   SCKOST,=C'MLT'                                                   
         L     R3,TACWTAX                                                       
         L     RF,OTHTAX                                                        
         AR    R3,RF                                                            
         LA    R5,SCKOTX                                                        
         BAS   RE,EDITAMT                                                       
         J     POTHX                                                            
*                                                                               
POTH12   MVC   SCKOST,TACWUNIT     DISPLAY UNIT                                 
         MVC   OTHTAX,TACWTAX                                                   
*                                                                               
         L     R3,TACWTAX          DISPLAY AMOUNT IN SCREEN                     
         LA    R5,SCKOTX                                                        
         BAS   RE,EDITAMT                                                       
         L     R3,TACWSUI                                                       
         LA    R5,SCKOTUN                                                       
*                                                                               
         CLC   TACWUNIT,=C'CN '    IF UNIT IS CANADA                            
         BNE   POTH15                                                           
         S     R3,PSTAMT                                                        
*                                                                               
POTH15   BAS   RE,EDITAMT                                                       
         L     R3,TACWSDI                                                       
         LA    R5,SCKOTDI                                                       
         BAS   RE,EDITAMT                                                       
*                                                                               
POTHX    B     XIT1                                                             
         DROP  R4                                                               
         EJECT                                                                  
* THIS ROUTINE EDITS THE AMOUNT AT R3 TO THE ADDRESS AT R5.                     
*                                                                               
EDITAMT  DS    0H                                                               
         LTR   R3,R3               RETURN IF AMOUNT IS ZERO                     
         BZR   RE                                                               
*                                  EDIT AMOUNT                                  
EDITAMTZ EDIT  (R3),(11,0(R5)),2,MINUS=YES,ALIGN=LEFT                           
         BR    RE                                                               
         SPACE 2                                                                
         EJECT                                                                  
XIT1     XIT1                                                                   
         SPACE 2                                                                
LTRCHK   DC    C'This check was'                                                
         LTORG                                                                  
         EJECT                                                                  
* THIS ROUTINE GETS THE YTD AMOUNTS FOR THE CURRENT EMPLOYER/SSN FROM           
* TAYTD AND SAVES THE AMOUNTS FOR THE CURRENT SOT, COT IN LOCAL                 
* STORAGE (MYDEARN, MYDFTAX, ETC.).                                             
*                                                                               
         DS    0D                                                               
GETYTD   NTR1  BASE=*,LABEL=*                                                   
*                                  PRE-CLEAR YTD AMOUNTS                        
         XC    MYDAMNTS(MYDAMNTL),MYDAMNTS                                      
*                                                                               
         MVC   TAXUNIT,COT         TAX UNIT = COT IF EXISTS                     
         OC    COT,COT                                                          
         BNZ   *+10                                                             
         MVC   TAXUNIT,SOT         ELSE TAX UNIT = SOT                          
*                                                                               
         LA    R3,BLOCK            R3 = A(TAYTD PARAMETER BLOCK)                
         USING TYD,R3                                                           
*                                                                               
         L     R4,AYTDTAB          R4=A(YTD TABLE)                              
         USING YTDD,R4                                                          
*                                                                               
         XC    0(TYLNQ,R3),0(R3)   BUILD TAYTD PARAMETER BLOCK                  
         MVC   TYASYSIO,TASYSIO    A(SYSIO)                                     
         ST    R4,TYATAB           A(YTD TABLE)                                 
         MVC   TYPEND,TGTODAY1     END DATE = TODAY                             
*                                                                               
         CLI   LASTYEAR,C'Y'       IF FORCING LAST YEAR                         
         BNE   GY5                                                              
         GOTO1 ADDAY,DMCB,(C'Y',TGTODAY0),DUB,-1                                
         MVC   DUB+2(4),=C'1231'                                                
         GOTO1 DATCON,DMCB,(0,DUB),(1,TYPEND)                                   
*                                                                               
*        PACK  DUB,TGTODAY0(2)     CALC. LAST YEAR                              
*        SP    DUB,=P'1'                                                        
*        MP    DUB,=P'10'                                                       
*        MVC   TYPEND(1),DUB+6                                                  
*        MVC   TYPEND+1(2),=X'1231'  END DATE IS 12/31 OF LAST YEAR             
*                                                                               
GY5      MVC   TYEMP,TGEMP         EMPLOYER                                     
         MVC   TYSSN,TGSSN         SOCIAL SECURITY NUMBER                       
         MVC   TYCUR,TGCUR         CURRENCY                                     
*                                                                               
*                                  FILL YTD TABLE                               
         GOTO1 TGTAYTD,DMCB,(RC),SYSCOMM,(R3)                                   
*                                                                               
         MVC   MYDEARN,TYCEARN     SET TAXABLE EARNINGS                         
         MVC   MYDNTAX,TYCNTAX     AND NON-TAXABLE EARNINGS                     
*                                                                               
GY10     CLI   0(R4),0             WHILE NOT END OF YTD TABLE                   
         BE    GYX                                                              
*                                                                               
         CLC   YTDUNIT,=C'CN '     IF UNIT IS CANADA                            
         BNE   GY15                                                             
         MVC   MYDCTX,YTDTAX       SAVE YTD TAX IN LOCAL STORAGE                
         B     GY90                                                             
*                                                                               
GY15     CLC   YTDUNIT,=C'FD '     IF UNIT IS FEDERAL                           
         BNE   GY20                                                             
         MVC   MYDFTAX,YTDTAX      SAVE YTD FTAX IN LOCAL STORAGE               
         MVC   MYDFICA,YTDFICA              FICA                                
         MVC   MYDREXP,YTDREXP              REIMBURSED EXPENSES                 
*                                                                               
         CLC   =C'P+',TGEMP        MYDNTAX ALREADY CORRECT IF P+                
         BE    GY90                                                             
         L     R0,MYDNTAX                                                       
         S     R0,MYDREXP          SUBTRACT REIMB EXP FROM NON-TAXABLE          
         ST    R0,MYDNTAX                                                       
         B     GY90                                                             
*                                                                               
GY20     CLI   YTDUNIT+2,C' '      ELSE IF UNIT IS A STATE                      
         BH    GY30                                                             
         CLC   YTDUNIT,SOT         AND IT IS SOT                                
         BNE   GY90                                                             
         MVC   MYDSTAX,YTDTAX      THEN SAVE YTD STAX IN LOCAL STORAGE          
         MVC   MYDSUI,YTDSUI                     SUI                            
         MVC   MYDSDI,YTDSDI                     SDI                            
         MVC   MYDFLI,YTDSFLI                     FLI                           
         B     GY90                                                             
*                                                                               
GY30     CLC   YTDUNIT,COT         ELSE IF CITY IS COT                          
         BNE   GY90                                                             
         MVC   MYDLTAX,YTDTAX      THEN SAVE YTD LTAX IN LOCAL STORAGE          
*                                                                               
GY90     CLC   TAXUNIT,YTDUNIT     IF THIS UNIT MATCHES TAX UNIT                
         BNE   *+10                                                             
         MVC   MYDEARN,YTDEARN     THEN SAVE YTD EARN IN LOCAL STORAGE          
*                                                                               
         LA    R4,YTDNEXT          BUMP R4 TO NEXT YTD TABLE ENTRY              
         B     GY10                                                             
*                                                                               
GYX      XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         DROP  R3,R4                                                            
*                                                                               
CHKCANL  NTR1  BASE=*,LABEL=*                                                   
         CLI   TGCUR,C'U'                                                       
         JE    XIT                                                              
         CLI   LASTYEAR,C'Y'                                                    
         JNE   XIT                                                              
*                                                                               
         LA    R2,SSTFRCH          CANADIAN ADJ TO LASTYEAR NOT ALLOWED         
         CLI   RECNUM,SS                                                        
         JE    *+8                                                              
         LA    R2,STTFRCH                                                       
         J     ERRCANL                                                          
         LTORG                                                                  
         EJECT                                                                  
* THIS ROUTINE READS THE AGENCY '999999' RECORD TO GET THE NEXT INVOICE         
* NUMBER, UPDATES IT, AND WRITES THE RECORD BACK.                               
*                                                                               
GETINV   NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,SETTAL           SET SYSDIR/SYSFIL FOR NON-CHECKS             
*                                                                               
         MVC   AIO,AIO3            READ AGENCY '999999' REC FOR UPDATE          
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'B1',=C'999999')                           
         JE    *+6                                                              
         DC    H'0'                DIE IF NOT FOUND                             
*                                                                               
         GOTOR DOTRACE,DMCB,AIO,0,=C'OLD AY',6                                  
*                                                                               
         GOTO1 CHNINV,DMCB,=X'001C',WORK   GET NEXT INVOICE NUMBER              
         JNE   ERRAGY                                                           
         MVC   PINVNUM,6(R1)       SAVE PACKED NUMBER OF INVOICE                
         XC    TGINV,=6X'FF'       UN-COMPLEMENT GLOBAL INVOICE                 
*                                                                               
         GOTOR DOTRACE,DMCB,AIO,0,=C'UPD AY',6                                  
*                                                                               
         MVC   AIO,AIO1            RESTORE AIO TO AIO1                          
         BRAS  RE,SETCHK           RESET SYSDIR/SYSFIL FOR CHECKS               
         J     XIT                                                              
         LTORG                                                                  
*                                                                               
* GET CANADIAN TAXES FOR P+ CHECKS                                              
*                                                                               
GETCTAX  NTR1  BASE=*,LABEL=*                                                   
         XC    TGFULL,TGFULL                                                    
*                                                                               
         CLC   SCKEMP,=C'P+ '                                                   
         JNE   GETCT40                                                          
*                                                                               
         USING TAATD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAATELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   NO                                                               
         MVC   SCKOST,=C'CN '      DISPLAY CANADIAN TAXES                       
         J     GETCT20                                                          
*                                                                               
GETCT10  BRAS  RE,NEXTEL                                                        
         JNE   GETCTAXX                                                         
*                                                                               
GETCT20  L     RF,TGFULL                                                        
         ICM   RE,15,TAATTAX                                                    
         AR    RF,RE                                                            
         CLC   TAATUNIT,=C'CN '                                                 
         JE    GETCT30                                                          
         ICM   RE,15,TAATPP                                                     
         AR    RF,RE                                                            
         ICM   RE,15,TAATEI                                                     
         AR    RF,RE                                                            
         ICM   RE,15,TAATPIP                                                    
         AR    RF,RE                                                            
GETCT30  ST    RF,TGFULL                                                        
         J     GETCT10                                                          
*                                                                               
         USING TACWD,R4                                                         
GETCT40  MVI   ELCODE,TACWELQ                                                   
         GOTO1 GETL,DMCB,(3,=C'CN ')                                            
         JNE   NO                                                               
         L     R4,TGELEM                                                        
         L     RF,TGFULL                                                        
         ICM   RE,15,TACWTAX                                                    
         AR    RF,RE                                                            
         ST    RF,TGFULL                                                        
*                                                                               
         GOTO1 GETL,DMCB,(3,=C'QC ')                                            
         JNE   YES                                                              
         L     R4,TGELEM                                                        
         L     RF,TGFULL                                                        
         ICM   RE,15,TACWTAX                                                    
         AR    RF,RE                                                            
         ST    RF,TGFULL                                                        
*                                                                               
GETCTAXX J     YES                                                              
         LTORG                                                                  
* THIS ROUTINE VALIDATES THE CITY OF TAX FIELD POINTED TO BY R2                 
* AND SAVES THE COT IN LOCAL STORAGE.                                           
*                                                                               
VKCOT    NTR1  BASE=*,LABEL=*                                                   
         XC    COT,COT             IF NOTHING ENTERED THEN CLEAR COT            
         CLI   5(R2),0                                                          
         JE    VKCOTX                                                           
*                                                                               
         CLC   SOT,=C'RET'         IF RETURN, THEN NO COT ALLOWED               
         JE    VKCOTX                                                           
*                                                                               
         GOTO1 ANY                 ELSE VALIDATE COT                            
         GOTO1 TAXVAL,DMCB,(3,WORK)                                             
         JNE   ERRINV                                                           
*                                                                               
         CLI   TGTACODE+2,C' '     MAKE SURE IT'S A CITY                        
         JNH   ERRINV                                                           
*                                                                               
         CLC   TGTASTCY,SOT        STATE OF CITY MUST MATCH SOT                 
         JNE   ERRINV                                                           
*                                                                               
         MVC   COT,TGTACODE        SAVE COT IN LOCAL STORAGE                    
*                                                                               
VKCOTX   J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
* ADD P+ CANADIAN ELEMENTS                                                      
*********************************************************************           
BLDPCAN  NTR1  BASE=*,LABEL=*                                                   
         CLC   TGEMP,=C'P+ '                                                    
         JNE   BLDPCANX                                                         
         CLI   PAYTYPE,TAW4TYCA    P+ CANADIAN?                                 
         JNE   BLDPCANX                                                         
*                                                                               
         MVI   ELCODE,TACWELQ                                                   
         GOTO1 GETL,DMCB,(3,=C'CN ')                                            
         JNE   BLDPCANX                                                         
*                                                                               
         L     R4,TGELEM                                                        
         USING TACWD,R4                                                         
         MVC   TGFULL,TACWTAX      FEDERAL CANADIAN TAXES                       
*                                                                               
         MVI   0(R4),X'FF'                                                      
         MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM             REMOVE CN TACW ELEMENT                       
*                                                                               
         LA    R4,ELEM                                                          
         USING TAATD,R4                                                         
*                                                                               
         XC    ELEM,ELEM                                                        
         MVI   TAATEL,TAATELQ                                                   
         MVI   TAATLEN,TAATLN2Q                                                 
         MVC   TAATUNIT,=C'CN '                                                 
         MVC   TAATCCVT,SVCCVT                                                  
         MVC   TAATTAX,TGFULL                                                   
         GOTO1 CONVCAD,DMCB,TGFULL,TAATCTAX                                     
         GOTO1 ADDELEM                                                          
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    RF,ELEM                                                          
         USING TATUD,RF                                                         
         MVI   TATUEL,TATUELQ                                                   
         MVI   TATULEN,TATULNQ                                                  
         MVC   TATUUNIT,=C'CN '                                                 
         MVC   TATUWAGE,EARNINGS                                                
         MVC   TATUNNWA,REXP                                                    
*                                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    RF,ELEM                                                          
         USING TATUD,RF                                                         
         MVI   TATUEL,TATUELQ                                                   
         MVI   TATULEN,TATULNQ                                                  
         MVC   TATUUNIT,=C'FD '                                                 
         MVC   TATUWAGE,EARNINGS                                                
         MVC   TATUNNWA,REXP                                                    
*                                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
BLDPCANX J     YES                                                              
         DROP  R4,RF                                                            
         LTORG                                                                  
*                                                                               
* CONVERT TO CANADIAN DOLLARS                                                   
*        ON ENTRY, PARAM 1 = FIELD TO BE CONVERTED                              
*                  PARAM 2 = CONVERTED LOCATION                                 
*                                                                               
CONVCAD  NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)                                                         
         L     R3,4(R1)                                                         
*                                                                               
         ZAP   DUB,SVCCVT                                                       
         CVB   R1,DUB                                                           
         ST    R1,FULL                                                          
*                                                                               
         ICM   R1,15,0(R2)                                                      
         TM    0(R2),X'80'         NEGATIVE? (VOID/CREDIT/CANCEL)               
         BZ    *+6                                                              
         LPR   R1,R1                                                            
*                                                                               
         XR    R0,R0                                                            
         MHI   R1,10000                                                         
         D     R0,FULL                                                          
*                                                                               
         L     RE,FULL                                                          
         AHI   RE,1                                                             
         SRA   RE,1                                                             
         CR    R0,RE                                                            
         BL    *+8                                                              
         AHI   R1,1                                                             
*                                                                               
         TM    0(R2),X'80'         NEGATIVE? (VOID/CREDIT/CANCEL)               
         BZ    *+6                                                              
         LNR   R1,R1                                                            
*                                                                               
         STCM  R1,15,0(R3)                                                      
         J     XIT                                                              
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        ROUTINE REMOVES RCHECK-RELATED INFORMATION FROM CHECK        *         
*        ON ENTRY ... AIO = A(CHECK RECORD)                           *         
***********************************************************************         
                                                                                
DELRCHK  NTR1  BASE=*,LABEL=*                                                   
         MVI   ELCODE,TARNELQ                                                   
         GOTO1 REMELEM                                                          
                                                                                
         MVI   ELCODE,TACMELQ                                                   
         GOTO1 DELL,DMCB,(1,=AL1(TACMTYPR))                                     
                                                                                
         USING TAACD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAACELQ                                                   
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
DRC10    BRAS  RE,NEXTEL                                                        
         JNE   XIT                                                              
         CLI   TAACSCR,X'DC'                                                    
         JNE   DRC10                                                            
         MVI   TAACEL,X'FF'                                                     
         DROP  R4                                                               
                                                                                
         MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM                                                          
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE VALIDATES THE SSN FIELD AND READS THE W4 RECORD INTO   *         
* AW4IO.  IT THEN CALLS EXTRW4 WHICH USES THE W4 RECORD TO            *         
* INITIALIZE THE CW ELEMENT BLOCK IN THE CHECK RECORD, FOUND IN       *         
* AIO1.  EXTRW4 CAN BE CALLED LATER AGAIN TO REINITIALIZE THE CW      *         
* ELEMENT BLOCK, AS IS DONE IN BLDST.                                 *         
***********************************************************************         
                                                                                
VALSSN   NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,SETTAL           SET SYSDIR/SYSFIL FOR NON-CHECKS             
                                                                                
         MVC   AIO,AW4IO           VALIDATE SSN                                 
         L     R2,ASSNFLD          POINT TO SSN FIELD                           
         CLI   5(R2),6                                                          
         JH    VALSS10                                                          
         MVC   TGPID,8(R2)                                                      
         GOTO1 SSNUNPK,DMCB,TGPID,TGSSN                                         
         JNE   VALSS10                                                          
         MVC   8(9,R2),TGSSN                                                    
         MVI   5(R2),9                                                          
VALSS10  GOTO1 RECVAL,DMCB,TLW4CDQ,(X'0A',ASSNFLD),ASSNNFLD                     
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   8(9,R2),SSNSPAC2                                                 
         MVC   8(L'TGPID,R2),TGPID                                              
         MVI   5(R2),6                                                          
         OI    6(R2),X'80'                                                      
                                                                                
VALSS20  BRAS  RE,SETCHK           RESET SYSDIR/SYSFIL FOR CHECKS               
         MVC   AIO,AIO1            RESTORE AIO TO AIO1                          
                                                                                
         L     R4,AW4IO            R4 = A(W4 DETAILS ELEMENT)                   
         MVI   ELCODE,TAW4ELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         USING TAW4D,R4                                                         
                                                                                
         MVC   W4TYPE,TAW4TYPE     SAVE W4 TYPE                                 
         MVC   W4STA2,TAW4STA2     SAVE W4 2ND STATUS BYTE                      
         MVI   ELCODE,TAA2ELQ                                                   
         L     R4,AW4IO            R4 = A(W4 DETAILS ELEMENT)                   
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAA2D,R4                                                         
         XC    W4STATE,W4STATE                                                  
         MVC   W4STATE(L'TAA2ST),TAA2ST                                         
                                                                                
VLSS26   L     R4,AW4IO            GET EMPLOYEE WITHHOLDING DETAILS             
         USING TAWHD,R4                                                         
         MVI   ELCODE,TAWHELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   VLSS80              MUST BE AT LEAST 2                           
                                                                                
VLSS30   CLC   TAWHUNIT(2),=C'FD' FEDERAL TAX UNIT                              
         JE    VLSS60                                                           
                                                                                
VLSS40   CLI   TAWHUNIT+2,C' '     STATE TAX UNIT - 2 CHARS                     
         JNH   VLSS60                                                           
                                                                                
VLSS50   MVC   W4CITY,TAWHUNIT                                                  
*                                                                               
VLSS60   BRAS  RE,NEXTEL                                                        
         JE    VLSS30                                                           
                                                                                
VLSS80   BRAS  RE,EXTRW4           INITIALIZE CW ELEMENT BLOCK IN CHK           
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
SSNSPAC2 DC    CL9' '              SPACES FIELD FOR SS#                         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TALIMCHK                                                       
       ++INCLUDE TAYTDD                                                         
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR29D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR07D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR08D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR09D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR02D                                                       
         EJECT                                                                  
         ORG   SCKWORK                                                          
ASVPBLK  DS    A                   A(SAVED PASSIVE POINTER BLOCK)               
AUPPBLK  DS    A                   A(UPDATED PASSIVE POINTER BLOCK)             
AYTDTAB  DS    A                   A(YTDTAB)                                    
AADJKEY  DS    A                   A(ADJKEY)                                    
AEMPIO   DS    A                   A(EMPLOYER RECORD)                           
AW4IO    DS    A                   A(W4 RECORD)                                 
*                                                                               
SAVERD   DS    A                   SAVED RD                                     
*                                                                               
DSKADD   DS    XL4                                                              
*                                                                               
CHKTRAN  DS    C                   SINGLE CHECK TRANSFER (Y/N)                  
EARNONLY DS    C                   TRANSFER EARNINGS ONLY (Y/N)                 
LASTYEAR DS    C                   TREAT AS CHECK FOR LAST YEAR (Y/N)           
*                                                                               
ASKED    DS    C                   USER HAS BEEN ASKED TO PF19 (Y/N)            
PFADJ    EQU   19                                                               
ADJTYPE  DS    X                   ADJUSTMENT TYPE (FOR ACTION REISSUE)         
OPTIONS  DS    X                   OPTIONS                                      
OPTRACE  EQU   X'80'               TRACE VOIDS/REISSUES                         
OPURGNT  EQU   X'40'               MARK URGENT                                  
*                                                                               
OIELEM   DS    XL(TAOILNQ)         ORIGINAL AGENCY/INVOICE ELEMENT              
ORIGDTE  DS    PL3                 ORIGINAL CHECK DATE                          
ORIGSEQ  DS    XL4                 ORIGINAL CHECK PROCESSED SEQNUM              
OKELEM   DS    XL(TAOKLNQ)         ORIGINAL CHECK DATE/SEQNUM ELEMENT           
PDELEM   DS    XL(TAPDLNQ)         PAYMENT DETAILS ELEMENT FROM CHECK           
PDEUELEM DS    XL(TAPDLNQ)         PAYMENT DETAILS ELEM FROM CHK EURO           
PINVNUM  DS    PL3                 PACKED NEXT INVOICE NUMBER                   
CHKSSN   DS    CL9                 ISSUE CHECK SSN                              
W4TYPE   DS    C                   W4 TYPE FROM W4 RECORD                       
W4STA2   DS    C                   W4 2ND STATUS BYTE                           
W4STATE  DS    CL2                 W4 STATE                                     
W4CITY   DS    CL3                 W4 WITHHOLDING CITY                          
PAYTYPE  DS    C                   W4 TYPE THAT CHECK IS PAID UNDER             
SVECVT   DS    PL3                 EURO CONVERSION RATE                         
SVCCVT   DS    PL3                 CANADIAN CONVERSION RATE                     
*                                                                               
SVCADTL  DS    0CL(L'TACAUN+L'TACALOCL+L'TACAYEAR)                              
SVCAUN   DS    CL(L'TACAUN)        ORIGINAL INVOICE TACAD UNION                 
SVCALOCL DS    CL(L'TACALOCL)      ORIGINAL INVOICE TACAD LOCAL                 
SVCAYEAR DS    CL(L'TACAYEAR)      ORIGINAL INVOICE TACAD YEAR                  
*                                                                               
SVCNUTAX DS    F                   P+ CANADIAN FEDERAL TAX                      
SVCNCTAX DS    F                   P+ CANADIAN FEDERAL TAX (CAD)                
*                                                                               
SOW      DS    CL3                 STATE OF WORK TAX UNIT                       
COW      DS    CL3                 CITY OF WORK TAX UNIT                        
SOR      DS    CL3                 STATE OF RESIDENCE TAX UNIT                  
COR      DS    CL3                 CITY OF RESIDENCE TAX UNIT                   
SOT      DS    CL3                 TAXABLE STATE TAX UNIT                       
SOTST    DS    XL1                 TAXABLE STATE TAX UNIT STATUS BYTE           
COT      DS    CL3                 TAXABLE CITY TAX UNIT                        
TAXUNIT  DS    CL3                 COT IF EXISTS ELSE SOT                       
OLDSOT   DS    CL3                 OLD STATE OF TAX UNIT                        
OLDSOTST DS    XL1                 OLD STATE OF TAX UNIT STATUS BYTE            
OLDCOT   DS    CL3                 OLD CITY OF TAX UNIT                         
NEWSOT   DS    CL3                 NEW STATE OF TAX UNIT                        
NEWSOTST DS    XL1                 NEW STATE OF TAX UNIT STATUS BYTE            
NEWCOT   DS    CL3                 NEW CITY OF TAX UNIT                         
*                                                                               
DRCRFLAG DS    C                   DEBIT/CREDIT AMOUNTS FLAG                    
*                                                                               
STARTCHK DS    F                   STARTING CHECK NUMBER FOR MREISSUE           
ENDCHK   DS    F                   ENDING CHECK NUMBER FOR MREISSUE             
CHKCNT   DS    F                   CHECK NUMBER COUNTER FOR MREISSUE            
*                                                                               
EARNINGS DS    F                   EARNINGS (AFTER DUE COMPANY)                 
NONTAX   DS    F                   NON-TAXABLE EARNINGS (AFTER DUECOMP)         
REXP     DS    F                   REIMBURSED EXPENSES                          
MDED     DS    F                   MISCELLANEOUS DEDUCTIONS                     
DUES     DS    F                   UNION DUES                                   
DIRCT    DS    F                   DIRECT DEPOSIT AMOUNT                        
WIRE     DS    F                   WIRE TRANSFER AMOUNT                         
NETAMNT  DS    F                   NET CHECK AMOUNT                             
DUETOT   DS    F                   DUE COMP TOTAL                               
DUETOTTR DS    F                   DUE COMP TOTAL TAX REIM                      
DUETOTNR DS    F                   DUE COMP TOTAL NON TAX REIM                  
TOTDED   DS    F                   TOTAL DEDUCTIONS (MINUS DUE COMPANY)         
*                                                                               
OLDDED   DS    F                   TOTAL CREDIT DEDUCTIONS                      
OLDEARN  DS    F                   CREDIT TAXABLE EARNINGS                      
OLDNTAX  DS    F                   CREDIT NON-TAXABLE EARNINGS                  
OLDREXP  DS    F                   CREDIT REIMBURSED EXPENSES                   
OLDNET   DS    F                   CREDIT NET                                   
*                                                                               
MYDAMNTS DS    0F                  YTD AMOUNTS                                  
MYDEARN  DS    F                   YTD TAXABLE EARNINGS                         
MYDNTAX  DS    F                   YTD NON-TAXABLE EARNINGS                     
MYDREXP  DS    F                   YTD REIMBURSED EXPENSES                      
MYDFICA  DS    F                   YTD FICA                                     
MYDSUI   DS    F                   YTD SUI                                      
MYDSDI   DS    F                   YTD SDI                                      
MYDFTAX  DS    F                   YTD FEDERAL TAX                              
MYDSTAX  DS    F                   YTD STATE TAX                                
MYDLTAX  DS    F                   YTD LOCAL TAX                                
MYDMDED  DS    F                   YTD MISCELLANEOUS DEDUCTIONS                 
MYDDUES  DS    F                   YTD UNION DUES                               
MYDMPR   DS    F                   YTD MPR FUND + PERM CHAR                     
MYDFLI   DS    F                   YTD FLI                                      
MYDCTX   DS    F                   YTD CANADIAN TAX                             
MYDNET   DS    F                   NET - FOR CHECK TAX TRANSFERS ONLY           
MYDAMNTL EQU   *-MYDAMNTS                                                       
*                                                                               
PSTAMT   DS    F                   PST AMOUNT                                   
*                                                                               
AEMPFLD  DS    A                   A(EMPLOYER FIELD)                            
ASSNFLD  DS    A                   A(SSN FIELD)                                 
ASSNNFLD DS    A                   A(SSN NAME FIELD)                            
*                                                                               
AEARNFLD DS    A                   A(TAXABLE EARNINGS FIELD)                    
ANTAXFLD DS    A                   A(NON-TAXABLE EARNINGS FIELD)                
AREXPFLD DS    A                   A(REIMBURSED EXPENSES FIELD)                 
AFTAXFLD DS    A                   A(FEDERAL TAX FIELD)                         
AFICAFLD DS    A                   A(FICA FIELD)                                
ASTAXFLD DS    A                   A(STATE TAX FIELD)                           
ASUIFLD  DS    A                   A(SUI FIELD)                                 
ASDIFLD  DS    A                   A(SDI FIELD)                                 
ALTAXFLD DS    A                   A(LOCAL TAX FIELD)                           
AMDEDFLD DS    A                   A(MISCELLANEOUS DEDUCTIONS FIELD)            
ADUESFLD DS    A                   A(UNION DUES FIELD)                          
AMPRFLD  DS    A                   A(MPR FUND + PERM CHAR FIELD)                
AFLIFLD  DS    A                   A(FLI FIELD)                                 
ACTXFLD  DS    A                   A(CANADIAN TAX FIELD)                        
NAMTS1   EQU   (*-AFTAXFLD)/L'AFTAXFLD                                          
NAMTS2   EQU   (*-AEARNFLD)/L'AEARNFLD                                          
ANETFLD  DS    A                   A(NET FIELD) - FOR TAX TRANSFERS             
NAMTS3   EQU   (*-AEARNFLD)/L'AEARNFLD                                          
         SPACE 1                                                                
STAXFLAG DS    C                                                                
*                                                                               
MYBYTE   DS    XL1                 LOCAL BYTE                                   
MYSSN    DS    CL9                 MY SSN                                       
MYPID    DS    CL6                 MY PID                                       
*                                                                               
OTHTAX   DS    F                                                                
*                                                                               
MSBSTAT  DS    X                   MULTI-STATE BUCKET STATUS                    
MSBSSE   EQU   X'80'               STATE TAX ENCOUNTERED PREVIOUSLY             
MSBSCE   EQU   X'40'               CITY TAX ENCOUNTERED PREVIOUSLY              
MSBUCKTS DS    0F                  MULTI-STATE BUCKETS                          
MSSTAX   DS    F                   STATE INCOME TAX                             
MSSSUI   DS    F                   STATE UNEMPLOYMENT INSURANCE                 
MSSSDI   DS    F                   STATE DISABILITY INSURANCE                   
MSSFLI   DS    F                   STATE FLI                                    
MSCTAX   DS    F                   CITY INCOME TAX                              
MSBLNQ   EQU   *-MSBUCKTS                                                       
WSEND    EQU   *                                                                
*                                                                               
         EJECT                                                                  
* TAGENWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE TAGENWORKD                                                     
         PRINT ON                                                               
         ORG   TWAHOLE                                                          
ADJKEY   DS    CL(L'TLDRREC)       KEY FOR ADJUSTMENTS                          
*                                                                               
*TDTAB   DS    CL(NYTD*YTDLNQ+1)   YTD TABLE                                    
YTDTAB   DS    CL(56*YTDLNQ+1)   YTD TABLE                                      
*                                                                               
SVPBLK   DS    XL(L'TLDRREC*22+1)  SAVED PASSIVE POINTER BLOCK                  
HOLSPARE DS    CL(L'TWAHOLE-(*-ADJKEY))   DEFINE SPARE SO ERROR IF              
*                                                              EXCEEDED         
         EJECT                                                                  
* TAGENFILE                                                                     
* TASYSEQUS                                                                     
* TASYSDSECT                                                                    
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* TAGENEQUS                                                                     
* FAWSSVRD                                                                      
* FAGETTXTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE TAGENEQUS                                                      
       ++INCLUDE FAWSSVRD                                                       
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'161TAGEN29   12/30/14'                                      
         END                                                                    
