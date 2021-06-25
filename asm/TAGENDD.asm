*          DATA SET TAGENDD    AT LEVEL 064 AS OF 08/13/14                      
*PHASE T702DDC,*                                                                
         TITLE 'T702DD - DUE COMPANY LETTER MAINTENANCE'                        
T702DD   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T702DD,R5                                                      
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         LA    R7,TWAHOLE          R7=WORKING STORAGE                           
         USING DUD,R7                                                           
         SPACE 1                                                                
         L     R1,TWAMASTC                                                      
         USING MASTD,R1                                                         
         MVC   DURERUN,MCRERUN     SAVE RERUN FLAG                              
         DROP  R1                                                               
         EJECT                                                                  
*                                                                               
*              MODE CONTROLLED ROUTINES                                         
*                                                                               
         GOTO1 INITIAL,DMCB,PFTAB                                               
         SPACE 1                                                                
         MVC   SDUPERF(13),=C'Performer Pid'                                    
         OI    SDUPERFH+6,X'80'                                                 
*                                                                               
MAIN10   CLI   MODE,VALKEY         VALIDATE THE KEY                             
         BNE   *+12                                                             
         BAS   RE,VKEY                                                          
         B     XIT                                                              
         SPACE 1                                                                
         CLI   MODE,VALREC         VALIDATE THE RECORD                          
         BNE   MAIN20                                                           
         BAS   RE,BLDREC                                                        
         CLI   ACTNUM,ACTADD       IF JUST ADDED DLETTER                        
         BE    MAIN40              RE-DISPLAY                                   
         B     MAIN50                                                           
         SPACE 1                                                                
MAIN20   CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    MAIN40                                                           
         CLI   MODE,XRECPUT                                                     
         BNE   MAIN60                                                           
         SPACE 1                                                                
MAIN40   BAS   RE,DISPLAY          (RE-)DISPLAY THE RECORD                      
         SPACE 1                                                                
MAIN50   CLI   ACTNUM,ACTADD       IF JUST ADDED RECORD                         
         BE    ADDMSG              GIVE APPROPRIATE MESSAGE                     
         CLI   ACTNUM,ACTDEL       IF JUST DELETED RECORD                       
         BE    DELMSG              GIVE APPROPRIATE MESSAGE                     
         B     XIT                 ELSE, MESSAGE ALREADY SET                    
         SPACE 1                                                                
MAIN60   CLI   MODE,PRINTREP       PRINT THE LETTER                             
         BNE   MAINX                                                            
         BAS   RE,PREP                                                          
         TM    WHEN,X'40'          IF SPOOLING NOW                              
         BZ    MAINX                                                            
         XC    CONSERV,CONSERV     GO TO IT                                     
         MVC   CONSERV(4),=C'$DQU'                                              
         SPACE 1                                                                
MAINX    B     XIT                                                              
         SPACE 2                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
*              VALIDATE THE KEY                                                 
         SPACE 1                                                                
VKEY     NTR1                                                                   
         CLI   ACTNUM,ACTREST      RESTORE NOT ALLOWED                          
         BE    ERRREST                                                          
         CLI   ACTNUM,ACTREP       IF ACTION IS REPORT                          
         BNE   VKEY5                                                            
         CLI   OFFLINE,C'Y'        IF OFFLINE                                   
         BE    VKEYX               KEY FIELDS NOT USED                          
         TM    WHEN,X'40'                                                       
         BZ    ERRPRT              ONLY NOW ALLOWED ONLINE                      
*                                                                               
VKEY5    MVI   KEYCHANG,C'N'       SET NO KEY CHANGE                            
         GOTO1 FLDVAL,DMCB,(X'40',SDUSSNH),(X'80',SDUREFH)                      
         BE    *+12                                                             
         MVI   PFAID,0             RESET PFKEY                                  
         MVI   KEYCHANG,C'Y'       AND SET KEY CHANGED                          
*                                                                               
         BAS   RE,VALSSN           VALIDATE SS NUMBER                           
         BAS   RE,VALREF           VALIDATE REFERENCE NUMBER                    
*                                                                               
         CLI   ACTNUM,ACTADD       IF ACTION IS ADD                             
         BNE   VKEYX                                                            
         GOTO1 FLDVAL,DMCB,(X'80',AFRSTREC),(X'80',999) IF NO INPUT YET         
         BE    *+12                                                             
         CLI   KEYCHANG,C'Y'       OR IF KEY CHANGED                            
         BNE   VKEYX                                                            
         BAS   RE,DEFAULT          DISPLAY DEFAULT INFORMATION                  
         B     DFLTDISP            GIVE DEFAULT DSPLY'D MESSAGE                 
*                                                                               
VKEYX    B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO VALIDATE SSN                                          
         SPACE 1                                                                
VALSSN   NTR1                                                                   
         CLI   SCRSTAT,0           IF SCREEN CHANGE                             
         BE    *+8                                                              
         NI    SDUSSNH+4,X'DF'     FORCE KEY RE-VALIDATION                      
*                                                                               
         CLI   SDUSSNH+5,0                                                      
         BE    VSSN03                                                           
         CLI   SDUSSNH+5,9         MUST CHECK FOR LENGTH HERE SINCE             
         BE    VSSN05              RECVAL CALL DOES NOT CHECK FOR               
         CLI   SDUSSNH+5,6         INVALID ENTRIES DUE TO X'40' PARAM           
         BNE   INVERR                                                           
         MVC   TGPID,SDUSSN                                                     
VSSN03   OC    TGPID,TGPID                                                      
         BZ    MISSERR                                                          
         GOTO1 SSNUNPK,DMCB,TGPID,TGSSN                                         
         BNE   VSSN05                                                           
         MVC   SDUSSN,TGSSN                                                     
         MVI   SDUSSNH+5,9                                                      
*                                                                               
VSSN05   GOTO1 RECVAL,DMCB,TLW4CDQ,(X'08',SDUSSNH),SDUSSNMH                     
*                                                                               
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   SDUSSN,SPACES                                                    
         MVC   SDUSSN(L'TGPID),TGPID                                            
         MVI   SDUSSNH+5,6                                                      
         OI    SDUSSNH+6,X'80'                                                  
*                                                                               
VSSN10   L     R4,AIO                                                           
         MVI   ELCODE,TAW4ELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAW4D,R4                                                         
*                                                                               
**NO-OP* TM    TAW4STAT,TAW4STDU   BUG NOT ALWAYS ON WHEN IT SHOULD BE          
**NO-OP* BZ    ERRDUE              DUE COMPANY MUST BE PENDING                  
*                                                                               
         MVC   DUW4TYPE,TAW4TYPE   SAVE W4 TYPE                                 
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE DUE COMPANY REFERENCE NUMBER                 
         SPACE 1                                                                
VALREF   NTR1                                                                   
         LA    R2,SDUREFH                                                       
         CLI   5(R2),0             IF NO INPUT                                  
         BNE   VALREF2                                                          
         OC    TGDUC,TGDUC         CHECK FOR GLOBAL                             
         BZ    MISSERR                                                          
         GOTO1 RECVAL,DMCB,TLDUCDQ,(X'24',(R2))  GET DUECOMP RECORD             
         BE    VALREF4                                                          
         B     ERRDUE2                                                          
VALREF2  MVC   WORK,8(R2)          GET INPUT INTO WORK AND SPACE FILLED         
         OC    WORK,SPACES                                                      
         CLI   5(R2),6             IF LENGTH IS 6                               
         BNE   VALREF3                                                          
         MVC   DUB(2),WORK+2       TEST IF VALID DATE                           
         MVC   DUB+2(2),=C'01'     CHANGE TO M/D/Y FORMAT                       
         MVC   DUB+4(2),WORK                                                    
         GOTO1 DATVAL,DMCB,(0,DUB),WORK+6                                       
         CLI   3(R1),0                                                          
         BE    *+10                NOT VALID DATE                               
         MVC   WORK(4),WORK+6      YES - SO USE INTERNAL FORMAT                 
*                                                                               
VALREF3  GOTO1 RECVAL,DMCB,TLDUCDQ,(X'A0',WORK)  GET REC, NON-SCRN DATA         
         BE    VALREF4                                                          
         NI    4(R2),X'DF'         INVALIDATE FIELD IF ERROR                    
         B     ERRDUE2                                                          
VALREF4  OI    4(R2),X'20'         SET PREVIOUSLY VALIDATED                     
         L     R4,AIO                                                           
         MVI   ELCODE,TADUELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TADUD,R4                                                         
         CLI   ACTNUM,ACTADD       IF ADDING                                    
         BNE   VALREF5                                                          
         OC    TADUINV,TADUINV                                                  
         BZ    ERRDUEIN            MUST HAVE INVOICE NUMBER                     
         B     VALREF10                                                         
*                                                                               
VALREF5  CLI   ACTNUM,ACTREP       IF REPORTING (NOW)                           
         BNE   VALREF10                                                         
         TM    TADUSTAT,TADUSHLD                                                
         BO    ERRDUEAP            DUE COMPANY IS AWAITING APPROVAL             
*                                                                               
VALREF10 L     R4,AIO                                                           
         MVI   ELCODE,TADRELQ      IF DLETTER DETAILS ELEMENT FOUND             
         BAS   RE,GETEL                                                         
         BNE   *+16                                                             
         CLI   ACTNUM,ACTADD       ADD NOT ALLOWED                              
         BE    ERREXIST                                                         
         B     *+12                                                             
         CLI   ACTNUM,ACTADD       ELSE, MUST FIRST ADD                         
         BNE   ERRNOFND                                                         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO DISPLAY DEFAULT INFO TO SCREEN                        
         SPACE 1                                                                
DEFAULT  NTR1                                                                   
         GOTO1 FLDVAL,DMCB,(X'23',SDUAGYH),(X'80',SDUWORK) CLR SCRN             
         XC    SDULCHG,SDULCHG                                                  
         OI    SDULCHGH+6,X'80'                                                 
*                                                                               
         BAS   RE,DRTADU           DISPLAY DUE COMPANY DETAILS ELEMENT          
*                                                                               
         MVC   AIO,AIO2            READ INVOICE RECORD IN AIO2                  
         GOTO1 RECVAL,DMCB,TLINCDQ,(X'A0',0)                                    
         BNE   DFLTX                                                            
         BAS   RE,DFLTINV          GET DEFAULT INVOICE INFORMATION              
         BAS   RE,DRINV            DISPLAY THAT INVOICE INFORMATION             
*                                                                               
         BAS   RE,SETCHK           SET FILE OVERRIDES                           
         LA    R4,KEY                                                           
         USING TLCKD,R4                                                         
         XC    KEY,KEY                                                          
         MVI   TLCKCD,TLCKCDQ      READ CHECKS FOR THIS INVOICE                 
         MVC   TLCKAGY,TGAGY                                                    
         MVC   TLCKINV,TGINV                                                    
         XC    TLCKINV,ALLFF                                                    
         GOTO1 HIGH                                                             
         B     DFLT20                                                           
*                                                                               
DFLT10   GOTO1 SEQ                                                              
DFLT20   CLC   KEY(TLCKSORT-TLCKD),KEYSAVE                                      
         BNE   DFLTX                                                            
         CLC   TLCKSSN,TGSSN       IF MATCH ON PERFORMER                        
         BNE   DFLT25                                                           
         GOTO1 GETREC              GET CHECK RECORD                             
         B     DFLT30              AND DISPLAY VALUES                           
*                                                                               
DFLT25   CLI   DUW4TYPE,TAW4TYCO   ELSE, IF CORP                                
         BE    DFLT26                                                           
         CLI   DUW4TYPE,TAW4TYCA   OR CANADIAN                                  
         BE    DFLT26                                                           
         CLI   DUW4TYPE,TAW4TYTR   OR TRUSTEE                                   
         BNE   DFLT10                                                           
DFLT26   GOTO1 GETREC              GET CHECK RECORD                             
         L     R4,AIO                                                           
         MVI   ELCODE,TATIELQ      MATCH ON CORP ID TOO                         
         BAS   RE,GETEL                                                         
         BNE   DFLT10                                                           
         USING TATID,R4                                                         
         CLC   TGSSN,TATIID                                                     
         BNE   DFLT10                                                           
*                                                                               
DFLT30   BAS   RE,DFLTCHK          GET DEFAULT CHECK INFORMATION                
         BAS   RE,DRCHK            DISPLAY THAT CHECK INFORMATION               
*                                                                               
DFLTX    BAS   RE,SETTAL           RESET FILE VALUES                            
         MVC   AIO,AIO1            RESTORE IOAREA                               
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO GET DEFAULT INVOICE INFO                              
*              ON ADD ONLY                                                      
         SPACE 1                                                                
DFLTINV  NTR1                                                                   
         L     R4,AIO              R4=A(INVOICE RECORD)                         
         MVI   ELCODE,TACOELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACOD,R4                                                         
         MVC   DUCID,TACOCID      COMMERCIAL ID                                 
*                                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,TAPDELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAPDD,R4                                                         
         MVC   DUCYC,SPACES        PRE-CLEAR CYCLE DATES                        
         OC    TAPDCYCS,TAPDCYCS   IF CYCLE START DATE                          
         BZ    DFLTINV5                                                         
         GOTO1 DATCON,DMCB,(1,TAPDCYCS),(8,DUCYC)                               
         MVI   DUCYC+8,C'-'                                                     
         GOTO1 DATCON,DMCB,(1,TAPDCYCE),(8,DUCYC+9)                             
DFLTINV5 MVC   DUUSE,TAPDUSE       USE CODE                                     
         MVC   DUUSENM,SPACES                                                   
         GOTO1 USEVAL,DMCB,(X'60',DUUSE)                                        
         BNE   *+10                                                             
         MVC   DUUSENM,TGUSNAME    USE INFO W/O TYPE                            
*                                                                               
         XC    DUUID,DUUID                                                      
         XC    DUSTAF,DUSTAF                                                    
         MVC   DUSTFNM,SPACES                                                   
         L     R4,AIO                                                           
         MVI   ELCODE,TAINELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   DFLTINVX                                                         
         USING TAIND,R4                                                         
         OC    TAINPINF,TAINPINF   IF ANY PAY INFORMATION                       
         BZ    DFLTINVX                                                         
         OC    TAINPID,TAINPID     STAFF ID                                     
         BZ    DFLTINVX                                                         
         MVC   AIO,AIO3                                                         
         XC    WORK,WORK                                                        
         MVC   WORK+8(L'TAINPID),TAINPID                                        
         GOTO1 USERVAL,DMCB,(X'80',WORK)                                        
         MVC   DUUID,TGUSERID                                                   
*                                                                               
         MVC   DUSTAF,TAINPST                                                   
         GOTO1 RECVAL,DMCB,TLSTCDQ,(X'A8',DUSTAF),0                             
         MVC   AIO,AIO2                                                         
         BNE   DFLTINVX                                                         
         MVC   DUSTFNM,TGNAME                                                   
*                                                                               
DFLTINVX B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO GET DEFAULT CHECK INFORMATION                         
*              ON ADD ONLY                                                      
         SPACE 1                                                                
DFLTCHK  NTR1                                                                   
         L     R4,AIO              R4=A(CHECK RECORD)                           
         MVI   ELCODE,TACAELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACAD,R4            R4=A(CAST DETAILS ELEMENT)                   
         SPACE 1                                                                
         MVC   DUNCDE,TACANCDE     AGENT CODE                                   
         XC    DUNNAME,DUNNAME     PRE CLEAR AGENT NAME                         
         OC    DUNCDE,DUNCDE       IF AGENT CODE                                
         BZ    DFLTCHK5                                                         
         MVC   AIO,AIO3                                                         
         BAS   RE,SETTAL           RESET FILE VALUES                            
         GOTO1 TRNSAGT,DMCB,(X'40',DUNCDE),TGAGT                                
         XC    DUNNAMH,DUNNAMH                                                  
         MVI   DUNNAMH,8+36                                                     
         GOTO1 RECVAL,DMCB,TLANCDQ,(X'A8',0),DUNNAMH                            
         MVC   AIO,AIO2                                                         
         BAS   RE,SETCHK           SET FILE OVERRIDES AGAIN                     
*                                                                               
DFLTCHK5 XC    DUCHK,DUCHK                                                      
         XC    DUGRS,DUGRS                                                      
         XC    DUNET,DUNET                                                      
         L     R4,AIO              R4=A(CHECK RECORD)                           
         MVI   ELCODE,TACDELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   DFLTCHKX                                                         
         USING TACDD,R4            R4=A(CHECK DETAILS ELEMENT)                  
         SPACE 1                                                                
         MVC   DUCHK,TACDCHK                              CHECK NUMBER          
         GOTO1 DATCON,DMCB,(1,TACDDTE),(8,DUDTE)          CHECK DATE            
         L     R1,TACDEARN                                                      
         A     R1,TACDNTAX                                                      
         EDIT  (R1),(11,DUGRS),2,MINUS=YES,ALIGN=LEFT     GROSS EARN            
*                                                                               
         L     R2,TACDNET          R2=(NET AMOUNT)                              
*                                                                               
         USING TAODD,R4                                                         
         MVI   ELCODE,TAODELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAODTYPD))                                     
         BNE   *+12                                                             
         L     R4,TGELEM           IF DIRECT DEPOSIT AMOUNT                     
         A     R2,TAODAMT          ADD TO NET AMOUNT                            
*                                                                               
         USING TAODD,R4                                                         
         GOTO1 GETL,DMCB,(1,=AL1(TAODTYPW))                                     
         BNE   *+12                                                             
         L     R4,TGELEM           IF WIRE TRANSFER AMOUNT                      
         A     R2,TAODAMT          ADD TO NET AMOUNT                            
*                                                                               
         EDIT  (R2),(11,DUNET),2,MINUS=YES,ALIGN=LEFT  NET AMOUNT               
DFLTCHKX B     XIT                                                              
         EJECT                                                                  
*              DISPLAY THE RECORD                                               
         SPACE 1                                                                
DISPLAY  NTR1                                                                   
         BAS   RE,GETTADR          GET INFO FROM DLETTER ELEMENT                
*                                                                               
         BAS   RE,DRTADU           DISPLAY DUE COMPANY DETAILS ELEMENT          
         BAS   RE,DRINV            DISPLAY INVOICE INFORMATION                  
         BAS   RE,DRCHK            DISPLAY CHECK INFORMATION                    
*                                                                               
         GOTO1 ACTVOUT,DMCB,(X'80',SDULCHGH) DISPLAY LAST CHANGED INFO          
*                                                                               
         CLI   PFAID,15            IF PRINT/STOP PRINT PFKEY HIT                
         BNE   *+8                                                              
         BAS   RE,SETPRNT          SET ON/OFF PRINT PENDING                     
*                                                                               
         BAS   RE,DRPINFO          DISPLAY STATUS MESSAGE/PRINT DATE            
*                                                                               
         CLI   PFAID,15                                                         
         BE    PRNTMSG             AND GIVE APPROPRIATE MESSAGE                 
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO GET INFO FROM DLETTER ELEMENT                         
         SPACE 1                                                                
GETTADR  NTR1                                                                   
         L     R4,AIO              R4=A(DUE COMPANY RECORD)                     
         MVI   ELCODE,TADRELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TADRD,R4                                                         
*                                  GET INVOICE INFORMATION                      
         MVC   DUUSE,TADRUSE       USE TYPE                                     
         MVC   DUUSENM,SPACES                                                   
         GOTO1 USEVAL,DMCB,(X'60',DUUSE)                                        
         BNE   *+10                                                             
         MVC   DUUSENM,TGUSNAME    USE INFO W/O TYPE                            
         MVC   DUCID,TADRCID       COMMERCIAL ID                                
*                                  CYCLE DATES                                  
         MVC   DUCYC,SPACES        PRE-CLEAR CYCLE DATES                        
         OC    TADRCYCS,TADRCYC    IF NO CYCLE START                            
         BZ    GETTADR5                                                         
         GOTO1 DATCON,DMCB,(1,TADRCYCS),(8,DUCYC)                               
         MVI   DUCYC+8,C'-'                                                     
         GOTO1 DATCON,DMCB,(1,TADRCYCE),(8,DUCYC+9)                             
*                                  GET CHECK INFORMATION                        
GETTADR5 MVC   DUCHK,TADRCHK       CHECK NUMBER                                 
         GOTO1 DATCON,DMCB,(1,TADRDTE),(8,DUDTE)          CHECK DATE            
         XC    DUNNAME,DUNNAME     PRE - CLEAR AGENT NAME                       
         MVC   DUNCDE,TADRNCDE     AGENT CODE                                   
         OC    DUNCDE,DUNCDE       IF AGENT CODE                                
         BZ    GETTADR6                                                         
         GOTO1 TRNSAGT,DMCB,(X'40',DUNCDE),TGAGT                                
         L     R0,AIO                                                           
         MVC   AIO,AIO2                                                         
         XC    DUNNAMH,DUNNAMH                                                  
         MVI   DUNNAMH,8+36                                                     
         GOTO1 RECVAL,DMCB,TLANCDQ,(X'A8',0),DUNNAMH                            
         ST    R0,AIO                                                           
*                                                                               
GETTADR6 EDIT  TADREARN,(11,DUGRS),2,MINUS=YES,ALIGN=LEFT GROSS EARN            
         EDIT  TADRNET,(11,DUNET),2,MINUS=YES,ALIGN=LEFT  NET AMOUNT            
*                                  'NET AMOUNT' MADE PAYABLE TO TP FOR          
         MVC   DUNETLIT,SPACES                                                  
         LA    R1,DUNETLIT                                                      
         EDIT  TADRNET,(11,(R1)),2,MINUS=YES,ALIGN=LEFT                         
         AR    R1,R0                                                            
         MVC   0(L'DULNET,R1),DULNET                                            
*                                                                               
         MVC   DUPDTE,SPACES                                                    
         OC    TADRPDTE,TADRPDTE                                                
         BZ    GETTADR7                                                         
         GOTO1 DATCON,DMCB,(1,TADRPDTE),(8,DUPDTE)        DATE PRINTED          
*                                                                               
GETTADR7 XC    DUUID,DUUID                                                      
         XC    TGUSER,TGUSER                                                    
         OC    TADRUID,TADRUID     USER ID                                      
         BZ    GETTADR9                                                         
         XC    WORK,WORK                                                        
         MVC   WORK+8(L'TADRUID),TADRUID                                        
         L     R0,AIO                                                           
         MVC   AIO,AIO2                                                         
         GOTO1 USERVAL,DMCB,(X'80',WORK)                                        
         ST    R0,AIO                                                           
         MVC   DUUID,TGUSERID                                                   
*                                                                               
GETTADR9 MVC   DUSTAF,TADRSTAF     STAFF CODE                                   
         OC    TGUSER,TGUSER       IF USER NOT DEFINED FROM ABOVE               
         BNZ   *+10                                                             
         MVC   TGUSER,TWAORIG      USE CONNECT ID FOR USER-ID                   
         MVC   DUSTFNM,SPACES                                                   
         L     R0,AIO                                                           
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLSTCDQ,(X'A8',DUSTAF),0                             
         ST    R0,AIO                                                           
         BNE   *+10                                                             
         MVC   DUSTFNM,TGNAME                                                   
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO DISPLAY DUE COMPANY DETAILS ELEMENT                   
         SPACE 1                                                                
DRTADU   NTR1                                                                   
         L     R4,AIO              R4=A(DUE COMPANY RECORD)                     
         USING TLDUD,R4                                                         
*                                                                               
         MVI   ELCODE,TADUELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TADUD,R4            R4=A(DUE COMPANY DETAILS ELEMENT)            
         SPACE 1                                                                
         MVC   SDUAGY,TADUAGY                                                   
         OI    SDUAGYH+6,X'80'                                                  
         MVI   SDUAGYH+5,6         DISPLAY AGENCY NAME                          
         MVC   AIO,AIO2            SET IO AREA                                  
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'2C',SDUAGYH),SDUAGYNH                     
         MVC   AIO,AIO1            RESTORE IO AREA                              
         GOTO1 RAVPPLSA,DMCB,0     SEE IF REC / ACT INVALID FOR P+              
         JNE   ERPPLSI                                                          
*                                                                               
         MVC   TGINV,TADUINV                                                    
         XC    TGINV,ALLFF                                                      
*                                                                               
         XC    SDUINV,SDUINV                                                    
         OC    TADUINV,TADUINV                                                  
         BZ    DRTADU10                                                         
         GOTO1 TINVCON,DMCB,TADUINV,SDUINV,DATCON                               
DRTADU10 OI    SDUINVH+6,X'80'                                                  
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO DISPLAY INVOICE INFORMATION                           
         SPACE 1                                                                
DRINV    NTR1                                                                   
*                                                                               
         MVC   SDUUSE,DUUSE              USE TYPE                               
         OI    SDUUSEH+6,X'80'                                                  
         MVC   SDUUSED(L'TGUSNAME),TGUSNAME                                     
         OI    SDUUSEDH+6,X'80'          USE NAME W/O TYPE                      
*                                                                               
         MVC   SDUCID,DUCID              COMMERCIAL ID                          
         OI    SDUCIDH+6,X'80'                                                  
*                                                                               
         MVC   SDUCYC,DUCYC                                                     
         OI    SDUCYCH+6,X'80'           CYCLE DATES                            
*                                                                               
         MVC   SDUUID,DUUID              USER ID                                
         OI    SDUUIDH+6,X'80'                                                  
         MVC   SDUSTAF,DUSTAF            STAFF CODE                             
         OI    SDUSTAFH+6,X'80'                                                 
         MVC   SDUSTFN,DUSTFNM           STAFF NAME                             
         OI    SDUSTFNH+6,X'80'                                                 
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO DISPLAY CHECK INFORMATION                             
         SPACE 1                                                                
DRCHK    NTR1                                                                   
         MVC   SDUCHK,DUCHK        CHECK NUMBER                                 
         OI    SDUCHKH+6,X'80'                                                  
*                                                                               
         MVC   SDUDTE,DUDTE                                                     
         OI    SDUDTEH+6,X'80'     CHECK DATE                                   
*                                                                               
         MVC   SDUGRS,DUGRS                                                     
         OI    SDUGRSH+6,X'80'     GROSS EARNINGS                               
         MVC   SDUNET,DUNET                                                     
         OI    SDUNETH+6,X'80'     NET AMOUNT                                   
*                                                                               
         XC    SDUNCDE,SDUNCDE     PRE - CLEAR SCREEN FIELDS                    
         XC    SDUNCDN,SDUNCDN                                                  
         OC    DUNCDE,DUNCDE       IF AGENT CODE                                
         BZ    DRCHK30                                                          
         GOTO1 TRNSAGT,DMCB,(X'40',DUNCDE),SDUNCDE                              
         MVC   SDUNCDN,DUNNAME     AGENT NAME                                   
DRCHK30  OI    SDUNCDEH+6,X'80'                                                 
         OI    SDUNCDNH+6,X'80'                                                 
*                                                                               
DRCHKX   B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO SET PRINT PENDING IN STATUS                           
         SPACE 1                                                                
SETPRNT  NTR1                                                                   
         MVC   AIO,AIO1            MAKE SURE IOAREA SET                         
         GOTO1 RECVAL,DMCB,TLDUCDQ,(X'B4',0)                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R4,AIO                                                           
         USING TLDUD,R4                                                         
         TM    TLDUSTAT,TLDUSPND   IF PENDING PRINT                             
         BZ    SETPRNT5                                                         
         NI    TLDUSTAT,X'FF'-TLDUSPND  TURN OFF PRINT PENDING IN REC           
         MVI   DUMSGNO,244              SET CANCELLED MESSAGE                   
         B     SETPRNT8                                                         
*                                                                               
SETPRNT5 CLC   SDUWAIT,LTAPP       IF INVOICE WAITING APPROVAL                  
         BE    ERRDUEAP            GIVE ERROR MESSAGE - CANNOT PRINT            
         OI    TLDUSTAT,TLDUSPND   ELSE,SET PRINT PENDING IN RECORD             
         MVI   DUMSGNO,243         SET WILL PRINT MESSAGE                       
*                                                                               
SETPRNT8 GOTO1 PUTREC                                                           
*                                  AND IN KEY                                   
         MVC   KEY+TLDRSTAT-TLDRD(1),TLDUSTAT                                   
         GOTO1 WRITE                                                            
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO DISPLAY STATUS MESSAGE AND DATE PRINTED               
         SPACE 1                                                                
DRPINFO  NTR1                                                                   
         MVC   SDUPDTE,DUPDTE      DISPLAY DATE PRINTED                         
         OI    SDUPDTEH+6,X'80'                                                 
*                                                                               
         OI    SDUWAITH+1,X'04'    TURN MESSAGE TO LOW INTENSITY                
         OI    SDUWAITH+6,X'80'                                                 
*                                                                               
         L     R6,AIO              R6=A(DUE COMPANY RECORD)                     
         USING TLDUD,R6                                                         
         LR    R4,R6                                                            
*                                                                               
         MVI   ELCODE,TADUELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   DRPINFO5                                                         
         USING TADUD,R4                                                         
         TM    TADUSTAT,TADUSHLD   IF WAITING APPROVAL                          
         BZ    *+14                                                             
         MVC   SDUWAIT,LTAPP       SET '* AWAITING APPROVAL *'                  
         B     DRPINFO8                                                         
*                                                                               
DRPINFO5 TM    TLDUSTAT,TLDUSPND   IF PRINT PENDING                             
         BZ    DRPINFOX                                                         
         MVC   SDUWAIT,LTPEND      SET '* PRINT PENDING *'                      
*                                                                               
DRPINFO8 NI    SDUWAITH+1,X'F8'    TURN MESSAGE TO HIGH INTENSITY               
DRPINFOX B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*              BUILD THE RECORD - ADDS THE DLETTER ELEMENT                      
*                                 TO THE DUE COMPANY RECORD                     
         SPACE 1                                                                
BLDREC   NTR1                                                                   
         XC    SVPDTE,SVPDTE       PRECLEAR SAVED DATE PRINTED                  
         L     R4,AIO                                                           
         MVI   ELCODE,TADRELQ                                                   
         USING TADRD,R4                                                         
         BAS   RE,GETEL                                                         
         BNE   BLDREC2                                                          
         MVC   SVPDTE,TADRPDTE     SAVE DATE PRINTED                            
*                                                                               
BLDREC2  MVI   ELCODE,TADRELQ      REMOVE DLETTER ELEMENT                       
         GOTO1 REMELEM                                                          
*                                                                               
         CLI   ACTNUM,ACTDEL       IF DELETING                                  
         BNE   *+12                                                             
         MVI   BYTE,X'C0'          DON'T ADD ACTIVITY                           
         B     BLDREC5                                                          
*                                                                               
         LA    R4,ELEMENT          ADD NEW DLETTER ELEMENT                      
         USING TADRD,R4                                                         
         XC    ELEMENT,ELEMENT                                                  
         MVI   TADREL,TADRELQ      SET ELEMENT CODE                             
         MVI   TADRLEN,TADRLNQ     SET ELEMENT LENGTH                           
         MVC   TADRPDTE,SVPDTE     RESTORE DATE PRINTED                         
*                                                                               
         BAS   RE,BLDINV           BUILD INVOICE INFORMATION                    
         BAS   RE,BLDCHK           BUILD CHECK INFORMATION                      
*                                                                               
         GOTO1 ADDELEM             ADD THE DLETTER ELEMENT                      
         MVI   BYTE,X'80'                                                       
*                                                                               
BLDREC5  GOTO1 ACTVIN,DMCB,(BYTE,SDULCHGH)  ADD BY ACTIVITY SCREEN              
         CLI   ACTNUM,ACTADD       IF ACTION IS ADD                             
         BE    *+12                                                             
         CLI   ACTNUM,ACTDEL       OR DELETE                                    
         BNE   BLDREC8                                                          
         MVC   AIO,AIO2            MUST DO MY OWN MAINT                         
         GOTO1 RECVAL,DMCB,TLDUCDQ,(X'B4',0)  PREVENT GET/PUT SYNDROME          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO1                                                         
         GOTO1 PUTREC                                                           
         B     BLDRECX                                                          
*                                                                               
BLDREC8  MVC   AIO,AIO2            RE-READ RECORD                               
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
BLDRECX  B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO VALIDATE INVOICE INFORMATION                          
         USING TADRD,R4            R4=A(DLETTER ELEMENT)                        
         SPACE 1                                                                
BLDINV   NTR1                                                                   
         MVC   SVKEY,KEY           SAVE THE KEY                                 
         LA    R2,SDUUSEH          VALIDATE USE                                 
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
         GOTO1 USEVAL,DMCB,(X'40',8(R2))                                        
         BNE   INVERR                                                           
         MVC   TADRUSE,TGUSCDE                                                  
*                                                                               
         LA    R2,SDUCIDH          R2=A(COMMERCIAL ID)                          
         GOTO1 ANY                 FORCE INPUT                                  
         GOTO1 RECVAL,DMCB,TLCOICDQ,(R2)                                        
         MVC   TADRCID,TGCID                                                    
*                                                                               
         LA    R2,SDUCYCH          VALIDATE CYCLE DATES                         
         LA    R3,BLOCK                                                         
         GOTO1 PDVAL,DMCB,(X'80',(R3))                                          
         USING PERVALD,R3                                                       
         MVC   TADRCYC,PVALPSTA                                                 
*                                                                               
         LA    R2,SDUUIDH          VALIDATE USER ID                             
         CLI   5(R2),0                                                          
         BE    BLDINV5                                                          
         MVC   AIO,AIO2                                                         
         GOTO1 USERVAL,DMCB,SDUUIDH                                             
         MVC   AIO,AIO1                                                         
         MVC   TADRUID,TGUSER                                                   
         B     BLDINV10                                                         
*                                                                               
BLDINV5  CLI   SDUSTAFH+5,0        VALIDATE STAFF CODE                          
         BE    BLDINVX                                                          
BLDINV10 MVC   TGUSER,TADRUID      SET USER ID FROM ELEMENT                     
         OC    TGUSER,TGUSER       IF NOT DEFINED                               
         BNZ   *+10                                                             
         MVC   TGUSER,TWAORIG      USE CONNECT ID FOR USER ID                   
         MVC   TADRUID,TGUSER      SET USER ID IN ELEMENT                       
         NI    SDUSTAFH+4,X'DF'    FORCE REVALIDATION                           
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLSTCDQ,(X'08',SDUSTAFH),SDUSTFNH                    
         MVC   AIO,AIO1                                                         
         MVC   TADRSTAF,TGSTAF     SET STAFF CODE IN ELEMENT                    
*                                                                               
BLDINVX  MVC   KEY,SVKEY           RESTORE THE KEY                              
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE CHECK INFORMATION                            
         USING TADRD,R4            R4=A(DLETTER ELEMENT)                        
         SPACE 1                                                                
BLDCHK   NTR1                                                                   
         MVC   SVKEY,KEY           SAVE KEY                                     
         LA    R2,SDUCHKH          VALIDATE CHECK NUMBER                        
         GOTO1 ANY                                                              
         CLI   5(R2),L'TLCKCCHK    INSURE FULL CHECK NUMBER INPUT               
         BNE   INVERR                                                           
         LA    R1,KEY                                                           
         USING TLCKPD,R1                                                        
         XC    KEY,KEY                                                          
         MVI   TLCKPCD,TLCKCCDQ                                                 
         MVC   TLCKCCHK,8(R2)                                                   
         XC    TLCKCCHK,ALLFF                                                   
         BAS   RE,SETCHK           SET FILE OVERRDES                            
         GOTO1 HIGH                                                             
         BAS   RE,SETTAL           RESET FILE VALUES                            
         CLC   KEY(TLCKCBNK-TLCKPD),KEYSAVE                                     
         BNE   ERRNOFND                                                         
         MVC   TADRCHK,8(R2)                                                    
*                                                                               
         LA    R2,SDUDTEH          VALIDATE CHECK DATE                          
         GOTO1 DTVAL,DMCB,TADRDTE                                               
*                                                                               
         LA    R2,SDUGRSH                                                       
         BAS   RE,VALAMNT          VALIDATE GROSS AMOUNT                        
         MVC   TADREARN,4(R1)                                                   
*                                                                               
         LA    R2,SDUNETH                                                       
         BAS   RE,VALAMNT          VALIDATE NET AMOUNT                          
         MVC   TADRNET,4(R1)                                                    
         CLC   TADREARN,TADRNET                                                 
         BL    AMTINV                                                           
*                                                                               
         LA    R2,SDUNCDEH         IF AGENT CODE DISPLAYED                      
         CLI   5(R2),0             IF INPUT                                     
         BE    BLDCHKX                                                          
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLANCDQ,(X'08',SDUNCDEH),SDUNCDNH                    
         MVC   AIO,AIO1            SAVE IN INPUT                                
         GOTO1 TRNSAGT,DMCB,(X'80',TGAGT),TADRNCDE                              
*                                                                               
BLDCHKX  MVC   KEY,SVKEY           RESTORE KEY                                  
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE AN AMOUNT                                    
*                                  R2=A(AMOUNT SCREEN HEADER)                   
         SPACE 1                                                                
VALAMNT  LR    R0,RE               SAVE RE                                      
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
         ZIC   RF,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,8(R2),(RF)                                          
         CLI   0(R1),0                                                          
         BNE   AMTINV              AMOUNT INVALID                               
         OC    4(4,R1),4(R1)                                                    
         BZ    AMTINV              AMOUNT INVALID                               
         LR    RE,R0               RESTORE RE                                   
         BR    RE                                                               
         SPACE 2                                                                
*              ROUTINE TO SET SYSDIR/SYSFIL FOR NON CHECKS                      
         SPACE 1                                                                
SETTAL   DS    0H                                                               
         MVC   SYSDIR,SVSYSDIR     RESET FILE OVERRIDES                         
         MVC   SYSFIL,SVSYSFIL                                                  
         BR    RE                                                               
         SPACE 2                                                                
*              ROUTINE TO SET SYSDIR/SYSFIL TO CHECKS                           
         SPACE 1                                                                
SETCHK   DS    0H                                                               
         MVC   SYSDIR,=CL8'CHKDIR' SET FILE OVERRIDES                           
         MVC   SYSFIL,=CL8'CHKFIL'                                              
         BR    RE                                                               
         EJECT                                                                  
*              ROUTINE TO PRINT DUE COMPANY LETTERS                             
         SPACE 1                                                                
PREP     NTR1                                                                   
         TM    WHEN,X'40'          IF RUNNING NOW                               
         BZ    PREP10                                                           
         MVI   FORCEHED,C'Y'       START LETTER ON NEW PAGE                     
         BAS   RE,GETTADR          GET INFO FROM DLETTER ELEMENT                
         BAS   RE,SETVALS          SET OTHER VARIOUS INFO                       
         BAS   RE,SETAGNT          SET AGENT NAME & ADDRESS(OVER SSN)           
         BAS   RE,PLETTER          PRINT THE LETTER                             
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLDUCDQ,(X'B4',0)                                    
         MVC   AIO,AIO1                                                         
         BAS   RE,UNMARK           TURN OFF PENDING/SET DATE/WRITE BACK         
**NO-OP* BAS   RE,DRPINFO          RE-DISPLAY PRINT INFORMATION                 
         BAS   RE,DISPLAY          RE-DISPLAY RECORD INFORMATION                
         B     XIT                                                              
*                                                                               
PREP10   LH    RF,=AL2(TIEND-TASYSIOD) CLEAR SYSIO FILTERS                      
         XCEFL TASYSIOD,(RF)                                                    
*                                                                               
         MVC   TIACOMFC,ACOMFACS   SET UP SYSIO BLOCK                           
         LA    R1,PRHOOK           A(SYSIO HOOK)                                
         ST    R1,TIHOOK                                                        
         MVC   TIACCS,TWAACCS      LIMIT ACCESS                                 
         MVC   TIAUTH,TWAAUTH      AUTHORIZATION                                
         MVC   TIUSERID,TWAORIG    REQUESTING ID                                
         MVI   TIREAD,TLDUCDQ      READ ALL DUE COMPANY RECORDS                 
         CLI   DURERUN,C'Y'        IF NOT RERUN                                 
         BE    *+8                                                              
         OI    TIQFLAGS,TIQFDIR    PASS DIRECTORY HOOKS                         
*                                                                               
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
         B     XIT                                                              
         EJECT                                                                  
*              HOOK FROM SYSIO - PROCESS DUE COMPANY RECORD                     
         SPACE 1                                                                
PRHOOK   NTR1                                                                   
         CLI   TIMODE,PROCDIR      DIRECTORY HOOK                               
         BNE   PRHK20                                                           
         TM    TIKEYST,TLDUSPND    IF PRINT PENDING                             
         BZ    NO                                                               
         B     YES                                                              
*                                                                               
PRHK20   CLI   TIMODE,PROCREC      PROCESS RECORD                               
         BNE   XIT                                                              
         MVC   AIO,TIAREC          SET A(DUE COMPANY RECORD)                    
         CLI   DURERUN,C'Y'        IF RERUN MODE                                
         BNE   *+12                                                             
         BAS   RE,CHKRERUN         CHECK RERUN CANDIDATE                        
         BNE   PRHKX                                                            
*                                                                               
         BAS   RE,GETTADR          GET INFO FROM DLETTER ELEMENT                
         BAS   RE,SETVALS          SET OTHER VARIOUS INFO                       
         BAS   RE,SETAGNT          SET AGENT NAME & ADDRESS(OVER SSN)           
*                                                                               
         MVI   FORCEHED,C'Y'       START LETTER ON NEW PAGE                     
         BAS   RE,PLETTER          PRINT THE LETTER                             
         BAS   RE,UPDTLDU          UPDATE DUE COMPANY RECORD                    
*                                                                               
PRHKX    MVC   AIO,AIO1            RESET IOAREA                                 
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO SET INFO NEEDED IN PRINTING LETTER                    
         SPACE 1                                                                
SETVALS  NTR1                                                                   
         L     R4,AIO              R4=A(DUE COMPANY RECORD)                     
         USING TLDUD,R4                                                         
*                                                                               
         L     R0,AIO              SAVE IOAREA                                  
         MVC   AIO,AIO2            SET IOAREA                                   
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'A4',TLDUSSN)                              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   W4SSN,TLDUSSN       SAVE SSN NUMBER                              
         BAS   RE,SETW4VAL         GET W4 NAME AND ADDRESS                      
         ST    R0,AIO              RESET IOAREA                                 
*                                                                               
         L     R4,AIO              R4=A(DUE COMPANY RECORD)                     
         MVI   ELCODE,TADUELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TADUD,R4                                                         
*                                                                               
         MVC   DUAGYCD,TADUAGY     SAVE AGENCY CODE                             
         MVC   DUAGYNM,SPACES                                                   
         L     R0,AIO              SAVE IOAREA                                  
         MVC   AIO,AIO2            SET IOAREA                                   
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'A8',TADUAGY),0                            
         ST    R0,AIO              RESET IOAREA                                 
         BNE   *+10                                                             
         MVC   DUAGYNM,TGNAME      SET AGENCY NAME                              
*                                                                               
         MVC   DUCLINM,SPACES                                                   
         CLC   TADUFCLI,SPACES                                                  
         BNH   SETVAL10                                                         
         L     R0,AIO              SAVE IOAREA                                  
         MVC   AIO,AIO2            SET IOAREA                                   
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'A8',TADUFCLI),0                           
         ST    R0,AIO              RESET IOAREA                                 
         BNE   SETVAL10                                                         
         MVC   DUCLINM,TGNAME      SET AGENCY NAME                              
*                                                                               
SETVAL10 MVC   DUINV,SPACES                                                     
         OC    TADUINV,TADUINV                                                  
         BZ    SETVAL20                                                         
         GOTO1 TINVCON,DMCB,TADUINV,DUINV,DATCON                                
*                                                                               
SETVAL20 MVI   DUNR1H,68           L'FLD=8(L'HDR)+60(NARRATIVE)                 
         XC    DUNR1,DUNR1                                                      
         MVI   DUNR2H,68           L'FLD=8(L'HDR)+60(NARRATIVE)                 
         XC    DUNR2,DUNR2                                                      
         MVI   DUNR3H,68           L'FLD=8(L'HDR)+60(NARRATIVE)                 
         XC    DUNR3,DUNR3                                                      
         GOTO1 CHAROUT,DMCB,TACMELQ,(3,DUNR1H),TACMTYPG                         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO GET W4 NAME AND ADDRESS                               
SETW4VAL NTR1                                                                   
         L     R4,AIO                                                           
         MVI   ELCODE,TAW4ELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAW4D,R4                                                         
         CLI   TAW4TYPE,TAW4TYIN   IF INDIVIDUAL                                
         BE    SETW4V0                                                          
         CLI   TAW4TYPE,TAW4TYCA   OR CANADIAN                                  
         BE    SETW4V0                                                          
         CLI   TAW4TYPE,TAW4TYFO   OR FOREIGNER                                 
         BNE   SETW4V1                                                          
SETW4V0  MVC   W4NAME(16),TAW4NAM1 USE FIRST/LAST NAME                          
         MVC   W4NAME+16(16),TAW4NAM2                                           
         B     *+10                                                             
SETW4V1  MVC   W4NAME,TAW4CRPN     ELSE, CORPORATION NAME                       
         GOTO1 SQUASHER,DMCB,W4NAME,33                                          
*                                                                               
         XC    W4ADD,W4ADD                                                      
         L     R4,AIO                                                           
         MVI   ELCODE,TAA2ELQ      TEST FOR NEW STYLE ADDRESS                   
         BAS   RE,GETEL                                                         
         BNE   SETW4V7                                                          
         USING TAA2D,R4                                                         
         LA    R2,W4ADD                                                         
         LA    R1,TAA2ADD1                                                      
         LHI   R0,3                ASSUME 3 ADDRESS LINES                       
         CLI   TAA2LEN,TAA2LNQ     FOR OLD STYLE ADDRESSES                      
         BL    SETW4V2                                                          
         CLC   TAA2CTRY,=C'US'     AND NEW STYLE US ADDRESSES                   
         BE    SETW4V2                                                          
         LHI   R0,2                ELSE USE 2                                   
SETW4V2  CLC   0(30,R1),SPACES     TEST ANY DATA LEFT                           
         BNH   SETW4V5                                                          
         MVC   0(30,R2),0(R1)                                                   
         LA    R2,39(R2)                                                        
         LA    R1,30(R1)                                                        
         BCT   R0,SETW4V2                                                       
*                                                                               
SETW4V5  XC    WORK(39),WORK                                                    
         MVC   WORK(25),TAA2CITY   CITY                                         
         MVC   WORK+26(2),TAA2ST   STATE                                        
         MVC   WORK+29(10),TAA2ZIP ZIP                                          
         GOTO1 SQUASHER,DMCB,WORK,39                                            
         MVC   0(39,R2),WORK                                                    
*                                                                               
         CLI   TAA2LEN,TAA2LNQ        FOR OLD STYLE ADDRESSES                   
         BL    SETW4VX                                                          
         CLC   TAA2CTRY,=C'US'        AND NEW STYLE US ADDRESSES                
         BE    SETW4VX                                                          
         GOTO1 VALCTRY,DMCB,(X'80',TAA2CTRY)                                    
         BNE   SETW4VX                                                          
         USING CTRYTABD,R1                                                      
         L     R1,TGACTRY                                                       
         ZIC   RE,CTRYDSP                                                       
         LTR   RE,RE                                                            
         BNZ   *+8                                                              
         IC    RE,CTRYLEN                                                       
         SHI   RE,CTRYDESC-CTRYTABD+1                                           
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   39(0,R2),CTRYDESC                                                
         EX    RE,*+8                                                           
         B     *+10                                                             
         OC    39(0,R2),SPACES                                                  
         B     SETW4VX                                                          
         DROP  R1                                                               
*                                                                               
SETW4V7  L     R4,AIO                                                           
         MVI   ELCODE,TAADELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   SETW4VX                                                          
         BAS   RE,SETTAAD          SET ADDRESS OLD STYLE (TAADD)                
*                                                                               
SETW4VX  B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO REPLACES SSN ADDRESS WITH AGENT ADDRESS               
*                                                                               
SETAGNT  NTR1                                                                   
         MVC   AGNTNAME,SPACES     PRE CLEAR NAME TO SPACES                     
         OC    DUNCDE,DUNCDE       IF AGENT CODE                                
         BZ    SETAGNTX                                                         
*                                                                               
         GOTO1 TRNSAGT,DMCB,(X'40',DUNCDE),TGAGT                                
*                                                                               
         L     R0,AIO                                                           
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLANCDQ,(X'A8',0),0                                  
         ST    R0,AIO                                                           
         BNE   SETAGNTX                                                         
*                                                                               
         L     R0,AIO                                                           
         MVC   AIO,AIO2                                                         
         MVC   AGNTNAME(3),=C'C/O'                                              
         GOTO1 CHAROUT,DMCB,TANAELQ,0                                           
         MVC   AGNTNAME+4(L'TGNAME),TGNAME                                      
         ST    R0,AIO                                                           
*                                                                               
         XC    W4ADD,W4ADD                                                      
         L     R4,AIO2                                                          
         MVI   ELCODE,TAADELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   *+8                                                              
         BAS   RE,SETTAAD          SET ADDRESS OLD STYLE (TAADD)                
SETAGNTX B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO SET W4ADD                                             
         USING TAADD,R4            R4=A(ADDRESS ELEMENT)                        
SETTAAD  NTR1                                                                   
         LA    R2,W4ADD                                                         
         LA    R1,TAADADD                                                       
         ZIC   R0,TAADLNES                                                      
SETTAAD5 CLC   0(30,R1),SPACES                                                  
         BNH   SETTAADX                                                         
         MVC   0(30,R2),0(R1)                                                   
         LA    R2,39(R2)                                                        
         LA    R1,30(R1)                                                        
         BCT   R0,SETTAAD5                                                      
SETTAADX B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PRINT A LETTER                                        
         SPACE 1                                                                
PLETTER  NTR1                                                                   
         LA    R2,P                R2=A(PRINT LINE)                             
         USING PRTD,R2                                                          
*                                                                               
         MVI   SPACING,11                                                       
         BAS   RE,PRNTIT                                                        
*                                                                               
         MVC   PRLTDATE,DULDATE    PRINT TODAYS DATE                            
         MVC   PRTDATE,TGTODAY8                                                 
         BAS   RE,PRNTIT                                                        
*                                                                               
         BAS   RE,PRNTW4           PRINT TALENT'S NAME AND ADDRESS              
*                                  PRINT BODY OF TEXT                           
         LA    R3,1                R3=LINE COUNT                                
         LA    R1,DULSTRT          R1=A(TEXT TO PRINT)                          
*                                                                               
PLET10   CLI   0(R1),X'FF'         WHILE NOT END OF TEXT                        
         BE    XIT                                                              
         MVI   BYTE,0              NO TEXT IN PRINT LINE                        
         LA    R4,BLNKTAB          R4=A(TABLE)                                  
         USING BLNKTABD,R4                                                      
*                                                                               
PLET20   CLI   0(R4),X'FF'         WHILE NOT END OF TABLE                       
         BE    PLET60                                                           
         CLM   R3,1,BLNKCNT        MATCH AGAINST LINE NUMBER                    
         BNE   PLET40                                                           
*                                                                               
         TM    BLNKACT,BLNKSPAC    IF SPACING REQUESTED                         
         BZ    *+8                                                              
         BAS   RE,PRNTIT           PRINT A BLANK LINE NOW                       
*                                                                               
         TM    BLNKACT,BLNKNTXT    IF CAN MOVE TEXT TO PRINT LINE               
         BO    PLET30                                                           
         CLI   BYTE,1              AND HAVEN'T ALREADY DONE THAT                
         BE    PLET30                                                           
         MVC   0(PRTLNQ,R2),0(R1)  THEN DO IT                                   
         LA    R1,PRTLNQ(R1)       AND BUMP TO NEXT TEXT LINE                   
*                                                                               
PLET30   OC    BLNKLEN,BLNKLEN     IF ANY DATA                                  
         BZ    *+8                                                              
         BAS   RE,SETBLNKS         FILL IN BANKS                                
         MVI   BYTE,1              TEXT IN PRINT LINE                           
         TM    BLNKACT,BLNKPRT     IF NEED TO PRINT                             
         BZ    PLET40                                                           
         CLC   0(PRTLNQ,R2),SPACES AND SOMETHING IN PRINT LINE                  
         BNH   PLET40                                                           
         BAS   RE,PRNTIT           THEN PRINT IT NOW                            
*                                                                               
PLET40   LA    R4,L'BLNKTAB(R4)    BUMP TO NEXT TABLE ENTRY                     
         B     PLET20              CHECK FOR MORE ON SAME LINE                  
*                                                                               
PLET60   CLI   BYTE,0              IF NO TEXT ON PRINT LINE                     
         BNE   *+14                                                             
         MVC   0(PRTLNQ,R2),0(R1)  MOVE IT THERE NOW                            
         LA    R1,PRTLNQ(R1)       BUMP TO NEXT TEXT LINE                       
*                                                                               
         BAS   RE,PRNTIT           PRINT THE LINE                               
         LA    R3,1(R3)            BUMP LINE COUNT                              
         B     PLET10                                                           
         SPACE 2                                                                
*              ROUTINE TO PRINT W4 NAME AND ADDRESS                             
         SPACE 1                                                                
PRNTW4   NTR1                                                                   
         MVC   PRW4NAME,W4NAME                                                  
         BAS   RE,PRNTIT                                                        
*                                                                               
         CLC   AGNTNAME,SPACES                                                  
         BNH   *+14                                                             
         MVC   PRANAME,AGNTNAME                                                 
         BAS   RE,PRNTIT                                                        
*                                                                               
         LA    R2,PRW4ADD                                                       
         LA    R1,W4ADD            R1=A(SSN ADDRESS OR AGENT ADDRESS)           
         LA    R0,4                                                             
PRNTW410 CLC   0(39,R1),SPACES                                                  
         BNH   PRNTW4X                                                          
         MVC   0(39,R2),0(R1)                                                   
         LA    R1,39(R1)                                                        
         BAS   RE,PRNTIT                                                        
         BCT   R0,PRNTW410                                                      
*                                                                               
PRNTW4X  BAS   RE,PRNTIT                                                        
         BAS   RE,PRNTIT                                                        
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO SET BLANKS IN PRINT LINE                              
*                                  R2=A(P),R7=A(TWAHOLE),R4=A(BLNKTAB)          
         USING BLNKTABD,R4                                                      
         SPACE 1                                                                
SETBLNKS NTR1                                                                   
         A     R2,BLNKDISP         R2=A(P) + DISPLACEMENT                       
         A     R7,BLNKDATA         R7=A(TWAHOLE) + DISPLACMENT                  
*                                                                               
         LH    RF,BLNKLEN          RF=LENGTH OF DATA TO PRINT                   
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(R7)       MOVE DATA TO PRINT LINE                      
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO UPDATE DUE COMPANY RECORD                             
         SPACE 1                                                                
UPDTLDU  NTR1                                                                   
         MVC   AIO,AIO2            SET IOAREA                                   
         XC    KEY,KEY             RESET READ SEQUENCE FOR SYSIO                
         MVC   KEY(L'TIKEY),TIKEY                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'TIKEY),TIKEY                                               
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 GETREC              REREAD THE RECORD                            
         BAS   RE,UNMARK           TURN OFF PENDING/SET DATE/WRITE BACK         
         MVC   AIO,TIAREC          RESET IOAREA                                 
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO TURN OFF PRINT PENDING IN RECORD                      
*              AND KEY AND SET PRINTED DATE                                     
         SPACE 1                                                                
UNMARK   NTR1                                                                   
         L     R4,AIO                                                           
         MVI   ELCODE,TADRELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TADRD,R4                                                         
         MVC   TADRPDTE,TGTODAY1   SET PRINTED TODAY                            
         TM    WHEN,X'40'          IF PRINTING NOW                              
         BZ    UNMARK5                                                          
         GOTO1 DATCON,DMCB,(1,TADRPDTE),(8,DUPDTE)                              
*                                                                               
UNMARK5  L     R4,AIO              TURN OFF PRINT PENDING IN RECORD             
         USING TLDUD,R4                                                         
         NI    TLDUSTAT,X'FF'-TLDUSPND                                          
         GOTO1 PUTREC              WRITE BACK THE RECORD                        
*                                                                               
*                                  TURN OFF PRINT PENDING IN KEY                
         NI    KEY+TLDRSTAT-TLDRD,X'FF'-TLDUSPND                                
         GOTO1 WRITE                                                            
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO CHECK IF CANDIDATE FOR RERUN                          
         SPACE 1                                                                
CHKRERUN NTR1                                                                   
         L     R4,AIO              R4=A(DUE COMPANY RECORD)                     
         MVI   ELCODE,TADRELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   NO                                                               
         USING TADRD,R4            R4=A(DLETTER ELEMENT)                        
*                                                                               
         CLC   TADRPDTE,TGTODAY1   IF PRINTED TODAY                             
         BE    YES                 THEN, OKAY TO RERUN                          
         B     NO                  ELSE, SKIP                                   
         SPACE 2                                                                
*              ROUTINE TO PRINT A LINE                                          
         SPACE 1                                                                
PRNTIT   NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         EJECT                                                                  
*              ERROR MESSAGES                                                   
*                                                                               
MISSERR  MVI   ERROR,MISSING       MISSING INPUT FIELD                          
         B     ERRXIT                                                           
*                                                                               
AMTINV   MVI   ERROR,ERINVAMT      INVALID AMOUNT                               
         B     ERRXIT                                                           
*                                                                               
INVERR   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     ERRXIT                                                           
*                                                                               
ERRREST  MVI   ERROR,INVACT        RESTORE NOT ALLOWED                          
         LA    R2,CONACTH                                                       
         B     ERRXIT                                                           
*                                                                               
ERRPRT   MVI   ERROR,INVPRINT      DDS NOT ALLOWED ONLINE                       
         LA    R2,CONWHENH                                                      
         B     ERRXIT                                                           
*  -------- DELETED MESSAGE SINCE NO LONGER USED  -------- *                    
*ERRDUE   MVI   ERROR,ERNODUE       NO MONEY DUE COMPANY                        
*         B     ERRXIT                                                          
*                                                                               
ERRDUE2  MVI   ERROR,ERDUECMP      DUE COMPANY NOT ON FILE                      
         B     ERRXIT                                                           
*                                                                               
ERRDUEIN MVI   ERROR,ERDUEINV      MUST SPECIFY INVOICE ON DUE COMPANY          
         LA    R2,SDUREFH                                                       
         B     ERRXIT                                                           
*                                                                               
ERRDUEAP MVI   ERROR,ERDUEAPP      DUE COMPANY AWAITING APPROVAL -              
         B     ERRXIT              CANNOT PRINT                                 
*                                                                               
ERRNOFND MVI   ERROR,NOTFOUND      NOT FOUND                                    
         B     ERRXIT                                                           
*                                                                               
ERREXIST MVI   ERROR,RECEXIST      RECORD ALREADY EXISTS                        
         B     ERRXIT                                                           
*                                                                               
DFLTDISP MVI   MYMSGNO1,29         DEFAULTS DISPLAYED                           
         LA    R2,SDUUSEH                                                       
         B     INFEND                                                           
*                                                                               
PRNTMSG  MVC   MYMSGNO1,DUMSGNO    DUE COMPANY LETTER WILL BE PRINTED           
         LA    R2,SDUSSNH          OR DUE COMP LETTER PRINT CANCELLED           
         B     INFEND                                                           
*                                                                               
ADDMSG   MVI   MYMSGNO1,6          RECORD HAS BEEN ADDED                        
         MVI   MYMSYS,X'FF'                                                     
         B     RECEND                                                           
*                                                                               
DELMSG   MVI   MYMSGNO1,7          RECORD HAS BEEN DELETED                      
         MVI   MYMSYS,X'FF'                                                     
         B     RECEND                                                           
*                                                                               
ERRXIT   XC    DMCB,DMCB                                                        
         B     THEEND                                                           
*                                                                               
ERPPLSI  MVC   MYMSGNO,=Y(ERRIAPPA)   RECORD / ACTION INVALID FOR P+            
         J     ERREND                                                           
                                                                                
ERREND   MVI   MYMTYP,GTMERR       ERROR MESSAGE EXIT                           
         J     INFEND                                                           
                                                                                
RECEND   LA    R2,CONRECH                                                       
INFEND   OI    GENSTAT2,USGETTXT                                                
THEEND   GOTO1 EXIT,DMCB,0                                                      
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 2                                                                
ALLFF    DC    8X'FF'                                                           
         SPACE 1                                                                
PFTAB    DS    0C                  PF KEYS TABLE                                
         DC    AL1(PF13X-*,13,0,0,0)                                            
         DC    CL3' ',CL8'DUECOMP ',CL8'DISPLAY '                               
PF13X    EQU   *                                                                
         DC    AL1(PF14X-*,14,0,(PF14X-PF14)/KEYLNQ,0)                          
         DC    CL3' ',CL8'CHECK   ',CL8'DISPLAY '                               
PF14     DC    AL1(KEYTYTWA,L'SDUCHK-1),AL2(SDUCHK-T702FFD)                     
PF14X    EQU   *                                                                
         DC    AL1(PF15X-*,15,0,0,PFTRETRN)                                     
         DC    CL3' ',CL8'        ',CL8'        '                               
PF15X    EQU   *                                                                
         DC    AL1(PF24X-*,24,0,0,PFTSETPN)                                     
         DC    CL3' ',CL8'        ',CL8'REPORT  '                               
PF24X    EQU   *                                                                
         DC    X'FF'                                                            
         SPACE 2                                                                
*              TABLE OF BLANKS TO FILL IN                                       
*              LINE #, ACTION, DISP TO DATA, DISP TO PRINT POSITION             
         SPACE 1                                                                
         DS    0D                                                               
BLNKTAB  DS    0CL12                                                            
         DC    AL1(1),AL1(0),AL2(0),AL4(0),AL4(0)                               
         DC    AL1(2),AL1(BLNKSPAC),AL2(L'DUCHK),AL4(DUCHK-DUD)                 
         DC    AL4(PRCHK-PRTD)                                                  
         DC    AL1(3),AL1(0),AL2(L'DUDTE),AL4(DUDTE-DUD)                        
         DC    AL4(PRDTE-PRTD)                                                  
         DC    AL1(3),AL1(0),AL2(L'DUGRS),AL4(DUGRS-DUD)                        
         DC    AL4(PRGRS-PRTD)                                                  
         DC    AL1(5),AL1(0),AL2(L'DUAGYNM),AL4(DUAGYNM-DUD)                    
         DC    AL4(PRAGY-PRTD)                                                  
         DC    AL1(6),AL1(0),AL2(L'DUCLINM),AL4(DUCLINM-DUD)                    
         DC    AL4(PRSPR-PRTD)                                                  
         DC    AL1(6),AL1(0),AL2(L'DUCID),AL4(DUCID-DUD)                        
         DC    AL4(PRCID-PRTD)                                                  
         DC    AL1(7),AL1(0),AL2(L'DUUSENM),AL4(DUUSENM-DUD)                    
         DC    AL4(PRUSENM-PRTD)                                                
         DC    AL1(7),AL1(0),AL2(L'DUCYC),AL4(DUCYC-DUD)                        
         DC    AL4(PRCYC-PRTD)                                                  
         DC    AL1(8),AL1(BLNKPRT),AL2(L'DUNR1),AL4(DUNR1-DUD)                  
         DC    AL4(PRNAR1-PRTD)                                                 
         DC    AL1(8),AL1(BLNKNTXT+BLNKPRT),AL2(L'DUNR2)                        
         DC    AL4(DUNR2-DUD),AL4(PRNAR2-PRTD)                                  
         DC    AL1(8),AL1(BLNKNTXT),AL2(L'DUNR3),AL4(DUNR3-DUD)                 
         DC    AL4(PRNAR3-PRTD)                                                 
         DC    AL1(9),AL1(BLNKSPAC),AL2(0),AL4(0),AL4(0)                        
         DC    AL1(14),AL1(BLNKSPAC),AL2(0),AL4(0),AL4(0)                       
         DC    AL1(18),AL1(BLNKSPAC),AL2(L'DUNETLIT)                            
         DC    AL4(DUNETLIT-DUD),AL4(PRNET-PRTD)                                
         DC    AL1(21),AL1(BLNKSPAC),AL2(0),AL4(0),AL4(0)                       
         DC    AL1(22),AL1(BLNKSPAC),AL2(0),AL4(0),AL4(0)                       
         DC    AL1(24),AL1(BLNKSPAC),AL2(0),AL4(0),AL4(0)                       
         DC    AL1(26),AL1(BLNKSPAC),AL2(L'DUAGYCD)                             
         DC    AL4(DUAGYCD-DUD),AL4(PRAGYCD-PRTD)                               
         DC    AL1(27),AL1(BLNKNTXT),AL2(L'DUSTFNM),AL4(DUSTFNM-DUD)            
         DC    AL4(PRSTFNM-PRTD)                                                
         DC    AL1(27),AL1(BLNKNTXT),AL2(L'DUINV),AL4(DUINV-DUD)                
         DC    AL4(PRINV-PRTD)                                                  
         DC    AL1(28),AL1(0),AL2(L'W4SSN),AL4(W4SSN-DUD)                       
         DC    AL4(PRSSN-PRTD)                                                  
         DC    X'FF'                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAGENDDTXT                                                     
         EJECT                                                                  
*              DSECT TO COVER WORKING STORAGE                                   
*                                                                               
DUD      DSECT                                                                  
DURERUN  DS    CL1                 RERUN FLAG                                   
DUMSGNO  DS    CL1                 SAVED MESSAGE FOR PRINT/STOP OV              
KEYCHANG DS    CL1                 Y = KEY CHANGED (RE-DISPLAY DEFLTS)          
*                                                                               
SVKEY    DS    CL(L'TLDRREC)       SAVED KEY                                    
SVPDTE   DS    XL3                 SAVED PRINTED DATE                           
DUW4TYPE DS    CL1                 W4 TYPE                                      
W4SSN    DS    CL9                 SOCIAL SECURITY NUMBER                       
W4NAME   DS    CL33                FIRST LAST NAME                              
W4ADD    DS    CL(4*39)            ADDRESS                                      
AGNTNAME DS    CL(4+36)            C/O - AGENT NAME                             
*                                                                               
DUAGYCD  DS    CL(L'TGAGY)         AGENCY CODE                                  
DUAGYNM  DS    CL(L'TGNAME)        AGENCY NAME                                  
DUCLINM  DS    CL(L'TGNAME)        CLIENT NAME                                  
*                                                                               
DUUSE    DS    CL(L'TADRUSE)       USE TYPE                                     
DUUSENM  DS    CL(L'TGUSNAME)      USE NAME W/O TYPE                            
DUCID    DS    CL(L'TADRCID)       COMMERCIAL ID                                
DUCYC    DS    CL17                DISPLAYABLE CYCLE DATES                      
*                                                                               
DUINV    DS    CL(L'TGINV)         INVOICE NUMBER                               
DUCHK    DS    CL(L'TADRCHK)       CHECK NUMBER                                 
DUDTE    DS    CL8                 CHECK DATE                                   
DUNCDE   DS    XL2                 AGENT CODE                                   
DUNNAMH  DS    CL8                 DUMMY SCREEN HEADER (FOR NAME)               
DUNNAME  DS    CL(L'TGNAME)        AGENT NAME                                   
DUGRS    DS    CL11                GROSS AMOUNT                                 
DUNET    DS    CL11                NET AMOUNT                                   
DUNETLIT DS    CL51                NET AMOUNT + LITERAL                         
*                                                                               
DUUID    DS    CL6                 USER ID                                      
DUSTAF   DS    CL8                 STAFF CODE                                   
DUSTFNM  DS    CL(L'TGNAME)        STAFF NAME                                   
DUPDTE   DS    CL8                 DATE PRINTED                                 
*                                                                               
DUNR1H   DS    CL8                 DUMMY FIELD HEADER FOR NARRATIVE             
DUNR1    DS    CL60                NARRATIVE                                    
DUNR2H   DS    CL8                                                              
DUNR2    DS    CL60                NARRATIVE                                    
DUNR3H   DS    CL8                                                              
DUNR3    DS    CL60                NARRATIVE                                    
         EJECT                                                                  
*              DSECT TO COVER BLNKTAB                                           
         SPACE 1                                                                
BLNKTABD DSECT                                                                  
BLNKCNT  DS    X                   LINE NUMBER                                  
BLNKACT  DS    X                   ACTION TO TAKE                               
BLNKSPAC EQU   X'80'               SKIP TWO SPACES BEFORE LINE                  
BLNKNTXT EQU   X'40'               DON'T MOVE TEXT TO PRINT LINE                
BLNKPRT  EQU   X'20'               DO PRINTIT                                   
BLNKLEN  DS    H                   LENGTH OF DATA                               
BLNKDATA DS    F                   DATA TO PRINT                                
BLNKDISP DS    F                   DISPLACEMENT FROM PRINT LINE                 
         SPACE 2                                                                
*              DSECT TO COVER PRINT LINE                                        
         SPACE 1                                                                
PRTD     DSECT                                                                  
         ORG   PRTD+75                                                          
PRLTDATE DS    CL5                 DATE:                                        
         DS    CL1                                                              
PRTDATE  DS    CL8                 MMM/DD/YY                                    
         ORG   PRTD                                                             
PRW4NAME DS    CL33                TALENT'S NAME                                
         ORG   PRTD                                                             
PRANAME  DS    CL40                AGENT NAME                                   
         ORG   PRTD                                                             
PRW4ADD  DS    CL39                TALENT'S ADDRESS OR AGENT'S ADDRESS          
         ORG   PRTD+72                                                          
PRCHK    DS    CL8                 CHECK NUMBER                                 
         ORG   PRTD+6                                                           
PRDTE    DS    CL8                 CHECK DATE                                   
         ORG   PRTD+80                                                          
PRGRS    DS    CL11                GROSS EARNINGS                               
         ORG   PRTD+20                                                          
PRAGY    DS    CL36                ADVERTISING AGENCY                           
         ORG   PRTD+9                                                           
PRSPR    DS    CL36                SPONSOR                                      
         ORG   PRTD+65                                                          
PRCID    DS    CL12                COMMERCIAL ID                                
         ORG   PRTD+17                                                          
PRUSENM  DS    CL16                TYPE OF PAYMENT                              
         ORG   PRTD+65                                                          
PRCYC    DS    CL17                CYCLE DATES                                  
         ORG   PRTD+28                                                          
PRNAR1   DS    CL40                EXPLAN. OF OVERPAYMENT (LINE 1)              
         ORG   PRTD+28                                                          
PRNAR2   DS    CL60                "  "  "      NARRATIVE (LINE 2)              
         ORG   PRTD+28                                                          
PRNAR3   DS    CL60                "  "  "      NARRATIVE (LINE 3)              
         ORG   PRTD+34                                                          
PRNET    DS    CL51                NET AMOUNT + LITERAL                         
         ORG   PRTD+50                                                          
PRSTFNM  DS    CL36                STAFF NAME                                   
         ORG   PRTD                                                             
PRAGYCD  DS    CL6                 AGENCY CODE                                  
         ORG   PRTD                                                             
PRINV    DS    CL6                 INVOICE NUMBER                               
         ORG   PRTD                                                             
PRSSN    DS    CL9                 SSN OR CORP ID#                              
*                                                                               
PRTLNQ   EQU   96                  LENGTH OF PRINT LINES                        
*                                                                               
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRDDD                                                       
         EJECT                                                                  
* TASYSIOD (MUST FOLLOW LAST SCREEN)                                            
* DDGENTWA (MUST FOLLOW LAST SCREEN)                                            
* TASYSEQUS                                                                     
* TAGENEQUS                                                                     
* TAGENWORKD                                                                    
* DDMASTD                                                                       
* DDTWADCONS                                                                    
* TAGENFILE                                                                     
* DDPERVALD                                                                     
* TASYSDSECT                                                                    
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
       ++INCLUDE TASYSIOD                                                       
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAGENEQUS                                                      
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'064TAGENDD   08/13/14'                                      
         END                                                                    
