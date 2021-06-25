*          DATA SET TAGENB7    AT LEVEL 020 AS OF 01/05/11                      
*PHASE T702B7B                                                                  
         TITLE 'T702B7 - FTRACK LIST'                                           
T702B7   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T702B7                                                         
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         L     R7,AIO3             R7=A(RATE CALC., SYSIO STORAGE)              
         USING TEMPD,R7                                                         
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
*                                                                               
         GOTO1 INITIAL,DMCB,PFTABLE                                             
*                                                                               
         TM    TGSYSTAT,TASYSPID                                                
         BZ    FT05                                                             
         MVC   SFTSHED(7),=C'Pid Num'                                           
         OI    SFTSHEDH+6,X'80'                                                 
         MVC   SFTPHED(8),=C'Pid Num '                                          
         OI    SFTPHEDH+6,X'80'                                                 
*                                                                               
FT05     CLI   MODE,VALKEY         FIRST TIME IN                                
         BNE   *+12                                                             
         BAS   RE,VKEY                                                          
         B     FTX                                                              
*                                                                               
         CLI   MODE,LVALREC        VALIDATE LISTED RECORD                       
         BNE   *+12                                                             
         BAS   RE,LVREC                                                         
         B     FTX                                                              
*                                                                               
FT10     CLI   MODE,LISTRECS                                                    
         BNE   FT30                                                             
         ZIC   R0,NLISTS           R0=N'LINES                                   
         LA    R2,SFTLHSH          R2=A(FIRST LINE)                             
         USING LINED,R2                                                         
FT20     OI    LINAMTH+1,X'20'     SET AMOUNT PROTECTED                         
         OI    LINAMTH+6,X'80'     TRANSMIT                                     
         OI    LINGUARH+1,X'20'                                                 
         OI    LINGUARH+6,X'80'                                                 
         LA    R2,LINNEXT                                                       
         BCT   R0,FT20                                                          
         LA    R0,LRHOOK           SET HOOK TO SYSIO                            
         BAS   RE,LREC                                                          
         B     FTX                                                              
*                                                                               
FT30     CLI   MODE,PRINTREP                                                    
         BNE   FTX                                                              
         XC    KEY,KEY             START REPORT FROM BEGINING                   
         ZAP   COUNTER,=P'0'       LINE COUNTER                                 
         LA    R2,FTSPECS          FTRACK                                       
         ST    R2,SPECS                                                         
         LA    R0,PRHOOK           SET HOOK TO SYSIO                            
         BAS   RE,LREC                                                          
*                                                                               
FTX      B     XIT                                                              
         SPACE 2                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
* VALIDATE THE KEY                                                              
*                                                                               
VKEY     NTR1                                                                   
         LA    RF,TEMPLNQ          CLEAR TEMP STORAGE AREAS                     
         XCEFL TEMPD                                                            
         SPACE 1                                                                
         LA    R2,SFTAGYH          AGENCY                                       
         TM    4(R2),X'20'                                                      
         BO    VK10                                                             
         NI    SFTCIDH+4,X'DF'                                                  
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'20',(R2))                                 
         L     R4,AIO                                                           
         MVI   ELCODE,TAAYELQ      GET AGENCY DETAILS EL.                       
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAAYD,R4                                                         
         MVC   AGYSTAT,TAAYSTAT    SAVE AGENCY STATUS                           
VK10     OI    4(R2),X'20'                                                      
*                                                                               
         LA    R2,SFTCIDH          COMMERCIAL ID (ALWAYS READ FOR CALC)         
         TM    4(R2),X'20'                                                      
         BO    *+8                                                              
         NI    SFTUSEH+4,X'DF'                                                  
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLCOICDQ,(X'08',(R2)),SFTCIDNH                       
         MVC   AIO,AIO1                                                         
         L     R4,AIO2                                                          
         USING TLCOD,R4                                                         
         MVC   TGCOM,TLCOCOM       SAVE INTERNAL COMMERCIAL NUMBER              
         SPACE 1                                                                
         MVI   ELCODE,TACOELQ      GET COMMERCIAL DETAILS EL.                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         ST    R4,TCATACO          SAVE ADDR. FOR RATE CALC. MODULE             
         USING TACOD,R4                                                         
         GOTO1 MEDVAL,DMCB,TACOMED SET GLOBAL MEDIA VALUES                      
         OI    4(R2),X'20'                                                      
*                                                                               
         LA    R2,SFTUSEH          USE CODE                                     
         OC    SFTUSE,SPACES                                                    
         TM    4(R2),X'20'                                                      
         BO    VK30                                                             
         NI    SFTINVH+4,X'DF'                                                  
         CLI   5(R2),0                                                          
         BE    VK30                                                             
         GOTO1 USEVAL,DMCB,(X'40',8(R2))                                        
         BNE   FLDINV                                                           
         TM    TGUSSTA2,APPREUSE   MUST BE APPLICABLE TOWARDS REUSE             
         BZ    FLDINV                                                           
*                                                                               
         TM    TGUSSTA3,ADDENUSE   AND IF ADDENDUM USE                          
         BZ    VK30                                                             
         TM    TGMEEQU,RADIO       AND RADIO COMMERCIAL                         
         BZ    VK28                                                             
         MVC   AIO,AIO2            SET RECORDING DATE                           
         MVI   ELCODE,TACSELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TACSTYPR))                                     
         MVC   AIO,AIO1                                                         
         BNE   VK28                                                             
         L     R4,TGELEM                                                        
         USING TACSD,R4                                                         
         MVC   TCRECDTE,TACSDATE                                                
*                                                                               
VK28     MVI   TGUSTYP,UADT13W     SET 13W TYPE (04) FOR ADT, ADC & ADO         
*                                                                               
VK30     OI    4(R2),X'20'                                                      
*                                                                               
         LA    R2,SFTINVH          INVOICE NUMBER                               
         TM    4(R2),X'20'                                                      
         BO    VK40                                                             
         NI    SFTPDH+4,X'DF'                                                   
         XC    TGINV,TGINV                                                      
         CLI   5(R2),0                                                          
         BE    VK40                                                             
         GOTO1 TINVCON,DMCB,8(R2),TGINV,DATCON                                  
         CLI   0(R1),X'FF'                                                      
         BE    FLDINV                                                           
         XC    TGINV,=6X'FF'                                                    
         GOTO1 RECVAL,DMCB,TLINCDQ,0                                            
         BNE   FLDINV                                                           
         XC    TGINV,=6X'FF'                                                    
VK40     OI    4(R2),X'20'                                                      
*                                                                               
         LA    R2,SFTPDH           PERIOD                                       
         TM    4(R2),X'20'                                                      
         BO    *+8                                                              
         NI    SFTSSNH+4,X'DF'                                                  
         LA    R3,BLOCK                                                         
         GOTO1 PDVAL,DMCB,(X'80',(R3))                                          
         USING PERVALD,R3                                                       
         CLI   5(R2),0             IF PERIOD INPUT                              
         BE    VK50                                                             
         CLC   PVALPSTA,PVALPEND   AND START=END (IMPLIES END NOT I/P)          
         BNE   VK50                                                             
         MVC   17(8,R2),SPACES     THEN SET TO GO BACK WITH 13W/3M PD.          
         MVC   17(5,R2),=C'(3M) '                                               
         TM    AGYSTAT,TAAYS13W                                                 
         BZ    *+10                                                             
         MVC   17(5,R2),=C'(13W)'                                               
         GOTO1 PDVAL,DMCB,(X'80',(R3))                                          
VK50     MVC   TCPCYC,PVALPSTA                                                  
         OI    4(R2),X'20'                                                      
*                                                                               
         LA    R2,SFTSSNH          S/S NUMBER                                   
         TM    4(R2),X'20'                                                      
         BO    VK60                                                             
         NI    SFTCATH+4,X'DF'                                                  
         CLI   5(R2),0                                                          
         BE    VK60                                                             
*                                                                               
         CLI   SFTSSNH+5,9         MUST CHECK FOR LENGTH HERE SINCE             
         BE    VK55                RECVAL CALL DOES NOT CHECK FOR               
         TM    TGSYSTAT,TASYSPID   ARE WE USING PID#                            
         BZ    FLDINV                                                           
         CLI   SFTSSNH+5,6         INVALID ENTRIES DUE TO X'40' PARAM           
         BNE   FLDINV                                                           
         MVC   TGPID,SFTSSN                                                     
         GOTO1 SSNUNPK,DMCB,TGPID,TGSSN                                         
         BNE   VK55                                                             
         MVC   SFTSSN,TGSSN                                                     
         MVI   SFTSSNH+5,9                                                      
*                                                                               
VK55     GOTO1 RECVAL,DMCB,TLW4CDQ,(R2)                                         
*                                                                               
         TM    TGSYSTAT,TASYSPID   ARE WE USING PID#                            
         BZ    VK60                                                             
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   SFTSSN,SPACES                                                    
         MVC   SFTSSN(L'TGPID),TGPID                                            
         MVI   SFTSSNH+5,6                                                      
         OI    SFTSSNH+6,X'80'                                                  
*                                                                               
VK60     OI    4(R2),X'20'                                                      
*                                                                               
         LA    R2,SFTCATH          CATEGORY                                     
         OC    SFTCAT,SPACES                                                    
         TM    4(R2),X'20'                                                      
         BO    VK70                                                             
         NI    SFTOPTH+4,X'DF'                                                  
         CLI   5(R2),0                                                          
         BE    VK70                                                             
         GOTO1 CATVAL,DMCB,8(R2)                                                
         BNE   FLDINV                                                           
VK70     OI    4(R2),X'20'                                                      
*                                                                               
         LA    R2,SFTOPTH          OPTIONS                                      
         TM    4(R2),X'20'         ALREADY VALIDATED                            
         BO    VK100                                                            
         MVI   OPTS,0              CLEAR OPTIONS STATUS                         
         CLI   5(R2),0                                                          
         BE    VK90                                                             
         LA    R3,BLOCK            SET FOR SCANNER                              
         USING SCAND,R3                                                         
         GOTO1 SCANNER,DMCB,(R2),(R3)                                           
         ZIC   R0,4(R1)                                                         
         LTR   R0,R0               INVALID INPUT                                
         BZ    FLDINV                                                           
*                                                                               
VK80     DS    0H                                                               
         CLC   =C'ALL',SCDATA1     OPTION TO DISPLAY ALL CYCLES FOR PER         
         BNE   *+12                                                             
         OI    OPTS,OPTALL         SET LOCAL STATUS                             
         B     VK90                                                             
*                                                                               
         CLC   =C'ADD',SCDATA1     OPTION TO ADD NEW RECORDS                    
         BNE   FLDINV                                                           
         OI    OPTS,OPTADD         SET ADDING                                   
*                                                                               
         CLI   SFTUSEH+5,0         REQUIRE USE TYPE                             
         BE    USEMISS                                                          
         CLI   SFTPDH+5,0          AND PERIOD                                   
         BE    PDMISS                                                           
*                                                                               
         LA    R3,SCANNEXT                                                      
         BCT   R0,VK80                                                          
VK90     OI    4(R2),X'20'                                                      
*                                                                               
         XC    KEY,KEY             RESTART LIST - CLEAR KEY                     
*                                                                               
VK100    DS    0H                                                               
         OI    GLSTSTAT,APPLCDSP+RETEXTRA  SET I DISPLAY/RETURN EXTRA           
         MVC   LLIST,=Y(LINLNQ)            AND L'DATA LINE                      
         MVI   NLISTS,15                   AND N'LINES                          
*                                                                               
         TM    OPTS,OPTADD         IF ADDING NEW RECORDS                        
         BZ    VKX                                                              
         OI    GLSTSTAT,CHNGLIST   LET GENCON KNOW NEED TO VALIDATE             
*                                                                               
VKX      B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO CONTROL LISTING RECORDS                               
         SPACE 1                                                                
LREC     NTR1                                                                   
         ST    R0,TIHOOK           SET HOOK TO SYSIO                            
         MVC   TIACOMFC,ACOMFACS                                                
         MVC   TIUSERID,TWAORIG    SET UP NECCESSARY DATA FROM                  
         MVC   TIQSTAFF,TGCTSTAF       GLOBAL STORAGE                           
         MVI   TIREAD,TLCACDQ      READ CAST RECORDS                            
         MVC   TIFCOM,TGCOM        INTERNAL COMMERCIAL NUMBER                   
         MVC   TIFSSN,SFTSSN       S/S NUMBER                                   
         TM    TGSYSTAT,TASYSPID   ARE WE USING PID#                            
         BZ    LR05                                                             
         MVC   TGPID,SFTSSN                                                     
         GOTO1 SSNUNPK,DMCB,TGPID,TIFSSN                                        
*                                                                               
LR05     MVC   TIFCAT,SFTCAT       CATEGORY                                     
         MVC   TIFUSE,SFTUSE       USE CODE                                     
         MVC   TIFINV,TGINV        INVOICE                                      
*                                                                               
         GOTO1 SETLSTK,DMCB,(TIREAD,TIQSKEY)  SET KEY OURSELVES                 
         SPACE 1                                                                
         OC    KEY,KEY             IF WE'RE IN MIDDLE OF LIST                   
         BZ    LR10                                                             
         CLC   DMDSKADD,VERYFRST   AND CURRENT KEY IS VERY FIRST                
         BNE   LR10                                                             
         CLI   LISTSW,C'N'         OR NO PFKEY PRESSED                          
         BNE   LR10                                                             
         TM    MYSTAT,NEXTPEND     AND WE COULDN'T FIT PREV. RECORD             
         BZ    LR10                                                             
         GOTO1 SEQ                 NEED TO START WITH NEXT RECORD               
         MVC   TIQSKEY,KEY                                                      
LR10     NI    MYSTAT,X'FF'-NEXTPEND                                            
*                                                                               
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
         XC    TIQSKEY,TIQSKEY                                                  
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BNE   LRX                                                              
         CP    COUNTER,=P'0'                                                    
         BE    LRX                                                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
         EDIT  COUNTER,(8,P),ALIGN=LEFT                                         
         LR    R1,R0                                                            
         LA    R1,P+1(R1)                                                       
         MVC   0(14,R1),=C'FTRACK RECORDS'                                      
         GOTO1 SPOOL,DMCB,(R8)                                                  
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
         CLI   TIMODE,PROCREC      PROCESS RECORDS                              
         BNE   LRHX                                                             
         BAS   RE,TESTEND          TEST END OF LIST                             
         BE    LRHX                                                             
         BAS   RE,ANYTACR          IF NO APPLIED CREDIT HISTORY EL              
         BNE   LRHX                   GET OUT                                   
         CLC   LISTNUM,NLISTS      IF ALREADY DISPLAYED MAX                     
         BE    LRH90                  GO BACK TO LISTMON                        
*                                                                               
         L     R2,ATHISLST         SET A(THIS LINE)                             
         USING LINED,R2                                                         
         BAS   RE,LININIT          INITIALIZE LINE                              
*                                                                               
         BAS   RE,COUNTCR          ENSURE ENOUGH ROOM ON PAGE                   
         BE    LRH10                                                            
         OI    MYSTAT,NEXTPEND     SET ANOTHER RECORD PENDING                   
         MVC   LINSSN(L'LTMORE),LTMORE  DISPLAY MORE PENDING MESSAGE            
         ZIC   R0,NLISTS                                                        
         ZIC   R1,LISTNUM                                                       
         LA    R1,1(R1)                                                         
         SR    R0,R1                                                            
         BZ    *+16                                                             
         LA    R2,LINNEXT                                                       
         BAS   RE,LININIT          CLEAR REMAINING LINES                        
         BCT   R0,*-8                                                           
         B     ENDPAGE                                                          
*                                                                               
LRH10    TM    OPTS,OPTADD         IF ADDING NEW RECORDS                        
         BZ    LRH20                                                            
         XC    TCPAY,TCPAY                                                      
         MVI   TCINPUT,0           CLEAR INPUT OVERRIDE                         
         XC    TCOVAMT,TCOVAMT                                                  
         MVI   TCCASTST,0                                                       
         MVC   TCACAST,TIAREC      SET A(CAST RECORD)                           
         BAS   RE,GETRATE          GET RATE FOR THIS PERF.                      
*                                                                               
LRH20    MVC   LINSSN,TISSN        SS NUMBER                                    
*                                                                               
         TM    TGSYSTAT,TASYSPID                                                
         BZ    LRH30                                                            
         MVC   LINSSN,SPACES                                                    
         GOTO1 SSNPACK,DMCB,TISSN,LINSSN                                        
*                                                                               
LRH30    MVC   LINCAT,TICAT        CATEGORY                                     
*                                                                               
         L     R4,TIAREC                                                        
         USING TAHFD,R4                                                         
         MVI   ELCODE,TAHFELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   LRH40                                                            
         GOTO1 DATCON,DMCB,(1,TAHFNXTS),(8,LINHFN) HFN DATE                     
         SPACE 1                                                                
LRH40    BAS   RE,GETTACR          GET (NEXT) TACR ELEMENT                      
         BNE   LRHX                                                             
         MVC   LINSSN2,TISSN       SAVE SSN & CAST SEQ FOR PFKEYS               
         MVC   LINSEQ,CASTSEQ                                                   
         SPACE 1                                                                
         L     R4,TGELEM           A(TACR EL RETURNED BY GETTACR)               
         USING TACRD,R4                                                         
         OC    TACRINV,TACRINV                                                  
         BZ    LRH50                                                            
         CLC   TACRINV,=6X'FF'                                                  
         BE    LRH50                                                            
         GOTO1 TINVCON,DMCB,TACRINV,LININV,DATCON                               
*                                                                               
LRH50    MVC   LINUSE,TACRUSE      USE CODE                                     
         TM    TACRSTAT,TACRSGUA   TEST THIS IS GUARANTEE                       
         BNO   *+8                                                              
         MVI   LINGUAR,C'Y'                                                     
         GOTO1 DATCON,DMCB,(X'11',TACRSTRT),(8,LINPER) PERIOD                   
         EDIT  TACRAPPL,(10,LINAMT),2,FLOAT=-          APPLIED AMOUNT           
         EDIT  TACRBAL,(10,LINBAL),2,FLOAT=-           BALANCE                  
*                                                                               
         MVC   DMDSKADD,TIDSKADD   PASS DISK ADDRESS                            
         SPACE 1                                                                
LRH90    GOTO1 LISTMON             CALL LISTMON                                 
         SPACE 1                                                                
         TM    OPTS,OPTALL         TEST DISPLAYING ALL ELS.                     
         BZ    LRHX                                                             
         CLC   LISTNUM,NLISTS      IF WE'VE DISPLAYED MAX                       
         BL    *+12                                                             
         OI    MYSTAT,NEXTPEND     SET NEXT PEND IN CASE FIT ONLY 1ST           
         B     LRHX                AND GET OUT                                  
         SPACE 1                                                                
         MVI   TACREL,X'FF'        ELSE DELETE THIS ELEMENT                     
         MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM                                                          
         L     R2,ATHISLST         SET A(NEW LINE)                              
         BAS   RE,LININIT          INITIALIZE LINE                              
         B     LRH40               AND GO LOOK FOR ANOTHER ONE                  
         SPACE 1                                                                
LRHX     B     XIT                                                              
         EJECT                                                                  
*              PROCESS SYSIO RECORDS WHEN PRINTING                              
         SPACE 1                                                                
PRHOOK   NTR1                                                                   
         CLI   TIMODE,PROCREC      PROCESS RECORDS                              
         BNE   PRHX                                                             
         BAS   RE,TESTEND          TEST END OF LIST                             
         BE    PRHX                                                             
         BAS   RE,ANYTACR          IF NO APPLIED CREDIT HISTORY ELS             
         BNE   LRHX                   GET OUT                                   
         LA    R2,P                R2=A(PRINT LINE)                             
         USING PRNTD,R2                                                         
         SPACE 1                                                                
         MVC   PRNTSSN,TISSN       SS NUMBER                                    
         MVC   PRNTCAT,TICAT       CATEGORY                                     
         SPACE 1                                                                
         L     R4,TIAREC                                                        
         USING TAHFD,R4                                                         
         MVI   ELCODE,TAHFELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   PRH30                                                            
         GOTO1 DATCON,DMCB,(1,TAHFNXTS),(8,PRNTHFN) HFN DATE                    
         SPACE 1                                                                
PRH30    BAS   RE,GETTACR          GET (NEXT) TACR ELEMENT                      
         BNE   PRHX                                                             
         L     R4,TGELEM           A(TACR EL RETURNED BY GETTACR)               
         USING TACRD,R4                                                         
         OC    TACRINV,TACRINV                                                  
         BZ    PRH50                                                            
         CLC   TACRINV,=6X'FF'                                                  
         BE    PRH50                                                            
         GOTO1 TINVCON,DMCB,TACRINV,PRNTINV,DATCON                              
         SPACE 1                                                                
PRH50    MVC   PRNTUSE,TACRUSE     USE CODE                                     
         TM    TACRSTAT,TACRSGUA   TEST THIS IS GUARANTEE                       
         BNO   *+8                                                              
         MVI   PRNTGUAR,C'Y'                                                    
         GOTO1 DATCON,DMCB,(X'11',TACRSTRT),(8,PRNTPER) PERIOD                  
         EDIT  TACRAPPL,(10,PRNTAMT),2,FLOAT=-          APPLIED AMOUNT          
         EDIT  TACRBAL,(10,PRNTBAL),2,FLOAT=-           BALANCE                 
         SPACE 1                                                                
PRH80    GOTO1 SPOOL,DMCB,(R8)                                                  
         AP    COUNTER,=P'1'                                                    
         SPACE 1                                                                
         TM    OPTS,OPTALL         IF DISPLAYING ALL ELS.                       
         BZ    PRHX                                                             
         MVI   TACREL,X'FF'        THEN DELETE THIS ELEMENT                     
         MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM                                                          
         B     PRH30               AND GO LOOK FOR ANOTHER ONE                  
         SPACE 1                                                                
PRHX     B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TESTS WHETHER WE'VE REACHED LOGICAL END OF LIST          
         SPACE 1                                                                
TESTEND  NTR1                                                                   
         L     R4,TIAREC                                                        
         USING TLCAD,R4                                                         
         GOTO1 HEXOUT,DMCB,TLCASEQ,FULL,2,0  CVT CAST SEQ NO. TO CHAR           
         MVC   CASTSEQ,FULL+1                SAVE 3 LOBS                        
         SPACE 1                                                                
         MVI   ELCODE,TACAELQ      STOP WHEN REACH FIRST MUSICIAN               
         BAS   RE,GETEL                                                         
         BNE   NO                                                               
         USING TACAD,R4                                                         
         CLC   TACAUN,=C'AFM'                                                   
         BNE   NO                                                               
         MVC   KEY(L'TLCAKEY),TIKEY                                             
         LA    R4,KEY              MAKE SYSIO END ITS READ                      
         USING TLCAD,R4                                                         
         MVI   TLCACOM,X'FF'                                                    
         GOTO1 HIGH                                                             
         B     YES                                                              
         SPACE 3                                                                
*              ROUTINE TO INITIALIZE SCREEN LINE                                
         SPACE 1                                                                
         USING LINED,R2            R2=A(LINE)                                   
LININIT  NTR1                                                                   
         TM    OPTS,OPTADD         IF ADDING NEW RECORDS                        
         BZ    LIN20                                                            
         NI    LINAMTH+1,X'DF'     THEN UNPROTECT AMOUNT FIELD                  
         NI    LINGUARH+1,X'DF'                                                 
         LA    R3,X'13'            SET TO CLEAR / INVALIDATE LINE               
         B     LIN30                                                            
*                                                                               
LIN20    OI    LINAMTH+1,X'20'     ELSE PROTECT AMOUNT FIELD                    
         OI    LINGUARH+1,X'20'                                                 
         LA    R3,X'23'            SET TO CLEAR / VALIDATE LINE                 
*                                                                               
LIN30    GOTO1 FLDVAL,DMCB,((R3),LINLHSH),LINRHSH  CLEAR/SET VER LINE           
         B     XIT                                                              
         EJECT                                                                  
*        COUNT NUMBER OF TACR ELEMENTS TO DISPLAY                               
*        & ENSURE THERE IS ENOUGH ROOM ON PAGE                                  
         SPACE 1                                                                
COUNTCR  NTR1                                                                   
         TM    OPTS,OPTALL         IF OPTION ALL THEN                           
         BZ    YES                                                              
         MVI   ELCODE,TACRELQ      LOOK FOR TACR ELEMENTS                       
         L     R4,TIAREC                                                        
         XR    R3,R3               COUNTER                                      
         BAS   RE,GETEL                                                         
         LA    R3,1(R3)            INCREMENT COUNTER                            
         BAS   RE,NEXTEL                                                        
         BE    *-8                                                              
*                                                                               
         ZIC   R1,LISTNUM          THIS LINE NUMBER MUST BE LESS THAN           
         AR    R1,R3               + LINES FOR THIS RECORD                      
         CLM   R1,1,NLISTS         MUST BE LESS THAN TOTAL # OF LINES           
         BNH   YES                                                              
         CLM   R3,1,NLISTS         IF THIS PERSON HAS MORE THAN MAX             
         BNH   NO                                                               
         CLI   LISTNUM,0           AND AT START OF NEW PAGE                     
         BH    NO                                                               
         B     YES                 THEN DISPLAY AS MUCH AS POSSIBLE NOW         
         EJECT                                                                  
*              ROUTINE TO GET THE (NEXT) TACR ELEMENT                           
*              RETURNS ITS ADDRESS IN R4 AND SETS CC EQ IF FINDS IT             
*              ELSE RETURNS CC NOT EQ                                           
*                                                                               
GETTACR  NTR1                                                                   
         XC    TGELEM,TGELEM       CLEAR SAVED ADDRESS AREA                     
*                                                                               
         L     R4,TIAREC                                                        
         MVI   ELCODE,TACRELQ      GET 1ST ELEMENT                              
         BAS   RE,GETEL                                                         
         BNE   GTCRX                                                            
*                                                                               
         USING TACRD,R4            R4=A(APPLIED CREDIT HISTORY EL.)             
GTCR4    BAS   RE,FILTTACR         APPLY FILTERS                                
         BNE   *+8                                                              
         ST    R4,TGELEM           SAVE A(LAST ONE FOUND)                       
*                                                                               
         BAS   RE,NEXTEL           TRY NEXT ELEMENT                             
         BE    GTCR4                                                            
*                                                                               
GTCRX    OC    TGELEM,TGELEM       DID WE FIND AN ELEMENT                       
         BZ    NO                  NO - RETURN CC NE                            
         B     YES                 RETURN CC EQ                                 
         EJECT                                                                  
*              LOOK FOR TACREL FOR THIS PERFORMER                               
         SPACE 1                                                                
ANYTACR  NTR1                                                                   
         L     R4,TIAREC                                                        
         MVI   ELCODE,TACRELQ                                                   
         BAS   RE,GETEL            IF NO APPLIED CREDIT HISTORY EL              
         BE    ANYCR2                                                           
         TM    OPTS,OPTADD         AND ADDING NEW RECORDS                       
         BO    YES                 THEN RETURN CC EQ TO PROCESS                 
         B     NO                  ELSE REJECT THIS PERFORMER                   
*                                                                               
ANYCR2   TM    OPTS,OPTADD         IF ADDING NEW RECORDS                        
         BO    NO                  THEN REJECT IF FTRACKS EXIST                 
         B     YES                 ELSE OK TO CONTINUE                          
         SPACE 3                                                                
*              ROUTINE TO FILTER TACREL SELECTION                               
         SPACE 1                                                                
         USING TACRD,R4            R4=A(APPLIED CREDIT HISTORY EL.)             
FILTTACR NTR1                                                                   
         CLC   TIFUSE,SPACES       IF USE CODE FILTER PRESENT                   
         BNH   *+14                                                             
         CLC   TACRUSE,TIFUSE      THEN SKIP IF NO MATCH                        
         BNE   NO                                                               
*                                                                               
         OC    TIFINV,TIFINV       IF INVOICE NUMBER FILTER PRESENT             
         BZ    *+14                                                             
         CLC   TACRINV,TIFINV      THEN SKIP IF NO MATCH                        
         BNE   NO                                                               
*                                                                               
         OC    TCPCYCS,TCPCYCS     IF START DATE INPUT                          
         BZ    *+14                                                             
         CLC   TACRSTRT,TCPCYCS    MUST BE GE START DATE                        
         BL    NO                                                               
*                                                                               
         OC    TCPCYCE,TCPCYCE     IF END DATE INPUT                            
         BZ    *+14                                                             
         CLC   TACRSTRT,TCPCYCE    MUST BE LE END DATE                          
         BH    NO                                                               
*                                                                               
         ICM   R3,15,TGELEM        IF WE HAVE A SAVED EL. ALREADY               
         BZ    FILTX                                                            
         CLC   TACRSTRT(6),TACRSTRT-TACRD(R3)  AND CYCLE DATES THE SAME         
         BNE   FILTX                                                            
         CLC   TACRINV,TACRINV-TACRD(R3)       THEN INV# MUST BE LARGER         
         BL    NO                                                               
*                                                                               
FILTX    B     YES                                                              
         EJECT                                                                  
*              GET RATE FOR THIS PERFORMER                                      
         SPACE 1                                                                
GETRATE  NTR1                                                                   
         XC    TCTOTS(TCTOTLNQ),TCTOTS  CLEAR ACCUMS                            
         SPACE 1                                                                
         L     R4,TCACAST          R4=A(CAST RECORD) FOR CALC.                  
         USING TLCAD,R4                                                         
         GOTO1 CATVAL,DMCB,TLCACAT  SET CATEGORY DETAILS                        
         SPACE 1                                                                
         MVI   ELCODE,TACAELQ      GET CAST DETAILS EL.                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACAD,R4            R4=A(CAST DETAILS EL.)                       
         GOTO1 SETOV2,DMCB,(R4),TCACAST,TGUSCDE                                 
         SPACE 1                                                                
         GOTO1 UNIVAL,DMCB,TACAUN   SET UNION DETAILS                           
         GOTO1 YRVAL,DMCB,TACAYEAR  AND YEAR DETAILS                            
         SPACE 1                                                                
         MVC   AIO,TCACAST                                                      
         GOTO1 GETOV1,DMCB,TGUSCDE,TCOV1  1ST OVERSCALE RATE                    
         MVC   AIO,AIO1                                                         
         CLI   0(R1),X'FF'         IF IT'S AN AMOUNT                            
         BNE   GRTE8                                                            
         MVC   TCPAY,TCOV1         SAVE AS PAYMENT                              
         OI    TCINPUT,TCINPAY     SET WE HAVE OVERRIDE                         
         MVC   TCOVAMT,TCPAY       SAVE IT IN CAST OVERRIDE FIELDS TOO          
         OI    TCCASTST,TCCAOVAM   SET CORRESPONDING STATUS                     
         XC    TCOV1,TCOV1         CLEAR RATE                                   
         SPACE 1                                                                
GRTE8    MVC   TCCADBL,TACADBL     N'DOUBLES                                    
         MVC   TCOV2,TACAOV2       2ND OVERSCALE RATE                           
         MVC   TCCASTAT,TACASTAT   STATUS BYTE                                  
         MVC   TCCAONOF,TACAONOF   ON/OFF CAMERA                                
         MVC   TGGUA,TACAGUA       GUARANTEE CODE                               
         OI    TCOPTS,TCNOCKDL     DON'T CHECK FOR DLR CYCLES                   
         DROP  R4                                                               
         SPACE 1                                                                
         GOTO1 TASYSCLC,DMCB,(RC),TCD,SYSCOMM  CALCULATE RATES/ADD ELEM         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE VALIDATES LISTED RECORDS                                 
         SPACE 2                                                                
LVREC    NTR1                                                                   
         MVI   IOOPT,C'Y'          MAKE GENCON THINK I'M DOING WRITE            
         SPACE 1                                                                
         L     R2,ATHISLST         R2=A(THIS DATA LINE)                         
         USING LINED,R2                                                         
         LR    R4,R2                                                            
         SH    R4,=Y(LINNEXT-LINSELH)  R4=A(SELECT FLD FOR THIS LINE)           
         CLI   8(R4),C'A'          TEST OK TO ADD THIS LINE                     
         BNE   XIT                                                              
         CLI   LINAMTH+5,0         TEST AMOUNT INPUT                            
         BE    XIT                                                              
         SPACE 1                                                                
         ZIC   R3,LINAMTH+5                                                     
         GOTO1 CASHVAL,DMCB,LINAMT,(R3)  VALIDATE AMOUNT                        
         SPACE 1                                                                
         CLI   0(R1),X'FF'                                                      
         BE    AMTINV                                                           
         TM    4(R1),X'80'         INSURE AMOUNT ISN'T NEGATIVE                 
         BO    AMTINV                                                           
         SPACE 1                                                                
         MVC   TCPAY,4(R1)         SET PAYMENT AMOUNT OVERRIDE                  
         OI    TCINPUT,TCINPAY                                                  
         SPACE 1                                                                
         XC    TCOVAMT,TCOVAMT                                                  
         MVI   TCCASTST,0                                                       
         CLI   LINGUAR,C'Y'        IF AMOUNT WAS FROM CAST                      
         BNE   *+14                                                             
         MVC   TCOVAMT,TCPAY       SAVE IT IN CAST OVERRIDE FIELDS TOO          
         OI    TCCASTST,TCCAOVAM   SET CORRESPONDING STATUS                     
         SPACE 1                                                                
         GOTO1 ASAVPTRS,DMCB,PTRBLK  SAVE POINTERS                              
         SPACE 1                                                                
         MVC   TCACAST,AIO         SET A(CAST RECORD)                           
         BAS   RE,GETRATE          GO TO RATE CALC. TO ADD ELEMENT              
         SPACE 1                                                                
         MVI   IOOPT,C'N'          SET OK FOR GENCON TO WRITE BACK REC          
         SPACE 1                                                                
         XC    8(L'LINSEL,R4),8(R4)  CLEAR SELECT FIELD                         
         MVI   5(R4),0                                                          
         OI    6(R4),X'80'                                                      
         SPACE 1                                                                
         GOTO1 AADDPTRS,DMCB,PTRBLK  UPDATE POINTERS                            
         B     XIT                                                              
         EJECT                                                                  
*              ERRORS, EXITS                                                    
*                                                                               
FLDINV   MVI   ERROR,INVALID                                                    
         B     THEEND                                                           
*                                                                               
USEMISS  LA    R2,SFTUSEH                                                       
         B     FLDMISS                                                          
PDMISS   LA    R2,SFTPDH                                                        
FLDMISS  MVI   ERROR,MISSING                                                    
         B     THEEND                                                           
*                                                                               
AMTINV   MVI   ERROR,ERINVAMT                                                   
         USING LINED,R2                                                         
         LA    R2,LINAMTH                                                       
         B     THEEND                                                           
*                                                                               
ENDPAGE  MVC   MYMSGNO1,OKNO       MSG - HIT ENTER FOR NEXT                     
         MVI   MYMSYS,X'FF'                                                     
         OI    GENSTAT2,USGETTXT                                                
         LA    R2,SFTSELH                                                       
         B     THEEND                                                           
*                                                                               
THEEND   GOTO1 EXIT,DMCB,0                                                      
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
*                                                                               
PFTABLE  DS    0H                                                               
         DC    AL1(EPF13X-*,13,0,(EPF13X-EPF13)/KEYLNQ,0)                       
         DC    CL3'CH ',CL8'CHECK   ',CL8'LIST'                                 
EPF13    DC    AL1(KEYTYCUR,L'TGSSN-1),AL2(LINSSN2-LINLHS)                      
         DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYGLB,L'TGCID-1),AL2(TGCID-TGD)                           
EPF13X   EQU   *                                                                
*                                                                               
         DC    AL1(LPF14DX-*,14,0,(LPF14DX-LPF14D)/KEYLNQ,0)                    
         DC    CL3'FT ',CL8'FTRACK  ',CL8'REPORT'                               
LPF14D   DC    AL1(KEYTYCUR,L'TGSSN-1),AL2(LINSSN2-LINLHS)                      
         DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYGLB,L'TGCID-1),AL2(TGCID-TGD)                           
         DC    AL1(KEYTYCUR,L'TGCAT-1),AL2(LINSEQ-LINLHS)                       
         DC    AL1(KEYTYCUR,16),AL2(LINPER-LINLHS)                              
LPF14DX  EQU   *                                                                
         DC    X'FF'                                                            
         SPACE 2                                                                
LTMORE   DC    C'(More on next page)'                                           
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              SPECS FOR REPORT                                                 
         SPACE 1                                                                
FTSPECS  DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,56,REPORT                                                     
         SSPEC H1,73,PAGE                                                       
         SSPEC H2,56,REQUESTOR                                                  
         SPACE 1                                                                
         SSPEC H1,33,C'FTRACK LIST'                                             
         SSPEC H2,33,C'-----------'                                             
         SPACE 1                                                                
         SSPEC H4,2,C'S/S NUMB  CAT PERIOD'                                     
         SSPEC H5,2,C'--------  --- ------'                                     
         SSPEC H4,34,C'G'                                                       
         SSPEC H5,34,C'-'                                                       
         SSPEC H4,40,C'AMOUNT    BALANCE USE INV    HFN DATE'                   
         SSPEC H5,40,C'------    ------- --- ---    --------'                   
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
*              DSECT TO COVER SCREEN LINE                                       
         SPACE 1                                                                
LINED    DSECT                                                                  
LINLHSH  DS    CL8                                                              
LINLHS   DS    CL(L'SFTLHS)                                                     
         ORG   LINLHS                                                           
LINSSN   DS    CL9                                                              
         DS    CL1                                                              
LINCAT   DS    CL3                                                              
         DS    CL1                                                              
LINPER   DS    CL17                                                             
*                                                                               
LINNOPH  DS    CL8                                                              
LINNOP   DS    CL(L'SFTNOP)                                                     
         ORG   LINNOP                                                           
LINSSN2  DS    CL9                                                              
LINSEQ   DS    CL3                                                              
*                                                                               
LINGUARH DS    CL8                                                              
LINGUAR  DS    CL1                                                              
LINGUARX DS    CL8                                                              
*                                                                               
LINAMTH  DS    CL8                                                              
LINAMT   DS    CL10                                                             
LINAMTX  DS    CL8                                                              
*                                                                               
LINRHSH  DS    CL8                                                              
LINRHS   DS    CL(L'SFTRHS)                                                     
         ORG   LINRHS                                                           
LINBAL   DS    CL10                                                             
         DS    CL1                                                              
LINUSE   DS    CL3                                                              
         DS    CL1                                                              
LININV   DS    CL6                                                              
         DS    CL1                                                              
LINHFN   DS    CL8                                                              
         ORG                                                                    
LINLNQ   EQU   *-LINED                                                          
*                                                                               
LINSELH  DS    CL8                 A(SELECT FIELD ON NEXT LINE)                 
LINSEL   DS    CL3                                                              
LINSELX  DS    CL8                                                              
LINNEXT  EQU   *                   A(NEXT DATA LINE)                            
         EJECT                                                                  
*              DSECT TO COVER PRINT LINE                                        
         SPACE 1                                                                
PRNTD    DSECT                                                                  
         DS    CL1                                                              
PRNTSSN  DS    CL9                                                              
         DS    CL1                                                              
PRNTCAT  DS    CL3                                                              
         DS    CL1                                                              
PRNTPER  DS    CL17                                                             
         DS    CL1                                                              
PRNTGUAR DS    CL1                                                              
         DS    CL1                                                              
PRNTAMT  DS    CL10                                                             
         DS    CL1                                                              
PRNTBAL  DS    CL10                                                             
         DS    CL1                                                              
PRNTUSE  DS    CL3                                                              
         DS    CL1                                                              
PRNTINV  DS    CL6                                                              
         DS    CL1                                                              
PRNTHFN  DS    CL8                                                              
         EJECT                                                                  
*              TEMPORARY STORAGE AREAS                                          
         SPACE 1                                                                
TEMPD    DSECT                                                                  
COUNTER  DS    PL4                 LINE COUNTER                                 
PTRBLK   DS    CL(6*L'TLDRREC+1)   BLOCK FOR POINTER MAINTENANCE                
         SPACE 2                                                                
       ++INCLUDE TASYSCALCD                                                     
         EJECT                                                                  
       ++INCLUDE TASYSIOD                                                       
         SPACE 1                                                                
TEMPLNQ  EQU   *-TEMPD             L'TEMP STORAGE AREAS                         
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRB7D                                                       
         SPACE 3                                                                
*              LOCAL SAVED STORAGE AT END OF TWA0                               
         SPACE 1                                                                
AGYSTAT  DS    XL1                 AGENCY STATUS BYTE                           
OPTS     DS    XL1                 OPTIONS                                      
OPTADD   EQU   X'80'               ADDING NEW RECORDS                           
OPTALL   EQU   X'40'               DISPLAY ALL CYCLES FOR PERF.                 
MYSTAT   DS    XL1                                                              
NEXTPEND EQU   X'80'               ANOTHER RECORD PENDING                       
CASTSEQ  DS    CL3                 CAST SEQUENCE NUMBER OF CURRENT REC.         
         EJECT                                                                  
* DDGENTWA                                                                      
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
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020TAGENB7   01/05/11'                                      
         END                                                                    
