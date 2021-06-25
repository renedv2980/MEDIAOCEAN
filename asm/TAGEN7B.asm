*          DATA SET TAGEN7B    AT LEVEL 005 AS OF 07/20/12                      
*PHASE T7027BC                                                                  
         TITLE 'T7027B - CYCLE DISPLAY'                                         
T7027B   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 CMCTBLNQ,T7027B,R7                                               
         LR    R6,RC                                                            
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         ST    R6,ACMCTB                                                        
         AHI   R6,CMCTBLNQ/2                                                    
         AHI   R6,CMCTBLNQ/2                                                    
         ST    R6,ACMCTBX                                                       
         SPACE 1                                                                
         LA    R6,TWAHOLE                                                       
         USING TWD,R6                                                           
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
         SPACE 1                                                                
         GOTO1 INITIAL,DMCB,0                                                   
         SPACE 1                                                                
         GOTO1 FLDVAL,DMCB,(X'02',CYCAGYH),999                                  
         SPACE 1                                                                
         TM    TGSYSTAT,TASYSPID   CHANGE SCREEN IF SHOWING PID                 
         BZ    VCYC10                                                           
         MVC   CYCHED(3),=C'Pid'                                                
         SPACE 1                                                                
VCYC10   BRAS  RE,VKEY                                                          
         SPACE 1                                                                
VCYC20   CLI   MODE,DISPREC                                                     
         BNE   VCYC30                                                           
         BRAS  RE,DREC                                                          
         SPACE 1                                                                
VCYC30   CLI   MODE,VALREC                                                      
         BNE   XIT                                                              
         BRAS  RE,VREC                                                          
         SPACE 1                                                                
XIT      XIT1                                                                   
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              ROUTINE VALIDATES KEY FIELDS AND BUILDS THE KEY                  
         SPACE                                                                  
VKEY     NTR1  BASE=*,LABEL=*                                                   
         GOTO1 FLDVAL,DMCB,(X'40',CYCAGYH),(X'80',CYCCATH)                      
         BE    VKX                                                              
         SPACE 1                                                                
         XC    CYCHD1,CYCHD1       IF ANY KEY FIELDS CHANGED                    
         XC    CYCHD2,CYCHD2       CLEAR OUT FIELDS                             
         XC    CYCHD3,CYCHD3                                                    
         GOTO1 FLDVAL,DMCB,(X'01',CYCFRSTH),CYCLSTH                             
         SPACE 1                                                                
         BAS   RE,VALAGY           VALIDATE AGENCY                              
         BAS   RE,VALCID           VALIDATE COMMERCIAL                          
         BAS   RE,VALUSE           VALIDATE USE                                 
         BAS   RE,VALCMC           VALIDATE CNET/MKT/CSYS                       
         BAS   RE,VALCYC           VALIDATE CYCLE                               
         BAS   RE,VALINV           VALIDATE INVOICE                             
         BAS   RE,VALVER           VALIDATE VERSION                             
         BAS   RE,VPIDCAT          VALIDATE PID AND CATEGORY                    
         SPACE 1                                                                
         MVI   SRCHSTAT,0                                                       
         SPACE 1                                                                
         USING TLUHD,R3                                                         
         LA    R3,KEY              WHEN FIELDS HAVE BEEN VALIDATED              
         XC    TLUHKEY,TLUHKEY     ENSURE AT LEAST ONE KEY EXISTS               
         MVI   TLUHCD,TLUHCDQ                                                   
         MVC   TLUHCOM,TGCOM       INTERNAL COMMERCIAL NUMBER                   
         MVC   TLUHCSEQ,CSEQ       CAST INPUT SEQ NUMBER                        
         MVC   TLUHUSE,TGUSCDE     USE CODE                                     
         MVC   TLUHINV,TGINV       INVOICE                                      
         GOTO1 HIGH                                                             
         B     VK20                                                             
VK10     GOTO1 SEQ                                                              
         SPACE 1                                                                
VK20     CLC   TLUHKEY(TLUHINV-TLUHD),KEYSAVE                                   
         BE    VK40                                                             
         SPACE 1                                                                
         LA    R2,CYCUSEH                       GIVE ERROR IF NOT               
         CLC   TLUHKEY(TLUHCSEQ-TLUHD),KEYSAVE  FOUND FOR USE                   
         BNE   VK30                                                             
         CLC   TLUHUSE,KEYSAVE+TLUHUSE-TLUHD                                    
         BNE   NOREC                                                            
         SPACE 1                                                                
VK30     LA    R2,CYCSSNH                       GIVE ERROR IF NOT               
         CLC   TLUHCSEQ,KEYSAVE+TLUHCSEQ-TLUHD  FOUND FOR CAST                  
         BNE   NOREC                            MEMBER                          
         SPACE 1                                                                
VK40     CLI   CYCINVH+5,0         IF FILTERING BY INVOICE                      
         BE    VK50                                                             
         LA    R2,CYCINVH                                                       
         CLC   TGINV,TLUHINV       GIVE ERROR IF NOT FOUND FOR INVOICE          
         BNE   NOREC                                                            
         DROP  R3                                                               
         SPACE 1                                                                
VK50     CLI   CYCCMCH+5,0         IF FILTERING BY CNET/MKT/CSYS                
         BNE   VK60                                                             
         CLI   CYCCYCH+5,0         OR CYCLE                                     
         BNE   VK60                                                             
         CLI   VERFILT,0           OR VERSION                                   
         BE    VK110                                                            
VK60     GOTO1 GETREC              GET THE RECORD                               
         SPACE 1                                                                
         USING TAMTD,R4                                                         
         L     R4,AIO              IF FILTERING BY CNET/MKT/CSYS                
         OC    MTINTC,MTINTC                                                    
         BZ    VK80                                                             
         MVI   SRCHSTAT,SRCHCMC                                                 
         MVI   ELCODE,TAMTELQ                                                   
         BRAS  RE,GETEL            GET NEXT RECORD IF CNET/MKT/CSYS             
         B     *+8                 NOT FOUND ON THIS INVOICE                    
VK70     BRAS  RE,NEXTEL                                                        
         BNE   VK10                                                             
         CLC   MTINTC,TAMTINUM                                                  
         BNE   VK70                                                             
         MVI   SRCHSTAT,0                                                       
         L     R4,AIO                                                           
         DROP  R4                                                               
         SPACE 1                                                                
         USING TAUHD,R4                                                         
VK80     OC    STRT,STRT           IF FILTERING BY CYCLE                        
         BZ    VK90                                                             
         MVI   SRCHSTAT,SRCHCYC                                                 
         MVI   ELCODE,TAUHELQ      GET NEXT RECORD IF CYCLE DOES                
         BRAS  RE,GETEL            NOT MATCH THIS INVOICE                       
         BNE   VK10                                                             
         CLC   TAUHSTRT,STRT                                                    
         BNE   VK10                                                             
         GOTO1 DATCON,DMCB,(X'11',TAUHSTRT),(8,CYCCYC)                          
         MVI   CYCCYCH+5,17                                                     
         MVC   STRT(6),TAUHSTRT                                                 
         MVI   SRCHSTAT,0                                                       
         L     R4,AIO                                                           
         DROP  R4                                                               
         SPACE 1                                                                
         USING TAVRD,R4                                                         
VK90     CLI   VERFILT,0           IF FILTERING BY VERSION                      
         BE    VK110                                                            
         MVI   SRCHSTAT,SRCHVER                                                 
         MVI   ELCODE,TAVRELQ      GET NEXT RECORD IF VERSION DOES              
         BRAS  RE,GETEL            NOT MATCH THIS INVOICE                       
         BNE   VK10                                                             
         CLC   TAVRVERS,VERFILT                                                 
         BNE   VK10                IF NOT, GET NEXT                             
         MVI   SRCHSTAT,0                                                       
         DROP  R4                                                               
         SPACE 1                                                                
VK110    GOTO1 FLDVAL,DMCB,(X'22',CYCAGYH),(X'80',CYCCATH)                      
         MVC   SAVEKEY,KEY                                                      
         XC    LASTCMC,LASTCMC                                                  
VKX      XIT1                                                                   
         SPACE 2                                                                
         EJECT                                                                  
*              ROUTINE TO VALIDATE AGENCY FIELD                                 
         SPACE 1                                                                
VALAGY   NTR1                                                                   
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'20',CYCAGYH)                              
         GOTO1 RAVPPLSA,DMCB,0     SEE IF REC / ACT INVALID FOR P+              
         JNE   ERPPLSI                                                          
         B     VKX                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE COMMERCIAL ID FIELD                          
*              AND SAVE OFF SOME VARIABLES                                      
         SPACE 1                                                                
VALCID   NTR1                                                                   
         GOTO1 RECVAL,DMCB,(X'20',TLCOICDQ),(X'20',CYCCIDH)                     
         SPACE 1                                                                
         USING TLCOPD,R3                                                        
         LA    R3,KEY                                                           
         MVC   TGCOM,TLCOICOM      SAVE INTERNAL COMMERCIAL NUMBER              
         SPACE 1                                                                
         MVC   TGVER,TLCOIVER                                                   
         CLI   TGVER,0                                                          
         BE    VCID20                                                           
         MVC   CYCVERS,TLCOIVER                                                 
         MVI   CYCVERSH+5,1                                                     
         TM    TGSYSTAT,TASYS3VR                                                
         BZ    VCID20                                                           
         EDIT  TLCOIVER,CYCVERS,ALIGN=LEFT                                      
         STC   R0,CYCVERSH+5                                                    
         DROP  R3                                                               
         SPACE 1                                                                
VCID10   CLI   TGVER,26            IF VERSION CODE IS GREATER THAN 26           
         BNH   VCID20              READ MAIN COMMERCIAL RECORD                  
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'A4',0)                                   
         BE    VCID20                                                           
         DC    H'00'                                                            
         SPACE 1                                                                
         USING TACOD,R4                                                         
VCID20   L     R4,AIO                                                           
         MVI   ELCODE,TACOELQ      GET COMMERCIAL DETAILS EL.                   
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 MEDVAL,DMCB,TACOMED SET MEDIA EQUATE                             
         SPACE 1                                                                
         CLI   TGVER,0                                                          
         BE    VCID30                                                           
         MVC   CYCCID,TACOCID                                                   
         B     VCID40                                                           
         DROP  R4                                                               
         SPACE 1                                                                
         USING TAVRD,R4                                                         
VCID30   L     R4,AIO                                                           
         MVI   ELCODE,TAVRELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   VCID40                                                           
         MVI   TGVER,C'A'                                                       
         DROP  R4                                                               
         SPACE 1                                                                
VCID40   XC    ELTAOC,ELTAOC                                                    
         SPACE 1                                                                
         L     R4,AIO                                                           
         MVI   ELCODE,TAOCELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   VKX                                                              
         MVC   ELTAOC,0(R4)                                                     
         B     VKX                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE USE                                          
         SPACE 1                                                                
VALUSE   NTR1                                                                   
         LA    R2,CYCUSEH          R2=A(USE CODE FIELD)                         
         CLI   5(R2),0             IF NO USE CODE INPUT                         
         BNE   VU10                                                             
         OC    TGUSCDE,TGUSCDE     USE GLOBAL IF AROUND                         
         BZ    FLDMISS             IF NOT, REQUIRE INPUT                        
         MVC   CYCUSE,TGUSCDE                                                   
         SPACE 1                                                                
VU10     GOTO1 USEVAL,DMCB,(X'40',CYCUSE)                                       
         BNE   FLDINV                                                           
         SPACE 1                                                                
         MVC   CYCHD3,CNETHEAD                                                  
         TM    TGUSSTA3,CBLUSE                                                  
         BNZ   VU20                                                             
         MVC   CYCHD3,CSYSHEAD                                                  
         CLI   TGUSEQU,ULCB                                                     
         BE    VU20                                                             
         MVC   CYCHD3,MKTHEAD                                                   
         CLI   TGUSEQU,UWSP                                                     
         BE    VU20                                                             
         CLI   TGUSEQU,USWS                                                     
         BE    VU20                                                             
         CLI   TGUSEQU,UWSC                                                     
         BE    VU20                                                             
         CLI   TGUSEQU,UADW                                                     
         BE    VU20                                                             
         B     FLDINV                                                           
         SPACE 1                                                                
VU20     MVC   CYCHD1,CYCHD3                                                    
         MVC   CYCHD2,CYCHD3                                                    
         B     VKX                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE CNET/MKT/CSYS                                
         SPACE 1                                                                
VALCMC   NTR1                                                                   
         XC    MTINTC,MTINTC                                                    
         SPACE 1                                                                
         LA    R2,CYCCMCH                                                       
         CLI   5(R2),0                                                          
         BE    VKX                                                              
         SPACE 1                                                                
         MVI   TGMTTYPE,TANPNET  SET TO READ CNET RECORDS                       
         TM    TGUSSTA3,CBLUSE   FOR CABLE AND SPANISH CABLE                    
         BNZ   VCMC10                                                           
         MVI   TGMTTYPE,C'S'     READ CSYS RECORDS                              
         CLI   TGUSEQU,ULCB      FOR LOCAL CABLE                                
         BE    VCMC10                                                           
         MVI   TGMTTYPE,C'T'     OTHERWISE READ TMKT FOR TELEVISION             
         CLI   TGMEEQU,RADIO                                                    
         BNE   VCMC10                                                           
         MVI   TGMTTYPE,C'R'     OR RMKT FOR RADIO                              
         SPACE 1                                                                
VCMC10   LHI   RF,TLMTCDQ                                                       
         TM    TGMEEQU,LIKETV    IF PAYING TELEVISION COMMERCIAL                
         BZ    VCMC20                                                           
         TM    TGUSSTA3,CBLUSE   AND USE TYPE IS NOT CABLE                      
         BNZ   VCMC20            OR SPANISH CABLE                               
         CLI   TGUSEQU,ULCB      OR LOCAL CABLE                                 
         BE    VCMC20                                                           
         LHI   RF,TLMTALDQ                                                      
VCMC20   GOTO1 RECVAL,DMCB,(RF),(X'04',(R2))                                    
         BE    VCMC30                                                           
         SPACE 1                                                                
         CLI   TGMTTYPE,TANPNET  IF PAYING CABLE OR SPANISH CABLE               
         BNE   FLDINV            AND CNET NOT FOUND, SEE IF CODE                
         MVI   TGMTTYPE,C'S'                    EXISTS AS A CSYS                
         GOTO1 RECVAL,DMCB,TLMTCDQ,(X'04',(R2))                                 
         MVI   TGMTTYPE,TANPNET                                                 
         BNE   FLDINV                                                           
         SPACE 1                                                                
VCMC30   MVC   MTINTC,TGMTINTC                                                  
         B     VKX                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE VERSION                                      
         SPACE 1                                                                
VALVER   NTR1                                                                   
         MVI   VERFILT,0         CLEAR VERSION FILTER                           
         SPACE 1                                                                
         CLI   CYCVERSH+5,0      IF NOT ENTERED, EXIT                           
         BE    VKX                                                              
         SPACE 1                                                                
         L     R4,AIO                                                           
         SPACE 1                                                                
         LA    R2,CYCVERSH                                                      
         SPACE 1                                                                
         TM    TGSYSTAT,TASYS3VR IF USING 1-LETTER VERSIONS                     
         BO    VV10                                                             
         CLI   5(R2),1           INPUT MUST BE 1 CHARACTER LONG                 
         BNE   FLDINV                                                           
         MVC   VERFILT,CYCVERS                                                  
         B     VV40                                                             
         SPACE 1                                                                
VV10     GOTO1 VALINUM           IF USING NUMERIC VERSIONS                      
         MVC   VERFILT,ACTUAL    INPUT MUST BE VALID NUMERIC                    
         SPACE 1                                                                
         CLI   VERFILT,26        IF VERSION CODE IS GREATER THAN                
         BNH   VV40              26                                             
         SPACE                                                                  
         USING VINDEXD,RE                                                       
         LA    RE,VERINDEX       FIND RECORD EQUATE FOR THIS                    
VV20     CLI   0(RE),X'FF'       VERSION NUMBER                                 
         BNE   *+6                                                              
         DC    H'00'                                                            
         CLC   VERFILT,VINDUPLM                                                 
         BNH   VV30                                                             
         LA    RE,VINDLNQ(RE)                                                   
         B     VV20                                                             
         SPACE 1                                                                
VV30     XC    KEY,KEY              GET COMMERCIAL RECORD FOR                   
         MVC   KEY(L'TLCOKEY),0(R4) THAT VERSION                                
         MVC   KEY+TLCOVER-TLCOD(L'TLCOVER),VINDEQUT                            
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLCOKEY),KEYSAVE                                           
         BNE   FLDINV                                                           
         GOTO1 GETREC                                                           
         DROP  RE                                                               
         SPACE                                                                  
         USING TAVRD,R4                                                         
VV40     MVI   ELCODE,TAVRELQ    MAKE SURE VERSION IS ON                        
         BRAS  RE,GETEL          THE COMMERCIAL                                 
         B     *+8                                                              
VV50     BRAS  RE,NEXTEL                                                        
         BNE   FLDINV                                                           
         CLC   TAVRVERS,VERFILT                                                 
         BNE   VV50                                                             
         DROP  R4                                                               
         SPACE 1                                                                
         B     VKX                                                              
         SPACE 2                                                                
*              TABLE TO DETERMINE WHICH COMMERCIAL RECORD THE                   
*              VERSION CODE IS ON                                               
         SPACE 1                                                                
VERINDEX DC    X'1A',AL1(TLCOV026)                                              
         DC    X'78',AL1(TLCOV120)                                              
         DC    X'D2',AL1(TLCOV210)                                              
         DC    X'FA',AL1(TLCOV250)                                              
         DC    X'FF'                                                            
         EJECT                                                                  
*              ROUTINE TO VALIDATE PID AND CATEGORY                             
         SPACE 1                                                                
VPIDCAT  NTR1                                                                   
         XC    CSEQ,CSEQ           CLEAR CAST SEQUENCE                          
         XC    CYCSSNN,CYCSSNN                                                  
         SPACE 1                                                                
         CLI   CYCSSNH+5,0         IF NO SSN INPUT                              
         BNE   VPC20                                                            
         XC    CYCCAT,CYCCAT       CLEAR CATEGORY INPUT                         
         MVI   CYCCATH+5,0                                                      
         B     VKX                                                              
         SPACE 1                                                                
VPC20    LA    R2,CYCSSNH          IF ACTION IS CHANGE                          
         CLI   ACTNUM,ACTCHA       INPUT IS NOT ALLOWED                         
         BE    FLDINV                                                           
         SPACE 1                                                                
         TM    TGSYSTAT,TASYSPID   IF USING PIDS                                
         BZ    VPC30                                                            
         CLI   CYCSSNH+5,9         AND INPUT IS NOT 9 CHARS LONG                
         BE    VPC30                                                            
         CLI   CYCSSNH+5,6         MUST BE 6 CHARACTERS LONG                    
         BNE   FLDINV                                                           
         MVC   TGPID,CYCSSN        TEMP REPLACE PID WITH SSN                    
         GOTO1 SSNUNPK,DMCB,TGPID,TGSSN                                         
         BNE   VPC30                                                            
         MVC   CYCSSN,TGSSN                                                     
         MVI   CYCSSNH+5,9                                                      
         SPACE 1                                                                
VPC30    GOTO1 RECVAL,DMCB,TLW4CDQ,(X'08',CYCSSNH),CYCSSNNH                     
         SPACE 1                                                                
         BAS   RE,VALCAT           VALIDATE CATEGORY                            
         SPACE 1                                                                
         TM    TGSYSTAT,TASYSPID   IF USING PIDS                                
         BZ    VKX                 REPLACE SSN WITH PID AGAIN                   
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   CYCSSN,SPACES                                                    
         MVC   CYCSSN(L'TGPID),TGPID                                            
         MVI   CYCSSNH+5,6                                                      
         B     VKX                                                              
         SPACE 1                                                                
*              ROUTINE TO VALIDATE CATEGORY FIELD                               
         SPACE 1                                                                
VALCAT   NTR1                                                                   
         LA    R2,CYCCATH                                                       
         XC    TGCAT,TGCAT                                                      
         CLI   5(R2),0             IF NOT BLANK                                 
         BE    VC10                                                             
         OC    8(3,R2),SPACES                                                   
         GOTO1 CATVAL,DMCB,8(R2)   VALIDATE CATEGORY CODE                       
         BE    VC10                                                             
         SPACE 1                                                                
         MVC   FULL(3),=3C'0'      IF NOT VALID TEST IF VALID HEX               
         ZIC   R1,5(R2)                                                         
         LA    RF,L'FULL                                                        
         SR    RF,R1                                                            
         LA    RF,FULL(RF)         SET TO RIGHT-ALIGN IN FULL                   
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),8(R2)                                                    
         SPACE 1                                                                
         GOTO1 HEXIN,DMCB,FULL,CSEQ,4                                           
         OC    12(4,R1),12(R1)                                                  
         BZ    FLDINV                                                           
         SPACE 1                                                                
VC10     XC    KEY,KEY             BUILD CAST KEY                               
         LA    R3,KEY                                                           
         USING TLCAPD,R3                                                        
         MVI   TLCAPCD,TLCACCDQ                                                 
         MVC   TLCACSSN,TGSSN      SOCIAL SECURITY NUMBER                       
         MVC   TLCACCOM,TGCOM      INTERNAL COMMERCIAL NUMBER                   
         MVC   TLCACCAT,TGCAT      CATEGORY                                     
         GOTO1 HIGH                                                             
         SPACE 1                                                                
VC20     CLC   TLCAPKEY(TLCACCAT-TLCAPD),KEYSAVE  STILL SAME COMML              
         BNE   FLDINV                                                           
         OC    TGCAT,TGCAT         IF NO CATEGORY INPUT, TAKE FIRST ONE         
         BZ    VC30                                                             
         CLC   TLCACCAT,TGCAT      ELSE TAKE FIRST MATCH                        
         BE    VC50                                                             
         B     VC40                                                             
         SPACE 1                                                                
VC30     CLI   5(R2),0             IF NO INPUT TAKE FIRST ONE FOUND             
         BE    VC50                                                             
         CLC   TLCACSEQ,CSEQ       ELSE SCAN FOR MATCHING SEQUENCE NO.          
         BE    VC60                                                             
         SPACE 1                                                                
VC40     GOTO1 SEQ                 GET NEXT CAST RECORD                         
         B     VC20                AND KEEP ON TRYING                           
         SPACE 1                                                                
VC50     MVC   CSEQ,TLCACSEQ       SAVE CAST SEQ                                
         SPACE 1                                                                
VC60     MVC   8(3,R2),TLCACCAT    DISPLAY ACTUAL CATEGORY                      
         B     VKX                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE CYCLE                                        
         SPACE 1                                                                
VALCYC   NTR1                                                                   
         XC    STRT(6),STRT        IF NOT ENTERED, EXIT                         
         CLI   CYCCYCH+5,0                                                      
         BE    VKX                                                              
         SPACE 1                                                                
         LA    R2,CYCCYCH                                                       
         GOTO1 DATVAL,DMCB,CYCCYC,DUB  VALIDATE START DATE ONLY                 
         OC    0(4,R1),0(R1)                                                    
         BZ    DATEINV                                                          
         XC    CYCCYC,CYCCYC                                                    
         GOTO1 DATCON,DMCB,DUB,(1,STRT)  SAVE PACKED START                      
         GOTO1 (RF),(R1),,(8,CYCCYC)     REDISPLAY START DATE                   
         B     VKX                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE INVOICE                                      
         SPACE 1                                                                
VALINV   NTR1                                                                   
         XC    TGINV,TGINV         IF NOT ENTERED, EXIT                         
         CLI   CYCINVH+5,0                                                      
         BE    VKX                                                              
         SPACE 1                                                                
         LA    R2,CYCINVH                                                       
         GOTO1 TINVCON,DMCB,CYCINV,INV,DATCON                                   
         CLI   0(R1),X'FF'                                                      
         BE    FLDINV                                                           
         MVC   TGINV,INV                                                        
         XC    TGINV,HEXFFS        COMPLEMENT GLOBAL INVOICE NUMBER             
         B     VKX                                                              
         EJECT                                                                  
FLDINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     THEEND                                                           
NOINPUT  MVI   ERROR,ERNOINP       INPUT NOT ALLOWED                            
         B     THEEND                                                           
FLDMISS  MVI   ERROR,MISSING       MISSING INPUT FIELD                          
         B     THEEND                                                           
DATEINV  MVI   ERROR,INVDATE       INVALID DATE                                 
         B     THEEND                                                           
NOREC    MVI   ERROR,NOTFOUND      RECORD NOT FOUND                             
         CLI   SRCHSTAT,0                                                       
         BE    THEEND                                                           
         LA    R2,CYCCMCH                                                       
         CLI   SRCHSTAT,SRCHCMC                                                 
         BE    THEEND                                                           
         LA    R2,CYCCYCH                                                       
         CLI   SRCHSTAT,SRCHCYC                                                 
         BE    THEEND                                                           
         LA    R2,CYCVERSH                                                      
         CLI   SRCHSTAT,SRCHVER                                                 
         BE    THEEND                                                           
         DC    H'00'                                                            
THEEND   GOTO1 EXIT,DMCB,0                                                      
         SPACE 2                                                                
HEXFFS   DC    10X'FF'                                                          
         LTORG                                                                  
         EJECT                                                                  
*              DISPLAY THE RECORD                                               
         SPACE 1                                                                
DREC     NTR1  BASE=*,LABEL=*                                                   
         L     R2,ACMCTB           CLEAR CNET/MKT/CSYS TABLE                    
         L     R3,ACMCTBX                                                       
DR01     XC    0(250,R2),0(R2)                                                  
         LA    R2,250(R2)                                                       
         CR    R2,R3                                                            
         BL    DR01                                                             
         L     R2,ACMCTB                                                        
         MVI   0(R2),X'FF'                                                      
         SPACE 1                                                                
         XC    DISPCMC(7*CMCLNQ),DISPCMC                                        
         XC    DISPCMC+(7*CMCLNQ)((7*CMCLNQ)+1),DISPCMC+(7*CMCLNQ)              
         MVI   DISPCMC,X'FF'                                                    
         SPACE 1                                                                
         USING TLUHD,R3                                                         
         LA    R3,KEY              GET ALL USAGE HISTORY KEYS                   
         MVC   KEY,SAVEKEY         FOR THIS COMMERCIAL/CAST/USE                 
DR10     GOTO1 HIGH                                                             
         B     DR30                                                             
DR20     GOTO1 SEQ                                                              
DR30     CLC   TLUHKEY(TLUHUSE+L'TLUHUSE-TLUHD),KEYSAVE                         
         BE    DR40                                                             
         SPACE 1                                                                
         TM    TGUSSTA3,CBLUSE     IF ALL USAGE HISTORY KEYS READ               
         BZ    DR180               FOR CABLE OR SPANISH CABLE                   
         CLC   KEYSAVE+TLUHUSE-TLUHD(L'TLUHUSE),=C'LCB'                         
         BE    DR180                                                            
         MVC   KEY,SAVEKEY         GO READ LCB TOO                              
         MVC   TLUHUSE,=C'LCB'                                                  
         B     DR10                                                             
         SPACE 1                                                                
DR40     CLI   CYCINVH+5,0         IF FILTERING BY INVOICE                      
         BE    DR50                                                             
         CLC   TGINV,TLUHINV       ONLY GET RECORD FOR THAT INVOICE             
         BNE   DR180                                                            
         SPACE 1                                                                
DR50     MVC   SVINV,TLUHINV       SAVE INVOICE NUMBER                          
         DROP  R3                                                               
         SPACE 1                                                                
         USING TLDRD,R3                                                         
         MVC   SVDA,TLDRDA         SAVE DISK ADDRESS                            
         DROP  R3                                                               
         SPACE 1                                                                
         GOTO1 GETREC              GET THE RECORD                               
         SPACE 1                                                                
         USING TAUHD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAUHELQ      GET USAGE HISTORY ELEMENT                    
         BRAS  RE,GETEL                                                         
         BNE   DR20                                                             
         SPACE 1                                                                
         OC    STRT,STRT           IF FILTERING BY CYCLE                        
         BZ    DR60                                                             
         CLC   TAUHSTRT,STRT       CYCLE OF RECORD MUST MATCH                   
         BNE   DR20                                                             
         SPACE 1                                                                
DR60     MVC   SVCMCYC,TAUHSTRT    SAVE COMMERCIAL START AND END                
         SPACE 1                                                                
         MVC   SVCRED,TAUHLCST     SAVE CREDIT STATUS                           
         CLI   TGUSEQU,ULCB                                                     
         BE    DR70                                                             
         MVC   SVCRED,TAUHCSTA                                                  
         CLI   TGUSEQU,UCBL                                                     
         BE    DR70                                                             
         CLI   TGUSEQU,USCB                                                     
         BE    DR70                                                             
         MVC   SVCRED,TAUHSTAT                                                  
         DROP  R4                                                               
         SPACE 1                                                                
         USING TAVRD,R4                                                         
DR70     MVI   SVVERS,0                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAVRELQ      GET VERSION ELEMENT                          
         BRAS  RE,GETEL                                                         
         BNE   DR80                                                             
         MVC   SVVERS,TAVRVERS     SAVE VERSION                                 
         SPACE 1                                                                
DR80     CLI   VERFILT,0           IF FILTERING BY VERSION                      
         BE    DR90                                                             
         CLC   TAVRVERS,VERFILT    VERSION OF RECORD MUST MATCH                 
         BNE   DR20                                                             
         DROP  R4                                                               
         SPACE 1                                                                
         USING TAMTD,R4                                                         
DR90     L     R4,AIO                                                           
         MVI   ELCODE,TAMTELQ      GET ALL CNET/MKT/CSYS ELEMENTS               
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
DR100    BRAS  RE,NEXTEL                                                        
         BNE   DR20                                                             
         SPACE 1                                                                
         L     R1,ACMCTBX          MAKE SURE THERE IS STILL ROOM                
         SHI   R1,CMCLNQ           IN TABLE                                     
         OC    0(CMCLNQ,R1),0(R1)                                               
         BNZ   DR180                                                            
         SPACE 1                                                                
         OC    MTINTC,MTINTC       IF FILTERING BY CNET/MKT/CSYS                
         BZ    DR110               ONLY GET THE MATCHES                         
         CLC   MTINTC,TAMTINUM                                                  
         BNE   DR100                                                            
         SPACE 1                                                                
         USING CMCD,R1                                                          
DR110    LA    R1,ELEMENT         PUT ALL SAVED VARIABLES INTO ELEMENT          
         XC    ELEMENT,ELEMENT                                                  
         MVC   CMCCODE,TAMTCODE   SAVE CODE                                     
         MVC   CMCINUM,TAMTINUM   SAVE INTERNAL CODE                            
         MVC   CMCCYC,TAMTCYCS    CODE'S CYCLE                                  
         MVC   CMCCMCYC,SVCMCYC   COMMERCIAL'S CYCLE                            
         MVC   CMCINV,SVINV       INVOICE                                       
         MVC   CMCVER,SVVERS      VERSION                                       
         MVC   CMCCRED,SVCRED     AND CREDIT STATUS                             
         DROP  R4                                                               
         SPACE 1                                                                
*                                 NOW SORT THIS INFO INTO LIST                  
         L     R2,ACMCTB          OF CNET/MKT/CSYS'S                            
         SPACE 1                                                                
DR120    CLI   0(R2),X'FF'        X'FF' MARKS WHERE TO PUT INTO                 
         BNE   DR130              TABLE, IF FOUND IT, PUT IT                    
         MVC   0(CMCLNQ,R2),ELEMENT                                             
         SPACE 1                                                                
         CLI   CMCLNQ(R2),0       IF THIS IS AT THE END OF THE TABLE            
         BNE   DR100              MARK END OF TABLE                             
         MVI   CMCLNQ(R2),X'FF'                                                 
         B     DR100                                                            
         SPACE 1                                                                
DR130    CLC   CMCCODE,0(R2)      IF CODE/CYCLE ALREADY ENCOUNTERED             
         BL    DR150              DON'T ADD IT TO TABLE AGAIN                   
         BNE   DR140                                                            
         CLC   CMCCYC,CMCCYC-CMCD(R2)                                           
         BE    DR100                                                            
         SPACE 1                                                                
         CLI   TGVER,0                                                          
         BE    DR140                                                            
         CLC   CMCCMCYC,CMCCMCYC-CMCD(R2)                                       
         BNE   DR140                                                            
         CLC   CMCCYC,CMCCYC-CMCD(R2)                                           
         BH    DR140                                                            
*        MVC   CMCCYC,CMCCYC-CMCD(R2)                                           
         MVC   0(CMCLNQ,R2),CMCD                                                
         B     DR100                                                            
         SPACE 1                                                                
DR140    LA    R2,CMCLNQ(R2)      IF CODE ALPHABETICALLY HIGHER THAN            
         B     DR120              CURRENT ENTRY, BUMP TO NEXT ENTRY             
         SPACE 1                                                                
DR150    LR    RE,R2              IF CODE LESS THAN CURRENT ENTRY,              
DR160    CLI   0(RE),X'FF'        SHIFT ALL ENTRIES IN TABLE DOWN               
         BE    DR170              AND GO TRY TO ADD IT AGAIN                    
         LR    RF,RE                                                            
         LA    RE,CMCLNQ(RE)                                                    
         B     DR160                                                            
DR170    MVC   0(CMCLNQ,RE),0(RF)                                               
         MVI   0(RF),X'FF'                                                      
         CLI   CMCLNQ(RE),0                                                     
         BNE   DR120                                                            
         MVI   CMCLNQ(RE),X'FF'                                                 
         B     DR120                                                            
         DROP  R1                                                               
         SPACE 1                                                                
*                                  ALL ENTRIES HAVE BEEN ADDED TO               
*                                  TABLE, SO NOW DISPLAY THEM                   
         SPACE 1                                                                
DR180    GOTO1 FLDVAL,DMCB,(X'21',CYCFRSTH),CYCLSTH                             
         SPACE 1                                                                
         USING CMCD,R3                                                          
         L     R3,ACMCTB                                                        
DR184    CLI   0(R3),X'FF'                                                      
         BE    NOMORE                                                           
         SPACE 1                                                                
         CLI   CYCINVH+5,0         IF NOT DISPLAYING JUST ONE INVOICE           
         BNE   DR186                                                            
         OC    CSEQ,CSEQ           AND DISPLAYING ONE CAST MEMBER               
         BNZ   DR185                                                            
         CLI   TGVER,0             OR COMMERCIAL HAS VERSIONS                   
         BNE   DR186                                                            
DR185    TM    CMCCRED,TAUHWCRD    SKIP CREDITED CNET/MKT/CSYS                  
         BZ    DR186                                                            
         LA    R3,CMCLNQ(R3)                                                    
         B     DR184                                                            
         SPACE 1                                                                
         USING SCRDETD,R2                                                       
DR186    LA    R2,CYCFRSTH                                                      
         SPACE 1                                                                
         MVC   LLASTCMC,LASTCMC                                                 
         SPACE 1                                                                
DR190    CLI   0(R3),X'FF'         IF AT END OF TABLE, GIVE NO MORE             
         BNE   DR200               TO DISPLAY MESSAGE                           
         OC    CYCFRST,CYCFRST                                                  
         BNZ   NOMORE                                                           
         XC    LASTCMC,LASTCMC                                                  
         B     DR180                                                            
         SPACE 1                                                                
DR200    LA    RF,CYCLSTH          IF AT END OF SCREEN, GIVE MORE TO            
         CR    R2,RF               DISPLAY MESSAGE                              
         BH    MORE                                                             
         SPACE 1                                                                
         OC    CSEQ,CSEQ                                                        
         BNZ   DR210                                                            
         CLI   TGVER,0             IF COMMERCIAL DOES NOT HAVE VERSIONS         
         BNE   DR220                                                            
DR210    TM    CMCCRED,TAUHWCRD    AND CNET/MKT/CSYS WAS LAST CREDITED          
         BZ    DR220                                                            
         CLI   CYCINVH+5,0         AND NOT DISPLAYING JUST THIS INVOICE         
         BE    DR260               DON'T DISPLAY CNET/MKT/CSYS                  
         SPACE 1                                                                
DR220    CLC   CMCCODE,LASTCMC     IF CURRENT CODE/CYCLE IS NOT GREATER         
         BL    DR260               THAN LAST ONE DISPLAYED, SKIP TO             
         BNE   DR230               NEXT TABLE ENTRY                             
         CLC   CMCCYC,LASTCMC+6                                                 
         BNH   DR260                                                            
         SPACE 1                                                                
DR230    MVC   SDCODE,CMCCODE      DISPLAY INFORMATION                          
         GOTO1 DATCON,DMCB,(1,CMCCYC),(8,SDCYCS)                                
         MVI   SDDASH1,C'-'                                                     
         GOTO1 DATCON,DMCB,(1,CMCCYC+3),(8,SDCYCE)                              
         GOTO1 DATCON,DMCB,(1,CMCCMCYC),(8,SDCCYCS)                             
         MVI   SDDASH2,C'-'                                                     
         GOTO1 DATCON,DMCB,(1,CMCCMCYC+3),(8,SDCCYCE)                           
         MVC   DUB,CMCINV                                                       
         XC    DUB(6),DRHEXFFS                                                  
         GOTO1 TINVCON,DMCB,DUB,SDINV,DATCON                                    
         MVC   SDVER(1),CMCVER                                                  
         TM    TGSYSTAT,TASYS3VR                                                
         BZ    DR240                                                            
         EDIT  CMCVER,SDVER,ALIGN=LEFT                                          
         SPACE 1                                                                
         LA    RE,DISPCMC                                                       
DR240    CLI   0(RE),X'FF'                                                      
         BE    DR250                                                            
         LA    RE,CMCLNQ(RE)                                                    
         B     DR240                                                            
DR250    MVC   0(CMCLNQ,RE),CMCD                                                
         MVI   CMCLNQ(RE),X'FF'                                                 
         SPACE 1                                                                
         MVC   LASTCMC,CMCD        SAVE LAST DISPLAYED INFORMATION              
         SPACE 1                                                                
         LA    R2,SDLNQ(R2)        BUMP TO NEXT SCREEN FIELD                    
DR260    LA    R3,CMCLNQ(R3)       BUMP TO NEXT TABLE ENTRY                     
         B     DR190                                                            
         DROP  R2,R3                                                            
         SPACE 1                                                                
MORE     MVC   MYMSGNO,=H'261'                                                  
         B     INFEND                                                           
NOMORE   MVC   MYMSGNO,=H'262'                                                  
*        NI    CYCAGYH+4,X'FF'-X'20'                                            
INFEND   LA    R2,CYCFRSTH                                                      
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         MVI   BLOCK,0                                                          
         MVI   MYMTYP,GTMINF                                                    
         OI    GENSTAT2,USGETTXT                                                
         GOTO1 EXIT,DMCB,0                                                      
         SPACE 2                                                                
CNETHEAD DC    CL10'CNet Cycle'                                                 
MKTHEAD  DC    CL10'Mkt Cycle'                                                  
CSYSHEAD DC    CL10'CSys Cycle'                                                 
         SPACE 2                                                                
DRHEXFFS DC    10X'FF'                                                          
         LTORG                                                                  
         EJECT                                                                  
*              VALIDATE THE RECORD                                              
         SPACE 1                                                                
VREC     NTR1  BASE=*,LABEL=*                                                   
         XR    RF,RF             COUNT NUMBER OF CHANGED FIELDS WITH RF         
         SPACE 1                                                                
         LA    R2,CYCFRSTH                                                      
         LA    R3,CYCLSTH        ONLY ONE CYCLE CAN BE CHANGED ON               
VR10     CR    R2,R3             THE SCREEN                                     
         BH    VR30                                                             
         TM    4(R2),X'20'                                                      
         BO    VR20                                                             
         AHI   RF,1                                                             
         CHI   RF,1                                                             
         BH    CYCMTO                                                           
VR20     ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         B     VR10                                                             
         SPACE 1                                                                
VR30     CHI   RF,1                                                             
         BNE   VR320                                                            
         SPACE 1                                                                
         LA    R2,CYCVERSH         IF ANY FIELDS ARE INVALIDATED                
         CLI   5(R2),0             VERSION INPUT CANNOT BE PRESENT              
         BNE   CYNFCC                                                           
         LA    R2,CYCSSNH          IF ANY FIELDS ARE INVALIDATED                
         CLI   5(R2),0             SS#/PID# INPUT CANNOT BE PRESENT             
         BNE   CYNFCC                                                           
         SPACE 1                                                                
         USING CMCD,R3                                                          
         LA    R3,DISPCMC                                                       
         SPACE 1                                                                
         USING SCRDETD,R2                                                       
         LA    R2,CYCFRSTH                                                      
         SPACE 1                                                                
VR40     CLI   0(R3),X'FF'         IF ALL FIELDS VALIDATED, REDISPLAY           
         BE    VR220                                                            
         SPACE 1                                                                
         TM    SDCYCSH+4,X'20'     IF FIELD IS VALIDATED, BUMP TO               
         BNZ   VR200               NEXT FIELD AND NEXT TABLE ENTRY              
         SPACE 1                                                                
         TM    CMCCRED,TAUHWCRD    CANNOT CHANGE IF CREDIT INVOICE              
         BNZ   CYNFCC                                                           
         SPACE 1                                                                
         LR    R4,R2                                                            
         LA    R2,SDCYCSH          INPUT MUST BE A VALID DATE                   
         GOTO1 DTVAL,DMCB,CMCCYC                                                
         LR    R2,R4                                                            
         SPACE 1                                                                
         CLC   CMCCYC(3),CMCCMCYC  DATE MUST FIT WITHIN COMMERCIAL              
         BL    CYCINC              CYCLE                                        
         CLC   CMCCYC(3),CMCCMCYC+3                                             
         BH    CYCINC                                                           
         SPACE 1                                                                
         XC    WORK,WORK                                                        
         GOTO1 DATCON,DMCB,(1,CMCCMCYC),(0,TGDUB)                               
         GOTO1 DATCON,DMCB,(1,CMCCMCYC+3),(0,TGDUB+6)                           
         SPACE 1                                                                
         LHI   R4,1                SAVE LENGTH OF CYCLE (IN DAYS)               
VR50     STC   R4,CYCLEN           IN CYCLEN                                    
         GOTO1 ADDAY,DMCB,TGDUB,WORK,(R4)                                       
         CLC   WORK(6),TGDUB+6                                                  
         BE    VR60                                                             
         AHI   R4,1                                                             
         B     VR50                                                             
         SPACE 1                                                                
VR60     MVC   LAGEND,CMCCMCYC+3   IF CYCLE IS 8 WEEKS                          
         LHI   RF,14               LAG END IS 2 WEEKS LATER                     
         CLI   CYCLEN,56           IF CYCLE IS 13 WEEKS                         
         BL    VR80                LAG END IS 4 WEEKS LATER                     
         BE    VR70                ELSE, LAG END IS SAME AS                     
         LHI   RF,28               CYCLE END                                    
         SPACE 1                                                                
VR70     GOTO1 ADDAY,DMCB,TGDUB+6,WORK,(RF)       SAVE LAG END DATE             
         GOTO1 DATCON,DMCB,(0,WORK),(1,LAGEND)    IN LAGEND                     
         SPACE 1                                                                
VR80     GOTO1 DATCON,DMCB,(1,CMCCYC),(0,TGDUB)                                 
         SPACE 1                                                                
         ZIC   RF,CYCLEN                          CALCULATE NEW CYCLE           
         GOTO1 ADDAY,DMCB,TGDUB,WORK,(RF)         END                           
         GOTO1 DATCON,DMCB,(0,WORK),(1,CMCCYC+3)                                
         SPACE 1                                                                
         CLC   CMCCYC+3(3),LAGEND                 IF LATER THAN LAG             
         BNH   *+10                               END DATE                      
         MVC   CMCCYC+3(3),LAGEND                 USE LAG END DATE              
         SPACE 1                                                                
*                                CHECK NEW START DATE AGAINST HISTORY           
         SPACE 1                                                                
         MVI   TGBYTE,UHCUR      SET TRIED CURRENT AGENCY/COMM'L                
         SPACE 1                                                                
         USING TLUHPD,R4                                                        
         LA    R4,KEY            READ ALL USAGE HISTORY KEYS                    
         XC    TLUHPKEY,TLUHPKEY FOR THIS CNET/MKT/CSYS ON THIS                 
         MVI   TLUHPCD,TLUHMTDQ  COMMERCIAL                                     
         MVC   TLUHMINM,CMCINUM                                                 
         MVC   TLUHMCOM,TGCOM                                                   
VR90     GOTO1 HIGH                                                             
         B     VR110                                                            
VR100    GOTO1 SEQ                                                              
VR110    CLC   KEY(TLUHMUSE-TLUHPCD),KEYSAVE                                    
         BE    VR120                                                            
         SPACE 1                                                                
         BAS   RE,RDPREVUH       IF NONE FOUND, SEARCH PREV AGY/COM             
         BNE   VR140                                                            
         B     VR90                                                             
         SPACE 1                                                                
VR120    CLC   TLUHMCCS,CMCCYC   IF NEW START DATE FITS WITHIN A                
         BH    VR100             EXISTING CYCLE FOR THIS CNET/MKT/              
         CLC   TLUHMCYE,CMCCYC   CSYS ...                                       
         BL    VR100                                                            
         SPACE 1                                                                
         CLC   TLUHMINV,CMCINV   ... OK IF SAME INVOICE                         
         BE    VR100                                                            
         SPACE 1                                                                
         CLC   TLUHMCCS,CMCCMCYC ... IF COMM'L CYCLE IS THE SAME                
         BNE   VR130                                                            
         CLC   TLUHMC1D,CMCCYC   ALL EXISTING DATE MUST FIT WITHIN              
         BL    CYCNFT            THIS NEW CNET/MKT/CSYS CYCLE                   
         CLC   TLUHMC1D,CMCCYC+3                                                
         BH    CYCNFT                                                           
         B     VR100                                                            
         SPACE 1                                                                
VR130    CLI   TGVER,0           ... IF COMM'L CYCLE NOT THE SAME               
         BNE   CYCOVP            NEVER OK IF COMMERCIAL HAS VERSIONS            
         TM    TLUHMSTA,TLUHMCRD OK IF FOUND ON A PREV CREDIT PAYMENT           
         BZ    CYCOVP            ELSE, ERROR                                    
         SPACE 1                                                                
*                                CHECK NEW END DATE AGAINST HISTORY             
         SPACE 1                                                                
VR140    MVI   TGBYTE,UHCUR      SET TRIED CURRENT AGENCY/COMM'L                
         SPACE 1                                                                
         LA    R4,KEY            READ ALL USAGE HISTORY KEYS                    
         XC    TLUHPKEY,TLUHPKEY FOR THIS CNET/MKT/CSYS ON THIS                 
         MVI   TLUHPCD,TLUHMTDQ  COMMERCIAL                                     
         MVC   TLUHMINM,CMCINUM                                                 
         MVC   TLUHMCOM,TGCOM                                                   
VR150    GOTO1 HIGH                                                             
         B     VR170                                                            
VR160    GOTO1 SEQ                                                              
VR170    CLC   KEY(TLUHMUSE-TLUHPCD),KEYSAVE                                    
         BE    VR180                                                            
         SPACE 1                                                                
         BAS   RE,RDPREVUH       IF NONE FOUND, SEARCH PREV AGY/COM             
         BNE   VR190                                                            
         B     VR150                                                            
         SPACE 1                                                                
VR180    CLC   TLUHMCCS,CMCCYC+3 IF NEW END DATE FITS WITHIN A                  
         BH    VR160             EXISTING CYCLE FOR THIS CNET/MKT/              
         CLC   TLUHMCYE,CMCCYC+3 CSYS ...                                       
         BL    VR160                                                            
         SPACE 1                                                                
         CLC   TLUHMINV,CMCINV   ... OK IF SAME INVOICE                         
         BE    VR160                                                            
         SPACE 1                                                                
         CLC   TLUHMCCS,CMCCMCYC ... IF COMM'L CYCLE IS THE SAME                
         BNE   VR185                                                            
         CLC   TLUHMC1D,CMCCYC   ALL EXISTING DATE MUST FIT WITHIN              
         BL    CYCNFT            THIS NEW CNET/MKT/CSYS CYCLE                   
         CLC   TLUHMC1D,CMCCYC+3                                                
         BH    CYCNFT                                                           
         B     VR160                                                            
         SPACE 1                                                                
VR185    CLI   TGVER,0           IF COMMERCIAL CYCLE IS NOT THE SAME            
         BNE   CYCOVP            NEVER OK IF COMMERCIAL HAS VERSIONS            
         TM    TLUHMSTA,TLUHMCRD OK IF FOUND ON A PREV CREDIT PAYMENT           
         BNZ   VR190                                                            
         CLC   TLUHMCCS,CMCCMCYC OR IF CYCLE START IS THE SAME                  
         BE    VR160             ELSE, ERROR                                    
         B     CYCOVP                                                           
         DROP  R4                                                               
         SPACE 1                                                                
VR190    OI    CMCSTAT,CMCCHGD   CYCLE HAS PASSED TEST, WE WILL CHG IT          
         SPACE 1                                                                
VR200    LA    R2,SDLNQ(R2)      BUMP TO NEXT SCREEN FIELD                      
VR210    LA    R3,CMCLNQ(R3)     BUMP TO NEXT TABLE ENTRY                       
         B     VR40                                                             
         DROP  R2                                                               
         SPACE 1                                                                
VR220    LA    R3,DISPCMC        NOW ACTUALLY GO UPDATE UH RECORDS              
         SPACE 1                                                                
VR230    CLI   0(R3),X'FF'       IF ALL RECORDS HAVE BEEN UPDATED               
         BE    VR310             GO RE-DISPLAY                                  
         TM    CMCSTAT,CMCCHGD                                                  
         BZ    VR300                                                            
         SPACE 1                                                                
         USING TLUHPD,R4                                                        
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   TLUHPCD,TLUHMTDQ  READ ALL USAGE HISTORY KEYS                    
         MVC   TLUHMINM,CMCINUM  FOR THIS CNET/MKT/CSYS                         
         MVC   TLUHMCOM,TGCOM    AND INTERNAL COMMERCIAL NUMBER                 
         GOTO1 HIGH                                                             
         B     VR250                                                            
VR240    GOTO1 SEQ                                                              
VR250    CLC   KEY(TLUHMSEQ-TLUHPD),KEYSAVE                                     
         BNE   VR300                                                            
         SPACE 1                                                                
         LA    R4,KEY            ONLY PROCESS RECORDS WITH                      
         CLC   TLUHMCCS,CMCCMCYC SAME COMMERCIAL CYCLE START                    
         BNE   VR240                                                            
         SPACE 1                                                                
         MVI   CHG1DATE,C'Y'     IF THIS IS THE INVOICE RECORD                  
         CLC   TLUHMINV,CMCINV   WE'RE CHANGING                                 
         BE    VR260             CHANGE THE 1ST AIR DATE                        
         CLI   TGVER,0                                                          
         BE    VR240                                                            
         MVI   CHG1DATE,C'N'                                                    
         DROP  R4                                                               
         SPACE 1                                                                
VR260    MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         SPACE 1                                                                
         XC    SVPTRS(255),SVPTRS                                               
         XC    NEWPTRS(255),NEWPTRS                                             
         SPACE 1                                                                
         GOTO1 SAVPTRS,DMCB,SVPTRS                                              
         SPACE 1                                                                
         USING TAMTD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAMTELQ                                                   
         BRAS  RE,GETEL                                                         
         B     VR280                                                            
VR270    BRAS  RE,NEXTEL                                                        
VR280    BNE   VR290                                                            
         CLC   TAMTINUM,CMCINUM                                                 
         BNE   VR270                                                            
         MVC   TAMTCYCE,CMCCYC+3                                                
         CLI   CHG1DATE,C'N'                                                    
         BE    VR290                                                            
         MVC   TAMTCYCS,CMCCYC                                                  
         DROP  R4                                                               
         SPACE 1                                                                
VR290    GOTO1 ACTVIN,DMCB,(X'80',0)                                            
         SPACE 1                                                                
         GOTO1 PUTREC                                                           
         GOTO1 ADDPTRS,DMCB,(X'08',SVPTRS),NEWPTRS                              
         B     VR240                                                            
VR300    LA    R3,CMCLNQ(R3)                                                    
         B     VR230                                                            
         SPACE 1                                                                
VR310    MVC   LASTCMC,LLASTCMC  SET TO RE-DISPLAY SAME SCREEN                  
VR320    BRAS  RE,DREC                                                          
         B     VRX                                                              
         SPACE 1                                                                
VRYES    XR    RC,RC                                                            
VRNO     LTR   RC,RC                                                            
VRX      XIT1                                                                   
         EJECT                                                                  
*              ROUTINE TO SET UP USAGE HISTORY READ IF A PREVIOUS               
*              AGENCY/COMMERCIAL EXISTS                                         
         SPACE 1                                                                
         USING CMCD,R3                                                          
         USING TLUHPD,R4                                                        
RDPREVUH NTR1                                                                   
         CLI   TGBYTE,UHPRE      EXIT IF PREVIOUS AGENCY/COMMERCIAL             
         BE    VRNO              ALREADY SEARCHED                               
         SPACE 1                                                                
         MVI   TGBYTE,UHPRE      SET PREVIOUS AGY/COM READ                      
         SPACE 1                                                                
         OC    ELTAOC,ELTAOC     EXIT IF PREVIOUS AGENCY/COMMERCIAL             
         BZ    VRNO              DOES NOT EXIST                                 
         SPACE 1                                                                
         USING TAOCD,RE                                                         
         LA    RE,ELTAOC         SET UP USAGE HISTORY KEY                       
         TM    TAOCSTAT,TAOCSFRO                                                
         BZ    VRNO                                                             
         XC    KEY,KEY                                                          
         MVI   TLUHPCD,TLUHMTDQ                                                 
         MVC   TLUHMINM,CMCINUM                                                 
         MVC   TLUHMCOM,TAOCCOM                                                 
         B     VRYES                                                            
         DROP  RE,R3,R4                                                         
         EJECT                                                                  
*              ERROR MESSAGES AND EQUATES                                       
         SPACE 2                                                                
UHCUR    EQU   X'80'               TRIED THIS USE FOR CURR AGY/COMM'L           
UHPRE    EQU   X'40'               TRIED THIS USE FOR PREV AGY/COMM'L           
UHCURLCB EQU   X'20'               TRIED LCB FOR CURRENT AGY/COMM'L             
UHPRELCB EQU   X'10'               TRIED LCB FOR PREVIOUS AGY/COMM'L            
UHDONE   EQU   X'08'                                                            
         SPACE 2                                                                
         USING SCRDETD,R2                                                       
VRINVR2  LA    R2,SDCYCSH                                                       
VRINV    MVI   ERROR,INVALID      INVALID INPUT FIELD                           
         B     VREND                                                            
         SPACE 1                                                                
VRNOINP  MVI   ERROR,ERNOINP      INPUT NOT ALLOWED                             
         B     VREND                                                            
         SPACE 1                                                                
VRMISS   MVI   ERROR,MISSING      MISSING INPUT FIELD                           
         B     VREND                                                            
         SPACE 1                                                                
VRDTINV  MVI   ERROR,INVDATE      INVALID DATE                                  
         B     VREND                                                            
         SPACE 1                                                                
CYCMTO   MVC   MYMSGNO,=Y(ERCYCMTO)                                             
         B     CYCENDX                                                          
         SPACE 1                                                                
CYNFCC   MVC   MYMSGNO,=Y(ERCYNFCC)                                             
         B     CYCENDX                                                          
         SPACE 1                                                                
CYCINC   MVC   MYMSGNO,=Y(ERCYCINC)                                             
         B     CYCEND                                                           
         SPACE 1                                                                
CYCNFT   MVC   MYMSGNO,=Y(ERCYCNFT)                                             
         B     CYCEND                                                           
         SPACE 1                                                                
CYCOVP   MVC   MYMSGNO,=Y(ERCYCOVP)                                             
         B     CYCEND                                                           
         SPACE 1                                                                
ERPPLSI  MVC   MYMSGNO,=Y(ERRIAPPA)   RECORD / ACTION INVALID FOR P+            
         J     CYCENDX                                                          
                                                                                
CYCEND   LA    R2,SDCYCSH                                                       
CYCENDX  MVI   BLOCK,0                                                          
         MVI   MYMTYP,GTMERR                                                    
         OI    GENSTAT2,USGETTXT                                                
         SPACE 1                                                                
VREND    GOTO1 EXIT,DMCB,0                                                      
         DROP  R2                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              DSECT TO COVER TABLE THAT DETERMINES WHICH COMMERCIAL            
*              RECORD THE VERSION CODE IS ON                                    
         SPACE 1                                                                
VINDEXD  DSECT                                                                  
VINDUPLM DS    X                 RECORD'S UPPER LIMIT                           
VINDEQUT DS    X                 RECORD'S EQUATE                                
VINDLNQ  EQU   *-VINDEXD                                                        
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR7BD                                                       
         SPACE 3                                                                
         ORG CYCWORK                                                            
         DS    D                                                                
ACMCTB   DS    A                                                                
ACMCTBX  DS    A                                                                
         SPACE 1                                                                
STRT     DS    PL3                 PACKED START                                 
END      DS    CL3                 AND END                                      
INV      DS    CL6                 INVOICE                                      
TYPE     DS    XL1                 USE TYPE EQUATE                              
CSEQ     DS    XL2                 CAST INPUT SEQUENCE NUMBER                   
MTINTC   DS    XL4                 CNET/MKT/CSYS INTERNAL CODE                  
         SPACE 1                                                                
SAVEKEY  DS    CL(L'KEY)                                                        
SRCHSTAT DS    XL1                                                              
SRCHCMC  EQU   X'80'                                                            
SRCHCYC  EQU   X'40'                                                            
SRCHVER  EQU   X'20'                                                            
         SPACE 1                                                                
SVINV    DS    XL6                                                              
SVVERS   DS    XL1                                                              
SVCRED   DS    XL1                                                              
SVCMCYC  DS    XL12                                                             
SVDA     DS    XL4                                                              
         SPACE 1                                                                
VERFILT  DS    XL1                                                              
         SPACE 1                                                                
LASTCMC  DS    XL30                                                             
LLASTCMC DS    XL30                                                             
         SPACE 1                                                                
CYCLEN   DS    X                                                                
LAGEND   DS    XL3                                                              
         SPACE 1                                                                
CHG1DATE DS    X                                                                
ELTAOC   DS    CL(TAOCLNQ)         OLD AGENCY/CID ELEMENT                       
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
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
*              DSECT TO COVER PASSIVE POINTER BLOCKS                            
         SPACE 1                                                                
TWD      DSECT                                                                  
DISPCMC  DS    CL((14*CMCLNQ)+1)       DISPLAYED CNET/MKT/CSYS                  
SVPTRS   DS    CL((56*L'TLDRREC)+1)    SAVED POINTERS                           
NEWPTRS  DS    CL((56*L'TLDRREC)+1)    NEW POINTERS                             
TWDLNQ   EQU   L'TWAHOLE-(*-TWD)                                                
         EJECT                                                                  
*              DSECT TO COVER CNET/MKT/CSYS TABLE ENTRIES                       
         SPACE 1                                                                
CMCD     DSECT                                                                  
CMCCODE  DS    CL(L'TAMTCODE)                                                   
CMCCYC   DS    XL6                                                              
CMCCMCYC DS    XL6                                                              
CMCINV   DS    XL6                                                              
CMCVER   DS    CL1                                                              
CMCCRED  DS    CL1                                                              
CMCINUM  DS    CL(L'TAMTINUM)                                                   
CMCSTAT  DS    XL1                                                              
CMCCHGD  EQU   X'80'                                                            
CMCLNQ   EQU   *-CMCD                                                           
CMCTBLNQ EQU   CMCLNQ*1130                                                      
         EJECT                                                                  
*              DSECT TO COVER CNET/MKT/CSYS SCREEN LINES                        
         SPACE 1                                                                
SCRDETD  DSECT                                                                  
SDCODEH  DS    XL8                                                              
SDCODE   DS    XL6                                                              
SDCYCSH  DS    XL8                                                              
SDCYCS   DS    XL8                                                              
         DS    XL8                                                              
SDDASH1H DS    XL8                                                              
SDDASH1  DS    XL1                                                              
SDCYCEH  DS    XL8                                                              
SDCYCE   DS    XL8                                                              
SDCCYCSH DS    XL8                                                              
SDCCYCS  DS    XL8                                                              
SDDASH2H DS    XL8                                                              
SDDASH2  DS    XL1                                                              
SDCCYCEH DS    XL8                                                              
SDCCYCE  DS    XL8                                                              
SDINVH   DS    XL8                                                              
SDINV    DS    XL6                                                              
SDVERH   DS    XL8                                                              
SDVER    DS    XL3                                                              
SDLNQ    EQU   *-SCRDETD                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005TAGEN7B   07/20/12'                                      
         END                                                                    
