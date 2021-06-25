*          DATA SET ACPRF02    AT LEVEL 018 AS OF 12/27/12                      
*PHASE T63002A                                                                  
*INCLUDE ACJOBCOL                                                               
*INCLUDE VATICAN                                                                
         TITLE 'ACPRF02 - PRESTO FALINK INTERFACE: JOB DOWNLOAD'                
T63002   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,ACPRF02*,RR=R4,R7                                              
*                                                                               
         LR    RC,R1                                                            
         USING WORKD,RC                                                         
         ST    R4,RELO                                                          
*                                                                               
         L     RA,ATWA                                                          
         USING TWAD,RA                                                          
*                                                                               
         USING TSARD,TSARBLK                                                    
*                                                                               
         MVC   ERROR,=Y(QNOJOB)                                                 
         CLC   CPJ,SPACES                                                       
         BE    SNDERMSG                                                         
*                                                                               
* VERSION CHECK                                                                 
         CLC   VERSION,=X'02070007'      VERSION 2.7.0.7 & HIGH ONLY            
         BNL   INIT                       ALLOWED!                              
         LA    R4,FAMSGBLK                                                      
         USING FAMSGD,R4                                                        
         MVC   FAMSGXTR(4),=X'02070007'                                         
         GOTO1 AADDDATA,DMCB,AFABLK,FALAYRF,FALAYRF                             
         B     XIT                                                              
*                                                                               
INIT     MVI   DELMTER,C'`'              SET DELIMITER                          
         CLC   VERSION,=X'02070015'      VERSION 2.7.0.21 & HIGH ONLY           
         BNL   *+8                                                              
         MVI   DELMTER,C'|'              OLD DELIMITER                          
*                                                                               
         CLI   SVRCVEL+1,X'01'           TEST                                   
         BE    RCV01H                                                           
         B     XIT                                                              
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *             
* RECEIVE 01 HEADER - JOB DOWNLOAD                                              
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *             
RCV01H   DS    0H                                                               
*                                                                               
         LA    R1,X'0001'          SEND RECORD TYPE (01=JOB DWNLD)              
         BAS   RE,SENDH                                                         
*                                                                               
         BAS   RE,BUILDDT          SEND DATE/TIME OBJECT                        
*                                                                               
         BAS   RE,FSTJOSP                                                       
         BNE   R01H10              IF END OF KEYS THEN NEXT RECFILT             
         GOTO1 GETREC                                                           
         BAS   RE,BUILDJO          SEND JOB OPEN OBJECT                         
*                                                                               
         BAS   RE,BUILDUF          BUILD USER FIELD OBJECTS                     
*                                                                               
R01H10   BAS   RE,FSTJESP          BUILD JOB ESTIMATE KEY                       
R01H20   BNE   R01H30              IF END OF KEYS THEN NEXT RECFILT             
         GOTO1 GETREC                                                           
         BAS   RE,BUILDJE          SEND JOB ESTIMATE OBJECT                     
*                                                                               
R01H25   GOTO1 SEQ                 GET NEXT KEY                                 
         USING EVERECD,R2                                                       
         LA    R2,KEY                                                           
         CLC   EVEKEY(EVEKJOB-EVEKEY+L'EVEKJOB),KEYSAVE                         
         BNE   R01H30                                                           
         OC    EVEKWC,EVEKWC       IS THIS A TIME ESTIMATE RECORD?              
         BNZ   R01H25              YES, SKIP IT                                 
         B     R01H20                                                           
         DROP  R2                                                               
*                                                                               
R01H30   BAS   RE,FSTJWSP                                                       
         BNE   XIT                 IF END OF KEYS THEN NEXT RECFILT             
         GOTO1 GETREC                                                           
         BAS   RE,BUILDJW                                                       
         B     XIT                                                              
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* THIS ROUTINE GETS THE CURRENT DATE/TIME AND BUILDS A DATE/TIME OBJECT         
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
BUILDDT  NTR1                                                                   
         L     R3,ACOMFACS         R3 = A(COMFACS)                              
         USING COMFACSD,R3                                                      
*                                                                               
         XC    BLOCK,BLOCK                                                      
         LA    R4,BLOCK                                                         
*                                                                               
         GOTO1 VDATCON,DMCB,(5,0),(X'20',0(R4))                                 
*                                                                               
         TIME  DEC                 BINARY CURRENT TIME IN FULL                  
         ST    R0,FULL                                                          
*                                                                               
         XC    DUB,DUB             R2 = HOUR + 8                                
         ZIC   RF,FULL                                                          
         SLL   RF,4                                                             
         ST    RF,DUB+4                                                         
         OI    DUB+7,X'0F'                                                      
         CVB   R2,DUB                                                           
*&&US*&& LA    R2,6(R2)            US USES 6AM BASED TIME                       
*&&UK*&& LA    R2,0(R2)            UK USES REAL TIME                            
*                                                                               
         C     R2,=F'24'           IF HOUR IS PAST 24 THEN SUBTRACT             
         BL    PDT50                   24 AND ADD 1 DAY                         
         S     R2,=F'24'                                                        
         GOTO1 VADDAY,DMCB,0(R4),(X'20',0(R4)),1                                
*                                                                               
PDT50    CVD   R2,DUB              STORE R2 BACK TO BINARY HOUR                 
         L     RF,DUB+4                                                         
         SRL   RF,4                                                             
         STC   RF,FULL                                                          
*                                  CURRENT TIME IN BYTES 14-19                  
         GOTO1 VHEXOUT,DMCB,FULL,6(R4),3                                        
*                                                                               
         LA    R1,MCJOB01                                                       
         SR    R5,R5                                                            
         LA    R4,BLOCK                                                         
         BAS   RE,SENDD                                                         
*                                                                               
PDTX     B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
*        CALL GETOPT IN PREPARATION FOR BUILDING JOB OPEN OBJECT                
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
FSTJOSP  NTR1                                                                   
*                                                                               
         LH    RF,=Y(GOBBLKX-GOBLOCKD)   CLEAR GOBLOCK AND GOBBLOCK             
         LA    RE,GOBLK                                                         
         ST    RE,AGOBLOCK                                                      
         XR    R1,R1                                                            
         MVCL  RE,R0                                                            
         L     R5,AGOBLOCK                                                      
         USING GOBLOCKD,R5                                                      
         LA    RE,GOBBLK           SET POINTER TO CBILL BLOCK                   
         ST    RE,GOABEXT                                                       
*                                  CALL GETOPT                                  
         GOTO1 =A(RDOPT),DMCB,(RC),(R5),RR=RELO                                 
*                                                                               
         MVC   ERROR,=Y(QNOSCHM)   MISSING SCHEME ON JOB                        
         CLC   GOSCHEME,SPACES     CHECK FOR MISSING SCHEME                     
         BNH   SNDERMSG                                                         
*                                                                               
         XC    GOABEXT,GOABEXT     DON'T NEED CBILL OPTIONS ANY MORE            
*                                                                               
         USING ACTRECD,R3          BUILD JOB KEY                                
         LA    R3,KEY                                                           
         MVC   KEY,SPACES                                                       
         MVC   ACTKCPY(3),CUL                                                   
         MVC   ACTKACT(12),CPJ                                                  
         GOTO1 HIGH                                                             
         CLC   ACTKCULA,KEYSAVE    SET CONDITION CODE FOR XIT                   
*                                                                               
         B     XIT                                                              
         DROP  R3,R5                                                            
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* BUILD A JOB ESTIMATE KEY BASED ON REQUEST JOB                                 
*        RETURNS CC NEQ IF NO JOB ESTIMATES ARE FOUND                           
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
FSTJESP  NTR1                                                                   
         LA    R2,KEY                                                           
         USING EVERECD,R2                                                       
         XC    EVEKEY,EVEKEY       BUILD ESTIMATE KEY                           
         MVI   EVEKTYP,EVEKTYPQ                                                 
         MVI   EVEKSUB,EVEKSUBQ                                                 
         MVC   EVEKCPY(3),CUL                                                   
*                                                                               
         MVC   EVEKCLI,QCLNT                                                    
         MVC   EVEKPRO,QPROD                                                    
         MVC   EVEKJOB,QJOB                                                     
         MVI   EVEKTYPE,EVEKTPLN                                                
*                                                                               
         GOTO1 HIGH                GET FIRST KEY                                
         CLC   EVEKEY(EVEKJOB-EVEKEY+L'EVEKJOB),KEYSAVE                         
         B     XIT                                                              
         DROP  R2                                                               
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
*     PREP BUILDING THE JOB WORKCODE OBEJCTS                                    
*        JUST GET THE JOB IN AIO                                                
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
FSTJWSP  NTR1                                                                   
         USING ACTRECD,R3          BUILD JOB KEY                                
         LA    R3,KEY                                                           
         MVC   KEY,SPACES                                                       
         MVC   ACTKCPY(3),CUL                                                   
         MVC   ACTKACT(12),CPJ                                                  
         GOTO1 HIGH                                                             
         CLC   ACTKCULA,KEYSAVE                                                 
         B     XIT                 JOB VALIDATED IN VALREQ                      
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* BUILD JOB OPEN OBJECT, GETOPT CALLED IN FSTJOSP                               
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
BUILDJO  NTR1                                                                   
         L     R5,AGOBLOCK                                                      
         USING GOBLOCKD,R5                                                      
*                                                                               
         XC    BLOCK,BLOCK                                                      
         LA    R4,BLOCK                                                         
*                                                                               
         LA    R1,QCLNT                    CLIENT                               
         MVI   LENGTH,L'QCLNT                                                   
         BAS   RE,SNDCODE                                                       
*                                                                               
         LA    R1,QPROD                    PRODUCT                              
         MVI   LENGTH,L'QPROD                                                   
         BAS   RE,SNDCODE                                                       
*                                                                               
         LA    R1,QJOB                     JOB                                  
         MVI   LENGTH,L'QJOB                                                    
         BAS   RE,SNDCODE                                                       
*                                                                               
         LA    R1,GOSCHEME                 SCHEME                               
         MVI   LENGTH,L'GOSCHEME                                                
         BAS   RE,SNDCODE                                                       
*                                                                               
         LA    R1,GONEEDAE                 ESTIMATE APPROVAL FLAG               
         MVI   LENGTH,L'GONEEDAE                                                
         BAS   RE,SNDDATA                                                       
*                                                                               
         LA    R1,GOBILTYP                 BILL TYPE                            
         MVI   LENGTH,L'GOBILTYP                                                
         BAS   RE,SNDDATA                                                       
*                                                                               
         EDIT  (P4,GOOVRPER),(5,(R4)),FILL=0  MAX OVER EST PCT                  
         AHI   R4,5-1                                                           
         BAS   RE,SNDDATA                                                       
*                                                                               
         MVC   JOBCUR,GOBILCUR     BILLING CURRENCY                             
         MVI   SECCURR,C'N'        SET SECOND CURRENCY FLAG TO 'NO'             
*&&UK                                                                           
         TM    COMPSTA7,CPYSSCNV   TEST CONVERTED AGENCY                        
         BZ    BJO20                                                            
*                                                                               
         CLC   JOBCUR,COMPCUR      TEST PRIMARY CURRENCY                        
         BE    BJO20               YES-ALL DONE                                 
         CLC   JOBCUR,COMPCURS     TEST SECONDARY CURRENCY                      
         BE    *+14                YES-SET FLAG ACCORDINGLY                     
         MVC   JOBCUR,COMPCUR                                                   
         B     BJO20                                                            
*                                                                               
         MVI   SECCURR,C'Y'                                                     
*&&                                                                             
*                                                                               
BJO20    LA    R1,JOBCUR           BILLING CURRENCY                             
         MVI   LENGTH,L'JOBCUR                                                  
         BAS   RE,SNDDATA                                                       
*                                                                               
         MVI   ELCODE,JOBELQ       GET NEXT BILLING PERCENTAGE                  
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING JOBELD,R6                                                        
*                                                                               
         MVC   ERROR,=Y(QBOEST)                                                 
         TM    JOBSTA1,JOBSMCSE    TEST JOB IS BRANDOCEAN                       
         BO    SNDERMSG            SEND AN ERROR IF IT IS                       
*                                                                               
         MVI   XJOB,C'N'           SET XJOB FLAG                                
         TM    JOBSTA1,JOBSXJOB    TEST FOR XJOB                                
         BZ    *+8                                                              
         MVI   XJOB,C'Y'           YES                                          
*                                                                               
         SR    R0,R0                                                            
         CLI   GOBILTYP,C'E'       PCT OF EST BILLING                           
         BNE   BJO50                                                            
         L     R1,GOBILAM1                                                      
         TM    JOBBIST,JOBB1BIL    FIRST BILLING DONE?                          
         BZ    BJO30               NO, CURRENT PCT IN R1                        
*                                                                               
         A     R1,GOBILAM2                                                      
         TM    JOBBIST,JOBB2BIL    SECOND?                                      
         BZ    BJO30               NO, CURRENT PCT IN R1                        
*                                                                               
         A     R1,GOBILAM3                                                      
*                                                                               
BJO30    LR    R0,R1                                                            
BJO50    BRAS  RE,SNDNUM                                                        
*                                                                               
         LA    R1,MCJOB02                                                       
         LA    R5,BLOCK                                                         
         SR    R5,R4               GIVES OUTPUT DATA                            
         LPR   R5,R5                                                            
         LA    R4,BLOCK                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         B     YES                                                              
         DROP  R3,R5,R6                                                         
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
*        BUILD JOB USER FIELD OBJECTS                                           
*              AT ENTRY, AIO = A(JOB)                                           
*              CALLED BY BUILDJO                                                
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
BUILDUF  NTR1  ,                                                                
         L     R6,AIO                                                           
         MVI   ELCODE,UFSELQ       USER FIELD ELEMENT                           
         BAS   RE,GETEL                                                         
*                                                                               
BUF10    BNE   BUFX                ALL DONE WITH PROCESSING                     
         USING UFSELD,R6                                                        
*                                                                               
         CLI   UFSLN,UFSDATA-UFSELD TEST FOR NO DATA                            
         BE    BUF20                YES-SKIP THE ELEMENT                        
*                                                                               
         XC    BLOCK,BLOCK                                                      
         LA    R4,BLOCK                                                         
*                                                                               
         LA    R1,UFSCODE          USER FIELD CODE                              
         MVI   LENGTH,L'UFSCODE                                                 
         BAS   RE,SNDCODE                                                       
*                                                                               
         LA    R1,UFSDESC          USER FIELD DESCRIPTION                       
         MVI   LENGTH,L'UFSDESC                                                 
         BAS   RE,SNDDATA                                                       
*                                                                               
         ZIC   R1,UFSLN                                                         
         SH    R1,=Y(UFSDATA-UFSELD)                                            
         STC   R1,LENGTH                                                        
         LA    R1,UFSDATA          USER FIELD DATA                              
         BAS   RE,SNDDATA                                                       
*                                                                               
         MVI   0(R4),C'0'          SHOW ON ESTIMATES FLAG= 0 OR 1               
         TM    UFSSTAT,UFSSSHES                                                 
         BZ    *+8                                                              
         MVI   0(R4),C'1'                                                       
         BAS   RE,SNDDLM                                                        
*                                                                               
         GOTO1 VHEXOUT,DMCB,UFSSEQ,WORK,L'UFSSEQ,0                              
         MVC   0(2,R4),WORK        SEQUENCE NUMBER                              
         AHI   R4,1                                                             
         BAS   RE,SNDDLM                                                        
*                                                                               
         LA    R1,MCJOB03                                                       
         LA    R5,BLOCK                                                         
         SR    R5,R4               GIVES OUTPUT DATA                            
         LPR   R5,R5                                                            
         LA    R4,BLOCK                                                         
         BAS   RE,SENDD                                                         
*                                                                               
BUF20    BAS   RE,NEXTEL                                                        
         B     BUF10                                                            
*                                                                               
BUFX     XIT1  REGS=(R3)                                                        
         DROP  R6                                                               
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
*        BUILD JOB ESTIMATE OBJECT FOR THE ESTIMATE RECORD IN PARM 1            
*              BUILD CELL OBJECTS FOR EACH WORKCODE ON THER ESTIMATE            
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
BUILDJE  NTR1                                                                   
         L     R2,AIO                                                           
         USING EVERECD,R2                                                       
*                                                                               
         XC    BLOCK,BLOCK                                                      
         LA    R4,BLOCK                                                         
*                                                                               
         LA    R1,EVEKTYPE         ESTIMATE TYPE                                
         MVI   LENGTH,L'EVEKTYPE                                                
         BAS   RE,SNDCODE                                                       
*                                                                               
         ZIC   R0,EVEKVER          VERSION NUMBER                               
         BRAS  RE,SNDNUM                                                        
*                                                                               
         MVI   BYTE,FFTTACUR       LOOK FOR CURRENCY ELEMENT                    
         GOTO1 SRCHGET,DMCB,FFTELQ,(1,BYTE)                                     
         BNE   BJE005                                                           
         USING FFTELD,R6                                                        
*                                                                               
         LA    R1,FFTDATA          EXTRACT CURRENCY                             
         MVI   LENGTH,3                                                         
BJE005   BAS   RE,SNDCODE                                                       
*                                                                               
         LR    R6,R2                                                            
*                                                                               
         MVI   ELCODE,ENAELQ       GET ESTIMATE NAME DATA                       
         BAS   RE,GETEL                                                         
         BE    BJE006                                                           
         BAS   RE,SNDDLM                                                        
         B     BJE008                                                           
*                                                                               
         USING ENAELD,R6                                                        
BJE006   ZIC   R1,ENALN            SET ESTIMATE NAME                            
         SHI   R1,3                                                             
         CHI   R1,14                                                            
         BNH   *+8                                                              
         LA    R1,14                                                            
         STC   R1,LENGTH                                                        
         LA    R1,ENAME                                                         
         BAS   RE,SNDDATA                                                       
*                                                                               
         BAS   RE,NEXTEL           IS THERE A SECOND ESTIMATE NAME              
         BNE   BJE008              NO                                           
*                                                                               
         ZIC   R1,ENALN            WRITE NAME 2                                 
         SHI   R1,3                                                             
         CHI   R1,14                                                            
         BNH   *+8                                                              
         LA    R1,14                                                            
         STC   R1,LENGTH                                                        
         LA    R1,ENAME                                                         
BJE008   BAS   RE,SNDDATA                                                       
*                                                                               
         LR    R6,R2                                                            
*                                                                               
         MVI   ELCODE,EUPELQ       GET ESTIMATE UPDATE ELEMENT                  
         BAS   RE,GETEL                                                         
         BE    BJE010                                                           
         MVC   0(3,R4),=C'```'     SKIP ESTIMATE CHANGE DATA                    
         CLC   VERSION,=X'02070015' VERSION 2.7.0.21 & HIGH ONLY                
         BNL   *+10                                                             
         MVC   0(3,R4),=C'|||'      OLD DELIMITER                               
         LA    R4,3(R4)                                                         
         B     BJE020                                                           
*                                                                               
         USING EUPELD,R6                                                        
BJE010   LA    R1,EUPERS           LAST CHANGE PERSON                           
         MVI   LENGTH,L'EUPERS                                                  
         BAS   RE,SNDDATA                                                       
*                                                                               
         GOTO1 VDATCON,DMCB,(1,EUPLAST),(2,MONDATE)  SET MONDATE/TIME           
         MVC   MONTIME,EUPTIME                      FOR MAIN2PC                 
         GOTO1 =A(MAIN2PC),DMCB,(RC),WORK,WORK2,RR=RELO                         
         LA    R1,WORK             LAST CHANGE DATE                             
         MVI   LENGTH,6                                                         
         BAS   RE,SNDDATA                                                       
         LA    R1,WORK2            LAST CHANGE TIME                             
         MVI   LENGTH,6                                                         
         BAS   RE,SNDDATA                                                       
*                                                                               
BJE020   LR    R6,R2                                                            
         MVI   ELCODE,EAPELQ       GET ESTIMATE APPROVAL ELEMENT                
         XC    WORK2(3),WORK2      CLEAR IF NO DATE                             
         BAS   RE,GETEL                                                         
         BE    BJE025                                                           
         BAS   RE,SNDDLM           NO DATA                                      
         B     BJE030                                                           
*                                                                               
         USING EAPELD,R6                                                        
BJE025   LA    R1,EAPPBY           ESTIMATE APPROVER                            
         MVI   LENGTH,L'EAPPBY                                                  
         BAS   RE,SNDDATA                                                       
*                                                                               
         MVC   WORK2(3),EAPDATE                                                 
BJE030   BRAS  RE,SETDATE1                                                      
*                                                                               
         LR    R6,R2                                                            
         MVI   ELCODE,EPRELQ       ESTIMATE PREPARER                            
         XC    WORK2(3),WORK2      CLEAR IF NO DATE                             
         BAS   RE,GETEL                                                         
         BE    BJE035                                                           
         BAS   RE,SNDDLM           NO DATA                                      
         B     BJE040                                                           
*                                                                               
         USING EPRELD,R6                                                        
BJE035   LA    R1,EPRPREP          ESTIMATE PREPARER                            
         MVI   LENGTH,L'EPRPREP                                                 
         BAS   RE,SNDDATA                                                       
*                                                                               
         MVC   WORK2(3),EPRDATE                                                 
BJE040   BRAS  RE,SETDATE1                                                      
*                                                                               
         LA    R1,MCJOB04          ESTIMATE OBJECT                              
         LA    R5,BLOCK                                                         
         SR    R5,R4               GIVES OUTPUT DATA                            
         LPR   R5,R5                                                            
         LA    R4,BLOCK                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         USING EDAELD,R6           FROM ESTIMATE DATA ELEMENTS                  
         LR    R6,R2                                                            
         MVI   ELCODE,EDAELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   BJEX                NO DATA ON ESTIMATE                          
*                                                                               
BJE050   TM    EDATYPE,EDATWORK    TEST FOR WORKCODE ESTIMATE                   
         BZ    BJE090              NO                                           
*                                                                               
         XC    BLOCK,BLOCK                                                      
         LA    R4,BLOCK                                                         
*                                                                               
         LA    R1,EDAWORK          CELL WORKCODE                                
         MVI   LENGTH,L'EDAWORK                                                 
         BAS   RE,SNDCODE                                                       
*                                                                               
         ZAP   DUB,EDACOMM         GET ESTIMATE AMOUNT                          
         TM    EDATYPE,EDATCOHE    COMMISSION OVERRIDE?                         
         BO    BJE060              YES, IT'S IN NON-COMM                        
         CLI   EDALN,EDALNQ2       NO, ANY NON-COMM AMOUNT?                     
         BL    *+10                NO                                           
         AP    DUB,EDANCOM         YES, ADD TO COMMISSIONABLE                   
*                                                                               
*                                  PROTECT AGAINST BAD DATA(TEST FILES)         
BJE060   CP    DUB,MAXEST          TEST FOR FULLWORD OVERFLOW                   
         BNH   *+10                                                             
         ZAP   DUB,MAXEST          YES-SET VALUE TO MAXIMUM                     
         CP    DUB,MINEST                                                       
         BNL   *+10                                                             
         ZAP   DUB,MINEST                                                       
         CVB   R0,DUB              CONVERT ESTIMATE & MOVE TO CELL              
         BRAS  RE,SNDNUM           SEND AMOUNT                                  
*                                                                               
         TM    EDATYPE,EDATCOHE    COMMISSION OVERRIDE?                         
         BNZ   BJE065              YES                                          
         BAS   RE,SNDDLM           NO, AND NO AMOUNT EITHER                     
         B     BJE070                                                           
*                                                                               
BJE065   ZAP   DUB,EDANCOM         YES, GET AMOUNT                              
         CVB   R0,DUB              CONVERT IT & MOVE TO CELL                    
         BRAS  RE,SNDNUM                                                        
*                                                                               
         MVI   0(R4),C'Y'          SET THE FLAG                                 
BJE070   BAS   RE,SNDDLM                                                        
*                                                                               
         SR    R0,R0               SET SUFFIX=0                                 
         TM    EDATYPE,EDATSUB     TEST FOR SUB-WORKCODE                        
         BZ    BJE080              NO                                           
         IC    R0,EDASUBC                                                       
BJE080   BRAS  RE,SNDNUM                                                        
*                                                                               
         LA    R1,MCJOB05          CELL OBJECT                                  
         LA    R5,BLOCK                                                         
         SR    R5,R4               GIVES OUTPUT DATA                            
         LPR   R5,R5                                                            
         LA    R4,BLOCK                                                         
         BAS   RE,SENDD                                                         
*                                                                               
BJE090   BAS   RE,NEXTEL                                                        
         BE    BJE050                                                           
*                                                                               
BJEX     B     YES                                                              
         DROP  R2,R6                                                            
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
*    BUILD JOB WORKCODE OBJECTS FOR THE LIST OF WORKCODES WHICH IS              
*    DEFINED AS THE UNION OF WORKCODES IN THE JOBS SCHEME, WORKCODES            
*    WHICH HAVE BEEN ESTIMATED AGAINST, AND WORKCODES AGAINST WHICH             
*    CHARGES HAVE BEEN BATCHED.                                                 
*    THE LIST OF WORKCODES IS BUILT USING TSAR BUFFERING.                       
*    FOR EACH WORKCODE IN THE LIST, ACTUAL AMOUNTS, COMMISSION RATES            
*    AND ACTUAL CHARGES ARE MAINTAINED                                          
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
*    JWBUF IS STORAGE NEEDED TO FOR VATICAN CALL                                
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
BUILDJW  NTR1  WORK=(R2,JWBUFSZ)                                                
         ST    R2,ATAXTAB                                                       
         XC    0(TAXTABSZ,R2),0(R2)                                             
         LA    R5,TAXTABSZ(R2)                                                  
         ST    R5,AVATBUFF                                                      
*                                                                               
         LA    R5,GOBLK                                                         
         ST    R5,AGOBLOCK                                                      
*                                                                               
         LH    RF,=Y(GOBLOCKX-GOBLOCKD)  CLEAR GOBLOCK                          
         L     RE,AGOBLOCK                                                      
         XR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         GOTO1 =A(SETTSAR),DMCB,(RC),RR=RELO                                    
         GOTO1 =A(RDOPT),DMCB,(RC),AGOBLOCK,RR=RELO                             
*                                                                               
*&&US                                                                           
         CLI   CTRY,CTRYCAN        CANADA                                       
         BNE   *+8                                                              
*&&                                                                             
         OI    RUNOPT,NEEDTAX                                                   
*                                                                               
*        CALL JOBBER, WRITE SCHEME WORKCODES TO TSAR                            
         GOTO1 =A(JWPUTSCH),DMCB,(RC),RR=RELO                                   
*                                                                               
*        PUT WORKCODES WHICH HAVE BEEN ESTTIMATED TO TSAR                       
         GOTO1 =A(JWPUTEST),DMCB,(RC),RR=RELO                                   
*                                                                               
*        PUT WORKCODES WHICH ARE IN OPEN ORDERS TO TSAR                         
         GOTO1 =A(JWPUTORD),DMCB,(RC),RR=RELO                                   
*                                                                               
*        PUT ACTUAL WORKCODES TO TSAR                                           
         GOTO1 =A(JWPUTACT),RR=RELO                                             
*                                                                               
*        BUILD JOB WORKCODE OBJECTS FROM TSAR                                   
         BAS   RE,BLDJWOBJ                                                      
*                                                                               
         B     YES                                                              
*                                                                               
         EJECT                                                                  
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
*        READ JOB CHARGES, SET ACTUAL BUCKETS, PUT RECORD TO TSAR               
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
*                                                                               
JWPUTACT NTR1                                                                   
         USING ACTRECD,R3          BUILD JOB KEY                                
         LA    R3,KEY                                                           
         MVC   KEY,SPACES                                                       
         MVC   ACTKCPY(3),CUL                                                   
         MVC   ACTKACT(12),CPJ                                                  
         GOTO1 HIGH                                                             
*                                                                               
         USING TRNRECD,R3                                                       
JWPA010  LA    R3,KEY                                                           
         MVC   MYKEYSV,KEY         SAVE KEY IN CASE OF INTERVENING IO'S         
*                                                                               
         CLC   TRNKCULA,KEYSAVE    IF NO LONGER SAME JOB THEN DONE              
         BNE   JWPAX                                                            
*                                                                               
         CLC   TRNKREF,=CL15' '    SKIP NON-TRANSACTIONS                        
         BNH   JWPA30                                                           
*                                                                               
         CLC   TRNKWORK,=C'**'     SKIP PURCHASE ORDERS                         
         BE    JWPA30                                                           
*                                                                               
         LA    R5,BLOCK            POINT R5 TO BUCKETS                          
         USING JWTABD,R5                                                        
         USING JWCABD,BLOCK2                                                    
*                                                                               
         GOTO1 GETREC              GET THE TRANSACTION RECORD                   
         L     R3,AIO                                                           
*                                                                               
         L     R0,AIO4             SAVE COPY OF RECORD IN IO4                   
         L     R1,=A(LENIO)        LENGTH OF IO AREAS                           
         LR    RF,R1                                                            
         LR    RE,R3               ADDRESS OF ORIGINAL RECORD                   
         MVCL  R0,RE                                                            
*                                                                               
         TM    TRNRSTAT,TRNSDRFT   SKIP DRAFT ITEMS                             
         BO    JWPA30                                                           
*                                                                               
         GOTO1 GETELEM,DMCB,TRNELQ SKIP NON-TRANSACTIONS                        
         BNE   JWPA30                                                           
*                                                                               
         ST    R6,ATRNEL           SAVE A(TRANSACTION ELEMENT)                  
*                                                                               
         BAS   RE,XTRACT           EXTRACT ACTUAL BUCKETS                       
*                                                                               
*&&UK                                                                           
         L     R3,AIO4             USE IO4 IN CASE CHANGED BY TOBACCO           
*&&                                                                             
         MVI   BILFLAG,0           INITIALIZE FLAG                              
         BAS   RE,XTRBIL           GET BILLING                                  
*                                                                               
JWPA20   XC    KEY,KEY             RESTORE READ SEQ                             
         LA    R3,KEY              BUMP KEY                                     
         MVC   TRNKEY(TRNKEND),MYKEYSV                                          
*                                                                               
* BUMP KEY TO GET NEXT (USE 2 BYTES BECAUSE TRNKSBR CAN REACH 255               
         ZICM  RF,TRNKREF+L'TRNKREF-1,(3)     BUMP KEY TO GET NEXT              
         LA    RF,1(RF)                                                         
         STCM  RF,3,TRNKREF+L'TRNKREF-1                                         
         GOTO1 HIGH                                                             
         B     JWPA010                                                          
*                                                                               
JWPA30   GOTO1 SEQ                 READ SEQ NOT BROKEN                          
         B     JWPA010                                                          
*                                                                               
JWPAX    XIT1                                                                   
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
*        EXTRACT ACTUAL BUCKET AMOUNTS                                          
*        R3 HAS A(RECORD) WHICH IS COVERED BY TRNRECD                           
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
XTRACT   NTR1                                                                   
         CLC   TRNKWORK,=C'99'     SKIP 99'S WHICH ARE FOR BILLING ONLY         
         BE    XTRACTX                                                          
*                                                                               
         BAS   RE,JWCRCLR          CLEAR BLOCK TO BUILD TSAR RECORD             
         MVC   JWTWC,TRNKWORK      SAVE WORKCODE IN BLOCK                       
         MVC   JWCCONTR,TRNKULC    SAVE THE CONTRA IN BLOCK2                    
*                                                                               
         USING TRNELD,R6                                                        
         MVC   WORKCODE,TRNKWORK   SET WORKCODE FOR GETWCRAT CALL               
         GOTO1 =A(GETWCRAT),DMCB,(RC),RR=RELO                                   
*                                                                               
         ZAP   JWTCRATE,COMRATE    COMRATE RETURNED BY GETWCRAT                 
         OI    JWTSTAT,JWTSRATE                                                 
*                                                                               
         TM    RUNOPT,NEEDTAX      DO I NEED TO GET VAT/GST RATE                
         BNO   XACT10                                                           
         GOTO1 =A(GETRATE),DMCB,(RC),RR=RELO                                    
         ZAP   JWTTRATE,PL16       GET VAT RATE                                 
*                                                                               
XACT10   DS    0H                                                               
*&&UK                                                                           
         TM    COMPSTA7,CPYSSCNV   TEST EURO ZONE COMPANY                       
         BZ    XACT20              NO                                           
         BAS   RE,EURACT                                                        
         B     XACT40                                                           
*&&                                                                             
XACT20   LA    R4,JWTBUCKS         ADDRESS BUCKETS                              
         USING BUCKD,R4                                                         
*                                                                               
         L     R6,ATRNEL           RESTORE R6=A(TRANSACTION)                    
         USING TRNELD,R6                                                        
         ZAP   DUB,TRNAMNT                                                      
*                                                                               
         CLI   XJOB,C'Y'           TEST FOR XJOB                                
         BNE   XACT30              NO                                           
*                                                                               
*&&US                                                                           
         MVI   BYTE,SCITSJXP       LOOK FOR 'S' BUCKETS                         
         GOTO1 SRCHGET,DMCB,SCIELQ,(1,BYTE)                                     
         BNE   *+10                COULD NOT FIND IT                            
         USING SCIELD,R6                                                        
         ZAP   DUB,SCIAMNT         GET MEMO AMOUNT                              
*&&                                                                             
*                                                                               
XACT30   GOTO1 =A(BUCKET),DMCB,(RC),RR=RELO                                     
*                                                                               
XACT40   L     R6,ATRNEL              RESTORE TRANSACTION                       
         USING TRNELD,R6                                                        
         LR    R4,R6                  SAVE TRANSACTION ADDRESS                  
         LA    R2,JWTTBUCK            GET TIME BUCKETS                          
         CLI   TRNTYPE,49             TYPE 49 IS ALWAYS TIME                    
         BE    XACT50                                                           
*&&UK                                                                           
         CLI   TRNTYPE,34             SOME TYPE 34'S ARE TIME                   
         BE    XACT60                 THEY ARE ONLY B TIME W/SCIELS             
         CLI   TRNTYPE,14             WRITE-OFF WITH TIME                       
         BE    XACT60                                                           
         B     XACT100                                                          
*&&                                                                             
XACT50   DS    0H                                                               
         GOTO1 GETELEM,DMCB,PRTELQ,0                                            
         BNE   XACT100                 NO PERSONNEL ELEMENT                     
         USING PRTELD,R6                                                        
*&&US                                                                           
         TM    PRTSTAT,PRTSBILQ        IS IT 'B' TIME?                          
         BZ    *+10                    NO                                       
         ZAP   0(BUCKLN,R2),PRTHOUR                                             
         LA    R2,BUCKLN(0,R2)                                                  
         TM    PRTSTAT,PRTSNOTQ        IS IT 'N' TIME?                          
         BZ    *+10                    NO                                       
         ZAP   0(BUCKLN,R2),PRTHOUR                                             
         LA    R2,BUCKLN(0,R2)                                                  
         TM    PRTSTAT,PRTSRTEQ        MUST BE 'R' TIME                         
         BZ    XACT100                                                          
         ZAP   0(BUCKLN,R2),PRTHOUR                                             
         LA    R2,BUCKLN(0,R2)                                                  
         ZAP   PL16,PRTRATE                                                     
         MP    PL16,PRTHOUR                                                     
         SRP   PL16,64-2,5                                                      
         ZAP   0(BUCKLN,R2),PL16                                                
*&&                                                                             
*&&UK                                                                           
         CP    TRNAMNT-TRNEL(L'TRNAMNT,R4),=P'0' ANY AMOUNT?                    
         BNE   XACT60                 YES, MUST BE 'B' TIME                     
*                                                                               
         LA    R2,BUCKLN(0,R2)        NO, IS THERE A RATE?                      
         CP    PRTRATE,=P'0'                                                    
         BE    XACT60                 NO, IT'S 'N' TIME                         
*                                                                               
         LA    R2,BUCKLN(0,R2)        MUST BE 'R' TIME                          
*                                                                               
XACT60   MVI   BYTE,SCITSJHR          LOOK FOR 'T' BUCKETS                      
         GOTO1 SRCHGET,DMCB,SCIELQ,(1,BYTE)                                     
         BNE   XACT100                COULD NOT FIND IT                         
         USING SCIELD,R6                                                        
         ZAP   0(BUCKLN,R2),SCIAMNT                                             
*                                                                               
         LA    RE,JWTRHR              DO WE HAVE 'R' TIME?                      
         CR    R2,RE                  NOPE, ALL DONE                            
         BNE   XACT100                                                          
*                                                                               
         LA    R2,BUCKLN(0,R2)        YES, GET THE RVALUE                       
         MVI   BYTE,SCITCRAT          LOOK FOR 'C' BUCKETS                      
         GOTO1 SRCHGET,DMCB,SCIELQ,(1,BYTE)                                     
         BNE   XACT100                COULD NOT FIND IT                         
         USING SCIELD,R6                                                        
         ZAP   0(BUCKLN,R2),SCIAMNT                                             
*                                                                               
         LA    R2,BUCKLN(0,R2)                                                  
         GOTO1 GETELEM,DMCB,OCAELQ,0  GET SECOND CURRENCY                       
         BNE   XACT100                                                          
         USING OCAELD,R6                                                        
         SR    R0,R0                                                            
         IC    R0,OCANUM              GET NUMBER OF ENTRIES                     
         LA    RE,OCANTRY             GET FIRST ENTRY                           
*                                                                               
         USING OCANTRY,RE                                                       
XACT70   CLI   OCANTYPE,QSCITCOMM     IS IT THE EXTENDED COST?                  
         BE    XACT90                 YES                                       
         CLI   OCANTYPE,QPRTRATE      IS IT THE RATE?                           
         BE    *+8                    YES, ONLY MOVE 6 BYTES                    
*                                                                               
XACT80   LA    RE,2(RE)               NO, MOVE 2 + 6                            
         LA    RE,OCANTRYL-2(RE)                                                
         BCT   R0,XACT70                                                        
         B     XACT100                                                          
*                                                                               
XACT90   CLI   OCANSEQN,2             MUST BE SEQUENCE NUMBER 2                 
         BNE   XACT80                                                           
         ZAP   0(BUCKLN,R2),OCANCASH  GET THE 2ND CURRENCY VALUE                
*&&                                                                             
*                                                                               
XACT100  GOTO1 =A(PUTTSAR),DMCB,(RC),BLOCK,RR=RELO                              
*                                                                               
         LA    R2,JWTTBUCK         LOOK FOR DATA IN TIME BUCKETS                
         LA    R0,JWCTADDS                                                      
*                                                                               
XACT120  CP    0(BUCKLN,R2),=P'0'  ONLY WANT THIS IF TIME                       
         BNE   XACT130                                                          
         LA    R2,BUCKLN(0,R2)                                                  
         BCT   R0,XACT120                                                       
         B     XTRACTX                                                          
*                                                                               
XACT130  MVC   JWCWC,JWTWC         SET WC AND BUCKETS                           
         MVC   JWCNBKT(JWCNBKTL),JWTBUCKS                                       
         MVC   JWCSBKT(JWCSBKTL),JWTSBUCK                                       
         MVC   JWCTBKT(JWCTBKTL),JWTTBUCK                                       
         GOTO1 =A(PUTTSA2),DMCB,(RC),BLOCK2,RR=RELO                             
*                                                                               
XTRACTX  XIT1                                                                   
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
*        EXTRACT BILLING BUCKET AMOUNTS                                         
*        R3 HAS A(RECORD) WHICH IS COVERED BY TRNRECD                           
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
XTRBIL   NTR1                                                                   
         BAS   RE,JWCRCLR          CLEAR BLOCK TO BUILD TSAR RECORD             
*                                                                               
         LR    R6,R3               SET UP R6 FOR GETEL CALLS                    
         CLC   TRNKWORK,=C'99'     FIRST DEAL WITH WORKCODE TRASACTIONS         
         BE    XBIL100                                                          
*                                                                               
* HANDLE WORKCODE TRANSACTIONS (THOSE OTHER THAN 99)                            
*    TOTAL(21), 1-LINE(21), PROGRESSIVE(21),                                    
*    ALLOCATED BILLING (27) - OFTEN REFERRED TO AS CLIENT BILLING,              
*    CLIENT BILLING ($BILL)                                                     
*                                                                               
         MVI   ELCODE,PTAELQ       GET PROD TRANS ACTIVITY ELEMENT (77)         
         BAS   RE,GETEL                                                         
         BNE   XBIL30              SKIP TO BNDEL(4B) LOOP                       
         USING PTAELD,R6                                                        
*                                                                               
XBIL10   CLI   PTATYPE,PTATRAL     MUST BE TYPE ALLOCATE TO BILL                
         BNE   XBIL20                                                           
         TM    PTASTAT1,PTASPEND   SKIP PENDING ACTIVITY                        
         BO    XBIL20                                                           
*                                                                               
         MVC   JWTWC,TRNKWORK      SET WORKCODE                                 
*                                                                               
         AP    JWTBNET,PTANET      ACCUMULATE NET BILLED AMOUNT                 
         MVC   HALF,PTAHOURS       DO AN LH IN CASE NEGATIVE HOURS              
         LH    R1,HALF             ACCUMULATE BILLED HOURS                      
         CVD   R1,DUB                                                           
         AP    JWTBHOUR,DUB                                                     
         AP    JWTBCOMM,PTARCOM    ACCUMULATE COMMISSION                        
*                                                                               
*&&UK*&& GOTO1 =A(BILLNO),DMCB,(C'A',PTARBLNO),PTANET,PTARCOM,RR=RELO           
*                                                                               
XBIL20   BAS   RE,NEXTEL           GET NEXT ELEMENT AND LOOP BACK               
         BE    XBIL10                                                           
*                                                                               
XBIL30   DS    0H                                                               
*&&UK                                                                           
* PROCESS 4B ELEMENT                                                            
*                                                                               
         LR    R6,R3                                                            
         MVI   ELCODE,BNDELQ       GET BILL NUMBER/DATE ELEMENT (4B)            
         BAS   RE,GETEL                                                         
         BNE   XBIL60                                                           
         USING BNDELD,R6                                                        
*                                                                               
XBIL40   CLC   BNDBNO,SPACES       TEST IF ITEM HAS BEEN BILLED                 
         BE    XBIL50              CHECK BILL NUMBER = SPACES                   
*                                                                               
         MVC   JWTWC,TRNKWORK      SET WORKCODE                                 
*                                                                               
         ICM   R1,15,BNDAMNT       ACCUMULATE NET BILLED AMOUNT                 
         CVD   R1,DUB                                                           
         AP    JWTBNET,DUB                                                      
*                                                                               
* ADD BILL TO TABLE                                                             
         ICM   R1,15,BNDAMNT       ALLOCATED BILL AMOUNT                        
         CVD   R1,DUB                                                           
         ZAP   ALCTNET,DUB         SEND IN PACKED FORM HERE                     
         ZAP   ALCTCOM,=P'0'       NO ALLOCATED COMMISSION IN 4B'S              
         GOTO1 =A(BILLNO),DMCB,(C'A',BNDBNO),ALCTNET,ALCTCOM,RR=RELO            
*                                                                               
XBIL50   BAS   RE,NEXTEL           GET NEXT ELEMENT AND LOOP BACK               
         BE    XBIL40                                                           
*&&                                                                             
*                                                                               
XBIL60   OC    JWTWC,JWTWC         IF WC SET, SEND TO TSAR                      
         BZ    XTRBILX                                                          
         B     XBIL300                                                          
*                                                                               
* HANDLE 99 TRANSACTIONS                                                        
*                                                                               
XBIL100  DS    0H                                                               
*&&US                                                                           
* CHECK FOR BILL/UNBILL PAIR IN E5 ELEMENT                                      
*    THIS MEANS A TOTAL BILL RAN AFTER THIS BILL SO IGNORE IT.                  
*                                                                               
         USING GDAEL,R6            CHECK FOR BILL/UNBILL PAIR                   
         MVI   ELCODE,GDAELQ       GENERAL DATE ELEMENT 'E5'                    
         BAS   RE,GETEL                                                         
         BNE   XBIL105                                                          
*                                                                               
* FIND LAST E5 ELEMENT WITH BILLING INFO DATES                                  
XBIL103  CLI   GDATYPE,GDATBILL    BILLING INFO DATES                           
         BNE   *+6                 DON'T NEED THIS ELEMENT                      
         LR    R2,R6               SAVE ADDRESS OF LAST GDAEL                   
         BAS   RE,NEXTEL                                                        
         BE    XBIL103                                                          
*                                                                               
         LR    R6,R2               LOAD ADDRESS OF LAST GDAEL                   
         CLI   GDALN,GDALNQ        CHECK LENGTH OF ELEMENT                      
         BE    XTRBILX             ALREADY BILLED SO DON'T INCLUDE              
         OC    GDADATE2,GDADATE2   IS IT UNBILLED?                              
         BZ    XTRBILX             NO, ALREADY BILLED SO DON'T INCLUDE          
         DROP  R6                                                               
*                                                                               
XBIL105  LR    R6,R3               SET UP R6 FOR GETEL CALLS                    
*&&                                                                             
*&&UK                                                                           
XBIL107  GOTO1 =A(BILLNO),DMCB,(C'G',TRNKREF),RR=RELO   CHECK IF ALOCTD         
*&&                                                                             
*                                                                               
         USING TRNELD,R6                                                        
         LR    R6,R3                                                            
         MVI   ELCODE,TRNELQ       GET TRANSACTION ELEMENT (44)                 
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*&&US                                                                           
*                                                                               
         CLI   TRNBTYPE,TRNBTSPE   CONTINUE IF SPECIAL AMOUNT                   
         BE    XBIL110                                                          
         CLI   TRNBTYPE,TRNBTMAN   CONTINUE IF MANUAL BILLING                   
         BE    XBIL110                                                          
         CLI   TRNBTYPE,TRNBTPER   CONTINUE IF PERCENT OF ESTIMATE              
         BNE   XTRBILX                                                          
*                                                                               
XBIL110  MVC   JWTWC,TRNKWORK      SET WORKCODE                                 
         AP    JWTBNET,TRNAMNT     ACCUMULATE NET BILLED AMOUNT                 
         AP    JWTBCOMM,TRNBLCOM   ACCUMULATE COMMISSION AMOUNT                 
         B     XBIL300             PUT RECORD TO TSAR THEN DONE                 
*&&                                                                             
*                                                                               
* IN UK, CHECK LENGTH OF ELEMENT TO SEE IF COMM & VAT IS IN 44 ELEMENT          
*   OR IN SCIEL (50) ELEMENT                                                    
*&&UK                                                                           
XBIL190  MVC   JWTWC,TRNKWORK      SET WORKCODE                                 
         AP    JWTBNET,TRNAMNT     ACCUMULATE NET BILLED AMOUNT                 
*                                                                               
         CLI   TRNLN,TRNLNBQ       CHECK LENGTH OF ELEMENT                      
         BL    XBIL210             TOO SMALL, CHECK SCIEL (50)                  
*                                                                               
         AP    JWTBCOMM,TRNCOMM    ACCUMULATE COMMISSION AMOUNT                 
*                                                                               
         LA    RF,TRNVATS          RF=A(VAT AMOUNT FIELDS)                      
         LA    R0,TRNVATN          R0=(MAX NUM OF VAT AMOUNTS)                  
XBIL200  AP    JWTBTAX,0(L'TRNVATS,RF) ACCUMULATE EST TAX AMOUNT                
         LA    RF,L'TRNVATS(RF)    GET NEXT VAT AMOUNT                          
         BCT   R0,XBIL200                                                       
         B     XBIL250             SKIP SCIEL (50) & SEND TO TSAR               
*                                                                               
* IN UK, CHECK SCIEL(50) FOR COMMISSION AND VAT                                 
*                                                                               
XBIL210  LR    R6,R3                                                            
         MVI   ELCODE,SCIELQ       GET SUBSIDIARY CASH INFO ELEMENT(50)         
         BAS   RE,GETEL                                                         
         BNE   XBIL250                                                          
         USING SCIELD,R6                                                        
*                                                                               
XBIL220  TM    SCITYPE,X'40'       TEST IF VAT TYPE FOR BILL                    
         BO    XBIL230                                                          
         AP    JWTBTAX,SCIVBIL     VAT TAX BILLED                               
         B     XBIL240                                                          
*                                                                               
XBIL230  CLI   SCITYPE,SCITCOMM    TEST IF COMMISSION BILLED                    
         BNE   XBIL240                                                          
         AP    JWTBCOMM,SCIAMNT    ACCUMULATE COMMISSION BILLED                 
*                                                                               
XBIL240  BAS   RE,NEXTEL           GET NEXT ELEMENT AND LOOP BACK               
         BE    XBIL220                                                          
*                                                                               
XBIL250  TM    BILFLAG,BLUSDAT     IF BILL HAS A USED DATE                      
         BNO   XBIL260             CLEAR EVERYTHING EXCEPT TAX                  
         ZAP   JWTBHOUR,=P'0'      CLEAR HOURS                                  
         ZAP   JWTBCOMM,=P'0'      CLEAR COMMISION                              
         ZAP   JWTBNET,=P'0'       CLEAR NET, ONLY NEED TAX IN 99               
         B     XBIL300                                                          
*                                                                               
XBIL260  TM    BILFLAG,BLALCTD     IF BILL NOT ALCTD OR USED DATE               
         BNO   XBIL300             THEN WE ARE DONE                             
         ZAP   JWTBHOUR,=P'0'      CLEAR HOURS                                  
         SP    JWTBNET,ALCTNET     (-) NET AMOUNT ALREADY ALLOCATED             
         SP    JWTBCOMM,ALCTCOM    (-) COMM ALREADY ALLOCATED                   
*&&                                                                             
*                                                                               
XBIL300  GOTO1 =A(PUTTSAR),DMCB,(RC),BLOCK,RR=RELO                              
*                                                                               
XTRBILX  XIT1                                                                   
         DROP  R3,R4,R5,R6                                                      
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* THIS ROUTINE KEEPS TRACK OF HOW MUCH OF A BILL HAS BEEN ALLOCATED             
* WHEN THE 99 RECORD IS FOUND WITH A MATCHING BILL, THE AMOUNT THAT             
* HAS BEEN ALLOCATED SHOULD BE SUBRACTED FROM THE AMOUNT IN THE 99              
* RECORD.                                                                       
*    PARM 1  BYTE 0   = A: ADD BILL # TO THE TABLE                              
*                       G: CHECK IF BILL # IS IN THE TABLE                      
*                          TURN ON BLALCTD BIT IN BILFLAG IF FOUND              
*                          GET ALLOCATED BILL AMOUNTS                           
*            BYTE 1-3 = A(BILL #)                                               
*    PARM 2  NET BILLED AMOUNT                                                  
*    PARM 3  COMMISSION AMOUNT                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
*&&UK                                                                           
BILLNO   NTR1                                                                   
         MVC   BYTE,0(R1)          ACTION                                       
         L     R3,0(R1)            A(BILL #)                                    
         L     R4,4(R1)            A(BILL AMOUNT)                               
         L     R5,8(R1)            A(BILL AMOUNT)                               
         LA    R2,ALBILLS          A(TABLE OF BILL #'S)                         
         USING ALBILLD,R2                                                       
*                                                                               
BILNO10  CLI   ALBILNO,X'FF'       CHECK IF RAN OUT OF SPACE IN TABLE           
         BNE   *+6                                                              
         DC    H'0'                NEED TO MAKE TABLE LARGER                    
         CLI   ALBILNO,0           CHECK IF REACHED END OF TABLE                
         BE    BILNO20                                                          
         CLC   ALBILNO,0(R3)       CHECK IF BILL # ALREADY IN TABLE             
         BE    BILNO30                                                          
         LA    R2,L'ALBILLS(R2)                                                 
         B     BILNO10                                                          
*                                                                               
BILNO20  CLI   BYTE,C'G'           JUST CHECKING IF BILL NO IN TABLE?           
         BE    XIT                 YES, COULDN'T FIND IT SO DONE.               
         MVC   ALBILNO,0(R3)       NO, ADDING TO TABLE                          
         MVC   ALBILNET,0(R4)      BILL AMOUNT                                  
         MVC   ALBILCOM,0(R5)      COMMISSION AMOUNT                            
         B     XIT                                                              
*                                                                               
BILNO30  CLI   BYTE,C'G'           JUST CHECKING IF BILL NO IN TABLE?           
         BE    BILNO40                                                          
         AP    ALBILNET,0(L'ALBILNET,R4) ADD MORE NET BILLED AMOUNTS            
         AP    ALBILCOM,0(L'ALBILCOM,R5) ADD MORE COMM BILLED AMOUNTS           
         B     XIT                                                              
*                                                                               
BILNO40  OI    BILFLAG,BLALCTD     BILL WAS ALLOCATED                           
         ZAP   ALCTNET,ALBILNET    ALLOCATED NET BILL AMOUNT                    
         ZAP   ALCTCOM,ALBILCOM    ALLOCATED COMM AMOUNT                        
         B     XIT                                                              
         DROP  R2                                                               
*&&                                                                             
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
*        INITIALIZE PACKED BUCKETS IN BLOCK & BLOCK2                            
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
         USING JWTABD,R5                                                        
JWCRCLR  NTR1                                                                   
         LA    R5,BLOCK                                                         
         XC    JWTREC(JWTABLN),JWTREC                                           
         LA    R1,JWTFBUCK                                                      
         LA    R0,JWTNBUCK                                                      
JWC40    ZAP   0(BUCKLN,R1),=P'0'                                               
         LA    R1,BUCKLN(R1)                                                    
         BCT   R0,JWC40                                                         
*                                                                               
         LA    R5,BLOCK2                                                        
         XC    JWCREC(JWCABLN),JWCREC                                           
         LA    R1,JWCNBKT                                                       
         LA    R0,JWCNADDS                                                      
JWC60    ZAP   0(BUCKLN,R1),=P'0'                                               
         LA    R1,BUCKLN(R1)                                                    
         BCT   R0,JWC60                                                         
         XIT1                                                                   
         DROP  R5                                                               
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* SUB-ROUTINE TO GET EURO-ZONE ACTUALS                                          
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
*&&UK                                                                           
EURACT   NTR1  ,                                                                
         L     R6,ATRNEL                                                        
         USING TRNELD,R6                                                        
         LA    R5,BLOCK                                                         
         USING JWTABD,R5                                                        
         ZAP   DUB,TRNAMNT         GET PRIMARY CURRENCY AMOUNT                  
*                                                                               
         MVC   WORK(L'COMPCUR),COMPCUR                                          
         MVC   WORK+L'COMPCUR(L'COMPCURS),COMPCURS                              
         GOTO1 VTOBACCO,DMCB,('TOBAAOUT',WORK),AIO,ACOMFACS,0,0,0               
         GOTO1 GETELEM,DMCB,TRNELQ,0                                            
         ZAP   TAMT,TRNAMNT        GET SECONDARY CURRENCY AMOUNT                
*                                                                               
* SET PRIMARY AND SECONDARY AMOUNTS BASED ON WHICH CURRENCY REQUESTED           
*                                                                               
EURACT10 ZAP   PRIMAMT,DUB                                                      
         ZAP   SECAMT,TAMT                                                      
         CLI   SECCURR,C'N'        TEST PRIMARY CURRENCY REQUESTED              
         BE    EURACT20            YES                                          
         ZAP   PRIMAMT,TAMT        NO-SECONDARY CURRENCY WANTED                 
         ZAP   SECAMT,DUB                                                       
*                                                                               
EURACT20 LA    R4,JWTBUCKS         PRIMARY CURRENCY BUCKETS                     
         ZAP   DUB,PRIMAMT                                                      
         GOTO1 =A(BUCKET),DMCB,(RC),RR=RELO                                     
*                                                                               
         LA    R4,JWTSBUCK         SECONDARY CURRENCY BUCKETS                   
         ZAP   DUB,SECAMT                                                       
         GOTO1 =A(BUCKET),DMCB,(RC),RR=RELO                                     
*                                                                               
EURACTX  XIT1                                                                   
         DROP  R5,R6                                                            
*&&                                                                             
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
*        SENDS TSAR RECORDS TO FALINK                                           
*        BUILD OBJECTS IN TIA+GETOPPT OPTIMIZATION TABLE                        
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
         USING JWTABD,R3                                                        
BLDJWOBJ NTR1                      BUILD OBJECTS FROM TSAR RECORDS              
         LA    R3,MEDWORK                                                       
         XC    JWTREC(JWTKEYLN),JWTREC                                          
*                                                                               
         USING TSARD,R1                                                         
         LA    R1,TSARBLK                                                       
         MVI   TSACTN,TSARDH                                                    
         ST    R3,TSAREC                                                        
         GOTO1 VTSAR                                                            
         TM    TSERRS,TSEEOF       ANY TSAR RECORDS                             
         BO    BJW0X               NO, NO OBJECTS                               
*                                                                               
         XC    BLOCK,BLOCK                                                      
         LA    R4,BLOCK                                                         
*                                                                               
BJW010   MVC   SVTSRNUM,TSRNUM   SAVE RECORD NUMBER IN CASE RATE                
*                                  NEEDED                                       
         MVI   MAPCODE,MCJOB06                                                  
         CLI   JWTREC,0            IF FIRST BYTE IS ZERO, USE JWCABD            
         BE    BJW100                                                           
*                                                                               
         CLC   JWTWC,=C'99'        SKIP SENDING COMM RATE FOR 99'S              
         BE    BJW020                                                           
*                                                                               
         TM    JWTSTAT,JWTSRATE    IS RATE SET                                  
         BO    BJW020              YES                                          
         MVC   WORKCODE,JWTWC                                                   
         GOTO1 =A(GETWCRAT),DMCB,(RC),RR=RELO                                   
         ZAP   JWTCRATE,COMRATE                                                 
         OI    JWTSTAT,JWTSRATE    FLAG RATE IS SET                             
*                                                                               
BJW020   LA    R1,JWTWC            WORKCODE                                     
         MVI   LENGTH,L'JWTWC                                                   
         BAS   RE,SNDCODE                                                       
*                                                                               
         ZIC   R0,JWTSUFF                                                       
         BRAS  RE,SNDNUM                                                        
*                                                                               
         LA    R2,JWTCRATE         FIRST PACKED BUCKET                          
         LA    R1,JWTNBUCK                                                      
*                                                                               
BJW030   ZAP   DUB,0(BUCKLN,R2)                                                 
*                                  PROTECT AGAINST BAD DATA(TEST FILES)         
         CP    DUB,MAXBUCK         TEST FOR FULLWORD OVERFLOW                   
         BNH   *+10                                                             
         ZAP   DUB,MAXBUCK         YES-SET VALUE TO MAXIMUM                     
         CP    DUB,MINBUCK                                                      
         BNL   *+10                                                             
         ZAP   DUB,MINBUCK                                                      
*                                                                               
         CVB   R0,DUB              SEND BUCKET TO FALINK                        
         BRAS  RE,SNDNUM                                                        
*                                                                               
         LA    R2,BUCKLN(R2)       NEXT PACKED BUCKED                           
         BCT   R1,BJW030                                                        
*                                                                               
         ZIC   R1,MAPCODE                                                       
         LA    R5,BLOCK                                                         
         SR    R5,R4               GIVES OUTPUT DATA                            
         LPR   R5,R5                                                            
         LA    R4,BLOCK                                                         
         BAS   RE,SENDD                                                         
*                                                                               
BJW040   DS    0H                                                               
         USING TSARD,R1                                                         
         LA    R1,TSARBLK                                                       
         MVI   TSACTN,TSANXT       GET THE NEXT RECORD NUMBER                   
         MVC   TSRNUM,SVTSRNUM                                                  
         ST    R3,TSAREC                                                        
         GOTO1 VTSAR                                                            
         TM    TSERRS,TSEEOF       ANY TSAR RECORDS                             
         BNO   BJW010              NO, BO OBJECTS                               
         B     BJW0X                                                            
*                                                                               
         USING JWCABD,R3                                                        
BJW100   MVI   MAPCODE,MCJOB07                                                  
         LA    R1,JWCWC            WORKCODE                                     
         MVI   LENGTH,L'JWCWC                                                   
         BAS   RE,SNDCODE                                                       
*                                                                               
         ZIC   R0,JWCSUFF          SUFFIX                                       
         BRAS  RE,SNDNUM                                                        
*                                                                               
         LA    R1,JWCCONTR         CONTRA ACCOUNT                               
         MVI   LENGTH,L'JWCCONTR                                                
         BAS   RE,SNDCODE                                                       
*                                                                               
         LA    R2,JWCNBKT          FIRST PACKED BUCKET                          
         LA    R1,JWCNADDS                                                      
         B     BJW030                                                           
*                                                                               
BJW0X    XIT1                                                                   
         LTORG                                                                  
         DROP  R1,R3                                                            
*                                                                               
MAXBUCK  DC    P'2147000000'                                                    
MINBUCK  DC    P'-2147000000'                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
SNDERMSG GOTO1 SENDMSG                                                          
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
*                                                                               
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* ON ENTRY R1 CONTAINS HEADER CODE                                    *         
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
SENDH    LR    R0,RE                                                            
         GOTO1 GETHDR              GET HEADER ADDRESS                           
         GOTO1 ASETELEM,DMCB,AFABLK,HDRADDR,0,0                                 
SENDHX   LR    RE,R0                                                            
         BR    RE                                                               
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* PARMS ARE FABLK,MAP_TABLE_ENTRY,A(DATA),OVRD_LEN                    *         
* ON ENTRY R1 CONTAINS DATA ITEM NUMBER WITHIN CURRENT ELEMENT        *         
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
SENDD    LR    R0,RE                                                            
         GOTO1 GETDATA             GET DATA ITEM                                
         GOTO1 AADDDATA,DMCB,AFABLK,DATADDR,(R4),(R5)                           
         SR    R5,R5               CLEAR OVERRIDE LENGTH                        
         LR    RE,R0                                                            
         BR    RE                                                               
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* PUT DATA IN THE BUFFER, IF ANY                                                
* CHECK IF DATA SENDING HAS A DELIMITER IN IT                                   
* BACK UP TO LAST NONBLANK AND INSERT DELIMITER                                 
* ON ENTRY R1 POINTS TO THE DATA SENDING                                        
*          R4 POINTS TO SPOT IN BUFFER TO PUT DATA                              
*          RE RETURN ADDRESS                                                    
*          LENGTH OF DATA SENDING                                               
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
SNDCODE  MVI   CODE,C'Y'                   DATA IS A CODE                       
*                                                                               
* MOVE DATA TO THE BUFFER POINTED TO BY R4                                      
SNDDATA  SR    RF,RF                       LENGTH OF DATA                       
         ICM   RF,1,LENGTH                                                      
         BZ    SNDDLM                      NO DATA                              
         MVI   LENGTH,0                    CLEAR FOR NEXT TIME                  
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R1)               MOVE DATA TO BUFFER                  
         LR    R1,R4                       POINT R1 TO START OF DATA            
         AR    R4,RF                       POINT R4 TO THE END OF DATA          
         AHI   RF,1                                                             
*                                                                               
CHK10    CLC   0(1,R1),DELMTER             REPLACE "`" WITH " ", IF ANY         
         BNE   CHK20                         IN THE DATA                        
         CLI   CODE,C'Y'                   WAS DATA A CODE?                     
         BE    CHK30                                                            
         MVI   0(R1),C' '                                                       
CHK20    AHI   R1,1                                                             
         BRCT  RF,CHK10                                                         
         MVI   CODE,C' '                   CLEAR CODE SETTING                   
         B     SNDDLM                                                           
CHK30    MVC   ERROR,=Y(QINVCODE)          UNLESS THIS IS A CODE, THEN          
         B     SNDERMSG                       SEND AN ERROR                     
*                                                                               
SNDDLM   CLI   0(R4),C' '                  REMOVE TRAILING SPACES               
         JH    *+8                                                              
         BRCT  R4,SNDDLM                                                        
         MVC   1(1,R4),DELMTER                                                  
         AHI   R4,2                                                             
         BR    RE                                                               
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* SEND A DATE IN MMDDYY FORMAT                                        *         
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
SETDATE1 DS    0H                                                               
         LR    R5,RE                                                            
         OC    WORK2(3),WORK2                                                   
         BZ    SDATE10                                                          
         GOTO1 VDATCON,DMCB,(1,WORK2),(X'20',WORK)                              
         MVC   0(6,R4),WORK                                                     
         AHI   R4,5                                                             
SDATE10  BRAS  RE,SNDDLM                                                        
         LR    RE,R5                                                            
         BR    RE                                                               
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* TRANSMIT NUMERIC VALUE IN R0 AND INSERT SEMICOLON DELIMITER                   
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
SNDNUM   DS    0H                                                               
         LTR   R0,R0               TEST IF NEGATIVE                             
         BNM   *+12                                                             
         MVI   0(R4),C'-'          PUT NEGATIVE SIGN IN FRONT OF NUMBER         
         LA    R4,1(R4)                                                         
*                                                                               
         EDIT  (R0),(10,(R4)),ZERO=NOBLANK,ALIGN=LEFT                           
         AR    R4,R0                                                            
         MVC   0(1,R4),DELMTER                                                  
         LA    R4,1(R4)                                                         
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
MAXEST   DC    P'2147000000'                                                    
MINEST   DC    P'-2147000000'                                                   
*                                                                               
SECSINMN EQU   60                  SECONDS IN A MIN                             
SECSINHR EQU   60*SECSINMN         NUMBER OF SECONDS IN AN HOUR                 
EIGHTHRS EQU   8*SECSINHR          SECONDS IN EIGHT HOURS                       
TWO4HRS  EQU   24*SECSINHR         SECONDS IN A DAY                             
*                                                                               
LENTIA   EQU   18432                                                            
LENGOOPT EQU   4000                                                             
LENJBBUF EQU   LENTIA-LENGOOPT                                                  
JWBUFSZ  EQU   TAXTABSZ+VTCLNQ                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* SUB-ROUTINE TO COMPUTE BUCKETS FOR POSTING                                    
* AT ENTRY, DUB CONTAINS TRANSACTION AMOUNT AND R4=A(BUCKETS)                   
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
BUCKET   NMOD1 0,**BUKT**                                                       
         L     RC,0(R1)                                                         
*                                                                               
         USING JWTABD,R5                                                        
         USING BUCKD,R4                                                         
         L     R6,ATRNEL                                                        
         USING TRNELD,R6                                                        
         TM    TRNSTAT,TRNSNOCM    TEST NON-COMMISSIONABLE                      
         BNO   BUCKET2                                                          
*                                                                               
         ZAP   BUCKNCOM,DUB                                                     
         B     BUCKET5                                                          
*                                                                               
BUCKET2  ZAP   BUCKNET,DUB         COMPUTE COMMISSION                           
         ZAP   PL16,DUB                                                         
         MP    PL16,COMRATE                                                     
         SRP   PL16,64-6,5                                                      
         ZAP   BUCKCOM,PL16                                                     
         ZAP   DUB,BUCKNET                                                      
*                                                                               
BUCKET5  TM    RUNOPT,NEEDTAX                                                   
         BNO   BUCKETX                                                          
*                                                                               
*        ZAP   DUB,BUCKNET         APPLY TAX RATE TO NET+COMMISSION             
         AP    DUB,BUCKCOM                                                      
         ZAP   PL16,JWTTRATE                                                    
         MP    PL16,DUB                                                         
         SRP   PL16,64-4,5                                                      
         ZAP   BUCKTAX,PL16                                                     
*                                                                               
BUCKETX  XIT1                                                                   
         DROP  R4,R5,R6                                                         
         LTORG                                                                  
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
*        CALL GETOPT, P1 IS A(18 BYTE CL6 CLIENT, CL6 PRODUCT AND A             
*              CL6 JOB)                                                         
*             P2 IS A(SPACE TO BUILD GOBLOCK)                                   
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
RDOPT    NMOD1 0,**RDOP**                                                       
         L     RC,0(R1)                                                         
         L     R5,4(R1)                                                         
         USING GOBLOCKD,R5         BUILD GETOPT KEY                             
         MVC   GOADM,VDATAMGR                                                   
         MVC   GOSELCUL,CUL                                                     
         MVC   GOSELCLI,QCLNT                                                   
         MVC   GOSELPRO,QPROD                                                   
         MVC   GOSELJOB,QJOB                                                    
*                                                                               
         L     RE,ATIA             CLEAR GETOPT OPT TABLE                       
         L     RF,=A(LENGOOPT)                                                  
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         L     RF,ATIA             USE 2K OF TIA FOR OPT TABLE                  
         ST    RF,GOABUFF                                                       
         MVC   GOLBUFF,=A(LENGOOPT)                                             
         GOTO1 VGETOPT,DMCB,GOBLOCK                                             
         XIT1                                                                   
         DROP  R5                                                               
         LTORG                                                                  
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
*        READ ESTIMATE RECORDS FOR JOB WORKCODE BUILD                           
*        PUT WORKCODE VALUES TO TSAR                                            
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
JWPUTEST NMOD1 0,**PEST**                                                       
         L     RC,0(R1)                                                         
         USING EVERECD,R6                                                       
         LA    R6,KEY              BUILD ESTIMATE KEY                           
         XC    EVEKEY,EVEKEY                                                    
         MVI   EVEKTYP,EVEKTYPQ                                                 
         MVI   EVEKSUB,EVEKSUBQ                                                 
         MVC   EVEKCPY(3),CUL                                                   
         MVC   EVEKCLI,QCLNT                                                    
         MVC   EVEKPRO,QPROD                                                    
         MVC   EVEKJOB,QJOB                                                     
         MVI   EVEKTYPE,EVEKTPLN                                                
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
JWPE10   CLC   KEY(EVEKTYPE-EVEKEY),KEYSAVE                                     
         BNE   JWPEX               ALL DONE WITH ESTIMATES                      
*                                                                               
         GOTO1 GETREC              GET THE ESTIMATE RECORD                      
*                                                                               
         USING EVERECD,R6                                                       
         L     R6,AIO                                                           
         LA    R6,EVERFST          EXTRACT ELEMENTS FROM ESTIMATE REC           
*                                                                               
         USING EDAELD,R6                                                        
JWPE30   CLI   0(R6),0             END OF RECORD                                
         BE    JWPE50              GET NEXT ESTIMATE                            
*                                                                               
         CLI   0(R6),EDAELQ        ESTIMATE DATA ELEMENT                        
         BNE   JWPE40              GET NEXT ELEMENT                             
         TM    EDATYPE,EDATWORK    TEST FOR WORKCODE ESTIMATE                   
         BZ    JWPE40              NO                                           
*                                                                               
         USING JWTABD,R5           BUILD BINSRCH RECORD IN BLOCK                
         BAS   RE,JWPECLR          INITIALIZE BLOCK AS TABLE ENTRY              
         LA    R5,BLOCK                                                         
         MVC   JWTWC,EDAWORK                                                    
         TM    EDATYPE,EDATSUB     TEST FOR SUB-WORKCODE ESTIMATE               
         BZ    *+10                                                             
         MVC   JWTSUFF,EDASUBC     EXTRACT SUFFIX                               
         GOTO1 =A(PUTTSAR),DMCB,(RC),BLOCK,RR=RELO                              
*                                                                               
JWPE40   ZIC   R1,1(R6)            GET NEXT ESTIMATE DATA ELEMENT               
         LA    R6,0(R1,R6)                                                      
         B     JWPE30                                                           
*                                                                               
JWPE50   GOTO1 SEQ                 READ NEXT ESTIMATE RECORD                    
         B     JWPE10                                                           
*                                                                               
JWPEX    XIT1                                                                   
*                                                                               
*        CLEAR PACKED BUCKETS IN TSAR INPUT RECORDS                             
*                                                                               
         USING JWTABD,R5                                                        
JWPECLR  EQU   *                                                                
         LA    R5,BLOCK                                                         
         XC    JWTREC(JWTABLN),JWTREC                                           
         LA    R1,JWTFBUCK                                                      
         LA    R0,JWTNBUCK                                                      
JWPEC40  ZAP   0(BUCKLN,R1),=P'0'                                               
         LA    R1,BUCKLN(R1)                                                    
         BCT   R0,JWPEC40                                                       
         BR    RE                                                               
*                                                                               
         DROP  R5,R6                                                            
         LTORG                                                                  
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
*        READ OPEN ORDER RECORDS FOR JOB - BUILD JOB WORKCODE DATA              
*        PUT WORKCODE VALUES TO TSAR                                            
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
JWPUTORD NMOD1 0,**PORD**                                                       
         L     RC,0(R1)                                                         
         USING ACTRECD,R3                VALIDATE KEY                           
         LA    R3,KEY                                                           
         MVC   KEY,SPACES                                                       
         MVC   ACTKCPY(3),CUL                                                   
         MVC   ACTKACT(12),CPJ                                                  
         DROP  R3                                                               
*                                                                               
         LA    R6,KEY              BUILD TRANSACTION KEY                        
         USING TRNRECD,R6                                                       
         MVC   TRNKWORK,=C'**'     ORDERS ARE UNDER WORKCODE = **               
         GOTO1 HIGH                                                             
*                                                                               
JWPO10   LA    R6,KEY                                                           
         CLC   TRNKEY(TRNKCULC-TRNKEY),KEYSAVE                                  
         BNE   JWPOX               ALL DONE WITH ORDERS                         
*                                                                               
         CLC   TRNKDATE,=CL3' '    TEST FOR VALID DATE                          
         BE    JWPO50              NO-GET NEXT RECORD                           
         GOTO1 GETREC              GET THE ORDER RECORD                         
         L     R6,AIO                                                           
*                                                                               
*&&UK                                                                           
         TM    COMPSTA7,CPYSSCNV   TEST EURO ZONE COMPANY                       
         BNO   JWPO15                                                           
         BAS   RE,EURORD                                                        
         GOTO1 =A(PUTTSAR),DMCB,(RC),BLOCK,RR=RELO                              
         B     JWPO45              GET NEXT RECORD                              
*&&                                                                             
JWPO15   LA    R6,TRNRFST          EXTRACT ELEMENTS FROM ORDER REC              
         USING OAMELD,R6                                                        
*                                                                               
JWPO20   CLI   0(R6),0             END OF RECORD                                
         BE    JWPO45              GET NEXT ORDER                               
*                                                                               
         CLI   0(R6),OAMELQ        ORDER AMOUNT ELEMENT                         
         BNE   JWPO40              GET NEXT ELEMENT                             
*                                                                               
         ST    R6,AOAMEL                                                        
         USING JWTABD,R5           BUILD BINSRCH RECORD IN BLOCK                
         BAS   RE,JWPOCLR          INITIALIZE BLOCK AS TABLE ENTRY              
         LA    R5,BLOCK                                                         
         MVC   JWTWC,OAMWORK                                                    
         ZAP   OPENAMT,OAMAMNT     GET ORDER AMOUNT                             
         SP    OPENAMT,OAMIVAL     SUBTRACT INVOICED AMOUNT                     
         BP    *+10                (CAN'T HAVE NEGATIVE RESULT)                 
         ZAP   OPENAMT,=P'0'       TO DERIVE OPEN ORDER AMOUNT                  
*                                                                               
         GOTO1 POSTORD,POSTOPRI                                                 
*                                                                               
JWPO36   GOTO1 =A(PUTTSAR),DMCB,(RC),BLOCK,RR=RELO                              
*                                                                               
JWPO40   ZIC   R1,1(R6)            GET NEXT ORDER AMOUNT ELEMENT                
         LA    R6,0(R1,R6)                                                      
         B     JWPO20                                                           
*                                                                               
JWPO45   GOTO1 READ                RESTORE KEY SEQUENCE FIRST                   
JWPO50   GOTO1 SEQ                 READ NEXT PSEUDO ORDER RECORD                
         B     JWPO10                                                           
*                                                                               
JWPOX    XIT1                                                                   
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
*        CLEAR PACKED BUCKETS IN TSAR INPUT RECORDS                             
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
         USING JWTABD,R5                                                        
JWPOCLR  EQU   *                                                                
         LA    R5,BLOCK                                                         
         XC    JWTREC(JWTABLN),JWTREC                                           
         LA    R1,JWTFBUCK                                                      
         LA    R0,JWTNBUCK                                                      
JWPOC40  ZAP   0(BUCKLN,R1),=P'0'                                               
         LA    R1,BUCKLN(R1)                                                    
         BCT   R0,JWPOC40                                                       
         BR    RE                                                               
*                                                                               
         DROP  R5,R6                                                            
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* SUB-ROUTINE TO POST OPEN ORDER AMOUNT AND DERIVATIVES TO BUCKETS              
* AT ENTRY, OPENAMT = OPEN ORDER AMOUNT, R1=BUCKET TYPE                         
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
POSTORD  NTR1  ,                                                                
         STC   R1,BYTE                                                          
         LA    R5,BLOCK                                                         
         USING JWTABD,R5                                                        
         L     R6,AOAMEL                                                        
         USING OAMELD,R6                                                        
*                                                                               
         LA    R1,JWTONET                                                       
*&&UK                                                                           
         CLI   BYTE,POSTOPRI                                                    
         BE    *+8                                                              
         LA    R1,JWTOSNT                                                       
*&&                                                                             
         ZAP   0(L'JWTONET,R1),OPENAMT                                          
*                                                                               
         TM    OAMSTAT,OAMSNOCM    TEST NON-COMMISSIONABLE WC                   
         BNO   POSTORD4            NO                                           
*                                                                               
         LA    R1,JWTOCOM          SET COMMISSION BUCKET TO ZERO                
*&&UK                                                                           
         CLI   BYTE,POSTOPRI                                                    
         BE    *+8                                                              
         LA    R1,JWTOSCM                                                       
*&&                                                                             
         ZAP   0(L'JWTOCOM,R1),=P'0'                                            
         B     POSTORD6                                                         
*                                                                               
* LOOK UP WC COMMISSION RATE AND CALCULATE COMMISSION                           
*                                                                               
POSTORD4 MVC   WORKCODE,OAMWORK                                                 
         GOTO1 =A(GETWCRAT),DMCB,(RC),RR=RELO                                   
         TM    JWTSTAT,JWTSRATE    TEST RATE ALREADY LOOKED UP                  
         BO    *+14                YES                                          
         OI    JWTSTAT,JWTSRATE    NO-SET INDICATOR                             
         ZAP   JWTCRATE,COMRATE    SAVE COMMISSION RATE                         
*                                                                               
         ZAP   PL16,OPENAMT                                                     
         MP    PL16,COMRATE        OPEN ORDER AMT X COMMISSION RATE             
         SRP   PL16,64-6,5                                                      
         LA    R1,JWTOCOM                                                       
*&&UK                                                                           
         CLI   BYTE,POSTOPRI                                                    
         BE    *+8                                                              
         LA    R1,JWTOSCM                                                       
*&&                                                                             
         ZAP   0(L'JWTOCOM,R1),PL16                                             
*                                                                               
* LOOK UP VAT RATE AND CALCULATE OPEN ORDER TAX                                 
*                                                                               
POSTORD6 TM    RUNOPT,NEEDTAX      TEST TO COMPUTE VAT TAX                      
         BNO   POSTORDX                                                         
*                                                                               
         GOTO1 =A(GETRATE),DMCB,(RC),RR=RELO                                    
         ZAP   JWTTRATE,PL16       GET VAT RATE (2 DECIMAL PLACES)              
         LA    R1,JWTONET                                                       
*&&UK                                                                           
         CLI   BYTE,POSTOPRI                                                    
         BE    *+8                                                              
         LA    R1,JWTOSNT                                                       
*&&                                                                             
*                                                                               
         ZAP   DUB,0(L'JWTONET,R1)                                              
         AP    DUB,L'JWTONET(L'JWTOCOM,R1)                                      
         ZAP   PL16,JWTTRATE                                                    
         MP    PL16,DUB                                                         
         SRP   PL16,64-4,5                                                      
         ZAP   L'JWTONET+L'JWTOCOM(L'JWTOTAX,R1),PL16                           
*                                                                               
POSTORDX XIT1                                                                   
         DROP  R5,R6                                                            
POSTOPRI EQU   0                                                                
POSTOSEC EQU   1                                                                
         EJECT                                                                  
*&&UK                                                                           
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* SUB-ROUTINE TO PROCESS EURO-ZONE ORDERS                                       
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
EURORD   NTR1  ,                                                                
         BAS   RE,JWPOCLR          INITIALIZE BLOCK AS TABLE ENTRY              
         LA    R5,BLOCK                                                         
         USING JWTABD,R5           BUILD BINSRCH RECORD IN BLOCK                
*                                                                               
* FIRST GET AGENCY PRIMARY CURRENCY OPEN ORDER AMOUNT                           
*                                                                               
EURORD05 DS    0H                                                               
         MVI   ELCODE,OAMELQ                                                    
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING OAMELD,R6                                                        
         MVC   JWTWC,OAMWORK                                                    
         ZAP   DUB,OAMAMNT         GET ORDER AMOUNT                             
         SP    DUB,OAMIVAL         SUBTRACT INVOICED AMOUNT                     
         BP    *+10                (CAN'T HAVE NEGATIVE RESULT)                 
         ZAP   DUB,=P'0'           TO DERIVE OPEN ORDER AMOUNT                  
*                                                                               
* NOW USE TOBACCO TO GET SECONDARY CURRENCY OPEN ORDER AMOUNT                   
*                                                                               
EURORD10 MVC   WORK(L'COMPCUR),COMPCUR                                          
         MVC   WORK+L'COMPCUR(L'COMPCURS),COMPCURS                              
         GOTO1 VTOBACCO,DMCB,('TOBAAOUT',WORK),AIO,ACOMFACS,0,0,0               
         L     R6,AIO                                                           
*                                                                               
         MVI   ELCODE,OAMELQ                                                    
         BAS   RE,GETEL                                                         
         ST    R6,AOAMEL                                                        
         ZAP   OPENAMT,OAMAMNT     GET ORDER AMOUNT                             
         SP    OPENAMT,OAMIVAL     SUBTRACT INVOICED AMOUNT                     
         BP    *+10                                                             
         ZAP   OPENAMT,=P'0'                                                    
*                                                                               
* SET PRIMARY (PRIMAMT) AND SECONDARY (SECAMT) AMOUNTS BASED ON                 
* REQUESTED CURRENCY                                                            
*                                                                               
EURORD20 ZAP   PRIMAMT,DUB                                                      
         ZAP   SECAMT,OPENAMT                                                   
         CLI   SECCURR,C'N'        TEST PRIMARY CURRENCY REQUESTED              
         BE    EURORD30            YES                                          
*                                                                               
         ZAP   PRIMAMT,OPENAMT                                                  
         ZAP   SECAMT,DUB                                                       
*                                                                               
EURORD30 ZAP   OPENAMT,PRIMAMT                                                  
         GOTO1 POSTORD,POSTOPRI                                                 
         ZAP   OPENAMT,SECAMT                                                   
         GOTO1 POSTORD,POSTOSEC                                                 
*                                                                               
EURORDX  XIT1                                                                   
         DROP  R5,R6                                                            
*&&                                                                             
         LTORG                                                                  
         GETEL (R6),DATADISP,ELCODE                                             
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
*        JOB WORKCODE - SAVE SCHEME WORKCODES TO TSAR                           
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
JWPUTSCH NMOD1 JBLOCKL,**SCHE**                                                 
*                                                                               
         LR    R2,RC                                                            
         L     RC,0(R1)                                                         
         LR    RE,R2               CLEAR JOB BLOCK                              
         LA    RF,JBLOCKL                                                       
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         L     RE,ATIA             CLEAR TIA                                    
         A     RE,=A(LENGOOPT)     LESS SPACE RESERVED FOR GETOPT               
         L     RF,=A(LENTIA-LENGOOPT)                                           
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         XC    BLOCK,BLOCK                                                      
*&&US*&& GOTO1 =V(ACJOBCOL),DMCB,FLDH,BLOCK,ACOMFACS,RR=RELO                    
*&&UK*&& GOTO1 =V(ACJOBCOL),DMCB,(X'FF',COLLIST),BLOCK,ACOMFACS,       X        
               RR=RELO                                                          
         USING JBLOCKD,R2                                                       
         L     RF,ATIA             USE FIRST HALF OF TIA FOR COLUMNS            
         A     RF,=A(LENGOOPT)     LESS SPACE RESERVED FOR GETOPT               
         ST    RF,JBACOLTB                                                      
         L     RE,=A(LENJBBUF)                                                  
         SRL   RE,1                DIVIDE LENGTH BY 2                           
         ST    RE,JBLCOLTB                                                      
         ST    RE,JBLOPVTB                                                      
         LA    RF,0(RF,RE)                                                      
         ST    RF,JBAOPVTB         SECOND HALF FOR OPERANDS                     
*                                                                               
*                                                                               
         USING ACTRECD,R3                                                       
         LA    R3,KEY                                                           
*                                                                               
         MVC   KEY,SPACES          BUILD JOB KEY                                
         MVC   ACTKCPY(3),CUL                                                   
         MVC   ACTKACT(12),CPJ                                                  
*                                                                               
         GOTO1 HIGH                                                             
         CLC   ACTKCULA,KEYSAVE                                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
*                                                                               
* CREATE OLD ACC RECORD FOR JOBBER                                              
* CHANGE RECORD LENGTH (SUBTRACT 7)                                             
         L     R3,AIO                                                           
         SR    R1,R1                                                            
         ICM   R1,3,ACTRLEN                                                     
         SHI   R1,7                                                             
         STCM  R1,3,ACTRLEN                                                     
*                                                                               
* MOVE RECORD OVER THE EXTRA 7 BYTES                                            
         LA    R0,ACTRSAF1                                                      
         SHI   R1,45                                                            
         LR    RF,R1                                                            
         LA    RE,ACTRLNK                                                       
         MVCL  R0,RE                                                            
         DROP  R3                                                               
*                                                                               
         MVC   JBAJOB,AIO          JOB IS IN AIO                                
*                                                                               
         LA    RE,BLOCK                                                         
         ST    RE,JBACOLS                                                       
         MVC   JBACOM,ACOMFACS                                                  
         MVC   JBAIO,AIO2                                                       
         L     R1,AGOBLOCK                                                      
         ST    R1,JBAGOBLK                                                      
*                                                                               
         XC    WCTAB,WCTAB                                                      
         LA    R1,WCTAB                                                         
         ST    R1,JBAWCTB          PASS JOBBER A WORKCODE FORMULA TABLE         
         LA    R1,L'WCTAB                                                       
         ST    R1,JBAWCTBL                                                      
*                                                                               
         USING GOBLOCKD,R5                                                      
         L     R5,AGOBLOCK                                                      
         MVC   JBSELSCH,GOSCHEME                                                
         MVC   JBGETOPT,VGETOPT                                                 
*                                                                               
*&&UK                                                                           
         GOTO1 VDATCON,DMCB,(5,0),(1,JBTODAYP)                                  
         L     RE,=V(VATICAN)                                                   
         A     RE,RELO                                                          
         ST    RE,JBVATICN                                                      
*&&                                                                             
         GOTO1 VJOBBER,DMCB,JBLOCK                                              
         CLI   JBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R4,JBACOLTB                                                      
         USING JBCOLD,R4                                                        
         LH    R0,JBNROWS                                                       
*                                                                               
         USING JWTABD,R5           BUILD TSAR RECORD IN BLOCK                   
         LA    R5,BLOCK                                                         
JWPS50   CLI   JBCOLTYP,JBCOLTWC   WORKCODE ROW?                                
         BNE   JWPS60              NO                                           
*                                                                               
         BAS   RE,JWPCRCLR         INITIALIZE RECORD                            
*                                                                               
         MVC   JWTWC,JBCOLWC                                                    
         MVC   JWTSUFF,JBCOLSUF                                                 
         ZAP   JWTCRATE,JBCOLVAL                                                
         OI    JWTSTAT,JWTSRATE    FLAG TABLE ENTRY AS HAVING RATE SET          
*                                                                               
*&&UK                                                                           
         ZAP   JWTTRATE,JBCOLVAL+6 VAT RATE                                     
*&&                                                                             
*                                                                               
         GOTO1 GETPCAT,DMCB,JBCOLD                                              
*                                                                               
         GOTO1 =A(PUTTSAR),DMCB,(RC),BLOCK,RR=RELO                              
*                                                                               
JWPS60   AH    R4,JBLCOL           GET NEXT SCHEME WORKCODE                     
         BCT   R0,JWPS50                                                        
*                                                                               
JWPSX    XIT1                                                                   
*                                                                               
*        CLEAR THE TSAR INPUT RECORD                                            
*                                                                               
         USING JWTABD,R5                                                        
JWPCRCLR NTR1                                                                   
         LA    R5,BLOCK                                                         
         XC    JWTREC(JWTABLN),JWTREC                                           
         LA    R1,JWTFBUCK                                                      
         LA    R0,JWTNBUCK                                                      
JWP40    ZAP   0(BUCKLN,R1),=P'0'                                               
         LA    R1,BUCKLN(R1)                                                    
         BCT   R0,JWP40                                                         
         B     JWPSX                                                            
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* SUB-ROUTINE TO GET THE PERCENT OF CATEGORY PERCENTAGE FOR A WORKCODE          
* AT ENTRY, R2=A(JOBBER BLOCK), R5=A(JOB WORKCODE BUCKETS)                      
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
GETPCAT  NTR1  ,                                                                
         USING JBCOLD,R4                                                        
         USING JWTABD,R5                                                        
         L     R4,0(R1)            GET A(JOBBER COL)                            
         L     RE,JBAWCTB          RE=A(JOBBER WORKCODE TABLE)                  
         USING JBWCTABD,RE                                                      
*                                                                               
GETPCAT2 CLI   JBWCKEY,0           TEST FOR EOT                                 
         BE    GETPCATX            YES                                          
*                                                                               
         CLC   JBCOLCOD,JBWCKEY    MATCH ON WORKCODE/SUFFIX                     
         BE    GETPCAT4            YES                                          
         LA    RE,JBWCTABL(RE)                                                  
         B     GETPCAT2                                                         
*                                                                               
GETPCAT4 L     R3,AGOBLOCK                                                      
         USING GOBLOCKD,R3                                                      
         MVC   GOSELWC,JBWCODE                                                  
         GOTO1 VGETOPT,DMCB,GOBLOCK                                             
         XC    GOSELWC,GOSELWC                                                  
         ZAP   JWTPCAT,GOESTPER                                                 
*                                                                               
GETPCATX B     JWPSX                                                            
         DROP  R3,RE                                                            
*                                                                               
*&&US                                                                           
FLDH     DC    AL1(8+L'FLD),4X'00',AL1(L'FLD),AL2(0)                            
FLD      DC    C'RATE'                                                          
*&&                                                                             
*&&UK                                                                           
COLLIST  DC    AL2(AC#CRATE,AC#VATRJ,0)                                         
*&&                                                                             
*                                                                               
         DROP  R2,R4,R5                                                         
         LTORG                                                                  
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
*        RETURN, IN COMRATE, THE COMMISSION RATE OF WORKCODE                    
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
         USING JWTABD,R5                                                        
GETWCRAT NMOD1 0,**CRTE**                                                       
         L     RC,0(R1)                                                         
         LA    R5,MEDWORK                                                       
         MVC   JWTWC,WORKCODE                                                   
         USING TSARD,R1                                                         
         LA    R1,TSARBLK                                                       
         MVI   TSACTN,TSARDH                                                    
         ST    R5,TSAREC                                                        
         GOTO1 VTSAR                                                            
         TM    TSERRS,TSERNF       NOT FOUND                                    
         BO    GWR50               YES, GETOPT THE RATE.                        
*                                                                               
         TM    JWTSTAT,JWTSRATE    MAKE SURE RATE HAS BEEN SET IN TABLE         
         BNO   GWR50                                                            
*                                                                               
         ZAP   COMRATE,JWTCRATE    RETURN RATE IN COMRATE                       
         B     GWRX                                                             
*                                                                               
GWR50    L     R4,AGOBLOCK         GET RATE FROM GETOPT                         
         USING GOBLOCKD,R4         WORKCODE NOT FOUND IN TSAR                   
         MVC   GOSELWC,WORKCODE                                                 
         GOTO1 VGETOPT,DMCB,GOBLOCK                                             
         XC    GOSELWC,GOSELWC                                                  
         ZAP   COMRATE,GOAGYCOM                                                 
*                                                                               
GWRX     XIT1                                                                   
         LTORG                                                                  
         DROP  R1,R4,R5                                                         
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
*        ADD AN ITEM TO TSAR                                                    
*        P2 IS ITEM,                                                            
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
         USING JWTABD,R5                                                        
PUTTSAR  NMOD1  JWTABLN,**PTSA**                                                
         LR    R4,RC               SPACE TO READ HI INTO                        
         L     RC,0(R1)                                                         
         L     R5,4(R1)            A(RECORD TO PUT TO TSAR)                     
         USING TSARD,R1                                                         
         LA    R1,TSARBLK                                                       
         MVI   TSACTN,TSARDH                                                    
         MVC   0(JWTKEYLN,R4),0(R5)  SET KEY FOR READ HIGH                      
         ST    R4,TSAREC                                                        
         GOTO1 VTSAR                                                            
         TM    TSERRS,TSERNF       WAS RECORD FOUND?                            
         BO    PTT70               NO                                           
*                                                                               
         LA    R1,JWTNADDS         ADD ITEM BUCKETS TO TSAR BUCKETS             
         LA    R3,JWTBUCKS-JWTABD(R5)  A(I/P RECORD BUCKETS)                    
         LA    R2,JWTBUCKS-JWTABD(R4)  A(TSAR BUCKETS)                          
*                                                                               
PTT50    AP    0(BUCKLN,R2),0(BUCKLN,R3)                                        
         LA    R2,BUCKLN(R2)                                                    
         LA    R3,BUCKLN(R3)                                                    
         BCT   R1,PTT50                                                         
*                                                                               
         USING TSARD,R1                                                         
         LA    R1,TSARBLK                                                       
         MVI   TSACTN,TSAWRT       WRITE RECORD BACK TO TSAR                    
         B     PTT90                                                            
*                                                                               
PTT70    LA    R1,TSARBLK                                                       
         MVI   TSACTN,TSAADD       ADD THE INPUT RECORD TO TSAR                 
         ST    R5,TSAREC                                                        
*                                                                               
PTT90    GOTO1 VTSAR                                                            
         CLI   TSERRS,0            SET CONDITION CODE                           
         BE    *+6                                                              
         DC    H'0'                                                             
PTTX     XIT1                                                                   
         DROP  R1,R5                                                            
         LTORG                                                                  
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
*        ADD AN ITEM TO TSAR BUT USE DIFFERENT DSECT                            
*        P2 IS ITEM,                                                            
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
         USING JWCABD,R5                                                        
PUTTSA2  NMOD1 JWCABLN,**PTS2**                                                 
         LR    R4,RC               SPACE TO READ HI INTO                        
         L     RC,0(R1)                                                         
         L     R5,4(R1)            A(RECORD TO PUT TO TSAR)                     
         USING TSARD,R1                                                         
         LA    R1,TSARBLK                                                       
         MVI   TSACTN,TSARDH                                                    
         MVC   0(JWCKEYLN,R4),0(R5)  SET KEY FOR READ HIGH                      
         ST    R4,TSAREC                                                        
         GOTO1 VTSAR                                                            
         TM    TSERRS,TSERNF       WAS RECORD FOUND?                            
         BO    PUTS4               NO                                           
*                                                                               
         LA    R1,JWCNADDS         ADD ITEM BUCKETS TO TSAR BUCKETS             
         LA    R3,JWCNBKT-JWCABD(R5)   A(I/P RECORD BUCKETS)                    
         LA    R2,JWCNBKT-JWCABD(R4)   A(TSAR BUCKETS)                          
*                                                                               
PUTS2    AP    0(BUCKLN,R2),0(BUCKLN,R3)                                        
         LA    R2,BUCKLN(R2)                                                    
         LA    R3,BUCKLN(R3)                                                    
         BCT   R1,PUTS2                                                         
*                                                                               
         USING TSARD,R1                                                         
         LA    R1,TSARBLK                                                       
         MVI   TSACTN,TSAWRT       WRITE RECORD BACK TO TSAR                    
         B     PUTS6                                                            
*                                                                               
PUTS4    LA    R1,TSARBLK                                                       
         MVI   TSACTN,TSAADD       ADD THE INPUT RECORD TO TSAR                 
         ST    R5,TSAREC                                                        
*                                                                               
PUTS6    GOTO1 VTSAR                                                            
         CLI   TSERRS,0            SET CONDITION CODE                           
         BE    *+6                                                              
         DC    H'0'                                                             
PUTSX    XIT1                                                                   
         DROP  R1,R5                                                            
         LTORG                                                                  
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
*        ESTABLISH VAT RATE OR GET FROM TABLE                                   
*        RETURN RATE IN PL16                                                    
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
         USING VTCD,R2                                                          
         USING TAXTABD,R4                                                       
GETRATE  NMOD1 0,**RATE**                                                       
         L     RC,0(R1)                                                         
         ZAP   PL16,=P'0'                                                       
         L     R2,AVATBUFF         LOOK FOR TAXRATE IN TAXTAB                   
         XC    VTCD(VTCLNQ),VTCD                                                
         L     R4,ATAXTAB                                                       
         LA    R5,TAXMAX                                                        
         USING GOBLOCKD,R3                                                      
         L     R3,AGOBLOCK                                                      
         OC    GOTAXCOD,GOTAXCOD                                                
         BZ    GETRX               NO TAXCODE, NO RATE                          
*                                                                               
GETR02   CLI   TTTAXCOD,0          NOTHING IN TABLE OR END                      
         BE    GETR04              CALL VATICAN FOR THE RATE                    
*                                                                               
         CLC   TTTAXCOD,GOTAXCOD   DOES CODE MATCH ?                            
         BNE   GETR03              NO, TRY NEXT                                 
         ZAP   PL16,TTTAXRTE                                                    
         B     GETRX                                                            
*                                                                               
GETR03   LA    R4,TAXTABLN(R4)                                                  
         BCT   R5,GETR02                                                        
         DC    H'0'                TOO MANY RATES                               
*                                                                               
GETR04   MVC   VTCTYPE,GOTAXCOD    PASS VAT TYPE CODE                           
         MVI   VTCACTN,VTCALOOK                                                 
         MVC   VTCCPY,CUL                                                       
         MVC   VTCCOMF,ACOMFACS                                                 
         MVC   VTCOFFC,GOEFFOFC                                                 
         GOTO1 VDATCON,DMCB,(5,0),(1,VTCINVD) AS OF TODAY                       
         GOTO1 =V(VATICAN),(R2),RR=RELO                                         
*                                                                               
         TM    VTCINDS,VTCINA      IS TAX APPLICABLE ?                          
         BO    GETRX               NO                                           
         CLI   VTCERR,X'00'        YES, FIND TYPE ?                             
         BNE   GETRX               NO                                           
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,VTCRATE                                                     
         CVD   RE,DUB                                                           
         ZAP   TTTAXRTE,DUB        UPDATE TAXTAB                                
         MVC   TTTAXCOD,GOTAXCOD                                                
*                                                                               
         ZAP   PL16,DUB            RETURN RATE                                  
*                                                                               
GETRX    XIT1                                                                   
         LTORG                                                                  
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
*        CONVERT THE MAINFRAME DATE/TIMES IN MONTIME/DATE TO CL6                
*        PC STYLE VALUES ADDRESSED BY P1 AND P2                                 
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
         USING DTTMD,R6                                                         
MAIN2PC  NMOD1 DTTMLN,**M2PC**                                                  
         LR    R6,RC                                                            
         L     RC,0(R1)                                                         
         L     R2,4(R1)            A(WHERE TO WRITE THE DATE)                   
         L     R3,8(R1)            A(WHERE TO WRITE THE TIME)                   
         GOTO1 VDATCON,DMCB,(2,MONDATE),(X'20',DTTMDATE)                        
*                                                                               
         XR    R5,R5                                                            
         ICM   R5,7,MONTIME                                                     
*&&US*&& AH    R5,=Y(EIGHTHRS)     ADD 8 HOURS IN BINARY SCEONDS                
*&&UK*&& AH    R5,=H'0'            NO ADJUSTMENT NEEDED IN UK                   
         C     R5,=A(TWO4HRS)      MORE THAN 24                                 
         BNH   M2P50                                                            
         S     R5,=A(TWO4HRS)                                                   
*                                                                               
         XR    RF,RF               SUBTRACT 1 FROM DATE                         
         BCTR  RF,0                                                             
         ST    RF,DMCB+8                                                        
         L     RF,ACOMFACS                                                      
         L     RF,CADDAY-COMFACSD(RF)                                           
         GOTO1 (RF),DMCB,DTTMDATE,(X'20',WORK)                                  
         MVC   DTTMDATE,WORK                                                    
*                                                                               
M2P50    XR    R4,R4               R4 IS E OF E/O PAIR                          
         D     R4,=F'3600'         SECONDS/HOUR                                 
         CVD   R5,DUB                                                           
         EDIT  (P8,DUB),(2,DTTMHRS),FILL=0                                      
         LR    R5,R4               DIVIDE REMAINDER BY MINUTES                  
         XR    R4,R4                                                            
         D     R4,=F'60'           SECONDS/MIN                                  
         CVD   R5,DUB                                                           
         EDIT  (P8,DUB),(2,DTTMMIN),FILL=0                                      
         CVD   R4,DUB                      REMAINDER IS SECONDS                 
         EDIT  (P8,DUB),(2,DTTMSEC),FILL=0                                      
         MVC   0(L'DTTMDATE,R2),DTTMDATE   RETURN DATE/TIME                     
         MVC   0(L'DTTMTIME,R3),DTTMTIME                                        
         XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
SETTSAR  NMOD1 0,**ITSR**                                                       
         L     RC,0(R1)                                                         
         GOTO1 VCALLOV,DMCB,0,X'D9000A5D'                                       
         CLI   4(R1),X'FF'         CAN'T GET TSAR                               
         BNE   *+6                 GOT OT                                       
         DC    H'0'                DIE IF TSAR UNAVAILABLE                      
         MVC   VTSAR,0(R1)                                                      
*                                                                               
         USING TSARD,R1                                                         
         LA    R1,TSARBLK                                                       
         XC    0(TSARDL,R1),0(R1)                                               
         MVI   TSACTN,TSAINI                                                    
         OI    TSINDS,TSINODSK                                                  
         MVI   TSKEYL,JWTKEYLN                                                  
         MVC   TSRECL,=Y(JWTABLN)                                               
         MVI   TSPAGN,TSNMAX       REQUEST MAX N' DISK PAGES                    
         MVC   TSACOM,ACOMFACS                                                  
         GOTO1 VTSAR                                                            
         XMOD1                                                                  
*                                                                               
       ++INCLUDE ACPRFWRK                                                       
WORKD    DSECT                                                                  
*                                                                               
* OVERLAY WORKING STORAGE                                                       
         ORG   BIGWORK                                                          
RELO     DS    A                   RELOCATION CONSTANT                          
MAPCODE  DS    X                   MAPCODE FOR DATA ITEM SENDING                
BILFLAG  DS    X                                                                
BLALCTD  EQU   X'80'               BILL WAS ALLOCATED TO A WORKCODE             
BLUSDAT  EQU   X'20'               BILL HAS A USED DATE                         
LENGTH   DS    X                   LENGTH OF DATA TO DOWNLOAD                   
CODE     DS    C                   Y = DATE IS A CODE                           
DELMTER  DS    C                   CHARACTER TO DELIMIT DOWNLOAD                
MONDATE  DS    XL2                 MONITOR REQUESTED FILTER BY DATE             
MONTIME  DS    XL3                 MONITOR REQUESTED FILTER BY TIME             
*                                                                               
ALCTNET  DS    PL6                 ALLOCATED AMOUNT                             
ALCTCOM  DS    PL6                 ALLOCATED COMMISION                          
*                                                                               
JOBCUR   DS    CL3                 JOB BILLING CURRENCY                         
SECCURR  DS    CL1                 SECONDARY CURRENCY FOR JOB (Y/N)             
*                                                                               
AGOBLOCK DS    A                                                                
ATAXTAB  DS    A                                                                
AVATBUFF DS    A                   VATICAN INTERFACE BLOCK                      
ATRNEL   DS    A                   A(TRANSACTION ELEMENT)                       
AOAMEL   DS    A                   A(ORDER AMOUNT ELEMENT)                      
*                                                                               
*                                                                               
*                                                                               
PL16     DS    PL16                                                             
RUNOPT   DS    CL1                                                              
NEEDTAX  EQU   1                                                                
XJOB     DS    CL1                                                              
COMRATE  DS    PL6                                                              
WORKCODE DS    CL2                                                              
TAMT     DS    PL8                                                              
PRIMAMT  DS    PL8                                                              
SECAMT   DS    PL8                                                              
OPENAMT  DS    PL8                                                              
*                                                                               
WCTAB    DS    XL(10*JBWCTABL)       JOBBER WORKCODE TABLE                      
*                                                                               
BLOCK2   DS    CL(L'BLOCK)        BUILD SECOND TSAR RECORD HERE                 
*                                                                               
SVTSRNUM DS    CL2                                                              
MYKEYSV  DS    CL(L'KEY)                                                        
*                                                                               
GOBLK    DS    XL(L'GOBLOCK)                                                    
GOBBLK   DS    XL(GOBBLKX-GOBBLOCK)                                             
*                                                                               
ALBILLS  DS    50CL(ALBILLQ)       TABLE OF ALLOCATED BILL #'S                  
         DC    X'FF'               SEE ALBILLD                                  
         EJECT                                                                  
*                                                                               
ALBILLD  DSECT                     TABLE OF ALLOCATED BILLS                     
ALBILNO  DS    CL6                                                              
ALBILNET DS    PL6                 NET BILLED ALLOCATED                         
ALBILCOM DS    PL6                 ALLOCATED COMMISSION OF BILL                 
ALBILLQ  EQU   *-ALBILNO                                                        
*                                                                               
*        WORKING STORAGE FOR SETDTTM AND VALDTTM                                
*                                                                               
DTTMD    DSECT                                                                  
DTTMDATE DS    CL6                                                              
DTTMTIME DS    0CL6                                                             
DTTMHRS  DS    CL2                                                              
DTTMMIN  DS    CL2                                                              
DTTMSEC  DS    CL2                                                              
*                                                                               
DTPDATE  DS    XL2                                                              
DTBSECS  DS    XL3                                                              
DTTMLN   EQU   *-DTTMD                                                          
         EJECT                                                                  
*                                                                               
*        JOB WORKCODE DOWNLOAD BINSRCH TABLE                                    
*                                                                               
JWTABD   DSECT                                                                  
JWTREC   DS    0C                                                               
JWTWC    DS    CL2                                                              
JWTSUFF  DS    XL1                 WORKCODE SUFFIX (SUBWORKCODES)               
         DS    CL15                USE MAX KEY SIZE                             
JWTKEYLN EQU   *-JWTABD                                                         
JWTSTAT  DS    XL1                                                              
JWTSRATE EQU   1                   COMMISSION RATE HAS BEEN SET                 
JWTFBUCK DS    0C                                                               
JWTCRATE DS    PL6                 COMMISSION RATE                              
*                                                                               
JWTTRATE DS    PL6                 TAX RATE                                     
JWTPCAT  DS    PL6                 PERCENT OF CATEGORY                          
*                                                                               
JWTBUCKS DS    0C                                                               
JWTNET   DS    PL6                 NET                                          
JWTNCN   DS    PL6                 NON COMMISSIONABLE NET                       
JWTCOM   DS    PL6                 COMMISSION                                   
JWTTAX   DS    PL6                 TAX                                          
*                                                                               
JWTONET  DS    PL6                 OPEN ORDER NET AMOUNT                        
JWTOCOM  DS    PL6                 OPEN ORDER COMMISSION AMOUNT                 
JWTOTAX  DS    PL6                 OPEN ORDER TAX AMOUNT                        
*                                                                               
JWTSBUCK DS    0C                  SECONDARY CURRENCY BUCKETS                   
JWTSNET  DS    PL6                 SECONDARY NET                                
JWTSNCN  DS    PL6                 SECONDARY NON-COMMISSIONABLE                 
JWTSCOM  DS    PL6                 SECONDARY COMMISSION                         
JWTSTAX  DS    PL6                 SECONDARY TAX                                
*                                                                               
JWTOSNT  DS    PL6                 SECONDARY OPEN ORDER NET                     
JWTOSCM  DS    PL6                 SECONDARY OPEN ORDER COMM                    
JWTOSTX  DS    PL6                 SECONDARY OPEN ORDER TAX                     
*                                                                               
JWTTBUCK DS    0C                  TIME BUCKETS                                 
JWTBHR   DS    PL6                 'B' TIME                                     
JWTNHR   DS    PL6                 'N' TIME                                     
JWTRHR   DS    PL6                 'R' TIME                                     
JWTRMEM  DS    PL6                 'R' HOURS VALUE (MEMO ITEM)                  
JWTRSME  DS    PL6                 'R' HOURS SECOND CURR VALUE                  
*                                                                               
* BILLING BUCKETS                                                               
JWTBNET  DS    PL6                 NET BILLED AMOUNT                            
JWTBCOMM DS    PL6                 BILLED COMMISSION                            
JWTBTAX  DS    PL6                 BILLED TAX                                   
JWTBHOUR DS    PL6                 BILLED HOURS                                 
*                                                                               
JWTABLN  EQU   *-JWTABD                                                         
JWTNBUCK EQU   (*-JWTFBUCK)/6                                                   
JWTNADDS EQU   (*-JWTBUCKS)/6      NUMBER OF BUCKETS TO ACCUMULATE              
BUCKLN   EQU   6                                                                
*                                                                               
         EJECT                                                                  
*        JOB WORKCODE-CONTRA DOWNLOAD BINSRCH TABLE                             
*                                                                               
JWCABD   DSECT                                                                  
JWCREC   DS    0C                                                               
JWCTYP   DS    CL1                 RECORD TYPE 0                                
JWCWC    DS    CL2                                                              
JWCSUFF  DS    XL1                 WORKCODE SUFFIX (SUBWORKCODES)               
JWCCONTR DS    CL14                CONTRA ACCOUNT                               
JWCKEYLN EQU   *-JWCABD                                                         
*                                                                               
JWCNBKT  DS    0C                                                               
JWCNET   DS    PL6                 NET                                          
JWCNCN   DS    PL6                 NON COMMISSIONABLE NET                       
JWCCOM   DS    PL6                 COMMISSION                                   
JWCTAX   DS    PL6                 TAX                                          
JWCNBKTL EQU   *-JWCNBKT                                                        
*                                                                               
JWCSBKT  DS    0C                  SECONDARY CURRENCY BUCKETS                   
JWCSNET  DS    PL6                 SECONDARY NET                                
JWCSNCN  DS    PL6                 SECONDARY NON-COMMISSIONABLE                 
JWCSCOM  DS    PL6                 SECONDARY COMMISSION                         
JWCSTAX  DS    PL6                 SECONDARY TAX                                
JWCSBKTL EQU   *-JWCSBKT                                                        
*                                                                               
JWCTBKT  DS    0C                  TIME BUCKETS                                 
JWCBHR   DS    PL6                 'B' TIME                                     
JWCNHR   DS    PL6                 'N' TIME                                     
JWCRHR   DS    PL6                 'R' TIME                                     
JWCRMEM  DS    PL6                 'R' HOURS VALUE (MEMO ITEM)                  
JWCRSME  DS    PL6                 'R' HOURS SECOND CURR VALUE                  
JWCTBKTL EQU   *-JWCTBKT                                                        
JWCBLN   EQU   *-JWCNBKT        LENGTH OF ALL BUCKETS                           
JWCNADDS EQU   (*-JWCNBKT)/6    NUMBER OF BUCKETS TO ACCUMULATE                 
JWCTADDS EQU   (*-JWCTBKT)/6    NUMBER OF TIME BUCKETS TO ACCUMULATE            
         DS    CL(JWTABLN-(*-JWCABD))   MAKE SAME SIZE AS JWTABD                
JWCABLN  EQU   *-JWCABD         LENGTH OF RECORD                                
         EJECT                                                                  
BUCKD    DSECT                                                                  
BUCKNET  DS    PL6                 NET                                          
BUCKNCOM DS    PL6                 NON COMMISSIONABLE NET                       
BUCKCOM  DS    PL6                 COMMISSION                                   
BUCKTAX  DS    PL6                 TAX                                          
*                                                                               
TAXTABD  DSECT                     TABLE OF TAX RATES                           
TTTAXCOD DS    CL1                                                              
TTTAXRTE DS    PL6                                                              
TAXTABLN EQU   *-TAXTABD                                                        
TAXTABSZ EQU   TAXTABLN*TAXMAX                                                  
TAXMAX   EQU   7                                                                
         EJECT                                                                  
* CTMADEQUS                                                                     
       ++INCLUDE CTMADEQUS                                                      
         EJECT                                                                  
* CTMADDSECT                                                                    
       ++INCLUDE CTMADDSECT                                                     
         EJECT                                                                  
* DDTSARD                                                                       
       ++INCLUDE DDTSARD                                                        
         EJECT                                                                  
* ACGENFILE                                                                     
* FAUTL                                                                         
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE FAUTL                                                          
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
* ACJOBBLOCK                                                                    
JBLOCKD  DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE ACJOBBLOCK                                                     
         PRINT ON                                                               
* ACGOBLOCKD ACGOBBLOCK                                                         
GOBLOCKD DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE ACGOBLOCK                                                      
       ++INCLUDE ACGOBBLOCK                                                     
         PRINT ON                                                               
* ACJOBERD                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACJOBBERD                                                      
         PRINT ON                                                               
* ACVATICAND                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACVATICAND                                                     
         PRINT ON                                                               
* DDCTRYEQUS                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDCTRYEQUS                                                     
         PRINT ON                                                               
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
* ACTOBACCOD (UK ONLY)                                                          
         PRINT OFF                                                              
*&&UK                                                                           
       ++INCLUDE ACTOBACCOD                                                     
*&&                                                                             
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'018ACPRF02   12/27/12'                                      
         END                                                                    
