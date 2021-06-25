*          DATA SET TAGEN32    AT LEVEL 017 AS OF 03/14/13                      
*PHASE T70232C                                                                  
         TITLE 'T70232 - AGENCY LIST'                                           
T70232   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70232                                                         
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         LA    R7,TWAHOLE          R7=TWAHOLE                                   
         USING WORKD,R7                                                         
         EJECT                                                                  
* MODE CONTROLLED ROUTINES                                                      
*                                                                               
         GOTO1 INITIAL,DMCB,PFTAB                                               
*                                                                               
         CLI   MODE,VALKEY         FIRST TIME IN                                
         BE    VK                                                               
*                                                                               
         CLI   MODE,LISTRECS       IF MODE LISTRECS                             
         BNE   AGY10                                                            
         LA    R2,LISTAR           POINT R2 TO LISTAR                           
         B     LR                  GO TO LR                                     
*                                                                               
AGY10    CLI   MODE,PRINTREP       ELSE IF MODE PRINTREP                        
         BNE   AGYX                                                             
         LA    RF,MYSPECS          SET REPORT SPECS                             
         ST    RF,SPECS                                                         
         ZAP   PRCOUNT,=P'0'       CLEAR RECORD COUNT                           
         LA    R2,P                POINT R2 TO PRINT LINE                       
         B     LR                  AND GO TO LR                                 
*                                                                               
AGYX     B     XIT                                                              
*                                                                               
         SPACE 2                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
* VALIDATE THE KEY                                                              
*                                                                               
VK       DS    0H                                                               
*                                                                               
         LA    R2,SALSTRH          VALIDATE START AT KEY                        
         TM    4(R2),X'20'                                                      
         BO    VK20                                                             
         NI    SALAGGH+4,X'DF'                                                  
*                                                                               
         XC    STARTAT,STARTAT     SET TO ZEROS IF NOTHING ENTERED              
         CLI   5(R2),0                                                          
         BE    VK10                                                             
*                                                                               
         ZIC   R3,5(R2)            ELSE EXTRACT KEY FROM FIELD                  
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   STARTAT(0),8(R2)                                                 
         OC    STARTAT,SPACES      PAD WITH SPACES                              
*                                                                               
VK10     OI    4(R2),X'20'                                                      
*                                                                               
VK20     LA    R2,SALAGGH          VALIDATE AGENCY GROUP                        
         TM    4(R2),X'20'                                                      
         BO    VK40                                                             
         NI    SALFMTH+4,X'DF'                                                  
*                                                                               
         XC    GROUP,GROUP         SET TO ZEROS IF NOTHING ENTERED              
         CLI   5(R2),0                                                          
         BE    VK30                                                             
*                                                                               
         MVC   GROUP,8(R2)         ELSE EXTRACT GROUP FROM FIELD                
         OC    GROUP,SPACES        PAD WITH SPACES                              
*                                                                               
VK30     OI    4(R2),X'20'                                                      
*                                                                               
VK40     LA    R2,SALFMTH          VALIDATE FORMAT                              
         TM    4(R2),X'20'                                                      
         BO    VK70                                                             
         NI    SALOPTSH+4,X'DF'                                                 
*                                                                               
         MVI   RDSEQ,C'A'          DEFAULT TO ALPHA SEQUENCE                    
         OC    GROUP,GROUP         UNLESS GROUP SPECIFIED                       
         BZ    *+8                                                              
         MVI   RDSEQ,C'C'          IN WHICH CASE DEFAULT TO CODE                
         CLI   5(R2),0                                                          
         BE    VK60                                                             
*                                                                               
         CLI   5(R2),1             ONLY 1 CHAR CAN BE INPUT                     
         BNE   INVERR                                                           
*                                                                               
         CLI   8(R2),C'A'          IF 'A' INPUT                                 
         BNE   VK50                                                             
         OC    GROUP,GROUP         THEN MUST NOT HAVE GROUP FILTER              
         BNZ   INVERR                                                           
         B     VK60                                                             
*                                                                               
VK50     CLI   8(R2),C'C'          ELSE IF 'C' INPUT                            
         BNE   INVERR                                                           
         MVI   RDSEQ,C'C'          SET TO READ IN CODE SEQUENCE                 
*                                                                               
VK60     OI    4(R2),X'20'                                                      
*                                                                               
VK70     LA    R2,SALOPTSH         VALIDATE OPTIONS                             
         TM    4(R2),X'20'                                                      
         BO    VKX                                                              
         BAS   RE,VALOPTS                                                       
*                                                                               
         BAS   RE,SETSYS           SET UP SYSIO BLOCK                           
*                                                                               
VKX      B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE OPTIONS                                      
*                                                                               
*                                  R2=A(FIELD)                                  
VALOPTS  NTR1                                                                   
         MVI   OPTS,0              CLEAR INTERNAL BYTE                          
         XC    OPTBTYPE,OPTBTYPE                                                
         MVI   OPTOFF,0                                                         
         MVI   OPTSRV,0                                                         
         MVI   OPTPUR,0                                                         
*                                                                               
         CLI   5(R2),0             GET OUT IF NOTHING INPUT                     
         BE    VOPTX                                                            
*                                                                               
         LA    R3,BLOCK            R3=A(SCAN BLOCK)                             
         USING SCAND,R3                                                         
         GOTO1 SCANNER,DMCB,(R2),(X'80',(R3))                                   
         CLI   4(R1),0                                                          
         BE    INVERR                                                           
         ZIC   R0,4(R1)            R0=N'SCAN BLOCK ENTRIES                      
*                                                                               
VOPT4    DS    0H                                                               
         MVC   ERRDISP,SCDISP1     SET DISP. INTO FLD OF LHS FOR ERRORS         
*                                                                               
         ZIC   RF,SCLEN1           RF=L'LHS                                     
         LTR   RF,RF                                                            
         BZ    INVERR                                                           
         BCTR  RF,0                RF=L'LHS-1                                   
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   SCDATA1(0),=C'LOCKED'                                            
         BNE   VOPT5                                                            
*                                                                               
         MVC   ERRDISP,SCDISP2     SET DISP. INTO FLD OF RHS FOR ERRORS         
         CLI   SCDATA2,C'Y'                                                     
         BNE   *+12                                                             
         OI    OPTS,OPTLOCK        SET LIST LOCKED AGENCIES                     
         B     VOPT8                                                            
*                                                                               
VOPT5    EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   SCDATA1(0),=C'BTYPE'                                             
         BNE   VOPT6                                                            
*                                                                               
         MVC   ERRDISP,SCDISP2                                                  
         TM    SCVAL2,X'80'                                                     
         BZ    INVERR                                                           
         GOTO1 BTYPVAL,DMCB,SCBIN2+3                                            
         BNE   INVERR                                                           
         MVC   OPTBTYPE,SCDATA2                                                 
         CLI   OPTBTYPE+1,C' '                                                  
         BNE   VOPT8                                                            
         MVC   OPTBTYPE+1(1),OPTBTYPE                                           
         MVI   OPTBTYPE,C' '                                                    
         B     VOPT8                                                            
*                                                                               
VOPT6    EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   SCDATA1(0),=C'OFFICE'                                            
         BNE   VOPT7                                                            
*                                                                               
         MVC   ERRDISP,SCDISP2                                                  
         CLI   SCLEN2,1                                                         
         BNE   INVERR                                                           
         GOTO1 RECVAL,DMCB,TLOFCDQ,(X'84',SCDATA2)                              
         BNE   INVERR                                                           
         MVC   OPTOFF,SCDATA2                                                   
         B     VOPT8                                                            
*                                                                               
VOPT7    EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   SCDATA1(0),=C'SERVICE'   FILTER BY SERVICE TYPE                  
         BNE   VOPTSRVN                                                         
*                                                                               
         MVC   ERRDISP,SCDISP2                                                  
         CLI   SCLEN2,3                                                         
         BNE   INVERR                                                           
         MVI   OPTSRV,C'R'                                                      
         CLC   =C'PRE',SCDATA2     PREMIUM                                      
         BE    VOPT8                                                            
         CLC   =C'PAY',SCDATA2     PAYROLL                                      
         BNE   INVERR                                                           
         MVI   OPTSRV,C'P'                                                      
         B     VOPTPURX                                                         
*                                                                               
VOPTSRVN DS    0H                                                               
*                                                                               
VOPTPUR  DS    0H                                                               
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   SCDATA1(0),=C'PURCHASE'   FILTER BY PURCHASE TYPE                
         BNE   VOPTPURN                                                         
*                                                                               
         MVC   ERRDISP,SCDISP2                                                  
*                                                                               
         CLI   SCLEN2,3                                                         
         BH    INVERR                                                           
*                                                                               
         CLI   SCLEN2,0                                                         
         BE    INVERR                                                           
*                                                                               
         LLC   RF,SCLEN2           GET OPTION LENGTH                            
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   SCDATA2(0),=C'YES'  CHECK FOR YES                                
         BNE   *+12                                                             
         MVI   OPTPUR,C'Y'                                                      
         B     VOPTPURX                                                         
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   SCDATA2(0),=C'NO '  CHECK FOR NO                                 
         BNE   *+12                                                             
         MVI   OPTPUR,C'N'                                                      
         B     VOPTPURX                                                         
*                                                                               
         B     INVERR                                                           
*                                                                               
VOPTPURX DS    0H                                                               
         B     VOPT8                                                            
*                                                                               
VOPTPURN DS    0H                                                               
*                                                                               
         B     INVERR                                                           
*                                                                               
VOPT8    LA    R3,SCANNEXT         BUMP TO NEXT                                 
         BCT   R0,VOPT4            AND CONTINUE                                 
*                                                                               
VOPTX    OI    4(R2),X'20'                                                      
         B     XIT                                                              
         EJECT                                                                  
* SET UP SYSIO DSECT WITH NECESSARY INFORMATION                                 
*                                                                               
SETSYS   NTR1                                                                   
*                                                                               
         MVC   TIUSERID,TWAORIG    SET UP NECCESSARY DATA FROM                  
         MVC   TIQSTAFF,TGCTSTAF       GLOBAL STORAGE                           
*                                                                               
         CLI   RDSEQ,C'A'          IF ALPHA SEQUENCE REQUESTED                  
         BNE   SS10                                                             
         MVI   TIREAD,TLAYNCDQ     LIST BY STAFF NAME                           
         B     SS20                                                             
*                                                                               
SS10     MVI   TIREAD,TLAYCDQ      ELSE LIST BY STAFF CODE                      
*                                                                               
SS20     XC    TIQFLAGS,TIQFLAGS   CLEAR REQUESTED FLAGS                        
*        OI    TIQFLAG2,TIQFNLIM                                                
*                                                                               
         MVC   TIQSTART,STARTAT    SET START KEY                                
         MVC   TIFAGG,GROUP        SET AGENCY GROUP FILTER                      
*                                                                               
SSX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
* LIST THE RECORDS                                                              
*                                                                               
LR       LA    R0,LRHOOK           SET HOOK TO SYSIO                            
         ST    R0,TIHOOK                                                        
         MVC   TIKHOOK,SETLSTK     SET A(CONTINUE ROUTINE)                      
         MVC   TIQSKEY,KEY         SET CONTINUE KEY                             
         MVC   TIACOMFC,ACOMFACS   SET A(COMFACS)                               
*                                                                               
         MVI   NLISTS,16           CALL SYSIO                                   
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
         MVI   NLISTS,15                                                        
*                                                                               
         CLI   MODE,PRINTREP       IF MODE IS PRINT REPORT                      
         BNE   LRX                 THEN PRINT NUMBER OF AGENCIES                
         MVC   P(20),=C'NUMBER OF AGENCIES :'                                   
         EDIT  PRCOUNT,(7,P+21),ALIGN=LEFT,ZERO=NOBLANK                         
         GOTO1 CATCHIOS            LIMIT IOS                                    
         GOTO1 SPOOL,DMCB,(R8)     CALL SPOOL                                   
*                                                                               
         XC    CONSERV,CONSERV     AUTO CALL $DQU                               
         MVC   CONSERV(4),=C'$DQU'                                              
         OI    CONSERVH+6,X'80'                                                 
*                                                                               
LRX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        PROCESS SYSIO RECORDS                                                  
*                                                                               
LRHOOK   NTR1                                                                   
         CLI   TIMODE,PROCREC      PROCESS RECORDS                              
         BE    PRREC                                                            
*                                                                               
LRHX     B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        PROCESS RECORD                                                         
*                                                                               
PRREC    DS    0H                                                               
         CLC   TIAGY,=C'999999'    RETURN IF AGENCY '999999'                    
         BNE   PR2                                                              
         CLI   TGCTSTTY,TASTTYPP   AND NOT PROGRAMMER                           
         BNE   PRRX                                                             
*&&DO                                                                           
         LHI   R3,1                                                             
*                                                                               
         USING FAWSSVRD,R1                                                      
PR1B     LA    R1,LIMBLK                                                        
         XC    LIMBLK,LIMBLK                                                    
         MVC   FAWSTOKN(3),=C'STF'                                              
         STC   R3,FAWSTOKN+3                                                    
         MVI   FAWSACTN,FAWSARST   RECALL STAFF2 INFORMATION VIA WWSVR          
         XC    FAWSLEN,FAWSLEN                                                  
         MVC   FAWSADR,TGAS2ACC                                                 
         GOTO1 WSSVR,(R1)                                                       
         CLI   FAWSRTN,0           IF NOT FOUND, STAFF HAS NO ACCESS            
         BNE   PRRX                                                             
         DROP  R1                                                               
*                                                                               
         AHI   R3,1                                                             
*                                                                               
         USING TAVAD,R1                                                         
         L     R1,TGAS2ACC                                                      
         OC    TAVAAGY,TAVAAGY     IF STAFF HAS NO AGENCY LIMITS,               
         BZ    PR2                 STAFF HAS ACCESS TO ALL RECORDS              
*                                                                               
PR1C     CLI   0(R1),0             RECALL NEXT RECORD FROM WSSVR                
         BE    PR1B                                                             
*                                                                               
         CLC   TIAGY,TAVAAGY       IF AGENCY IS FOUND IN STAFF LIMITS           
         BE    PR2                                                              
         ZIC   RE,TAVALEN          BUMP TO NEXT VALID AGENCY/CLIENT             
         AR    R1,RE               ELEMENT                                      
         B     PR1C                                                             
*&&                                                                             
PR2      MVC   LISTAR,SPACES       CLEAR PREVIOUS LIST LINE                     
         USING LISTD,R2                                                         
*                                                                               
         L     R4,TIAREC           R4 =A(AGENCY ELEMENT)                        
         MVI   ELCODE,TAAYELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   PR5                 SKIP IF NOT FOUND                            
         USING TAAYD,R4                                                         
*                                                                               
         CLI   OPTOFF,0                                                         
         BE    PR3                                                              
         CLC   TAAYTPOF,OPTOFF                                                  
         BNE   PRRX                                                             
*                                                                               
PR3      TM    OPTS,OPTLOCK        IF DIDN'T REQUEST LOCKED AGENCIES            
         BO    *+16                                                             
         TM    TAAYSTA3,TAAYSLCK   THEN EXCLUDE THEM                            
         BO    PRRX                                                             
         B     *+12                                                             
         TM    TAAYSTA3,TAAYSLCK   ELSE INCLUDE ONLY LOCKED AGENCIES            
         BZ    PRRX                                                             
*                                  MOVE IN BILLING COPIES                       
         EDIT  (1,TAAYNBIL),(2,ALNBIL),ZERO=NOBLANK                             
*                                                                               
         MVI   ALCOD,C'N'          MOVE IN COD FLAG                             
         TM    TAAYSTAT,TAAYSCOD                                                
         BZ    *+8                                                              
         MVI   ALCOD,C'Y'                                                       
*                                                                               
         MVC   ALOFF,TAAYTPOF      MOVE IN TALENT PARTNERS OFFICE               
*                                                                               
         MVC   ALAGG,TAAYAGG       MOVE IN AGENCY GROUP CODE                    
*                                                                               
         CLI   OPTSRV,0            FILTERING ON SERVICE TYPE?                   
         BE    PR4                                                              
         CLI   OPTSRV,C'R'         PREMIUM ONLY                                 
         BNE   PR3A                                                             
         TM    TAAYMISC,TAAYPREM                                                
         BNO   PRRX                                                             
         B     PR4                                                              
PR3A     TM    TAAYMISC,TAAYPREM   PAYROLL ONLY                                 
         BO    PRRX                                                             
*                                                                               
PR4      DS    0H                                                               
*                                                                               
PRPUR    DS    0H                                                               
*                                                                               
         CLI   OPTPUR,0            SKIP IF NOT FILTERING ON PURCHASE            
         BE    PRPURX                 TYPE                                      
*                                                                               
         CLI   OPTPUR,C'Y'         IF WE WANT COD AGYS                          
         BNE   PRPUR10                                                          
*                                                                               
         TM    TAAYSTAT,TAAYSCOD      DROP NON COD AGENCY                       
         BNO   PRRX                                                             
*                                                                               
         B     PRPURX                                                           
*                                                                               
PRPUR10  DS    0H                                                               
*                                                                               
         CLI   OPTPUR,C'N'         IF WE WANT NON COD AGYS                      
         BNE   PRPUR10                                                          
*                                                                               
         TM    TAAYSTAT,TAAYSCOD      DROP COD AGENCY                           
         BO    PRRX                                                             
*                                                                               
PRPURX   DS    0H                                                               
*                                                                               
         MVC   ALSRV,=C'PAY'       SERVICE LEVEL = PAYROLL                      
         TM    TAAYMISC,TAAYPREM                                                
         BNO   *+10                                                             
         MVC   ALSRV,=C'PRE'       SERVICE LEVEL = PREMIUM                      
*                                                                               
PR5      MVC   ALAGY,TIAGY         MOVE IN AGENCY                               
         MVC   ALSHORT,TISHORT     MOVE IN SHORT NAME                           
*                                                                               
         CLC   TISHORT,SPACES      IF NO SHORT NAME                             
         BH    *+10                                                             
         MVC   ALSHORT,TINAME      THEN USE FIRST 16 BYTES OF LONG NAME         
*                                                                               
         L     R4,TIAREC           POINT R4 TO BILLING RULES ELEMENT            
         MVI   ELCODE,TABRELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   PR7                 SKIP IF NOT FOUND                            
         USING TABRD,R4                                                         
*                                  MOVE IN BILLING TYPE                         
         MVC   ALBTYP+1(1),TABRTYPE                                             
         CLI   TABRTYPE,X'C0'      ALPHABETIC OR NUMERIC?                       
         BH    PR6                                                              
         EDIT  (1,TABRTYPE),(2,ALBTYP),ZERO=NOBLANK                             
*                                                                               
PR6      CLI   OPTBTYPE,0                                                       
         BE    PR6A                                                             
         CLC   ALBTYP,OPTBTYPE                                                  
         BNE   PRRX                                                             
*                                                                               
PR6A     MVC   ALRECV,TABRRECV     MOVE IN RECEIVABLE A/C                       
*                                                                               
PR7      MVC   DMDSKADD,TIDSKADD   PASS DISK ADDRESS TO LISTMON                 
*                                                                               
         CLI   MODE,PRINTREP       IF MODE IS PRINT REPORT                      
         BNE   PR10                                                             
         GOTO1 CATCHIOS            LIMIT IOS                                    
         GOTO1 SPOOL,DMCB,(R8)     THEN CALL SPOOL                              
         AP    PRCOUNT,=P'1'       INCREMENT RECORD COUNT                       
         B     PRRX                                                             
*                                                                               
PR10     CLI   LISTNUM,15          ELSE IF END OF PAGE                          
         BNE   PR20                BUT THERE ARE MORE RECS                      
         MVC   MYMSGNO1,OKNO           PUT MSG - HIT ENTER FOR NEXT             
         MVI   MYMSYS,X'FF'                                                     
         OI    GENSTAT2,USGETTXT                                                
         LA    R2,SALSELH                                                       
         B     ERRXIT                                                           
*                                                                               
PR20     GOTO1 LISTMON             CALL LISTMON                                 
*                                                                               
PRRX     B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
         B     ERRXIT                                                           
*                                                                               
ERRXIT   XC    DMCB,DMCB                                                        
         GOTO1 EXIT,DMCB                                                        
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
*                                                                               
PFTAB    DS    0X                   TABLE OF PFKEY ACTIONS                      
         DC    AL1(PF13X-*,13,0,(PF13X-PF13)/KEYLNQ,0)                          
         DC    CL3'CL ',CL8'CLIENT',CL8'LIST'                                   
PF13     DC    AL1(KEYTYCUR,L'ALAGY),AL2(ALAGY-LISTD)                           
PF13X    EQU   *                                                                
         SPACE                                                                  
         DC    AL1(PF14X-*,14,0,(PF14X-PF14)/KEYLNQ,0)                          
         DC    CL3'IF ',CL8'INTF',CL8'LIST'                                     
PF14     DC    AL1(KEYTYCUR,L'ALAGY),AL2(ALAGY-LISTD)                           
PF14X    EQU   *                                                                
         SPACE                                                                  
         DC    AL1(PF15X-*,15,0,(PF15X-PF15)/KEYLNQ,0)                          
         DC    CL3'AL ',CL8'ATTN',CL8'LIST'                                     
PF15     DC    AL1(KEYTYCUR,L'ALAGY),AL2(ALAGY-LISTD)                           
PF15X    EQU   *                                                                
         SPACE                                                                  
         DC    X'FF'                                                            
         SPACE 2                                                                
MYSPECS  DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,56,REQUESTOR                                                  
         SSPEC H2,56,REPORT                                                     
         SSPEC H2,73,PAGE                                                       
         SPACE 1                                                                
         SSPEC H1,32,C'AGENCY LIST'                                             
         SSPEC H2,32,C'-----------'                                             
         SPACE 1                                                                
         SSPEC H4,1,C'AGENCY'                                                   
         SSPEC H4,10,C'SHORT NAME'                                              
         SSPEC H4,32,C'RECEIVABLE'                                              
         SSPEC H4,44,C'BTYP CPY PUR OFF GROUP'                                  
         SPACE 1                                                                
         SSPEC H5,1,C'------'                                                   
         SSPEC H5,10,C'----------'                                              
         SSPEC H5,32,C'----------'                                              
         SSPEC H5,44,C'---- --- --- --- -----'                                  
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
* APPLICATION STORAGE                                                           
*                                                                               
WORKD    DSECT                                                                  
RDSEQ    DS    CL1                 READ WITH ACTIVE/PASSIVE POINTER             
STARTAT  DS    CL24                STARTAT KEY                                  
GROUP    DS    CL6                 AGENCY GROUP FILTER                          
PRCOUNT  DS    PL4                 RECORD COUNT FOR REPORTS                     
OPTS     DS    XL1                                                              
OPTLOCK  EQU   X'80'               LIST LOCKED RECORDS                          
OPTBTYPE DS    CL2                                                              
OPTOFF   DS    CL1                                                              
OPTSRV   DS    CL1                                                              
OPTPUR   DS    CL1                                                              
LIMBLK   DS    XL(FAWSSVRL)                                                     
*                                                                               
         EJECT                                                                  
       ++INCLUDE TASYSIOD                                                       
         EJECT                                                                  
* DSECT TO COVER LIST LINE                                                      
*                                                                               
LISTD    DSECT                                                                  
ALAGY    DS    CL6                 AGENCY                                       
         DS    XL3                                                              
ALSHORT  DS    CL16                SHORT NAME                                   
         DS    XL6                                                              
ALRECV   DS    CL11                RECEIVABLE A/C                               
         DS    XL2                                                              
ALBTYP   DS    CL2                 BILLING TYPE                                 
         DS    XL2                                                              
ALNBIL   DS    CL2                 BILLING COPIES                               
         DS    XL3                                                              
ALCOD    DS    CL1                 COD (Y/N)                                    
         DS    XL3                                                              
ALOFF    DS    CL1                 TP OFFICE                                    
         DS    XL2                                                              
ALAGG    DS    CL6                 AGENCY GROUP                                 
         DS    XL1                                                              
ALSRV    DS    CL3                 SERVICE LEVEL                                
*                                                                               
         EJECT                                                                  
* TAGENWORKD                                                                    
* TASYSDSECT                                                                    
* TAGENFILE                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* FAWSSVRD                                                                      
         PRINT OFF                                                              
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FAWSSVRD                                                       
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR32D                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017TAGEN32   03/14/13'                                      
         END                                                                    
