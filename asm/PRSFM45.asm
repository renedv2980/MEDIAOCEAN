*          DATA SET PRSFM45    AT LEVEL 029 AS OF 10/13/20                      
*PHASE T41C45A                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE DLFLD                                                                  
                                                                                
         TITLE 'T41C45 - CLIENT FREEZE OVERNIGHT UPDATIVE REPORT'               
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* KWAN MAR/2020 SPEC-39079 FREEZE INACTIVE CLIENT CODES                         
* KWAN SEP/2020 SPEC-49757 CHECK FOR LIMIT ACCESS BEFORE CLIENT FREEZE          
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
T41C45   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T41C45,R7,RR=R3                                                
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         USING OFFICED,OFCBLK                                                   
*                                                                               
         ST    R3,RELO                                                          
*                                                                               
         XC    DMCB(12),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A38'                                           
         GOTO1 CALLOV,DMCB         GET OFFICER ADDRESS                          
         CLI   4(R1),X'FF'                                                      
         JNE   *+6                                                              
         DC    H'0'                                                             
         MVC   OFFICER,DMCB        SAVE ADDRESS OF OFFICER                      
*                                                                               
         MVI   CONSERVH+6,X'81'    FORCE SRV REQ FIELD MODIFIED                 
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         JNE   *+12                                                             
         BRAS  RE,VK                                                            
         J     MAINX                                                            
*                                                                               
         CLI   MODE,PRINTREP       PRINT RECORDS                                
         JNE   *+12                                                             
         BRAS  RE,PR                                                            
         J     MAINX                                                            
*                                                                               
MAINX    DS    0H                                                               
EXIT     XIT1                                                                   
         LTORG                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* VALIDATE KEY                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VK       NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R2,CLFMEDH                                                       
         CLI   5(R2),0                                                          
         JE    MISSFLD                                                          
*                                                                               
         MVI   BYTE,C'A'                                                        
         MVC   AIO,AIO3                                                         
*                                                                               
         GOTO1 VALIMED                                                          
*                                                                               
         LA    R2,CLFTESTH                                                      
         CLI   5(R2),0                                                          
         JNE   *+8                                                              
         MVI   8(R2),C'Y'          FORCE IT TO TEST RUN = Y                     
*                                                                               
         CLI   8(R2),C'Y'          TEST RUN = Y?                                
         JE    VK02                                                             
         CLI   8(R2),C'N'          TEST RUN = N?                                
         JNE   INVLFLD                                                          
         TM    WHEN,WOK$OV         CAN ONLY RUN IT IF OVERNIGHT                 
         JZ    OVERROR                                                          
*                                                                               
VK02     LA    R2,CLFDATEH         END DATE                                     
         CLI   5(R2),0                                                          
         JE    MISSFLD                                                          
*                                                                               
         GOTO1 DATVAL,DMCB,(0,CLFDATE),WORK                                     
         OC    DMCB(4),DMCB                                                     
         JZ    INVLFLD                                                          
*                                                                               
         GOTO1 DATCON,DMCB,WORK,(2,DATEC)                                       
         GOTO1 DATCON,DMCB,WORK,(3,DATEB)                                       
         MVC   DATEE,WORK                                                       
*                                                                               
VKX      J     EXIT                                                             
*                                                                               
MISSFLD  MVI   ERROR,MISSING                                                    
         J     XITERREX                                                         
INVACTER MVI   ERROR,INVACT                                                     
         J     XITERREX                                                         
INVLFLD  MVI   ERROR,INVALID                                                    
         J     XITERREX                                                         
OVERROR  MVC   HALF,=AL2(OVERRQ)                                                
         J     GTXTERRX                                                         
*                                                                               
GTXTERRX OI    GENSTAT2,USGETTXT                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTMSGNO,HALF                                                     
         MVI   GTMTYP,GTMERR                                                    
         MVI   GTMSYS,4                                                         
         MVC   AIO,AIO1                                                         
         DROP  RF                                                               
*                                                                               
XITERREX GOTOR ERREX                                                            
*                                                                               
OVERRQ   EQU   0345                LIVE RUNS ON OVERNIGHT                       
*                                                                               
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         LTORG                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* PROCCESS RECORDS AND PRINT THEM                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PR       NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLC   =C'DOWN',CONOUT                                                  
         JNE   PR00                                                             
         BAS   RE,INITDL                                                        
*                                                                               
* DOWNLOAD HEADER FIELDS                                                        
*                                                                               
         LA    RF,HDLIST                                                        
         ST    RF,AOUTREC          A(OUTPUT FIELDS)                             
         LA    R1,HDTAB            TABLE OF DISPLACEMENTS                       
         BAS   RE,PDENTRY                                                       
*                                                                               
         OI    GENSTAT2,NOREQDET                                                
PR00     XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING PCLTREC,R6                                                       
         MVC   AIO,AIO1                                                         
*                                                                               
         MVC   PCLTKAGY,AGENCY     AGENCY                                       
         MVC   PCLTKMED,QMED       MEDIA                                        
         MVI   PCLTKRCD,X'02'      CLIENT RECORD                                
         GOTO1 HIGH                                                             
         J     PR04                                                             
*                                                                               
PR02     GOTO1 SEQ                                                              
*                                                                               
PR04     CLC   KEY(4),KEYSAVE      SAME AGY/MED/RECORD?                         
         JNE   PRX                                                              
         LA    R6,KEY                                                           
         MVC   CLIKEY,KEY                                                       
         MVI   FREEZEIT,C'Y'       DEFAULT TO FREEZE CLIENT CODE                
         MVC   CLICODE,PCLTKCLT    SET CURRENT CLIENT                           
*                                                                               
         XC    KEY,KEY             CHECK FOR ESTIMATES                          
         LA    R6,KEY                                                           
         USING PESTREC,R6                                                       
         MVC   PESTKAGY,AGENCY     AGENCY                                       
         MVC   PESTKMED,QMED       MEDIA                                        
         MVI   PESTKRCD,X'07'      ESTIMATE RECORD                              
         MVC   PESTKCLT,CLICODE    CURRENT CLIENT CODE                          
         GOTO1 HIGH                                                             
         CLC   KEY(PESTKPRD-PESTKEY),KEYSAVE                                    
         JE    PR15                                                             
         MVI   FREEZEIT,C'Y'       NO EST FOUND, FREEZE CLT                     
         J     PR18                                                             
*                                                                               
PR12     GOTO1 SEQ                                                              
*                                                                               
PR14     CLC   KEY(PESTKPRD-PESTKEY),KEYSAVE                                    
         JNE   PR18                                                             
*                                                                               
PR15     L     R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         GOTO1 DATCON,DMCB,PESTST,(3,WORK)                                      
         GOTO1 DATCON,DMCB,PESTEND,(3,WORK+3)                                   
         CLC   DATEB,WORK          REQ DATE < START DATE?                       
         JL    PR16                                                             
         CLC   DATEB,WORK+3        REQ DATE < END DATE?                         
         JNL   PR12                                                             
*                                                                               
PR16     MVI   FREEZEIT,C'N'                                                    
*                                                                               
PR18     XC    KEY,KEY             RESTORE CLIENT SEQUENCE                      
         LA    R6,KEY                                                           
         USING PCLTREC,R6                                                       
         MVC   KEY(L'PCLTKEY),CLIKEY                                            
         GOTO1 HIGH                                                             
*                                                                               
         CLI   FREEZEIT,C'Y'       FREEZE THIS ONE?                             
         JNE   PR02                                                             
*                                                                               
         L     R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         MVC   BYTE,PCLTOFF        DEFAULT TO CLIENT OFFICE CODE                
         MVI   ELCODE,X'50'        CLIENT TRAFFIC OFFICE ELEM CODE              
         BRAS  RE,GETEL                                                         
         JNE   *+10                                                             
         MVC   BYTE,2(R6)          USE CLIENT TRAFFIC OFFICE CODE               
         L     R6,AIO              POINT TO CLIENT REC                          
*                                                                               
         XC    WORK,WORK           WORK MUST BE AT LEAST 48 BYTES               
         LA    R4,WORK             (LENGTH OF OFFICED IS 48 BYTES)              
         USING OFFICED,R4                                                       
         MVI   OFCSYS,C'P'                                                      
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCPMED,PCLTKMED                                                 
         MVC   OFCOFC,BYTE         CLT OR CLT TRAFFIC OFFICE CODE               
         MVC   OFCCLT,PCLTKCLT                                                  
         OC    OFCCLT,SPACES                                                    
         MVC   OFCSECD,ASECBLK     A("SECRET BLOCK")                            
         L     RF,ATWA                                                          
         MVC   OFCLMT,6(RF)                                                     
*                                                                               
         MVI   LIMACCOK,C'Y'                                                    
         GOTO1 OFFICER,DMCB,(C'2',OFFICED),ACOMFACS                             
         CLI   0(R1),0                                                          
         JE    *+8                                                              
         MVI   LIMACCOK,C'N'                                                    
*                                                                               
         MVC   SVOFC2,OFCOFC2                                                   
         DROP  R4                                                               
*                                                                               
         L     RF,ATWA                                                          
         CLI   6(RF),C'*'          LIMIT ACCESS?                                
         JNE   PR18M                                                            
         MVC   KEY(L'PCLTKEY),CLIKEY                                            
         GOTO1 READ                RESTORE SEQ FROM LIMIT ACCESS CALL           
PR18M    CLI   LIMACCOK,C'N'       ACCESS TO THIS CLIENT CODE?                  
         JE    PR02                                                             
*                                                                               
PR19     MVC   CLINAME,PCLTNAME                                                 
         OI    PCLTSTAT,X'02'      FREEZE IT                                    
         CLI   CLFTEST,C'Y'        TEST RUN = Y?                                
         JE    PR20                                                             
         GOTO1 PUTREC                                                           
*                                                                               
PR20     BRAS  RE,PRINTIT                                                       
         J     PR02                                                             
*                                                                               
PRX      CLC   =C'DOWN',CONOUT                                                  
         JNE   *+8                                                              
         BAS   RE,ENDDL                                                         
         J     EXIT                                                             
*                                                                               
PRINTIT  NTR1                                                                   
         L     R6,AIO                                                           
         USING PCLTREC,R6                                                       
         LA    R2,MYP                                                           
         USING PLINED,R2                                                        
         MVC   MYP,SPACES                                                       
*                                                                               
         MVC   PSYS,=C'PRT'        PRINT SYSTEM                                 
         MVC   PMED,QMED           MEDIA CODE                                   
         MVC   PCLI,CLICODE        CLIENT CODE                                  
         MVC   PCNAME,CLINAME      CLIENT NAME                                  
         MVC   PFROZEN,FREEZEIT    FROZEN STATUS                                
*                                                                               
         MVC   POFC,SPACES                                                      
         OC    PCLTOFF,PCLTOFF     HAVE CLIENT OFFICE CODE?                     
         JZ    *+10                                                             
         MVC   POFC,SVOFC2                                                      
*                                                                               
         MVC   PAOF(2),PCLTAOFC    ACC OFFICE/AGY                               
*                                                                               
PI10     CLC   =C'DOWN',CONOUT                                                  
         JE    PI20                                                             
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         LA    R1,HDRTN                                                         
         ST    R1,HEADHOOK                                                      
         MVC   P,MYP                                                            
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         J     EXIT                                                             
*                                                                               
PI20     MVC   MYPD,SPACES                                                      
         LA    RF,MYPD                                                          
         USING PDLINED,RF                                                       
         LA    R2,MYP                                                           
         USING PLINED,R2                                                        
*                                                                               
         MVC   PDSYS(L'PSYS),PSYS                                               
         MVC   PDMED(L'PMED),PMED                                               
         MVC   PDOFC(L'POFC),POFC                                               
         MVC   PDAOF(L'PAOF),PAOF                                               
         MVC   PDCLI(L'CLICODE),CLICODE                                         
         MVC   PDCNAME(L'CLINAME),CLINAME                                       
         MVC   PDFROZEN(L'FREEZEIT),FREEZEIT                                    
         DROP  R2,RF                                                            
*                                                                               
         LA    RF,MYPD                                                          
         ST    RF,AOUTREC          A(OUTPUT FIELDS)                             
         LA    R1,COLTAB           TABLE OF DISPLACEMENTS                       
         BAS   RE,PDENTRY                                                       
         J     EXIT                                                             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* HEADER AND HEAD HOOK ROUTINES                                                 
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
HEADING  DS    0H                                                               
         SSPEC H1,03,RUN                                                        
         SSPEC H1,46,C'CLIENT LOCK REPORT'                                      
         SSPEC H1,90,REQUESTOR                                                  
         SSPEC H2,46,C'------------------'                                      
         SSPEC H2,90,REPORT                                                     
         SSPEC H2,103,PAGE                                                      
         DC    X'00'                                                            
*                                                                               
HDRTN    NTR1                                                                   
         MVC   H8(6),=C'SYSTEM'                                                 
         MVC   H8+10(3),=C'MED'                                                 
         MVC   H8+17(4),=C'OFC#'                                                
         MVC   H8+25(7),=C'AOF/AGY'                                             
         MVC   H8+36(8),=C'CLT CODE'                                            
         MVC   H8+48(8),=C'CLT NAME'                                            
         MVC   H8+70(6),=C'FROZEN'                                              
                                                                                
         MVC   H9(6),=C'------'                                                 
         MVC   H9+10(3),=C'---'                                                 
         MVC   H9+17(4),=C'----'                                                
         MVC   H9+25(7),=C'-------'                                             
         MVC   H9+36(8),=C'--------'                                            
         MVC   H9+48(8),=C'--------'                                            
         MVC   H9+70(6),=C'------'                                              
         J     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
* END OF DOWNLOAD REPORT                                                        
*                                                                               
ENDDL    NTR1                                                                   
         MVC   P,SPACES                                                         
         MVI   DLCB+DLCBACT-DLCBD,C'R'                                          
         GOTO1 =V(DLFLD),DLCB                                                   
         J     EXIT                                                             
*                                                                               
* INITIALIZE DOWNLOAD REPORT                                                    
*                                                                               
INITDL   NTR1                                                                   
         XC    HEADHOOK,HEADHOOK                                                
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,0                                                       
                                                                                
         MVI   DNFIRST,C'Y'                                                     
         LA    R2,DLCB                                                          
         USING DLCBD,R2                                                         
         OI    DLCBFLG1,DLCBFXTN                                                
         MVC   DLCXTND,MAXLINE                                                  
         MVC   DLCBAPR,=A(DNPRINT)                                              
         LA    R0,P                                                             
         ST    R0,DLCBAPL                                                       
         MVC   P,SPACES                  JUST IN CASE                           
         MVI   DLCBACT,C'I'              START AND INTIALIZE REPORT             
         GOTO1 =V(DLFLD),DLCB                                                   
*                                                                               
         MVI   FORCEHED,C'Y'       FORCE 2ND PAGE                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         J     EXIT                                                             
         DROP  R2                                                               
*                                                                               
DNPRINT  NTR1                                                                   
         MVI   LINE,0                                                           
         MVI   FORCEHED,C'N'                                                    
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   FORCEHED,C'N'                                                    
         J     EXIT                                                             
*                                                                               
* PRINT IN DOWNLOADABLE FORMAT                                                  
* R1 = A(TABLE OF DISPLACEMENTS)                                                
* AOUTREC = A(OUTPUT FIELDS)                                                    
*                                                                               
PDENTRY  NTR1  BASE=*,LABEL=*                                                   
         LA    R4,DLCB                                                          
         USING DLCBD,R4                                                         
*                                                                               
         LR    R7,R1                                                            
         USING COLTABD,R7                                                       
*                                                                               
PDENT10  CLI   0(R7),X'FF'         END OF COLTAB?                               
         BE    PDENTRYX                                                         
*                                                                               
         CLI   0(R7),X'00'         NOT PRINTING THIS COLUMN?                    
         BE    PDENT20                                                          
*                                                                               
         MVC   DLCBFLD,SPACES                                                   
         MVI   DLCBTYP,C'T'        TEXT FIELD                                   
         MVI   DLCBACT,DLCBPUT                                                  
*                                                                               
         L     RE,AOUTREC                                                       
         ICM   RF,15,CTDISP        DISPLACEMENT TO DATA                         
         AR    RE,RF                                                            
*                                                                               
         LLC   RF,CTLEN            LENGTH OF DATA                               
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   DLCBFLD(0),0(RE)                                                 
*                                                                               
         GOTO1 =V(DLFLD),DLCB      SEND FIELD                                   
*                                                                               
PDENT20  AHI   R7,CTTABDLQ                                                      
         B     PDENT10                                                          
*                                                                               
PDENTRYX MVI   DLCBACT,DLCBEOL           SEND END OF LINE                       
         GOTO1 =V(DLFLD),DLCB                                                   
         J     EXIT                                                             
         DROP  R4                                                               
*                                                                               
* TABLE OF DOWNLOADABLE FIELDS                                                  
*                                                                               
* AL1 - LENGTH OF ENTRY                                                         
* AL4 - ENTRY'S DISPLACEMENT                                                    
*                                                                               
COLTAB   DS    0XL5                                                             
         DC    AL1(L'PDSYS),AL4(PDSYS-PDLINED)                                  
         DC    AL1(L'PDMED),AL4(PDMED-PDLINED)                                  
         DC    AL1(L'PDOFC),AL4(PDOFC-PDLINED)                                  
         DC    AL1(L'PDAOF),AL4(PDAOF-PDLINED)                                  
         DC    AL1(L'PDCLI),AL4(PDCLI-PDLINED)                                  
         DC    AL1(L'PDCNAME),AL4(PDCNAME-PDLINED)                              
         DC    AL1(L'PDFROZEN),AL4(PDFROZEN-PDLINED)                            
         DC    X'FF'                                                            
*                                                                               
* HEADER TABLE FOR PRINTING WITH PDENTRY, SAME FORMAT AS COLTAB                 
* MAKE SURE TO UPDATE, IF PDLINED CHANGES                                       
*                                                                               
* AL1 - LENGTH OF ENTRY                                                         
* AL4 - ENTRY'S DISPLACEMENT                                                    
*                                                                               
HDTAB    DS    0XL5                                                             
         DC    AL1(HDSYSLQ),AL4(HDSYS-HDLIST)                                   
         DC    AL1(HDMEDLQ),AL4(HDMED-HDLIST)                                   
         DC    AL1(HDOFCLQ),AL4(HDOFC-HDLIST)                                   
         DC    AL1(HDAOFCLQ),AL4(HDAOFC-HDLIST)                                 
         DC    AL1(HDCLILQ),AL4(HDCLI-HDLIST)                                   
         DC    AL1(HDCNAMLQ),AL4(HDCNAME-HDLIST)                                
         DC    AL1(HDFRZLQ),AL4(HDFRZ-HDLIST)                                   
         DC    X'FF'                                                            
*                                                                               
* TABLE OF DOWNLOADABLE HEADER FIELDS                                           
*                                                                               
HDLIST   DS    0H                                                               
HDSYS    DC    C'SYSTEM'                                                        
HDSYSLQ  EQU   *-HDSYS                                                          
HDMED    DC    C'MEDIA'                                                         
HDMEDLQ  EQU   *-HDMED                                                          
HDOFC    DC    C'OFC#'                                                          
HDOFCLQ  EQU   *-HDOFC                                                          
HDAOFC   DC    C'AOF/AGY'                                                       
HDAOFCLQ EQU   *-HDAOFC                                                         
HDCLI    DC    C'CLT CODE'                                                      
HDCLILQ  EQU   *-HDCLI                                                          
HDCNAME  DC    C'CLT NAME'                                                      
HDCNAMLQ EQU   *-HDCNAME                                                        
HDFRZ    DC    C'FROZEN'                                                        
HDFRZLQ  EQU   *-HDFRZ                                                          
HDLISTLQ EQU   *-HDLIST                                                         
*                                                                               
*                                                                               
* DLCXTND(8) CONTENTS                                                           
MAXLINE  DC    H'132'                                                           
DELIM    DC    C' '        FIELD DELIMITER                                      
EOTCHR   DC    C'"'        END OF TEXT FIELD DELIMITER                          
EOTALT   DC    C''''       END OF TEXT CHR ALTERNATE                            
EOLCHR   DC    X'5E'       END OF LINE CHAR - SEMI-COLON                        
EORCHR   DC    C':'        END OF REPORT CHR                                    
         DC    X'00'       SPARE                                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DK       NTR1  BASE=*,LABEL=*                                                   
DKX      J     EXIT                                                             
         LTORG                                                                  
*                                                                               
VR       NTR1  BASE=*,LABEL=*                                                   
VRX      J     EXIT                                                             
         LTORG                                                                  
*                                                                               
LR       NTR1  BASE=*,LABEL=*                                                   
LRX      J     EXIT                                                             
         LTORG                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
*                                                                               
       ++INCLUDE PRSFMFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFM99D                                                       
*                                                                               
       ++INCLUDE DDGENTWA                                                       
*                                                                               
       ++INCLUDE PRSFMWORKD                                                     
         ORG   SYSSPARE                                                         
         DS    XL64                AREA FOR BOOK= NOTICE                        
RELO     DS    F                                                                
OFFICER  DS    A                                                                
AOUTREC  DS    A                   A(OUTPUT FIELDS)                             
*                                                                               
CLIKEY   DS    XL(L'KEY)           CLIENT KEY                                   
CLICODE  DS    CL(L'PCLTKCLT)      CLIENT CODE                                  
CLINAME  DS    CL(L'PCLTNAME)      CLIENT NAME                                  
CLTOFN   DS    CL8                 OFFICE CODE (HEXOUT)                         
*                                                                               
DATEC    DS    XL2                 COMPRESSED DATE                              
DATEB    DS    XL3                 BINARY DATE                                  
DATEE    DS    CL6                 YYMMDD                                       
*                                                                               
FREEZEIT DS    CL1                 YES/NO                                       
LIMACCOK DS    CL1                 YES/NO                                       
SVOFC2   DS    CL2                                                              
*                                                                               
* DLFLD PARAMETER BLOCK                                                         
*                                                                               
DLCB     DS    XL256                                                            
DNLINE   DS    CL132                                                            
DNLINE2  DS    CL132                                                            
DNLINE3  DS    CL132                                                            
DNFIRST  DS    X                                                                
*                                                                               
*                                                                               
MYP      DS    CL132                                                            
MYPD     DS    CL132                                                            
OFCBLK   DS    XL(OFCLENQ)         OFFICER BLOCK                                
*                                                                               
         DS    0F                                                               
PLINED   DSECT                                                                  
PSYS     DS    CL3                 SYSTEM                                       
         DS    CL7                                                              
PMED     DS    CL1                 MEDIA                                        
         DS    CL7                                                              
POFC     DS    CL2                 OFFICE                                       
         DS    CL5                                                              
PAOF     DS    CL2                 AOF                                          
         DS    CL9                                                              
PCLI     DS    CL3                 CLIENT CODE                                  
         DS    CL9                                                              
PCNAME   DS    CL20                CLIENT NAME                                  
         DS    CL4                                                              
PFROZEN  DS    CL1                 FORZEN                                       
*                                                                               
*                                                                               
PDLINED  DSECT                                                                  
PDSYS    DS    CL5                 SYSTEM                                       
         DS    CL1                                                              
PDMED    DS    CL3                 MEDIA                                        
         DS    CL1                                                              
PDOFC    DS    CL4                 OFFICE                                       
         DS    CL1                                                              
PDAOF    DS    CL4                 AOF                                          
         DS    CL1                                                              
PDCLI    DS    CL5                 CLIENT CODE                                  
         DS    CL1                                                              
PDCNAME  DS    CL22                CLIENT NAME                                  
         DS    CL1                                                              
PDFROZEN DS    CL3                 FORZEN                                       
PDEND    DS    CL1                                                              
*                                                                               
PDHEADD  DSECT                                                                  
PDHSYS   DS    CL8                                                              
         DS    CL1                                                              
PDHMED   DS    CL5                                                              
         DS    CL1                                                              
PDHOFC   DS    CL6                                                              
         DS    CL1                                                              
PDHAOF   DS    CL9                                                              
         DS    CL1                                                              
PDHCLI   DS    CL10                                                             
         DS    CL1                                                              
PDHCNAME DS    CL10                                                             
         DS    CL1                                                              
PDHFRZN  DS    CL8                                                              
PDHEND   DS    CL1                                                              
*                                                                               
COLTABD  DSECT                                                                  
CTLEN    DS    X                   ENTRY LENGTH                                 
CTDISP   DS    AL4                 ENTRY'S DISPLACEMENT                         
CTTABDLQ EQU  *-COLTABD                                                         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PCLTRECD DSECT                                                                  
       ++INCLUDE PCLTREC                                                        
PESTRECD DSECT                                                                  
       ++INCLUDE PESTREC                                                        
*                                                                               
       ++INCLUDE DDOFFICED         TWO CHARS OFFICE CODE CONVERSION             
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE DDDLCB                                                         
       ++INCLUDE DDMASTD                                                        
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'029PRSFM45   10/13/20'                                      
         END                                                                    
