*          DATA SET NESFM60    AT LEVEL 057 AS OF 10/13/20                      
*PHASE T31C60A                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE DLFLD                                                                  
*                                                                               
***********************************************************************         
*                                                                               
*  TITLE: T31C60 - CLIENT FREEZE OVERNIGHT UPDATIVE REPORT                      
*                                                                               
***********************************************************************         
*                                                                               
* KWAN SEP/2020 SPEC-49757 CHECK FOR LIMIT ACCESS BEFORE CLIENT FREEZE          
*                                                                               
***********************************************************************         
*                                                                               
         TITLE 'T31C60 CLIENT FREEZE OV REPORT'                                 
T31C60   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,CLFRZ,R7                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         MVI   IOOPT,C'Y'                                                       
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,PRINTREP       PRINT RECORDS                                
         BE    PR                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE KEY                                                                  
***********************************************************************         
VK       XC    AOFFICER,AOFFICER                                                
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A38'  GET OFFICER ADDRESS                      
         L     RF,CALLOV                                                        
         GOTO1 (RF),DMCB                                                        
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   AOFFICER,DMCB                                                    
*                                                                               
         LA    R2,CLFMEDH                                                       
         CLI   5(R2),0                                                          
         JE    MISSFLD                                                          
*                                                                               
         MVI   BYTE,C'A'                                                        
         MVC   AIO,AIO3                                                         
*                                                                               
         GOTO1 VALIMED             AGY/MED IN BAGYMD                            
*                                                                               
         LA    R2,CLFTESTH                                                      
         CLI   5(R2),0                                                          
         JNE   *+8                                                              
         MVI   8(R2),C'Y'          FORCE IT TO TEST RUN = Y                     
*                                                                               
         CLI   8(R2),C'Y'          TEST RUN = Y?                                
         JE    VR02                                                             
         CLI   8(R2),C'N'          TEST RUN = N?                                
         JNE   INVLFLD                                                          
         TM    WHEN,WOK$OV         CAN ONLY RUN IT IF OVERNIGHT                 
         JZ    OVERROR                                                          
*                                                                               
VR02     LA    R2,CLFDATEH         END DATE                                     
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*                                                                               
         GOTO1 DATVAL,DMCB,(0,CLFDATE),WORK                                     
         OC    DMCB(4),DMCB                                                     
         BZ    INVLFLD                                                          
*                                                                               
         GOTO1 DATCON,DMCB,WORK,(2,DATEC)                                       
         GOTO1 DATCON,DMCB,WORK,(3,DATEB)                                       
         MVC   DATEE,WORK                                                       
*                                                                               
VKX      J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PROCCESS RECORDS AND PRINT THEM                                               
***********************************************************************         
PR       ICM   RE,15,TWAMASTC                                                   
         JZ    *+2                                                              
         ICM   RF,15,MCSSB-MASTD(RE)                                            
         JZ    *+2                                                              
         OI    SSOSTAT2-SSOOFF(RF),SSOSROLC  WRITE COPY TO RECOVERY             
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
PR00     XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING CLTRECD,R6                                                       
         MVC   AIO,AIO1                                                         
*                                                                               
         MVC   CKEYAM,BAGYMD       AGENCY/MEDIA                                 
*                                                                               
         GOTO1 HIGH                                                             
         J     PR04                                                             
PR02     GOTO1 SEQ                                                              
PR04     CLC   KEY(2),KEYSAVE                                                   
         JNE   PRX                                                              
         LA    R6,KEY                                                           
         OC    CKEYCLT+2(9),CKEYCLT+2                                           
         JNZ   PR02                                                             
*                                                                               
         MVI   FREEZEIT,C'Y'       DEFAULT TO YES                               
         MVC   CLIKEY,KEY                                                       
         MVC   CLIHEX,CKEYCLT      SET CURRENT CLIENT                           
         GOTO1 CLUNPK,DMCB,(BCLIAAN,CKEYCLT),CLIALPH                            
*                                                                               
* TEST ESTIMATES                                                                
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING ESTHDRD,R6                                                       
         MVC   EKEYAM,BAGYMD       AGENCY/MEDIA                                 
         MVC   EKEYCLT,CLIHEX      CLIENT                                       
         MVC   EKEYPRD,=C'POL'                                                  
*                                                                               
         GOTO1 HIGH                                                             
         J     PR14                                                             
PR12     GOTO1 SEQ                                                              
PR14     CLC   KEY(EKEYEST-EKEY),KEYSAVE                                        
         JNE   PR18                                                             
         LA    R6,KEY                                                           
         CLI   EKEYEST,0                                                        
         JE    PR12                                                             
         OC    EKEYEST+1(5),EKEYEST+1                                           
         JNZ   PR12                                                             
*                                                                               
         L     R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         GOTO1 DATCON,DMCB,ESTART,(3,WORK)                                      
         GOTO1 DATCON,DMCB,EEND,(3,WORK+3)                                      
         CLC   DATEB,WORK          REQ DATE < START DATE?                       
         JL    PR16                                                             
         CLC   DATEB,WORK+3        REQ DATE < END DATE?                         
         JNL   PR12                                                             
*                                                                               
PR16     MVI   FREEZEIT,C'N'       DONT FREEZE IT                               
*                                                                               
PR18     XC    KEY,KEY             RESTORE CLIENT SEQUENCE                      
         LA    R6,KEY                                                           
         USING CLTRECD,R6                                                       
         MVC   KEY(L'CKEY),CLIKEY                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLI   FREEZEIT,C'Y'       FREEZE THIS ONE?                             
         JNE   PR02                                                             
*                                                                               
         L     R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         XC    WORK,WORK                                                        
         LA    R4,WORK                                                          
         USING OFFICED,R4                                                       
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC,COFFICE                                                   
         MVC   OFCCLT,CLIALPH                                                   
         OC    OFCCLT,SPACES                                                    
         MVC   OFCSAGMD,CKEYAM                                                  
         MVC   OFCLMT,T31CFFD+6                                                 
         MVC   OFCACCSC(3),CACCESS    ACCESS LIST FROM CLTHDR                   
         MVC   OFCSECD,ASECBLK                                                  
*                                                                               
         MVI   LIMACCOK,C'Y'                                                    
         GOTO1 AOFFICER,DMCB,(C'2',OFFICED),ACOMFACS                            
         CLI   0(R1),0                                                          
         JE    *+8                                                              
         MVI   LIMACCOK,C'N'                                                    
*                                                                               
         MVC   SVOFC2,OFCOFC2                                                   
*                                                                               
         CLI   T31CFFD+6,C'*'      LIMIT ACCESS?                                
         JNE   PR22                                                             
         MVC   KEY,CLIKEY                                                       
         GOTO1 READ                RESTORE SEQ FROM LIMIT ACCESS CALL           
PR22     CLI   LIMACCOK,C'N'       ACCESS TO THIS CLIENT CODE?                  
         JE    PR02                                                             
         DROP  R4                                                               
*                                                                               
*                                                                               
         OI    COPT2,COP2FRZ       FREEZE IT                                    
         BAS   RE,MYFILWRT                                                      
*                                                                               
         BAS   RE,PRINTIT                                                       
         J     PR02                                                             
*                                                                               
PRX      CLC   =C'DOWN',CONOUT                                                  
         JNE   *+8                                                              
         BAS   RE,ENDDL                                                         
         J     EXIT                                                             
*                                                                               
*                                                                               
*                                                                               
PRINTIT  NTR1                                                                   
         LA    R2,MYP                                                           
         USING PLINED,R2                                                        
         MVC   MYP,SPACES                                                       
*                                                                               
         MVC   PSYS,=C'NET'        ALWAYS NET                                   
         MVI   PMED,C'N'           ALWAYS N                                     
         MVC   PCLI,CLIALPH        CLIENT CODE                                  
         MVC   PCNAME,CNAME        CLIENT NAME                                  
         MVC   PFROZEN,FREEZEIT    FROZEN STATUS                                
*                                                                               
         OC    COFFICE,COFFICE                                                  
         JZ    *+10                                                             
         MVC   POFC,SVOFC2                                                      
*                                                                               
         MVC   PAOF(2),CACCOFC     ACC OFFICE/AGY                               
         LA    RF,PAOF                                                          
         AHI   RF,1                                                             
         CLI   0(RF),C' '                                                       
         JNH   *+8                                                              
         AHI   RF,1                                                             
         OC    CACCAGY,CACCAGY     IF AGY OVERRIDE                              
         JZ    *+14                                                             
         MVI   0(RF),C'/'          SEPARATE WITH '/'                            
         MVC   1(2,RF),CACCAGY     AND PASS AGY OVERRIDE                        
         DROP  R2                                                               
*                                                                               
         CLC   =C'DOWN',CONOUT                                                  
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
PI20     LA    RF,MYPD                                                          
         USING PDLINED,RF                                                       
         LA    R2,MYP                                                           
         USING PLINED,R2                                                        
*                                                                               
         MVC   PDSYS,PSYS          ALWAYS NET                                   
         MVC   PDMED,PMED          ALWAYS N                                     
         MVC   PDOFC,POFC          OFFICE CODE                                  
         MVC   PDAOF,PAOF          ACC OFFICE/AGY                               
         MVC   PDCLI,CLIALPH       CLIENT CODE                                  
         MVC   PDCNAME,CNAME       CLIENT NAME                                  
         MVC   PDFROZEN,FREEZEIT   FROZEN STATUS                                
         DROP  R2,RF                                                            
*                                                                               
         LA    RF,MYPD                                                          
         ST    RF,AOUTREC          A(OUTPUT FIELDS)                             
         LA    R1,COLTAB           TABLE OF DISPLACEMENTS                       
         BAS   RE,PDENTRY                                                       
         J     EXIT                                                             
*                                                                               
***********************************************************************         
* DATAMGR INTERFACE                                                             
***********************************************************************         
MYFILADD NTR1                                                                   
         CLI   CLFTEST,C'Y'        TEST RUN = Y?                                
         JE    YES                                                              
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'SPTFILE ',KEY+14,AIO,MYDMWRK           
         BAS   RE,DMCHECK                                                       
         B     YES                                                              
*                                                                               
MYFILWRT NTR1                                                                   
         CLI   CLFTEST,C'Y'        TEST RUN = Y?                                
         JE    EXIT                                                             
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'SPTFILE ',KEY+14,AIO,MYDMWRK           
         BAS   RE,DMCHECK                                                       
         J     EXIT                                                             
*                                                                               
MYDIRWRT NTR1                                                                   
         CLI   CLFTEST,C'Y'        TEST RUN = Y?                                
         JE    EXIT                                                             
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'SPTDIR  ',KEY,KEY                      
         BAS   RE,DMCHECK                                                       
         J     EXIT                                                             
*                                                                               
MYDIRADD NTR1                                                                   
         CLI   CLFTEST,C'Y'        TEST RUN = Y?                                
         JE    EXIT                                                             
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'SPTDIR  ',KEY,KEY                      
         BAS   RE,DMCHECK                                                       
         J     EXIT                                                             
*                                                                               
DMCHECK  CLI   8(R1),0                                                          
         BER   RE                                                               
         TM    8(R1),X'90'                                                      
         BM    NO                                                               
         DC    H'0'                                                             
*                                                                               
YES      SR    R1,R1                                                            
         B     *+8                                                              
NO       LA    R1,1                                                             
         LTR   R1,R1                                                            
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* HEADER AND HEAD HOOK ROUTINES                                                 
***********************************************************************         
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
PDENTRY  NTR1                                                                   
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
***********************************************************************         
* ERROR MESSAGES                                                                
***********************************************************************         
INVLFLD  MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
MISSFLD  MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
OVERROR  MVC   HALF,=AL2(OVERRQ)                                                
         B     SPERREX                                                          
*                                                                               
MYERR    GOTO1 ERREX2                                                           
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
*                                                                               
SPERREX  OI    GENSTAT2,USGETTXT                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTMSGNO,HALF                                                     
         MVI   GTMTYP,GTMERR                                                    
         MVI   GTMSYS,2                                                         
VSFMERR  MVC   AIO,AIO1                                                         
         GOTO1 ERREX                                                            
*                                                                               
OVERRQ   EQU   1472                LIVE RUNS ON OVERNIGHT                       
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
       ++INCLUDE NESFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NESFM5ED                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE NESFMWORKD                                                     
         ORG   SYSSPARE                                                         
*                           *******  T31C47 WORK AREA  *******                  
WORKAREA DS    0CL1                                                             
MYDMWRK  DS    12D                                                              
*                                                                               
DATEC    DS    XL2                 COMPRESSED DATE                              
DATEB    DS    XL3                 BINARY DATE                                  
DATEE    DS    CL6                 YYMMDD                                       
*                                                                               
CLIKEY   DS    XL13                CLIENT KEY                                   
CLIHEX   DS    XL2                 CLIENT HEX CODE                              
CLIALPH  DS    CL3                 CLIENT ALPHA CODE                            
CLINAME  DS    CL20                CLIENT NAME                                  
*                                                                               
AOFFICER DS    A                   A(OFFICER)                                   
AOUTREC  DS    A                   A(OUTPUT FIELDS)                             
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
MYP      DS    CL132                                                            
MYPD     DS    CL132                                                            
*                                                                               
WORKEND  EQU   *                                                                
         EJECT                                                                  
*                                                                               
CLTRECD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE DDOFFICED                                                      
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE DDDLCB                                                         
       ++INCLUDE FASSBOFF                                                       
       ++INCLUDE DDMASTD                                                        
*                                                                               
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
PFROZEN  DS    CL1                 FROZEN                                       
*                                                                               
PDLINED  DSECT                                                                  
PDSYS    DS    CL3                 SYSTEM                                       
PDMED    DS    CL1                 MEDIA                                        
PDOFC    DS    CL2                 OFFICE                                       
PDAOF    DS    CL2                 AOF                                          
PDCLI    DS    CL3                 CLIENT CODE                                  
PDCNAME  DS    CL20                CLIENT NAME                                  
PDFROZEN DS    CL1                 FROZEN                                       
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
*                                                                               
*                                                                               
COLTABD  DSECT                                                                  
CTLEN    DS    X                   ENTRY LENGTH                                 
CTDISP   DS    AL4                 ENTRY'S DISPLACEMENT                         
CTTABDLQ EQU  *-COLTABD                                                         
*                                                                               
         PRINT GEN                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'057NESFM60   10/13/20'                                      
         END                                                                    
