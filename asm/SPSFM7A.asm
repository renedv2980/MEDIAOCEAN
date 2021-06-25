*          DATA SET SPSFM7A    AT LEVEL 049 AS OF 10/13/20                      
*******20200305:050346: NEW MEMBER ADDED BY WHOA FOR PROJ# SPEC-39079           
*******        :050346: client lock - freeze client if no estimate past         
*                                                                               
* KWAN SEP/2020 SPEC-49757 CHECK FOR LIMIT ACCESS BEFORE CLIENT FREEZE          
*                                                                               
*PHASE T2177AA                                                                  
*INCLUDE DLFLD                                                                  
T2177A   TITLE 'SPSFM7A - CLIENT FREEZE REPORT'                                 
*                                                                               
T2177A   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T2177A*,R7,RR=R3                                              
*                                                                               
         L     RC,0(R1)            STANDARD CODING                              
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN + OUR SCREEN                     
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         LA    R5,SYSSPARE         R5 = A(OVERLAY STORAGE AREA)                 
         USING SAVED,R5                                                         
         ST    R3,RELO                                                          
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY?                                
         BE    VK                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PR                                                               
         B     XIT                                                              
***********************************************************************         
* VALIDATE THE KEY                                                              
***********************************************************************         
VK       DS    0H                                                               
         MVI   MISCFLG1,0                                                       
         LA    R2,CONWHENH                                                      
         CLI   CONWHENH+5,0        MUST HAVE REPORT ID                          
         BE    MISSFLD                                                          
***************                                                                 
* VALIDATE THE MEDIA                                                            
***************                                                                 
VKMED00  DS    0H                                                               
         LA    R2,CLFMEDH                                                       
         CLI   5(R2),0             NO MEDIA?                                    
         JE    NEEDFLDS            THIS IS REQUIRED                             
         GOTO1 VALIMED             VALIDATE MEDIA                               
*                                                                               
         OI    4(R2),X'20'                                                      
         OI    6(R2),X'80'         TRANSMIT                                     
***************                                                                 
* VALIDATE TEST RUN - Y/N                                                       
***************                                                                 
VKTST00  DS    0H                                                               
         LA    R2,CLFTESTH                                                      
         CLI   5(R2),0             ANY INPUT?                                   
         JNE   VKTST10             YES                                          
         MVI   CLFTEST,C'Y'        NONE, ASSUME TEST RUN                        
*                                                                               
VKTST10  CLI   CLFTEST,C'Y'                                                     
         JE    VKTSTX                                                           
         CLI   CLFTEST,C'N'                                                     
         JNE   INVLFLD                                                          
         CLC   =C'SOON',CONWHEN    Live Runs can only be requested              
         JE    LIVERUNS                using the OV                             
*                                                                               
VKTSTX   OI    CLFTESTH+4,X'20'                                                 
         OI    CLFTESTH+6,X'80'    TRANSMIT                                     
***************                                                                 
* VALIDATE THE DATE                                                             
***************                                                                 
VKDAT00  LA    R2,CLFDATEH                                                      
         CLI   5(R2),0             LENGTH OF INPUT = 0?                         
         JE    NEEDFLDS                                                         
*                                                                               
         GOTO1 PERVAL,DMCB,(5(R2),8(R2)),PERVALST                               
         TM    4(R1),X'03'         ANY INVALID DATES?                           
         JNZ   INVLFLD             YES                                          
         TM    4(R1),X'04'         1 INPUT DATE?                                
         JZ    INVLFLD             NO, WE ONLY WANT ONE                         
*                                                                               
         LA    R2,PERVALST                                                      
         USING PERVALD,R2                                                       
         MVC   QDATE,PVALESTA      SAVE OFF EBCDIC DATE  YYMMDD                 
         DROP  R2                                                               
*                                                                               
         OI    CLFDATEH+4,X'20'                                                 
         OI    CLFDATEH+6,X'80'    TRANSMIT                                     
VKX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE RECORD                                                               
***********************************************************************         
PR       CLC   =C'DOWN',CONOUT                                                  
         JNE   PR00                                                             
         BRAS  RE,INITDL                                                        
         OI    GENSTAT2,NOREQDET                                                
*                                                                               
* DOWNLOAD HEADER FIELDS                                                        
*                                                                               
         LA    RF,HDLIST                                                        
         ST    RF,AOUTREC          A(OUTPUT FIELDS)                             
         LA    R1,HDTAB            TABLE OF DISPLACEMENTS                       
         BAS   RE,PDENTRY                                                       
*                                                                               
PR00     XC    LSTCLTKY,LSTCLTKY   NO CLIENT KEY YET                            
         ZAP   CLTCOUNT,=P'0'                                                   
*                                                                               
         LA    R4,KEY                                                           
         USING ESTHDRD,R4                                                       
         XC    KEY,KEY                                                          
         MVI   EKEYTYPE,EKEYTYPQ   X'00'                                        
         MVC   EKEYAM,BAGYMD       BINARY AGENCY/MEDIA                          
*                                                                               
PRHIGH   GOTO1 HIGH                                                             
PR10     LA    R4,KEY                                                           
         CLC   KEY(EKEYCLT-EKEY),KEYSAVE   SAME AGENCY/MEDIA?                   
         JNE   PRX                         NO, DONE                             
*                                                                               
         OC    EKEYPRD,EKEYPRD     DID WE FIND A CLIENT KEY?                    
         JZ    PRCLTKY             YES                                          
         CLI   EKEYEST,0           DO WE HAVE AN ESTIMATE?                      
         JE    PRSEQ               NO, WE HAVE A PRODUCT KEY                    
*                                                                               
         OC    EKEYEST+1(EKCNTRL-EKEYEST-1),EKEYEST+1  BILLING?                 
         JNZ   PRSEQ               YES, BUT WE ONLY WANT ESTIMATES              
*                                                                               
         GOTO1 GETREC              GET THE ESTIMATE                             
         L     R4,AIO                                                           
         CLC   EEND,QDATE          ESTIMATE END BEFORE THE DATE?                
         JH    PRBMPCLT            NO, SKIP THIS CLIENT                         
         J     PRSEQ               YES, CHECK FOR NEXT ESTIMATE                 
*                                                                               
PRCLTKY  OC    LSTCLTKY,LSTCLTKY   DID WE SEE A CLIENT BEFORE?                  
         JZ    PRCLT60             NO, FIRST ENCOUNTER AND CHECK ESTS           
*                                                                               
         MVC   SAVEKEY,KEY         SAVE OFF WHERE WE ARE TO GET BACK            
* FREEZE THE PREVIOUS CLIENT                                                    
         XC    KEY,KEY                                                          
         MVC   KEY,LSTCLTKY                                                     
         GOTO1 HIGH                                                             
*                                                                               
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         USING CLTHDRD,R4                                                       
         MVC   SHOWAAN,CPROF+6     PRINT CLT CODE AS AAN                        
*                                                                               
         XC    WORK,WORK                                                        
         LA    R2,WORK                                                          
         USING OFFICED,R2                                                       
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC,COFFICE                                                   
         GOTO1 CLUNPK,DMCB,(SHOWAAN,CKEYCLT),OFCCLT                             
         OC    OFCCLT,SPACES                                                    
         MVC   OFCSAGMD,CKEYAM                                                  
         L     RF,ATWA                                                          
         MVC   OFCLMT,6(RF)                                                     
         MVC   OFCACCSC(3),CACCESS    ACCESS LIST FROM CLTHDR                   
         MVC   OFCSECD,ASECBLK                                                  
*                                                                               
         MVI   LIMACCOK,C'Y'                                                    
         GOTO1 OFFICER,DMCB,(C'2',OFFICED),ACOMFACS                             
         CLI   0(R1),0                                                          
         JE    *+8                                                              
         MVI   LIMACCOK,C'N'                                                    
*                                                                               
         MVC   SVOFC2,OFCOFC2                                                   
         DROP  R2                                                               
*                                                                               
         L     RF,ATWA                                                          
         CLI   6(RF),C'*'          LIMIT ACCESS?                                
         JNE   PRCLT04                                                          
         MVC   KEY,LSTCLTKY                                                     
         GOTO1 READ                RESTORE SEQ FROM LIMIT ACCESS CALL           
PRCLT04  CLI   LIMACCOK,C'N'       ACCESS TO THIS CLIENT CODE?                  
         JE    PRCLT50                                                          
*                                                                               
         OI    COPT2,COP2FRZ       FREEZE IT SO NO NEW BUYS                     
         AP    CLTCOUNT,=P'1'      INCREMENT # OF CLIENTS FROZEN                
*                                                                               
         CLI   CLFTEST,C'N'        TEST RUN?                                    
         JNE   PRCLT10             YES, JUST PRINT                              
* SPOT HEADER RECORDS NEED A DMWRT  *NOT* A PUTREC                              
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'SPTFILE ',KEY+14,AIO,DMWORK            
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
*                                                                               
PRCLT10  MVC   PSPOT(4),=C'SPOT'                                                
         MVC   PMED(1),CLFMED                                                   
*                                                                               
         OC    COFFICE,COFFICE     Have office code?                            
         JZ    *+10                                                             
         MVC   POFC,SVOFC2                                                      
*                                                                               
         OC    CACCOFC,CACCOFC     ANY ACC OFFICE?                              
         JZ    PRCLT20                                                          
         MVC   PAOFAGY(2),CACCOFC                                               
         OC    PAOFAGY(2),=C'  '                                                
*                                                                               
PRCLT20  OC    CACCAGY,CACCAGY     ANY ACC AGENCY?                              
         JZ    *+14                                                             
         MVI   PAOFAGY+2,C'/'                                                   
         MVC   PAOFAGY+3(2),CACCAGY   SHOW IT                                   
*                                                                               
         GOTO1 CLUNPK,DMCB,(SHOWAAN,CKEYCLT),PCLT                               
         MVC   PCLTNAME,CNAME                                                   
         MVC   PFROZEN(1),=C'Y'                                                 
*                                                                               
         CLC   =C'DOWN',CONOUT     DOWNLOAD??                                   
         JE    PRCLT30                                                          
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         LA    R1,HDRTN                                                         
         ST    R1,HEADHOOK                                                      
         GOTO1 SPOOL,DMCB,(R8)                                                  
         J     PRCLT50                                                          
         DROP  R4                                                               
*                                                                               
PRCLT30  LA    R4,ELEM             DOWNLOAD FORMAT                              
         USING PDLINED,R4                                                       
         MVC   PDSYS,PSPOT         SPOT FOREVER                                 
         MVC   PDMED,PMED                                                       
         MVC   PDOFC,POFC          OFFICE CODE                                  
         MVC   PDAOF,PAOFAGY       ACC OFFICE/AGY                               
         MVC   PDCLI,PCLT          CLIENT CODE                                  
         MVC   PDCNAME,PCLTNAME    CLIENT NAME                                  
         MVC   PDFROZEN,PFROZEN    FROZEN STATUS                                
         MVC   P,SPACES                                                         
         ST    R4,AOUTREC          A(OUTPUT FIELDS)                             
         LA    R1,COLTAB           TABLE OF DISPLACEMENTS                       
         BAS   RE,PDENTRY                                                       
         DROP  R4                                                               
*                                                                               
PRCLT50  MVC   KEY,SAVEKEY         CONTINUE WHERE WE LEFT OFF                   
         GOTO1 HIGH                                                             
PRCLT60  MVC   LSTCLTKY,KEY        SAVE OFF LAST CLIENT KEY                     
         J     PRSEQ               AND LOOK FOR ITS ESTIMATES                   
*                                                                               
PRBMPCLT LA    R4,KEY                                                           
         USING ESTHDRD,R4                                                       
         MVC   KEY,LSTCLTKY        RESTORE LAST CLIENT KEY                      
         XR    R1,R1               BUMP UP THE CLIENT                           
         ICM   R1,3,EKEYCLT                                                     
         LA    R1,1(R1)                                                         
         STCM  R1,3,EKEYCLT                                                     
         XC    LSTCLTKY,LSTCLTKY   WE NO LONGER HAS A PREV CLIENT               
         J     PRHIGH              READ NEXT CLIENT                             
*                                                                               
PRSEQ    GOTO1 SEQ                                                              
         B     PR10                                                             
*                                                                               
PRX      CLC   =C'DOWN',CONOUT                                                  
         JE    PRX1                                                             
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   P+1(19),=C'# CLIENTS FROZEN = '                                  
         EDIT  (P8,CLTCOUNT),(10,P+20),ALIGN=LEFT                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         J     XIT                                                              
*                                                                               
PRX1     BRAS  RE,ENDDL                                                         
         J     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* GENERAL ERROR/INFO MESSAGES AND STUFF                                         
***********************************************************************         
MISSFLD  MVI   ERROR,MISSING                                                    
         B     ERREXIT                                                          
INVLFLD  MVI   ERROR,INVALID                                                    
         B     ERREXIT                                                          
RECNOTF  MVI   ERROR,NOTFOUND                                                   
         B     ERREXIT                                                          
NEEDFLDS MVI   ERROR,REQFIELD                                                   
         B     ERREXIT                                                          
*                                                                               
LIVERUNQ EQU   1472                                                             
LIVERUNS MVC   ERRNUM,=AL2(LIVERUNQ) Live Runs can only be requested            
*                                       using the OV option                     
SPERREX  OI    GENSTAT2,USGETTXT                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTMSGNO,ERRNUM                                                   
         MVI   GTMTYP,GTMERR                                                    
         MVI   GTMSYS,2                                                         
         MVC   AIO,AIO1                                                         
         DROP  RF                                                               
*                                                                               
ERREXIT  GOTO1 ERREX                                                            
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
*                                                                               
XIT      XIT1                                                                   
*                                                                               
***********************************************************************         
* HEADING AND HEADHOOK ROUTINES                                                 
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
         MVC   H8+9(3),=C'MED'                                                  
         MVC   H8+15(4),=C'OFC#'                                                
         MVC   H8+22(7),=C'AOF/AGY'                                             
         MVC   H8+33(8),=C'CLT CODE'                                            
         MVC   H8+43(8),=C'CLT NAME'                                            
         MVC   H8+66(6),=C'FROZEN'                                              
                                                                                
         MVC   H9(6),=C'------'                                                 
         MVC   H9+9(3),=C'---'                                                  
         MVC   H9+15(4),=C'----'                                                
         MVC   H9+22(7),=C'-------'                                             
         MVC   H9+33(8),=C'--------'                                            
         MVC   H9+43(8),=C'--------'                                            
         MVC   H9+66(6),=C'------'                                              
         B     XIT                                                              
*                                                                               
RELO     DS    A                                                                
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
* END OF DOWNLOAD REPORT                                                        
*                                                                               
ENDDL    NTR1                                                                   
         MVC   P,SPACES                                                         
         MVI   DLCB+DLCBACT-DLCBD,C'R'                                          
         GOTO1 =V(DLFLD),DLCB                                                   
         J     XIT                                                              
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
         J     XIT                                                              
         DROP  R2                                                               
*                                                                               
DNPRINT  NTR1                                                                   
         MVI   LINE,0                                                           
         MVI   FORCEHED,C'N'                                                    
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   FORCEHED,C'N'                                                    
         J     XIT                                                              
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
         J     XIT                                                              
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
       ++INCLUDE SPCGRTAB                                                       
         EJECT                                                                  
* DDBIGBOX                                                                      
* FAGETTXTD                                                                     
* DDGLOBEQUS                                                                    
* DDGLVXCTLD                                                                    
* DDCOMFACSD                                                                    
* DMPRTQL                                                                       
* FAFACTS                                                                       
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* DDPERVALD                                                                     
* SPGENCLT                                                                      
* SPGENMKT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DMPRTQL                                                        
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE DDPERVALD                                                      
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPSFMFFD          (BASE SCREEN FOR SYSTEM)                     
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM2CD          (OUR CLFRZ SCREEN)                           
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE SPSFMWORKD        (SYSTEM AREAS)                               
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         PRINT ON                                                               
         EJECT                                                                  
***********************************************************************         
* SYSSPARE SAVED STORAGE AREA                                                   
***********************************************************************         
SAVED    DSECT                                                                  
*                                                                               
AESTTAB  DS    V                                                                
AOUTREC  DS    A                                                                
*                                                                               
MISCFLG1 DS    X                   MISCELLANEOUS FLAGS                          
MF1KYCHG EQU   X'80'                - KEY WAS CHANGED                           
MF1MKCHG EQU   X'40'                - MARKET(S) WAS CHANGED                     
*                                                                               
SHOWAAN  DS    C                   FROM CLIENT RECORD, SHOW CLT AS AAN          
QDATE    DS    CL6                 DATE (YYMMDD)                                
*                                                                               
CLTCOUNT DS    PL8                                                              
LSTCLTKY DS    XL(L'KEY)           LAST CLIENT KEY                              
SAVEKEY  DS    XL(L'KEY)           SAVED KEY                                    
FAKEFLDH DS    XL8                 FAKE HEADER                                  
FAKEFLD  DS    XL80                FAKE FIELD                                   
PERVALST DS    XL56                BLOCK FOR PERVAL                             
SVCLT    DS    XL2                                                              
*                                                                               
OFCBLK   DS    XL(OFCLENQ)                                                      
ERRNUM   DS    XL2                                                              
*                                                                               
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
       ++INCLUDE DDOFFICED                                                      
***********************************************************************         
* PRINT LINE DSECT                                                              
***********************************************************************         
SPOOLD   DSECT                                                                  
         ORG   P                                                                
PSPOT    DS    CL6'SYSTEM'         SPOT                                         
         DS    3C                                                               
PMED     DS    CL3'MED'            MEDIA                                        
         DS    3C                                                               
POFC     DS    CL4'OFC#'           OFFICE                                       
         DS    3C                                                               
PAOFAGY  DS    CL7'AOF/AGY'        AOF/AGY                                      
         DS    4C                                                               
PCLT     DS    CL8'CLT CODE'       CLIENT CODE                                  
         DS    CL2                                                              
PCLTNAME DS    CL20'CLT NAME'                                                   
         DS    CL3                                                              
PFROZEN  DS    CL6'FROZEN'                                                      
*                                                                               
PDLINED  DSECT                                                                  
PDSYS    DS    CL4                 SPOT                                         
PDMED    DS    CL1                 MEDIA                                        
PDOFC    DS    CL2                 OFFICE                                       
PDAOF    DS    CL2                 AOF                                          
PDCLI    DS    CL3                 CLIENT CODE                                  
PDCNAME  DS    CL20                CLIENT NAME                                  
PDFROZEN DS    CL1                 FROZEN                                       
*                                                                               
PDHEADD  DSECT                                                                  
PDHSPOT  DS    CL8                                                              
         DS    CL1                                                              
PDHMED   DS    CL5                                                              
         DS    CL1                                                              
PDHOFC   DS    CL6                                                              
         DS    CL1                                                              
PDHAOF   DS    CL9                                                              
         DS    CL1                                                              
PDHCLT   DS    CL10                                                             
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
       ++INCLUDE DDDLCB                                                         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'049SPSFM7A   10/13/20'                                      
         END                                                                    
