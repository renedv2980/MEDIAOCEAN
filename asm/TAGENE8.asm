*          DATA SET TAGENE8    AT LEVEL 006 AS OF 05/30/14                      
*PHASE T702E8A                                                                  
         TITLE 'T702E8 - TIME LIST'                                             
T702E8   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T702E8                                                         
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
         BRAS  RE,TESTSEL          IF ANYTHING SELECTED, START THERE            
         BE    TM10                                                             
         LH    R2,CURDISP          DISPLACEMENT OF CURSOR                       
         AR    R2,RA                                                            
TM10     ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         USING LINED,R2                                                         
         GOTO1 HEXIN,DMCB,LINSORT,TGCSORT,12                                    
         DROP  R2                                                               
*                                                                               
         CLI   STMSEL,C'D'       TO DELETE RECORD, MUST USE DE                  
         BNE   TM20                                                             
         CLI   STMSEL+1,C'E'                                                    
         BE    TM20                                                             
         LA    R2,STMSELH                                                       
         B     INVERR                                                           
*                                                                               
TM20     LA    RF,PFTAB                                                         
         CLI   RECNUM,TM                                                        
         BE    *+8                                                              
         LA    RF,DPFTAB                                                        
         GOTO1 INITIAL,DMCB,(RF)                                                
         TM    TGSYSTAT,TASYSPID   ARE WE USING PID#?                           
         BZ    *+14                                                             
         MVC   STMTAG(8),=C'Sel ^Pid'                                           
         OI    STMTAGH+6,X'80'                                                  
*                                                                               
         CLI   MODE,VALKEY         FIRST TIME IN                                
         BNE   TM30                                                             
         TM    TRNSTAT,OKINTPFK    IF RETURNING FROM SELECT (DISPLAY)           
         BNO   *+8                                                              
         MVI   LISTSW,C'T'         SET TO RE-DISPLAY SAME PAGE                  
         B     VK                                                               
*                                                                               
TM30     CLI   MODE,LISTRECS       IF MODE LISTRECS                             
         BE    LR                                                               
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
* VALIDATE THE KEY                                                              
*                                                                               
VK       DS    0H                                                               
*                                                                               
         LA    R2,STMAGYH          VALIDATE AGENCY                              
         TM    STMAGYH+4,X'20'     WAS AGENCY VALIDATED?                        
         BO    VK8                                                              
         NI    STMINVH+4,X'DF'                                                  
         GOTO1 CHKCLG,DMCB,STMAGYH,STMCIDH    CHECK/HANDLE CLI GRP              
         MVI   BYTE,0              ENFORCE AGENCY LIMIT RESTRICTIONS            
         BE    *+8                                                              
         MVI   BYTE,X'80'          SKIP AGENCY LIMIT RESTRICTIONS               
         GOTO1 RECVAL,DMCB,(BYTE,TLAYCDQ),(X'20',STMAGYH)                       
         OI    STMAGYH+4,X'20'     VALIDATED                                    
         GOTO1 RAVPPLSA,DMCB,0     SEE IF REC / ACT INVALID FOR P+              
         JNE   ERPPLSI                                                          
*                                                                               
VK8      LA    R2,STMCIDH                                                       
         TM    4(R2),X'20'                                                      
         BO    VK10                                                             
         GOTO1 RECVAL,DMCB,(X'30',TLCOICDQ),(X'08',STMCIDH),STMCOMNH            
         L     R4,AIO          SAVE INTERNAL CMCL NUMBER IN GLOBAL              
         USING TLCOD,R4                                                         
         MVC   TIFCOM,TLCOCOM                                                   
         MVC   TGCOM,TLCOCOM                                                    
         OI    STMCIDH+4,X'20'                                                  
*                                                                               
VK10     NI    STMCIDH+4,X'DF'     VALIDATE INVOICE NUMBER                      
         LA    R2,STMINVH                                                       
         CLI   STMINVH+5,0                                                      
         BNE   VK15                                                             
         GOTO1 RECVAL,DMCB,TLINCDQ,(X'24',STMINVH) USE GLOBAL INVOICE           
*                                                                               
         CLI   ERROR,MISSING       IF THERE'S AN ERROR                          
         BE    ERRXIT                                                           
         XC    STMINV,=X'FFFFFFFFFFFF'  UNCOMPLEMENT INVOICE NUMBER             
         GOTO1 TINVCON,DMCB,STMINV,INVNO,DATCON CONVERT FOR DISPLAY             
         MVC   STMINV,INVNO                                                     
         CLI   0(R1),X'FF'         IF INVALID                                   
         BNE   VK13                                                             
         XC    STMINV,STMINV       CLEAR FROM SCREEN                            
         B     INVERR              REQUIRE INVOICE INPUT                        
*                                                                               
VK13     CLI   ERROR,NOTFOUND                                                   
         BE    ERRXIT                                                           
         B     VK18                                                             
*                                                                               
VK15     GOTO1 TINVCON,DMCB,STMINV,INVNO,DATCON                                 
         CLI   0(R1),X'FF'                                                      
         BE    INVERR                                                           
         XC    INVNO,=6X'FF'       COMPLEMENT INVOICE NUMBER                    
         MVC   TGINV,INVNO                                                      
         GOTO1 RECVAL,DMCB,TLINCDQ,(X'A0',INVNO)                                
         BNE   RECNTFND                                                         
*                                                                               
         USING TAIND,R4                                                         
VK18     L     R4,AIO              R4=A(INVOICE RECORD)                         
         MVI   ELCODE,TAINELQ                                                   
         BAS   RE,GETEL            GET INVOICE STATUS EL                        
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
*        TM    TAINSTAT,TAINSPAY   BAD IF PAID ALREADY                          
*        BO    INVERR                                                           
         OC    TAINTMCO,TAINTMCO   IF INVOICE ALREADY HAS A TIMESHEET           
         BZ    VK30                INTERNAL COMM ID, IT MUST MATCH              
         CLC   TAINTMCO,TGCOM                                                   
         BNE   INVUSED                                                          
VK30     OI    STMINVH+4,X'20'                                                  
*                                                                               
*        XC    KEY,KEY                                                          
*        MVI   LISTSW,C'F'        KEY FIELD CHANGED - START OVER                
*                                                                               
VK50     DS    0H                                                               
VKX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
* LIST THE RECORDS                                                              
*                                                                               
LR       LA    R0,LRHOOK           SET HOOK TO SYSIO                            
         ST    R0,TIHOOK                                                        
         MVC   LLIST,=Y(LINLNQ)                                                 
         OI    GLSTSTAT,APPLCDSP+RETEXTRA                                       
*                                                                               
         MVC   TIACOMFC,ACOMFACS   SET A(COMFACS)                               
         MVC   TIUSERID,TWAORIG    SET UP NECCESSARY DATA FROM                  
         MVC   TIQSTAFF,TGCTSTAF       GLOBAL STORAGE                           
         MVI   TIREAD,TLCACDQ      LIST BY CAST RECORDS                         
*                                                                               
         TWAXC STMSELH,STMLSTH,PROT=Y  CLEAR SCREEN FOR NEW LIST                
*                                                                               
         GOTO1 PGCNTL,DMCB,PGTBL,TIKEY,TIQSKEY        SET KEY                   
*                                                                               
         MVI   NLISTS,8                                                         
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
         XC    TIQSKEY,TIQSKEY     CLEAR CONTINUE KEY                           
         XC    TIQSTART,TIQSTART                                                
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        PROCESS SYSIO RECORDS                                                  
*                                                                               
LRHOOK   NTR1                                                                   
         CLI   TIMODE,PROCREC      PROCESS RECORDS                              
         BNE   XIT                                                              
         CLC   LISTNUM,NLISTS      IF ALREADY DISPLAYED MAX                     
         BE    LRH20               GO RIGHT TO LISTMON                          
         BAS   RE,DISPLAY          ELSE DISPLAY TO SCREEN                       
         MVC   DMDSKADD,TIDSKADD   SET DISK ADDRESS FOR LISTMON                 
*                                                                               
LRH10    GOTO1 LISTMON                                                          
         B     XIT                                                              
LRH20    GOTO1 LISTMON                                                          
         B     XIT                                                              
*                                                                               
DISPLAY  NTR1                                                                   
         L     R2,ATHISLST                                                      
         USING LINED,R2                                                         
*                                                                               
****     OI    GLSTSTAT,RETEXTRA    WANT LISTMON TO COME BACK ON EOF            
         MVC   LINEDATA,SPACES                                                  
         MVC   LINSSN,TISSN         MOVE IN SSN                                 
         MVC   TGSSN,TISSN                                                      
         TM    TGSYSTAT,TASYSPID    ARE WE USING PID#?                          
         BZ    DISP05                                                           
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   LINSSN,SPACES                                                    
         MVC   LINSSN(L'TGPID),TGPID                                            
*                                                                               
DISP05   MVC   LINCAT,TICAT                                                     
         MVC   TGCAT,TICAT                                                      
         LA    RF,TIKEY                                                         
         MVC   TGCSORT,TLCASORT-TLCAD(RF)                                       
*                                                                               
         MVC   AIO,AIO3                                                         
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'88',TGSSN),LINNAMEH                       
         BE    *+10                THEN INDICATE THAT IN NAME FIELD             
         MVC   LINNAME(16),=C'** NOT FOUND ** '                                 
*                                                                               
         GOTO1 HEXOUT,DMCB,TGCSORT,LINSORT,6,0  HIDE CAST SORT KEY              
*                                                                               
         CLI   RECNUM,DM                                                        
         BNE   *+8                                                              
         OI    TGFASTAT,TGRDWBTS                                                
         GOTO1 RECVAL,DMCB,TLTMCDQ,(X'A4',0)                                    
         BNE   DISP30                                                           
*                                                                               
         L     R4,AIO              LOOK FOR ALL TOTALS ELEMENT                  
         USING TATTD,R4                                                         
         MVI   ELCODE,TATTELQ                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
DISP10   BAS   RE,NEXTEL                                                        
         BNE   DISP30                                                           
         CLC   TATTDATE,=X'FFFFFF'                                              
         BNE   DISP10                                                           
         EDIT  TATTSPOT,LINSP,ZERO=BLANK                                        
         EDIT  TATTDAYS,LINDY,ZERO=BLANK                                        
         EDIT  TATTOVTM,LINOT,ZERO=BLANK                                        
         EDIT  TATTDBTM,LINDT,ZERO=BLANK                                        
         EDIT  TATTTRVL,LINTRVL,2,ZERO=BLANK                                    
         EDIT  TATTPDWD,LINPDWD,2,ZERO=BLANK                                    
         EDIT  TATTTAG,LINTG,ZERO=BLANK                                         
         EDIT  TATTREIM,LINREXP,2,ZERO=BLANK                                    
         DROP  R4                                                               
*                                                                               
DISP30   DS    0H                                                               
         OI    LINEH+6,X'80'                                                    
         OI    LINNAMEH+6,X'80'                                                 
         OI    LINSORTH+6,X'80'                                                 
         OI    LINSORTH+1,X'0C'    HIDE FIELD                                   
         MVI   LSTMSTAT,0                                                       
*                                                                               
         L     RF,AIO                                                           
         MVC   AIO,AIO1            RESTORE AIO TO AIO1                          
         MVC   KEY,TIKEY           PASS KEY BACK                                
         GOTO1 HIGH                                                             
         XC    TGCSORT,TGCSORT     CLEAR CAST SORT KEY                          
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
RECNTFND MVI   ERROR,NOTFOUND                                                   
         B     ERRXIT                                                           
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
         B     ERRXIT                                                           
*                                                                               
INVUSED  MVC   MYMSGNO,=Y(ERINVUSD)  INVOICE USED ON DIFF COMMERCIAL            
         J     ERREND                                                           
                                                                                
ERPPLSI  MVC   MYMSGNO,=Y(ERRIAPPA)   RECORD / ACTION INVALID FOR P+            
         J     ERREND                                                           
                                                                                
ERREND   OI    GENSTAT2,USGETTXT     TWO BYTE ERROR MESSAGES                    
         MVI   BLOCK,0                                                          
         MVI   MYMTYP,GTMERR                                                    
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
         DC    CL3'   ',CL8'INVOICE',CL8'DIS'                                   
PF13     DC    AL1(KEYTYTWA,L'STMAGY-1),AL2(STMAGY-T702FFD)                     
         DC    AL1(KEYTYTWA,L'STMINV-1),AL2(STMINV-T702FFD)                     
PF13X    EQU   *                                                                
*                                                                               
         DC    AL1(PF20X-*,20,PFTINT+PFTCPROG,(PF20X-PF20)/KEYLNQ,0)            
         DC    CL3'DE ',CL8'TIME',CL8'DEL'                                      
PF20     DC    AL1(KEYTYTWA,L'STMAGY-1),AL2(STMAGY-T702FFD)                     
         DC    AL1(KEYTYTWA,L'STMINV-1),AL2(STMINV-T702FFD)                     
         DC    AL1(KEYTYTWA,L'STMCID-1),AL2(STMCID-T702FFD)                     
         DC    AL1(KEYTYCUR,L'LINSSN-1),AL2(LINSSN-LINEDATA)                    
         DC    AL1(KEYTYCUR,L'LINCAT-1),AL2(LINCAT-LINEDATA)                    
PF20X    EQU   *                                                                
*                                                                               
         DC    AL1(PF21X-*,21,PFTINT+PFTCPROG,(PF21X-PF21)/KEYLNQ,0)            
         DC    CL3'A  ',CL8'TIME',CL8'ADD'                                      
PF21     DC    AL1(KEYTYTWA,L'STMAGY-1),AL2(STMAGY-T702FFD)                     
         DC    AL1(KEYTYTWA,L'STMINV-1),AL2(STMINV-T702FFD)                     
         DC    AL1(KEYTYTWA,L'STMCID-1),AL2(STMCID-T702FFD)                     
         DC    AL1(KEYTYCUR,L'LINSSN-1),AL2(LINSSN-LINEDATA)                    
         DC    AL1(KEYTYCUR,L'LINCAT-1),AL2(LINCAT-LINEDATA)                    
PF21X    EQU   *                                                                
*                                                                               
         DC    AL1(PF22X-*,22,PFTINT+PFTCPROG)                                  
         DC    AL1((PF22X-PF22)/KEYLNQ,0)                                       
         DC    CL3'S',CL8'TIME    ',CL8'DISP'                                   
PF22     DC    AL1(KEYTYGLB,L'STMAGY-1),AL2(TGAGY-TGD)                          
         DC    AL1(KEYTYTWA,L'STMINV-1),AL2(STMINV-T702FFD)                     
         DC    AL1(KEYTYTWA,L'STMCID-1),AL2(STMCID-T702FFD)                     
         DC    AL1(KEYTYCUR,L'LINSSN-1),AL2(LINSSN-LINEDATA)                    
         DC    AL1(KEYTYCUR,L'LINCAT-1),AL2(LINCAT-LINEDATA)                    
PF22X    EQU   *                                                                
*                                                                               
         DC    AL1(PF23X-*,23,PFTINT+PFTCPROG)                                  
         DC    AL1((PF23X-PF23)/KEYLNQ,0)                                       
         DC    CL3'C',CL8'TIME    ',CL8'CHA'                                    
PF23     DC    AL1(KEYTYGLB,L'STMAGY-1),AL2(TGAGY-TGD)                          
         DC    AL1(KEYTYTWA,L'STMINV-1),AL2(STMINV-T702FFD)                     
         DC    AL1(KEYTYTWA,L'STMCID-1),AL2(STMCID-T702FFD)                     
         DC    AL1(KEYTYCUR,L'LINSSN-1),AL2(LINSSN-LINEDATA)                    
         DC    AL1(KEYTYCUR,L'LINCAT-1),AL2(LINCAT-LINEDATA)                    
PF23X    EQU   *                                                                
*                                                                               
         SPACE                                                                  
         DC    X'FF'                                                            
         SPACE 2                                                                
DPFTAB   DS    0X                   TABLE OF PFKEY ACTIONS                      
         DC    AL1(DPF13X-*,13,0,(DPF13X-DPF13)/KEYLNQ,0)                       
         DC    CL3'   ',CL8'INVOICE',CL8'DIS'                                   
DPF13    DC    AL1(KEYTYTWA,L'STMAGY-1),AL2(STMAGY-T702FFD)                     
         DC    AL1(KEYTYTWA,L'STMINV-1),AL2(STMINV-T702FFD)                     
DPF13X   EQU   *                                                                
*                                                                               
         DC    AL1(DPF22X-*,22,PFTINT+PFTCPROG)                                 
         DC    AL1((DPF22X-DPF22)/KEYLNQ,0)                                     
         DC    CL3'S',CL8'DTIME   ',CL8'DISP'                                   
DPF22    DC    AL1(KEYTYGLB,L'STMAGY-1),AL2(TGAGY-TGD)                          
         DC    AL1(KEYTYTWA,L'STMINV-1),AL2(STMINV-T702FFD)                     
         DC    AL1(KEYTYTWA,L'STMCID-1),AL2(STMCID-T702FFD)                     
         DC    AL1(KEYTYCUR,L'LINSSN-1),AL2(LINSSN-LINEDATA)                    
         DC    AL1(KEYTYCUR,L'LINCAT-1),AL2(LINCAT-LINEDATA)                    
DPF22X   EQU   *                                                                
*                                                                               
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
         SSPEC H4,44,C'BTYP CPY COD OFF GROUP'                                  
         SPACE 1                                                                
         SSPEC H5,1,C'------'                                                   
         SSPEC H5,10,C'----------'                                              
         SSPEC H5,32,C'----------'                                              
         SSPEC H5,44,C'---- --- --- --- -----'                                  
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
*                                                                               
*        ROUTINE TESTS IF ANY FIELDS ARE SELECTED                               
*        RETURNS R2 WITH ADDRESS OF FIRST FIELD SELECTED AND CCEQ               
*                                                                               
TESTSEL  NTR1  BASE=*,LABEL=*                                                   
         LA    R2,STMSELH            R2 -> 1ST SELECT LINE                      
         LA    R3,8                  NUMBER OF SELECT FIELDS ON SCREEN          
         B     *+8                                                              
TSEL10   BRAS  RE,BUMP4              BUMP R2 4 FIELDS TO NEXT SEL LINE          
         CLI   5(R2),0               CHECK FOR INPUT                            
         BE    TSEL30                                                           
         CLI   8(R2),C'S'            S,C, OR D ARE VALID                        
         BE    TSELYES                                                          
         CLI   8(R2),C'C'                                                       
         BE    TSELYES                                                          
         CLI   8(R2),C'D'                                                       
         BE    TSELYES                                                          
TSEL30   BCT   R3,TSEL10                                                        
         B     TSELNO                NO FIELDS ARE SELECTED                     
*                                                                               
TSELYES  XR    RC,RC                                                            
TSELNO   LTR   RC,RC                                                            
         XIT1  REGS=(R2)                                                        
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*        ROUTINE BUMPS R2 4 FIELDS TO NEXT SELECT FIELD                         
*                                                                               
BUMP4    NTR1  BASE=*,LABEL=*                                                   
         LA    R1,4                                                             
BUMP410  ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCT   R1,BUMP410                                                       
         XIT1  REGS=(R2)                                                        
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* APPLICATION STORAGE                                                           
*                                                                               
WORKD    DSECT                                                                  
RDSEQ    DS    CL1                 READ WITH ACTIVE/PASSIVE POINTER             
STARTAT  DS    CL24                STARTAT KEY                                  
GROUP    DS    CL6                 AGENCY GROUP FILTER                          
PRCOUNT  DS    PL4                 RECORD COUNT FOR REPORTS                     
OPTS     DS    XL1                                                              
OPTLOCK  EQU   X'80'               LIST LOCKED RECORDS                          
INVNO    DS    XL6                 INVOICE NUMBER                               
LSTMSTAT DS    XL1                 LISTMON STATUS                               
LSTMEOF  EQU   X'80'               LISTMON EOF                                  
PGTBL    DS    CL(16*L'TLRCKEY)    SAVED KEYS FOR PAGE CONTROL RTN.             
         EJECT                                                                  
* DSECT TO COVER LIST LINE                                                      
LINED    DSECT                                                                  
LINEH    DS    XL8                 ENTRY FIELD HEADER                           
LINEDATA DS    CL70                                                             
         ORG   LINEDATA                                                         
LINSSN   DS    CL9                 SOCIAL SECURITY NUMBER                       
         DS    CL1                                                              
LINCAT   DS    CL3                 CATEGORY                                     
         DS    CL1                                                              
LINSP    DS    CL2                 SP                                           
         DS    CL1                                                              
LINDY    DS    CL2                 DY                                           
         DS    CL1                                                              
LINOT    DS    CL2                 OT                                           
         DS    CL1                                                              
LINDT    DS    CL2                 DT                                           
         DS    CL1                                                              
LINTRVL  DS    CL5                 TRAVEL                                       
         DS    CL1                                                              
LINPDWD  DS    CL5                 PD-WD                                        
         DS    CL1                                                              
LINTG    DS    CL2                 TG                                           
         DS    CL1                                                              
LINREXP  DS    CL9                 REIMBURSED EXPENSE                           
         ORG                                                                    
LINNAMEH DS    XL8                 CAST NAME FIELD HEADER                       
LINNAME  DS    CL16                CAST NAME                                    
LINSORTH DS    XL8                 CAST SORT KEY FIELD HEADER                   
LINSORT  DS    XL55                CAST SORT KEY (HIDDEN)                       
LINLNQ   EQU   *-LINED                                                          
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRE8D                                                       
         EJECT                                                                  
* TASYSIOD                                                                      
* DDGENTWA                                                                      
* TASYSDSECT                                                                    
* TAGENFILE                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* TAGENWORKD                                                                    
* TASYSEQUS                                                                     
* FAGETTXTD                                                                     
* TAGENEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE TASYSIOD                                                       
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE TAGENEQUS                                                      
         PRINT ON                                                               
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006TAGENE8   05/30/14'                                      
         END                                                                    
