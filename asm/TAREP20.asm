*          DATA SET TAREP20    AT LEVEL 135 AS OF 10/04/16                      
*PHASE T70320B                                                                  
*INCLUDE DLFLD                                                                  
*INCLUDE POWWOW                                                                 
*INCLUDE SMTP                                                                   
         TITLE 'T70320 - UNION TAPE/UNION DISK'                                 
*  NEWEST CHANGE                                                                
***********************************************************************         
*                                                                     *         
* ALL REQUESTS GENERATE A PRINTED REPORT                              *         
*                                                                     *         
* UNTAPE REPORT - UT - ALSO GENERATES A TAPE                          *         
* UNDISK REPORT - UD - ALSO GENERATES A DISK                          *         
* UNDISK DOWN   - OD - ALSO GENERATES A DOWNLOADABLE REPORT           *         
***********************************************************************         
*                                                                               
T70320   CSECT                                                                  
         ENTRY SSB                                                              
         PRINT NOGEN                                                            
         NMOD1 MYEND-MYSTART,T70320,R7                                          
         LR    RA,RC                                                            
         USING MYD,RA                                                           
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9                                                       
         L     R6,ATWA                                                          
         USING CONHEADH-64,R6                                                   
         GOTO1 INITIAL,DMCB,0                                                   
*                                                                               
         MVC   AMASTD,TWAMASTC                                                  
         L     R1,TWADCONS                                                      
         USING TWADCOND,R1                                                      
         MVC   DYNALLOC,TDYNALLO                                                
         MVC   ALOGOC,TLOGOC                                                    
         L     R1,AMASTD                                                        
         USING MASTD,R1                                                         
         MVC   ALOGO,MCVLOGO       SAVE A(LOGO)                                 
         MVC   AREMOT,MCVREMOT     SAVE A(REMOTE)                               
         MVC   VMQRPT,MCVMQRPT                                                  
         MVC   VSMTP,=V(SMTP)                                                   
         CLI   MCTSTRUN,X'FF'      TEST RUN=TEST                                
         BNE   *+8                                                              
         MVI   SFTPTEST,C'Y'                                                    
         DROP  R1                                                               
*                                                                               
         ZAP   TAPCOUNT,=P'0'                                                   
*                                                                               
         CLI   MODE,VALREC                                                      
         BNE   MODE2                                                            
         BRAS  RE,VREC                                                          
         B     XIT                                                              
         SPACE 1                                                                
MODE2    CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
         BRAS  RE,VREC                                                          
         GOTOR OPENTAPE                                                         
         BAS   RE,PREP                                                          
***      BRAS   RE,CLOSTAPE                                                     
         BAS   RE,CLOSTAPE                                                      
         BRAS  RE,CALLPOW                                                       
         CLI   SFTPOPT,C'Y'        MQ?                                          
         BNE   *+8                                                              
         BRAS  RE,MQRPT                                                         
         CLI   ACTEQU,ACTDOWN      DOWNLOADING?                                 
         BNE   XIT                 NO                                           
         GOTO1 =A(PREPD),DMCB,(RC) YES, PRINT DOWNLOADABLE REPORT               
XIT      XIT1                                                                   
         EJECT                                                                  
         EJECT                                                                  
         DROP  R6                                                               
         SPACE 1                                                                
         LTORG                                                                  
*              PRINT REPORT                                                     
         SPACE 3                                                                
PREP     NTR1                                                                   
         MVI   SORTFRST,C'Y'                                                    
         SPACE 1                                                                
*        CLI   ACTEQU,ACTDOWN                                                   
*        BE    PREP1                                                            
         LA    R1,MYSPECS                                                       
         ST    R1,SPECS                                                         
         LA    R1,HOOK             INITIALIZE FOR REPORT                        
         ST    R1,HEADHOOK                                                      
         SPACE 1                                                                
PREP1    MVC   TIACOMFC,ACOMFACS   SET UP FOR TASYSIO                           
         LA    R1,IOHOOK                                                        
         ST    R1,TIHOOK                                                        
         SPACE 1                                                                
PREP2    MVI   TIREAD,TLCKCDQ      SET TO READ CHECKS                           
         OI    TIQFLAGS,TIQFPBNP   ASK TO PASS BNP AS WELL                      
         OI    TIFINS2N,TAINSADJ   NO ADJUSTMENTS                               
         MVI   TIFINCVS,1          (NO CONVERTED RECORDS)                       
         CLI   BILLOPT,C'Y'        OPTION TO FILTER BILLED INVOICES             
         BNE   *+8                                                              
         OI    TIFINSTY,TAINSBIL                                                
         CLI   SESSOPT,C'Y'        OPTION FOR SESSION USES ONLY                 
         BNE   PREP3                                                            
         MVI   TIFPTYPE,C'S'       SET FILTER ON SESSION TYPE                   
         LA    R1,TGD              SET A(TALENT GLOBALS)                        
         ST    R1,TIATGLOB                                                      
*                                                                               
PREP3    OI    TIFPO3N,TAPDORET    SET DON'T WANT RETRO PAYMENTS                
         CLI   RETROOPT,C'A'       IF OPTION TO GET RETROS ALSO                 
         BE    PREP3B                                                           
         CLI   RETROOPT,C'Y'       IF OPTION TO GET ONLY RETROS                 
         BE    *+8                                                              
         CLI   RETROOPT,C'D'                                                    
         BNE   *+12                                                             
         OI    TIFPO3Y,TAPDORET      SET ONLY WANT RETRO PAYMENTS               
PREP3B   NI    TIFPO3N,ALL-TAPDORET  SET WANT RETRO PAYMENTS ALSO               
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
         CLC   TIFUN,=C'SAG'       IF WE JUST DID SAG,                          
         BNE   PREP4                                                            
         MVC   TIFUN,=C'SEG'          HANDLE SEG ON SAME TAPE                   
         B     PREP2                                                            
         SPACE 1                                                                
PREP4    BAS   RE,DOREST           SORT, PRINT AND WRITE TAPE                   
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
NOMORE   CLOSE ((2))               CLOSE THE DATASET                            
         LTR   RF,RF                                                            
         BZ    XIT                                                              
         L     R1,ALOGO            NO-OP SO DON'T END AGAIN                     
         MVC   0(2,R1),=X'07FE'                                                 
         DC    H'0'                                                             
         EJECT                                                                  
*              ROUTINE TO SET UP SECOND REPORT ON QUEUE                         
*                                                                               
IOHOOK   NTR1                                                                   
         CLI   TIMODE,PROCREC      IF THERE IS SOMETHING INTERESTING            
         BNE   XIT                                                              
         CLC   TIAGY,=C'999999'    NO ADJUSTMENTS                               
         BE    XIT                                                              
*                                                                               
         TM    PLNOPT,X'80'                                                     
         BZ    IOHOOK2                                                          
         L     R6,TIAMAIN                                                       
         MVI   ELCODE,TAPDELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   XIT                                                              
         USING TAPDD,R6                                                         
         CLC   TAPDUSE,=C'PAX'                                                  
         BE    IOHOOK2                                                          
         TM    PLNOPT,X'40'        PAX ONLY                                     
         BO    XIT                                                              
         CLC   TAPDUSE,=C'LNA'                                                  
         BE    IOHOOK2                                                          
         CLC   TAPDUSE,=C'LNC'                                                  
         BE    IOHOOK2                                                          
         CLC   TAPDUSE,=C'LNF'                                                  
         BE    IOHOOK2                                                          
         CLC   TAPDUSE,=C'LNN'                                                  
         BNE   XIT                                                              
         DROP  R6                                                               
*                                                                               
IOHOOK2  BAS   RE,INVHEAD          INVOICE HEADERS                              
         BAS   RE,PROCCLA          CLA, PAX, LNA, LNC AND LNN                   
         BAS   RE,INVDET           INVOICE DETAILS                              
         BAS   RE,PROCTOT          TOTALS                                       
         B     XIT                                                              
         EJECT                                                                  
*              BUILD INVOICE HEADER (FROM INVOICE)                              
         SPACE 3                                                                
INVHEAD  NTR1                                                                   
         LA    R5,TAPEIO                                                        
         USING TAPED,R5                                                         
         BAS   RE,CLEARTAP                                                      
         XC    RET4INV,RET4INV                                                  
         L     R4,TIAMAIN                                                       
         USING TLIND,R4                                                         
         MVC   INAGY,TLINAGY                                                    
*                                                                               
         MVC   AIO,AIO3                                                         
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'A0',INAGY)                                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   CONTFLAG,C'Y'                                                    
         BNE   INVHD1D                                                          
*                                                                               
         LR    R0,R4               SAVE                                         
         MVI   ELCODE,TAFNELQ                                                   
         MVC   AIO,TIAMAIN                                                      
         USING TAFND,R4                                                         
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTASC))   LOOK FOR ASC                      
         BNE   INVHD1C                                                          
         L     R4,TGELEM           R4=A(TGELEM)                                 
         ZIC   R1,TAFNLEN                                                       
         BCTR  R1,0                                                             
         SHI   R1,3                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   INAGYCOD(0),TAFNNAME       USE INAGYCOD TEMPORARILY              
*                                                                               
         MVC   AIO,AIO3                                                         
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'A0',INAGYCOD)                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
INVHD1C  L     R0,TIAMAIN                                                       
         MVC   INAGYCOD,SPACES                                                  
         MVI   ELCODE,TAFNELQ                                                   
         MVC   AIO,AIO3                                                         
         L     R4,AIO3                                                          
         USING TAFND,R4                                                         
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTSTU))                                     
         LR    R4,R0                                                            
         BNE   INVHD1D                                                          
         L     R4,TGELEM           R4=A(TGELEM)                                 
         ZIC   R1,TAFNLEN                                                       
         BCTR  R1,0                                                             
         SHI   R1,3                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   INAGYCOD(0),TAFNNAME                                             
         LR    R4,R0                                                            
                                                                                
         USING TLIND,R4                                                         
*                                                                               
*                                                                               
INVHD1D  MVC   AIO,AIO1                                                         
         MVC   WORK(6),TLININV                                                  
         XC    WORK(6),=6X'FF'                                                  
         GOTO1 TINVCON,DMCB,WORK,ININV,DATCON                                   
         MVI   INTYPE,C'1'         TYPE 1 RECORDS                               
         SPACE 1                                                                
         LR    R6,R4                                                            
         XC    CRINVNUM,CRINVNUM                                                
         MVC   INFLMDTE,=C'000000'                                              
         MVC   INRECDTE,=C'000000'                                              
         MVC   INLFSEC,=C'000'                                                  
         MVC   INLOCS,=C'00000'                                                 
         ZAP   INGROSS,=P'0'                                                    
         ZAP   INMISC,=P'0'                                                     
         ZAP   INSPNH,=P'0'                                                     
         ZAP   INPNH,=P'0'                                                      
         ZAP   INTAX,=P'0'                                                      
         ZAP   INHAND,=P'0'                                                     
         ZAP   INAMT,=P'0'                                                      
         CLI   GRNYOPT,C'Y'                                                     
         BNE   *+10                                                             
         ZAP   INGST,=P'0'                                                      
         MVI   ELCODE,0                                                         
         BRAS  RE,GETEL                                                         
         B     INVHEAD4                                                         
         SPACE 1                                                                
INVHEAD2 BRAS  RE,NEXTEL                                                        
         SPACE 1                                                                
INVHEAD4 BNE   HEADCH                                                           
         CLI   0(R6),TABDELQ                                                    
         BE    HEADBD                                                           
         CLI   0(R6),TAINELQ                                                    
         BE    HEADIN                                                           
         CLI   0(R6),TACOELQ                                                    
         BE    HEADCO                                                           
         CLI   0(R6),TACSELQ                                                    
         BE    HEADCS                                                           
         CLI   0(R6),TACCELQ                                                    
         BE    HEADCC                                                           
         CLI   0(R6),TACMELQ                                                    
         BE    HEADCM                                                           
         CLI   0(R6),TALFELQ                                                    
         BE    HEADLF                                                           
         CLI   0(R6),TAFNELQ                                                    
         BE    HEADFN                                                           
         CLI   0(R6),TANUELQ                                                    
         BE    HEADNU                                                           
         CLI   0(R6),TAUPELQ                                                    
         BE    HEADUP                                                           
         B     INVHEAD2                                                         
         SPACE 1                                                                
*                                  BILLING DETAIL                               
         USING TABDD,R6                                                         
HEADBD   L     R1,TABDTAX                                                       
         A     R1,TABDFICR                                                      
         CLI   GRNYOPT,C'Y'        ONLY FOR GREY                                
         BE    *+12                                                             
         A     R1,TABDGST                                                       
         A     R1,TABDPST                                                       
         ST    R1,DUB                                                           
         GOTO1 CONVCASH,DMCB,DUB,INTAX                                          
*                                                                               
         CLI   GRNYOPT,C'Y'                                                     
         BNE   HEADBD5                                                          
         L     R1,TABDGST                                                       
         A     R1,TABDPST                                                       
         ST    R1,DUB                                                           
         GOTO1 CONVCASH,DMCB,DUB,INGST                                          
*                                                                               
HEADBD5  L     R1,TABDHND                                                       
         A     R1,TABDHNDC                                                      
         ST    R1,DUB                                                           
         GOTO1 CONVCASH,DMCB,DUB,INHAND                                         
         GOTO1 CONVCASH,DMCB,TABDTOT,INAMT                                      
         MVC   INUUNITS,=C'000'    PRECLEAR UPGRADE UNITS                       
         MVI   INUMAJ,C'0'                          AND MAJORS                  
         B     INVHEAD2                                                         
         SPACE 1                                                                
*                                  INVOICE DETAIL                               
         USING TAIND,R6                                                         
HEADIN   GOTO1 CONVDATE,DMCB,TAINBDTE,INBILDTE                                  
         CLI   CONTFLAG,C'Y'                                                    
         BNE   INVHEAD2                                                         
         GOTO1 CONVDATE,DMCB,TAINCDTE,INCHKDTE                                  
         B     INVHEAD2                                                         
         SPACE 1                                                                
*                                  COMMERCIAL DETAIL                            
         USING TACOD,R6                                                         
HEADCO   MVC   INCID,TACOCID                                                    
         GOTO1 CONVDATE,DMCB,TACOFCYC,INFFCDTE                                  
         GOTO1 CONVDATE,DMCB,TACODUB,INDUBDTE                                   
         GOTO1 CONVDATE,DMCB,TACOAIR,INAIRDTE                                   
         MVC   INMEDIA,TACOMED                                                  
         MVC   INCTYPE,TACOTYPE                                                 
         MVC   INTID,TACOTID       TRACK ID                                     
         EDIT  (1,TACOTLN),(3,INTLEN),FILL=0,ZERO=NOBLANK                       
         MVC   INADST,TACOADST     ADDENDUM STATE                               
         EDIT  (1,TACOSEC),(3,INCOSEC),FILL=0,ZERO=NOBLANK                      
*                                                                               
         CLI   CONTFLAG,C'Y'                                                    
         BNE   INVHEAD2                                                         
         MVC   ICONT,TACOCONT                                                   
         MVC   INCONTYP,ICONT                                                   
*                                                                               
         OC    CRINVNUM,CRINVNUM                                                
         BZ    *+8                                                              
         BRAS  RE,RDR4INV           READ INVOICE RETRO IS FOR                   
*                                                                               
                                                                                
         B     INVHEAD2                                                         
         SPACE 1                                                                
*                                  FREE FORM NAME                               
         USING TAFND,R6                                                         
HEADFN   LA    R2,INTITLE                                                       
         CLI   TAFNTYPE,TAFNTTTL                                                
         BE    HEADFN2                                                          
         LA    R2,INPRDNM                                                       
         CLI   TAFNTYPE,TAFNTPRD                                                
         BNE   INVHEAD2                                                         
         SPACE 1                                                                
HEADFN2  ZIC   R1,TAFNLEN                                                       
         AHI   R1,-4                                                            
         CHI   R1,29                                                            
         BL    *+8                                                              
         LHI   R1,29                                                            
         EX    R1,*+8                                                           
         B     INVHEAD2                                                         
         MVC   0(0,R2),TAFNNAME                                                 
         SPACE 1                                                                
*                                  FREE FORM NUMBER                             
         USING TANUD,R6                                                         
HEADNU   LA    R2,INEST                                                         
         LHI   R3,14                                                            
         CLI   TANUTYPE,TANUTEST                                                
         BE    HEADNU2                                                          
         LA    R2,INPO                                                          
         LHI   R3,9                                                             
         CLI   TANUTYPE,TANUTAUT                                                
         BE    HEADNU2                                                          
         LA    R2,INSIGN                                                        
         LHI   R3,5                                                             
         CLI   TANUTYPE,TANUTSIG                                                
         BE    HEADNU2                                                          
         LA    R2,CRINVNUM                                                      
         LHI   R3,6                                                             
         CLI   TANUTYPE,TANUTINV                                                
         BE    HEADNU2                                                          
         LA    R2,RET4INV                                                       
         LHI   R3,6                                                             
         CLI   TANUTYPE,TANURT4I                                                
         BNE   INVHEAD2                                                         
         SPACE 1                                                                
HEADNU2  ZIC   R1,TANULEN                                                       
         AHI   R1,-4                                                            
         CR    R1,R3                                                            
         BL    *+6                                                              
         LR    R1,R3                                                            
         EX    R1,*+8                                                           
         B     INVHEAD2                                                         
         MVC   0(0,R2),TANUMBER                                                 
         SPACE 1                                                                
*                                  LIFT DETAILS                                 
         USING TALFD,R6                                                         
HEADLF   MVC   INLFTID,TALFLID                                                  
         EDIT  (1,TALFSEC),(3,INLFSEC),FILL=0,ZERO=NOBLANK                      
         B     INVHEAD2                                                         
         SPACE 1                                                                
*                                  STUDIOS ETC                                  
         USING TACSD,R6                                                         
HEADCS   CLI   TACSTYPE,TACSTYPF                                                
         BE    HEADCSF                                                          
         CLI   TACSTYPE,TACSTYPM                                                
         BE    HEADCSM                                                          
         CLI   TACSTYPE,TACSTYPR                                                
         BE    HEADCSR                                                          
         B     INVHEAD2                                                         
         SPACE 1                                                                
HEADCSF  MVC   INFSTUD(12),TACSSTUD                                             
         MVC   INFCITY(12),TACSCITY                                             
         GOTO1 CONVDATE,DMCB,TACSDATE,INFLMDTE                                  
         B     INVHEAD2                                                         
         SPACE 1                                                                
HEADCSM  CLC   TIFUN,=C'AFM'       IF AFM UNION FILTER                          
         BE    *+12                                                             
         CLI   INCTYPE,C'M'        OR IF COMML TYPE 'M'                         
         BNE   INVHEAD2                                                         
         MVC   INRSTUD(12),TACSSTUD             SET MUSIC STUDIO                
         MVC   INRCITY(12),TACSCITY                 MUSIC CITY                  
         GOTO1 CONVDATE,DMCB,TACSDATE,INRECDTE      MUSIC DATE                  
         B     INVHEAD2                                                         
         SPACE 1                                                                
HEADCSR  CLC   TIFUN,=C'AFM'       IF AFM UNION FILTER                          
         BE    *+12                                                             
         CLI   INCTYPE,C'M'        OR IF COMML TYPE 'M'                         
         BNE   *+14                                                             
         CLC   INRECDTE,=C'000000'   AND NO MUSIC INFO FOUND                    
         BNE   INVHEAD2                                                         
         MVC   INRSTUD(12),TACSSTUD  USE RECORDING INFO                         
         MVC   INRCITY(12),TACSCITY                                             
         GOTO1 CONVDATE,DMCB,TACSDATE,INRECDTE                                  
         B     INVHEAD2                                                         
         SPACE 1                                                                
*                                  UPGRADE ELEMENT                              
         USING TAUPD,R6                                                         
HEADUP   DS    0H                                                               
         CLC   TIUSE,=C'CBL'       CABLE UPGRADES -                             
         BE    HEADUPC               4 BYTES - NO MAJOR                         
         CLC   TIUSE,=C'SCB'                                                    
         BE    HEADUPC                                                          
         CLC   TIUSE,=C'LCB'       LOCAL IS DIFFERENT AGAIN                     
         BE    HEADUPCL                                                         
*                                                                               
         GOTO1 =A(CONVMAJ),DMCB,TAUPIMAJ,INUMAJ                                 
         EDIT  (2,TAUPIUNT),(3,INUUNITS),FILL=0,ZERO=NOBLANK                    
         B     INVHEAD2                                                         
*                                                                               
HEADUPC  EDIT  (2,TAUPIUNT),(4,INCUUNTS),FILL=0,ZERO=NOBLANK                    
         B     INVHEAD2                                                         
*                                                                               
HEADUPCL MVC   INUMAJ,TAUPILCM                                                  
         B     INVHEAD2                                                         
         SPACE 1                                                                
*                                  CONTRACTS                                    
         USING TACCD,R6                                                         
HEADCC   MVC   INAFMC1,TACCCON                                                  
         CLI   TACCNCON,1                                                       
         BE    INVHEAD2                                                         
         MVC   INAFMC2,TACCCON+12                                               
         B     INVHEAD2                                                         
         SPACE 1                                                                
*                                  COMMENTS                                     
         USING TACMD,R6                                                         
HEADCM   DS    0H                                                               
         ZIC   R1,TACMLEN                                                       
         SH    R1,=H'4'                                                         
         CH    R1,=H'39'                                                        
         BL    *+8                                                              
         LA    R1,39                                                            
         EX    R1,*+8                                                           
         B     INVHEAD2                                                         
         MVC   INCOMM(0),TACMCOMM                                               
         B     INVHEAD2                                                         
         EJECT                                                                  
*              BUILD INVOICE HEADER (FROM CHECK)                                
         SPACE 3                                                                
HEADCH   XC    SAVEBASE,SAVEBASE                                                
         XC    SAVEDIFF,SAVEDIFF                                                
         MVI   SVLNTR,0            LIEN OR TRUSTEE STATUS                       
*                                                                               
         L     R6,TIAREC                                                        
         USING TACDD,R6                                                         
         MVI   ELCODE,TACDELQ      GET CHECK DETAILS ELEMENT                    
         BRAS  RE,GETEL                                                         
         BNE   HEADCH1                                                          
         TM    TACDSTAT,TACDSLIN   IS THIS A LIEN CHECK?                        
         BO    HEADCHA                                                          
         TM    TACDSTAT,TACDSTRS   IS THIS A TRUSTEE CHECK?                     
         BNO   HEADCH1                                                          
HEADCHA  MVI   SVLNTR,C'Y'         LIEN OR TRUSTEE CHECK                        
         DROP  R6                                                               
                                                                                
HEADCH1  MVI   ELCODE,0                                                         
         L     R6,TIAREC                                                        
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
         SPACE 1                                                                
HEADCH2  BRAS  RE,NEXTEL                                                        
         BNE   HEADX                                                            
         CLI   0(R6),TACAELQ                                                    
         BE    HEADCA                                                           
         CLI   0(R6),TAPDELQ                                                    
         BE    HEADPD                                                           
         CLI   0(R6),TARPELQ                                                    
         BE    HEADRP                                                           
         B     HEADCH2                                                          
         SPACE 1                                                                
*                                  CAST DETAILS                                 
         USING TACAD,R6                                                         
HEADCA   MVC   INUN,TACAUN                                                      
         MVC   SAVEUNI,INUN        SAVE OFF UNION CODE                          
         CLI   SEQOPT,C'A'                                                      
         BNE   *+10                                                             
         MVC   INUN,=C'***'                                                     
         MVC   INYEAR,TACAYEAR                                                  
         B     HEADCH2                                                          
         SPACE 1                                                                
*                                  RETRO PAYMENT ELEMENT                        
         USING TARPD,R6                                                         
HEADRP   CLI   SVLNTR,C'Y'         IF LIEN OR TRUSTEE CHECK,                    
         BE    HEADCH2             IGNORE THIS ELEMENT                          
         MVC   SAVEBASE,TARPBASE                                                
         MVC   SAVEDIFF,TARPDIFF                                                
         B     HEADCH2                                                          
         SPACE 1                                                                
*                                  PAYMENT DETAILS                              
         USING TAPDD,R6                                                         
HEADPD   MVC   INUSE,TAPDUSE                                                    
         MVC   INUTYP,TAPDTYPE                                                  
         OI    INUTYP,X'F0'                                                     
         CLI   TAPDTYPE,10                                                      
         BL    *+8                                                              
         MVI   INUTYP,C'A'                                                      
         EDIT  TAPDTYPE,(3,INXUTYP),0,FILL=0                                    
         GOTO1 USEVAL,DMCB,TAPDUSE,TAPDTYPE                                     
         MVC   INPDESC(16),TGUSNAME                                             
*                                                                               
         MVI   INSSORRS,C'R'       DEFAULT REUSE                                
         TM    TGUSSTAT,SESSION                                                 
         BZ    *+8                                                              
         MVI   INSSORRS,C'S'       SESSION                                      
*                                                                               
         LA    R2,INUSDESC                                                      
         BRAS  RE,EDITDET                                                       
         GOTO1 CONVDATE,DMCB,TAPDCYCS,INCYCSTR                                  
         GOTO1 CONVDATE,DMCB,TAPDCYCE,INCYCEND                                  
         MVC   INWEEKS,=C'26'                                                   
         CLI   TGUSWKS,X'86'       (6 MONTHS)                                   
         BE    HEADPD2                                                          
         MVC   INWEEKS,=C'13'                                                   
         CLI   TGUSWKS,X'83'       (3 MONTHS)                                   
         BE    HEADPD2                                                          
         MVC   INWEEKS,=C'00'                                                   
         TM    TGUSWKS,X'80'                                                    
         BO    HEADPD2                                                          
         ZIC   R1,TGUSWKS                                                       
         SLL   R1,26               STRIP OUT X'80' AND X'40'                    
         SRL   R1,26                                                            
         SR    R0,R0                                                            
         TM    TGUSWKS,X'40'       X'40' MEANS DAYS                             
         BNO   *+12                                                             
         LA    R1,7(R1)            ROUND UP TO NEAREST N'WEEKS                  
         D     R0,=F'7'                                                         
         EDIT  (R1),(2,INWEEKS),FILL=0                                          
         SPACE 1                                                                
HEADPD2  GOTO1 CONVCASH,DMCB,TAPDPAYI,INGROSS                                   
         L     R1,TAPDPAYC                                                      
         A     R1,TAPDREXP                                                      
         ST    R1,DUB                                                           
         GOTO1 CONVCASH,DMCB,DUB,INMISC                                         
         GOTO1 CONVCASH,DMCB,TAPDSPNH,INSPNH                                    
*                                                                               
         CLI   RETROOPT,C'Y'       IF GETTING RETROS ONLY                       
         BNE   HEADPD3                                                          
**NO-OP  SR    R1,R1                                                            
*07/13   OC    TAPDSPNH,TAPDSPNH   IF SUBJ TO P&H = 0                           
**       BZ    HEADPD2X                                                         
**       L     R1,TAPDPAYI         SUBJ. TO P&H = PAYMENT AMOUNT                
**       A     R1,TAPDPAYC                                                      
**NO-OP  A     R1,TAPDREXP                                                      
         L     R1,TAPDSPNH                                                      
         ICM   RF,15,SAVEBASE                                                   
         SR    R1,RF                                                            
HEADPD2X ST    R1,DUB                                                           
         B     HEADPD3X                                                         
*                                                                               
HEADPD3  CLI   RETROOPT,C'D'                                                    
         BNE   HEADPD4                                                          
         MVC   DUB(4),SAVEBASE                                                  
*                                                                               
HEADPD3X GOTO1 CONVCASH,DMCB,DUB,INSPNH                                         
*                                                                               
HEADPD4  GOTO1 CONVCASH,DMCB,TAPDPNH,INPNH                                      
         CLI   RETROOPT,C'Y'                                                    
         BNE   HEADPD5                                                          
         L     R1,TAPDPNH                                                       
         ICM   RF,15,SAVEDIFF                                                   
         SR    R1,RF                                                            
         ST    R1,DUB                                                           
         B     HEADPD5X                                                         
                                                                                
HEADPD5  CLI   RETROOPT,C'D'                                                    
         BNE   HEADPD6                                                          
         MVC   DUB(4),SAVEDIFF                                                  
                                                                                
HEADPD5X GOTO1 CONVCASH,DMCB,DUB,INPNH                                          
HEADPD6  GOTO1 =A(CONVMAJ),DMCB,TAPDMAJ,INMAJ                                   
         MVI   INVER,C'1'          1=MAIN ONLY                                  
         TM    TAPDSTAT,TAPDSLFT                                                
         BNO   *+8                                                              
         MVI   INVER,C'0'          0=BOTH                                       
*                                  2=LIFT ONLY (NOT AVAILABLE)                  
         EDIT  (2,TAPDUNIT),(3,INUNITS),FILL=0                                  
*                                                                               
         CLC   TAPDUSE,=C'CBL'     CABLE USE 4 BYTES FOR UNITS                  
         BE    HEADPD7                                                          
         CLC   TAPDUSE,=C'SCB'                                                  
         BNE   HEADPD8                                                          
*                                                                               
HEADPD7  EDIT  (2,TAPDUNIT),(4,INCUNITS),FILL=0                                 
*                                                                               
HEADPD8  MVI   INCRIND,C'0'                                                     
         TM    TAPDPSTS,TAPDPCRD                                                
         BNO   *+8                                                              
         MVI   INCRIND,C'1'                                                     
         MVI   INCANIND,C'0'                                                    
         TM    TAPDOPT1,TAPDOCAN                                                
         BNO   *+8                                                              
         MVI   INCANIND,C'1'                                                    
         MVC   INEMP,TAPDEOR       EMPLOYER                                     
         MVC   INCLI,TAPDCLI       CLIENT                                       
         MVC   INPRD,TAPDPRD       PRODUCT                                      
         EDIT  (1,TAPDTAGS),(3,INTAGS),FILL=0,ZERO=NOBLANK                      
*                                                                               
         CLC   TAPDUSE,=C'LCB'     LOCAL CABLE?                                 
         BNE   HEADCH2             NO                                           
         EDIT  (1,TAPDTYPE),(1,INMAJ),FILL=0,ZERO=NOBLANK                       
         B     HEADCH2                                                          
         SPACE 1                                                                
HEADX    CLI   RETROOPT,C'D'        RETROS ONLY P&H DIFF OPTION                 
         BE    *+12                                                             
         CLI   RETROOPT,C'Y'        OR RETROS ONLY                              
         BNE   HEADXX                                                           
         BRAS  RE,RDR4INV           READ INVOICE RETRO IS FOR                   
HEADXX   MVC   RECTYPE,=CL16'INVOICE HEADER'                                    
         BRAS  RE,SORTPUT                                                       
         MVC   SAVETPKY,TAPEKEY                                                 
         MVC   SAVEAMTS,TAPEAMTS                                                
         MVC   SAVEGST,INGST                                                    
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------          
*              READ CREDIT INVOICE                                              
*---------------------------------------------------------------------          
RDCRINV  NTR1                                                                   
         XC    CRINVNUM,=6X'FF'   UNCOMPLEMENT INV NUMBER                       
         MVC   TGAGY,TLINAGY                                                    
         GOTO1 RECVAL,DMCB,TLINCDQ,(X'A0',CRINVNUM) GET THE RECORD              
         BNE   XIT                                                              
         B     XIT                                                              
*---------------------------------------------------------------------          
*              ROUTINES FOR CLASS A AND PAX                                     
*---------------------------------------------------------------------          
*                                                                               
PROCCLA  NTR1                                                                   
         L     R4,TIAMAIN                                                       
         USING TLIND,R4                                                         
         CLC   THISINV,0(R4)       HANDLE ONCE FOR EACH INVOICE                 
         BE    XIT                                                              
         MVC   THISINV,0(R4)                                                    
         LR    R6,R4                                                            
         MVI   ELCODE,TAPDELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   XIT                                                              
         USING TAPDD,R6                                                         
         CLC   TAPDUSE,=C'CLA'     CLASS A                                      
         BNE   *+14                                                             
         MVC   RECTYPE,=CL16'CLASS A'                                           
         B     PROCCLA0                                                         
*                                                                               
         CLC   TAPDUSE,=C'PAX'     PAX                                          
         BNE   *+14                                                             
         MVC   RECTYPE,=CL16'PAX'                                               
         B     PROCCLA0                                                         
*                                                                               
         CLC   TAPDUSE,=C'LNA'     LNA                                          
         BNE   *+14                                                             
         MVC   RECTYPE,=CL16'LATE NIGHT ABC'                                    
         B     PROCCLA0                                                         
*                                                                               
         CLC   TAPDUSE,=C'LNC'     LNC                                          
         BNE   *+14                                                             
         MVC   RECTYPE,=CL16'LATE NIGHT CBS'                                    
         B     PROCCLA0                                                         
*                                                                               
         CLC   TAPDUSE,=C'LNF'     LNF                                          
         BNE   *+14                                                             
         MVC   RECTYPE,=CL16'LATE NIGHT FOX'                                    
         B     PROCCLA0                                                         
*                                                                               
         CLC   TAPDUSE,=C'LNN'     LNN ONLY                                     
         BNE   XIT                                                              
         MVC   RECTYPE,=CL16'LATE NIGHT NBC'                                    
*                                                                               
PROCCLA0 LA    R5,TAPEIO                                                        
         USING TAPED,R5                                                         
         BAS   RE,CLEARTAP                                                      
         MVC   TAPEKEY,SAVETPKY                                                 
         MVI   CATYPE,C'2'                                                      
         MVC   CALFROM,=C'000'                                                  
         MVC   CALTO,=C'000'                                                    
         EDIT  (2,TAPDSTUS),(3,CAFROM),FILL=0                                   
         LH    R1,TAPDSTUS                                                      
         AH    R1,TAPDUSES                                                      
         BCTR  R1,0                                                             
         EDIT  (R1),(3,CATO),FILL=0                                             
         DROP  R6                                                               
*                                                                               
         USING TANDD,R6                                                         
         LR    R4,R6               SAVE A(TAPD EL) IN R4                        
         L     R6,TIAMAIN                                                       
         MVI   ELCODE,TANDELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   PROCCLA1                                                         
         OC    TANDUSEL,TANDUSEL        IF PAID LIFT USES                       
         BZ    PROCCLA1                                                         
         LH    R2,TANDSTUL              LIFT START USE NUMBER                   
         EDIT  (R2),(3,CALFROM),FILL=0                                          
         AH    R2,TANDUSEL                                                      
         BCTR  R2,0                                                             
         EDIT  (R2),(3,CALTO),FILL=0    LIFT END USE NUMBER                     
         SPACE 1                                                                
         USING TAPDD,R4                                                         
PROCCLA1 LA    R2,CAUSES                                                        
         LA    R3,CAPROGS                                                       
         DROP  R5                                                               
         LH    R5,TAPDSTUS         R5=STARTING USE NUMBER                       
         L     R6,TIAMAIN                                                       
         DROP  R4                                                               
         LA    R4,20                                                            
         MVI   ELCODE,TANPELQ                                                   
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
         SPACE 1                                                                
PROCCLA2 BRAS  RE,NEXTEL                                                        
         BNE   PROCCLAX                                                         
         USING TANPD,R6                                                         
         EDIT  (R5),(4,0(R2)),FILL=0    USE NUMBER                              
         UNPK  WORK(5),TANPDATE+1(3)    DATE (MMDD)                             
         MVC   80(4,R2),WORK                                                    
         MVC   0(20,R3),SPACES                                                  
         MVC   0(15,R3),TANPPNME                                                
         MVC   15(1,R3),TANPNWK                                                 
         MVC   16(1,R3),TANPLFT                                                 
         LA    R2,4(R2)                                                         
         LA    R3,20(R3)                                                        
         LA    R5,1(R5)                                                         
         BCT   R4,PROCCLA2                                                      
         SPACE 1                                                                
PROCCLAX BRAS  RE,SORTPUT                                                       
         CLI   FIRSTCLA,C'Y'                                                    
         BNE   XIT                                                              
         MVI   FIRSTCLA,C'N'                                                    
         B     XIT                                                              
         SPACE 1                                                                
THISINV  DC    XL32'00'                                                         
FIRSTCLA DC    C'Y'                                                             
         EJECT                                                                  
*              ROUTINES FOR INVOICE DETAILS (CHECKS)                            
         SPACE 3                                                                
INVDET   NTR1                                                                   
         LA    R5,TAPEIO                                                        
         USING TAPED,R5                                                         
         BAS   RE,CLEARTAP                                                      
         MVC   TAPEKEY,SAVETPKY                                                 
         MVI   CKTYPE,C'3'                                                      
         MVC   TAPEAMTS,SAVEAMTS                                                
         L     R4,TIAREC                                                        
         USING TLCKD,R4                                                         
         MVC   CKSSN,TLCKSSN                                                    
         MVC   CKCAT,TLCKCAT                                                    
         MVI   CKMINOR,C'0'                                                     
         MVI   CKMULTI,C'0'                                                     
         MVI   CKSWEET,C'0'                                                     
         MVI   ELCODE,0                                                         
         MVC   CKOV1,=C'0000'                                                   
         MVC   CKOV2,=C'0000'                                                   
         MVC   CKHOURS,=C'0000'                                                 
         MVC   CKSPOTS,=C'0000'                                                 
         MVC   CKDAYS,=C'0000'                                                  
         MVC   CKFROM,=C'000'                                                   
         MVC   CKTO,=C'000'                                                     
         CLC   SAVEUNI,=C'ACT'     ONLY FOR UNION ACTRA                         
         BNE   INVDET1                                                          
         XC    WORK,WORK                                                        
         GOTO1 CONVCASH,DMCB,WORK,CKGST   SET DEFAULT TO 0.00                   
INVDET1  CLI   GRNYOPT,C'Y'                                                     
         BNE   INVDET2                                                          
         MVC   CKOVRT,=C'0000'                                                  
         MVC   CKTRVT,=C'0000'                                                  
*                                                                               
INVDET2  LR    R6,R4                                                            
         CLI   CONTFLAG,C'Y'                                                    
         BNE   INVDET2B                                                         
         MVI   ELCODE,TANUELQ                                                   
**       USING TANUD,R6                                                         
         USING TANUD,R4                                                         
         ZICM  R0,AIO,(15)                                                      
         ST    R4,AIO                                                           
         GOTO1 GETL,DMCB,(1,=AL1(TANUTPHR))                                     
         STCM  R0,15,AIO                                                        
         BNE   INVDET2B                                                         
         L     R4,TGELEM           R4=A(TGELEM)                                 
         ZIC   R1,1(R4)                                                         
         SHI   R1,2                                                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CKPHRATE(0),TANUMBER                                             
         EDIT  (B2,CKPHRATE),(9,CKPHRATE),FILL=0,ALIGN=RIGHT                    
**INVDET2B LR    R6,R4                                                          
INVDET2B BRAS  RE,GETEL                                                         
         B     *+8                                                              
         SPACE 1                                                                
INVDET3  BRAS  RE,NEXTEL                                                        
         BNE   INVDETX                                                          
         CLI   0(R6),TACAELQ                                                    
         BE    DETCA                                                            
         CLI   0(R6),TATIELQ                                                    
         BE    DETTI                                                            
         CLI   0(R6),TAPDELQ                                                    
         BE    DETPD                                                            
         CLI   0(R6),TASDELQ                                                    
         BE    DETSD                                                            
         CLI   0(R6),TAOPELQ                                                    
         BE    DETOP                                                            
         CLI   0(R6),TAO2ELQ                                                    
         BE    DETO2                                                            
         CLI   0(R6),TACWELQ                                                    
         BE    DETCW                                                            
         CLI   0(R6),TACDELQ                                                    
         BE    DETCD                                                            
         B     INVDET3                                                          
         SPACE 1                                                                
         USING TACAD,R6                                                         
DETCA    SR    RE,RE                                                            
         ICM   RF,15,TACAOV2                                                    
         D     RE,=X'00000064'     DIVIDE BY 100                                
         EDIT  (RF),(4,CKOV2),FILL=0                                            
         MVI   CKONOFF,C'0'        0=OFF         CAMERA                         
         CLC   TACAONOF(2),=C'ON'                                               
         BNE   *+8                                                              
         MVI   CKONOFF,C'1'        1=ON                                         
         MVC   CKUNLOC,TACALOCL                                                 
         MVC   CKACCNO(3),TACAUN                                                
         MVI   CKVER,C'O'          O=LIFT ONLY                                  
         TM    TACASTAT,TACASTLO                                                
         BO    DETCA5                                                           
         MVI   CKVER,C' '          BLANK=NOT ON LIFT                            
         TM    TACASTAT,TACASTLF                                                
         BZ    DETCA5                                                           
         MVI   CKVER,C'Y'          Y=BOTH MAIN AND LIFT                         
         SPACE                                                                  
DETCA5   GOTO1 CONVDATE,DMCB,TACAFRST,CKFRSDTE                                  
         EDIT  (1,TACADBL),(2,CKDUBS),FILL=0                                    
         GOTO1 TRNSAGT,DMCB,(X'40',TACANCDE),CKAGENT                            
         MVC   CKYEAR,TACAYEAR                                                  
         B     INVDET3                                                          
         SPACE 1                                                                
         USING TACDD,R6                                                         
DETCD    CLC   TIFUN,=C'SAG'                                                    
         BNE   INVDET3                                                          
         TM    TACDSTA2,TACDSMSC   TEST MULTI-SERVICE CONTRACT                  
         BZ    *+10                                                             
         MVC   CKCAT,=C'MSC'                                                    
         B     INVDET3                                                          
         SPACE 1                                                                
         USING TATID,R6                                                         
DETTI    MVC   CKCORPID,TATIID                                                  
         B     INVDET3                                                          
         SPACE 1                                                                
         USING TACWD,R6                                                         
DETCW    CLC   SAVEUNI,=C'ACT'     ONLY FOR UNION ACTRA                         
         BNE   DETCW5                                                           
         L     R1,TACWGST                                                       
         ST    R1,WORK                                                          
         GOTO1 CONVCASH,DMCB,WORK,CKGST                                         
DETCW5   CLC   TACWUNIT(2),=C'FD'                                               
         BE    INVDET3                                                          
         CLI   TACWUNIT+2,C' '                                                  
         BH    DETCWLOC                                                         
         MVC   CKSTATE,TACWUNIT                                                 
         MVC   CKSTATE2,TACWUNIT                                                
         B     INVDET3                                                          
         SPACE 1                                                                
DETCWLOC MVC   CKLOCAL,TACWUNIT                                                 
         B     INVDET3                                                          
         SPACE 1                                                                
         USING TAOPD,R6                                                         
DETOP    LA    RF,CKOV1                                                         
         TM    TAOPPCT,X'80'       IS THIS A PERCENT SCALE?                     
         BNO   *+8                                                              
         NI    TAOPPCT,X'FF'-X'80' YES                                          
         SR    RE,RE                                                            
         ICM   RF,15,TAOPPCT                                                    
         D     RE,=X'0000000A'                                                  
         EDIT  (RF),(4,CKOV1),FILL=0                                            
         B     INVDET3                                                          
         SPACE 1                                                                
         USING TAO2D,R6                                                         
DETO2    LA    RF,CKOV2                                                         
         TM    TAO2PCT,X'80'       IS THIS A PERCENT SCALE?                     
         BNO   *+8                                                              
         NI    TAO2PCT,X'FF'-X'80' YES                                          
         SR    RE,RE                                                            
         ICM   RF,15,TAO2PCT                                                    
         D     RE,=X'0000000A'                                                  
         EDIT  (RF),(4,CKOV2),FILL=0                                            
         B     INVDET3                                                          
         SPACE 1                                                                
         USING TAPDD,R6                                                         
DETPD    CLI   RETROOPT,C'Y'       IF GETTING RETROS ONLY                       
         BE    *+8                                                              
         CLI   RETROOPT,C'D'                                                    
         BNE   DETPD3                                                           
         MVI   CKAPPCDE,APPLOTH    SET APPLIED CODE FOR OTHER                   
         L     R1,TAPDSPNH           SUBJ. TO P&H                               
         S     R1,TAPDPAYI         - PAYMENT AMOUNT                             
         S     R1,TAPDPAYC                                                      
         S     R1,TAPDREXP                                                      
         ST    R1,WORK             = ORIGINAL PAYMENT AMOUNT (APPLIED)          
         B     DETPD5                                                           
DETPD3   L     R1,TAPDAPPL                                                      
         LCR   R1,R1                                                            
         ST    R1,WORK                                                          
         MVC   CKAPPCDE,TAPDACDE   APPLIED CODE                                 
         SPACE 1                                                                
DETPD5   GOTO1 CONVCASH,DMCB,WORK,CKPAID                                        
         SPACE 1                                                                
         CLC   SAVEUNI,=C'ACT'     ONLY FOR UNION ACTRA                         
         BNE   DETPD8                                                           
         L     R1,TAPDDUES         UNION DUES                                   
         ST    R1,WORK                                                          
         GOTO1 CONVCASH,DMCB,WORK,CKUDUES                                       
*                                                                               
DETPD8   MVC   CKINCCDE,TAPDICDE   INCLUDE CODE                                 
         MVC   CKEMPNM(3),TAPDEOR                                               
         CLC   TAPDUSE,=C'CLA'     CLASS A                                      
         BE    *+10                                                             
         CLC   TAPDUSE,=C'PAX'     PAX                                          
         BE    *+10                                                             
         CLC   TAPDUSE,=C'LNA'     LNA                                          
         BE    *+10                                                             
         CLC   TAPDUSE,=C'LNC'     LNC                                          
         BE    *+10                                                             
         CLC   TAPDUSE,=C'LNF'     LNF                                          
         BE    *+10                                                             
         CLC   TAPDUSE,=C'LNN'     AND LNN ONLY                                 
         BNE   DETPD10                                                          
         EDIT  (2,TAPDSTUS),(3,CKFROM),FILL=0                                   
         LH    R1,TAPDSTUS                                                      
         AH    R1,TAPDUSES                                                      
         BCTR  R1,0                                                             
         EDIT  (R1),(3,CKTO),FILL=0                                             
*                                                                               
DETPD10  SR    RE,RE                                                            
         ICM   RF,15,TAPDOV1                                                    
         D     RE,=X'0000000A'     DIVIDE BY 10                                 
         EDIT  (RF),(4,CKOV1),FILL=0                                            
         B     INVDET3                                                          
         SPACE 1                                                                
DETSD    BRAS  RE,DETSDR                                                        
         B     INVDET3                                                          
         SPACE 1                                                                
INVDETX  MVC   RECTYPE,=CL16'CHECK RECORD'                                      
         BRAS  RE,SORTPUT                                                       
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*              ROUTINES FOR TOTAL RECORDS                                       
         SPACE 3                                                                
PROCTOT  NTR1                                                                   
         LA    R5,TAPEIO                                                        
         USING TAPED,R5                                                         
         BAS   RE,CLEARTAP                                                      
         MVC   TAPEKEY,SAVETPKY                                                 
         MVC   TTINV,=C'999999'                                                 
         MVI   TTTYPE,C'4'                                                      
**       CLI   CONTFLAG,C'Y'                                                    
**       BNE   *+8                                                              
**       MVI   TTCONT,X'FF'    MAKE IS SORT LAST                                
                                                                                
         MVC   TAPEAMTS,SAVEAMTS                                                
         MVC   TTGST,SAVEGST                                                    
         L     R4,TIAMAIN                                                       
         USING TLIND,R4                                                         
         MVC   RECTYPE,=CL16'AGENCY TOTAL'                                      
         BRAS  RE,SORTPUT                                                       
*                                                                               
*                     CONTRACT TOTALS                                           
         CLI   CONTFLAG,C'Y'                                                    
         BNE   PROCTOT2                                                         
         MVI   T7TYPE,C'7'                                                      
         MVC   TTAGY,=C'999999'                                                 
         MVC   RECTYPE,=CL16'CONTRACT TOTAL'                                    
         MVC   T7CONTTY,T7CONT                                                  
*                                                                               
         LA    RE,T7ZEROS                                                       
         LA    R0,9                                                             
PROCTOT1 MVC   0(L'T7ZEROS,RE),=9C'0'                                           
         AHI   RE,9                                                             
         BCT   R0,PROCTOT1                                                      
**       MVC   T7GROSS,SPACES                                                   
**       MVC   T7MISC,SPACES                                                    
**       MVC   T7GST,SPACES                                                     
* THESE ARE SUPPOSE TO BE BLANK BUT WHY NOT ZEROS?                              
         MVC   T7GROSS,=9C'0'                                                   
         MVC   T7MISC,=9C'0'                                                    
         MVC   T7GST,=9C'0'                                                     
*                                                                               
*                                                                               
         BRAS  RE,SORTPUT                                                       
*                                                                               
*                     UNION TOTALS USED FOR TAPE ONLY                           
PROCTOT2 CLI   CONTFLAG,C'Y'                                                    
         BNE   *+8                                                              
         MVI   TTCONT,X'FF'    ONLY SHOW END                                    
         MVI   TTTYPE,C'5'                                                      
         MVC   TTAGY,=C'999999'                                                 
         MVC   RECTYPE,=CL16'UNION TOTAL'                                       
         BRAS  RE,SORTPUT                                                       
*                     SPECIAL UNION TOTALS USED FOR PRINTING ONLY               
         MVI   TTTYPE,C'5'                                                      
         CLI   CONTFLAG,C'Y'                                                    
         BNE   *+8                                                              
         MVI   TTCONT,X'FF'                                                     
         MVC   TTAGY,=C'999998'                                                 
         MVC   RECTYPE,=CL16'UNION TOTAL'                                       
         MVI   WORK,C'0'                                                        
         MVC   WORK+1(9),INPNH                                                  
         MVC   GTPNH,WORK                                                       
         MVC   WORK+1(9),INSPNH                                                 
         MVC   GTSPNH,WORK                                                      
         MVC   WORK+1(9),INMISC                                                 
         MVC   GTMISC,WORK                                                      
         MVC   WORK+1(9),INGROSS                                                
         MVC   GTGROSS,WORK                                                     
         BRAS  RE,SORTPUT                                                       
*                                                                               
         CLC   TTUN,=C'SAG'                                                     
         BE    *+14                                                             
         CLC   TTUN,=C'SEG'                                                     
         JNE   XIT                                                              
         MVI   TTTYPE,C'6'                                                      
         CLI   CONTFLAG,C'Y'                                                    
         BNE   *+8                                                              
         MVI   TTCONT,X'FF'                                                     
         MVC   TTUN,=C'999999'                                                  
         MVC   TTEMP,=C'999999'                                                 
         MVC   RECTYPE,=CL16'GRAND TOTAL'                                       
         BRAS  RE,SORTPUT                                                       
*                                                                               
*                                                                               
         J     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*              NOW HANDLE THE OUTPUT                                            
         SPACE 3                                                                
DOREST   NTR1                                                                   
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         LA    R5,TAPEIO                                                        
         USING TAPED,R5                                                         
         XC    TAPEKEY,TAPEKEY                                                  
         CLI   SORTFRST,C'Y'       IF SORT WAS ACTIVE                           
         BE    XIT                                                              
         SPACE 1                                                                
DOREST2  GOTO1 SORTER,DMCB,=C'GET' GET A SORT RECORD                            
         L     R2,DMCB+4                                                        
         LTR   R2,R2               IF NO MORE SORT RECORDS                      
         BNZ   DOREST4                                                          
         GOTOR PUTTAPE             WRITE OUT LAST TAPE RECORD                   
         BAS   RE,DOREPORT         AND PRINT REPORT                             
         B     XIT                                                              
         SPACE 1                                                                
DOREST4  MOVE  (SORTIO,700),0(R2)  ELSE, MOVE SORT REC TO LOCAL STORAGE         
**REST4  MOVE  (SORTIO,700),1(R2)  ELSE, MOVE SORT REC TO LOCAL STORAGE         
         CLC   TAPEKEY,0(R2)       SAME KEY?                                    
         BNE   DOREST6                                                          
         CLI   CKTYPE,C'3'         (DON'T COMBINE DETAIL RECORDS)               
         BE    DOREST6                                                          
         CLI   CKTYPE,C'2'         (DON'T CLASS A RECORDS)                      
         BE    DOREST6                                                          
         LA    R2,SORTIO+20                                                     
         LA    R3,TAPEIO+20                                                     
         LA    R0,4                                                             
         CLI   GTTYPE,C'6'         IF GRAND TOTS                                
         BE    DOREST5G                                                         
         CLI   TTTYPE,C'5'         OR SPECIAL UNION TOTALS                      
         BNE   *+14                                                             
         CLC   TTAGY,=C'999998'                                                 
         BE    DOREST5G            ADD WITH CORRECT AMOUNT LENGTH               
         SPACE 1                                                                
DOREST5  PACK  DUB,0(9,R2)         ADD VALUES TO SAVED RECORD                   
         PACK  WORK(8),0(9,R3)                                                  
         AP    DUB,WORK(8)                                                      
         UNPK  0(9,R3),DUB                                                      
         LA    R2,9(R2)                                                         
         LA    R3,9(R3)                                                         
         BCT   R0,DOREST5                                                       
         B     DOREST2                                                          
         SPACE 1                                                                
DOREST5G PACK  DUB,0(10,R2)       ADD VALUES FOR GRAND TOTALS                   
         PACK  WORK(8),0(10,R3)                                                 
         AP    DUB,WORK(8)                                                      
         UNPK  0(10,R3),DUB                                                     
         LA    R2,10(R2)                                                        
         LA    R3,10(R3)                                                        
         BCT   R0,DOREST5G                                                      
         B     DOREST2                                                          
         SPACE 1                                                                
DOREST6  OC    TAPEKEY,TAPEKEY     UNLESS FIRST TIME,                           
         BZ    *+12                                                             
         GOTOR PUTTAPE                WRITE OUT PENDING TAPE RECORD             
         BAS   RE,DOREPORT                  AND PRINT REPORT                    
         BRAS  RE,SORTMOVE         NOW MOVE IN NEW RECORD                       
         GOTO1 =A(FLESHOUT)        AND FLESH OUT NAMES ETC                      
         B     DOREST2                                                          
         EJECT                                                                  
                                                                                
*              PRINT REPORT                                                     
         SPACE 3                                                                
DOREPORT NTR1                                                                   
         LA    R2,P                                                             
         USING PRINTD,R2                                                        
         LA    R5,TAPEIO                                                        
         USING TAPED,R5                                                         
         CLI   INTYPE,C'1'                                                      
         BE    REPINV                                                           
         CLI   CKTYPE,C'2'         DON'T PRINT CLASS A                          
         BE    XIT                                                              
         CLI   CKTYPE,C'3'                     OR CHECKS                        
         BE    XIT                                                              
         CLI   TTTYPE,C'5'         IF UNION TOTAL RECORD                        
         BNE   *+18                                                             
         CLC   TTAGY,=C'999999'    FOR 999999                                   
         BE    XIT                 DON'T PRINT (FOR TAPE ONLY)                  
*                                                                               
         BAS   RE,SPLAT                                                         
         MVC   PTOTALS(25),=C'*** TOTALS FOR AGENCY ***'                        
         CLI   TTTYPE,C'4'                                                      
         BE    REPAMT                                                           
         MVC   PTOTALS(27),=C'*** TOTALS FOR EMPLOYER ***'                      
         CLI   TTTYPE,C'5'                                                      
         BE    REPGRAND                                                         
         MVC   PTOTALS(27),=C'*** GRAND TOTALS ***       '                      
         CLI   TTTYPE,C'6'                                                      
         BE    REPGRAND                                                         
         MVC   PTOTALS(34),=C'*** TOTALS FOR CONTRACT TYPE X ***'               
         MVC   PTOTALS+29(1),T7CONTTY                                           
         CLI   TTTYPE,C'7'                                                      
         BE    REPCONT                                                          
         B     XIT                                                              
         SPACE 1                                                                
REPINV   MVC   PUNION,INUN                                                      
         MVC   PEMP,INEMP                                                       
         MVC   PAGENCY,INAGY                                                    
         MVC   PINVOICE(6),ININV                                                
         MVC   PCLIENT,INCLI                                                    
         MVC   PCID,INCID                                                       
         MVC   PUSE,INUSE                                                       
         CLC   INCYCSTR,=C'000000'                                              
         BE    REPAMT                                                           
         GOTO1 DATCON,DMCB,(0,INCYCSTR),(8,PCYCLE)                              
         CLC   INCYCEND,=C'000000'                                              
         BE    REPAMT                                                           
         MVI   PCYCLE+8,C'-'                                                    
         GOTO1 DATCON,DMCB,(0,INCYCEND),(8,PCYCLE+9)                            
         SPACE 1                                                                
REPAMT   PACK  DUB,INGROSS                                                      
         EDIT  (P8,DUB),(13,PGROSS),2,MINUS=YES,ZERO=BLANK                      
         PACK  DUB,INMISC                                                       
         EDIT  (P8,DUB),(13,PMISC),2,MINUS=YES,ZERO=BLANK                       
         PACK  DUB,INSPNH                                                       
         EDIT  (P8,DUB),(13,PSPNH),2,MINUS=YES,ZERO=BLANK                       
         PACK  DUB,INPNH                                                        
         EDIT  (P8,DUB),(13,PPNH),2,MINUS=YES,ZERO=BLANK                        
         BAS   RE,SPLAT                                                         
         CLI   TTTYPE,C'1'                                                      
         BE    XIT                                                              
         BAS   RE,SPLAT                                                         
         B     XIT                                                              
         SPACE 1                                                                
REPGRAND PACK  DUB,GTGROSS                                                      
         EDIT  (P8,DUB),(13,PGROSS),2,MINUS=YES,ZERO=BLANK                      
         PACK  DUB,GTMISC                                                       
         EDIT  (P8,DUB),(13,PMISC),2,MINUS=YES,ZERO=BLANK                       
         PACK  DUB,GTSPNH                                                       
         EDIT  (P8,DUB),(13,PSPNH),2,MINUS=YES,ZERO=BLANK                       
         PACK  DUB,GTPNH                                                        
         EDIT  (P8,DUB),(13,PPNH),2,MINUS=YES,ZERO=BLANK                        
         BAS   RE,SPLAT                                                         
         BAS   RE,SPLAT                                                         
         B     XIT                                                              
REPCONT  PACK  DUB,T7SPNH                                                       
         EDIT  (P8,DUB),(13,PSPNH),2,MINUS=YES,ZERO=BLANK                       
         PACK  DUB,T7PNH                                                        
         EDIT  (P8,DUB),(13,PPNH),2,MINUS=YES,ZERO=BLANK                        
         BAS   RE,SPLAT                                                         
         BAS   RE,SPLAT                                                         
         B     XIT                                                              
         EJECT                                                                  
                                                                                
*                                                                               
CLOSTAPE NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R2,P                                                             
         USING PRINTD,R2                                                        
         EDIT  (P6,TAPCOUNT),(7,PTOTALS),ZERO=NOBLANK                           
         MVC   PTOTALS+8(12),=C'TAPE RECORDS'                                   
         DROP  R2                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         CLI   TAPEOPT,C'Y'                                                     
         BNE   CLOSXIT                                                          
         L     R2,=A(UNITAPE)                                                   
         CLI   SFTPOPT,C'Y'        MQ?                                          
         BNE   *+8                                                              
         L     R2,=A(UNISFTP)                                                   
         CLI   ACTEQU,ACTDOWN      DOWNLOADING?                                 
         BNE   *+8                 NO                                           
         L     R2,=A(TADOWN)       YES, USE TEMPORARY DATASET                   
         CLOSE ((2))                                                            
CLOSXIT  XIT1                                                                   
*                                                                               
*                                                                               
         EJECT                                                                  
*              UTILITIES                                                        
         SPACE 3                                                                
CLEARTAP NTR1                                                                   
         LA    R2,TAPEIO                                                        
         LA    R0,7                                                             
         SPACE 1                                                                
CLEARTP2 MVC   0(100,R2),SPACES                                                 
         LA    R2,100(R2)                                                       
         BCT   R0,CLEARTP2                                                      
         B     XIT                                                              
         SPACE 1                                                                
CONVDATE NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         MVC   0(6,R3),=C'000000'                                               
         OC    0(3,R2),0(R2)                                                    
         BZ    XIT                                                              
         GOTO1 DATCON,DMCB,(1,0(R2)),(X'20',0(R3))                              
         B     XIT                                                              
         SPACE 1                                                                
CONVCASH NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         L     R4,0(R2)                                                         
         EDIT  (R4),(9,0(R3)),FILL=0                                            
         NI    8(R3),X'CF'         CONVERT SIGN                                 
         LTR   R4,R4                                                            
         BNM   XIT                                                              
         OI    8(R3),X'10'         NEGATIVE                                     
         B     XIT                                                              
         EJECT                                                                  
*              UTILITIES                                                        
         SPACE 3                                                                
*              ODD ROUTINES                                                     
         SPACE 3                                                                
                                                                                
SPLAT    NTR1                                                                   
*        CLI   ACTEQU,ACTDOWN                                                   
*        BE    XIT                                                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         SPACE 1                                                                
                                                                                
INVERR   MVI   ERROR,INVALID                                                    
         GOTO1 ERREX                                                            
         SPACE 1                                                                
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 1                                                                
         EJECT                                                                  
*              HEADLINES ETC                                                    
         SPACE 3                                                                
HOOK     NTR1                                                                   
         MVC   H1+52(24),=CL24'UNION TAPE'                                      
         GOTO1 CENTER,DMCB,H1+52,24                                             
         GOTO1 UNDERLIN,DMCB,(24,H1+52),(X'BF',H2+52)                           
         MVC   H3+52(6),=C'PERIOD'                                              
         L     R1,APERH                                                         
         MVC   H3+59(17),8(R1)                                                  
         B     XIT                                                              
         SPACE 1                                                                
         EJECT                                                                  
*                                                                               
         SPACE 1                                                                
MYSPECS  SSPEC H1,1,RUN                                                         
         SSPEC H3,1,REQUESTOR                                                   
         SSPEC H1,99,AGYNAME                                                    
         SSPEC H2,99,AGYADD                                                     
         SSPEC H3,99,REPORT                                                     
         SSPEC H3,112,PAGE                                                      
         SSPEC H5,7,C'UN. EMP AGENCY INVOICE CLIENT COMMERCIAL   USE'           
         SSPEC H6,7,C'--- --- CODE   NUMBER  CODE      ID.       ---'           
         SSPEC H5,60,C'CYCLE       GROSS AMOUNT  MISCELLANOUS'                  
         SSPEC H6,60,C'-----       ------------     AMOUNT'                     
         SSPEC H5,102,C'SUBJECT TO       P AND H'                               
         SSPEC H6,102,C'  P AND H        -------'                               
         DC    H'0'                                                             
APERH    DS    A                                                                
         LTORG                                                                  
         DROP  R6                  DROP SECONDARY BASE REGISTERS                
         EJECT                                                                  
*              LTORG ETC                                                        
         SPACE 3                                                                
*                                  COUNTS                                       
*                                                                               
*              OTHER AREAS NOT DIRECTLY ADDRESSABLE                             
*                                                                               
         ENTRY UNITAPE                                                          
*                                                                               
*  UNITAPE DEFINED AS TAPE FOR UT REPORT.                                       
*  UNITAPE DEFINED AS DISK FOR UD REPORT.                                       
*                                                                               
UNITAPE  DCB   DDNAME=UNITAPE,DSORG=PS,MACRF=(PM),                     X        
               RECFM=FB,LRECL=700,BUFNO=2,BLKSIZE=7000                          
*                                                                               
*  TADOWN IS USED FOR OD REPORT.                                                
*                                                                               
TADOWN   DCB   DDNAME=TADOWN,DSORG=PS,MACRF=(GM,PM),                   X        
               RECFM=FB,LRECL=700,BUFNO=2,BLKSIZE=7000,EODAD=NOMORE             
*                                                                               
*  UNISFTP IS USED FOR MQ                                                       
*                                                                               
UNISFTP  DCB   DDNAME=UNISFTP,DSORG=PS,MACRF=(GM,PM),                  X        
               RECFM=FB,LRECL=700,BUFNO=2,BLKSIZE=7000,EODAD=NOMORE             
         EJECT                                                                  
*---------------------------------------------------------------------          
*              VALIDATE RECORD                                                  
*---------------------------------------------------------------------          
         USING CONHEADH-64,R6                                                   
VREC     NTR1  BASE=*,LABEL=*                                                   
*                                  OPTIONAL FIELDS                              
*                                                                               
         LA    R2,SPLPERH          PERIOD                                       
         GOTO1 ANY                 (REQUIRED)                                   
         ST    R2,APERH                                                         
         BRAS  RE,VSFTDAT          SOFTDATE                                     
         JE    VREC1                                                            
         GOTO1 VALPERD                                                          
VREC1    MVI   TIQDTYPE,TIQDCHK    (FILTER ON CHECK DATE)                       
*                                                                               
VREC2    LA    R2,SPLUNH           UNION                                        
         CLI   5(R2),0                                                          
         JE    VREC4                                                            
         CLC   8(3,R2),=C'ALL'                                                  
         JE    VREC4                                                            
         GOTO1 ANY                                                              
         MVI   CONTFLAG,C'N'                                                    
         CLC   =C'SAG',WORK        ALL SAG REQUESTS                             
         JNE   *+8                                                              
*        CLI   ACTEQU,ACTDOWN                                                   
*        JNE   *+8                                                              
         MVI   CONTFLAG,C'Y'                                                    
                                                                                
*                                                                               
         MVC   TIFUN,WORK                                                       
         GOTO1 UNIVAL,DMCB,8(R2)                                                
         SPACE 1                                                                
VREC4    LA    R2,SPLLCLH          LOCAL                                        
         CLI   5(R2),0                                                          
         JE    VREC5                                                            
         OC    TIFUN,TIFUN         CAN'T HAVE LOCAL WITHOUT UNION               
         JZ    INVERR                                                           
         GOTO1 RECVAL,DMCB,TLLOCDQ,(R2),0                                       
         MVC   TIFLOCL,TGLCL                                                    
         SPACE 1                                                                
VREC5    LA    R2,SPLAGGH          AGENCY GROUP                                 
         CLI   5(R2),0                                                          
         JE    VREC6                                                            
         CLC   8(3,R2),=C'ALL'                                                  
         JE    VREC6                                                            
         GOTO1 ANY                                                              
         MVC   TIFAGG,WORK                                                      
         GOTO1 RECVAL,DMCB,TLAGCDQ,(R2),0                                       
         SPACE 1                                                                
VREC6    LA    R2,SPLAGYH          AGENCY                                       
         CLI   5(R2),0                                                          
         JE    VREC8                                                            
         CLC   8(3,R2),=C'ALL'                                                  
         JE    VREC8                                                            
         MVI   GRNYOPT,C'N'                                                     
         CLC   8(4,R2),=C'GRNY'                                                 
         JNE   *+8                                                              
         MVI   GRNYOPT,C'Y'                                                     
         LA    R3,TIFAGY                                                        
         LA    R4,L'TIFAGY                                                      
         LA    R5,TLAYCDQ                                                       
         BRAS  RE,SPECFILT                                                      
*        OI    TIQFLAG2,TIQFSUB    SET TO PASS SUBS OF SPLITS                   
*                                                                               
VREC8    LA    R2,SPLEMPH          EMPLOYER                                     
         CLI   5(R2),0                                                          
         JE    VREC10                                                           
         CLC   8(3,R2),=C'ALL'                                                  
         JE    VREC10                                                           
         GOTO1 ANY                                                              
         MVC   TIFEMP,WORK                                                      
         CLI   WORK,C'-'                                                        
         JE    VREC8M                                                           
         GOTO1 RECVAL,DMCB,TLEMCDQ,(R2),0                                       
         J     VREC10                                                           
         SPACE 1                                                                
VREC8M   MVC   TIFEMP,WORK+1                                                    
         NI    TIFEMP,X'FF'-X'40'                                               
         SPACE 1                                                                
VREC10   LA    R2,SPLOPTH          VALIDATE OPTIONS                             
         BRAS  RE,VOPTS                                                         
         J     XIT                                                              
         EJECT                                                                  
*              VALIDATE A FILTER EXPRESSION                                     
         SPACE 3                                                                
*              INPUT               R2=A(HEADER)                                 
*                                  R3=A(SYSIO FILTER AREA)                      
*                                  R4=L'ABOVE                                   
*                                  R5=RECORD TYPE CODE                          
         SPACE 1                                                                
SPECFILT NTR1  BASE=*,LABEL=*                                                   
         CLI   5(R2),0             ANY DATA                                     
         BE    FILTX                                                            
         OI    6(R2),X'80'                                                      
         GOTO1 ANY                 PUT INTO WORK                                
         BCTR  R4,0                (L'FILTER - 1)                               
         CLC   WORK(2),=C'-@'      CHECK FOR NEGATIVE LIST                      
         BE    NEGLIST                                                          
         CLC   WORK(2),=C'@-'                                                   
         BE    NEGLIST                                                          
         CLI   WORK,C'-'           CHECK FOR NEGATIVE FILTER                    
         BE    NEGFILT                                                          
         CLI   WORK,C'@'           CHECK FOR POSITIVE LIST                      
         BE    POSLIST                                                          
         SPACE 1                                                                
POSFILT  EX    R4,*+8              POSITIVE FILTER                              
         B     *+10                                                             
         MVC   0(0,R3),WORK                                                     
         LA    R4,WORK                                                          
         B     ALLFILT                                                          
         SPACE 1                                                                
NEGFILT  EX    R4,*+8              NEGATIVE FILTER                              
         B     *+10                                                             
         MVC   0(0,R3),WORK+1                                                   
         NI    0(R3),X'FF'-X'40'                                                
         LA    R4,WORK+1                                                        
         SPACE 1                                                                
ALLFILT  LTR   R5,R5                                                            
         BZ    FILTX                                                            
         CHI   R5,1                                                             
         BE    ALLUNVL                                                          
         CHI   R5,2                                                             
         BE    ALLUSEVL                                                         
         GOTO1 RECVAL,DMCB,(R5),(X'80',(R4)),0                                  
         JNE   INVERR                                                           
         B     FILTX                                                            
         SPACE 1                                                                
ALLUNVL  GOTO1 UNIVAL,DMCB,(R4)                                                 
         JNE   INVERR                                                           
         B     FILTX                                                            
         SPACE 1                                                                
ALLUSEVL GOTO1 USEVAL,DMCB,(X'40',(R4))                                         
         JNE   INVERR                                                           
         B     FILTX                                                            
         SPACE 1                                                                
POSLIST  EX    R4,*+8              POSITIVE LIST                                
         B     *+10                                                             
         MVC   0(0,R3),WORK+1                                                   
         NI    0(R3),X'FF'-X'80'                                                
         LA    R5,WORK+1                                                        
         B     VALLIST                                                          
         SPACE 1                                                                
NEGLIST  EX    R4,*+8              NEGATIVE LIST                                
         B     *+10                                                             
         MVC   0(0,R3),WORK+2                                                   
         NI    0(R3),X'FF'-X'80'-X'40'                                          
         LA    R5,WORK+2                                                        
         SPACE 1                                                                
VALLIST  LA    R1,1(R5,R4)         CHECK CODE IS NOT TOO LONG                   
         CLI   0(R1),C' '                                                       
         JH    INVERR                                                           
         XC    KEY,KEY             CHECK LIST EXISTS                            
         LA    R3,KEY                                                           
         USING TLGLD,R3                                                         
         MVI   TLGLCD,TLGLCDQ                                                   
         MVI   TLGLTYPE,TLGLTYPF                                                
         MVC   TLGLLST,0(R5)                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(32),KEYSAVE                                                  
         JNE   INVERR                                                           
FILTX    XIT1                                                                   
         SPACE 1                                                                
         EJECT                                                                  
EDITDET  NTR1 BASE=*,LABEL=*                                                    
*                                  R6=A(PAYMENT DETAIL ELEMENT)                 
*                                  R2=A(OUTPUT AREA)                            
         USING TAPDD,R6                                                         
         OC    TAPDSTUS,TAPDSTUS   *** FUDGE                                    
         BNZ   INPDU10                                                          
         OC    TAPDUNIT(3),TAPDUNIT                                             
         BNZ   INPDU20                                                          
***      GOTO1 USEVAL,DMCB,TAPDUSE,0                                            
***      TM    TGUSTYST,MAJORS                                                  
***      BO    INPDU20                                                          
***      TM    TGUSTYST,USES                                                    
***      BO    INPDU10                                                          
         J     XIT                                                              
         SPACE 1                                                                
INPDU10  CLC   TAPDUSES,=H'1'                                                   
         BNE   INPDU12                                                          
         MVC   0(3,R2),=C'USE'                                                  
         EDIT  (2,TAPDSTUS),(4,4(R2)),ALIGN=LEFT                                
         J     XIT                                                              
         SPACE 1                                                                
INPDU12  MVC   0(4,R2),=C'USES'                                                 
         LA    R2,5(R2)                                                         
         LH    R3,TAPDSTUS                                                      
         EDIT  (R3),(4,0(R2)),ALIGN=LEFT                                        
         AR    R2,R0                                                            
         MVI   0(R2),C'-'                                                       
         AH    R3,TAPDUSES                                                      
         BCTR  R3,0                                                             
         EDIT  (R3),(4,1(R2)),ALIGN=LEFT                                        
         J     XIT                                                              
         SPACE 1                                                                
INPDU20  GOTO1 MAJVAL,DMCB,(X'80',TAPDMAJ)                                      
         MVC   0(L'TGMACHAR,R2),TGMACHAR                                        
         LA    R2,L'TGMACHAR-1(R2)                                              
         CLI   0(R2),C' '                                                       
         BNE   *+8                                                              
         BCT   R2,*-8                                                           
         LA    R2,2(R2)                                                         
         OC    TAPDUNIT,TAPDUNIT                                                
         JZ    XIT                                                              
         EDIT  (2,TAPDUNIT),(4,0(R2)),ALIGN=LEFT,ZERO=NOBLANK                   
         AR    R2,R0                                                            
         MVC   1(5,R2),=C'UNITS'                                                
         J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* READ INVOICE RETRO IS FOR TO GET CHECK DATE ON ORIGINAL             *         
* FOR OPTION RETROPH ONLY                                             *         
***********************************************************************         
RDR4INV  NTR1  BASE=*,LABEL=*                                                   
         MVC   BYTE,ELCODE       SAVE OFF ELCODE BEFORE CALL                    
         OC    CRINVNUM,CRINVNUM                                                
         JNZ   *+14                                                             
         OC    RET4INV,RET4INV                                                  
         JZ    XIT                                                              
*                                                                               
         MVC   AIO,AIO2                                                         
RDR4IN01 XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING TLIND,R6                                                         
         MVI   TLINCD,TLINCDQ                                                   
         MVC   TLINAGY,INAGY                                                    
         MVC   TLININV,RET4INV                                                  
         OC    RET4INV,RET4INV                                                  
         JNZ   *+10                                                             
         MVC   TLININV,CRINVNUM                                                 
         XC    TLININV,=X'FFFFFFFFFFFF'                                         
         GOTO1 HIGH                                                             
         CLC   KEY(TLINLEN-TLIND),KEYSAVE                                       
***      BE    *+6                                                              
***      DC    H'00'                                                            
         BE    *+8                                                              
         B     *+4                                                              
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,TAINELQ                                                   
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         USING TAIND,R6                                                         
         OC    CRINVNUM,CRINVNUM                                                
         BZ    RDR4IN10                                                         
         GOTOR CONVDAT2,DMCB,TAINCDTE,INCRCKDT                                  
         B     RDR4IN20                                                         
RDR4IN10 GOTOR CONVDAT2,DMCB,TAINCDTE,INRECKDT                                  
RDR4IN20 MVC   AIO,AIO1                                                         
         DROP  R6                                                               
         MVC   ELCODE,BYTE    RESTORE ELCODE BEFORE RETURNING                   
         J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CONVERT DATE FORMAT                                                 *         
***********************************************************************         
CONVDAT2 NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R3,0(R1)                                                      
         MVC   0(6,R3),=C'000000'                                               
         OC    0(3,R2),0(R2)                                                    
         JZ    XIT                                                              
         GOTO1 DATCON,DMCB,(1,0(R2)),(20,WORK)                                  
         MVC   0(6,R3),WORK+2                                                   
         J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SORT UTILITIES                                                      *         
***********************************************************************         
SORTPUT  NTR1  BASE=*,LABEL=*                                                   
         LA    R5,TAPEIO                                                        
         USING TAPED,R5                                                         
         OC    INUN,INUN           NEED A UNION HERE                            
         JZ    XIT                                                              
*                                                                               
         CLI   SORTFRST,C'Y'                                                    
         BNE   SORTPUT2                                                         
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD                                     
         MVI   SORTFRST,C'N'                                                    
*                                                                               
SORTPUT2 GOTO1 SORTER,DMCB,=C'PUT',TAPEIO                                       
         J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
SORTMOVE NTR1  BASE=*,LABEL=*                                                   
         LA    R4,TAPEIO                                                        
         MOVE  (TAPEIO,700),0(R2)                                               
         J     XIT                                                              
         DS    0F                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,20,A),FORMAT=BI,EQUALS'                      
***SORTCARD DC    CL80'SORT FIELDS=(1,21,A),FORMAT=BI,EQUALS'                   
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=(700)'                                 
         LTORG                                                                  
         EJECT                                                                  
*              TAPE ROUTINES                                                    
OPENTAPE NTR1  BASE=*,LABEL=*                                                   
         CLI   TAPEOPT,C'Y'                                                     
         BNE   OPENXIT                                                          
         L     R2,=A(TADOWN)       START WITH TEMPORARY DATASET                 
         CLI   ACTEQU,ACTDOWN      DOWNLOADING?                                 
         BE    OPEN2               YES, SKIP THIS                               
*                                                                               
         L     R1,DRONE            USING UNUSED CORERES AREA                    
         CLI   0(R1),X'90'         TO SAVE RESULTS FILE NUMBER                  
         BNE   *+8                 BETWEEN REQUESTS                             
         MVI   0(R1),0                                                          
         ZIC   RF,0(R1)                                                         
         LA    RF,1(RF)                                                         
         STC   RF,0(R1)                                                         
         BCTR  RF,0                                                             
         CLI   SFTPOPT,C'Y'                                                     
         BNE   OPEN1                                                            
         GOTO1 DATCON,DMCB,(1,TIQPSTR),(X'20',DYDDSN5)                          
         GOTO1 DATCON,DMCB,(1,TIQPEND),(X'20',DYDDSN6)                          
         CLI   SFTPTEST,C'Y'       RUN=TEST?                                    
         BNE   *+10                                                             
         MVC   DYDDSN2,=C'TEST.'                                                
         MVC   WORK,SPACES                                                      
         MVC   WORK(DYDDSNQ),DYDDSN                                             
         MVC   DUB,=X'000005000001'    PRI=5,SEC=1                              
         GOTO1 DYNALLOC,DMCB,(X'80',=CL8'UNISFTP'),(X'41',DUB),        X        
               (X'80',WORK)                                                     
         L     R2,=A(UNISFTP)                                                   
         MVC   DSNME,=CL8'UNISFTP'                                              
         B     OPEN2                                                            
OPEN1    MVC   WORK(20),=CL20'TALTAPE.TA0UTDS1'                                 
         GOTO1 DYNALLOC,DMCB,(0,=CL8'UNITAPE'),((RF),WORK)                      
         L     R2,=A(UNITAPE)                                                   
*                                                                               
OPEN2    OPEN  ((2),OUTPUT)                                                     
         CLI   POWOPT,C'Y'                                                      
         BE    *+12                                                             
         CLI   SFTPOPT,C'Y'                                                     
         BNE   OPENXIT                                                          
         LINKX MF=(E,PARMLST),SF=(E,LINKLST)                                    
OPENXIT  XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
PUTTAPE  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R0,17               RECORD IS 704 BYTES LONG (40X17+24)          
         LA    R5,TAPEIO                                                        
         LA    RE,700              LENGTH OF TAPEIO                             
         USING TAPED,R5                                                         
*                                                                               
         CLI   TTTYPE,C'1'                                                      
         BE    *+10                                                             
         MVC   INCONTYP,SPACES                                                  
*                                                                               
         CLI   TTTYPE,C'5'         IF SPECIAL UNION PRINTING TOTAL              
         BNE   PUTTAPE1                                                         
         CLC   TTAGY,=C'999998'                                                 
         BE    PUTXIT              DON'T PUT TO TAPE                            
*                                                                               
PUTTAPE1 CLI   NODTLOPT,C'Y'       IF NO DETAIL RECORDS TO TAPE                 
         BNE   PUTTAPE3                                                         
         CLI   CATYPE,C'2'         SKIP 2 & 3 (PROCESS 1,4,5&6)                 
         BE    PUTXIT                                                           
         CLI   CKTYPE,C'3'                                                      
         BE    PUTXIT                                                           
*                                                                               
PUTTAPE3 CLI   0(R5),0             IF NULLS                                     
         BNE   *+8                                                              
         MVI   0(R5),C' '          SET TO SPACES                                
         LA    R5,1(R5)                                                         
         BCT   RE,PUTTAPE3                                                      
*                                                                               
         AP    TAPCOUNT,=P'1'                                                   
         MVC   RECTYPE,=CL16'OUTPUT'                                            
*                                                                               
         CLI   TAPEOPT,C'Y'                                                     
         BNE   PUTXIT                                                           
         LA    R0,TAPEIO                                                        
         L     R1,=A(UNITAPE)                                                   
         CLI   SFTPOPT,C'Y'        MQ?                                          
         BNE   *+8                                                              
         L     R1,=A(UNISFTP)                                                   
         CLI   ACTEQU,ACTDOWN      DOWNLOADING?                                 
         BNE   *+8                 NO                                           
         L     R1,=A(TADOWN)       YES, USE TEMPORARY DATASET                   
* ONLY PUT RECORD WIHOUT THE 1ST CONTRACT TYPE BYTE                             
         MVC   SAVEKEY,TAPEIO                                                   
         LA    RE,TAPEIO                                                        
         MVC   0(L'TAPEKEY-1,RE),1(RE)                                          
         MVI   L'TAPEKEY-1(RE),X'40'                                            
*                                                                               
         PUT   (1),(0)                                                          
         MVC   TAPEIO(L'SAVEKEY),SAVEKEY  RESTORE THE KEY BACK                  
PUTXIT   XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
         SPACE 1                                                                
***********************************************************************         
* WRITE OUT TO THE MQ FILE                                            *         
***********************************************************************         
MQRPT    NTR1  BASE=*,LABEL=*                                                   
         GOTO1 VMQRPT,DMCB,(0,=C'OPEN'),(0,MQFILID),(X'E0',0),0                 
         CLI   DMCB+8,0            DID MQ OPEN FAIL?                            
         BE    *+14                                                             
         MVC   MQERRMSG,OPENFAIL                                                
         B     MQERREND                                                         
*                                                                               
         ZAP   FULL,TAPCOUNT                                                    
         OI    FULL+L'FULL-1,X'0F'                                              
         UNPK  MQMTREC,FULL                                                     
         MVC   MQMDSN,SPACES                                                    
         MVC   MQMDSN(L'RETAREA-14),RETAREA+14                                  
         LA    R3,MQMESS+MQMLNQ-1                                               
         ST    R3,FULL                                                          
         BRAS  RE,FNDNXT                                                        
         L     R3,FULL                                                          
         LA    R5,MQMESS                                                        
         SR    R3,R5                                                            
         GOTO1 VMQRPT,DMCB,(0,=C'PUT'),MQMESS,(R3),0                            
         CLI   DMCB+8,0            DID MQ PUT FAIL?                             
         BE    *+14                                                             
         MVC   MQERRMSG,PUTFAIL                                                 
         B     MQERREND                                                         
*                                                                               
         GOTO1 VMQRPT,DMCB,(0,=C'CLOSE'),0,0,0                                  
         CLI   DMCB+8,0            DID MQ CLOSE FAIL?                           
         BE    MQRPTX                                                           
         MVC   MQERRMSG,CLOSFAIL                                                
*                                                                               
MQERREND BAS   RE,SENDMAIL         E-MAIL ERROR REPORT                          
         DC    H'0'                                                             
*                                                                               
MQRPTX   XIT1                                                                   
***********************************************************************         
* MQ CONSTANTS                                                        *         
***********************************************************************         
MQFILID  DC    CL16'EDIHUBSFTP******'                                           
MQERRMSG DS    CL15                                                             
OPENFAIL DC    CL(L'MQERRMSG)'MQ OPEN FAILED'                                   
PUTFAIL  DC    CL(L'MQERRMSG)'MQ PUT ERROR'                                     
CLOSFAIL DC    CL(L'MQERRMSG)'MQ CLOSE FAILED'                                  
***********************************************************************         
* MQ TABLES                                                           *         
***********************************************************************         
MQMESS   DS    0C                                                               
         DC    CL6'SFTPID'         RECORD TYPE                                  
MQMRECTY DC    CL8'AFTRA'          MQ KEY                                       
         DC    CL8' '                                                           
MQMTREC  DC    CL8' '              TOTAL RECORD COUNT                           
MQMDSN   DC    CL64' '             DATASET NAME                                 
MQMLNQ   EQU   *-MQMESS                                                         
         DC    AL1(EOF)                                                         
***********************************************************************         
* OTHER ROUTINES                                                      *         
* FULL CONTAINS THE ADDRESS                                           *         
***********************************************************************         
         SPACE 2                                                                
FNDNXT   L     R1,FULL                                                          
FNDNXT5  CLI   0(R1),C' '          FIND LAST CHARACTER                          
         JH    FNDNXT10                                                         
         BCTR  R1,0                                                             
         J     FNDNXT5                                                          
FNDNXT10 LA    R1,1(R1)            R5 TO NEXT SPACE                             
         ST    R1,FULL             STORE ADDR OF NEXT SPACE                     
         BR    RE                                                               
         SPACE 2                                                                
**********************************************************************          
* E-MAIL ERROR REPORT                                                *          
**********************************************************************          
         SPACE 1                                                                
SENDMAIL NTR1                                                                   
*                                                                               
         GOTOR VSMTP,DMCB,('SMTPAINI',JESMAIL)                                  
*                                                                               
         GOTOR VSMTP,DMCB,('SMTPAPRS',TOWHO),(L'SUBJDSC,SUBJDSC)                
*                                                                               
         MVC   BODYERR,MQERRMSG    MOVE IN ERROR MSG                            
         GOTOR VSMTP,DMCB,('SMTPAPTL',BODYLIN1)                                 
*                                                                               
         GOTOR VSMTP,DMCB,('SMTPASND',0)                                        
         GOTOR VSMTP,DMCB,('SMTPAEND',0) DETACH SMTP                            
SENDMX   XIT1                                                                   
*                                                                               
JESMAIL  DC    CL8'JESMAIL '                                                    
*                                                                               
TOWHO    DC    C'US-TALENT_TEAM:'                                               
*                                                                               
SUBJDSC  DC    0CL80                                                            
         DC    C'MQ ERROR FOR UNDISK REPORT'                                    
         DC    CL(L'SUBJDSC-(*-SUBJDSC))' '     SPARE SPACES                    
BODYLIN1 DC    0CL80                                                            
BODYERR  DC    C'               '               ERROR MESSAGE                   
         DC    CL(L'BODYLIN1-(*-BODYLIN1))' '   SPARE SPACES                    
*                                                                               
         EJECT                                                                  
***********************************************************************         
*              VALIDATE OPTIONS                                       *         
***********************************************************************         
         SPACE 3                                                                
VOPTS    NTR1 BASE=*,LABEL=*                                                    
         MVI   TAPEOPT,C'Y'                                                     
         MVI   SORTOPT,C'N'                                                     
         MVI   LISTOPT,C'N'                                                     
         MVI   BILLOPT,C'N'                                                     
         MVI   SEQOPT,C'N'                                                      
         MVI   SESSOPT,C'N'                                                     
         MVI   RETROOPT,C'N'                                                    
         MVI   NODTLOPT,C'N'                                                    
         MVI   TRPWOPT,C'N'                                                     
         MVI   POWOPT,C'N'                                                      
         MVI   SFTPOPT,C'N'                                                     
*                                                                               
         CLI   RECNUM,UD           UNDISK CAN POWWOW                            
         BNE   OPT3                                                             
         CLI   ACTEQU,ACTDOWN      DOWNLOADING, NO POWWOW                       
         BE    OPT3                                                             
         CLC   TIFUN,=C'SAG'       SAG AND UNDISK                               
         BNE   OPT3                                                             
         MVI   POWOPT,C'Y'                                                      
*                                                                               
OPT3     MVI   PLNOPT,0                                                         
         CLI   5(R2),0                                                          
         BE    OPTX                                                             
         GOTO1 SCANNER,DMCB,(R2),(10,BLOCK),0                                   
         ZIC   R0,4(R1)                                                         
         LA    R4,BLOCK                                                         
         LTR   R0,R0                                                            
         BZ    INVERR                                                           
         SPACE 1                                                                
OPT4     CLC   12(5,R4),=C'LIMIT'  LIMIT OPTION                                 
         BNE   OPT5                                                             
         L     R1,8(R4)            (CHECK NUMERIC)                              
         LTR   R1,R1                                                            
         BZ    INVERR                                                           
         CVD   R1,DUB                                                           
         B     OPTEND                                                           
         SPACE 1                                                                
OPT5     CLC   12(6,R4),=C'REPORT' REPORT LIMIT                                 
         BNE   OPT6                                                             
         L     R1,8(R4)            (CHECK NUMERIC)                              
         LTR   R1,R1                                                            
         BZ    INVERR                                                           
         CVD   R1,DUB                                                           
         B     OPTEND                                                           
         SPACE 1                                                                
OPT6     CLC   12(4,R4),=C'TAPE'   TAPE OPTION                                  
         BNE   OPT7                                                             
         CLI   ACTEQU,ACTDOWN      TAPE NOT VALID OPTION FOR THIS               
         BE    INVERR                                                           
         MVC   TAPEOPT,22(R4)                                                   
         B     OPTEND                                                           
         SPACE 1                                                                
OPT7     CLC   12(4,R4),=C'SORT'   SORT OPTION                                  
         BNE   OPT8                                                             
         MVI   SORTOPT,C'Y'                                                     
         B     OPTEND                                                           
         SPACE 1                                                                
         SPACE 1                                                                
OPT8     CLC   12(4,R4),=C'LIST'   LIST OPTION                                  
         BNE   OPT10                                                            
         MVI   LISTOPT,C'Y'                                                     
         B     OPTEND                                                           
         SPACE 1                                                                
OPT10    CLC   12(4,R4),=C'BILL'   BILL OPTION                                  
         BNE   OPT12                                                            
         MVI   BILLOPT,C'Y'                                                     
         MVI   TIQDTYPE,TIQDBILL   (SET TO READ BILL DATE POINTERS)             
         B     OPTEND                                                           
         SPACE 1                                                                
OPT12    CLC   12(3,R4),=C'SEQ'    SEQUENCE OPTION                              
         BNE   OPT14                                                            
         MVC   SEQOPT,22(R4)                                                    
         B     OPTEND                                                           
         SPACE 1                                                                
OPT14    CLC   12(7,R4),=C'SESSION' SESSIONS ONLY OPTION                        
         BNE   OPT16                                                            
         MVI   SESSOPT,C'Y'                                                     
         B     OPTEND                                                           
         SPACE 1                                                                
OPT16    CLC   12(7,R4),=C'RETROPH'   RETROS ONLY P&H DIFF OPTION               
         BNE   OPT16Y                                                           
         MVI   RETROOPT,C'D'                                                    
         MVI   POWOPT,C'N'          CANNOT POWWOW                               
         B     OPTEND                                                           
         SPACE 1                                                                
OPT16Y   CLC   12(5,R4),=C'RETRO'   RETROS ONLY OPTION                          
         BNE   OPT17                                                            
         MVI   RETROOPT,C'Y'                                                    
         MVI   POWOPT,C'N'          CANNOT POWWOW                               
         B     OPTEND                                                           
         SPACE 1                                                                
OPT17    CLC   12(7,R4),=C'RETALSO' ALSO RETROS OPTION                          
         BNE   OPT18                                                            
         MVI   RETROOPT,C'A'                                                    
         B     OPTEND                                                           
         SPACE 1                                                                
OPT18    CLC   12(5,R4),=C'TYPE1'   PUT TYPE1,4,5,6 TO TAPE                     
         BNE   OPT20                                                            
         MVI   NODTLOPT,C'Y'        (NO DETAIL LEVEL - SKIP 2 & 3)              
         B     OPTEND                                                           
*                                                                               
OPT20    CLC   12(5,R4),=C'SPCL4'   PAX AND LATE NIGHT USES ONLY                
         BNE   OPT22                                                            
         OI    PLNOPT,X'80'                                                     
         B     OPTEND                                                           
*                                                                               
OPT22    CLC   12(3,R4),=C'PAX'     PAX USES ONLY                               
         BNE   OPT24                                                            
         TM    PLNOPT,X'80'                                                     
         BZ    OPTEND                                                           
         OI    PLNOPT,X'40'                                                     
         B     OPTEND                                                           
*                                                                               
OPT24    CLC   12(5,R4),=C'NOPOW'   NO POWWOW                                   
         BNE   OPT26                                                            
         MVI   POWOPT,C'N'                                                      
         B     OPTEND                                                           
*                                                                               
OPT26    CLC   12(5,R4),=C'TRPOW'   TRACE POWWOW                                
         BNE   OPT27                                                            
         MVI   TRPWOPT,C'Y'                                                     
         B     OPTEND                                                           
*                                                                               
OPT27    CLC   12(4,R4),=C'SFTP'   SFTP (MQ)                                    
         BNE   OPT90                                                            
         MVI   SFTPOPT,C'Y'                                                     
         B     OPTEND                                                           
*                                                                               
OPT90    DS    0H                                                               
OPTEND   LA    R4,32(R4)                                                        
         BCT   R0,OPT4                                                          
OPTX     XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================          
*              VALIDATE USING SOFT DATES                                        
*              R2 = FIELD HEADER                                                
*=====================================================================          
         USING SOFDATD,R1                                                       
VSFTDAT  NTR1  BASE=*,LABEL=*                                                   
         L     R6,ATWA                                                          
         USING CONHEADH-64,R6                                                   
         LA    R1,SDBLOCK                                                       
         XC    SOFDATD(SOFDATL),SOFDATD                                         
         ST    R2,SOFAINP          A(INPUT)                                     
         LA    R3,OUTDATE                                                       
         ST    R3,SOFAOUT          A(OUTPUT)                                    
         MVC   SOFACOM,ACOMFACS    A(COMFACS)                                   
         MVI   SOFITYPE,SOFITYMD   VALIDATE FOR YEAR, MONTH, DAY                
         MVI   SOFOTYPE,SOFOTSD2   12 BYTE EBCIDIC (YYMMDDYYMMDD)               
         MVI   SOFIINDS,SOFIISFT   VALIDATE ONLY SOFT DATES                     
*                                                                               
         MVC   SOFTODAY,TGTODAY0   TODAY'S DATE                                 
         MVC   SOFCTRY,CTRY        COUNTRY CODE                                 
         MVC   SOFLANG,LANG        LANGUAGE CODE                                
         MVI   SOFSYSN,7           TALENT SYSTEM                                
         GOTO1 SOFTDATE,SOFDATD                                                 
         BZ    *+14                                                             
         MVC   ERROR,SOFERROR                                                   
         B     NO                                                               
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BE    VSFTDAT5                                                         
         CLI   CONOUTH+5,0         RLP ONLINE, IF GROUP NAME IN OUTPUT          
         BE    VSFTDAT5                                                         
         CLC   =C'FILE',CONDEST    AND FILE IN DESTINATION                      
         BE    YES                 DON'T RESOLVE DATES                          
*                                                                               
VSFTDAT5 OI    SOFIINDS,SOFIIRES   RESOLVE THE DATES TO ACTUAL                  
         GOTO1 SOFTDATE,SOFDATD                                                 
         GOTO1 DATCON,DMCB,(0,OUTDATE),(1,TIQPSTR)                              
         GOTO1 DATCON,DMCB,(0,OUTDATE+6),(1,TIQPEND)                            
*                                                                               
YES      SR    R1,R1                                                            
         B     *+8                                                              
*                                                                               
NO       LA    R1,1                                                             
         LTR   R1,R1                                                            
         XIT1                                                                   
*                                                                               
         DROP  R1                                                               
         LTORG                                                                  
SDBLOCK  DS    CL(SOFXTNL)       SOFTDATE BLOCK                                 
OUTDATE  DS    CL12                                                             
*                                                                               
         EJECT                                                                  
*              ROUTINE TO SUBMIT JCL TO POWWOW                                  
*              FOR ADVANTIS TRANSMISSION                                        
*                                                                               
CALLPOW  NTR1  BASE=*,LABEL=*                                                   
         XC    HEADHOOK,HEADHOOK                                                
         MVC   TAPEC4,RTGEN        UPDATE GENERATION NUMBER                     
         MVC   FILE3(5),RTGEN      "                                            
         LA    R0,NUMCRD           R0=N'OF JCL CARDS                            
         LA    R3,JCL              R3=A(JCL FOR POWWOW)                         
*                                                                               
CALLP5   CLI   TRPWOPT,C'Y'        IF TRACING REQUESTED                         
         BNE   CALLP8                                                           
         MVC   P(L'POWJCL),0(R3)             PRINT JCL                          
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
CALLP8   MVC   POWJCL,0(R3)                                                     
         CLI   POWOPT,C'Y'         UNLESS OTHERWISE REQUESTED                   
         BNE   CALLP10                                                          
         GOTO1 =V(POWWOW),DMCB,=C'PUT',=C'POWER',POWKEY,POWHEAD                 
*                                                                               
CALLP10  LA    R3,L'JCL(R3)                                                     
         BCT   R0,CALLP5                                                        
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
JCL      DS    0CL80                                                            
JOBC     DC    CL80'//TPCHTUDP  JOB   ''CNTL'',MSGLEVEL=(1,1),COND=((0,X        
               NE)),'                                                           
         DC    CL80'//     MSGCLASS=X,CLASS=A,NOTIFY=CCOL'                      
         DC    CL80'//*MAIN CLASS=ADVANTIS,SYSTEM=SY1'                          
         DC    CL80'//SS  EXEC  BDEDICT'                                        
         DC    CL80'//EXTSYSIN DD *'                                            
*                                                                               
EDICT1   DC    C'EDICTKEY='                                                     
EDICT2   DC    C'FTPINTER'                                                      
EDICT3   DC    CL(80-(*-EDICT1))' '                                             
*                                                                               
SUBJ1    DC    C'SUBJECT=SAG INTERFACE'                                         
         DC    CL(80-(*-SUBJ1))' '                                              
*                                                                               
FILE1    DC    C'FILE='                                                         
FILE2    DC    C'TA0UDDS1'                                                      
         DC    C'.'                                                             
FILE3    DC    C'G0000'                                                         
FILE4    DC    CL(80-(*-FILE1))' '                                              
*                                                                               
         DC    CL80'EXT=ROE'                                                    
*                                                                               
TAPEC1   DC    C'DSN='                                                          
TAPEC2   DC    CL16'TALDISK.TA0UDDS1'                                           
TAPEC3   DC    CL1'.'                                                           
TAPEC4   DC    CL8'G0000000'                                                    
TAPEC5   DC    CL(80-(*-TAPEC1))' '                                             
*                                                                               
**********************************************************************          
* DATASET                                                            *          
**********************************************************************          
DYDDSN   DS    0C                                                               
DYDDSN1  DC    CL9'SFTPDISK.'                                                   
DYDDSN2  DC    CL5'PROD.'          PROD OR TEST BASED ON THE RUN                
DYDDSN3  DC    CL6'TALUD.'                                                      
DYDDSN4  DC    CL6'AFTRA.'                                                      
         DC    C'S'                                                             
DYDDSN5  DC    CL6'YYMMDD'         PERIOD START                                 
         DC    C'.E'                                                            
DYDDSN6  DC    CL6'YYMMDD'         PERIOD END                                   
DYDDSNQ  EQU   *-DYDDSN                                                         
*                                                                               
NUMCRD   EQU   (*-JCL)/80                                                       
*                                                                               
         DC    XL32'00'                                                         
POWKEY   DC    CL10' '                                                          
POWHEAD  DC    XL8'00'                                                          
POWJCL   DS    CL80                                                             
*                                                                               
PARMLST  CALL  ,(DSNME,RETAREA),MF=L                                            
LINKLST  LINKX EP=FRGETDSN,SF=L                                                 
*                                                                               
DSNME    DC    CL8'UNITAPE'                                                     
RETAREA  DS    0CL44                                                            
RTDSN    DC    CL17' '                                                          
RTGEN    DC    CL8' '        G0000000                                           
RTND     DC    CL19' '       SPARE                                              
ERRCHK   DS    CL1                                                              
         EJECT                                                                  
         USING TASDD,R6                                                         
DETSDR   NTR1  BASE=*,LABEL=*                                                   
         CLI   GRNYOPT,C'Y'                                                     
         BNE   DETSD2                                                           
         EDIT  (1,TASDOT),(2,CKOVRT),FILL=0                                     
         EDIT  (1,TASDTRV),(2,CKTRVT),FILL=0                                    
DETSD2   CLI   TASDEQU,UBSS                                                     
         BE    DETBSS                                                           
         CLI   TASDEQU,UBSM                                                     
         BE    DETBSM                                                           
         CLI   TASDEQU,UIMS                                                     
         BE    DETBSM                                                           
         CLI   TASDEQU,UBSR                                                     
         BE    DETBSR                                                           
         B     DETSDRX                                                          
*                                                                               
DETBSS   EDIT  (1,TASDSP),(2,CKSPOTS),FILL=0                                    
         EDIT  (1,TASDDAY),(2,CKDAYS),FILL=0                                    
         B     DETSDRX                                                          
*                                                                               
DETBSM   EDIT  (1,TASDMSP),(2,CKSPOTS),FILL=0                                   
         EDIT  (2,TASDMHM),(4,CKHOURS),FILL=0                                   
         B     DETSDRX                                                          
*                                                                               
DETBSR   EDIT  (1,TASDRSP),(2,CKSPOTS),FILL=0                                   
         EDIT  (2,TASDRHM),(4,CKHOURS),FILL=0                                   
*                                                                               
DETSDRX  XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*              ROUTINE TO GET FROM DATASET AND PRINT SECOND REPORT              
*                                                                               
PREPD    NMOD1 0,*PREPD*,R7                                                     
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R6,ATWA                                                          
         USING CONHEADH-64,R6                                                   
*                                                                               
         BRAS  RE,NEWPRTQ           SET NEW PRINT QUEUE REPORT                  
         GOTO1 REQTWA,DMCB,(3,ATWA),,VPRINT,(C'B',ABOX)                         
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 SPOOL,DMCB,(R8)                                                  
         DROP  R6                                                               
*                                                                               
         L     R2,=A(TADOWNG)                                                   
         OPEN  ((2),INPUT)                                                      
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R3,DLBLOCK           INITIALIZE DLFLD                            
         USING DLCBD,R3                                                         
         BAS   RE,INITDWN                                                       
*                                                                               
PREPD2   GET   (R2),TAPEIO         GET RECORD FROM TEMP DATASET                 
         LA    R5,TAPEIO                                                        
*                                                                               
         CLI   RETROOPT,C'Y'       IF OPTION TO GET ONLY RETROS                 
         BE    PREPD23                                                          
         CLI   RETROOPT,C'D'       DO A SPECIAL DOWNLOAD                        
         BNE   PREPD24                                                          
PREPD23  BAS   RE,PREPRD           PRINT RETRO DOWNLOADBLE REPORT               
         B     PREPD2                                                           
*                                                                               
PREPD24  LA    R0,17               RECORD IS 704 BYTES LONG (40X17+24)          
*                                                                               
PREPD25  MVI   DLCBACT,DLCBPUT     PUT ITEM TO PRINT LINE                       
         MVI   DLCBTYP,DLCBTXT     DATA TYPE IS TEXT                            
*        MVI   DLCBTYP,DLCBNUM     DATA TYPE IS NUMERIC                         
         MVI   DLCBLEN,40                                                       
         MVC   DLCBFLD(40),0(R5)    PASS DATA                                   
         GOTO1 =V(DLFLD),DLCBD                                                  
*                                                                               
         LA    R5,40(R5)           BUMP TO NEXT TAPE FIELD                      
         BCT   R0,PREPD25          GET NEXT FIELD                               
*                                                                               
         MVI   DLCBACT,DLCBPUT     PUT ITEM TO PRINT LINE                       
         MVI   DLCBTYP,DLCBTXT     DATA TYPE IS TEXT                            
*        MVI   DLCBTYP,DLCBNUM     DATA TYPE IS NUMERIC                         
         MVI   DLCBLEN,20                                                       
         MVC   DLCBFLD(20),0(R5)    PASS DATA                                   
         GOTO1 =V(DLFLD),DLCBD                                                  
*                                                                               
         MVI   DLCBACT,DLCBEOL     END OF LINE                                  
         GOTO1 =V(DLFLD),DLCBD                                                  
         B     PREPD2              GET NEXT RECORD                              
         SPACE 2                                                                
PREPDX   XIT1                                                                   
         SPACE 2                                                                
                                                                                
*              CALL DLFLD TO INITIALISE REPORT                                  
         SPACE 1                                                                
INITDWN  NTR1                                                                   
         XC    DLCBD(DLCBXLX),DLCBD                                             
*                                                                               
         MVI   DLCBACT,DLCBINIT    INITIALIZE FOR DOWNLOAD                      
         LA    R1,SPLATDWN         A(HOOK ROUTINE FOR PRINTING)                 
         ST    R1,DLCBAPR                                                       
         LA    R1,P                A(PRINT LINE)                                
         ST    R1,DLCBAPL                                                       
         MVC   DLCBAED,EDITOR                                                   
         MVC   DLCXMAXL,=Y(L'P)    MAXIMUM LENGTH OF PRINT LINE                 
         MVI   DLCXDELC,C' '       DELIMITER                                    
         MVI   DLCXEOTC,C'"'       TEXT DELIMITER                               
         MVI   DLCXEOTA,C''''      TEXT DELIMITER ALTERNATE                     
         MVI   DLCXEOLC,X'5E'      SEMI-COLON FOR END OF LINE                   
         MVI   DLCXEORC,C':'       END OF REPORT                                
         GOTO1 =V(DLFLD),DLCBD                                                  
         B     PREPDX                                                           
         EJECT                                                                  
*              USER SUPPLIED PRINT ROUTINE - DOWNLOAD                           
         SPACE 1                                                                
SPLATDWN NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   LINE,1              PREVENT PAGE BREAK                           
         B     PREPDX                                                           
         SPACE 2                                                                
*              DATA SET ROUTINE                                                 
         SPACE 1                                                                
NOMORE2  CLOSE ((2))               CLOSE DATASET                                
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVI   DLCBACT,DLCBEOR     END OF REPORT                                
         GOTO1 =V(DLFLD),DLCBD                                                  
         B     PREPDX                                                           
         EJECT                                                                  
*                                                                               
TADOWNG  DCB   DDNAME=TADOWN,DSORG=PS,MACRF=(GM),                      X        
               RECFM=FB,LRECL=700,BUFNO=2,BLKSIZE=7000,EODAD=NOMORE2            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*              ROUTINE TO GET FROM DATASET AND PRINT SECOND REPORT              
*                                                                               
         USING DLCBD,R3                                                         
         USING TAPED,R5                                                         
PREPRD   NTR1                                                                   
*                                             D/L DOESNT SAVE CONTRACT          
         MVC   SAVEKEY,SPACES                   SHIFT 1 BYTE OVER               
         MVC   SAVEKEY(L'TAPEKEY),TAPEIO                                        
         MVI   TAPEIO,C' '                                                      
         MVC   TAPEIO+1(L'TAPEKEY-1),SAVEKEY                                    
*                                                                               
         CLI   INTYPE,C'1'                                                      
         BNE   PRRD20                                                           
         GOTO1 OUTPDOWN,DMCB,(C'T',INUN),L'INUN                                 
         GOTO1 (RF),(R1),(C'T',INEMP),L'INEMP                                   
         GOTO1 (RF),(R1),(C'T',INAGY),L'INAGY                                   
         GOTO1 (RF),(R1),(C'T',ININV),L'ININV                                   
         GOTO1 (RF),(R1),(C'T',INTYPE),L'INTYPE                                 
         GOTO1 FIXAMT,DMCB,INGROSS                                              
         GOTO1 (RF),(R1),INMISC                                                 
         GOTO1 (RF),(R1),INSPNH                                                 
         GOTO1 (RF),(R1),INPNH                                                  
         GOTO1 (RF),(R1),INTAX                                                  
         GOTO1 (RF),(R1),INHAND                                                 
         GOTO1 (RF),(R1),INAMT                                                  
         GOTO1 OUTPDOWN,DMCB,(C'T',INBILDTE),L'INBILDTE                         
         GOTO1 (RF),(R1),(C'T',INCLI),L'INCLI                                   
         GOTO1 (RF),(R1),(C'T',INPRD),L'INPRD                                   
         GOTO1 (RF),(R1),(C'T',INUSE),L'INUSE                                   
         GOTO1 (RF),(R1),(C'T',INUTYP),L'INUTYP                                 
         GOTO1 (RF),(R1),(C'T',INWEEKS),L'INWEEKS                               
         GOTO1 (RF),(R1),(C'T',INCYCSTR),L'INCYCSTR                             
         GOTO1 (RF),(R1),(C'T',INCYCEND),L'INCYCEND                             
         GOTO1 (RF),(R1),(C'T',INMEDIA),L'INMEDIA                               
         GOTO1 (RF),(R1),(C'T',INCTYPE),L'INCTYPE                               
         GOTO1 (RF),(R1),(C'T',INFLMDTE),L'INFLMDTE                             
         GOTO1 (RF),(R1),(C'T',INRECDTE),L'INRECDTE                             
         GOTO1 (RF),(R1),(C'T',INFFCDTE),L'INFFCDTE                             
         GOTO1 (RF),(R1),(C'T',INDUBDTE),L'INDUBDTE                             
         GOTO1 (RF),(R1),(C'T',INAIRDTE),L'INAIRDTE                             
         GOTO1 (RF),(R1),(C'T',INCUNITS),L'INCUNITS                             
         GOTO1 (RF),(R1),(C'T',INCUUNTS),L'INCUUNTS                             
         GOTO1 (RF),(R1),(C'T',INVER),L'INVER                                   
         GOTO1 (RF),(R1),(C'T',INCRIND),L'INCRIND                               
         GOTO1 (RF),(R1),(C'T',INCANIND),L'INCANIND                             
         GOTO1 (RF),(R1),(C'T',INCABUP),L'INCABUP                               
         GOTO1 (RF),(R1),(C'T',INAGYNM),L'INAGYNM                               
         GOTO1 (RF),(R1),(C'T',INYEAR),L'INYEAR                                 
         GOTO1 (RF),(R1),(C'T',INCID),L'INCID                                   
         GOTO1 (RF),(R1),(C'T',INTITLE),L'INTITLE                               
         GOTO1 (RF),(R1),(C'T',INCLINM),L'INCLINM                               
         GOTO1 (RF),(R1),(C'T',INPRDNM),L'INPRDNM                               
         GOTO1 (RF),(R1),(C'T',INEST),L'INEST                                   
         GOTO1 (RF),(R1),(C'T',INPO),L'INPO                                     
         GOTO1 (RF),(R1),(C'T',INLFTID),L'INLFTID                               
         GOTO1 (RF),(R1),(C'T',INFSTUD),L'INFSTUD                               
         GOTO1 (RF),(R1),(C'T',INRSTUD),L'INRSTUD                               
         GOTO1 (RF),(R1),(C'T',INAFMC1),L'INAFMC1                               
         GOTO1 (RF),(R1),(C'T',INAFMC2),L'INAFMC2                               
         GOTO1 (RF),(R1),(C'T',INCOMM),L'INCOMM                                 
         GOTO1 (RF),(R1),(C'T',INFCITY),L'INFCITY                               
         GOTO1 (RF),(R1),(C'T',INRCITY),L'INRCITY                               
         GOTO1 (RF),(R1),(C'T',INPDESC),L'INPDESC                               
         GOTO1 (RF),(R1),(C'T',INUSDESC),L'INUSDESC                             
         GOTO1 (RF),(R1),(C'T',INEMPNM),L'INEMPNM                               
         GOTO1 (RF),(R1),(C'T',INTID),L'INTID                                   
         GOTO1 (RF),(R1),(C'T',INTLEN),L'INTLEN                                 
         GOTO1 (RF),(R1),(C'T',INADST),L'INADST                                 
         GOTO1 (RF),(R1),(C'T',INCOSEC),L'INCOSEC                               
         GOTO1 (RF),(R1),(C'T',INLFSEC),L'INLFSEC                               
         GOTO1 (RF),(R1),(C'T',INTAGS),L'INTAGS                                 
         GOTO1 (RF),(R1),(C'T',INLOCS),L'INLOCS                                 
         GOTO1 (RF),(R1),(C'T',INSIGN),L'INSIGN                                 
         GOTO1 (RF),(R1),(C'T',INSIGNM),L'INSIGNM                               
         GOTO1 (RF),(R1),(C'T',INSSORRS),L'INSSORRS                             
         GOTO1 (RF),(R1),(C'T',INGST),L'INGST                                   
         GOTO1 (RF),(R1),(C'T',INXUTYP),L'INXUTYP                               
         GOTO1 (RF),(R1),(C'T',INRECKDT),L'INRECKDT                             
         B     PRRD60                                                           
PRRD20   CLI   INTYPE,C'2'                                                      
         BNE   PRRD30                                                           
         GOTO1 OUTPDOWN,DMCB,(C'T',CAUN),L'CAUN                                 
         GOTO1 (RF),(R1),(C'T',CAEMP),L'CAEMP                                   
         GOTO1 (RF),(R1),(C'T',CAAGY),L'CAAGY                                   
         GOTO1 (RF),(R1),(C'T',CAINV),L'CAINV                                   
         GOTO1 (RF),(R1),(C'T',CATYPE),L'CATYPE                                 
         LA    R0,20                                                            
         LA    R2,CAUSES                                                        
PRRD23   GOTO1 OUTPDOWN,DMCB,(C'T',0(R2)),4                                     
         LA    R2,4(R2)                                                         
         BCT   R0,PRRD23                                                        
         LA    R0,20                                                            
         LA    R2,CADATES                                                       
PRRD24   GOTO1 OUTPDOWN,DMCB,(C'T',0(R2)),4                                     
         LA    R2,4(R2)                                                         
         BCT   R0,PRRD24                                                        
         GOTO1 OUTPDOWN,DMCB,(C'T',CAFROM),L'CAFROM                             
         GOTO1 (RF),(R1),(C'T',CATO),L'CATO                                     
         GOTO1 (RF),(R1),(C'T',CALFROM),L'CALFROM                               
         GOTO1 (RF),(R1),(C'T',CALTO),L'CALTO                                   
         LA    R0,20                                                            
         LA    R2,CAPROGS                                                       
PRRD25   GOTO1 OUTPDOWN,DMCB,(C'T',0(R2)),20                                    
         LA    R2,20(R2)                                                        
         BCT   R0,PRRD25                                                        
         B     PRRD60                                                           
PRRD30   CLI   INTYPE,C'3'                                                      
         BNE   PRRD40                                                           
         GOTO1 OUTPDOWN,DMCB,(C'T',CKUN),L'CKUN                                 
         GOTO1 (RF),(R1),(C'T',CKEMP),L'CKEMP                                   
         GOTO1 (RF),(R1),(C'T',CKAGY),L'CKAGY                                   
         GOTO1 (RF),(R1),(C'T',CKINV),L'CKINV                                   
         GOTO1 (RF),(R1),(C'T',CKTYPE),L'CKTYPE                                 
         GOTO1 FIXAMT,DMCB,CKGROSS                                              
         GOTO1 (RF),(R1),CKMISC                                                 
         GOTO1 (RF),(R1),CKSPNH                                                 
         GOTO1 (RF),(R1),CKPNH                                                  
         GOTO1 OUTPDOWN,DMCB,(C'T',CKSSN),L'CKSSN                               
         GOTO1 (RF),(R1),(C'T',CKCORPID),L'CKCORPID                             
         GOTO1 (RF),(R1),(C'T',CKOV1),L'CKOV1                                   
         GOTO1 (RF),(R1),(C'T',CKOV2),L'CKOV2                                   
         GOTO1 (RF),(R1),(C'T',CKONOFF),L'CKONOFF                               
         GOTO1 (RF),(R1),(C'T',CKMINOR),L'CKMINOR                               
         GOTO1 (RF),(R1),(C'T',CKAGENT),L'CKAGENT                               
         GOTO1 (RF),(R1),(C'T',CKSTATE),L'CKSTATE                               
         GOTO1 (RF),(R1),(C'T',CKLOCAL),L'CKLOCAL                               
         GOTO1 (RF),(R1),(C'T',CKVER),L'CKVER                                   
         GOTO1 (RF),(R1),(C'T',CKMULTI),L'CKMULTI                               
         GOTO1 (RF),(R1),(C'T',CKSWEET),L'CKSWEET                               
         GOTO1 (RF),(R1),(C'T',CKDUBS),L'CKDUBS                                 
         GOTO1 (RF),(R1),(C'T',CKHOURS),L'CKHOURS                               
         GOTO1 (RF),(R1),(C'T',CKSPOTS),L'CKSPOTS                               
         GOTO1 (RF),(R1),(C'T',CKFRSDTE),L'CKFRSDTE                             
         GOTO1 (RF),(R1),(C'T',CKDAYS),L'CKDAYS                                 
         GOTO1 (RF),(R1),(C'T',CKAPPCDE),L'CKAPPCDE                             
         GOTO1 (RF),(R1),(C'T',CKINCCDE),L'CKINCCDE                             
         GOTO1 FIXAMT,DMCB,(C'T',CKPAID),L'CKPAID                               
         GOTO1 OUTPDOWN,DMCB,(C'T',CKLAST),L'CKLAST                             
         GOTO1 (RF),(R1),(C'T',CKFIRST),L'CKFIRST                               
         GOTO1 (RF),(R1),(C'T',CKCAT),L'CKCAT                                   
         GOTO1 (RF),(R1),(C'T',CKEMPNM),L'CKEMPNM                               
         GOTO1 (RF),(R1),(C'T',CKYEAR),L'CKYEAR                                 
         GOTO1 (RF),(R1),(C'T',CKACCNO),L'CKACCNO                               
         GOTO1 (RF),(R1),(C'T',CKCORPNM),L'CKCORPNM                             
         GOTO1 (RF),(R1),(C'T',CKSTATE2),L'CKSTATE2                             
         GOTO1 (RF),(R1),(C'T',CKUNLOC),L'CKUNLOC                               
         GOTO1 (RF),(R1),(C'T',CKFROM),L'CKFROM                                 
         GOTO1 (RF),(R1),(C'T',CKTO),L'CKTO                                     
         GOTO1 (RF),(R1),(C'T',CKOVRT),L'CKOVRT                                 
         GOTO1 (RF),(R1),(C'T',CKTRVT),L'CKTRVT                                 
         GOTO1 (RF),(R1),(C'T',CKUDUES),L'CKUDUES                               
         GOTO1 (RF),(R1),(C'T',CKGST),L'CKGST                                   
         B     PRRD60                                                           
PRRD40   CLI   INTYPE,C'4'                                                      
         BE    PRRD45                                                           
         CLI   INTYPE,C'5'                                                      
         BNE   PRRD50                                                           
PRRD45   GOTO1 OUTPDOWN,DMCB,(C'T',TTUN),L'TTUN                                 
         GOTO1 (RF),(R1),(C'T',TTEMP),L'TTEMP                                   
         GOTO1 (RF),(R1),(C'T',TTAGY),L'TTAGY                                   
         GOTO1 (RF),(R1),(C'T',TTINV),L'TTINV                                   
         GOTO1 (RF),(R1),(C'T',TTTYPE),L'TTTYPE                                 
         GOTO1 FIXAMT,DMCB,TTGROSS                                              
         GOTO1 (RF),(R1),TTMISC                                                 
         GOTO1 (RF),(R1),TTSPNH                                                 
         GOTO1 (RF),(R1),TTPNH                                                  
         GOTO1 (RF),(R1),TTGST                                                  
         B     PRRD60                                                           
PRRD50   CLI   INTYPE,C'6'                                                      
         BNE   PRRDX                                                            
         GOTO1 OUTPDOWN,DMCB,(C'T',TTUN),L'TTUN                                 
         GOTO1 (RF),(R1),(C'T',TTEMP),L'TTEMP                                   
         GOTO1 (RF),(R1),(C'T',TTAGY),L'TTAGY                                   
         GOTO1 (RF),(R1),(C'T',TTINV),L'TTINV                                   
         GOTO1 (RF),(R1),(C'T',GTTYPE),L'GTTYPE                                 
         GOTO1 FIXAMT2,DMCB,GTGROSS                                             
         GOTO1 (RF),(R1),GTMISC                                                 
         GOTO1 (RF),(R1),GTSPNH                                                 
         GOTO1 (RF),(R1),GTPNH                                                  
         GOTO1 (RF),(R1),GTGST                                                  
PRRD60   BAS   RE,EOLDOWN                                                       
         SPACE 2                                                                
PRRDX    XIT1                                                                   
         DROP R3,R5                                                             
         SPACE 2                                                                
*                                                                               
***********************************************************************         
*        GETS RID OF PACKED CHARACTERS IN AMOUNT                                
*        AMOUNT IS SAVED IN SVAMT AND DOWNLOADED                                
***********************************************************************         
                                                                                
FIXAMT   NTR1                                                                   
         L     R1,0(R1)                                                         
         ZAP   DUB,=P'0'                                                        
         CLC   0(9,R1),SPACES                                                   
         BE    FAMT10                                                           
         PACK  DUB,0(9,R1)                                                      
FAMT10   EDIT  (P8,DUB),(13,SVAMT),2,MINUS=YES,ZERO=NOBLANK                     
         GOTO1 OUTPDOWN,DMCB,(C'T',SVAMT),L'SVAMT                               
         J     PRRDX                                                            
***********************************************************************         
*        GETS RID OF PACKED CHARACTERS IN AMOUNT                                
*        AMOUNT IS SAVED IN SVAMT AND DOWNLOADED                                
***********************************************************************         
                                                                                
FIXAMT2  NTR1                                                                   
         L     R1,0(R1)                                                         
         ZAP   DUB,=P'0'                                                        
         CLC   0(10,R1),SPACES                                                  
         BE    FAMT210                                                          
         PACK  DUB,0(10,R1)                                                     
FAMT210  EDIT  (P8,DUB),(13,SVAMT),2,MINUS=YES,ZERO=NOBLANK                     
         GOTO1 OUTPDOWN,DMCB,(C'T',SVAMT),L'SVAMT                               
         J     PRRDX                                                            
***********************************************************************         
*        FINISH LINE OF DOWNLOAD OUPUT                                *         
***********************************************************************         
                                                                                
         USING DLCBD,R3                                                         
EOLDOWN  NTR1                                                                   
         MVI   DLCBACT,DLCBEOL      END OF LINE                                 
         GOTO1 =V(DLFLD),DLCBD      (DON'T USE RF, BASR RE,RF BELOW)            
         J     PRRDX                                                            
         SPACE 1                                                                
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*        OUTPUT DOWNLOAD ENTRY                                        *         
*        ON ENTRY ... P1, B0    = TYPE TO PASS                        *         
*                         B1-B3 = ADDRESS OF DATA                     *         
*                     P2        = LENGTH                              *         
***********************************************************************         
                                                                                
         USING DLCBD,R3                                                         
OUTPDOWN NTR1                                                                   
         BRAS  RE,OUTPDWN                                                       
         GOTO1 =V(DLFLD),DLCBD                                                  
         J     PRRDX                                                            
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        OUTPUT DOWNLOAD ENTRY                                        *         
***********************************************************************         
         USING DLCBD,R3                                                         
OUTPDWN  NTR1  BASE=*,LABEL=*                                                   
         MVI   DLCBACT,DLCBPUT                                                  
         MVC   DLCBTYP,0(R1)                                                    
         OI    DLCBFLG1,DLCBFXFL                                                
                                                                                
         L     RF,0(R1)            RF=A(DOWNLOAD FIELD)                         
         L     RE,4(R1)            RE=L'DOWNLOAD FIELD                          
                                                                                
         LR    R1,RF                                                            
         AR    R1,RE                                                            
OPD10    SHI   R1,1                                                             
         CLI   0(R1),C' '                                                       
         JNE   OPD20                                                            
         BCT   RE,OPD10                                                         
         LHI   RE,1                                                             
OPD20    STC   RE,DLCBLEN                                                       
                                                                                
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   DLCBFLX(0),0(RF)                                                 
         J     XIT                                                              
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*              ROUTINE TO SET UP SECOND REPORT ON QUEUE                         
*                                                                               
NEWPRTQ  NTR1  BASE=*,LABEL=*                                                   
         L     R2,ALOGOC                                                        
         USING LOGOD,R2                                                         
         MVI   LOGOTYPE,C'E'                                                    
         GOTO1 ALOGO,DMCB,(R2)     END REPORT LOGOS                             
*                                                                               
         TM    WHEN,X'20'          SOON?                                        
         BO    NPQ010                                                           
         GOTO1 VPRINT,DMCB,=C'CLOSE'                                            
*                                                                               
NPQ010   XC    SPECS,SPECS         CLEAR SPECS                                  
         XC    HEADHOOK,HEADHOOK   AND HEADLINE HOOK                            
*                                                                               
         L     R2,ABOX                                                          
         USING BOXD,R2                                                          
         LA    RE,132                                                           
         ST    RE,BOXWIDTH         SET LENGTH OF PRINT LINE                     
*                                                                               
         L     RF,AMASTD                                                        
         USING MASTD,RF                                                         
         L     R2,AREMOT                                                        
         USING REMOTED,R2                                                       
         TM    WHEN,X'20'          SOON?                                        
         BZ    NPQ050                                                           
         XC    MCREMPQK,MCREMPQK                                                
         B     NPQ060                                                           
*                                                                               
NPQ050   MVC   REMOTABF,MCVPQBUF   OVERNIGHT                                    
         MVC   REMOTADM,MCVDMGR                                                 
         MVC   REMOTAOP,MCVPQOPN                                                
         MVC   REMOTDST,MCDESTID                                                
         XC    MCALTREF,MCALTREF                                                
         OI    REMOTTYP,X'10'      DOWNLOAD                                     
         L     RF,MCVLOGO          DOWN/SQL - PATCH LOGOS NOT TO PRINT          
         MVC   0(2,RF),=X'07FE'                                                 
         LA    RF,MCREMOTE                                                      
*                                                                               
         MVI   REMOTCPY,C'1'                                                    
         MVI   REMOTCLS,C'Q'                                                    
         MVC   REMOTJID,=C'TOD'                                                 
NPQ060   MVC   REMOTKEY(11),SPACES                                              
         MVC   REMOTSYS(6),=C'UNDATA'                                           
         MVC   REMOTFRM(4),=C'DATA'                                             
         J     XIT                                                              
         DROP  R2                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO CONVERT MAJORS                                              
***********************************************************************         
         USING TAPED,R5                                                         
CONVMAJ  NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R3,0(R1)         R2=A(MAJORS), R3=A(TAPE FIELD)               
         MVI   0(R3),C'0'          INIT TAPE FIELD                              
         CLI   0(R2),0             DONE IF NO MAJORS                            
         BE    CONVX                                                            
         CLC   INUSE,=C'FGR'       IF FGR USE                                   
         BNE   *+12                                                             
         CLI   INUTYP,C'0'         AND TYPE IS 0(LOCATIONS LIKE MAJORS)         
         BE    CONVMAJ4            HANDLE DIFFERENTLY                           
         LA    R4,MAJTAB                                                        
         SPACE 1                                                                
CONVMAJ2 MVC   0(1,R3),1(R4)                                                    
         CLC   0(1,R2),0(R4)                                                    
         BE    CONVX                                                            
         CLI   0(R2),X'FF'                                                      
         BE    CONVX                                                            
         LA    R4,2(R4)                                                         
         B     CONVMAJ2                                                         
         SPACE 1                                                                
CONVMAJ4 LA    R4,LOCTAB           R4 = A(LOCATION TABLE)                       
         LA    R3,INLOCS           R3 = A(LOCATION FIELDS ON TAPE)              
         LA    R0,L'INLOCS         R0 = TOTAL NUM OF LOCATIONS                  
CONVMAJ5 MVC   BYTE,0(R2)          LOCATIONS FOR USE                            
         NC    BYTE,0(R4)                                                       
         BZ    *+8                                                              
         MVI   0(R3),C'1'          SET TAPE FIELD TO 1                          
         LA    R4,1(R4)            BUMP TO NEXT LOCATION TABLE ENTRY            
         LA    R3,1(R3)            BUMP TO NEXT LOCATION FIELD ON TAPE          
         BCT   R0,CONVMAJ5         LOOP FOR TOTAL N'LOCATIONS                   
CONVX    XIT1                                                                   
         DROP  R5                                                               
         SPACE 1                                                                
MAJTAB   DS    0H                                                               
         DC    X'80',C'1'          NY                                           
         DC    X'20',C'2'          CHI                                          
         DC    X'A0',C'3'          NY + CHI                                     
         DC    X'40',C'4'          LA                                           
         DC    X'C0',C'5'          NY + LA                                      
         DC    X'60',C'6'          CHI + LA                                     
         DC    X'E0',C'7'          ALL                                          
         DC    X'FF',C'0'          BOO BOO                                      
         SPACE 1                                                                
LOCTAB   DS    0H                                                               
         DC    AL1(UK)                                                          
         DC    AL1(EUR)                                                         
         DC    AL1(JAP)                                                         
         DC    AL1(AP)                                                          
         DC    AL1(REST)                                                        
         EJECT                                                                  
*              FLESH OUT RECORDS WITH NAMES ETC                                 
         SPACE 3                                                                
FLESHOUT NTR1  BASE=*,LABEL=*                                                   
         LA    R5,TAPEIO                                                        
         USING TAPED,R5                                                         
         CLI   INTYPE,C'3'                                                      
         BE    FLESHCK                                                          
         CLI   INTYPE,C'1'                                                      
         BNE   FLESHX                                                           
         SPACE 1                                                                
*                                  INVOICE HEADERS                              
         BAS   RE,NEEDCL                                                        
         MVC   INCLINM,NEEDNAME                                                 
         CLI   INPRDNM,C' '        (MAY HAVE PRODUCT NAME)                      
         BH    FLESHIN2                                                         
*                                                                               
         BAS   RE,NEEDPR                                                        
         MVC   INPRDNM,NEEDNAME                                                 
*                                                                               
FLESHIN2 BAS   RE,NEEDAY                                                        
         MVC   INAGYNM,NEEDNAME                                                 
*                                                                               
         MVC   INEMPNM(15),=C'TALENT PARTNERS'                                  
         BAS   RE,NEEDOFF                                                       
*                                                                               
         CLC   INSIGN,SPACES       ANY SIGNATORY?                               
         BH    *+10                YES                                          
         MVC   INSIGN,INAGY        NO, USE AGENCY AND NAME                      
*****                                                                           
*****    CLC   INUSE,=C'INA'       ALL THESE TYPES HAVE 0101 SIGNATORY          
*****    BE    FLESHIN4                                                         
*****    CLC   INUSE,=C'INR'                                                    
*****    BE    FLESHIN4                                                         
*****    CLC   INUSE,=C'INS'                                                    
*****    BE    FLESHIN4                                                         
*****    CLC   INUSE,=C'ISS'                                                    
*****    BNE   FLESHIN6                                                         
*                                                                               
         CLI   INCTYPE,C'I'        IF COMMERCIAL TYPE IS INDUSTRIAL             
         BE    *+8                                                              
         CLI   INCTYPE,C'1'                                                     
         BE    *+8                                                              
         CLI   INCTYPE,C'2'                                                     
         BNE   FLESHIN6                                                         
*                                                                               
         TM    SAVESTA6,TAAYSTPS   AND STATUS TPSIG IS ON                       
         BNO   FLESHIN6                                                         
*                                                                               
FLESHIN4 MVC   INSIGN,SPACES          CLEAR FIELD                               
         MVC   INSIGN(4),=CL4'0101'   THESE TYPES GET 0101                      
*                                                                               
FLESHIN6 BAS   RE,NEEDSG                                                        
         MVC   INSIGNM,NEEDSHRT                                                 
         CLC   NEEDSHRT,SPACES     SHORT NAME?                                  
         BH    *+10                YES                                          
         MVC   INSIGNM,NEEDNAME    NOPE, USE AGENCY                             
         B     FLESHX                                                           
*                                                                               
FLESHCK  LA    R2,CKSSN            INVOICE DETAILS (CHECKS)                     
         BAS   RE,NEEDW4                                                        
         L     R6,NEEDAREC                                                      
         MVI   ELCODE,TAW4ELQ                                                   
         BRAS  RE,GETEL                                                         
         USING TAW4D,R6                                                         
         MVC   CKLAST,TAW4NAM2                                                  
         MVC   CKFIRST,TAW4NAM1                                                 
         CLI   CKCORPID,C'0'                                                    
         BL    FLESHCK2                                                         
         LA    R2,CKCORPID                                                      
         BAS   RE,NEEDW4                                                        
         MVC   CKCORPNM,NEEDNAME                                                
         SPACE 1                                                                
FLESHCK2 LA    R2,CKEMP                                                         
         BAS   RE,NEEDEM                                                        
         MVC   CKEMPNM,NEEDNAME                                                 
         BAS   RE,NEEDLO                                                        
         L     R6,NEEDAREC                                                      
         MVI   ELCODE,TANUELQ                                                   
                                                                                
         BRAS  RE,GETEL                                                         
         BNE   FLESHX                                                           
         USING TANUD,R6                                                         
         ZIC   R1,TANULEN                                                       
         SH    R1,=H'4'                                                         
         CH    R1,=H'9'                                                         
         BL    *+8                                                              
         LA    R1,9                                                             
         EX    R1,*+8                                                           
         B     FLESHX                                                           
         MVC   CKACCNO(0),TANUMBER                                              
                                                                                
                                                                                
FLESHX   XIT1                                                                   
         EJECT                                                                  
*              ROUTINES TO ENSURE SUB RECORDS AROUND                            
         SPACE 3                                                                
NEEDLO   NTR1                                                                   
         XC    NEEDKEY,NEEDKEY     ENSURE LOCAL AROUND                          
         LA    R4,NEEDKEY                                                       
         USING TLLOD,R4                                                         
         MVI   TLLOCD,TLLOCDQ                                                   
         MVC   TLLOUN,CKACCNO                                                   
         MVC   TLLOLCL,CKUNLOC                                                  
         BAS   RE,NEEDREC                                                       
         B     NOGOODX                                                          
         SPACE 1                                                                
NEEDAY   NTR1                                                                   
         LA    R5,TAPEIO                                                        
         USING TAPED,R5                                                         
         XC    NEEDKEY,NEEDKEY     ENSURE AGENCY AROUND                         
         LA    R4,NEEDKEY                                                       
         USING TLAYD,R4                                                         
         MVI   TLAYCD,TLAYCDQ                                                   
         MVC   TLAYAGY,INAGY                                                    
         BAS   RE,NEEDREC                                                       
         B     NOGOODX                                                          
NEEDSG   NTR1                                                                   
         LA    R5,TAPEIO                                                        
         USING TAPED,R5                                                         
         XC    NEEDKEY,NEEDKEY     GET SIGNATORY NAME                           
         LA    R4,NEEDKEY                                                       
         USING TLAYD,R4                                                         
         MVI   TLAYCD,TLAYCDQ                                                   
         MVC   TLAYAGY,INSIGN                                                   
         BAS   RE,NEEDREC                                                       
         L     R4,NEEDAREC                                                      
         BAS   RE,GETSHORT                                                      
         B     NOGOODX                                                          
         SPACE 1                                                                
NEEDCL   NTR1                                                                   
         LA    R5,TAPEIO                                                        
         USING TAPED,R5                                                         
         XC    NEEDKEY,NEEDKEY     ENSURE RECORD AROUND                         
         LA    R4,NEEDKEY                                                       
         USING TLCLD,R4                                                         
         MVI   TLCLCD,TLCLCDQ                                                   
         MVC   TLCLAGY,INAGY       TRY FOR AGENCY CLIENT FIRST                  
         MVC   TLCLCLI,INCLI                                                    
         BAS   RE,NEEDREC                                                       
         CLI   NEEDHIT,C'Y'                                                     
         BE    NOGOODX                                                          
         XC    TLCLAGY,TLCLAGY     THEN FOR GLOBAL CLIENT                       
         BAS   RE,NEEDREC                                                       
         B     NOGOODX                                                          
         SPACE 1                                                                
NEEDPR   NTR1                                                                   
         LA    R5,TAPEIO                                                        
         USING TAPED,R5                                                         
         XC    NEEDKEY,NEEDKEY     ENSURE RECORD AROUND                         
         MVC   NEEDNAME,SPACES                                                  
         CLI   INPRD,C'A'                                                       
         BL    NOGOODX                                                          
         LA    R4,NEEDKEY                                                       
         USING TLPRD,R4                                                         
         MVI   TLPRCD,TLPRCDQ                                                   
         MVC   TLPRAGY,INAGY                                                    
         MVC   TLPRCLI,INCLI                                                    
         MVC   TLPRPRD,INPRD                                                    
         BAS   RE,NEEDREC          TRY FOR AGENCY PRODUCT                       
         CLI   NEEDHIT,C'Y'                                                     
         BE    NOGOODX                                                          
         XC    TLPRAGY,TLPRAGY                                                  
         XC    TLPRCLI,TLPRCLI                                                  
         BAS   RE,NEEDREC          THEN TRY FOR GLOBAL PRODUCT                  
         B     NOGOODX                                                          
         SPACE 1                                                                
NEEDEM   NTR1                                                                   
         LA    R5,TAPEIO                                                        
         USING TAPED,R5                                                         
         XC    NEEDKEY,NEEDKEY     ENSURE EMPLOYER AROUND                       
         LA    R4,NEEDKEY                                                       
         USING TLEMD,R4                                                         
         MVI   TLEMCD,TLEMCDQ                                                   
         MVC   TLEMEMP,0(R2)                                                    
         BAS   RE,NEEDREC                                                       
         B     NOGOODX                                                          
         SPACE 1                                                                
NEEDW4   NTR1                                                                   
         LA    R5,TAPEIO                                                        
         USING TAPED,R5                                                         
         XC    NEEDKEY,NEEDKEY     ENSURE W4 AROUND                             
         LA    R4,NEEDKEY                                                       
         USING TLW4D,R4                                                         
         MVI   TLW4CD,TLW4CDQ                                                   
         MVC   TLW4SSN,0(R2)                                                    
         BAS   RE,NEEDREC                                                       
         B     NOGOODX                                                          
         SPACE 1                                                                
NEEDOFF  NTR1                                                                   
         L     R6,NEEDAREC                                                      
         MVI   ELCODE,TAAYELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   NOGOODX                                                          
         USING TAAYD,R6                                                         
         MVC   SAVESTA6,TAAYSTA6   SAVE STATUS BYTE 6                           
         LA    R2,OFFTAB                                                        
*                                                                               
NEEDO2   CLI   0(R2),X'FF'         END OF TABLE?                                
         BE    NOGOODX                                                          
         CLC   TAAYTPOF,0(R2)                                                   
         BE    NEEDO4                                                           
         LA    R2,L'OFFTAB(R2)                                                  
         B     NEEDO2                                                           
*                                                                               
NEEDO4   MVC   INEMPNM+30-2(2),1(R2)                                            
         B     NOGOODX                                                          
         EJECT                                                                  
*              GENERAL BUFFER HANDLER                                           
         SPACE 3                                                                
NEEDREC  NTR1                                                                   
         L     R2,ABUFFER                                                       
         LTR   R2,R2                                                            
         BNZ   NREC2                                                            
         L     R0,LBUFFER                                                       
         GETMAIN RU,LV=(0)                                                      
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         ST    R1,ABUFFER                                                       
         L     R2,ABUFFER                                                       
         MVC   0(4,R2),=F'100'     SET UP FOR 100 RECORDS                       
         MVC   4(4,R2),=F'4000'    4000 BYTES EACH                              
         XC    8(4,R2),8(R2)                                                    
         LA    RF,100                                                           
         M     RE,=F'4000'                                                      
         LA    RE,12(R2)                                                        
*                                  CLEAR BUFFER FIRST TIME                      
         XCEF                                                                   
         B     NREC2                                                            
         SPACE 1                                                                
ABUFFER  DC    A(0)                                                             
LBUFFER  DC    F'400016'           (100*4000 + 16)                              
         SPACE 1                                                                
NREC2    DS    0H                  NOW R2 HAS A(BUFFER)                         
*                                  BYTES  1-4 N'ENTRIES                         
*                                  BYTES  5-8 L'ENTRY                           
*                                  BYTES 9-12 NUMBER OF LAST ENTRY              
         LA    R4,12(R2)           BYTES 13+  THE BUFFER!                       
         L     R0,0(R2)                                                         
         SPACE 1                                                                
NREC6    CLC   NEEDKEY,0(R4)       IS MY RECORD IN THE BUFFER?                  
         BE    NREC10                                                           
         A     R4,4(R2)                                                         
         BCT   R0,NREC6                                                         
         SPACE 1                                                                
         MVI   NEEDHIT,C'N'                                                     
         MVC   KEY,NEEDKEY         NO, NOW NEED THE RECORD                      
         GOTO1 HIGH                                                             
         CLC   NEEDKEY(32),KEY                                                  
         BNE   NOGOODX                                                          
         SPACE 1                                                                
NREC8    L     R1,8(R2)            NO - PICK UP N'LAST ENTRY                    
         LA    R1,1(R1)                 ROUND ROBIN                             
         C     R1,0(R2)            HAVE WE GOT TO THE END OF BUFFER?            
         BNH   *+8                                                              
         LA    R1,1                YES, SO GO BACK TO THE BEGINNING             
         ST    R1,8(R2)                                                         
         BCTR  R1,0                                                             
         M     R0,4(R2)            DISPLACE INTO THE BUFFER                     
         LA    R4,12(R1,R2)                                                     
         GOTO1 GETREC                                                           
         L     R3,AIO                                                           
         L     R2,4(R2)                                                         
         MOVE  ((R4),(R2)),(R3)    MOVE INTO OUR AREA                           
         OC    TIKEY,TIKEY         IS SYSIO READING RECORDS                     
         BZ    NREC10                                                           
         TM    TISTAT,TISTRDCK     UNLESS READING CHECK FILE                    
         BO    NREC10                                                           
         MVC   KEY,TIKEY                                                        
         GOTO1 HIGH                REREAD TO ESTABLISH SEQUENCE                 
         SPACE 1                                                                
NREC10   ST    R4,NEEDAREC         PASS BACK A RECORD                           
         BAS   RE,GETNAME                                                       
         MVI   NEEDHIT,C'Y'                                                     
         B     ITSFINE                                                          
         SPACE 1                                                                
NEEDAREC DS    A                                                                
NEEDKEY  DC    XL32'00'                                                         
NEEDNAME DS    CL36                                                             
NEEDSHRT DS    CL16                                                             
NEEDHIT  DS    CL1                                                              
NEEDTYPE DS    CL1                                                              
         EJECT                                                                  
*              ODDMENTS                                                         
         SPACE 3                                                                
GETNAME  NTR1                                                                   
*                                  R4=A(RECORD) TO NEEDNAME                     
         MVC   SAVEEL,ELCODE       SAVE PRESENT ELCODE                          
         MVC   NEEDNAME,SPACES                                                  
         CLI   0(R4),TLW4CDQ                                                    
         BE    GETW4NM                                                          
         MVI   ELCODE,TANAELQ                                                   
         LR    R6,R4                                                            
         BRAS  RE,GETEL                                                         
         MVC   ELCODE,SAVEEL       RESTORE PRESENT ELCODE                       
         BNE   NOGOOD                                                           
         USING TANAD,R6                                                         
         ZIC   R1,TANALEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     ITSFINE                                                          
         MVC   NEEDNAME(0),TANANAME                                             
         SPACE 1                                                                
GETW4NM  MVI   ELCODE,TAW4ELQ                                                   
         LR    R6,R4                                                            
         BRAS  RE,GETEL                                                         
         MVC   ELCODE,SAVEEL       RESTORE PRESENT ELCODE                       
         BNE   NOGOOD                                                           
         USING TAW4D,R6                                                         
         MVC   NEEDNAME(32),TAW4CRPN                                            
         MVC   NEEDTYPE,TAW4TYPE                                                
         B     ITSFINE                                                          
         SPACE 1                                                                
GETSHORT NTR1                                                                   
*                                  R4=A(RECORD) TO NEEDSHRT                     
         MVC   SAVEEL,ELCODE       SAVE PRESENT ELCODE                          
         MVI   ELCODE,TASNELQ      SHORT NAME                                   
         LR    R6,R4                                                            
         BRAS  RE,GETEL                                                         
         MVC   NEEDSHRT,SPACES                                                  
         MVC   ELCODE,SAVEEL                                                    
         BNE   NOGOODX                                                          
         USING TASND,R6                                                         
         ZIC   R1,TASNLEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     NOGOODX                                                          
         MVC   NEEDSHRT(0),TASNAME                                              
         SPACE 1                                                                
ITSFINE  SR    RC,RC                                                            
NOGOOD   LTR   RC,RC                                                            
         B     NOGOODX                                                          
OFFTAB   DS    0CL3                OFFICE NAME TABLE                            
         DC    C'1',C'CH'                                                       
         DC    C'2',C'NY'                                                       
         DC    C'3',C'DA'                                                       
         DC    C'4',C'AT'                                                       
         DC    C'5',C'LA'                                                       
         DC    C'8',C'CH'                                                       
         DC    X'FF'                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
NOGOODX  XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         SPACE 1                                                                
*              EDIT DETAILS OF PAYMENT                                          
         SPACE 3                                                                
*              TASYSEQUS                                                        
*              TASYSDSECT                                                       
         PRINT OFF                                                              
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TASYSDSECT                                                     
         PRINT ON                                                               
         EJECT                                                                  
*              MY WORKING STORAGE                                               
*                                                                               
MYD      DSECT                                                                  
MYSTART  DS    0D                                                               
MYREGS   DS    16F                                                              
ODDAREA  DS    CL12                                                             
*                                                                               
DYNALLOC DS    A                                                                
AMASTD   DS    A                   A(MASTER)                                    
ALOGOC   DS    A                   A(LOGOC)                                     
ALOGO    DS    A                   A(LOGO)                                      
AREMOT   DS    A                   A(REMOTE)                                    
VSMTP    DS    V                   V(SMTP)                                      
*                                                                               
*                                  OPTIONS                                      
         SPACE 1                                                                
LASTCHEK DS    CL32                                                             
*                                  OPTIONS                                      
TAPEOPT  DS    CL1                 Y=GENERATE OUTPUT TAPE                       
SORTOPT  DS    CL1                                                              
LISTOPT  DS    CL1                 Y=LIST AND COUNT ONLY                        
BILLOPT  DS    CL1                 BILLING ONLY                                 
SEQOPT   DS    CL1                 SEQUENCE - DEFAULT IS UNION                  
PAYPSTAT DS    CL1                                                              
SESSOPT  DS    CL1                 Y=SESSION USES ONLY                          
RETROOPT DS    CL1                 Y=GET ONLY RETROACTIVE PAYMENTS              
*                                  D=GET ONLY RETRO PAYMENTS P&H DIFF           
*                                  A=ALSO GET RETROACTIVE PAYMENTS              
*                                  D=GET ONLY RETROACTIVE PAYMENTS              
NODTLOPT DS    CL1                 Y=SUPPRESS DETAIL ON TAPE (TYPS 2&3)         
MYTROPT  DS    CL1                 Y=TRACE RECORDS IN AND OUT OF SORTER         
PLNOPT   DS    CL1                 Y=PAX AND LATE NIGHT USES ONLY               
GRNYOPT  DS    CL1                 AGENCY GRNY, SPECIAL                         
POWOPT   DS    CL1                 POWWOW OPTION                                
TRPWOPT  DS    CL1                 TRACE POWWOW OPTION                          
TPOFF    DS    CL1                 TP OFFICE CODE                               
SORTFRST DS    CL1                                                              
SFTPOPT  DS    CL1                 Y=USE MQ                                     
SFTPTEST DS    CL1                 Y=(RUN=TEST)                                 
*                                                                               
RECTYPE  DS    CL16                                                             
SAVEEL   DS    CL1                                                              
SAVETPKY DS    CL20                                                             
*****SAVETPKY DS    CL21                                                        
SAVEKEY  DS    CL20                                                             
SAVEAMTS DS    CL36                                                             
SAVEGST  DS    CL9                                                              
SAVEUNI  DS    CL3                                                              
SAVEBASE DS    XL4                 P&H BASE                                     
SAVEDIFF DS    XL4                 P&H DIFF                                     
SVLNTR   DS    XL1                 Y=LIEN OR TRUSTEE CHECK                      
SAVESTA6 DS    XL1                 STATUS 6 FROM AGY ELM                        
RET4INV  DS    XL6                 INVOICE RETRO IS FOR                         
SVAMT    DS    CL13                SAVED AMOUNT                                 
CONTFLAG DS    CL1                 CONTRACT TYPE FLAG                           
CRINVNUM DS    CL6                 CREDIT INVOICE NUMBER                        
         SPACE 1                                                                
DLBLOCK  DS    CL(DLCBXLX)         DOWNLOAD INTERFACE BLOCK                     
         DS    0D                                                               
TAPCOUNT DS    PL6                                                              
         SPACE 1                                                                
         DS    0D                                                               
TAPEIO   DS    704C                                                             
         DS    C                                                                
SORTIO   DS    704C                                                             
         SPACE 1                                                                
MYEND    DS    0D                                                               
         EJECT                                                                  
*---------------------------------------------------------------                
         DS    0D                                                               
         DC    CL8'**SSB **'                                                    
SSB      CSECT                                                                  
         DC    X'0000FF',X'00',XL252'00'                                        
*---------------------------------------------------------------                
*              DSECT TO COVER PRINT LINES                                       
         SPACE 3                                                                
PRINTD   DSECT                                                                  
         DS    CL6                                                              
PUNION   DS    CL3                                                              
         DS    CL1                                                              
PTOTALS  DS    0CL20                                                            
PEMP     DS    CL3                                                              
         DS    CL1                                                              
PAGENCY  DS    CL6                                                              
         DS    CL1                                                              
PINVOICE DS    CL7                                                              
         DS    CL1                                                              
PCLIENT  DS    CL6                                                              
         DS    CL1                                                              
PCID     DS    CL12                                                             
         DS    CL1                                                              
PUSE     DS    CL3                                                              
         DS    CL1                                                              
PCYCLE   DS    CL17                                                             
         DS    CL1                                                              
PGROSS   DS    CL13                                                             
         DS    CL1                                                              
PMISC    DS    CL13                                                             
         DS    CL1                                                              
PSPNH    DS    CL13                                                             
         DS    CL1                                                              
PPNH     DS    CL13                                                             
         DS    CL6                                                              
         EJECT                                                                  
       ++INCLUDE DDDLCB                                                         
         EJECT                                                                  
*              DSECT TO COVER TAPE RECORDS                                      
         SPACE 3                                                                
TAPED    DSECT                                                                  
TAPEKEY  DS    0CL20               KEY LENGTH OF 20                             
TAPEREC  DS    0C                  RECORD LENGTH OF 700                         
         SPACE 1                                                                
INREC    DS    0C                  INVOICE RECORD TYPE 1                        
ICONT    DS    CL1                 CONTRACT TYPE                                
INUN     DS    CL3                 UNION (AFM AFT SAG ETC)                      
INEMP    DS    CL3                 EMPLOYER (TP PG ETC)                         
INAGY    DS    CL6                 AGENCY CODE                                  
ININV    DS    CL6                 INVOICE NUMBER                               
INTYPE   DS    CL1                 TYPE 1                                       
**       DS    CL1                 FILLER                                       
         SPACE 1                                                                
TAPEAMTS DS    0CL36                                                            
INGROSS  DS    CL9                 GROSS AMOUNT (INVIDUAL)                      
INMISC   DS    CL9                 MISCELLANEOUS AMOUNT (CORP + REEX)           
INSPNH   DS    CL9                 SUBJECT TO PNH                               
INPNH    DS    CL9                 PNH                                          
INTAX    DS    CL9                 PAYROLL TAXES                                
INHAND   DS    CL9                 HANDLING CHARGES                             
INAMT    DS    CL9                 BILLING AMOUNT                               
         SPACE 1                                                                
INBILDTE DS    CL6                 BILLED DATE YYMMDD                           
INCLI    DS    CL6                 CLIENT CODE                                  
INPRD    DS    CL6                 PRODUCT CODE                                 
INUSE    DS    CL3                 USE CODE                                     
INUTYP   DS    CL1                 USE TYPE                                     
INWEEKS  DS    CL2                 CYCLE WEEKS                                  
INCYCSTR DS    CL6                       START                                  
INCYCEND DS    CL6                       END                                    
INMEDIA  DS    CL1                 MEDIA R=RADIO T=TV ETC                       
INCTYPE  DS    CL1                 COMMERCIAL TYPE                              
INFLMDTE DS    CL6                 FILM DATE                                    
INRECDTE DS    CL6                 RECORD DATE                                  
INFFCDTE DS    CL6                 FIRST FIXED CYCLE                            
INDUBDTE DS    CL6                 DUB DATE                                     
INAIRDTE DS    CL6                 AIR DATE                                     
INMAJ    DS    CL1                 MAJOR CITIES CODE 0=NONE 1=NY                
*                                  2=CHI 3=NY+CHI 4=LA 5=NY+LA                  
*                                  6=CHI+LA 7=NY+CHI+LA                         
INUNITS  DS    CL3                 NUMBER OF UNITS                              
         ORG   INMAJ                                                            
INCUNITS DS    CL4                 CABLE UNITS                                  
         ORG   ,                                                                
INUMAJ   DS    CL1                 UPGRADE MAJORS (SEE ABOVE)                   
INUUNITS DS    CL3                 UPGRADE UNITS                                
         ORG   INUMAJ                                                           
INCUUNTS DS    CL4                 CABLE UPGRADE UNITS                          
         ORG   ,                                                                
INVER    DS    CL1                 0=BOTH 1=PRIMARY ONLY 2=LIFT ONLY            
INCRIND  DS    CL1                 0=NOT, 1=CREDIT INVOICE                      
INCANIND DS    CL1                 0=NOT, 1=CANADIAN TAX WITHHELD               
INCABUP  DS    CL1                 CABLE UPGRADE CODE  (TBD)                    
INAGYNM  DS    CL30                AGENCY NAME                                  
INYEAR   DS    CL3                 YEAR CODE                                    
INCID    DS    CL12                COMMERCIAL ID                                
INTITLE  DS    CL30                COMMERCIAL TITLE                             
INCLINM  DS    CL30                CLIENT NAME                                  
INPRDNM  DS    CL30                PRODUCT NAME                                 
INEST    DS    CL15                ESTIMATE NO.                                 
INPO     DS    CL10                PO NUMBER                                    
INLFTID  DS    CL12                LIFT COMMERCIAL ID                           
INFSTUD  DS    CL15                FILM STUDIO                                  
INRSTUD  DS    CL15                RECORD STUDIO                                
INAFMC1  DS    CL12                AFM CONTRACT 1                               
INAFMC2  DS    CL12                             2                               
INCOMM   DS    CL40                INVOICE COMMENT                              
INFCITY  DS    CL15                FILM CITY                                    
INRCITY  DS    CL15                RECORD CITY                                  
INPDESC  DS    CL20                DESRIPTION OF PAYMENT                        
INUSDESC DS    CL30                DESCRIPTION OF USE                           
INEMPNM  DS    CL30                EMPLOYER OF RECORD                           
INTID    DS    CL2                 TRACK ID                                     
INTLEN   DS    CL3                 TRACK LENGTH                                 
INADST   DS    CL2                 ADDENDUM STATE                               
INCOSEC  DS    CL3                 COMMERCIAL LENGTH IN SECS                    
INLFSEC  DS    CL3                 LIFT LENGTH IN SECS                          
INTAGS   DS    CL3                 N'TAGS FOR TAG USE                           
INLOCS   DS    0CL5                LOCATIONS FOR FGR USE                        
INUK     DS    CL1                 1 = UK, ELSE 0                               
INEU     DS    CL1                 1 = EUROPE W/O UK, ELSE 0                    
INJAP    DS    CL1                 1 = JAPAN, ELSE 0                            
INAP     DS    CL1                 1 = ASIA PACIFIC, ELSE 0                     
INREST   DS    CL1                 1 = REST OF WORLD, ELSE 0                    
INSIGN   DS    CL6                 SIGNATORY                                    
INSIGNM  DS    CL16                SIGNATORY NAME                               
INSSORRS DS    CL1                 SESSION OR REUSE (S / R)                     
INGST    DS    CL9                 GST (GRNY ONLY)                              
INXUTYP  DS    CL3                 EXTENDED USE TYPE                            
INRECKDT DS    CL6                 RETROPH ONLY, CHECK DATE ON ORIGINAL         
*                                                                               
INAGYCOD DS    CL10                AGENCY STUDIO CODE                           
INCHKDTE DS    CL6                 CHECK DATE                                   
*                                                                               
INCONTYP DS    C                   CONTRACT TYPE                                
INCRCKDT DS    CL6                 CREDIT ORIGINAL INVOICE CHECK DATE           
                                                                                
         DS    CL(INREC+700-*)                                                  
         EJECT                                                                  
*              CLASS A TAPE RECORDS (TYPE 2)                                    
         SPACE 3                                                                
         ORG   TAPEREC                                                          
CAREC    DS    0C                  CLASS A RECORD TYPE 2                        
CACONT   DS    CL1                 CONTRACT TYPE                                
CAUN     DS    CL3                 UNION (AFM AFT SAG ETC)                      
CAEMP    DS    CL3                 EMPLOYER (TP PG ETC)                         
CAAGY    DS    CL6                 AGENCY CODE                                  
CAINV    DS    CL6                 INVOICE NUMBER                               
CATYPE   DS    CL1                 TYPE 2                                       
****     DS    CL1                 FILLER                                       
         SPACE 1                                                                
CAUSES   DS    CL80                UP TO 20 USE NUMBERS (9999)                  
CADATES  DS    CL80                UP TO 20 DATES (MMDD)                        
CAFROM   DS    CL3                 FROM USE NUMBER                              
CATO     DS    CL3                 TO USE NUMBER                                
CALFROM  DS    CL3                 FROM USE NUMBER FOR LIFT                     
CALTO    DS    CL3                 TO USE NUMBER FOR LIFT                       
         DS    CL14                FILLER                                       
CAPROGS  DS    400C                MAX 20 (PRG NAME CL15, NETWORK CL1,          
*                                          LIFT CODE CL1, SPACES CL3)           
         DS    CL(TAPEREC+700-*)   FILLER                                       
         EJECT                                                                  
*              CHECK RECORDS (TYPE 3)                                           
         SPACE 3                                                                
         ORG   TAPEREC                                                          
CKREC    DS    0C                  INVOICE DETAIL TYPE 3                        
CKCONT   DS    CL1                 CONTRACT TYPE                                
CKUN     DS    CL3                 UNION (AFM AFT SAG ETC)                      
CKEMP    DS    CL3                 EMPLOYER (TP PG ETC)                         
CKAGY    DS    CL6                 AGENCY CODE                                  
CKINV    DS    CL6                 INVOICE NUMBER                               
CKTYPE   DS    CL1                 TYPE 3                                       
***      DS    CL1                 FILLER                                       
         SPACE 1                                                                
CKGROSS  DS    CL9                 GROSS AMOUNT                                 
CKMISC   DS    CL9                 MISCELLANEOUS AMOUNT                         
CKSPNH   DS    CL9                 SUBJECT TO PNH                               
CKPNH    DS    CL9                 PNH                                          
CKSSN    DS    CL9                 SOCIAL SECURITY NUMBER                       
CKCORPID DS    CL9                 CORP ID                                      
CKOV1    DS    CL4                 OVERSCALE PERCENT 1 (2 DEC)                  
CKOV2    DS    CL4                                   2 (2 DEC)                  
CKONOFF  DS    CL1                 CAMERA: 0=OFF 1=0N                           
CKMINOR  DS    CL1                 N/A (SET TO ZERO)                            
CKAGENT  DS    CL4                 AGENT CODE                                   
CKSTATE  DS    CL2                 STATE CODE (ALPHA - NY ETC)                  
CKLOCAL  DS    CL3                 LOCAL CODE (ALPHA - NYC ETC)                 
CKVER    DS    CL1                 BLANK=MAIN ONLY, O=LIFT ONLY, Y=BOTH         
CKMULTI  DS    CL1                 N/A (SET TO ZERO)                            
CKSWEET  DS    CL1                 N/A (SET TO ZERO)                            
CKDUBS   DS    CL2                 NUMBER OF DOUBLES                            
CKHOURS  DS    CL4                 SESSION HOURS (2 DEC)                        
CKSPOTS  DS    CL2                 NUMBER OF SPOTS                              
CKFRSDTE DS    CL6                 FIRST SERVICES DATE                          
CKDAYS   DS    CL2                 NUMBER OF DAYS WORKED                        
CKAPPCDE DS    CL1                 APPLY CODE                                   
CKINCCDE DS    CL1                 INCLUDE CODE                                 
CKPAID   DS    CL9                 AMOUNT ALREADY PAID (APPLIED)                
CKLAST   DS    CL16                LAST NAME                                    
CKFIRST  DS    CL16                FIRST NAME                                   
CKCAT    DS    CL3                 CATEGORY CODE                                
CKEMPNM  DS    CL30                EMPLOYER NAME                                
CKYEAR   DS    CL3                 YEAR CODE                                    
CKACCNO  DS    CL10                ACCOUNT NUMBER (OF LOCAL)                    
CKCORPNM DS    CL32                CORPORATION NAME                             
CKSTATE2 DS    CL2                 STATE (SAME AS CKSTATE)                      
CKUNLOC  DS    CL3                 UNION LOCAL CODE                             
CKFROM   DS    CL3                 FROM USE NUMBER                              
CKTO     DS    CL3                 TO USE NUMBER                                
CKOVRT   DS    CL4                 OVERTIME                                     
CKTRVT   DS    CL4                 TRAVEL TIME                                  
CKUDUES  DS    CL9                 UNION DUES                                   
CKGST    DS    CL9                 GST                                          
*                                                                               
CKPHRATE DS    CL9                 P&H RATE                                     
*                                                                               
         DS    CL(TAPEREC+700-*)   FILLER                                       
         EJECT                                                                  
*              TOTAL RECORDS (TYPE 4-5)                                         
         SPACE 3                                                                
         ORG   TAPEREC                                                          
TTREC    DS    0C                  TOTAL RECORD TYPE 4-5                        
TTCONT   DS    CL1                 CONTRACT TYPE                                
TTUN     DS    CL3                 UNION (AFM AFT SAG ETC)                      
TTEMP    DS    CL3                 EMPLOYER (TP PG ETC) (TYPE 4)                
TTAGY    DS    CL6                 AGENCY CODE (TYPE 4) OR 999999               
TTINV    DS    CL6                 FILLER (999999)                              
TTTYPE   DS    CL1                 TYPE 4 (AGY) 5(UNION-TAPE ONLY)              
***      DS    CL1                 FILLER                                       
         SPACE 1                                                                
TTGROSS  DS    CL9                 GROSS AMOUNT                                 
TTMISC   DS    CL9                 MISCELLANEOUS AMOUNT                         
TTSPNH   DS    CL9                 SUBJECT TO PNH                               
TTPNH    DS    CL9                 PNH                                          
TTGST    DS    CL9                 GST                                          
         DS    635C                FILLER                                       
         SPACE 3                                                                
*              GRAND TOTALS                                                     
         SPACE 3                                                                
         ORG   TAPEREC                                                          
GTREC    DS    0C                  TOTAL RECORD TYPE 5(PRINT ONLY) & 6          
GTCONT   DS    CL1                 CONTRACT TYPE                                
         DS    CL3                 FILLER 999                                   
         DS    CL3                 FILLER 999                                   
         DS    CL6                 FILLER 999999                                
         DS    CL6                 FILLER 999999                                
GTTYPE   DS    CL1                 TYPE 5 & 6                                   
***      DS    CL1                 FILLER                                       
         SPACE 1                                                                
GTGROSS  DS    CL10                GROSS AMOUNT                                 
GTMISC   DS    CL10                MISCELLANEOUS AMOUNT                         
GTSPNH   DS    CL10                SUBJECT TO PNH                               
GTPNH    DS    CL10                PNH                                          
GTGST    DS    CL10                GST                                          
         DS    630C                FILLER                                       
         SPACE 3                                                                
*              TOTAL RECORDS (TYPE 7)                                           
         SPACE 3                                                                
         ORG   TAPEREC                                                          
T7REC    DS    0C                  TOTAL RECORD TYPE 7                          
T7CONT   DS    CL1                 CONTRACT TYPE                                
T7UN     DS    CL3                 UNION (AFM AFT SAG ETC)                      
T7EMP    DS    CL3                 EMPLOYER (TP PG ETC) (TYPE 4)                
T7AGY    DS    CL6                 AGENCY CODE (TYPE 4) OR 999999               
         DS    CL6                 FILLER                                       
T7TYPE   DS    CL1                 TYPE 7                                       
         SPACE 1                                                                
T7GROSS  DS    CL9                 GROSS AMOUNT                                 
T7MISC   DS    CL9                 MISCELLANEOUS AMOUNT                         
T7SPNH   DS    CL9                 SUBJECT TO PNH                               
T7PNH    DS    CL9                 PNH                                          
T7GST    DS    CL9                 GST                                          
T7ZEROS  DS    CL9                 0                                            
         DS    CL9                 0                                            
         DS    CL9                 0                                            
         DS    CL9                 0                                            
         DS    CL9                 0                                            
         DS    CL9                 0                                            
         DS    CL9                 0                                            
         DS    CL9                 0                                            
         DS    CL9                 0                                            
T7CONTTY DS    CL1                 CONTRACT TYPE                                
         DS    560C                FILLER                                       
         SPACE 3                                                                
         EJECT                                                                  
*              OTHER DSECTS ARE HIDDEN IN HERE                                  
         SPACE 3                                                                
*TAREPWORKD                                                                     
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*DDREMOTED                                                                      
*DDMASTD                                                                        
*DDLOGOD                                                                        
*DDBIGBOX                                                                       
*TAGENFILE                                                                      
*DDSOFDATD                                                                      
*DDSMTPD                                                                        
*DDTWADCONS                                                                     
*FAFACTS                                                                        
*FATIOB                                                                         
*TAREPFFD                                                                       
       ++INCLUDE TAREPWORKD                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDREMOTED                                                      
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDLOGOD                                                        
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDSOFDATD                                                      
       ++INCLUDE DDSMTPD                                                        
TWADCOND DSECT                                                                  
       ++INCLUDE DDTWADCONS                                                     
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FATIOB                                                         
       ++INCLUDE TAREPFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE TAREPE0D                                                       
         SPACE 1                                                                
       ++INCLUDE DDGENTWA                                                       
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'135TAREP20   10/04/16'                                      
         END                                                                    
