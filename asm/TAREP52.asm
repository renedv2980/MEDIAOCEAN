*          DATA SET TAREP52    AT LEVEL 110 AS OF 10/21/14                      
*PHASE T70352C,*                                                                
*INCLUDE DLFLD                                                                  
         TITLE 'T70352 - BILLING ACTIVITY DISK/DOWNLOAD'                        
*                        COPIED FROM TAREP20                                    
*                                                                               
T70352   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70352,R6                                                      
         L     RC,0(R1)            RC=A(CONTROLLER W/S)                         
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(SCREEN)                                 
         USING T703FFD,RA                                                       
         L     R9,ASUBSYSD         R9=A(SYSTEM W/S)                             
         USING SUBSYSD,R9                                                       
         L     R8,ASPOOLD          R8=A(SPOOLD DSECT)                           
         USING SPOOLD,R8                                                        
         LA    R7,BUFF             R7=A(LOCAL W/S)                              
         LA    R7,8(R7)                                                         
         USING MYD,R7                                                           
*                                                                               
         GOTO1 INITIAL,DMCB,0                                                   
         SPACE 1                                                                
         MVC   AMASTD,TWAMASTC     SAVE ADDRESS OF MASTER                       
         L     R1,TWADCONS                                                      
         USING TWADCOND,R1                                                      
         MVC   ALOGOC,TLOGOC       SAVE A(LOGOC)                                
         L     R1,AMASTD                                                        
         USING MASTD,R1                                                         
         MVC   ALOGO,MCVLOGO       SAVE A(LOGO)                                 
         MVC   AREMOT,MCVREMOT     SAVE A(REMOTE)                               
         DROP  R1                                                               
*                                                                               
         CLI   MODE,VALREC                                                      
         BNE   *+12                                                             
         BAS   RE,VREC                                                          
         B     XIT                                                              
         SPACE 1                                                                
         CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
         BAS   RE,VREC                                                          
         BAS   RE,OPENTAPE                                                      
         CLI   RECNUM,MCINTER      IF MCCANN INTERFACE,                         
         BNE   MAIN10                                                           
         BRAS  RE,PREPMC           READ INVOICE RECORDS                         
         B     *+8                                                              
MAIN10   BAS   RE,PREP                                                          
         BAS   RE,CLOSTAPE                                                      
         SPACE 1                                                                
         CLI   RECNUM,MCINTER      IF MCCANN INTERFACE,                         
         BNE   MAIN20                                                           
         BRAS  RE,MCDOWN           DOWNLOAD                                     
         B     XIT                                                              
MAIN20   CLI   ACTEQU,ACTDOWN      IF DOWNLOADING                               
         BNE   XIT                                                              
         GOTO1 =A(PREPD),DMCB,(RC) PRINT DOWNLOADABLE REPORT                    
         B     XIT                                                              
         SPACE 1                                                                
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 1                                                                
*                                  PROGRAM IS HANDLING THE FOLLOWING            
BAINTER  EQU   95                  BAINTER REPORT                               
MCINTER  EQU   115                 MCCANN INTERFACE REPORT                      
         EJECT                                                                  
*              VALIDATE RECORD                                                  
         SPACE 1                                                                
VREC     NTR1                                                                   
         LA    R2,SPLPERH          PERIOD                                       
         CLI   5(R2),0                                                          
         BE    INVERR                                                           
         BRAS  RE,VSFTDAT          VALIDATE USING SOFDAT                        
         BE    VREC2               IF NOT GOOD,                                 
         GOTO1 VALPERD             USE OLD METHOD                               
VREC2    MVI   TIQDTYPE,TIQDCHK    (FILTER ON CHECK DATE)                       
         SPACE 1                                                                
         LA    R2,SPLUNH           UNION                                        
         CLI   5(R2),0                                                          
         BE    VREC4                                                            
         CLC   8(3,R2),=C'ALL'                                                  
         BE    VREC4                                                            
         GOTO1 ANY                                                              
         MVC   TIFUN,WORK                                                       
         GOTO1 UNIVAL,DMCB,8(R2)                                                
         SPACE 1                                                                
VREC4    LA    R2,SPLLCLH          LOCAL                                        
         CLI   5(R2),0                                                          
         BE    VREC5                                                            
         OC    TIFUN,TIFUN         CAN'T HAVE LOCAL WITHOUT UNION               
         BZ    INVERR                                                           
         GOTO1 RECVAL,DMCB,TLLOCDQ,(R2),0                                       
         MVC   TIFLOCL,TGLCL                                                    
         SPACE 1                                                                
VREC5    LA    R2,SPLAGGH          AGENCY GROUP                                 
         CLI   5(R2),0                                                          
         BE    VREC6                                                            
         CLC   8(3,R2),=C'ALL'                                                  
         BE    VREC6                                                            
         GOTO1 ANY                                                              
         MVC   TIFAGG,WORK                                                      
         GOTO1 RECVAL,DMCB,TLAGCDQ,(R2),0                                       
         SPACE 1                                                                
VREC6    LA    R2,SPLAGYH          AGENCY                                       
         CLI   RECNUM,MCINTER      IF MCCANN INTERFACE,                         
         BNE   VREC7               AGENCY IS REQUIRED                           
         CLC   8(3,R2),=C'ALL'                                                  
         BE    INVERR                                                           
         CLI   5(R2),0                                                          
         BE    ERRMIS                                                           
         B     VREC7A                                                           
VREC7    CLI   5(R2),0                                                          
         BE    VREC8                                                            
         CLC   8(3,R2),=C'ALL'                                                  
         BE    VREC8                                                            
VREC7A   LA    R3,TIFAGY                                                        
         LA    R4,L'TIFAGY                                                      
         LA    R5,TLAYCDQ                                                       
         BRAS  RE,SPECFILT                                                      
         SPACE 1                                                                
VREC8    LA    R2,SPLEMPH          EMPLOYER                                     
         CLI   5(R2),0                                                          
         BE    VREC10                                                           
         CLC   8(3,R2),=C'ALL'                                                  
         BE    VREC10                                                           
         GOTO1 ANY                                                              
         MVC   TIFEMP,WORK                                                      
         CLI   WORK,C'-'                                                        
         BE    VREC8M                                                           
         GOTO1 RECVAL,DMCB,TLEMCDQ,(R2),0                                       
         B     VREC10                                                           
         SPACE 1                                                                
VREC8M   MVC   TIFEMP,WORK+1                                                    
         NI    TIFEMP,X'FF'-X'40'                                               
         SPACE 1                                                                
VREC10   LA    R2,SPLOPTH          VALIDATE OPTIONS                             
         BAS   RE,VOPTS                                                         
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE OPTIONS                                                 
         SPACE 1                                                                
VOPTS    NTR1                                                                   
         MVI   TAPEOPT,C'Y'                                                     
         MVI   BILLOPT,C'N'                                                     
         MVI   SESSOPT,C'N'                                                     
         MVI   RETROOPT,C'N'                                                    
         MVI   NEWOPT,C'N'                                                      
         XC    AGYOPT,AGYOPT                                                    
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         GOTO1 SCANNER,DMCB,(R2),(10,BLOCK),0                                   
         ZIC   R0,4(R1)                                                         
         LA    R4,BLOCK                                                         
         LTR   R0,R0                                                            
         BZ    INVERR                                                           
         SPACE 1                                                                
OPT2     CLC   12(4,R4),=C'TAPE'   TAPE OPTION                                  
         BNE   OPT10                                                            
         CLI   ACTEQU,ACTDOWN                                                   
         BE    INVERR                                                           
         MVC   TAPEOPT,22(R4)                                                   
         B     OPTEND                                                           
         SPACE 1                                                                
OPT10    CLC   12(4,R4),=C'BILL'   BILL OPTION                                  
         BNE   OPT12                                                            
         MVI   BILLOPT,C'Y'                                                     
         MVI   TIQDTYPE,TIQDBILL   (SET TO READ BILL DATE POINTERS)             
         B     OPTEND                                                           
         SPACE 1                                                                
OPT12    CLC   12(7,R4),=C'SESSION' SESSIONS ONLY OPTION                        
         BNE   OPT16                                                            
         MVI   SESSOPT,C'Y'                                                     
         B     OPTEND                                                           
         SPACE 1                                                                
OPT16    CLC   12(6,R4),=C'RETROS'  RETROS ONLY OPTION                          
         BNE   OPT17                                                            
         MVI   RETROOPT,C'Y'                                                    
         B     OPTEND                                                           
         SPACE 1                                                                
OPT17    CLC   12(7,R4),=C'RETALSO' ALSO RETROS OPTION                          
         BNE   OPT20                                                            
         MVI   RETROOPT,C'A'                                                    
         B     OPTEND                                                           
         SPACE 1                                                                
OPT20    CLC   12(4,R4),=C'AUTH'  AUTH=NEW OPTION                               
         BNE   OPT25                                                            
         CLI   RECNUM,MCINTER      IF MCCANN INTERFACE,                         
         BE    INVERR              NOT ALLOWED                                  
         CLC   22(3,R4),=C'NEW'                                                 
         BNE   INVERR                                                           
         MVI   NEWOPT,C'Y'                                                      
         B     OPTEND                                                           
OPT25    CLC   12(2,R4),=C'AG'   AGENCY OVERRIDE OPTION                         
         BNE   INVERR                                                           
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'80',22(R4))                               
         BNE   INVERR                                                           
         CLI   RECNUM,MCINTER      IF MCCANN INTERFACE,                         
         BE    INVERR              NOT ALLOWED                                  
         MVC   AGYOPT,TGAGY                                                     
         B     OPTEND                                                           
OPTEND   LA    R4,32(R4)                                                        
         BCT   R0,OPT2                                                          
         B     XIT                                                              
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              PRINT REPORT                                                     
         SPACE 1                                                                
PREP     NTR1                                                                   
*        CLI   ACTEQU,ACTDOWN      IF NOT DOWNLOADING                           
*        BE    PREP1                                                            
         LA    R1,MYSPECS                                                       
         ST    R1,SPECS                                                         
         LA    R1,HOOK             INITIALIZE FOR REPORT                        
         ST    R1,HEADHOOK                                                      
         SPACE 1                                                                
PREP1    ZAP   TAPCOUNT,=P'0'                                                   
         MVI   ACTVSW,C'N'         NO PREVIOUS ACTIVITY                         
         MVI   SORTFRST,C'Y'       FIRST TIME FOR SORT                          
         MVC   TIACOMFC,ACOMFACS   SET UP FOR TASYSIO                           
         LA    R1,IOHOOK                                                        
         ST    R1,TIHOOK                                                        
         SPACE 1                                                                
PREP2    MVI   TIREAD,TLCKCDQ      SET TO READ CHECKS                           
         OI    TIQFLAGS,TIQFPBNP   ASK TO PASS BNP AS WELL                      
         OI    TIQFLAG2,TIQFSUB    SET TO PASS SUBS OF SPLITS                   
         OI    TIFINS2N,TAINSADJ   NO ADJUSTMENTS                               
         OI    TIFINSTN,TAINSCAN   NO CANCELLED INVOICES                        
         MVI   TIFINCVS,1          (NO CONVERTED RECORDS)                       
         CLI   BILLOPT,C'Y'        OPTION TO FILTER BILLED INVOICES             
         BNE   *+8                                                              
         OI    TIFINSTY,TAINSBIL                                                
         CLI   SESSOPT,C'Y'        OPTION FOR SESSION USES ONLY                 
         BNE   PREP3                                                            
         MVI   TIFPTYPE,C'S'       SET FILTER ON SESSION TYPE                   
         LA    R1,TGD              SET A(TALENT GLOBALS)                        
         ST    R1,TIATGLOB                                                      
         SPACE 1                                                                
PREP3    OI    TIFPO3N,TAPDORET    SET DON'T WANT RETRO PAYMENTS                
         CLI   RETROOPT,C'A'       IF OPTION TO GET RETROS ALSO                 
         BE    PREP15                                                           
         CLI   RETROOPT,C'Y'       IF OPTION TO GET ONLY RETROS                 
         BNE   *+12                                                             
         OI    TIFPO3Y,TAPDORET      SET ONLY WANT RETRO PAYMENTS               
PREP15   NI    TIFPO3N,ALL-TAPDORET  SET WANT RETRO PAYMENTS ALSO               
         SPACE 1                                                                
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
         CLI   ACTVSW,C'N'         IF PREVIOUS ACTIVITY                         
         BE    PREP20                                                           
         BAS   RE,AUDIT            ENSURE SOME OF PARTS = WHOLE                 
         BAS   RE,SETIAMTS         SET INVOICE AMOUNTS                          
         BAS   RE,PUTSORTI         PUT INVOICE HEADER TO SORT (TYPE 1)          
         BAS   RE,PROCTOT          TOTALS                                       
*                                                                               
PREP20   CLC   TIFUN,=C'SAG'       IF WE JUST DID SAG,                          
         BNE   PREP40                                                           
         MVC   TIFUN,=C'SEG'          HANDLE SEG ON SAME TAPE                   
         B     PREP2                                                            
         SPACE 1                                                                
PREP40   BAS   RE,DOREST           SORT, PRINT AND WRITE TAPE                   
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PROCESS RECORDS FROM SYSIO                            
         SPACE 1                                                                
IOHOOK   NTR1                                                                   
         CLI   TIMODE,PROCINV      IF INVOICE HOOK                              
         BNE   IOHK20                                                           
*                                                                               
         USING TLIND,R4                                                         
         L     R4,TIAREC           R4=A(INVOICE RECORD)                         
         CLC   =C'2430',TLINAGY                                                 
         BNE   IOHK00                                                           
         CLC   =X'DFECFDFE6CFF',TLININV     B30193                              
         BE    XIT                                                              
         CLC   =X'DFECFDFE6BFF',TLININV     B30194                              
         BE    XIT                                                              
         CLC   =X'DFECFDFE6AFF',TLININV     B30195                              
         BE    XIT                                                              
         CLC   =X'DFECFDFE69FF',TLININV     B30196                              
         BE    XIT                                                              
         CLC   =X'DFECFDFE68FF',TLININV     B30197                              
         BE    XIT                                                              
         CLC   =X'DFECFCFF6CFF',TLININV     C30093                              
         BE    XIT                                                              
         CLC   =X'DFECFBFF97FF',TLININV     D30068                              
         BE    XIT                                                              
         CLC   =X'DFECFBFF96FF',TLININV     D30069                              
         BE    XIT                                                              
         CLC   =X'DFECFBFF8FFF',TLININV     D30070                              
         BE    XIT                                                              
         CLC   =X'DFECFBFF8EFF',TLININV     D30071                              
         BE    XIT                                                              
         CLC   =X'DFECFBFF8DFF',TLININV     D30072                              
         BE    XIT                                                              
         DROP  R4                                                               
*                                                                               
IOHK00   CLI   ACTVSW,C'N'         IF PREVIOUS ACTIVITY                         
         BE    IOHK10                                                           
         BAS   RE,AUDIT            ENSURE SOME OF PARTS = WHOLE                 
         BAS   RE,SETIAMTS         SET INVOICE AMOUNTS                          
         BAS   RE,PUTSORTI         PUT INVOICE HEADER TO SORT (TYPE 1)          
         BAS   RE,PROCTOT          TOTALS                                       
         MVI   ACTVSW,C'N'                                                      
*                                                                               
IOHK10   BAS   RE,SETINV           SET INVOICE INFORMATION                      
         B     XIT                                                              
*                                                                               
IOHK20   CLI   TIMODE,PROCREC      IF THERE IS SOMETHING INTERESTING            
         BNE   XIT                                                              
                                                                                
         USING TLCKD,R4                                                         
         L     R4,TIAREC           R4=A(INVOICE RECORD)                         
         CLC   =C'2430',TLCKAGY                                                 
         BNE   IOHK30                                                           
         CLC   =X'201302019300',TLCKINV     B30193                              
         BE    XIT                                                              
         CLC   =X'201302019400',TLCKINV     B30194                              
         BE    XIT                                                              
         CLC   =X'201302019500',TLCKINV     B30195                              
         BE    XIT                                                              
         CLC   =X'201302019600',TLCKINV     B30196                              
         BE    XIT                                                              
         CLC   =X'201302019700',TLCKINV     B30197                              
         BE    XIT                                                              
         CLC   =X'201303009300',TLCKINV     C30093                              
         BE    XIT                                                              
         CLC   =X'201304006800',TLCKINV     D30068                              
         BE    XIT                                                              
         CLC   =X'201304006900',TLCKINV     D30069                              
         BE    XIT                                                              
         CLC   =X'201304007000',TLCKINV     D30070                              
         BE    XIT                                                              
         CLC   =X'201304007100',TLCKINV     D30071                              
         BE    XIT                                                              
         CLC   =X'201304007200',TLCKINV     D30072                              
         BE    XIT                                                              
         DROP  R4                                                               
*                                                                               
IOHK30   BAS   RE,SETCHK           ACCUMULATE CHECK DOLLARS                     
         B     XIT                                                              
         EJECT                                                                  
*              START BUILDING TYPE 1 RECORD WITH INVOICE INFORMATION            
         SPACE 1                                                                
SETINV   NTR1                                                                   
         BRAS  RE,CLEARTAP                                                      
         LA    R5,TAPEIO           R5=A(TAPEIO AREA)                            
         USING TAPED,R5                                                         
         L     R4,TIAREC           R4=A(INVOICE RECORD)                         
         USING TLIND,R4                                                         
*                                                                               
         CLC   TLINAGY,=C'999999'  SKIP ADJUSTMENTS                             
         BE    SETINVNO                                                         
*                                                                               
         CLI   NEWOPT,C'Y'         IF NEW OPTION ON                             
         BNE   SETINV1                                                          
         MVC   AIO,TIAREC                                                       
         MVI   ELCODE,TANUELQ      ONLY WANT AUTH/PO = NEW                      
         GOTO1 GETL,DMCB,(1,=AL1(TANUTAUT))                                     
         MVC   AIO,AIO1                                                         
         BNE   SETINVNO                                                         
         L     R4,TGELEM                                                        
         USING TANUD,R4                                                         
         CLC   =C'NEW',TANUMBER                                                 
         BNE   SETINVNO                                                         
*                                                                               
SETINV1  L     R4,TIAREC           R4=A(INVOICE RECORD)                         
         USING TLIND,R4                                                         
         BRAS  RE,SETCAN           SET CANADIAN CONVERSION RATE                 
         BRAS  RE,FIXCAN           CONVERT INVOICE AMOUNTS TO US DOLLAR         
*                                                                               
         BRAS  RE,SETRET           SET RETRO STATUS                             
*                                                                               
         MVI   ACTVSW,C'Y'         SET ACTIVITY SWITCH                          
         MVC   INAGY,TLINAGY                                                    
         MVC   WORK(6),TLININV                                                  
         XC    WORK(6),=6X'FF'                                                  
         GOTO1 TINVCON,DMCB,WORK,ININV,DATCON                                   
         MVI   INTYPE,C'1'         TYPE 1 RECORDS                               
         BAS   RE,PRESET1          DEFAULT SOME FIELDS                          
         XC    TOTIAMTS,TOTIAMTS                                                
*                                                                               
         MVI   ELCODE,0                                                         
         BAS   RE,GETEL                                                         
         B     *+8                                                              
SETINV2  BAS   RE,NEXTEL                                                        
         BNE   SETINV3                                                          
*                                                                               
         CLI   0(R4),TABDELQ                                                    
         BE    SETIBD                                                           
         CLI   0(R4),TAINELQ                                                    
         BE    SETIIN                                                           
         CLI   0(R4),TACOELQ                                                    
         BE    SETICO                                                           
         CLI   0(R4),TACSELQ                                                    
         BE    SETICS                                                           
         CLI   0(R4),TACCELQ                                                    
         BE    SETICC                                                           
         CLI   0(R4),TACMELQ                                                    
         BE    SETICM                                                           
         CLI   0(R4),TALFELQ                                                    
         BE    SETILF                                                           
         CLI   0(R4),TAFNELQ                                                    
         BE    SETIFN                                                           
         CLI   0(R4),TANUELQ                                                    
         BE    SETINU                                                           
         CLI   0(R4),TAUPELQ                                                    
         BE    SETIUP                                                           
         CLI   0(R4),TAPDELQ                                                    
         BE    SETIPD                                                           
         B     SETINV2                                                          
*                                                                               
SETINV3  BRAS  RE,LOOKSUB          LOOK AT SUBSIDIARIES                         
         B     XIT                                                              
*                                                                               
*                                                                               
SETINVNO MVI   TIMODE,PROCNOCK                                                  
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINES THAT PROCESS ELEMENTS FROM INVOICE RECORDS              
         SPACE 2                                                                
*              PROCESS BILLING DETAILS ELEMENT                                  
         SPACE 1                                                                
         USING TABDD,R4                                                         
SETIBD   L     R1,TABDTAX                                                       
         A     R1,TABDFICR                                                      
         A     R1,TABDGST                                                       
         A     R1,TABDPST                                                       
         ST    R1,TOTTAX                                                        
*                                                                               
         L     R1,TABDHND                                                       
         A     R1,TABDHNDC                                                      
         ST    R1,TOTHAND                                                       
*                                                                               
         A     R1,TABDSIGN                                                      
         L     R1,TABDACOM                                                      
         MVC   TOTSIGF,TABDSIGN                                                 
         MVC   TOTACOM,TABDACOM                                                 
*                                                                               
         MVC   TOTCSF,TABDCSF                                                   
*                                                                               
         MVC   TOTAMT,TABDTOT                                                   
         B     SETINV2                                                          
         SPACE 2                                                                
*              PROCESS INVOICE DETAILS ELEMENT                                  
         SPACE 1                                                                
         USING TAIND,R4                                                         
SETIIN   GOTO1 CONVDATE,DMCB,TAINBDTE,INBILDTE                                  
         B     SETINV2                                                          
         SPACE 2                                                                
*              PROCESS COMMERCIAL DETAILS ELEMENT                               
         SPACE 1                                                                
         USING TACOD,R4                                                         
SETICO   MVC   INCID,TACOCID                                                    
         GOTO1 CONVDATE,DMCB,TACOFCYC,INFFCDTE                                  
         GOTO1 CONVDATE,DMCB,TACODUB,INDUBDTE                                   
         GOTO1 CONVDATE,DMCB,TACOAIR,INAIRDTE                                   
         MVC   INMEDIA,TACOMED                                                  
         MVC   INCTYPE,TACOTYPE                                                 
         MVC   INTID,TACOTID       TRACK ID                                     
         EDIT  (1,TACOTLN),(3,INTLEN),FILL=0,ZERO=NOBLANK                       
         MVC   INADST,TACOADST     ADDENDUM STATE                               
         EDIT  (1,TACOSEC),(3,INCOSEC),FILL=0,ZERO=NOBLANK                      
         B     SETINV2                                                          
         EJECT                                                                  
*              ROUTINES THAT PROCESS ELEMENTS FROM INVOICE RECORDS              
         SPACE 2                                                                
*              PROCESS FREE FORM NAME ELEMENT                                   
         SPACE 1                                                                
         USING TAFND,R4                                                         
SETIFN   LA    R2,INTITLE                                                       
         CLI   TAFNTYPE,TAFNTTTL                                                
         BE    *+16                                                             
         LA    R2,INPRDNM                                                       
         CLI   TAFNTYPE,TAFNTPRD                                                
         BNE   SETINV2                                                          
         ZIC   R1,TAFNLEN                                                       
         AHI   R1,-4                                                            
         CHI   R1,29                                                            
         BL    *+8                                                              
         LA    R1,29                                                            
         EX    R1,*+8                                                           
         B     SETINV2                                                          
         MVC   0(0,R2),TAFNNAME                                                 
         SPACE 2                                                                
*              PROCESS FREE FORM NUMBER ELEMENT                                 
         SPACE 1                                                                
         USING TANUD,R4                                                         
SETINU   LA    R2,INEST                                                         
         LA    R3,14                                                            
         CLI   TANUTYPE,TANUTEST                                                
         BE    SETINU2                                                          
         LA    R2,INPO                                                          
         LA    R3,9                                                             
         CLI   TANUTYPE,TANUTAUT                                                
         BE    SETINU2                                                          
         LA    R2,INSIGN                                                        
         LA    R3,5                                                             
         CLI   TANUTYPE,TANUTSIG                                                
         BNE   SETINV2                                                          
         SPACE 1                                                                
SETINU2  ZIC   R1,TANULEN                                                       
         AHI   R1,-4                                                            
         CR    R1,R3                                                            
         BL    *+6                                                              
         LR    R1,R3                                                            
         EX    R1,*+8                                                           
         B     SETINV2                                                          
         MVC   0(0,R2),TANUMBER                                                 
         EJECT                                                                  
*              ROUTINES THAT PROCESS ELEMENTS FROM INVOICE RECORDS              
         SPACE 2                                                                
*              PROCESS LIFT DETAILS ELEMENT                                     
         SPACE 1                                                                
         USING TALFD,R4                                                         
SETILF   MVC   INLFTID,TALFLID                                                  
         EDIT  (1,TALFSEC),(3,INLFSEC),FILL=0,ZERO=NOBLANK                      
         B     SETINV2                                                          
         SPACE 2                                                                
*              PROCESS COMMERCIAL STUDIO ELEMENTS                               
         SPACE 1                                                                
         USING TACSD,R4                                                         
SETICS   CLI   TACSTYPE,TACSTYPF                                                
         BE    SETICSF                                                          
         CLI   TACSTYPE,TACSTYPM                                                
         BE    SETICSM                                                          
         CLI   TACSTYPE,TACSTYPR                                                
         BE    SETICSR                                                          
         B     SETINV2                                                          
         SPACE 1                                                                
SETICSF  MVC   INFSTUD(12),TACSSTUD                                             
         MVC   INFCITY(12),TACSCITY                                             
         GOTO1 CONVDATE,DMCB,TACSDATE,INFLMDTE                                  
         B     SETINV2                                                          
         SPACE 1                                                                
SETICSM  CLC   TIFUN,=C'AFM'       IF AFM UNION FILTER                          
         BE    *+12                                                             
         CLI   INCTYPE,C'M'        OR IF COMML TYPE 'M'                         
         BNE   SETINV2                                                          
         MVC   INRSTUD(12),TACSSTUD             SET MUSIC STUDIO                
         MVC   INRCITY(12),TACSCITY                 MUSIC CITY                  
         GOTO1 CONVDATE,DMCB,TACSDATE,INRECDTE      MUSIC DATE                  
         B     SETINV2                                                          
         SPACE 1                                                                
SETICSR  CLC   TIFUN,=C'AFM'       IF AFM UNION FILTER                          
         BE    *+12                                                             
         CLI   INCTYPE,C'M'        OR IF COMML TYPE 'M'                         
         BNE   *+14                                                             
         CLC   INRECDTE,CHAR00S    AND NO MUSIC INFO FOUND                      
         BNE   SETINV2                                                          
         MVC   INRSTUD(12),TACSSTUD  USE RECORDING INFO                         
         MVC   INRCITY(12),TACSCITY                                             
         GOTO1 CONVDATE,DMCB,TACSDATE,INRECDTE                                  
         B     SETINV2                                                          
         EJECT                                                                  
*              ROUTINES THAT PROCESS ELEMENTS FROM INVOICE RECORDS              
         SPACE 2                                                                
*              PROCESS UPGRADE ELEMENT                                          
         SPACE 1                                                                
         USING TAUPD,R4                                                         
SETIUP   CLC   TIUSE,=C'CBL'       CABLE UPGRADES -                             
         BE    SETIUPC               4 BYTES - NO MAJOR                         
         CLC   TIUSE,=C'SCB'                                                    
         BE    SETIUPC                                                          
         CLC   TIUSE,=C'LCB'       LOCAL IS DIFFERENT AGAIN                     
         BE    SETIUPL                                                          
*                                                                               
         GOTO1 =A(CONVMAJ),DMCB,(RC),TAUPIMAJ,INUMAJ                            
         EDIT  (2,TAUPIUNT),(3,INUUNITS),FILL=0,ZERO=NOBLANK                    
         B     SETINV2                                                          
*                                                                               
SETIUPC  EDIT  (2,TAUPIUNT),(4,INCUUNTS),FILL=0,ZERO=NOBLANK                    
         B     SETINV2                                                          
*                                                                               
SETIUPL  MVC   INUMAJ,TAUPILCM                                                  
         B     SETINV2                                                          
         SPACE 2                                                                
*              PROCESS COMMERCIAL CONTRACT ELEMENT                              
         SPACE 1                                                                
         USING TACCD,R4                                                         
SETICC   MVC   INAFMC1,TACCCON                                                  
         CLI   TACCNCON,1                                                       
         BE    *+10                                                             
         MVC   INAFMC2,TACCCON+12                                               
         B     SETINV2                                                          
         SPACE 2                                                                
*              PROCESS COMMENTS ELEMENT                                         
         SPACE 1                                                                
         USING TACMD,R4                                                         
SETICM   ZIC   R1,TACMLEN                                                       
         AHI   R1,-4                                                            
         CHI   R1,39                                                            
         BL    *+8                                                              
         LA    R1,39                                                            
         EX    R1,*+8                                                           
         B     SETINV2                                                          
         MVC   INCOMM(0),TACMCOMM                                               
         EJECT                                                                  
*              ROUTINES THAT PROCESS ELEMENTS FROM INVOICE RECORDS              
         SPACE 2                                                                
*              PROCESS PAYMENT DETAILS                                          
         SPACE 1                                                                
         USING TAPDD,R4                                                         
SETIPD   MVC   INUSE,TAPDUSE                                                    
         MVC   INUTYP,TAPDTYPE                                                  
         OI    INUTYP,X'F0'                                                     
         CLI   TAPDTYPE,10                                                      
         BL    *+8                                                              
         MVI   INUTYP,C'A'                                                      
         GOTO1 USEVAL,DMCB,TAPDUSE,TAPDTYPE                                     
         MVC   INPDESC(16),TGUSNAME                                             
         MVI   INSESREU,C'S'       SESSION                                      
         TM    TGUSSTAT,SESSION                                                 
         BO    *+8                                                              
         MVI   INSESREU,C'R'       REUSE                                        
         LA    R2,INUSDESC                                                      
         BAS   RE,EDITDET                                                       
         GOTO1 CONVDATE,DMCB,TAPDCYCS,INCYCSTR                                  
         GOTO1 CONVDATE,DMCB,TAPDCYCE,INCYCEND                                  
         MVC   INWEEKS,=C'26'                                                   
         CLI   TGUSWKS,X'86'       (6 MONTHS)                                   
         BE    SETIPD2                                                          
         MVC   INWEEKS,=C'13'                                                   
         CLI   TGUSWKS,X'83'       (3 MONTHS)                                   
         BE    SETIPD2                                                          
         MVC   INWEEKS,CHAR00S                                                  
         TM    TGUSWKS,X'80'                                                    
         BO    SETIPD2                                                          
         ZIC   R1,TGUSWKS                                                       
         SLL   R1,26               STRIP OUT X'80' AND X'40'                    
         SRL   R1,26                                                            
         SR    R0,R0                                                            
         TM    TGUSWKS,X'40'       X'40' MEANS DAYS                             
         BNO   *+12                                                             
         LA    R1,7(R1)            ROUND UP TO NEAREST N'WEEKS                  
         D     R0,=F'7'                                                         
         EDIT  (R1),(2,INWEEKS),FILL=0                                          
*                                                                               
SETIPD2  CLC   TAPDUSE,=C'CBL'     CABLE USE 4 BYTES FOR UNITS                  
         BE    *+14                                                             
         CLC   TAPDUSE,=C'SCB'                                                  
         BNE   SETIPD5                                                          
         EDIT  (2,TAPDUNIT),(4,INCUNITS),FILL=0                                 
         B     SETIPD8                                                          
*                                                                               
SETIPD5  EDIT  (2,TAPDUNIT),(3,INUNITS),FILL=0                                  
         CLC   TAPDUSE,=C'LCB'     LOCAL CABLE?                                 
         BNE   SETIPD7                                                          
         EDIT  (1,TAPDTYPE),(1,INMAJ),FILL=0,ZERO=NOBLANK                       
         B     SETIPD8                                                          
SETIPD7  GOTO1 =A(CONVMAJ),DMCB,(RC),TAPDMAJ,INMAJ                              
*                                                                               
SETIPD8  MVC   INCRIND,CHAR00S                                                  
         TM    TAPDPSTS,TAPDPCRD                                                
         BNO   *+8                                                              
         MVI   INCRIND,C'1'                                                     
         MVC   INCANIND,CHAR00S                                                 
         TM    TAPDOPT1,TAPDOCAN                                                
         BNO   *+8                                                              
         MVI   INCANIND,C'1'                                                    
         MVC   INEMP,TAPDEOR       EMPLOYER                                     
         MVC   INCLI,TAPDCLI       CLIENT                                       
         MVC   INPRD,TAPDPRD       PRODUCT                                      
         EDIT  (1,TAPDTAGS),(3,INTAGS),FILL=0,ZERO=NOBLANK                      
*                                                                               
         MVI   INVER,C'1'          1=MAIN ONLY                                  
         TM    TAPDSTAT,TAPDSLFT                                                
         BNO   SETIPDX                                                          
         MVC   INVER,CHAR00S       0=BOTH                                       
*                                  2=LIFT ONLY (NOT AVAILABLE)                  
SETIPDX  B     SETINV2                                                          
         EJECT                                                                  
*              ENSURE SUM OF PARTS = WHOLE                                      
*              ON CANADIAN INVOICES CONVERTED TO US DOLLARS                     
         SPACE 1                                                                
AUDIT    NTR1                                                                   
         L     R1,TOTAMT           R1=INVOICE TOTAL                             
         L     R4,TOTGROSS                                                      
         A     R4,TOTMISC                                                       
         A     R4,TOTTAX                                                        
         A     R4,TOTPNH                                                        
         A     R4,TOTHAND                                                       
         A     R4,TOTSIGF                                                       
         A     R4,TOTACOM                                                       
         A     R4,TOTCSF                                                        
         SR    R1,R4                                                            
         BZ    AUDITX                                                           
         OC    CANRATE,CANRATE     IF THIS ISN'T CANADIAN INVOICE               
         BNZ   AUDIT10                                                          
         TM    SVPDOPT3,TAPDORET   OR RETRO PAYMENT                             
         BO    AUDIT10                                                          
         OC    SVFEE,SVFEE         IF SUB INVOICE HAS EXTRA FEE                 
         BZ    AUDIT05                                                          
         C     R1,SVFEE            SEE IF DIFFERENCE = FEE                      
         BE    AUDIT10                                                          
AUDIT05  DC    H'0'                DIE IF SUM OF PARTS NEQ WHOLE                
AUDIT10  L     R4,TOTHAND          ELSE, ADD DIFF TO HANDLING AMT               
         AR    R1,R4                                                            
         ST    R1,TOTHAND                                                       
AUDITX   B     XIT                                                              
         EJECT                                                                  
*              SET INVOICE AMOUNTS ACCUMULATED THROUGH CHKS                     
         SPACE 1                                                                
SETIAMTS NTR1                                                                   
         LA    R5,TAPEIO           R5=A(TAPEIO AREA)                            
         USING TAPED,R5                                                         
         GOTO1 CONVCASH,DMCB,TOTAMT,INAMT                                       
         GOTO1 CONVCASH,DMCB,TOTHAND,INHAND                                     
         GOTO1 CONVCASH,DMCB,TOTTAX,INTAX                                       
*                                                                               
         GOTO1 CONVCASH,DMCB,TOTGROSS,INGROSS                                   
         L     RF,TOTMISC                                                       
         A     RF,TOTCSF                                                        
         ST    RF,TOTMISC                                                       
         GOTO1 CONVCASH,DMCB,TOTMISC,INMISC                                     
         GOTO1 CONVCASH,DMCB,TOTSPNH,INSPNH                                     
         GOTO1 CONVCASH,DMCB,TOTPNH,INPNH                                       
*                                                                               
         GOTO1 CONVCASH,DMCB,TOTSIGF,INSIGFEE                                   
         GOTO1 CONVCASH,DMCB,TOTACOM,INAGYCOM                                   
*                                                                               
         MVC   SAVETPKY,TAPEKEY                                                 
         MVC   SAVEAMTS,TAPEAMTS                                                
         B     XIT                                                              
         EJECT                                                                  
*              BUILD INVOICE HEADER (FROM CHECK)                                
         SPACE 2                                                                
SETCHK   NTR1                                                                   
         L     R4,TIAREC                                                        
         BRAS  RE,FIXCAN           FIX CHECK AMOUNTS                            
         SPACE 1                                                                
         MVI   ELCODE,0                                                         
         BAS   RE,GETEL                                                         
         B     *+8                                                              
         SPACE 1                                                                
SETCHK2  BAS   RE,NEXTEL                                                        
         BNE   XIT                                                              
         CLI   0(R4),TAPDELQ                                                    
         BE    SETCPD                                                           
         B     SETCHK2                                                          
         SPACE 1                                                                
*              ACCUMULATE CHECK AMOUNTS                                         
*                                                                               
         USING TAPDD,R4                                                         
SETCPD   L     R1,TOTGROSS                                                      
         A     R1,TAPDPAYI                                                      
         ST    R1,TOTGROSS                                                      
*                                                                               
         L     R1,TOTMISC                                                       
         A     R1,TAPDPAYC                                                      
         A     R1,TAPDREXP                                                      
         S     R1,TAPDINR                                                       
         ST    R1,TOTMISC                                                       
*                                                                               
         L     R1,TOTSPNH                                                       
         A     R1,TAPDSPNH                                                      
         ST    R1,TOTSPNH                                                       
*                                                                               
         L     R1,TOTPNH                                                        
         CLI   RETROOPT,C'Y'       IF GETTING RETROS ONLY                       
         BNE   SETCPD4                                                          
         A     R1,TAPDPAYI         SUBJ. TO P&H = PAYMENT AMOUNT                
         A     R1,TAPDPAYC                                                      
         A     R1,TAPDREXP                                                      
         B     SETCPD6                                                          
SETCPD4  A     R1,TAPDPNH                                                       
         A     R1,TAPDINR                                                       
*                                                                               
SETCPD6  ST    R1,TOTPNH                                                        
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINES FOR TOTAL RECORDS                                       
         SPACE 3                                                                
PROCTOT  NTR1                                                                   
         BRAS  RE,CLEARTAP                                                      
         LA    R5,TAPEIO                                                        
         USING TAPED,R5                                                         
         MVC   TAPEKEY,SAVETPKY                                                 
         MVC   TTINV,=C'999999'                                                 
         MVI   TTTYPE,C'4'                                                      
         MVC   TAPEAMTS,SAVEAMTS                                                
         BAS   RE,PUTSORTA         PUT AGENCY TOTAL                             
*                                                                               
         MVI   TTTYPE,C'5'                                                      
         MVC   TTAGY,=C'999999'                                                 
         BAS   RE,PUTSORTU         PUT UNION TOTALS (TAPE ONLY)                 
*                                                                               
         MVI   TTTYPE,C'5'                                                      
         MVC   TTAGY,=C'999998'                                                 
         MVC   WORK(1),CHAR00S                                                  
         MVC   WORK+1(9),INPNH                                                  
         MVC   GTPNH,WORK                                                       
         MVC   WORK+1(9),INSPNH                                                 
         MVC   GTSPNH,WORK                                                      
         MVC   WORK+1(9),INMISC                                                 
         MVC   GTMISC,WORK                                                      
         MVC   WORK+1(9),INGROSS                                                
         MVC   GTGROSS,WORK                                                     
         BAS   RE,PUTSRTU2         PUT UNION TOTAL (PRINT ONLY)                 
*                                                                               
         CLC   TTUN,=C'SAG'                                                     
         BE    *+14                                                             
         CLC   TTUN,=C'SEG'                                                     
         BNE   XIT                                                              
         MVI   TTTYPE,C'6'                                                      
         MVC   TTUN,=C'999999'                                                  
         MVC   TTEMP,=C'999999'                                                 
         BAS   RE,PUTSORTG         PUT GRAND TOTAL                              
         B     XIT                                                              
         EJECT                                                                  
*              NOW HANDLE THE OUTPUT                                            
         SPACE 1                                                                
DOREST   NTR1                                                                   
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         SPACE 1                                                                
         ZAP   ATOTAL,=P'0'                                                     
         ZAP   ETOTAL,=P'0'                                                     
         ZAP   GTOTAL,=P'0'                                                     
         SPACE 1                                                                
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
         BAS   RE,PUTTAPE          WRITE OUT LAST TAPE RECORD                   
         BAS   RE,DOREPORT         AND PRINT REPORT                             
         B     XIT                                                              
         SPACE 1                                                                
DOREST4  MOVE  (SORTIO,700),0(R2)  ELSE, MOVE SORT REC TO LOCAL STORAGE         
         CLC   TAPEKEY,0(R2)       SAME KEY?                                    
         BNE   DOREST6                                                          
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
         SPACE 1                                                                
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
         BAS   RE,PUTTAPE             WRITE OUT PENDING TAPE RECORD             
         BAS   RE,DOREPORT                  AND PRINT REPORT                    
         MOVE  (TAPEIO,700),0(R2)  MOVE IN NEW RECORD                           
         GOTO1 =A(FLESHOUT),DMCB,(RC)  AND FLESH OUT NAMES ETC                  
         CLI   TTTYPE,C'4'                                                      
         BE    DOREST2                                                          
         CLI   TTTYPE,C'5'                                                      
         BE    DOREST2                                                          
         CLI   TTTYPE,C'6'                                                      
         BE    DOREST2                                                          
         DROP  R5                                                               
         USING TAPED,R2                                                         
         LA    R2,TAPEIO                                                        
         LA    R3,TOTALS                                                        
         LA    R0,3                                                             
DOREST7  PACK  WORK(9),INAMT(9)                                                 
         AP    WORK+1(8),0(8,R3)                                                
         MVC   0(8,R3),WORK+1                                                   
         LA    R3,8(R3)                                                         
         BCT   R0,DOREST7                                                       
         DROP  R2                                                               
         B     DOREST2                                                          
         EJECT                                                                  
*              PRINT REPORT                                                     
         SPACE 1                                                                
DOREPORT NTR1                                                                   
         LA    R2,P                                                             
         USING PRINTD,R2                                                        
         LA    R5,TAPEIO                                                        
         USING TAPED,R5                                                         
         CLI   INTYPE,C'1'                                                      
         BE    REPINV                                                           
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
         B     XIT                                                              
         SPACE 1                                                                
REPINV   MVC   PUNION,INUN                                                      
         MVC   PEMP,INEMP                                                       
         LA    R5,SORTIO           GET AGENCY FROM SORT RECORD BECAUSE          
         MVC   PAGENCY,INAGY       TAPE MAY HAVE BEEN OVERWRITTEN BY            
         LA    R5,TAPEIO           AGYOPT                                       
         MVC   PINVOICE(6),ININV                                                
         MVC   PCLIENT,INCLI                                                    
         MVC   PCID,INCID                                                       
         MVC   PUSE,INUSE                                                       
         CLC   INCYCSTR,CHAR00S                                                 
         BE    REPAMT                                                           
         GOTO1 DATCON,DMCB,(0,INCYCSTR),(8,PCYCLE)                              
         CLC   INCYCEND,CHAR00S                                                 
         BE    REPAMT                                                           
         MVI   PCYCLE+8,C'-'                                                    
         GOTO1 DATCON,DMCB,(0,INCYCEND),(8,PCYCLE+9)                            
         SPACE 1                                                                
REPAMT   PACK  DUB,INGROSS                                                      
         EDIT  (P8,DUB),PGROSS,2,MINUS=YES,ZERO=BLANK                           
         PACK  DUB,INMISC                                                       
         EDIT  (P8,DUB),PMISC,2,MINUS=YES,ZERO=BLANK                            
*        PACK  DUB,INSPNH                                                       
*        EDIT  (P8,DUB),PSPNH,2,MINUS=YES,ZERO=BLANK                            
*        PACK  DUB,INPNH                                                        
*        EDIT  (P8,DUB),PPNH,2,MINUS=YES,ZERO=BLANK                             
         SPACE 1                                                                
         CLI   TTTYPE,C'4'                                                      
         BE    RAMT14                                                           
         CLI   TTTYPE,C'5'                                                      
         BE    RAMT15                                                           
         CLI   TTTYPE,C'6'                                                      
         BE    RAMT16                                                           
         PACK  DUB,INAMT                                                        
         EDIT  (P8,DUB),PSPNH,2,MINUS=YES,ZERO=BLANK                            
         B     RAMT20                                                           
         SPACE 1                                                                
RAMT14   EDIT  (P8,ATOTAL),PSPNH,2,MINUS=YES,ZERO=BLANK                         
         ZAP   ATOTAL,=P'0'                                                     
         B     RAMT20                                                           
         SPACE 1                                                                
RAMT15   EDIT  (P8,ETOTAL),PSPNH,2,MINUS=YES,ZERO=BLANK                         
         ZAP   ETOTAL,=P'0'                                                     
         B     RAMT20                                                           
         SPACE 1                                                                
RAMT16   EDIT  (P8,GTOTAL),PSPNH,2,MINUS=YES,ZERO=BLANK                         
         ZAP   GTOTAL,=P'0'                                                     
         B     RAMT20                                                           
         SPACE 1                                                                
RAMT20   BAS   RE,SPLAT                                                         
         CLI   TTTYPE,C'1'                                                      
         BE    XIT                                                              
         BAS   RE,SPLAT                                                         
         B     XIT                                                              
         EJECT                                                                  
REPGRAND PACK  DUB,GTGROSS                                                      
         EDIT  (P8,DUB),PGROSS,2,MINUS=YES,ZERO=BLANK                           
         PACK  DUB,GTMISC                                                       
         EDIT  (P8,DUB),PMISC,2,MINUS=YES,ZERO=BLANK                            
*        PACK  DUB,GTPNH                                                        
*        EDIT  (P8,DUB),PPNH,2,MINUS=YES,ZERO=BLANK                             
         SPACE 1                                                                
         CLI   TTTYPE,C'5'                                                      
         BNE   RGRN10                                                           
         EDIT  (P8,ETOTAL),PSPNH,2,MINUS=YES,ZERO=BLANK                         
         ZAP   ETOTAL,=P'0'                                                     
         B     RGRN20                                                           
         SPACE 1                                                                
RGRN10   CLI   TTTYPE,C'6'                                                      
         BNE   RGRN20                                                           
         EDIT  (P8,GTOTAL),PSPNH,2,MINUS=YES,ZERO=BLANK                         
         ZAP   GTOTAL,=P'0'                                                     
         SPACE 1                                                                
RGRN20   BAS   RE,SPLAT                                                         
         BAS   RE,SPLAT                                                         
         B     XIT                                                              
         SPACE 2                                                                
TOTALS   DS    0XL24                                                            
ATOTAL   DS    XL8                                                              
ETOTAL   DS    XL8                                                              
GTOTAL   DS    XL8                                                              
         EJECT                                                                  
*              UTILITIES                                                        
         SPACE 3                                                                
         SPACE 1                                                                
*              PRE-SET CERTAIN FIELDS ON TYPE 1 RECORD                          
         SPACE 1                                                                
PRESET1  NTR1                                                                   
         USING TAPED,R5                                                         
         MVC   INFLMDTE,CHAR00S                                                 
         MVC   INRECDTE,CHAR00S                                                 
         MVC   INLFSEC,CHAR00S                                                  
         MVC   INLOCS,CHAR00S                                                   
         ZAP   INGROSS,=P'0'                                                    
         ZAP   INMISC,=P'0'                                                     
         ZAP   INSPNH,=P'0'                                                     
         ZAP   INPNH,=P'0'                                                      
         ZAP   INTAX,=P'0'                                                      
         ZAP   INHAND,=P'0'                                                     
         ZAP   INAMT,=P'0'                                                      
         MVC   INUUNITS,CHAR00S                                                 
         MVI   INUMAJ,C'0'                                                      
         MVC   INUN,=C'***'                                                     
         B     XIT                                                              
         DROP  R5                                                               
CONVDATE NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         MVC   0(6,R3),CHAR00S                                                  
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
*              EDIT DETAILS OF PAYMENT                                          
         SPACE 3                                                                
EDITDET  NTR1                                                                   
*                                  R2=A(OUTPUT AREA)                            
         USING TAPDD,R4            R4=A(PAYMENT DETAILS ELEMENT)                
         OC    TAPDSTUS,TAPDSTUS   *** FUDGE                                    
         BNZ   INPDU10                                                          
         OC    TAPDUNIT(3),TAPDUNIT                                             
         BNZ   INPDU20                                                          
         B     XIT                                                              
         SPACE 1                                                                
INPDU10  CLC   TAPDUSES,=H'1'                                                   
         BNE   INPDU12                                                          
         MVC   0(3,R2),=C'USE'                                                  
         EDIT  (2,TAPDSTUS),(4,4(R2)),ALIGN=LEFT                                
         B     XIT                                                              
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
         B     XIT                                                              
         SPACE 1                                                                
INPDU20  GOTO1 MAJVAL,DMCB,(X'80',TAPDMAJ)                                      
         MVC   0(L'TGMACHAR,R2),TGMACHAR                                        
         LA    R2,L'TGMACHAR-1(R2)                                              
         CLI   0(R2),C' '                                                       
         BNE   *+8                                                              
         BCT   R2,*-8                                                           
         LA    R2,2(R2)                                                         
         OC    TAPDUNIT,TAPDUNIT                                                
         BZ    XIT                                                              
         EDIT  (2,TAPDUNIT),(4,0(R2)),ALIGN=LEFT,ZERO=NOBLANK                   
         AR    R2,R0                                                            
         MVC   1(5,R2),=C'UNITS'                                                
         B     XIT                                                              
         EJECT                                                                  
*              TAPE ROUTINES                                                    
         SPACE 1                                                                
OPENTAPE NTR1                                                                   
         CLI   TAPEOPT,C'Y'                                                     
         BNE   XIT                                                              
         CLI   RECNUM,MCINTER      IF MCCANN INTERFACE,                         
         BNE   OPEN20                                                           
         L     R2,=A(MCDISK)                                                    
         OPEN  ((2),OUTPUT)                                                     
         B     XIT                                                              
OPEN20   L     R2,=A(BTDISK)                                                    
         CLI   ACTEQU,ACTDOWN                                                   
         BNE   *+8                                                              
         L     R2,=A(TADOWNP)                                                   
         OPEN  ((2),OUTPUT)                                                     
         B     XIT                                                              
         SPACE 1                                                                
CLOSTAPE NTR1                                                                   
         BAS   RE,SPLAT                                                         
         LA    R2,P                                                             
         USING PRINTD,R2                                                        
         EDIT  (P6,TAPCOUNT),(7,PTOTALS),ZERO=NOBLANK                           
         MVC   PTOTALS+8(12),=C'TAPE RECORDS'                                   
         DROP  R2                                                               
         BAS   RE,SPLAT                                                         
         SPACE 1                                                                
         CLI   RECNUM,MCINTER      IF MCCANN INTERFACE,                         
         BNE   CLOS20                                                           
         L     R2,=A(MCDISK)                                                    
         CLOSE ((2))                                                            
         B     XIT                                                              
CLOS20   CLI   TAPEOPT,C'Y'                                                     
         BNE   XIT                                                              
         L     R2,=A(BTDISK)                                                    
         CLI   ACTEQU,ACTDOWN                                                   
         BNE   *+8                                                              
         L     R2,=A(TADOWNP)                                                   
         CLOSE ((2))                                                            
         B     XIT                                                              
         SPACE 1                                                                
PUTTAPE  NTR1                                                                   
         LA    RE,700              LENGTH OF TAPEIO                             
         LA    R5,TAPEIO                                                        
         USING TAPED,R5                                                         
         CLI   TTTYPE,C'5'         IF SPECIAL UNION PRINTING TOTAL              
         BNE   *+14                                                             
         CLC   TTAGY,=C'999998'                                                 
         BE    XIT                 DON'T PUT TO TAPE                            
         DROP  R5                                                               
*                                                                               
PUTTAPE3 CLI   0(R5),0             IF NULLS                                     
         BNE   *+8                                                              
         MVI   0(R5),C' '          SET TO SPACES                                
         LA    R5,1(R5)                                                         
         BCT   RE,PUTTAPE3                                                      
*                                                                               
         AP    TAPCOUNT,=P'1'                                                   
         CLI   TAPEOPT,C'Y'                                                     
         BNE   XIT                                                              
         USING TAPED,RE                                                         
         LA    RE,TAPEIO                                                        
         CLI   INTYPE,C'1'       TYPE 1 RECORD                                  
         BE    PUTTAPE4                                                         
         CLI   TTTYPE,C'4'       AGENCY TOTAL RECORD                            
         BE    PUTTAPE4                                                         
         CLI   TTTYPE,C'6'       GRAND TOTAL RECORD                             
         BNE   PUTTAPE5                                                         
PUTTAPE4 OC    AGYOPT,AGYOPT     IF AGENCY OVERRIDE OPTION SET,                 
         BZ    PUTTAPE5          OVERWRITE AGENCY NAME WITH                     
         MVC   INAGY,AGYOPT      AGENCY OVERRIDE                                
         DROP  RE                                                               
PUTTAPE5 LA    R0,TAPEIO                                                        
         L     R1,=A(BTDISK)                                                    
         CLI   ACTEQU,ACTDOWN                                                   
         BNE   *+8                                                              
         L     R1,=A(TADOWNP)                                                   
         PUT   (1),(0)                                                          
         B     XIT                                                              
         EJECT                                                                  
*              SORT UTILITIES                                                   
         SPACE 3                                                                
PUTSORTI MVC   RECTYPE,=CL16'INVOICE HEADER'                                    
         B     *+10                                                             
PUTSORTA MVC   RECTYPE,=CL16'AGENCY TOTAL'                                      
         B     *+10                                                             
PUTSORTU MVC   RECTYPE,=CL16'UNION TOTAL'                                       
         B     *+10                                                             
PUTSRTU2 MVC   RECTYPE,=CL16'UNION TOTAL'                                       
         B     *+10                                                             
PUTSORTG MVC   RECTYPE,=CL16'GRAND TOTAL'                                       
         SPACE 1                                                                
PUTSORT  NTR1                                                                   
         LA    R5,TAPEIO                                                        
         USING TAPED,R5                                                         
         CLI   SORTFRST,C'Y'                                                    
         BNE   PUTSORT2                                                         
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD                                     
         MVI   SORTFRST,C'N'                                                    
         SPACE 1                                                                
PUTSORT2 GOTO1 SORTER,DMCB,=C'PUT',TAPEIO                                       
         B     XIT                                                              
         EJECT                                                                  
*              ODD ROUTINES                                                     
         SPACE 2                                                                
SPLAT    NTR1                                                                   
*        CLI   ACTEQU,ACTDOWN                                                   
*        BE    XIT                                                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         SPACE 2                                                                
*              HEADLINES ETC                                                    
         SPACE 3                                                                
HOOK     NTR1                                                                   
         MVC   H1+52(16),=CL16'BILLING ACTIVITY'                                
         GOTO1 CENTER,DMCB,H1+52,24                                             
         GOTO1 UNDERLIN,DMCB,(24,H1+52),(X'BF',H2+52)                           
         MVC   H3+52(6),=C'PERIOD'                                              
         L     R1,APERFLD                                                       
         MVC   H3+59(17),8(R1)                                                  
         XIT1                                                                   
         SPACE 3                                                                
ERRMIS   MVI   ERROR,MISSING                                                    
         GOTO1 ERREX                                                            
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
         GOTO1 ERREX                                                            
         SPACE 1                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
CHAR00S  DC    C'000000'                                                        
SORTCARD DC    CL80'SORT FIELDS=(1,20,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=(700)'                                 
         SPACE 2                                                                
*                                                                               
BTDISK   DCB   DDNAME=BTDISK,DSORG=PS,MACRF=(PM),                      X        
               RECFM=FB,LRECL=700,BUFNO=2,BLKSIZE=7000                          
         SPACE 1                                                                
TADOWNP  DCB   DDNAME=TADOWN,DSORG=PS,MACRF=(PM),                      X        
               RECFM=FB,LRECL=700,BUFNO=2,BLKSIZE=7000                          
*                                                                               
MCDISK   DCB   DDNAME=TADOWN,DSORG=PS,MACRF=(GM,PM),                   X        
               RECFM=FB,LRECL=92,BUFNO=2,BLKSIZE=1840,EODAD=NOMORE2             
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
         SSPEC H5,102,C'INVOICE TOTAL'                                          
         SSPEC H6,102,C'-------------'                                          
*        SSPEC H5,102,C'SUBJECT TO       P AND H'                               
*        SSPEC H6,102,C'  P AND H        -------'                               
         DC    H'0'                                                             
         LTORG                                                                  
         DROP  R6                  DROP SECONDARY BASE REGISTERS                
         EJECT                                                                  
*              CONVERT MAJORS UTILITY                                           
         USING TAPED,R5            R5=A(TAPE RECORD)                            
         SPACE 2                                                                
         DS    0D                                                               
CONVMAJ  NMOD1 0,*CVMAJ*                                                        
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         SPACE 1                                                                
         LM    R2,R3,4(R1)         R2=A(MAJORS), R3=A(TAPE FIELD)               
         MVI   0(R3),C'0'          INIT TAPE FIELD                              
         CLI   0(R2),0             DONE IF NO MAJORS                            
         BE    CONVMAJX                                                         
         CLC   INUSE,=C'FGR'       IF FGR USE                                   
         BNE   *+12                                                             
         CLI   INUTYP,C'0'         AND TYPE IS 0(LOCATIONS LIKE MAJORS)         
         BE    CONVMAJ4            HANDLE DIFFERENTLY                           
         LA    R4,MAJTAB                                                        
         SPACE 1                                                                
CONVMAJ2 MVC   0(1,R3),1(R4)                                                    
         CLC   0(1,R2),0(R4)                                                    
         BE    CONVMAJX                                                         
         CLI   0(R2),X'FF'                                                      
         BE    CONVMAJX                                                         
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
CONVMAJX XIT1                                                                   
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
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              FLESH OUT RECORDS WITH NAMES ETC                                 
         SPACE 2                                                                
         DS    0D                                                               
FLESHOUT NMOD1 0,*FLESH*                                                        
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         SPACE 1                                                                
         LA    R5,TAPEIO                                                        
         USING TAPED,R5                                                         
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
*                                                                               
         CLC   INUSE,=C'INA'       ALL THESE TYPES HAVE 0101 SIGNATORY          
         BE    FLESHIN4                                                         
         CLC   INUSE,=C'INR'                                                    
         BE    FLESHIN4                                                         
         CLC   INUSE,=C'INS'                                                    
         BE    FLESHIN4                                                         
         CLC   INUSE,=C'ISS'                                                    
         BNE   FLESHIN6                                                         
                                                                                
FLESHIN4 MVC   INSIGN,SPACES        CLEAR FIELD                                 
         MVC   INSIGN(4),=CL4'0101' THESE TYPES GET 0101                        
*                                                                               
FLESHIN6 BAS   RE,NEEDSG                                                        
         MVC   INSIGNM,NEEDSHRT                                                 
         CLC   NEEDSHRT,SPACES       SHORT NAME?                                
         BH    *+10                YES                                          
         MVC   INSIGNM,NEEDNAME    NOPE, USE AGENCY                             
FLESHX   XIT1                                                                   
         EJECT                                                                  
*              ROUTINES TO ENSURE SUB RECORDS AROUND                            
         SPACE 3                                                                
NEEDAY   NTR1                                                                   
         LA    R5,TAPEIO                                                        
         USING TAPED,R5                                                         
         XC    NEEDKEY,NEEDKEY     ENSURE AGENCY AROUND                         
         LA    R4,NEEDKEY                                                       
         USING TLAYD,R4                                                         
         MVI   TLAYCD,TLAYCDQ                                                   
         MVC   TLAYAGY,INAGY                                                    
         BAS   RE,NEEDREC                                                       
         B     FLESHX                                                           
         SPACE 1                                                                
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
         B     FLESHX                                                           
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
         BE    FLESHX                                                           
         XC    TLCLAGY,TLCLAGY     THEN FOR GLOBAL CLIENT                       
         BAS   RE,NEEDREC                                                       
         B     FLESHX                                                           
         SPACE 1                                                                
NEEDPR   NTR1                                                                   
         LA    R5,TAPEIO                                                        
         USING TAPED,R5                                                         
         XC    NEEDKEY,NEEDKEY     ENSURE RECORD AROUND                         
         MVC   NEEDNAME,SPACES                                                  
         CLI   INPRD,C'A'                                                       
         BL    FLESHX                                                           
         LA    R4,NEEDKEY                                                       
         USING TLPRD,R4                                                         
         MVI   TLPRCD,TLPRCDQ                                                   
         MVC   TLPRAGY,INAGY                                                    
         MVC   TLPRCLI,INCLI                                                    
         MVC   TLPRPRD,INPRD                                                    
         BAS   RE,NEEDREC          TRY FOR AGENCY PRODUCT                       
         CLI   NEEDHIT,C'Y'                                                     
         BE    FLESHX                                                           
         XC    TLPRAGY,TLPRAGY                                                  
         XC    TLPRCLI,TLPRCLI                                                  
         BAS   RE,NEEDREC          THEN TRY FOR GLOBAL PRODUCT                  
         B     FLESHX                                                           
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
         B     FLESHX                                                           
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
         B     FLESHX                                                           
         SPACE 1                                                                
NEEDOFF  NTR1                                                                   
         L     R6,NEEDAREC                                                      
         MVI   ELCODE,TAAYELQ                                                   
         BAS   RE,GETEL2                                                        
         BNE   FLESHX                                                           
         USING TAAYD,R6                                                         
         LA    R2,OFFTAB                                                        
*                                                                               
NEEDO2   CLI   0(R2),X'FF'         END OF TABLE?                                
         BE    FLESHX                                                           
         CLC   TAAYTPOF,0(R2)                                                   
         BE    NEEDO4                                                           
         LA    R2,L'OFFTAB(R2)                                                  
         B     NEEDO2                                                           
*                                                                               
NEEDO4   MVC   INEMPNM+30-2(2),1(R2)                                            
         B     FLESHX                                                           
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
         BNE   FLESHX                                                           
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
         BAS   RE,GETEL2                                                        
         MVC   ELCODE,SAVEEL       RESTORE PRESENT ELCODE                       
         BNE   NOGOOD                                                           
         USING TANAD,R6                                                         
         ZIC   R1,TANALEN                                                       
         AHI   R1,-3                                                            
         EX    R1,*+8                                                           
         B     ITSFINE                                                          
         MVC   NEEDNAME(0),TANANAME                                             
         SPACE 1                                                                
GETW4NM  MVI   ELCODE,TAW4ELQ                                                   
         LR    R6,R4                                                            
         BAS   RE,GETEL2                                                        
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
         BAS   RE,GETEL2                                                        
         MVC   NEEDSHRT,SPACES                                                  
         MVC   ELCODE,SAVEEL                                                    
         BNE   FLESHX                                                           
         USING TASND,R6                                                         
         ZIC   R1,TASNLEN                                                       
         AHI   R1,-3                                                            
         EX    R1,*+8                                                           
         B     FLESHX                                                           
         MVC   NEEDSHRT(0),TASNAME                                              
         SPACE 1                                                                
ITSFINE  SR    R1,R1                                                            
         B     *+8                                                              
         SPACE 1                                                                
NOGOOD   LA    R1,1                                                             
         LTR   R1,R1                                                            
         B     FLESHX                                                           
         SPACE 1                                                                
         GETEL2 R6,DATADISP,ELCODE                                              
         SPACE 2                                                                
OFFTAB   DS    0CL3                OFFICE NAME TABLE                            
         DC    C'1',C'CH'                                                       
         DC    C'2',C'NY'                                                       
         DC    C'3',C'DA'                                                       
         DC    C'4',C'AT'                                                       
         DC    C'5',C'LA'                                                       
         DC    C'8',C'CH'                                                       
         DC    X'FF'                                                            
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO GET FROM DATASET AND PRINT SECOND REPORT              
         SPACE 1                                                                
         DS    0D                                                               
PREPD    NMOD1 0,*PREPD*                                                        
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
*                                                                               
         BAS   RE,NEWPRTQ           SET NEW PRINT QUEUE REPORT                  
         GOTO1 REQTWA,DMCB,(3,ATWA),,VPRINT,(C'B',ABOX)                         
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         L     R2,AMASTD            DO NOT PRINT LOGOS                          
         USING MASTD,R2                                                         
         NI    MCPRTIND,X'FF'-MCPRTINL                                          
         DROP  R2                                                               
*                                                                               
         L     R2,=A(TADOWNG)      OPEN DATASET FOR GETS                        
         OPEN  ((2),INPUT)                                                      
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R3,DLBLOCK           INITILISE DLFLD                             
         USING DLCBD,R3                                                         
         BAS   RE,INITDWN                                                       
*                                                                               
PREPD2   GET   (R2),TAPEIO         GET RECORD FROM TEMP DATASET                 
         LA    R5,TAPEIO                                                        
         USING TAPED,R5                                                         
*                                                                               
         LHI   R0,17               17*40=680                                    
*                                                                               
PREPD10  MVI   DLCBACT,DLCBPUT     PUT ITEM TO PRINT LINE                       
         MVI   DLCBTYP,DLCBTXT     DATA TYPE IS TEXT                            
*                                                                               
         LHI   R1,40               R1=MAX LENGTH OF DATA                        
         STC   R1,DLCBLEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DLCBFLD(0),0(R5)    PASS DATA                                    
         GOTO1 =V(DLFLD),DLCBD                                                  
         LA    R5,40(R5)           BUMP TO NEXT POSITION IN TAPE                
         BCT   R0,PREPD10          GET NEXT FIELD                               
*                                                                               
         MVI   DLCBLEN,20                                                       
         MVI   DLCBACT,DLCBPUT     PUT ITEM TO PRINT LINE                       
         MVI   DLCBTYP,DLCBTXT     DATA TYPE IS TEXT                            
         MVC   DLCBFLD(20),0(R5)   MOVE LAST 20 CHARACTERS                      
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
NOMORE   CLOSE ((2))               CLOSE DATASET                                
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVI   DLCBACT,DLCBEOR     END OF REPORT                                
         GOTO1 =V(DLFLD),DLCBD                                                  
         B     PREPDX                                                           
         EJECT                                                                  
*              ROUTINE TO SET UP SECOND REPORT ON QUEUE                         
         SPACE 1                                                                
NEWPRTQ  NTR1                                                                   
         L     R2,ALOGOC                                                        
         USING LOGOD,R2                                                         
         MVI   LOGOTYPE,C'E'                                                    
         GOTO1 ALOGO,DMCB,(R2)     END REPORT LOGOS                             
*                                                                               
         TM    WHEN,X'20'          SOON?                                        
         BO    NPRT10                                                           
         GOTO1 VPRINT,DMCB,=C'CLOSE'                                            
*                                                                               
NPRT10   XC    SPECS,SPECS                                                      
         XC    HEADHOOK,HEADHOOK                                                
*                                                                               
         L     R2,ABOX                                                          
         USING BOXD,R2                                                          
         LA    RE,132                                                           
         ST    RE,BOXWIDTH                                                      
*                                                                               
         L     RF,AMASTD                                                        
         USING MASTD,RF                                                         
         L     R2,AREMOT                                                        
         USING REMOTED,R2                                                       
*                                                                               
         TM    WHEN,X'20'                                                       
         BZ    NPRT20                                                           
         XC    MCREMPQK,MCREMPQK                                                
         B     NPRT30                                                           
*                                                                               
NPRT20   MVC   REMOTABF,MCVPQBUF                                                
         MVC   REMOTADM,MCVDMGR                                                 
         MVC   REMOTAOP,MCVPQOPN                                                
         MVC   REMOTDST,MCDESTID                                                
         XC    MCALTREF,MCALTREF                                                
         MVI   REMOTCPY,C'1'                                                    
         MVI   REMOTCLS,C'Q'                                                    
         MVC   REMOTJID,=C'TBD'                                                 
NPRT30   MVC   REMOTKEY(11),SPACES                                              
         MVC   REMOTSYS(6),=C'BTDATA'                                           
         MVC   REMOTFRM(4),=C'DATA'                                             
         B     PREPDX                                                           
         DROP  R2                                                               
         EJECT                                                                  
         SPACE 1                                                                
TADOWNG  DCB   DDNAME=TADOWN,DSORG=PS,MACRF=(GM),                      X        
               RECFM=FB,LRECL=700,BUFNO=2,BLKSIZE=0,EODAD=NOMORE                
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO LOOK FOR SUBSIDIARY INVOICES                          
*              SAVES EXTRA HANDLING IF THEY HAVE FEE= OPTION                    
         SPACE                                                                  
*                                                                               
         USING TAPED,R5                                                         
LOOKSUB  NTR1  BASE=*,LABEL=*                                                   
         XC    SVFEE,SVFEE                                                      
*                                                                               
         L     R4,TIAREC                                                        
         MVI   ELCODE,TAINELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   XIT                                                              
         USING TAIND,R4                                                         
         TM    TAINSTA2,TAINSPRM   ONLY WANT SUBSIDIARIES                       
         JNZ   XIT                                                              
         DROP  R4                                                               
*                                                                               
         L     R4,TIAREC                                                        
         MVC   AIO,TIAREC                                                       
         MVI   ELCODE,TAPAELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAPATFEE))                                     
         MVC   AIO,AIO1                                                         
         JNE   XIT                                                              
         L     R4,TGELEM                                                        
         USING TAPAD,R4                                                         
         MVC   SVFEE,TAPADATA    SAVE FEE FOR LATER                             
*                                                                               
         J     XIT                                                              
         DROP  R4,R5                                                            
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO GENERATE REPORTS - CALLS SYSIO                        
         SPACE                                                                  
PREPMC   NTR1  BASE=*,LABEL=*                                                   
         LA    R1,IOHKMC           A(I/O HOOK)                                  
         ST    R1,TIHOOK                                                        
         MVC   TIACOMFC,ACOMFACS   SET UP SYSIO BLOCK                           
*                                                                               
         ZAP   TAPCOUNT,=P'0'                                                   
         MVI   FRSTTIME,C'Y'       FIRST TIME IN HOOK                           
         MVI   SORTFRST,C'Y'       FIRST TIME FOR SORT                          
*                                                                               
         MVI   TIREAD,TLCKCDQ      SET TO READ CHECKS                           
         OI    TIQFLAGS,TIQFPBNP   ASK TO PASS BNP AS WELL                      
         OI    TIQFLAG2,TIQFSUB    SET TO PASS SUBS OF SPLITS                   
         OI    TIFINS2N,TAINSADJ   NO ADJUSTMENTS                               
         OI    TIFINSTN,TAINSCAN   NO CANCELLED INVOICES                        
         MVI   TIFINCVS,1          (NO CONVERTED RECORDS)                       
         CLI   BILLOPT,C'Y'        OPTION TO FILTER BILLED INVOICES             
         BNE   *+8                                                              
         OI    TIFINSTY,TAINSBIL                                                
         CLI   SESSOPT,C'Y'        OPTION FOR SESSION USES ONLY                 
         BNE   PRMC3                                                            
         MVI   TIFPTYPE,C'S'       SET FILTER ON SESSION TYPE                   
         LA    R1,TGD              SET A(TALENT GLOBALS)                        
         ST    R1,TIATGLOB                                                      
         SPACE 1                                                                
PRMC3    OI    TIFPO3N,TAPDORET    SET DON'T WANT RETRO PAYMENTS                
         CLI   RETROOPT,C'A'       IF OPTION TO GET RETROS ALSO                 
         BE    PRMC15                                                           
         CLI   RETROOPT,C'Y'       IF OPTION TO GET ONLY RETROS                 
         BNE   *+12                                                             
         OI    TIFPO3Y,TAPDORET      SET ONLY WANT RETRO PAYMENTS               
PRMC15   NI    TIFPO3N,ALL-TAPDORET  SET WANT RETRO PAYMENTS ALSO               
         GOTO1 TASYSIO,DMCB,TASYSIOD  OFF TO SYSIO TO DO I/O                    
         BAS   RE,ADDCKTOT         ADD LAST CHECK TOTALS                        
         BRAS  RE,AUDITMC          ENSURE SUM OF PARTS = WHOLE                  
         BAS   RE,PUTINTOT         PUT LAST INVOICE TOTALS                      
         BAS   RE,PUTCKTOT         PUT LAST INV. CHECK TOTALS                   
*                                                                               
         BRAS  RE,DORESTMC         SORT, PRINT AND WRITE TAPE                   
*                                                                               
         J     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PROCESS RECORDS FROM SYSIO                            
         SPACE 1                                                                
IOHKMC   NTR1                                                                   
         CLI   TIMODE,PROCINV      IF INVOICE HOOK                              
         BNE   IOHKMC80                                                         
*                                                                               
         CLI   FRSTTIME,C'Y'                                                    
         BE    IOHKMC05                                                         
         BAS   RE,ADDCKTOT         ADD PREVIOUS CHECK TOTALS                    
         BRAS  RE,AUDITMC          ENSURE SUM OF PARTS = WHOLE                  
         BAS   RE,PUTINTOT         PUT PREVIOUS INVOICE TOTALS                  
         BAS   RE,PUTCKTOT         PUT PREVIOUS INV. CHECK TOTALS               
*                                                                               
IOHKMC05 BAS   RE,CLRWKTOT         CLEAR WORK CODE TOTAL TABLE                  
         XC    TOTIAMTS,TOTIAMTS   CLEAR TOTAL AMOUNTS                          
         XC    TOTAMTS2,TOTAMTS2   CLEAR TOTAL AMOUNTS                          
         MVI   FRSTTIME,C'N'                                                    
*                                                                               
         BRAS  RE,SETINVMC         SET INVOICE INFORMATION                      
*                                                                               
         BRAS  RE,FINDAGY          FIND AGENCY IN AGY TABLE                     
         BE    *+6                                                              
         DC    H'00'               AGENCY NOT FOUND IN TABLE                    
*                                                                               
IOHKMC75 J     XIT                                                              
*                                                                               
*                                  CHECK HOOK                                   
IOHKMC80 CLI   TIMODE,PROCREC      IF THERE IS SOMETHING INTERESTING            
         JNE   XIT                                                              
         LA    R5,TAPEIO           R5=A(TAPEIO AREA)                            
         USING MCTAPED,R5                                                       
         L     R4,TIAREC                                                        
         USING TLCKD,R4                                                         
         XC    SVONOFF,SVONOFF                                                  
                                                                                
         BRAS  RE,CKCAN            SKIP IF I&R CHECK                            
         JE    XIT                                                              
*                                                                               
         CLC   TLCKSSN,=C'953967876'  SKIP IF SAG FOUNDATION                    
         JE    XIT                                                              
*                                                                               
         BRAS  RE,SETCHKMC         ACCUMULATE CHECK DOLLARS                     
*                                                                               
         OC    TOTHNW,TOTHNW       IF WE HAVE HNW,                              
         BZ    IOHKMC83                                                         
         CLC   TGCAT,=CL3'ZZZ'     SKIP CATEGORY ZZZ                            
         JE    XIT                                                              
*                                                                               
IOHKMC83 CLC   SVCOMP(4),=C'4501'   IF COMPANY 45, OFFICE 01,                   
         BNE   IOHKMC85                                                         
         MVC   MCWRK,=CL4'BI'       RESIDUALS AND SESSIONS                      
         B     IOHMC100                                                         
*                                                                               
IOHKMC85 CLC   SVCOMP(4),=C'3001'   IF COMPANY 30, OFFICE 01,                   
         BNE   IOHKMC87                                                         
         MVC   MCWRK,=CL4'TUP'      RESIDUALS                                   
         TM    TGUSSTAT,SESSION                                                 
         BZ    IOHMC100                                                         
         MVC   MCWRK,=CL4'ZA'       SESSIONS                                    
         B     IOHMC100                                                         
*                                                                               
IOHKMC87 CLC   SVCOMP(4),=C'0202'   IF COMPANY 02, OFFICE 02,                   
         BE    IOHKMC88                                                         
         CLC   SVCOMP(4),=C'0216'   OR COMPANY 02, OFFICE 16                    
         BE    IOHKMC88                                                         
         CLC   SVCOMP(4),=C'0206'   OR COMPANY 02, OFFICE 06                    
         BNE   IOHKMC90                                                         
IOHKMC88 MVC   MCWRK,=CL4'TL'       RESIDUALS AND SESSIONS                      
         B     IOHMC100                                                         
*                                   COMPANY 02, OFFICE 01                       
IOHKMC90 CLC   TLCKAGY,=CL6'9226'   IF PRINT AGENCY 9226,                       
         BNE   IOHKMC95                                                         
         MVC   MCWRK,=CL4'MUF'         RESIDUALS                                
         TM    TGUSSTAT,SESSION        IF SESSION,                              
         BZ    IOHMC100                                                         
         MVC   MCWRK,=CL4'MU'          SESSION                                  
         B     IOHMC100                                                         
*                                                                               
IOHKMC95 MVC   MCWRK,=CL4'ZA'          RESIDUALS                                
         TM    TGUSSTAT,SESSION        IF SESSION,                              
         BZ    IOHMC100                                                         
         MVC   MCWRK,=CL4'ZE'          EXTRAS                                   
         TM    TGCATYPE,EXTRA                                                   
         BO    IOHMC100                                                         
         MVC   MCWRK,=CL4'ZC'          SINGERS                                  
         TM    TGCASTAT,SINGER                                                  
         BO    IOHMC100                                                         
         MVC   MCWRK,=CL4'ZB'          MUSICIANS                                
         GOTO1 UNITEST,DMCB,TGCAUNIS,AFM,0,0,0                                  
         BO    IOHMC100                                                         
         MVC   MCWRK,=CL4'ZH'          HAND MODELS                              
         CLC   TGCAT,=CL3'HM '                                                  
         BE    IOHMC100                                                         
         CLC   TGCAT,=CL3'HMB'                                                  
         BE    IOHMC100                                                         
         MVC   MCWRK,=CL4'ZD'          ON CAMERA                                
         CLC   SVONOFF,=CL3'ON'                                                 
         BE    IOHMC100                                                         
         MVC   MCWRK,=CL4'ZF'          VOICE OVERS AND ANYTHING ELSE            
*                                                                               
IOHMC100 BAS   RE,UPDWKTOT                                                      
         J     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
*                                                                               
*              CLEAR WORK CODE TOTALS                                           
*                                                                               
CLRWKTOT NTR1                                                                   
         XC    TOTCOST,TOTCOST         CLEAR TOTAL COST                         
         LA    R1,WKTOTTB                                                       
         USING WKTOTD,R1                                                        
CWKT10   CLI   0(R1),X'FF'             END OF TABLE                             
         JE    XIT                                                              
         XC    WKTCOST,WKTCOST         CLEAR COST AMOUNTS                       
         LA    R1,WKTLNQ(R1)                                                    
         B     CWKT10                                                           
         DROP  R1                                                               
         EJECT                                                                  
*                                                                               
*              UPDATE WORK CODE TOTALS                                          
*                                                                               
         SPACE 1                                                                
UPDWKTOT NTR1                                                                   
         LA    R5,TAPEIO           R5=A(TAPEIO AREA)                            
         USING MCTAPED,R5                                                       
         LA    R1,WKTOTTB                                                       
         USING WKTOTD,R1                                                        
UWKT10   CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'00'                   WORK CODE NOT IN TABLE                   
         CLC   MCWRK,WKTWKCD           MATCH WORK CODE                          
         BE    UWKT20                                                           
         LA    R1,WKTLNQ(R1)           TRY NEXT                                 
         B     UWKT10                                                           
*                                                                               
UWKT20   L     RE,WKTCOST              ADD AMOUNT TO COST                       
         A     RE,TOTGROSS                                                      
         ST    RE,WKTCOST                                                       
         J     XIT                                                              
         DROP  R1,R5                                                            
         EJECT                                                                  
PUTSORTM NTR1                                                                   
         LA    R5,TAPEIO                                                        
         CLI   SORTFRST,C'Y'                                                    
         BNE   PUTSRTM2                                                         
         GOTO1 SORTER,DMCB,SORTCRD2,RECCARD2                                    
         MVI   SORTFRST,C'N'                                                    
         SPACE 1                                                                
PUTSRTM2 GOTO1 SORTER,DMCB,=C'PUT',TAPEIO                                       
         J     XIT                                                              
         EJECT                                                                  
                                                                                
*                                                                               
*        PUT INVOICE TOTALS BY WORK CODES                                       
*                                                                               
PUTINTOT NTR1                                                                   
         LA    R5,TAPEIO           R5=A(TAPEIO AREA)                            
         USING MCTAPED,R5                                                       
*                                                                               
         MVC   MCCOMP,SVCOMP             COMPANY                                
         MVC   MCOFF,SVOFF               OFFICE                                 
*                                                                               
         XC    BLOCK,BLOCK                                                      
         MVC   BLOCK(MCTAPLNQ),TAPEIO  MAKE A COPY                              
*                                                                               
         CLC   TOTTAX,=F'0'                                                     
         BE    PINT10                                                           
         EDIT  TOTTAX,MCCOST,2,MINUS=YES TAX                                    
         MVC   MCWRK,=CL4'ZM'         WORK CODE                                 
         CLC   SVCOMP(4),=C'0206'     IF COMPANY 02, OFFICE 06                  
         BE    PINT05                 OR                                        
         CLC   SVCOMP(4),=C'0216'     IF COMPANY 02, OFFICE 16                  
         BE    PINT05                 OR                                        
         CLC   SVCOMP(4),=C'0202'     IF COMPANY 02, OFFICE 02                  
         BNE   *+10                                                             
PINT05   MVC   MCWRK,=CL4'TETH'       USE TETH WORK CODE                        
         CLC   SVCOMP(4),=C'4501'     IF COMPANY 45, OFFICE 01,                 
         BNE   *+10                                                             
         MVC   MCWRK,=CL4'10'         USE WORK CODE 10                          
         BAS   RE,PUTSORTM            PUT TO SORTER                             
*                                                                               
PINT10   CLC   TOTHAND,=F'0'                                                    
         BE    PINT20                                                           
         MVC   TAPEIO(MCTAPLNQ),BLOCK                                           
         EDIT  TOTHAND,MCCOST,2,MINUS=YES HANDLING                              
         MVC   MCWRK,=CL4'ZM'         WORK CODE                                 
         CLC   SVCOMP(4),=C'0206'     IF COMPANY 02, OFFICE 06                  
         BE    PINT15                 OR                                        
         CLC   SVCOMP(4),=C'0216'     IF COMPANY 02, OFFICE 16                  
         BE    PINT15                 OR                                        
         CLC   SVCOMP(4),=C'0202'     IF COMPANY 02, OFFICE 02                  
         BNE   *+10                                                             
PINT15   MVC   MCWRK,=CL4'TETH'       USE TETH WORK CODE                        
         CLC   SVCOMP(4),=C'4501'     IF COMPANY 45, OFFICE 01,                 
         BNE   *+10                                                             
         MVC   MCWRK,=CL4'10'         USE WORK CODE 10                          
         BAS   RE,PUTSORTM            PUT TO SORTER                             
*                                                                               
PINT20   CLC   TOTGST,=F'0'                                                     
         BE    PINT30                                                           
         MVC   TAPEIO(MCTAPLNQ),BLOCK                                           
         EDIT  TOTGST,MCCOST,2,MINUS=YES GST                                    
         MVC   MCWRK,=CL4'ZM'         WORK CODE                                 
         CLC   SVCOMP(4),=C'0206'     IF COMPANY 02, OFFICE 06                  
         BE    PINT25                 OR                                        
         CLC   SVCOMP(4),=C'0216'     IF COMPANY 02, OFFICE 16                  
         BE    PINT25                 OR                                        
         CLC   SVCOMP(4),=C'0202'     IF COMPANY 02, OFFICE 02                  
         BNE   *+10                                                             
PINT25   MVC   MCWRK,=CL4'TETH'       USE TETH WORK CODE                        
         CLC   SVCOMP(4),=C'4501'     IF COMPANY 45, OFFICE 01,                 
         BNE   *+10                                                             
         MVC   MCWRK,=CL4'10'         USE WORK CODE 10                          
         BAS   RE,PUTSORTM            PUT TO SORTER                             
*                                                                               
PINT30   CLC   TOTPNH,=F'0'                                                     
         BE    PINT40                                                           
         MVC   TAPEIO(MCTAPLNQ),BLOCK                                           
         EDIT  TOTPNH,MCCOST,2,MINUS=YES P&H                                    
         MVC   MCWRK,=CL4'ZW'         WORK CODE                                 
         CLC   SVCOMP(4),=C'4501'     IF COMPANY 45, OFFICE 01,                 
         BNE   *+10                                                             
         MVC   MCWRK,=CL4'14'         USE WORK CODE 14                          
         BAS   RE,PUTSORTM            PUT TO SORTER                             
*                                                                               
PINT40   CLC   TOTINR,=F'0'                                                     
         BE    PINT50                                                           
         MVC   TAPEIO(MCTAPLNQ),BLOCK                                           
         EDIT  TOTINR,MCCOST,2,MINUS=YES I&R                                    
         MVC   MCWRK,=CL4'ZW'         WORK CODE                                 
         CLC   SVCOMP(4),=C'4501'     IF COMPANY 45, OFFICE 01,                 
         BNE   *+10                                                             
         MVC   MCWRK,=CL4'14'         USE WORK CODE 14                          
         BAS   RE,PUTSORTM            PUT TO SORTER                             
*                                                                               
PINT50   CLC   TOTHNW,=F'0'                                                     
         BE    PINT60                                                           
         MVC   TAPEIO(MCTAPLNQ),BLOCK                                           
         L     RE,TOTHNW                                                        
         A     RE,TOTHNWAJ            ADD ANY H&W ADJUSTMENTS                   
         EDIT  (RE),MCCOST,2,MINUS=YES H&W                                      
         MVC   MCWRK,=CL4'ZW'         WORK CODE                                 
         CLC   SVCOMP(4),=C'4501'     IF COMPANY 45, OFFICE 01,                 
         BNE   *+10                                                             
         MVC   MCWRK,=CL4'14'         USE WORK CODE 14                          
         BAS   RE,PUTSORTM            PUT TO SORTER                             
*                                                                               
PINT60   CLC   TOTREIM,=F'0'                                                    
         BE    PINT70                                                           
         MVC   TAPEIO(MCTAPLNQ),BLOCK                                           
         EDIT  TOTREIM,MCCOST,2,MINUS=YES REIMBURSED EXPENSES                   
         MVC   MCWRK,=CL4'ZY'         WORK CODE                                 
         CLC   SVCOMP(4),=C'3001'     IF COMPANY 30, OFFICE 01                  
         BNE   *+14                                                             
         MVC   MCWRK,=CL4'TUP'        USE TUP WORK CODE                         
         B     PINT65                                                           
         CLC   SVCOMP(4),=C'0206'     IF COMPANY 02, OFFICE 06                  
         BE    PINT62                 OR                                        
         CLC   SVCOMP(4),=C'0216'     IF COMPANY 02, OFFICE 16                  
         BE    PINT62                 OR                                        
         CLC   SVCOMP(4),=C'0202'     IF COMPANY 02, OFFICE 02                  
         BNE   *+10                                                             
PINT62   MVC   MCWRK,=CL4'TL'         USE TL WORK CODE                          
         CLC   SVCOMP(4),=C'4501'     IF COMPANY 45, OFFICE 01,                 
         BNE   *+10                                                             
         MVC   MCWRK,=CL4'BI'         USE BI WORK CODE                          
PINT65   BAS   RE,PUTSORTM            PUT TO SORTER                             
*                                                                               
PINT70   CLC   TOTMISC,=F'0'                                                    
         BE    PINT80                                                           
         CLI   MISCSTAT,C'Y'          IF MISC DED IS INCLUDED IN GROSS,         
         BE    PINT80                 DON'T LIST IT SEPARATELY                  
         MVC   TAPEIO(MCTAPLNQ),BLOCK                                           
         EDIT  TOTMISC,MCCOST,2,MINUS=YES MISCELLANEOUS EXPENSES                
         MVC   MCWRK,=CL4'ZY'         WORK CODE                                 
         CLC   SVCOMP(4),=C'3001'     IF COMPANY 30, OFFICE 01                  
         BNE   *+14                                                             
         MVC   MCWRK,=CL4'TUP'        USE TUP WORK CODE                         
         B     PINT73                                                           
         CLC   SVCOMP(4),=C'0206'     IF COMPANY 02, OFFICE 06                  
         BE    PINT71                 OR                                        
         CLC   SVCOMP(4),=C'0216'     IF COMPANY 02, OFFICE 16                  
         BE    PINT71                 OR                                        
         CLC   SVCOMP(4),=C'0202'     IF COMPANY 02, OFFICE 02                  
         BNE   *+10                                                             
PINT71   MVC   MCWRK,=CL4'TL'         USE TL WORK CODE                          
         CLC   SVCOMP(4),=C'4501'     IF COMPANY 45, OFFICE 01,                 
         BNE   *+10                                                             
         MVC   MCWRK,=CL4'BI'         USE BI WORK CODE                          
PINT73   BAS   RE,PUTSORTM            PUT TO SORTER  b                          
*                                                                               
PINT80   CLC   TOTCSF,=F'0'                                                     
         JE    XIT                                                              
         MVC   TAPEIO(MCTAPLNQ),BLOCK                                           
         EDIT  TOTCSF,MCCOST,2,MINUS=YES    COMM SERVICE FEE                    
         MVC   MCWRK,=CL4'MS'         WORK CODE                                 
         CLC   SVCOMP(4),=C'0206'     IF COMPANY 02, OFFICE 06 (MCLA)           
         BNE   PINT85                 OR                                        
         MVC   MCWRK,=CL4'ZM'         WORK CODE                                 
         B     PINT87                                                           
PINT85   CLC   SVCOMP(4),=C'0202'     IF COMPANY 02, OFFICE 02  (MCDE)          
         BE    PINT85A                OR                                        
         CLC   SVCOMP(4),=C'0216'     COMPANY 02, OFFICE 16                     
         BNE   *+10                                                             
PINT85A  MVC   MCWRK,=CL4'TETH'       WORK CODE                                 
         CLC   SVCOMP(4),=C'4501'     IF COMPANY 45, OFFICE 01,                 
         BNE   *+10                                                             
         MVC   MCWRK,=CL4'10'         USE WORK CODE 10                          
         CLC   SVAGY,=CL6'MCNY'       IF AGENCY MCNY                            
         BE    PINT86                 OR                                        
         CLC   SVAGY,=CL6'8085'       AGENCY 8085                               
         BE    PINT86                 OR                                        
         CLC   SVAGY,=CL6'8095'       AGENCY 8095                               
         BNE   PINT87                                                           
PINT86   MVC   MCWRK,=CL4'CMS'        USE CMS WORK CODE                         
PINT87   BAS   RE,PUTSORTM            PUT TO SORTER                             
         J     XIT                                                              
         DROP  R5                                                               
*                                                                               
*              PUT CHECK COST TOTALS BY WORK CODES                              
*                                                                               
PUTCKTOT NTR1                                                                   
         LA    R5,TAPEIO           R5=A(TAPEIO AREA)                            
         USING MCTAPED,R5                                                       
         LA    R1,WKTOTTB                                                       
         USING WKTOTD,R1                                                        
PCKT10   CLI   0(R1),X'FF'             END OF TABLE                             
         JE    XIT                                                              
         CLC   WKTCOST,=F'0'           ONLY PUT NON ZERO AMOUNTS                
         BE    PCKT20                                                           
         MVC   TAPEIO(MCTAPLNQ),BLOCK                                           
         MVC   MCWRK,WKTWKCD           WORK CODE                                
         EDIT  WKTCOST,MCCOST,2,MINUS=YES COST                                  
         BAS   RE,PUTSORTM            PUT TO SORTER                             
*                                                                               
PCKT20   LA    R1,WKTLNQ(R1)                                                    
         B     PCKT10                                                           
*                                                                               
         DROP  R1,R5                                                            
         EJECT                                                                  
*                                                                               
*              ADD CHECK COST TOTALS TOGETHER                                   
*                                                                               
ADDCKTOT NTR1                                                                   
         LA    R1,WKTOTTB                                                       
         USING WKTOTD,R1                                                        
         LA    RE,0                                                             
ACKT10   CLI   0(R1),X'FF'             END OF TABLE                             
         BE    ACKT20                                                           
         A     RE,WKTCOST              ADD COST TO TOTALS                       
         LA    R1,WKTLNQ(R1)                                                    
         B     ACKT10                                                           
*                                                                               
ACKT20   ST    RE,TOTCOST                                                       
         J     XIT                                                              
         DROP  R1                                                               
         EJECT                                                                  
*                                                                               
*        WORK CODE TOTALS TABLE                                                 
*                                                                               
WKTOTTB  DC    CL4'ZH',XL4'00'                                                  
         DC    CL4'ZF',XL4'00'                                                  
         DC    CL4'ZE',XL4'00'                                                  
         DC    CL4'ZD',XL4'00'                                                  
         DC    CL4'ZC',XL4'00'                                                  
         DC    CL4'ZB',XL4'00'                                                  
         DC    CL4'ZA',XL4'00'                                                  
         DC    CL4'TUP',XL4'00'                                                 
         DC    CL4'TL',XL4'00'                                                  
         DC    CL4'MU',XL4'00'                                                  
         DC    CL4'MUF',XL4'00'                                                 
         DC    CL4'BI',XL4'00'                                                  
         DC    CL4'CMS',XL4'00'                                                 
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
SORTCRD2 DC    CL80'SORT FIELDS=(14,5,A,69,12,A,19,10,A),FORMAT=BI,WORKX        
               =1'                                                              
**RTCRD2 DC    CL80'SORT FIELDS=(14,5,A),FORMAT=BI,WORK=1'                      
RECCARD2 DC    CL80'RECORD TYPE=F,LENGTH=(92)'                                  
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*        FIND AGENCY'S COMPANY AND OFFICE CODES IN TABLE                        
*                                                                               
FINDAGY  NTR1  BASE=*,LABEL=*                                                   
         XC    SVCOMP,SVCOMP                                                    
         XC    SVOFF,SVOFF                                                      
         XC    SVAGY,SVAGY                                                      
*                                                                               
         L     R4,TIAREC           R4=A(INVOICE RECORD)                         
         USING TLIND,R4                                                         
         LA    R2,AGYTAB                                                        
         USING AGYTABD,R2                                                       
FDAGY10  CLI   0(R2),X'FF'         END OF TABLE                                 
         JE    NO                                                               
         CLC   TLINAGY,AGYAGY                                                   
         BNE   FDAGY20                                                          
         MVC   SVCOMP,AGYCOMP      COMPANY                                      
         MVC   SVOFF,AGYOFF        OFFICE                                       
         MVC   SVAGY,AGYAGY        AGENCY                                       
         J     XIT                                                              
FDAGY20  LA    R2,AGYTBLNQ(R2)                                                  
         B     FDAGY10                                                          
         DROP  R2,R4                                                            
*                                                                               
*        AGENCY TABLE WITH COMPANY AND OFFICE                                   
*                                                                               
AGYTAB   DC    CL6'8085',C'02',C'01'                                            
         DC    CL6'MCNY',C'02',C'01'                                            
         DC    CL6'9226',C'02',C'01'                                            
         DC    CL6'MCDE',C'02',C'02'                                            
         DC    CL6'MCLA',C'02',C'06'                                            
         DC    CL6'2430',C'02',C'16'                                            
         DC    CL6'8095',C'30',C'01'                                            
         DC    CL6'LCNY',C'45',C'01'                                            
         DC    X'FF'                                                            
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*              ENSURE SUM OF PARTS = WHOLE                                      
         SPACE 1                                                                
AUDITMC  NTR1  BASE=*,LABEL=*                                                   
         MVI   MISCSTAT,0                                                       
         L     R1,TOTAMT           R1=INVOICE TOTAL                             
         L     R4,TOTTAX                                                        
         A     R4,TOTHAND                                                       
         A     R4,TOTGST                                                        
         A     R4,TOTCSF                                                        
         A     R4,TOTPNH                                                        
         A     R4,TOTINR                                                        
         A     R4,TOTHNW                                                        
         A     R4,TOTHNWAJ                                                      
         A     R4,TOTREIM                                                       
         A     R4,TOTMISC                                                       
         A     R4,TOTCOST                                                       
         SR    R1,R4                                                            
         BZ    AUDITMCX                                                         
         OC    CANRATE,CANRATE     IF THIS ISN'T CANADIAN INVOICE               
         BNZ   AUDITMC5                                                         
*                                                                               
         BRAS  RE,INVSUB           IS INVOICE SUBSIDIARY?                       
         BNE   AUDITMC3            PRIMARY, ABEND                               
         CHI   R1,10               DIFFERENCE NOT MORE THAN 0.10                
         BH    AUDITMC3                                                         
         CHI   R1,-10                                                           
         BNL   AUDITMC5                                                         
AUDITMC3 S     R4,TOTMISC          GET RID OF MISC DED                          
         MVI   MISCSTAT,C'Y'       MISC DED IS INCLUDED IN GROSS                
         L     R1,TOTAMT                                                        
         SR    R1,R4                                                            
         BZ    AUDITMCX                                                         
         DC    H'0'                DIE IF SUM OF PARTS NEQ WHOLE                
*                                                                               
AUDITMC5 L     R4,TOTHAND          ELSE, ADD DIFF TO HANDLING AMT               
         AR    R1,R4                                                            
         ST    R1,TOTHAND                                                       
AUDITMCX J     XIT                                                              
         EJECT                                                                  
INVSUB   NTR1  BASE=*,LABEL=*                                                   
         L     R4,TIAREC           R4=A(INVOICE RECORD)                         
         USING TAPDD,R4                                                         
         MVI   ELCODE,TAPDELQ                                                   
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    TAPDSTA2,TAPDSSUB                                                
         BZ    INVSNO                                                           
         CR    RB,RB               CC=EQUAL                                     
         B     *+6                                                              
INVSNO   LTR   RB,RB               CC=NOT EQUAL                                 
         J     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              SORT, PRINT REPORT, AND PUT TO TAPE                              
         SPACE 1                                                                
DORESTMC NTR1  BASE=*,LABEL=*                                                   
         LA    R5,TAPEIO                                                        
         USING TAPED,R5                                                         
         XC    TAPEKEY,TAPEKEY                                                  
         CLI   SORTFRST,C'Y'       IF SORT WAS ACTIVE                           
         JE    XIT                                                              
*                                                                               
DRSTMC2  GOTO1 SORTER,DMCB,=C'GET' GET A SORT RECORD                            
         L     R2,DMCB+4                                                        
         LTR   R2,R2               IF NO MORE SORT RECORDS                      
         JZ    XIT                 EXIT                                         
*                                                                               
         MVC   TAPEIO(MCTAPLNQ),0(R2)  ELSE, MOVE SORT REC TO TAPEIO            
         BRAS  RE,PUTMCTP          WRITE OUT TAPE RECORD                        
         BRAS  RE,MCREPORT         AND PRINT REPORT                             
         J     DRSTMC2             GET NEXT RECORD                              
         SPACE 1                                                                
*              START BUILDING RECORD WITH INVOICE INFORMATION                   
         SPACE 1                                                                
SETINVMC NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,CLEARTAP                                                      
         LA    R5,TAPEIO           R5=A(TAPEIO AREA)                            
         USING MCTAPED,R5                                                       
         L     R4,TIAREC           R4=A(INVOICE RECORD)                         
         USING TLIND,R4                                                         
*                                                                               
         CLC   TLINAGY,=C'999999'  SKIP ADJUSTMENTS                             
         BNE   SMCIN10                                                          
         MVI   TIMODE,PROCNOCK     SKIP CHECKS                                  
         J     XIT                                                              
*                                                                               
SMCIN10  MVC   AIO,TIAREC                                                       
         L     R4,TIAREC           R4=A(INVOICE RECORD)                         
         USING TLIND,R4                                                         
         BRAS  RE,SETCAN           SET CANADIAN CONVERSION RATE                 
         BRAS  RE,FIXCAN           CONVERT INVOICE AMOUNTS TO US DOLLAR         
*                                                                               
         BRAS  RE,SETRET           SET RETRO STATUS                             
*                                                                               
         MVC   MCAGY,TLINAGY       AGENCY CODE (4 CHARACTERS)                   
         MVC   WORK(6),TLININV                                                  
         XC    WORK(6),=6X'FF'                                                  
         GOTO1 TINVCON,DMCB,WORK,MCINVNM,DATCON   INVOICE NUMBER                
*                                                                               
         MVC   MCVEND,=C'XTALENTPAR'     VENDOR                                 
         CLC   MCAGY,=C'LCNY  '          GOTHAM                                 
         BE    SCMIN12                                                          
         CLC   MCAGY,=C'MCDE  '                                                 
         BE    SCMIN12                                                          
         CLC   MCAGY,=C'2430  '                                                 
         BE    SCMIN12                                                          
         CLC   MCAGY,=C'MCLA  '                                                 
         BNE   SMCIN15                                                          
SCMIN12  MVC   MCVEND,=C'TALPARTIL '     DETROIT VENDOR                         
         B     SMCIN20                                                          
SMCIN15  CLC   MCAGY,=C'9226  '                                                 
         BNE   SMCIN20                                                          
         MVC   MCVEND,=C'PRINTPAYRO'     PRINT VENDOR                           
*                                                                               
SMCIN20  MVC   MCTRANS,=C'800'           TRANS CODE                             
*                                                                               
         MVI   SVACTTYP,0                CLEAR ACTRA TYPE                       
*                                                                               
         MVI   ELCODE,0                                                         
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
SMCINV2  BRAS  RE,NEXTEL                                                        
         JNE   XIT                                                              
*                                                                               
         CLI   0(R4),TABDELQ                                                    
         BE    SMCIBD                    GET BILLING DETAILS                    
         CLI   0(R4),TAINELQ                                                    
         BE    SMCIIN                    GET INVOICE DATE                       
         CLI   0(R4),TANUELQ                                                    
         BE    SMCINU                    GET ESTIMATE NUMBER                    
         CLI   0(R4),TAPDELQ                                                    
         BE    SMCIPD                    GET P&H, H&W, I&R                      
         CLI   0(R4),TAPAELQ                                                    
         BE    SMCIPA                    GET H&W ADJUSTMENT                     
         CLI   0(R4),TACOELQ                                                    
         BE    SMCICO                    GET ACTRA TYPE                         
         B     SMCINV2                                                          
*                                                                               
         EJECT                                                                  
*              ROUTINES THAT PROCESS ELEMENTS FROM INVOICE RECORDS              
         SPACE 2                                                                
*              PROCESS BILLING DETAILS ELEMENT                                  
         SPACE 1                                                                
         USING TABDD,R4                                                         
SMCIBD   MVC   TOTTAX,TABDTAX          TAX                                      
         L     RF,TABDGST              GST AND PST                              
         A     RF,TABDPST                                                       
         ST    RF,TOTGST                                                        
         MVC   TOTCSF,TABDCSF          CSF                                      
         MVC   TOTAMT,TABDTOT          TOTAL AMOUNT                             
*                                                                               
         L     R1,TABDHND                                                       
         A     R1,TABDHNDC                                                      
         ST    R1,TOTHAND              TOTAL HANDLING                           
*                                                                               
         B     SMCINV2                                                          
         SPACE 2                                                                
*              PROCESS INVOICE DETAILS ELEMENT                                  
         SPACE 1                                                                
         USING TAIND,R4                                                         
***IIN   GOTOR CONVDAT2,DMCB,TAINBDTE,MCINVDT    INVOICE DATE                   
SMCIIN   GOTO1 DATCON,DMCB,(1,TAINBDTE),(20,MCINVDT)    INVOICE DATE            
         B     SMCINV2                                                          
         SPACE 2                                                                
*              PROCESS FREE FORM NUMBER ELEMENT                                 
         SPACE 1                                                                
         USING TANUD,R4                                                         
SMCINU   LA    R2,MCJOBEST                                                      
         LA    R3,L'MCJOBEST-1                                                  
         CLI   TANUTYPE,TANUTEST                                                
         BNE   SMCINV2                                                          
         SPACE 1                                                                
         ZIC   R1,TANULEN                                                       
         AHI   R1,-4                                                            
         CR    R1,R3                                                            
         BL    *+6                                                              
         LR    R1,R3                                                            
         EX    R1,*+8                                                           
         B     SMCINV2                                                          
         MVC   0(0,R2),TANUMBER                                                 
         EJECT                                                                  
*              PROCESS PAYMENT DETAILS                                          
         SPACE 1                                                                
         USING TAPDD,R4                                                         
SMCIPD   MVC   TOTPNH,TAPDPNH      P&H                                          
         MVC   TOTHNW,TAPDHNW      H&W                                          
         MVC   TOTINR,TAPDINR      I&R                                          
         MVC   TOTREIM,TAPDREXP    REIMBURSED EXPENSES                          
         MVC   TOTMISC,TAPDMDED    MISC DEDUCTIONS                              
         B     SMCINV2                                                          
         EJECT                                                                  
*              PROCESS PAYMENT OPTIONS                                          
         SPACE 1                                                                
         USING TAPAD,R4                                                         
SMCIPA   CLI   TAPATYPE,TAPATHNW   H&W ADJUSTMENT?                              
         BNE   SMCINV2                                                          
         MVC   TOTHNWAJ,TAPADATA   SAVE H&W ADJUSTMENT                          
         B     SMCINV2                                                          
         EJECT                                                                  
*              PROCESS COMMERCIAL DETAILS                                       
         SPACE 1                                                                
         USING TACOD,R4                                                         
SMCICO   MVC   SVACTTYP,TACOCTYP   SAVE ACTRA TYPE                              
         B     SMCINV2                                                          
         EJECT                                                                  
*                                                                               
         DROP  R4,R5                                                            
         LTORG                                                                  
         EJECT                                                                  
*              GET INFORMATION FROM CHECKS                                      
         SPACE 2                                                                
SETCHKMC NTR1  BASE=*,LABEL=*                                                   
         L     R4,TIAREC                                                        
         USING TLCKD,R4                                                         
         BRAS  RE,FIXCAN           FIX CHECK AMOUNTS                            
*                                                                               
         GOTO1 CATVAL,DMCB,TLCKCAT     SET TGCAT, TGCATYPE, TGCASTAT            
         DROP  R4                                                               
*                                                                               
         MVI   ELCODE,TAPDELQ                                                   
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'               SHOULD HAVE THIS ELEMENT                     
         USING TAPDD,R4                                                         
         XR    R1,R1                                                            
         A     R1,TAPDPAYI                                                      
         A     R1,TAPDPAYC                                                      
         ST    R1,TOTGROSS         TOTAL GROSS AMOUNT                           
         GOTO1 USEVAL,DMCB,(X'20',TAPDUSE),TAPDTYPE   SET TGUSSTAT              
*                                                                               
         L     R4,TIAREC                                                        
         MVI   ELCODE,TACAELQ                                                   
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'               SHOULD HAVE THIS ELEMENT                     
         USING TACAD,R4                                                         
         MVC   SVONOFF,TACAONOF    ON/OFF CAMERA                                
         J     XIT                                                              
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
                                                                                
*        CHECK IF CANADIAN I&R ACCOUNT CHECK                                    
*                R3 = CHECK TABLE                                               
*                                                                               
CKCAN    NTR1  BASE=*,LABEL=*                                                   
         L     R4,TIAREC                                                        
         MVI   ELCODE,TAPDELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   NO                                                               
         USING TAPDD,R4                                                         
         TM    TAPDSTAT,TAPDSCAN   IF CANADIAN INVOICE                          
         JO    CCAN02                                                           
         DROP  R4                                                               
         CLI   SVACTTYP,CCTY04A    OR IF ACTRA TYPE 2404A                       
         JE    CCAN02                                                           
         CLI   SVACTTYP,CCTY2404   OR IF ACTRA TYPE 2404                        
         JE    CCAN02                                                           
         CLI   SVACTTYP,CCTY04B    OR IF ACTRA TYPE 2404B                       
         JNE   NO                                                               
                                                                                
         USING TLCKD,R4                                                         
CCAN02   L     R4,TIAREC                                                        
                                                                                
CCAN05   LA    R1,IRCAN            TABLE OF SSN'S FOR I&R CANADA                
                                                                                
CCAN10   CLI   0(R1),X'FF'         END OF TABLE                                 
         JE    NO                                                               
         CLC   TLCKSSN,0(R1)       IF THIS IS I&R ACCOUNT                       
         JE    YES                                                              
         LA    R1,L'IRCAN(R1)      BUMP TABLE                                   
         J     CCAN10                                                           
         DROP  R4                                                               
         SPACE 2                                                                
                                                                                
*        TABLE OF I&R SSN'S                                                     
IRCAN    DS    0CL9                                                             
         DC    CL9'000000055'                                                   
         DC    CL9'000003755'                                                   
         DC    CL9'000003855'                                                   
         DC    CL9'000003876'                                                   
         DC    CL9'000007106'                                                   
         DC    CL9'000008213'                                                   
         DC    CL9'000000066'                                                   
         DC    X'FF'                                                            
         LTORG                                                                  
         EJECT                                                                  
                                                                                
PUTMCTP  NTR1  BASE=*,LABEL=*                                                   
         AP    TAPCOUNT,=P'1'                                                   
         L     R1,=A(MCDISK)                                                    
         LA    R0,TAPEIO                                                        
         PUT   (1),(0)                                                          
         J     XIT                                                              
         EJECT                                                                  
                                                                                
MCREPORT NTR1  BASE=*,LABEL=*                                                   
         LA    R2,P                                                             
         USING PRINTD,R2                                                        
         LA    R5,TAPEIO           R5=A(TAPEIO AREA)                            
         USING MCTAPED,R5                                                       
         MVC   PAGENCY(L'MCAGY),MCAGY       AGENCY                              
         MVC   PINVOICE,MCINVNM    INVOICE NUMBER                               
         GOTO1 SPOOL,DMCB,(R8)     PRINT ON REPORT                              
         J     XIT                                                              
         DROP  R2,R5                                                            
         LTORG                                                                  
         EJECT                                                                  
*              SET CANADIAN CONVERSION RATE FOR CAN$ INVOICES                   
         SPACE 1                                                                
SETCAN   NTR1  BASE=*,LABEL=*                                                   
         XC    CANRATE,CANRATE                                                  
         L     R4,TIAREC                                                        
         MVI   ELCODE,TAPDELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   XIT                                                              
         USING TAPDD,R4                                                         
         TM    TAPDSTAT,TAPDSCAN   IF CANADIAN INVOICE                          
         JNO   XIT                                                              
*                                                                               
         L     R4,TIAREC                                                        
         MVI   ELCODE,TABDELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   XIT                                                              
         USING TABDD,R4                                                         
         OC    TABDCCVT,TABDCCVT                                                
         JZ    XIT                                                              
         ZAP   DUB,TABDCCVT                                                     
         CVB   R1,DUB                                                           
         ST    R1,CANRATE                                                       
         J     XIT                                                              
         DROP  R4                                                               
         SPACE 1                                                                
*              CONVERTS INVOICE AMOUNTS TO US DOLLAR                            
         SPACE 1                                                                
FIXCAN   NTR1  BASE=*,LABEL=*                                                   
         OC    CANRATE,CANRATE     IF CANADIAN DOLLAR INVOICE                   
         JZ    XIT                                                              
         L     R4,TIAREC                                                        
         MVI   ELCODE,TAPDELQ                                                   
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAPDD,R4                                                         
         SPACE 1                                                                
         LA    R0,TAPDAMTL/L'TAPDAMTS                                           
         LA    R1,TAPDAMTS                                                      
FIXCAN5  BAS   RE,FIXAMT                                                        
         LA    R1,4(R1)                                                         
         BCT   R0,FIXCAN5                                                       
         SPACE 2                                                                
         L     R4,TIAREC                                                        
         MVI   ELCODE,TABDELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   XIT                                                              
*                                                                               
         USING TABDD,R4                                                         
         TM    TABDSTAT,TABDSCNW   IF T&H IN CANADIAN DOLLARS                   
         JZ    XIT                                                              
         LA    R1,TABDTAX                                                       
         BAS   RE,FIXAMT                                                        
         LA    R1,TABDFICR                                                      
         BAS   RE,FIXAMT                                                        
         LA    R1,TABDGST                                                       
         BAS   RE,FIXAMT                                                        
         LA    R1,TABDPST                                                       
         BAS   RE,FIXAMT                                                        
         LA    R1,TABDHND                                                       
         BAS   RE,FIXAMT                                                        
         LA    R1,TABDHNDC                                                      
         BAS   RE,FIXAMT                                                        
         J     XIT                                                              
         SPACE 2                                                                
FIXAMT   NTR1                      ADJUST R1=A(FULLWORD) BY CANRATE             
         L     RF,0(R1)                                                         
         M     RE,CANRATE                                                       
         D     RE,=F'5000'                                                      
         LTR   RF,RF                                                            
         BM    *+8                                                              
         AHI   RF,1                                                             
         SRA   RF,1                                                             
         ST    RF,0(R1)                                                         
         J     XIT                                                              
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*              SET RETRO STATUS FOR INVOICE                                     
         SPACE 1                                                                
SETRET   NTR1  BASE=*,LABEL=*                                                   
         MVI   SVPDOPT3,0                                                       
                                                                                
         USING TAPDD,R4                                                         
         L     R4,TIAREC                                                        
         MVI   ELCODE,TAPDELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   XIT                                                              
         MVC   SVPDOPT3,TAPDOPT3                                                
         J     XIT                                                              
         DROP  R4                                                               
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
CLEARTAP NTR1  BASE=*,LABEL=*                                                   
         LA    R2,TAPEIO                                                        
         LA    R0,7                                                             
         SPACE 1                                                                
CLEARTP2 MVC   0(100,R2),SPACES                                                 
         LA    R2,100(R2)                                                       
         BCT   R0,CLEARTP2                                                      
         J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
CONVDAT2 NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R3,0(R1)                                                      
         MVC   0(6,R3),=C'000000'                                               
         OC    0(3,R2),0(R2)                                                    
         JZ    XIT                                                              
         GOTO1 DATCON,DMCB,(1,0(R2)),0(R3)                                      
         J     XIT                                                              
         LTORG                                                                  
         SPACE 1                                                                
*              VALIDATE A FILTER EXPRESSION                                     
         SPACE 3                                                                
*              INPUT               R2=A(HEADER)                                 
*                                  R3=A(SYSIO FILTER AREA)                      
*                                  R4=L'ABOVE                                   
*                                  R5=RECORD TYPE CODE                          
         SPACE 1                                                                
SPECFILT NTR1  BASE=*,LABEL=*                                                   
         CLI   5(R2),0             ANY DATA                                     
         JE    XIT                                                              
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
         JZ    XIT                                                              
         CHI   R5,1                                                             
         BE    ALLUNVL                                                          
         CHI   R5,2                                                             
         BE    ALLUSEVL                                                         
         GOTO1 RECVAL,DMCB,(R5),(X'80',(R4)),0                                  
         BNE   BADFILT                                                          
         J     XIT                                                              
         SPACE 1                                                                
ALLUNVL  GOTO1 UNIVAL,DMCB,(R4)                                                 
         BNE   BADFILT                                                          
         J     XIT                                                              
         SPACE 1                                                                
ALLUSEVL GOTO1 USEVAL,DMCB,(X'40',(R4))                                         
         BNE   BADFILT                                                          
         J     XIT                                                              
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
         BH    BADLONG                                                          
         XC    KEY,KEY             CHECK LIST EXISTS                            
         LA    R3,KEY                                                           
         USING TLGLD,R3                                                         
         MVI   TLGLCD,TLGLCDQ                                                   
         MVI   TLGLTYPE,TLGLTYPF                                                
         MVC   TLGLLST,0(R5)                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(32),KEYSAVE                                                  
         BNE   BADLIST                                                          
         J     XIT                                                              
         SPACE 1                                                                
BADFILT  MVC   CONHEAD(L'FLTERR),FLTERR                                         
         B     *+10                                                             
BADLIST  MVC   CONHEAD(L'LSTERR),LSTERR                                         
         B     *+10                                                             
BADLONG  MVC   CONHEAD(L'LNGERR),LNGERR                                         
         GOTO1 ERREX2                                                           
         SPACE 1                                                                
FLTERR   DC    C'** ERROR ** RECORD NOT FOUND'                                  
LSTERR   DC    C'** ERROR ** MISSING FILTER LIST'                               
LNGERR   DC    C'** ERROR ** CODE TOO LONG'                                     
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================          
*              VALIDATE USING SOFT DATES                                        
*              R2 = FIELD HEADER                                                
*=====================================================================          
         USING SOFDATD,R1                                                       
VSFTDAT  NTR1  BASE=*,LABEL=*                                                   
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
         B     VSFTDATN                                                         
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BE    VSFTDAT5                                                         
         CLI   CONOUTH+5,0         RLP ONLINE, IF GROUP NAME IN OUTPUT          
         BE    VSFTDAT5                                                         
         CLC   =C'FILE',CONDEST    AND FILE IN DESTINATION                      
         BE    VSFTDATY            DON'T RESOLVE DATES                          
*                                                                               
VSFTDAT5 OI    SOFIINDS,SOFIIRES   RESOLVE THE DATES TO ACTUAL                  
         GOTO1 SOFTDATE,SOFDATD                                                 
         GOTO1 DATCON,DMCB,(0,OUTDATE),(1,TIQPSTR)                              
         GOTO1 DATCON,DMCB,(0,OUTDATE+6),(1,TIQPEND)                            
*                                                                               
VSFTDATY SR    RC,RC                                                            
VSFTDATN LTR   RC,RC                                                            
         XIT1                                                                   
         DROP  R1                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*=====================================================================          
*              MCINTER DOWNLOAD                                                 
*=====================================================================          
MCDOWN   NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,NEWPRTQ2          SET NEW PRINT QUEUE REPORT                  
         GOTO1 REQTWA,DMCB,(3,ATWA),,VPRINT,(C'B',ABOX)                         
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         L     R2,=A(MCDISK)                                                    
         USING MCTAPED,R2                                                       
         OPEN  ((2),INPUT)                                                      
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R3,DLBLOCK                                                       
         USING DLCBD,R3                                                         
         BRAS  RE,INITDWN2                                                      
*                                                                               
****     BRAS  RE,DOWNHEAD         PUT HEADERS TO DOWNLOAD                      
*                                                                               
MCDWN10  GET   (R2),TAPEIO         GET RECORD                                   
***      BAS   RE,DOWNTAPE         PUT TAPE RECORD TO DOWNLOAD                  
         LA    R4,TAPEIO                                                        
*                                                                               
         SR    R0,R0                                                            
         LHI   R1,MCTAPLNQ         RECORD LENGTH                                
         D     R0,=F'40'           R1  = # OF 40 BYTE CHUNKS                    
         STC   R0,REM              REM = LEFT OVER BYTES                        
*                                                                               
         LTR   R1,R1                                                            
         BZ    MCDWN30                                                          
*                                                                               
MCDWN20  STC   R1,QUO                                                           
         MVI   DLCBACT,DLCBPUT     PUT ITEM TO PRINT LINE                       
         MVI   DLCBTYP,DLCBTXT     DATA TYPE IS TEXT                            
*                                                                               
         MVI   DLCBLEN,40                                                       
         MVC   DLCBFLD(40),0(R4)    PASS DATA                                   
         GOTO1 =V(DLFLD),DLCBD                                                  
*                                                                               
         LA    R4,40(R4)           BUMP TO NEXT TAPE FIELD                      
         ZIC   R1,QUO                                                           
         BCT   R1,MCDWN20                                                       
*                                                                               
MCDWN30  CLI   REM,0                                                            
         BE    MCDWN40                                                          
*                                                                               
         MVI   DLCBACT,DLCBPUT     PUT ITEM TO PRINT LINE                       
         MVI   DLCBTYP,DLCBTXT     DATA TYPE IS TEXT                            
         MVC   DLCBLEN,REM                                                      
         ZIC   RE,REM                                                           
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   DLCBFLD(0),0(R4)    PASS DATA                                    
         GOTO1 =V(DLFLD),DLCBD                                                  
*                                                                               
MCDWN40  MVI   DLCBACT,DLCBEOL     END OF LINE                                  
         GOTO1 =V(DLFLD),DLCBD                                                  
         B     MCDWN10                                                          
*                                                                               
         DROP  R2                                                               
*                                                                               
*----------------------------------------------------------------------         
*              DATA SET ROUTINE                                                 
*----------------------------------------------------------------------         
NOMORE2  CLOSE ((2))               CLOSE DATASET                                
         LA    R3,DLBLOCK                                                       
         USING DLCBD,R3                                                         
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVI   DLCBACT,DLCBEOR     END OF REPORT                                
         GOTO1 =V(DLFLD),DLCBD                                                  
         J     XIT                                                              
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------          
*              ROUTINE TO SET UP SECOND REPORT ON QUEUE                         
*---------------------------------------------------------------------          
NEWPRTQ2 NTR1  BASE=*,LABEL=*                                                   
         L     R2,ALOGOC                                                        
         USING LOGOD,R2                                                         
         MVI   LOGOTYPE,C'E'                                                    
         GOTO1 ALOGO,DMCB,(R2)     END REPORT LOGOS                             
*                                                                               
         TM    WHEN,X'20'          SOON?                                        
         BO    NPQ010                                                           
         GOTO1 VPRINT,DMCB,=C'CLOSE'                                            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
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
*                                                                               
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
         MVI   REMOTCPY,C'1'                                                    
         MVI   REMOTCLS,C'Q'                                                    
         MVC   REMOTJID,=C'TMI'                                                 
NPQ060   MVC   REMOTKEY(11),=CL11' '                                            
         MVC   REMOTSYS(6),=C'MCDATA'                                           
         MVC   REMOTFRM(4),=C'DATA'                                             
         J     XIT                                                              
         LTORG                                                                  
         DROP R2,RF                                                             
*---------------------------------------------------------------------          
INITDWN2 NTR1  BASE=*,LABEL=*                                                   
         LA    R3,DLBLOCK                                                       
         XC    DLCBD(DLCBXLX),DLCBD                                             
         MVI   DLCBACT,DLCBINIT    INITIALIZE FOR DOWNLOAD                      
         LA    R1,SPLATDW2         A(HOOK ROUTINE FOR PRINTING)                 
         ST    R1,DLCBAPR                                                       
         LA    R1,P                A(PRINT LINE)                                
         MVC   P,SPACES                                                         
         ST    R1,DLCBAPL                                                       
         MVC   DLCBAED,EDITOR                                                   
         MVC   DLCXMAXL,=Y(L'P)    MAXIMUM LENGTH OF PRINT LINE                 
         MVI   DLCXDELC,C' '       DELIMITER                                    
         MVI   DLCXEOTC,C'"'       TEXT DELIMITER                               
         MVI   DLCXEOTA,C''''      TEXT DELIMITER ALTERNATE                     
         MVI   DLCXEOLC,X'5E'      SEMI-COLON FOR END OF LINE                   
         MVI   DLCXEORC,C':'       END OF REPORT                                
         OI    DLCBFLG1,DLCBFXTN                                                
         GOTO1 =V(DLFLD),DLCBD                                                  
         J     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------          
SPLATDW2 NTR1                                                                   
         MVI   LINE,1              PREVENT PAGE BREAK                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
         J     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------          
*              ROUTINE TO OUTPUT HEADLINES TO DOWNLOAD                          
*---------------------------------------------------------------------          
         USING MCTAPED,R2                                                       
DOWNHEAD NTR1  BASE=*,LABEL=*                                                   
         LA    R2,TAPEIO                                                        
         GOTOR =A(FLDDOWN),DMCB,(7,=C'Company')                                 
         GOTOR (RF),DMCB,(6,=C'Office')                                         
         GOTOR (RF),DMCB,(6,=C'Vendor')                                         
         GOTOR (RF),DMCB,(14,=C'Invoice Number')                                
         GOTOR (RF),DMCB,(12,=C'Invoice Date')                                  
         GOTOR (RF),DMCB,(9,=C'PO Number')                                      
         GOTOR (RF),DMCB,(4,=C'Cost')                                           
         GOTOR (RF),DMCB,(6,=C'Client')                                         
         GOTOR (RF),DMCB,(7,=C'Product')                                        
         GOTOR (RF),DMCB,(5,=C'Brand')                                          
         GOTOR (RF),DMCB,(11,=C'Job/EDI Tag')                                   
         GOTOR (RF),DMCB,(7,=C'Use Tax')                                        
         GOTOR (RF),DMCB,(9,=C'Work Code')                                      
         GOTOR (RF),DMCB,(9,=C'Tran Code')                                      
         BRAS  RE,EOLDOWN                                                       
*                                                                               
         J     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------          
*              ROUTINE TO OUTPUT TAPE RECORD TO DOWNLOAD                        
*---------------------------------------------------------------------          
         USING MCTAPED,R2                                                       
DOWNTAPE NTR1  BASE=*,LABEL=*                                                   
         LA    R2,TAPEIO                                                        
         GOTOR =A(FLDDOWN),DMCB,(L'MCCOMP,MCCOMP)                               
         GOTOR (RF),DMCB,(L'MCOFF,MCOFF)                                        
         GOTOR (RF),DMCB,(L'MCVEND,MCVEND)                                      
         GOTOR (RF),DMCB,(L'MCINVNM,MCINVNM)                                    
         GOTOR (RF),DMCB,(L'MCINVDT,MCINVDT)                                    
         GOTOR (RF),DMCB,(L'MCPONM,MCPONM)                                      
         GOTOR (RF),DMCB,(L'MCCOST,MCCOST)                                      
         GOTOR (RF),DMCB,(L'MCCLI,MCCLI)                                        
         GOTOR (RF),DMCB,(L'MCPROD,MCPROD)                                      
         GOTOR (RF),DMCB,(L'MCBRAND,MCBRAND)                                    
         GOTOR (RF),DMCB,(L'MCJOBEST,MCJOBEST)                                  
         GOTOR (RF),DMCB,(L'MCUTAX,MCUTAX)                                      
         GOTOR (RF),DMCB,(L'MCWRK,MCWRK)                                        
         GOTOR (RF),DMCB,(L'MCTRANS,MCTRANS)                                    
         BRAS  RE,EOLDOWN                                                       
*                                                                               
         J     XIT                                                              
         DROP  R2                                                               
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------          
*              ROUTINE TO PUT FIELD INTO DOWNLOAD                               
*---------------------------------------------------------------------          
FLDDOWN  NTR1  BASE=*,LABEL=*                                                   
         LA    R3,DLBLOCK                                                       
         USING DLCBD,R3                                                         
         MVI   DLCBACT,DLCBPUT     PUT ITEM TO PRINT LINE                       
         MVI   DLCBTYP,DLCBTXT     DATA TYPE IS TEXT                            
         L     R4,0(R1)            ADDRESS OF FIELD                             
         LA    R4,0(R4)                                                         
         ZIC   R2,0(R1)            LENGTH OF FIELD                              
         LTR   R2,R2                                                            
         BZ    FD010                                                            
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   DLCBFLD(0),0(R4)                                                 
FD010    GOTO1 =V(DLFLD),DLCBD                                                  
         J     XIT                                                              
         LTORG                                                                  
         DROP  R3                                                               
         EJECT                                                                  
*---------------------------------------------------------------------          
*              ROUTINE TO PUT EOL INTO DOWNLOAD                                 
*---------------------------------------------------------------------          
EOLDOWN  NTR1  BASE=*,LABEL=*                                                   
         LA    R3,DLBLOCK                                                       
         USING DLCBD,R3                                                         
         MVI   DLCBACT,DLCBPUT     PUT ITEM TO PRINT LINE                       
         MVI   DLCBTYP,DLCBTXT     DATA TYPE IS TEXT                            
         MVI   DLCBACT,DLCBEOL     END OF LINE                                  
         GOTO1 =V(DLFLD),DLCBD                                                  
         J     XIT                                                              
         DROP   R3                                                              
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
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
*                                                                               
AMASTD   DS    A                                                                
ALOGOC   DS    A                                                                
ALOGO    DS    A                                                                
AREMOT   DS    A                                                                
*                                                                               
SVFEE    DS    F                   FEE= OPTION                                  
*                                                                               
TAPCOUNT DS    PL6                                                              
*                                                                               
TAPEOPT  DS    CL1                 Y=GENERATE OUTPUT TAPE                       
BILLOPT  DS    CL1                 BILLING ONLY                                 
SESSOPT  DS    CL1                 Y=SESSION USES ONLY                          
RETROOPT DS    CL1                 Y=GET ONLY RETROACTIVE PAYMENTS              
*                                  A=ALSO GET RETROACTIVE PAYMENTS              
NEWOPT   DS    CL1                 AUTH/PO = NEW                                
AGYOPT   DS    CL6                 AGENCY OVERRIDE OPTION                       
         DS    CL3                 SPARE                                        
*                                                                               
TAPETYPE DS    CL1                                                              
ACTVSW   DS    CL1                                                              
SORTFRST DS    CL1                                                              
SAVEEL   DS    CL1                                                              
RECTYPE  DS    CL16                                                             
SAVETPKY DS    CL20                                                             
SAVEAMTS DS    CL36                                                             
         SPACE 1                                                                
SVPDOPT3 DS    XL(L'TAPDOPT3)      SAVED PAYMENT OPTION 3                       
         SPACE 1                                                                
         DS    0F                                                               
CANRATE  DS    F                   CANADIAN CONVERSION RATE                     
TOTIAMTS DS    0CL(TOTLNQ)                                                      
TOTAMT   DS    F                   BILLING TOTAL                                
TOTTAX   DS    F                   TAXES FROM INVOICE                           
TOTHAND  DS    F                   HANDLING FROM INVOICE                        
TOTGROSS DS    F                   PAYI FROM CHECKS                             
TOTSPNH  DS    F                   SUBJECT TO PNH FROM CHECKS                   
TOTPNH   DS    F                   PNH & INR FROM CHECKS                        
TOTMISC  DS    F                   PAYC+REXP-INR FROM CHECKS                    
TOTSIGF  DS    F                   SIGNATORY FEE                                
TOTACOM  DS    F                   AGENCY COMMISSION                            
TOTLNQ   EQU   *-TOTAMT                                                         
         SPACE 1                                                                
TOTAMTS2 DS    0CL(TOT2LNQ)                                                     
TOTGST   DS    F                   GST FROM INVOICE                             
TOTINR   DS    F                   INR FROM INVOICE                             
TOTHNW   DS    F                   HNW FROM INVOICE                             
TOTREIM  DS    F                   REIMBURSED EXPENSES FROM INVOICE             
TOTCOST  DS    F                   TOTAL GROSS FROM CHECKS PER INVOICE          
TOTHNWAJ DS    F                   HNW ADJUSTMENT                               
TOTCSF   DS    F                   CSF FROM INVOICE                             
TOT2LNQ  EQU   *-TOTGST                                                         
*                                                                               
SVONOFF  DS    CL3                 ON/OFF CAMERA                                
*                                                                               
REM      DS    XL1                 REMAINDER                                    
QUO      DS    XL1                 QUOTIENT                                     
*                                                                               
SVCOMP   DS    XL2                 COMPANY                                      
SVOFF    DS    XL2                 OFFICE                                       
SVAGY    DS    CL6                 AGENCY                                       
FRSTTIME DS    CL1                 FIRST TIME                                   
SVACTTYP DS    XL1                 ACTRA TYPE                                   
*                                                                               
MISCSTAT DS    XL1                 MISC DED STATUS                              
*                                                                               
DLBLOCK  DS    CL(DLCBXLX)         DOWNLOAD INTERFACE BLOCK                     
*                                                                               
SDBLOCK  DS    CL(SOFXTNL)       SOFTDATE BLOCK                                 
OUTDATE  DS    CL12                                                             
         SPACE 1                                                                
         DS    0D                                                               
TAPEIO   DS    700C                                                             
SORTIO   DS    700C                                                             
         SPACE 1                                                                
MYEND    DS    0D                                                               
         EJECT                                                                  
*              DSECT FOR DOWNLOAD MODULE                                        
       ++INCLUDE DDDLCB                                                         
         EJECT                                                                  
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
         DS    CL2                                                              
PSPNH    DS    CL13                                                             
         DS    CL1                                                              
PPNH     DS    CL13                                                             
         DS    CL5                                                              
         EJECT                                                                  
*              DSECT TO COVER TAPE RECORDS                                      
         SPACE 3                                                                
TAPED    DSECT                                                                  
TAPEKEY  DS    0CL20               KEY LENGTH OF 20                             
TAPEREC  DS    0C                  RECORD LENGTH OF 700                         
         SPACE 1                                                                
INREC    DS    0C                  INVOICE RECORD TYPE 1                        
INUN     DS    CL3                 ***                                          
INEMP    DS    CL3                 EMPLOYER (TP PG ETC)                         
INAGY    DS    CL6                 AGENCY CODE                                  
ININV    DS    CL6                 INVOICE NUMBER                               
INTYPE   DS    CL1                 TYPE 1                                       
         DS    CL1                 FILLER                                       
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
INSESREU DS    CL1                 Session / Reuse                              
INSIGFEE DS    CL9                 SIGNATORY FEE                                
INAGYCOM DS    CL9                 AGENCY COMMISSION                            
         DS    CL(INREC+700-*)                                                  
         EJECT                                                                  
*              TOTAL RECORDS (TYPE 4-5)                                         
         SPACE 3                                                                
         ORG   TAPEREC                                                          
TTREC    DS    0C                  TOTAL RECORD TYPE 4-5                        
TTUN     DS    CL3                 UNION (AFM AFT SAG ETC)                      
TTEMP    DS    CL3                 EMPLOYER (TP PG ETC) (TYPE 4)                
TTAGY    DS    CL6                 AGENCY CODE (TYPE 4) OR 999999               
TTINV    DS    CL6                 FILLER (999999)                              
TTTYPE   DS    CL1                 TYPE 4 (AGY) 5(UNION-TAPE ONLY)              
         DS    CL1                 FILLER                                       
         SPACE 1                                                                
TTGROSS  DS    CL9                 GROSS AMOUNT                                 
TTMISC   DS    CL9                 MISCELLANEOUS AMOUNT                         
TTSPNH   DS    CL9                 SUBJECT TO PNH                               
TTPNH    DS    CL9                 PNH                                          
         DS    644C                FILLER                                       
         SPACE 3                                                                
*              GRAND TOTALS                                                     
         SPACE 3                                                                
         ORG   TAPEREC                                                          
GTREC    DS    0C                  TOTAL RECORD TYPE 5(PRINT ONLY) & 6          
         DS    CL3                 FILLER 999                                   
         DS    CL3                 FILLER 999                                   
         DS    CL6                 FILLER 999999                                
         DS    CL6                 FILLER 999999                                
GTTYPE   DS    CL1                 TYPE 5 & 6                                   
         DS    CL1                 FILLER                                       
         SPACE 1                                                                
GTGROSS  DS    CL10                GROSS AMOUNT                                 
GTMISC   DS    CL10                MISCELLANEOUS AMOUNT                         
GTSPNH   DS    CL10                SUBJECT TO PNH                               
GTPNH    DS    CL10                PNH                                          
         DS    640C                FILLER                                       
         EJECT                                                                  
*              DSECT TO COVER MCCANN TAPE RECORDS                               
         SPACE 3                                                                
MCTAPED  DSECT                                                                  
*                                                                               
MCCOMP   DS    CL2                 COMPANY (02 FOR MCCANN)                      
MCOFF    DS    CL2                 OFFICE CODE                                  
MCVEND   DS    CL10                VENDOR - XTALENTPAR                          
MCAGY    DS    CL4                 AGENCY CODE                                  
         DS    CL1                 SPACE                                        
MCINVNM  DS    CL10                INVOICE NUMBER                               
MCINVDT  DS    CL8                 INVOICE DATE CCYYMMDD                        
MCPONM   DS    CL8                 PO NUMBER - NOT USED                         
MCCOST   DS    CL11                COST (PAYMENT AMOUNT) OF INVOICE             
MCCLI    DS    CL5                 CLIENT                                       
MCPROD   DS    CL4                 PRODUCT                                      
MCBRAND  DS    CL4                 BRAND                                        
MCJOBEST DS    CL12                JOB NUMBER (ESTIMATE)                        
MCUTAX   DS    CL4                 USE TAX - NOT USED                           
MCWRK    DS    CL4                 WORK CODE                                    
MCTRANS  DS    CL3                 TRANSACTION CODE (800)                       
MCTAPLNQ EQU   *-MCTAPED                                                        
         EJECT                                                                  
*                                                                               
WKTOTD   DSECT                                                                  
WKTWKCD  DS    CL4               WORK CODE                                      
WKTCOST  DS    XL4               TOTAL COST                                     
WKTLNQ   EQU   *-WKTOTD                                                         
         EJECT                                                                  
*                                                                               
AGYTABD  DSECT                                                                  
AGYAGY   DS    CL6               AGENCY                                         
AGYCOMP  DS    XL2               COMPANY CODE                                   
AGYOFF   DS    XL2               OFFICE CODE                                    
AGYTBLNQ EQU   *-AGYTABD                                                        
         EJECT                                                                  
*                                                                               
         SPACE 1                                                                
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
**PAN#1  DC    CL21'110TAREP52   10/21/14'                                      
         END                                                                    
