*          DATA SET TAREP5F    AT LEVEL 075 AS OF 08/09/16                      
*PHASE T7035FC                                                                  
*INCLUDE POWWOW                                                                 
         TITLE 'T7035F - JWT INTERFACE'                                         
***********************************************************************         
* PROGRAM READS ALL INVOICES FOR REQUESTED PERIOD                               
* PUTS THEM IN SORTER                                                           
* THEN BUILDS THE HEADER RECORD, OUTPUTS TO FILE                                
* GETS RECORDS FROM SORTER, OUTPUTS TO FILE                                     
***********************************************************************         
T7035F   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T7035F,R7                                                      
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9                                                       
*                                                                               
         LA    RA,BUFF                                                          
         LA    RA,8(RA)                                                         
         USING MYD,RA              R7=A(LOCAL W/S)                              
*                                                                               
         L     R6,ATWA                                                          
         USING CONHEADH-64,R6                                                   
         MVC   VSORTER,SORTER                                                   
         GOTO1 INITIAL,DMCB,0                                                   
*                                                                               
         CLI   MODE,VALREC         VALIDATE SCREEN                              
         BE    *+12                                                             
         CLI   MODE,DISPREC                                                     
         BNE   *+12                                                             
         BAS   RE,VKEY                                                          
         B     XIT                                                              
*                                                                               
         CLI   MODE,PRINTREP       PROCESS REPORT                               
         BNE   XIT                                                              
         MVC   AMASTD,TWAMASTC     SAVE ADDRESS OF MASTER                       
         L     R2,TWADCONS                                                      
         USING TWADCOND,R2                                                      
         MVC   ALOGOC,TLOGOC       SAVE A(LOGOC)                                
         L     R2,AMASTD                                                        
         USING MASTD,R2                                                         
         MVC   ALOGO,MCVLOGO       SAVE A(LOGO)                                 
         MVC   AREMOT,MCVREMOT     SAVE A(REMOTE)                               
         DROP  R2                                                               
*                                                                               
         BAS   RE,PREP                                                          
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
*---------------------------------------------------------------------          
*              VALIDATE KEY ROUTINE                                             
*---------------------------------------------------------------------          
VKEY     NTR1                                                                   
         LR    RE,R7               A(LOCAL WORKING STORAGE)                     
         XC    AGYFLST,AGYFLST                                                  
         MVI   AGYFLST,X'FF'                                                    
         XC    TIFILTS,TIFILTS     CLEAR SYSIO FILTERS                          
         XC    SITCLIN,SITCLIN     CLEAR CLIENT NAME                            
         OI    SITCLINH+6,X'80'                                                 
         GOTO1 RECVAL,DMCB,(X'40',TLAYCDQ),(X'08',SITAGYH),SITAGYNH             
         CLI   SITAGY,C'@'                                                      
         BE    VK10                                                             
         BAS   RE,SETAGY           SET SVAGYCO AND LENGTHS                      
         MVC   TIFAGY,TGAGY        SET SYSIO AGENCY FILTER                      
         B     VK20                                                             
VK10     MVC   TIFAGY,TGLST                                                     
         NI    TIFAGY,X'7F'        TURN OFF X'80' FOR SYSIO=FLIST               
         USING TAGLD,R4                                                         
         LA    R2,AGYFLST                                                       
         L     R4,AIO              BUILD FLIST LIST                             
         MVI   ELCODE,TAGLELQ                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
VK15     BAS   RE,NEXTEL                                                        
         BNE   VK20                                                             
         ZIC   RE,TAGLLEN                                                       
         AHI   RE,-4                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),TAGLDATA                                                 
         OC    0(L'TGAGY,R2),SPACES                                             
         LA    R2,L'TGAGY(R2)                                                   
         MVI   0(R2),X'FF'                                                      
         B     VK15                                                             
         DROP  R4                                                               
*                                                                               
VK20     LA    R2,SITPERH          VALIDATE PERIOD                              
         BRAS  RE,VSFTDAT          VALIDATE USING SOFDAT                        
         BE    VK30                OK, CONTNUE                                  
         LA    R3,BLOCK                                                         
         USING PERVALD,R3                                                       
         GOTO1 PDVAL,DMCB,(R3)                                                  
         MVC   PRPER,PVALCPER      SAVE PRINTABLE PERIOD                        
         MVC   TIQPSTR,PVALPSTA                                                 
         MVC   TIQPEND,PVALPEND                                                 
         MVI   TIQDTYPE,TIQDBILL  SET FILTERING ON BILL DATE                    
         DROP  R3                                                               
*                                                                               
VK30     LA    R2,SITCLIH          VALIDATE CLIENT FIELD                        
         CLI   5(R2),0                                                          
         BE    VK40                                                             
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'08',(R2)),SITCLINH                        
         MVC   TIFCLI,TGCLI        SET SYSIO FILTER                             
*                                                                               
VK40     BAS   RE,VOPTS            VALIDATE OPTIONS                             
         B     XIT                                                              
*---------------------------------------------------------------------          
*              VALIDATE OPTIONS                                                 
*---------------------------------------------------------------------          
VOPTS    NTR1                                                                   
         MVI   TAPEOPT,C'Y'                                                     
         MVI   TRPWOPT,C'N'                                                     
         MVI   POWOPT,C'Y'                                                      
*                                                                               
         LA    R2,SITOPTH                                                       
         CLI   5(R2),0                                                          
         BE    VOPTX                                                            
         GOTO1 SCANNER,DMCB,(R2),(10,BLOCK),0                                   
         ZIC   R0,4(R1)                                                         
         LA    R4,BLOCK                                                         
         LTR   R0,R0                                                            
         BZ    INVERR                                                           
*                                                                               
VOPT10   CLC   12(4,R4),=C'TAPE'   TAPE OPTION                                  
         BNE   VOPT20                                                           
         CLI   ACTEQU,ACTDOWN      TAPE NOT VALID OPTION FOR THIS               
         BE    INVERR                                                           
         MVC   TAPEOPT,22(R4)                                                   
         B     VOPTEND                                                          
*                                                                               
VOPT20   CLC   12(5,R4),=C'NOPOW'   NO POWWOW                                   
         BNE   VOPT30                                                           
         MVI   POWOPT,C'N'                                                      
         B     VOPTEND                                                          
*                                                                               
VOPT30   CLC   12(5,R4),=C'TRPOW'   TRACE POWWOW                                
         BNE   VOPT90                                                           
         MVI   TRPWOPT,C'Y'                                                     
         B     VOPTEND                                                          
*                                                                               
VOPT90   DS    0H                                                               
VOPTEND  LA    R4,32(R4)                                                        
         BCT   R0,VOPT10                                                        
VOPTX    B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------          
*              THIS ROUTINE SETS SVAGYCO AND LENGHTS                            
*---------------------------------------------------------------------          
SETAGY   NTR1                                                                   
         GOTO1 CHAROUT,DMCB,TANUELQ,0,TANUTCO                                   
         MVC   SVAGYCO,TGNAME                                                   
*                                                                               
         XC    LENGTHS,LENGTHS                                                  
         L     R4,AIO                                                           
         MVI   ELCODE,TANUELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TANUTJOB))                                     
         BNE   XIT                                                              
         L     R4,TGELEM                                                        
         USING TANUD,R4                                                         
         MVC   LENGTHS,TANUMBER                                                 
         NI    LENGTHS,X'0F'       TURN OFF CHARACTERS                          
         NI    LENGTHS+1,X'0F'                                                  
         NI    LENGTHS+2,X'0F'                                                  
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*---------------------------------------------------------------------          
*              PRINT REPORT                                                     
*---------------------------------------------------------------------          
PREP     NTR1                                                                   
         MVI   SORTFRST,C'Y'                                                    
         ZAP   CNTINVS,=P'0'                                                    
         ZAP   CNTDTLS,=P'0'                                                    
         XC    TOTAMT,TOTAMT                                                    
*                                                                               
         LA    R1,MYSPECS                                                       
         ST    R1,SPECS                                                         
         LA    R1,HOOK             INITIALIZE FOR REPORT                        
         ST    R1,HEADHOOK                                                      
*                                                                               
         MVC   TIACOMFC,ACOMFACS   SET UP FOR TASYSIO                           
         LA    R1,IOHOOK                                                        
         ST    R1,TIHOOK                                                        
*                                                                               
         MVI   TIREAD,TLINDCDQ     READ INVOICE RECORDS                         
         OI    TIQFLAGS,TIQFPBNP   ASK TO PASS BNP AS WELL                      
         LA    R1,TGD              SET A(TALENT GLOBALS)                        
         ST    R1,TIATGLOB                                                      
         MVI   TIQDTYPE,TIQDBILL   SET FILTERING ON BILL DATE                   
         OI    TIQFLAG2,TIQFSUB    PASS SUBSIDIARY INVOICES                     
*                                                                               
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
*                                                                               
         CP    CNTINVS,=P'0'       MUST HAVE 1 INVOICE                          
         BE    PREPX                                                            
*                                                                               
         BAS   RE,OPENTAPE                                                      
         BAS   RE,FILHDR           BUILD FILE HEADER RECORD                     
*                                                                               
PREP10   GOTO1 VSORTER,DMCB,=C'GET' GET A SORT RECORD                           
         L     R2,DMCB+4                                                        
         LTR   R2,R2               IF NO MORE SORT RECORDS                      
         BZ    PREP50                                                           
         BAS   RE,PUTTAPE          WRITE OUT LAST TAPE RECORD                   
         B     PREP10                                                           
*                                                                               
PREP50   BAS   RE,CLOSTAPE                                                      
         BAS   RE,CALLPOW                                                       
PREPX    B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------           
*              HEADLINES ETC                                                    
*--------------------------------------------------------------------           
HOOK     NTR1                                                                   
         MVC   H1+47(13),=CL24'JWT INTERFACE'                                   
         GOTO1 CENTER,DMCB,H1+47,24                                             
         GOTO1 UNDERLIN,DMCB,(24,H1+52),(X'BF',H2+52)                           
         MVC   H3+52(6),=C'PERIOD'                                              
         LA    R1,SITPERH                                                       
         MVC   H3+59(17),8(R1)                                                  
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------          
*              ROUTINE TO SET UP SECOND REPORT ON QUEUE                         
*---------------------------------------------------------------------          
IOHOOK   NTR1                                                                   
         CLI   TIMODE,PROCREC      IF THERE IS SOMETHING INTERESTING            
         BNE   XIT                                                              
*                                                                               
         L     R4,TIAREC                                                        
         MVI   ELCODE,TAPDELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING TAPDD,R4                                                         
*                                                                               
         MVC   SVUSE,TAPDUSE       USE                                          
***      L     RF,TAPDGRS                                                       
***      A     RF,TAPDAPPL                                                      
***      A     RF,TAPDGUAR                                                      
         L     RF,TAPDPAYI         IND PAYMENT                                  
         A     RF,TAPDPAYC         CORP PAYMENT                                 
         A     RF,TAPDREXP         REIMB EXP                                    
         ST    RF,SVGROSS                                                       
         MVC   SVPNH,TAPDPNH       P&H                                          
*                                                                               
         MVC   KEY,TIKEY                                                        
         GOTO1 HIGH                                                             
*                                                                               
         L     R4,TIAREC                                                        
         MVI   ELCODE,TABDELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING TABDD,R4                                                         
         MVC   SVINVTOT,TABDTOT    INVOICE TOTAL                                
         L     RF,TABDTAX                                                       
         A     RF,TABDHND                                                       
         A     RF,TABDHNDC                                                      
         A     RF,TABDSIGN                                                      
         ST    RF,SVTAXHND         TAX+HANDLING                                 
*                                                                               
         L     RF,TABDCSF                                                       
         ST    RF,SVCSF            CSF                                          
*                                                                               
         L     RF,TABDGST                                                       
         A     RF,TABDPST                                                       
         ST    RF,SVGSTPST         GST, PST AND QST COMBINED                    
*                                                                               
         XC    CANRATE,CANRATE                                                  
         OC    TABDCCVT,TABDCCVT                                                
         BZ    IOHK4                                                            
         ZAP   DUB,TABDCCVT                                                     
         CVB   R1,DUB                                                           
         ST    R1,CANRATE                                                       
*                                                                               
IOHK4    MVC   SVCLI,SPACES                                                     
         MVC   SVPRJ,SPACES                                                     
         L     R4,TIAREC           GET ESTIMATE NUMBER                          
         MVI   ELCODE,TANUELQ                                                   
         BAS   RE,GETEL                                                         
         B     IOHK7                                                            
IOHK5    BAS   RE,NEXTEL                                                        
IOHK7    BNE   XIT                                                              
         USING TANUD,R4                                                         
         CLI   TANUTYPE,TANUTEST                                                
         BNE   IOHK5                                                            
*                                                                               
         MVC   WORK,SPACES                                                      
         SR    RF,RF                                                            
         IC    RF,TANULEN                                                       
         SH    RF,=Y(TANULNQ+1)                                                 
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),TANUMBER   ESTIMATE                                      
         MVC   SVPRJ,WORK                                                       
         MVC   SVCLI,WORK+7                                                     
         MVC   SVPRD,WORK+13                                                    
*                                                                               
         BAS   RE,GETBUNIT         GET BUNIT FROM PROJECT OR AGENCY             
         BAS   RE,CONVUSD          CONVERT TO US$                               
         BAS   RE,INVDET           INVOICE DETAILS                              
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------          
*              CONVERT AMOUNTS TO USD                                           
*---------------------------------------------------------------------          
CONVUSD  NTR1                                                                   
         OC    CANRATE,CANRATE     DON'T BOTHER IF RATE NOT DEFINED             
         BZ    XIT                                                              
         LA    R1,SVAMTS                                                        
         LA    R0,4                                                             
CUSD04   BAS   RE,CUSD06           CONVERT THE TAPDS                            
         LA    R1,4(R1)                                                         
         BCT   R0,CUSD04                                                        
*                                                                               
         L     RF,SVGROSS                                                       
         A     RF,SVPNH                                                         
         A     RF,SVTAXHND                                                      
         A     RF,SVGSTPST                                                      
         A     RF,SVCSF                                                         
         C     RF,SVINVTOT         DO PARTS ADD UP?                             
         BE    XIT                                                              
         S     RF,SVINVTOT         PUT DIFFERENCE IN HANDLING                   
         L     RE,SVTAXHND                                                      
         SR    RE,RF                                                            
         ST    RE,SVTAXHND                                                      
*                                                                               
         B     XIT                                                              
*                                                                               
CUSD06   NTR1                      ADJUST FULLWORD BY CANRATE                   
         L     RF,0(R1)                                                         
         M     RE,CANRATE                                                       
         D     RE,=F'5000'                                                      
         LTR   RF,RF                                                            
         BM    *+8                                                              
         AH    RF,=H'1'                                                         
         SRA   RF,1                                                             
         ST    RF,0(R1)                                                         
         B     XIT                                                              
*---------------------------------------------------------------------          
*              BUILD INVOICE DETAILS                                            
*---------------------------------------------------------------------          
INVDET   NTR1                                                                   
         LA    R5,TAPEIO                                                        
         USING TAPED,R5                                                         
         BAS   RE,CLEARTAP         CLEAR TAPEIO                                 
*                                                                               
         MVC   DTLTYPE,=C'DTL'     SETS UP DETAIL SECTION                       
         MVC   DTLBUNIT,SVBUNIT                                                 
         MVC   DTLCSTNM(L'SVCLI),SVCLI      CLIENT                              
         MVC   DTLPRDNM(L'SVPRD),SVPRD      PRODUCT                             
         MVC   DTLPRJID(L'SVPRJ),SVPRJ      ESTIMATE                            
*                                                                               
         L     R4,TIAREC           INVOICE NUMBER                               
         USING TLIND,R4                                                         
         MVC   WORK(6),TLININV                                                  
         XC    WORK(6),=6X'FF'                                                  
         GOTO1 TINVCON,DMCB,WORK,SVINVNM,DATCON                                 
         MVC   DTLINVNM(6),SVINVNM                                              
*                                                                               
         L     R4,TIAREC           INVOICE DATE                                 
         MVI   ELCODE,TAINELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAIND,R4                                                         
         GOTO1 DATCON,DMCB,(1,TAINBDTE),(20,DTLINVDT)                           
         GOTO1 DATCON,DMCB,(1,TAINBDTE),(13,SVINVDT)                            
*                                                                               
         L     R4,TIAREC                                                        
         MVI   ELCODE,TANUELQ                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
INVDET03 BAS   RE,NEXTEL                                                        
         BNE   INVDET05                                                         
         USING TANUD,R4                                                         
         CLI   TANUTYPE,TANUTAUT    LOOK FOR AUTH/PO                            
         BNE   INVDET03                                                         
         MVC   WORK,SPACES                                                      
         ZIC   RF,TANULEN                                                       
         SH    RF,=Y(TANULNQ+1)                                                 
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),TANUMBER                                                 
         MVC   DTLORDNM,WORK                                                    
         DROP  R4                                                               
*                                                                               
INVDET05 OC    SVAMTS,SVAMTS                                                    
         BZ    XIT                                                              
         AP    CNTINVS,=P'1'       BUMP UP # OF INVOICES                        
*                                                                               
         L     RF,TOTAMT           ACCUMULATE INVOICE TOTALS                    
         A     RF,SVINVTOT                                                      
         ST    RF,TOTAMT                                                        
*                                                                               
         EDIT  SVINVTOT,(17,DTLSUBTT+10),2,FLOAT=-                              
         EDIT  SVINVTOT,(17,DTLINVAM+10),2,FLOAT=-                              
*                                                                               
         OC    SVGROSS,SVGROSS     GROSS                                        
         BZ    INVDET10                                                         
         MVC   DTLSBCAT,=C'20900'                                               
         BAS   RE,MUSICWC                                                       
         EDIT  SVGROSS,(17,DTLCHGAM+10),2,FLOAT=-                               
         BAS   RE,SORTPUT                                                       
         AP    CNTDTLS,=P'1'       BUMP UP # OF DETAILS                         
*                                                                               
INVDET10 L     RF,SVPNH            PENSION & HEALTH + GST, PST, QST             
         A     RF,SVGSTPST                                                      
         LTR   RF,RF                                                            
         BZ    INVDET20                                                         
         MVC   DTLSBCAT,=C'20750'                                               
         EDIT  (RF),(17,DTLCHGAM+10),2,FLOAT=-                                  
         BAS   RE,SORTPUT                                                       
         AP    CNTDTLS,=P'1'       BUMP UP # OF DETAILS                         
*                                                                               
INVDET20 OC    SVTAXHND,SVTAXHND   TAX AND HANDLING                             
         BZ    INVDET30                                                         
         MVC   DTLSBCAT,=C'22450'                                               
         EDIT  SVTAXHND,(17,DTLCHGAM+10),2,FLOAT=-                              
         BAS   RE,SORTPUT                                                       
         AP    CNTDTLS,=P'1'       BUMP UP # OF DETAILS                         
*                                                                               
INVDET30 OC    SVCSF,SVCSF         CSF                                          
         BZ    INVDET40                                                         
         MVC   DTLSBCAT,=C'22004'                                               
         EDIT  SVCSF,(17,DTLCHGAM+10),2,FLOAT=-                                 
         BAS   RE,SORTPUT                                                       
         AP    CNTDTLS,=P'1'       BUMP UP # OF DETAILS                         
*                                                                               
INVDET40 LA    R2,P                                                             
         USING PRINTD,R2                                                        
         MVC   PBUNIT,SVBUNIT                                                   
         MVC   PINVOICE(6),SVINVNM                                              
         MVC   PINVDATE,SVINVDT                                                 
         MVC   PCLIENT,SVCLI                                                    
         MVC   PPRJCT(L'SVPRJ),SVPRJ                                            
         EDIT  SVGROSS,(15,PGROSS),2,FLOAT=-                                    
         L     RF,SVPNH                                                         
         A     RF,SVGSTPST                                                      
         EDIT  (RF),(15,PPNH),2,FLOAT=-                                         
         L     RF,SVTAXHND                                                      
         A     RF,SVCSF                                                         
         EDIT  (RF),(15,PTNH),2,FLOAT=-                                         
         EDIT  SVINVTOT,(15,PINVAMT),2,FLOAT=-                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------           
*              SPACE OUT TAPEIO AREA                                            
*--------------------------------------------------------------------           
CLEARTAP NTR1                                                                   
         LA    R2,TAPEIO                                                        
         MVI   0(R2),C' '                                                       
         MVC   1(255,R2),0(R2)                                                  
         AHI   R2,256                                                           
         MVC   0(57,R2),TAPEIO                                                  
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------           
*              SPACE OUT TAPEIO AREA                                            
*--------------------------------------------------------------------           
MUSICWC  NTR1                                                                   
         LA    RF,MUSUSETB         MUSIC USE TABLE                              
MWC10    CLI   0(RF),X'FF'                                                      
         BE    XIT                                                              
         CLC   SVUSE,0(RF)                                                      
         BE    MWC90                                                            
         AHI   RF,3                                                             
         B     MWC10                                                            
*                                                                               
MWC90    MVC   DTLSBCAT,=C'20622'                                               
         B     XIT                                                              
*                                                                               
MUSUSETB DC    C'BSM',C'CMS',C'FMU',C'IMS',C'MUS',C'OTM',C'PEM',C'SMU'          
         DC    X'FFFFFF'                                                        
         EJECT                                                                  
*--------------------------------------------------------------------           
*              GET BUSINESS UNIT FROM AGENCY RECORD                             
*--------------------------------------------------------------------           
GETBUNIT NTR1                                                                   
         MVC   SVBUNIT,=CL5'12000'   DEFAULT=ATLANTA                            
*                                                                               
         L     R4,TIAREC             INVOICE NUMBER                             
         USING TLIND,R4                                                         
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'A0',TLINAGY)                              
*                                                                               
         USING TAAYD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAAYELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   GBUNIT10                                                         
         EDIT  TAAYBUNT,(5,SVBUNIT),0,FLOAT=0                                   
*                                                                               
GBUNIT10 MVC   AGYBUNIT,SVBUNIT                                                 
*                                                                               
         USING TLJBD,R4                                                         
GBUNIT20 XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         MVI   TLJBCD,TLJBCDQ                                                   
         MVI   TLJBSPCL,TLJBSPJW   JWT JOBS                                     
         MVC   TLJBPRJI,SVPRJ                                                   
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(TLJBDATE-TLJBD),KEYSAVE     FIND THE JWT JOB?                
         BNE   GBUNIT99                                                         
         MVC   SVBUNIT,TLJBBUNT    USE THE JOB'S BUNIT                          
         CLC   SVCLI,SPACES        IF CLIENT EMPTY                              
         BH    *+10                                                             
         MVC   SVCLI,TLJBCSTI      CUSTOMER ID                                  
         CLC   SVPRD,SPACES        IF PRODUCT EMPTY                             
         BH    *+10                                                             
         MVC   SVPRD,TLJBPRDI      PRODUCT ID                                   
*                                                                               
GBUNIT99 MVC   KEY,TIKEY           RESTORE KEY                                  
         GOTO1 HIGH                                                             
*                                                                               
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*---------------------------------------------------------------------          
*              BUILD FILE HEADER                                                
*---------------------------------------------------------------------          
FILHDR   NTR1                                                                   
         LA    R5,TAPEIO                                                        
         USING TAPED,R5                                                         
         BAS   RE,CLEARTAP         CLEAR TAPEIO                                 
*                                                                               
         MVC   HDRTYPE,=C'HDR'     HEADER RECORD                                
         MVC   HDRVNDR,=C'0000000739'                                           
         MVC   HDRVNDRL(8),=C'REMIT TO'                                         
         GOTO1 DATCON,DMCB,(1,TGTODAY1),(20,HDRFILDT)     TODAY'S DATE          
         MVC   HDRFILSQ,=C'001'               FILE SEQUENCE NUMBER              
         MVC   HDRBCURR,=C'USD'               CURRENCY                          
         EDIT  CNTDTLS,(5,HDRROWCT),0                                           
         EDIT  CNTINVS,(5,HDRINVCT),0                                           
         EDIT  TOTAMT,(17,HDRTOTCH+10),2,FLOAT=-                                
         LR    R2,R5                                                            
         BAS   RE,PUTTAPE                                                       
         B     XIT                                                              
*                                                                               
         DROP  R5                                                               
         EJECT                                                                  
*--------------------------------------------------------------------           
* SORT UTILITIES                                                                
*--------------------------------------------------------------------           
SORTPUT  NTR1                                                                   
         LA    R5,TAPEIO                                                        
         USING TAPED,R5                                                         
         CLI   SORTFRST,C'Y'                                                    
         BNE   SORTPUT2                                                         
         GOTO1 VSORTER,DMCB,SORTCARD,RECCARD                                    
         MVI   SORTFRST,C'N'                                                    
*                                                                               
SORTPUT2 GOTO1 VSORTER,DMCB,=C'PUT',TAPEIO                                      
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------          
*              PUT RECORD TO TAPE                                               
*---------------------------------------------------------------------          
PUTTAPE  NTR1                                                                   
*                                                                               
         L     R1,=A(JWTTAPE)                                                   
         PUT   (1),(R2)                                                         
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
*---------------------------------------------------------------------          
*              CLOSE TAPE                                                       
*---------------------------------------------------------------------          
CLOSTAPE NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R2,P                                                             
         USING PRINTD,R2                                                        
         L     R2,=A(JWTTAPE)                                                   
         CLOSE ((2))                                                            
         XIT1                                                                   
         EJECT                                                                  
*              ODD ROUTINES                                                     
         SPACE 3                                                                
SPLAT    NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         SPACE 1                                                                
INVERR   MVI   ERROR,INVALID                                                    
         GOTO1 ERREX                                                            
         SPACE 1                                                                
         GETEL R4,DATADISP,ELCODE                                               
         SPACE 1                                                                
         EJECT                                                                  
*                                                                               
         DS    0F                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,38,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=(312)'                                 
*                                                                               
MYSPECS  SSPEC H1,1,RUN                                                         
         SSPEC H3,1,REQUESTOR                                                   
         SSPEC H1,99,AGYNAME                                                    
         SSPEC H2,99,AGYADD                                                     
         SSPEC H3,99,REPORT                                                     
         SSPEC H3,112,PAGE                                                      
         SSPEC H5,2,C'BU    INVOICE DATE     CLIENT PROJECT         '           
         SSPEC H6,2,C'----- ------- -------- ------ --------------- '           
         SSPEC H5,48,C'GROSS           P+H             '                        
         SSPEC H6,48,C'--------------- --------------- '                        
         SSPEC H5,80,C'TAX+HANDLING    INVOICE        '                         
         SSPEC H6,80,C'--------------- ---------------'                         
         DC    H'0'                                                             
APERH    DS    A                                                                
         LTORG                                                                  
         EJECT                                                                  
*              LTORG ETC                                                        
         SPACE 3                                                                
*                                  COUNTS                                       
*                                                                               
*              OTHER AREAS NOT DIRECTLY ADDRESSABLE                             
*                                                                               
         ENTRY JWTTAPE                                                          
*                                                                               
*  UNITAPE DEFINED AS TAPE FOR UT REPORT.                                       
*  UNITAPE DEFINED AS DISK FOR UD REPORT.                                       
*                                                                               
JWTTAPE  DCB   DDNAME=JWTTAPE,DSORG=PS,MACRF=(PM),                     X        
               RECFM=FB,LRECL=312,BUFNO=2,BLKSIZE=3120                          
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
         JZ    VSFTDAT3                                                         
* CHECK FOR YEAR, MONTH VALIDATION                                              
         MVI   SOFITYPE,SOFITYM    VALIDATE FOR YEAR, MONTH THEN                
         GOTO1 SOFTDATE,SOFDATD                                                 
         JZ    VSFTDAT3                                                         
         MVC   ERROR,SOFERROR                                                   
         J     NO                                                               
*                                                                               
VSFTDAT3 CLI   OFFLINE,C'Y'                                                     
         JE    VSFTDAT5                                                         
         CLI   CONOUTH+4,0         RLP ONLINE, IF GROUP NAME IN OUTPUT          
         JE    VSFTDAT5                                                         
         CLC   =C'FILE',CONDEST    AND FILE IN DESTINATION                      
         JE    YES                 DON'T RESOLVE DATES                          
*                                                                               
VSFTDAT5 OI    SOFIINDS,SOFIIRES   RESOLVE THE DATES TO ACTUAL                  
         GOTO1 SOFTDATE,SOFDATD                                                 
         GOTO1 DATCON,DMCB,(0,OUTDATE),(1,TIQPSTR)                              
         GOTO1 DATCON,DMCB,(0,OUTDATE+6),(1,TIQPEND)                            
         J     YES                                                              
*                                                                               
         DROP  R1                                                               
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------           
*              TAPE ROUTINES                                                    
*--------------------------------------------------------------------           
OPENTAPE NTR1  BASE=*,LABEL=*                                                   
         L     R2,=A(JWTTAPE)                                                   
         OPEN  ((2),OUTPUT)                                                     
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LINKX MF=(E,PARMLST),SF=(E,LINKLST)                                    
OPENXIT  XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------           
*              PUT TAPEIO TO TAPE                                               
*--------------------------------------------------------------------           
*--------------------------------------------------------------------           
*              ROUTINE TO SUBMIT JCL TO POWWOW                                  
*              FOR ADVANTIS TRANSMISSION                                        
*--------------------------------------------------------------------           
CALLPOW  NTR1  BASE=*,LABEL=*                                                   
         XC    HEADHOOK,HEADHOOK                                                
         MVC   TAPEC4,RTGEN        UPDATE GENERATION NUMBER                     
         LA    R0,NUMCRD           R0=N'OF JCL CARDS                            
         LA    R3,JCL              R3=A(JCL FOR POWWOW)                         
*        MVC   FILE2(5),AGYBUNIT   (AS PER JAV, 2010/02/22)                     
         GOTO1 DATCON,DMCB,(1,TGTODAY1),(20,FILE3)     TODAY'S DATE             
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
JOBC     DC    CL80'//TPCHTJWP  JOB   ''CNTL'',MSGLEVEL=(1,1),COND=((0,X        
               NE)),'                                                           
         DC    CL80'//     MSGCLASS=X,CLASS=A,NOTIFY=CCOL'                      
         DC    CL80'//*MAIN CLASS=ADVANTIS,SYSTEM=SY1'                          
         DC    CL80'//SS  EXEC  BDEDICT'                                        
         DC    CL80'//EXTSYSIN DD *'                                            
*                                                                               
EDICT1   DC    C'EDICTKEY=FTPINTE'                                              
         DC    CL(80-(*-EDICT1))' '                                             
*                                                                               
SUBJ1    DC    C'SUBJECT=JWT INTERFACE'                                         
         DC    CL(80-(*-SUBJ1))' '                                              
*                                                                               
FILE1    DC    C'FILE=APINV_TP_'                                                
*ILE2    DC    C'00000'                                                         
*        DC    C'_'                                                             
FILE3    DC    C'YYYYMMDD'                                                      
FILE4    DC    CL(80-(*-FILE1))' '                                              
*                                                                               
         DC    CL80'EXT=TXT'                                                    
*                                                                               
TAPEC1   DC    C'DSN='                                                          
TAPEC2   DC    CL16'TALDISK.TA0JWDS1'                                           
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
DSNME    DC    CL8'JWTTAPE'                                                     
RETAREA  DS    0CL44                                                            
RTDSN    DC    CL17' '                                                          
RTGEN    DC    CL8' '        G0000000                                           
RTND     DC    CL19' '       SPARE                                              
ERRCHK   DS    CL1                                                              
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
MYSTART  DS    0D                                                               
MYREGS   DS    16F                                                              
ODDAREA  DS    CL12                                                             
*                                                                               
DYNALLOC DS    A                                                                
AMASTD   DS    A                   A(MASTER)                                    
ALOGOC   DS    A                   A(LOGOC)                                     
ALOGO    DS    A                   A(LOGO)                                      
AREMOT   DS    A                   A(REMOTE)                                    
VSORTER  DS    V                   V(SORTER)                                    
*                                                                               
*                                  OPTIONS                                      
TAPEOPT  DS    CL1                 Y=GENERATE OUTPUT TAPE                       
SORTOPT  DS    CL1                                                              
LISTOPT  DS    CL1                 Y=LIST AND COUNT ONLY                        
BILLOPT  DS    CL1                 BILLING ONLY                                 
SEQOPT   DS    CL1                 SEQUENCE - DEFAULT IS UNION                  
PAYPSTAT DS    CL1                                                              
SESSOPT  DS    CL1                 Y=SESSION USES ONLY                          
RETROOPT DS    CL1                 Y=GET ONLY RETROACTIVE PAYMENTS              
*                                  A=ALSO GET RETROACTIVE PAYMENTS              
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
SVINVNM  DS    CL6                                                              
SVINVDT  DS    CL10                                                             
PRPER    DS    CL17                PRINTABLE PERIOD                             
SVAGYCO  DS    CL4                 SAVED AGENCY CO=                             
LENGTHS  DS    CL3                 AGENCY ESTIMATE SETUP LENGTHS                
SVOFF    DS    CL1                 OFFICE                                       
SVBUNIT  DS    CL5                 BUSINESS UNIT                                
SVUSE    DS    CL3                 USE                                          
SVCLI    DS    CL6                 CLIENT                                       
SVPRD    DS    CL3                 PRODUCT                                      
SVPRJ    DS    CL7                 ESTIMATE                                     
         DS    0F                                                               
SVAMTS   DS    0XL20                                                            
SVGROSS  DS    F                   GROSS                                        
SVPNH    DS    F                   P&H                                          
SVTAXHND DS    F                   TAX AND HANDLING AND CSF                     
SVGSTPST DS    F                   GST, PST AND QST                             
SVCSF    DS    F                   CSF                                          
SVINVTOT DS    F                   INVOICE TOTAL                                
*                                                                               
CANRATE  DS    F                   CANADIAN CONVERSION RATE                     
*                                                                               
AGYBUNIT DS    CL5                 AGENCY'S BUSINESS UNIT                       
TOTAMT   DS    F                                                                
*                                                                               
SDBLOCK  DS    CL(SOFXTNL)         SOFTDATE BLOCK                               
OUTDATE  DS    CL12                                                             
*                                                                               
         DS    0D                                                               
CNTINVS  DS    PL6                 COUNT INVOICES                               
CNTDTLS  DS    PL6                 COUNT DETAIL RECORDS                         
TAPCOUNT DS    PL6                                                              
*                                                                               
AGYFLST  DS    CL241               AGENCY FLIST                                 
         DS    0D                                                               
TAPEIO   DS    312C                                                             
         SPACE 1                                                                
MYEND    DS    0D                                                               
         EJECT                                                                  
*---------------------------------------------------------------                
*              DSECT TO COVER PRINT LINES                                       
         SPACE 3                                                                
PRINTD   DSECT                                                                  
         DS    CL1                                                              
PBUNIT   DS    CL5                                                              
         DS    CL1                                                              
PINVOICE DS    CL7                                                              
         DS    CL1                                                              
PINVDATE DS    CL8                                                              
         DS    CL1                                                              
PCLIENT  DS    CL6                                                              
         DS    CL1                                                              
PPRJCT   DS    CL15                                                             
         DS    CL1                                                              
PGROSS   DS    CL15                                                             
         DS    CL1                                                              
PPNH     DS    CL15                                                             
         DS    CL1                                                              
PTNH     DS    CL15                                                             
         DS    CL1                                                              
PINVAMT  DS    CL15                                                             
         EJECT                                                                  
       ++INCLUDE DDDLCB                                                         
         EJECT                                                                  
*---------------------------------------------------------------------          
*              DSECT TO COVER TAPE RECORDS                                      
*---------------------------------------------------------------------          
TAPED    DSECT                                                                  
TAPEREC  DS    0C                  RECORD LENGTH OF 312                         
*                                                                               
HDRREC   DS    0C                  HEADER RECORD                                
HDRTYPE  DS    CL3                 HDR                                          
HDRVNDR  DS    CL10                VENDOR                                       
HDRVNDRL DS    CL10                VENDOR LOCATION                              
HDRFILDT DS    CL8                 FILE DATE (YYYYMMDD)                         
HDRFILSQ DS    CL3                 SEQUENCE NUMBER                              
HDRBCURR DS    CL3                 BILLING CURRENCY (USD)                       
HDRROWCT DS    CL5                 ROW COUNT                                    
HDRINVCT DS    CL5                 INVOICE COUNT                                
HDRTOTCH DS    CL27                TOTAL CHARGE AMOUNT SIGNED                   
*                                                                               
         ORG   HDRREC                                                           
DTLREC   DS    0C                  DETAIL RECORD                                
DTLTYPE  DS    CL3                 DTL                                          
DTLBUNIT DS    CL5                 BUSINESS UNIT (12000 = TP OFFICE 4)          
DTLINVNM DS    CL30                INVOICE NUMBER                               
DTLINVDT DS    CL8                 INVOICE DATE (YYYYMMDD)                      
DTLPOID  DS    CL10      BLANK     PO ID                                        
DTLSUBTT DS    CL27                SUBTOTAL AMOUNT SNUM23.3 (3 DECS)            
DTLSTAX  DS    CL27      BLANK     SALES TAX                                    
DTLINVAM DS    CL27                INVOICE AMOUNT SNUM23.3 (3 DECS)             
DTLCSTNM DS    CL30                CUSTOMER NAME                                
DTLPRDNM DS    CL30      BLANK     PRODUCT NAME                                 
DTLPRJID DS    CL15                PROJECT ID                                   
DTLSBCAT DS    CL5                 SUB-CATEGORY                                 
DTLCHGAM DS    CL27                CHARGE AMOUNT SNUM23.3 (3 DECS)              
DTLORDNM DS    CL30      BLANK     ORDER NUMBER                                 
DTLORDDT DS    CL8       BLANK     ORDER DATE                                   
DTLEMPNM DS    CL30      BLANK     EMPLOYEE NAME                                
*                                                                               
TAPELLN  EQU   *-TAPEREC                                                        
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
       ++INCLUDE DDPERVALD                                                      
TWADCOND DSECT                                                                  
       ++INCLUDE DDTWADCONS                                                     
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FATIOB                                                         
       ++INCLUDE TAREPFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE TAREPC0D                                                       
         SPACE 1                                                                
       ++INCLUDE DDGENTWA                                                       
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'075TAREP5F   08/09/16'                                      
         END                                                                    
