*          DATA SET TAREP61    AT LEVEL 015 AS OF 11/26/14                      
*PHASE T70361E                                                                  
*INCLUDE DLFLD                                                                  
*        TITLE 'T70361 - GREY INTERFACE - INIT'                                 
***********************************************************************         
* PROGRAM READS ALL INVOICES FOR REQUESTED PERIOD                               
* PUTS THEM IN SORTER                                                           
* THEN BUILDS THE HEADER RECORD, OUTPUTS TO FILE                                
* GETS RECORDS FROM SORTER, OUTPUTS TO FILE                                     
***********************************************************************         
*                                                                               
T70361   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70361,R7                                                      
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
*                                                                               
         GOTO1 INITIAL,DMCB,0                                                   
*                                                                               
         CLI   MODE,VALREC         VALIDATE SCREEN                              
         BE    *+12                                                             
         CLI   MODE,DISPREC                                                     
         BNE   *+12                                                             
         BRAS  RE,VKEY                                                          
         B     XIT                                                              
*                                                                               
         CLI   MODE,PRINTREP       PROCESS REPORT                               
         BNE   XIT                                                              
*                                                                               
         MVC   AMASTD,TWAMASTC     SAVE ADDRESS OF MASTER                       
*                                                                               
         L     R2,TWADCONS                                                      
         USING TWADCOND,R2                                                      
*                                                                               
         MVC   ALOGOC,TLOGOC       SAVE A(LOGOC)                                
*                                                                               
         L     R2,AMASTD                                                        
         USING MASTD,R2                                                         
*                                                                               
         MVC   ALOGO,MCVLOGO       SAVE A(LOGO)                                 
         MVC   AREMOT,MCVREMOT     SAVE A(REMOTE)                               
*                                                                               
         DROP  R2                                                               
*                                                                               
         BRAS  RE,PREP                                                          
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T70361 - GREY INTERFACE - VKEY'                                 
*---------------------------------------------------------------------          
*              VALIDATE KEY ROUTINE                                             
*---------------------------------------------------------------------          
*                                                                               
VKEY     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LR    RE,R7               A(LOCAL WORKING STORAGE)                     
*                                                                               
         XC    TIFILTS,TIFILTS     CLEAR SYSIO FILTERS                          
         XC    SITCLIN,SITCLIN     CLEAR CLIENT NAME                            
         OI    SITCLINH+6,X'80'                                                 
*                                                                               
*        VALIDATE AGENCY FIELD AND READ AGENCY RECORD                           
*                                                                               
         GOTO1 RECVAL,DMCB,(X'40',TLAYCDQ),(X'08',SITAGYH),SITAGYNH             
*                                                                               
         CLI   SITAGY,C'@'         SKIP IF FLIST ID                             
         BE    VK10                                                             
*                                                                               
         MVC   TIFAGY,TGAGY        SET SYSIO AGENCY FILTER                      
         MVC   AGYFLST(L'TGAGY),TGAGY                                           
         MVI   AGYFLST+L'TGAGY,X'FF'                                            
         B     VK20                                                             
*                                                                               
*        IF LIST RECORD, BUILD LIST OF AGENCIES                                 
*                                                                               
VK10     MVC   TIFAGY,TGLST        SAVE LIST CODE                               
         NI    TIFAGY,X'7F'        TURN OFF X'80' FOR SYSIO=FLIST               
*                                                                               
         USING TAGLD,R4                                                         
*                                                                               
         LA    R2,AGYFLST          AGENCY LIST SAVEAREA                         
*                                                                               
         L     R4,AIO              BUILD FLIST LIST                             
         MVI   ELCODE,TAGLELQ                                                   
         BRAS  RE,GETEL            FIND LIST ELEMENT                            
         B     *+8                                                              
VK15     BRAS  RE,NEXTEL                                                        
         BNE   VK20                DONE AT END OF ELEMENTS                      
*                                                                               
         ZIC   RE,TAGLLEN                                                       
         AHI   RE,-4                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),TAGLDATA    MOVE LIST OF AGYS TO SAVEAREA                
*                                                                               
         OC    0(L'TGAGY,R2),SPACES  SPACE FILL                                 
*                                                                               
         LA    R2,L'TGAGY(R2)      BUMP TO NEXT POSITION IN LIST                
         MVI   0(R2),X'FF'         SET END OF LIST MARKER                       
*                                                                               
         B     VK15                FIND NEXT LIST ELEMENT                       
*                                                                               
         DROP  R4                                                               
*                                                                               
*        VALIDATE PERIOD                                                        
*                                                                               
VK20     CLI   SITPERH+5,0                                                      
         BNE   VK30                                                             
         MVC   PRPER(L'TGTODAY8),TGTODAY8                                       
         MVI   PRPER+L'TGTODAY8,C'-'                                            
         MVC   PRPER+L'TGTODAY8+1(L'TGTODAY8),TGTODAY8                          
         MVC   TIQPSTR,TGTODAY1                                                 
         MVC   TIQPEND,TGTODAY1                                                 
         B     VK40                                                             
                                                                                
VK30     LA    R2,SITPERH          VALIDATE PERIOD                              
         BRAS  RE,VSFTDAT          VALIDATE FOR SOFTDATES FIRST                 
         BE    VK40                                                             
         LA    R3,BLOCK                                                         
         USING PERVALD,R3                                                       
         GOTO1 PDVAL,DMCB,(R3)                                                  
         MVC   PRPER,PVALCPER      SAVE PRINTABLE PERIOD                        
         MVC   TIQPSTR,PVALPSTA                                                 
         MVC   TIQPEND,PVALPEND                                                 
VK40     MVI   TIQDTYPE,TIQDBILL  SET FILTERING ON BILL DATE                    
         DROP  R3                                                               
*                                                                               
*        VALIDATE CLIENT                                                        
*                                                                               
         LA    R2,SITCLIH          VALIDATE CLIENT FIELD                        
         CLI   5(R2),0                                                          
         BE    VK50                SKIP IF NO CLIENT ENTERED                    
*                                                                               
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'08',(R2)),SITCLINH                        
*                                                                               
         MVC   TIFCLI,TGCLI        SET SYSIO FILTER                             
*                                                                               
VK50     BRAS  RE,VOPTS            VALIDATE OPTIONS                             
*                                                                               
         XIT1                                                                   
*                                                                               
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
         BZ    VSFTDAT3                                                         
* CHECK FOR YEAR, MONTH VALIDATION                                              
         MVI   SOFITYPE,SOFITYM    VALIDATE FOR YEAR, MONTH THEN                
         GOTO1 SOFTDATE,SOFDATD                                                 
         BZ    VSFTDAT3                                                         
         MVC   ERROR,SOFERROR                                                   
         J     NO                                                               
*                                                                               
VSFTDAT3 CLI   OFFLINE,C'Y'                                                     
         BE    VSFTDAT5                                                         
         CLI   CONOUTH+4,0         RLP ONLINE, IF GROUP NAME IN OUTPUT          
         BE    VSFTDAT5                                                         
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
*                                                                               
         TITLE 'T70361 - GREY INTERFACE - VOPTS'                                
*---------------------------------------------------------------------          
*              VALIDATE OPTIONS                                                 
*---------------------------------------------------------------------          
VOPTS    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   TAPEOPT,C'N'        INIT OPTIONS TO DEFAULT                      
         MVI   PRNTOPT,C'N'        INIT OPTIONS TO DEFAULT                      
         MVI   TRPWOPT,C'N'                                                     
         MVI   POWOPT,C'Y'                                                      
*                                                                               
         LA    R2,SITOPTH          POINT TO OPTIONS FIELD                       
*                                                                               
         CLI   5(R2),0                                                          
         BE    VOPTX               DONE IF NO ENTRY                             
*                                                                               
         GOTO1 SCANNER,DMCB,(R2),(10,BLOCK),0                                   
         ZIC   R0,4(R1)                                                         
         LA    R4,BLOCK                                                         
         LTR   R0,R0                                                            
         BZ    INVERR                                                           
*                                                                               
VOPT10   DS    0H                                                               
*                                                                               
VOPTTAP  CLC   12(4,R4),=C'TAPE'   TAPE OPTION                                  
         BNE   VOPTTAPN                                                         
*                                                                               
         CLI   ACTEQU,ACTDOWN      TAPE NOT VALID OPTION FOR THIS               
         BE    INVERR                                                           
*                                                                               
         MVC   TAPEOPT,22(R4)                                                   
*                                                                               
         B     VOPTEND                                                          
*                                                                               
VOPTTAPN DS    0H                                                               
*                                                                               
VOPTPRN  CLC   12(5,R4),=C'PRINT'  PRINT OPTION                                 
         BNE   VOPTPRNN                                                         
*                                                                               
         CLI   ACTEQU,ACTDOWN      TAPE NOT VALID OPTION FOR THIS               
         BE    INVERR                                                           
*                                                                               
         MVC   PRNTOPT,22(R4)                                                   
*                                                                               
         B     VOPTEND                                                          
*                                                                               
VOPTPRNN DS    0H                                                               
*                                                                               
VOPTEND  LA    R4,32(R4)                                                        
         BCT   R0,VOPT10                                                        
*                                                                               
VOPTX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
         GOTO1 ERREX                                                            
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T70361 - GREY INTERFACE - SETAGY'                               
*---------------------------------------------------------------------          
*              THIS ROUTINE SETS SVAGYCO AND LENGHTS                            
*---------------------------------------------------------------------          
SETAGY   NTR1  BASE=*,LABEL=*                                                   
         USING TLAYD,R4                                                         
         L     R4,AIO              ESTABLISH AGENCY RECORD                      
         MVC   SVAGYCO,TLAYAGY     SAVE AGENCY GROUP CODE                       
         DROP  R4                                                               
*                                                                               
         CLI   RECNUM,CPOR         CRISPIN PORTER?                              
         BE    SETAGYX                                                          
*                                                                               
         GOTO1 RECVAL,DMCB,TLIFCDQ,(X'A4',SVAGYCO)                              
         BE    *+6                                                              
         DC    H'00'               INTERACE RECORD MUST EXIST                   
*                                                                               
         MVC   WC(WCLNQ),SPACES                                                 
*                                                                               
         USING TAIFD,R4                                                         
         MVI   ELCODE,TAIFELQ                                                   
         BRAS  RE,GETEL            A(INTERFACE ELEMENT)                         
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   SVIFVEND,SPACES                                                  
         MVC   SVIFVEND(L'TAIFVEND),TAIFVEND                                    
                                                                                
         ZIC   R0,TAIFNWCS         R0=N'SUB-ELEMENTS IN EL.                     
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         DC    H'00'                                                            
         LA    R1,TAIFWCS          R1=A(FIELD IN EL.)                           
         DROP  R4                                                               
*                                                                               
SA10     CLI   0(R1),X'85'                                                      
         BNE   SA20                                                             
         MVC   SVWCTNH,1(R1)       SAVE T&H WORK CODE                           
         LA    R1,3(R1)                                                         
         B     SA100                                                            
*                                                                               
SA20     CLI   0(R1),X'83'                                                      
         BNE   SA30                                                             
         MVC   SVWCPNH,1(R1)       SAVE P&H WORK CODE                           
         LA    R1,3(R1)                                                         
         B     SA100                                                            
*                                                                               
SA30     CLI   0(R1),X'81'                                                      
         BNE   SA40                                                             
         MVC   SVWCSES,1(R1)       SAVE SESSION WORK CODE                       
         LA    R1,3(R1)                                                         
         B     SA100                                                            
*                                                                               
SA40     CLI   0(R1),X'0A'                                                      
         BNE   SA50                                                             
         MVC   SVWCRES,2(R1)       SAVE REUSE WORK CODE                         
         LA    R1,4(R1)                                                         
         SHI   R0,1                                                             
         B     SA100                                                            
*                                                                               
SA50     CLI   0(R1),X'0C'                                                      
         BNE   SA60                                                             
         CLI   1(R1),UGRT                                                       
         BNE   SA60                                                             
         MVC   SVWCGRT,3(R1)       SAVE GUARANTEE WORK CODE                     
         LA    R1,5(R1)                                                         
         SHI   R0,1                                                             
         B     SA100                                                            
*                                                                               
SA60     CLI   0(R1),X'88'                                                      
         BNE   SA70                                                             
         MVC   SVWCTAX,1(R1)       SAVE TAX WORK CODE                           
         LA    R1,3(R1)                                                         
         B     SA100                                                            
*                                                                               
SA70     CLI   0(R1),X'87'                                                      
         BNE   SA80                                                             
         MVC   SVWCHND,1(R1)       SAVE HND WORK CODE                           
         LA    R1,3(R1)                                                         
         B     SA100                                                            
*                                                                               
SA80     CLI   0(R1),X'0B'                                                      
         BNE   SA100                                                            
         CLI   1(R1),X'0E'         ZZZ                                          
         BNE   SA100                                                            
         MVC   SVWCCPORZ,3(R1)     SAVE C=ZZZ WORK CODE                         
         LA    R1,5(R1)                                                         
         B     SA100                                                            
*                                                                               
SA100    BCT   R0,SA10                                                          
*                                                                               
         USING TAFND,R4                                                         
         MVI   ELCODE,TAFNELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTT10))                                     
         BNE   SETAGYX                                                          
         MVC   SVIFVEND,SPACES                                                  
         L     R4,TGELEM                                                        
         ZIC   RF,TAFNLEN                                                       
         SHI   RF,4                                                             
         EX    RF,*+8                                                           
         J     *+10                                                             
         MVC   SVIFVEND(0),TAFNNAME                                             
         DROP  R4                                                               
*                                                                               
SETAGYX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T70361 - GREY INTERFACE - PREP'                                 
*---------------------------------------------------------------------          
*              PRINT REPORT                                                     
*---------------------------------------------------------------------          
PREP     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   SORTFRST,C'Y'                                                    
         ZAP   CNTINVS,=P'0'                                                    
         ZAP   CNTDTLS,=P'0'                                                    
         XC    TOTAMT,TOTAMT                                                    
         XC    SVAGYCO,SVAGYCO     INIT AGENCY CODE                             
*                                                                               
         CLI   PRNTOPT,C'Y'        SKIP IF NOT PRINTING                         
         BNE   PREPPRNX                                                         
*                                                                               
         LA    R1,MYSPECS                                                       
         ST    R1,SPECS                                                         
         LA    R1,HOOK             INITIALIZE FOR REPORT                        
         ST    R1,HEADHOOK                                                      
*                                                                               
PREPPRNX DS    0H                                                               
*                                                                               
         CLI   ACTEQU,ACTDOWN      SKIP IF NOT DOWN LOADING                     
         BNE   PREPDWNX                                                         
*                                                                               
         BRAS  RE,INITDWN          INITIALIZE DOWNLOAD                          
*                                                                               
PREPDWNX DS    0H                                                               
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
         CLI   RECNUM,CPOR                                                      
         BE    *+8                                                              
         BRAS  RE,OPENTAPE                                                      
*                                                                               
         BRAS  RE,FILHDR           BUILD FILE HEADER RECORD                     
*                                                                               
         CLI   SORTFRST,C'Y'                                                    
         BE    PREPDONE                                                         
*                                                                               
PREPLOOP GOTO1 VSORTER,DMCB,=C'GET' GET A SORT RECORD                           
*                                                                               
         L     R2,DMCB+4                                                        
         LTR   R2,R2               IF NO MORE SORT RECORDS                      
         BZ    PREPDONE               DONE                                      
*                                                                               
         BRAS  RE,FILDET           PUT OUT DETAIL RECORDS                       
*                                                                               
PREPCONT DS    0H                                                               
         B     PREPLOOP                                                         
*                                                                               
PREPDONE DS    0H                                                               
*                                                                               
*        PUTOUT TRAILER RECORD                                                  
*                                                                               
*        BUILD TRAILER RECORD                                                   
*                                                                               
         LA    R5,TAPEIO                                                        
         USING TAPED,R5            ETABLISH OUTPUT AREA                         
*                                                                               
         MVC   TAPEIO(128),SPACES  INIT IO AREA                                 
         MVC   TAPEIO+128(128),SPACES                                           
         MVC   TAPEIO+256(28),SPACES                                            
*                                                                               
         MVC   TRLTYPE,=C'TLR*'    BATCH HEADER RECORD ID                       
*                                                                               
         LR    R2,R5                                                            
         BRAS  RE,PUTTAPE                                                       
*                                                                               
         CLI   RECNUM,CPOR                                                      
         BE    *+8                                                              
         BRAS  RE,CLOSTAPE                                                      
*                                                                               
PREPX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*--------------------------------------------------------------------           
*              HEADLINES ETC                                                    
*--------------------------------------------------------------------           
HOOK     NTR1  BASE=*,LABEL=*                                                   
         MVC   H1+47(14),=CL24'GREY INTERFACE'                                  
         GOTO1 CENTER,DMCB,H1+47,24                                             
         GOTO1 UNDERLIN,DMCB,(24,H1+52),(X'BF',H2+52)                           
         MVC   H3+52(6),=C'PERIOD'                                              
         LA    R1,SITPERH                                                       
         MVC   H3+59(17),8(R1)                                                  
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*---------------------------------------------------------------------          
*              ROUTINE TO SET UP SECOND REPORT ON QUEUE                         
*---------------------------------------------------------------------          
IOHOOK   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   TIMODE,PROCREC      IF THERE IS SOMETHING INTERESTING            
         BNE   IOHOOKX                                                          
                                                                                
         USING TLIND,R4                                                         
         L     R4,TIAREC                                                        
                                                                                
         LA    R2,AGYFLST                                                       
IOHOOK10 CLI   0(R2),X'FF'                                                      
         BE    IOHOOKX                                                          
         CLC   TLINAGY,0(R2)                                                    
         BE    IOHOOK20                                                         
         LA    R2,L'TLINAGY(R2)                                                 
         B     IOHOOK10                                                         
         DROP  R4                                                               
*                                                                               
*        PROCESS INVOICE DETAILS                                                
*                                                                               
IOHOOK20 BRAS  RE,INVDET           INVOICE DETAILS                              
*                                                                               
IOHOOKX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*                                                                               
*---------------------------------------------------------------------          
*              BUILD SORT RECORD FROM INVOICE                                   
*---------------------------------------------------------------------          
INVDET   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*        CHECK FOR CHANGE IN AGENCY                                             
*                                                                               
         LA    R5,TAPEIO           CLEAR OUTPUT AREA                            
         USING SRTD,R5                                                          
         MVC   TAPEIO(128),SPACES  INIT IO AREA                                 
         MVC   TAPEIO+128(128),SPACES                                           
         MVC   TAPEIO+256(28),SPACES                                            
*                                                                               
         L     R4,TIAREC                                                        
         USING TLIND,R4            ESTABLISH INVOICE RECORD KEY                 
*                                                                               
         CLC   SVAGYCO,TLINAGY     IF AGENCY HAS CHANGED                        
         BE    IVDAGYX                                                          
*                                                                               
*        READ IN AGENCY RECORD                                                  
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'A0',TLINAGY)                              
*                                                                               
         BRAS  RE,SETAGY           FIND CLT/PRD/JOB CDE LENGTHS                 
*                                                                               
         MVC   AIO,AIO1            RESTORE IOAREA                               
         MVC   KEY,TIKEY           RESTORE READ SEQUENCE                        
         GOTO1 HIGH                                                             
*                                                                               
IVDAGYX  DS    0H                                                               
         MVC   SRTWC(WCLNQ),WC     SAVE OFF INTFACE VALUES                      
*                                                                               
         MVI   ELCODE,TABDELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   INVDETX                                                          
*                                                                               
         USING TABDD,R4            ESTABLISH BILL DETAILS ELEMENT               
*                                                                               
         MVC   SRTTOT,TABDTOT      INVOICE TOTAL                                
*                                                                               
         L     RF,TABDTAX                                                       
         A     RF,TABDHND                                                       
         A     RF,TABDHNDC                                                      
         A     RF,TABDGST                                                       
         A     RF,TABDPST                                                       
         ST    RF,SRTTXHND         TAX+HANDLING                                 
*                                                                               
         MVC   SRTTAX,TABDTAX      TAX                                          
         L     RF,TABDHND                                                       
         A     RF,TABDHNDC                                                      
         ST    RF,SRTHND           HANDLING                                     
*                                                                               
         XC    SRTPDZ,SRTPDZ                                                    
         CLC   SVWCCPORZ,SPACES    ZZZ?                                         
         BE    IVDAGY30                                                         
*                                                                               
         MVC   SYSFIL,=CL8'CHKFIL'                                              
         MVC   SYSDIR,=CL8'CHKDIR'                                              
*                                                                               
         L     R4,TIAREC           YES - NEED TO LOOK UP ZZZ CHECKS             
         USING TLIND,R4            ESTABLISH INVOICE RECORD KEY                 
*                                                                               
         LA    RF,KEY                                                           
         USING TLCKD,RF                                                         
         XC    KEY,KEY                                                          
         MVI   TLCKCD,TLCKCDQ                                                   
         MVC   TLCKAGY,TLINAGY                                                  
         MVC   TLCKINV,TLININV                                                  
         XC    TLCKINV,=X'FFFFFFFFFFFF'                                         
*                                                                               
         GOTO1 HIGH                                                             
         B     IVDAGY10                                                         
IVDAGY05 GOTO1 SEQ                                                              
IVDAGY10 CLC   KEY(TLCKSORT-TLCKKEY),KEYSAVE                                    
         BNE   IVDAGY20                                                         
         LA    RF,KEY                                                           
         CLC   TLCKCAT,=C'ZZZ'                                                  
         BNE   IVDAGY05                                                         
         DROP  RF                                                               
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
*                                                                               
         CLI   RECNUM,CPOR                                                      
         BNE   IVDAGY15                                                         
*                                                                               
         USING TAPUD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAPUELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   SRTPO,TAPUINUM      PURCHASE ORDER NUMBER                        
*                                                                               
         USING TAPDD,R4                                                         
IVDAGY15 L     R4,AIO                                                           
         MVI   ELCODE,TAPDELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   IVDAGY05                                                         
         L     RF,SRTPDZ           ACCUMULATED PAID AMOUNTS FOR ZZZ             
         A     RF,TAPDGRS                                                       
         ST    RF,SRTPDZ                                                        
         B     IVDAGY05                                                         
*                                                                               
IVDAGY20 MVC   SYSFIL,=CL8'TALFIL'                                              
         MVC   SYSDIR,=CL8'TALDIR'                                              
         MVC   AIO,AIO1            RESTORE IOAREA                               
         MVC   KEY,TIKEY           RESTORE READ SEQUENCE                        
         GOTO1 HIGH                                                             
         L     R4,AIO                                                           
*                                                                               
*        GET CLT/PRD/JOB CODES                                                  
*                                                                               
IVDAGY30 MVC   SVCLT,SPACES                                                     
         MVC   SVPRD,SPACES                                                     
         MVC   SVPRJ,SPACES                                                     
*                                                                               
         L     R4,TIAREC           GET ESTIMATE NUMBER                          
*                                                                               
         MVI   ELCODE,TAPDELQ      FIND PAYMENT DETAILS                         
         BRAS  RE,GETEL                                                         
         BNE   IVDPAYX                                                          
*                                                                               
         USING TAPDD,R4            ESTABLISH PAYMENT DETAILS                    
*                                                                               
         MVC   SRTUSE,TAPDUSE      SAVE USE                                     
         MVC   SRTTYPE,TAPDTYPE    SAVE PAYMENT TYPE                            
*                                                                               
         L     RF,TAPDPNH                                                       
         A     RF,TAPDHNW                                                       
         A     RF,TAPDINR                                                       
         ST    RF,SRTPNH           PENSION+HEALTH                               
                                                                                
IVDPAYX  DS    0H                                                               
*                                                                               
         L     R4,TIAREC           GET ESTIMATE NUMBER                          
*                                                                               
         MVI   ELCODE,TANUELQ                                                   
         BRAS  RE,GETEL                                                         
         B     IVD7                                                             
*                                                                               
IVD5     BRAS  RE,NEXTEL                                                        
IVD7     BNE   INVDETX                                                          
*                                                                               
         USING TANUD,R4                                                         
*                                                                               
         CLI   TANUTYPE,TANUTEST   MUST BE ESTIMATE ELEMENT                     
         BNE   IVD5                                                             
*                                                                               
         MVC   WORK,SPACES                                                      
         SR    RF,RF                                                            
*                                                                               
         ZIC   RF,TANULEN                                                       
         SHI   RF,TANULNQ+1                                                     
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SVCLT(0),TANUMBER                                                
*                                                                               
         MVC   SRTEST,SVCLT                                                     
*                                                                               
         MVC   SRTAGY,SVAGYCO      SET AGENCY                                   
         MVC   SRTCLT,SVCLT        SET CLIENT                                   
         MVC   SRTPRD,SVPRD        SET PRODUCT                                  
         MVC   SRTJOB,SVPRJ        SET ESTIMATE                                 
*                                                                               
         L     R4,TIAREC           INVOICE NUMBER                               
         USING TLIND,R4                                                         
*                                                                               
         MVC   WORK(6),TLININV     COMPLEMENTED INVOICE NUMBER                  
         XC    WORK(6),=6X'FF'     UNCOMPLEMENT                                 
         GOTO1 TINVCON,DMCB,WORK,SVINVNM,DATCON                                 
         MVC   SRTINVNM(6),SVINVNM PRINTABLE INVOICE NUMBER                     
*                                                                               
         CLI   RECNUM,CPOR                                                      
         JNE   IVD10                                                            
*                                                                               
         USING TANUD,R4                                                         
         MVC   AIO,TIAREC                                                       
         MVI   ELCODE,TANUELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TANUTAUT))                                     
         BNE   IVD10                                                            
         L     R4,TGELEM                                                        
         ZIC   R1,TANULEN                                                       
         SHI   R1,4                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SRTPO(0),TANUMBER   PO                                           
*                                                                               
IVD10    L     R4,TIAREC           INVOICE DATE                                 
         MVI   ELCODE,TAINELQ                                                   
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING TAIND,R4                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(1,TAINBDTE),(0,SRTINVDT)                            
*                                                                               
         BRAS  RE,SORTPUT                                                       
*                                                                               
INVDETX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*--------------------------------------------------------------------           
*              SPACE OUT TAPEIO AREA                                            
*--------------------------------------------------------------------           
CLEARTAP NTR1  BASE=*,LABEL=*                                                   
         LA    R2,TAPEIO                                                        
         MVI   0(R2),C' '                                                       
         MVC   1(255,R2),0(R2)                                                  
         AHI   R2,256                                                           
         MVI   0(R2),C' '                                                       
         MVC   1(27,R2),0(R2)                                                   
         AHI   R2,28                                                            
         MVC   0(57,R2),TAPEIO                                                  
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*---------------------------------------------------------------------          
*              BUILD FILE HEADER                                                
*---------------------------------------------------------------------          
FILHDR   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R5,TAPEIO                                                        
         USING TAPED,R5            ETABLISH OUTPUT AREA                         
*                                                                               
         MVC   TAPEIO(128),SPACES  INIT IO AREA                                 
         MVC   TAPEIO+128(128),SPACES                                           
         MVC   TAPEIO+256(28),SPACES                                            
*                                                                               
         CLI   RECNUM,CPOR         CRISPIN PORTER?                              
         BE    FILHDRX                                                          
*                                                                               
*        BUILD HEADER RECORD                                                    
*                                                                               
         MVC   HDRTYPE,=C'HDR*'    HEADER RECORD ID                             
         MVC   HDRSCRID,=CL8'ACINP10'  SCRIPT ID                                
         MVC   HDRSCRCD,=CL8'002'      SCRIPT SODE (VERSION #)                  
         MVI   HDRLANG,C'E'        LANGUAGE                                     
*                                                                               
         LR    R2,R5                                                            
         BRAS  RE,PUTTAPE                                                       
*                                                                               
*        BUILD BATCH HEADER RECORD                                              
*                                                                               
         MVC   BHDTYPE,=C'ITM1'    BATCH HEADER RECORD ID                       
         MVC   BHDREF,=C'IN01'     REFERENCE NUMBER                             
*                                                                               
         MVC   BHDNAME,=CL15'INVOICES - MMM/YY' BATCH NAME                      
         GOTOR DATCON,DMCB,(1,TIQPSTR),(6,BHDNAME+11)                           
         MVC   BHDNAME+14(3),=CL3' '  CLEAR YEAR                                
*                                                                               
         MVC   BHDINTYP,=CL3'10'   INPUT TYPE                                   
*                                                                               
         MVC   FULL,TIQPSTR        COPY INVOICE DATE                            
         MVI   FULL+2,0            KILL DAY OF MONTH                            
         GOTOR DATCON,DMCB,(1,FULL),(6,BHDMONTH) MON OF ACTVY                   
*                                                                               
         MVI   BHDEND,C'U'         SET FOR UPDATE                               
*                                                                               
         LR    R2,R5                                                            
         BRAS  RE,PUTTAPE                                                       
*                                                                               
FILHDRX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R5                                                               
*                                                                               
         EJECT                                                                  
*--------------------------------------------------------------------           
* SORT UTILITIES                                                                
*--------------------------------------------------------------------           
SORTPUT  NTR1  BASE=*,LABEL=*                                                   
         LA    R5,TAPEIO                                                        
         USING TAPED,R5                                                         
         CLI   SORTFRST,C'Y'                                                    
         BNE   SORTPUT2                                                         
         GOTO1 VSORTER,DMCB,SORTCARD,RECCARD                                    
         MVI   SORTFRST,C'N'                                                    
*                                                                               
SORTPUT2 GOTO1 VSORTER,DMCB,=C'PUT',TAPEIO                                      
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*---------------------------------------------------------------------          
*              PUT RECORD TO TAPE                                               
*---------------------------------------------------------------------          
PUTTAPE  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*        PUT OUT TAPE                                                           
*                                                                               
         CLI   RECNUM,CPOR         CRISPIN PORTER?                              
         BE    PUTCPOR                                                          
         CLI   TAPEOPT,C'Y'        IF PRODUCING TAPE                            
         BNE   PUTTAPEN                                                         
         L     R1,=A(GRYTAPE)                                                   
         PUT   (1),(R2)                                                         
         B     PUTTAPEN                                                         
*                                                                               
PUTCPOR  DS    0H                                                               
         USING TAPED,R5                                                         
         GOTO1 OUTPDOWN,DMCB,(C'T',C50COMP),L'C50COMP                           
         GOTO1 OUTPDOWN,DMCB,(C'T',C50OFF),L'C50OFF                             
         GOTO1 OUTPDOWN,DMCB,(C'T',C50VEND),L'C50VEND                           
         GOTO1 OUTPDOWN,DMCB,(C'T',C50INV),L'C50INV                             
         GOTO1 OUTPDOWN,DMCB,(C'T',C50INVD),L'C50INVD                           
         GOTO1 OUTPDOWN,DMCB,(C'T',C50PO),L'C50PO                               
         GOTO1 OUTPDOWN,DMCB,(C'N',C50COST),L'C50COST                           
         GOTO1 OUTPDOWN,DMCB,(C'T',C50CLI),L'C50CLI                             
         GOTO1 OUTPDOWN,DMCB,(C'T',C50PRD),L'C50PRD                             
         GOTO1 OUTPDOWN,DMCB,(C'T',C50BRAND),L'C50BRAND                         
         GOTO1 OUTPDOWN,DMCB,(C'T',C50JOB),L'C50JOB                             
         GOTO1 OUTPDOWN,DMCB,(C'T',C50USETX),L'C50USETX                         
         GOTO1 OUTPDOWN,DMCB,(C'T',C50WORK),L'C50WORK                           
         GOTO1 OUTPDOWN,DMCB,(C'T',C50TRAN),L'C50TRAN                           
         GOTO1 OUTPDOWN,DMCB,(C'T',C50EMP),L'C50EMP                             
         GOTO1 OUTPDOWN,DMCB,(C'T',C50DESC),L'C50DESC                           
         GOTO1 OUTPDOWN,DMCB,(C'T',C50AP),L'C50AP                               
         GOTO1 OUTPDOWN,DMCB,(C'T',C50IRD),L'C50IRD                             
         GOTO1 OUTPDOWN,DMCB,(C'T',C50EXCH),L'C50EXCH                           
         GOTO1 OUTPDOWN,DMCB,(C'T',C50FEXCH),L'C50FEXCH                         
         GOTO1 OUTPDOWN,DMCB,(C'T',C50MWO),L'C50MWO                             
         GOTO1 OUTPDOWN,DMCB,(C'T',C50WOD),L'C50WOD                             
         GOTO1 OUTPDOWN,DMCB,(C'T',C50WOA),L'C50WOA                             
         BAS   RE,EOLDOWN                                                       
         J     XIT                                                              
*                                                                               
*                                                                               
PUTTAPEN DS    0H                                                               
*                                                                               
*        PUT OUT REPORT                                                         
*                                                                               
         MVC   P,0(R5)             MOVE TO PRINT AREA                           
         MVC   P2(100),132(R5)                                                  
*                                                                               
         CLI   PRNTOPT,C'Y'        IF PRINTING REPORT                          
         BNE   PUTPRNTX                                                         
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)     PRINT DATA                                   
*                                                                               
PUTPRNTX DS    0H                                                               
*                                                                               
*        DOWNLOAD REPORT                                                        
*                                                                               
         CLI   ACTEQU,ACTDOWN      IF DOWNLOADING                               
         BNE   PUTDWNX                                                          
*                                                                               
         LA    R3,DLBLOCK           INITIALIZE DLFLD                            
         USING DLCBD,R3                                                         
*                                                                               
         LR    R5,R2               COPY DATA POINTER                            
*                                                                               
         LA    RF,RECTBL           POINT TO RECORD TABLE                        
*                                                                               
         CLI   0(RF),X'FF'         PROBLEMS IF END OF TABLE                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(4,R2),0(RF)       FIND RECORD IF IN THE TABLE                  
         BE    *+12                                                             
         LA    RF,5(RF)            NEXT TABLE ENTRY                             
         B     *-24                                                             
*                                                                               
         LLC   R0,4(RF)            SAVE RECORD LENGTH                           
*                                                                               
PUTDWNLP DS    0H                                                               
*                                                                               
         MVI   DLCBACT,DLCBPUT     PUT ITEM TO PRINT LINE                       
         MVI   DLCBTYP,DLCBTXT     DATA TYPE IS TEXT                            
*                                                                               
         LA    RF,40               MOVE MAX 40 BYTES AT A TIME                  
         CR    R0,RF               IF FEWER BYTES LEFT                          
         BH    *+6                                                              
         LR    RF,R0               DEFAULT TO RENAINDER                         
*                                                                               
         STC   RF,DLCBLEN          SET NUMBER OF BYTES TO MOVE                  
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   DLCBFLD(0),0(R5)    PASS DATA                                    
*                                                                               
         LA    R5,1(RF,R5)         BUMP TO NEXT TAPE FIELD                      
         SR    R0,RF               DECREMENT LENGTH TO MOVE                     
*                                                                               
         GOTO1 =V(DLFLD),DLCBD     'DOWN' LOAD DATA                             
*                                                                               
PUTDWNCN DS    0H                                                               
*                                                                               
         BCT   R0,PUTDWNLP         GET NEXT FIELD                               
*                                                                               
PUTDWNDN DS    0H                                                               
*                                                                               
         MVI   DLCBACT,DLCBEOL     END OF LINE                                  
         GOTO1 =V(DLFLD),DLCBD                                                  
*                                                                               
PUTDWNX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
*        TABLE OF RECORD IDS AND LENGTHS                                        
*                                                                               
RECTBL   DS    0D                                                               
         DC    C'HDR*',AL1(HDRRECLQ)                                            
RECTBLLQ EQU   *-RECTBL            LENGTH OF TABLE ENTRY                        
         DC    C'ITM1',AL1(BHDRECLQ)                                            
         DC    C'ITM2',AL1(IVHRECLQ)                                            
         DC    C'ITM3',AL1(IVDRECLQ)                                            
         DC    C'5001',AL1(C50RECLQ)                                            
         DC    C'TLR*',AL1(TRLRECLQ)                                            
         DC    X'FF'               END OF TABLE                                 
*                                                                               
***********************************************************************         
*        OUTPUT DOWNLOAD ENTRY                                        *         
*        ON ENTRY ... P1, B0    = TYPE TO PASS                        *         
*                         B1-B3 = ADDRESS OF DATA                     *         
*                     P2        = LENGTH                              *         
***********************************************************************         
                                                                                
OUTPDOWN NTR1                                                                   
         LA    R3,DLBLOCK           INITIALIZE DLFLD                            
         USING DLCBD,R3                                                         
         MVI   DLCBACT,DLCBPUT                                                  
         MVC   DLCBTYP,0(R1)                                                    
         OI    DLCBFLG1,DLCBFXFL                                                
*                                                                               
         L     RF,0(R1)            RF=A(DOWNLOAD FIELD)                         
         L     RE,4(R1)            RE=L'DOWNLOAD FIELD                          
*                                                                               
         LR    R1,RF                                                            
         AR    R1,RE                                                            
OPD10    SHI   R1,1                                                             
         CLI   0(R1),C' '                                                       
         JNE   OPD20                                                            
         BCT   RE,OPD10                                                         
         LHI   RE,1                                                             
OPD20    STC   RE,DLCBLEN                                                       
*                                                                               
         BCTR  RE,0                                                             
                                                                                
         LA    R1,DLCBFLD                                                       
         CLI   DLCBTYP,DLCBNUM     DATA TYPE IS NUMERIC                         
         JE    *+8                                                              
         LA    R1,DLCBFLX                                                       
                                                                                
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   0(0,R1),0(RF)                                                    
                                                                                
OPD50    GOTO1 =V(DLFLD),DLCBD                                                  
         J     XIT                                                              
                                                                                
EOLDOWN  NTR1                                                                   
         LA    R3,DLBLOCK           INITIALIZE DLFLD                            
         USING DLCBD,R3                                                         
         MVI   DLCBACT,DLCBEOL      END OF LINE                                 
         GOTO1 =V(DLFLD),DLCBD      (DON'T USE RF, BASR RE,RF BELOW)            
         J     XIT                                                              
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------          
*              CLOSE TAPE                                                       
*---------------------------------------------------------------------          
CLOSTAPE NTR1  BASE=*,LABEL=*                                                   
         CLI   RECNUM,CPOR         CRISPIN PORTER?                              
         BE    CLSTAPEX                                                         
*                                                                               
         CLI   PRNTOPT,C'Y'        IF PRINT ING REPORT                          
         BNE   CLSPRNTX                                                         
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R3,P                                                             
         USING PRINTD,R3                                                        
*                                                                               
CLSPRNTX DS    0H                                                               
*                                                                               
         CLI   TAPEOPT,C'Y'        IF CREATING TAPE                             
         BNE   CLSTAPEX                                                         
*                                                                               
         L     R2,=A(GRYTAPE)                                                   
         CLOSE ((2))                                                            
*                                                                               
CLSTAPEX DS    0H                                                               
*                                                                               
CLOSETPX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R3                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
*              USER SUPPLIED PRINT ROUTINE - DOWNLOAD                           
*                                                                               
SPLATDWN NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   LINE,1              PREVENT PAGE BREAK                           
*                                                                               
SPLATDWX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*              ODD ROUTINES                                                     
SPLAT    NTR1  BASE=*,LABEL=*                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
SPLATX   DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*                                                                               
         DS    0F                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,38,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=(256)'                                 
SORTCAR2 DC    CL80'SORT FIELDS=(1,10,A),FORMAT=BI,WORK=1'                      
RECCARD2 DC    CL80'RECORD TYPE=F,LENGTH=(20)'                                  
*                                                                               
MYSPECS  SSPEC H1,1,RUN                                                         
         SSPEC H3,1,REQUESTOR                                                   
         SSPEC H1,99,AGYNAME                                                    
         SSPEC H2,99,AGYADD                                                     
         SSPEC H3,99,REPORT                                                     
         SSPEC H3,112,PAGE                                                      
         SSPEC H5,2,C'AGENCY INVOICE INV DATE CLIENT PRD    EST    W'           
         SSPEC H6,2,C'------ ------- -------- ------ ------ ------ -'           
         SSPEC H5,48,C'RK CD     HANDLING     INVOICE AMOUNT'                   
         SSPEC H6,48,C'-----     --------     --------------'                   
         DC    H'0'                                                             
*                                                                               
APERH    DS    A                                                                
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*              LTORG ETC                                                        
         SPACE 3                                                                
*                                  COUNTS                                       
*                                                                               
*              OTHER AREAS NOT DIRECTLY ADDRESSABLE                             
*                                                                               
         ENTRY GRYTAPE                                                          
*                                                                               
*  UNITAPE DEFINED AS TAPE FOR UT REPORT.                                       
*  UNITAPE DEFINED AS DISK FOR UD REPORT.                                       
*                                                                               
GRYTAPE  DCB   DDNAME=GRYTAPE,DSORG=PS,MACRF=(PM),                     X        
               RECFM=FB,LRECL=284,BUFNO=2,BLKSIZE=2840                          
         EJECT                                                                  
*                                                                               
*--------------------------------------------------------------------           
*                                                                               
*        PUT OUT INVOICE DETAIL RECORDS                                         
*                                                                               
*NTRY    R2 ==> SORT RECORD                                                     
*                                                                               
*--------------------------------------------------------------------           
*                                                                               
FILDET   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SRTD,R2             ESTABLISH SORT RECORD                        
*                                                                               
         LA    R5,TAPEIO                                                        
         USING TAPED,R5            ETABLISH OUTPUT AREA                         
*                                                                               
         MVC   TAPEIO(128),SPACES  INIT IO AREA                                 
         MVC   TAPEIO+128(128),SPACES                                           
         MVC   TAPEIO+256(28),SPACES                                            
*                                                                               
         CLI   RECNUM,CPOR         CRISPIN PORTER?                              
         BNE   FDT001                                                           
*                                                                               
         MVC   C50COMP,=C'50'                                                   
         MVC   C50OFF,=C'01'                                                    
         MVC   C50VEND(8),=C'TALPARCP'                                          
*                                                                               
         MVC   C50INV(L'SRTAGY),SRTAGY AGENCY                                   
         LA    RF,C50INV                                                        
         LHI   R0,L'SRTAGY                                                      
FDT000   CLI   0(RF),C' '                                                       
         BE    *+12                                                             
         AHI   RF,1                                                             
         BCT   R0,FDT000                                                        
         AHI   RF,1                                                             
         MVC   0(L'SRTINVNM,RF),SRTINVNM                                        
*                                                                               
*        MVC   C50INV(L'SRTINVNM),SRTINVNM   INVOICE NUMBER                     
         GOTO1 DATCON,DMCB,(0,SRTINVDT),(20,C50INVD)                            
         MVC   C50PO,SRTPO         PURCHASE ORDER NUMBER                        
         EDIT  SRTTOT,(11,C50COST),2,FLOAT=-,ALIGN=RIGHT COST                   
*        MVC   C50CLI(L'SRTCLT),SRTCLT       CLIENT                             
*        MVC   C50PRD(L'SRTPRD),SRTPRD       PRODUCT                            
         MVC   C50WORK,=C'TALE'                                                 
         MVC   C50TRAN,=C'VND'                                                  
         MVC   C50JOB,SRTEST                                                    
*                                                                               
         LR    R0,R2               SAVE R2                                      
         LR    R2,R5                                                            
         BRAS  RE,PUTTAPE                                                       
         LR    R2,R0               RESTORE R2                                   
         B     FILDETX                                                          
*                                                                               
*        BUILD INVOICE HEADER RECORD                                            
*                                                                               
FDT001   MVC   IVHTYPE,=C'ITM2'    INVOICE HEADER RECORD ID                     
         MVI   IVHTYPID,C'H'       INVOICE RECORD TYPE - H                      
         MVC   IVHINVNM(L'SRTINVNM),SRTINVNM   INVOICE NUMBER                   
*                                  INVOICE DATE                                 
         GOTO1 DATCON,DMCB,(0,SRTINVDT),(5,IVHINVDT)                            
         MVC   IVHVNDPR,SVIFVEND   PRODUCTION VENDOR                            
*                                                                               
         LR    R0,R2               SAVE R2                                      
         LR    R2,R5                                                            
         BRAS  RE,PUTTAPE                                                       
         LR    R2,R0               RESTORE R2                                   
*                                                                               
*        BUILD INVOICE DETAIL RECORD                                            
*                                                                               
         MVC   TAPEIO(128),SPACES  INIT IO AREA                                 
         MVC   TAPEIO+128(128),SPACES                                           
         MVC   TAPEIO+256(28),SPACES                                            
*                                                                               
         MVC   IVDTYPE,=C'ITM3'    BATCH HEADER RECORD ID                       
         MVI   IVDTYPID,C'D'       INVOICE RECORD TYPE - D                      
*                                                                               
         MVC   IVDCLT,SRTCLT       CLIENT                                       
         MVC   IVDPRD,SRTPRD       PRODUCT                                      
         MVC   IVDEST,SRTJOB       ESTIMATE                                     
*                                                                               
*        DETERMINE WORK CODE AND NUMBER OF DETAILS                              
*                                                                               
         CLC   SRTWCTAX,SPACES     IF TAX, THEN PRINT OUT INDIVIDUALLY          
         BNE   FDT10                                                            
         OC    SRTTXHND,SRTTXHND   SKIP IF NO EXTRA COSTS                       
         BZ    FDT10                                                            
*                                                                               
         MVC   IVDWRKCD,SRTWCTNH   SET WORK CODE                                
*                                                                               
         EDIT  (4,SRTTXHND),(12,IVDAMT),2,FLOAT=-                               
*                                                                               
*        PRINT DETAIL LINE                                                      
*                                                                               
         LA    R3,P                                                             
         USING PRINTD,R3                                                        
*                                                                               
         MVC   PAGY,SRTAGY         AGENCY                                       
         MVC   PINVOICE(6),SRTINVNM   INVOICE NUMBER                            
         GOTO1 DATCON,DMCB,(0,SRTINVDT),(5,PINVDATE) INVOICE DATE               
         MVC   PCLIENT(3),SRTCLT   CLIENT CODE                                  
         MVC   PPRD(3),SRTPRD      PRODUCT CODE                                 
         MVC   PEST(3),SRTJOB      ESTIMATE CODE                                
*                                                                               
         MVC   PWRKCD(4),SRTWCTNH WORK CODE                                     
         MVC   PPNH,IVDAMT         HANDLING ETC                                 
*                                                                               
******   GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         LR    R0,R2               SAVE R2                                      
         LR    R2,R5                                                            
         BRAS  RE,PUTTAPE                                                       
         LR    R2,R0               RESTORE R2                                   
*                                                                               
FDT10    CLC   SRTWCPNH,SPACES     SKIP IF NO P&H                               
         BE    FDT20                                                            
*                                                                               
         MVC   IVDWRKCD,SRTWCPNH   SET WORK CODE                                
*                                                                               
         EDIT  (4,SRTPNH),(12,IVDAMT),2,FLOAT=-  P&H                            
*                                                                               
*        PRINT DETAIL LINE                                                      
*                                                                               
         LA    R3,P                                                             
         USING PRINTD,R3                                                        
*                                                                               
         MVC   PAGY,SRTAGY         AGENCY                                       
         MVC   PINVOICE(6),SRTINVNM   INVOICE NUMBER                            
         GOTO1 DATCON,DMCB,(0,SRTINVDT),(5,PINVDATE) INVOICE DATE               
         MVC   PCLIENT(3),SRTCLT   CLIENT CODE                                  
         MVC   PPRD(3),SRTPRD      PRODUCT CODE                                 
         MVC   PEST(3),SRTJOB      ESTIMATE CODE                                
*                                                                               
         MVC   PWRKCD(4),SRTWCPNH WORK CODE                                     
         MVC   PPNH,IVDAMT         HANDLING ETC                                 
*                                                                               
******   GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         LR    R0,R2               SAVE R2                                      
         LR    R2,R5                                                            
         BRAS  RE,PUTTAPE                                                       
         LR    R2,R0               RESTORE R2                                   
*                                                                               
FDT20    CLC   SRTWCTAX,SPACES                                                  
         BE    FDT30                                                            
*                                                                               
         MVC   IVDWRKCD,SRTWCTAX   SET WORK CODE                                
*                                                                               
         EDIT  (4,SRTTAX),(12,IVDAMT),2,FLOAT=-                                 
*                                                                               
*        PRINT DETAIL LINE                                                      
*                                                                               
         LA    R3,P                                                             
         USING PRINTD,R3                                                        
*                                                                               
         MVC   PAGY,SRTAGY         AGENCY                                       
         MVC   PINVOICE(6),SRTINVNM   INVOICE NUMBER                            
         GOTO1 DATCON,DMCB,(0,SRTINVDT),(5,PINVDATE) INVOICE DATE               
         MVC   PCLIENT(3),SRTCLT   CLIENT CODE                                  
         MVC   PPRD(3),SRTPRD      PRODUCT CODE                                 
         MVC   PEST(3),SRTJOB      ESTIMATE CODE                                
*                                                                               
         MVC   PWRKCD(4),SRTWCTAX  WORK CODE                                    
         MVC   PPNH,IVDAMT         HANDLING ETC                                 
*                                                                               
******   GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         LR    R0,R2               SAVE R2                                      
         LR    R2,R5                                                            
         BRAS  RE,PUTTAPE                                                       
         LR    R2,R0               RESTORE R2                                   
*                                                                               
FDT30    CLC   SRTWCHND,SPACES     SKIP IF NO HANDLING                          
         BE    FDT40                                                            
*                                                                               
         MVC   IVDWRKCD,SRTWCHND   SET WORK CODE                                
*                                                                               
         EDIT  (4,SRTHND),(12,IVDAMT),2,FLOAT=-                                 
*                                                                               
*        PRINT DETAIL LINE                                                      
*                                                                               
         LA    R3,P                                                             
         USING PRINTD,R3                                                        
*                                                                               
         MVC   PAGY,SRTAGY         AGENCY                                       
         MVC   PINVOICE(6),SRTINVNM   INVOICE NUMBER                            
         GOTO1 DATCON,DMCB,(0,SRTINVDT),(5,PINVDATE) INVOICE DATE               
         MVC   PCLIENT(3),SRTCLT   CLIENT CODE                                  
         MVC   PPRD(3),SRTPRD      PRODUCT CODE                                 
         MVC   PEST(3),SRTJOB      ESTIMATE CODE                                
*                                                                               
         MVC   PWRKCD(4),SRTWCHND  WORK CODE                                    
         MVC   PPNH,IVDAMT         HANDLING ETC                                 
*                                                                               
******   GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         LR    R0,R2               SAVE R2                                      
         LR    R2,R5                                                            
         BRAS  RE,PUTTAPE                                                       
         LR    R2,R0               RESTORE R2                                   
*                                                                               
FDT40    CLC   SRTWCCPORZ,SPACES   SKIP IF NO ZZZ CAST                          
         BE    FDTB1X                                                           
*                                                                               
         MVC   IVDWRKCD,SRTWCCPORZ SET WORK CODE                                
*                                                                               
         EDIT  (4,SRTPDZ),(12,IVDAMT),2,FLOAT=-                                 
*                                                                               
*        PRINT DETAIL LINE                                                      
*                                                                               
         LA    R3,P                                                             
         USING PRINTD,R3                                                        
*                                                                               
         MVC   PAGY,SRTAGY         AGENCY                                       
         MVC   PINVOICE(6),SRTINVNM   INVOICE NUMBER                            
         GOTO1 DATCON,DMCB,(0,SRTINVDT),(5,PINVDATE) INVOICE DATE               
         MVC   PCLIENT(3),SRTCLT   CLIENT CODE                                  
         MVC   PPRD(3),SRTPRD      PRODUCT CODE                                 
         MVC   PEST(3),SRTJOB      ESTIMATE CODE                                
*                                                                               
         MVC   PWRKCD(4),SRTWCCPORZ WORK CODE                                   
         MVC   PPNH,IVDAMT         HANDLING ETC                                 
*                                                                               
******   GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         LR    R0,R2               SAVE R2                                      
         LR    R2,R5                                                            
         BRAS  RE,PUTTAPE                                                       
         LR    R2,R0               RESTORE R2                                   
*                                                                               
FDTB1X   DS    0H                                                               
*                                                                               
         CLC   SRTUSE,=C'GRT'      IF USE=GRT                                   
         BE    FDTB2X                                                           
         CLC   SRTUSE,=C'GRR'      OR GRR                                       
         BNE   FDTB3X                                                           
FDTB2X   MVC   IVDWRKCD,SRTWCGRT      SET SRT WORK CODE                         
         B     FTDWKCDX                                                         
*                                                                               
*        EXPAND USE                                                             
*                                                                               
FDTB3X   GOTO1 USEVAL,DMCB,SRTUSE,SRTTYPE                                       
*                                                                               
         TM    TGUSSTAT,SESSION    IF SESSION USE                               
         BNO   *+14                                                             
         MVC   IVDWRKCD,SRTWCSES      SET WORKCODE                              
         B     FTDWKCDX                                                         
*                                                                               
         MVC   IVDWRKCD,SRTWCRES   ELSE SET WORK CODE                           
*                                                                               
FTDWKCDX DS    0H                                                               
*                                                                               
         ICM   RF,15,SRTTOT        GET INVOICE TOTAL                            
         S     RF,SRTTXHND         SUBTRACT AMOUNT ALREADY SENT                 
         S     RF,SRTPNH                                                        
         S     RF,SRTPDZ                                                        
*                                                                               
         EDIT  (RF),(12,IVDAMT),2,FLOAT=-  INVOICE AMOUNT                       
*                                                                               
*        PRINT DETAIL LINE OF REPORT                                            
*                                                                               
         B     FTDPRNTX                                                         
*                                                                               
         L     RF,TOTAMT           ACCUMULATE INVOICE TOTALS                    
         A     RF,SVINVTOT                                                      
         ST    RF,TOTAMT                                                        
*                                                                               
         LA    R3,P                                                             
         USING PRINTD,R3                                                        
*                                                                               
         MVC   PAGY,SRTAGY         AGENCY                                       
         MVC   PINVOICE(6),SRTINVNM   INVOICE NUMBER                            
         GOTO1 DATCON,DMCB,(0,SRTINVDT),(5,PINVDATE) INVOICE DATE               
         MVC   PCLIENT(3),SRTCLT   CLIENT CODE                                  
         MVC   PPRD(3),SRTPRD      PRODUCT CODE                                 
         MVC   PEST(3),SRTJOB      ESTIMATE CODE                                
*                                                                               
         MVC   PWRKCD(4),IVDWRKCD  WORK CODE                                    
         MVC   PINVAMT,IVDAMT      INVOICE AMOUNT                               
*                                                                               
******   GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
FTDPRNTX DS    0H                                                               
*                                                                               
         LR    R0,R2               SAVE R2                                      
         LR    R2,R5                                                            
         BRAS  RE,PUTTAPE                                                       
         LR    R2,R0               RESTORE R2                                   
*                                                                               
FILDETX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R3                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
*--------------------------------------------------------------------           
*              TAPE ROUTINES                                                    
*--------------------------------------------------------------------           
OPENTAPE NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   RECNUM,CPOR         CRISPIN PORTER?                              
         BE    OPENXIT                                                          
         CLI   TAPEOPT,C'Y'        SKIP IF NOT PRODUCING TAPE                   
         BNE   OPENXIT                                                          
*                                                                               
         L     R2,=A(GRYTAPE)                                                   
         OPEN  ((2),OUTPUT)                                                     
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LINKX MF=(E,PARMLST),SF=(E,LINKLST)                                    
*                                                                               
OPENXIT  XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*                                                                               
*              CALL DLFLD TO INITIALISE REPORT                                  
*                                                                               
INITDWN  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R3,DLBLOCK           INITIALIZE DLFLD                            
         USING DLCBD,R3                                                         
*                                                                               
         XC    DLCBD(DLCBXLX),DLCBD                                             
*                                                                               
         MVI   DLCBACT,DLCBINIT    INITIALIZE FOR DOWNLOAD                      
         LAY   R1,SPLATDWN         A(HOOK ROUTINE FOR PRINTING)                 
         ST    R1,DLCBAPR                                                       
         LA    R1,P                A(PRINT LINE)                                
         ST    R1,DLCBAPL                                                       
         MVC   DLCBAED,EDITOR                                                   
         MVC   DLCXMAXL,=Y(L'P)    MAXIMUM LENGTH OF PRINT LINE                 
         MVI   DLCXDELC,C' '       DELIMITER                                    
         CLI   RECNUM,CPOR         CRISPIN PORTER?                              
         JNE   *+8                                                              
         MVI   DLCXDELC,C' '       DELIMITER                                    
         MVI   DLCXEOTC,C'"'       TEXT DELIMITER                               
         MVI   DLCXEOTA,C''''      TEXT DELIMITER ALTERNATE                     
         MVI   DLCXEOLC,X'5E'      SEMI-COLON FOR END OF LINE                   
         MVI   DLCXEORC,C':'       END OF REPORT                                
*                                                                               
         GOTO1 =V(DLFLD),DLCBD                                                  
*                                                                               
INITDWNX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R3                                                               
*                                                                               
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
         GOTO1 DATCON,DMCB,(1,TGTODAY1),(5,FILE3)     TODAY'S DATE              
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
EDICT1   DC    C'EDICTKEY=TALJWT'                                               
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
DSNME    DC    CL8'GRYTAPE'                                                     
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
PRNTOPT  DS    CL1                 Y=GENERATE PRINTED REPORT                    
SORTOPT  DS    CL1                                                              
LISTOPT  DS    CL1                 Y=LIST AND COUNT ONLY                        
BILLOPT  DS    CL1                 BILLING ONLY                                 
SEQOPT   DS    CL1                 SEQUENCE - DEFAULT IS UNION                  
PAYPSTAT DS    CL1                                                              
SESSOPT  DS    CL1                 Y=SESSION USES ONLY                          
RETROOPT DS    CL1                 Y=GET ONLY RETROACTIVE PAYMENTS              
*                                  A=ALSO GET RETROACTIVE PAYMENTS              
NOSRTOPT DS    CL1                 Y=SUPPRESS DETAIL ON TAPE (TYPS 2&3)         
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
SVAGYCO  DS    CL6                 SAVED AGENCY CO=                             
SVIFVEND DS    CL(L'IVHVNDPR)      VENDOR ACCOUNT                               
*                                                                               
WC       DS    0C                                                               
SVWCTNH  DS    CL2                 SAVED T&H WORK CODE                          
SVWCTNHS DS    CL2                 SPACE PADDING                                
SVWCPNH  DS    CL2                 SAVED P&H WORK CODE                          
SVWCPNHS DS    CL2                 SPACE PADDING                                
SVWCSES  DS    CL2                 SAVED SESSION WORK CODE                      
SVWCSESS DS    CL2                 SPACE PADDING                                
SVWCRES  DS    CL2                 SAVED REUSE WORK CODE                        
SVWCRESS DS    CL2                 SPACE PADDING                                
SVWCGRT  DS    CL2                 SAVED GUARANTEE WORK CODE                    
SVWCGRTS DS    CL2                 SPACE PADDING                                
SVWCTAX  DS    CL2                 SAVED TAX WORK CODE                          
SVWCTAXS DS    CL2                 SPACE PADDING                                
SVWCHND  DS    CL2                 SAVED HND WORK CODE                          
SVWCHNDS DS    CL2                 SPACE PADDING                                
SVWCCPORZ DS   CL2                 SAVED C=ZZZ WORK CODE                        
SVWCCPORZS DS  CL2                 SPACE PADDING                                
WCLNQ    EQU   *-WC                                                             
                                                                                
SVOFF    DS    CL1                 OFFICE                                       
SVBUNIT  DS    CL5                 BUSINESS UNIT                                
SVUSE    DS    CL3                 USE                                          
SVCLT    DS    CL3                 CLIENT                                       
SVPRD    DS    CL3                 PRODUCT                                      
SVPRJ    DS    CL6                 ESTIMATE                                     
DLBLOCK  DS    XL(DLCBXLX)         DOWNLOAD INTERFACE BLOCK                     
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
         DS    0D                                                               
CNTINVS  DS    PL6                 COUNT INVOICES                               
CNTDTLS  DS    PL6                 COUNT DETAIL RECORDS                         
TAPCOUNT DS    PL6                                                              
*                                                                               
AGYFLST  DS    CL241               AGENCY FLIST                                 
         DS    0D                                                               
TAPEIO   DS    284C                                                             
C50TAPE  DS    284C                                                             
SDBLOCK  DS    CL(SOFXTNL)         SOFTDATE BLOCK                               
OUTDATE  DS    CL12                                                             
         SPACE 1                                                                
MYEND    DS    0D                                                               
         EJECT                                                                  
*---------------------------------------------------------------                
*              DSECT TO COVER PRINT LINES                                       
         SPACE 3                                                                
PRINTD   DSECT                                                                  
         DS    CL1                                                              
PAGY     DS    CL6                                                              
         DS    CL1                                                              
PINVOICE DS    CL7                                                              
         DS    CL1                                                              
PINVDATE DS    CL8                                                              
         DS    CL1                                                              
PCLIENT  DS    CL6                                                              
         DS    CL1                                                              
PPRD     DS    CL6                                                              
         DS    CL1                                                              
PEST     DS    CL6                                                              
         DS    CL1                                                              
PWRKCD   DS    CL6                                                              
         DS    CL1                                                              
PPNH     DS    CL12                                                             
         DS    CL4                                                              
         DS    CL3                                                              
PINVAMT  DS    CL12                                                             
         EJECT                                                                  
       ++INCLUDE DDDLCB                                                         
         EJECT                                                                  
*---------------------------------------------------------------------          
*              DSECT TO COVER TAPE RECORDS                                      
*---------------------------------------------------------------------          
TAPED    DSECT                                                                  
TAPEREC  DS    0C                  RECORD LENGTH OF 312                         
*                                                                               
*        HEADER RECORD                                                          
*                                                                               
HDRREC   DS    0C                  HEADER RECORD                                
HDRTYPE  DS    CL4'HDR*'           HDR ID - HDR*                                
HDRSCRID DS    CL8                 SCRIPT ID - ACINP10                          
HDRSCRCD DS    CL3                 SCRIPT CODE - 002                            
HDRLANG  DS    CL1                 LANGUAGE - E                                 
HDRERRCT DS    CL4                 ERROR COUNT - RESERVED                       
HDRITMCT DS    CL5                 ITEM COUNT - BLANK FOR DEFAULT               
*                                                                               
HDRRECLQ EQU   *-HDRREC            RECORD LENGTH                                
*                                                                               
*                                                                               
*                                                                               
         ORG   HDRREC                                                           
C50REC   DS    0C                                                               
C50COMP  DS    CL2'50'             COMPANY                                      
C50OFF   DS    CL2'01'             OFFICE                                       
C50VEND  DS    CL10'TALPARCP'      VENDOR                                       
C50INV   DS    CL15                TP INVOICE NUMBER                            
C50INVD  DS    CL8                 INVOICE DATE                                 
C50PO    DS    CL8                 PO NUMBER                                    
C50COST  DS    CL11                COST                                         
C50CLI   DS    CL5                 CLIENT                                       
C50PRD   DS    CL4                 PRODUCT                                      
C50BRAND DS    CL4                 BRAND                                        
C50JOB   DS    CL12                JOB/EDI TAG                                  
C50USETX DS    CL4                 USE TAX (OPTIONAL)                           
C50WORK  DS    CL4'TALE'           WORK CODE                                    
C50TRAN  DS    CL3'VND'            TRAN CODE                                    
C50EMP   DS    CL5                 EMPLOYEE CODE                                
C50DESC  DS    CL30                G/L DESCRIPTION                              
C50AP    DS    CL8                 A/P ACCOUNT                                  
C50IRD   DS    CL30                INVOICE REMITTANCE ADVICE                    
C50EXCH  DS    CL3                 EXCHANGE CODE                                
C50FEXCH DS    CL15                FOREIGN EXCHANGE AMOUNT                      
C50MWO   DS    CL1                 MEMO WRITEOFF                                
C50WOD   DS    CL3                 WRITEOFF DEPT                                
C50WOA   DS    CL8                 WRITEOFF ACCOUNT                             
*                                                                               
C50RECLQ EQU   *-C50REC                                                         
*                                                                               
*        BATCH HEADER RECORD                                                    
*                                                                               
         ORG   HDRREC                                                           
BHDREC   DS    0C                  BATCH HEADER                                 
BHDTYPE  DS    CL4'ITM1'           RECORD TYPE - ITM1                           
BHDREF   DS    CL4'IN01'           BATCH REFERENCE - IN01                       
BHDNAME  DS    CL15                BATCH NAME - 'INVOICES - MMM/YY'             
BHDINTYP DS    CL3'10 '            INPUT TYPW '10 '                             
BHDMONTH DS    CL6                 MONTH OF ACTVITY - (MMM/YY)                  
BHDEFFDT DS    CL8                 EFFECTIVE DATE - NOT USED                    
BHDCOM   DS    CL50                COMMENTS - NOT USED                          
BHDEND   DS    CL1                 BATCH END ACTION -  'U' - UPDATE             
*                                                                               
BHDRECLQ EQU   *-BHDREC            RECORD LENGTH                                
*                                                                               
*        INVOICE HEADER RECORD                                                  
*                                                                               
         ORG   HDRREC                                                           
IVHREC   DS    0C                  INVOICE HEADER                               
IVHTYPE  DS    CL4'ITM2'           RECORD TYPE - ITM2                           
IVHTYPID DS    CL1'H'              HEADER/DETAIL - 'H'                          
IVHPO#   DS    CL8                 PO NUMBER - NOT USED                         
IVHINVNM DS    CL27                INVOICE NUMBER                               
IVHINVDT DS    CL8                 INVOICE DATE (MMMDD/YY)                      
IVHINVDU DS    CL8                 INVOICE DUE DATE - NOT USED                  
IVHVNDPR DS    CL15                PRODUCTION VENDOR                            
IVHCDPR  DS    CL1                 CASH DISCOUNT - NOT USED                     
IVHVNDEX DS    CL15                EXPENSE VENODR - NOT USED                    
IVHCDEX  DS    CL1                 CASH DISCOUNT - NOT USED                     
IVHCD$   DS    CL15                CASH DISCOUNT - NOT USED                     
         DS    CL1                 RESERVED                                     
IVHURGNT DS    CL1'U'              URGENT                                       
*                                                                               
IVHRECLQ EQU   *-IVHREC            RECORD LENGTH                                
*                                                                               
*        INVOICE DETAIL RECORD                                                  
*                                                                               
         ORG   HDRREC                                                           
IVDREC   DS    0C                  INVOICE HEADER                               
IVDTYPE  DS    CL4'ITM3'           RECORD TYPE - ITM3                           
IVDTYPID DS    CL1'D'              HEADER/DETAIL - 'D'                          
IVDAMT   DS    CL12                AMOUNT - EG -1000.00                         
IVDWRKCD DS    CL4                 WORK CODE                                    
IVDCLT   DS    CL3                 CLIENT                                       
IVDPRD   DS    CL3                 PRODUCT                                      
IVDEST   DS    CL6                 ESTIMATE                                     
IVDEXACC DS    CL15                EXPENSE ACCOUNT                              
IVDDBOFF DS    CL2                 DEBIT    OFFICE                              
IVDCROFF DS    CL2                 CREDIT   OFFICE                              
IVDANOFF DS    CL2                 ANALYSIS OFFICE                              
IVDDEPT  DS    CL3                 DEPARTMENT                                   
IVDPERS  DS    CL6                 PERSON                                       
IVDNAR1  DS    CL60                NARRATIVE LINE 1                             
IVDNAR2  DS    CL60                NARRATIVE LINE 2                             
IVDTAXBS DS    CL12                TAX BASIS                                    
IVDTAXCD DS    CL2                 TAX WORKCODE                                 
IVDTAXLC DS    CL17                TAX LOCALITY                                 
IVDSPDPT DS    CL7                 SPECIAL DEPARTMENT                           
*                                                                               
IVDRECLQ EQU   *-IVDREC            RECORD LENGTH                                
*        TRAILER RECORD                                                         
*                                                                               
         ORG   HDRREC                                                           
TRLREC   DS    0C                  INVOICE HEADER                               
TRLTYPE  DS    CL4'TLR*'           RECORD TYPE - TRL*                           
*                                                                               
TRLRECLQ EQU   *-TRLREC            RECORD LENGTH                                
*                                                                               
         EJECT                                                                  
*---------------------------------------------------------------------          
*              DSECT TO COVER SORT RECORDS                                      
*---------------------------------------------------------------------          
SRTD     DSECT                                                                  
SRTREC   DS    0C                  RECORD LENGTH OF 256                         
SRTAGY   DS    CL6                 AGENCY                                       
SRTINVDT DS    CL6                 INVOICE DATE                                 
SRTINVNM DS    CL8                 INVOICE NUMBER                               
SRTCLT   DS    CL3                 CLIENT                                       
SRTPRD   DS    CL3                 PRODUCT                                      
SRTJOB   DS    CL6                 JOB                                          
         DS    XL2                 SPARE                                        
SRTTOT   DS    F                   INVOICE TOTAL                                
SRTTXHND DS    F                   TAX AND HANDLING                             
SRTTAX   DS    F                   TAX                                          
SRTHND   DS    F                   HANDLING                                     
SRTPDZ   DS    F                   PAID FOR ZZZ CAST MEMBERS                    
SRTPNH   DS    F                   PENSION AND HEALTH                           
SRTUSE   DS    CL3                 INVOICE USE                                  
SRTTYPE  DS    CL1                 INVOICE TYPE OF PAYMENT FOR USE              
SRTWC    DS    0C                                                               
SRTWCTNH DS    CL2                 SAVED T&H WORK CODE                          
         DS    CL2                                                              
SRTWCPNH DS    CL2                 SAVED P&H WORK CODE                          
         DS    CL2                                                              
SRTWCSES DS    CL2                 SAVED SESSION WORK CODE                      
         DS    CL2                                                              
SRTWCRES DS    CL2                 SAVED REUSE WORK CODE                        
         DS    CL2                                                              
SRTWCGRT DS    CL2                 SAVED GUARANTEE WORK CODE                    
         DS    CL2                                                              
SRTWCTAX DS    CL2                 SAVED TAX WORK CODE                          
         DS    CL2                                                              
SRTWCHND DS    CL2                 SAVED HND WORK CODE                          
         DS    CL2                                                              
SRTWCCPORZ DS  CL2                 SAVED C=ZZZ WORK CODE                        
         DS    CL2                                                              
SRTPO    DS    CL10                PURCHASE ORDER                               
SRTEST   DS    CL12                ESTIMATE/JOB/EDI                             
SRTRECLN EQU   *-SRTREC                                                         
*                                                                               
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
**PAN#1  DC    CL21'015TAREP61   11/26/14'                                      
         END                                                                    
