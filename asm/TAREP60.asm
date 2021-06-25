*          DATA SET TAREP60    AT LEVEL 002 AS OF 09/30/10                      
*PHASE T70360D                                                                  
*INCLUDE POWWOW                                                                 
         TITLE 'T70360 - CHRYSLER INTERACE'                                     
***********************************************************************         
* PROGRAM READS ALL INVOICES FOR REQUESTED PERIOD                               
* PUTS THEM IN SORTER                                                           
* THEN BUILDS THE HEADER RECORD, OUTPUTS TO FILE                                
* GETS RECORDS FROM SORTER, OUTPUTS TO FILE                                     
***********************************************************************         
T70360   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70360,R7                                                      
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
*                                                                               
         MVC   AMASTD,TWAMASTC     SAVE ADDRESS OF MASTER                       
         L     R2,TWADCONS                                                      
         USING TWADCOND,R2                                                      
*                                                                               
         MVC   ALOGOC,TLOGOC       SAVE A(LOGOC)                                
         L     R2,AMASTD                                                        
         USING MASTD,R2                                                         
*                                                                               
         MVC   ALOGO,MCVLOGO       SAVE A(LOGO)                                 
         MVC   AREMOT,MCVREMOT     SAVE A(REMOTE)                               
*                                                                               
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
         LA    R3,BLOCK                                                         
         USING PERVALD,R3                                                       
         GOTO1 PDVAL,DMCB,(R3)                                                  
         MVC   PRPER,PVALCPER      SAVE PRINTABLE PERIOD                        
         MVC   TIQPSTR,PVALPSTA                                                 
         MVC   TIQPEND,PVALPEND                                                 
         MVI   TIQDTYPE,TIQDBILL  SET FILTERING ON BILL DATE                    
         DROP  R3                                                               
*                                                                               
         LA    R2,SITCLIH          VALIDATE CLIENT FIELD                        
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
         LA    R1,TGD              SET A(TALENT GLOBALS)                        
         ST    R1,TIATGLOB                                                      
*                                                                               
         MVI   TIREAD,TLINDCDQ     READ INVOICE RECORDS                         
******   OI    TIQFLAGS,TIQFPBNP   ASK TO PASS BNP AS WELL                      
         MVI   TIQDTYPE,TIQDBILL   SET FILTERING ON BILL DATE                   
         OI    TIQFLAG2,TIQFSUB    PASS SUBSIDIARY INVOICES                     
*                                                                               
         GOTO1 TASYSIO,DMCB,TASYSIOD   READ INVOICE RECORDS                     
*                                                                               
         CP    CNTINVS,=P'0'       MUST HAVE 1 INVOICE                          
         BE    PREPX                                                            
*                                                                               
         BAS   RE,OPENTAPE         OPEN OUTPUT TAPE                             
*                                                                               
******   BAS   RE,FILHDR           BUILD FILE HEADER RECORD                     
*                                                                               
*        RETRIEVE RECORDS FROM SORT                                             
*                                                                               
PREP10   GOTO1 VSORTER,DMCB,=C'GET' GET A SORT RECORD                           
         ICM   R2,15,DMCB+4        POINT TO RETURNED RECORD                     
         BZ    PREP50              SKIP IF NO MORE SORT RECORDS                 
*                                                                               
         BRAS  RE,FILDTL           OUTPUT DATA TO FILE                          
*                                                                               
******   BAS   RE,PUTTAPE          WRITE OUT TAPE RECORD                        
         B     PREP10                                                           
*                                                                               
PREP50   DS    0H                                                               
*                                                                               
         BRAS  RE,FILTRL           SEND TRAILER RECORD                          
*                                                                               
         BAS   RE,CLOSTAPE         CLOSE TAPE                                   
*                                                                               
*******  BAS   RE,CALLPOW                                                       
*                                                                               
PREPX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
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
         EJECT                                                                  
*---------------------------------------------------------------------          
*              BUILD FILE HEADER                                                
*---------------------------------------------------------------------          
FILHDR   NTR1                                                                   
*                                                                               
         MVC   CHRREC,=CL256' '    INIT CHRYSLER RECORD AREA                    
         LA    R2,CHRREC           ESTABLISH CHRYSLER RECORDS                   
         USING CHRRECD,R2                                                       
*                                                                               
         MVC   CSTTP,=C'ST '       SET HEADER RECORD TYPE                       
         MVC   CSTID,=CL6'810'     SET TRANSACTION IDENTIFIER                   
         MVC   CSTCTL#,=C'0001'    SET TRANSACTION CONTROL #                    
*                                                                               
         BRAS  RE,PUTTAPE          PUT TO TAPE                                  
*                                                                               
FILHDRX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------          
*              BUILD FILE TRAILER                                               
*---------------------------------------------------------------------          
FILTRL   NTR1                                                                   
*                                                                               
         AP    CNTDTLS,=P'1'       BUMP COUNTER FOR THIS RECORD                 
*                                                                               
         MVC   CHRREC,=CL256' '    INIT CHRYSLER RECORD AREA                    
*                                                                               
         B     FILTRL10            'SE' RECORD NOT NEEDED                       
*                                                                               
         LA    R2,CHRREC           ESTABLISH CHRYSLER RECORDS                   
         USING CHRRECD,R2                                                       
*                                                                               
         MVC   CSETP,=C'SE '       SET TRAILER RECORD TYPE                      
*                                  SET NUMBER OF INCLUDED RECS                  
         EDIT  (P6,CNTDTLS),CSESEG#,0,ALIGN=LEFT                                
         MVC   CSECTL#,=C'0001'    SET TRANSACTION CONTROL #                    
*                                                                               
         BRAS  RE,PUTTAPE          PUT TO TAPE                                  
*                                                                               
*        PRINT TOTALS FOR REPORT                                                
*                                                                               
FILTRL10 DS    0H                                                               
*                                                                               
         LA    R2,P                                                             
         USING PRINTD,R2           ESTABLISH PRINT LINE                         
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)     PRINT BLANK LINE                             
*                                                                               
         MVC   PINVDATE(6),=C'TOTALS'   TITLE                                   
*                                                                               
         EDIT  (P6,CNTINVS),PINVOICE   NUMBER OF INVOICES                       
*                                                                               
         EDIT  TOTAMT,(15,PINVAMT),2,FLOAT=-  TOTAL DOLLARS                     
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)     PRINT LINE                                   
*                                                                               
FILTRLX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------          
*              CLOSE TAPE                                                       
*---------------------------------------------------------------------          
CLOSTAPE NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R2,P                                                             
         USING PRINTD,R2                                                        
         L     R2,=A(CHRTAPE)                                                   
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
MYSPECS  SSPEC H1,1,RUN                                                         
         SSPEC H3,1,REQUESTOR                                                   
         SSPEC H1,99,AGYNAME                                                    
         SSPEC H2,99,AGYADD                                                     
         SSPEC H3,99,REPORT                                                     
         SSPEC H3,112,PAGE                                                      
         SSPEC H5,2,C'INVOICE DATE  INVOICE   P.O.#'                            
         SSPEC H6,2,C'------------  -------   -----'                            
         SSPEC H5,48,C'TOTAL'                                                   
         SSPEC H6,48,C'-----'                                                   
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
         ENTRY CHRTAPE                                                          
*                                                                               
*  UNITAPE DEFINED AS TAPE FOR UT REPORT.                                       
*  UNITAPE DEFINED AS DISK FOR UD REPORT.                                       
*                                                                               
CHRTAPE  DCB   DDNAME=CHRTAPE,DSORG=PS,MACRF=(PM),                     X        
               RECFM=FB,LRECL=256,BUFNO=2,BLKSIZE=2560                          
         EJECT                                                                  
*--------------------------------------------------------------------           
*              HEADLINES ETC                                                    
*--------------------------------------------------------------------           
HOOK     NTR1  BASE=*,LABEL=*                                                   
         MVC   H1+52(18),=CL24'CHRYSLER INTERFACE'                              
         GOTO1 CENTER,DMCB,H1+52,24                                             
         GOTO1 UNDERLIN,DMCB,(24,H1+52),(X'BF',H2+52)                           
         MVC   H3+52(6),=C'PERIOD'                                              
         LA    R1,SITPERH                                                       
         MVC   H3+59(17),8(R1)                                                  
*                                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*---------------------------------------------------------------------          
*              ROUTINE TO SET UP SECOND REPORT ON QUEUE                         
*---------------------------------------------------------------------          
*                                                                               
IOHOOK   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   TIMODE,PROCREC      IF THERE IS SOMETHING INTERESTING            
         BNE   IOHOOKX                                                          
*                                                                               
         L     R4,TIAREC           FIND FIRST BILLING DETAILS ELEMENT           
         MVI   ELCODE,TABDELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   IOHOOKX                                                          
*                                                                               
         USING TABDD,R4            ESTABLISH BILLING DETAIL ELEMENT             
*                                                                               
         MVC   SVINVTOT,TABDTOT    INVOICE TOTAL                                
*                                                                               
         BRAS  RE,INVDET           INVOICE DETAILS                              
*                                                                               
IOHOOKX  DS    0H                                                               
*                                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*                                                                               
*---------------------------------------------------------------------          
*              BUILD INVOICE DETAILS                                            
*---------------------------------------------------------------------          
*                                                                               
INVDET   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R5,CHRREC                                                        
         USING CSRTRECD,R5         ESTABLISH CHRYSLER RECORD AREA               
         MVC   CSRTREC,=CL256' '   INIT RECORD AREA                             
*                                                                               
         MVC   CSRTFLTP,=CL6'810'  SET FILE TYPE                                
         MVC   CSRTTP,=C'BIG'      RECORD IDENTIFIER                            
         MVC   CSRTSEQ,=C'002'     'BIG' SEQUEMCE NUMBER                        
         MVC   CSRTZROS,=C'00000'  ZEROS                                        
*                                                                               
         L     R4,TIAREC           INVOICE NUMBER                               
         USING TLIND,R4                                                         
*                                                                               
         MVC   WORK(6),TLININV     COMPLEMENTED INVOICE NUMBER                  
         XC    WORK(6),=6X'FF'     UNCOMPLEMENT                                 
*                                                                               
         GOTO1 TINVCON,DMCB,WORK,SVINVNM,DATCON   CHARACTER                     
*                                                                               
         MVC   CSRTIV#(6),SVINVNM  ADD INVOICE NUMBER TO RECORD                 
*                                                                               
         L     R4,TIAREC           INVOICE DATE                                 
         MVI   ELCODE,TAINELQ                                                   
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING TAIND,R4            ESTABLISH INVOICE STATUS ELEMENT             
*                                  INVOICE DATE                                 
         GOTO1 DATCON,DMCB,(1,TAINBDTE),(X'20',CSRTIVDT)                        
         GOTO1 DATCON,DMCB,(1,TAINBDTE),(X'20',SVINVDT)                         
*                                                                               
*         PO#                                                                   
*                                                                               
         L     R4,TIAREC           POINT TO INVOICE RECORD                      
         MVI   ELCODE,TANUELQ      WANT FREE NUMBER ELEMENT                     
*                                                                               
         BRAS  RE,GETEL            FIND FIRST ELEMENT OF TYPE                   
*                                                                               
IDTLPOLP DS    0H                                                               
*                                                                               
         BNE   IDTLPODN            PO# NOT FOUND                                
*                                                                               
         USING TANUD,R4            ESTABLISH FREE NUMBER ELEMENT                
*                                                                               
         CLI   TANUTYPE,TANUTAUT   FIND PO# ELEMENT                             
         BE    IDTLPOFD                                                         
*                                                                               
IDTLPOCN DS    0H                                                               
*                                                                               
         BRAS  RE,NEXTEL           FIND NEXT ELM                                
         B     IDTLPOLP                                                         
*                                                                               
IDTLPOFD DS    0H                                                               
*                                                                               
         LLC   RF,TANULEN          GET ELEMENT LENGTH                           
         SHI   RF,TANULNQ          SUBTRACT HEADER LENGTH                       
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   CSRTPO#(0),TANUMBER SAVE PO#                                     
*                                                                               
IDTLPODN DS    0H                                                               
*                                                                               
         AP    CNTINVS,=P'1'       BUMP UP # OF INVOICES                        
*                                                                               
         L     RF,TOTAMT           ACCUMULATE INVOICE TOTALS                    
         A     RF,SVINVTOT                                                      
         ST    RF,TOTAMT                                                        
*                                                                               
         MVC   CSRTIVTL,SVINVTOT   PASS INVOICE TOTAL                           
*                                                                               
         BRAS  RE,SORTPUT          ADD RECORD TO SORT                           
*                                                                               
INVDETX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R5                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
*--------------------------------------------------------------------           
* SORT UTILITIES                                                                
*--------------------------------------------------------------------           
SORTPUT  NTR1  BASE=*,LABEL=*                                                   
         LA    R5,CHRREC           POINT TO SORT RECORD AREA                    
         USING CHRRECD,R5          ESTABLISH CHRYSLER RECORDS                   
*                                                                               
         CLI   SORTFRST,C'Y'       IF FIRST TIME TO SORT                        
         BNE   SORTPUT2                                                         
*                                  OPEN SORT                                    
         GOTO1 VSORTER,DMCB,SORTCARD,RECCARD                                    
*                                                                               
         MVI   SORTFRST,C'N'       INDICATE SORT OPEN                           
*                                                                               
SORTPUT2 GOTO1 VSORTER,DMCB,=C'PUT',CHRREC  ADD RECORD TO SORT                  
*                                                                               
SORTPUTX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         DS    0F                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,16,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=(256)'                                 
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R5                                                               
*                                                                               
         EJECT                                                                  
*--------------------------------------------------------------------           
*              TAPE ROUTINES                                                    
*--------------------------------------------------------------------           
OPENTAPE NTR1  BASE=*,LABEL=*                                                   
         L     R2,=A(CHRTAPE)      OPEN GDG                                     
         OPEN  ((2),OUTPUT)                                                     
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                  GET GENERATION NUMBER                        
         LINKX MF=(E,PARMLST),SF=(E,LINKLST)                                    
*                                                                               
OPENXIT  XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------          
*                                                                               
*              BUILD INVOICE DETAIL RECORDS FOR TAPE                            
*                                                                               
*NTRY    R2     A(SORTREC)                                                      
*                                                                               
*---------------------------------------------------------------------          
*                                                                               
FILDTL   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LR    R5,R2               SAVE SORT POINTER                            
         USING CSRTRECD,R5         ESTABLISH SORT RECORD                        
*                                                                               
         LA    R2,CHRREC                                                        
         USING CHRRECD,R2          ESTABLISH CHRYSLER RECORD AREA               
         MVC   CHRREC,=CL256' '    INIT RECORD AREA                             
*                                                                               
*        PUT OUT HEADER RECORD                                                  
*                                                                               
         MVC   CHDRFLTP,CSRTFLTP   SET FILE TYPE                                
         MVC   CHDRTP,=C'000'      SET RECORD TYPE                              
         MVC   CHDRSEQ,=C'000'     SET SEQUENCE NUMBER                          
         MVC   CHDRZROS,CSRTZROS   SET ZEROS                                    
         MVC   CHDRDDS(3),=C'DDS'  SET DDS ID                                   
         MVC   CHDRPROF,=CL15'CHRYSLER EDI'                                     
*                                                                               
         BRAS  RE,PUTTAPE          WRITE TO FILE                                
*                                                                               
*        PUT OUT BEGINNING SEGMENT OF INVOICE                                   
*                                                                               
         MVC   CHRREC,=CL256' '    INIT RECORD AREA                             
*                                                                               
         MVC   CBIGREC(CBIGRECL),CSRTRECD COPY START OF SORT RECORD             
*                                                                               
         BRAS  RE,PUTTAPE          WRITE TO FILE                                
*                                                                               
*        PUT CURRENCY RECORD TO FILE                                            
*                                                                               
         MVC   CHRREC,=CL256' '    INIT RECORD AREA                             
*                                                                               
         MVC   CCURFLTP,CSRTFLTP   SET FILE TYPE                                
         MVC   CCURTP,=C'CUR'      SET RECORD TYPE                              
         MVC   CCURSEQ,=C'004'     SET SEQUENCE NUMBER                          
         MVC   CCURZROS,CSRTZROS   SET ZEROS                                    
         MVC   CCURID,=C'SU'       SET SUBMITTER TYPE AS SUPPLIER               
         MVC   CCURCUR,=C'USD'     CURRENCY IS US $                             
*                                                                               
         BRAS  RE,PUTTAPE          WRITE TO FILE                                
*                                                                               
*        PUT NAME 1   RECORD TO FILE                                            
*                                                                               
         MVC   CHRREC,=CL256' '    INIT RECORD AREA                             
*                                                                               
         MVC   CN1FLTP,CSRTFLTP    SET FILE TYPE                                
         MVC   CN1TP,=C'N1 '       SET RECORD TYPE                              
         MVC   CN1SEQ,=C'007'      SET SEQUENCE NUMBER                          
         MVC   CN1ZROS,CSRTZROS    SET ZEROS                                    
         MVC   CN1ID,=C'SU'        SET SUBMITTER TYPE AS SUPPLIER               
         MVC   CN1NAME,=CL35'TALENT PARTNERS'  SET NAME                         
         MVC   CN1CDTP,=C'92'      SET CODE STRUCTE CODE                        
         MVC   CN1CDE,=CL17'63826' SET CHRYSLER'S CODE FOR TP                   
*                                                                               
         BRAS  RE,PUTTAPE          WRITE TO FILE                                
*                                                                               
*        PUT DATE RECORD TO FILE                                                
*                                                                               
         MVC   CHRREC,=CL256' '    INIT RECORD AREA                             
*                                                                               
         MVC   CDTMFLTP,CSRTFLTP   SET FILE TYPE                                
         MVC   CDTMTP,=C'DTM'      SET RECORD TYPE                              
         MVC   CDTMSEQ,=C'014'     SET SEQUENCE NUMBER                          
         MVC   CDTMZROS,CSRTZROS   SET ZEROS                                    
         MVC   CDTMDTTP,=C'011'    TYPE OF DATE CODE - DATE SENT                
         MVC   CDTMDATE,CSRTIVDT   INVOICE DATE                                 
*                                                                               
         BRAS  RE,PUTTAPE          WRITE TO FILE                                
*                                                                               
*        PUT PAPERWORK 1 RECORD TO FILE                                         
*                                                                               
         MVC   CHRREC,=CL256' '    INIT RECORD AREA                             
*                                                                               
         MVC   CPW1FLTP,CSRTFLTP   SET FILE TYPE                                
         MVC   CPW1TP,=C'PWK'      SET RECORD TYPE                              
         MVC   CPW1SEQ,=C'018'     SET SEQUENCE NUMBER                          
         MVC   CPW1ZROS,CSRTZROS   SET ZEROS                                    
         MVC   CPW1PWTP,=C'IV'     TYPE OF PAPERWORK RECORD - INVOICE           
         MVC   CPW1TRCD,=C'EL'     TRANSMISSION TYPE - ELECTRONIC               
*                                                                               
         BRAS  RE,PUTTAPE          WRITE TO FILE                                
*                                                                               
*        PUT TOTAL SUMMARY RECORD TO FILE                                       
*                                                                               
         MVC   CHRREC,=CL256' '    INIT RECORD AREA                             
*                                                                               
         MVC   CTDSFLTP,CSRTFLTP   SET FILE TYPE                                
         MVC   CTDSTP,=C'TDS'      SET RECORD TYPE                              
         MVC   CTDSSEQ,=C'050'     SET SEQUENCE NUMBER                          
         MVC   CTDSZROS,CSRTZROS   SET ZEROS                                    
         MVI   CTDSDECS,C'2'       2 DECIMALS                                   
         L     R3,CSRTIVTL         INVOICE TOTAL                                
         EDIT  (R3),(10,CTDSIVTL),0,FILL=0,FLOAT=-                              
*                                                                               
         BRAS  RE,PUTTAPE          WRITE TO FILE                                
*                                                                               
*        PUT TRANSACTION TOTALS RECORD TO FILE                                  
*                                                                               
         MVC   CHRREC,=CL256' '    INIT RECORD AREA                             
*                                                                               
         MVC   CCTTFLTP,CSRTFLTP   SET FILE TYPE                                
         MVC   CCTTTP,=C'CTT'      SET RECORD TYPE                              
         MVC   CCTTSEQ,=C'056'     SET SEQUENCE NUMBER                          
         MVC   CCTTZROS,CSRTZROS   SET ZEROS                                    
         MVI   CCTTDECS,C'0'       TWO DECIMAL PLACES                           
         MVI   CCTTTRN#,C'0'       NO DETAIL SEGMENTS IN SET                    
*                                                                               
         BRAS  RE,PUTTAPE          WRITE TO FILE                                
*                                                                               
*        PRINT LINE OF REPORT                                                   
*                                                                               
         LA    R2,P                                                             
         USING PRINTD,R2           ESTABLISH PRINT LINE                         
*                                                                               
         GOTOR DATCON,DMCB,(0,CSRTIVDT),(17,PINVDATE) INV DATE                  
*                                                                               
         MVC   PINVOICE(6),CSRTIV#   INVOICE NUMBER                             
*                                                                               
         MVC   PPO#,CSRTPO#          PO      NUMBER                             
*                                                                               
         EDIT  CSRTIVTL,(15,PINVAMT),2,FLOAT=-                                  
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)     PRINT LINE                                   
*                                                                               
FILDTLX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         DROP  R2,R5                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*---------------------------------------------------------------------          
*              PUT RECORD TO TAPE                                               
*---------------------------------------------------------------------          
PUTTAPE  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         AP    CNTDTLS,=P'1'       BUMP UP # OF DETAILS                         
*                                                                               
******   BRAS  RE,TRNTAPE          MAKE FIELD DELIMITED                         
******   LA    R2,TAPEIO           NEW OUTPUT AREA                              
*                                                                               
         L     R1,=A(CHRTAPE)                                                   
         PUT   (1),(R2)                                                         
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*---------------------------------------------------------------------          
*        TRANSLATE TAPE TO * DELINEATED FIELDS                                  
*        R2 ==> RECORD TO BE TRANSLATED                                         
*---------------------------------------------------------------------          
TRNTAPE  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    RE,RECTBL           POINT TO TRANSLATION TABLE                   
         USING CSRTRECD,R2         ESTABLISH GENERIC RECORD                     
*                                                                               
TTPTBLLP DS    0H                                                               
*                                                                               
         CLC   =X'FFFFFF',0(RE)    ERROR IF END OF TABLE REACHED                
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   CSRTTP,0(RE)        FIND RECORD ID IN TABLE                      
         BE    TTPTBLFD                                                         
*                                                                               
TTPTBLCN DS    0H                                                               
*                                                                               
         LLC   RF,L'CSRTTP(RE)     GET ENTRY LENGTH                             
         LA    RE,0(RF,RE)         BUMP TO NEXT TABLE ENTRY                     
         B     TTPTBLLP                                                         
*                                                                               
TTPTBLFD DS    0H                                                               
*                                                                               
         LA    R1,TAPEIO           POINT TO TAPE I/O AREA                       
         MVC   TAPEIO,=CL256' '    INIT IO AREA                                 
*                                                                               
         LA    RE,4(RE)            POINT TO LENGTH OF 1ST FLD IN REC            
         SR    RF,RF                                                            
*                                                                               
TBLMVCLP DS    0H                                                               
*                                                                               
         CLI   0(RE),X'FF'         DONE AT END OF ENTRY                         
         BE    TBLMVCDN                                                         
*                                                                               
         ICM   RF,1,0(RE)                                                       
         BZ    TBLMVCCN            NO DATA                                      
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),0(R2)       MOVE DATA TO I/O AREA                        
*                                                                               
         LA    R1,0(RF,R1)         POINT TO LAST BYTE OF DATA                   
*                                                                               
         CLI   0(R1),C' '          FIND LAST NON BLANK                          
         BH    *+10                                                             
         BCTR  R1,0                BACK UP A BYTE                               
         B     *-10                                                             
*                                                                               
         MVI   1(R1),C'*'          SET FIELD DELIMITER                          
         LA    R1,2(R1)            BUMP TO NEXT POSITION                        
*                                                                               
TBLMVCCN DS    0H                                                               
         LA    R2,1(RF,R2)         BUMP TO NEXT FIELD IN RECORD                 
         LA    RE,1(RE)            BUMP TO NEXT LENGTH IN TABLE                 
         B     TBLMVCLP                                                         
*                                                                               
TBLMVCDN DS    0H                                                               
*                                                                               
         BCTR  R1,0                BACKUP TO LAST BYTE                          
         CLI   0(R1),C'*'          IF FIELD DELIMITER                           
         BNE   *+12                                                             
         MVI   0(R1),C' '             REMOVE IT                                 
         B     *-14                   AGAIN                                     
*                                                                               
TRNTAPEX DS    0H                                                               
         XIT1                                                                   
*                                                                               
*---------------------------------------------------------------------          
*        TABLE DESCRIBING LENGTH OF EACH FIELD IN RECORD                        
*---------------------------------------------------------------------          
*                                                                               
RECTBL   DS    0D                  TABLE OF FIELD LENGTHS                       
*                                                                               
TBLST    DS    0C                  ENTRY FOR ST RECORD                          
         DC    CL3'ST '            RECORD IDENTIFIER                            
         DC    AL1(TBLSTX-TBLST)   TABLE LENGTH                                 
*                                                                               
         DC    AL1(L'CSTTP)        RECORD TYPE - HEADER                         
         DC    AL1(L'CSTID)        TRANSACTION IDENTIFIER CODE                  
         DC    AL1(L'CSTCTL#)      CONTROL NUMBER                               
         DC    X'FF'               END OF TABLE ENTRY                           
TBLSTX   DS    0C                                                               
*                                                                               
TBLBIG   DS    0C                  ENTRY FOR BIG RECORD                         
         DC    CL3'BIG'            RECORD IDENTIFIER                            
         DC    AL1(TBLBIGX-TBLBIG) TABLE LENGTH                                 
*                                                                               
         DC    AL1(L'CBIGTP)       RECORD TYPE - BEGINNING SEGMENT              
         DC    AL1(L'CBIGIVDT)     INVOICE DATE                                 
         DC    AL1(L'CBIGIV#)      INVOICE NUMBER                               
         DC    AL1(L'CBIGPODT)     PURCHASE ORDER DATE - NOT USED               
         DC    AL1(L'CBIGPO#)      PURCHASE ORDER NUMBER                        
         DC    X'FF'               END OF TABLE ENTRY                           
TBLBIGX  DS    0C                                                               
*                                                                               
TBLCUR   DS    0C                  ENTRY FOR CUR RECORD                         
         DC    CL3'CUR'            RECORD IDENTIFIER                            
         DC    AL1(TBLCURX-TBLCUR) TABLE LENGTH                                 
*                                                                               
         DC    AL1(L'CCURTP)       RECORD TYPE - CURRENCY RECORD                
         DC    AL1(L'CCURID)       TYPE OF TAPE SUBMITTER - SUPPLIER            
         DC    AL1(L'CCURCUR)      CURRENCY - ALWAYS USD                        
         DC    X'FF'               END OF TABLE ENTRY                           
TBLCURX  DS    0C                                                               
*                                                                               
TBLN1    DS    0C                  ENTRY FOR N1 RECORD                          
         DC    CL3'N1'             RECORD IDENTIFIER                            
         DC    AL1(TBLN1X-TBLN1) TABLE LENGTH                                   
*                                                                               
         DC    AL1(L'CN1TP)        RECORD TYPE - NAME RECORD                    
         DC    AL1(L'CN1ID)        TYPE OF TAPE SUBMITTER - SUPPLIER            
         DC    AL1(L'CN1NAME)      NAME - TALENT PARTNERS                       
         DC    AL1(L'CN1CDTP)      CODE STRUCTURE - ASSIGNED BY BUYER           
         DC    AL1(L'CN1CDE)       CHRYSLER'S CODE FOR TP                       
         DC    X'FF'               END OF TABLE ENTRY                           
TBLN1X   DS    0C                                                               
*                                                                               
TBLDTM   DS    0C                  ENTRY FOR DTM RECORD                         
         DC    CL3'DTM'            RECORD IDENTIFIER                            
         DC    AL1(TBLDTMX-TBLDTM) TABLE LENGTH                                 
*                                                                               
         DC    AL1(L'CDTMTP)       RECORD TYPE - DATE RECORD                    
         DC    AL1(L'CDTMDTTP)     TYPE OF DATE RECORD - DATE SENT              
         DC    AL1(L'CDTMDATE)     DATE - INVOICE DATE                          
         DC    X'FF'               END OF TABLE ENTRY                           
TBLDTMX  DS    0C                                                               
*                                                                               
TBLPWK   DS    0C                  ENTRY FOR PWK RECORD                         
         DC    CL3'PWK'            RECORD IDENTIFIER                            
         DC    AL1(TBLPWKX-TBLPWK) TABLE LENGTH                                 
*                                                                               
         DC    AL1(L'CPW1TP)       RECORD TYPE - PAPERWORK RECORD               
         DC    AL1(L'CPW1PWTP)     TYPE OF PAPERWORK RECORD - INVOICE           
         DC    AL1(L'CPW1TRCD)     TRANSMISSION TYPE - ELECTRONIC               
         DC    X'FF'               END OF TABLE ENTRY                           
TBLPWKX  DS    0C                                                               
*                                                                               
TBLTDS   DS    0C                  ENTRY FOR TDS RECORD                         
         DC    CL3'TDS'            RECORD IDENTIFIER                            
         DC    AL1(TBLTDSX-TBLTDS) TABLE LENGTH                                 
*                                                                               
         DC    AL1(L'CTDSTP)       RECORD TYPE - TOTAL     RECORD               
         DC    AL1(L'CTDSIVTL)     INVOICE TOTAL AMOUNT                         
         DC    X'FF'               END OF TABLE ENTRY                           
TBLTDSX  DS    0C                                                               
*                                                                               
TBLCTT   DS    0C                  ENTRY FOR CTT RECORD                         
         DC    CL3'CTT'            RECORD IDENTIFIER                            
         DC    AL1(TBLCTTX-TBLCTT) TABLE LENGTH                                 
*                                                                               
         DC    AL1(L'CCTTTP)       RECORD TYPE - TOTAL TRANSACTION              
         DC    AL1(L'CCTTTRN#)     NUMBER OF TRANSACTIONS                       
         DC    X'FF'               END OF TABLE ENTRY                           
TBLCTTX  DS    0C                                                               
*                                                                               
TBLSE    DS    0C                  ENTRY FOR SE RECORD                          
         DC    CL3'SE'             RECORD IDENTIFIER                            
         DC    AL1(TBLSEX-TBLSE) TABLE LENGTH                                   
*                                                                               
         DC    AL1(L'CSETP)        RECORD TYPE - TRANSACTION TRAILER            
         DC    AL1(L'CSESEG#)      NUMBER OF SEGMENTS                           
         DC    AL1(L'CSECTL#)      CONTROL NUMBER                               
         DC    X'FF'               END OF TABLE ENTRY                           
TBLSEX   DS    0C                                                               
*                                                                               
         DC    X'FFFFFF'           END OF TABLE                                 
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*                                                                               
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
JOBC     DC    CL80'//TPCHTCHP  JOB   ''CNTL'',MSGLEVEL=(1,1),COND=((0,X        
               NE)),'                                                           
         DC    CL80'//     MSGCLASS=X,CLASS=A,NOTIFY=CCOL'                      
         DC    CL80'//*MAIN CLASS=ADVANTIS,SYSTEM=SY1'                          
         DC    CL80'//SS  EXEC  BDEDICT'                                        
         DC    CL80'//EXTSYSIN DD *'                                            
*                                                                               
EDICT1   DC    C'EDICTKEY=TALCHR'                                               
         DC    CL(80-(*-EDICT1))' '                                             
*                                                                               
SUBJ1    DC    C'SUBJECT=CHRYSLER INTERFACE'                                    
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
TAPEC2   DC    CL16'TALDISK.TA0CHDS1'                                           
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
DSNME    DC    CL8'CHRTAPE'                                                     
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
         DS    0D                                                               
CNTINVS  DS    PL6                 COUNT INVOICES                               
CNTDTLS  DS    PL6                 COUNT DETAIL RECORDS                         
TAPCOUNT DS    PL6                                                              
*                                                                               
AGYFLST  DS    CL241               AGENCY FLIST                                 
         DS    0D                                                               
TAPEIO   DS    CL256                                                            
*                                                                               
         DS    0D                  ALIGNMENT                                    
CHRREC   DS    CL256               CHRYSLER RECORD BUILD AREA                   
*                                                                               
MYEND    DS    0D                                                               
         EJECT                                                                  
*---------------------------------------------------------------                
*              DSECT TO COVER PRINT LINES                                       
         SPACE 3                                                                
PRINTD   DSECT                                                                  
         DS    CL1                                                              
PINVDATE DS    CL8                                                              
         DS    CL6                                                              
PINVOICE DS    CL8                                                              
         DS    CL2                                                              
PPO#     DS    CL8                                                              
         DS    CL4                                                              
PINVAMT  DS    CL15                                                             
         EJECT                                                                  
       ++INCLUDE DDDLCB                                                         
         EJECT                                                                  
*---------------------------------------------------------------------          
*              DSECT TO COVER CHRYSLER RECORDS                                  
*---------------------------------------------------------------------          
*                                                                               
CHRRECD  DSECT                                                                  
CSTREC   DS    0C                  TRANSACTION SET HEADER                       
CSTFLTP  DS    CL6'810 '           FILE TYPE                                    
CSTTP    DS    CL3'ST '            RECORD TYPE - HEADER                         
CSTSEQ   DS    CL3'001'            SEQUENCE NUMBER                              
CSTZROS  DS    CL5'00000'          ZEROS                                        
CSTID    DS    CL3'810'            TRANSACTION IDENTIFIER CODE                  
CSTCTL#  DS    CL4                 CONTROL NUMBER                               
CSTRECL  EQU   *-CSTREC            LENGTH OF HEADER RECORD                      
*                                                                               
         ORG   CHRRECD                                                          
*                                                                               
*        HEADER RECORD                                                          
*                                                                               
CHDRREC  DS    0C                  HEADER                                       
CHDRFLTP DS    CL6'810 '           FILE TYPE                                    
CHDRTP   DS    CL3'000'            RECORD TYPE - HEADER RECORD                  
CHDRSEQ  DS    CL3'000'            SEQUENCE NUMBER                              
CHDRZROS DS    CL5'00000'          ZEROS                                        
         DS    CL24                SPARE                                        
CHDRDDS  DS    CL15                C'DDS'                                       
CHDRPROF DS    CL15                USER PROFILE                                 
CHDRRECL EQU   *-CHDRREC           LENGTH OF BEGINNING SEGMENT RECORD           
*                                                                               
         ORG   CHRRECD                                                          
*                                                                               
*        BEGINNING SEGMENT OF INVOICE                                           
*                                                                               
CBIGREC  DS    0C                  BEGINNING SEGMENT RECORD                     
CBIGFLTP DS    CL6'810 '           FILE TYPE                                    
CBIGTP   DS    CL3'BIG'            RECORD TYPE - BEGINNING SEGMENT              
CBIGSEQ  DS    CL3'002'            SEQUENCE NUMBER                              
CBIGZROS DS    CL5'00000'          ZEROS                                        
CBIGIVDT DS    CL8                 INVOICE DATE                                 
CBIGIV#  DS    CL22                INVOICE NUMBER                               
CBIGPODT DS    CL8                 PURCHASE ORDER DATE - NOT USED               
CBIGPO#  DS    CL22                PURCHASE ORDER NUMBER                        
CBIGRECL EQU   *-CBIGREC           LENGTH OF BEGINNING SEGMENT RECORD           
*                                                                               
         ORG   CHRRECD                                                          
*                                                                               
*        CURRENCY RECORD                                                        
*                                                                               
CCURREC  DS    0C                  CURRENCY RECORD                              
CCURFLTP DS    CL6'810 '           FILE TYPE                                    
CCURTP   DS    CL3'CUR'            RECORD TYPE - CURRENCY RECORD                
CCURSEQ  DS    CL3'004'            SEQUENCE NUMBER                              
CCURZROS DS    CL5'00000'          ZEROS                                        
CCURID   DS    CL2'SU'             TYPE OF TAPE SUBMITTER - SUPPLIER            
CCURCUR  DS    CL3'USD'            CURRENCY - ALWAYS USD                        
CCURRECL EQU   *-CCURREC           LENGTH OF CURRENCY RECORD                    
*                                                                               
         ORG   CHRRECD                                                          
*                                                                               
*        NAME 1   RECORD                                                        
*                                                                               
CN1REC   DS    0C                  NAME     RECORD                              
CN1FLTP  DS    CL6'810 '           FILE TYPE                                    
CN1TP    DS    CL3'N1 '            RECORD TYPE - NAME RECORD                    
CN1SEQ   DS    CL3'007'            SEQUENCE NUMBER                              
CN1ZROS  DS    CL5'00000'          ZEROS                                        
CN1ID    DS    CL2'SU'             TYPE OF TAPE SUBMITTER - SUPPLIER            
CN1NAME  DS    CL35'TALENT PARTNERS'  NAME - TALENT PARTNERS                    
CN1CDTP  DS    CL2'92'             CODE STRUCTURE - ASSIGNED BY BUYER           
CN1CDE   DS    CL17'63826'         CHRYSLER'S CODE FOR TP                       
CN1RECL  EQU   *-CN1REC            LENGTH OF NAME RECORD                        
*                                                                               
         ORG   CHRRECD                                                          
*                                                                               
*        DATE     RECORD                                                        
*                                                                               
CDTMREC  DS    0C                  DATE RECORD                                  
CDTMFLTP DS    CL6'810 '           FILE TYPE                                    
CDTMTP   DS    CL3'DTM'            RECORD TYPE - DATE RECORD                    
CDTMSEQ  DS    CL3'014'            SEQUENCE NUMBER                              
CDTMZROS DS    CL5'00000'          ZEROS                                        
CDTMDTTP DS    CL3'011'            TYPE OF DATE RECORD - DATE SENT              
CDTMDATE DS    CL6                 DATE - INVOICE DATE                          
CDTMRECL EQU   *-CDTMREC           LENGTH OF DATE RECORD                        
*                                                                               
         ORG   CHRRECD                                                          
*                                                                               
*        PAPERWORK 1 RECORD                                                     
*                                                                               
CPW1REC  DS    0C                  PAPERWORK RECORD                             
CPW1FLTP DS    CL6'810 '           FILE TYPE                                    
CPW1TP   DS    CL3'PWK'            RECORD TYPE - PAPERWORK RECORD               
CPW1SEQ  DS    CL3'018'            SEQUENCE NUMBER                              
CPW1ZROS DS    CL5'00000'          ZEROS                                        
CPW1PWTP DS    CL2'IV'             TYPE OF PAPERWORK RECORD - INVOICE           
CPW1TRCD DS    CL2'EL'             TRANSMISSION TYPE - ELECTRONIC               
CPW1RECL EQU   *-CPW1REC           LENGTH OF PAPERWORK RECORD                   
*                                                                               
         ORG   CHRRECD                                                          
*                                                                               
*        TOTAL SUMMARY                                                          
*                                                                               
CTDSREC  DS    0C                  TOTAL     RECORD                             
CTDSFLTP DS    CL6'810 '           FILE TYPE                                    
CTDSTP   DS    CL3'TDS'            RECORD TYPE - TOTAL     RECORD               
CTDSSEQ  DS    CL3'050'            SEQUENCE NUMBER                              
CTDSZROS DS    CL5'00000'          ZEROS                                        
CTDSDECS DS    CL1                 NUMBER OF DECIMALS                           
CTDSIVTL DS    CL15                INVOICE TOTAL AMOUNT                         
CTDSRECL EQU   *-CTDSREC           LENGTH OF TOTAL     RECORD                   
*                                                                               
         ORG   CHRRECD                                                          
*                                                                               
*        TRANSACTION TOTALS                                                     
*                                                                               
CCTTREC  DS    0C                  TRANSACTION TOTAL RECORD                     
CCTTFLTP DS    CL6'810 '           FILE TYPE                                    
CCTTTP   DS    CL3'CTT'            RECORD TYPE - TOTAL TRANSACTION              
CCTTSEQ  DS    CL3'056'            SEQUENCE NUMBER                              
CCTTZROS DS    CL5'00000'          ZEROS                                        
CCTTDECS DS    CL1                 NUMBER OF DECIMALS                           
CCTTTRN# DS    CL6                 NUMBER OF TRANSACTIONS                       
CCTTRECL EQU   *-CCTTREC           LENGTH OF TOTAL     RECORD                   
*                                                                               
         ORG   CHRRECD                                                          
*                                                                               
*        TRANSACTION TRAILER                                                    
*                                                                               
CSEREC   DS    0C                  TRANSACTION TRAILER RECORD                   
CSEFLTP  DS    CL4'810 '           FILE TYPE                                    
CSETP    DS    CL3'SE '            RECORD TYPE - TRANSACTION TRAILER            
CSESEQ   DS    CL3'089'            SEQUENCE NUMBER                              
CSEZROS  DS    CL5'00000'          ZEROS                                        
CSESEG#  DS    CL6                 NUMBER OF SEGMENTS                           
CSECTL#  DS    CL4                 CONTROL NUMBER                               
CSERECL  EQU   *-CSEREC            LENGTH OF TOTAL     RECORD                   
*                                                                               
         ORG   CHRRECD                                                          
*                                                                               
*        SORT RECORD FOR INVOICE                                                
*                                                                               
CSRTRECD DSECT                     BEGINNING SEGMENT RECORD                     
CSRTREC  DS    0CL256              BEGINNING SEGMENT RECORD                     
CSRTFLTP DS    CL6'810 '           FILE TYPE                                    
CSRTTP   DS    CL3'SRT'            RECORD TYPE - BEGINNING SEGMENT              
CSRTSEQ  DS    CL3                 SEQUENCE NUMBER                              
CSRTZROS DS    CL5'00000'          ZEROS                                        
CSRTIVDT DS    CL8                 INVOICE DATE                                 
CSRTIV#  DS    CL22                INVOICE NUMBER                               
CSRTPODT DS    CL8                 PURCHASE ORDER DATE - NOT USED               
CSRTPO#  DS    CL22                PURCHASE ORDER NUMBER                        
         DS    XL1                 ALIGNMENT                                    
CSRTIVTL DS    F                   INVOICE TOTAL AMOUNT                         
CSRTRECL EQU   *-CSRTREC           LENGTH OF BEGINNING SEGMENT RECORD           
*                                                                               
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
**PAN#1  DC    CL21'002TAREP60   09/30/10'                                      
         END                                                                    
