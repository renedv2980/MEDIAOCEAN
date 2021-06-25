*          DATA SET TAREP29    AT LEVEL 040 AS OF 11/18/12                      
*PHASE T70329A,*                                                                
         TITLE 'T70329 - P&&G ESTIMATES'                                        
T70329   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 MYEND-MYSTART,T70329,R7,R8                                       
         LR    RA,RC                                                            
         USING MYD,RA                                                           
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9                                                       
         L     R6,ATWA                                                          
         USING CONHEADH-64,R6                                                   
         GOTO1 INITIAL,DMCB,0                                                   
         BAS   RE,MYCLEAR                                                       
         SPACE 1                                                                
         CLI   MODE,VALREC                                                      
         BNE   MODE2                                                            
         BAS   RE,VREC                                                          
         B     XIT                                                              
         SPACE 1                                                                
MODE2    CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
         BAS   RE,VREC                                                          
         BAS   RE,PREP                                                          
         OC    ABUFFER,ABUFFER     FREEMAIN IF NEEDED                           
         BZ    XIT                                                              
         L     R0,LBUFFER                                                       
         L     R1,ABUFFER                                                       
         FREEMAIN RC,A=(1),LV=(0)                                               
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE RECORD                                                  
         SPACE 3                                                                
VREC     NTR1                                                                   
*                                  OPTIONAL FIELDS                              
         SPACE 1                                                                
         LA    R2,SPLOPTH          VALIDATE OPTIONS BEFORE OTHERS               
         BAS   RE,VOPTS                                                         
         SPACE 1                                                                
         LA    R2,SPLAGYH          AGENCY                                       
         CLI   5(R2),0                                                          
         BE    VREC2                                                            
         BRAS  RE,SPECFILT         POSITIVE OR NEGATIVE FLIST?                  
         BE    VREC4                                                            
         GOTO1 ANY                                                              
         MVC   TIFAGY,WORK                                                      
         GOTO1 RECVAL,DMCB,TLAYCDQ,(R2),0                                       
         B     VREC4                                                            
         SPACE 1                                                                
VREC2    LA    R2,SPLAGGH          AGENCY GROUP                                 
         CLI   5(R2),0                                                          
         BE    VREC4                                                            
         GOTO1 ANY                                                              
         MVC   TIFAGG,WORK                                                      
         GOTO1 RECVAL,DMCB,TLAGCDQ,(R2),0                                       
         SPACE 1                                                                
VREC4    LA    R2,SPLCLIH          CLIENT                                       
         CLI   5(R2),0                                                          
         BE    VREC4B                                                           
         GOTO1 ANY                                                              
         MVC   TIFCLI,WORK                                                      
         GOTO1 RECVAL,DMCB,TLCLCDQ,(R2),0                                       
         SPACE 1                                                                
VREC4B   LA    R2,SPLCGGH          CLIENT GROUP                                 
         CLI   5(R2),0                                                          
         BE    VREC4P                                                           
         GOTO1 ANY                                                              
         MVC   TIFCLG,WORK                                                      
         GOTO1 RECVAL,DMCB,TLCGCDQ,(R2),0                                       
         SPACE 1                                                                
VREC4P   LA    R2,SPLPRDH          PRODUCT                                      
         CLI   5(R2),0                                                          
         BE    VREC5                                                            
         GOTO1 ANY                                                              
         MVC   TIFPRD,WORK                                                      
         GOTO1 RECVAL,DMCB,TLPRCDQ,(R2),0                                       
         SPACE 1                                                                
VREC5    LA    R2,SPLEMPH          EMPLOYER                                     
         CLI   5(R2),0                                                          
         BE    VREC6                                                            
         GOTO1 ANY                                                              
         MVC   TIFEMP,WORK         ELSE, LET SYSIO FILTER                       
         GOTO1 RECVAL,DMCB,TLEMCDQ,(R2),0                                       
         SPACE 1                                                                
VREC6    LA    R2,SPLPERH          REQUEST START                                
         GOTO1 ANY                                                              
         GOTO1 VALIDATE,DMCB,QPER                                               
         SPACE 1                                                                
         LA    R2,SPLREQH          REQUEST END DATE                             
         GOTO1 ANY                                                              
         GOTO1 VALIDATE,DMCB,QREQ                                               
         SPACE 1                                                                
         LA    R2,SPLRUNH          RUN DATE (PRINT ONLY)                        
         GOTO1 ANY                                                              
         GOTO1 VALIDATE,DMCB,QRUN                                               
         SPACE 1                                                                
         LA    R2,SPLPREVH         PREVIOUS END                                 
         GOTO1 ANY                                                              
         GOTO1 VALIDATE,DMCB,QPREV                                              
         SPACE 1                                                                
         GOTO1 DATCON,DMCB,(0,QPER),(1,TIQPSTR)                                 
         GOTO1 DATCON,DMCB,(0,QREQ),(1,TIQPEND)                                 
         GOTO1 DATCON,DMCB,(0,QPREV),(1,PPREV)                                  
         SPACE 1                                                                
         LA    R2,SPLEPERH         ESTIMATE PERIOD NUMBER                       
         GOTO1 VALINUM                                                          
         MVC   THISMON,ACTUAL                                                   
         CLI   THISMON,0                                                        
         BE    INVXIT                                                           
         CLI   THISMON,12                                                       
         BH    INVXIT                                                           
         MVI   MONSTART,1                                                       
         MVI   MONEND,6                                                         
         CLI   THISMON,7                                                        
         BL    VREC8                                                            
         MVI   MONSTART,7                                                       
         MVI   MONEND,12                                                        
         SPACE 1                                                                
VREC8    LA    R2,SPLYEARH         OPTIONAL YEAR EXPRESSION                     
         GOTO1 DATCON,DMCB,(0,QREQ),(20,WORK)                                   
         MVC   OPTYEAR,WORK        DEFAULT TO DISPLAYABLE END YEAR              
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         GOTO1 ANY                                                              
         CLI   5(R2),4             MUST BE 4 CHARACTERS                         
         BNE   INVXIT                                                           
         MVC   WORK(4),=C'0000'                                                 
         MVZ   WORK(4),8(R2)                                                    
         CLC   WORK(4),=C'0000'                                                 
         BNE   INVXIT                                                           
         MVC   OPTYEAR,8(R2)       SAVE FOUR CHARACTER DISPLAYABLE YEAR         
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE OPTIONS                                                 
         SPACE 3                                                                
VOPTS    NTR1                                                                   
         XC    TRACECID,TRACECID                                                
         MVI   PGOPT,0                                                          
         MVC   ACOMRATE,=F'10'     DEFAULT TO 10%                               
         MVI   TRACOPT,0                                                        
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         GOTO1 SCANNER,DMCB,(R2),(10,BLOCK),0                                   
         ZIC   R0,4(R1)                                                         
         LA    R4,BLOCK                                                         
         USING SCAND,R4                                                         
         LTR   R0,R0                                                            
         BZ    BADOPT                                                           
         SPACE 1                                                                
OPT2     CLC   12(5,R4),=C'TRACE'  TRACE OPTION                                 
         BNE   OPT3                                                             
         MVC   TRACECID,22(R4)                                                  
         B     OPTEND                                                           
         SPACE 1                                                                
OPT3     CLC   12(2,R4),=C'AC'     AGENCY COMM RATE                             
         BNE   OPT4                                                             
         TM    3(R4),X'80'         VALID NUMERIC? ZERO IS OK                    
         BNO   BADOPT                                                           
         MVC   ACOMRATE,8(R4)                                                   
         B     OPTEND                                                           
         SPACE 1                                                                
OPT4     CLC   12(5,R4),=C'LIMIT'  LIMIT OPTION                                 
         BNE   OPT5                                                             
         L     R1,8(R4)            (CHECK NUMERIC)                              
         LTR   R1,R1                                                            
         BZ    BADOPT                                                           
         CVD   R1,DUB                                                           
         ZAP   RECLIMIT,DUB                                                     
         B     OPTEND                                                           
         SPACE 1                                                                
OPT5     CLC   12(6,R4),=C'REPORT' REPORT LIMIT                                 
         BNE   OPT6                                                             
         L     R1,8(R4)            (CHECK NUMERIC)                              
         LTR   R1,R1                                                            
         BZ    BADOPT                                                           
         CVD   R1,DUB                                                           
         ZAP   REPLIMIT,DUB                                                     
         B     OPTEND                                                           
         SPACE 1                                                                
OPT6     CLC   =C'TNH',SCDATA1    IF T AND H OPTION                             
         BNE   OPT7                                                             
         CLI   SCDATA2,C'Y'                                                     
         BE    OPT6X                                                            
         CLI   SCDATA2,C'N'                                                     
         BE    OPTEND                                                           
         B     BADOPT                                                           
OPT6X    OI    PGOPT,PGOPTTNH      SET IT ON                                    
         B     OPTEND                                                           
         SPACE 1                                                                
OPT7     CLC   =C'PRDAC',SCDATA1   IF IGNORING SPECIAL COMMRATE OPTION          
         BNE   OPT10                                                            
         CLI   SCDATA2,C'N'                                                     
         BNE   BADOPT                                                           
         OI    PGOPT,PGOPTSAC      SET IT ON                                    
         B     OPTEND                                                           
OPT10    CLC   =C'MYTRACE',SCDATA1 IF TRACING AGYTAB FOR AGENCY FLIST           
         BNE   BADOPT                                                           
         CLI   SCDATA2,C'Y'                                                     
         BNE   BADOPT                                                           
         MVI   TRACOPT,C'Y'                                                     
         B     OPTEND                                                           
         SPACE 1                                                                
BADOPT   MVC   CONHEAD(L'OPTERR),OPTERR                                         
         B     BADXIT                                                           
INVXIT   MVC   CONHEAD(L'INVERR),INVERR                                         
BADXIT   GOTO1 ERREX2                                                           
         SPACE 1                                                                
OPTERR   DC    C'** ERROR ** INVALID OPTION'                                    
INVERR   DC    C'** ERROR ** INVALID FIELD'                                     
         SPACE 1                                                                
OPTEND   LA    R4,32(R4)                                                        
         BCT   R0,OPT2                                                          
         B     XIT                                                              
         DROP  R6,R4                                                            
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 1                                                                
TRALIMIT DC    PL6'0'                                                           
RECLIMIT DC    PL6'9999999'                                                     
REPLIMIT DC    PL6'9999999'                                                     
         EJECT                                                                  
*              PRINT REPORT                                                     
         SPACE 3                                                                
PREP     NTR1                                                                   
         L     R5,ASPOOLD                                                       
         USING SPOOLD,R5                                                        
         LA    R1,HOOK             INITIALIZE FOR REPORT                        
         ST    R1,HEADHOOK                                                      
         MVC   MYSORTER,SORTER                                                  
         DROP  R5                                                               
         SPACE 1                                                                
         BRAS  RE,AGYFLIST         IF AGENCY IS AN FLIST, SET UP AGYTAB         
         L     R0,=AL4(NFAGY*FAGYLNQ)                                           
         GOTO1 MYTRACE,DMCB,=C'AGYTAB',(R0),A(AGYTAB)                           
*                                                                               
         MVC   TIACOMFC,ACOMFACS   SET UP FOR TASYSIO                           
         LA    R1,IOHOOK                                                        
         ST    R1,TIHOOK                                                        
         MVI   TIREAD,TLCKCDQ      SET TO READ CHECKS                           
         MVI   TIQDTYPE,TIQDBILL   FROM BILLING DATE POINTERS                   
         MVI   TIFCTYPE,C'O'       NO 'SOAPS' (TYPE O)                          
         NI    TIFCTYPE,X'FF'-X'40'                                             
         OI    TIQFLAGS,TIQFPDUM   HANDLE DUMMY RECORDS                         
         OI    TIQFLAG2,TIQFSUB    PASS SUBSIDIARY INVOICES                     
*                                                                               
         CLC   TIFEMP,=C'PG '      IF EMPLOYER FILTER IS PG                     
         BNE   PREP2                                                            
         XC    TIFEMP,TIFEMP                                                    
         OI    TIQFLAGS,TIQFDIR    FILTER LOCALLY AT DIRECTORY LEVEL            
*                                                                               
PREP2    BRAS  RE,GETAGY           IF AGY IS A POS FLIST, SET 1ST AGY           
*                                                                               
PREP3    GOTO1 TASYSIO,DMCB,TASYSIOD                                            
*                                                                               
         BRAS  RE,GETNAGY          IF AGY IS POS FLIST, SET NEXT AGY            
         BE    PREP3                                                            
*                                                                               
         BAS   RE,DOREST           SORT AND PRINT REPORT                        
         B     XIT                                                              
         SPACE 1                                                                
IOHOOK   NTR1                                                                   
         CLI   TIMODE,PROCDIR      FILTERING DIRECTORY                          
         BNE   IOHOOK1                                                          
         LA    R4,TIKEY                                                         
         USING TLINPD,R4                                                        
         CLI   0(R4),TLCKCDQ DON'T FILTER CHECKS                                
         BE    XIT                                                              
*                                  CHECK INVOICES                               
         CLC   =C'PG ',TIEMP       OK IF PG                                     
         BE    XIT                                                              
         CLC   TGTPEMP,TIEMP       OR IF TP                                     
         BNE   XIT                                                              
         CLI   TLINDCUR,C'C'          AND CANADIAN $                            
         B     XIT                    (CC IS SET FOR SYSIO)                     
         SPACE 1                                                                
IOHOOK1  CLC   TIAGY,=C'999999'    NO ADJUSTMENTS                               
         BE    XIT                                                              
*                                                                               
         L     RE,ATWA                                                          
         USING CONHEADH-64,RE                                                   
         LA    R2,SPLAGYH                                                       
         DROP  RE                                                               
         BRAS  RE,NEGFLIST         IF AGENCY IS A NEGATIVE FLIST,               
         BNE   *+12                                                             
         BRAS  RE,LOOKAGY          IT MUST NOT BE IN THE AGY TABLE              
         BE    XIT                                                              
*                                                                               
         CLI   TIMODE,PROCINV      IF THERE IS SOMETHING INTERESTING            
         BNE   IOHOOK2                                                          
         BAS   RE,SVPRDIN          SAVE PRODUCT INFORMATION                     
         BAS   RE,EXTDATA          EXTRACT DATA FROM INVOICES                   
         BAS   RE,FILLINV          FILL SORT RECORDS - INVOICE LEVEL            
         B     XIT                                                              
         SPACE 1                                                                
IOHOOK2  CLI   TIMODE,PROCREC      IF THERE IS SOMETHING INTERESTING            
         BNE   XIT                                                              
         BAS   RE,EXTCHECK         EXTRACT DATA FROM CHECKS                     
         BAS   RE,FILLCHEK         FILL SORT RECORDS                            
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO SAVE PRODUCT INFORMATION                              
         SPACE 1                                                                
SVPRDIN  NTR1                                                                   
         XC    PRDCRATE,PRDCRATE  CLEAR PRODUCT COMMISSION RATE                 
         TM    PGOPT,PGOPTSAC     SPECIAL COMMRATE OPTION?                      
         BO    SVPRDINX                                                         
         SPACE 1                                                                
         MVC   TGAGY,TIAGY                                                      
         MVC   TGCLI,TICLI        GET INVOICE'S PRODUCT RECORD                  
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLPRCDQ,(X'A0',TIPRD)                                
         BNE   SVPRDINX           IF INVOICE DOESN'T HAVE A PRODUCT             
         SPACE 1                                                                
         USING TAPID,R6           IF INVOICE DOES HAVE PRODUCT ...              
         L     R6,AIO2            SAVE PRODUCT'S COMMISSION RATE                
         MVI   ELCODE,TAPIELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   SVPRDINX                                                         
         MVC   PRDCRATE,TAPIAC                                                  
SVPRDINX MVC   AIO,AIO1                                                         
         MVC   KEY,TIKEY           RESTORE KEY                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'00'                                                            
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*              EXTRACT THE DATA WE NEED                                         
         SPACE 3                                                                
EXTDATA  NTR1                                                                   
         MVC   EXTAGY,TIAGY                                                     
         MVC   EXTCLI,=C'00600 '   FORCE IN P&G                                 
*******  MVC   EXTPRD,TIPRD                                                     
         XC    EXTPRD,EXTPRD                                                    
         MVC   EXTPNAME,MYSPACES                                                
         MVC   EXTCID,TICID                                                     
         MVC   EXTINV,TIINV                                                     
         MVC   EXTCAT,TICAT                                                     
         MVC   EXTONOF,TIONOF                                                   
         MVC   EXTCYC,TICSDATE                                                  
         MVC   EXTMED,TIMED                                                     
         CLI   EXTMED,TACOMEDC     IF MEDIA IS CABLE                            
         BNE   *+8                                                              
         MVI   EXTMED,TACOMEDT     SET IT TO TV                                 
         MVC   EXTEST,MYSPACES                                                  
         MVC   EXTAUTH,MYSPACES                                                 
         MVC   EXTEST(15),=C'*** MISSING ***'                                   
         MVC   EXTTITLE,MYSPACES                                                
         XC    EXTGST,EXTGST                                                    
         XC    EXTTNH,EXTTNH                                                    
         SPACE 1                                                                
         L     R6,TIAREC           BROWSE INVOICE                               
         BAS   RE,FIXCAN           CONVERT CANADIAN TO US                       
         MVI   ELCODE,0                                                         
         BAS   RE,GETEL                                                         
         B     *+8                                                              
INV2     BAS   RE,NEXTEL                                                        
         BNE   INVEND                                                           
         CLI   0(R6),TAFNELQ                                                    
         BE    INVFN                                                            
         CLI   0(R6),TANUELQ                                                    
         BE    INVNU                                                            
         CLI   0(R6),TAPDELQ                                                    
         BE    INVPD                                                            
         CLI   0(R6),TABDELQ                                                    
         BE    INVBD                                                            
         B     INV2                                                             
         SPACE 1                                                                
         USING TAFND,R6                                                         
INVFN    CLI   TAFNTYPE,TAFNTTTL   TITLE                                        
         BNE   INVFN2                                                           
         ZIC   R1,TAFNLEN                                                       
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     INV2                                                             
         MVC   EXTTITLE(0),TAFNNAME                                             
         SPACE 1                                                                
INVFN2   CLI   TAFNTYPE,TAFNTPRD   PRODUCT NAME                                 
         BNE   INV2                                                             
         B     INV2                NO PROD NAME FORM INVOICES                   
         SPACE 1                                                                
         ZIC   R1,TAFNLEN                                                       
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     INV2                                                             
         MVC   EXTPNAME(0),TAFNNAME                                             
         SPACE 1                                                                
         USING TANUD,R6                                                         
INVNU    CLI   TANUTYPE,TANUTEST   ESTIMATE                                     
         BNE   INVNU2                                                           
         MVC   EXTEST,MYSPACES                                                  
         ZIC   R1,TANULEN                                                       
         SH    R1,=H'4'                                                         
         CLC   EXTAGY,=CL6'DWSF'   FOR AGENCY 4937 & 0129 & DWSF                
         BE    INVNUA                                                           
         CLC   EXTAGY,=CL6'4937'                                                
         BE    INVNUA                                                           
         CLC   EXTAGY,=CL6'0129'                                                
         BE    INVNUA                                                           
         CLC   EXTAGY,=CL6'DWNY'   AND DWNY                                     
         BNE   INVNU1                                                           
INVNUA   LTR   R1,R1                                                            
         BZ    INV2                                                             
         BCTR  R1,0                DON'T PASS LAST CHARACTER                    
INVNU1   EX    R1,*+8                                                           
         B     INV2                                                             
         MVC   EXTEST(0),TANUMBER                                               
         SPACE 1                                                                
INVNU2   CLI   TANUTYPE,TANUTAUT   PO/AUTH                                      
         BNE   INV2                                                             
         MVC   EXTAUTH,MYSPACES                                                 
         ZIC   R1,TANULEN                                                       
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     INV2                                                             
         MVC   EXTAUTH(0),TANUMBER                                              
         SPACE 1                                                                
         USING TABDD,R6                                                         
INVBD    MVC   EXTGST,TABDGST      SET GST                                      
         TM    PGOPT,PGOPTTNH      IF TNH OPTION REQUESTED                      
         BZ    INV2                                                             
*                                                                               
         L     R1,TABDTAX          SET TNH AMOUNT                               
         A     R1,TABDHND                                                       
         A     R1,TABDHNDC                                                      
         A     R1,TABDFICR                                                      
         A     R1,TABDGST                                                       
         STCM  R1,15,EXTTNH                                                     
         B     INV2                                                             
         SPACE 1                                                                
         USING TAPDD,R6                                                         
INVPD    MVC   EXTMON,TAPDESPD     MONTH=ESTIMATE PERIOD                        
         GOTO1 USEVAL,DMCB,TAPDUSE,TAPDTYPE                                     
         MVC   EXTUNAM,TGUSNAME                                                 
         MVC   EXTUDET,MYSPACES                                                 
         LA    R2,EXTUDET                                                       
         BAS   RE,EDITDET                                                       
         MVC   EXTACDE,TAPDACDE                                                 
         L     R1,TAPDPAYI         PAY=PAYI+PAYC+REXP                           
         A     R1,TAPDPAYC                                                      
         A     R1,TAPDREXP                                                      
         TM    TAPDPSTS,TAPDPBNP   IF BNP                                       
         BNO   INVPD1                                                           
         CLI   TAPDINV+5,X'03'        AND OLD T&R                               
         BNE   INVPD1                                                           
         L     R1,TAPDSPNH         (FUDGE FOR CONVERTED BNP)                    
         SPACE 1                                                                
INVPD1   ST    R1,EXTPAY                                                        
         MVC   EXTPNH,TAPDPNH      PNH=PNH                                      
         B     INV2                                                             
         EJECT                                                                  
INVEND   L     R1,EXTPAY           AC=RATE(PAY+PNH)                             
         A     R1,EXTPNH                                                        
         SPACE 1                                                                
         OC    PRDCRATE,PRDCRATE                                                
         BZ    INVEND1                                                          
         SPACE 1                                                                
         CLI   PRDCRATE,X'FF'      COMMISSION RATE FOR PRODUCT OF X'FF'         
         BNE   *+12                MEANS 0                                      
         LA    R1,0                                                             
         B     INVEND1A                                                         
         SPACE 1                                                                
         SR    R0,R0               PREPARE FOR DIVIDE                           
         MH    R1,PRDCRATE                                                      
         D     R0,=F'100'                                                       
         B     *+8                                                              
         SPACE 1                                                                
INVEND1  M     R0,ACOMRATE         AC RATE CAN BE INPUT                         
         D     R0,=F'50'                                                        
INVEND1A LTR   R1,R1                                                            
         BM    *+8                                                              
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         ST    R1,EXTAC                                                         
         SPACE 1                                                                
         L     R1,EXTPAY                                                        
         A     R1,EXTPNH                                                        
         TM    PGOPT,PGOPTTNH      IF TNH OPTION                                
         BO    *+8                                                              
         A     R1,EXTGST           GST ALREADY IN TNH (DON'T ADD TWICE)         
         A     R1,EXTTNH                                                        
         A     R1,EXTAC                                                         
         ST    R1,EXTGRS                                                        
         SPACE 1                                                                
         ST    R1,EXTRTOT                                                       
         XC    EXTRPAY,EXTRPAY                                                  
         XC    EXTRADJ,EXTRADJ                                                  
         LA    R1,EXTRPAY          ESTABLISH WHETHER AN ADJUSTMENT              
         MVI   EXTMONA,C' '                                                     
         LA    RF,EXTAUTH+15       CHECK THE LAST CHARACTER OF                  
         LA    R0,16               PO/AUTH NUMBER....                           
         SPACE 1                                                                
INVEND2  CLI   0(RF),C'*'          IF *, THEN CAN'T BE ADJUSTMENT               
         BE    INVEND8                                                          
         CLI   0(RF),C'@'          IF @, THEN MUST BE ADJUSTMENT                
         BE    INVEND6                                                          
         CLI   0(RF),C' '                                                       
         BH    INVEND4                                                          
         BCTR  RF,0                                                             
         BCT   R0,INVEND2                                                       
         SPACE 1                                                                
INVEND4  CLC   TIBIDATE,PPREV      IF BILLED AFTER PREVIOUS RUN DATE            
         BNH   INVEND8                                                          
         L     RF,ATWA                                                          
         USING CONHEADH-64,RF                                                   
         CLI   SPLATP,C'Y'         OPTION TO ADJUST THIS PERIOD                 
         BE    INVEND6                                                          
         CLC   EXTMON,THISMON      AND ESTIMATE PERIOD < CURRENT                
         BE    INVEND8                                                          
         SPACE 1                                                                
INVEND6  LA    R1,EXTRADJ          THEN THIS IS ADJUSTMENT                      
         MVI   EXTMONA,C'*'                                                     
         SPACE 1                                                                
INVEND8  MVC   0(4,R1),EXTRTOT                                                  
         B     XIT                                                              
         DROP  RF                                                               
         EJECT                                                                  
*              DATA FROM CHECK RECORD                                           
         SPACE 3                                                                
EXTCHECK NTR1                                                                   
         L     R6,TIAREC           CHECK FOR LIENS                              
         MVI   ELCODE,TACDELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING TACDD,R6                                                         
         TM    TACDSTAT,TACDSLIN                                                
         BO    XIT                                                              
         TM    TACDSTAT,TACDSTRS                                                
         BO    XIT                                                              
         SPACE 1                                                                
         L     R6,TIAREC           BROWSE CHECK                                 
         BAS   RE,FIXCAN           CONVERT CANADIAN TO US                       
         MVI   ELCODE,0                                                         
         BAS   RE,GETEL                                                         
         B     *+8                                                              
         SPACE 1                                                                
CHK2     BAS   RE,NEXTEL                                                        
         BNE   XIT                                                              
         CLI   0(R6),TAPDELQ                                                    
         BE    CHKPD                                                            
         B     CHK2                                                             
         SPACE 1                                                                
         USING TAPDD,R6                                                         
*                                  USE DETAILS                                  
CHKPD    MVC   EXTACDE,TAPDACDE                                                 
         L     R1,TAPDPAYI         PAY=PAYI+PAYC+REXP                           
         A     R1,TAPDPAYC                                                      
         A     R1,TAPDREXP                                                      
         ST    R1,EXTRATE          RATE=PAY                                     
         B     CHK2                                                             
         EJECT                                                                  
*              FIX CANADIAN DOLLARS                                             
         SPACE 1                                                                
FIXCAN   NTR1                                                                   
         L     R6,TIAREC                                                        
         MVI   ELCODE,TAPDELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING TAPDD,R6                                                         
         TM    TAPDSTAT,TAPDSCAN   CHECK THIS IS CANADIAN                       
         BNO   XIT                                                              
         SPACE 1                                                                
         CLI   TIMODE,PROCINV      GET RATE FOR INVOICE                         
         BNE   FIXCAN2                                                          
         XC    CANRATE,CANRATE                                                  
         LR    R2,R6               SAVE A(PMT DTLS EL.)                         
         L     R6,TIAREC                                                        
         MVI   ELCODE,TABDELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING TABDD,R6                                                         
         OC    TABDCCVT,TABDCCVT                                                
         BZ    XIT                                                              
         ZAP   DUB,TABDCCVT                                                     
         CVB   R1,DUB                                                           
         ST    R1,CANRATE                                                       
         SPACE 1                                                                
         LA    R1,TABDAMTS         CONVERT GST TO US$                           
         LA    R0,TABDAMTL/L'TABDGST                                            
FIXCAN1  BAS   RE,FIXCAN6                                                       
         LA    R1,L'TABDGST(R1)                                                 
         BCT   R0,FIXCAN1                                                       
         LR    R6,R2               RESTORE A(PMT DTLS EL.)                      
         SPACE 1                                                                
         USING TAPDD,R6                                                         
FIXCAN2  OC    CANRATE,CANRATE                                                  
         BZ    XIT                                                              
         TM    TAPDPSTS,TAPDPBNP   UNLESS THIS IS A BNP,                        
         BO    *+10                                                             
         XC    TAPDPNH,TAPDPNH     CREAM THE P&H                                
         LA    R1,TAPDAMTS                                                      
         LA    R0,11                                                            
         SPACE 1                                                                
FIXCAN4  BAS   RE,FIXCAN6          CONVERT THE TAPDS                            
         LA    R1,4(R1)                                                         
         BCT   R0,FIXCAN4                                                       
         B     XIT                                                              
         SPACE 1                                                                
FIXCAN6  NTR1                      ADJUST FULLWORD BY CANRATE                   
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
*              FILL SORT RECORDS AT INVOICE LEVEL                               
         SPACE 3                                                                
FILLINV  NTR1                                                                   
         CLC   EXTMON,MONSTART     MUST MATCH 'MONTH' PERIOD RANGE              
         BL    NOCHECK                                                          
         CLC   EXTMON,THISMON                                                   
         BH    NOCHECK                                                          
         CLC   EXTAGY(4),=C'0601'  SPECIAL REQUEST FOR 0601                     
         BNE   FILLINVB                                                         
         CLI   EXTEST,C'R'                                                      
         BNE   NOCHECK                                                          
         SPACE 1                                                                
FILLINVB LA    R5,SORTIO                                                        
         USING SORTD,R5                                                         
         CLC   TIUSE,=C'HLD'                                                    
         BE    FILL2                                                            
         CLC   TIUSE,=C'SHL'                                                    
         BE    FILL2                                                            
         SPACE 1                                                                
FILL1    XC    SORTIO,SORTIO       RECORD 1/1                                   
         MVC   SORTAGY,EXTAGY                                                   
         MVC   SORTCLI,EXTCLI                                                   
         MVC   SORTEST,EXTEST                                                   
         MVC   SORTMED,EXTMED                                                   
         MVI   SORTREP,1           COMMERCIAL HEADER                            
         MVC   SORTCID,EXTCID                                                   
         MVC   SORTCOM,TICOM                                                    
         MVI   SORTSREP,1                                                       
         MVC   SORTTIT,EXTTITLE                                                 
         MVC   SORTPNAM,EXTPNAME                                                
         BAS   RE,SORTPUT                                                       
         SPACE 1                                                                
         XC    SORTIO,SORTIO       RECORD 1/2/1                                 
         MVC   SORTAGY,EXTAGY                                                   
         MVC   SORTCLI,EXTCLI                                                   
         MVC   SORTEST,EXTEST                                                   
         MVC   SORTMED,EXTMED                                                   
         MVI   SORTREP,1                                                        
         MVC   SORTCID,EXTCID                                                   
         MVC   SORTCOM,TICOM                                                    
         MVI   SORTSREP,2                                                       
         MVC   SORTINV,EXTINV                                                   
         MVI   SORTINVT,1          INVOICE HEADER                               
         MVC   SORTUNAM,EXTUNAM                                                 
         MVC   SORTUDET,EXTUDET                                                 
         MVC   SORTACDE,EXTACDE                                                 
         MVC   SORTCYC,EXTCYC                                                   
         BAS   RE,SORTPUT                                                       
         SPACE 1                                                                
         XC    SORTIO,SORTIO       RECORD 1/2/3                                 
         MVC   SORTAGY,EXTAGY                                                   
         MVC   SORTCLI,EXTCLI                                                   
         MVC   SORTEST,EXTEST                                                   
         MVC   SORTMED,EXTMED                                                   
         MVI   SORTREP,1                                                        
         MVC   SORTCID,EXTCID                                                   
         MVC   SORTCOM,TICOM                                                    
         MVI   SORTSREP,2                                                       
         MVC   SORTINV,EXTINV                                                   
         MVI   SORTINVT,3          INVOICE TOTAL                                
         MVC   SORTPAY,EXTPAY                                                   
         MVC   SORTPNH,EXTPNH                                                   
         TM    PGOPT,PGOPTTNH      IF NOT TNH OPTION                            
         BO    FILL1X                                                           
         OC    EXTGST,EXTGST       AND  WE HAVE GST                             
         BZ    *+10                                                             
         MVC   SORTPNH,EXTGST      MOVE IT INTO P&H                             
FILL1X   MVC   SORTTNH,EXTTNH      SET TAX AND HANDLING                         
         MVC   SORTAC,EXTAC                                                     
         MVC   SORTGRS,EXTGRS                                                   
         MVC   SORTMON,EXTMON                                                   
         MVC   SORTMONA,EXTMONA                                                 
         BAS   RE,SORTPUT                                                       
         SPACE 1                                                                
*                                  RECORD 1/2/4 BRAND TOTAL                     
         XC    SORTINV,SORTINV                                                  
         MVI   SORTINV,X'FF'                                                    
         MVI   SORTINVT,4          BRAND   TOTAL                                
         BAS   RE,SORTPUT                                                       
         B     FILL3                                                            
         EJECT                                                                  
*              FILL SORT RECORDS AT INVOICE LEVEL                               
*              REPORT 2 IS HOLDING FEE ANALYSIS                                 
         SPACE 3                                                                
FILL2    XC    SORTIO,SORTIO       RECORD 2/2/1                                 
         MVC   SORTAGY,EXTAGY                                                   
         MVC   SORTCLI,EXTCLI                                                   
         MVC   SORTEST,EXTEST                                                   
         MVC   SORTMED,EXTMED                                                   
         MVI   SORTREP,2                                                        
         MVC   SORTCID,EXTCID                                                   
         MVC   SORTCOM,TICOM                                                    
         MVI   SORTSREP,2                                                       
         MVC   SORTINV,EXTINV                                                   
         MVI   SORTINVT,1          COMMERCIAL HEADER                            
         MVC   SORTTIT,EXTTITLE                                                 
         MVC   SORTCYC,EXTCYC                                                   
         BAS   RE,SORTPUT                                                       
         SPACE 1                                                                
         XC    SORTIO,SORTIO       RECORD 2/2/3                                 
         MVC   SORTAGY,EXTAGY                                                   
         MVC   SORTCLI,EXTCLI                                                   
         MVC   SORTEST,EXTEST                                                   
         MVC   SORTMED,EXTMED                                                   
         MVI   SORTREP,2                                                        
         MVC   SORTCID,EXTCID                                                   
         MVC   SORTCOM,TICOM                                                    
         MVI   SORTSREP,2                                                       
         MVC   SORTINV,EXTINV                                                   
         MVI   SORTINVT,3          COMMERCIAL TOTAL                             
         MVC   SORTPAY,EXTPAY                                                   
         MVC   SORTPNH,EXTPNH                                                   
         TM    PGOPT,PGOPTTNH      IF NOT TNH OPTION                            
         BO    FILL2X                                                           
         OC    EXTGST,EXTGST       AND WE HAVE GST                              
         BZ    *+10                                                             
         MVC   SORTPNH,EXTGST      MOVE IT INTO P&H                             
FILL2X   MVC   SORTTNH,EXTTNH      SET TAX AND HANDLING                         
         MVC   SORTAC,EXTAC                                                     
         MVC   SORTGRS,EXTGRS                                                   
         MVC   SORTMON,EXTMON                                                   
         MVC   SORTMONA,EXTMONA                                                 
         BAS   RE,SORTPUT                                                       
         EJECT                                                                  
*              FILL SORT RECORDS AT INVOICE LEVEL                               
*              REPORT 3 IS PRODUCT RECAP                                        
         SPACE 3                                                                
FILL3    XC    SORTIO,SORTIO       RECORD 3/1                                   
         MVC   SORTAGY,EXTAGY                                                   
         MVC   SORTCLI,EXTCLI                                                   
         MVC   SORTEST,EXTEST                                                   
         MVC   SORTMED,EXTMED                                                   
         MVI   SORTREP,3                                                        
         MVI   SORTSREP,1          MONTH DETAILS                                
         MVC   SORTKMON,EXTMON                                                  
         MVC   SORTRPAY,EXTRPAY                                                 
         MVC   SORTRADJ,EXTRADJ                                                 
         MVC   SORTRTOT,EXTRTOT                                                 
         BAS   RE,SORTPUT                                                       
         SPACE 1                                                                
         MVI   SORTSREP,2          RECORD 3/2 - BRAND TOTALS                    
         MVI   SORTKMON,0                                                       
         BAS   RE,SORTPUT                                                       
         B     XIT                                                              
         DROP  R5                                                               
         SPACE 1                                                                
NOCHECK  MVI   TIMODE,PROCNOCK     TELL SYSIO TO IGNORE CHECKS                  
         B     XIT                                                              
         EJECT                                                                  
*              FILL SORT RECORDS AT CHECK LEVEL                                 
*              RATE ANALYSES                                                    
         SPACE 3                                                                
FILLCHEK NTR1                                                                   
         L     R6,TIAREC           CHECK FOR LIENS                              
         MVI   ELCODE,TACDELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING TACDD,R6                                                         
         TM    TACDSTAT,TACDSLIN                                                
         BO    XIT                                                              
         SPACE 1                                                                
         CLC   EXTMON,MONSTART     MUST MATCH 'MONTH' PERIOD RANGE              
         BL    NOCHECK                                                          
         CLC   EXTMON,THISMON                                                   
         BH    NOCHECK                                                          
         CLC   EXTAGY(4),=C'0601'  SPECIAL REQUEST FOR 0601                     
         BNE   FILLCHKB                                                         
         CLI   EXTEST,C'R'                                                      
         BNE   NOCHECK                                                          
         SPACE 1                                                                
FILLCHKB LA    R5,SORTIO                                                        
         USING SORTD,R5                                                         
         CLC   TIUSE,=C'HLD'                                                    
         BE    FILLCH2                                                          
         CLC   TIUSE,=C'SHL'                                                    
         BE    FILLCH2                                                          
         SPACE 1                                                                
         XC    SORTIO,SORTIO       RECORD 1/2/2                                 
         MVC   SORTAGY,EXTAGY                                                   
         MVC   SORTCLI,EXTCLI                                                   
         MVC   SORTEST,EXTEST                                                   
         MVC   SORTMED,EXTMED                                                   
         MVI   SORTREP,1                                                        
         MVC   SORTCID,EXTCID                                                   
         MVC   SORTCOM,TICOM                                                    
         MVI   SORTSREP,2                                                       
         MVC   SORTINV,EXTINV                                                   
         MVI   SORTINVT,2          RATE ANALYSIS                                
         MVC   SORTRATE,EXTRATE                                                 
         MVC   SORTCNT,=F'1'                                                    
         MVC   SORTACDE,EXTACDE                                                 
         BAS   RE,SORTPUT                                                       
         B     XIT                                                              
         SPACE 1                                                                
FILLCH2  XC    SORTIO,SORTIO       RECORD 2/2/2                                 
         MVC   SORTAGY,EXTAGY                                                   
         MVC   SORTCLI,EXTCLI                                                   
         MVC   SORTEST,EXTEST                                                   
         MVC   SORTMED,EXTMED                                                   
         MVI   SORTREP,2                                                        
         MVC   SORTCID,EXTCID                                                   
         MVC   SORTCOM,TICOM                                                    
         MVI   SORTSREP,2                                                       
         MVC   SORTINV,EXTINV                                                   
         MVI   SORTINVT,2                                                       
         MVC   SORTRATE,EXTRATE                                                 
         MVC   SORTCNT,=F'1'                                                    
         BAS   RE,SORTPUT                                                       
         B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
*              NOW HANDLE THE OUTPUT                                            
         SPACE 3                                                                
DOREST   NTR1                                                                   
         CLI   SORTFRST,C'Y'       GET OUT IF NO DATA                           
         BE    XIT                                                              
         L     R5,ASPOOLD                                                       
         USING SPOOLD,R5                                                        
         MVI   FORCEHED,C'Y'                                                    
         XC    PAGE,PAGE                                                        
         DROP  R5                                                               
         LA    R5,SORTIO                                                        
         USING SORTD,R5                                                         
         XC    SORTKEY,SORTKEY                                                  
         SPACE 1                                                                
DOREST2  GOTO1 MYSORTER,DMCB,=C'GET'                                            
         L     R2,DMCB+4                                                        
         LTR   R2,R2                                                            
         BNZ   DOREST4                                                          
         BAS   RE,DOREPORT         EOF - HANDLE LAST RECORD                     
         B     XIT                                                              
         SPACE 1                                                                
DOREST4  CLC   SORTKEY,0(R2)       KEYS WERE THE SAME                           
         BNE   DOREST6                                                          
         BAS   RE,MERGEM           SO MERGE RECORDS TOGETHER                    
         B     DOREST2                                                          
         SPACE 1                                                                
DOREST6  OC    SORTKEY,SORTKEY     KEYS WERE DIFFERENT                          
         BZ    *+8                 IF NOT FIRST TIME,                           
         BAS   RE,DOREPORT            HANDLE REPORTING                          
         SPACE 1                                                                
*                                  REFRESH NAMES                                
         MVC   LASTKEY,SORTKEY                                                  
         MVC   SORTIO,0(R2)                                                     
         SPACE 1                                                                
DOREST8  CLC   SORTKEY(L'SORTAGY),LASTKEY                                       
         BE    *+8                                                              
         BAS   RE,NEEDAY                                                        
         CLC   SORTKEY(L'SORTAGY+L'SORTCLI),LASTKEY                             
         BE    *+8                                                              
         BAS   RE,NEEDCL                                                        
         AP    OUTCOUNT,=P'1'                                                   
         B     DOREST2                                                          
         SPACE 1                                                                
OUTCOUNT DC    PL6'0'                                                           
         EJECT                                                                  
*              PRINT REPORT - CONTROL WHERE WE GO                               
         SPACE 3                                                                
DOREPORT NTR1                                                                   
         LA    R5,SORTIO                                                        
         USING SORTD,R5                                                         
         CLI   SORTREP,2                                                        
         BE    REP22                                                            
         CLI   SORTREP,3                                                        
         BE    REP30                                                            
         CLI   SORTSREP,1                                                       
         BE    REP11                                                            
         SPACE 1                                                                
REP12    CLI   SORTINVT,1                                                       
         BE    REP121                                                           
         CLI   SORTINVT,2                                                       
         BE    REP122                                                           
         CLI   SORTINVT,3                                                       
         BE    REP123                                                           
         B     REP124                                                           
         SPACE 1                                                                
REP22    CLI   SORTINVT,1                                                       
         BE    REP221                                                           
         CLI   SORTINVT,2                                                       
         BE    REP222                                                           
         B     REP223                                                           
         SPACE 1                                                                
REP30    CLI   SORTSREP,1                                                       
         BE    REP31                                                            
         CLI   SORTSREP,2                                                       
         BE    REP32                                                            
         DC    H'0'                                                             
         EJECT                                                                  
*              REPORT ROUTINES FOR REPORT 1/1                                   
*              CAST ANALYSIS FOR COMMERCIAL                                     
         SPACE 3                                                                
REP11    BAS   RE,NEEDPR           MAKE SURE WE HAVE PRODUCT                    
         CLI   LASTSREP,1          IF THIS IS THE FIRST                         
         BE    XIT                                                              
         L     R1,ASPOOLD                                                       
         USING SPOOLD,R1                                                        
         MVI   FORCEHED,C'Y'                                                    
         DROP  R1                                                               
         MVC   MYP+16(30),SORTTIT     SHOW TITLE AND CID                        
         MVC   MYP+48(12),SORTCID                                               
         SPACE 1                                                                
         L     R2,=A(CATENTS)      REPORT CATEGORY ANALYSIS                     
         USING CATPOOLD,R2                                                      
         LR    RE,R2                                                            
         LA    RF,1600                                                          
         XCEF                                                                   
         SPACE 1                                                                
         LA    RE,TIHOOK                                                        
         LA    RF,TIEND                                                         
         SR    RF,RE                                                            
         XCEF                                                                   
         MVC   TIACOMFC,ACOMFACS   USE SYSIO TO GET CAST                        
         LA    R1,CASTHOOK                                                      
         ST    R1,TIHOOK                                                        
         MVI   TIREAD,TLCACDQ      SET TO READ CAST                             
*        MVC   TIFAGY,SORTAGY      FOR THIS AGENCY                              
         MVC   TIFCOM,SORTCOM                     /COMMERCIAL                   
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
         SPACE 1                                                                
         L     R2,=A(CATENTS)      REPORT CATEGORY ANALYSIS                     
         USING CATPOOLD,R2                                                      
         SPACE 1                                                                
REPCAT2  CLI   0(R2),0                                                          
         BE    XIT                                                              
         MVC   MYP+59(3),POOLCAT   CATEGORY                                     
         LA    R3,MYP+69           POINT TO ON                                  
         CLC   POOLONOF,=C'OFF'                                                 
         BNE   *+8                                                              
         LA    R3,MYP+76                 OR OFF                                 
         EDIT  (4,POOLCNT),(3,0(R3))     DISPLAY NUMBER                         
         MVC   DUB,POOLOV                                                       
         L     R1,DUB                                                           
         SR    R0,R0                                                            
         D     R0,=F'100'                                                       
         EDIT  (R1),(3,MYP+84)           OVERSCALE (NO DECS)                    
         EDIT  (1,POOLDBL),(3,MYP+90)                                           
         BAS   RE,BLEND                                                         
         LA    R2,L'POOLENT(R2)                                                 
         B     REPCAT2                                                          
         SPACE 1                                                                
CASTHOOK NTR1                                                                   
         CLI   TIMODE,PROCREC      IF THERE IS SOMETHING INTERESTING            
         BNE   XIT                                                              
         SPACE 1                                                                
         XC    EXTOV,EXTOV                                                      
         MVI   EXTDBL,0                                                         
         L     R6,TIAREC           BROWSE CHECK                                 
         MVI   ELCODE,0                                                         
         BAS   RE,GETEL                                                         
         B     *+8                                                              
         SPACE 1                                                                
CST2     BAS   RE,NEXTEL                                                        
         BNE   POSTCAT                                                          
         CLI   0(R6),TAOPELQ                                                    
         BE    CSTOP                                                            
         CLI   0(R6),TACAELQ                                                    
         BE    CSTCA                                                            
         B     CST2                                                             
         SPACE 1                                                                
         USING TACAD,R6                                                         
CSTCA    MVC   EXTDBL,TACADBL      CAST DETAILS                                 
         NI    EXTDBL,X'0F'                                                     
         B     CST2                                                             
         SPACE 1                                                                
         USING TAOPD,R6                                                         
CSTOP    MVC   EXTOV,TAOPPCT                                                    
         B     CST2                                                             
         SPACE 1                                                                
POSTCAT  L     R2,=A(CATENTS)      POST CATEGORY ANALYSIS                       
         USING CATPOOLD,R2                                                      
         LA    R0,100              (MAX 100 ENTRIES)                            
         SPACE 1                                                                
CSTHK2   CLI   0(R2),0             FIND AN EMPTY ENTRY                          
         BE    CSTHK6                                                           
         CLC   TICAT,POOLCAT       ...OR A MATCH                                
         BNE   CSTHK4                                                           
         CLC   TIONOF,POOLONOF                                                  
         BNE   CSTHK4                                                           
         CLC   EXTOV,POOLOV                                                     
         BNE   CSTHK4                                                           
         CLC   EXTDBL,POOLDBL                                                   
         BNE   CSTHK4                                                           
         B     CSTHK6                                                           
         SPACE 1                                                                
CSTHK4   LA    R2,L'POOLENT(R2)                                                 
         BCT   R0,CSTHK2                                                        
         B     XIT                                                              
         SPACE 1                                                                
CSTHK6   MVC   POOLCAT,TICAT       CREATE OR ADD ENTRY                          
         MVC   POOLONOF,TIONOF                                                  
         MVC   POOLOV,EXTOV                                                     
         MVC   POOLDBL,EXTDBL                                                   
         L     R1,POOLCNT                                                       
         LA    R1,1(R1)                                                         
         ST    R1,POOLCNT                                                       
         B     XIT                                                              
         EJECT                                                                  
*              REPORT ROUTINES FOR REPORT 1/2                                   
         SPACE 3                                                                
REP121   CLI   LASTSREP,2          IF THIS IS THE FIRST                         
         BE    *+8                                                              
*        BAS   RE,HL12                DO THE LITTLE HEADINGS                    
         BAS   RE,HEADERS                                                       
         LA    R1,ASPOOLD          CHECK WE HAVE ROOM                           
         USING SPOOLD,R1                                                        
         CLI   LINE,54                                                          
         BL    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         DROP  R1                                                               
         MVC   PSTACK+15(16),SORTUNAM                                           
         OC    SORTCYC,SORTCYC                                                  
         BZ    REP121D                                                          
         SPACE 1                                                                
*              SHOW CYCLE          MM/DD-MM/DD(/YY)                             
         GOTO1 DATCON,DMCB,(1,SORTCYC),(10,PSTACK+32)                           
         MVI   PSTACK+37,C'-'                                                   
         GOTO1 DATCON,DMCB,(1,SORTCYC+3),(10,PSTACK+38)                         
         MVC   PSTACK+43(3),MYSPACES                                            
         SPACE 1                                                                
REP121D  MVI   PSTACK+66,C'#'      SHOW INVOICE NUMBER                          
         GOTO1 TINVCON,DMCB,SORTINV,MYP+67,DATCON                               
         CLI   SORTACDE,C'1'       EXPAND APPLY CODE                            
         BNE   *+10                                                             
         MVC   PSTACK2+15(12),=C'LESS SESSION'                                  
         CLI   SORTACDE,C'2'                                                    
         BNE   *+10                                                             
         MVC   PSTACK2+15(12),=C'LESS H.F.   '                                  
         MVC   PSTACK2+28(16),SORTUDET                                          
         B     XIT                                                              
         SPACE 1                                                                
REP122   CLC   LASTINV,SORTINV     IF THIS IS THE FIRST FOR INVOICE             
         BE    REP122B                                                          
         CLI   SORTACDE,C'1'       EXPAND APPLY CODE                            
         BNE   *+10                                                             
         MVC   PSTACK2+15(12),=C'LESS SESSION'                                  
         CLI   SORTACDE,C'2'                                                    
         BNE   *+10                                                             
         MVC   PSTACK2+15(12),=C'LESS H.F.   '                                  
         SPACE 1                                                                
REP122B  EDIT  (4,SORTCNT),(3,MYP+45)     SHOW NUMBER AT THIS RATE              
         MVI   MYP+48,C'@'                                                      
         EDIT  (4,SORTRATE),(11,MYP+49),2,MINUS=YES,ZERO=BLANK                  
         BAS   RE,BLEND                                                         
         B     XIT                                                              
         SPACE 1                                                                
*                                  INVOICE TOTALS                               
REP123   LA    R1,ASPOOLD          CHECK WE HAVE ROOM                           
         USING SPOOLD,R1                                                        
         CLI   LINE,54                                                          
         BL    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         DROP  R1                                                               
         SPACE 1                                                                
         MVC   MYP+48(11),=C'-----------'                                       
         BAS   RE,BLEND                                                         
         SPACE 1                                                                
         EDIT  (4,SORTPAY),(11,MYP+49),2,MINUS=YES,ZERO=BLANK                   
         EDIT  (4,SORTPNH),(9,MYP+60),2,MINUS=YES,ZERO=BLANK                    
         LA    R2,MYP+69                                                        
         TM    PGOPT,PGOPTTNH                                                   
         BZ    REP123S                                                          
         EDIT  (4,SORTTNH),(9,(R2)),2,MINUS=YES,ZERO=BLANK                      
         LA    R2,9(R2)                                                         
*                                                                               
REP123S  EDIT  (4,SORTAC),(9,(R2)),2,MINUS=YES,ZERO=BLANK                       
         LA    R2,9(R2)                                                         
         EDIT  (4,SORTGRS),(11,(R2)),2,MINUS=YES,ZERO=BLANK                     
         LA    R2,12(R2)                                                        
         ZIC   R1,SORTMON                                                       
         BCTR  R1,0                                                             
         MH    R1,=H'3'                                                         
         LA    R1,LITMONS(R1)                                                   
         MVC   0(3,R2),0(R1)                                                    
         LA    R2,3(R2)                                                         
         MVC   0(1,R2),SORTMONA                                                 
         MVI   MYSPING,2                                                        
         BAS   RE,BLEND                                                         
         B     XIT                                                              
         SPACE 1                                                                
REP124   LA    R2,MYP+69                                                        
         TM    PGOPT,PGOPTTNH                                                   
         BZ    *+8                                                              
         LA    R2,MYP+78                                                        
         MVC   0(6,R2),=C'TOTAL:'                                               
         LA    R2,9(R2)                                                         
         EDIT  (4,SORTGRS),(11,(R2)),2,MINUS=YES,ZERO=BLANK                     
         BAS   RE,BLEND                                                         
         B     XIT                                                              
         EJECT                                                                  
*              REPORT ROUTINES FOR REPORT 2/2 & 3                               
         SPACE 3                                                                
REP221   CLI   LASTREP,2           IF THIS IS THE FIRST                         
         BE    REP221B                                                          
         BAS   RE,NEEDPR           MAKE SURE WE HAVE PRODUCT                    
         L     R1,ASPOOLD                                                       
         USING SPOOLD,R1                                                        
         MVI   FORCEHED,C'Y'          SKIP TO NEW PAGE                          
         DROP  R1                                                               
         SPACE 1                                                                
REP221B  MVC   PSTACK+15(12),SORTCID                                            
         MVC   PSTACK2+15(30),SORTTIT                                           
         OC    SORTCYC,SORTCYC                                                  
         BZ    REP221D                                                          
         SPACE 1                                                                
*              SHOW CYCLE          MM/DD-MM/DD/YY                               
         GOTO1 DATCON,DMCB,(1,SORTCYC),(10,PSTACK+28)                           
         MVI   PSTACK+33,C'-'                                                   
         GOTO1 DATCON,DMCB,(1,SORTCYC+3),(10,PSTACK+34)                         
         SPACE 1                                                                
REP221D  MVI   PSTACK+66,C'#'      SHOW INVOICE NUMBER                          
         GOTO1 TINVCON,DMCB,SORTINV,MYP+67,DATCON                               
         B     XIT                                                              
         SPACE 1                                                                
REP222   B     REP122              USE RATE ANALYSIS ABOVE                      
         SPACE 1                                                                
REP223   B     REP123              USE TOTAL ROUTINE ABOVE                      
         EJECT                                                                  
*              REPORT ROUTINES FOR REPORT 3                                     
         SPACE 3                                                                
REP31    CLI   LASTREP,3           IF THIS IS THE FIRST                         
         BE    REP311                                                           
         L     R1,ASPOOLD                                                       
         USING SPOOLD,R1                                                        
         MVI   FORCEHED,C'Y'                                                    
         DROP  R1                                                               
         MVC   MYP+15(6),=C'BRAND:'  SHOW BRAND                                 
         MVC   MYP+23(30),PRDNAME                                               
         MVI   MYSPING,3                                                        
         BAS   RE,BLEND                                                         
         SPACE 1                                                                
         MVC   MYP+52(8),=C'PAYMENTS'     AND SOME HEADINGS                     
         MVC   MYP+65(11),=C'ADJUSTMENTS'                                       
         MVC   MYP+83(5),=C'TOTAL'                                              
         MVI   MYSPING,2                                                        
         BAS   RE,BLEND                                                         
         SPACE 1                                                                
         ZIC   R0,SORTKMON                                                      
         ZIC   R1,MONSTART                                                      
         BAS   RE,DOBIGS           MAYBE SOME MONTHS TO SHOW                    
         B     REP312                                                           
         SPACE 1                                                                
REP311   ZIC   R0,SORTKMON         COULD BE A GAP IN THE MONTHS                 
         ZIC   R1,LSTRCMON         CHECK V LAST +1                              
         LA    R1,1(R1)                                                         
         BAS   RE,DOBIGS           MAYBE SOME MONTHS TO SHOW                    
         SPACE 1                                                                
REP312   ZIC   R1,SORTKMON         BIG MONTH                                    
         MVC   LSTRCMON,SORTKMON   (SAVE LAST MONTH PRINTED)                    
         BAS   RE,EDBIG                                                         
         EDIT  (4,SORTRPAY),(12,MYP+49),2,MINUS=YES,ZERO=BLANK                  
         EDIT  (4,SORTRADJ),(12,MYP+65),2,MINUS=YES,ZERO=BLANK                  
         EDIT  (4,SORTRTOT),(12,MYP+81),2,MINUS=YES,ZERO=BLANK                  
         MVI   MYSPING,2                                                        
         BAS   RE,BLEND                                                         
         B     XIT                                                              
         SPACE 1                                                                
REP32    ZIC   R0,MONEND           SHOW MONTHS 'TIL PERIOD END                  
         ZIC   R1,LSTRCMON         (LAST ONE PRINTED)                           
         AH    R0,=H'1'                                                         
         AH    R1,=H'1'                                                         
         BAS   RE,DOBIGS                                                        
         SPACE 1                                                                
         MVC   MYP+29(11),=C'BRAND TOTAL'                                       
         EDIT  (4,SORTRPAY),(12,MYP+49),2,MINUS=YES,ZERO=BLANK                  
         EDIT  (4,SORTRADJ),(12,MYP+65),2,MINUS=YES,ZERO=BLANK                  
         EDIT  (4,SORTRTOT),(12,MYP+81),2,MINUS=YES,ZERO=BLANK                  
         MVI   MYSPING,2                                                        
         BAS   RE,BLEND                                                         
         L     R1,ASPOOLD                                                       
         USING SPOOLD,R1                                                        
         XC    PAGE,PAGE           RESET PAGE NUMBER                            
         DROP  R1                                                               
         B     XIT                                                              
         SPACE 1                                                                
DOBIGS   NTR1                                                                   
         SPACE 1                                                                
DOBIGS2  CR    R1,R0                                                            
         BE    XIT                                                              
         BL    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,EDBIG                                                         
         MVI   MYSPING,2                                                        
         BAS   RE,BLEND                                                         
         LA    R1,1(R1)                                                         
         B     DOBIGS2                                                          
EDBIG    NTR1                                                                   
         BCTR  R1,0                                                             
         MH    R1,=H'9'                                                         
         LA    R1,BIGMONS(R1)                                                   
         MVC   MYP+35(9),0(R1)                                                  
         B     XIT                                                              
         EJECT                                                                  
*              MERGE RECORDS TOGETHER - CONTROL MERGE                           
         SPACE 3                                                                
*                                  R2=A(SORT RECORD)                            
*                                  R5=A(CURRENT RECORD IN SORTIO)               
         SPACE 1                                                                
MERGEM   NTR1                                                                   
         USING SORTD,R5                                                         
         CLI   SORTREP,2                                                        
         BE    MERGE2                                                           
         CLI   SORTREP,3                                                        
         BE    MERGE3                                                           
         SPACE 1                                                                
         CLI   SORTSREP,1                                                       
         BE    XIT                                                              
         CLI   SORTINVT,2                                                       
         BE    MERGECNT                                                         
         B     MERGECSH                                                         
         SPACE 1                                                                
MERGE2   CLI   SORTINVT,2                                                       
         BE    MERGECNT                                                         
         B     MERGECSH                                                         
         EJECT                                                                  
*              MERGE RECORDS TOGETHER - ADD THE VALUES                          
         SPACE 3                                                                
MERGECNT LA    R3,SORTCNT-SORTKEY                                               
         BAS   RE,ADDEM                                                         
         B     XIT                                                              
         SPACE 1                                                                
MERGECSH LA    R3,SORTPAY-SORTKEY                                               
         BAS   RE,ADDEM                                                         
         LA    R3,SORTPNH-SORTKEY                                               
         BAS   RE,ADDEM                                                         
         LA    R3,SORTTNH-SORTKEY                                               
         BAS   RE,ADDEM                                                         
         LA    R3,SORTAC-SORTKEY                                                
         BAS   RE,ADDEM                                                         
         LA    R3,SORTGRS-SORTKEY                                               
         BAS   RE,ADDEM                                                         
         B     XIT                                                              
         SPACE 1                                                                
MERGE3   LA    R3,SORTRPAY-SORTKEY                                              
         BAS   RE,ADDEM                                                         
         LA    R3,SORTRADJ-SORTKEY                                              
         BAS   RE,ADDEM                                                         
         LA    R3,SORTRTOT-SORTKEY                                              
         BAS   RE,ADDEM                                                         
         B     XIT                                                              
         SPACE 1                                                                
ADDEM    L     R1,0(R2,R3)                                                      
         A     R1,0(R5,R3)                                                      
         ST    R1,0(R5,R3)                                                      
         BR    RE                                                               
         EJECT                                                                  
*              EDIT DETAILS OF PAYMENT                                          
         SPACE 3                                                                
EDITDET  NTR1                                                                   
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
*              ROUTINES TO ENSURE SUB RECORDS AROUND                            
         SPACE 3                                                                
NEEDAY   NTR1                                                                   
         LA    R5,SORTIO                                                        
         USING SORTD,R5                                                         
         XC    NEEDKEY,NEEDKEY     ENSURE AGENCY AROUND                         
         LA    R4,NEEDKEY                                                       
         USING TLAYD,R4                                                         
         MVI   TLAYCD,TLAYCDQ                                                   
         MVC   TLAYAGY,SORTAGY                                                  
         BAS   RE,NEEDREC                                                       
         MVC   AGYNAME,NEEDNAME                                                 
         B     XIT                                                              
         SPACE 1                                                                
NEEDCL   NTR1                                                                   
         LA    R5,SORTIO                                                        
         USING SORTD,R5                                                         
         XC    NEEDKEY,NEEDKEY     ENSURE RECORD AROUND                         
         LA    R4,NEEDKEY                                                       
         USING TLCLD,R4                                                         
         MVI   TLCLCD,TLCLCDQ                                                   
         MVC   TLCLCLI,SORTCLI                                                  
         BAS   RE,NEEDREC          TRY FOR GLOBAL CLIENT                        
         MVC   CLINAME,NEEDNAME                                                 
         CLI   NEEDHIT,C'Y'                                                     
         BE    XIT                                                              
         MVC   TLCLAGY,SORTAGY                                                  
         BAS   RE,NEEDREC          THEN FOR AGENCY CLIENT                       
         MVC   CLINAME,NEEDNAME                                                 
         B     XIT                                                              
         SPACE 1                                                                
NEEDPR   NTR1                                                                   
         LA    R5,SORTIO                                                        
         USING SORTD,R5                                                         
         CLC   SORTCOM,THISCOM     GET COMMERCIAL ON FIRST FOR COMM             
         BE    XIT                                                              
         MVC   THISCOM,SORTCOM                                                  
         MVC   PRDNAME,MYSPACES                                                 
         XC    KEY,KEY             ENSURE RECORD AROUND                         
         LA    R4,KEY                                                           
         USING TLCOPD,R4                                                        
         MVI   TLCOPCD,TLCOCCDQ                                                 
         MVC   TLCOCCOM,SORTCOM                                                 
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         USING TLCOD,R4                                                         
         MVC   COMMPRD,TLCOPRD     GET PRODUCT CODE FROM COMM                   
         CLI   COMMPRD,C' '                                                     
         BH    NEEDPR6                                                          
         LR    R6,R4               CHECK FOR PRODUCT NAME                       
         MVI   ELCODE,TAFNELQ                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
         SPACE 1                                                                
NEEDPR2  BAS   RE,NEXTEL                                                        
         BNE   NEEDPR4                                                          
         USING TAFND,R6                                                         
         CLI   TAFNTYPE,TAFNTPRD                                                
         BNE   NEEDPR2                                                          
         ZIC   R1,TAFNLEN                                                       
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     XIT                                                              
         MVC   PRDNAME(0),TAFNNAME                                              
         SPACE 1                                                                
NEEDPR4  LR    R6,R4               CHECK FOR PRODUCT CODE EL                    
         MVI   ELCODE,TAPRELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING TAPRD,R6                                                         
         MVC   COMMPRD,TAPRPRD                                                  
         SPACE 1                                                                
NEEDPR6  XC    NEEDKEY,NEEDKEY     TRY FOR SPECIFIC PRODUCT RECORD              
         LA    R4,NEEDKEY                                                       
         USING TLPRD,R4                                                         
         MVI   TLPRCD,TLPRCDQ                                                   
         MVC   TLPRPRD,COMMPRD                                                  
         MVC   TLPRAGY,SORTAGY                                                  
         MVC   TLPRCLI,SORTCLI                                                  
         BAS   RE,NEEDREC                                                       
         MVC   PRDNAME,NEEDNAME                                                 
         CLI   NEEDHIT,C'Y'                                                     
         BE    XIT                                                              
         SPACE 1                                                                
         XC    NEEDKEY,NEEDKEY     THEN TRY FOR GLOBAL PRODUCT                  
         LA    R4,NEEDKEY                                                       
         USING TLPRD,R4                                                         
         MVI   TLPRCD,TLPRCDQ                                                   
         MVC   TLPRPRD,COMMPRD                                                  
         BAS   RE,NEEDREC                                                       
         MVC   PRDNAME,NEEDNAME                                                 
         CLI   NEEDHIT,C'Y'                                                     
         BE    XIT                                                              
         MVC   PRDNAME,MYSPACES                                                 
         B     XIT                                                              
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
         BNE   XIT                                                              
         SPACE 1                                                                
         L     R1,8(R2)            NO - PICK UP N'LAST ENTRY                    
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
         MVC   NEEDNAME,MYSPACES                                                
         MVI   ELCODE,TANAELQ                                                   
         LR    R6,R4                                                            
         BAS   RE,GETEL                                                         
         MVC   ELCODE,SAVEEL       RESTORE PRESENT ELCODE                       
         BNE   NOGOOD                                                           
         USING TANAD,R6                                                         
         ZIC   R1,TANALEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     ITSFINE                                                          
         MVC   NEEDNAME(0),TANANAME                                             
         SPACE 1                                                                
GETSHORT NTR1                                                                   
*                                  R4=A(RECORD) TO NEEDSHRT                     
         MVC   SAVEEL,ELCODE       SAVE PRESENT ELCODE                          
         MVI   ELCODE,TASNELQ      SHORT NAME                                   
         LR    R6,R4                                                            
         BAS   RE,GETEL                                                         
         MVC   NEEDSHRT,MYSPACES                                                
         MVC   ELCODE,SAVEEL                                                    
         BNE   XIT                                                              
         USING TASND,R6                                                         
         ZIC   R1,TASNLEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     XIT                                                              
         MVC   NEEDSHRT(0),TASNAME                                              
         SPACE 1                                                                
ITSFINE  SR    R1,R1                                                            
         B     *+8                                                              
         SPACE 1                                                                
NOGOOD   LA    R1,1                                                             
         LTR   R1,R1                                                            
         B     XIT                                                              
         EJECT                                                                  
*              TRACING ROUTINES                                                 
         SPACE 3                                                                
TRACEINP NTR1                                                                   
         L     R6,TIAMAIN                                                       
         MVC   RECTYPE,=CL16'INVOICE'                                           
         BAS   RE,TRACEREC                                                      
         L     R6,TIAREC                                                        
         MVC   RECTYPE,=CL16'CHECK'                                             
         BAS   RE,TRACEREC                                                      
         B     XIT                                                              
         SPACE 1                                                                
TRACEREC NTR1                                                                   
         MVI   MYP,X'BF'                                                        
         MVC   MYP+1(131),MYP                                                   
         MVC   MYP+60(16),RECTYPE                                               
         BAS   RE,SPLAT                                                         
         LA    R2,40                                                            
         BAS   RE,TRACEL                                                        
         MVI   ELCODE,0                                                         
         BAS   RE,GETEL                                                         
         B     *+8                                                              
         SPACE 1                                                                
TRACERC2 BAS   RE,NEXTEL                                                        
         BNE   XIT                                                              
         ZIC   R2,1(R6)                                                         
         BAS   RE,TRACEL                                                        
         B     TRACERC2                                                         
         SPACE 1                                                                
TRACEM   NTR1                                                                   
         MVI   MYP,X'BF'                                                        
         MVC   MYP+1(131),MYP                                                   
         MVC   MYP+60(16),RECTYPE                                               
         BAS   RE,SPLAT                                                         
         LA    R6,SORTIO                                                        
         LA    R2,132                                                           
         BAS   RE,TRACEL                                                        
         B     XIT                                                              
         SPACE 1                                                                
TRACEL   NTR1                                                                   
*                                  R2=LENGTH, R6=ADDRESS                        
         MVC   MYP,MYSPACES                                                     
         LR    R1,R2                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MYP(0),0(R6)                                                     
         OC    MYP,MYSPACES                                                     
         GOTO1 HEXOUT,DMCB,(R6),MYP3,132,=C'SEP'                                
         LR    R1,R2                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MYP2(0),MYP3                                                     
         MVC   MYP3,MYSPACES                                                    
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MYP3(0),MYP4                                                     
         MVC   MYP4,MYSPACES                                                    
         BAS   RE,SPLAT                                                         
         BAS   RE,SPLAT                                                         
         B     XIT                                                              
         SPACE 1                                                                
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
*              SORT UTILITIES                                                   
         SPACE 3                                                                
SORTPUT  NTR1                                                                   
         MVC   RECTYPE,=CL16'SORT INPUT'                                        
         CLC   EXTCID(8),TRACECID                                               
         BNE   *+8                                                              
         BAS   RE,TRACEM                                                        
         CLI   SORTFRST,C'Y'                                                    
         BNE   SORTPUT2                                                         
         GOTO1 MYSORTER,DMCB,SORTCARD,RECCARD                                   
         MVI   SORTFRST,C'N'                                                    
         SPACE 1                                                                
SORTPUT2 GOTO1 MYSORTER,DMCB,=C'PUT',SORTIO                                     
         B     XIT                                                              
         SPACE 1                                                                
         B     XIT                                                              
         DS    0F                                                               
LASTSKEY DC    XL64'00'                                                         
SORTFRST DC    C'Y'                                                             
SORTCARD DC    CL80'SORT FIELDS=(1,64,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=(144)'                                 
         EJECT                                                                  
*              ODD ROUTINES                                                     
         SPACE 3                                                                
YES      SR    R1,R1                                                            
         B     *+8                                                              
         SPACE 1                                                                
NO       LA    R1,1                                                             
         LTR   R1,R1                                                            
         B     XIT                                                              
         SPACE 1                                                                
BLEND    NTR1                                                                   
         BAS   RE,HEADERS                                                       
         L     R5,ASPOOLD                                                       
         USING SPOOLD,R5                                                        
         MVC   P,MYP                                                            
*                                                                               
         CLC   PSTACK+16(16),MYSPACES                                           
         BNH   *+10                                                             
         MVC   P+16(16),MYSPACES                                                
*                                                                               
         OC    P,PSTACK                                                         
         BAS   RE,SPOOLEM                                                       
         MVC   MYP,MYSPACES                                                     
         MVC   PSTACK,PSTACK2                                                   
         MVC   PSTACK2,MYSPACES                                                 
         B     XIT                                                              
         SPACE 1                                                                
SPLAT    NTR1                                                                   
         L     R5,ASPOOLD                                                       
         USING SPOOLD,R5                                                        
         MVC   P,MYP                                                            
         MVC   P2,MYP2                                                          
         MVC   P3,MYP3                                                          
         MVC   P4,MYP4                                                          
         BAS   RE,SPOOLEM                                                       
         DROP  R5                                                               
         BAS   RE,MYCLEAR                                                       
         B     XIT                                                              
         SPACE 1                                                                
SPOOLEM  NTR1                                                                   
         L     R5,ASPOOLD                                                       
         USING SPOOLD,R5                                                        
         MVC   SPACING,MYSPING                                                  
         GOTO1 SPOOL,DMCB,(R5)                                                  
         MVI   MYSPING,1                                                        
         B     XIT                                                              
         SPACE 1                                                                
MYCLEAR  NTR1                                                                   
         MVI   MYP,C' '                                                         
         MVC   MYP+1(131),MYP                                                   
         MVC   MYP2,MYP                                                         
         MVC   MYP3,MYP                                                         
         MVC   MYP4,MYP                                                         
         MVC   MYSPACES,MYP                                                     
         B     XIT                                                              
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              HEADLINES ETC                                                    
         SPACE 3                                                                
HOOK     NTR1                                                                   
         L     R5,ASPOOLD                                                       
         USING SPOOLD,R5                                                        
*                                  NOT DOING ANYTHING HERE YET                  
*                                  HEADLINES ARE COVERED BELOW                  
         DROP  R5                                                               
         XIT1                                                                   
         SPACE 1                                                                
HEADERS  NTR1                                                                   
         L     R5,ASPOOLD                                                       
         USING SPOOLD,R5                                                        
         LA    R4,SORTIO                                                        
         USING SORTD,R4                                                         
         L     R6,ATWA                                                          
         USING CONHEADH-64,R6                                                   
         CLI   FORCEHED,C'Y'       CHECK IF WE NEED TO DO HEADLINES             
         BE    HL2                                                              
         CLI   LINE,56                                                          
         BL    XIT                                                              
         MVI   FORCEHED,C'Y'                                                    
         SPACE 1                                                                
HL2      BAS   RE,SPOOLEM                                                       
         MVC   P+15(8),=C'DDS-TAPG'                                             
         MVC   P+33(42),=C'TV  COMMERCIAL  PRODUCTION  &&  REUSE  FEES'         
         MVI   MYSPING,3                                                        
         BAS   RE,SPOOLEM                                                       
         MVC   P+15(5),=C'PAGE:'                                                
         EDIT  (2,PAGE),(2,P+21),FILL=0                                         
         MVC   P+33(5),=C'RADIO'                                                
         MVC   P+44(10),=C'TELEVISION'                                          
         LA    R1,P+31                                                          
*                                                                               
         CLI   SORTMED,C'R'        MARK MEDIA WITH AN X                         
         BE    *+8                                                              
         LA    R1,P+42                                                          
         MVI   0(R1),C'X'                                                       
         MVC   P+63(4),=C'TIME'                                                 
         MVC   P+75(6),=C'TALENT'                                               
         MVC   P+85(8),=C'ESTIMATE'                                             
         MVI   MYSPING,2                                                        
         BAS   RE,SPOOLEM                                                       
         SPACE 1                                                                
         MVC   P+15(12),=C'ESTIMATE NO:'                                        
         MVC   P+28(15),SORTEST                                                 
         MVC   P+62(7),=C'AGENCY:'                                              
         MVC   P+70(30),AGYNAME                                                 
         MVI   MYSPING,2                                                        
         BAS   RE,SPOOLEM                                                       
         SPACE 1                                                                
         MVC   P+15(5),=C'DATE:'                                                
         MVC   P+21(8),SPLRUN                                                   
         MVC   P+62(7),=C'PERIOD:'                                              
         MVC   P+70(15),=C'JANUARY - JUNE '                                     
         CLI   MONSTART,7                                                       
         BNE   *+10                                                             
         MVC   P+70(15),=C'JULY - DECEMBER'                                     
         MVC   P+86(4),OPTYEAR     SHOW FOUR CHARACTER DISPLAYABLE YEAR         
         MVI   MYSPING,2                                                        
         BAS   RE,SPOOLEM                                                       
         SPACE 1                                                                
         MVC   P+15(7),=C'CLIENT:'                                              
         MVC   P+23(30),CLINAME                                                 
         MVC   P+62(6),=C'BRAND:'                                               
         MVC   P+70(30),PRDNAME                                                 
         MVI   MYSPING,2                                                        
         BAS   RE,SPOOLEM                                                       
         SPACE 1                                                                
         MVC   P+75(11),=C'TO INDICATE'                                         
         MVI   MYSPING,2                                                        
         BAS   RE,SPOOLEM                                                       
         SPACE 1                                                                
         MVC   P+18(25),=C'THIS SUPERCEDES ESTIMATE:'                           
         MVC   P+55(6),=C'DATED:'                                               
         MVC   P+62(8),SPLPREV                                                  
         MVC   P+75(12),=C'NET INCREASE'                                        
         MVI   MYSPING,2                                                        
         BAS   RE,SPOOLEM                                                       
         SPACE 1                                                                
         BAS   RE,PRDOTLN          PRINT DOTTED LINE                            
         SPACE 1                                                                
         CLI   SORTREP,2                                                        
         BE    HL20                                                             
         CLI   SORTREP,3                                                        
         BE    HL30                                                             
         SPACE 1                                                                
*                                  HEADERS FOR REPORT 1                         
         CLI   SORTSREP,1                                                       
         BNE   HL122                                                            
         MVC   P+16(10),=C'COMMERCIAL'                                          
         MVC   P+69(25),=C'  ON     OFF   OVER   DBL'                           
         BAS   RE,SPOOLEM                                                       
         SPACE 1                                                                
         MVC   P+20(2),=C'ID'                                                   
         MVC   P+48(15),=C'NUMBER     CAST'                                     
         MVC   P+69(25),=C'CAMERA CAMERA SCALE%     '                           
         BAS   RE,SPOOLEM                                                       
         B     XIT                                                              
         DROP  R6                                                               
         SPACE 1                                                                
HL12     NTR1                                                                   
         L     R5,ASPOOLD          HEADERS FOR REPORT 1/2                       
         BAS   RE,PRDOTLN          PRINT DOTTED LINE                            
         SPACE 1                                                                
HL122    MVC   P+20(3),=C'USE'                                                  
         MVC   P+35(5),=C'CYCLE'                                                
         MVC   P+44(4),=C'USES'                                                 
         MVC   P+49(18),=C'FEES   NET     P&&H'                                 
         LA    R2,P+72                                                          
         TM    PGOPT,PGOPTTNH                                                   
         BZ    *+14                                                             
         MVC   0(3,R2),=C'T&&H'                                                 
         LA    R2,9(R2)                                                         
*                                                                               
         MVC   0(3,R2),=C'A/C'                                                  
         LA    R2,9(R2)                                                         
         MVC   0(14,R2),=C'GROSS  PAY MTH'                                      
         MVI   MYSPING,2                                                        
         BAS   RE,SPOOLEM                                                       
         B     XIT                                                              
         SPACE 1                                                                
*                                  HEADERS FOR REPORT 2                         
HL20     MVC   P+15(30),=C'HOLDING FEES HOLDING FEE CYCLE'                      
         MVC   P+49(18),=C'FEES   NET     P&&H'                                 
*                                                                               
         LA    R2,P+72                                                          
         TM    PGOPT,PGOPTTNH                                                   
         BZ    *+14                                                             
         MVC   0(3,R2),=C'T&&H'                                                 
         LA    R2,9(R2)                                                         
*                                                                               
         MVC   0(3,R2),=C'A/C'                                                  
         LA    R2,9(R2)                                                         
         MVC   0(14,R2),=C'GROSS  PAY MTH'                                      
         MVI   MYSPING,2                                                        
         BAS   RE,SPOOLEM                                                       
         B     XIT                                                              
         SPACE 1                                                                
*                                  HEADERS FOR REPORT 3                         
HL30     MVI   MYSPING,3                                                        
         BAS   RE,SPOOLEM                                                       
         MVC   P+32(23),=C'* * *  PAYMENT  SUMMARY'                             
         MVC   P+57(23),=C'RECAPITULATION  * * *  '                             
         MVI   MYSPING,2                                                        
         BAS   RE,SPOOLEM                                                       
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PRINT DOTTED LINE                                     
         SPACE                                                                  
PRDOTLN  NTR1                                                                   
         MVI   P+15,C'-'                                                        
         LA    R2,78                                                            
         TM    PGOPT,PGOPTTNH                                                   
         BZ    *+8                                                              
         LA    R2,87                                                            
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   P+16(0),P+15                                                     
         BAS   RE,SPOOLEM                                                       
         B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
*              ROUTINE HANDLES DATA TRACES                                      
*                                                                               
MYTRACE  NTR1                                                                   
         CLI   TRACOPT,C'Y'        TEST TRACE ENABLED                           
         BNE   XIT                                                              
         LM    R2,R4,0(R1)         A(LITERAL), L(DATA), A(DATA)                 
         ZIC   RF,0(R1)            L'LITERAL                                    
         GOTO1 TRACE,DMCB,(R4),(R3),(R2),(RF)                                   
         B     XIT                                                              
         EJECT                                                                  
*              LTORG ETC                                                        
         SPACE 3                                                                
*                                  COUNTS                                       
TRACOUNT DC    PL6'0'                                                           
CHKCOUNT DC    PL6'0'                                                           
RECCOUNT DC    PL6'0'                                                           
         DS    0D                                                               
THSCOUNT DC    PL8'0'                                                           
REPCOUNT DC    PL6'0'                                                           
TPCH     DC    H'1878'                                                          
         SPACE 1                                                                
MYSPING  DC    AL1(1)                                                           
PSTACK   DC    CL132' '                                                         
PSTACK2  DC    CL132' '                                                         
LITMONS  DC    C'JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC'                          
BIGMONS  DS    0H                                                               
         DC    CL9'JANUARY'                                                     
         DC    CL9'FEBRUARY'                                                    
         DC    CL9'MARCH'                                                       
         DC    CL9'APRIL'                                                       
         DC    CL9'MAY'                                                         
         DC    CL9'JUNE'                                                        
         DC    CL9'JULY'                                                        
         DC    CL9'AUGUST'                                                      
         DC    CL9'SEPTEMBER'                                                   
         DC    CL9'OCTOBER'                                                     
         DC    CL9'NOVEMBER'                                                    
         DC    CL9'DECEMBER'                                                    
         LTORG                                                                  
         EJECT                                                                  
*              OTHER AREAS NOT DIRECTLY ADDRESSABLE                             
         SPACE 3                                                                
CATENTS  DS    0D                                                               
         DC    1600X'00'           100 ENTRIES - SEE CATPOOLD                   
         SPACE 3                                                                
         EJECT                                                                  
*                                                                               
*              VALIDATE A FILTER EXPRESSION                                     
*              INPUT: R2=A(AGENCY HEADER)                                       
*                                                                               
         SPACE 1                                                                
SPECFILT NTR1  BASE=*,LABEL=*                                                   
         CLI   5(R2),0             ANY DATA                                     
         BE    SFILTNO                                                          
         OI    6(R2),X'80'                                                      
         GOTO1 ANY                 PUT INTO WORK                                
         CLC   WORK(2),=C'-@'      CHECK FOR NEGATIVE LIST                      
         BE    NEGLIST                                                          
         CLC   WORK(2),=C'@-'                                                   
         BE    NEGLIST                                                          
         CLI   WORK,C'@'           CHECK FOR POSITIVE LIST                      
         BNE   SFILTNO                                                          
         SPACE 1                                                                
         LA    R5,WORK+1           POSITIVE LIST                                
         B     VALLIST                                                          
         SPACE 1                                                                
NEGLIST  LA    R5,WORK+2           NEGATIVE LIST                                
         SPACE 1                                                                
VALLIST  XC    KEY,KEY             CHECK LIST EXISTS                            
         LA    R3,KEY                                                           
         USING TLGLD,R3                                                         
         MVI   TLGLCD,TLGLCDQ                                                   
         MVI   TLGLTYPE,TLGLTYPF                                                
         MVC   TLGLLST,0(R5)                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(32),KEYSAVE                                                  
         BNE   SPFILERR                                                         
         B     SFILTYES                                                         
*                                                                               
SFILTYES XR    RC,RC                                                            
SFILTNO  LTR   RC,RC                                                            
         XIT1                                                                   
         DROP  R3                                                               
*                                                                               
SPFILERR MVI   ERROR,INVALID                                                    
         GOTO1 ERREX2                                                           
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE SETS UP AGYTAB IF AGENCY IS AN FLIST                     
*                                                                               
AGYFLIST NTR1  BASE=*,LABEL=*                                                   
         L     RE,ATWA                                                          
         USING CONHEADH-64,RE                                                   
         LA    R2,SPLAGYH          IF FIELD IS A POSITIVE OR                    
         DROP  RE                                                               
         BRAS  RE,POSFLIST         NEGATIVE FLIST                               
         BE    AFL10                                                            
         BRAS  RE,NEGFLIST                                                      
         BE    AFL20                                                            
         B     AFLXX                                                            
*                                                                               
AFL10    ZIC   R1,5(R2)                                                         
         SHI   R1,2                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TGLST(0),9(R2)      SKIP 1 FOR '@'                               
         OC    TGLST,MYSPACES                                                   
         B     AFL30                                                            
*                                                                               
AFL20    ZIC   R1,5(R2)                                                         
         SHI   R1,3                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TGLST(0),10(R2)     SKIP 2 FOR '@-' OR '-@'                      
         OC    TGLST,MYSPACES                                                   
*                                                                               
AFL30    BRAS  RE,GETFFLST         GET AGENCY FLIST                             
         BE    AFL50                                                            
AFL40    BRAS  RE,GETNFLST         GET NEXT CODE IN FLIST                       
         BNE   AFLX                                                             
AFL50    BRAS  RE,FLTAB            STORE FLIST INTO TABLE                       
         B     AFL40                                                            
*                                                                               
AFLX     XC    TIFAGY,TIFAGY                                                    
AFLXX    XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE CHECKS IF FIELD IS A POSITIVE FLIST                      
*              R2 =A(FIELD HEADER)                                              
*                                                                               
POSFLIST NTR1  BASE=*,LABEL=*                                                   
         CLI   8(R2),C'@'          POSITIVE FLIST?                              
         BNE   PFNO                                                             
         CLI   9(R2),C'-'          MAKE SURE ITS NOT NEGATIVE                   
         BE    PFNO                                                             
         B     PFYES                                                            
PFYES    SR    RC,RC               SET CONDITION CODE                           
PFNO     LTR   RC,RC                                                            
         XIT1                                                                   
         EJECT                                                                  
*              ROUTINE CHECKS IF FIELD IS A NEGATIVE FLIST                      
*              R2 =A(FIELD HEADER)                                              
*                                                                               
NEGFLIST NTR1  BASE=*,LABEL=*                                                   
         CLC   8(2,R2),=C'-@'        NEGATIVE FLIST?                            
         BE    NFYES                                                            
         CLC   8(2,R2),=C'@-'        NEGATIVE FLIST?                            
         BE    NFYES                                                            
         B     NFNO                                                             
         EJECT                                                                  
NFYES    SR    RC,RC               SET CONDITION CODE                           
NFNO     LTR   RC,RC                                                            
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE CHECKS FOR AGENCY FLIST. SETS CC NEQ IF 1ST              
*              CODE IN FLIST IS INVALID, ELSE SETS CC EQ                        
*                                                                               
GETFFLST NTR1  BASE=*,LABEL=*                                                   
         XC    THSVFILT,THSVFILT                                                
         MVC   THFLCNT,=H'1'       FLIST COUNTER                                
         XC    TIFAGY,TIFAGY                                                    
         MVC   AIO,AIO2            SET AIO AND READ FLIST                       
         MVI   TGLTYP,TLGLTYPF                                                  
         GOTO1 RECVAL,DMCB,TLGLCDQ,(X'A0',0)                                    
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         L     R6,AIO2                                                          
         MVI   ELCODE,TAGLELQ                                                   
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAGLD,R6                                                         
         ZIC   R1,TAGLLEN                                                       
         SHI   R1,4                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TIFAGY(0),TAGLDATA  CHECK THAT AGENCY EXISTS                     
         OC    TIFAGY,MYSPACES                                                  
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'80',TIFAGY)                               
         BE    GFLYES              IF VALID RETURN CC EQ                        
*                                                                               
         BRAS  RE,AGYWILD          CHECK FOR WILDCARD AGENCY                    
         B     GFLNO               ALWAYS RETURN CC NOT EQ HERE                 
         DROP  R6                                                               
         SPACE 2                                                                
GFLYES   SR    RC,RC               SET CONDITION CODE                           
GFLNO    LTR   RC,RC                                                            
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO CHECK IF ANOTHER REQUEST IS NECESSARY                 
*                                                                               
         USING TAGLD,R6            R6=A(CURRENT TAGLD ELEMENT)                  
GETNFLST NTR1  BASE=*,LABEL=*                                                   
GETNF5   XC    THSVFILT,THSVFILT                                                
         MVC   AIO,AIO2            SET AIO AND READ FLIST                       
         MVI   TGLTYP,TLGLTYPF                                                  
         GOTO1 RECVAL,DMCB,TLGLCDQ,(X'A0',0)                                    
         BE    *+6                                                              
         DC    H'00'                                                            
         MVI   ELCODE,TAGLELQ      RESET ELEMENT CODE                           
         LH    R3,THFLCNT                                                       
         L     R6,AIO2                                                          
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
GETNF10  BRAS  RE,NEXTEL           GET NEXT ONE                                 
         BNE   GNFLNO                                                           
         BCT   R3,GETNF10          KEEP ADVANCING TILL WE GET A NEW ONE         
*                                                                               
         LH    RE,THFLCNT          INCREMENT FLIST COUNTER                      
         AHI   RE,1                                                             
         STH   RE,THFLCNT                                                       
*                                                                               
         LHI   R3,1                IN CASE WE GO BACK TO THE BCT LOOP           
*                                                                               
         ZIC   R1,TAGLLEN          IF CODE IS EQUAL TO PREVIOUS                 
         SHI   R1,4                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   TIFAGY(0),TAGLDATA                                               
         BE    GETNF10             SKIP IT                                      
*                                                                               
         XC    TIFAGY,TIFAGY       SAVE NEXT AGENCY FROM FLIST                  
         ZIC   R1,TAGLLEN                                                       
         SHI   R1,4                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TIFAGY(0),TAGLDATA  CHECK THAT AGENCY EXISTS                     
         OC    TIFAGY,MYSPACES                                                  
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'80',TIFAGY)                               
         BE    GNFLYES             SET CC EQ                                    
         BRAS  RE,AGYWILD          IF INVALID CHECK FOR WILDCARD AGY            
         B     GETNF5              GET NEXT ELEMENT (RESTORE AIO)               
         DROP  R6                                                               
         SPACE 2                                                                
GNFLYES  SR    RC,RC               SET CONDITION CODE                           
GNFLNO   LTR   RC,RC                                                            
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO CHECK IF AGENCY IS A WILDCARD AND                     
*              STORE WILDCARD AGENCIES IN AGYTAB OF FAGYTAB                     
AGYWILD  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,TIFAGY                                                        
         LA    R3,6                CHECK 6 CHARACTERS                           
         BAS   RE,CKWILD           IS THIS A WILDCARD?                          
         BNE   AGWLDNO             IF INVALID, RETURN CC NOT EQ                 
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING TLAYD,R4                                                         
         MVI   TLAYCD,TLAYCDQ                                                   
         GOTO1 HIGH                READ FIRST AGENCY                            
AGYWLD10 CLC   KEY(1),KEYSAVE                                                   
         BNE   AGWLDYES                                                         
         LA    R3,6                                                             
         LA    R2,TLAYAGY                                                       
         BAS   RE,COMPWILD                                                      
         BNE   AGYWLD20                                                         
         MVC   THSVFILT(6),TIFAGY  SAVE AGENCY FILTER                           
         MVC   TIFAGY,TLAYAGY                                                   
         DROP  R4                                                               
*                                                                               
         BRAS  RE,FLTAB            STORE FLIST AGENCIES INTO AGYTAB             
         MVC   TIFAGY,THSVFILT     RESTORE AGENCY FILTER                        
AGYWLD20 GOTO1 SEQ                                                              
         B     AGYWLD10                                                         
*                                                                               
AGWLDYES SR    RC,RC               SET CONDITION CODE                           
AGWLDNO  LTR   RC,RC                                                            
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO CHECK FOR WILD CARD                                   
*              R3=NUMBER OF CHARS, R1-->FILTER                                  
CKWILD   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
CKW10    CLI   0(R1),C'*'          IF THERE IS A '*' THEN THIS                  
         BE    CKWYES              IS A WILDCARD                                
         LA    R1,1(R1)            BUMP TO NEXT LETTER                          
         BCT   R3,CKW10                                                         
         B     CKWNO                                                            
*                                                                               
CKWYES   SR    RC,RC               SET CONDITION CODE                           
CKWNO    LTR   RC,RC                                                            
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO COMPARE WITH WILDCARD                                 
*              R3=NUMBER OF CHARS, R1-->FILTER, R2-->FIELD IN RECORD            
COMPWILD NTR1  BASE=*,LABEL=*                                                   
*                                                                               
CMPW10   CLI   0(R1),C'*'          IF THIS IS A WILD CARD                       
         BE    CMPW20              THEN DON'T COMPARE LETTERS                   
         CLC   0(1,R1),0(R2)       IF THE LETTER IS DIFFERENT                   
         BNE   CMPWNO              RETURN CC NEQ                                
*                                                                               
CMPW20   LA    R1,1(R1)            BUMP TO NEXT LETTER                          
         LA    R2,1(R2)                                                         
         BCT   R3,CMPW10                                                        
         B     CMPWYES                                                          
*                                                                               
CMPWYES  SR    RC,RC               SET CONDITION CODE                           
CMPWNO   LTR   RC,RC                                                            
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE STORES ENTRIES FROM THE FLIST INTO A TABLE               
*                                                                               
FLTAB    NTR1  BASE=*,LABEL=*                                                   
         USING FAGYD,R4                                                         
         L     R4,=A(AGYTAB)                                                    
FLT10    CLI   0(R4),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                TABLE IS NOT BIG ENOUGH                      
         CLI   0(R4),0             NEXT SLOT                                    
         BE    FLT20                                                            
         LA    R4,FAGYNEXT                                                      
         B     FLT10               GO TO NEXT OPEN SLOT                         
*                                                                               
FLT20    MVC   FAGYAGY,TIFAGY      BUILD NEW ENTRY                              
         XIT1                                                                   
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*              IF AGENCY IS A POSITIVE FLIST, SET FIRST                         
*              AGENCY OF FLIST INTO TIFAGY                                      
*                                                                               
GETAGY   NTR1  BASE=*,LABEL=*                                                   
         L     RE,ATWA                                                          
         USING CONHEADH-64,RE                                                   
         LA    R2,SPLAGYH                                                       
         DROP  RE                                                               
         BRAS  RE,POSFLIST         IF AGENCY IS A POSITIVE FLIST                
         BNE   GAGYX               SET FIRST AGENCY IN TIFAGY                   
*                                                                               
         MVC   THFLCNT,=H'1'                                                    
         L     R3,=A(AGYTAB)       R3=A(TABLE)                                  
         USING FAGYD,R3                                                         
         MVC   TIFAGY,FAGYAGY      SAVE AGENCY AS FILTER                        
GAGYX    XIT1                                                                   
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
*              IF AGENCY IS A POSITIVE FLIST, ROUTINE                           
*              SETS NEXT AGENCY OF FLIST INTO TIFAGY                            
*                                                                               
GETNAGY  NTR1  BASE=*,LABEL=*                                                   
         L     RE,ATWA                                                          
         USING CONHEADH-64,RE                                                   
         LA    R2,SPLAGYH                                                       
         DROP  RE                                                               
         BRAS  RE,POSFLIST         IF AGENCY IS A POSITIVE FLIST                
         BNE   GNAGNO              SET NEXT AGENCY IN TIFAGY                    
*                                                                               
         XC    TIFAGY,TIFAGY                                                    
         L     R3,=A(AGYTAB)       R3=A(TABLE)                                  
         USING FAGYD,R3                                                         
         LH    R1,THFLCNT                                                       
GETNAG2  LA    R3,FAGYNEXT         NEXT AGENCY                                  
         CLI   0(R3),X'FF'         END OF TABLE                                 
         BE    GNAGNO                                                           
         CLC   0(6,R3),=6X'00'     NO MORE AGENCIES                             
         BE    GNAGNO                                                           
         BCT   R1,GETNAG2          KEEP ON GOING TILL WE GET A NEW ONE          
         LH    RE,THFLCNT          INCREMENT FLIST COUNTER                      
         AHI   RE,1                                                             
         STH   RE,THFLCNT                                                       
         MVC   TIFAGY,FAGYAGY      SET NEXT AGENCY FILTER                       
         B     GNAGYES             RETURN CC EQ - AGENCY IN TABLE               
         DROP  R3                                                               
GNAGYES  SR    RC,RC               SET CONDITION CODE                           
GNAGNO   LTR   RC,RC                                                            
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE LOOKS TO SEE IF AGENCY IS IN THE FLIST TABLE             
*                                                                               
LOOKAGY  NTR1  BASE=*,LABEL=*                                                   
         L     R3,=A(AGYTAB)       R3=A(TABLE)                                  
         USING FAGYD,R3                                                         
LKAGY10  CLI   0(R3),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                (INCREASE TABLE SIZE)                        
         CLI   0(R3),0             NO MORE AGENCIES                             
         BE    LKAGNO                                                           
         CLC   TIAGY,FAGYAGY       MATCH ON AGENCY                              
         BE    LKAGYES             RETURN CC EQ - AGENCY IN TABLE               
         LA    R3,FAGYNEXT                                                      
         B     LKAGY10             KEEP ON LOOKING                              
         DROP  R3                                                               
*                                                                               
LKAGYES  SR    RC,RC                                                            
LKAGNO   LTR   RC,RC                                                            
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*              AGENCY FLIST TABLE                                               
*                                                                               
         DS    0D                                                               
         DC    CL8'*AGYTAB*'                                                    
AGYTAB   DC    (NFAGY*FAGYLNQ)X'00'                                             
         DC    X'FF'                                                            
*                                                                               
NFAGY    EQU   500                 MAXIMUM N'AGENCIES                           
         EJECT                                                                  
         SPACE 1                                                                
*              TASYSCATS BELOW                                                  
*              TASYSEQUS                                                        
*              TASYSDSECT                                                       
         PRINT OFF                                                              
       ++INCLUDE TASYSCATS                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TASYSDSECT                                                     
         PRINT ON                                                               
         SPACE 1                                                                
         EJECT                                                                  
*              MY WORKING STORAGE                                               
         SPACE 3                                                                
MYD      DSECT                                                                  
MYSTART  DS    0D                                                               
MYREGS   DS    16F                                                              
MYTITLE  DS    CL32                                                             
MYP      DS    CL132                                                            
MYP2     DS    CL132                                                            
MYP3     DS    CL132                                                            
MYP4     DS    CL132                                                            
MYSPACES DS    CL132                                                            
ODDAREA  DS    CL12                                                             
MYSORTER DS    A                                                                
CANRATE  DS    F                                                                
PRDCRATE DS    H                                                                
         SPACE 1                                                                
LASTCHEK DS    CL32                                                             
*                                  OPTIONS                                      
TRACECID DS    CL12                                                             
ACOMRATE DS    F                                                                
PGOPT    DS    XL1                                                              
PGOPTTNH EQU   X'80'               T&H OPTION REQUESTED                         
PGOPTSAC EQU   X'40'               IGNORE SPECIAL COMMRATES                     
         SPACE 1                                                                
QRUN     DS    CL6                 DATES                                        
QPER     DS    CL6                                                              
QREQ     DS    CL6                                                              
QPREV    DS    CL6                                                              
OPTYEAR  DS    CL4                 FOUR CHARACTER DISPLAYABLE YEAR              
PPREV    DS    PL3                                                              
RECTYPE  DS    CL16                                                             
SAVEEL   DS    CL1                                                              
MONSTART DS    XL1                                                              
MONEND   DS    XL1                                                              
THISMON  DS    XL1                                                              
THISCOM  DS    XL4                                                              
COMMPRD  DS    CL6                                                              
LSTRCMON DS    XL1                                                              
         SPACE 1                                                                
AGYNAME  DS    CL36                                                             
CLINAME  DS    CL36                                                             
PRDNAME  DS    CL36                                                             
         SPACE 1                                                                
THSVFILT DS    CL6                 SAVED FILTER FOR AGENCY                      
THFLCNT  DS    H                   FLIST COUNTER                                
TRACOPT  DS    CL1                 OPTION MYTRACE - AGYTAB FOR FLIST            
         EJECT                                                                  
*              DATA EXTRACTED FROM CHECK/INVOICE ETC                            
         SPACE 3                                                                
EXTAGY   DS    CL6                                                              
EXTCLI   DS    CL6                                                              
EXTPRD   DS    CL6                                                              
EXTEST   DS    CL16                                                             
EXTAUTH  DS    CL16                                                             
EXTCID   DS    CL12                                                             
EXTCAT   DS    CL3                                                              
EXTONOF  DS    CL3                                                              
EXTOV    DS    F                                                                
EXTDBL   DS    XL1                                                              
EXTTITLE DS    CL36                                                             
EXTPNAME DS    CL36                                                             
EXTINV   DS    XL6                                                              
EXTRATE  DS    F                                                                
EXTUDET  DS    CL48                                                             
EXTUNAM  DS    CL48                                                             
EXTCYC   DS    XL6                                                              
EXTPAY   DS    F                                                                
EXTPNH   DS    F                                                                
EXTGST   DS    F                                                                
EXTTNH   DS    F                                                                
EXTAC    DS    F                                                                
EXTGRS   DS    F                                                                
EXTMON   DS    XL1                                                              
EXTMONA  DS    XL1                                                              
EXTMED   DS    CL1                                                              
EXTACDE  DS    CL1                                                              
EXTRPAY  DS    F                                                                
EXTRADJ  DS    F                                                                
EXTRTOT  DS    F                                                                
         SPACE 1                                                                
LASTKEY  DS    0CL64                                                            
LASTAGY  DS    CL6                     AGENCY                                   
LASTCLI  DS    CL6                     CLIENT                                   
LASTEST  DS    CL15                    ESTIMATE                                 
LASTMED  DS    CL1                     ESTIMATE                                 
LASTREP  DS    XL1                     REPORT 1                                 
LASTCID  DS    CL12                    COMMERCIAL ID                            
LASTCOM  DS    CL4                 INT. COMMERCIAL #                            
         DS    CL2                                                              
LASTSREP DS    XL1                     SUB REPORT 1                             
LASTINV  DS    XL6                     INVOICE                                  
         DS    XL1                                                              
         ORG   LASTKEY+64                                                       
         DS    0D                                                               
SORTIO   DS    CL144                                                            
         SPACE 1                                                                
MYEND    DS    0D                                                               
         EJECT                                                                  
*              DSECT TO COVER CATEGORY ANALYSIS BLOCK                           
         SPACE 3                                                                
CATPOOLD DSECT                                                                  
POOLENT  DS    0CL16                                                            
POOLCAT  DS    CL3                                                              
POOLONOF DS    CL3                                                              
POOLOV   DS    XL4                                                              
POOLDBL  DS    XL1                                                              
         DS    XL1                                                              
POOLCNT  DS    F                                                                
         SPACE 3                                                                
*              DSECT TO COVER SORT RECORDS                                      
         SPACE 3                                                                
SORTD    DSECT                                                                  
*                                  RECORD 1    PAYMENT REPORT                   
*                                  RECORD 1/1  COMMERCIAL HEADER                
SORTKEY  DS    0CL64               KEY                                          
SORTAGY  DS    CL6                     AGENCY                                   
SORTCLI  DS    CL6                     CLIENT                                   
SORTEST  DS    CL15                    ESTIMATE                                 
SORTMED  DS    CL1                     MEDIA                                    
SORTREP  DS    XL1                     REPORT 1                                 
SORTCID  DS    CL12                    COMMERCIAL ID                            
SORTCOM  DS    XL4                     INT COM NUMBER                           
         DS    XL2                                                              
SORTSREP DS    XL1                     SUB REPORT 1                             
         ORG   SORTKEY+64                                                       
SORTDATA DS    0CL80               DATA                                         
SORTTIT  DS    CL30                    TITLE                                    
SORTPNAM DS    CL36                    PRODUCT NAME                             
         EJECT                                                                  
*              SORT RECORD 1/2                                                  
         SPACE 3                                                                
*                                  RECORD 1/2  INVOICE RATE ANALYSIS            
         ORG   SORTKEY             KEY                                          
         DS    CL6                     AGENCY                                   
         DS    CL6                     CLIENT                                   
         DS    CL15                    ESTIMATE                                 
         DS    CL1                     MEDIA                                    
         DS    XL1                     REPORT 1                                 
         DS    CL12                    COMMERCIAL ID                            
         DS    CL6                                                              
         DS    XL1                     SUB REPORT 2                             
SORTINV  DS    XL6                     INVOICE                                  
SORTINVT DS    XL1                     1=HEADER 2=RATE ANAL. 3=TOTAL            
SORTRATE DS    F                       PAY RATE                                 
         SPACE 1                                                                
         ORG   SORTDATA            DATA                                         
SORTCNT  DS    F                       COUNT                                    
SORTUNAM DS    CL16                    USE NAME                                 
SORTUDET DS    CL32                    USE DETAILS                              
SORTCYC  DS    PL6                     CYCLE DATES                              
SORTACDE DS    CL1                     APPLIED CODE                             
         SPACE 1                                                                
         ORG   SORTDATA            DATA                                         
SORTPAY  DS    F                       PAYI + PAYC + REXP                       
SORTPNH  DS    F                       PNH OR GST (NO GST IF TNH)               
SORTTNH  DS    F                       TX + HND + HNDC + FICR+ GST              
SORTAC   DS    F                       15% OF PAY + PNH                         
SORTGRS  DS    F                       PAY + PNH + GRS                          
SORTMON  DS    XL1                                                              
SORTMONA DS    CL1                                                              
         EJECT                                                                  
*              SORT RECORDS 3/1 AND 3/2                                         
         SPACE 3                                                                
*                                  RECORD 3    RECAP                            
*                                  RECORD 3/1  MONTH BREAKDOWN                  
         ORG   SORTKEY             KEY                                          
         DS    CL6                     AGENCY                                   
         DS    CL6                     CLIENT                                   
         DS    CL15                    ESTIMATE                                 
         DS    XL1                     REPORT 3                                 
         ORG   SORTSREP                                                         
         DS    XL1                     SUB REPORT 1                             
SORTKMON DS    CL1                     MONTH NUMBER                             
         SPACE 1                                                                
         ORG   SORTDATA            DATA                                         
SORTRPAY DS    F                       PAYMENT                                  
SORTRADJ DS    F                       ADJUSTMENT                               
SORTRTOT DS    F                       TOTAL                                    
         SPACE 1                                                                
*                                  RECORD 3/2  RECAP TOTALS                     
         ORG   SORTKEY             KEY                                          
         DS    CL6                     AGENCY                                   
         DS    CL6                     CLIENT                                   
         DS    CL15                    ESTIMATE                                 
         DS    XL1                     REPORT 3                                 
         ORG   SORTSREP                                                         
         DS    XL1                     SUB REPORT 2                             
         SPACE 1                                                                
         ORG   SORTDATA            DATA                                         
         DS    F                       PAYMENT                                  
         DS    F                       ADJUSTMENT                               
         DS    F                       TOTAL                                    
         EJECT                                                                  
*                                                                               
*              AGENCY FLIST TABLE DSECT                                         
*                                                                               
FAGYD    DSECT                                                                  
FAGYAGY  DS    CL6                     AGENCY                                   
FAGYLNQ  EQU   *-FAGYD                                                          
FAGYNEXT EQU   *                                                                
         EJECT                                                                  
*              OTHER DSECTS ARE HIDDEN IN HERE                                  
         SPACE 3                                                                
*TAREPWORKD                                                                     
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
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
       ++INCLUDE TAGENFILE                                                      
TWADCOND DSECT                                                                  
       ++INCLUDE DDTWADCONS                                                     
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FATIOB                                                         
       ++INCLUDE TAREPFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE TAREPE9D                                                       
         SPACE 1                                                                
       ++INCLUDE DDGENTWA                                                       
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'040TAREP29   11/18/12'                                      
         END                                                                    
