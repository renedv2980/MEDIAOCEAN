*          DATA SET ACREPBT02  AT LEVEL 044 AS OF 05/01/02                      
*PHASE ACBT02A,*                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*                                                                               
         TITLE 'ACBT02 - BILLING INTERFACE FOR BANKERS TRUST'                   
ACBT02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACBT**,R9                                                    
         USING ACWORKD,RA                                                       
         L     RA,0(R1)                                                         
         USING ACBTD,RC                                                         
         LA    RC,SPACEND                                                       
         CLI   MODE,RUNFRST        Q, VERY FIRST TIME AROUND                    
         BE    RUNINIT              Y, DO RUN INITIALIZATION                    
         CLI   MODE,REQFRST        Q, FIRST TIME FOR A REQUEST                  
         BE    REQINIT              Y, DO REQUEST INITIALIZATION                
         CLI   MODE,PROCTRNS       Q, A TRANSACTION                             
         BE    TRNRT                Y, PROCESS TRANSACTION/ELEMENTS             
         CLI   MODE,PROCLEVB       Q, A PRODUCT RECORD                          
         BE    PRODRT               Y, PROCESS                                  
         CLI   MODE,PROCACC        Q, AN ACCOUNT RECORD                         
         BE    ACCRT                Y, PROCESS ACCOUNT LEVEL RECORD             
         CLI   MODE,REQLAST        Q, END OF REQUEST                            
         BE    REQL                 Y, PRINT REQUEST STATUS                     
         CLI   MODE,RUNLAST        Q, END OF RUN                                
         BE    SRTRD                Y, SORT, PRINT REPORTS & WRITE TAPE         
EXIT     XIT1                                                                   
         EJECT                                                                  
RUNINIT  GOTO1 DATCON,DMCB,(5,0),(X'20',DUB)   -- NO DDS INT DATES              
         MVC   MMDDYY(4),DUB+2     SAVE MMDD                                    
         MVC   MMDDYY+4(2),DUB     SAVE YY                                      
         SPACE 1                                                                
         L     RF,=A(ADRC)         SAVE RC FOR BINADD                           
         ST    RC,0(RF)                                                         
         L     R2,VEXTRAS                                                       
         USING RUNXTRAD,R2                                                      
         L     R2,ADMASTD                                                       
         USING MASTD,R2                                                         
         L     R2,MCBXAREA                                                      
         ST    R2,ADBOX                                                         
         L     R2,=A(HDHOOK)                                                    
         ST    R2,HEADHOOK                                                      
         SPACE 1                                                                
         DROP  R2                                                               
         GOTO1 BUFFALO,DMCB,=C'SET',ADBUFC                                      
         SPACE 1                                                                
         MVC   DSPARM+13(2),ALPHAID                                             
         USING CPYRECD,R2                                                       
         L     R2,ADCOMP                                                        
         MVC   SVCOMP,CPYKCPY      SAVE HEX COMP                                
         SPACE 1                                                                
         XC    OUTCNT,OUTCNT                                                    
         XC    IOSW,IOSW                                                        
         ZAP   TPCT,=P'0'                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         ZAP   RUNTOT,=P'0'                                                     
         ZAP   RUNCNT,=P'0'                                                     
         B     EXIT                                                             
         EJECT                                                                  
*---------------------------------------------------------------------          
*        FIRST FOR REQUEST                                                      
*---------------------------------------------------------------------          
REQINIT  MVC   QEND3,=XL3'FF'      INIT DATES                                   
         MVC   QEND2,=XL3'FF'                                                   
         XC    QSTR3,QSTR3                                                      
         XC    QSTR2,QSTR2                                                      
         CLC   QSTART(L'QSTART+L'QEND),SPACES  ANY START OR END                 
         BE    REQIB                                                            
         CLC   QSTART,SPACES                                                    
         BE    REQIA                                                            
         GOTO1 DATCON,DMCB,(0,QSTART),(1,QSTR3)                                 
         GOTO1 DATCON,DMCB,(0,QSTART),(2,QSTR2)                                 
         SPACE 1                                                                
REQIA    CLC   QEND,SPACES                                                      
         BE    REQIB                                                            
         GOTO1 DATCON,DMCB,(0,QEND),(1,QEND3)                                   
         GOTO1 DATCON,DMCB,(0,QEND),(2,QEND2)                                   
         SPACE 1                                                                
REQIB    ZAP   REQTOT,=P'0'                                                     
         ZAP   REQCNT,=P'0'                                                     
         MVI   RCSUBPRG,1                                                       
         SPACE 1                                                                
         MVI   SVBATCH,C'D'                                                     
         MVC   SVBATCH+1(2),MMDDYY     BATCH NUMBER IS D+MM                     
         GOTO1 DATCON,DMCB,(5,0),(6,SVMONTH)                                    
*                                                                               
         CLC   QAPPL+6(2),SPACES         MONTH INPUT                            
         BE    REQIX                   NO                                       
*                                                                               
         GOTO1 DATCON,DMCB,(2,QAPPL+6),(0,WORK)                                 
         MVC   SVBATCH+1(2),WORK+2    YES, GET THE MM                           
         GOTO1 DATCON,DMCB,(2,QAPPL+6),(6,SVMONTH)                              
REQIX    B     EXIT                                                             
         EJECT                                                                  
*---------------------------------------------------------------------          
*        STORE PRODUCT LEVEL COMMENTS                                           
*---------------------------------------------------------------------          
         SPACE 1                                                                
PRODRT   LA    R2,PRODCOMM                                                      
         BAS   RE,GETCOMMS                                                      
         B     EXIT                                                             
         EJECT                                                                  
*---------------------------------------------------------------------          
*        PROCESS AN ACCOUNT                                                     
*        VALIDATE AN ACCOUNT FOR TAPE                                           
*        READ USER FIELDS                                                       
*        WRITE ESTIMATE RECORDS TO SORT (AS 'X' TYPE)                           
*---------------------------------------------------------------------          
         SPACE 1                                                                
         USING BIND,R4                                                          
ACCRT    L     R4,TAXTAB           CLEAR TABLE AT START OF EACH ACCOUNT         
         XC    BININ,BININ                                                      
         DROP  R4                                                               
         SPACE 1                                                                
         USING SRTD,R4                                                          
         LA    R4,SRTDATA                                                       
         MVC   0(SRTDLN,R4),SPACES                                              
         SPACE 1                                                                
         USING UFSELD,R7                                                        
         L     R7,ADACC                                                         
         MVI   ELCODE,UFSELQ                                                    
         SR    R0,R0                                                            
         BAS   RE,GETEL                                                         
ACC10    BNE   ACC40                                                            
         SPACE 1                                                                
         CLC   UFSCODE,=C'CC'      IS THERE A CC USER FIELD HERE                
         BE    ACC20                                                            
         CLC   UFSCODE,=C'AC'      IS THERE A AC USER FIELD HERE                
         BE    ACC25                                                            
ACC15    BAS   RE,NEXTEL                                                        
         B     ACC10                                                            
         SPACE 1                                                                
ACC20    ZIC   R1,UFSLN                                                         
         LA    R0,UFSLN1Q                                                       
         CR    R1,R0                                                            
         BNH   ACC15               NO DATA ON THIS ELEMENT                      
         SR    R1,R0                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SRTCC(0),UFSDATA                                                 
         MVI   SRTCTYPE,C'N'       FLAG SRTCC AS CONTAINING A NAME              
         B     ACC15               GET NEXT EL                                  
         SPACE 1                                                                
ACC25    ZIC   R1,UFSLN                                                         
         LA    R0,UFSLN1Q                                                       
         CR    R1,R0                                                            
         BNH   ACC15               NO DATA ON THIS ELEMENT                      
         SR    R1,R0                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SRTAC(0),UFSDATA                                                 
         B     ACC15                                                            
         SPACE 1                                                                
ACC40    LA    R2,ACCCOMM                                                       
         BAS   RE,GETCOMMS                                                      
         CLC   SRTCC,SPACES        CC FROM USER FIELD                           
         BNE   ACC50               YES                                          
         MVC   SRTCC(L'SCMCODE-2),PRODCC+2      SAVE NARRATIVE CODE             
         CLC   ACCCC(L'SCMCODE),SPACES          AFTER THE 'CC'                  
         BE    *+10                                                             
         MVC   SRTCC(L'SCMCODE-2),ACCCC+2                                       
         SPACE 1                                                                
ACC50    CLC   SRTAC,SPACES        AC FROM USER FIELD                           
         BNE   ACC60               YES                                          
         MVC   SVCODE(L'SCMCODE),PRODAC                                         
         CLC   ACCAC(L'SCMCODE),SPACES                                          
         BE    *+10                                                             
         MVC   SVCODE(L'SCMCODE),ACCAC                                          
         LA    R2,SRTAC                                                         
         LA    R1,L'SRTAC                                                       
         BAS   RE,GETNARA                                                       
         SPACE 1                                                                
         USING ACTRECD,R7                                                       
ACC60    L     R7,ADACC                                                         
         MVC   SRTCPJ,ACTKACT                                                   
         B     EXIT                                                             
         DROP  R7                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
*        PROCESS A TRANSACTION                                                  
*----------------------------------------------------------------------         
         USING TRNRECD,R6                                                       
TRNRT    L     R6,ADTRANS                                                       
         SH    R6,DATADISP                                                      
         USING TRNELD,R7                                                        
         L     R7,ADTRANS          GETS ADDRESS OF TRAN ELEMENT                 
         CLI   TRNEL,X'44'         Q, TRAN ELEMENT-44, ALWAYS FIRST             
         BNE   EXIT                 N, EXIT---BAD TRAN                          
         CLC   TRNANAL,=C'**'      NO P/O'S                                     
         BE    EXIT                                                             
         CLC   TRNANAL,=C'99'      BUILD TABLE IF WORK CODE NOT '99'            
         BE    TRN040                                                           
         SPACE 1                                                                
*----------------------------------------------------------------------         
*        SAVE AMOUNT OF BILLING ON  "TAX" WORKCODES                             
*        FOR REPORT                                                             
*----------------------------------------------------------------------         
         SPACE 1                                                                
         L     R2,TAXWCS           LIST OF TAX WORK CODES                       
         LA    R1,TAXWCNUM                                                      
TRN05    CLC   TRNANAL,0(R2)       IS THIS A TAXWC                              
         BE    TRN10               YES, SAVE BILLING INFO                       
         LA    R2,2(R2)            NEXT WC IN TABLE                             
         BCT   R1,TRN05            CHECK AGAIN                                  
         B     EXIT                                                             
*                                                                               
         USING TAXTABD,R5                                                       
TRN10    LA    R5,TAXREC                                                        
         SPACE 1                                                                
         USING TRNELD,R7                                                        
         L     R7,ADTRANS                                                       
         MVC   TAXSTAT,TRNSTAT                                                  
         ZAP   DUB,TRNAMNT         HOLD TRANS AMOUNT FOR REVERSED               
*                                                                               
         USING ACMD,R7                                                          
         MVI   ELCODE,PTAELQ                                                    
         L     R7,AMONACC                                                       
         L     R7,ACMAPRO2         FIRST 77 ELEM                                
         CLI   0(R7),PTAELQ                                                     
         B     TRN010A                                                          
*                                                                               
TRN010   BAS   RE,NEXTEL                                                        
TRN010A  BNE   TRN015              NO (MORE) 77'S, CONTINUE                     
         USING PTAELD,R7          GOT A 77 ELEMENT                              
         TM    PTASTAT1,PTASREVU   THIS BILL REVERSED?                          
         BO    TRN013                                                           
         TM    PTASTAT1,PTASREVS                                                
         BO    TRN014                                                           
         B     TRN010                                                           
*                                                                               
TRN013   MVC   TAXNUM,=6X'EE'      EE TO INVOICE                                
         MVC   TAXDATE,PTARBLDT   SAVE ORIGINAL BILL DATE                       
         ZAP   TAXAMNT,DUB                                                      
         BAS   RE,ADDIT            ADD TO BINTABLE                              
         B     TRN010                                                           
*                                                                               
TRN014   MVC   TAXDATE,PTARBLDT   SAVE DATE REVERSED                            
         MVC   TAXNUM,=6X'FF'      FF TO INVOICE                                
         ZAP   TAXAMNT,DUB                                                      
         MP    TAXAMNT,=P'-1'      'REVERSED'                                   
         BAS   RE,ADDIT            ADD TO BINTABLE                              
         B     TRN010                                                           
*                                                                               
TRN015   ZAP   DOUBLE,=P'0'                                                     
         USING ACMD,R7                                                          
         L     R7,AMONACC                                                       
         L     R7,ACMAPRO2                                                      
         CLI   0(R7),PTAELQ                                                     
         B     *+8                                                              
*                                                                               
         USING PTAELD,R7                                                        
TRN015A  BAS   RE,NEXTEL                                                        
         BNE   TRN016                                                           
         CLI   PTATYPE,PTATRAL     IS THIS A BILING ELEMENT                     
         BNE   TRN015A                                                          
         TM    PTASTAT1,PTASPEND                                                
         BO    TRN015A                                                          
         TM    PTASTAT1,X'0E'                                                   
         BNZ   TRN015A                                                          
         CLC   PTARBLDT,QEND2      FILTER BILL ON DATE                          
         BH    TRN015A                                                          
*                                                                               
TRN015J  MVC   TAXNUM,PTARBLNO     MOVE INVOICE AND AMOUNT TO TABLE             
         MVC   TAXDATE,PTARBLDT                                                 
         ZAP   TAXAMNT,PTANET                                                   
         AP    DOUBLE,PTANET       KEEP TOTAL BILLED ON THIS TRANS              
         ZAP   DUB,PTANET                                                       
         CP    DOUBLE,=P'0'        ANY BILLED AMOUNT IN THE 4B                  
         BE    TRN016              NO, MIGHT BE FULLY BILLED                    
*                                                                               
         BAS   RE,ADDIT                                                         
         B     TRN015A                                                          
*                                                                               
TRN016   OC    ACCOUSED(2,R6),ACCOUSED(R6)                                      
         BZ    TRN020              NO                                           
         SPACE 1                                                                
         CP    DOUBLE,=P'0'        DID I GET AN AMOUNT IN THE 4B                
         BNE   EXIT                YES                                          
         SPACE 1                                                                
         USING TRNELD,R7                                                        
         L     R7,ADTRANS          IF TRNKSUSE AND ZERO 4B'S USE TRN            
         ZAP   TAXAMNT,TRNAMNT                                                  
         MVC   TAXDATE,TRNKSUSE                                                 
         BAS   RE,ADDIT                                                         
         B     EXIT                GET NEXT TRANSACTION                         
         SPACE 1                                                                
TRN020   B     EXIT                                                             
         DROP  R5,R7                                                            
         EJECT                                                                  
*----------------------------------------------------------------------         
*        PROCESS BILLING, WRITE RECORDS TO TAPE                                 
*           FILTER BIL ON DA DATE VS START/END DATES ON REQUEST                 
*----------------------------------------------------------------------         
         USING SRTD,R4                                                          
TRN040   EQU   *                                                                
         LA    R4,SRTDATA                                                       
         SPACE 1                                                                
         USING TRNELD,R7                                                        
         L     R7,ADTRANS                                                       
         CLC   TRNDATE,QSTR3       DATE FILTERING                               
         BL    EXIT                                                             
         CLC   TRNDATE,QEND3                                                    
         BH    EXIT                                                             
         SPACE 1                                                                
         CP    TRNNARR+27(6),=P'0' ZERO BILL                                    
         BE    EXIT                                                             
         SPACE 1                                                                
*        MVI   ELCODE,X'60'        LOOK FOR STATUS ELEMENT-60                   
*        BAS   RE,NEXTEL           GETS STATUS ELEMENT---MAYBE                  
*        BNE   EXIT                BRANCH IF NO STATUS ELEMENT FOUND            
*                                                                               
*        USING TRSELD,R7                                                        
         MVC   SVDT2,TRNDATE       SAVE BILL'S DA DATE                          
         SPACE 1                                                                
*---------------------------------------------------------------------          
*        PUT BILL TO SORT                                                       
*---------------------------------------------------------------------          
         USING TRNELD,R7                                                        
         L     R7,ADTRANS                                                       
         MVC   SRTDATE,TRNDATE                                                  
         MVC   SRTNO,TRNREF                                                     
         SPACE 1                                                                
         BAS   RE,GETTAX                                                        
         ZAP   SRTTAX,DUB                                                       
         ZAP   SRTAMNT,TRNNARR+27(6)                                            
         USING ACMD,RF                                                          
         L     RF,AMONACC                                                       
         MVC   SRTMOS,ACMMDTE                                                   
         BAS   RE,PUTSORT          PUT THE INVOICE DATA TO SORT                 
         B     EXIT                                                             
         SPACE 1                                                                
         DROP  R4,R7,RF                                                         
         EJECT                                                                  
ADDIT    NTR1                        ADD AN ENTRY TO TAXTAB                     
         USING TAXTABD,R5                                                       
         USING TRNELD,R7                                                        
         CLI   QOPT2,C'X'                                                       
         BNE   ADDIT01                                                          
         GOTO1 PRNTBL,DMCB,=C'PTAEL',(R7),C'DUMP',96,=C'1D'                     
ADDIT01  LA    R7,ADTRANS                                                       
         ZAP   DUB,=P'0'                                                        
         TM    TAXSTAT,X'01'       NON COMM CHARGE                              
         BO    ADDIT50             YES                                          
         SPACE 1                                                                
         USING TABLED,R4                                                        
         L     R4,WCTAB            GET WORK CODE COMISSION                      
         LH    R0,TABNUM                                                        
         LA    R3,TABDATA                                                       
         LTR   R0,R0               ANYTHING SAVED HERE                          
         BZ    ADDIT07             NO, CALL GETOPT FOR RATE                     
         CH    R0,TABMAX           AM I AT MAX FOR TABLE                        
         BL    *+6                 NO, THERE IS STILL ROOM                      
         DC    H'0'                                                             
         USING WCTABD,R3                                                        
ADDIT06  CLC   WCTBCODE,TRNANAL                                                 
         BE    ADDIT10                                                          
         LA    R3,WCTABLN(R3)                                                   
         BCT   R0,ADDIT06                                                       
ADDIT07  EQU   *                   RATE FOR WORK CODE NOT FOUND                 
         L     R6,ADGOBLOC         GET RATE FOR THIS WORKCODE                   
         USING GETOPTD,R6                                                       
         MVC   GOSELWC,TRNANAL                                                  
         GOTO1 GETOPT,DMCB,GETOPTD   GET WORK CODE RATE                         
         XC    GOSELWC,GOSELWC       RESET GETOPT                               
         ZAP   WCTBRATE,GOAGYCOM                                                
         MVC   WCTBCODE,TRNANAL                                                 
         SPACE 1                                                                
         LH    R1,TABNUM                                                        
         LA    R1,1(R1)                                                         
         STH   R1,TABNUM                                                        
         SPACE 1                                                                
ADDIT10  ZAP   PL16,TAXAMNT                                                     
         MP    PL16,WCTBRATE                                                    
         SRP   PL16,64-6,5                                                      
         ZAP   DUB,PL16                                                         
         AP    TAXAMNT,DUB                                                      
ADDIT50  GOTO1 BINADD,DMCB,(R5),TAXTAB                                          
         B     EXIT                                                             
         DROP  R3,R4,R5,R6                                                      
         EJECT                                                                  
*--------------------------------------------------------------------           
*        LAST FOR REQUEST, PRINT SOME TOTALS                                    
*--------------------------------------------------------------------           
REQL     MVI   RCSUBPRG,1                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVC   P+1(15),=C'REQUEST TOTALS:'                                      
         ZAP   DOUBLE,REQCNT                                                    
         LA    R3,P+17                                                          
         BAS   RE,PRTCNT                                                        
         MVC   P+30(9),=C'INVOICES,'                                            
         ZAP   DOUBLE,REQTOT                                                    
         LA    R3,P+40                                                          
         BAS   RE,PRTAMNT                                                       
         GOTO1 ADSQUASH,DMCB,P+1,60                                             
         SPACE 1                                                                
         CLI   QOPT1,C'Y'                                                       
         BNE   REQLX                                                            
         MVC   PSECOND+1(24),=C'RECORDS WRITTEN TO TAPE.'                       
         SPACE 1                                                                
REQLX    BAS   RE,PRINTIT                                                       
         AP    RUNTOT,REQTOT                                                    
         AP    RUNCNT,REQCNT                                                    
         B     EXIT                                                             
         EJECT                                                                  
*--------------------------------------------------------------------           
*        RUN LAST CALL FROM MONACC                                              
*--------------------------------------------------------------------           
SRTRD    TM    IOSW,SRTOPEN        Q, ANY INUT RECORDS                          
         BZ    EXIT                 N, EXIT                                     
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,0                                                       
         USING TOTTABD,R2                                                       
         USING SRTD,R5                                                          
         L     R2,TOTTAB                                                        
         LA    R0,TOTNUM                                                        
SRT010   ZAP   TOTTAX,=P'0'                                                     
         ZAP   TOTAMNT,=P'0'                                                    
         LA    R2,TOTLEN(R2)                                                    
         BCT   R0,SRT010                                                        
         MVC   SRTDATA(SRTDLN),SPACES                                           
         SPACE 1                                                                
SRT050   MVC   LASTREC(SRTDLN),SRTDATA                                          
         GOTO1 ADSORTER,DMCB,=C'GET'                                            
         ICM   R5,15,DMCB+4        LAST RECORD FROM SORT                        
         BZ    SRT300                                                           
         MVC   SRTDATA(SRTDLN),0(R5)                                            
         SPACE 1                                                                
         LA    R5,SRTDATA                                                       
         CLI   SRTTYPE,1           IS THIS A REPORT RECORD                      
         BNE   SRT100              NO USE FOR TAPE                              
         SPACE 1                                                                
         CLC   LASTREC(SRTCC-SRTD+L'SRTCC),SRTDATA                              
         BE    SRT060                                                           
         BAS   RE,TOTALS                                                        
         SPACE 1                                                                
SRT060   L     R2,TOTTAB           ACCUMULATE SORT DATA INTO TOTALS             
         AP    TOTTAX,SRTTAX                                                    
         AP    TOTAMNT,SRTAMNT                                                  
         SPACE 1                                                                
*---------------------------------------------------------------------          
*        PRODUCE A REPORT LINE                                                  
*---------------------------------------------------------------------          
         USING PRINTD,R6                                                        
         BAS   RE,FIRSTS                                                        
         LA    R5,SRTDATA                                                       
         LA    R6,P                                                             
         MVC   PRINV,SRTNO                                                      
         GOTO1 DATCON,DMCB,(1,SRTDATE),(5,PRDATE)                               
         MVC   PRCPJ,SRTCPJ                                                     
         MVC   PREXCODE,SRTAC                                                   
         MVC   PRACCNUM(4),=C'2233'                                             
         CLC   SRTCPJ(3),=C'BTS'                                                
         BNE   *+10                                                             
         MVC   PRACCNUM(4),=C'2239'                                             
         SPACE 1                                                                
         ZAP   DOUBLE,SRTAMNT                                                   
         SP    DOUBLE,SRTTAX                                                    
         LA    R3,PRLESSTX                                                      
         BAS   RE,PRTAMNT                                                       
         SPACE 1                                                                
         ZAP   DOUBLE,SRTTAX                                                    
         LA    R3,PRTAX                                                         
         BAS   RE,PRTAMNT                                                       
         SPACE 1                                                                
         ZAP   DOUBLE,SRTAMNT                                                   
         LA    R3,PRGROSS                                                       
         BAS   RE,PRTAMNT                                                       
         BAS   RE,PRINTIT                                                       
         B     SRT050                                                           
         SPACE 1                                                                
*---------------------------------------------------------------------          
*        PRODUCE A VE TAPE RECORD                                               
*---------------------------------------------------------------------          
         SPACE 1                                                                
         USING TPRECD,R6                                                        
SRT100   LA    R6,TPDATA                                                        
         LA    R5,SRTDATA                                                       
         MVC   VEREC,SPACES                                                     
         MVC   VETYPE,=C'VE'                                                    
         MVI   VEACTION,C'1'                                                    
         CP    SRTAMNT,=P'0'                                                    
         BNL   *+8                                                              
         MVI   VEACTION,C'2'                                                    
         MVC   VECOMP,=C'NY '                                                   
         MVC   VEBATCH,SVBATCH     TAPE MONTH                                   
         SPACE 1                                                                
         L     R2,PVOUTAB                                                       
         LA    R0,PVOUNUM                                                       
SRT190   CLC   SRTMOS+1(1),0(R2)                                                
         BE    SRT200                                                           
         LA    R2,L'PVOUTABC(R2)                                                
         BCT   R0,SRT190                                                        
         DC    H'0'                MOS NOT 1-12 !                               
SRT200   MVC   VEPV,1(R2)                                                       
         SPACE 1                                                                
         MVC   VESV,SRTNO+2        LAST 4 OF BILL NUMBER                        
         ZAP   DUB,SRTAMNT                                                      
         UNPK  VEGROSS,DUB                                                      
         OI    VEGROSS+L'VEGROSS-1,X'F0'                                        
         SPACE 1                                                                
         MVC   SVAMOUNT,VEGROSS    SAVE CHARACTER AMOUBNT                       
         SPACE 1                                                                
         MVC   VEMEDIA(1),SRTCPJ+6                                              
         MVC   VEINVNO,SRTNO                                                    
         MVC   VECLI,SRTCPJ                                                     
         GOTO1 DATCON,DMCB,(1,SRTDATE),(X'20',WORK)  --NO DDS INT DATES         
         MVC   VEDATE(4),WORK+2    MMDD                                         
         MVC   VEDATE+4(2),WORK    YY                                           
*                                                                               
         MVC   VENAME,=CL10'DOREMUS'                                            
         MVI   VE3,C'3'                                                         
         MVC   VERCDATE,MMDDYY                                                  
         MVC   VED44,=C'D44'                                                    
         BAS   RE,WRTP                                                          
*---------------------------------------------------------------------          
*        PRODUCE A VL TAPE RECORD                                               
*---------------------------------------------------------------------          
         MVC   VLDATA,SPACES                                                    
         MVC   VLTYPE,=C'VL'                                                    
         MVC   VLNUMBER,=CL3'001'                                               
         MVC   VLAMOUNT,SVAMOUNT                                                
         SPACE 1                                                                
         BAS   RE,WRTP                                                          
*---------------------------------------------------------------------          
*        PRODUCE A VM TAPE RECORD                                               
*---------------------------------------------------------------------          
         MVC   VMDATA,SPACES                                                    
         MVC   VMTYPE,=C'VM'                                                    
         MVC   VMACCNUM(4),=C'2233'                                             
         CLC   SRTCPJ(3),=C'BTS'                                                
         BNE   *+10                                                             
         MVC   VMACCNUM(4),=C'2239'                                             
         SPACE 1                                                                
         MVC   VMNUMBER,=C'001'                                                 
         SPACE 1                                                                
         MVC   VMCPJ,SRTCPJ                                                     
         MVC   VMAC,SRTAC                                                       
         MVI   VME,C'E'                                                         
         BAS   RE,WRTP                                                          
         B     SRT050                                                           
         SPACE 1                                                                
SRT300   GOTO1 ADSORTER,DMCB,=C'END' END THE SORT                               
         NI    IOSW,X'FF'-SRTOPEN                                               
         SPACE 1                                                                
         MVI   SRTDATA,X'FF'                                                    
         BAS   RE,TOTALS           DO LAST SUBTOTALS                            
         SPACE 1                                                                
*----------------------------------------------------------------------         
*        PRODUCE REPORT TOTALS                                                  
*----------------------------------------------------------------------         
         USING TOTTABD,R2                                                       
         L     R2,TOTTAB                                                        
         LA    R0,TOTNUM                                                        
SRT310   CLI   TOTLEV,X'FF'                                                     
         BE    SRT320                                                           
         LA    R2,TOTLEN(R2)                                                    
         BCT   R0,SRT310                                                        
         SPACE 1                                                                
SRT320   EQU   *                                                                
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,2                                                       
         USING PRINTD,R6                                                        
         LA    R6,P                                                             
         MVC   PRINV(9),=C'RUN TOTAL'                                           
         ZAP   DOUBLE,TOTAMNT                                                   
         SP    DOUBLE,TOTTAX                                                    
         LA    R3,PRLESSTX                                                      
         BAS   RE,PRTAMNT                                                       
         ZAP   DOUBLE,TOTTAX                                                    
         LA    R3,PRTAX                                                         
         BAS   RE,PRTAMNT                                                       
         ZAP   DOUBLE,TOTAMNT                                                   
         LA    R3,PRGROSS                                                       
         BAS   RE,PRTAMNT                                                       
         MVI   SPACING,2                                                        
         BAS   RE,PRINTIT                                                       
         SPACE 1                                                                
         SPACE 1                                                                
         CLI   QOPT1,C'Y'          Q, TAPE OUTPUT OPTION                        
         BNE   SRTX                 N, DON'T CLOSE TAPE OUTPUT                  
         NI    IOSW,X'FF'-TPOPEN    Y, CLOSE TAPE OUTPUT FILE                   
         CLOSE (OUTP)                                                           
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
SRTX     EQU   *                                                                
         CLI   QOPT1,C'Y'                                                       
         BNE   SRTXX                                                            
         MVI   RCSUBPRG,1                                                       
         MVC   P+1(22),=C'TOTAL RECORDS ON TAPE:'                               
         LA    R3,P+25                                                          
         EDIT  (P4,TPCT),(15,(R3)),COMMAS=YES                                   
         BAS   RE,PRINTIT                                                       
SRTXX    B     EXIT                                                             
         DROP  R5,R6                                                            
         EJECT                                                                  
*----------------------------------------------------------------------         
*        RETURN TAX OF A BILL IN DUB                                            
*----------------------------------------------------------------------         
GETTAX   NTR1                                                                   
         ZAP   DUB,=P'0'                                                        
         USING TRNELD,R7                                                        
         L     R7,ADTRANS                                                       
         USING BIND,R3                                                          
         L     R3,TAXTAB           TABLE OF BILLED DR'S                         
         L     R0,BININ            NUMBER OF ENTRIES IN TABLE                   
         LTR   R0,R0                                                            
         BZ    EXIT                                                             
         LA    R3,BINTABLE                                                      
         USING TAXTABD,R3                                                       
GETTX10  CLC   TRNREF,TAXNUM                                                    
         BNE   GETTX20                                                          
         SPACE 1                                                                
         MVC   TAXNUM,SPACES                                                    
         ZAP   DUB,TAXAMNT                                                      
         B     EXIT                                                             
GETTX20  EQU   *                                                                
         CLC   TAXDATE,SVDT2                                                    
         BNE   GETTX30                                                          
         MVC   TAXDATE,SPACES                                                   
         ZAP   DUB,TAXAMNT                                                      
         B     EXIT                                                             
         SPACE 1                                                                
GETTX30  LA    R3,TAXTABLN(R3)                                                  
         BCT   R0,GETTX10                                                       
         B     EXIT                FOR NOW                                      
         DROP  R3,R7                                                            
         EJECT                                                                  
*----------------------------------------------------------------------         
*        PUT A TRANSACTION LEVEL RECORD TO SORT                                 
*        FILL TPREC WITH TRANSACTION LEVEL DATA                                 
*----------------------------------------------------------------------         
PUTSORT  NTR1                                                                   
         TM    IOSW,SRTOPEN                                                     
         BO    PUTS020                                                          
         OI    IOSW,SRTOPEN                                                     
         GOTO1 ADSORTER,DMCB,SORTCARD,RECCARD,0                                 
*                                                                               
         USING SRTD,R5                                                          
PUTS020  LA    R5,SRTDATA                                                       
         MVI   SRTTYPE,1           SEND REPORT SORT REC                         
         GOTO1 ADSORTER,DMCB,=C'PUT',SRTDATA                                    
*                                                                               
         AP    REQCNT,=P'1'                                                     
         AP    REQTOT,SRTAMNT                                                   
*                                  SEND TAPE REPORT REV                         
         LA    R5,SRTDATA2                                                      
         MVC   SRTDATA2(SRTDLN),SRTDATA                                         
         MVI   SRTTYPE,2                                                        
         XC    SRTCC,SRTCC                                                      
         MVC   SRTKNO,SRTNO        SORT BY INVOICE NUMBER                       
         GOTO1 ADSORTER,DMCB,=C'PUT',SRTDATA2                                   
         B     EXIT                                                             
         EJECT                                                                  
WRTP     NTR1                                                                   
         CLI   QOPT1,C'Y'          TAPE REQUESTED                               
         BNE   EXIT                NO                                           
         TM    IOSW,TPOPEN         HAS TAPE BEEN OPENED                         
         BO    WRTP50              YES                                          
         LH    R2,OUTCNT           NUMBER OF TIMES TAPE OPENED/CLOSED           
         LA    R2,1(R2)                                                         
         STH   R2,OUTCNT                                                        
         GOTO1 DYNALLOC,DMCB,(0,DDPARM),((R2),DSPARM)                           
         OPEN  (OUTP,(OUTPUT))     NO, OPEN IT                                  
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OI    IOSW,TPOPEN                                                      
         SPACE 1                                                                
         MVC   TPSAVE(L'TPREC),TPDATA                                           
         LA    R3,TPDATA           BUILD VA TYPE TAPE REC                       
         USING TPRECD,R3                                                        
         MVC   VAREC,SPACES                                                     
         MVC   VATYPE,=C'VA'                                                    
         MVI   VAACTION,C'1'       ASSUME POSITIVE GROSS                        
*                                                                               
         USING TOTTABD,R2                                                       
         L     R2,TOTTAB           GET TAPE TOTAL                               
         LA    R0,TOTNUM                                                        
WRTP30   CLI   TOTLEV,X'FF'                                                     
         BE    WRTP40                                                           
         LA    R2,TOTLEN(R2)                                                    
         BCT   R0,WRTP30                                                        
*                                                                               
WRTP40   CP    TOTAMNT,=P'0'                                                    
         BNL   *+8                                                              
         MVI   VAACTION,C'2'                                                    
         MVC   VACOMP,=C'NY '                                                   
         MVC   VABATCH,SVBATCH                                                  
         MVC   VABTDATE,MMDDYY       TODAY                                      
         MVC   VADIST,=C'DO'                                                    
*                                                                               
         ZAP   DUB,RUNCNT                                                       
         UNPK  VACOUNT,DUB                                                      
         OI    VACOUNT+L'VACOUNT-1,X'F0'                                        
*                                                                               
         ZAP   DUB,RUNTOT                                                       
         UNPK  VAAMOUNT,DUB                                                     
         OI    VAAMOUNT+L'VAAMOUNT-1,X'F0'                                      
         PUT   OUTP,TPDATA                                                      
*                                                                               
         MVC   TPDATA(L'TPREC),TPSAVE                                           
         SPACE 1                                                                
WRTP50   PUT   OUTP,TPDATA                                                      
         AP    TPCT,=P'1'                                                       
         B     EXIT                                                             
         EJECT                                                                  
*---------------------------------------------------------------------          
*        STORE 'CC' COMMENTS AT 0(R2) AND 'AC' COMMENTS AT 6(R2)                
*---------------------------------------------------------------------          
         SPACE 1                                                                
GETCOMMS NTR1                                                                   
         USING SCMELD,R7                                                        
         MVC   0(L'SCMCODE,R2),SPACES                                           
         MVC   6(L'SCMCODE,R2),SPACES                                           
         L     R7,ADACC                                                         
         MVI   ELCODE,X'3E'                                                     
         BAS   RE,GETEL                                                         
         BNE   GETCX                                                            
         SPACE 1                                                                
         USING SCMELD,R7                                                        
GETC10   MVC   WORK(L'SCMCODE),SCMCODE                                          
         CLC   WORK(L'SCMCODE),SPACES                                           
         BNH   GETC50                                                           
         SPACE 1                                                                
GETC15   CLI   WORK,C' '           FIRST CHAR IS SPACE                          
         BH    GETC20                                                           
         MVC   WORK(L'SCMCODE),WORK+1 SHIFT OUT LEADING SPACES                  
         B     GETC15                                                           
GETC20   CLC   =C'CC',WORK         IS THIS A CONTACT NAME?                      
         BNE   GETC30                                                           
         MVC   0(L'SCMCODE,R2),SCMCODE                                          
         B     GETC50                                                           
         SPACE 1                                                                
GETC30   CLC   =C'AC',WORK         IS THIS AN EXPENSE CODE                      
         BNE   GETC50                                                           
         MVC   6(L'SCMCODE,R2),SCMCODE                                          
         SPACE 1                                                                
GETC50   BAS   RE,NEXTEL                                                        
         BE    GETC10                                                           
         SPACE 1                                                                
GETCX    B     EXIT                                                             
         EJECT                                                                  
PRTAMNT  ST    RE,SVRE             GENERAL EDIT ROUTINE                         
         EDIT  (P8,DOUBLE),(11,(R3)),2,COMMAS=YES,MINUS=YES                     
         L     RE,SVRE                                                          
         BR    RE                                                               
         SPACE 1                                                                
PRTCNT   ST    RE,SVRE                                                          
         EDIT  (P8,DOUBLE),(11,(R3)),COMMAS=YES,MINUS=YES                       
         L     RE,SVRE                                                          
         BR    RE                                                               
         SPACE 1                                                                
PRINTIT  ST    RE,SVRE                                                          
         GOTO1 ACREPORT            PRINT SUBROUTINE                             
         L     RE,SVRE                                                          
         BR    RE                  RETURN TO USER                               
         SPACE 3                                                                
TPDUMP   ST    RE,SVRE                                                          
         LA    R2,256                                                           
         GOTO1 PRNTBL,DMCB,(5,=C'TPREC'),TPREC,C'DUMP',(R2),=C'1D'              
         L     RE,SVRE                                                          
         BR    RE                  RETURN TO USER                               
         SPACE 3                                                                
         GETEL R7,DATADISP,ELCODE  GET ELEMENT                                  
         EJECT                                                                  
*----------------------------------------------------------------------         
*        BUMP AMOUNTS, PRODUCE TOTALS ETC                                       
*----------------------------------------------------------------------         
TOTALS   NTR1                                                                   
         USING TOTTABD,R2                                                       
         L     R2,TOTTAB                                                        
         CLC   LASTREC(SRTDLN),SPACES  WAS THERE A PREVIOUS SORT REC            
         BZ    TOTX               NO                                            
TOT30    CLI   TOTLEV,X'FF'      REPORT TOTALS                                  
         BE    TOTX              YES, HANDLE WHEN YOU CLOSE SORT                
         ZIC   R1,TOTCLC                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SRTDATA(0),LASTREC                                               
         BE    TOTX                                                             
         SPACE 1                                                                
         USING PRINTD,R6                                                        
         ZIC   R1,TOTOFF                                                        
         LA    R6,P                                                             
         AR    R1,R6                                                            
         MVC   0(5,R1),=C'TOTAL'                                                
         ZAP   DOUBLE,TOTAMNT                                                   
         SP    DOUBLE,TOTTAX                                                    
         LA    R3,PRLESSTX                                                      
         BAS   RE,PRTAMNT                                                       
         ZAP   DOUBLE,TOTTAX                                                    
         LA    R3,PRTAX                                                         
         BAS   RE,PRTAMNT                                                       
         ZAP   DOUBLE,TOTAMNT                                                   
         LA    R3,PRGROSS                                                       
         BAS   RE,PRTAMNT                                                       
         MVI   SPACING,2                                                        
         BAS   RE,PRINTIT                                                       
         LA    R3,TOTLEN(R2)       NEXT BUCKET                                  
         AP    TOTAMNT-TOTTABD(L'TOTAMNT,R3),TOTAMNT                            
         AP    TOTTAX-TOTTABD(L'TOTTAX,R3),TOTTAX                               
         ZAP   TOTTAX,=P'0'                                                     
         ZAP   TOTAMNT,=P'0'                                                    
         LR    R2,R3               BUMP R3                                      
         B     TOT30                                                            
         SPACE 1                                                                
TOTX     B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
*        FIRSTS - GET A NEW COMMENT NAME                                        
*----------------------------------------------------------------------         
FIRSTS   NTR1                                                                   
         USING SRTD,R6                                                          
         LA    R6,SRTDATA                                                       
         CLC   SRTCC,LASTREC+1     SAME PERSON CODE                             
         BE    FIRX                YES                                          
         MVC   CONNAME,SPACES                                                   
         CLI   SRTCTYPE,C'N'       IS THEIR A NAME SAVED HERE                   
         BNE   FIR40               NO, GET NARRATIVE                            
         MVC   CONNAME(26),SRTCC                                                
         MVI   FORCEHED,C'Y'                                                    
         B     FIRX                                                             
FIR40    MVC   SVCODE(2),=C'CC'                                                 
         MVC   SVCODE+2(L'SCMKCODE-2),SRTCC                                     
         LA    R2,CONNAME                                                       
         LA    R1,L'CONNAME                                                     
         BAS   RE,GETNARA                                                       
         MVI   FORCEHED,C'Y'                                                    
FIRX     B     EXIT                                                             
         EJECT                                                                  
*----------------------------------------------------------------------         
*        GET A NARRATIVE RECORD FROM A CODE PASSED IN SVCODE                    
*        SAVE IN 0(R2) FOR A LENGTH OF R1                                       
*----------------------------------------------------------------------         
GETNARA  NTR1                                                                   
         MVC   MYKEYSV(L'KEY),KEY  SAVE MONACCS KEY                             
         BCTR  R1,0                DECRINENT R1 FOR MVC                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),SPACES                                                   
         USING SCMRECD,R7                                                       
         L     R7,AIOAREA                                                       
         XC    SCMKEY,SCMKEY                                                    
         MVI   SCMKTYP,SCMKTYPQ                                                 
         MVC   SCMKCPY,SVCOMP      COMP CODE                                    
         MVC   SCMKCODE,SVCODE                                                  
         BAS   RE,READHIGH                                                      
         CLC   SCMKCODE,SVCODE     DID I GET THE COMMENT                        
         BNE   GETNX               NO                                           
         MVI   ELCODE,X'3E'                                                     
         BAS   RE,GETEL                                                         
         BNE   GETNX                                                            
         USING SCMELD,R7                                                        
         ZIC   R3,SCMLN                                                         
         SH    R3,=H'5'                                                         
         CR    R3,R1                                                            
         BNH   GETN50                                                           
         LR    R3,R1               USE R1 IF LEN IS GREATER THAN MAX            
GETN50   EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),SCMNARR     SAVE NARRATIVE DATA                          
         SPACE 1                                                                
GETNX    CLI   MODE,RUNLAST        DO I NEED TO RESTORE MONACCS KEY             
         BE    GETNXX              NO                                           
         L     R7,AIOAREA                                                       
         MVC   KEY,MYKEYSV                                                      
         MVC   KEYSAVE,MYKEYSV                                                  
         MVC   0(42,R7),SPACES                                                  
         MVC   0(L'KEY,R7),MYKEYSV                                              
         BAS   RE,READREAD                                                      
GETNXX   B     EXIT                                                             
         EJECT                                                                  
*----------------------------------------------------------------------         
*        DATA MANAGER INTERFACE                                                 
*----------------------------------------------------------------------         
READHIGH MVC   COMMAND,=CL8'DMRDHI'                                             
         B     CALDMGR                                                          
         SPACE 1                                                                
READREAD MVC   COMMAND,=CL8'DMREAD'                                             
         B     CALDMGR                                                          
         SPACE 1                                                                
CALDMGR  NTR1                                                                   
         GOTO1 DATAMGR,DMCB,COMMAND,=C'ACCOUNT',(R7),(R7)                       
         B     EXIT                                                             
         EJECT                                                                  
*----------------------------------------------------------------------         
*        CONSTANTS                                                              
*----------------------------------------------------------------------         
DSPARM   DC    CL20'ACCTAPE.AC0BTXX1'                                           
DDPARM   DC    CL8'OUTP'                                                        
OUTP     DCB   DDNAME=OUTP,DSORG=PS,LRECL=TPRECLN,MACRF=PM,            X        
               BLKSIZE=TPRECLN*BLKFACT                                          
         SPACE 1                                                                
SORTCARD DC    CL80'SORT FIELDS=(1,55,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=69'                                    
         SPACE 1                                                                
         DS    0D                  DOUBLEWORD ALIGNMENT FOR TPVOLS              
TPVOLS   DC    PL8'1'              GENERATE TAPE(+1) FOR FIRST TAPE             
SQUASHER DC    V(SQUASHER)                                                      
PRNTBL   DC    V(PRNTBL)                                                        
UNDERLIN DC    V(UNDERLIN)                                                      
ADBUFC   DC    A(BUFFALOC)                                                      
TAXTAB   DC    A(TAXTABC)                                                       
WCTAB    DC    A(WCTABC)                                                        
BINADD   DC    A(BINADDC)                                                       
TOTTAB   DC    A(TOTTABC)                                                       
PVOUTAB  DC    A(PVOUTABC)         PRIMARY VOUCHER NUMBERS                      
TAXWCS   DC    A(TAXWCSC)          LIST OF TAX WORK CODES                       
AIOAREA  DC    A(IOAREA)                                                        
         EJECT                                                                  
*----------------------------------------------------------------------         
*        LITERALS                                                               
*----------------------------------------------------------------------         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*----------------------------------------------------------------------         
*        TABLE TO GENERATE SUB TOTALS                                           
*----------------------------------------------------------------------         
         SPACE 1                                                                
TOTTABC  DS    0D                                                               
         SPACE 1                                                                
         DC    AL1(1)                                                           
         DC    AL1((SRTCC-SRTD)+L'SRTCC)                                        
         DC    AL1(PRINV-PRINTD)                                                
         DC    PL8'0'                                                           
         DC    PL8'0'                                                           
         SPACE 1                                                                
         SPACE 1                                                                
         DC    XL1'FF'             END OF TOTAL TABLE                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    PL8'0'                                                           
         DC    PL8'0'                                                           
TOTNUM   EQU   (*-TOTTABC)/TOTLEN  NUMBER OF LEVELS IN TABLE                    
         EJECT                                                                  
*----------------------------------------------------------------------         
*        TABLE OF WC/RATES S                                                    
*----------------------------------------------------------------------         
WCTABC   DS    0D                                                               
         DS    H                                                                
         DC    Y(WCMAX)                                                         
         DS    (WCMAX+WCTABLN)C                                                 
WCMAX    EQU   255                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
*        TABLE OF BILLED TAX  ON A JOB                                          
*        KEY AREA IS COVERED BY BIND                                            
*        DATA AREA IS COVERED BY TAXTABD                                        
*----------------------------------------------------------------------         
         SPACE 1                                                                
TAXTABC  DS    0D                                                               
         DC    F'0'                NUMBER OF TABLE ENTRIES                      
         DC    AL4(TAXTABLN)       RECORD LENGTH                                
         DC    AL4(TAXKYLN)        DISP OF KEY/ KEY LENGTH                      
         DC    A(TAXMAX)           MAXIMUM NUMBER OF ENTRIES                    
         DC    AL1(1)              NUMBER OF BUCKETS                            
         DC    AL1(TAXBUCK-TAXTABD) DISP TO FIRST BUCK                          
         DS    (TAXMAX*TAXTABLN)C                                               
TAXMAX   EQU   500                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
*                                                                               
*----------------------------------------------------------------------         
*        TABLE OF MOS OF TRAN VS PRIMARY VOUCHER NUMBERS                        
*----------------------------------------------------------------------         
PVOUTABC DS    0CL3                                                             
         DC    X'01',C'DI'                                                      
         DC    X'02',C'DJ'                                                      
         DC    X'03',C'DK'                                                      
         DC    X'04',C'DL'                                                      
         DC    X'05',C'DM'                                                      
         DC    X'06',C'DN'                                                      
         DC    X'07',C'DO'                                                      
         DC    X'08',C'DP'                                                      
         DC    X'09',C'DQ'                                                      
         DC    X'10',C'DR'                                                      
         DC    X'11',C'DS'                                                      
         DC    X'12',C'DT'                                                      
PVOUNUM  EQU   (*-PVOUTABC)/L'PVOUTABC                                          
         SPACE 1                                                                
*----------------------------------------------------------------------         
*        TABLE OF WORKCODES CONSIDERED TAX                                      
*----------------------------------------------------------------------         
TAXWCSC  DS    0CL2                                                             
         DC    C'33'                                                            
         DC    C'34'                                                            
         DC    C'35'                                                            
         DC    C'36'                                                            
         DC    C'37'                                                            
         DC    C'38'                                                            
         DC    C'39'                                                            
TAXWCNUM EQU   (*-TAXWCSC)/L'TAXWCSC                                            
         EJECT                                                                  
*----------------------------------------------------------------------         
*        BUFFALO CSECT                                                          
*----------------------------------------------------------------------         
         SPACE 1                                                                
         BUFF  LINES=20,ROWS=1,COLUMNS=2,FLAVOR=PACKED,                X        
               KEYLIST=(1,A)                                                    
         EJECT                                                                  
*              ROUTINE TO ADD TO A BINSRCH TABLE                                
*              PARAM1              A(RECORD TO BE ADDED)                        
*              PARAM2              A(BINSRCH PARAMS)                            
         USING BIND,R5                                                          
BINADDC  DS    0D                                                               
         NMOD1 0,*BINADD*                                                       
         L     R2,=A(ADRC)         RESTORE REGISTER 12                          
         ICM   RC,15,0(R2)                                                      
         L     R3,0(R1)            A(RECORD)                                    
         L     R5,4(R1)            BINSRCH PARAMETERS                           
         MVC   DMCB+8(16),BININ                                                 
         LA    R2,BINTABLE                                                      
         GOTO1 BINSRCH,DMCB,(1,(R3)),(R2)                                       
         OC    DMCB(4),DMCB                                                     
         BNZ   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
         MVC   BININ,DMCB+8        UPDATE COUNT                                 
         CLI   DMCB,1                                                           
         BE    BINXIT              NOT FOUND - ADDED                            
         L     R4,DMCB             A(RECORD FOUND)                              
         ZIC   R6,BINFRST          DISP. TO FIRST BUCKET                        
         AR    R4,R6               RECORD FOUND                                 
         AR    R3,R6               NEW RECORD                                   
         ZIC   R0,BINNUMB          NUMBER OF BUCKETS                            
         AP    0(8,R4),0(8,R3)     ADD NEW TO OLD                               
         LA    R4,8(R4)                                                         
         LA    R3,8(R3)                                                         
         BCT   R0,*-14                                                          
BINXIT   XIT1                                                                   
         LTORG                                                                  
         SPACE 3                                                                
ADRC     DS    F                   SAVE ADDRESS OF REGISTER 12                  
         SPACE 3                                                                
HDHOOK   DS    0D                                                               
         NMOD1 0,*HDHOOK*                                                       
         L     R1,=A(ADRC)         RESTORE REGISTER 12                          
         ICM   RC,15,0(R1)                                                      
         L     R5,ADBOX                                                         
         MVC   HEAD3+66(L'SVMONTH),SVMONTH                                      
         USING BOXD,R5                                                          
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+5,C'T'                                                   
         MVI   BOXROWS+7,C'M'                                                   
         MVI   BOXROWS+56,C'B'                                                  
         USING PRINTD,R6                                                        
         LA    R6,BOXCOLS                                                       
         CLI   RCSUBPRG,1                                                       
         BE    HDH50                                                            
         MVI   PRLEFT,C'L'                                                      
         MVI   PRCOL1,C'C'                                                      
         MVI   PRCOL2,C'C'                                                      
         MVI   PRCOL3,C'C'                                                      
         MVI   PRCOL3A,C'C'                                                     
         MVI   PRCOL4,C'C'                                                      
         MVI   PRCOL5,C'C'                                                      
         MVI   PRCOL6,C'C'                                                      
         MVI   PRRIGHT,C'R'                                                     
         CLI   RCSUBPRG,2          NO CONTACT NAME IN RUN SUMMARY               
         BE    HDH100                                                           
         MVC   HEAD5+15(L'CONNAME),CONNAME                                      
         B     HDH100                                                           
         SPACE 1                                                                
HDH50    MVI   BOXYORN,C'N'        NO BOXES IN REQUEST SUMMARY                  
         B     HDHX                                                             
         SPACE 1                                                                
HDH100   MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
HDHX     XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         DC    C'IOAREA'                                                        
IOAREA   DS    CL49,CL2000                                                      
         EJECT                                                                  
ACBTD    DSECT                                                                  
SVRE     DS    F                                                                
ADBOX    DS    A                                                                
OUTCNT   DS    H                                                                
         SPACE 1                                                                
BUFREC   DS    0D                                                               
BUFKEY   DS    0CL1                                                             
BUFCOMP  DS    CL1                                                              
BUFNET   DS    PL8                                                              
BUFCOM   DS    PL8                                                              
         SPACE 1                                                                
ELCODE   DS    CL1                                                              
         SPACE 1                                                                
TPCT     DS    PL4                                                              
         SPACE 1                                                                
*        NOTE- STORAGE BELOW IS USED TO PASS DATA                               
*        TO SUB-ROUTINES                                                        
         SPACE 1                                                                
COMMAND  DS    CL8                                                              
SVCOMP   DS    XL1                                                              
SVBATCH  DS    CL3                 BATCH NUMBER BASED ON DATE OF REQ            
SVAMOUNT DS    CL11                INVOICE AMOUNT IN CHARACTER FORMAT           
MMDDYY   DS    CL6                 RCDATE IN MMDDYY FORMAT                      
         SPACE 1                                                                
SRTDATA  DS    (SRTDLN)C                                                        
SRTDATA2 DS    (SRTDLN)C                                                        
LASTREC  DS    (SRTDLN)C                                                        
TPDATA   DS    (TPRECLN)C                                                       
TPSAVE   DS    (TPRECLN)C                                                       
VEDATA   DS    CL13                                                             
TAXREC   DS    CL(TAXTABLN)        PUTTAX  INTERFACE                            
PRODCOMM DS    0C                                                               
PRODCC   DS    (L'SCMCODE)C                                                     
PRODAC   DS    (L'SCMCODE)C                                                     
ACCCOMM  DS    0C                                                               
ACCCC    DS    (L'SCMCODE)C                                                     
ACCAC    DS    (L'SCMCODE)C                                                     
SVCODE   DS    (L'SCMCODE)C                                                     
         SPACE 1                                                                
CONNAME  DS    CL36                CONTACT NAME                                 
SVMONTH  DS    CL6                 TAPE MONTH FOR HEADER                        
         SPACE 1                                                                
IOSW     DS    CL1                                                              
SRTOPEN  EQU   1                                                                
TPOPEN   EQU   2                                                                
         SPACE 1                                                                
SVDT2    DS    XL3                                                              
SVDT3    DS    XL3                                                              
SVMOS    DS    CL2                 MOS  FOR TRNBTCH                             
PL16     DS    PL16                                                             
SVNET    DS    PL6                                                              
QSTR3    DS    CL3                 YYMMDD DATES                                 
QEND3    DS    CL3                                                              
QSTR2    DS    CL2                 PACKED DATES                                 
QEND2    DS    CL2                                                              
REQTOT   DS    PL8                                                              
REQCNT   DS    PL8                                                              
RUNTOT   DS    PL8                                                              
RUNCNT   DS    PL8                                                              
MYKEYSV  DS    (L'KEY)C                                                         
         EJECT                                                                  
*----------------------------------------------------------------------         
*        TABLE OF BILLED DEBITS ON A JOB                                        
*----------------------------------------------------------------------         
TAXTABD  DSECT                     TABLE OF DR AMOUNTS                          
TAXNUM   DS    CL6                 BILL NUMBER                                  
TAXDATE  DS    CL2                 DATE FROM 49 EL                              
TAXKYLN  EQU   *-TAXTABD                                                        
TAXSTAT  DS    CL1                                                              
TAXBUCK  EQU   *                                                                
TAXAMNT  DS    PL8                 BILLED FOR W/C                               
TAXTABLN EQU   *-TAXTABD                                                        
         EJECT                                                                  
*----------------------------------------------------------------------         
*        BINSRCH DSECT                                                          
*----------------------------------------------------------------------         
BIND     DSECT                                                                  
BININ    DS    F                   NUMBER OF ENTRIES IN TABLE                   
BINLEN   DS    F                   RECORD LENGTH                                
BINDISP  DS    CL1                 DISPLACEMENT OF KEY                          
BINKEY   DS    CL3                 KEY LENGTH                                   
BINMAX   DS    F                   MAXIMUM NUMBER OF ENTRIES                    
BINNUMB  DS    CL1                 NUMBER OF BUCKETS                            
BINFRST  DS    CL1                 DISPLACEMENT OF 1ST BUCKET                   
BINTABLE DS    0CL1                                                             
         EJECT                                                                  
*----------------------------------------------------------------------         
*        OUTPUT TAPE AND SORT RECORD DSECT                                      
*----------------------------------------------------------------------         
TPRECD   DSECT                                                                  
TPREC    DS    0CL80                                                            
VAREC    DS    0CL80                                                            
VATYPE   DS    CL2                 INPUT TYPE                                   
VAACTION DS    CL1                 ACTION CODE (POSITIVE OR NEG AMOUNT          
VACOMP   DS    CL3                 COMPANY (C'NY ')                             
VABATCH  DS    CL3                                                              
*                                                                               
*                                                                               
*                                                                               
         DS    CL9                 FILLER                                       
VABTDATE DS    CL6                 BATCH DATE (TODAY)                           
VADIST   DS    CL2                 VENDOR INITIALS (DO)                         
         DS    CL4                 FILLER                                       
VACOUNT  DS    CL4                 NUMBER OF INVOICES IN THIS BATCH             
VAAMOUNT DS    CL11                INVOICES TOTAL                               
         DS    CL35                                                             
VALN     EQU   *-VAREC                                                          
         SPACE 1                                                                
         ORG   TPREC                                                            
VEREC    DS    0CL80                                                            
VETYPE   DS    CL2                 INPUT TYPE                                   
VEACTION DS    CL1                 ACTION CODE (POSITIVE OR NEG AMOUNT          
VECOMP   DS    CL3                 COMPANY (C'NY ')                             
VEBATCH  DS    CL3                 D01 TO D12                                   
VEPV     DS    CL2                 PRIMARY VOUCHER (DI THRU DT, MONTH)          
VESV     DS    CL4                 SECONDARY VOUCHER-LAST 4 OF INVOICE          
VEGROSS  DS    CL11                PAYABLE OF INVOICE                           
VENUM    DS    0CL10               INVOICE NUMBER                               
VEMEDIA  DS    CL1                     MEDIA                                    
VEINVNO  DS    CL6                     INVLOICE NUMBER                          
VECLI    DS    CL3                      AND THE CLIENT                          
VEDATE   DS    CL6                 INVOICE DATE                                 
VED44    DS    CL3                 CONSTANT D44                                 
         DS    CL6                 FILLER                                       
VENAME   DS    CL10                CL10'DOREMUS'                                
VE3      DS    CL1                 C'3'                                         
         DS    CL7                                                              
VERCDATE DS    CL6                 MMDDYY RCDATE                                
         DS    CL5                                                              
VELN     EQU   *-VEREC                                                          
         SPACE 1                                                                
         ORG   TPREC                                                            
VLREC    DS    0CL80                                                            
VLTYPE   DS    CL2                 C'VL'                                        
VLFROMVE DS    CL13                COPY FIRST 15 CHARS FROM VE RECORD           
VLDATA   DS    0CL65                                                            
VLNUMBER DS    CL3                 C'001'                                       
         DS    CL24                                                             
VLAMOUNT DS    CL11                                                             
         DS    CL27                                                             
VLLN     EQU   *-VLREC                                                          
         SPACE 1                                                                
         ORG   TPREC                                                            
VMREC    DS    0CL80                                                            
VMTYPE   DS    CL2                 C'VM'                                        
VMFROMVE DS    CL13                SEE ABOVEE                                   
VMDATA   DS    0CL65                                                            
VMNUMBER DS    CL3                                                              
         DS    CL1                                                              
VMACCNUM DS    CL7                 CL7'2210' OR CL7'2230'                       
VMAC     DS    CL6                 USER FIELD AC                                
         DS    CL17                                                             
VMCPJ    DS    CL12                CLIENT PRODUCT JOB                           
         DS    CL8                                                              
VME      DS    CL1                 C'E'                                         
         DS    CL10                                                             
VMLN     EQU   *-VMREC                                                          
         ORG   TPREC+L'TPREC                                                    
TPRECLN  EQU   *-TPREC                                                          
BLKFACT  EQU   1                                                                
         EJECT                                                                  
*----------------------------------------------------------------------         
*        DSECT TO COVER TRANSACTION LEVEL INFORMATION NEEDED TO                 
*        PASS A RECORD TO SORT                                                  
*----------------------------------------------------------------------         
SRTD     DSECT                                                                  
SRTTYPE  DS    CL1                 01, REPORT REC                               
*                                  02, TAPE REC                                 
SRTCC    DS    CL26                COMMENT CODE OR NAME                         
         ORG   SRTCC                                                            
SRTKNO   DS    CL4                 LAST 4 OF BILL NUMBER                        
         ORG   SRTCC+L'SRTCC                                                    
*                                  WHEN THIS IS A TAPE RECORD                   
SRTCTYPE DS    CL1                 C- SRTCC IS A CODE, N- A NAME                
SRTNO    DS    CL6                 INVOICE NUMBER                               
SRTDATE  DS    XL3                 INV DATE (TRNDATE)                           
SRTAC    DS    CL6                 EC NARRATIVE                                 
SRTCPJ   DS    CL12                CLIENT, PRO, JOB                             
SRTAMNT  DS    PL6                 RECEIVABLE                                   
SRTTAX   DS    PL6                 TAX                                          
SRTMOS   DS    CL2                 MDATE FROM MONACC                            
SRTDLN   EQU   *-SRTD                                                           
         EJECT                                                                  
*----------------------------------------------------------------------         
*        DSECT TO COVER PRINT LINE                                              
*----------------------------------------------------------------------         
PRINTD   DSECT                                                                  
PRLEFT   DS    CL1                                                              
PRINV    DS    CL6                 "INVOICE NUMBER"                             
         DS    CL8                                                              
PRCOL1   DS    CL1                                                              
PRDATE   DS    CL8                 "INVOICE DATE"                               
         DS    CL4                                                              
PRCOL2   DS    CL1                                                              
PRCPJ    DS    0CL12                                                            
PRDESC   DS    CL40                "DESCRIPTION""                               
PRCOL3   DS    CL1                                                              
PREXCODE DS    CL6                 "EXPENSE CODE"                               
         DS    CL6                                                              
PRCOL3A  DS    CL1                                                              
PRACCNUM DS    CL6                 "ACCOUNT CODE"                               
         DS    CL6                                                              
PRCOL4   DS    CL1                                                              
PRLESSTX DS    CL11                "  LESS TAX "                                
PRCOL5   DS    CL1                                                              
PRTAX    DS    CL11                "    TAX    "                                
PRCOL6   DS    CL1                                                              
PRGROSS  DS    CL11                "TOTAL COST "                                
PRRIGHT  DS    CL1                                                              
         SPACE 1                                                                
         ORG   PRINV                                                            
PRSUMNET DS    CL15                                                             
PRSUMCOL DS    CL1                                                              
PRSUMCOM DS    CL15                                                             
PRSUMRIT DS    CL1                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
*        DSECT TO COVER HEADER INFO IN TABLES                                   
*----------------------------------------------------------------------         
TABLED   DSECT                                                                  
TABNUM   DS    H                                                                
TABMAX   DS    H                                                                
TABDATA  DS    0C                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
*        DSECT TO COVER WORK CODE RATE TABLE                                    
*----------------------------------------------------------------------         
WCTABD   DSECT                                                                  
WCTBCODE DS    CL2                                                              
WCTBRATE DS    PL6                                                              
WCTABLN  EQU   *-WCTABD                                                         
         EJECT                                                                  
TOTTABD  DSECT                     TO COVER TOTTAB                              
TOTLEV   DS    CL1                                                              
TOTCLC   DS    AL1                                                              
TOTOFF   DS    AL1                 OFFSET TO PRINT "TOTAL"                      
TOTTAX   DS    PL8                                                              
TOTAMNT  DS    PL8                                                              
TOTLEN   EQU   *-TOTTABD                                                        
         EJECT                                                                  
* ACGENMODES ACGENBOTH ACREPWORKD GETOPTD                                       
GETOPTD  DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE ACGOBLOCK                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACMASTD                                                        
       ++INCLUDE ACBIGPRNTD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDREPXTRAD                                                     
       ++INCLUDE DDREPMASTD                                                     
       ++INCLUDE DDBOXEQUS                                                      
         PRINT ON                                                               
JBLOCKD  DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE ACJOBBLOCK                                                     
       ++INCLUDE ACJOBBERD                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'044ACREPBT02 05/01/02'                                      
         END                                                                    
