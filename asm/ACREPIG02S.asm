*          DATA SET ACREPIG02S AT LEVEL 120 AS OF 05/01/02                      
*PHASE ACIG02A                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*                                                                               
         TITLE 'ACIG02 - BILLING INTERFACE FOR COLGATE'                         
ACIG02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACIG**,R9                                                    
         USING ACWORKD,RA                                                       
         L     RA,0(R1)                                                         
         USING ACIGD,RC                                                         
         LA    RC,SPACEND                                                       
*                                                                               
         CLI   MODE,RUNFRST        Q, VERY FIRST TIME AROUND                    
         BE    RUNF                 Y, DO RUN INITIALIZATION                    
         CLI   MODE,REQFRST        Q, FIRST TIME FOR A REQUEST                  
         BE    REQF                 Y, DO REQUEST INITIALIZATION                
         CLI   MODE,PROCACC        Q, AN ACCOUNT RECORD                         
         BE    PACC                 Y, PROCESS ACCOUNT LEVEL RECORD             
         CLI   MODE,PROCSBAC       Q, A SUB ACCOUNT RECORD                      
         BE    PSBACC               Y, PROCESS THE SUB ACCOUNT                  
         CLI   MODE,PROCTRNS       Q, A TRANSACTION                             
         BE    PTRN                 Y, PROCESS TRANSACTION/ELEMENTS             
         CLI   MODE,REQLAST        Q, END OF REQUEST                            
         BE    REQL                 Y, END                                      
EXIT     XIT1                                                                   
         EJECT                                                                  
*******************************************************************             
* RUN FIRST                                                       *             
*******************************************************************             
         SPACE 1                                                                
RUNF     GOTO1 DATCON,DMCB,(5,0),(0,TODAY)                                      
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
*                                                                               
         DROP  R2                                                               
*                                                                               
         MVI   ZEROS,C'0'                                                       
         MVC   ZEROS+1(L'ZEROS-1),ZEROS                                         
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*******************************************************************             
* REQUEST FIRST                                                   *             
*******************************************************************             
         SPACE 1                                                                
REQF     MVC   QEND3,=XL3'FF'      INIT DATES                                   
         MVC   QEND2,=XL3'FF'                                                   
         XC    QSTR3,QSTR3                                                      
         XC    QSTR2,QSTR2                                                      
         CLC   QSTART(L'QSTART+L'QEND),SPACES  ANY START OR END                 
         BE    REQF20                                                           
         CLC   QSTART,SPACES                                                    
         BE    REQF10                                                           
         GOTO1 DATCON,DMCB,(0,QSTART),(1,QSTR3)                                 
         GOTO1 DATCON,DMCB,(0,QSTART),(2,QSTR2)                                 
*                                                                               
REQF10   CLC   QEND,SPACES                                                      
         BE    REQF20                                                           
         GOTO1 DATCON,DMCB,(0,QEND),(1,QEND3)                                   
         GOTO1 DATCON,DMCB,(0,QEND),(2,QEND2)                                   
*                                                                               
REQF20   XC    IOSW,IOSW                                                        
         ZAP   TPCT,=P'0'                                                       
         ZAP   TOTLNET,=P'0'       ACCUMS FOR TRAILER RECORD                    
         ZAP   TOTLCOM,=P'0'                                                    
         ZAP   TOTLRECV,=P'0'      REPORT ONLY                                  
         ZAP   INVCNT,=P'0'        NO. OF HEADER RECORDS                        
         ZAP   TOTLRECS,=P'0'      NO. OF HEADER + DETAIL RECS                  
         ZAP   TOTDETRE,=P'0'      NO. OF DETAIL RECS                           
         ZAP   BINETTOT,=P'0'      INVOICE HEADER'S NET'S TOTALS                
         ZAP   DENETTOT,=P'0'      INVOICE DETAIL'S NET'S TOTALS                
*                                                                               
         MVI   RCSUBPRG,0                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         CLI   QOPT2,C'Y'          PRINT CD OPTION                              
         BNE   *+8                                                              
         MVI   RCSUBPRG,2                                                       
*                                                                               
         CLI   QOPT1,C'Y'          TAPE REQUESTED                               
         BNE   REQF30              NO                                           
*                                                                               
         LH    R2,OUTCNT           NUMBER OF TIMES TAPE OPENED/CLOSED           
         LA    R2,1(R2)                                                         
         STH   R2,OUTCNT                                                        
         GOTO1 DYNALLOC,DMCB,(0,DDPARM),((R2),DSPARM)                           
         OPEN  (OUTP,(OUTPUT))     NO, OPEN IT                                  
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         OI    IOSW,TPOPEN                                                      
*                                                                               
REQF30   DS    0H                                                               
         GOTO1 TIME,SVTIME         GET CURRENT TIME                             
*                                                                               
         MVC   VENID,SPACES                                                     
         LA    R1,VENTAB           VENDOR NUMBER TABLE                          
         LA    R0,VENTBLNQ         NUMBER OF ENTRIES IN TABLE                   
REQF40   CLC   ORIGINUM,0(R1)      MATCH ON ORIGIN NUMBER                       
         BNE   REQF50                                                           
         MVC   VENID,2(R1)         MOVE IN VENDOR NUMBER                        
         B     REQFX                                                            
*                                                                               
REQF50   LA    R1,L'VENTAB(R1)                                                  
         BCT   R0,REQF40                                                        
*        DC    H'0'                                                             
         MVC   VENID,=C'1111111'   VENDOR NUMBER                                
REQFX    B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* PROCESS AN ACCOUNT & READ USER FIELDS                              *          
**********************************************************************          
         SPACE 1                                                                
         USING BIND,R4                                                          
PACC     L     R4,DRTAB            CLEAR TABLE AT START OF EACH ACCOUNT         
         XC    BININ,BININ                                                      
         USING TABLED,R4                                                        
         L     R4,WCTAB                                                         
         XC    TABNUM,TABNUM                                                    
         XC    JOBSTAT,JOBSTAT                                                  
*                                                                               
         BAS   RE,BLDJBREC         BUILD JOB LEVEL DATA                         
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
*******************************************************************             
* PROCESS A SUB ACCOUNT                                           *             
*******************************************************************             
         SPACE 1                                                                
PSBACC   MVC   SAVECANM,SPACES                                                  
         L     R7,ADSUBAC                                                       
         AH    R7,DATADISP                                                      
         CLI   0(R7),CACELQ                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CACELD,R7                                                        
         ZIC   R1,CACLN                                                         
         SH    R1,=H'18'                                                        
         BM    EXIT                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SAVECANM(0),CACNAME                                              
         B     EXIT                                                             
         DROP  R7                                                               
         EJECT                                                                  
*******************************************************************             
* PROCESS A TRANSACTION                                           *             
*******************************************************************             
         SPACE 1                                                                
         USING TRNRECD,R6                                                       
PTRN     L     R6,ADTRANS                                                       
         SH    R6,DATADISP                                                      
         USING TRNELD,R7                                                        
         L     R7,ADTRANS          GETS ADDRESS OF TRAN ELEMENT                 
         CLI   TRNEL,TRNELQ       Q, TRAN ELEMENT-44, ALWAYS FIRST              
         BNE   EXIT                 N, EXIT---BAD TRAN                          
         CLC   TRNANAL,=C'**'      NO P/O'S                                     
         BE    EXIT                                                             
         CLC   TRNANAL,=C'99'      BUILD TABLE IF WORK CODE NOT '99'            
         BE    PTRN100                                                          
*                                                                               
* PROCESS MARKED CHARGES ON THE JOB, SAVE TO DETERMINE WORK                     
*         CODE VALUES FOR BILLS                                                 
*                                                                               
         ZAP   INVNET,=P'0'                                                     
         ZAP   INVCOM,=P'0'                                                     
         ZAP   INVCD,=P'0'                                                      
         USING DRTABD,R5                                                        
         LA    R5,DRTABREC                                                      
*                                                                               
         MVC   DRTSUB,TRNKSBR      TRANSACTION SUB REFERENCE NUM                
         MVC   DRTWC,TRNANAL                                                    
         MVC   DRTSTAT,TRNSTAT                                                  
         MVC   DRTNAME,SAVECANM                                                 
         ZAP   INVNET,TRNAMNT                                                   
         MVC   DRTNARR,SPACES                                                   
*                                                                               
         ZIC   R1,TRNLN                                                         
         SH    R1,=Y(TRNLN1Q)                                                   
         CH    R1,=Y(L'DRTNARR)    BIG NARRATIVE                                
         BNH   *+8                                                              
         LH    R1,=Y(L'DRTNARR)                                                 
         BCTR  R1,0                                                             
         LTR   R1,R1                                                            
         BM    PTRN10              NO NARRATIVE                                 
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DRTNARR(0),TRNNARR                                               
*                                                                               
         USING SCIELD,R7                                                        
PTRN10   MVI   ELCODE,SCIELQ       GET ANY CASH DISCOUNT                        
         L     R7,ADTRANS                                                       
PTRN20   BAS   RE,NEXTEL                                                        
         BNE   PTRN30                                                           
*                                                                               
         CLI   SCITYPE,SCITCDSC                                                 
         BNE   PTRN20                                                           
*                                                                               
         ZAP   INVCD,SCIAMNT                                                    
*                                                                               
         USING ACMD,R7                                                          
PTRN30   MVI   ELCODE,PTAELQ                                                    
         ZAP   DOUBLE,=P'0'                                                     
         L     R7,AMONACC                                                       
         L     R7,ACMAPRO2                                                      
         CLI   0(R7),PTAELQ        ANY 77 ELEMENTS?                             
         B     *+8                                                              
*                                                                               
         USING PTAELD,R7                                                        
PTRN40   BAS   RE,NEXTEL                                                        
         BNE   PTRN80              NO (MORE) 77'S                               
*                                                                               
         CLI   PTATYPE,PTATRAL     IS THIS A BILLING ELEMENTS?                  
         BNE   PTRN40                                                           
         TM    PTASTAT1,PTASPEND   IS IT PENDING?                               
         BO    PTRN40                                                           
         TM    PTASTAT1,PTASREVU   REVERSAL                                     
         BO    PTRN50                                                           
         TM    PTASTAT1,PTASREVS                                                
         BO    PTRN60                                                           
         B     PTRN70                                                           
*                                                                               
PTRN50   MVC   DRTNUM,=6X'EE'      EE TO INVOICE                                
         MVC   DRTDATE,PTARDATE   SAVE ORIGINAL BILL DATE                       
         ZAP   DRTAMNT,INVNET        TRNAMNT                                    
         ZAP   DRTCD,INVCD           CASH DISC AMOUNT                           
         BAS   RE,ADDIT              ADD TO BINTABLE                            
         B     PTRN40                                                           
*                                                                               
PTRN60   MVC   DRTDATE,PTARDATE   SAVE DATE REVERSED                            
         MVC   DRTNUM,=6X'FF'      FF TO INVOICE                                
         ZAP   DRTAMNT,INVNET      TRNAMNT,                                     
         ZAP   DRTCD,INVCD         CASH DISC AMOUNT                             
         MP    DRTAMNT,=P'-1'      'REVERSED'                                   
         MP    DRTCD,=P'-1'                                                     
         BAS   RE,ADDIT             ADD TO BINTABLE                             
         B     PTRN40                                                           
*                                                                               
PTRN70   MVC   DRTNUM,PTARBLNO     MOVE INVOICE AND AMOUNT TO TABLE             
         MVC   DRTDATE,PTARBLDT                                                 
         ZAP   DRTAMNT,PTANET                                                   
         AP    DOUBLE,PTANET       KEEP TOTAL BILLED ON THIS TRANS              
*                                                                               
         ZAP   DRTCD,PTACDSC                                                    
         AP    DOUBLE,PTACDSC      OR ANY CD                                    
         BAS   RE,ADDIT                                                         
         B     PTRN40                                                           
*                                                                               
PTRN80   OC    ACCOUSED(2,R6),ACCOUSED(R6)   FULLY BILLED                       
         BZ    PTRN90              NO                                           
*                                                                               
         CP    DOUBLE,=P'0'        DID I GET AN AMOUNT IN THE 77                
         BNE   EXIT                YES                                          
         ZAP   DRTAMNT,INVNET                                                   
         ZAP   DRTCD,INVCD                                                      
         MVC   DRTDATE,ACCOUSED(R6)                                             
         BAS   RE,ADDIT                                                         
         B     EXIT                GET NEXT TRANSACTION                         
*                                                                               
PTRN90   B     EXIT                                                             
         DROP  R5,R6,R7                                                         
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        PROCESS BILLING                                             *          
*           IF BILL HAS BEEN ADDED WITHIN THE DETAILS OF REQUEST     *          
*           WRITE IT TO TAPE                                         *          
*--------------------------------------------------------------------*          
         SPACE 1                                                                
PTRN100  L     R7,ADTRANS          FIND OUT WHAT TYPE OF BILL THIS IS           
*                                                                               
         XC    SVDT2,SVDT2                                                      
         XC    SVDT3,SVDT3                                                      
         MVI   ELCODE,TRSELQ       FIND DATE THIS BILL ADDED TO FILE            
         USING TRSELD,R7                                                        
         BAS   RE,NEXTEL                                                        
         BNE   EXIT                DONT KNOW WHEN IT WAS ADDED                  
*                                                                               
         CLC   TRSDATE,QEND2    IS TRANSACTION DATE = REQUESTED END DT          
         BH    EXIT                                                             
*                                                                               
         CLC   TRSDATE,QSTR2                                                    
         BL    EXIT                                                             
*                                                                               
         USING TRNELD,R7                                                        
         L     R7,ADTRANS          FILL BILL DATA                               
         MVC   BINUM,TRNREF                                                     
         GOTO1 DATCON,DMCB,(1,TRNDATE),(20,BIDATE)                              
*                                                                               
         MVC   BIDRCR,=C'DR'                                                    
         CP    TRNAMNT,=P'0'                                                    
         BNL   *+10                                                             
         MVC   BIDRCR,=C'CR'                                                    
*                                                                               
         ZAP   BINET,TRNAMNT                                                    
         AP    BINETTOT,BINET      INVOICE HEADER'S TOTALS                      
         ZAP   BICOM,TRNNARR+15(6)                                              
         ZAP   BICD,TRNNARR+21(6)                                               
         ZAP   BIREC,TRNNARR+27(6)                                              
*                                                                               
         AP    TOTLRECV,BIREC      BUMP THIS HERE                               
*                                                                               
         USING DUEELD,R7                                                        
         L     R7,ADTRANS          GET BILLS DUEDATE                            
         MVC   BIDUE,SPACES                                                     
         MVI   ELCODE,DUEELQ                                                    
         BAS   RE,NEXTEL                                                        
         BNE   PTRN120                                                          
         GOTO1 DATCON,DMCB,(2,DUEDATE),(20,BIDUE)                               
*                                                                               
PTRN120  BAS   RE,INVHEAD          PUT INVOICE HEADER RECORD                    
         BAS   RE,PRTINVHD         PRINT INVOICE HEADER INFO                    
         BAS   RE,INVDET           PUT INVOICE DETAIL RECORDS                   
*                                                                               
*        L     R6,ADTRANS                                                       
*        SH    R6,DATADISP                                                      
*        USING TRNRECD,R6                                                       
*        CLI   TRNKCUNT,C'3'       CONTRA UNIT 3                                
*        BE    PTRN140             YES, SKIP CHECK ON RETAIL BILL               
*                                                                               
*        CP    INVNET,BINET        MAKE SURE SUM OF DETAILS IS OK               
*        BNE   PTRN130                                                          
*                                                                               
*        CP    INVCD,BICD         MAKE SURE SUM OF CASH DISC  OK                
*        BNE   PTRN130                                                          
*                                                                               
*        CP    INVCOM,BICOM        MAKE SURE SUM OF COMMISSION OK               
*        BE    PTRN140                                                          
*                                                                               
*        USING DRTABD,R5                                                        
* PTRN130  DS    0H                                                             
*        LA    R5,DRTABREC                                                      
*        MVC   0(DRTABLN,R5),SPACES                                             
*                                                                               
*        ZAP   DRTAMNT,BINET                                                    
*        SP    DRTAMNT,INVNET                                                   
*                                                                               
*        ZAP   DRTCD,BICD                                                       
*        SP    DRTCD,INVCD                                                      
*                                                                               
*        ZAP   DRTCOM,BICOM                                                     
*        SP    DRTCOM,INVCOM                                                    
*                                                                               
*        MVC   DRTWC,=C'YO'                                                     
*        MVC   DRTNAME(14),=C'**COMMISSION**'                                   
*        LA    R3,DRTABREC                                                      
*        BAS   RE,PUTDR                                                         
*                                                                               
PTRN140  DS    0H                                                               
         B     EXIT                                                             
*                                                                               
*        DROP  R5                                                               
         DROP  R7                                                               
         EJECT                                                                  
*******************************************************************             
* REQUEST LAST                                                    *             
*******************************************************************             
         SPACE 1                                                                
REQL     BAS   RE,PUTTRAIL         PUT INVOICE TRAILER RECORD                   
         MVC   P+1(10),=C'END OF RUN'                                           
         MVI   SPACING,2                                                        
         BAS   RE,PRINTEM                                                       
*        CLI   QOPT1,C'Y'          Q, TAPE OUTPUT OPTION                        
*        BNE   REQL10               N, DON'T CLOSE TAPE OUTPUT                  
*                                                                               
         MVC   P+1(12),=C'SUM OF GROSS'                                         
         USING PRINTD,R3                                                        
         LA    R3,P                                                             
         ZAP   DOUBLE,BINETTOT     HEADER'S TOTALS                              
         LA    R3,PRNET                                                         
         BAS   RE,PRTAMNT                                                       
         BAS   RE,PRINTEM                                                       
*                                                                               
         MVC   P+1(20),=C'SUM DISTRIB LINE AMT'                                 
         USING PRINTD,R3                                                        
         LA    R3,P                                                             
         ZAP   DOUBLE,DENETTOT     DETAIL'S TOTALS                              
         LA    R3,PRNET                                                         
         BAS   RE,PRTAMNT                                                       
         BAS   RE,PRINTEM                                                       
         BAS   RE,PRINTEM                                                       
*                                                                               
         ZAP   DOUBLE,INVCNT                                                    
         LA    R3,P+23                                                          
         EDIT  (P8,DOUBLE),(4,(R3))                                             
         MVC   P+1(14),=C'HEADER RECORDS'                                       
         BAS   RE,PRINTEM                                                       
*                                                                               
         ZAP   DOUBLE,TOTDETRE     NO. OF DETAIL RECORDS                        
         LA    R3,P+23                                                          
         EDIT  (P8,DOUBLE),(4,(R3))                                             
         MVC   P+1(14),=C'DETAIL  RECORDS'                                      
         BAS   RE,PRINTEM                                                       
*                                                                               
         ZAP   DOUBLE,TOTLRECS                                                  
         LA    R3,P+23                                                          
         EDIT  (P8,DOUBLE),(4,(R3))                                             
         MVC   P+1(20),=C'HEADER + DETAIL RECS'                                 
         BAS   RE,PRINTEM                                                       
*                                                                               
         CLI   QOPT1,C'Y'          Q, TAPE OUTPUT OPTION                        
         BNE   EXIT                                                             
         NI    IOSW,X'FF'-TPOPEN    Y, CLOSE TAPE OUTPUT FILE                   
         CLOSE (OUTP)                                                           
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         B     EXIT                                                             
         EJECT                                                                  
*******************************************************************             
* ADD AN ENTRY TO DRTAB                                           *             
*******************************************************************             
         SPACE 1                                                                
ADDIT    NTR1                                                                   
         USING DRTABD,R5                                                        
         CP    DRTAMNT,=P'0'       ANYTHING TO ADD                              
         BE    ADDITX              NO                                           
*                                                                               
         ZAP   DRTCOM,=P'0'                                                     
         TM    DRTSTAT,X'01'       NON COMM CHARGE                              
         BO    ADDIT40             YES                                          
*                                                                               
         USING TABLED,R4                                                        
         L     R4,WCTAB            TABLE OF WORK CODE COMMISISONS               
         LH    R0,TABNUM                                                        
         LA    R3,TABDATA                                                       
         LTR   R0,R0               ANYTHING SAVED HERE                          
         BZ    ADDIT20             NO, CALL GETOPT FOR RATE                     
         CH    R0,TABMAX           AM I AT MAX FOR TABLE                        
         BL    *+6                 NO, THERE IS STILL ROOM                      
         DC    H'0'                                                             
         USING WCTABD,R3                                                        
ADDIT10  CLC   WCTBCODE,DRTWC                                                   
         BE    ADDIT30                                                          
         LA    R3,WCTABLN(R3)                                                   
         BCT   R0,ADDIT10                                                       
*                                                                               
ADDIT20  DS    0H                  RATE FOR WORK CODE NOT FOUND                 
         L     R6,ADGOBLOC         GET RATE FOR THIS WORKCODE                   
         USING GETOPTD,R6                                                       
         MVC   GOSELWC,DRTWC                                                    
         GOTO1 GETOPT,DMCB,GETOPTD   GET WORK CODE RATE                         
         XC    GOSELWC,GOSELWC       RESET GETOPT                               
         ZAP   WCTBRATE,GOAGYCOM                                                
         MVC   WCTBCODE,DRTWC                                                   
*                                                                               
         LH    R1,TABNUM                                                        
         LA    R1,1(R1)                                                         
         STH   R1,TABNUM                                                        
*                                                                               
ADDIT30  ZAP   PL16,DRTAMNT                                                     
         MP    PL16,WCTBRATE                                                    
         SRP   PL16,64-6,5                                                      
         ZAP   DRTCOM,PL16                                                      
ADDIT40  GOTO1 BINADD,DMCB,(R5),DRTAB                                           
*                                                                               
ADDITX   B     EXIT                                                             
         DROP  R3,R4,R5,R6                                                      
         EJECT                                                                  
*******************************************************************             
* BUILD TPREC WITH JOB LEVEL DATA AND CONSTANTS                   *             
*******************************************************************             
         SPACE 1                                                                
BLDJBREC NTR1                                                                   
         BAS   RE,RESUFDAT         CLEAR USER FIELD DATA                        
*                                                                               
         USING ACTRECD,R7                                                       
         L     R7,ADACC            POINTS TO ACCOUNT RECORD                     
         MVC   JBCPJ,ACTKACT       SAVE CLI,PRO,JOB                             
         MVC   JBPRO,ACTKACT+3     SAVE PRODUCT                                 
*                                                                               
         USING NAMELD,R7                                                        
         MVC   JBNAME,SPACES                                                    
         MVI   ELCODE,NAMELQ                                                    
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         ZIC   R1,NAMLN                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   JBNAME(0),NAMEREC                                                
*                                                                               
         BAS   RE,LOADUSER         LOAD USER FIELDS                             
         B     EXIT                                                             
         DROP  R7                                                               
         EJECT                                                                  
**********************************************************************          
* MOVE SPACES TO JBDATA                                              *          
**********************************************************************          
         SPACE 1                                                                
RESUFDAT NTR1                                                                   
         LA    R2,JBDATA                                                        
         LM    R3,R5,=A(JBLEN,0,C' ')                                           
         SLL   R5,24               SHIFT THE C' ' TO TOP BYTE                   
         MVCL  R2,R4                                                            
*                                                                               
         USING LUD,R1                                                           
         L     R1,AUSERTAB         RESET USER FIELD MAX LENS                    
         LA    R0,LUNUM                                                         
         MVI   LUJBLEN,L'JBUFS                                                  
         LA    R1,LUDLN(R1)                                                     
         BCT   R0,*-8                                                           
         B     EXIT                                                             
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* LOAD JBDATA WITH DATA FROM THE USER FIELDS ON THIS JOB              *         
***********************************************************************         
         SPACE 1                                                                
LOADUSER NTR1                                                                   
         USING UFSELD,R7                                                        
         L     R7,ADACC                                                         
         MVI   ELCODE,UFSELQ                                                    
         BAS   RE,GETEL                                                         
LU10     BNE   EXIT                                                             
         USING LUD,R3                                                           
         L     R3,AUSERTAB                                                      
         LA    R0,LUNUM                                                         
*                                                                               
LU20     CLC   UFSCODE,LUCODE      IS THIS CODE IN THE TABLE                    
         BE    LU30                YES, SAVE IT IN JBDATA                       
         LA    R3,LUDLN(R3)                                                     
         BCT   R0,LU20                                                          
         B     LU40                THIS USER FIELD NOT NEEDED ON TAPE           
*                                                                               
LU30     SR    R2,R2                                                            
         IC    R2,UFSLN                                                         
         SH    R2,=Y(UFSLN1Q+1)    SUBTRACT OVERHEAD+1 FOR EX                   
         BM    LU40                NO DATA ON THIS USER FIELD                   
*                                                                               
         LA    R1,JBUFS            POINT R1 TO FIELD FOR THIS UF                
         SR    R0,R0                                                            
         ICM   R0,B'0011',LUJBFLD  OFFSET INTO JBUFS OF THIS FIELD              
         AR    R1,R0                                                            
*                                                                               
         SR    R6,R6                                                            
         IC    R6,LUJBLEN          LENGTH OF THIS TAPE FIELD                    
         BCTR  R6,0                                                             
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),SPACES                                                   
*                                                                               
         CR    R2,R6               Q, ACTUAL LENGTH GREATER THAN MAX.           
         BNH   *+6                                                              
         LR    R2,R6               Y, ALLOW ONLY THE MAX. LENGTH                
         LR    R6,R2                                                            
         AHI   R6,1                ADD BACK THE 1 BYTE FOR THE EX               
         STC   R6,LUJBLEN          RESET ACTUAL LENGTH INTO FIELD               
         EX    R2,*+4                                                           
         MVC   0(0,R1),UFSDATA                                                  
LU40     BAS   RE,NEXTEL                                                        
         B     LU10                                                             
         DROP  R3,R7                                                            
         EJECT                                                                  
***********************************************************************         
* PUT A BILL OUT AS AN INVOICE HEADER RECORD                          *         
***********************************************************************         
         SPACE 1                                                                
INVHEAD  NTR1                                                                   
         USING TRNELD,R7                                                        
         L     R7,ADTRANS                                                       
         BAS   RE,CLRTP            CLEAR TAPE TO SPACES                         
*                                                                               
         USING PCOL1D,R5                                                        
         LA    R5,TPREC                                                         
*                                                                               
         MVC   PCOL1VID,VENID               VENDOR NUMBER                       
         MVC   PCOL1INV(L'BINUM),BINUM      INVOICE NUMBER                      
         MVC   PCOL1OID,=C'Y&&R'            OPERATOR ID                         
*                                                                               
         MVI   PCOL1IC,C'I'        ASSUME IT IS DEBIT                           
         MVC   PCOL1DT,=C'RN'      DOCUMENT TYPE RN FOR DEBIT                   
         CLC   BIDRCR,=C'DR'                                                    
         BE    *+14                                                             
         MVI   PCOL1IC,C'C'        IT'S CREDIT                                  
         MVC   PCOL1DT,=C'KG'      DOCUMENT TYPE KG FOR CREDIT                  
*                                                                               
         MVC   PCOL1ENM,JBNAME     ESTIMATE NAME                                
         MVC   PCOL1IDT(4),BIDATE+4 MMDD                                        
         MVC   PCOL1IDT+4(4),BIDATE YYYY                                        
         MVC   PCOL1PT,=C'0030'    PAYMENT TERM                                 
         MVC   PCOL1DD(4),BIDUE+4  DUE DATE MMDD                                
         MVC   PCOL1DD+4(4),BIDUE  DUE DATE YYYY                                
         MVI   PCOL1SN,C'+'        SIGN ALWAYS POSITIVE                         
*                                                                               
         MVC   PCOL1AD,ZEROS                                                    
         ZAP   DUB,BINET                                                        
         UNPK  PCOL1AD+2(16),DUB                                                
         OI    PCOL1AD+17,X'F0'                                                 
*                                                                               
         MVC   PCOL1CC,=CL4'USD'   CURRENCY CODE                                
         MVC   PCOL1LI,=C'US01'    LEDGER ID                                    
         MVC   PCOL1EN,=CL4'101'   ENTITY                                       
         MVI   PCOL1RT,C'1'        RECORD TYPE 1                                
*                                                                               
         BAS   RE,WRTP                                                          
         AP    TOTLRECS,=P'1'                                                   
         AP    INVCNT,=P'1'                                                     
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GIVEN   BILL, PUT ITS TRANSACTION DETAILS OUT TO TAPE               *         
***********************************************************************         
         SPACE 1                                                                
INVDET   NTR1                                                                   
         BAS   RE,CLRTP                                                         
*                                                                               
         ZAP   INVNET,=P'0'                SAVE DETAIL TOTALS                   
         ZAP   INVCOM,=P'0'                                                     
         ZAP   INVCD,=P'0'                                                      
*                                                                               
         USING PCOL2D,R5                                                        
         LA    R5,TPREC                                                         
*                                                                               
         USING BIND,R3                                                          
         L     R3,DRTAB            TABLE OF BILLED DR'S                         
         L     R0,BININ            NUMBER OF ENTRIES IN TABLE                   
         LTR   R0,R0                                                            
         BZ    EXIT                                                             
*                                                                               
         USING TRNELD,R7                                                        
         L     R7,ADTRANS                                                       
*                                                                               
         LA    R3,BINTABLE                                                      
         USING DRTABD,R3                                                        
INVDT10  CLC   TRNREF,DRTNUM                                                    
         BNE   INVDT20                                                          
*                                                                               
         BAS   RE,PUTDR                                                         
         B     INVDT40                                                          
*                                                                               
INVDT20  EQU   *                                                                
         CLI   DRTNUM,X'EE'        IS THIS THE DATE OF AN ORIGINAL BILL         
         BNE   INVDT30             THAT WAS REVERSED, NO                        
         OC    TRNNARR+35(2),TRNNARR+35  HAS THIS BILL BEEN REVERSED            
         BZ    INVDT40             NO                                           
         CLC   DRTDATE,TRNNARR+33                                               
         BNE   INVDT40                                                          
         MVC   DRTNUM,TRNREF                                                    
         BAS   RE,PUTDR                                                         
         MVI   DRTNUM,X'CC'                                                     
         B     INVDT40                                                          
*                                                                               
INVDT30  CLI   DRTNUM,X'FF'        IS THIS A DATE REVERSED                      
         BNE   INVDT40             NO                                           
         CLC   DRTDATE,TRNNARR+33 IS THIS THE REVERSAL                          
         BNE   INVDT40             NO                                           
         MVC   DRTNUM,TRNREF                                                    
         BAS   RE,PUTDR                                                         
         MVI   DRTNUM,X'CC'                                                     
*                                                                               
INVDT40  LA    R3,DRTABLN(R3)                                                   
         BCT   R0,INVDT10                                                       
         B     EXIT                FOR NOW                                      
         DROP  R3,R5,R7                                                         
         EJECT                                                                  
***********************************************************************         
*  PUT THE DRTAB ENTRY AT 0(R3) OUT TO TAPE                           *         
***********************************************************************         
         SPACE 1                                                                
         USING PCOL2D,R5                                                        
         USING DRTABD,R3                                                        
PUTDR    NTR1                                                                   
         LA    R5,TPREC                                                         
*                                                                               
         MVC   WCDESC,SPACES                                                    
         BAS   RE,GETWCNM          GET WORK CODE NAME                           
*                                                                               
         MVC   PCOL2VID,VENID       VENDOR NUMBER                               
         MVC   PCOL2INV(L'DRTNUM),DRTNUM                                        
         MVC   PCOL2SBA,=C'101'    SAP BUSINESS AREA                            
         MVI   PCOL2DH1,C'-'       SEPARATOR                                    
         MVC   PCOL2SGA,JBUFCC     SAP_GL_ACCOUNT                               
         MVI   PCOL2DH2,C'-'       SEPARATOR                                    
         MVC   PCOL2SCC,JBUFAC     SAP_COST-CENTER                              
         MVC   PCOL2SCM,=C'US01'   SAP_COM_CODE                                 
         MVC   PCOL2DES(L'DRTWC),DRTWC        WORK CODE                         
*                                                                               
         MVI   PCOL2SN,C'+'        DEBIT ASSUMED                                
         CP    DRTAMNT,=P'0'                                                    
         BNL   *+8                                                              
         MVI   PCOL2SN,C'-'        IT IS CREDIT                                 
         MVC   PCOL2AMT,ZEROS                                                   
         ZAP   DUB,DRTAMNT                                                      
         UNPK  PCOL2AMT,DUB                                                     
         OI    PCOL2AMT+12,X'F0'                                                
*        EDIT  (P8,DRTAMNT),PCOL2AMT                                            
*                                                                               
         MVC   PCOL2INI,=C'Y&&R'             SOURCE INITIALS                    
         MVC   PCOL2DD,SPACES                                                   
         MVC   PCOL2DD(L'WCDESC),WCDESC      WORK CODE DESCRIPTION              
         MVI   PCOL2RT,C'2'                  RECORD TYPE                        
*                                                                               
         AP    INVNET,DRTAMNT                                                   
         AP    DENETTOT,DRTAMNT    INVOICE DETAIL'S NET'S TOTALS                
         AP    INVCOM,DRTCOM                                                    
         AP    INVCD,DRTCD                                                      
*                                                                               
         AP    TOTLNET,DRTAMNT     TRAILER RECORD TOTALS                        
         AP    TOTLCOM,DRTCOM                                                   
         AP    TOTLRECS,=P'1'      NO. OF HEADER + DETAIL RECS                  
         AP    TOTDETRE,=P'1'      NO. OF DETAIL RECS                           
*                                                                               
         BAS   RE,WRTP                                                          
         BAS   RE,PRTINVDE         PRINT INVOICE DETAIL INFO                    
         B     EXIT                                                             
         DROP  R3,R5                                                            
         EJECT                                                                  
***********************************************************************         
* GET WORK CODE NAME AND PUT IT IN WCDESC                             *         
*     R3 = DRTAB ENTRY                                                *         
***********************************************************************         
         SPACE 1                                                                
         USING WCORECD,R4                                                       
         USING DRTABD,R3                                                        
GETWCNM  NTR1                                                                   
         LA    R4,SVKEY                                                         
         MVC   WCOKEY,SPACES                                                    
         MVI   WCOKTYP,WCOKTYPQ    X'0A' - WORK-CODE RECORD TYPE                
         MVC   WCOKCPY,RCCOMPFL                                                 
         MVC   WCOKUNT(2),=C'SJ'                                                
         MVC   WCOKWRK,DRTWC       WORK CODE                                    
*                                                                               
         BAS   RE,DMHIGHDR                  READ HIGH                           
         CLC   SVKEY(WCOKEND),IOKEY         SAME KEY?                           
         BNE   GETWCNX                                                          
*                                                                               
         BAS   RE,DMGETREC                  GETREC                              
*                                                                               
         LA    R4,IO                                                            
*                                                                               
         LA    R2,WCORFST          POINT TO FRST ELEMENT                        
GETWCN10 CLI   0(R2),0             END OF RECORD?                               
         BE    GETWCNX                                                          
         CLI   0(R2),WCOELQ        X'12' - WORKCODE ELEMENT                     
         BE    GETWCN30                                                         
         CLI   0(R2),NAMELQ        X'20' - NAME ELEMENT                         
         BE    GETWCN40                                                         
GETWCN20 SR    R1,R1                                                            
         IC    R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     GETWCN10                                                         
*                                                                               
         USING WCOELD,R2                                                        
GETWCN30 MVC   WCDESC(L'WCODESC),WCODESC    DEFAULT TO SHORT NAME               
         B     GETWCN20                                                         
*                                                                               
         USING NAMELD,R2                                                        
GETWCN40 SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q+1)                                                 
         EX    R1,*+4                                                           
         MVC   WCDESC(0),NAMEREC   OVERWRITE WITH LONG NAME                     
*                                                                               
GETWCNX  DS    0H                                                               
         B     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* PUT TRAILER RECORD OUT TO TAPE                                      *         
***********************************************************************         
         SPACE 1                                                                
PUTTRAIL NTR1                                                                   
         USING PCOL9D,R5                                                        
         LA    R5,TPREC                                                         
         BAS   RE,CLRTP                                                         
*                                                                               
         MVC   PCOL9RID,=20C'9'               SET TO ALL 9'S  RECORD ID         
         GOTO1 DATCON,DMCB,(4,RCDATE),(20,CREADATE)                             
         MVC   PCOL9CD(4),CREADATE+4           MMDD OF CREATION DATE            
         MVC   PCOL9CD+4(4),CREADATE           MMDD OF CREATION DATE            
*                                                                               
         MVC   PCOL9CT,SVTIME                 DON'T KNOW                        
         EDIT  (P8,TOTLRECS),(10,PCOL9TRC)    RECD CNT HEADER + DETAILS         
         EDIT  (P4,INVCNT),(10,PCOL9IRC)      RECD CNT HEADER                   
*                                                                               
         MVC   PCOL9AMT,ZEROS                                                   
         ZAP   DUB,BINETTOT                                                     
         UNPK  PCOL9AMT+3(16),DUB                                               
         OI    PCOL9AMT+18,X'F0'                                                
*                                                                               
         EDIT  (P8,TOTDETRE),(10,PCOL9DRC)    RECD CNT DETAIL                   
*                                                                               
         MVC   PCOL9DAM,ZEROS                                                   
         ZAP   DUB,DENETTOT                                                     
         UNPK  PCOL9DAM+3(16),DUB                                               
         OI    PCOL9DAM+18,X'F0'                                                
*                                                                               
         MVI   PCOL9RT,C'9'                   RECORD TYPE 9                     
*                                                                               
PUTTR40  BAS   RE,WRTP                                                          
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET TIME OF DAY AND EDIT INTO AREA ADDRESSED BY R1       *         
***********************************************************************         
         SPACE 1                                                                
TIME     STM   RE,R2,12(RD)                                                     
         LR    R2,R1                                                            
         TIME  DEC                                                              
*                                                                               
         SRL   R0,8                SHIFT OUT TENTHS & HUNDREDTHS                
         SLL   R0,4                MAKE ROOM FOR SIGN                           
         XC    DUB,DUB                                                          
         STCM  R0,15,DUB+4                                                      
         OI    DUB+7,X'0F'                                                      
         AP    DUB,=P'60000'       BUMP UP HOURS FROM DDS TO ACTUAL             
         CVB   R0,DUB                                                           
         EDIT  (R0),(6,0(R2))                                                   
*                                                                               
         CLI   0(R2),C' '          IS IT SPACE                                  
         BNE   *+8                                                              
         MVI   0(R2),C'0'                                                       
TIME10   LM    RE,R2,12(RD)                                                     
         BR    RE                                                               
         SPACE 2                                                                
***********************************************************************         
* CLEAR TPREC                                                         *         
***********************************************************************         
         SPACE 1                                                                
CLRTP    NTR1                                                                   
         LA    R2,TPREC                                                         
         LH    R3,=Y(COLRECL)                                                   
         XR    R4,R4                                                            
         XR    R5,R5                                                            
         ICM   R5,B'1000',SPACES                                                
         MVCL  R2,R4                                                            
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PRINT JOB DATA, CPJ AND USER FIELDS                                 *         
***********************************************************************         
         SPACE 1                                                                
PRTJOB   NTR1                                                                   
         USING PRINTD,R4                                                        
         LA    R4,P                                                             
         MVC   PRCPJ,JBCPJ                                                      
         BAS   RE,PRTUF                                                         
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PRINT INVOICE HEADER DATA                                           *         
***********************************************************************         
         SPACE 1                                                                
PRTINVHD NTR1                                                                   
         CLI   FORCEHED,C'Y'                                                    
         BE    *+8                                                              
         BAS   RE,PRINTEM          SKIP A LINE                                  
*                                                                               
         USING PRINTD,R4                                                        
         LA    R4,P                                                             
         MVC   PRBNUM,BINUM                                                     
         GOTO1 DATCON,DMCB,(9,BIDATE),(5,PRBDATE)                               
*                                                                               
* DUE DATE FIELD NOOPED 11/94                                                   
*        GOTO1 DATCON,DMCB,(9,BIDUE),(5,PRDUE)                                  
*                                                                               
         LA    R3,PRNET                                                         
         ZAP   DOUBLE,BINET                                                     
         BAS   RE,PRTAMNT                                                       
*                                                                               
*        LA    R3,PRCOM                                                         
*        ZAP   DOUBLE,BICOM                                                     
*        BAS   RE,PRTAMNT                                                       
*                                                                               
*        CLI   RCSUBPRG,2          PRINTING CD                                  
*        BNE   PRTINH10            NO, PRINT RECEIVABLE                         
*                                                                               
*        LA    R3,PRCD                                                          
*        ZAP   DOUBLE,BICD                                                      
*        BAS   RE,PRTAMNT                                                       
*        B     PRTINH20                                                         
*                                                                               
*PRTINH10 LA    R3,PRRECV                                                       
*        ZAP   DOUBLE,BIREC                                                     
*        BAS   RE,PRTAMNT                                                       
*                                                                               
PRTINH20 CLI   JOBSTAT,PRINTED     HAVE I PRINTED THIS JOBS DATA                
         BE    *+8                 YES                                          
         BAS   RE,PRTJOB                                                        
         MVI   JOBSTAT,PRINTED                                                  
*                                                                               
         BAS   RE,PRINTEM                                                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PRINT INVOICE DETAIL DATA AT 0(R3)                                  *         
***********************************************************************         
         SPACE 1                                                                
PRTINVDE NTR1                                                                   
         USING PRINTD,R4                                                        
         USING DRTABD,R5                                                        
         LA    R4,P                                                             
         LR    R5,R3               SET R5                                       
*                                                                               
         MVC   PRWC,DRTWC                                                       
*                                                                               
         GOTO1 CHOPPER,DMCB,('LDRTNAME',DRTNAME),('LPRCAN',PRCAN),(C'P'X        
               ,4)                                                              
*                                                                               
         ZAP   DOUBLE,DRTAMNT                                                   
         LA    R3,PRNET                                                         
         BAS   RE,PRTAMNT                                                       
*                                                                               
*        ZAP   DOUBLE,DRTCOM                                                    
*        LA    R3,PRCOM                                                         
*        BAS   RE,PRTAMNT                                                       
*                                                                               
*        CLI   RCSUBPRG,2                                                       
*        BNE   PRTIND10                                                         
*        ZAP   DOUBLE,DRTCD                                                     
*        LA    R3,PRCD                                                          
*        BAS   RE,PRTAMNT                                                       
*        B     PRTIND20                                                         
*                                                                               
*PRTIND10 ZAP   DOUBLE,DRTAMNT                                                  
*         AP    DOUBLE,DRTCOM                                                   
*         SP    DOUBLE,DRTCD                                                    
*         LA    R3,PRRECV                                                       
*         BAS   RE,PRTAMNT                                                      
*                                                                               
PRTIND20 BAS   RE,PRINTEM                                                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* LOOP THRU USERTAB, PRINTING USER FIELDS                             *         
***********************************************************************         
         SPACE 1                                                                
PRTUF    NTR1                                                                   
         USING PRINTD,R4                                                        
         LA    R4,P                                                             
         LA    R4,PRUFS            OFFSET INTO P TO PRINT USER FIELDS           
*                                                                               
         USING LUD,R3                                                           
         L     R3,AUSERTAB                                                      
         LA    R0,LUNUM                                                         
         XR    R1,R1                                                            
*                                                                               
PRUF10   LA    R5,JBUFS                                                         
         MVC   0(2,R4),LUCODE                                                   
         MVI   2(R4),C'='                                                       
         SR    R1,R1                                                            
         ICM   R1,3,LUJBFLD        GET DISPLACMENT TO FIELD                     
         AR    R5,R1               POINT R5 TO CORRECT USERFIELD                
         SR    R1,R1                                                            
         IC    R1,LUJBLEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R5),SPACES      IS THIS USER FIELD DEFINED                   
         BH    PRUF20              YES, PRINTEM                                 
*                                                                               
         MVC   3(11,R4),=C'*UNDEFINED*'                                         
         B     PRUF30                                                           
*                                                                               
PRUF20   EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   3(0,R4),0(R5)                                                    
*                                                                               
PRUF30   BAS   RE,PRINTEM                                                       
         LA    R1,L'JBUFS(R1)                                                   
         LA    R3,LUDLN(R3)        AND NEXT TABLE ENTRY                         
         BCT   R0,PRUF10                                                        
         B     EXIT                                                             
         DROP  R4,R5                                                            
         EJECT                                                                  
***********************************************************************         
* WRITE TAPE                                                          *         
***********************************************************************         
         SPACE 1                                                                
WRTP     NTR1                                                                   
         CLI   QOPT1,C'Y'          TAPE REQUESTED                               
         BNE   EXIT                NO                                           
*                                                                               
         USING PCOL1D,R5                                                        
         LA    R5,TPREC                                                         
*                                                                               
         PUT   OUTP,TPREC                                                       
         AP    TPCT,=P'1'                                                       
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* COMMON ROUNTINES (PRTAMNT/PRINTEM/TPDUMP)                           *         
***********************************************************************         
         SPACE 1                                                                
PRTAMNT  ST    RE,SVRE             GENERAL EDIT ROUTINE                         
         EDIT  (P8,DOUBLE),(15,(R3)),2,COMMAS=YES,MINUS=YES                     
         L     RE,SVRE                                                          
         BR    RE                                                               
*                                                                               
PRINTEM  ST    RE,SVRE                                                          
         GOTO1 ACREPORT            PRINT SUBROUTINE                             
         L     RE,SVRE                                                          
         BR    RE                  RETURN TO USER                               
         SPACE 3                                                                
TPDUMP   ST    RE,SVRE                                                          
         LA    R2,256                                                           
         GOTO1 PRNTBL,DMCB,(5,=C'TPREC'),TPREC,C'DUMP',(R2),=C'1D'              
         L     RE,SVRE                                                          
         BR    RE                  RETURN TO USER                               
         EJECT                                                                  
**********************************************************************          
* DATAMGR INTERFACE                                                  *          
**********************************************************************          
         SPACE 1                                                                
DMSEQDR  NTR1  0,SEQ                READ SEQUENTIAL                             
         GOTO1 DATAMGR,DMCB,DMRSEQ,=C'ACCDIR ',SVKEY,IO,0                       
         B     DMX                                                              
*                                                                               
DMHIGHDR NTR1  0,HIGH               READ HIGH                                   
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'ACCDIR ',SVKEY,IO,0                       
         B     DMX                                                              
*                                                                               
DMREADDR NTR1  0,READ               READ                                        
         GOTO1 DATAMGR,DMCB,DMREAD,=C'ACCDIR ',SVKEY,IO,0                       
         B     DMX                                                              
*                                                                               
DMGETREC NTR1  0,GREC               GET RECORD                                  
         USING ACCRECD,R3                                                       
         LA    R3,IO                                                            
         GOTO1 DATAMGR,DMCB,DMGET,=C'ACCMST ',ACCKDA,IO,DMWORK                  
*                                                                               
DMX      B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* GETEL                                                               *         
***********************************************************************         
         SPACE 1                                                                
         GETEL R7,DATADISP,ELCODE  GET ELEMENT                                  
         EJECT                                                                  
***********************************************************************         
* CONSTANTS                                                           *         
***********************************************************************         
         SPACE 1                                                                
DSPARM   DC    CL20'ACCTAPE.AC0IGXX1'                                           
DDPARM   DC    CL8'OUTP'                                                        
OUTP     DCB   DDNAME=OUTP,DSORG=PS,LRECL=COLRECL,MACRF=PM,            X        
               BLKSIZE=COLRECL*COLBLCK                                          
*                                                                               
ZEROS    DC    CL80'0'                                                          
TEX810   DC    CL13'*****TEX01810' HEADER FOR INVOICE HEADER                    
*                                                                               
*                                                                               
         DS    0D                  DOUBLEWORD ALIGNMENT FOR TPVOLS              
TPVOLS   DC    PL8'1'              GENERATE TAPE(+1) FOR FIRST TAPE             
SQUASHER DC    V(SQUASHER)                                                      
PRNTBL   DC    V(PRNTBL)                                                        
UNDERLIN DC    V(UNDERLIN)                                                      
DRTAB    DC    A(DRTABC)                                                        
XTAB     DC    A(XTABC)            TABLE OF UNMARKED CHARGES                    
BINADD   DC    A(BINADDC)                                                       
WCTAB    DC    A(WCTABC)                                                        
TOTTAB   DC    A(TOTTABC)                                                       
AUSERTAB DC    A(USERTAB)                                                       
         EJECT                                                                  
**********************************************************************          
* TABLES                                                             *          
**********************************************************************          
         SPACE 1                                                                
* USER FIELD TABLE - TABLE DEFINES WHERE IN TPREC TO PUT A USER FIELD           
*                    COVERED BY LUD                                             
USERTAB  DS    0C                                                               
         DC    C'AC',AL2(JBUFAC-JBUFS),AL1(L'JBUFAC)                            
*        DC    C'AN',AL2(JBUFAN-JBUFS),AL1(L'JBUFAN)                            
         DC    C'CC',AL2(JBUFCC-JBUFS),AL1(L'JBUFCC)                            
*        DC    C'CE',AL2(JBUFCE-JBUFS),AL1(L'JBUFCE)                            
*        DC    C'CI',AL2(JBUFCI-JBUFS),AL1(L'JBUFCI)                            
*        DC    C'CN',AL2(JBUFCN-JBUFS),AL1(L'JBUFCN)                            
*SERCHG  DC    C'JI',AL2(JBUFJI-JBUFS),AL1(L'JBUFJI)                            
*        DC    C'PA',AL2(JBUFPA-JBUFS),AL1(L'JBUFPA)                            
*        DC    C'PI',AL2(JBUFPI-JBUFS),AL1(L'JBUFPI)                            
*        DC    C'TY',AL2(JBUFTY-JBUFS),AL1(L'JBUFTY)                            
LUNUM    EQU   (*-USERTAB)/LUDLN                                                
*                                                                               
* VENDOR NUMBER TABLE - VENDOR NUMBER MATCHED BY ORIGIN NUMBER                  
VENTAB   DS    0CL9                                                             
         DC    HL2'1141',C'1252071'  YNRA  - Y&R SHARED FINANCIAL SVCS          
VENTBLNQ EQU   (*-VENTAB)/L'VENTAB                                              
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* TABLE TO GENERATE SUB TOTALS                                        *         
***********************************************************************         
         SPACE 1                                                                
TOTTABC  DS    0D                                                               
*                                                                               
         DC    XL1'FF'             END OF TOTAL TABLE                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    PL8'0'                                                           
         DC    PL8'0'                                                           
         DC    PL8'0'                                                           
TOTNUM   EQU   (*-TOTTABC)/TOTLEN  NUMBER OF LEVELS IN TABLE                    
         EJECT                                                                  
***********************************************************************         
* TABLE OF WORKCODES AND RATES                                        *         
***********************************************************************         
         SPACE 1                                                                
WCTABC   DS    0D                                                               
         DS    H                  NUMBER OF BILSLS SAVED                        
         DC    Y(WCMAX)         MAXIMUM NUMBER                                  
         DS    (WCMAX*WCTABLN)C TABLE AREA                                      
WCMAX    EQU   420                                                              
         EJECT                                                                  
***********************************************************************         
* TABLE OF BILLED DEBITS ON A JOB                                     *         
*       KEY AREA IS COVERED BY BIND                                   *         
*       DATA AREA IS COVERED BY DRTABD                                *         
***********************************************************************         
         SPACE 1                                                                
DRTABC   DS    0D                                                               
         DC    F'0'                NUMBER OF TABLE ENTRIES                      
         DC    AL4(DRTABLN)        RECORD LENGTH                                
         DC    AL4(DRTKYLN)        DISP OF KEY/ KEY LENGTH                      
         DC    A(DRTABMAX)         MAXIMUM NUMBER OF ENTRIES                    
         DC    AL1(3)              NUMBER OF BUCKETS                            
         DC    AL1(DRTBUCK-DRTABD) DISP TO FIRST BUCK                           
         DC    AL1(0)              SPARE                                        
         DS    (DRTABMAX*DRTABLN)C                                              
DRTABMAX EQU   1000                                                             
         EJECT                                                                  
***********************************************************************         
* TABLE OF UNBILLED DEBITS ON A JOB                                   *         
*       KEY AREA IS COVERED BY BIND                                   *         
*       DATA AREA IS COVERED BY DRTABD                                *         
***********************************************************************         
         SPACE 1                                                                
XTABC    DS    0D                                                               
         DC    F'0'                NUMBER OF TABLE ENTRIES                      
         DC    AL4(DRTABLN)        RECORD LENGTH                                
         DC    AL4(DRTKYLN)        DISP OF KEY/ KEY LENGTH                      
         DC    A(DRTABMAX)         MAXIMUM NUMBER OF ENTRIES                    
         DC    AL1(2)              NUMBER OF BUCKETS                            
         DC    AL1(DRTBUCK-DRTABD) DISP TO FIRST BUCK                           
         DC    AL1(0)              SPARE                                        
         DS    (DRTABMAX*DRTABLN)C                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD TO A BINSRCH TABLE                                   *         
*         PARAM1              A(RECORD TO BE ADDED)                   *         
*         PARAM2              A(BINSRCH PARAMS)                       *         
***********************************************************************         
         SPACE 1                                                                
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
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ADDRESS OF RC                                                       *         
***********************************************************************         
         SPACE 1                                                                
ADRC     DS    F                   SAVE ADDRESS OF REGISTER 12                  
         EJECT                                                                  
***********************************************************************         
* HEADHOOK                                                            *         
***********************************************************************         
         SPACE 1                                                                
HDHOOK   DS    0D                                                               
         NMOD1 0,*HDHOOK*                                                       
         L     R1,=A(ADRC)         RESTORE REGISTER 12                          
         ICM   RC,15,0(R1)                                                      
         L     R5,ADBOX                                                         
         USING BOXD,R5                                                          
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+6,C'T'                                                   
         MVI   BOXROWS+9,C'M'                                                   
         MVI   BOXROWS+56,C'B'                                                  
         USING PRINTD,R6                                                        
         LA    R6,BOXCOLS                                                       
         CLI   RCSUBPRG,0                                                       
         BE    HDH10                                                            
         CLI   RCSUBPRG,2          PRINT CD OPTION                              
         BNE   HDH50                                                            
HDH10    MVI   PRLEFT,C'L'                                                      
         MVI   PRCOL1,C'C'                                                      
         MVI   PRCOL2,C'C'                                                      
         MVI   PRCOL3,C'C'                                                      
         MVI   PRCOL4,C'C'                                                      
         MVI   PRCOL6,C'C'                                                      
*        MVI   PRCOL7,C'C'                                                      
*                                                                               
*        MVI   PRCOL8,C'R'                                                      
*        CLI   RCSUBPRG,2          PRINTING CD COL?                             
*        BNE   HDH100              NO, ALL DONE                                 
*        MVI   PRCOL8,C'C'                                                      
         MVI   PRRIGHT,C'R'                                                     
         B     HDH100                                                           
*                                                                               
HDH50    MVI   PRLEFT,C'L'         SUMMARY BOXES                                
         MVI   PRCOL6,C'R'                                                      
*                                                                               
HDH100   MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* STORAGE DSECT                                                       *         
***********************************************************************         
         SPACE 1                                                                
ACIGD    DSECT                                                                  
SVRE     DS    F                                                                
ADBOX    DS    A                                                                
OUTCNT   DS    H                                                                
*                                                                               
ELCODE   DS    CL1                                                              
JOBSTAT  DS    CL1                                                              
PRINTED  EQU   C'P'                                                             
*                                                                               
FULL1    DS    F                                                                
VENID    DS    CL(L'PCOL1VID)      VENDOR ID NUMBER                             
SVTIME   DS    CL6                                                              
CREADATE DS    CL8                 YYYYMMDD CREATION DATE                       
TPCT     DS    PL4                                                              
INVCNT   DS    PL4                                                              
GROSS    DS    PL8                                                              
*                                                                               
INVNET   DS    PL6                 VENDOR INVOICE AMOUNTS                       
INVCOM   DS    PL6                                                              
INVCD    DS    PL6                                                              
*                                                                               
********************************************************************            
* COLGATE SPECIFIED HARD CODED DATA STORAGE                        *            
********************************************************************            
COLRECL  EQU   256                 RECORD LENGTH                                
COLBLCK  EQU   256                 BLOCK SIZE                                   
*                                                                               
*        NOTE- STORAGE BELOW IS USED TO PASS DATA                               
*        TO SUB-ROUTINES                                                        
*                                                                               
*--------------------------------------------------------------------           
*        JOB LEVEL DATA                                                         
*--------------------------------------------------------------------           
JBDATA   DS    0C                                                               
JBCPJ    DS    CL12                                                             
JBPRO    DS    CL3                                                              
JBNAME   DS    CL36                                                             
JBUFS    DS    0CL40                                                            
JBUFAC   DS    CL40                                                             
JBUFAN   DS    CL40                                                             
JBUFCC   DS    CL40                                                             
JBUFCE   DS    CL40                                                             
JBUFCI   DS    CL40                                                             
JBUFCN   DS    CL40                                                             
JBUFJI   DS    CL40                                                             
JBUFPA   DS    CL40                                                             
JBUFPI   DS    CL40                                                             
JBUFTY   DS    CL40                                                             
JBLEN    EQU   *-JBCPJ                                                          
*                                                                               
SAVECANM DS    CL36                CONTRA ACCOUNT NAME                          
*                                                                               
BIDATA   DS    0CL(BILEN)                                                       
BIDUE    DS    CL8                                                              
BINUM    DS    CL6                                                              
BIDATE   DS    CL8                                                              
BIDRCR   DS    CL2                 NOT USED 12/93                               
BINET    DS    PL6                                                              
BICOM    DS    PL6                                                              
BICD     DS    PL6                                                              
BIREC    DS    PL6                                                              
BILEN    EQU   *-BIDUE                                                          
BINETTOT DS    PL8                 INVOICE HEADER'S NET'S TOTALS                
DENETTOT DS    PL8                 INVOICE DETAIL'S NET'S TOTALS                
*                                                                               
TPREC    DS    CL(COLRECL)                                                      
DRTABREC DS    CL(DRTABLN)         PUTWCDET INTERFACE                           
LASTREC  DS    CL(12)                                                           
*                                                                               
GOTTOTAL DS    CL1                                                              
GOTABILL DS    CL1                 Y, THIS JOB HAS A BILL                       
TESTBILL DS    CL1                                                              
REVFLAG  DS    CL1                 YES, BILL IS A REVERSAL                      
PROGRESS EQU   X'01'                                                            
TOTAL    EQU   X'02'                                                            
ONELINE  EQU   X'04'                                                            
PCTEST   EQU   X'08'                                                            
ALLOC    EQU   X'10'                                                            
SPECIAL  EQU   X'20'                                                            
CLIENT   EQU   X'40'                                                            
MARKED   EQU   X'80'                                                            
*                                                                               
SAVETYPE DS    CL1                                                              
*                                                                               
IOSW     DS    CL1                                                              
SRTOPEN  EQU   1                                                                
TPOPEN   EQU   2                                                                
*                                                                               
SVDT2    DS    XL2                                                              
SVDT3    DS    XL3                                                              
SVYYMMDD DS    0C                                                               
SVYY     DS    CL2                                                              
SVMMDD   DS    CL4                                                              
SVYYMM   DS    CL2                                                              
INVNUM   DS    CL6                                                              
SVDUE    DS    CL6                 SAVE BILLING DUEDATE                         
PL16     DS    PL16                                                             
TOTLNET  DS    PL8                                                              
TOTLCOM  DS    PL8                                                              
TOTLRECV DS    PL8                                                              
TOTLRECS DS    PL8                 NO. OF INV(HEAD)+DETAIL RECD                 
TOTDETRE DS    PL8                 NO. OF DETAIL RECORDS                        
TODAY    DS    CL6                                                              
QSTR3    DS    CL3                 YYMMDD DATES                                 
QEND3    DS    CL3                                                              
QSTR2    DS    CL2                 PACKED DATES                                 
QEND2    DS    CL2                                                              
WCDESC   DS    CL40                WORK CODE NAME                               
*                                                                               
SVKEY    DS    CL49                SAVED AREA FOR KEY                           
*                                                                               
IO       DS    0CL2042                                                          
IOKEY    DS    CL42                KEY                                          
IODATA   DS    CL2000              DATA                                         
IOLNQ    EQU   *-IO                LENGTH                                       
         EJECT                                                                  
***********************************************************************         
* TABLE OF BILLED DEBITS ON A JOB                                     *         
***********************************************************************         
         SPACE 1                                                                
DRTABD   DSECT                     TABLE OF DR AMOUNTS                          
DRTNUM   DS    CL6                 BILL NUMBER                                  
DRTWC    DS    CL2                 WORK CODE                                    
DRTDATE  DS    CL2                 PACKED BILL DATE                             
DRTNAME  DS    CL36                VENDOR NAME                                  
LDRTNAME EQU   L'DRTNAME                                                        
DRTSUB   DS    CL1                 SUB REF NO, TO KEEP UNIQUE                   
DRTKYLN  EQU   *-DRTABD                                                         
DRTSTAT  DS    CL1                 COMMISSIONABLE OR NOT                        
DRTNARR  DS    CL79                NARRITIVE                                    
DRTBUCK  EQU   *                                                                
DRTAMNT  DS    PL8                 BILLED                                       
DRTCOM   DS    PL8                 COMMISSION                                   
DRTCD    DS    PL8                 CASH DISC                                    
DRTABLN  EQU   *-DRTABD                                                         
         EJECT                                                                  
***********************************************************************         
* BINSEARCH DSECT                                                     *         
***********************************************************************         
         SPACE 1                                                                
BIND     DSECT                                                                  
BININ    DS    F                   NUMBER OF ENTRIES IN TABLE                   
BINLEN   DS    F                   RECORD LENGTH                                
BINDISP  DS    CL1                 DISPLACEMENT OF KEY                          
BINKEY   DS    CL3                 KEY LENGTH                                   
BINMAX   DS    F                   MAXIMUM NUMBER OF ENTRIES                    
BINNUMB  DS    CL1                 NUMBER OF BUCKETS                            
BINFRST  DS    CL1                 DISPLACEMENT OF 1ST BUCKET                   
         DS    CL1                 SPARE                                        
BINTABLE DS    0CL1                                                             
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER HEADER INFO IN TABLES                                *         
***********************************************************************         
         SPACE 1                                                                
TABLED   DSECT                                                                  
TABNUM   DS    H                                                                
TABMAX   DS    H                                                                
TABDATA  DS    0C                                                               
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER WORK CODE RATE TABLE                                 *         
***********************************************************************         
         SPACE 1                                                                
WCTABD   DSECT                                                                  
WCTBCODE DS    CL2                                                              
WCTBRATE DS    PL6                                                              
WCTABLN  EQU   *-WCTABD                                                         
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER USER FIELD TABLE                                     *         
***********************************************************************         
         SPACE 1                                                                
LUD      DSECT                                                                  
LUCODE   DS    CL2                                                              
LUJBFLD  DS    AL2                                                              
LUJBLEN  DS    AL1                                                              
LUDLN    EQU   *-LUD                                                            
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER PRINT LINE                                           *         
***********************************************************************         
         SPACE 1                                                                
PRINTD   DSECT                                                                  
PRLEFT   DS    CL1                                                              
PRCPJ    DS    0CL12                                                            
PRCAN    DS    CL20                                                             
LPRCAN   EQU   L'PRCAN                                                          
PRCOL1   DS    CL1                                                              
PRUFS    DS    CL25                                                             
PRCOL2   DS    CL1                                                              
PRBNUM   DS    CL6                                                              
PRCOL3   DS    CL1                                                              
PRBDATE  DS    CL8                                                              
PRCOL4   DS    CL1                                                              
PRWC     DS    CL2                                                              
         DS    CL2                                                              
PRCOL6   DS    CL1                                                              
PRNET    DS    CL15                NET AMOUNT                                   
PRCOL7   DS    CL1                                                              
PRRIGHT  DS    CL1                                                              
*                                                                               
         EJECT                                                                  
TOTTABD  DSECT                     TO COVER TOTTAB                              
TOTLEV   DS    CL1                                                              
TOTCLC   DS    AL1                                                              
TOTOFF   DS    AL1                 OFFSET TO PRINT "TOTAL"                      
TOTNET   DS    PL8                                                              
TOTCOM   DS    PL8                                                              
TOTCD    DS    PL8                                                              
TOTLEN   EQU   *-TOTTABD                                                        
         EJECT                                                                  
***********************************************************************         
* ++INCLUDES                                                          *         
***********************************************************************         
         SPACE 1                                                                
       ++INCLUDE PPCOLINTFD                                                     
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
**PAN#1  DC    CL21'120ACREPIG02S05/01/02'                                      
         END                                                                    
