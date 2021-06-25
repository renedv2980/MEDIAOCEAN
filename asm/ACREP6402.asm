*          DATA SET ACREP6402  AT LEVEL 016 AS OF 04/23/15                      
*PHASE AC6402A                                                                  
*INCLUDE ACCDIV                                                                 
*                                                                               
*              CONTROL PROFILES                                                 
*                                                                               
*        1.  GIVE RJR-TYPE CONTROL BREAKS                                       
*        2.  SUPPRESS JOBS WITHOUT ACTIVITY                                     
         TITLE 'CLIENT EXPENDITURE ANALYSIS'                                    
AC6402   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**AC64**,R9,RR=R3                                              
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING AC64D,RC                                                         
         ST    R3,PRELOC                                                        
         EJECT                                                                  
         CLI   MODE,RUNFRST                                                     
         BNE   CE1                                                              
*                                                                               
         L     R2,=A(BOXRC)                                                     
         ST    RC,0(R2)                                                         
         L     R3,=A(HDHOOK)                                                    
         ST    R3,HEADHOOK                                                      
         USING ACMD,R3                                                          
         L     R3,AMONACC                                                       
         GOTO1 ACMAJOBL,DMCB,FLDH,ACMACOLL,ADCOMFAC                             
         CLI   DMCB+4,X'00'                                                     
         BNE   CEXIT                                                            
         DC    H'0'                                                             
         DROP  R3                                                               
         EJECT                                                                  
CE1      CLI   MODE,REQFRST                                                     
         BNE   CE2                                                              
*                                                                               
         L     R2,VEXTRAS                                                       
         USING RUNXTRAD,R2                                                      
         L     R2,ADMASTD                                                       
         USING MASTD,R2                                                         
         L     R2,MCBXAREA                                                      
         ST    R2,ADBOX                                                         
         USING BOXD,R2                                                          
         L     R3,=A(BXHOOK)                                                    
         ST    R3,BOXHOOK                                                       
*                                                                               
         MVC   GOADM,DATAMGR                                                    
         MVC   GOABUFF,AGOBUFF                                                  
         MVC   GOLBUFF,=F'10000'                                                
         MVC   GOSELCUL,QCOMPANY                                                
         MVC   GOACOMP,ADCOMP                                                   
*                                                                               
         MVI   OP4MSW,C'N'                                                      
         MVC   FRST(4),SPACES                                                   
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
*                                                                               
         LA    R2,SAVEP                                                         
         LA    R3,L'SAVEP                                                       
         LA    R5,X'40'                                                         
         SLL   R5,24               SHIFT THE SPACE, ZERO THE REST               
         MVCL  R2,R4                                                            
*                                                                               
         GOTO1 PROLLER,DMCB,0,TABLE,5,7                                         
         L     RF,=A(BUFFALOC)                                                  
         ST    RF,ADBUFC                                                        
         GOTO1 BUFFALO,DMCB,=C'SET',ADBUFC                                      
         MVI   INTEXTSW,C'N'                                                    
         L     RF,ADCMPEL                                                       
         USING ACCOMPD,RF                                                       
         TM    ACMPSTA2,X'20'                                                   
         BZ    *+8                                                              
         MVI   INTEXTSW,C'Y'       COMPANY STATUS TO SHOW BOTH ESTS             
         MVI   RCSUBPRG,0                                                       
         CLI   QOPT4,C'Y'                                                       
         BNE   CEXIT                                                            
         MVI   RCSUBPRG,1                                                       
         B     CEXIT                                                            
*                                                                               
*                                                                               
CEXIT    EQU   *                                                                
*                                                                               
GENXT    EQU   *                                                                
EXIT     EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
CE2      CLI   MODE,LEDGFRST                                                    
         BNE   CE8                                                              
         GOTO1 DATCON,DMCB,(5,0),(1,TODAY)                                      
         XC    START,START                                                      
         XC    START2,START2                                                    
         MVC   END,=X'FFFFFF'                                                   
         MVC   END2,=X'FFFF'                                                    
         CLC   QSTART,SPACES                                                    
         BE    CE4                                                              
         GOTO1 DATCON,DMCB,(0,QSTART),(1,START)                                 
         GOTO1 DATCON,DMCB,(0,QSTART),(2,START2)                                
CE4      CLC   QEND,SPACES                                                      
         BE    CE6                                                              
         GOTO1 DATCON,DMCB,(0,QEND),(1,END)                                     
         GOTO1 DATCON,DMCB,(0,QEND),(2,END2)                                    
         MVC   TODAY,END           IF USER ENTERS AN END DATE, USE IT           
*                                  AS CURRENT MONTH                             
CE6      DS    0H                                                               
         GOTO1 DATCON,DMCB,(1,TODAY),(6,CURRMTHP)                               
*                                                                               
         L     R2,ADLDGHIR                                                      
         USING ACHEIRD,R2                                                       
         ZIC   RF,ACHRLEVB                                                      
         AH    RF,=H'3'                                                         
         STC   RF,MEDPOS                                                        
         B     CEXIT                                                            
*                                                                               
CE8      CLI   MODE,LEVAFRST                                                    
         BNE   CE9                                                              
         MVI   FORCEHED,C'Y'                                                    
         B     CEXIT                                                            
*                                                                               
CE9      CLI   MODE,LEVBFRST                                                    
         BNE   CE10                                                             
*                                                                               
         MVI   PRTSW,C'N'                                                       
         CLI   QOPT4,C'Y'                                                       
         BNE   CEXIT                                                            
         L     R4,ADLVBNAM                                                      
         LA    R3,P+1                                                           
         BAS   RE,NAMOUT                                                        
         MVC   SAVEN,SPACES                                                     
         EXMVC RF,SAVEN,0(R3)                                                   
         MVI   QOPT4SW,C'Y'                                                     
         B     CEXIT                                                            
         EJECT                                                                  
CE10     CLI   MODE,PROCACC                                                     
         BNE   CE30                                                             
*                                                                               
         ZAP   CURREST,=P'0'                                                    
         ZAP   UNBILLED,=P'0'                                                   
         MVI   BUFTYPE,BILLING                                                  
         GOTO1 BUFFALO,DMCB,=C'CLEAR',(BUFTYPE,ADBUFC),1,(X'80',1)              
*                                                                               
         MVC   GOALEDG,ADLEDGER                                                 
         MVC   GOACLI,ADHEIRA                                                   
         L     R4,ADHEIRA                                                       
         MVC   GOSELCLI,SPACES     FILL WITH SPACES                             
         MVC   GOSELCLI(3),3(R4)                                                
         MVC   GOAPRO,ADHEIRB                                                   
         L     R4,ADHEIRB                                                       
         MVC   GOSELPRO,SPACES     FILL WITH SPACES                             
         MVC   GOSELPRO(3),6(R4)                                                
         MVC   GOAJOB,ADACC                                                     
         MVC   GOAKEY,ADACC                                                     
         L     R4,ADACC                                                         
         MVC   GOSELJOB,SPACES     FILL WITH SPACES                             
         MVC   GOSELJOB(6),9(R4)                                                
         MVI   GOWHICH,0                                                        
         MVI   GOANYMED,0                                                       
         MVI   GOSELLEV,0                                                       
         MVI   FCRDTRNS,C'N'                                                    
         MVI   FCRDEST,C'N'                                                     
         MVI   CLOSED,C'N'                                                      
         MVC   PLINE,=7PL6'0'                                                   
         XC    BILLCNT,BILLCNT                                                  
         L     R4,ADACCSTA                                                      
         USING ACSTATD,R4                                                       
         CLI   QOPT1,C' '          INCLUDE LOCKED AND UNLOCKED ?                
         BE    CE14                YES                                          
         CLI   QOPT1,C'S'          NO, SUPPRESS LOCKED ACCOUNTS ?               
         BNE   CE12                NO, LOCKED ACCOUNTS ONLY                     
*                                                                               
* SUPPRESS LOCKED ACCOUNTS                                                      
         TM    ACSTSTAT,X'20'      YES, IS THIS ACCOUNT LOCKED ?                
         BO    CEXIT               YES, EXIT                                    
         B     CE14                NO, SEE ABOUT CLOSED                         
*                                                                               
* LOCKED ACCOUNTS                                                               
CE12     TM    ACSTSTAT,X'20'      IS THIS ACCOUNT LOCKED ?                     
         BZ    CEXIT               NO, EXIT                                     
*                                                                               
CE14     TM    ACSTSTAT,X'40'                                                   
         BZ    *+8                                                              
         MVI   CLOSED,C'Y'         SET FLAG IF CLOSED                           
         CLI   QOPT2,C' '          INCLUDE CLOSED AND OPEN JOBS ?               
         BE    CE18                YES                                          
         CLI   QOPT2,C'S'          NO, SUPPRESS CLOSED JOBS ?                   
         BNE   CE16                NO, CLOSED ACCOUNTS ONLY                     
*                                                                               
* SUPPRESS CLOSED JOBS                                                          
         CLI   CLOSED,C'Y'         IS THIS JOB CLOSED ?                         
         BE    CEXIT               YES, EXIT                                    
         B     CE18                NO                                           
*                                                                               
* CLOSED JOBS ONLY                                                              
CE16     CLI   CLOSED,C'Y'         IS THIS JOB CLOSED ?                         
         BNE   CEXIT               NO, EXIT                                     
*                                                                               
CE18     MVI   FCRDTRNS,C'Y'       INDICATE TRANSACTIONS WANTED                 
         GOTO1 GETOPT,DMCB,GOBLOCK                                              
         BAS   RE,LOOKUP                                                        
         BAS   RE,ESTFLT                                                        
*                                                                               
         USING JBLOCKD,R5                                                       
         CLI   JBNEWEST,JBMCSQ     IS THIS A BO JOB?                            
         BE    CE24                YES                                          
         USING JBCOLD,R3                                                        
         LH    R1,JBNROWS                                                       
CE19     CLI   JBCOLTYP,JBCOLTJB   JOB TOTALS?                                  
         BNE   CE22                                                             
*                                                                               
         ZAP   CURREST,JBCOLVAL+6(6)                                            
         ZAP   ORIGEST,JBCOLVAL                                                 
         CLI   QOPT3,C'G'          USE GROSS AMOUNT ?                           
         BNE   CE20                NO, USE NET                                  
*                                                                               
         ZAP   CURREST,JBCOLVAL+18(6)  YES, REPLACE WITH GROSS                  
         ZAP   ORIGEST,JBCOLVAL+12(6)                                           
*                                                                               
CE20     ZAP   UNBILLED,CURREST    UNBILLED EST = EST - BILLING                 
         B     CEXIT                                                            
*                                                                               
CE22     AH    R3,JBLCOL                                                        
         BCT   R1,CE19                                                          
         B     CEXIT                                                            
         DROP R3                                                                
*                                                                               
         USING MJETABD,R3                                                       
CE24     ZAP   CURREST,MJETVAL+6(6)                                             
         ZAP   ORIGEST,MJETVAL                                                  
         CLI   QOPT3,C'G'          USE GROSS AMOUNT ?                           
         BNE   CE26                NO, USE NET                                  
         ZAP   CURREST,MJETVAL+18(6)  YES, REPLACE WITH GROSS                   
         ZAP   ORIGEST,MJETVAL+12(6)                                            
*                                                                               
CE26     ZAP   UNBILLED,CURREST    UNBILLED EST = EST - BILLING                 
         B     CEXIT                                                            
*                                                                               
         DROP  R3,R5                                                            
         EJECT                                                                  
CE30     CLI   MODE,PROCTRNS                                                    
         BNE   CE50                                                             
         L     R4,ADTRANS                                                       
         USING TRANSD,R4                                                        
         CLI   TRNSEL,X'44'                                                     
         BNE   CEXIT                                                            
         CLI   QOPT5,C' '          OPTION FOR W-CODE/BILLING SUMMARY            
         BE    CE32                                                             
         CLC   TRNSANAL,=C'99'                                                  
         BE    CE32                                                             
*                                                                               
         LR    R5,R4                                                            
         SH    R5,DATADISP                                                      
         USING ACKEYD,R5                                                        
         OC    ACDTUSED,ACDTUSED   BILLED TRANSACTIONS ONLY                     
         BZ    CEXIT                                                            
         CLC   ACDTUSED,START2                                                  
         BL    CEXIT                                                            
         CLC   ACDTUSED,END2                                                    
         BH    CEXIT                                                            
         MVC   GOSELWC,TRNSANAL                                                 
         L     R2,AMONACC                                                       
         USING ACMD,R2                                                          
         MVC   GOAKEY,ACMALTN       GET ADDRESS LAST RECORD READ                
         GOTO1 GETOPT,DMCB,GOBLOCK                                              
         BAS   RE,ADDIT                                                         
         XC    GOSELWC,GOSELWC                                                  
         B     CEXIT                                                            
*                                                                               
CE32     CLC   TRNSANAL,=C'99'     ONLY WANT BILLS                              
         BNE   CEXIT                                                            
         CLC   TRNSDATE,START      WHICH ARE WITHIN DATE RANGE                  
         BL    CEXIT                                                            
         CLC   TRNSDATE,END                                                     
         BH    CEXIT                                                            
*                                                                               
         ZAP   BILLAMNT,TRNSAMNT    SET BILL AMOUNT                             
         CLI   QOPT3,C'G'                                                       
         BNE   *+10                                                             
         ZAP   BILLAMNT,TRNSNARR+27(6)                                          
*                                                                               
         AP    TODATE,BILLAMNT                                                  
         CLI   CLOSED,C'Y'                                                      
         BNE   *+10                                                             
         AP    FINAL,BILLAMNT                                                   
*                                                                               
         SP    UNBILLED,BILLAMNT                                                
*                                                                               
         CLC   TRNSDATE(2),TODAY   CURRENT MONTH BILLING?                       
         BNE   *+10                                                             
         AP    CURMNTH,BILLAMNT    SAVE CURRENT MONTH BILLS                     
         BAS   RE,PUTBILL          PUT INVOICE DETAILS TO BUFFALO               
         B     CEXIT                                                            
         EJECT                                                                  
CE50     CLI   MODE,ACCLAST                                                     
         BNE   CE80                                                             
         L     R2,ADACC                                                         
         CLI   PROGPROF+1,C'Y'                                                  
         BNE   CE50A                                                            
         CP    CURREST,=P'0'                                                    
         BNE   CE50A                                                            
         CP    UNBILLED,=P'0'                                                   
         BNE   CE50A                                                            
         B     CEXIT                                                            
*                                                                               
CE50A    CLI   FRST,C' '           IS THIS THE FIRST JOB FOR THE PROD           
         BNE   CE52                NO                                           
*                                                                               
         BAS   RE,MEDPRT                                                        
         MVI   MEDSW,C'N'                                                       
         MVI   FRST,C'N'                                                        
         MVC   CODE,11(R2)         PICK UP EXECUTION(HARD)                      
         LR    RE,R2                                                            
         ZIC   R1,MEDPOS                                                        
         AR    RE,R1                                                            
         MVC   MEDIA,0(RE)                                                      
*                                                                               
CE52     DS    0H                                                               
         LR    RE,R2                                                            
         ZIC   R1,MEDPOS                                                        
         AR    RE,R1                                                            
         CLI   QOPT4,C'Y'                                                       
         BNE   CE53                                                             
         CLC   MEDIA,0(RE)         BREAKS FOR SUMMARY OPTION                    
         BE    CE62                                                             
*                                                                               
         GOTO1 PROLLER,DMCB,2,TABLE,1                                           
         LA    R3,3                                                             
         BAS   RE,FORMAT                                                        
         BAS   RE,GETMED           GET THE NAME OF "MEDIA"                      
         MVC   P+1(12),SAVEMED                                                  
         MVI   SPACING,2                                                        
         BAS   RE,PRINTEM                                                       
*                                                                               
         LR    RE,R2                                                            
         ZIC   R1,MEDPOS                                                        
         AR    RE,R1                                                            
         MVC   MEDIA,0(RE)                                                      
         MVC   CODE,11(R2)                                                      
         BAS   RE,MEDPRT                                                        
         B     CE62                                                             
*                                                                               
CE53     DS    0H                                                               
         CLC   MEDIA,0(RE)         MEDIA BREAK                                  
         BE    CE54                                                             
         MVC   P+26(9),=C'SUB-TOTAL'                                            
         LA    R3,2                                                             
         BAS   RE,FORMAT                                                        
         CLI   PROGPROF,C'Y'                                                    
         BE    *+14                                                             
         MVC   P,SPACES                                                         
         B     *+12                                                             
         MVI   SPACING,2                                                        
         BAS   RE,PRINTEM                                                       
         MVC   P+18(11),=C'MEDIA TOTAL'                                         
         LA    R3,3                                                             
         BAS   RE,FORMAT                                                        
         MVI   SPACING,2                                                        
         BAS   RE,PRINTEM                                                       
         LR    RE,R2                                                            
         ZIC   R1,MEDPOS                                                        
         AR    RE,R1                                                            
         MVC   MEDIA,0(RE)                                                      
         MVC   CODE,11(R2)                                                      
         MVI   FORCEHED,C'Y'                                                    
         MVI   MEDSW,C'Y'                                                       
         B     CE60                                                             
*                                                                               
CE54     DS    0H                  CODE BREAK                                   
         CLC   CODE,11(R2)                                                      
         BE    CE60                                                             
         MVC   P+26(9),=C'SUB-TOTAL'                                            
         LA    R3,2                                                             
         BAS   RE,FORMAT                                                        
         CLI   PROGPROF,C'Y'                                                    
         BE    *+14                                                             
         MVC   P,SPACES                                                         
         B     *+12                                                             
         MVI   SPACING,2                                                        
         BAS   RE,PRINTEM                                                       
         MVC   CODE,11(R2)                                                      
*                                                                               
CE60     DS    0H                  SET UP PRINT/PROLLER LINE FOR JOB            
         BAS   RE,JOBNAME          PRINT JOB NAME, ST= AND EA=                  
*                                                                               
CE62     DS    0H                  PRINT A JOB TOTAL                            
         CLI   CLOSED,C'Y'                                                      
         BNE   *+10                                                             
         ZAP   UNBILLED,=P'0'                                                   
*                                                                               
         LA    R4,1                                                             
         LA    R5,6                                                             
         LA    R6,PLINE                                                         
CE64     DS    0H                                                               
         GOTO1 PROLLER,DMCB,3,TABLE,0(R6),1,(R4)                                
         LA    R4,1(R4)                                                         
         LA    R6,6(R6)                                                         
         BCT   R5,CE64                                                          
*                                                                               
         GOTO1 (RF),(R1),6         BUMP TOTALS INTO TABLE                       
*                                                                               
         LA    R3,1                DO JOB LINE                                  
         BAS   RE,FORMAT                                                        
         CLI   PRTSW,C'X'          ANYTHING TO PRINT                            
         BNE   CE64A               YES                                          
*                                                                               
         MVC   P,SPACES            CLEAR JOB NAME                               
         MVC   PSECOND,SPACES                                                   
         BAS   RE,MEDPRT           RESTORE MEDIA                                
         B     CEXIT                                                            
*                                                                               
CE64A    CLI   QOPT4,C'Y'                                                       
         BNE   CE65                                                             
*                                                                               
         CLI   QOPT4SW,C'Y'        SUMMARY OPTION                               
         BNE   CE65                NO                                           
*                                                                               
         MVI   OP4MSW,C'Y'                                                      
         BAS   RE,MEDPRT                                                        
         MVC   P,SPACES                                                         
         MVI   QOPT4SW,C'N'                                                     
         MVC   P+1(36),SAVEN                                                    
         MVI   PRTSW,C'M'                                                       
         BAS   RE,PRINTEM                                                       
         MVI   MEDSW,C'Y'                                                       
CE65     CLI   MEDSW,C'Y'                                                       
         BNE   CE66                                                             
*                                                                               
         MVC   P,SPACES                                                         
         BAS   RE,MEDPRT                                                        
         MVI   MEDSW,C'N'                                                       
         BAS   RE,PRTREST                                                       
*                                                                               
CE66     CLI   QOPT4,C'Y'                                                       
         BE    CEXIT                                                            
*                                                                               
         BAS   RE,PRINTBL          PRINT INVOICE DETAILS                        
         BAS   RE,PRINTEM          SKIP A LINE                                  
         B     CEXIT                                                            
         EJECT                                                                  
CE80     CLI   MODE,LEVBLAST       LAST FOR PRODUCT                             
         BNE   CE90                                                             
         MVC   P,SPACES                                                         
         CLI   PRTSW,C'N'                                                       
         BE    CEXIT                                                            
         CLI   QOPT4,C'Y'          SUMMARY FORMAT                               
         BNE   CE80A                                                            
         CLI   OP4MSW,C'Y'         MEDIA NAME SAVED                             
         BNE   CE82                                                             
         MVC   P+1(12),SVMED                                                    
         MVI   OP4MSW,C'N'                                                      
         B     CE82                                                             
*                                                                               
CE80A    MVC   P+26(9),=C'SUB-TOTAL'                                            
         LA    R3,2                                                             
         BAS   RE,FORMAT                                                        
         CLI   PROGPROF,C'Y'                                                    
         BE    *+14                                                             
         MVC   P,SPACES                                                         
         B     *+12                                                             
         MVI   SPACING,2                                                        
         BAS   RE,PRINTEM                                                       
*                                                                               
         MVC   P+18(11),=C'MEDIA TOTAL'                                         
CE82     LA    R3,3                                                             
         BAS   RE,FORMAT                                                        
         MVI   SPACING,2                                                        
         BAS   RE,PRINTEM                                                       
*                                                                               
         MVC   P+16(13),=C'PRODUCT TOTAL'                                       
         LA    R3,4                                                             
         BAS   RE,FORMAT                                                        
         MVI   SPACING,2                                                        
         BAS   RE,PRINTEM                                                       
         CLI   QOPT4,C'Y'                                                       
         BE    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         MVC   FRST(4),SPACES                                                   
         B     CEXIT                                                            
         EJECT                                                                  
CE90     CLI   MODE,LEVALAST       LAST FOR CLIENT                              
         BNE   CE100                                                            
         LA    R3,5                                                             
         MVC   P+16(13),=C' CLIENT TOTAL'                                       
         MVI   PRTSW2,C'N'         SET VALID AMOUNT SWITCH OFF                  
         BAS   RE,FORMAT           FORMAT CHGS PRTSW2 TO Y FOR ANY NON          
*                                  ZERO AMOUNT IT FINDS.                        
         CLI   PRTSW2,C'Y'         ANY VALID AMOUNTS FOR CLIENT                 
         BNE   CEXIT                                                            
         MVI   PRTSW,C'M'                                                       
         BAS   RE,PRINTEM                                                       
         B     CEXIT                                                            
*                                                                               
CE100    CLI   MODE,REQLAST                                                     
         BNE   CEXIT                                                            
         CLI   QOPT5,C' '                                                       
         BE    CEXIT                                                            
*                                                                               
         L     R2,VEXTRAS          TURN OFF SPECIAL BOXES                       
         USING RUNXTRAD,R2                                                      
         L     R2,ADMASTD                                                       
         USING MASTD,R2                                                         
         L     R2,MCBXAREA                                                      
         USING BOXD,R2                                                          
         XC    BOXHOOK,BOXHOOK                                                  
*                                                                               
         BAS   RE,PRINTIT          PRINT W-CODE TABLE                           
         B     CEXIT                                                            
         EJECT                                                                  
*                                                                               
JOBNAME  NTR1                                                                   
         MVC   WORK,SPACES                                                      
         ZIC   RE,MEDPOS                                                        
         LR    RF,R2                                                            
         AR    RF,RE                                                            
         LA    R1,14                                                            
         SR    R1,RE                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P+1(0),0(RF)                                                     
         L     R4,ADACCNAM                                                      
         LA    R3,P+9                                                           
         USING ACNAMED,R4                                                       
         ZIC   R5,ACNMLEN                                                       
         SH    R5,=H'3'                                                         
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),ACNMNAME                                                 
*                                                                               
         LA    R5,1(R5)            R5 IS LENGTH OF NAME                         
         LA    R5,WORK(R5)         R5 IS A(AVAILABLE SPACE IN WORK)             
*                                                                               
         CLI   CLOSED,C'Y'         CLOSED JOB                                   
         BNE   JOBN50              NO                                           
*                                                                               
         MVC   0(6,R5),=C', ST=C'                                               
         LA    R5,6(R5)                                                         
*                                                                               
JOBN50   BAS   RE,GETEST                                                        
         CLC   ESTPRT,SPACES                                                    
         BE    JOBN60                                                           
         MVI   0(R5),C','                                                       
         MVC   2(7,R5),ESTPRT                                                   
*                                                                               
JOBN60   LA    RF,32                                                            
         CLI   INTEXTSW,C'N'                                                    
         BE    *+8                                                              
         LA    RF,20                                                            
         GOTO1 CHOPPER,DMCB,(L'WORK,WORK),((RF),(R3)),(C'P',2)                  
         B     CEXIT                                                            
         EJECT                                                                  
ADDIT    NTR1                      ADD TO W-CODE/BILLING TABLE                  
         USING TRANSD,R4                                                        
         XC    BUFKEY,BUFKEY                                                    
         MVI   BUFTYPE,WORKCODE                                                 
         MVC   BUFWC,TRNSANAL                                                   
         ZAP   BUFAC1,TRNSAMNT     NET                                          
         ZAP   PL13,TRNSAMNT                                                    
         MP    PL13,GOAGYCOM     COMMISSION RATE                                
         SRP   PL13,64-6,5         RATE IS FOUR DECIMAL PLACES                  
         ZAP   BUFAC2,PL13         COMMISSION                                   
         TM    TRNSSTAT,X'01'      NON-COMMISSIONABLE                           
         BZ    *+10                                                             
         ZAP   BUFAC2,=P'0'                                                     
         AP    BUFAC2,BUFAC1                                                    
         MVC   BUFNAME,SPACES                                                   
         L     RF,ADLEDGER                                                      
         AH    RF,DATADISP                                                      
ADD2     CLI   0(RF),0                                                          
         BE    ADD8                                                             
         CLI   0(RF),X'12'                                                      
         BE    ADD6                                                             
ADD4     ZIC   RE,1(RF)                                                         
         AR    RF,RE                                                            
         B     ADD2                                                             
         USING ACANALD,RF                                                       
ADD6     CLC   ACANCODE,TRNSANAL                                                
         BNE   ADD4                                                             
         MVC   BUFNAME,ACANDESC                                                 
ADD8     DS    0H                                                               
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUFC,BUFREC                               
         B     CEXIT                                                            
         EJECT                                                                  
PUTBILL  NTR1                      PUT BILLING DETAILS TO BUFFALO               
         USING TRANSD,R4                                                        
         USING ACKEYD,R5                                                        
         L     R4,ADTRANS                                                       
         LR    R5,R4                                                            
         SH    R5,DATADISP                                                      
*                                                                               
         XC    BUFKEY,BUFKEY                                                    
         MVI   BUFTYPE,BILLING                                                  
         MVC   BUFNUM,TRNSREF                                                   
*                                                                               
         LH    RE,BILLCNT          KEEP A COUNT OF BILLS ON JOB                 
         LA    RE,1(RE)                                                         
         STH   RE,BILLCNT                                                       
*                                                                               
         MVC   BUFSEQ,BILLCNT      SAVE COUNT AS BUFF KEY                       
*                                                                               
         ZAP   BUFAC2,BILLAMNT     TO DATE BILLING                              
         ZAP   BUFAC1,=P'0'        CLEAR CURRENT MONTH                          
*                                                                               
         CLC   TRNSDATE(2),TODAY   CURRENT MONTH BILLING?                       
         BNE   *+10                                                             
         ZAP   BUFAC1,BILLAMNT     SAVE CURRENT MONTH BILLS                     
*                                                                               
         CP    BUFAC2,=P'0'        IS THIS A NIL BILL                           
         BNE   PUTB50              NO                                           
         ZAP   BUFAC2,=P'999999999999999' FILL W/ UNREASONABLE PL8              
*                                                                               
PUTB50   GOTO1 BUFFALO,DMCB,=C'PUT',ADBUFC,BUFREC                               
*                                                                               
         B     CEXIT                                                            
         EJECT                                                                  
PRINTIT  NTR1                      PRINT W-CODE/BILLING TABLE                   
         ZAP   NET,=P'0'                                                        
         ZAP   GROSS,=P'0'                                                      
         MVC   P,SPACES                                                         
         MVC   PSECOND,SPACES                                                   
*                                                                               
         XC    BUFKEY,BUFKEY                                                    
         MVI   BUFTYPE,WORKCODE                                                 
         MVI   RCSUBPRG,2                                                       
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 BUFFALO,DMCB,=C'HIGH',ADBUFC,BUFREC,1                            
PRNT2    TM    DMCB+8,X'80'                                                     
         BO    PRNTXT                                                           
         CLI   BUFTYPE,WORKCODE    GOT A WORKCODE RECORD?                       
         BNE   PRNTXT              NO                                           
*                                                                               
         AP    NET,BUFAC1                                                       
         AP    GROSS,BUFAC2                                                     
         MVC   P+1(2),BUFWC                                                     
         MVC   P+4(15),BUFNAME                                                  
         EDIT  BUFAC1,(13,P+23),2,MINUS=YES                                     
         EDIT  BUFAC2,(13,P+40),2,MINUS=YES                                     
         GOTO1 ACREPORT                                                         
         GOTO1 BUFFALO,DMCB,=C'SEQ',ADBUFC,BUFREC,1                             
         B     PRNT2                                                            
*                                                                               
PRNTXT   DS    0H                                                               
         GOTO1 BUFFALO,DMCB,=C'RESET',ADBUFC                                    
         CP    NET,=P'0'                                                        
         BE    CEXIT                                                            
         GOTO1 ACREPORT                                                         
         MVC   P+4(6),=C'TOTALS'                                                
         EDIT  NET,(13,P+23),2,MINUS=YES                                        
         EDIT  GROSS,(13,P+40),2,MINUS=YES                                      
         GOTO1 ACREPORT                                                         
         B     CEXIT                                                            
         EJECT                                                                  
PRINTBL  NTR1                      PRINT BILLING DETAILS                        
         OC    BILLCNT,BILLCNT     ANY BILLS ON JOB                             
         BZ    PBILLX              NO                                           
*                                                                               
         CLI   QOPT6,C'Y'          SUPRESS INVOICE DETAIL                       
         BE    PBILLX                                                           
*                                                                               
         CLI   QOPT6,C'N'          INCLUDE INVOICE DETAIL                       
         BE    PBILL05                                                          
*                                                                               
         CLI   PROGPROF+2,C'Y'     SUPPRESS INVOICE DETAIL?                     
         BE    PBILLX                                                           
*                                                                               
         USING PRINTD,R5                                                        
PBILL05  LA    R5,P                                                             
         MVC   PRINVNO,=C'  **  '                                               
*                                                                               
         LA    R5,PSECOND                                                       
         CLC   BILLCNT,=H'1'       ONLY ONE BILL ON JOB                         
         BNE   *+8                 NO                                           
         LA    R5,P                YES, PRINT BILL DETAILS ON JOB LINE          
*                                                                               
         XC    BUFKEY,BUFKEY                                                    
         MVI   BUFTYPE,BILLING                                                  
         GOTO1 BUFFALO,DMCB,=C'HIGH',ADBUFC,BUFREC,1                            
*                                                                               
PBILL10  TM    DMCB+8,X'80'                                                     
         BO    PBILLX                                                           
*                                                                               
         CLI   BUFTYPE,BILLING     GOT A WORKCODE RECORD?                       
         BNE   PBILLX              NO                                           
*                                                                               
         MVC   PRINVNO,BUFNUM                                                   
         CP    BUFAC1,=P'0'                                                     
         BE    PBILL30                                                          
         SRP   BUFAC1,64-2,5                                                    
         EDIT  BUFAC1,(11,PRCURMTH),MINUS=YES,COMMAS=YES                        
*                                                                               
PBILL30  CP    BUFAC2,=P'999999999999999' IS THIS REALLY A ZERO                 
         BNE   PBILL40                                                          
*                                                                               
         MVC   PRTODATE,=CL11'       NIL '                                      
         B     PBILL50                                                          
*                                                                               
PBILL40  SRP   BUFAC2,64-2,5                                                    
         EDIT  BUFAC2,(11,PRTODATE),MINUS=YES,COMMAS=YES                        
*                                                                               
PBILL50  LA    R5,L'P(R5)                                                       
         CLC   PFOURTH,SPACES      DID I PRINT INTO PFOURTH                     
         BE    PBILL60             NOT YET                                      
*                                                                               
         GOTO1 ACREPORT                                                         
         LA    R5,P                RESET R5                                     
*                                                                               
PBILL60  GOTO1 BUFFALO,DMCB,=C'SEQ',ADBUFC,BUFREC,1                             
         B     PBILL10                                                          
*                                                                               
PBILLX   DS    0H                                                               
         CLC   P,SPACES            ANYTHING LEFTOVER                            
         BE    PBILLX10            NO                                           
         GOTO1 ACREPORT            YES PRINT IT                                 
*                                                                               
PBILLX10 MVI   BUFTYPE,BILLING                                                  
         GOTO1 BUFFALO,DMCB,=C'CLEAR',(BUFTYPE,ADBUFC),1,(X'80',1)              
         B     CEXIT                                                            
         EJECT                                                                  
*              FORMAT AND PRINT A LINE OF PROLLER TABLE                         
*                                                                               
FORMAT   NTR1                                                                   
         GOTO1 PROLLER,DMCB,1,TABLE,(R3)                                        
         MVI   PRTSW,C'M'                                                       
         CLI   PROGPROF+1,C'Y'                                                  
         BNE   *+8                                                              
         MVI   PRTSW,C'X'                                                       
*                                                                               
         L     R3,0(R1)                                                         
         LA    RF,PRTOFFS+1                                                     
         LA    R5,5                                                             
         LA    R3,6(R3)                                                         
         SR    R1,R1                                                            
         CLI   INTEXTSW,C'N'       ARE THEY PRINTING 2 ESTIMATES?               
         BE    FMT2                NO                                           
*                                                                               
         LA    RF,PRTOFFS                                                       
         LA    R5,6                                                             
         SH    R3,=H'6'                                                         
*                                                                               
FMT2     IC    R1,0(RF)            GET PRINT OFFSET                             
         LA    R6,P                                                             
         LA    R6,0(R1,R6)                                                      
         MVC   0(10,R6),SPACES     CLEAR PRINT FIELD                            
         ZAP   DOUBLE,0(6,R3)      ROUND AND LOSE PENNIES                       
         CP    DOUBLE,=P'0'                                                     
         BE    FMT4                                                             
         MVI   PRTSW2,C'Y'         VALID AMT, TURN ON CLIENT TOTAL SW.          
         MVI   PRTSW,C'M'                                                       
         SRP   DOUBLE,64-2,5                                                    
         EDIT  (P8,DOUBLE),(11,0(R6)),MINUS=YES,COMMAS=YES                      
         ZAP   0(6,R3),=P'0'                                                    
FMT4     LA    RF,1(RF)            NEXT PRINT OFFSET                            
         LA    R3,6(R3)            NEXT BUCKET                                  
         BCT   R5,FMT2                                                          
*                                                                               
         BAS   RE,PRTSAVE                                                       
         B     GENXT                                                            
         EJECT                                                                  
*              DIG OUT AND PRINT MEDIA NAME                                     
*                                                                               
MEDPRT   NTR1                                                                   
         MVC   P+1(32),SPACES      CLEAR OUT ANY NAME HANGING AROUND            
         MVC   PSECOND(32),SPACES                                               
         MVC   PTHIRD(32),SPACES                                                
         MVI   PRTSW,C'M'                                                       
         ZIC   RF,LINE                                                          
         CH    RF,=H'2'                                                         
         BH    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         L     R3,ADACC                                                         
         ZIC   R1,MEDPOS                                                        
         AR    R3,R1                                                            
         L     R2,ADCOMP                                                        
         MVI   ELCODE,X'11'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACMEDIAD,R2                                                      
GETMD2   CLC   ACMDCODE,0(R3)                                                   
         BE    GETMD4                                                           
         BAS   RE,NEXTEL                                                        
         BE    GETMD2                                                           
         DC    H'0'                                                             
*                                                                               
GETMD4   MVC   P+1(12),ACMDDESC+3                                               
         MVC   SVMED,ACMDDESC+3                                                 
         CLI   QOPT4,C'Y'                                                       
         BE    GENXT                                                            
         BAS   RE,PRINTEM                                                       
         B     GENXT                                                            
         EJECT                                                                  
GETMED   NTR1                                                                   
         L     R2,ADCOMP                                                        
         MVI   ELCODE,X'11'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACMEDIAD,R2                                                      
GETM2    CLC   ACMDCODE,MEDIA                                                   
         BE    GETM4                                                            
         BAS   RE,NEXTEL                                                        
         BE    GETM2                                                            
         DC    H'0'                                                             
*                                                                               
GETM4    EQU   *                                                                
         MVC   SAVEMED,ACMDDESC+3                                               
         B     GENXT                                                            
         EJECT                                                                  
LOOKUP   NTR1                                                                   
         L     R3,AMONACC                                                       
         USING ACMD,R3                                                          
*                                                                               
         L     R5,ACMAJOBB                                                      
         USING JBLOCKD,R5                                                       
         MVC   JBAJOB,ADACC                                                     
         MVC   JBACOLS,ACMACOLL                                                 
         MVC   JBACOM,ADCOMFAC                                                  
         MVC   JBAGOBLK,ADGOBLOC                                                
         MVC   JBAIO,ACMAJOBI                                                   
         MVC   JBAKEY,LASTIO                                                    
         MVC   JBGETOPT,GETOPT                                                  
*                                                                               
         MVC   JBACOLTB,ACMACOL                                                 
         MVC   JBLCOLTB,ACMLCOL                                                 
         MVC   JBAOPVTB,ACMAOPV                                                 
         MVC   JBLOPVTB,ACMLOPV                                                 
*                                                                               
         LA    RE,FLDH                                                          
         ST    RE,JBORICLI                                                      
*                                                                               
         GOTO1 ACMAJOBR,DMCB,ACMAJOBB                                           
         CLI   JBERROR,X'00'                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,ACMACOL                                                       
         XIT1  REGS=(R3,R5)                                                     
*                                                                               
FLDH     DC    AL1(8+L'FLD),4X'00',AL1(L'FLD),AL2(0)                            
FLD      DC    C'OE,CE,OEG,CEG'                                                 
*                                                                               
         DROP  R3,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------+         
*              FILTER JOBS BASED ON ESTIMATE STATUS                   |         
*---------------------------------------------------------------------+         
*                                                                               
ESTFLT   NTR1                                                                   
         CLI   QOPT7,C' '          ANYTHING THERE                               
         BE    EXIT                NO. WE ARE DONE                              
*                                                                               
         L     R3,AMONACC                                                       
         USING ACMD,R3                                                          
*                                                                               
         L     R5,ACMAJOBB                                                      
         USING JBLOCKD,R5                                                       
         L     R3,ACMACOL                                                       
         USING JBLOCKD,R5                                                       
         LH    R1,JBNROWS                                                       
         XC    ESTSTAT,ESTSTAT                                                  
         CLI   JBNEWEST,JBMCSQ     IS THIS A BO JOB?                            
         BNE   EFLT5               NO                                           
         USING MJETABD,R3                                                       
EFLT4    CLI   MJETTYP,MJETTEQ                                                  
         BE    EFLT7                                                            
         CLI   MJETTYP,MJETTWQ                                                  
         BNE   EFLT4A                                                           
         OC    MJET1RA,MJET1RA     BUT NOT 1R-LEVEL DETAILS                     
         BZ    EFLT6                                                            
*                                                                               
EFLT4A   XR    R1,R1                                                            
         IC    R1,MJETLEN                                                       
         AR    R3,R1                                                            
         B     EFLT4                                                            
         DROP  R3                                                               
*                                                                               
         USING JBCOLD,R3                                                        
EFLT5    CLI   JBCOLTYP,JBCOLTWC  THE PRESENCE OF WORKCODES DETERMINES,         
         BE    EFLT6                                                            
         AH    R3,JBLCOL                                                        
         BCT   R1,EFLT5                                                         
         B     EFLT7                                                            
*                                                                               
EFLT6    OI    ESTSTAT,HAVENEST    IF WE HAVE AN ESTIMATE                       
*                                                                               
*                                  CHECK ESTIMATE OPTION ON REQCARD             
EFLT7    CLI   QOPT7,C'R'          ANYTHING THERE                               
         BE    EFLT9               ONLY JOBS WITH HIGHER UNAPP REVS             
*                                                                               
EFLT8    CLI   JBNEWEST,C'Y'       IS JOB ON NEW ESTIMATES                      
         BNE   EFLT8B              NO                                           
         OC    JBHIAPP,JBHIAPP     HAS IT BEEN APPROVED?                        
         BNZ   *+8                 YES                                          
         OI    ESTSTAT,ESTUNAPP                                                 
*                                                                               
         CLI   GONEEDAE,C'Y'       THIS JOB NEED APPROV TO BILL                 
         BNE   *+8                NO                                            
         OI    ESTSTAT,NEEDNAPP                                                 
*                                                                               
         CLI   GONEEDES,C'Y'      NEED AN ESTIMATE                              
         BNE   *+8                NO                                            
         OI    ESTSTAT,NEEDNEST                                                 
*                                                                               
         B     EFLT8E                                                           
*                                                                               
         USING ACJOBD,R2                                                        
EFLT8B   EQU   *                                                                
         OI    ESTSTAT,NEEDNAPP    OLD EST NEED APPROVAL IF UNAPP               
*                                                                               
         CLI   GONEEDES,C'Y'       DOES THIS JOB NEED AN ESTIMATE?              
         BNE   *+8                                                              
         OI    ESTSTAT,NEEDNEST                                                 
         L     R2,ADACC            JOB USES OLD ESTIMATE                        
         MVI   ELCODE,X'26'        GET JOB ELEMENT                              
         BAS   RE,GETEL            IS IT UPAPPROVED                             
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   ACJBLEN,ACJBLNQ3    IS EL LONG ENUF TO HAVE UPAPP BIT            
         BL    EFLT8E              NO, ITS APPROVED                             
         TM    ACJBSTAT,X'80'      DOES JOB HAVE AN UNAPPROVED EST              
         BNO   EFLT8E              NO                                           
         OI    ESTSTAT,ESTUNAPP                                                 
*                                                                               
*                                                                               
EFLT8E   LA    R4,ESTTAB                                                        
EFLT8F   CLC   0(1,R4),ESTSTAT     PATTERN MATCH ON TABLE                       
         BNE   EFLT8G                                                           
         CLC   1(1,R4),QOPT7       IS THIS THE PATTERN THEY WANT                
         BE    EXIT                YES                                          
         B     EFLTX               NO                                           
EFLT8G   LA    R4,L'ESTTAB(R4)                                                  
         B     EFLT8F                                                           
*                                                                               
EFLT9    CLI   JBNEWEST,C'Y'       SPECIAL CHECK FOR HIGHER REVS                
         BNE   EFLTX                                                            
         CLI   GONEEDAE,C'Y'                                                    
         BNE   EFLTX                                                            
         CLI   JBHIAPP,0                                                        
         BE    EFLTX                                                            
         CLC   JBHIAPP,JBHIREV                                                  
         BE    EFLTX               JOB NOT WANTED                               
         B     EXIT                                                             
*                                                                               
EFLTX    MVI   FCRDTRNS,C'N'       THIS JOB NOT WANTED                          
         B     EXIT                                                             
*                                                                               
ESTSTAT  DS    CL1                                                              
ESTTAB   DS    0CL2                                                             
*        NEED APPROVAL AND OLD EST JOBS                                         
         DC    B'00001111',C'U'    EST PRES, NEEDANEST,EST UNAPP                
         DC    B'00001110',C'A'                        EST APP                  
         DC    B'00001101',C'A'              DONT NEED,EST UNAPP                
         DC    B'00001100',C'A'                        EST APP                  
         DC    B'00001011',C'N'    NO ESTIMAT NEED     EST UNAPP                
         DC    B'00001010',C'N'                        EST APP                  
         DC    B'00001001',C'A'               DONT NEEDEST UNAPP                
         DC    B'00001000',C'A'                        EST APP                  
*        NEW EST JOBS WHICH DON'T NEED AN APPROVAL                              
         DC    B'00000111',C'A'    EST PRES, NEEDANEST,EST UNAPP                
         DC    B'00000110',C'A'                        EST APP                  
         DC    B'00000101',C'A'              DONT NEED,EST UNAPP                
         DC    B'00000100',C'A'                        EST APP                  
         DC    B'00000011',C'N'    NO ESTIMAT NEED     EST UNAPP                
         DC    B'00000010',C'N'                        EST APP                  
         DC    B'00000001',C'A'               DONT NEEDEST UNAPP                
         DC    B'00000000',C'A'                        EST APP                  
ESTTNUM  EQU   ((*-ESTTAB)/L'ESTTAB)                                            
NEEDNAPP EQU   X'08'                                                            
HAVENEST EQU   X'04'                                                            
NEEDNEST EQU   X'02'                                                            
ESTUNAPP EQU   X'01'                                                            
         DROP R2                                                                
*---------------------------------------------------------------------          
*        RETURN IN FULL EA=Y OR EA=N OR SPACES IF ESTIMATE STATUS IS            
*        NOT IMPORTANT FOR THIS JOB, FOR NEW EST JOBS, PRINT EA=RN              
*---------------------------------------------------------------------          
GETEST   NTR1                                                                   
         L     R3,AMONACC                                                       
         USING ACMD,R3                                                          
*                                                                               
         L     R5,ACMAJOBB                                                      
         USING JBLOCKD,R5                                                       
         L     R3,ACMACOL                                                       
         CLI   JBNEWEST,JBMCSQ     IS THIS A BO JOB?                            
         BNE   GET04               NO                                           
         USING MJETABD,R3                                                       
*                                                                               
GET02    CLI   MJETTYP,MJETTEQ     ARE WE AT THE END?                           
         BE    GET10                                                            
         CLI   MJETTYP,MJETTWQ     NO, LOOK FOR WORKCODE                        
         BNE   GET03                                                            
         OC    MJET1RA,MJET1RA     BUT NOT 1R-LEVEL DETAILS                     
         BZ    GET08                                                            
*                                                                               
GET03    XR    R1,R1                                                            
         IC    R1,MJETLEN                                                       
         AR    R3,R1                                                            
         B     GET02                                                            
         DROP  R3                                                               
*                                                                               
         USING JBCOLD,R3                                                        
GET04    LH    R1,JBNROWS                                                       
         MVI   HALF,C'N'                                                        
*                                                                               
GET06    CLI   JBCOLTYP,JBCOLTWC                                                
         BE    GET08                                                            
         AH    R3,JBLCOL                                                        
         BCT   R1,GET06                                                         
         B     *+8                                                              
*                                                                               
GET08    MVI   HALF,C'Y'           YES, WE HAVE AN ESTIMATE                     
*                                                                               
GET10    CLI   JBNEWEST,C'Y'       IS JOB ON NEW ESTIMATES                      
         BNE   GET12               NO                                           
         CLI   GONEEDAE,C'Y'       THIS JOB NEED APPROV TO BILL                 
         BNE   GET14              NO                                            
         CLI   GONEEDES,C'Y'      NEED AN ESTIMATE                              
         BNE   GET14              NO                                            
         OC    JBHIAPP,JBHIAPP     HAS IT BEEN APPROVED?                        
         BNZ   GETNEW                                                           
         B     GETNO                                                            
*                                                                               
GET12    EQU   *                   OLD ESTS                                     
         CLI   HALF,C'Y'           HAVE AN ESTIMATE                             
         BNE   GET14               NO                                           
         CLI   GONEEDES,C'Y'       DOES THIS JOB NEED AN ESTIMATE?              
         BNE   GET14                                                            
*                                                                               
         USING ACJOBD,R2                                                        
         L     R2,ADACC            JOB USES OLD ESTIMATE                        
         MVI   ELCODE,X'26'        GET JOB ELEMENT                              
         BAS   RE,GETEL            IS IT UPAPPROVED                             
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   ACJBLEN,ACJBLNQ3    IS EL LONG ENUF TO HAVE UPAPP BIT            
         BL    GETYES              NO, ITS APPROVED                             
         TM    ACJBSTAT,X'80'      DOES JOB HAVE AN UNAPPROVED EST              
         BNO   GETYES              NO                                           
*                                                                               
GETNO    MVC   ESTPRT,=CL7'EA=N'                                                
         B     GETX                                                             
*                                                                               
GETYES   MVC   ESTPRT,=CL7'EA=Y'                                                
         B     GETX                                                             
*                                                                               
GETNEW   MVC   ESTPRT,=CL7'EA=R'                                                
         LA    R4,JBHIAPP                                                       
         EDIT  (B1,0(R4)),(3,ESTPRT+4),ALIGN=LEFT,WRK=DMCB                      
         B     GETX                                                             
*                                                                               
GET14    MVC   ESTPRT,SPACES                                                    
GETX     B     CEXIT                                                            
         DROP R3,R5                                                             
         EJECT                                                                  
*              PRINT LINE AND FILL HEADLINE                                     
*                                                                               
PRINTEM  NTR1                                                                   
         CLI   PRTSW,C'M'                                                       
         BE    *+14                                                             
         MVC   P,SPACES                                                         
         B     GENXT                                                            
         GOTO1 ACREPORT                                                         
         B     GENXT                                                            
*                                                                               
         USING ACNAMED,R4                                                       
NAMOUT   ZIC   RF,ACNMLEN                                                       
         SH    RF,=H'3'                                                         
         EX    RF,*+6                                                           
         BR    RE                                                               
         MVC   0(0,R3),ACNMNAME                                                 
*                                                                               
         GETEL R2,DATADISP,ELCODE                                               
*                                                                               
PRTSAVE  EQU   *                                                                
         MVC   SAVEP(110),P                                                     
         MVC   SAVEP+110(110),PSECOND                                           
         MVC   SAVEP+220(110),PTHIRD                                            
         MVC   SAVEP+330(110),PFOURTH                                           
         BR    RE                                                               
*                                                                               
PRTREST  EQU   *                                                                
         MVC   P(110),SAVEP                                                     
         MVC   PSECOND(110),SAVEP+110                                           
         MVC   PTHIRD(110),SAVEP+220                                            
         MVC   PFOURTH(110),SAVEP+330                                           
         BR    RE                                                               
*                                                                               
PRTCLEAR EQU   *                                                                
         MVC   SAVEP(L'SPACES),SPACES                                           
         BR    RE                                                               
*                                                                               
PRTOFFS  DC    AL1(PRORIG-PRINTD)                                               
         DC    AL1(PRCURR-PRINTD)                                               
         DC    AL1(PRCURMTH-PRINTD)                                             
         DC    AL1(PRTODATE-PRINTD)                                             
         DC    AL1(PRFINAL-PRINTD)                                              
         DC    AL1(PRUNBEST-PRINTD)                                             
*                                                                               
AGOBUFF  DC    A(GOBUFF)                                                        
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              HEADHOOK - CALLED FROM ACPRINT                                   
*--------------------------------------------------------------------*          
HDHOOK   NMOD1 0,*HEADHK                                                        
         L     R2,=A(BOXRC)                                                     
         ICM   RC,15,0(R2)                                                      
         CLI   RCSUBPRG,2          DOING W/C SUMMARY                            
         BE    PRT2                                                             
*                                                                               
         MVI   HEAD10,X'FA'        GET RID OF THAT PESKY MIDLINE                
         CLI   INTEXTSW,C'Y'                                                    
         BE    HDH20                                                            
         XC    HEAD10+1(40),HEAD10+1 CLEAR MIDLINE FOR 40                       
         MVI   HEAD10+41,X'EB'                                                  
         B     HDH30                                                            
*                                                                               
HDH20    XC    HEAD10+1(28),HEAD10+1 CLEAR MIDLINE FOR 28                       
         MVI   HEAD10+29,X'EB'                                                  
*                                                                               
HDH30    MVC   HEAD8+43(8),=C'ESTIMATE'                                         
         MVC   HEAD9+44(6),=C'AMOUNT'                                           
*                                                                               
         CLI   INTEXTSW,C'N'                                                    
         BE    PRT1                                                             
         MVC   HEAD8+31(20),=C'EXTERNAL    INTERNAL'                            
         MVC   HEAD9+31(20),=C'ESTIMATE    ESTIMATE'                            
PRT1     DS    0H                                                               
*                                                                               
         L     R2,ADHEIRB                                                       
         GOTO1 =V(ACCDIV),DMCB,ADLDGHIR,(R2),WORK                               
         MVC   HEAD4+10(6),WORK+1  CLIENT CODE                                  
*                                                                               
         L     R4,ADLVANAM         CLIENT NAME                                  
         LA    R3,HEAD4+16                                                      
         BAS   RE,VNAMOUT                                                       
         CLI   QOPT4,C'Y'                                                       
         BE    PRT2                                                             
*                                                                               
         CLI   MODE,LEVALAST                                                    
         BE    PRT2                                                             
*                                                                               
         MVC   HEAD6+1(7),=C'PRODUCT'                                           
         MVC   HEAD6+10(6),WORK+14 PRODUCT CODE                                 
         L     R4,ADLVBNAM         PRODUCT NAME                                 
         LA    R3,HEAD6+16                                                      
         BAS   RE,VNAMOUT                                                       
PRT2     MVC   HEAD4+83(10),=C'NET OPTION'                                      
         CLI   QOPT3,C'G'                                                       
         BNE   *+10                                                             
         MVC   HEAD4+83(12),=C'GROSS OPTION'                                    
         MVC   HEAD4+44(14),=C'CURRENT MONTH:'                                  
         MVC   HEAD4+59(6),CURRMTHP                                             
*                                                                               
         CLI   QOPT7,C' '                                                       
         BE    PRT3                                                             
*                                                                               
         MVC   HEAD5+70(L'JOBSWITH),JOBSWITH                                    
         LA    RF,ESTLITS                                                       
         LA    R0,NLITS                                                         
PRT2C    CLC   QOPT7,0(RF)                                                      
         BE    PRT2D                                                            
         LA    RF,L'ESTLITS(RF)                                                 
         BCT   R0,PRT2C                                                         
         DC    H'0'                                                             
*                                                                               
PRT2D    MVC   HEAD5+80(32),1(RF)                                               
*                                                                               
PRT3     L     R4,ADBOX                                                         
         USING BOXD,R4                                                          
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXROWS,SPACES                                                   
         SPACE 1                                                                
         CLI   RCSUBPRG,2          DOING W/C SUMMARY                            
         BE    PRT4                YES                                          
*                                                                               
         MVI   BOXROWS+6,C'T'      SET ROWS                                     
         MVI   BOXROWS+9,C'M'                                                   
         MVI   BOXROWS+12,C'M'                                                  
         MVI   BOXROWS+56,C'B'                                                  
         SPACE 1                                                                
         USING PRINTD,R2                                                        
         LA    R2,BOXCOLS                                                       
         MVI   PRLEFT,C'L'                                                      
         CLI   INTEXTSW,C'Y'       SHOW 2 ESTS                                  
         BNE   *+8                 NO, DON'T NEED COL1                          
         MVI   PRCOL1,C'C'                                                      
         MVI   PRCOL2,C'C'                                                      
         MVI   PRCOL3,C'C'                                                      
         MVI   PRCOL7,C'C'                                                      
         MVI   PRRIGHT,C'R'                                                     
         B     PRT5                                                             
*                                                                               
PRT4     EQU   *                                                                
         MVI   BOXROWS+6,C'T'      SET ROWS                                     
         MVI   BOXROWS+9,C'M'                                                   
         MVI   BOXROWS+56,C'B'                                                  
         MVI   BOXCOLS,C'L'                                                     
         MVI   BOXCOLS+19,C'C'                                                  
         MVI   BOXCOLS+36,C'C'                                                  
         MVI   BOXCOLS+53,C'R'                                                  
*                                                                               
PRT5     MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         XMOD1 1                                                                
         DROP  R2                                                               
*                                                                               
         LTORG                                                                  
         USING ACNAMED,R4                                                       
VNAMOUT  ZIC   RF,ACNMLEN                                                       
         SH    RF,=H'3'                                                         
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    RF,*+6                                                           
         BR    RE                                                               
         MVC   0(0,R3),ACNMNAME                                                 
         EJECT                                                                  
JOBSWITH DC    C'JOBS WITH'                                                     
ESTLITS  DS    0CL33                                                            
         DC    C'N'                                                             
         DC    CL32'NO ESTIMATE BUT NEED ONE TO BILL'                           
         DC    C'A'                                                             
         DC    CL32'AN APPROVED ESTIMATE'                                       
         DC    C'U'                                                             
         DC    CL32'AN UNAPPROVED ESTIMATE'                                     
         DC    C'R'                                                             
         DC    CL32'AN ESTIMATE AWAITING APPROVAL'                              
NLITS    EQU   (*-ESTLITS)/L'ESTLITS                                            
*--------------------------------------------------------------------*          
*              BOXHOOK - CALLED FROM DDPRINT                                    
*--------------------------------------------------------------------*          
BXHOOK   NMOD1 0,*BHOOK                                                         
         L     R2,=A(BOXRC)                                                     
         ICM   RC,15,0(R2)                                                      
         L     R4,ADBOX                                                         
         USING BOXD,R4                                                          
         USING PRINTD,R5                                                        
         LA    R5,BOXCOLS                                                       
         SPACE 1                                                                
         CLI   4(R1),8             START ROW 8 AT 50                            
         BNE   BOX20                                                            
         MVI   PRLEFT,C'V'                                                      
         MVI   PRCOL3,C'C'                                                      
         MVI   PRRIGHT,C'R'                                                     
         SPACE 1                                                                
BOX20    CLI   4(R1),10                                                         
         BNE   BOXX                                                             
         MVI   PRLEFT,C'L'                                                      
         MVI   PRCOL3,C'C'                                                      
         MVI   PRCOL4,C'D'         SUB BOXES GET A DASH                         
         MVI   PRCOL5,C'D'                                                      
         MVI   PRCOL6,C'D'                                                      
         MVI   PRRIGHT,C'R'                                                     
         SPACE 1                                                                
BOXX     XMOD1 1                                                                
*                                                                               
BOXRC    DC    A(0)                                                             
         LTORG                                                                  
         EJECT                                                                  
         BUFF  LINES=300,ROWS=1,COLUMNS=2,COMMENT=15,                  X        
               FLAVOR=PACKED,KEYLIST=(3,A)                                      
         EJECT                                                                  
         DS    0D                                                               
         DC    CL8'*GOBUFF*'                                                    
GOBUFF   DC    10000X'00'                                                       
*                                                                               
*              DSECT FOR PROGRAM                                                
*                                                                               
AC64D    DSECT                                                                  
PRELOC   DS    F                                                                
ADBUFC   DS    A                                                                
ADBOX    DS    A                                                                
SAVERE   DS    A                                                                
BILLCNT  DS    H                   COUNT OF BILLS ON JOB                        
BUFREC   DS    0CL34                                                            
BUFKEY   DS    0CL3                                                             
BUFTYPE  DS    CL1                                                              
BILLING  EQU   1                                                                
WORKCODE EQU   2                                                                
BUFWC    DS    CL2                                                              
         ORG   BUFWC                                                            
BUFSEQ   DS    CL2                 SAVED BILL SEQU NUMBER                       
BUFNUM   DS    CL6                                                              
         ORG   BUFNUM                                                           
BUFNAME  DS    CL15                                                             
BUFAC1   DS    PL8                                                              
BUFAC2   DS    PL8                                                              
FRST     DS    CL1                                                              
CLOSED   DS    CL1                                                              
CODE     DS    CL2                                                              
MEDIA    DS    CL1                                                              
PREVMED  DS    CL1                                                              
PLANWC   DS    CL2                                                              
ELCODE   DS    CL1                                                              
TABLE    DS    CL218               L=5,C=7                                      
PLINE    DS    0CL42                                                            
ORIGEST  DS    PL6                                                              
CURREST  DS    PL6                                                              
CURMNTH  DS    PL6                                                              
TODATE   DS    PL6                                                              
FINAL    DS    PL6                                                              
UNBILLED DS    PL6                                                              
XTRA     DS    PL6                                                              
*                                                                               
BILLAMNT DS    PL6                 NET OR GROSS AMOUNT OF BILL                  
MEDPOS   DS    CL1                                                              
TODAY    DS    CL3                                                              
START    DS    CL3                                                              
END      DS    CL3                                                              
START2   DS    CL2                                                              
END2     DS    CL2                                                              
CURRMTHP DS    CL6                 CURRENT MONTH FOR HEADER                     
ESTPRT   DS    CL7                 EA=Y, N, OR RN                               
PRTSW    DS    CL1                                                              
PRTSW2   DS    CL1                                                              
MEDSW    DS    CL1                                                              
SAVEN    DS    CL36                                                             
QOPT4SW  DS    CL1                                                              
INTEXTSW DS    CL1                                                              
DIV      DS    PL14                                                             
PL13     DS    PL13                                                             
OP4MSW   DS    C                                                                
SVMED    DS    CL12                                                             
SAVEMED  DS    CL12                                                             
NET      DS    PL6                                                              
GROSS    DS    PL6                                                              
WC       DS    CL2                                                              
*        GOBLOCK BUILT/MAINTAINED OFF OF RC                                     
*                                                                               
GOBLK    DS    0C                                                               
*                                                                               
       ++INCLUDE ACGOBLOCK                                                      
GOBLKLN  EQU   *-GOBLK                                                          
*                                                                               
*                                                                               
SAVEP    DS    CL440               4 PRINT LINES, 110 EACH                      
         EJECT                                                                  
PRINTD   DSECT                     DSECT TO COVER A PRINT LINE                  
PRLINE   DS    0CL110                                                           
PRLEFT   DS    CL1                                                              
PRNAME   DS    CL28                                                             
PRCOL1   DS    CL1                                                              
PRORIG   DS    CL11                                                             
PRCOL2   DS    CL1                                                              
PRCURR   DS    CL11                                                             
PRCOL3   DS    CL1                                                              
PRINVNO  DS    CL6                                                              
         DS    CL1                                                              
PRCOL4   DS    CL1                                                              
PRCURMTH DS    CL11                                                             
PRCOL5   DS    CL1                                                              
PRTODATE DS    CL11                                                             
PRCOL6   DS    CL1                                                              
PRFINAL  DS    CL11                                                             
PRCOL7   DS    CL1                                                              
PRUNBEST DS    CL11                                                             
PRRIGHT  DS    CL1                                                              
PRLEN    EQU   *-PRLINE                                                         
         EJECT                                                                  
* ACREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
* ACGENBOTH                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
* ACGENMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
* ACMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
* DDBIGBOX                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
* DDBOXEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDBOXEQUS                                                      
         PRINT ON                                                               
* DDREPXTRAD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDREPXTRAD                                                     
         PRINT ON                                                               
* DDREPMASTD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDREPMASTD                                                     
         PRINT ON                                                               
* ACJOBBERD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACJOBBERD                                                      
         PRINT ON                                                               
         EJECT                                                                  
JBLOCKD  DSECT                                                                  
       ++INCLUDE ACJOBBLOCK                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016ACREP6402 04/23/15'                                      
         END                                                                    
