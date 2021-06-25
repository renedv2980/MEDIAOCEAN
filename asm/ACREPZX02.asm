*          DATA SET ACREPZX02  AT LEVEL 086 AS OF 12/19/11                      
*PHASE ACZX02A                                                                  
*INCLUDE ACPAYCHK                                                               
*INCLUDE PRNTBL                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE CLPACK                                                                 
*INCLUDE CLUNPK                                                                 
*INCLUDE PUBVAL                                                                 
         TITLE 'CLEARANCE UPDATE FIX'                                           
ACZX02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACZX**,R9                                                    
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACZXD,RC                                                         
         L     R8,VBIGPRNT                                                      
         USING BIGPRNTD,R8                                                      
*                                                                               
         MVI   FCRESET,C'Y'                                                     
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,LEDGFRST                                                    
         BE    LDGF                                                             
         CLI   MODE,PROCACC                                                     
         BE    PACC                                                             
         CLI   MODE,PROCTRNS                                                    
         BE    PTRN                                                             
         CLI   MODE,ACCLAST                                                     
         BE    ACCL                                                             
         CLI   MODE,LEDGLAST                                                    
         BE    LDGL                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*********************************************************************           
* FIRST FOR RUN                                                     *           
*********************************************************************           
         SPACE 1                                                                
RUNF     DS    0H                                                               
         MVC   VTYPES(VTYPLNQ),ADCONS                                           
         ZAP   TOTCR,=P'0'                                                      
*                                                                               
         OPEN  (SPOTDATA,OUTPUT)   OPEN SEQUENTIAL DATA SET                     
         LTR   RF,RF               FOR OUTPUT                                   
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OPEN  (NETDATA,OUTPUT)    OPEN SEQUENTIAL DATA SET                     
         LTR   RF,RF               FOR OUTPUT                                   
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OPEN  (PRNTDATA,OUTPUT)   OPEN SEQUENTIAL DATA SET                     
         LTR   RF,RF               FOR OUTPUT                                   
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*              INITIALIZE ACPAYCHK BLOCK                                        
*                                                                               
         MVI   PCBACTN,PCBAACCF    PROCESS ACCOUNT FILE                         
         MVC   PCBCOMF,ADCOMFAC    A(COMFACS)                                   
         MVC   PCBCLPK,CLPACK      A(CLPACK)                                    
         MVC   PCBPUBV,PUBVAL      A(PUBVAL)                                    
*                                                                               
         L     RE,ADMASTC                                                       
         USING MASTD,RE                                                         
         MVC   PCBAUTL,MCUTL       A(UTL)                                       
         L     R4,MCBXAREA                                                      
         USING BOXD,R4                                                          
         MVC   BOXWIDTH,=F'198'    SET WIDTH FOR REPORT                         
         DROP  RE                                                               
*                                                                               
         LA    RE,IDTAB                                                         
         STCM  RE,15,PCBIDT        A(ID TABLE)                                  
         L     RF,=A(IDTABX)                                                    
         SR    RF,RE                                                            
         STCM  RF,3,PCBIDTL        LENGTH OF ID TABLE                           
         L     RE,ASEQTAB                                                       
         STCM  RE,15,PCBSQTB       A(SEQUENCE TABLE)                            
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*********************************************************************           
* FIRST FOR REQUEST                                                 *           
*********************************************************************           
         SPACE 1                                                                
REQF     DS    0H                                                               
         XC    START,START         CLEAR START DATE                             
         CLC   QSTART,SPACES       ANY START DATE?                              
         BE    REQF10                                                           
         GOTO1 DATCON,DMCB,(0,QSTART),(1,START)                                 
*                                                                               
REQF10   MVC   END,=X'FFFFFF'                                                   
         CLC   QEND,SPACES                                                      
         BE    REQFX                                                            
         GOTO1 DATCON,DMCB,(0,QEND),(1,END)                                     
*                                                                               
REQFX    B     EXIT                                                             
         EJECT                                                                  
*********************************************************************           
* FIRST FOR LEDGER                                                  *           
*********************************************************************           
         SPACE 1                                                                
LDGF     DS    0H                                                               
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVI   FCRDTRNS,C'Y'                                                    
         ZAP   LEGCR,=P'0'                                                      
         B     EXIT                                                             
         EJECT                                                                  
*********************************************************************           
* PROCACC                                                           *           
*********************************************************************           
         SPACE 1                                                                
PACC     DS    0H                                                               
         ZAP   ACCR,=P'0'                                                       
         MVI   ACTSW,C'N'                                                       
         B     EXIT                                                             
         EJECT                                                                  
*********************************************************************           
* PROCESS TRANSACTIONS                                              *           
*********************************************************************           
         SPACE 1                                                                
         USING TRNELD,R5                                                        
PTRN     DS    0H                                                               
         L     R5,ADTRANS                                                       
*                                                                               
         LR    R3,R5                                                            
         SH    R3,DATADISP                                                      
         USING TRNRECD,R3                                                       
*                                                                               
         CLC   TRNKUNT(2),=C'SC'   ARE WE DOING CASH?                           
         BE    PTRN10                                                           
         CLC   QAPPL(3),SPACES                                                  
         BE    PTRN10                                                           
         CLC   TRNKCACT+9(3),QAPPL IS THIS A REQUESTED CLIENT                   
         BNE   PTRNX                                                            
*                                                                               
PTRN10   MVC   DKEY,0(R3)                                                       
*                                                                               
         MVC   PKDTE(L'PKDTE+L'PKADTE),SPACES   CLEAR RECONCILED DATES          
*                                                                               
         LR    R4,R5                                                            
         MVI   ELCODE,TRSELQ       X'60' - TRANSACTION STATUS ELEM              
         BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING TRSELD,R4                                                        
*        TM    TRSMARK,TRSMBRQ     DONE BY MARKER?                              
*        BO    PTRNX               YES - SKIP                                   
         GOTO1 DATCON,DMCB,(2,TRSDATE),(1,PKADTE)                               
         DROP  R4                                                               
*                                                                               
         CLC   QSELECT,SPACES      ANY CHECK NUMBER FILTERING?                  
         BNH   PTRN25                                                           
         CLC   TRNKUNT(2),=C'SC'   ARE WE DOING CASH?                           
         BNE   PTRN20                                                           
         CLC   QSELECT,TRNKREF     SAME CHECK NUMBER?                           
         BNE   PTRNX               NO - SKIP TRANSACTION                        
         B     PTRN25                                                           
*                                                                               
PTRN20   LR    R4,R5                                                            
         MVI   ELCODE,MPYELQ       X'64' - MANUAL PAYMENT ELEMENT               
         BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING MPYELD,R4                                                        
         CLC   QSELECT,MPYNO       SAME CHECK NUMBER?                           
         BNE   PTRNX               NO - SKIP TRANSACTION                        
         DROP  R4                                                               
*                                                                               
PTRN25   LR    R4,R5                                                            
         MVI   ELCODE,GDAELQ       X'E5' - GENERAL DATE ELEMENT                 
PTRN30   BAS   RE,NEXTEL                                                        
         BNE   PTRN40              NO ELEMENT SKIP TRANSACTION                  
*                                                                               
         USING GDAELD,R4                                                        
         CLI   GDATYPE,GDATRECN    BANKREC GDADATE=DATE RECONCILED              
         BNE   PTRN30                                                           
         MVC   PKDTE,GDADATE       SAVE OFF RECONCILED DATE                     
         DROP  R4                                                               
*                                                                               
PTRN40   DS    0H                                                               
         LA    RE,TRNDATE                                                       
         CLI   QOPT2,C'Y'                                                       
         BNE   PTRN45                                                           
         CLC   PKDTE,SPACES        MAKE SURE DATE WAS FOUND                     
         BNH   PTRNX                                                            
         LA    RE,PKDTE                                                         
         B     PTRN50                                                           
PTRN45   CLI   QOPT2,C'A'          RUN BY ACTIVITY DATE?                        
         BNE   PTRN50                                                           
         CLC   PKADTE,SPACES       MAKE SURE DATE WAS FOUND                     
         BNH   PTRNX                                                            
         LA    RE,PKADTE                                                        
*                                                                               
PTRN50   CLC   0(L'PKDTE,RE),START  MAKE SURE WITHIN RANGE                      
         BL    PTRNX                                                            
         CLC   0(L'PKDTE,RE),END                                                
         BH    PTRNX                                                            
*                                                                               
         MVC   MSG,=CL10'TRNS  REC'                                             
         SR    R6,R6                                                            
         ICM   R6,3,TRNRLEN                                                     
         GOTO1 ADUMP,DMCB,(RC),(R3),(R6)                                        
*                                                                               
         CLI   QOPT1,C'D'          DEBITS OPTION                                
         BNE   PTRN60                                                           
         TM    TRNSTAT,TRNSDR      ONLY DEBITS                                  
         BZ    PTRNX                                                            
         LR    R4,R3                                                            
         MVI   ELCODE,MPYELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   PTRNX                                                            
         USING MPYELD,R4                                                        
         CLI   MPYLN,MPYLN2Q                                                    
         BL    PTRNX                                                            
         MVC   DKEY+(TRNKSBR-TRNKEY)(1),MPYSUB  SEQUENCE FOR CREDIT             
         B     PTRN70                                                           
         DROP  R4                                                               
*                                                                               
PTRN60   TM    TRNSTAT,TRNSDR      ONLY CREDITS                                 
         BO    PTRNX                                                            
*                                                                               
*                                  GET RECORD IN NEW FORMAT                     
PTRN70   GOTO1 DATAMGR,DMCB,DMREAD,ACCDIR,DKEY,DIR                              
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R3,DIR                                                           
         GOTO1 DATAMGR,DMCB,GETREC,ACCMST,TRNKDA,IO,DMWORK                      
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R3,IO                                                            
         STCM  R3,15,PCBINPUT                                                   
         LA    R5,TRNRFST                                                       
         CLI   0(R5),TRNELQ                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RE,ASEQTAB          INITIALIZE SEQUENCE TABLE                    
         MVI   0(RE),X'FF'                                                      
*                                                                               
         GOTO1 ACPAYCHK,PCB                                                     
         BNE   PTRNX                                                            
*                                                                               
         MVC   MSG,=CL10'PCB BLOCK'                                             
         GOTO1 ADUMP,DMCB,(RC),PCB,PCBLNQ                                       
*                                                                               
         USING PLINED,R6                                                        
         LA    R6,XP                                                            
*                                                                               
         LA    RE,PRTLNE                                                        
         LHI   RF,PRLNQ                                                         
         SR    R1,R1                                                            
         ICM   R1,8,SPACES         FILL PRINT LINE WITH SPACES                  
         MVCL  RE,R0                                                            
*                                                                               
         MVC   PLACC,TRNKACT                                                    
         MVC   PLOFC,TRNKOFF                                                    
         MVC   PLCON,TRNKCUNT                                                   
         GOTO1 DATCON,DMCB,(1,TRNDATE),(8,PLDTE)                                
         MVC   PLREF,TRNKREF                                                    
*        MVC   PLSEQ#,XNULLS       NO SEQ # ON ACCOUNT INFO                     
         EDIT  PCBSEQN,PLSEQ#,ZERO=NOBLANK                                      
         MVC   PLSTAPUB,XNULLS     NO PUB ON ACCOUNT INFO                       
         MVC   PLINV#,PCBINV#      INVOINCE #                                   
         MVC   PLPRO,PCBPRO                                                     
         MVC   PLPRO2,PCBPRO2                                                   
         EDIT  PCBEST,PLEST#,ZERO=NOBLANK                                       
         EDIT  TRNKSBR,PLSUB,FILL=0                                             
         EDIT  TRNAMNT,PLAMT,2,ZERO=NOBLANK,CR=YES,MINUS=YES                    
         GOTO1 DATCON,DMCB,(2,PCBCLRDT),(8,PLCLR)                               
         MVC   PLCHK,PCBCKNUM                                                   
         GOTO1 DATCON,DMCB,(2,PCBCKDTE),(8,PLCHKD)                              
         CLC   PCBBMKT,SPACES                                                   
         BNH   PTRN80                                                           
         EDIT  PCBBMKT,PLBMKT#,ZERO=NOBLANK                                     
PTRN80   CLC   PCBPDDTE,SPACES                                                  
         BNH   PTRN90                                                           
         GOTO1 DATCON,DMCB,(2,PCBPDDTE),(8,PLPDDT)                              
PTRN90   CLC   PKDTE,SPACES                                                     
         BNH   PTRN100                                                          
         GOTO1 DATCON,DMCB,(1,PKDTE),(8,PLRECDT)                                
*                                                                               
PTRN100  DS    0H                                                               
         GOTO1 ACREPORT                                                         
*                                                                               
         AP    ACCR,TRNAMNT                                                     
         MVI   ACTSW,C'Y'                                                       
*                                                                               
         TM    PCBTYPE,PCBTREC     IS THIS A CASH RECONCILIATION?               
         BO    *+12                                                             
         BAS   RE,SORTPUT                                                       
         B     PTRNX                                                            
*                                                                               
         USING SEQTBD,R4                                                        
         L     R4,ASEQTAB          INITIALIZE SEQUENCE TABLE                    
PTRN110  CLI   0(R4),X'FF'                                                      
         BE    PTRNX                                                            
*                                                                               
         MVC   MSG,=CL10'SEQ TABLE'                                             
         GOTO1 ADUMP,DMCB,(RC),(R4),SEQTLNQ                                     
*                                                                               
         EDIT  SEQNUM,PLSEQ#,ZERO=NOBLANK                                       
         EDIT  SEQAMNT,PLAMT,2,ZERO=NOBLANK,CR=YES,MINUS=YES                    
         GOTO1 DATCON,DMCB,(2,SEQCLRDT),(8,PLCLR)                               
*                                                                               
         MVC   PCBSEQN,SEQNUM      UPDATE SEQUENCE NUMBER                       
         MVC   PCBCLRDT,SEQCLRDT   UPDATE CLEAR DATE                            
         MVC   PCBINV#,SEQINV#     INVOICE NUMBER                               
         MVC   PCBPRO,SEQPRO       PRODUCT 1                                    
         MVC   PCBPRO2,SEQPRO2     PRODUCT 2                                    
         MVC   PCBEST,SEQEST       ESTIMATE #                                   
         MVC   PCBBMKT,SEQBMKT     BINARY MARKET NUMBER                         
         MVC   PLPRO,SEQPRO                                                     
         MVC   PLPRO2,SEQPRO2                                                   
         MVC   PLINV#,SEQINV#                                                   
         EDIT  SEQEST,PLEST#,ZERO=NOBLANK                                       
         EDIT  SEQBMKT,PLBMKT#,ZERO=NOBLANK                                     
         CLI   PCBKSYS,PCBKSYPR    ARE WE DOING PRINT?                          
         BNE   PTRN120                                                          
         GOTO1 HEXOUT,DMCB,SEQPUB,PLSTAPUB,L'SEQPUB,0,0                         
         MVC   PLCLI,SEQPCLI                                                    
         MVC   PCBKPUB,SEQPUB      PUB FOR PRINT                                
         MVC   PCBKPCLT,SEQPCLI    PRINT CLIENT                                 
         B     PTRN130                                                          
*                                                                               
PTRN120  DS    0H                                                               
         MVC   PLSTAPUB,SPACES                                                  
         MVC   PLSTAPUB(L'PCBKSTN),PCBKSTN                                      
         GOTO1 CLUNPK,DMCB,SEQSCLI,PLCLI                                        
         MVC   PCBKSTN,SEQSTA      STATION FOR SPOT/NET                         
         MVC   PCBKSCLT,SEQSCLI    SPOT CLIENT                                  
*                                                                               
PTRN130  DS    0H                                                               
         GOTO1 ACREPORT                                                         
*                                                                               
         BAS   RE,SORTPUT                                                       
         LA    R4,SEQTLNQ(R4)                                                   
         B     PTRN110                                                          
*                                                                               
PTRNX    B     EXIT                                                             
         DROP  R3,R5,R6                                                         
         EJECT                                                                  
*********************************************************************           
* ACCOUNT LAST                                                      *           
*********************************************************************           
         SPACE 1                                                                
ACCL     DS    0H                                                               
         CLI   ACTSW,C'Y'                                                       
         BNE   ACCLX                                                            
         AP    LEGCR,ACCR                                                       
         EDIT  ACCR,(15,XP+87),2,CR=YES                                         
         MVC   XP(13),=C'ACCOUNT TOTAL'                                         
         GOTO1 ACREPORT                                                         
*                                                                               
ACCLX    B     EXIT                                                             
         EJECT                                                                  
*********************************************************************           
* LAST FOR LEDGER                                                  *            
*********************************************************************           
         SPACE 1                                                                
LDGL     DS    0H                                                               
         AP    TOTCR,LEGCR                                                      
*                                                                               
         GOTO1 ACREPORT                                                         
*                                                                               
         L     R3,ADACC                                                         
         USING ACTRECD,R3                                                       
         MVC   XP(7),=C'LEDGER '                                                
         MVC   XP+8(1),ACTKLDG                                                  
         MVC   XP+10(6),=C'TOTAL '                                              
         EDIT  LEGCR,(15,XP+87),2,CR=YES                                        
         GOTO1 ACREPORT                                                         
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
*********************************************************************           
* RUNLAST                                                           *           
*********************************************************************           
         SPACE 1                                                                
RUNL     DS    0H                                                               
         GOTO1 ACREPORT                                                         
         MVC   XP+1(5),=C'TOTAL'                                                
         EDIT  TOTCR,(15,XP+87),2,CR=YES                                        
         GOTO1 ACREPORT                                                         
*                                                                               
         BAS   RE,PUTFILE          GET SORT RECORDS & PUT THEM TO FILE          
         CLI   SORTFRST,C'Y'                                                    
         BE    RUNL10                                                           
         GOTO1 ADSORTER,DMCB,=C'END'    END SORT                                
*                                                                               
RUNL10   CLOSE SPOTDATA            CLOSE SEQUENTIAL DATA SET                    
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLOSE NETDATA             CLOSE SEQUENTIAL DATA SET                    
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLOSE PRNTDATA            CLOSE SEQUENTIAL DATA SET                    
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         EDIT  COUNT,(12,XP+2),0,COMMAS=YES,ZERO=NOBLANK                        
         MVC   XP+15(30),=CL30'RECORDS'                                         
         GOTO1 ACREPORT                                                         
*                                                                               
         EDIT  SPOTCNT,(12,XP+2),0,COMMAS=YES,ZERO=NOBLANK                      
         MVC   XP+15(30),=CL30'SPOT RECORDS'                                    
         GOTO1 ACREPORT                                                         
*                                                                               
         EDIT  NETCNT,(12,XP+2),0,COMMAS=YES,ZERO=NOBLANK                       
         MVC   XP+15(30),=CL30'NET RECORDS'                                     
         GOTO1 ACREPORT                                                         
*                                                                               
         EDIT  PRNTCNT,(12,XP+2),0,COMMAS=YES,ZERO=NOBLANK                      
         MVC   XP+15(30),=CL30'PRINT RECORDS'                                   
         GOTO1 ACREPORT                                                         
*                                                                               
         EDIT  BADCNT,(12,XP+2),0,COMMAS=YES,ZERO=NOBLANK                       
         MVC   XP+15(30),=CL30'BAD RECORDS'                                     
         GOTO1 ACREPORT                                                         
*                                                                               
YES      CR    RB,RB                                                            
         B     EXIT                                                             
NO       LTR   RB,RB                                                            
         B     EXIT                                                             
         EJECT                                                                  
*********************************************************************           
* PUT RECORDS TO SORT                                               *           
*********************************************************************           
         SPACE 1                                                                
SORTPUT  NTR1                                                                   
         CLI   SORTFRST,C'Y'                                                    
         BNE   SORTPUT2                                                         
         GOTO1 ADSORTER,DMCB,SORTCARD,RECCARD                                   
         MVI   SORTFRST,C'N'                                                    
*                                                                               
SORTPUT2 GOTO1 ADSORTER,DMCB,=C'PUT',PCBKEY                                     
         LA    R1,SPOTCNT                                                       
         CLI   PCBKSYS,PCBKSYSP                                                 
         BE    SORTPUT5                                                         
         LA    R1,PRNTCNT                                                       
         CLI   PCBKSYS,PCBKSYPR                                                 
         BE    SORTPUT5                                                         
         LA    R1,NETCNT                                                        
*                                                                               
SORTPUT5 ICM   RF,15,0(R1)                                                      
         AHI   RF,1                                                             
         STCM  RF,15,0(R1)                                                      
         B     EXIT                                                             
         EJECT                                                                  
*********************************************************************           
* PUT RECORDS TO FILE                                               *           
*********************************************************************           
         SPACE 1                                                                
PUTFILE  NTR1                                                                   
         CLI   SORTFRST,C'Y'                                                    
         BE    EXIT                                                             
*                                                                               
PF10     GOTO1 ADSORTER,DMCB,=C'GET'                                            
         L     R2,DMCB+4                                                        
         LTR   R2,R2                                                            
         BZ    EXIT                                                             
*                                                                               
         MVC   PCBKEY(PCBDLNQ),0(R2)                                            
         MVC   MSG,=CL10'GET SRT'                                               
         GOTO1 ADUMP,DMCB,(RC),PCBKEY,PCBDLNQ                                   
*                                                                               
         LA    R3,SPOTDATA                                                      
         CLI   0(R2),C'S'                                                       
         BE    PF20                                                             
         LA    R3,NETDATA                                                       
         CLI   0(R2),C'N'                                                       
         BE    PF20                                                             
         LA    R3,PRNTDATA                                                      
         CLI   0(R2),C'P'                                                       
         BE    PF20                                                             
         DC    H'0'                                                             
*                                                                               
PF20     PUT   (R3),1(R2)          PUT RECORD TO SEQUENTIAL DATA SET            
         ICM   RF,15,COUNT                                                      
         AHI   RF,1                                                             
         STCM  RF,15,COUNT                                                      
         B     PF10                                                             
         EJECT                                                                  
*********************************************************************           
* GETELS                                                            *           
*********************************************************************           
         SPACE 1                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*********************************************************************           
* CONSTANTS AND LITERAL POOL                                        *           
*********************************************************************           
         SPACE 1                                                                
ADCONS   DS    0F                                                               
         DC    A(DUMP)             DUMP ROUTINE                                 
         DC    A(SEQTAB)           SEQUENCE TABLE                               
*                                                                               
         DC    V(ACPAYCHK)                                                      
         DC    V(CLPACK)                                                        
         DC    V(CLUNPK)                                                        
         DC    V(PUBVAL)                                                        
         DC    V(PRNTBL)                                                        
         DC    V(HELLO)                                                         
*                                                                               
ACCDIR   DC    CL8'ACCDIR  '                                                    
ACCMST   DC    CL8'ACCMST  '                                                    
GETREC   DC    CL8'GETREC  '                                                    
*                                                                               
XNULLS   DC    12C'X'                                                           
*                                                                               
SORTFRST DC    C'Y'                                                             
SORTCARD DC    CL80'SORT FIELDS=(1,14,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=(50)'                                  
*                                                                               
TNCR     DC    PL6'0'                                                           
*                                                                               
ACCR     DC    PL6'0'                                                           
*                                                                               
LEGCR    DC    PL6'0'                                                           
*                                                                               
TOTCR    DC    PL6'0'                                                           
*                                                                               
SPOTDATA DCB   DDNAME=SPOTDATA,DSORG=PS,MACRF=PM,                      X        
               RECFM=FB,LRECL=PCBBLKQ,BLKSIZE=PCBBLKQ*10                        
*                                                                               
NETDATA  DCB   DDNAME=NETDATA,DSORG=PS,MACRF=PM,                       X        
               RECFM=FB,LRECL=PCBBLKQ,BLKSIZE=PCBBLKQ*10                        
*                                                                               
PRNTDATA DCB   DDNAME=PRNTDATA,DSORG=PS,MACRF=PM,                      X        
               RECFM=FB,LRECL=PCBBLKQ,BLKSIZE=PCBBLKQ*10                        
         EJECT                                                                  
*********************************************************************           
* LITERALS                                                          *           
*********************************************************************           
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
* TABLES                                                            *           
*********************************************************************           
         SPACE 1                                                                
         DS    0D                                                               
*                                                                               
* TABLE 1 - ID TABLE                                                            
*                                                                               
         DC    CL8'*IDTAB**'                                                    
IDTAB    DS    1000CL(IDLNQ)      ALPHA ID/SE # TABLE                           
IDTABX   EQU   *                                                                
*                                                                               
* TABLE 2 - PAYABLE SEQUENCE NUMBER TABLE                                       
*                                                                               
         DC    CL8'*SEQTAB*'       SEQUENCE TABLES                              
SEQTAB   DS    XL(SEQTLNQ*SEQTMAX) SEQ NUMBER AND AMOUNT                        
         EJECT                                                                  
***********************************************************************         
* DUMP RECORDS                                                        *         
***********************************************************************         
         SPACE 1                                                                
DUMP     NMOD1 0,**DMP**                                                        
         L     RC,0(R1)                                                         
         CLI   QOPT7,C'Y'                                                       
         BNE   DUMPX                                                            
         LA    R0,L'MSG                                                         
         LA    R2,MSG                                                           
         L     R3,4(R1)                                                         
         L     R4,8(R1)                                                         
*                                                                               
         LA    R5,=C'2D'                                                        
         GOTO1 PRNTBL,DMCB,((R0),(R2)),(R3),C'DUMP',(R4),(R5),         X        
               (C'P',PRINT)                                                     
*                                                                               
DUMPX    XIT1                                                                   
*        MVC   MSG,=CL10'TRNS  REC'                                             
*        GOTO1 ADUMP,DMCB,(RC),(R2),(R6)                                        
         EJECT                                                                  
*********************************************************************           
* LITERALS                                                          *           
*********************************************************************           
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
* STORAGE                                                           *           
*********************************************************************           
         SPACE 1                                                                
ACZXD    DSECT                                                                  
       ++INCLUDE ACPAYCHKD                                                      
         EJECT                                                                  
VTYPES   DS    0F                                                               
ADUMP    DS    A                   DUMP ROUTINE                                 
ASEQTAB  DS    A                   SEQUENCE TABLE                               
*                                                                               
ACPAYCHK DS    V                                                                
CLPACK   DS    V                                                                
CLUNPK   DS    V                                                                
PUBVAL   DS    V                                                                
PRNTBL   DS    V                                                                
HELLO    DS    V                                                                
VTYPLNQ  EQU   *-VTYPES                                                         
*                                                                               
ACTSW    DS    CL1                                                              
*                                                                               
COUNT    DS    F                   COUNT OF OUTPUT RECORDS                      
SPOTCNT  DS    F                   COUNTER OF SPOT RECORDS                      
NETCNT   DS    F                   NET                                          
PRNTCNT  DS    F                   PRINT                                        
BADCNT   DS    F                   BAD RECORDS                                  
*                                                                               
MSG      DS    CL10                MESSAGE FOR DUMP                             
*                                                                               
START    DS    PL3                 SAVED AREA FOR START DATE                    
END      DS    PL3                 SAVED AREA FOR END DATE                      
PKDTE    DS    PL3                 SAVED AREA FOR GDADATE DATE                  
PKADTE   DS    PL3                 SAVED AREA FOR TRSDATE DATE                  
*                                                                               
ELCODE   DS    XL1                                                              
DKEY     DS    XL42                                                             
DIR      DS    XL60                                                             
IO       DS    XL2004                                                           
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER PRINT LINE                                           *         
***********************************************************************         
                                                                                
PLINED   DSECT                                                                  
PRTLNE   DS    0C                                                               
         DS    C                                                                
PLACC    DS    CL12                ACCOUNT                                      
         DS    C                                                                
PLOFC    DS    CL2                 OFFICE                                       
         DS    CL2                                                              
PLCON    DS    CL14                CONTRA                                       
         DS    C                                                                
PLDTE    DS    CL8                 DATE                                         
         DS    C                                                                
PLREF    DS    CL6                 REFERENCE                                    
         DS    C                                                                
PLSUB    DS    CL3                 SUB REFERENCE                                
         DS    C                                                                
PLSEQ#   DS    CL3                 SEQUENCE NUMBER WHEN MORE THAN 1             
         DS    C                                                                
PLCLI    DS    CL3                 CLIENT                                       
         DS    C                                                                
PLPRO    DS    CL3                 PRODUCT                                      
         DS    C                                                                
PLPRO2   DS    CL3                 PRODUCT 2(IF ANY)                            
         DS    C                                                                
PLINV#   DS    CL12                INVOICE #                                    
         DS    C                                                                
PLEST#   DS    CL5                 ESTIMATE #                                   
         DS    C                                                                
PLBMKT#  DS    CL5                 BINARY MARKET NUMBER                         
         DS    C                                                                
PLAMT    DS    CL13                AMOUNT                                       
         DS    C                                                                
PLCLR    DS    CL8                 CLEARANCE DATE                               
         DS    C                                                                
PLSTAPUB DS    CL12                PUB(PRINT) OR STATION(SPOT/NET)              
         DS    C                                                                
PLCHK    DS    CL6                 CHECK NUMBER                                 
         DS    C                                                                
PLCHKD   DS    CL8                 CHECK DATE                                   
         DS    C                                                                
PLPDDT   DS    CL8                 PAID DATE (CLEARED)                          
         DS    C                                                                
PLRECDT  DS    CL8                 RECONCILED DATE                              
         DS    C                                                                
PRLNQ    EQU   *-PRTLNE                                                         
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER ID TABLE                                             *         
***********************************************************************         
         SPACE 1                                                                
IDD      DSECT                     ALPHA ID/SE # TABLE DSECT                    
IDALPH   DS    CL2                 ALPHA ID                                     
IDCTRY   DS    CL1                 COUNTRY                                      
IDSPSE   DS    XL1                 SPOT SE NUMBER                               
IDNESE   DS    XL1                 NET SE NUMBER                                
IDPRSE   DS    XL1                 PRINT SE NUMBER                              
IDACSE   DS    XL1                 ACC SE NUMBER                                
IDLNQ    EQU   *-IDD                                                            
         EJECT                                                                  
***********************************************************************         
* INCLUDES                                                            *         
***********************************************************************         
         SPACE 1                                                                
*  ACPAYSEQD                                                                    
       ++INCLUDE ACPAYSEQD                                                      
       EJECT                                                                    
*  ACGENFILE                                                                    
*  ACGENMODES                                                                   
*  ACMASTD                                                                      
*  ACREPWORKD                                                                   
*  CTGENFILE                                                                    
*  DDMASTD                                                                      
*  DDBIGBOX                                                                     
*  SPSTAPACKD                                                                   
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACMASTD                                                        
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE ACBIGPRNTD                                                     
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE SPSTAPACKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'086ACREPZX02 12/19/11'                                      
         END                                                                    
