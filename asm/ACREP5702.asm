*          DATA SET ACREP5702  AT LEVEL 027 AS OF 09/01/17                      
*PHASE AC5702A,*                                                                
         TITLE 'BANK RECONCILIATION'                                            
**********************************************************************          
* PROGRAM OPTIONS :                                                  *          
*                                                                    *          
*         QOPT1 - 'A'  RUN ALL                                       *          
*                 'C'  RUN CLEARED ONLY                              *          
*                 'U'  RUN UNCLEARED ONLY                            *          
*         QOPT2 - 'Y'  RUN OUTPUT TAPE                               *          
*         QOPT3 - 'D'  RUN DEBITS ONLY                               *          
*         QOPT4 - 'Y'  IGNORE VOIDS                                  *          
*         QOPT5 - 'V'  VOID CHECK TAPE                               *          
*                 'N'  NON-VOID CHECK TAPE                           *          
*         QOPT6 - 'A'  RUN BY ACTIVITY DATE (POS PAY P/O REQUEST)    *          
*                 'N'  DO NOT RUN POS PAY OPTION (ONLY FOR SYSTEMS)  *          
*         QOPT7 - 'P'  PRINT POWWOW JCL (ONLY FOR SYSTEMS)           *          
*                 'N'  NON-POSPAY RUN REQUESTED                      *          
**********************************************************************          
*                                                                    *          
* MNAS 10/21/16 SPEC-6777 ADD PROFILE TO SUPPRESS MEDIA SUMMARY      *          
*                                                                    *          
* ABID 08/24/17 SPEC-13852 SAVE TRANSMISSION KEY  FOR SFTP PROCESS   *          
*                                                                    *          
**********************************************************************          
         SPACE 1                                                                
AC5702   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**AC57**,R8                                                    
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING AC57D,RC                                                         
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,PROCACC                                                     
         BE    PACC                                                             
         CLI   MODE,PROCTRNS                                                    
         BE    PTRN                                                             
         CLI   MODE,REQLAST                                                     
         BE    REQL                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* RUN FIRST                                                          *          
**********************************************************************          
         SPACE 1                                                                
RUNF     DS    0H                                                               
         MVC   ABKBUF,=A(BKBUF)    SETUP ADDRESSABILITY TO BKBUF                
         MVC   AACGTBNK,=V(ACGTBNK)                        ACGTBNK              
         MVC   ASPCBLK,=A(SPCBLK)                          SPEC BLOCK           
         BRAS  RE,LOAD             LOAD AC5703 PHASE                            
         J     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* REQUEST FIRST                                                      *          
**********************************************************************          
         SPACE 1                                                                
REQF     DS    0H                                                               
         LA    R1,TOTALS           INITALIZE TOTAL ACCUMULATORS                 
         LA    R0,TOTALQ                                                        
         ZAP   0(L'TOTALS,R1),=P'0'                                             
         LA    R1,L'TOTALS(R1)                                                  
         BCT   R0,*-10                                                          
*                                                                               
         USING BKD,R7                                                           
         L     R7,ABKBUF                                                        
         ZAP   BKCD,=P'0'                                                       
         ZAP   BKNETCD,=P'0'                                                    
         DROP  R7                                                               
*                                                                               
         MVI   FRSTSW,0                                                         
         MVI   FRSTSRT,C'Y'                                                     
         MVI   FORCEHED,C'Y'                                                    
         CLI   QOPT1,C'A'          CHECK IF REQUEST IS 'ALL'                    
         BH    *+8                                                              
         MVI   RCSUBPRG,1                                                       
         CLI   QOPT1,C'C'          CHECK IF REQUEST IS FOR CLEARED ONLY         
         BNE   *+8                                                              
         MVI   RCSUBPRG,2                                                       
         CLI   QOPT1,C'U'          CHECK IF REQUEST IS UNCLEARED ONLY           
         BNE   *+8                                                              
         MVI   RCSUBPRG,3                                                       
         GOTO1 ADSORTER,DMCB,SORTCARD,RECCARD,0                                 
         GOTO1 DATCON,DMCB,(0,QEND),(8,STDTE)                                   
*                                                                               
         L     RF,ADMASTC                                                       
         USING MASTD,RF                                                         
         TM    MCAGCOPT,MCAGCUAT   BEING RUN OFF A UAT FILE?                    
         BZ    *+12                                                             
         MVI   QOPT6,C'N'                                                       
         MVI   QOPT8,C'N'                                                       
         DROP  RF                                                               
*                                                                               
REQFX    J     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* PROCESS ACCOUNT                                                    *          
**********************************************************************          
         SPACE 1                                                                
PACC     DS    0H                                                               
         L     R2,ADACC            R2 = A(ACCOUNT RECORD)                       
         MVC   STACCNO(12),3(R2)   MOVE IN THE ACCOUNT#                         
         L     R2,ADACCNAM         R2 = A(ACCOUNT NAME RECORD)                  
         USING NAMELD,R2                                                        
         XC    STACCNM(36),STACCNM CLEAR NAME FIELD                             
         SR    R1,R1                                                            
         IC    R1,NAMLN            INSERT ELEMENT LENGTH INTO R1                
         SH    R1,=Y(NAMLN1Q+1)    SUBTRACT OVERHEAD + 1 FOR EX                 
         BNM   *+6                                                              
         DC    H'0'                ELEMENT HAS NO NAME IN NAME FIELD            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   STACCNM(0),NAMEREC  MOVE NAME INTO STORAGE                       
*                                                                               
         CLC   QEND(6),SPACES      SET UP WORK DATE                             
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   QSTART(6),SPACES                                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATCON,DMCB,(0,QSTART),(1,WKSTRT)   PACKING DATES                
         GOTO1 DATCON,DMCB,(0,QEND),(1,WKDTE)                                   
*                                                                               
PACCX    J     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
**********************************************************************          
* PROCESS ACCOUNT                                                    *          
**********************************************************************          
         SPACE 1                                                                
PTRN     DS    0H                                                               
         USING BKD,R7                                                           
         L     R7,ABKBUF           R7 = A(BANK ENTRY TABLE)                     
         USING TRNELD,R2                                                        
         L     R2,ADTRANS          R2 = A(TRANSACTION ELEMENT)                  
         LR    R4,R2               SAVE ADDRESS OF TRANSACTION ELEMENT          
*                                                                               
         XC    BKDTYP,BKDTYP       CLEAR DATE TYPE                              
*                                                                               
         CLI   TRNEL,TRNELQ        X'44' - TRANSACTION ELEMENT                  
         BNE   PTRNX                                                            
*                                                                               
         CLI   PROGPROF+6,C'Y'     OPTION TO SUPRESS TYPE 45'S                  
         BNE   PTRN10              NO                                           
         CLI   TRNTYPE,45                                                       
         BE    PTRNX                                                            
*                                                                               
PTRN10   CLI   QOPT5,C' '          VOIDS/NON VOIDS RUN                          
         BE    PTRN25              NO - CONTINUE                                
         CP    TRNAMNT,=P'0'                                                    
         BNH   PTRN20              IF NOT POSITIVE, ITS A VOID                  
*                                                                               
         CLI   QOPT5,C'N'         DO THEY WANT NON-VOIDS                        
         BE    PTRN25             THEY GOT EM                                   
         B     PTRNX              NON VOIDS ONLY SO REJECT                      
PTRN20   CLI   QOPT5,C'V'         DO THEY WANT VOIDS                            
         BE    PTRN25             THEY GOT EM                                   
         B     PTRNX              NON VOIDS ONLY SO REJECT                      
*                                                                               
PTRN25   CLI   PROGPROF+2,C'Y'     IGNORE REVERSALS (SEE BELOW)                 
         BNE   *+12                                                             
         TM    TRNSTAT,X'20'       IGNORE REVERSALS                             
         BO    PTRNX                                                            
         CLI   QOPT3,C'D'          DEBIT OPTION                                 
         BNE   PTRN30                                                           
         TM    TRNSTAT,X'80'                                                    
         BZ    PTRNX               TAKE ONLY DEBITS                             
         B     PTRN35                                                           
*                                  STATUS = CREDIT.                             
PTRN30   TM    TRNSTAT,X'80'                                                    
         BNZ   PTRNX               NO.                                          
         CLI   QOPT4,C'Y'          OPTION TO IGNORE VOIDS                       
         BNE   PTRN35                                                           
         L     R3,ADSUBAC                                                       
         USING CACELD,R3                                                        
         CLC   CACCNTA(7),=C'***VOID'                                           
         BE    PTRNX                                                            
PTRN35   LR    RF,R4               TAKE UNPEELED ITEMS ONLY.                    
         SH    RF,DATADISP         RF = A(TRANSACTION KEY).                     
         OC    ACCOPEEL(ACCOPLEN,RF),ACCOPEEL(RF)                               
         BNZ   PTRNX               ITEM WAS PEELED.                             
*                                                                               
         CLI   PROGPROF+7,C'Y'     DO THEY WANT TO EXCLUDE EFT?                 
         BNE   PTRN38                                                           
         USING CEDELD,R4                                                        
         L     R4,ADTRANS                                                       
         MVI   ELCODE,CEDELQ       CHECK EXTRA DETAIL ELEM                      
         BAS   RE,NEXTEL                                                        
         BNE   PTRN38                                                           
         TM    CEDBYTE,CEDEFT                                                   
         BO    PTRNX               POSTING IS FROM EFT                          
*                                                                               
         TM    CEDBYTE,CEDPCRD                                                  
         BO    PTRNX               POSTING IS FROM PCARD PAYMENT                
*                                                                               
PTRN38   CLI   QOPT1,C'A'                                                       
         BH    PTRN40                                                           
         TM    TRNSTAT,X'02'                                                    
         BZ    PTRN50                                                           
         MVI   BKCLEAR,C'C'        SHOW CLEARED WITH A 'C'                      
         B     PTRN60                                                           
*                                                                               
PTRN40   CLI   QOPT1,C'C'                                                       
         BNE   PTRN45              TAKE UNCLEARED ONLY.                         
         TM    TRNSTAT,X'02'       TAKE CLEARED ONLY.                           
         BZ    PTRNX               NOT CLEARED.                                 
         B     PTRN50              CLEARED                                      
PTRN45   TM    TRNSTAT,X'02'       TAKE UNCLEARED ONLY.                         
         BO    PTRNX                                                            
PTRN50   MVI   BKCLEAR,X'40'       TURN OFF CLEAR IND (FOR OPT A ONLY)          
*                                  WITHIN DATE RANGE                            
         USING TRSELD,R4                                                        
PTRN60   DS    0H                                                               
         L     R4,ADTRANS                                                       
         XC    BKACTDTE,BKACTDTE                                                
         MVI   ELCODE,X'60'        GET DATE ADDED TO FILE                       
         BAS   RE,NEXTEL                                                        
         BNE   PTRN65                                                           
         GOTO1 DATCON,DMCB,(2,TRSDATE),(1,BKACTDTE)                             
*                                                                               
PTRN65   CLI   TRNTYPE,37          FOR TYPE 37                                  
         BE    PTRN80              USE DATE ADDED TO FILE                       
         CLI   TRNTYPE,52          SAME FOR TYPE 52'S                           
         BE    PTRN90              BUT DON'T EVEN CHECK TRNSDATE                
*                                                                               
*        TM    FLAG,FLGPOS         IS THIS ACCOUNT A POS PAY ACCOUNT?           
*        BO    PTRN70                YES - USE ACTIVITY DATE                    
*        CLI   QOPT6,C'A'          WAS ACTIVITY DATE RUN REQUESTED?             
*        BE    PTRN70                YES - USE ACTIVITY DATE                    
*                                                                               
*        B     PTRN70                                                           
         CLC   TRNDATE(3),WKSTRT                                                
         BL    PTRN70                                                           
         CLC   TRNDATE(3),WKDTE                                                 
         BH    PTRN70                                                           
         OI    BKDTYP,BKDTRN       TRANSACTION DATE FITS RANGE                  
*                                                                               
         USING TRSELD,R4                                                        
PTRN70   L     R4,ADTRANS          RESET R4                                     
         MVI   ELCODE,X'60'        GET DATE ADDED TO FILE                       
         BAS   RE,NEXTEL                                                        
         BNE   PTRNX                                                            
         GOTO1 DATCON,DMCB,(2,TRSDATE),(1,WORK)                                 
         CLC   WORK(3),WKSTRT                                                   
         BL    PTRN75                                                           
         CLC   WORK(3),WKDTE                                                    
         BH    PTRN75                                                           
         OI    BKDTYP,BKDACT       ACTIVITY DATE FITS RANGE                     
PTRN75   OC    BKDTYP,BKDTYP       AT LEAST ONE DATE MUST BE IN RANGE           
         BZ    PTRNX                                                            
         CLI   QOPT2,C'Y'          OUTPUT TAPE REQUESTED?                       
         BE    PTRN120               YES - MOVE ON                              
         CLI   QOPT6,C'A'          WAS ACTIVITY DATE RUN REQUESTED?             
         BE    *+12                  YES - CHECK ACTIVITY IN RANGE              
         TM    BKDTYP,BKDTRN         NO - CHECK TRANSACTION                     
         BNO   PTRNX                                                            
         B     PTRN120                                                          
         TM    BKDTYP,BKDACT                                                    
         BNO   PTRNX                                                            
         B     PTRN120                                                          
*                                                                               
PTRN80   CLC   TRNDATE(3),WKSTRT                                                
         BL    PTRN110             TYPE 37 NOT WITHIN REQUEST PERIOD            
         CLC   TRNDATE(3),WKDTE                                                 
         BNH   PTRN90                                                           
*                                                                               
         CLI   PROGPROF+5,C'N'     DO THEY WANT TO EXCLUDE POSTDATED 37         
         BNE   PTRN110             NO, CONTINUE W/ DA DATE CHECK                
         B     PTRNX               YES, REJECT THIS CHECK                       
*                                                                               
*        FILTER 37'S WITHIN TRANSACTION DATE RANGE                              
*                                                                               
PTRN90   L     R4,ADTRANS          RESET R4                                     
         MVI   ELCODE,X'60'        GET DATE ADDED TO FILE                       
         BAS   RE,NEXTEL                                                        
         BNE   PTRNX                                                            
         USING TRSELD,R4                                                        
         GOTO1 DATCON,DMCB,(2,TRSDATE),(1,WORK)                                 
         CLC   WORK(3),WKSTRT      INCLUDE VOIDS POSTED THIS PERIOD             
         BNL   PTRN100             OK ON START, CHECK END                       
*                                                                               
         CLI   PROGPROF+5,C'N'     DO THEY WANT TO EXCLUDE POSTDATED 37         
         BNE   PTRNX               NO, REJECT THIS IN START                     
         OI    BKDTYP,BKDACT+BKDTRN                                             
         B     PTRN120             YES, ACCEPT A 37 IF T-DATES ARE OK           
*                                  EVEN IF DA-DATES FAIL                        
*                                                                               
PTRN100  CLC   WORK(3),WKDTE                                                    
         BH    PTRNX               IF NOT EXCLUDE                               
         OI    BKDTYP,BKDACT+BKDTRN                                             
         B     PTRN120                                                          
         DROP  R2                                                               
*                                                                               
*        FILTER 37'S OUTSIDE OF TRANSACTION DATE RANGE                          
*                                                                               
PTRN110  L     R4,ADTRANS                                                       
         MVI   ELCODE,X'60'        GET DATE ADDED TO FILE                       
         BAS   RE,NEXTEL                                                        
         BNE   PTRNX                                                            
         USING TRSELD,R4                                                        
         GOTO1 DATCON,DMCB,(2,TRSDATE),(1,WORK)                                 
         CLC   WORK(3),WKSTRT      INCLUDE VOIDS POSTED THIS PERIOD             
         BL    PTRNX                                                            
         CLC   WORK(3),WKDTE                                                    
         BH    PTRNX                                                            
*                                                                               
         OI    BKDTYP,BKDACT+BKDTRN                                             
         CLI   PROGPROF+4,C'Y'     SHOW 37'S WITHOUT OFFSET'S                   
         BE    PTRN120             YES                                          
*                                                                               
         USING TRNELD,R4                                                        
         L     R4,ADTRANS          SHOW 37'S WHICH DON'T HAVE AN OFFSET         
         ZAP   TRNAMNT,=P'0'       AS ZERO                                      
*                                                                               
PTRN120  L     R4,ADTRANS                                                       
         MVC   BKCHECK(6),TRNREF        CHECK NO                                
         GOTO1 DATCON,DMCB,(1,TRNDATE),(8,BKCKDTE)                              
*                                                                               
         LR    RF,R4               MAINTAIN FILE SEQ FOR A CHECK                
         SH    RF,DATADISP         RF = A(TRANSACTION KEY).                     
         USING TRNRECD,RF                                                       
         MVC   BKTRNSEQ,TRNKSBR    SUB REFERENCE NUMBER                         
*                                                                               
         ZAP   BKNET(6),TRNAMNT(6)     CHECK AMOUNT                             
         ZAP   BKNETCD(6),TRNAMNT(6)                                            
         ZAP   BKCD,=P'0'          CLEAR CASH DISCOUNT                          
*                                                                               
         CLI   QOPT5,C'N'          TALENT NON-VOID RUN                          
         BNE   PTRN130             NO                                           
         CP    BKNET,=P'0'         NO NEG AMOUNTS ON THIS TAPE                  
         BNL   PTRN130                                                          
         ZAP   BKNET,=P'0'                                                      
         ZAP   BKNETCD,=P'0'                                                    
*                                                                               
PTRN130  MVC   BKTRNDTE,TRNDATE        CK DATE YMD PACKED                       
         MVC   BKTRNTYP,TRNTYPE        INPUT TYPE                               
         MVC   BKTRNSTA,TRNSTAT        TRANSACTION STATUS BITS                  
         MVC   BKTRNANL,TRNANAL        TRANSACTION ANALYSIS CODE                
*                                                                               
         L     R4,ADTRANS                                                       
         MVI   ELCODE,X'50'                                                     
         BAS   RE,NEXTEL                GET SUB. CASH ELEMENT                   
         BNE   PTRN140                                                          
         USING SCIELD,R4                                                        
         CLI   SCITYPE,C'D'            TEST IF CASH DISCOUNT                    
         BNE   PTRN140                                                          
         ZAP   BKCD(6),SCIAMNT(6)                                               
         AP    BKNETCD(6),BKCD                                                  
PTRN140  MVC   BKDATE(6),QEND           REPORT PERIOD                           
         MVC   BKBANK(12),STACCNO       BANK ACCOUNT NO                         
         MVC   BKBNKNM(36),STACCNM      BANK ACCOUNT NAME                       
         L     R3,ADSUBAC          ACTIVE USING STILL CACELD FOR R3             
*                                                                               
         MVC   BKCULM(4),CACCNT    COMPANY,UNIT,LEDG,MEDIA                      
         MVC   BKPAYEE,SPACES                                                   
         SR    R1,R1               PAYEE NAME                                   
         IC    R1,1(R3)                                                         
         SH    R1,=Y(CACLN1Q+1)    SUBTRACT OVERHEAD                            
         BM    PTRN150             NO NAME IN SUB ACC                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BKPAYEE(0),CACNAME                                               
*                                                                               
PTRN150  MVC   BKACCNT(14),CACCNTU      PAYEE NO                                
         MVC   BKORULA,CACCNTU          SAVED PAYEE NO                          
         MVC   BKREQST(12),QUESTOR      REQUESTOR NAME                          
*                                                                               
         CP    BKNET,=P'0'         IF  ZERO  -  ITS A VOID                      
         BNE   PTRN160                                                          
         MVC   BKACCNT,=C'  ***VOID***  '   VENDOR ACCT TO VOID                 
         MVC   BKPAYEE,SPACES               CLEAR VENDOR NAME                   
         ZAP   BKCD,=P'0'                                                       
         ZAP   BKNETCD,=P'0'                                                    
*                                                                               
PTRN160  GOTO1 ADSORTER,DMCB,=C'PUT',ABKBUF                                     
*                                                                               
PTRNX    J     EXIT                                                             
         DROP  R3,R4,R7                                                         
         EJECT                                                                  
**********************************************************************          
* REQUEST LAST                                                       *          
**********************************************************************          
         SPACE 1                                                                
REQL     DS    0H                                                               
         USING BKD,R7                                                           
         L     R7,ABKBUF           R7 = A(BANK ENTRY TABLE)                     
         LR    RE,R7               CLEAR BKBUF TO BINARY ZEROS                  
         LA    RF,BKLNQ1                                                        
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         MVC   SVFRMT,SPACES                                                    
         XC    FLAG2,FLAG2                                                      
*                                                                               
REQL10   GOTO1 ADSORTER,DMCB,=C'GET'  GET A RECORD FROM SORTER                  
         L     R2,DMCB+4                                                        
         LTR   R2,R2                                                            
         BNZ   REQL20                                                           
         OC    0(BKLNQ,R7),0(R7)   IS THERE AN ENTRY?                           
         BZ    REQL110             NO -REQUEST GENERATED NOTHING                
         MVI   FRSTSRT,C'E'        END OF FILE                                  
         B     REQL60                                                           
*                                                                               
REQL20   CLI   FRSTSRT,C'Y'        IS THIS 1ST SORTED REC                       
         BNE   REQL30              NO - GO DO COMPARE                           
         MVI   FRSTSRT,C'N'        SET FIRST TIME TO NO                         
         MVC   0(BKLNQ,R7),0(R2)   SAVE THE FIRST RECORD                        
*                                                                               
         BRAS  RE,BLDSPEC                                                       
         BNE   *+8                                                              
         OI    FLAG,FLGREC                                                      
         BRAS  RE,OVERPROF         OVERRIDE PROFILE VALUES BY ACCOUNT           
*                                  IF NECESSARY                                 
         CLC   SVADVID,=C'POWSKIP'    DSDT-7 : SUPRESS POWWOW JOB               
         BNE   *+8                                                              
         MVI   QOPT8,C'N'                                                       
*                                                                               
         MVI   MYBYTE,1                                                         
         GOTO1 APHASE2,DMCB,(RA),(RC)  OPEN RECONCILE TAPE                      
         B     REQL10              GO GET THE NEXT RECORD TO COMPARE            
*                                                                               
REQL30   CLC   BKCHECK(14),BKCHECK-BKD(R2)      NUM AND DTE AS PREV             
         BNE   REQL60              NO - PROCESSED THE SAVED RECORD              
*                                                                               
         CLI   BKTRNTYP-BKD(R2),X'25'        IS IT A 37 VOID TYPE               
         BNE   REQL50                        NO                                 
*                                                                               
         CLI   PROGPROF+3,C'Y'     37'S AS VOIDS PROFILE OPTION                 
         BNE   REQL60              NO, OUTPUT CHECKS SEPARTLY                   
*                                                                               
         CLI   BKTRNTYP,X'25'      IS SAVED REC 37 VOID TYPE                    
         BE    REQL60              YES, OUTPUT SEPARTLY                         
*                                                                               
REQL40   ZAP   DUB,BKNET                     SAVED CK AMT INTO DUB              
         AP    DUB,BKNET-BKD(L'BKNET,R2)                                        
         BNZ   REQL60                        NO - PROCESS EACH INDIV.           
*                                                                               
         MVC   BKACCNT,=C'  ***VOID***  '    VENDOR ACCT TO VOID                
         MVC   BKPAYEE,SPACES                CLEAR VENDOR NAME                  
         ZAP   BKNET,=P'0'                   CLEAR TO ZERO , ITS A VOID         
         ZAP   BKCD,=P'0'                                                       
         ZAP   BKNETCD,=P'0'                                                    
         B     REQL10                                                           
*                                                                               
REQL50   EQU   *                            SAME CHK NO AND CHK DATE            
         CLC   BKACCNT(L'BKACCNT),BKACCNT-BKD(R2) SAME PAYEE                    
         BNE   REQL60                                 NO                        
         CLI   BKTRNTYP,X'25'      IS SAVED REC 37 VOID TYPE                    
         BE    REQL60              YES, OUTPUT SEPARTLY                         
         AP    BKNET,BKNET-BKD(L'BKNET,R2)       SUM THESE TRANSACTIONS         
         AP    BKCD,BKCD-BKD(L'BKCD,R2)                                         
         AP    BKNETCD,BKNETCD-BKD(L'BKNETCD,R2)                                
         B     REQL10                            AND GET NEXT                   
*                                                                               
REQL60   BRAS  RE,GETCACNT                                                      
         CLC   BKBANK,BKBANK-BKD(R2) SAME SC ACCOUNT?                           
         BE    REQL70                                 NO                        
         OI    FLAG2,FLGBLD        TURN ON BLDSPEC FLAG                         
*        BRAS  RE,BLDSPEC                                                       
*        BNE   *+8                                                              
*        OI    FLAG,FLGREC                                                      
*        BRAS  RE,OVERPROF         OVERRIDE PROFILE VALUES BY ACCOUNT           
REQL70   CLI   QOPT2,C'Y'          DO YOU WANT OUTPUT TAPE?                     
         BNE   REQL100                                                          
         CLI   OUTPUT,C'Y'         ARE THEY SETUP FOR TAPE?                     
         BE    REQL80                                                           
*       - - - - - - - - - - - - -                                               
* CL#0367170N                                                                   
*        MVC   P+1(36),=C'*** BANK ACCT NOT SETUP FOR TAPE ***'                 
*        GOTO1 ACREPORT                                                         
* CL#0367170N                                                                   
         B     REQL100                                                          
*                                                                               
REQL80   TM    FLAG,FLGSWCH        SWITCH IN TAPE ROUTINES                      
         BNO   REQL90              YES, EXIT                                    
*       - - - - - - - - - - - - -                                               
         MVC   P+1(37),=C'*** INVALID BANK ACCOUNT FOR TAPE ***'                
         MVC   P+39(12),BKBANK                                                  
         MVC   PSECOND+1(26),=C'*** TAPE IS INCOMPLETE ***'                     
         GOTO1 ACREPORT                                                         
         MVI   OUTPUT,C'N'         YES CLOSE SORT AND QUIT                      
         B     REQLX                                                            
*                                                                               
REQL90   MVI   MYBYTE,2            SET MYBYTE FOR AC5703                        
         GOTO1 APHASE2,DMCB,(RA),(RC),(RB)                                      
         TM    FLAG,FLGNOP         DON'T TOT/PRINT IF NOT USING RECORD          
         BZ    REQL100                                                          
         NI    FLAG,X'FF'-FLGNOP   RESET FLAG                                   
         B     *+8                                                              
REQL100  BRAS  RE,ADDTOT                                                        
* CL#0367170N                                                                   
         CLI   QOPT2,C'Y'          DO YOU WANT OUTPUT TAPE?                     
         BNE   REQL105                                                          
         CLI   OUTPUT,C'Y'         ARE THEY SETUP FOR TAPE?                     
         BE    REQL105                                                          
         MVC   P+1(36),=C'*** BANK ACCT NOT SETUP FOR TAPE ***'                 
         GOTO1 ACREPORT                                                         
REQL105  DS    0H                                                               
* CL#0367170N                                                                   
*                                                                               
         CLI   FRSTSRT,C'E'                                                     
         BE    REQL110             EOF                                          
         MVC   0(BKLNQ,R7),0(R2)                                                
         TM    FLAG2,FLGBLD        NEED TO GO TO BLDSPEC?                       
         BNO   REQL10                                                           
         BRAS  RE,BLDSPEC                                                       
         BNE   *+8                                                              
         OI    FLAG,FLGREC                                                      
         NI    FLAG2,X'FF'-FLGBLD  RESET FLAG                                   
         BRAS  RE,OVERPROF         OVERRIDE PROFILE VALUES BY ACCOUNT           
         B     REQL10                                                           
*                                                                               
REQL110  BRAS  RE,ACTOT                                                         
         BRAS  RE,LDGTOT                                                        
REQLX    GOTO1 ADSORTER,DMCB,=C'END'                                            
         CLI   OUTPUT,C'N'         DO YOU WANT OUTPUT TAPE?                     
         JE    EXIT                                                             
*        - - - - - - - -                                                        
         CLI   MYBYTE,2            WAS THERE ANY DATA PUT TO TAPE?              
         BE    *+8                  YES                                         
         MVI   QOPT6,C'N'           NO, DO NOT RUN POSPAY                       
*        - - - - - - - -                                                        
         MVI   MYBYTE,3            SET MYBYTE FOR AC5703                        
         GOTO1 APHASE2,DMCB,(RA),(RC),(RB)                                      
         J     EXIT                                                             
         EJECT                                                                  
*********************************************************************           
* LITERALS                                                          *           
*********************************************************************           
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
         DROP RB                                                                
*********************************************************************           
* BUILD SPEC BLOCK (SPECD)                                          *           
*       R7 - ADDRESS OF CURRENT BKBUF ENTRY                         *           
*  - READ SC ACCOUNT FOR BANKING INFORMATION                        *           
*         - BANK KEY                                                *           
*         - BANK ACCOUNT NUMBER                                     *           
*  - READ BANK RECORD ON CTFILE/ACCFILE                             *           
*         - FORMAT                                                  *           
*         - TRANSMISSION TYPE                                       *           
*         - TRANSMISSION KEY (DODS/MQ KEY)                          *           
*         - DATASET NAME                                            *           
*         - ADVANTIS ACCOUNT                                        *           
*         - ADVANTIS USER ID                                        *           
*         - MESSAGE CLASS                                           *           
*  - READ FORMAT RECORD                                             *           
*         - FORMAT BLOCK                                            *           
*         - BLOCK SIZE                                              *           
*         - RECORD SIZE                                             *           
*********************************************************************           
         SPACE 1                                                                
         USING ACTRECD,R4                                                       
         USING BKD,R7                                                           
BLDSPEC  NTR1  BASE=*,LABEL=*                                                   
         L     R4,AIO                                                           
         MVI   OUTPUT,C'N'                                                      
*                                                                               
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,RCCOMPFL                                                 
         MVC   ACTKUNT(2),=C'SC'                                                
         MVC   ACTKACT,BKBANK      MOVE IN ACCOUNT                              
         MVC   TEMPKEY(ACTKEND),0(R4)                                           
         GOTO1 DATAMGR,DMCB,DMREAD,ACCFIL,AIO,AIO                               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R3,ACTRFST                                                       
         SHI   R3,7                                                             
*                                                                               
         SR    R0,R0                                                            
BLDSP10  CLI   0(R3),0                                                          
         BE    BLDSP50                                                          
         CLI   0(R3),CBPELQ        X'2F'- BANK INFO ELEMENT                     
         BE    BLDSP20                                                          
         CLI   0(R3),RSTELQ        X'30'- STATUS ELEMENT                        
         BE    BLDSP30                                                          
         CLI   0(R3),FFTELQ        X'DB'- FREEFORM TEXT ELEMENT                 
         BE    BLDSP40                                                          
BLDSP15  IC    R0,1(R3)            BUMP TO NEXT ELEMENT                         
         AR    R3,R0                                                            
         B     BLDSP10                                                          
*                                                                               
         USING CBPFELD,R3                                                       
BLDSP20  MVC   SVBACCT,SPACES                                                   
         MVC   SVBKKEY,SPACES      BANK/BRANCH CODE                             
         LA    R0,L'SVBKKEY                                                     
         LA    R1,SVBKKEY                                                       
         LA    R2,CBPBCDE                                                       
BLDSP23  CLI   0(R2),X'40'                                                      
         BNH   BLDSP24                                                          
         MVC   0(1,R1),0(R2)                                                    
         LA    R1,1(R1)                                                         
         LA    R2,1(R2)                                                         
         BCT   R0,BLDSP23                                                       
BLDSP24  LA    R0,L'SVBACCT                                                     
         LA    R1,SVBACCT                                                       
         LA    R2,CBPBACC                                                       
BLDSP25  CLI   0(R2),X'40'                                                      
         BNH   BLDSP15                                                          
         MVC   0(1,R1),0(R2)                                                    
         LA    R1,1(R1)                                                         
         LA    R2,1(R2)                                                         
         BCT   R0,BLDSP25                                                       
         B     BLDSP15                                                          
         DROP  R3                                                               
*                                                                               
         USING RSTELD,R3                                                        
BLDSP30  CLI   QOPT2,C'Y'                                                       
         BNE   *+16                NO OUTPUT TAPE                               
         TM    RSTSTAT1,X'08'                                                   
         BZ    *+8                 NO OUTPUT FILE                               
         MVI   OUTPUT,C'Y'                                                      
         B     BLDSP15                                                          
         DROP  R3                                                               
*                                                                               
         USING FFTELD,R3                                                        
BLDSP40  CLI   FFTTYPE,FFTTEDIP    IS THIS EDI BANK PROFILE?                    
         BNE   BLDSP15                                                          
         CLI   FFTETYP,FFTPOS      IS THIS FOR POSPAY?                          
         BNE   BLDSP15                                                          
         MVC   SVMSGCL,FFTMSGC     MESSAGE CLASS                                
*        MVC   SVDSN,FFTDNAM       DATASET NAME                                 
         MVC   SVDSN,SPACES        CLEAR DATASET NAME                           
         MVC   SVDSN(L'FFTDNAM),FFTDNAM   SAVE DATASET NAME                     
         B     BLDSP15                                                          
         DROP  R3                                                               
*                                                                               
BLDSP50  CLC   SVBKKEY,SPACES      DID WE GET A BANK/BRANCH CODE                
         BNH   BLDSPNO                                                          
*                                                                               
* READ BANK INFORMATION                                                         
*                                                                               
         USING BANKD,R3                                                         
         L     RE,AIO              CLEAR IO TO SPACES                           
         LHI   RF,2000                                                          
         SR    R1,R1                                                            
         ICM   R1,8,SPACES                                                      
         MVCL  RE,R0                                                            
*                                                                               
         L     R3,AIO                                                           
         MVC   BANCPY,RCCOMPFL     COMPANY CODE                                 
         MVC   BANCDE,SVBKKEY      BANK CODE                                    
         MVC   BANBRN,SVBKKEY+3    HUB AND BRANCH                               
         MVC   BANORIG,ORIGINUM    SEND ORIGIN NUMBER.                          
         GOTO1 VGETBANK,DMCB,(1,AIO),ADCOMFAC,AACGTBNK                          
         CLC   BAN57FKY,SPACES     DO WE HAVE BANK RECORD FOR POSPAY ?          
         BNH   BLDSPNO                                                          
*                                                                               
         CLC   SVFRMT,SPACES                                                    
         BE    BLDSP52                                                          
         CLC   SVFRMT(6),BAN57FKY  CHECK THAT BANK HAS NOT CHANGED              
         BE    BLDSP52             FORMAT CAN STILL BE DIFFERENT                
         OI    FLAG,FLGSWCH        WITHIN THE SAME BANK                         
         B     BLDSPNO                                                          
*                                                                               
BLDSP52  MVC   SVFRMT,BAN57FKY     FORMAT                                       
         MVC   SVADVID,BAN57USR    ADVANTIS ID                                  
         CLC   SVADVID,SPACES                                                   
         BH    *+10                                                             
         MVC   SVADVID,BANDFUSR    USE DEFAULT ADVANTIS ID                      
         MVC   SVADVAC,BAN57ACN    ADVANTIS ACCOUNT                             
         CLC   SVADVAC,SPACES                                                   
         BH    *+10                                                             
         MVC   SVADVAC,BANDFACN    USE DEFAULT ADVANTIS ACCOUNT                 
         CLC   SVMSGCL,SPACES                                                   
         BH    BLDSP54             USE MESSAGE CLASS FROM SC ACCOUNT            
         MVC   SVMSGCL,BAN57CLS    MESSAGE CLASS                                
         CLC   SVMSGCL,SPACES                                                   
         BH    *+10                                                             
         MVC   SVMSGCL,BANDFCLS    USE DEFAULT MESSAGE CLASS                    
BLDSP54  CLC   SVDSN,SPACES                                                     
         BH    *+10                USE DATASET NAME FROM SC ACCOUNT             
         MVC   SVDSN,BAN57DSN      DATASET NAME                                 
         OC    SVDSN,SPACES        TURN BINARY ZEROS TO SPACES                  
*MN DSFTK-135                                                                   
         MVC   SVSRCRTE,BANRNO     SOURCE BANK ROUTING NUMBER                   
*MN DSFTK-135                                                                   
*        CLC   SVDSN,SPACES                                                     
*        BH    *+10                                                             
*        MVC   SVDSN,BANDFDSN      USE DEFAULT DATASET NAME                     
BLDSP56  DS    0H                                                               
         MVC   SVTYP,BAN57TYP      TRANSMISSION TYPE                            
         MVC   SVTRNKY,BAN57TKY    TRANSMISSION KEY                             
         CLC   SVTRNKY,SPACES                                                   
         BH    *+10                                                             
         MVC   SVTRNKY,BANDFTKY    USE DEFAULT TRANSMISSION KEY                 
         MVC   SVCHRG,BAN57CHR     CHARGE                                       
         CLC   SVCHRG,SPACES                                                    
         BH    *+10                                                             
         MVC   SVCHRG,BANDFCHR     USE DEFAULT CHARGE                           
*                                                                               
         XC    SVSTA,SVSTA                                                      
         OC    SVSTA,BAN57STA      SAVE STATUS FROM BANKPOSPAY LEVEL            
         OC    SVSTA,SVSTA                                                      
         BNZ   *+10                                                             
         OC    SVSTA,BANDFSTA      SAVE STATUS FROM DEFAULT BNK LVL.            
         MVC   SVORIGID,BAN57ORG   USERID FROM BANK RECORD IN =AFM              
         OC    SVORIGID,SPACES                                                  
         MVC   SVORIGNM,BAN57NME   COMPANY NAME ON IDI RECORD                   
         OC    SVORIGNM,SPACES                                                  
         MVC   SVDATE,BANDATE      DATE OF RUN                                  
         MVC   SVTIME,BANTIME      TIME OF RUN                                  
*                                                                               
* READ FORMAT INFORMATION                                                       
*                                                                               
         USING FORMD,R3                                                         
         L     RE,AFRMBLK           CLEAR FRMBLK TO SPACES                      
         LHI   RF,L'FRMBLK                                                      
         SR    R1,R1                                                            
         ICM   R1,8,SPACES                                                      
         MVCL  RE,R0                                                            
*                                                                               
         L     R3,AFRMBLK                                                       
         MVC   FORMCPY,RCCOMPFL    COMPANY CODE                                 
         MVC   FORMBCDE,SVBKKEY    BANK CODE                                    
         MVC   FORMFRM,SVFRMT      FORMAT CODE                                  
         OC    FORMFRM,SPACES      DON'T LEAVE ANY BINARY ZEROES                
         GOTO1 VGETFORM,DMCB,(X'01',(R3)),ADCOMFAC,0                            
         MVC   SVRECSZ,FFRMRLN     RECORD LENGTH                                
         MVC   SVBLKSZ,FFRMBSZ     BLOCK SIZE                                   
         LA    R1,FFRMFRM                                                       
         ST    R1,AFORMAT          ADDRESS OF FORMAT                            
*                                                                               
*  BUILD SPEC BLOCK                                                             
*                                                                               
         USING SPECD,R4                                                         
         L     R4,ASPCBLK                                                       
*        XC    SPSTAT,SPSTAT                                                    
*                                                                               
         CLC   SVBACCT,SPACES      BANK ACCOUNT                                 
         BNH   BLDSPNO                                                          
         CLC   SVBLKSZ,=X'0000'    BLOCK SIZE                                   
         BNH   BLDSPNO                                                          
         CLC   SVRECSZ,=X'0000'    RECORD SIZE                                  
         BNH   BLDSPNO                                                          
         CLC   SVDSN,SPACES        DATASET NAME                                 
         BNH   BLDSPNO                                                          
* ADVANTIS INFO SHOULD NOT BE REQUIRED                                          
*        CLC   SVADVID,SPACES      ADVANTIS ID                                  
*        BNH   BLDSPNO                                                          
*        CLC   SVADVAC,SPACES      ADVANTIS ACCOUNT                             
*        BNH   BLDSPNO                                                          
* ADVANTIS INFO SHOULD NOT BE REQUIRED                                          
         CLC   SVMSGCL,SPACES      MESSAGE CLASS                                
         BNH   BLDSPNO                                                          
         CLC   SVTRNKY,SPACES      TRANSMISSION KEY                             
         BNH   BLDSPNO                                                          
         CLC   SVCHRG,SPACES       CHARGE                                       
         BNH   BLDSPNO                                                          
*                                                                               
         MVC   SPAGYCD,ALPHAID     AGENCY CODE                                  
         MVC   SPQACCT,BKBANK      SC ACCOUNT                                   
         MVC   SPBACCT,SVBACCT     BANK ACCOUNT                                 
         MVC   SPDTFNM,=CL7'TAPEOUT'  DTF NAME                                  
         CLI   OUTPUT,C'Y'                                                      
         BNE   BLDSP55                                                          
         OI    SPSTAT,SPPOSEQ      ACCOUNT IS POSPAY                            
*                                                                               
         TM    SVSTA,BAN57CMR      DO WE NEED COMPRESSED DATA SET ?             
         BNO   *+8                                                              
         OI    SPSTAT,SPPOSCOM     COMPRESS OUTPUT DATASET                      
*                                                                               
BLDSP55  MVC   SPBLKSZ,SVBLKSZ     BLOCK SIZE                                   
         MVC   SPRECSZ,SVRECSZ     RECORD SIZE                                  
         MVC   SPROUTE,AFORMAT     FORMAT ADDRESS                               
         MVC   SPDSPRFX,=CL8'ACCTAPE.'   PREFIX                                 
         MVC   SPDSNME,SVDSN       DATASET NAME                                 
         MVC   SPCLASS,SVMSGCL     MESSAGE CLASS                                
         MVC   ACCNUM,SVADVAC      ADVANTIS ACCOUNT                             
         MVC   USRID,SVADVID       ADVANTIS USER ID                             
*        MVC   ORIGID,SVORIGID      ORIGIN USER ID                              
*        MVC   ORIGNM,SVORIGNM     ORIGIN USER ID NAME                          
*                                                                               
         LA    RE,CANTAB                                                        
BLDSP60  CLI   0(RE),EOF                                                        
         BE    BLDSPYES                                                         
         CLC   BKORULA+1(1),0(RE)                                               
         BNE   *+12                                                             
         OI    SPSTAT,SPCAN        SHOW ACCOUNT IS CANADIAN                     
         B     BLDSPYES                                                         
         LA    RE,L'CANTAB(RE)                                                  
         B     BLDSP60                                                          
*                                                                               
BLDSPYES SR    RC,RC                                                            
BLDSPNO  LTR   RC,RC                                                            
         J     EXIT                                                             
         DROP  R3,R4                                                            
         SPACE 2                                                                
*********************************************************************           
* TABLES                                                            *           
*********************************************************************           
         SPACE 1                                                                
CANTAB   DS    0C                                                               
         DC    C'Q'                CANADIAN PRINT                               
         DC    C'T'                CANADIAN SPOT                                
         DC    C'W'                CANADIAN PRODUCTION                          
         DC    AL1(EOF)                                                         
         SPACE 2                                                                
*********************************************************************           
* LITERALS                                                          *           
*********************************************************************           
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* CALCULATING TOTALS ROUTINE                                         *          
**********************************************************************          
         SPACE 1                                                                
ADDTOT   NTR1  BASE=*,LABEL=*                                                   
         CLI   FRSTIM,0                                                         
         BNE   ADDTOT10                                                         
         MVI   FRSTIM,1                                                         
         XC    MEDCNT,MEDCNT       MEDIA COUNT                                  
         ZAP   TOT,=P'0'                                                        
         ZAP   TOTCD,=P'0'                                                      
         ZAP   TOTNETCD,=P'0'                                                   
         ZAP   GRND,=P'0'                                                       
         ZAP   GRNDCD,=P'0'                                                     
         ZAP   GRNDNTCD,=P'0'                                                   
         MVI   FRST,0                                                           
*                                                                               
ADDTOT10 CLI   FRST,0              IF FRST TIME STORE KEY AND REPORT            
         BNE   ADDTOT20                 HEADING INFO.                           
         MVI   FRST,1                                                           
         MVI   FORCEHED,C'Y'                                                    
         MVC   STACCNO(12),BKBANK                                               
         MVC   STACCNM(36),BKBNKNM                                              
         MVC   STREQ(12),BKREQST                                                
         GOTO1 DATCON,DMCB,(0,BKDATE),(8,STDTE)                                 
         MVC   SVKEY(12),BKBANK                                                 
         GOTO1 ADSQUASH,DMCB,STACCNO,48                                         
*                                                                               
ADDTOT20 CLC   SVKEY(12),BKBANK    BANK ACCOUNT BREAK                           
         BE    ADDTOT30                                                         
         BRAS  RE,ACTOT            BANK TOTAL                                   
         MVI   FRST,0                                                           
         B     ADDTOT10                                                         
*                                                                               
ADDTOT30 MVC   P+1(6),BKCHECK      MOVE TO PRINT                                
         MVC   P+10(8),BKCKDTE         CHECK NO AND DATE                        
         MVC   P+19(14),BKACCNT         CHECK NO                                
         MVC   P+34(34),BKPAYEE         PAYEE NO AND NAME                       
         EDIT  (P6,BKNET),(15,P+68),2,COMMAS=YES,MINUS=YES                      
         CLI   PROGPROF,C'Y'                                                    
         BNE   ADDTOT40                                                         
         EDIT  (P6,BKCD),(11,P+84),2,COMMAS=YES,MINUS=YES                       
         EDIT  (P6,BKNETCD),(14,P+96),2,COMMAS=YES,MINUS=YES                    
*                                                                               
ADDTOT40 AP    TOT,BKNET(6)        ADD TO ACCOUNT TOTALS                        
         AP    TOTCD,BKCD(6)                                                    
         AP    TOTNETCD,BKNETCD(6)                                              
         CLI   BKCLEAR,C'C'                                                     
         BNE   ADDTOT50                                                         
         MVI   CLEARSW,X'01'                                                    
         MVC   P+8(1),BKCLEAR      PRINT 'C'                                    
         AP    CLRNET,BKNET(6)     ADD TO CLEARED TOTALS                        
         AP    CLRCD,BKCD(6)                                                    
         AP    CLRNETCD,BKNETCD(6)                                              
ADDTOT50 BRAS  RE,MEDADD           ADD TO MEDIA TOTALS                          
         MVI   SPACING,2                                                        
         BRAS  RE,DISC                                                          
*                                                                               
ADDTOTX  J     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* CASH DISCOUNTS ROUTINE                                             *          
**********************************************************************          
         SPACE 1                                                                
DISC     NTR1  BASE=*,LABEL=*                                                   
         MVC   HEAD5+12(48),STACCNO                                             
         MVC   HEAD5+96(8),STDTE                                                
         MVC   HEAD4+97(12),STREQ                                               
         CLI   PROGPROF,C'Y'       CASH DISCOUNT PROFILE OPTION                 
         BNE   DISC10                                                           
         MVC   HEAD8+86(23),=C'  CASH   CHECK AMOUNT +'                         
         MVC   HEAD9+86(23),=C'DISCOUNT CASH DISCOUNT '                         
DISC10   GOTO1 ACREPORT                                                         
*                                                                               
DISCX    J     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* ACCOUNT TOTALS ROUTINE                                             *          
**********************************************************************          
         SPACE 1                                                                
*                                  LAST FOR AN ACCNT - TOTALS                   
ACTOT    NTR1  BASE=*,LABEL=*                                                   
         MVC   P+24(22),=C'TOTAL FOR BANK ACCOUNT'                              
         EDIT  (P8,TOT),(15,P+68),2,COMMAS=YES,MINUS=YES,FLOAT=$                
         CLI   PROGPROF,C'Y'       TEST FOR CD PROFILE OPTION                   
         BNE   ACTOT10                                                          
         EDIT  (P8,TOTCD),(12,P+83),2,COMMAS=YES,MINUS=YES,FLOAT=$              
         EDIT  (P8,TOTNETCD),(15,P+95),2,COMMAS=YES,MINUS=YES,FLOAT=$           
ACTOT10  MVI   SPACING,3                                                        
         BRAS  RE,DISC                                                          
         CLI   CLEARSW,X'01'                                                    
         BNE   ACTOT30                                                          
         MVC   P+24(30),=C'TOTAL CLEARED FOR BANK ACCOUNT'                      
         EDIT  (P8,CLRNET),(15,P+68),2,COMMAS=YES,MINUS=YES,FLOAT=$             
         CLI   PROGPROF,C'Y'       TEST FOR CD PROFILE OPTION                   
         BNE   ACTOT20                                                          
         EDIT  (P8,CLRCD),(12,P+83),2,COMMAS=YES,MINUS=YES,FLOAT=$              
         EDIT  (P8,CLRNETCD),(15,P+95),2,COMMAS=YES,MINUS=YES,FLOAT=$           
ACTOT20  MVI   SPACING,3                                                        
         BRAS  RE,DISC                                                          
ACTOT30  AP    GRND,TOT      ADD TO LEDGER TOTALS                               
         AP    GRNDCD,TOTCD                                                     
         AP    GRNDNTCD,TOTNETCD                                                
*                                                                               
         MVI   SPACING,3                                                        
         BRAS  RE,DISC                                                          
*MN SPEC-6777                                                                   
         CLI   PROGPROF+8,C'Y'    SUPPRESS MEDIA TOTALS SUMMARY                 
         BE    *+8                SPEC-6777                                     
*MN SPEC-6777                                                                   
         BRAS  RE,MEDTOT         MEDIA TOTALS BY ACCOUNT                        
         XC    MEDCNT,MEDCNT                 CLEAR COUNT                        
         L     RE,AMEDTAB                                                       
         LH    RF,=Y(MEDMAX*MEDTABQ)                                            
         SR    R1,R1                                                            
         ICM   R1,8,SPACES                                                      
         MVCL  RE,R0                                                            
*                                                                               
         ZAP   TOT,=P'0'                                                        
         ZAP   TOTCD,=P'0'                                                      
         ZAP   TOTNETCD,=P'0'                                                   
*                                                                               
ACTOTX   J     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* LEDGER  TOTALS ROUTINE                                             *          
**********************************************************************          
         SPACE 1                                                                
LDGTOT   NTR1  BASE=*,LABEL=*                                                   
         MVI   FORCEHED,C'Y'                                                    
         MVC   P+24(16),=C'TOTAL FOR LEDGER'                                    
         EDIT  (P8,GRND),(15,P+68),2,COMMAS=YES,MINUS=YES,FLOAT=$               
         CLI   PROGPROF,C'Y'       TEST FOR CD PROFILE OPTION                   
         BNE   LDGTOT10                                                         
         EDIT  (P8,GRNDCD),(12,P+83),2,COMMAS=YES,MINUS=YES,FLOAT=$             
         EDIT  (P8,GRNDNTCD),(15,P+95),2,COMMAS=YES,MINUS=YES,FLOAT=$           
LDGTOT10 BRAS  RE,DISC                                                          
*                                                                               
LDGTOTX  J     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* OVER-RIDE PROFILES ROUTINE                                         *          
**********************************************************************          
         SPACE 1                                                                
*        OVERRIDE PROFILE VALUES BY ACCOUNT                                     
*        EXECUTED ONCE PER TAPE                                                 
*                                                                               
OVERPROF NTR1  BASE=*,LABEL=*                                                   
         CLC   ALPHAID,=C'BS'                                                   
         BNE   OPX                                                              
         LA    R0,OPNUM                                                         
         LA    R1,OPTAB                                                         
         LA    R2,BKBANK                                                        
OP10     CLC   0(12,R1),0(R2)                                                   
         BE    OP20                OVERRIDE FOUND                               
         LA    R1,L'OPTAB(R1)                                                   
         BCT   R0,OP10                                                          
         B     OPX                                                              
*                                                                               
OP20     MVI   PROGPROF+3,C'Y'     OVERRIDE PROFILE                             
*                                                                               
OPX      J     EXIT                                                             
*                                                                               
*        OVER-RIDE PROFILE TABLE                                                
*                                                                               
OPTAB    DS    0CL12                                                            
         DC    CL12'B007'                                                       
         DC    CL12'B001M'                                                      
         DC    CL12'B002M'                                                      
         DC    CL12'B003M'                                                      
         DC    CL12'B012M'                                                      
OPNUM    EQU   (*-OPTAB)/L'OPTAB                                                
         EJECT                                                                  
**********************************************************************          
* MEDIA ADD ROUTINE                                                  *          
**********************************************************************          
         SPACE 1                                                                
MEDADD   NTR1  BASE=*,LABEL=*                                                   
         CLC   BKCULM+1(2),=C'SJ'  MANUAL                                       
         BE    MEDADDX             EXIT ON MANUAL                               
         CLC   BKCULM+1(2),=C'SN'  EXIT FOR TALENT                              
         BE    MEDADDX                                                          
         CLC   BKACCNT(5),=C'  ***'  VOID                                       
         BE    MEDADDX             EXIT                                         
         L     R3,AMEDTAB                                                       
         L     R2,MEDCNT           NUMBER IN TABLE                              
         LTR   R2,R2                                                            
         BZ    MEDADD30                                                         
*                                                                               
MEDADD10 CLC   0(4,R3),BKCULM      MATCHING ENTRY                               
         BNE   MEDADD20                                                         
         AP    4(6,R3),BKNET(6)                                                 
         AP    10(6,R3),BKCD(6)                                                 
         AP    16(6,R3),BKNETCD(6)                                              
         B     MEDADDX             EXIT                                         
*                                                                               
MEDADD20 LA    R3,22(R3)           NEXT ENTRY                                   
         BCT   R2,MEDADD10                                                      
*                                                                               
MEDADD30 LH    R1,=Y(MEDMAX*MEDTABQ)   CHECK TABLE                              
         C     R1,MEDCNT                                                        
         BNE   *+6                                                              
         DC    H'0'                MEDTAB FULL                                  
*                                                                               
         MVC   0(4,R3),BKCULM      NEW ENTRY                                    
         ZAP   4(6,R3),BKNET(6)                                                 
         ZAP   10(6,R3),BKCD(6)                                                 
         ZAP   16(6,R3),BKNETCD(6)                                              
         L     R2,MEDCNT                                                        
         LA    R2,1(R2)                                                         
         ST    R2,MEDCNT                                                        
*                                                                               
MEDADDX  J     EXIT                EXIT                                         
         EJECT                                                                  
**********************************************************************          
* MEDIA TOTALS ROUTINE                                               *          
**********************************************************************          
         SPACE 1                                                                
MEDTOT   NTR1  BASE=*,LABEL=*                                                   
         L     R3,AMEDTAB                                                       
         L     R2,MEDCNT                                                        
         LTR   R2,R2                                                            
         BZ    MEDTOTX             EXIT                                         
         MVC   P+7(16),=C'**MEDIA TOTALS**'                                     
*                                                                               
MEDTOT10 MVC   MYKEY,SPACES                                                     
         MVC   MYKEY(4),0(R3)                                                   
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'ACCOUNT',MYKEY,AIO                        
         L     R4,AIO                                                           
         CLC   MYKEY(4),0(R4)                                                   
         BNE   MEDTOT20                                                         
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   MEDTOT20                                                         
         XR    R5,R5                                                            
         IC    R5,1(R4)                                                         
         SH    R5,=H'3'                                                         
         EX    R5,*+8                                                           
         B     MEDTOT20                                                         
         MVC   P+24(0),2(R4)                                                    
*                                                                               
MEDTOT20 EDIT  (P6,4(R3)),(15,P+68),2,COMMAS=YES,MINUS=YES,FLOAT=$              
         CLI   PROGPROF,C'Y'       TEST FOR CD PROFILE OPTION                   
         BNE   MEDTOT30                                                         
         EDIT  (P6,10(R3)),(12,P+83),2,COMMAS=YES,MINUS=YES,FLOAT=$             
         EDIT  (P6,16(R3)),(15,P+95),2,COMMAS=YES,MINUS=YES,FLOAT=$             
MEDTOT30 BRAS  RE,DISC                                                          
*                                                                               
         LA    R3,22(R3)                                                        
         BCT   R2,MEDTOT10                                                      
*                                                                               
MEDTOTX  J     EXIT                EXIT                                         
         EJECT                                                                  
         DROP  R7                                                               
**********************************************************************          
* GET ADDRESS ROUTINE                                                *          
*     R7 = ADDRESS OF BANK RECORD BUFFER (BKBUF)                     *          
**********************************************************************          
         SPACE 1                                                                
         USING BKD,R7                                                           
GETCACNT NTR1  BASE=*,LABEL=*                                                   
         MVC   BKPAYEE,SPACES                                                   
         LA    RE,BKADDR           CLEAR ADDRESS                                
         LA    RF,BKADLNQ                                                       
         SR    R1,R1                                                            
         ICM   R1,8,SPACES                                                      
         MVCL  RE,R0                                                            
*                                                                               
         USING ACTRECD,R4                                                       
         L     R4,AIO                                                           
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,RCCOMPFL                                                 
         MVC   ACTKULA,BKORULA                                                  
         GOTO1 DATAMGR,DMCB,DMREAD,=C'ACCOUNT',AIO,AIO                          
         CLI   8(R1),0                                                          
         BNE   GETCX                                                            
*                                                                               
         LR    R2,R4                                                            
         AH    R2,DATADISP                                                      
GETC10   CLI   0(R2),0                                                          
         BE    GETCX                                                            
         CLI   0(R2),NAMELQ        GET NAME                                     
         BE    GETC30                                                           
         CLI   0(R2),ADRELQ        GET ADDR                                     
         BE    GETC40                                                           
GETC20   SR    R1,R1                                                            
         IC    R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     GETC10                                                           
*                                                                               
         USING NAMELD,R2                                                        
GETC30   SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SHI   R1,NAMLN1Q+1                                                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BKPAYEE(0),NAMEREC                                               
         B     GETC20                                                           
         DROP  R2                                                               
*                                                                               
         USING ADRELD,R2                                                        
GETC40   SR    R1,R1                                                            
         IC    R1,ADRLN                                                         
         SHI   R1,ADRADD1-ADRELD+1                                              
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BKADDR1(0),ADRADD1                                               
         B     GETC20                                                           
         DROP  R2                                                               
*                                                                               
GETCX    J     EXIT                                                             
         EJECT                                                                  
         DROP  R4,R7                                                            
**********************************************************************          
* LOAD 5703 PHASE                                                    *          
**********************************************************************          
         SPACE 1                                                                
LOAD     NTR1  BASE=*,LABEL=*                                                   
         L     R5,ADMASTC                                                       
         USING MASTD,R5                                                         
         MVC   DUB,=CL8'AC5703'                                                 
         MVC   DUB+6(1),MCTEST3    LOAD EITHER LIVE OR TEST                     
         GOTO1 LOADER,DMCB,DUB,0,0                                              
         OC    4(4,R1),4(R1)                                                    
         BNZ   LOAD10                                                           
         MVC   DUB,=CL8'AC5703'                                                 
         GOTO1 LOADER,DMCB,DUB,0,0                                              
         OC    4(4,R1),4(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
LOAD10   MVC   APHASE2,4(R1)       A(AC5703)                                    
         MVC   MCAPHAS3,4(R1)      SET UP PATCHABILITY                          
*                                                                               
         ICM   R0,15,0(R1)         LENGTH OF PHASE                              
         ICM   RF,15,4(R1)         A(ADFORM)                                    
         STCM  RF,15,MCUSRDMP                                                   
         STCM  RF,15,WORD          ADDRESS OF 1ST PHASE                         
         STCM  R0,15,FULL          LENGTH OF 1ST PHASE                          
         AR    RF,R0                                                            
         LA    RF,4095(RF)                                                      
         STCM  RF,15,MCUSRDMP+4                                                 
*                                                                               
         MVC   DUB,=CL8'GETFRM'    GETFORM                                      
         MVC   DUB+6(1),MCTEST4    LOAD EITHER LIVE OR TEST                     
         GOTO1 LOADER,DMCB,DUB,0,0                                              
         OC    4(4,R1),4(R1)                                                    
         BNZ   LOAD20                                                           
         MVC   DUB,=CL8'GETFRM'                                                 
         GOTO1 LOADER,DMCB,DUB,0,0                                              
         OC    4(4,R1),4(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
LOAD20   MVC   VGETFORM,4(R1)      A(GETFORM)                                   
         MVC   MCAPHAS4,4(R1)      SET UP PATCHABILITY                          
*                                                                               
         ICM   R0,15,0(R1)         LENGTH OF PHASE                              
         L     R1,FULL                                                          
         AR    R0,R1               ADD LENGTH OF PREVIOUS PHASE                 
         ST    R0,FULL                                                          
         ICM   RF,15,WORD          A(GETFORM)                                   
         STCM  RF,15,MCUSRDMP                                                   
         AR    RF,R0                                                            
         LA    RF,4095(RF)                                                      
         STCM  RF,15,MCUSRDMP+4                                                 
*                                                                               
         XC    TEST5,TEST5                                                      
         MVC   BYTE,MCUPSI         MCUPSI 1=A, 2=B, 3=C                         
         NI    BYTE,TESTLVL                                                     
         BZ    *+14                                                             
         MVI   TEST5,X'C0'                                                      
         OC    TEST5,BYTE          SET TEST LEVEL                               
*                                                                               
         MVC   DUB,=CL8'T00A64'    GETBANK                                      
         MVC   DUB+6(1),TEST5                                                   
         GOTO1 LOADER,DMCB,DUB,0,0                                              
         OC    4(4,R1),4(R1)                                                    
         BNZ   LOAD30                                                           
         MVC   DUB,=CL8'T00A64'                                                 
         GOTO1 LOADER,DMCB,DUB,0,0                                              
         OC    4(4,R1),4(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
LOAD30   MVC   VGETBANK,4(R1)      A(GETBANK)                                   
         MVC   MCAPHAS1,4(R1)      SET UP PATCHABILITY                          
*                                                                               
         ICM   R0,15,0(R1)         LENGTH OF PHASE                              
         L     R1,FULL                                                          
         AR    R0,R1               ADD LENGTH OF PREVIOUS PHASE                 
         ST    R0,FULL                                                          
         ICM   RF,15,WORD          A(GETBANK)                                   
         STCM  RF,15,MCUSRDMP                                                   
         AR    RF,R0                                                            
         LA    RF,4095(RF)                                                      
         STCM  RF,15,MCUSRDMP+4                                                 
         J     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
**********************************************************************          
* GETEL                                                              *          
**********************************************************************          
         SPACE 1                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* DATA CONSTANTS                                                     *          
**********************************************************************          
         SPACE 1                                                                
AMEDTAB  DC    A(MEDTAB)           A(MEDIA TABLE)                               
AIO      DC    A(IO)               A(IO AREA)                                   
AFRMBLK  DC    A(FRMBLK)           FORMAT BLOCK                                 
*                                                                               
FRSTIM   DC    X'00'                                                            
FRSTSRT  DC    C'Y'                                                             
MEDCNT   DC    F'0'                                                             
CLEARSW  DC    X'00'                                                            
ACCFIL   DC    C'ACCOUNT '                                                      
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,27,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=179'                                   
         EJECT                                                                  
**********************************************************************          
* TABLES AND BUFFERS                                                 *          
**********************************************************************          
         SPACE 1                                                                
AACGTBNK DS    A                                                                
BKBUF    DS    CL(BKLNQ1)          BANK ENTRY BUFFER                            
TEMPKEY  DS    CL42                                                             
MYKEY    DS    CL42                                                             
TEST5    DS    XL1                  GETBANK A/B/C                               
TESTLVL  EQU   X'03'                PHASE TEST LEVEL (BLANK/A/B/C)              
*                                                                               
MEDTAB   DS    0H                  MEDIA TABLE                                  
         DS    (MEDTABQ*MEDMAX)C   C/U/L/MEDIA                                  
MEDTABQ  EQU   22                                                               
MEDMAX   EQU   100                                                              
*                                                                               
IO       DS    0CL2042                                                          
IOKEY    DS    CL42                                                             
IOAREA   DS    CL2000                                                           
*                                                                               
SPCBLK   DS    CL(SPECFLNQ)        SPEC BLOCK                                   
*                                                                               
FRMBLK   DS    CL(FORMLNQ)                                                      
*                                                                               
         EJECT                                                                  
**********************************************************************          
* INCLUDES                                                           *          
**********************************************************************          
         SPACE 1                                                                
* ACGETFORMD                                                                    
       ++INCLUDE ACGETFORMD                                                     
* DDGETBANKD                                                                    
       ++INCLUDE DDGETBANKD                                                     
* AC57D                                                                         
       ++INCLUDE AC57D                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'027ACREP5702 09/01/17'                                      
         END                                                                    
