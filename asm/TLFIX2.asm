*          DATA SET TLFIX2     AT LEVEL 008 AS OF 05/01/02                      
*PHASE TLFIX2,*                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE SORTER                                                                 
*INCLUDE DATCON                                                                 
         TITLE 'ACCOLADE FILE FIX'                                              
TLFIX2   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*TLFIX2*,R8                                                    
         LR    R9,R1                                                            
         USING ACCWORKD,R9                                                      
         L     R2,AIOAREA                                                       
         LA    R3,4(R2)                         SKIP 4 BYTE LENGTH              
         USING ACKEYD,R3                                                        
         L     RA,VCPRINT                                                       
         USING DPRINT,RA                                                        
         CLI   OVSWITCH,0                       TEST FIRST TIME HOOK.           
         BH    PROREC1                                                          
         MVI   OVSWITCH,1                       NOT FIRST TIME THROUGH.         
         OPEN  (RCVTAPE,(OUTPUT))                                               
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD     INITIALIZE SORTER.          
         SPACE 1                                                                
         B     EXIT                                                             
         EJECT                                                                  
PROREC1  CLI   OVSWITCH,1                       BEING PASSED A RECORD?          
         BH    GETSRT                                                           
         LA    R5,SRTREC                                                        
         USING SRTDSEC,R5                                                       
         CLI   ACKEYACC,X'7B'                      CHECK                        
         BE    PROREC5                             COMPANY                      
         CLI   ACKEYACC,X'67'                      CODES.                       
         BNE   EXIT                                                             
*                                                                               
PROREC5  CLI   PROCREC,TRNSACTN                    IS RECORD A                  
         BNE   PROREC20                            TRANSACTION.                 
*              PROCESS TRANSACTION RECORDS.                                     
         CLC   ACKEYACC+1(2),=C'SR'                UNIT S LEDGER R.             
         BE    PROREC10                                                         
         CLI   ACKEYACC+1,C'T'                     UNIT T.                      
         BNE   EXIT                                                             
         CLC   ACKEYCON(10),=C'999BILLING'         BILLING RECORD.              
         BNE   EXIT                                                             
*              SET UP KEY FOR SORT.                                             
         MVC   RCACCT,ACKEYACC+1              IN T RECORD: UNIT,LEDGER          
         MVI   RCTYPE,0                       RECORD TYPE.                      
         SR    R1,R1                          ZERO REGISTER ONE.                
         AH    R1,=H'29'                      RECORD LENGTH.                    
         STCM  R1,3,SRTLEN                                                      
         XC    SRTLEN+2(2),SRTLEN+2           MUST BE BINARY ZEROS.             
         MVC   RCRECRD(12),ACKEYACC+3         MOVE IN CLIENT,PROD,              
         B     PROREC15                       COMMERCIAL TO SORT REC.           
*                                                                               
PROREC10 MVC   RCACCT,ACKEYACC+3              IN SR RECORD: ACCOUNT             
         MVI   RCTYPE,1                                     REC TYPE.           
*                                                                               
         SR    R1,R1                          ZERO REGISTER ONE.                
         ICM   R1,3,ACLENGTH                  CALCULATE LENGTH OF               
         AH    R1,=H'17'                      RECORD AND SORT KEY.              
         STCM  R1,3,SRTLEN                    MOVE TO SORT HEADER.              
         XC    SRTLEN+2(2),SRTLEN+2           MUST BE BINARY ZEROS.             
*                                                                               
         ST    R5,SAVEREGS                                                      
         LA    R4,RCRECRD                                                       
         L     R5,=F'1000'                                                      
         SR    R7,R7                          ZERO REGISTER SEVEN.              
         ICM   R7,3,ACLENGTH                  LENGTH OF ACCT REC.               
         LR    R6,R3                          ACTUAL ACCT REC.                  
         MVCL  R4,R6                          MOVE RECORD TO OUTPUT.            
         L     R5,SAVEREGS                                                      
         MVI   WRITE,X'FF'                    SET TO DELETE RECORD.             
         AP    DELRCD,=P'1'                   ADD 1 TO DELETED RECORDS.         
PROREC15 MVC   RCCOMCD(1),ACKEYACC            COMPANY CODE.                     
         MVC   RCDATE,ACKEYDTE                DATE.                             
         MVC   RCINVREF,ACKEYREF              INVOICE REFERENCE.                
         GOTO1 =V(SORTER),DMCB,=C'PUT',SRTLEN    PUT RECORDS TO SORTER.         
         B     EXIT                                                             
         EJECT                                                                  
*              PROCESS SR RECORDS                                               
PROREC20 CLC   ACKEYACC+1(2),=C'SR'          IS RECORD UNIT S LEDGER R.         
         BNE   PROREC50                                                         
*                                                                               
         CLI   PROCREC,HISTORY               IS RECORD HISTORY.                 
         BNE   PROREC30                                                         
         MVI   WRITE,X'FF'                   DELETE RECORD FROM FILE.           
         AP    DELRCD,=P'1'                  ADD 1 TO NUM OF RECS DEL.          
         B     EXIT                                                             
*                                                                               
PROREC30 CLI   PROCREC,ACCLOW                IS RECORD ACCLOW.                  
         BNE   PROREC40                                                         
         BAS   RE,SRACCLOW                   DELETE 32 & 33 ELEMENTS.           
         MVI   WRITE,X'FF'                   DELETE RECORD FROM FILE.           
         AP    DELRCD,=P'1'                  ADD 1 TO DELETED RECORDS.          
         B     EXIT                                                             
*                                                                               
PROREC40 CLI   PROCREC,LEDGER                 IS RECORD LEDGER RECORD.          
         BNE   EXIT                                                             
*                                                                               
         SR    R1,R1                          ZERO REGISTER ONE.                
         ICM   R1,3,ACLENGTH                  CALCULATE LENGTH OF               
         AH    R1,=H'17'                      RECORD AND SORT KEY.              
         STCM  R1,3,SRTLEN                    MOVE TO SORT HEADER.              
         XC    SRTLEN+2(2),SRTLEN+2           MUST BE BINARY ZEROS.             
*                                                                               
         LA    R4,RCRECRD                                                       
         ST    R5,SAVEREGS                                                      
         L     R5,=F'1000'                                                      
         SR    R7,R7                          ZERO REGISTER SEVEN.              
         ICM   R7,3,ACLENGTH                  LENGTH OF ACCT REC.               
         LR    R6,R3                          ACTUAL ACCT REC.                  
         MVCL  R4,R6                          MOVE RECORD TO OUTPUT.            
         L     R5,SAVEREGS                                                      
*                                                                               
         MVC   RCCOMCD(12),ACKEYACC           SORT KEY IS COMP CODE,            
         MVI   RCTYPE,2                       UNIT, LEDG, CLIENT.               
         GOTO1 =V(SORTER),DMCB,=C'PUT',SRTLEN                                   
         MVI   WRITE,X'FF'                    DELETE RECORD FROM FILE.          
         AP    DELRCD,=P'1'                   ADD 1 TO RECORDS DELETED.         
         B     EXIT                                                             
*                                                                               
PROREC50 CLI   ACKEYACC+1,C'T'               IS RECORD UNIT T?                  
         BNE   EXIT                                                             
         CLI   PROCREC,LEDGER                IF LEDGER RECORD LEDGER IS         
         BNE   PROREC55                      CHANGING.                          
         BAS   RE,TLEDGER                    ADD SR-ACCLOW MISC REC             
         B     EXIT                                 AND HISTORY REC.            
*                                                                               
PROREC55 CLI   PROCREC,ACCHIGH               IS RECORD ACCHIGH.                 
         BNE   EXIT                                                             
         CLC   ACKEYACC+6(3),SPACES          MUST BE TOP ACCHIGH REC.           
         BNE   EXIT                                      IF NOT EXIT.           
         BAS   RE,TACCHIGH                   WRITE NEW SR-ACCLOW REC.           
         B     EXIT                                                             
         EJECT                                                                  
*              ROUTINE TO GET RECORDS FROM SORTER                               
         SPACE 2                                                                
GETSRT   CLI   OVSWITCH,X'FF'                NO MORE RECS BEING PASSED          
         BNE   EXIT                                                             
GETSR1   GOTO1 =V(SORTER),DMCB,=C'GET'       GET RECORDS FROM SORTER.           
         ICM   R4,15,DMCB+4                  WAS A RECORD RETURNED.             
         BZ    GETSR15                       NO MORE RECORDS FOUND.             
*                                                                               
         LA    R5,4(R4)                      SKIP 4 BYTE LENGTH.                
         USING SRTDSEC,R5                                                       
         LA    R3,RCRECRD                                                       
*                                                                               
GETSR4   CLI   RCTYPE,0                     RECORD TYPE ZERO OR ONE.            
         BNE   GETSR8                                                           
         MVC   SRTCDE,RCCOMCD               STORE HEADERS THE SAME?             
         MVC   TCLIENT,RCRECRD                                                  
         MVC   TPROD,RCRECRD+3                                                  
         MVC   TCMRCL,RCRECRD+6                                                 
         B     GETSR1                       GET NEXT RECORD.                    
*                                                                               
GETSR8   CLI   RCTYPE,1                                                         
         BNE   GETSR30                                                          
         CLC   ACKEYCON(12),=C'   TALENT TV'     IF CONTRA NOT TAL TV           
         BE    GETSR12                           OR                             
         CLC   ACKEYCON(15),=C'   TALENT RADIO'  TALENT RADIO                   
         BE    GETSR12                                                          
         MVC   ACKEYACC+5(3),=C'999'                                            
         BAS   RE,PUTITA                      WRITE TRANS REC TO RECV.          
*                                                                               
         LA    R6,ACRECORD                                                      
GETSR9   CLI   0(R6),X'00'                                                      
         BE    GETSR10                                                          
         MVI   0(R6),X'FF'                                                      
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     GETSR9                                                           
GETSR10  GOTO1 DELEL,DMCB,(X'FF',(R3)),0                                        
*                                                                               
         MVC   ACKEYACC+32(10),SPACES                                           
         BAS   RE,HEADEL                                                        
         BAS   RE,PUTITA                                                        
         B     GETSR1                         NEXT REC FROM SORTER.             
*                                                                               
GETSR12  MVC   ACKEYACC+5(3),=C'999'                                            
         CLC   SRTCDE,RCCOMCD               SORT HEADERS THE SAME.              
         BNE   GETSR13                      IF NOT NO MATCH.                    
         MVC   ACKEYACC+5(3),TCLIENT        MOVE CLIENT CODE TO KEY.            
         GOTO1 GETEL,DMCB,(X'44',(R3)),0                                        
         L     R6,DMCB+12                                                       
         USING TRANSD,R6                                                        
         CLI   TRNSSTAT,X'80'                                                   
         BNE   GETSR13                                                          
         LA    R4,ELEMENT                                                       
         USING TRCPJD,R4                                                        
         XC    ELEMENT,ELEMENT              CLEAR ELEMENT.                      
         MVI   TRCPEL,X'4F'                 BUILD NEW 4F ELEMENT.               
         MVI   TRCPLEN,X'17'                                                    
         MVI   TRCPTYPE,C'C'                                                    
         MVC   TRCPCLI(3),TCLIENT                                               
         MVC   TRCPPROD(3),TPROD                                                
         MVC   TRCPJOB,TCMRCL                                                   
         MVC   TRCPUL,RCACCT                                                    
         GOTO1 ADDEL,DMCB,(R3),(R4)         ADD ELEMENT TO RECORD.              
*                                                                               
GETSR13  BAS   RE,PUTITA                    PUT NEW REC TO RECOVERY.            
         B     GETSR1                                                           
GETSR15  BAS   RE,ENDPGM                                                        
         B     EXIT                                                             
*                                                                               
GETSR30  CLI   RCTYPE,2                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETEL,DMCB,(X'14',(R3)),0                                        
         CLI   DMCB+12,0                                                        
         BE    GETSR50                                                          
         GOTO1 GETEL,DMCB,(X'20',(R3)),0      IS RECORD ACCHIGH                 
         CLI   DMCB+12,0                      (HAS 20                           
         BNE   GETSR40                             30                           
         GOTO1 GETEL,DMCB,(X'30',(R3)),0           15 ELEMENT).                 
         CLI   DMCB+12,0                                                        
         BNE   GETSR40                                                          
         XC    ELEMENT,ELEMENT                                                  
         GOTO1 GETEL,DMCB,(X'15',(R3)),0                                        
         CLI   DMCB+12,0                                                        
         BNE   GETSR40                                                          
         L     R6,DMCB+12                    SAVE 15 ELEMENT FROM               
         ZIC   R1,1(R6)                      ACCHIGH RECORD.                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELEMENT(0),0(R6)                                                 
*                                                                               
         XC    ELEMENTA,ELEMENTA                                                
         GOTO1 GETEL,DMCB,(X'22',(R3)),0                                        
         CLI   DMCB+12,0                                                        
         BNE   GETSR35                                                          
         L     R6,DMCB+12                    SAVE 22 ELEMENT FROM               
         ZIC   R1,1(R6)                      ACCHIGH RECORD.                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELEMENTA(0),0(R6)                                                
GETSR35  GOTO1 DELEL,DMCB,(X'15',(R3)),0        DELETE 15 EL FROM               
         BAS   RE,PUTITA                        ACCHIGH REC.                    
         B     GETSR1                           PUT OLD ACCHIGH REC             
*                                               TO RECOVERY.                    
GETSR40  GOTO1 GETEL,DMCB,(X'32',(R3)),0                                        
         CLI   DMCB+12,0                        IS RECORD ACCLOW                
         BNE   GETSR50                          (HAS 20                         
         OC    ELEMENT,ELEMENT                                                  
         BZ    GETSR45                                                          
         LA    R6,ELEMENT                       IF ACCLOW ADD:                  
         GOTO1 ADDEL,DMCB,(R3),(R6)                 15 EL TO ACCLOW.            
GETSR45  OC    ELEMENTA,ELEMENTA                                                
         BZ    GETSR50                                                          
         LA    R6,ELEMENTA                                                      
         GOTO1 ADDEL,DMCB,(R3),(R6)                 22 EL TO ACCLOW.            
GETSR50  BAS   RE,PUTITA                        WRITE ACCLOW TO RECV.           
         B     GETSR1                                                           
         EJECT                                                                  
*              ROUTINE TO WRITE OUT RECOVERY RECORD AND SET TO DELETE           
         SPACE 2                                                                
PUTIT    NTR1                                                                   
         LA    R2,OUTREC           RECORD GOING TO RECOVERY TAPE.               
         XC    0(28,R2),0(R2)      CLEAR FIRST 28 BYTES FOR HEADER.             
         XR    R1,R1               CLEAR R1.                                    
         ICM   R1,3,ACLENGTH       LENGTH OF ACTUAL ACCT RECORD.                
         LR    R7,R1                                                            
         LA    R1,28(R1)           ADD HEADER LEN (24+4) TO REC LEN.            
         STH   R1,0(R2)                                                         
         MVC   4(2,R2),=X'6103'    INDICATE ACCOUNT/ADD                         
         LA    R4,OUTREC+28                                                     
         L     R5,=F'1000'                                                      
         LR    R6,R3               ACTUAL ACCOUNTING RECORD.                    
         MVCL  R4,R6               MOVE RECORD TO OUTPUT.                       
         LA    R4,OUTREC+28                                                     
         SPACE 1                                                                
         PUT   RCVTAPE,(R2)        PUT TO TAPE.                                 
         SPACE 1                                                                
         AP    OUTRCD,=P'1'        ADD ONE TO RECORDS PUT TO TAPE.              
*        BAS   RE,DMPTAPE                                                       
         SPACE 1                                                                
         B     EXIT                                                             
PUTITA   NTR1                                                                   
         LA    R2,OUTREC           RECORD GOING TO RECOVERY TAPE.               
         XC    0(28,R2),0(R2)      CLEAR FIRST 28 BYTES FOR HEADER.             
         XR    R1,R1               CLEAR R1.                                    
         ICM   R1,3,ACLENGTH       LENGTH OF ACTUAL ACCT RECORD.                
         LR    R7,R1                                                            
         LA    R1,28(R1)           ADD HEADER LEN (24+4) TO REC LEN.            
         STH   R1,0(R2)                                                         
         MVC   4(2,R2),=X'6103'    INDICATE ACCOUNT/ADD                         
         LA    R4,OUTREC+28                                                     
         L     R5,=F'1000'                                                      
         LR    R6,R3               ACTUAL ACCOUNTING RECORD.                    
         MVCL  R4,R6               MOVE RECORD TO OUTPUT.                       
         LA    R4,OUTREC+28                                                     
         SPACE 1                                                                
         PUT   RCVTAPE,(R2)        PUT TO TAPE.                                 
         SPACE 1                                                                
         AP    OUTRCD,=P'1'        ADD ONE TO RECORDS PUT TO TAPE.              
         BAS   RE,DMPTAPE                                                       
         SPACE 1                                                                
         B     EXIT                                                             
         EJECT                                                                  
*              ROUTINE TO CHANGE SR-ACCLOW TO SR-ACCHIGH RECORD.                
         SPACE 1                                                                
SRACCLOW NTR1                                                                   
         GOTO1 DELEL,DMCB,(X'32',(R3)),0             DELETE 32 ELEMENT.         
         GOTO1 DELEL,DMCB,(X'33',(R3)),0             DELETE 33 ELEMENT.         
*                                                                               
         SR    R1,R1                          ZERO REGISTER ONE.                
         ICM   R1,3,ACLENGTH                  CALCULATE LENGTH OF               
         AH    R1,=H'17'                      RECORD AND SORT KEY.              
         STCM  R1,3,SRTLEN                    MOVE TO SORT HEADER.              
         XC    SRTLEN+2(2),SRTLEN+2           MUST BE BINARY ZEROS.             
*                                                                               
         LA    R4,RCRECRD                                                       
         L     R5,=F'1000'                                                      
         SR    R7,R7                          ZERO REGISTER SEVEN.              
         ICM   R7,3,ACLENGTH                  LENGTH OF ACCT REC.               
         LR    R6,R3                          ACTUAL ACCT REC.                  
         MVCL  R4,R6                          MOVE RECORD TO OUTPUT.            
*                                                                               
         LA    R5,SRTREC                                                        
         USING SRTDSEC,R5                                                       
         MVC   RCCOMCD(12),ACKEYACC           SORT KEY IS COMP CODE,            
         MVI   RCTYPE,2                       UNIT, LEDG, CLIENT.               
         GOTO1 =V(SORTER),DMCB,=C'PUT',SRTLEN                                   
         B     EXIT                                                             
         EJECT                                                                  
*              ROUTINE TO CHANGE T-ACCHIGH REC TO SR-ACCLOW RECORD.             
         SPACE 1                                                                
TACCHIGH NTR1                                                                   
         GOTO1 GETEL,DMCB,(X'24',(R3)),0                                        
         CLI   DMCB+12,0                                                        
         BNE   TACCH1                                                           
*                                                                               
         L     R6,DMCB+12                    MOVE CLIENT CODE INTO              
         USING ACPROFD,R6                    24 ELEMENT - LITTLE FAVOR          
         MVC   ACPRRECV+5(3),ACKEYACC+5      FOR CLIENTS.                       
*                                                                               
TACCH1   LA    R4,RCRECRD                     MOVE RECORD INTO RCRECRD.         
         L     R5,=F'1000'                                                      
         SR    R7,R7                          ZERO REGISTER SEVEN.              
         ICM   R7,3,ACLENGTH                  LENGTH OF ACCT REC.               
         LR    R6,R3                          ACTUAL ACCT REC.                  
         MVCL  R4,R6                          MOVE RECORD TO OUTPUT.            
*                                                                               
         LA    R5,SRTREC                                                        
         USING SRTDSEC,R5                                                       
         LA    R3,RCRECRD                                                       
         MVC   UNLDCLI,ACKEYACC+1           SAVE UNIT LEDGER CLIENT.            
         MVC   ACKEYACC+3(5),UNLDCLI        SHIFT UNIT,LEDG,CLI 2 BYTES         
         MVC   ACKEYACC+1(2),=C'SR'         PUT SR IN UNIT,LEDGER.              
*                                                                               
         LA    R6,ACRECORD                                                      
TACCH3   CLI   0(R6),X'00'                  DELETE                              
         BE    TACCH10                      ALL                                 
         CLI   0(R6),X'20'                  BUT                                 
         BE    TACCH5                       THE                                 
         CLI   0(R6),X'30'                  20                                  
         BE    TACCH5                       AND                                 
         MVI   0(R6),X'FF'                  30                                  
TACCH5   ZIC   R0,1(R6)                     ELEMENTS.                           
         AR    R6,R0                                                            
         B     TACCH3                                                           
TACCH10  GOTO1 DELEL,DMCB,(X'FF',(R3)),0                                        
         BAS   RE,BALIN                     ADD 32 & 33 ELEMENT.                
*                                                                               
         SR    R1,R1                          ZERO REGISTER ONE.                
         ICM   R1,3,ACLENGTH                  CALCULATE LENGTH OF               
         AH    R1,=H'17'                      RECORD AND SORT KEY.              
         STCM  R1,3,SRTLEN                    MOVE TO SORT HEADER.              
         XC    SRTLEN+2(2),SRTLEN+2           MUST BE BINARY ZEROS.             
*                                                                               
         MVC   RCCOMCD(12),ACKEYACC           SORT KEY IS COMP CODE,            
         MVI   RCTYPE,2                       UNIT, LEDG, CLIENT.               
         GOTO1 =V(SORTER),DMCB,=C'PUT',SRTLEN                                   
*                                                                               
         GOTO1 DELEL,DMCB,(X'20',(R3)),0           AFTER PUTTING REC            
         GOTO1 DELEL,DMCB,(X'30',(R3)),0           TO SORTER DELETE             
         GOTO1 DELEL,DMCB,(X'32',(R3)),0           20,30,32,33 ELS.             
         GOTO1 DELEL,DMCB,(X'33',(R3)),0                                        
*                                                                               
         MVC   ACKEYCON(15),=C'   TALENT TV   '    CONTRA ACCOUNT.              
         BAS   RE,HEADEL                           ADD 43 ELEMENT.              
         GOTO1 =V(SORTER),DMCB,=C'PUT',SRTLEN                                   
         GOTO1 DELEL,DMCB,(X'43',(R3)),0           DELETE OLD 43 EL.            
         MVC   ACKEYCON+10(5),=C'RADIO'            CONTRA ACCOUNT.              
         BAS   RE,HEADEL                           ADD 43 ELEMENT.              
         GOTO1 =V(SORTER),DMCB,=C'PUT',SRTLEN                                   
         B     EXIT                                                             
         EJECT                                                                  
*              ROUTINE TO BUILD A MISCELLANEOUS RECEIVABLES ACCOUNT             
*              WHEN LEDGER CHANGES WHEN READING T                               
         SPACE 2                                                                
TLEDGER  NTR1                                                                   
         LA    R4,RCRECRD                     MOVE RECORD INTO RCRECRD.         
         L     R5,=F'1000'                                                      
         SR    R7,R7                          ZERO REGISTER SEVEN.              
         ICM   R7,3,ACLENGTH                  LENGTH OF ACCT REC.               
         LR    R6,R3                          ACTUAL ACCT REC.                  
         MVCL  R4,R6                          MOVE RECORD TO OUTPUT.            
         LA    R5,SRTREC                                                        
         USING SRTDSEC,R5                                                       
         LA    R3,RCRECRD                                                       
         MVC   UNLDCLI,ACKEYACC+1                                               
         MVC   ACKEYACC+3(5),UNLDCLI        SHIFT UNIT,LEDG,CLI 2 BYTES         
         MVC   ACKEYACC+1(2),=C'SR'           PUT SR IN UNIT,LEDG               
         MVC   ACKEYACC+5(3),=C'999'          999 IN FOR CLIENT                 
         MVC   ACKEYACC+8(34),SPACES                                            
         LA    R6,ACRECORD                                                      
TLED3    CLI   0(R6),X'00'                  DELETE                              
         BE    TLED10                       ALL                                 
         CLI   0(R6),X'30'                  BUT                                 
         BE    TLED5                        30                                  
         MVI   0(R6),X'FF'                  ELEMENT.                            
TLED5    ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     TLED3                                                            
TLED10   GOTO1 DELEL,DMCB,(X'FF',(R3)),0                                        
         LA    R4,ELEMENT                                                       
         USING ACNAMED,R4                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   ACNMEL,X'20'                   ADD 20 ELEMENT                    
         MVI   ACNMLEN,X'0F'                                                    
         MVC   ACNMNAME(13),=C'MISCELLANEOUS'                                   
         GOTO1 ADDEL,DMCB,(R3),(R4)                                             
         BAS   RE,BALIN                       ADD 32 & 33 ELEMENTS.             
*                                                                               
         SR    R1,R1                          ZERO REGISTER ONE.                
         ICM   R1,3,ACLENGTH                  CALCULATE LENGTH OF               
         AH    R1,=H'17'                      RECORD AND SORT KEY.              
         STCM  R1,3,SRTLEN                    MOVE TO SORT HEADER.              
         XC    SRTLEN+2(2),SRTLEN+2           MUST BE BINARY ZEROS.             
*                                                                               
         MVC   RCCOMCD(12),ACKEYACC                                             
         MVI   RCTYPE,2                                                         
         GOTO1 =V(SORTER),DMCB,=C'PUT',SRTLEN                                   
*                                                                               
         GOTO1 DELEL,DMCB,(X'20',(R3)),0      DELETE 20 ELEMENT                 
         GOTO1 DELEL,DMCB,(X'30',(R3)),0         "   30    "                    
         GOTO1 DELEL,DMCB,(X'32',(R3)),0         "   32    "                    
         GOTO1 DELEL,DMCB,(X'33',(R3)),0         "   33    "                    
         MVC   ACKEYCON(15),=C'   TALENT TV   '  CONTRA ACCOUNT.                
         BAS   RE,HEADEL                         ADD 43 ELEMENT.                
         GOTO1 =V(SORTER),DMCB,=C'PUT',SRTLEN                                   
*                                                                               
         GOTO1 DELEL,DMCB,(X'43',(R3)),0      DELETE OLD 43 ELEMENT.            
         MVC   ACKEYCON+10(5),=C'RADIO'       CONTRA ACCOUNT.                   
         BAS   RE,HEADEL                      ADD 43 ELEMENT.                   
         GOTO1 =V(SORTER),DMCB,=C'PUT',SRTLEN                                   
         B     EXIT                                                             
         EJECT                                                                  
*              ROUTINE TO ADD A STATUS ELEMENT  -- 30 ELEMENT                   
         SPACE 1                                                                
STATIN   NTR1                                                                   
         LA    R4,ELEMENT                                                       
         USING ACSTATD,R4                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVC   ACSTEL(2),=X'3016'                                               
         MVC   ACSTSECY,SPACES                                                  
         MVI   ACSTSUB,C' '                                                     
         MVC   ACSTCNTR,SPACES                                                  
         MVI   ACSTSTX,C' '                                                     
         GOTO1 ADDEL,DMCB,(R3),(R4)                                             
         B     EXIT                                                             
         SPACE 2                                                                
*              ROUTINE TO ADD A BALANCE ELEMENT       32 & 33 ELEMENT           
         SPACE 1                                                                
BALIN    NTR1                                                                   
         LA    R4,ELEMENT                                                       
         USING ACBALD,R4                                                        
         MVI   ACBLEL,X'32'                                                     
         MVI   ACBLLEN,ACBLLNQ                                                  
         ZAP   ACBLFRWD,=P'0'                                                   
         ZAP   ACBLDR,=P'0'                                                     
         ZAP   ACBLCR,=P'0'                                                     
         ZAP   ACBLURG,=P'0'                                                    
         GOTO1 ADDEL,DMCB,(R3),(R4)                                             
         SPACE 1                                                                
         USING ACPEELD,R4                                                       
         MVC   ACPEEL(2),=X'3314'                                               
         XC    ACPEPLDT(6),ACPEPLDT                                             
         ZAP   ACPEDR,=P'0'                                                     
         ZAP   ACPECR,=P'0'                                                     
         GOTO1 ADDEL,DMCB,(R3),(R4)                                             
         B     EXIT                                                             
         SPACE 2                                                                
*              ROUTINE TO ADD SUB-ACCT HEADER ELEMENT -- 43 ELEMENT             
HEADEL   NTR1                                                                   
         LA    R4,ELEMENT                    ADD 43 ELEMENT.                    
         USING TRSUBHD,R4                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   TRSBEL,X'43'                                                     
         MVI   TRSBLEN,X'12'                                                    
         MVC   TRSBACNT,ACKEYCON                                                
         GOTO1 ADDEL,DMCB,(R3),(R4)                                             
         B     EXIT                                                             
         EJECT                                                                  
ENDPGM   CLOSE (RCVTAPE)           LAST TIME                                    
         BAS   RE,PRNTIT                                                        
         B     EXIT                                                             
*                                                                               
EXIT     XMOD1 1                                                                
         SPACE 3                                                                
PRNTIT   NTR1                                                                   
         EDIT  DELRCD,(12,P+17),COMMAS=YES                                      
         MVC   P+1(15),=C'DELETED RECORDS'                                      
         GOTO1 VPRINTER                                                         
         MVC   P+1(15),=C'OUTPUT RECORDS '        OUTPUT FOR                    
         EDIT  OUTRCD,(12,P+17),COMMAS=YES        TOTALS OF                     
         GOTO1 VPRINTER                           DELETED                       
*        EDIT  CHNRCD,(12,P+17),COMMAS=YES        AND OUTPUT                    
*        MVC   P+1(15),=C'ADDED RECORDS  '        RECORDS                       
*        GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         B     EXIT                                                             
         EJECT                                                                  
DELETER  NTR1                                                                   
         GOTO1 DELEL,DMCB,(X'32',(R3)),0          DELETES 32                    
         GOTO1 DELEL,DMCB,(X'33',(R3)),0          AND 33 ELEMENTS.              
         LH    R0,ACLENGTH                                                      
         AH    R0,=H'4'                                                         
         L     R2,AIOAREA                                                       
         STH   R0,0(R2)                    ADJUSTS LENGTH FOR REC.              
*        BAS   RE,DMPPUT                                                        
         B     EXIT                                                             
DMPGET   NTR1                                                                   
         AP    DUMPCNT,=P'1'                                                    
         ZAP   DUB,DUMPCNT                                                      
         DP    DUB,EVERY                   SETS UP FOR                          
         CP    DUB+4(4),=P'0'              'GET' OUTPUT                         
         BNE   EXIT                        RECORD.                              
         AP    PDUMP,=P'1'                                                      
         CP    PDUMP,MAXDUMP                                                    
         BH    EXIT                                                             
         LA    R6,=C'GET '                                                      
         B     DUMP                                                             
         SPACE 1                                                                
DMPPUT   NTR1                                                                   
         AP    DUMPCNT,=P'1'                                                    
         ZAP   DUB,DUMPCNT                                                      
         DP    DUB,EVERY                   SETS UP FOR                          
         CP    DUB+4(4),=P'0'              'PUT' OUTPUT                         
         BNE   EXIT                        RECORD.                              
         AP    PDUMP,=P'1'                                                      
         CP    PDUMP,MAXDUMP                                                    
         BH    EXIT                                                             
         LA    R6,=C'PUT '                                                      
         B     DUMP                                                             
         SPACE 1                                                                
DMPTAPE  NTR1                                                                   
         AP    DUMPCNT,=P'1'                                                    
         ZAP   DUB,DUMPCNT                                                      
         DP    DUB,EVERY                 SHOWS WHAT IS                          
         CP    DUB+4(4),=P'0'            BEING WRITTEN                          
         BNE   EXIT                      TO RECOVERY                            
         AP    PDUMP,=P'1'               TAPE.                                  
         CP    PDUMP,MAXDUMP                                                    
         BH    EXIT                                                             
         LA    R6,=C'TAPE'                                                      
         EJECT                                                                  
DUMP     LR    R3,R2                                                            
         LH    R7,0(R3)                                                         
         GOTO1 PRNTBL,DMCB,(4,(R6)),(R3),C'DUMP',(R7),=C'2D'                    
         B     EXIT                                                             
         SPACE 1                                                                
*              ROUTINE TO GET AN ELEMENT                                        
         SPACE 1                                                                
*              P1   BYTE 0    ELEMENT CODE                                      
*                   BYTE 1-3  A(RECORD)                                         
*              P2   BYTE 0    LENGTH OF SEARCH ARGUMENT                         
*                   BYTE 1-3  A(SEARCH ARGUMENT)                                
         SPACE 1                                                                
GETEL    NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         ZIC   R4,0(R1)                                                         
         ZIC   R5,4(R1)                                                         
         GOTO1 HELLO,DMCB,(C'G',=C'ACCOUNT '),((R4),(R2)),((R5),(R3))           
         B     EXIT                                                             
         SPACE 2                                                                
*              ROUTINE TO DELETE AN ELEMENT                                     
         SPACE 1                                                                
*              P1   BYTE 0    ELEMENT CODE                                      
*                   BYTE 1-3  A(RECORD)                                         
*              P2   BYTE 0    LENGTH OF SEARCH ARGUMENT                         
*                   BYTE 1-3  A(SEARCH ARGUMENT)                                
         SPACE 1                                                                
DELEL    NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         ZIC   R4,0(R1)                                                         
         ZIC   R5,4(R1)                                                         
         GOTO1 HELLO,DMCB,(C'D',=C'ACCOUNT '),((R4),(R2)),((R5),(R3))           
         B     EXIT                                                             
         SPACE 1                                                                
*              ROUTINE TO ADD AN ELEMENT                                        
         SPACE 1                                                                
*              P1   A(RECORD)                                                   
*              P2   A(ELEMENT)                                                  
         SPACE 1                                                                
ADDEL    NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         GOTO1 HELLO,DMCB,(C'P',=C'ACCOUNT '),(R2),(R3)                         
         CLI   DMCB+12,0                                                        
         BE    EXIT                                                             
         DC    H'0'                CAN'T ADD THE ELEMENT                        
         SPACE 1                                                                
         EJECT                                                                  
RCVTAPE  DCB   DDNAME=RCVTAPE,RECFM=VB,DSORG=PS,MACRF=(PM),            X        
               BLKSIZE=8200,LRECL=2048,BUFNO=2                                  
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
PRNTBL   DC    V(PRNTBL)                                                        
HELLO    DC    V(HELLO)                                                         
         SPACE 1                                                                
SAVEREGS DS    4F                         SAVE OFF REGISTERS.                   
SRTLEN   DS    F                          FOUR BYTE HEADER LENGTH.              
SRTREC   DS    CL1013                     SORT RECORD.                          
OUTREC   DS    CL1028                     OUTPUT RECORD.                        
*                                                                               
DELRCD   DC    PL6'0'                     NO. OF DELETED RECS.                  
OUTRCD   DC    PL6'0'                     NO. OF OUTPUT RECS.                   
CHNRCD   DC    PL6'0'                     NO. OF CHANGED RECS.                  
         SPACE 1                                                                
DMPSW    DS    CL1                                                              
DUMPCNT  DC    PL4'0'                                                           
EVERY    DC    PL4'1'                                                           
PDUMP    DC    PL4'0'                                                           
MAXDUMP  DC    PL4'100'                                                         
         SPACE 1                                                                
SORTCARD DC    CL80'SORT FIELDS=(5,13,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=V,LENGTH=(1021,,,,)'                            
*                                                                               
*                                                                               
HEXFF    DC    H'255'                     MAX ELEMENT LENGTH.                   
TCLIENT  DS    CL3                        SAVE OFF CLIENT.                      
TPROD    DS    CL3                        SAVE OFF PRODUCT.                     
TCMRCL   DS    CL6                        SAVE OFF COMMERCIAL.                  
SRTCDE   DS    CL12                       SAVE OFF SORT-HEADER.                 
ELEMENT  DS    CL255                      STORE ELEMENT.                        
ELEMENTA DS    CL255                      STORE ELEMENT.                        
SAVLEDG  DS    CL1                        SAVE OFF LEDGER.                      
UNLDCLI  DS    CL5                        SAVE OFF UNIT,LED,CLI.                
         EJECT                                                                  
SRTDSEC  DSECT                            SORT RECORD DESECT                    
RCCOMCD  DS    CL1           SORT HEADER: COMPANY CODE                          
RCACCT   DS    CL2                        ACCOUNT OR UNIT & LEDGER              
RCDATE   DS    CL3                        DATE                                  
RCINVREF DS    CL6                        INVOICE NUMBER                        
RCTYPE   DS    CL1                        RECORD TYPE (1,2, OR 3)               
RCRECRD  DS    0C                         ACCOUNTING RECORD.                    
*                                                                               
       ++INCLUDE ACACCWORKD                                                     
         EJECT                                                                  
*        ACGENBOTH                                                              
*        DDDPRINT                                                               
*        ACTALENT                                                               
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE ACTALENT                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008TLFIX2    05/01/02'                                      
         END                                                                    
