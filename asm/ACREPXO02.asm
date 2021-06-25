*          DATA SET ACREPXO02  AT LEVEL 021 AS OF 08/16/00                      
*PHASE ACXO02A                                                                  
*INCLUDE HEXIN                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
                                                                                
         TITLE 'COMPARE/FIX JOB ORDER/ ORDER RECORDS'                           
ACXO02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACXO**,R9                                                    
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACXOD,RC                                                         
                                                                                
***********************************************************************         
* OPTION 1 = N   DON'T READ ORDER RECORDS                             *         
*          = M   ONLY PRINT 'MISSING' ORDER RECORDS                   *         
*          = S   SKIP 'MISSING' ORDER RECORDS                         *         
*          = C   ORDERS FROM CLOSED BATCHES                           *         
* OPTION 2 = N   DON'T READ JOB RECORDS                               *         
*          = M   ONLY PRINT 'MISSING' JOB RECORDS                     *         
*          = S   SKIP 'MISSING' JOB RECORDS                           *         
* OPTION 3 = O   DELETE SELECTED ORDER RECORDS                        *         
*          = J   DELETE JOB RECORD                                    *         
* OPTION 4 = D   PRINTABLE OF DELETED RECORDS                         *         
*                                                                     *         
*  FILTER OPTIONS FOR ORDERS:                                         *         
*     QAPPL(2) = THE STATUS BYTE FOR THE ORDER RECORD                 *         
*               BLANK  = ALL                                          *         
*                  80  = DELETED ORDERS                               *         
*                  40  = MATCHED ORDERS                               *         
*                  20  = LOGICALLY DELETED                            *         
*                  01  = CONTRACT                                     *         
*                  00  = OPEN                                         *         
*                                                                     *         
*     QAPPL+2(2) = THE STATUS BYTE FOR THE PRODUCTION ORDER           *         
*               BLANK  = ALL                                          *         
*                  80  = DELETED ORDERS                               *         
*                  00  = OPEN                                         *         
***********************************************************************         
                                                                                
         EJECT                                                                  
***********************************************************************         
*  FIRST FOR RUN                                                      *         
***********************************************************************         
                                                                                
RUNF     CLI   MODE,RUNFRST                                                     
         BNE   REQF                                                             
         L     R7,AMONACC                                                       
         USING ACMD,R7                                                          
         MVI   NCPY,0                                                           
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVI   FCSUPOFC,C'Y'                                                    
*                                                                               
         LA    R1,SRTLNQ           RECORD LENGTH                                
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  RECCARD+21(3),DUB+6(2)                                           
         LA    R1,SRTKEY-SRTREC+1    DISP. TO KEY                               
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SORTCARD+13(3),DUB+6(2)                                          
         LA    R1,SRTKLNQ           LENGTH OF KEY                               
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SORTCARD+17(3),DUB+6(2)                                          
         GOTO1 ADSORTER,DMCB,SORTCARD,RECCARD                                   
         B     XIT                                                              
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
*  FIRST FOR REQUEST                                                  *         
***********************************************************************         
                                                                                
REQF     CLI   MODE,REQFRST                                                     
         BNE   PRCOR                                                            
         MVI   FCRESET,C'Y'                                                     
         MVI   FCRDORD,C'Y'                                                     
         CLI   QOPT1,C'N'          NO ORDERS                                    
         BNE   *+8                                                              
         MVI   FCRDORD,C'N'                                                     
*                                                                               
REQF1    CLC   QAPPL(2),SPACES                                                  
         BE    REQF3                                                            
         GOTO1 HEXIN,DMCB,QAPPL,ORDOPT,2                                        
         CLC   12(4,R1),=F'1'                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
REQF3    CLC   QAPPL+2(2),SPACES                                                
         BE    REQF5                                                            
         GOTO1 HEXIN,DMCB,QAPPL+2,JOBOPT,2                                      
         CLC   12(4,R1),=F'1'                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
REQF5    LA    R0,L'QACCOUNT       GET LENGTH OF ACCOUNT                        
         LA    R1,QACCOUNT+L'QACCOUNT-1                                         
         CLI   0(R1),C' '                                                       
         BH    *+10                                                             
         BCTR  R1,0                                                             
         BCT   R0,*-10                                                          
         STC   R0,ACCFLN                                                        
*                                                                               
         CLC   QSTART,SPACES                                                    
         BE    REQF7                                                            
         GOTO1 DATCON,DMCB,(0,QSTART),(1,STRDTE)                                
*                                                                               
REQF7    CLC   QEND,SPACES                                                      
         BE    REQF9                                                            
         GOTO1 DATCON,DMCB,(0,QEND),(1,ENDDTE)                                  
*                                                                               
REQF9    CLI   QOPT1,C'C'          CLOSED BATCHES                               
         BNE   XIT                                                              
         BAS   RE,CLOSD      MAKE LIST OF ORDERS ON CLOSED BATCHES              
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PROCESS ORDER RECORD                                                *         
***********************************************************************         
                                                                                
PRCOR    CLI   MODE,PROCORD                                                     
         BNE   RUNL                                                             
         L     R2,ADACC            R4=A(ORDER KEY)                              
         USING ORDRECD,R2                                                       
         CLC   ORDKORD,=C'000000'  TEST FOR CONTROL RECORD                      
         BE    XIT                                                              
         MVC   DUB(6),ORDKORD                                                   
         CLI   QOPT1,C'C'          TEST CLOSED ORDERS                           
         BNE   PRCOR2                                                           
         BAS   RE,FLTORD           TEST ORDER ON A CLOSED BATCH                 
         BNE   XIT                                                              
*                                                                               
PRCOR2   SR    R0,R0                                                            
         LA    R3,ORDRECD+ACCORFST                                              
*                                                                               
         USING ORDELD,R3                                                        
PRCOR3   CLI   ORDEL,ORDELQ        GET ORDER ELEMENT                            
         BE    PRCOR5                                                           
         CLI   ORDEL,0                                                          
         BE    XIT                                                              
         IC    R0,ORDLN                                                         
         AR    R3,R0                                                            
         B     PRCOR3                                                           
*                                                                               
PRCOR5   CLC   ORDACCU(2),PRODUL   TEST FOR PRODUCTION LEDGER                   
         BNE   XIT                                                              
         CLC   ORDDATE,STRDTE      TEST DATE RANGE                              
         BL    XIT                                                              
         CLC   ORDDATE,ENDDTE                                                   
         BH    XIT                                                              
         SR    R1,R1                                                            
         ICM   R1,1,ACCFLN         LENGTH OF ACCOUNT FILTER                     
         BZ    PRCOR7                                                           
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   ORDACCA(0),QACCOUNT                                              
         BNE   XIT                                                              
*                                                                               
PRCOR7   CLI   ORDOPT,X'FF'        SELECT ALL                                   
         BE    PRCOR9                                                           
         LA    RF,ORDRECD+ACCOSTAT                                              
         CLC   0(1,RF),ORDOPT                                                   
         BE    PRCOR9                                                           
         SR    R1,R1                                                            
         IC    R1,ORDOPT                                                        
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    0(RF),0             TEST ORDER STATUS BYTES                      
         BZ    XIT                 SKIP                                         
*                                                                               
PRCOR9   XC    SRTREC,SRTREC                                                    
         MVC   SRTCPY,ORDKCPY      COMPANY                                      
         MVC   SRTORD,ORDKORD      ORDER NUMBER                                 
         MVC   SRTCPJ,ORDACCA      CLIENT/PRODUCT/JOB                           
         MVC   SRTSUP,ORDSUP       SUPPLIER                                     
         MVC   SRTDTE,ORDDATE      DATE                                         
         MVC   SRTSTA,ORDRECD+ACCOSTAT STATUS                                   
         MVI   SRTSRC,SRTSORD      FROM ORDER                                   
         CLI   QOPT1,C'C'          CLOSED BATCH OPTION                          
         BNE   PRCOR11                                                          
         L     RF,FULL             ADDRESS OF BATCH ORDER ENTRY                 
         USING CBOD,RF                                                          
         MVC   SRTBTYP,CBOBTYP                                                  
         MVC   SRTBREF,CBOBREF                                                  
         MVC   SRTBITM,CBOBITM                                                  
         DROP  RF                                                               
*                                                                               
PRCOR11  GOTO1 ADSORTER,DMCB,=C'PUT',SRTREC                                     
         MVI   ACTV,C'Y'                                                        
*                                                                               
         AP    CNTTOTL,=P'1'       ADD TO TOTAL                                 
         LA    RF,ORDRECD+ACCOSTAT                                              
         TM    0(RF),ORDSDEL                                                    
         BNO   *+10                                                             
         AP    CNTDELT,=P'1'       DELETED                                      
         TM    0(RF),ORDSFMCH                                                   
         BNO   *+10                                                             
         AP    CNTMTCH,=P'1'       MATCHED                                      
         TM    0(RF),ORDSLDEL                                                   
         BNO   *+10                                                             
         AP    CNTLDEL,=P'1'       LOGICALLY DELETED                            
         TM    0(RF),ORDSCON                                                    
         BNO   *+10                                                             
         AP    CNTCONT,=P'1'       CONTRACT                                     
         CLI   0(RF),0                                                          
         BNE   *+10                                                             
         AP    CNTOPEN,=P'1'       OPEN                                         
*                                                                               
         CLC   RCCOMPFL,LSTCPY                                                  
         BE    XIT                                                              
         MVC   LSTCPY,RCCOMPFL                                                  
         SR    R1,R1                                                            
         IC    R1,NCPY                                                          
         LA    RF,CPYTAB(R1)                                                    
         MVC   0(1,RF),RCCOMPFL     TABLE OF COMPANIES                          
         AH    R1,=H'1'                                                         
         STC   R1,NCPY                                                          
         LA    R0,MXCPY                                                         
         CR    R1,R0                                                            
         BNH   *+6                                                              
         DC    H'0'                COMPANY TABLE IS FULL                        
         B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* RUN LAST -  GET FROM SORT -  PRINT REPORT                           *         
***********************************************************************         
                                                                                
RUNL     CLI   MODE,RUNLAST                                                     
         BNE   XIT                                                              
         MVI   LSTCPY,0                                                         
         CLI   QOPT2,C'N'          SKIP JOBS                                    
         BE    *+8                                                              
         BAS   RE,PRJOB             PROCESS JOB ORDERS                          
         CLI   ACTV,C'Y'           ANY ACTIVITY                                 
         BNE   RUNLX                                                            
         XC    SRTREC,SRTREC                                                    
*                                                                               
RUNL3    XC    WRKORD,WRKORD                                                    
         MVI   STATORD,X'FF'                                                    
         MVI   STATJOB,X'FF'                                                    
         OC    SRTREC,SRTREC                                                    
         BNZ   RUNL7                                                            
*                                                                               
RUNL5    GOTO1 ADSORTER,DMCB,=C'GET'                                            
         ICM   R4,15,DMCB+4                                                     
         BZ    RUNL9                                                            
*                                                                               
         MVC   WRKORD,0(R4)       SAVE RECORD FROM SORT                         
         OC    SRTREC,SRTREC                                                    
         BNZ   *+10                                                             
         MVC   SRTREC,WRKORD                                                    
*                                                                               
         CLC   SRTREC(SRTKLNQ),WRKORD                                           
         BNE   RUNL9                                                            
         MVC   SRTREC(SRTMLNQ),WRKORD                                           
         OC    SRTBTYP,WRKORD+(SRTBTYP-SRTREC)                                  
         OC    SRTBREF,WRKORD+(SRTBREF-SRTREC)                                  
         OC    SRTBITM,WRKORD+(SRTBITM-SRTREC)                                  
*                                                                               
RUNL7    LA    R2,STATORD          R2=A(ORDER STATUS)                           
         CLI   SRTSRC,SRTSORD                                                   
         BE    *+8                                                              
         LA    R2,STATJOB          SAVE JOB DATA                                
         CLI   0(R2),X'FF'         TEST ALREADY HAVE DATA                       
         BE    RUNL8               FIRST ONE                                    
         TM    0(R2),X'80'         TEST OLD ONE DELETED                         
         BO    RUNL8               IF IT IS -- USE NEW                          
         TM    SRTSTA,X'80'        TEST NEW ONE DELETED                         
         BO    RUNL8+6             DON'T REPLACE                                
         DC    H'0'                CONFUSION?                                   
RUNL8    MVC   0(1,R2),SRTSTA       RECORD STATUS                               
         XC    WRKORD,WRKORD                                                    
         B     RUNL5                GET NEXT OF PAIR                            
*                                                                               
RUNL9    CLI   SRTREC,0            TEST ANY DATA                                
         BE    RUNL13                                                           
         BAS   RE,MISM              MISMATCHED ORDER                            
         MVC   SRTREC,WRKORD                                                    
         CLI   SRTREC,0                                                         
         BE    RUNL13                                                           
         B     RUNL3                                                            
*                                                                               
RUNL13   BAS   RE,PRNTOT            PRINT TOTALS                                
RUNLX    GOTO1 ADSORTER,DMCB,=C'END'                                            
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PROCESS JOB RECORDS                                                 *         
***********************************************************************         
                                                                                
PRJOB    NTR1 ,                                                                 
         LA    R5,CPYTAB           R5=LIST OF COMPANIES                         
         SR    R0,R0                                                            
         ICM   R0,1,NCPY           NUMBER OF COMPANIES                          
         BZ    XIT                                                              
*                                                                               
PRJOB1   MVC   DKEY,SPACES                                                      
         LA    R4,DKEY             READ FOR ORDER RECORD                        
         USING TRNRECD,R4                                                       
         MVC   TRNKCPY,0(R5)       COMPANY                                      
         MVC   TRNKUNT(2),PRODUL                                                
         SR    R1,R1                                                            
         ICM   R1,1,ACCFLN         LENGTH OF ACCOUNT FILTER                     
         BZ    PRJOB2                                                           
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   TRNKACT(0),QACCOUNT                                              
*                                                                               
PRJOB2   BAS   RE,DMHGH                                                         
*                                                                               
PRJOB3   LA    R4,DIR                                                           
         SR    R1,R1               SET COMPARE LENGTH                           
         ICM   R1,1,ACCFLN                                                      
         LA    R1,2(R1)                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   TRNKCPY(0),DKEY     TEST SAME CUL/ACCOUNT                        
         BNE   PRJOB9              GET NEXT COMPANY                             
         CLC   TRNKDATE,SPACES                                                  
         BNH   PRJOB4                                                           
         CLC   TRNKWORK,=C'**'     TEST ORDER                                   
         BE    PRJOB6              PROCESS ORDER                                
         BH    PRJOB5              SKIP TO NEXT JOB                             
*                                                                               
PRJOB4   BAS   RE,DMSEQ            GET TO FIRST TRANSACTION                     
         B     PRJOB3                                                           
*                                                                               
PRJOB5   MVC   DKEY(L'ACTKCULA),DIR                                             
         LA    R4,DKEY                                                          
         LA    R1,TRNKACT+L'TRNKACT-1                                           
         SR    RF,RF                                                            
         IC    RF,0(R1)            BUMP TO NEXT JOB                             
         AH    RF,=H'1'                                                         
         STC   RF,0(R1)                                                         
         BAS   RE,DMHGH                                                         
         B     PRJOB3                                                           
*                                                                               
PRJOB6   CLC   TRNKDATE,STRDTE     TEST DATE RANGE                              
         BL    PRJOB4                                                           
         CLC   TRNKDATE,ENDDTE                                                  
         BH    PRJOB4                                                           
         SR    R1,R1                                                            
         ICM   R1,1,ACCFLN         LENGTH OF ACCOUNT FILTER                     
         BZ    PRJOB7                                                           
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   TRNKACT(0),QACCOUNT                                              
         BNE   PRJOB5                                                           
*                                                                               
PRJOB7   CLI   JOBOPT,X'FF'        ALL ORDERS                                   
         BE    PRJOB8                                                           
         CLC   TRNKSTAT,JOBOPT                                                  
         BNE   PRJOB4                                                           
*                                                                               
PRJOB8   XC    SRTREC,SRTREC                                                    
         MVC   SRTCPY,TRNKCPY      COMPANY                                      
         MVC   SRTORD,TRNKREF      ORDER NUMBER                                 
         MVC   SRTCPJ,TRNKACT      CLIENT/PRODUCT/JOB                           
         MVC   SRTDTE,TRNKDATE     DATE                                         
         MVC   SRTSUP,TRNKCULC     SUPPLIER                                     
         MVC   SRTSTA,TRNKSTA      STATUS                                       
         MVI   SRTSRC,SRTSJOB      FROM JOB                                     
         GOTO1 ADSORTER,DMCB,=C'PUT',SRTREC                                     
         MVI   ACTV,C'Y'                                                        
*                                                                               
         AP    CNTSJTOT,=P'1'                                                   
         TM    TRNKSTA,TRNSDELT                                                 
         BNO   *+10                                                             
         AP    CNTSJDEL,=P'1'                                                   
         B     PRJOB4                                                           
*                                                                               
PRJOB9   LA    R5,1(R5)            NEXT COMPANY                                 
         BCT   R0,PRJOB1                                                        
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT ERROR LISTING - MISMATCH ORDER STATUS                         *         
***********************************************************************         
                                                                                
MISM     NTR1  ,                                                                
         CLC   LSTCPY,SRTCPY                                                    
         BE    *+8                                                              
         BAS   RE,SETCPY           SET COMPANY INFO                             
         LA    R3,P                                                             
         USING PRNTD,R3                                                         
         MVC   PRNTORD,SRTORD                                                   
         GOTO1 DATCON,DMCB,(1,SRTDTE),(10,PRNTDTE)                              
         MVC   PRNTJOB,SRTCPJ                                                   
         LA    R4,PRNTORDS                ORDER STATUS                          
         CLI   STATORD,X'FF'                                                    
         BNE   MISM1                                                            
         CLI   QOPT1,C'S'          SKIP MISSING                                 
         BE    MISM13                                                           
         CLI   QOPT1,C'C'          SKIP MISSING                                 
         BE    MISM13                                                           
         MVC   0(L'MSOMIS,R4),MSOMIS      ORDER MISSING                         
         LA    R4,L'MSOMIS+1(R4)                                                
         B     MISM5                                                            
MISM1    TM    STATORD,ORDSDEL                                                  
         BNO   *+14                                                             
         MVC   0(L'MSODEL,R4),MSODEL      ORDER DELETED                         
         LA    R4,L'MSODEL+1(R4)                                                
         TM    STATORD,ORDSFMCH                                                 
         BNO   *+14                                                             
         MVC   0(L'MSOMCH,R4),MSOMCH      ORDER MATCHED                         
         LA    R4,L'MSODEL+1(R4)                                                
         TM    STATORD,ORDSLDEL                                                 
         BNO   *+14                                                             
         MVC   0(L'MSOLDE,R4),MSOLDE      LOGICALLY DELETED                     
         LA    R4,L'MSOLDE+1(R4)                                                
         TM    STATORD,ORDSCON                                                  
         BNO   *+14                                                             
         MVC   0(L'MSOCON,R4),MSOCON      CONTRACT  ORDER                       
         LA    R4,L'MSOCON+1(R4)                                                
         CLI   STATORD,0                                                        
         BNE   *+14                                                             
         MVC   0(L'MSOOPN,R4),MSOOPN      OPEN                                  
         LA    R4,L'MSOOPN+1(R4)                                                
*                                                                               
MISM5    LA    R4,PRNTJOBS                JOB STATUS                            
         CLI   STATJOB,X'FF'       TEST MISSING ORDER                           
         BNE   MISM7                                                            
         CLI   QOPT2,C'S'          SKIP MISSING JOB ORDERS                      
         BE    MISM13                                                           
         MVC   0(L'MSOMIS,R4),MSOMIS      ORDER MISSING                         
         LA    R4,L'MSOMIS+1(R4)                                                
         B     MISM9                                                            
MISM7    TM    STATJOB,TRNSDELT                                                 
         BNO   *+14                                                             
         MVC   0(L'MSODEL,R4),MSODEL      ORDER DELETED                         
         LA    R4,L'MSODEL+1(R4)                                                
         CLI   STATJOB,0                                                        
         BNE   *+14                                                             
         MVC   0(L'MSOOPN,R4),MSOOPN      OPEN                                  
         LA    R4,L'MSOOPN+1(R4)                                                
*                                                                               
MISM9    CLI   QOPT1,C'M'          ONLY MISSING ORDERS                          
         BNE   *+12                                                             
         CLI   STATORD,X'FF'                                                    
         BE    MISM11                                                           
         CLI   QOPT2,C'M'          ONLY MISSING JOB RECORDS                     
         BNE   MISM11                                                           
         CLI   STATJOB,X'FF'                                                    
         BNE   MISM13                                                           
*                                                                               
MISM11   CLI   QOPT1,C'C'          CLOSED BATCH OPTION                          
         BNE   MISM12                                                           
         EDIT  (B1,SRTBTYP),(2,PRNTBTYP)                                        
         MVC   PRNTBREF,SRTBREF                                                 
         EDIT  (B2,SRTBITM),(4,PRNTBITM)                                        
*                                                                               
MISM12   BAS   RE,PRNT                                                          
         CLI   QOPT3,C'O'          DELETE ORDER                                 
         BNE   *+8                                                              
         BAS   RE,DELO                                                          
         CLI   QOPT3,C'J'          DELETE JOB                                   
         BNE   *+8                                                              
         BAS   RE,DELJ                                                          
*                                                                               
MISM13   MVC   P,SPACES                                                         
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DELETE ORDER RECORDS                                                *         
***********************************************************************         
                                                                                
DELO     NTR1 ,                                                                 
         CLI   STATORD,X'FF'       TEST ORDER MISSING                           
         BE    XIT                                                              
         XC    DKEY,DKEY                                                        
         LA    R2,DKEY                                                          
         USING ORDRECD,R2                                                       
         MVI   ORDKTYP,ORDKTYPQ                                                 
         MVC   ORDKCPY,SRTCPY      BUILD ORDER RECORD KEY                       
         MVC   ORDKORD,SRTORD                                                   
         BAS   RE,DMRD                                                          
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                RECORD NOT FOUND                             
         LA    R2,IO1                                                           
         BAS   RE,DMGETR                                                        
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                RECORD NOT FOUND                             
         OI    ORDRSTAT,ORDSDEL                                                 
         BAS   RE,DMPUTR           DELETE RECORD                                
*                                                                               
         LA    R2,DIR                                                           
         OI    ORDKSTAT,ORDSDEL                                                 
         BAS   RE,DMWRTR           DELETE POINTER                               
         AP    CNTMARK,=P'1'                                                    
*                                                                               
         CLI   QOPT4,C'D'                                                       
         BNE   *+8                                                              
         BAS   RE,DUMP                                                          
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* DELETE JOB RECORD                                                   *         
***********************************************************************         
                                                                                
DELJ     NTR1 ,                                                                 
         CLI   STATJOB,X'FF'       TEST JOB MISSING                             
         BE    XIT                                                              
         XC    DKEY,DKEY                                                        
         LA    R2,DKEY             BUILD TRANSACTION KEY                        
         USING TRNRECD,R2                                                       
         MVC   TRNKCPY,SRTCPY                                                   
         MVC   TRNKUNT(2),=C'SJ'                                                
         MVC   TRNKACT,SRTCPJ                                                   
         MVC   TRNKWORK,=C'**'                                                  
         MVC   TRNKCULC,SRTSUP                                                  
         MVC   TRNKDATE,SRTDTE                                                  
         MVC   TRNKREF,SRTORD                                                   
         BAS   RE,DMRD                                                          
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                RECORD NOT FOUND                             
         LA    R2,IO1                                                           
         BAS   RE,DMGETR                                                        
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                RECORD NOT FOUND                             
         OI    TRNRSTAT,TRNSDELT                                                
         BAS   RE,DMPUTR           DELETE RECORD                                
*                                                                               
         LA    R2,DIR                                                           
         OI    TRNKSTAT,TRNSDELT                                                
         BAS   RE,DMWRTR           DELETE POINTER                               
         AP    CNTMARK,=P'1'                                                    
*                                                                               
         CLI   QOPT4,C'D'                                                       
         BNE   *+8                                                              
         BAS   RE,DUMP                                                          
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* GET COMPANY RECORD                                                 *          
***********************************************************************         
                                                                                
SETCPY   NTR1  ,                                                                
         MVC   LSTCPY,WRKORD+(SRTCPY-SRTREC)                                    
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         MVC   DKEY,SPACES                                                      
         LA    R2,DKEY                                                          
         USING CPYRECD,R2                                                       
         MVC   CPYKCPY,LSTCPY                                                   
         BAS   RE,DMRD                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,IO1                                                           
         BAS   RE,DMGETR                                                        
*                                                                               
         SR    R0,R0                                                            
         LA    R3,CPYRFST                                                       
*                                                                               
         USING NAMELD,R3                                                        
SETCPY3  CLI   NAMEL,NAMELQ        GET NAME ELEMENT                             
         BE    SETCPY5                                                          
         CLI   NAMEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         IC    R0,NAMLN                                                         
         AR    R3,R0                                                            
         B     SETCPY3                                                          
*                                                                               
SETCPY5  MVC   CMPNME,SPACES                                                    
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+4                                                           
         MVC   CMPNME(0),NAMEREC                                                
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* FIND CLOSED BATCHES                                                 *         
***********************************************************************         
                                                                                
CLOSD    NTR1  ,                                                                
         XC    DKEY,DKEY                                                        
         LA    R4,DKEY                                                          
         USING TBARECD,R4                                                       
         MVI   TBAKTYP,TBAKTYPQ    GET BATCH HEADER RECORDS                     
         MVC   TBAKCPY,RCCOMPFL                                                 
*                                                                               
CLOSD1   BAS   RE,DMHGH                                                         
CLOSD2   LA    R4,DIR                                                           
         CLC   0(2,R4),DKEY        TEST BATCH/COMPANY                           
         BNE   XIT                                                              
         OC    TBAKTSEQ,TBAKTSEQ   TEST HEADER                                  
         BNZ   CLOSD9                                                           
         TM    TBAKHSTA,TBAHSEND   TEST CLOSED                                  
         BNO   CLOSD9                                                           
         MVC   DKEY,DIR                                                         
*                                                                               
CLOSD3   BAS   RE,DMSEQ                                                         
         CLC   DIR(TBAKTSEQ-TBARECD),DKEY    TEST SAME BATCH                    
         BNE   CLOSD2                                                           
         LA    R2,IO1                                                           
         BAS   RE,DMGETR           GET THE DETAIL RECORD                        
         LA    R4,IO1                                                           
         LA    R5,TBARFST                                                       
         SR    R0,R0                                                            
*                                                                               
CLOSD5   CLI   0(R5),0                                                          
         BE    CLOSD3                                                           
         CLI   0(R5),BIOELQ        TEST BATCH ITEM ORDER                        
         BNE   CLOSD8                                                           
         USING BIOELD,R5                                                        
         MVC   DUB(6),BIOONUM      ORDER NUMBER                                 
         BAS   RE,ADDO             ADD ORDER TO LIST                            
*                                                                               
CLOSD8   IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     CLOSD5                                                           
*                                                                               
CLOSD9   MVC   DKEY,DIR            READ NEXT BATCH HEADER                       
         LA    R4,DKEY                                                          
         MVC   TBAKTSEQ,=X'FFFF'                                                
         B     CLOSD1                                                           
         DROP  R4,R5                                                            
         EJECT                                                                  
***********************************************************************         
* ADD ORDER ON CLOSED BATCH TO THE LIST                               *         
***********************************************************************         
                                                                                
         USING TBARECD,R4                                                       
ADDO     NTR1  ,                                                                
         L     RF,AORDTAB                                                       
         USING CBOD,RF                                                          
         ICM   R0,15,NUMO         NUMBER IN LIST                                
         BZ    ADDO5                                                            
ADDO3    CLC   CBOCPY,RCCOMPFL    MATCH COMPANY                                 
         BNE   *+14                                                             
         CLC   CBOORD,DUB         TEST ORDER IN LIST                            
         BE    XIT                                                              
         LA    RF,CBOLNQ(RF)                                                    
         BCT   R0,ADDO3                                                         
*                                                                               
ADDO5    MVC   CBOCPY,RCCOMPFL    SAVE NEW ITEM                                 
         MVC   CBOORD,DUB                                                       
         MVC   CBOBTYP,TBAKBTYP   TYPE                                          
         MVC   CBOBREF,TBAKBREF   REFERENCE                                     
         MVC   CBOBITM,TBAKTSEQ   SEQUENCE                                      
         ICM   R0,15,NUMO                                                       
         AH    R0,=H'1'                                                         
         STCM  R0,15,NUMO                                                       
         CH    R0,=Y(MAXCORD)      TEST MAX CLOSED ORDERS                       
         BNH   XIT                                                              
         DC    H'0'                ORDER TABLE IS FULL                          
         DROP  R4,RF                                                            
         EJECT                                                                  
***********************************************************************         
* FIND ORDER IN TABLE OF ORDERS IN CLOSED BATCHES                     *         
***********************************************************************         
                                                                                
FLTORD   NTR1  ,                                                                
         L     RF,AORDTAB                                                       
         USING CBOD,RF                                                          
         ICM   R0,15,NUMO         NUMBER IN LIST                                
         BZ    FLTORDN            NOTHING TABLE                                 
FLTORD3  STCM  RF,15,FULL         RETURN ADDRESS OF ENTRY                       
         CLC   CBOCPY,RCCOMPFL    MATCH COMPANY                                 
         BNE   *+14                                                             
         CLC   CBOORD,DUB         TEST ORDER IN LIST                            
         BE    XIT                                                              
         LA    RF,CBOLNQ(RF)                                                    
         BCT   R0,FLTORD3                                                       
FLTORDN  LTR   RB,RB                                                            
         B     XIT                                                              
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT TOTALS                                                        *         
***********************************************************************         
                                                                                
PRNTOT   NTR1  ,                                                                
         MVI   FORCEHED,C'Y'                                                    
         MVC   CMPNME,SPACES                                                    
         LA    R5,CNTS             COUNT TABLE                                  
*                                                                               
PRNTOT3  CLC   5(20,R5),SPACES     TEST LEAVE A BLANK LINE                      
         BE    PRNTOT4                                                          
         CP    0(5,R5),=P'0'                                                    
         BE    PRNTOT5                                                          
         MVC   P+1(20),5(R5)                                                    
         EDIT  (P5,0(R5)),(7,P+25)                                              
PRNTOT4  BAS   RE,PRNT                                                          
*                                                                               
PRNTOT5  LA    R5,L'CNTS(R5)                                                    
         CLI   0(R5),X'FF'                                                      
         BNE   PRNTOT3                                                          
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PRINT ROUTINE                                                       *         
***********************************************************************         
                                                                                
PRNT     LR    R0,RE                                                            
         MVC   HEAD4+15(L'CMPNME),CMPNME                                        
         GOTO1 ACREPORT                                                         
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* DATA MANAGER ROUTINES                                               *         
***********************************************************************         
                                                                                
DMRD     ST    RE,SAVRE                                                         
         GOTO1 DATAMGR,DMCB,(X'08',DMREAD),ACCDIR,DKEY,DIR                      
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         B     DMERR                                                            
*                                                                               
DMHGH    ST    RE,SAVRE                                                         
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),ACCDIR,DKEY,DIR                      
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         B     DMERR                                                            
*                                                                               
DMSEQ    ST    RE,SAVRE                                                         
         GOTO1 DATAMGR,DMCB,(X'08',DMRSEQ),ACCDIR,DKEY,DIR                      
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         B     DMERR                                                            
*                                                                               
DMGETR   ST    RE,SAVRE                                                         
         GOTO1 DATAMGR,DMCB,(X'08',GETREC),ACCMST,DA,(R2),DMWORK                
         B     DMERR                                                            
*                                                                               
DMWRTR   ST    RE,SAVRE                                                         
         GOTO1 DATAMGR,DMCB,DMWRT,ACCDIR,DIR,DIR                                
         ORG   *-2                                                              
         CLI   RCWRITE,C'Y'                                                     
         BNE   DMERR                                                            
         BASR  RE,RF                                                            
         B     DMERR                                                            
*                                                                               
DMADDR   ST    RE,SAVRE                                                         
         GOTO1 DATAMGR,DMCB,ADDREC,ACCMST,DA,(R2),DMWORK                        
         ORG   *-2                                                              
         CLI   RCWRITE,C'Y'                                                     
         BNE   DMERR                                                            
         BASR  RE,RF                                                            
         B     DMERR                                                            
*                                                                               
DMPUTR   ST    RE,SAVRE                                                         
         GOTO1 DATAMGR,DMCB,PUTREC,ACCMST,DA,(R2),DMWORK                        
         ORG   *-2                                                              
         CLI   RCWRITE,C'Y'                                                     
         BNE   DMERR                                                            
         BASR  RE,RF                                                            
*                                                                               
DMERR    MVC   BYTE,8(R1)                                                       
         NI    BYTE,X'FF'-(X'10'+X'02') IGNORE RNF/RECORD DELETED               
         CLI   BYTE,0                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,SAVRE                                                         
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINES TO DUMP RECORDS                                            *         
***********************************************************************         
                                                                                
DUMP     NTR1  ,                                                                
         AP    DUMPCNT,=P'1'                                                    
         ZAP   DUB,DUMPCNT                                                      
         DP    DUB,EVERY                                                        
         CP    DUB+4(4),=P'0'                                                   
         BNE   XIT                                                              
         AP    PDUMP,=P'1'                                                      
         CP    PDUMP,MAXDUMP                                                    
         BH    XIT                                                              
         LA    R6,=C'DIR'                                                       
         LA    R3,DIR                                                           
         LA    R5,ORDKDA-ORDRECD+L'ORDKDA                                       
         GOTO1 PRNTBL,DMCB,(3,(R6)),(R2),C'DUMP',(R5),=C'2D'                    
*                                                                               
         LA    R6,=C'REC'                                                       
         LA    R2,IO1                                                           
         ICM   R5,3,ORDRLEN-ORDRECD(R2)                                         
         GOTO1 PRNTBL,DMCB,(3,(R6)),(R2),C'DUMP',(R5),=C'2D'                    
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* CONSTANTS                                                                     
***********************************************************************         
                                                                                
HEXIN    DC    V(HEXIN)                                                         
PRNTBL   DC    V(PRNTBL)                                                        
AORDTAB  DC    A(ORDTAB)                                                        
*                                                                               
ACCDIR   DC    CL8'ACCDIR '                                                     
ACCMST   DC    CL8'ACCMST '                                                     
GETREC   DC    CL8'GETREC '                                                     
ADDREC   DC    CL8'ADDREC '                                                     
PUTREC   DC    CL8'PUTREC '                                                     
*                                                                               
PRODUL   DC    C'SJ'                                                            
ORDOPT   DC    XL1'FF'             ORDER FILTER                                 
JOBOPT   DC    XL1'FF'             JOB FILTER                                   
ACCFLN   DC    XL1'00'             LENGTH OF ACCOUNT                            
STRDTE   DC    XL3'00'                                                          
ENDDTE   DC    XL3'FFFFFF'                                                      
*                                                                               
MSOMIS   DC    C'MISSING'                                                       
MSODEL   DC    C'DELETED'                                                       
MSOMCH   DC    C'MATCHED'                                                       
MSOLDE   DC    C'LOGICALLY DELETED'                                             
MSOCON   DC    C'CONTRACT'                                                      
MSOOPN   DC    C'OPEN'                                                          
*                                                                               
CNTS     DS    0XL25                                                            
CNTTOTL  DC    PL5'0',CL20'TOTAL ORDERS'                                        
CNTOPEN  DC    PL5'0',CL20'OPEN'                                                
CNTMTCH  DC    PL5'0',CL20'MATCHED'                                             
CNTDELT  DC    PL5'0',CL20'DELETED'                                             
CNTLDEL  DC    PL5'0',CL20'LOGICALLY DELETED'                                   
CNTCONT  DC    PL5'0',CL20'CONTRACT'                                            
         DC    PL5'0',CL20'               '                                     
CNTSJTOT DC    PL5'0',CL20'SJ TOTAL'                                            
CNTSJDEL DC    PL5'0',CL20'SJ DELETED'                                          
         DC    PL5'0',CL20'               '                                     
CNTMARK  DC    PL5'0',CL20'MARKED DELETED '                                     
         DC    X'FF'                                                            
*                                                                               
DUMPCNT  DC    PL4'0'                                                           
EVERY    DC    PL4'1'                                                           
PDUMP    DC    PL4'0'                                                           
                                                                                
MAXDUMP  DC    PL4'50'                                                          
*                                                                               
NUMO     DC    F'0'                NUMBER IN TABLE                              
*                                                                               
RECCARD  DC    C'RECORD TYPE=F,LENGTH=999 '                                     
SORTCARD DC    C'SORT FIELDS=(999,999,A),FORMAT=BI,WORK=1 '                     
*                                                                               
         EJECT                                                                  
***********************************************************************         
* LITERAL POOL - ORDER TABLE                                                    
***********************************************************************         
                                                                                
         LTORG                                                                  
*                                                                               
MAXCORD  EQU   1000                                                             
*                                                                               
ORDTAB   DS    (MAXCORD)XL(CBOLNQ)                                              
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER WORKING STORAGE                                      *         
***********************************************************************         
                                                                                
ACXOD    DSECT                                                                  
SAVRE    DS    F                                                                
LSTCPY   DS    XL1                 LAST COMPANY CODE                            
ACTV     DS    C                   ACTIVITY SWITCH                              
*                                                                               
MXCPY    EQU   50                                                               
NCPY     DS    XL1                 NUMBER IN COMPANY TABLE                      
CPYTAB   DS    (MXCPY)XL1          LIST OF COMPANIES WITH ORDERS                
*                                                                               
DKEY     DS    CL(L'ACCKEY)        DIRECTORY KEY                                
DIR      DS    CL64                DIRECTORY RECORD                             
DA       DS    XL4                                                              
*                                                                               
CMPNME   DS    CL36                COMPANY NAME                                 
*                                                                               
SRTREC   DS    0XL(SRTLNQ)                                                      
SRTKEY   EQU   *                                                                
SRTCPY   DS    XL1                 COMPANY                                      
SRTORD   DS    CL6                 ORDER NUMBER                                 
SRTCPJ   DS    CL12                CLIENT/PRODUCT/JOB                           
         ORG   SRTCPJ                                                           
SRTCLI   DS    CL3                                                              
SRTPRD   DS    CL3                                                              
SRTJOB   DS    CL6                                                              
SRTDTE   DS    XL3                 DATE                                         
SRTSUP   DS    CL15                SUPPLIER                                     
SRTKLNQ  EQU   *-SRTCPY                                                         
SRTSTA   DS    XL1                 STATUS                                       
SRTSRC   DS    XL1                 RECORD SOURCE                                
SRTSORD  EQU   1                   FROM ORDER                                   
SRTSJOB  EQU   2                   FROM JOB                                     
SRTMLNQ  EQU   *-SRTCPY                                                         
SRTBTYP  DS    XL1                 BATCH TYPE (CLOSED OPTION ONLY)              
SRTBREF  DS    CL5                 BATCH REFERENCE                              
SRTBITM  DS    XL2                 BATCH ITEM                                   
SRTLNQ   EQU   *-SRTCPY                                                         
*                                                                               
WRKORD   DS    CL(SRTLNQ)          ORDER DATA FROM SORT                         
*                                                                               
STATORD  DS    XL1                 STATUS OF ORDER RECORD                       
STATJOB  DS    XL1                 STATUS OF JOB RECORD                         
*                                                                               
IO1      DS    XL2000                                                           
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER PRINT LINE                                           *         
***********************************************************************         
                                                                                
PRNTD    DSECT                                                                  
         DS    XL1                                                              
PRNTORD  DS    CL6                 ORDER                                        
         DS    CL2                                                              
PRNTDTE  DS    CL8                 DATE                                         
         DS    CL2                                                              
PRNTJOB  DS    CL12                JOB                                          
         DS    CL2                                                              
PRNTORDS DS    CL20                ORDER STATUS                                 
         DS    CL2                                                              
PRNTJOBS DS    CL20                JOB STATUS                                   
         DS    CL2                                                              
PRNTBTYP DS    XL2                 BATCH TYPE                                   
         DS    CL1                                                              
PRNTBREF DS    CL4                 BATCH TYPE                                   
         DS    CL1                                                              
PRNTBITM DS    CL4                 BATCH ITEM                                   
***********************************************************************         
* DSECT TO CLOSED BATCH ORDER TABLE                                   *         
***********************************************************************         
                                                                                
CBOD     DSECT                                                                  
CBOCPY   DS    XL1                 COMPANY                                      
CBOORD   DS    CL6                 ORDER NUMBER                                 
CBOBTYP  DS    CL6                 BATCH TYPE                                   
CBOBREF  DS    CL4                 BATCH REFERENCE                              
CBOBITM  DS    XL2                 BATCH ITEM NUMBER                            
CBOLNQ   EQU   *-CBOD                                                           
         EJECT                                                                  
                                                                                
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* ACGENMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
* ACMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
* ACREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'021ACREPXO02 08/16/00'                                      
         END                                                                    
