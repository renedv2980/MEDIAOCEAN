*          DATA SET ACREPUN02  AT LEVEL 040 AS OF 03/29/04                      
*PHASE ACUN02A                                                                  
         TITLE 'UNMARKING REPORT'                                               
ACUN02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACUN**                                                       
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACUND,RC                                                         
         EJECT                                                                  
**********************************************************************          
* RUN FIRST                                                          *          
**********************************************************************          
         SPACE 1                                                                
RUNF     CLI   MODE,RUNFRST                                                     
         BNE   REQF                                                             
         ZAP   RUNTOT,=P'0'                                                     
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* REQUEST FIRST                                                      *          
**********************************************************************          
         SPACE 1                                                                
REQF     CLI   MODE,REQFRST                                                     
         BNE   LDGF                                                             
         MVI   UNDO,UN21                                                        
         CLC   QPROG,=C'21'        SET UNDO TYPE                                
         BE    REQF3                                                            
         MVI   UNDO,UN27                                                        
         CLC   QPROG,=C'27'                                                     
         BE    REQF3                                                            
         MVI   UNDO,UN25                                                        
         CLC   QPROG,=C'25'                                                     
         BE    REQF3                                                            
         MVI   UNDO,UN55                                                        
         CLC   QPROG,=C'55'                                                     
         BE    REQF3                                                            
         DC    H'0'                                                             
*                                                                               
REQF3    TM    UNDO,UNSJ           TEST BILLING RUN                             
         BNO   REQF5                                                            
         CLC   QUNIT(2),=C'SJ'     MUST  BE SJ                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
REQF5    XC    BINNUM,BINNUM        CLEAR BINSRCH TABLE                         
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         ZAP   REQTOT,=P'0'                                                     
         ZAP   CYCTOT,=P'0'                                                     
         GOTO1 DATCON,DMCB,(4,RCDATE),(2,TODAY2)                                
         MVC   BILLDATE,TODAY2     USE TODAY FOR BILL DATE                      
         CLC   QEND,SPACES                                                      
         B     XIT                                                              
         GOTO1 DATCON,DMCB,(0,QEND),(2,BILLDATE) UNLESS THERE'S AN END          
*                                                                               
XIT      XIT1  1                                                                
         EJECT                                                                  
**********************************************************************          
* LEDGER FIRST                                                       *          
**********************************************************************          
         SPACE 1                                                                
LDGF     CLI   MODE,LEDGFRST                                                    
         BNE   PRLA                                                             
         ZAP   LEDGTOT,=P'0'                                                    
         MVI   FORCEHED,C'Y'                                                    
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* PROCESS LEVEL A                                                    *          
**********************************************************************          
         SPACE 1                                                                
PRLA     CLI   MODE,PROCLEVA                                                    
         BNE   PRAC                                                             
         CLI   UNDO,UN27                                                        
         BNE   XIT                                                              
         USING ACTRECD,R2                                                       
         L     R2,ADACC                                                         
         MVC   SAVECLI,ACTKEY+3  SAVE CLIENT                                    
*                                                                               
         USING ACPROFSD,RE                                                      
         L     RE,APROFILE                                                      
         CLC   QAPPL+4(2),SPACES                                                
         BNE   PRLA3                                                            
         MVC   QAPPL+4(2),ACPPFA17                                              
         B     XIT                                                              
*                                                                               
PRLA3    CLI   QAPPL+4,C' '                                                     
         BNE   *+10                                                             
         MVC   QAPPL+4(1),ACPPFA17                                              
         CLI   QAPPL+5,C' '                                                     
         BNE   XIT                                                              
         MVC   QAPPL+5(1),ACPPFA18                                              
         B     XIT                                                              
         DROP  RE                                                               
         EJECT                                                                  
**********************************************************************          
* PROCESS ACCOUNT                                                    *          
**********************************************************************          
         SPACE 1                                                                
PRAC     CLI   MODE,PROCACC                                                     
         BNE   PRTN                                                             
         ZAP   ACCTOT,=P'0'                                                     
         TM    UNDO,UNSJ           TEST PRODUCTION                              
         BNO   XIT                                                              
*                                                                               
         CLI   UNDO,UN27                                                        
         BNE   PRAC03                                                           
*                                                                               
         ZAP   SCIUNET,=P'0'       CLEAR SCIEL BUCKETS                          
         ZAP   SCIUCOM,=P'0'                                                    
*                                                                               
         L     R4,ADGOBLOC                                                      
         USING GOBLOCKD,R4                                                      
         L     R3,AMONACC                                                       
         USING ACMD,R3                                                          
         MVC   GOAKEY,ACMALTN                                                   
         GOTO1 GETOPT,DMCB,ADGOBLOC                                             
         XC    GOAKEY,GOAKEY                                                    
         MVC   JGROUP,GOJG                                                      
*                                                                               
PRAC03   L     RF,ADACCSTA         RESTORE OLD FILTER IF CHANGED                
         USING RSTELD,RF                                                        
         CLC   RSTPBILL,TODAY2     BILLED TODAY                                 
         BNE   XIT                 IF NOT GET OUT                               
         XC    RSTPBILL,RSTPBILL   CLEAR BILLED DATE                            
         MVI   MODE,WRITACC                                                     
         L     R4,ADACC                                                         
         LA    R4,ACCORFST(R4)                                                  
         SR    R0,R0                                                            
*                                                                               
PRAC04   CLI   0(R4),0                                                          
         BE    PRAC11              GET JOB ELEMENT                              
         USING JOBELD,R4                                                        
         CLI   JOBEL,JOBELQ                                                     
         BE    PRAC05                                                           
         IC    R0,JOBLN                                                         
         AR    R4,R0                                                            
         B     PRAC04                                                           
*                                                                               
PRAC05   CLI   JOBLN,JOBLN3Q       LATEST ELEMENT?                              
         BL    PRAC11              NO, LOOK FOR CYCLE                           
         L     RF,ADACCSTA                                                      
         CLI   JOBF2BLB,0            DOES IT HAVE OLD FILTER 2 VALUE            
         BE    *+10                  IF NOT DON'T REPLACE                       
         MVC   RSTFILT1+1(1),JOBF2BLB  REPLACE OLD                              
         L     R3,ADACC                                                         
         LA    R3,ACCORFST(R3)                                                  
         SR    R0,R0                                                            
*                                                                               
PRAC07   CLI   0(R3),0                                                          
         BE    PRAC11              GET JOB ELEMENT                              
         USING PPRELD,R3                                                        
         CLI   PPREL,PPRELQ                                                     
         BE    PRAC09                                                           
         IC    R0,PPRLN                                                         
         AR    R3,R0                                                            
         B     PRAC07                                                           
*                                                                               
PRAC09   CLI   JOBBTBLB,0           DO WE HAVE SAVED BILL TYPE                  
         BE    *+10                 IF NOT DON'T REPLACE                        
         MVC   PPRBTYPE,JOBBTBLB                                                
*                                                                               
         USING JCBRECD,R3                                                       
PRAC11   CLI   UNDO,UN21                                                        
         BNE   XIT                                                              
         LA    R3,MYKEY                                                         
         L     R2,ADACC                                                         
         USING ACTRECD,R2                                                       
         XC    JCBKEY,JCBKEY                                                    
         MVI   JCBKTYP,JCBKTYPQ                                                 
         MVI   JCBKSUB,JCBKSUBQ                                                 
         MVC   JCBKCPY,RCCOMPFL                                                 
         MVC   JCBKUNT(2),QUNIT                                                 
         MVC   JCBKCLI(3),ACTKEY+3                                              
         OC    JCBKCLI,SPACES                                                   
         MVC   JCBKPRO(3),ACTKEY+6                                              
         OC    JCBKPRO,SPACES                                                   
         MVC   JCBKJOB(6),ACTKEY+9                                              
         OC    JCBKJOB,SPACES                                                   
         LA    R4,IOAREA                                                        
         MVC   0(L'JCBKEY,R4),JCBKEY                                            
         BAS   RE,READ             GET CYCLE RECORD, IF ANY                     
         CLC   0(42,R4),0(R3)                                                   
         BNE   XIT                                                              
         DROP  R3,R4                                                            
*                                                                               
         LA    R3,IOAREA                                                        
         LA    R3,ACCORFST(R3)                                                  
         SR    R0,R0                                                            
PRAC13   CLI   0(R3),0                                                          
         BE    XIT                                                              
         USING BCYELD,R3                                                        
         CLI   BCYEL,BCYELQ                                                     
         BE    PRAC17                                                           
PRAC15   IC    R0,BCYLN                                                         
         AR    R3,R0                                                            
         B     PRAC13                                                           
*                                                                               
PRAC17   CLC   BCYBILD,TODAY2      BILLED TODAY ?                               
         BNE   PRAC15                                                           
         XC    BCYBILD,BCYBILD     YES, CLEAR IT OUT                            
         ZAP   BCYESTA,=P'0'       CLEAR ESTIMATE                               
         ZAP   BCYBILA,=P'0'       CLEAR BILLED AMOUNT                          
         MVC   BCYREF,SPACES       AND CLEAR REFERENCE                          
         AP    CYCTOT,=P'1'        ADD TO COUNTER                               
         BAS   RE,WRITE            UPDATE THE RECORD                            
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* PROCESS TRANSACTIONS                                               *          
**********************************************************************          
         SPACE 1                                                                
PRTN     CLI   MODE,PROCTRNS                                                    
         BNE   ACCL                                                             
         LA    R1,AELS             CLEAR ELEMENT LIST                           
         XC    1(4,R1),1(R1)                                                    
         LA    R1,L'AELS(R1)                                                    
         CLI   0(R1),X'FF'         EOT                                          
         BNE   *-14                                                             
*                                                                               
         L     R4,ADTRANS                                                       
         USING TRNELD,R4                                                        
         CLI   TRNEL,X'44'                                                      
         BNE   XIT                                                              
         LR    R2,R4                                                            
         SH    R2,DATADISP                                                      
         USING TRNRECD,R2                                                       
         LR    R3,R4                                                            
         SR    R0,R0                                                            
*                                                                               
PRTN1    CLI   0(R3),0                                                          
         BE    PRTN8                                                            
         LA    R1,AELS             GET ELEMENT ADDRESSES                        
*                                                                               
PRTN2    CLC   0(1,R3),0(R1)       MATCH CODE TO LIST                           
         BE    PRTN3                                                            
         LA    R1,L'AELS(R1)                                                    
         CLI   0(R1),X'FF'         EOT                                          
         BNE   PRTN2                                                            
         B     PRTN4                                                            
*                                                                               
PRTN3    OC    1(4,R1),1(R1)       TEST ALREADY HAVE FIRST ELEMENT              
         BNZ   PRTN4                                                            
         STCM  R3,15,1(R1)         SAVE ELEMENT ADDRESS                         
*                                                                               
PRTN4    IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     PRTN1                                                            
         EJECT                                                                  
**********************************************************************          
* UNDO GENERAL LEDGER UPDATE                                         *          
**********************************************************************          
         SPACE 1                                                                
PRTN8    CLI   UNDO,UN25           GENERAL LEDGER UPDATE                        
         BNE   PRTN11                                                           
         CLC   TRNANAL,=C'**'      BYPASS ORDER RECORDS                         
         BE    XIT                                                              
         ICM   R3,15,AEL60                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         USING TRSELD,R3                                                        
         CLC   TRSUPDT,TODAY2      WAS IT UPDATED TODAY                         
         BNE   XIT                                                              
         TM    TRSSTAT,TRSSGLIP    WAS IT UPDATE VIA $INPUT?                    
         BO    XIT                 YES, CAN'T UNDO IT                           
         CLI   QOPT1,C'R'          REVERSE OPTION                               
         BNE   PRTN9                                                            
         TM    TRSSTAT,X'80'      STATUS BIT MUST BE OFF                        
         BO    XIT                                                              
         OI    TRSSTAT,X'80'      TURN IT BACK ON - UNFORTUNATELY,              
         B     PRTNX               WE HAVE LOST ORIGINAL UPDATE DATE.           
*                                                                               
PRTN9    TM    TRSSTAT,X'80'      NORMAL UPDATE-STATUS BIT MUST BE ON           
         BZ    XIT                                                              
         NI    TRSSTAT,X'7F'      NOW TURN IT OFF                               
         XC    TRSUPDT,TRSUPDT    APPEAR AS IF NEVER TOUCHED-WE DON'T           
         B     PRTNX              KNOW IF IT WAS PREVIOUSLY REVERSED.           
         EJECT                                                                  
**********************************************************************          
* UNDO CHECK RUN                                                     *          
**********************************************************************          
         SPACE 1                                                                
PRTN11   CLI   UNDO,UN55           CHECKS                                       
         BNE   PRTN15                                                           
         CLI   QOPT3,C'U'          FIX ALL UNUSED ITEMS                         
         BNE   *+18                                                             
         OC    TRNRECD+ACCOUSED(2),TRNRECD+ACCOUSED                             
         BNZ   XIT                                                              
         B     *+14                                                             
*                                                                               
         CLC   TRNRECD+ACCOUSED(2),TODAY2  FIX USED TODAY ITEMS                 
         BNE   XIT                                                              
         TM    TRNSTAT,X'80'                                                    
         BO    XIT                 WANT CREDITS FOR CASHPAK                     
         CLC   QUNIT,SPACES                                                     
         BH    *+6                                                              
         DC    H'0'                DUMP IF NO UNIT/LEDGER                       
         CLC   QLEDGER,SPACES                                                   
         BNH   *-6                                                              
*                                                                               
         CLI   QOPT2,C' '          OFFICE                                       
         BE    *+14                                                             
         CLC   QOPT2,TRNANAL                                                    
         BNE   XIT                                                              
         USING TRSELD,R3                                                        
         ICM   R3,15,AEL60                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         TM    TRSSTAT,TRSSACHQ    CHECK WRITTEN BY AC55                        
         BNO   XIT                                                              
         CLC   TRSUSER,ORIGINUM                                                 
         BNE   XIT                                                              
*                                                                               
         USING MPYELD,RF                                                        
         ICM   RF,15,AEL64         CLEAR MANUAL PAYMENT ELEMENT                 
         BZ    PRTN13                                                           
         CLI   QOPT3,C'U'          FIX ALL UNUSED ITEMS                         
         BNE   PRTN12                                                           
         CLC   MPYNO,SPACES                                                     
         BH    PRTN12                                                           
         OC    TRSUMOS,TRSUMOS                                                  
         BNZ   PRTN12                                                           
         OC    TRSUDAT,TRSUDAT                                                  
         BNZ   PRTN12                                                           
         B     XIT                                                              
*                                                                               
PRTN12   MVC   MPYNO,SPACES                                                     
         XC    MPYDTE,MPYDTE                                                    
         ZAP   MPYAMNT,=P'0'                                                    
         MVC   MPYBNK,SPACES                                                    
         CLI   MPYLN,MPYLN2Q                                                    
         BL    *+8                                                              
         MVI   MPYSUB,0                                                         
         CLI   MPYLN,MPYLN3Q                                                    
         BL    *+10                                                             
         ZAP   MPYPART,=P'0'                                                    
*                                                                               
         USING TRSELD,R3                                                        
PRTN13   ICM   R3,15,AEL60                                                      
         NI    TRSSTAT,X'FF'-TRSSACHQ                                           
         XC    TRSUMOS,TRSUMOS                                                  
         XC    TRSUDAT,TRSUDAT                                                  
         XC    TRNRECD+ACCOUSED(2),TRNRECD+ACCOUSED                             
         B     PRTNX                                                            
         EJECT                                                                  
**********************************************************************          
* UNDO AC21 BILLING                                                  *          
**********************************************************************          
         SPACE 1                                                                
PRTN15   CLI   UNDO,UN21           BILLING                                      
         BNE   PRTN21                                                           
         CLC   TRNRECD+ACCOUSED(2),TODAY2                                       
         BNE   XIT                                                              
         CLC   TRNANAL,=C'99'                                                   
         BNE   PRTN16                                                           
         CLC   TRNNARR+37(2),TODAY2     SI POSTED TODAY ?                       
         BNE   XIT                                                              
         XC    TRNNARR+37(2),TRNNARR+37                                         
         B     PRTN20                                                           
*                                                                               
PRTN16   TM    TRNSTAT,X'80'      WANT DEBITS ONLY FOR SJ                       
         BNO   XIT                                                              
         XC    TRNRECD+ACCOUSED(2),TRNRECD+ACCOUSED                             
*                                                                               
         SR    R3,R3                                                            
         ICM   R3,15,AEL4B         FIRST 4B ELEMENT                             
         BZ    PRTN17                                                           
*                                                                               
         USING BNDELD,R3                                                        
         XC    BNDDTE,BNDDTE                                                    
         XC    BNDCMP,BNDCMP                                                    
         XC    BNDBNO,BNDBNO                                                    
         CLI   BNDLN,X'19'                                                      
         BL    PRTN20                                                           
         ZAP   BNDCMP,=P'0'                                                     
         MVI   BNDVATC,X'00'                                                    
         CLI   BNDLN,BNDLN2Q                                                    
         BL    PRTN20                                                           
         MVI   BNDPSTC,X'00'                                                    
         B     PRTN20                                                           
*                                                                               
PRTN17   SR    R3,R3                                                            
         ICM   R3,15,AEL77         FIRST 77 ELEMENT                             
         BZ    XIT                 NO 4B OR 77, DON'T UPDATE                    
*                                                                               
         USING PTAELD,R3                                                        
PRTN18   CLI   PTATYPE,PTATRAL     IS THIS ALLOCATED?                           
         BNE   PRTN19              NO                                           
         TM    PTASTAT1,PTASPEND   YEA, IS IT STILL PENDING?                    
         BO    PRTN19              YES, KEEP LOOKING                            
         CLC   PTARDATE,TODAY2     YES, IS THIS THE DATE WE WANT ?              
         BNE   PRTN19              NO, KEEP LOOKING                             
*                                                                               
         ZAP   PTANET,=P'0'        YES, CLEAR FIELDS                            
         ZAP   PTARCOM,=P'0'                                                    
         ZAP   PTARCORT,=P'0'                                                   
*                                                                               
         XC    PTADATE,PTADATE                                                  
         XC    PTATYPE,PTATYPE                                                  
         XC    PTAMOA,PTAMOA                                                    
         XC    PTACUR,PTACUR                                                    
         XC    PTARDATE,PTARDATE                                                
         XC    PTARBLDT,PTARBLDT                                                
         XC    PTARFORM,PTARFORM                                                
         XC    PTARGSTC,PTARGSTC                                                
         XC    PTARPSTC,PTARPSTC                                                
         XC    PTARPRVC,PTARPRVC                                                
         XC    PTASTAT1,PTASTAT1                                                
*                                                                               
         MVC   HOLDNO,PTARBLNO     SAVE BILL NUMBER                             
         XC    PTARBLNO,PTARBLNO   THEN CLEAR IT                                
         B     PRTN20                                                           
*                                                                               
PRTN19   SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),PTAELQ                                                     
         BE    PRTN18                                                           
         CLI   0(R3),0                                                          
         BNE   PRTN19                                                           
         B     XIT                 NOTHING TO UNMARK                            
*                                                                               
PRTN20   B     PRTNX                                                            
         EJECT                                                                  
**********************************************************************          
* UNDO 27 BILLING RUN                                                *          
**********************************************************************          
         SPACE 1                                                                
PRTN21   CLI   UNDO,UN27           NEW BILLING                                  
         BNE   XIT                                                              
         CLC   TRNANAL,=C'99'                                                   
         BNE   PRTN24                                                           
         CLC   TRNNARR+37(2),TODAY2     SI POSTED TODAY ?                       
         BNE   XIT                                                              
         XC    TRNNARR+37(2),TRNNARR+37                                         
         B     PRTNX                                                            
*                                                                               
PRTN24   TM    TRNSTAT,X'80'       WANT DEBITS ONLY FOR SJ                      
         BZ    XIT                                                              
         CLC   QAPPL(3),SPACES     ANY JOB GROUP ?                              
         BE    PRTN26              NO                                           
         CLC   JGROUP,QAPPL        YES, DOES THIS MATCH ?                       
         BNE   XIT                 NO                                           
*                                                                               
PRTN26   MVC   HOLDNO,SPACES       CLEAR BILL NUMBER HOLD                       
         CLI   QAPPL+3,C' '        ANY PARTICULAR WORKCODE TYPE ?               
         BE    PRTN40              NO                                           
         CLC   TRNANAL,QAPPL+3     YES, DOES THIS MATCH ?                       
         BNE   XIT                 NO                                           
*                                                                               
PRTN40   SR    R3,R3               IF NO 77'S, DO 4B'S                          
         ICM   R3,15,AEL77                                                      
         BZ    PRTN50                                                           
*                                                                               
         USING PTAELD,R3                                                        
PRTN41   CLI   PTATYPE,PTATRAL     IS THIS ALLOCATED?                           
         BNE   PRTN43              NO                                           
         TM    PTASTAT1,PTASPEND   YEA, IS IT STILL PENDING?                    
         BO    PRTN43              YES, KEEP LOOKING                            
         CLC   PTARDATE,TODAY2     YES, IS THIS THE DATE WE WANT ?              
         BNE   PRTN43              NO, KEEP LOOKING                             
         CLC   PTARFORM,QAPPL+4    YES, DOES FORM MATCH                         
         BNE   PRTN43              NO                                           
*                                                                               
         AP    ACCTOT,PTANET       YES, ADD IT TO TOTAL                         
         AP    SCIUNET,PTANET      SAVE AMOUNTS TO UPDATE SCIEL                 
         AP    SCIUCOM,PTARCOM                                                  
*                                                                               
         XC    PTARDATE,PTARDATE   CLEAR FIELDS                                 
         XC    PTARBLDT,PTARBLDT                                                
         XC    PTARFORM,PTARFORM                                                
         XC    PTARGSTC,PTARGSTC                                                
         XC    PTARPSTC,PTARPSTC                                                
         XC    PTARPRVC,PTARPRVC                                                
         OI    PTASTAT1,PTASPEND   MAKE PENDING                                 
*                                                                               
         MVC   HOLDNO,PTARBLNO     SAVE BILL NUMBER                             
         XC    PTARBLNO,PTARBLNO   THEN CLEAR IT                                
         B     PRTN55              IF WE FIND 77'S, DON'T LOOK FOR 4B'S         
*                                                                               
PRTN43   SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),PTAELQ                                                     
         BE    PRTN41                                                           
         CLI   0(R3),0                                                          
         BNE   PRTN43                                                           
*                                                                               
PRTN50   SR    R3,R3               COME HERE NO 77'S OR NO MATCH                
         ICM   R3,15,AEL4B                                                      
         BZ    XIT                                                              
*                                                                               
         USING BNDELD,R3                                                        
PRTN51   CLC   BNDBNO,SPACES       WAS THIS BILLED ?                            
         BNH   PRTN53              NO                                           
         CLC   BNDDTE,TODAY2      YES, IS THIS THE DATE WE WANT ?               
         BNE   PRTN53              NO, KEEP LOOKING                             
         CLC   BNDFORM,QAPPL+4    YES, DOES FORM MATCH                          
         BNE   PRTN53              NO                                           
*                                                                               
         ICM   RF,15,BNDAMNT      YES, ADD IT TO TOTAL                          
         CVD   RF,DUB                                                           
         AP    ACCTOT,DUB                                                       
*                                                                               
         XC    BNDDTE,BNDDTE     CLEAR FIELDS                                   
         NI    BNDCMST,X'FF'-X'80'                                              
         XC    BNDFORM,BNDFORM                                                  
         XC    BNDVATC,BNDVATC                                                  
*                                                                               
         MVC   HOLDNO,BNDBNO       SAVE BILL NUMBER                             
         MVC   BNDBNO,SPACES       AND LEAVE AS ALLOCATED                       
         CLI   BNDLN,X'19'                                                      
         BL    *+10                                                             
         ZAP   BNDCMP,=P'0'                                                     
*                                                                               
         CLI   BNDLN,BNDLN2Q                                                    
         BL    PRTN55                                                           
         MVI   BNDPSTC,0                                                        
         XC    BNDPRVC,BNDPRVC                                                  
         B     PRTN55                                                           
*                                                                               
PRTN53   SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),BNDELQ                                                     
         BE    PRTN51                                                           
         CLI   0(R3),0                                                          
         BNE   PRTN53                                                           
         B     XIT                                                              
*                                                                               
         USING BIND,RE                                                          
PRTN55   LA    RE,BINREC                                                        
         MVC   BININV,HOLDNO                                                    
         MVC   BINCLI,SAVECLI                                                   
         MVC   DMCB(24),BINPARM                                                 
         GOTO1 BINSRCH,DMCB                                                     
         OC    DMCB(4),DMCB        TABLE FULL ?                                 
         BNZ   *+6                 NO                                           
         DC    H'0'                YES                                          
         MVC   BINNUM,DMCB+8       UPDATE COUNT                                 
*                                                                               
         XC    TRNRECD+ACCOUSED(2),TRNRECD+ACCOUSED                             
         B     PRTNXX                                                           
*                                                                               
PRTNX    AP    ACCTOT,TRNAMNT                                                   
PRTNXX   MVI   MODE,WRITRANS                                                    
         B     XIT                                                              
         DROP  RE                                                               
         EJECT                                                                  
**********************************************************************          
* LAST FOR ACCOUNT                                                   *          
**********************************************************************          
         SPACE 1                                                                
ACCL     CLI   MODE,ACCLAST                                                     
         BNE   LDGL                                                             
         CP    ACCTOT,=P'0'                                                     
         BE    XIT                                                              
         CLI   UNDO,UN27                                                        
         BNE   ACCL08                                                           
*                                                                               
         L     R4,ADACC                                                         
         LA    R4,ACCORFST(R4)                                                  
         SR    R0,R0                                                            
*                                                                               
ACCL02   CLI   0(R4),0                                                          
         BE    ACCL08              GET SCIEL                                    
         USING SCIELD,R4                                                        
         CLI   SCIEL,SCIELQ                                                     
         BE    ACCL06                                                           
*                                                                               
ACCL04   IC    R0,SCILN                                                         
         AR    R4,R0                                                            
         B     ACCL02                                                           
*                                                                               
ACCL06   CLI   SCITYPE,SCITCBAP    TYPE ALLOCATED?                              
         BNE   ACCL04              NO                                           
         AP    SCIAMNT,SCIUNET     YES, ADD NET                                 
         CLI   SCILN,SCILN2Q       DO WE HAVE A COMMISSION?                     
         BL    *+10                NO                                           
         AP    SCIADMN,SCIUCOM     YES, ADD TO IT                               
         MVI   MODE,WRITACC                                                     
*                                                                               
ACCL08   L     R4,ADACC                                                         
         MVC   P+1(12),3(R4)                                                    
         L     R4,ADACCNAM                                                      
         LA    R5,P+17                                                          
         BAS   RE,NAMOUT                                                        
         AP    LEDGTOT,ACCTOT                                                   
         LA    R4,ACCTOT                                                        
         BAS   RE,EDAMNT                                                        
         GOTO1 MYREPORT                                                         
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* LAST FOR LEDGER                                                    *          
**********************************************************************          
         SPACE 1                                                                
LDGL     CLI   MODE,LEDGLAST                                                    
         BNE   REQL                                                             
         CP    LEDGTOT,=P'0'                                                    
         BE    XIT                                                              
         MVC   P+40(12),=C'LEDGER TOTAL'                                        
         AP    REQTOT,LEDGTOT                                                   
         LA    R4,LEDGTOT                                                       
         BAS   RE,EDAMNT                                                        
         MVI   SPACING,2                                                        
         GOTO1 MYREPORT                                                         
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* LAST FOR REQUEST                                                   *          
**********************************************************************          
         SPACE 1                                                                
REQL     CLI   MODE,REQLAST                                                     
         BNE   RUNL                                                             
         CLI   UNDO,UN27                                                        
         BNE   REQL5                                                            
         CLI   QAPPL+5,C'0'                                                     
         BE    REQL5                                                            
         L     R0,BINNUM                                                        
         LTR   R0,R0               ANY RECORDS                                  
         BZ    REQL5               NO                                           
         USING BIND,R2                                                          
         LA    R2,BINTAB                                                        
         USING GRBRECD,R3                                                       
         LA    R3,KEY                                                           
*                                                                               
REQL3    XC    GRBKEY,GRBKEY                                                    
         MVI   GRBKTYP,GRBKTYPQ                                                 
         MVI   GRBKSUB,GRBKSUBQ                                                 
         MVC   GRBKCPY,QCOMPANY                                                 
         MVC   GRBKCLI(L'BINCLI),BINCLI                                         
         OC    GRBKCLI,SPACES                                                   
         MVC   GRBKBILN,BININV                                                  
         GOTO1 DATCON,DMCB,(2,TODAY2),(1,GRBKBILD)                              
*                                                                               
         SR    RF,RF               GET 2'S COMPLEMENT                           
         ICM   RF,7,GRBKBILD                                                    
         LNR   RF,RF                                                            
         STCM  RF,7,GRBKBILD                                                    
*                                                                               
         LA    R4,IOAREA                                                        
         MVC   0(L'GRBKEY,R4),GRBKEY                                            
         BAS   RE,READ             GET GROUP BILL RECORD                        
         CLC   0(42,R4),0(R3)                                                   
         BNE   REQL4                                                            
         MVI   GRBRSTA-GRBKEY(R4),X'80'  DELETE IT                              
         BAS   RE,WRITE            UPDATE THE RECORD                            
*                                                                               
REQL4    LA    R2,BINLNG(R2)       GET NEXT TABLE ENTRY                         
         BCT   R0,REQL3                                                         
         DROP  R2,R3                                                            
*                                                                               
REQL5    MVC   P+40(13),=C'REQUEST TOTAL'                                       
         AP    RUNTOT,REQTOT                                                    
         LA    R4,REQTOT                                                        
         BAS   RE,EDAMNT                                                        
         MVI   SPACING,2                                                        
         GOTO1 MYREPORT                                                         
         MVC   P+40(11),=C'CYCLE TOTAL'                                         
         LA    R4,CYCTOT                                                        
         BAS   RE,EDCOUNT                                                       
         MVI   SPACING,2                                                        
         GOTO1 MYREPORT                                                         
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* LAST FOR RUN                                                       *          
**********************************************************************          
         SPACE 1                                                                
RUNL     CLI   MODE,RUNLAST                                                     
         BNE   XIT                                                              
         MVC   P+40(09),=C'RUN TOTAL'                                           
         LA    R4,RUNTOT                                                        
         BAS   RE,EDAMNT                                                        
         MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* SUB-ROUTINES                                                       *          
**********************************************************************          
         SPACE 1                                                                
         USING NAMELD,R4                                                        
NAMOUT   ZIC   RF,NAMLN                                                         
         SH    RF,=H'3'                                                         
         EX    RF,*+6                                                           
         BR    RE                                                               
         MVC   0(0,R5),NAMEREC                                                  
*                                                                               
EDAMNT   EDIT  (P6,0(R4)),(13,P+62),2,MINUS=YES                                 
         ZAP   0(6,R4),=P'0'                                                    
         BR    RE                                                               
*                                                                               
EDCOUNT  EDIT  (P6,0(R4)),(13,P+62),0,MINUS=YES                                 
         ZAP   0(6,R4),=P'0'                                                    
         BR    RE                                                               
*                                                                               
MYREPORT NTR1                                                                   
         MVC   HEAD5+51(8),=C'LIVE RUN'                                         
         CLI   RCWRITE,C'Y'                                                     
         BE    MYREP01                                                          
         MVC   HEAD5+51(9),=C'DRAFT RUN'                                        
MYREP01  MVC   HEAD5+10(1),QLEDGER                                              
         LA    R5,HEAD5+12                                                      
         L     R4,ADLDGNAM                                                      
         BAS   RE,NAMOUT                                                        
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* DATAMGR ROUTINES                                                   *          
**********************************************************************          
         SPACE 1                                                                
HIGH     MVC   COMMAND,=C'DMRDHI'                                               
         B     GTFILE                                                           
         SPACE 1                                                                
READ     MVC   COMMAND,=C'DMREAD'                                               
         B     GTFILE                                                           
         SPACE 1                                                                
SEQ      MVC   COMMAND,=C'DMRSEQ'                                               
         B     GTFILE                                                           
*                                                                               
WRITE    NTR1                                                                   
         CLI   RCWRITE,C'N'                                                     
         BE    XIT                                                              
         MVC   COMMAND,=C'DMWRT '                                               
         B     GTFILE20                                                         
         SPACE 1                                                                
GTFILE   NTR1                                                                   
GTFILE20 LA    R4,IOAREA                                                        
         GOTO1 DATAMGR,DMCB,COMMAND,=C'ACCOUNT',(R4),(R4)                       
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* DATA CONSTANTS                                                     *          
**********************************************************************          
         SPACE 1                                                                
AELS     DS    0XL5                ADDRESSES OF FIRST ELEMENT                   
         DC    X'4B'                                                            
AEL4B    DC    AL4(0)                                                           
         DC    X'60'                                                            
AEL60    DC    AL4(0)                                                           
         DC    X'64'                                                            
AEL64    DC    AL4(0)                                                           
         DC    X'77'                                                            
AEL77    DC    AL4(0)                                                           
         DC    X'FF'                                                            
*                                                                               
BINPARM  DS    0D                                                               
         DC    A(BINREC)           RECORD                                       
         ORG   *-4                                                              
         DC    X'01'                                                            
         ORG   ,                                                                
         DC    A(BINTAB)           TABLE                                        
BINNUM   DC    F'0'                NUMBER OF ENTRIES                            
         DC    AL4(BINLNG)         RECORD LENGTH                                
         DC    AL4(BINKEY)         DISP OF KEY/KEY LENGTH                       
         DC    F'1000'             MAXIMUM NUMBER OF ENTRIES                    
         EJECT                                                                  
**********************************************************************          
* LITERAL POOL                                                       *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* BUFFERS AND TABLES                                                 *          
**********************************************************************          
         SPACE 1                                                                
BINREC   DS    (BINLNG)C                                                        
BINTAB   DS    (BINLNG*200)C                                                    
         EJECT                                                                  
**********************************************************************          
* DSECT FOR LOCAL STORAGE                                            *          
**********************************************************************          
         SPACE 1                                                                
ACUND    DSECT                                                                  
UNDO     DS    XL1                 UNDO TYPE                                    
UNSJ     EQU   X'80'               PRODUCTION                                   
UN21     EQU   X'81'               BILLING - AC21                               
UN27     EQU   X'82'               NEW BILLING - AC27                           
UN25     EQU   X'01'               GL/L UPDATE - AC25                           
UN55     EQU   X'02'               CHECKS - AC55                                
*                                                                               
MYKEY    DS    CL42                                                             
SAVECLI  DS    CL3                                                              
RUNTOT   DS    PL6                                                              
REQTOT   DS    PL6                                                              
CYCTOT   DS    PL6                                                              
LEDGTOT  DS    PL6                                                              
ACCTOT   DS    PL6                                                              
TODAY2   DS    CL2                                                              
BILLDATE DS    CL2                                                              
COMMAND  DS    CL6                                                              
SAVKEY   DS    CL42                                                             
JGROUP   DS    CL3                 JOB GROUP                                    
HOLDNO   DS    CL6                 BILL NUMBER FOR BINSRCH                      
SCIUNET  DS    PL6                 NET AMOUNT TO UPDATE SCIEL                   
SCIUCOM  DS    PL6                 COMMISSION TO UPDATE SCIEL                   
IOAREA   DS    2000C                                                            
         EJECT                                                                  
**********************************************************************          
* OTHER DSECTS                                                       *          
**********************************************************************          
         SPACE 1                                                                
BIND     DSECT                                                                  
BINCLI   DS    CL3                 CLIENT NUMBER (FILLED WITH SPACES)           
BININV   DS    CL6                 INVOICE NUMBER                               
BINKEY   EQU   *-BIND                                                           
BINLNG   EQU   *-BIND                                                           
         EJECT                                                                  
GOBLOCKD DSECT                                                                  
       ++INCLUDE ACGOBLOCK                                                      
         EJECT                                                                  
*ACMASTD                                                                        
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
*ACREPWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
*ACREPPROFD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACREPPROFD                                                     
         PRINT ON                                                               
*ACGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
*ACGENMODES                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'040ACREPUN02 03/29/04'                                      
         END                                                                    
