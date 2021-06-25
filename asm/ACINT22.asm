*          DATA SET ACINT22    AT LEVEL 022 AS OF 05/09/08                      
*PHASE T61922A                                                                  
*INCLUDE TWANG                                                                  
         TITLE 'T61922 - CHECK UPDATE/DRAFT/FILTER'                             
T61922   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T61922**,R7,RR=R2                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          SYSTEM SPECIFIC WORK                         
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         ST    R2,RELO                                                          
         SPACE 1                                                                
*                                                                               
* OPEN PRINT QUEUE, PRINT THE TWA, INITIALIZE FOR UPDATE                        
*                                                                               
UP       LA    RE,OVERWRK          CLEAR LOCAL WORKING STORAGE                  
         LA    RF,L'OVERWRK                                                     
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
UP1      MVC   REMUSER,PERSON      SUPPLY REQUESTOR ID                          
         GOTO1 OPENPQ                                                           
*                                                                               
         GOTO1 =V(TWANG),DMCB,ATWA,AIO3,0,RR=RELO                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         GOTO1 (RF),(R1),(R8)      SKIP TWO LINES                               
         MVI   P+1,C'*'                                                         
         MVC   P+2(82),P+1                                                      
         GOTO1 SPOOL,DMCB,(R8)                                                  
         L     R2,AIO3             R2=A(OUTPUT AREA)                            
         LA    R3,24               R3=LOOP COUNTER                              
*                                                                               
UP2      MVI   P+1,C'*'                                                         
         MVC   P+3(80),0(R2)       PRINT THE SCREEN IMAGE                       
         MVI   P+83,C'*'                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R2,80(R2)                                                        
         BCT   R3,UP2                                                           
*                                                                               
         MVI   P+1,C'*'            CLOSE THE SCREEN OFF                         
         MVC   P+2(82),P+1                                                      
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   FORCEHED,C'Y'                                                    
         LA    R1,HEDSPECS         NOW SET SPECS                                
         ST    R1,SPECS                                                         
         LA    R1,HOOK             AND HEADLINE HOOK                            
         ST    R1,HEADHOOK                                                      
         MVI   RCSUBPRG,0          SET SUB-PROGRAM                              
         CLI   ACTNUM,ACTNUPD      TEST ACTION=UPDATE                           
         BE    UP4                                                              
         MVI   RCSUBPRG,1                                                       
         CLI   ACTNUM,ACTNDFT                                                   
         BE    *+8                                                              
         MVI   RCSUBPRG,2                                                       
*                                                                               
UP4      LA    RE,TRNBLK                                                        
         LA    RF,TRNBLKL          CLEAR ADDTRN BLOCK                           
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         ZAP   CREDITS,=P'0'                                                    
         BAS   RE,BLDNAR           BUILD CHECK NARRATION                        
         GOTO1 DATCON,DMCB,(0,CHKDATE),(1,CHKDATP)                              
         GOTO1 (RF),(R1),(0,BANKDATE),(1,BANKDATP)                              
*                                                                               
UP5      L     R1,ATSARBLK                                                      
         USING TSARD,R1                                                         
         MVI   TSACTN,TSAGET                                                    
         MVC   TSRNUM,=H'1'        GET THE FIRST RECORD                         
         GOTO1 TSAR                                                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* SAVE OFF CLIENT CODE AND NAME FOR HEADER                                      
*                                                                               
         MVC   CLIENT,TSARCLT                                                   
         SR    R0,R0                                                            
         IC    R0,CLINUM                                                        
         USING CLITABD,R1                                                       
         L     R1,ACLITAB          R1=A(CLIENT CODE TABLE)                      
         CLC   CLIENT,CLICDE                                                    
         BE    *+14                                                             
         LA    R1,CLILNQ(R1)                                                    
         BCT   R0,*-14                                                          
         DC    H'0'                MUST BE THERE!                               
*                                                                               
         MVC   CLINAME,CLINM       SAVE OFF NAME                                
         DROP  R1                                                               
*                                                                               
* TEST IF ANY MONTHS WERE ALLOCATED FOR THE ESTIMATE                            
*                                                                               
UP10     CLI   ACTNUM,ACTNFILT     TEST FOR ACTION=FILTER                       
         BNE   UP12                                                             
         GOTO1 AFILTER             YES-APPLY FILTERS                            
         BNE   UP40                REJECT ITEM                                  
*                                                                               
UP12     LA    R1,TSARPAID         TEST IF PAYMENTS WERE MADE                   
         ZIC   R0,ADVNUM                                                        
         CP    0(L'TSARPAID,R1),=P'0'                                           
         BNE   UP20                YES                                          
         LA    R1,L'TSARPAID(R1)                                                
         BCT   R0,*-14                                                          
         B     UP40                NEXT TSAR RECORD                             
*                                                                               
* READ THE ESTIMATE AND INITIALIZE TO HANDLE POSTINGS FOR IT                    
*                                                                               
UP20     LA    R4,KEY              READ THE ESTIMATE                            
         USING ACKEYD,R4                                                        
         MVC   ACINKEY,SPACES                                                   
         MVI   ACINCOD,ACINEQU                                                  
         MVI   ACINSREC,ACINSEQU                                                
         MVC   ACINCUL(L'RECEIVE),RECEIVE                                       
         MVC   ACINCLT,TSARCLT                                                  
         MVC   ACINPRD,TSARPROD                                                 
         MVC   ACINMED,TSARMED                                                  
         MVC   ACINEST,TSAREST                                                  
         CLI   ACTNUM,ACTNUPD      TEST ACTION=UPDATE                           
         BNE   *+8                                                              
         MVI   RDUPDATE,C'Y'       YES-READ ESTIMATE FOR UPDATE                 
         GOTO1 HIGH                                                             
         CLC   ACINKEY,KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
UP25     LH    R1,NUMESTS          INCREMENT COUNT OF ESTS W PAYMENTS           
         LA    R1,1(R1)                                                         
         STH   R1,NUMESTS                                                       
         LA    R2,P                PRINT ESTIMATE DATA                          
         USING JOURNALD,R2                                                      
         MVC   JOPRD,TSARPROD                                                   
         MVC   JOEST,TSAREST                                                    
         MVC   JOOFFC,TSAROFFC                                                  
         MVC   JOMED(2),TSARMED                                                 
         XC    DUB,DUB             DUB=SEARCH PARMS FOR GETMED                  
         MVC   DUB+1(2),TSARMED                                                 
         L     R4,AIO1             POINT TO THE ESTIMATE                        
         TM    ACSTATUS,X'02'      TEST FOR MI ESTIMATE                         
         BNO   UP27                YES                                          
*                                                                               
         MVC   JOMED(3),=C'MI='                                                 
         MVC   JOMED+3(2),TSARMED                                               
         MVI   DUB,1               NOTE MEDIA INTERFACE                         
*                                                                               
UP27     CLC   LASTMED,DUB         TEST FOR CHANGE IN MEDIA VALUE               
         BE    *+14                                                             
         BAS   RE,GETMED                                                        
         MVC   LASTMED,DUB                                                      
*                                                                               
         MVC   JOCAC(12),CONTRA+3                                               
*                                                                               
* GENERATE POSTINGS FOR EACH MONTH ALLOCATED DURING BATCH                       
*                                                                               
UP30     LA    R5,TSARPAID         R5=A(TSAR PAID BUCKETS)                      
         LA    R3,ADVTAB           R3=A(ADV MONTH TABLE)                        
         ZIC   R2,ADVNUM           R2=LOOP COUNTER                              
         ZAP   ESTPAY,=P'0'        CLEAR BUCKET FOR ESTIMATE                    
*                                                                               
UP32     CP    0(L'TSARPAID,R5),=P'0' TEST PAYMENT FOR MONTH                    
         BE    UP36                NO                                           
*                                                                               
UP34     GOTO1 HELLO,DMCB,(C'G',SYSFIL),('ACIESEQU',AIO1),(2,0(R3))             
         CLI   12(R1),0            TEST IF MONTH ELEMENT FOUND                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,12(R1)                                                        
         USING ACINESTD,R6                                                      
         ZAP   THISPAY,0(L'TSARPAID,R5) SET PAID AMOUNT                         
         BAS   RE,BLDTRN           BUILD AND ADD TRANSACTION                    
         BAS   RE,PRTPOST                                                       
         OC    ACIESDTO,ACIESDTO   IS THERE AN ORIGINAL POSTING DATE?           
         BNZ   UP34A               YES-KEEP IT,IF NO CARRY DEPOSIT DATE         
         GOTO1 DATCON,DMCB,(1,BANKDATP),(2,ACIESDTO)                            
         MVC   ACIESDAT,ACIESDTO   SAVE ORIGINAL IN REVISED TOO                 
         OI    ACIESTAT,X'80'      MARK IT POSTED                               
UP34A    AP    ACIESPD,THISPAY     UPDATE PAID BUCKET                           
         AP    ESTPAY,THISPAY      UPDATE ESTIMATE BUCKET                       
         AP    CREDITS,THISPAY     UDPATE TOTAL CREDITS BUCKET                  
         LH    R1,NUMITEMS         INCREMENT COUNT OF POSTINGS                  
         LA    R1,1(R1)                                                         
         STH   R1,NUMITEMS                                                      
*                                                                               
UP36     LA    R5,L'TSARPAID(R5)   NEXT MONTH AND BUCKET                        
         LA    R3,L'ADVTAB(R3)                                                  
         BCT   R2,UP32                                                          
*                                                                               
         LA    R2,P                                                             
         USING JOURNALD,R2                                                      
         MVC   JOMSG(L'THISEMSG),THISEMSG                                       
         CURED (P6,ESTPAY),(L'JOCR,JOCR),2,MINUS=YES,ZERO=NOBLANK               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         GOTO1 (RF),(R1),(R8)                                                   
*                                                                               
* UPDATE CASH OFFICE BUCKETS AND WRITE BACK THE UPDATED ESTIMATE                
*                                                                               
UP38     BAS   RE,UPCASH           UPDATE CASH OFFICE BUCKETS                   
         CLI   ACTNUM,ACTNUPD      TEST FOR ACTION=UPDATE                       
         BNE   UP40                                                             
         MVC   AIO,AIO3                                                         
         MVI   RDUPDATE,C'Y'       RE-READ THE ESTIMATE INTO IO3                
         GOTO1 HIGH                                                             
         CLC   KEY(L'ACINKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO1            WRITE IT BACK FROM IO1                       
         GOTO1 WRITE                                                            
*                                                                               
* GET NEXT TSAR RECORD UNTIL EOF IS ENCOUTERED                                  
*                                                                               
UP40     L     R1,ATSARBLK                                                      
         USING TSARD,R1                                                         
         MVI   TSACTN,TSANXT       GET NEXT RECORD                              
         GOTO1 TSAR                                                             
         BE    UP10                HAVE ANOTHER ONE                             
         CLI   TSERRS,TSEEOF+TSERNF  TEST FOR EOF                               
         BE    UP50                                                             
         DC    H'0'                                                             
*                                                                               
UP50     LA    R2,P                                                             
         USING JOURNALD,R2                                                      
         MVC   JOMSG(L'ESTMSG),ESTMSG                                           
         LH    R0,NUMESTS                                                       
         EDIT  (R0),(6,JOCR)                                                    
         GOTO1 SPOOL,DMCB,(R8)                                                  
         GOTO1 (RF),(R1),(R8)                                                   
         MVC   JOMSG(L'RECVMSG),RECVMSG                                         
         LH    R0,NUMITEMS                                                      
         EDIT  (R0),(6,JOCR)                                                    
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         CP    TOTPAID,CHKAMT      TEST BATCH IS PAYABLE                        
         BE    UP52                YES                                          
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   JOMSG(L'WARNMSG),WARNMSG                                         
         CURED TOTPAID,(L'JOCR,JOCR),2,MINUS=YES                                
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
UP52     CLI   ACTNUM,ACTNUPD                                                   
         BNE   UP60                                                             
*                                                                               
         MVI   FORCEHED,C'Y'       BREAK A PAGE                                 
         MVI   RCSUBPRG,3          CHANGE HEADINGS                              
         BAS   RE,CASH             GENERATE CASH POSTINGS                       
         MVC   TRNREC,AIO2                                                      
         MVC   TRNCOMF,ACOMFACS                                                 
         OI    TRNINDS2,TRNIUPDG   Update GL posting                            
         OI    TRNINDS,TRNILAST    LAST TIME CALL TO ADDTRN                     
         GOTO1 ADDTRN,TRNBLK                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* UPDATE THE BATCH HEADER FOR THE CHECK                                         
*                                                                               
UP60     CLI   ACTNUM,ACTNUPD      TEST FOR ACTION=UPDATE                       
         BNE   UP65                NO                                           
*                                                                               
         LA    R4,KEY              UPDATE BATCH HEADER                          
         USING ACBKEYD,R4                                                       
         XC    ACBKEY(ACBKEYL),ACBKEY                                           
         MVC   ACBKEY+ACBKEYL(L'ACBKEY-ACBKEYL),SPACES                          
         MVI   ACBKCODE,X'0B'                                                   
         MVC   ACBKCOMP,COMPANY                                                 
         MVC   ACBKOFF,TWAORIG                                                  
         MVI   ACBKGRUP,C'G'       GENERAL ACCOUNTING                           
         MVI   ACBKTYPE,30         BATCH TYPE 30                                
         MVC   ACBKDATE,TODAYP                                                  
         MVC   ACBKREF,BATMON                                                   
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         CLC   ACBKEYD(42),KEYSAVE                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   ELCODE,ACBHELQ      GET BATCH HEADER ELEMENT                     
         GOTO1 REMELEM                                                          
         LA    R6,ELEM                                                          
         USING ACBTCHD,R6                                                       
         CLI   ACBHEL,ACBHELQ                                                   
         BE    *+6                                                              
         DC    H'0'                MUST FIND IT                                 
         MVI   ACBHLEN,ACBHLNQ                                                  
         LH    R0,NUMITEMS                                                      
         CVD   R0,DUB                                                           
         ZAP   ACBHITEM,DUB        SET ITEM COUNT                               
         GOTO1 ADDELEM                                                          
*                                                                               
         L     R4,AIO                                                           
         USING ACKEYD,R4                                                        
         MVI   ACSTATUS,X'30'      SET BEU AND RECOVERY                         
         GOTO1 WRITE                                                            
*                                                                               
UP65     MVI   SPMODE,X'FF'        CLOSE THE PRINT QUEUE                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         MVC   LISTAR,SPACES                                                    
         LA    R4,LISTAR           R4=OUTPUT POINTER                            
         CLI   ACTNUM,ACTNUPD      TEST ACTION=UPDATE                           
         BNE   UP68                                                             
*                                                                               
         MVC   0(L'UPCMSG,R4),UPCMSG                                            
         LA    R4,L'UPCMSG+1(R4)                                                
         MVI   0(R4),C'-'                                                       
         LA    R4,2(R4)                                                         
*                                                                               
UP68     MVC   2(3,R4),SPOOLID                                                  
         MVI   5(R4),C','                                                       
         SR    R0,R0                                                            
         ICM   R0,3,SPOOLRPN                                                    
         EDIT  (R0),(5,6(R4)),ALIGN=LEFT                                        
         AR    R4,R0                                                            
         LA    R4,7(R4)                                                         
*                                                                               
         MVC   0(L'PQMSG,R4),PQMSG                                              
         LA    R2,CONHEADH                                                      
         BAS   RE,MOVEFLD                                                       
*                                                                               
UPX      XMOD1 1                                                                
         EJECT                                                                  
* SUB-ROUTINE TO BUILD STANDARD NARRATION FOR POSTINGS                          
* ON EXIT, LISTAR CONTAINS THE NARRATIVE AND LNARR ITS LENGTH                   
*                                                                               
BLDNAR   NTR1  ,                                                                
         MVC   LISTAR,SPACES                                                    
         MVC   LISTAR(12),=C'CHECK NUMBER'                                      
         LA    R4,LISTAR+13                                                     
         MVC   0(L'CHKNUM,R4),CHKNUM                                            
         LA    R4,L'CHKNUM+1(R4)                                                
*                                                                               
         MVC   0(5,R4),=C'DATED'                                                
         LA    R4,6(R4)                                                         
         GOTO1 DATCON,DMCB,(0,CHKDATE),(8,(R4))                                 
         LA    R4,9(R4)                                                         
*                                                                               
         MVC   0(12,R4),=C'DEPOSITED ON'                                        
         LA    R4,13(R4)                                                        
         GOTO1 DATCON,DMCB,(0,BANKDATE),(8,(R4))                                
         LA    R4,8(R4)                                                         
*                                                                               
         LA    RE,LISTAR                                                        
         SR    R4,RE               COMPUTE LENGTH OF STRING                     
         GOTO1 SQUASHER,DMCB,LISTAR,(R4)                                        
         MVC   LNARR,DMCB+7        SAVE LENGTH OF NARRATION                     
*                                                                               
BLDNARX  B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO GET THE CONTRA ACCOUNT FOR A MEDIA                             
* ON ENTRY, DUB=SEARCH PARAMETERS                                               
*                                                                               
* ROUTINE TRIES TO OPTIMIZE IO BY KEEPING A MEDIA TABLE                         
* ON EXIT, CONTRA=RECEIVABLE CONTRA-ACCOUNT                                     
*                                                                               
GETMED   NTR1  ,                                                                
         MVC   CONTRA,SPACES                                                    
         LA    R2,MEDTAB           R2=A(MEDIA TABLE)                            
         USING MEDTABD,R2                                                       
         LA    R0,MEDTBMAX         R0=LOOP COUNTER                              
*                                                                               
GETMED1  OC    0(3,R2),0(R2)       TEST FOR EOT                                 
         BZ    GETMED2             YES                                          
         CLC   DUB,0(R2)           MATCH ON MEDIA                               
         BE    GETMED8             YES-ALREADY HAVE MEDIA                       
         LA    R2,L'MEDTAB(R2)                                                  
         BCT   R0,GETMED1                                                       
         SH    R2,=Y(L'MEDTAB)     RE-USE THE LAST ENTRY                        
*                                                                               
GETMED2  MVC   SAVEKEY1,KEY                                                     
         MVC   AIO,AIO3            USE IO3 TO READ RECORDS                      
         CLI   DUB,0               TEST FOR REGULAR MEDIA                       
         BE    GETMED6             YES                                          
*                                                                               
         MVC   KEY,SPACES          READ MEDIA INTERFACE RECORD                  
         MVI   KEY,X'08'                                                        
         MVC   KEY+1(1),COMPANY                                                 
         MVC   KEY+2(2),TSARMED                                                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(42),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
GETMED4  MVI   ELCODE,ACMIELQ                                                   
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACMID,R6                                                         
         MVI   MEDTYPE,1           NOTE MEDIA INTERFACE                         
         MVC   MEDCODE,TSARMED                                                  
         MVC   MEDDESC,ACMIDESC    EXTRACT DESCRIPTION                          
         B     GETMED8                                                          
*                                                                               
GETMED6  MVC   KEY,SPACES          READ INCOME A/C FOR MEDIA                    
         LA    R4,KEY                                                           
         USING ACKEYD,R4                                                        
         MVC   ACKEYACC(1),COMPANY                                              
         MVC   ACKEYACC+1(2),=C'SI'                                             
         MVC   ACKEYACC+3(2),TSARMED PLUG IN MEDIA                              
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(42),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
GETMED7  MVI   ELCODE,ACNMELQ                                                   
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACNAMED,R6                                                       
         MVI   MEDTYPE,0                                                        
         MVC   MEDCODE,TSARMED                                                  
         ZIC   R3,ACNMLEN                                                       
         SH    R3,=Y(ACNMNAME-ACNAMED)                                          
         GOTO1 CHOPPER,DMCB,((R3),ACNMNAME),(L'MEDDESC,MEDDESC),(0,1)           
*                                                                               
GETMED8  MVC   CONTRA+3(12),MEDDESC                                             
         MVC   AIO,AIO1            RESTORE IO AREA POINTER                      
         MVC   KEY(L'SAVEKEY1),SAVEKEY1 RESTORE KEY                             
*                                                                               
GETMEDX  B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* SUB-ROUTINE TO BUILD A TRANSACTION RECORD FOR A RECEIVABLES POSTING           
*                                                                               
* AT ENTRY, R6=A(ESTIMATE MONTH ELEMENT) AND THISPAY=PAID AMOUNT                
***********************************************************************         
BLDTRN   NTR1  ,                                                                
         USING ACINESTD,R6                                                      
         MVC   AIO,AIO2            USE IO2 FOR THE TRANSACTION                  
         L     R4,AIO                                                           
         USING ACKEYD,R4                                                        
         LR    RE,R4                                                            
         L     RF,SIZEIO                                                        
         SR    R1,R1                                                            
         MVCL  RE,R0               CLEAR IO AREA                                
*                                                                               
         MVC   ACKEYD(ACLENGTH-ACKEYD),SPACES                                   
         MVC   ACKEYACC,RECEIVE                                                 
         MVC   ACKEYCON,CONTRA                                                  
         MVC   ACKEYREF,TSAREST    REFERENCE=ESTIMATE                           
         MVC   ACKEYDTE,BANKDATP   POSTING DATE IS DEPOSIT DATE UNLESS          
         MVI   ACKEYSBR,0                                                       
         MVC   ACLENGTH,=Y(ACRECORD+1-ACKEYD)                                   
*                                                                               
         OC    ACIESDTO,ACIESDTO   TEST FOR ORIGINAL POSTING DATE               
         BZ    BLDTRN2                                                          
         GOTO1 DATCON,DMCB,(2,ACIESDTO),(1,ACKEYDTE)                            
*                                                                               
BLDTRN2  XC    ELEMENT,ELEMENT                                                  
         LA    R3,ELEMENT                                                       
         USING TRANSD,R3                                                        
         MVI   TRNSEL,TRNSELQ                                                   
         MVI   TRNSLEN,TRNSLNQ                                                  
         MVC   TRNSDATE,ACKEYDTE                                                
         MVC   TRNSREF,TSAREST     REFERENCE=ESTIMATE                           
         MVI   TRNSTYPE,30         TYPE=30                                      
         MVC   TRNSBTCH,BATMON                                                  
         ZAP   TRNSAMNT,THISPAY                                                 
         MVC   TRNSOFFC,TSAROFFC                                                
         MVC   TRNSNARR(L'LISTAR),LISTAR                                        
         ZIC   R1,TRNSLEN                                                       
         ZIC   RE,LNARR                                                         
         AR    R1,RE                                                            
         STC   R1,TRNSLEN          UPDATE ELEMENT LENGTH                        
         GOTO1 ADDELEM                                                          
*                                                                               
BLDTRN4  XC    ELEMENT,ELEMENT                                                  
         LA    R3,ELEMENT                                                       
         USING ACMTD,R3                                                         
         MVI   ACMTEL,ACMTELQ      BUILD A MEDIA TRANSFER ELEMENT               
         MVI   ACMTLEN,ACMTLNQ                                                  
         MVI   ACMTSYS,C'I'        SYSTEM=INTERAGENCY                           
         MVC   ACMTCLI,TSARCLT                                                  
         MVC   ACMTPRD,TSARPROD                                                 
         MVC   ACMTMED2,TSARMED    SET INTERAGENCY MEDIA                        
         MVC   ACMTEST,TSAREST                                                  
         L     RE,AIO1             POINT AT ESTIMATE RECORD                     
         TM    ACSTATUS-ACKEYD(RE),X'02' TEST FOR MI ESTIMATE                   
         BZ    *+8                 NO                                           
         OI    ACMTSTAT,X'02'      YES-NOTE IN ELEMENT                          
         MVC   ACMTMOS,ACIESMTH    ADVERTISING MONTH OF SERVICE                 
         GOTO1 HELLO,DMCB,(C'G',SYSFIL),('ACIPFEQU',AIO1),0                     
         CLI   12(R1),0            BETTER FIND PROFILE ELEMENT                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,12(R1)                                                        
         USING ACINPRFD,RE                                                      
         MVC   ACMTDSCP,ACIPFDES   ESTIMATE DESCRIPTION                         
         GOTO1 HELLO,DMCB,(C'P',SYSFIL),AIO,ELEM,(0,=C'ADD=END')                
*                                                                               
         USING ACRAD,R3                                                         
BLDTRN5  XC    ACRAD(ACRAALNQ),ACRAD BUILD RCV ALLOCATION ELEM                  
         MVI   ACRAEL,ACRAELQ                                                   
         MVI   ACRALEN,ACRAALNQ                                                 
         MVI   ACRATYPE,ACRATALC   TYPE=ALLOCATION                              
         MVC   ACRAAREF,CHKNUM     CHECK NUMBER                                 
         MVC   ACRAADAT,CHKDATP    CHECK DATE                                   
         MVC   ACRAADEP,BANKDATP   DEPOSIT DATE                                 
         GOTO1 HELLO,DMCB,(C'P',SYSFIL),AIO,ELEM,(0,=C'ADD=END')                
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         LA    R3,ELEMENT                                                       
         USING APEELD,R3                                                        
         MVI   APEEL,APEELQ        BUILD AN ANALYSIS ELEMENT                    
         MVI   APELN,APELN1Q                                                    
         MVI   APENUM,0                                                         
                                                                                
         SR    RE,RE                                                            
         IC    RE,APELN                                                         
         LA    RE,APEELD(RE)                                                    
         USING APENTRY,RE          RE=A(APEEL ENTRY)                            
         MVI   APENSTAT,X'80'                                                   
         MVC   APENACT,BANK+1                                                   
         LA    RF,APENACT+L'APENACT-1                                           
         CLI   0(RF),C' '          LOCATE END OF ACCOUNT CODE                   
         BH    *+12                                                             
         MVI   0(RF),0             AND DROP TRAILING SPACES                     
         BCT   RF,*-12                                                          
         AHI   RF,1                                                             
         SR    RF,RE               RF=L'SUB-ELEMENT                             
         STC   RF,APENLEN                                                       
                                                                                
         SR    RE,RE               INCREMENT ELEMENT LENGTH                     
         IC    RE,APELN                                                         
         AR    RE,RF                                                            
         STC   RE,APELN                                                         
         MVI   APENUM,1            SET NUMBER OF SUB-ELEMENTS                   
                                                                                
         GOTO1 HELLO,DMCB,(C'P',SYSFIL),AIO,ELEM,(0,=C'ADD=END')                
*                                                                               
BLDTRN6  MVC   TRNCTRY,AGYCTRY                                                  
         MVC   TRNCOMF,ACOMFACS                                                 
         MVC   TRNCPYS1(4),COMPSTA1 COMPANY STATUS BYTES                        
         MVC   TRNCPYS5,COMPSTA5                                                
         MVC   TRNCPYS6,COMPSTA6                                                
         MVC   TRNCPYS7,COMPSTA7                                                
         MVC   TRNCPYS8,COMPSTA8                                                
         MVC   TRNCPYS9,COMPSTA9                                                
         MVC   TRNCPYSA,COMPSTAA                                                
         MVC   TRNGLMOA,COMPGMOA                                                
*                                                                               
         LA    R0,PALAREA                                                       
         STCM  R0,15,TRNPAL                                                     
*                                                                               
         MVC   TRNLDG,ARCVLEDG     A(LEDGER TABLE ENTRY)                        
         MVI   TRN#LDGS,6          SET NUMBER OF LEDGER TABLE ENTRIES           
         MVC   TRNREC,AIO2                                                      
         MVC   TRNPUSER,TWAORIG                                                 
         SR    RE,RE                                                            
         ICM   RE,3,TRNBSEQN                                                    
         LA    RE,1(RE)                                                         
         STCM  RE,3,TRNBSEQN                                                    
         MVC   TRNBMOS,BATMONP                                                  
*                                                                               
BLDTRN8  CLI   ACTNUM,ACTNUPD      ONLY ADD TRANSACTIONS IF                     
         BNE   BLDTRN10            ACTION=UPDATE                                
         OI    TRNINDS2,TRNIADDG   ADD GL POSTING                               
         GOTO1 ADDTRN,TRNBLK                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
BLDTRN10 MVC   AIO,AIO1                                                         
*                                                                               
BLDTRNX  B     XIT                                                              
         DROP  R3,RE                                                            
         EJECT                                                                  
***********************************************************************         
* SUB-ROUTINE TO GENERATE THE CASH POSTINGS                                     
***********************************************************************         
CASH     NTR1  ,                                                                
         MVC   AIO,AIO2                                                         
         L     R4,AIO                                                           
         USING ACKEYD,R4                                                        
         LR    RE,R4                                                            
         L     RF,SIZEIO                                                        
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         MVC   ACKEYD(ACLENGTH-ACKEYD),SPACES                                   
         MVC   ACKEYACC,BANK                                                    
         MVC   ACKEYCON,RECEIVE                                                 
         MVC   ACKEYREF,CHKNUM                                                  
         MVC   ACKEYDTE,BANKDATP                                                
         MVI   ACKEYSBR,0                                                       
         MVC   ACLENGTH,=Y(ACRECORD+1-ACKEYD)                                   
*                                                                               
CASH2    XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT                                                       
         USING TRANSD,R6                                                        
         MVI   TRNSEL,TRNSELQ                                                   
         MVI   TRNSLEN,TRNSLNQ                                                  
         MVC   TRNSDATE,ACKEYDTE                                                
         MVC   TRNSREF,ACKEYREF                                                 
         MVI   TRNSTYPE,30         TYPE 30 TRANSACTIONS                         
         OI    TRNSSTAT,X'80'      DEBIT TO CASH                                
         MVC   TRNSBTCH,BATMON                                                  
         MVC   TRNSNARR(L'LISTAR),LISTAR                                        
         ZIC   R1,TRNSLEN                                                       
         ZIC   RE,LNARR                                                         
         AR    R1,RE                                                            
         STC   R1,TRNSLEN                                                       
         GOTO1 HELLO,DMCB,(C'P',SYSFIL),AIO,ELEM,0                              
*                                                                               
CASH4    LA    R5,CASHTAB          POINT AT CASH OFFICE TABLE                   
         LA    R0,MAXCASH          R0=LOOP COUNTER                              
*                                                                               
CASH6    OC    0(2,R5),0(R5)       TEST EOT                                     
         BZ    CASH10                                                           
*                                                                               
         MVI   ACKEYSBR,0                                                       
         MVI   ELCODE,TRNSELQ      UPDATE THE TRANSACTION ELEM                  
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   TRNSOFFC,0(R5)      GET OFFICE FROM TABLE                        
         ZAP   TRNSAMNT,2(8,R5)                                                 
*                                                                               
         LA    R2,P                                                             
         USING JOURNALD,R2                                                      
         MVC   JOCASH,ACKEYACC+1   PRINT CASH POSTING DETAIL                    
         MVC   JORECV,ACKEYCON+1                                                
         MVC   JOCOFFC,TRNSOFFC    OFFICE CODE                                  
         MVC   JOREF,ACKEYREF                                                   
         GOTO1 DATCON,DMCB,(1,ACKEYDTE),(17,JOCDATE)                            
         CURED TRNSAMNT,(L'JODR,JODR),2,MINUS=YES                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         DROP  R2                                                               
*                                                                               
CASH7    MVC   TRNLDG,ABNKLEDG                                                  
         MVI   TRN#LDGS,6          SET NUMBER OF LEDGER TABLE ENTRIES           
         MVC   TRNREC,AIO                                                       
         SR    RE,RE                                                            
         ICM   RE,3,TRNBSEQN                                                    
         LA    RE,1(RE)                                                         
         STCM  RE,3,TRNBSEQN                                                    
         MVC   TRNBMOS,BATMONP                                                  
         MVC   TRNCACNM,RECENAME   SET CONTRA-A/C NAME                          
         OI    TRNINDS2,TRNIADDG   ADD GL POSTING                               
*                                                                               
         GOTO1 ADDTRN,TRNBLK                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         LH    R3,NUMITEMS         UPDATE ITEM COUNT                            
         LA    R3,1(R3)                                                         
         STH   R3,NUMITEMS                                                      
         LA    R5,L'CASHTAB(R5)                                                 
         BCT   R0,CASH6                                                         
*                                                                               
CASH10   MVC   AIO,AIO1            RESTORE IO POINTER                           
*                                                                               
CASHX    B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* SUB-ROUTINE TO UPDATE THE CASH OFFICE TABLE                                   
***********************************************************************         
UPCASH   ST    RE,SAVERE                                                        
         LA    R0,MAXCASH          R0=LOOP COUNTER                              
         LA    RE,CASHTAB          RE=A(TABLE)                                  
*                                                                               
UPCASH2  OC    0(2,RE),0(RE)       TEST FOR EOT                                 
         BZ    UPCASH6             YES-ADD NEW ENTRY                            
         CLC   TSAROFFC,0(RE)      MATCH ON OFFICE                              
         BE    UPCASH4                                                          
         LA    RE,L'CASHTAB(RE)                                                 
         BCT   R0,UPCASH2                                                       
         DC    H'0'                                                             
*                                                                               
UPCASH4  AP    2(8,RE),ESTPAY                                                   
         B     UPCASHX                                                          
*                                                                               
UPCASH6  MVC   0(2,RE),TSAROFFC                                                 
         ZAP   2(8,RE),ESTPAY                                                   
*                                                                               
UPCASHX  L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO PRINT A RECEIVABLE POSTING                                     
* AT ENTRY, R6=A(ESTIMATE MONTH ELEMENT)                                        
*                                                                               
PRTPOST  NTR1  ,                                                                
         USING ACINESTD,R6                                                      
         LA    R2,P                                                             
         USING JOURNALD,R2                                                      
         MVC   FULL(2),ACIESMTH    SHOW ADV MOS                                 
         MVI   FULL+2,1                                                         
         GOTO1 DATCON,DMCB,(1,FULL),(18,JOADV)                                  
         CURED (P6,THISPAY),(L'JOCR,JOCR),2,MINUS=YES                           
         OC    ACIESDTO,ACIESDTO   TEST FOR ORIGINAL POSTING DATE               
         BZ    PRTPOST2            NONE-USE DEPOSIT DATE                        
         GOTO1 DATCON,DMCB,(2,ACIESDTO),(17,JODATE)                             
         B     PRTPOST4                                                         
*                                                                               
PRTPOST2 GOTO1 DATCON,DMCB,(1,BANKDATP),(17,JODATE)                             
*                                                                               
PRTPOST4 GOTO1 SPOOL,DMCB,ASPOOLD                                               
*                                                                               
PRTPOSTX B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO MOVE OUT DISPLAY DATA                                          
*                                                                               
MOVEFLD  ST    RE,SAVERE                                                        
         ZIC   R1,0(R2)                                                         
         LA    RE,8+1                                                           
         TM    1(R2),X'02'         TEST FOR EXTENDED FIELD                      
         BZ    *+8                                                              
         LA    RE,8+8+1                                                         
         SR    R1,RE                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),LISTAR                                                   
*                                                                               
MOVEFLDX L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
* SPOOL HEADLINE HOOK ROUTINE                                                   
*                                                                               
HOOK     NTR1  ,                                                                
         MVC   H3+16(L'RECEIVE-3),RECEIVE+3                                     
         MVC   H3+30(L'RECENAME),RECENAME                                       
         MVC   H3+96(L'BANK-3),BANK+3                                           
         MVC   H4+16(L'CLIENT),CLIENT                                           
         MVC   H4+30(L'CLINAME),CLINAME                                         
         CURED (P8,CHKAMT),(11,H4+96),2,ALIGN=LEFT,FLOAT=-                      
*                                                                               
         MVC   FULL(2),BATMONP                                                  
         MVI   FULL+2,1                                                         
         GOTO1 DATCON,DMCB,(1,FULL),(9,H5+96)                                   
*                                                                               
         MVC   FULL(2),ADVST                                                    
         MVI   FULL+2,1                                                         
         GOTO1 DATCON,DMCB,(1,FULL),(18,H5+16)                                  
         CLI   ADVNUM,1            TEST ONLY ONE MONTH                          
         BE    HOOK2               YES                                          
*                                                                               
         MVI   H5+22,C'-'                                                       
         MVC   FULL(2),ADVEND                                                   
         GOTO1 DATCON,DMCB,(1,FULL),(18,H5+23)                                  
*                                                                               
HOOK2    MVC   H6+16(L'LISTAR),LISTAR                                           
*                                                                               
HOOKX    B     XIT                                                              
         EJECT                                                                  
*              SUPPORTING SUBROUTINES                                           
*                                                                               
BUMP     ZIC   RF,0(R2)            BUMP TO NEXT HEADER                          
         AR    R2,RF                                                            
         BR    RE                                                               
         SPACE 1                                                                
BUMPTOUN ZIC   RF,0(R2)            BUMP TO NEXT UNPROTECTED                     
         AR    R2,RF                                                            
         CLI   0(R2),0                                                          
         BER   RE                                                               
         TM    1(R2),X'20'                                                      
         BO    BUMPTOUN                                                         
         BR    RE                                                               
         SPACE 1                                                                
GETELIO  L     R6,AIO                                                           
         GETEL (R6),DATADISP,ELCODE                                             
         SPACE 1                                                                
ERREND   GOTO1 VERRCUR                                                          
         SPACE 1                                                                
YESXIT   CR    RB,RB               SET CC=EQ AND EXIT                           
         B     XIT                                                              
         SPACE 1                                                                
NOXIT    LTR   RB,RB               SET CC=NEQ AND EXIT                          
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* LOCAL STORAGE                                                       *         
***********************************************************************         
         SPACE 1                                                                
CLIENT   DS    CL3                 CLIENT CODE                                  
CLINAME  DS    CL36                CLIENT NAME                                  
         EJECT                                                                  
***********************************************************************         
* PATCH AREA                                                          *         
***********************************************************************         
         SPACE 1                                                                
PATCH    DS    0H                                                               
         DC    XL32'00'                                                         
         EJECT                                                                  
***********************************************************************         
* CONSTANTS                                                           *         
***********************************************************************         
         SPACE 1                                                                
ESTMSG   DC    C'Total of estimates paid for batch'                             
RECVMSG  DC    C'Total Receivables postings'                                    
POSTMSG  DC    C'Total postings for batch'                                      
TOTMSG   DC    C'Total for batch'                                               
UPCMSG   DC    C'Update completed'                                              
THISEMSG DC    C'Total for estimate'                                            
WARNMSG  DC    C'**Total credits do not match check amount**'                   
PQMSG    DC    C'now on the Print Queue'                                        
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
* SPEC POOL                                                                     
*                                                                               
HEDSPECS DS    0D                                                               
         SPROG 0,1,2,3                                                          
         SSPEC H1,2,RUN                                                         
         SSPEC H1,83,PAGE                                                       
         SSPEC H2,83,REPORT                                                     
         SSPEC H3,2,C'Receivable A/C'                                           
         SSPEC H3,83,C'Bank Account'                                            
         SSPEC H4,2,C'Client'                                                   
         SSPEC H4,83,C'Check Amount'                                            
         SSPEC H5,2,C'Adv. Period'                                              
         SSPEC H5,83,C'Batch Month'                                             
         SSPEC H6,2,C'Narrative'                                                
*                                                                               
         SPROG 0,1,2                                                            
         SSPEC H8,2,C'Product'                                                  
         SSPEC H8,11,C'Media'                                                   
         SSPEC H8,18,C'Estimate'                                                
         SSPEC H8,28,C'Contra-Account'                                          
         SSPEC H8,46,C'Office'                                                  
         SSPEC H8,54,C'Adv. Month'                                              
         SSPEC H8,66,C'Transaction'                                             
         SSPEC H8,79,C'Debits'                                                  
         SSPEC H8,92,C'Credits'                                                 
*                                                                               
         SSPEC H9,69,C'Date'                                                    
*                                                                               
         SPROG 3                                                                
         SSPEC H8,2,C'Account'                                                  
         SSPEC H8,18,C'Contra-Account'                                          
         SSPEC H8,34,C'Office'                                                  
         SSPEC H8,42,C'Transaction'                                             
         SSPEC H9,45,C'Date'                                                    
         SSPEC H8,55,C'Reference'                                               
         SSPEC H8,79,C'Debits'                                                  
         SSPEC H8,92,C'Credits'                                                 
*                                                                               
         SPROG 0,3                                                              
         SSPEC H1,45,C'Journal of Check Postings'                               
*                                                                               
         SPROG 1                                                                
         SSPEC H1,42,C'Draft Journal of Check Postings'                         
*                                                                               
         SPROG 2                                                                
         SSPEC H1,37,C'Filtered Draft Journal of Check Postings'                
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
*  DDSPOOLD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
*  DDSPLWORKD                                                                   
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
*  ACGENBOTH                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
*  ACGENFILE                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
*  ACINTWORKD                                                                   
         PRINT OFF                                                              
       ++INCLUDE ACINTWORKD                                                     
         PRINT ON                                                               
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE ACINTF2D                                                       
         EJECT                                                                  
       ++INCLUDE ACINT12COM                                                     
         SPACE 2                                                                
* STORAGE USED BY UPDATE OVERLAY                                                
*                                                                               
         ORG   OVERWRK                                                          
LNARR    DS    X                   L'STANDARD NARRATION                         
CHKDATP  DS    PL3                 CHECK DATE (YMD) PWO                         
BANKDATP DS    PL3                 DEPOSIT DATE (YMD) PWO                       
LASTMED  DS    XL(L'MEDTYPE+L'MEDCODE)                                          
THISPAY  DS    PL(L'TSARPAID)                                                   
ESTPAY   DS    PL(L'TSARPAID)                                                   
CREDITS  DS    PL6                                                              
NUMESTS  DS    H                                                                
NUMITEMS DS    H                                                                
CONTRA   DS    CL(L'ACKEYACC)      POSTING CONTRA-ACCOUNT                       
MEDTAB   DS    (MEDTBMAX)CL(MEDTABL)                                            
CASHTAB  DS    (MAXCASH)CL10                                                    
PALAREA  DS    XL20                P&L BUCKET AREA                              
         DS    CL(L'OVERWRK-(*-OVERWRK))  SPARE                                 
         EJECT                                                                  
* EQUATES                                                                       
*                                                                               
MEDTBMAX EQU   8                                                                
MAXCASH  EQU   30                                                               
         EJECT                                                                  
* DSECT TO COVER JOURNAL PRINT LINE                                             
*                                                                               
JOURNALD DSECT                                                                  
         DS    CL1                 SPARE                                        
JOPRD    DS    CL3                 PRODUCT                                      
         DS    CL4                 SPARE                                        
         DS    CL2                 BETWEEN COLUMNS                              
JOMED    DS    CL5                 MEDIA                                        
         DS    CL2                 BETWEEN COLUMNS                              
JOEST    DS    CL6                 ESTIMATE                                     
         DS    CL2                 SPARE                                        
         DS    CL2                 BETWEEN COLUMNS                              
JOCAC    DS    CL14                CONTRA-ACCOUNT                               
         DS    CL4                 BETWEEN COLUMNS                              
JOOFFC   DS    CL2                 OFFICE                                       
         DS    CL4                 SPARE                                        
         DS    CL2                 BETWEEN COLUMNS                              
JOADV    DS    CL6                                                              
         DS    CL4                 SPARE                                        
         DS    CL2                 BETWEEN COLUMNS                              
JODATE   DS    CL8                                                              
         DS    CL3                 SPARE                                        
         DS    CL2                 BETWEEN COLUMNS                              
JODR     DS    CL11                DEBITS                                       
         DS    CL2                 BETWEEN COLUMNS                              
JOCR     DS    CL11                CREDITS                                      
         ORG   JOMED                                                            
JOMSG    DS    CL50                MESSAGE AREA                                 
*                                                                               
* CASH POSTINGS DETAIL LINE                                                     
*                                                                               
         ORG   JOPRD                                                            
JOCASH   DS    CL14                                                             
         DS    CL2                 BETWEEN COLUMNS                              
JORECV   DS    CL14                                                             
         DS    CL2                 BETWEEN COLUMNS                              
JOCOFFC  DS    CL2                                                              
         DS    CL4                 SPARE                                        
         DS    CL2                 BETWEEN COLUMNS                              
JOCDATE  DS    CL8                                                              
         DS    CL3                 SPARE                                        
         DS    CL2                                                              
JOREF    DS    CL6                                                              
         ORG                                                                    
         SPACE 2                                                                
* DSECT TO COVER MEDIA TABLE                                                    
*                                                                               
MEDTABD  DSECT                                                                  
MEDTYPE  DS    XL1                 0=MEDIA RECORD, 1=MEDIA INTERFACE            
MEDCODE  DS    CL2                                                              
MEDDESC  DS    CL12                                                             
MEDTABL  EQU   *-MEDTABD                                                        
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'022ACINT22   05/09/08'                                      
         END                                                                    
