*          DATA SET ACMAC01    AT LEVEL 027 AS OF 11/20/92                      
*PHASE T61101A,*                                                                
*                                                                               
         TITLE 'T61101 - MULTIPLE JOB CLOSE/REOPEN/ENQUIRY.'                    
T61101   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 000,**MAC01*                                                     
         L     RA,0(R1)            RA = A(TWA)                                  
         L     RC,4(R1)            RC = A(W/S)                                  
         L     R8,8(R1)            R8 = A(RECORD) IF ANY.                       
         USING MACWRKD,RC                                                       
         USING T611FFD,RA                                                       
         USING ACKEYD,R8                                                        
*                                                                               
         CLI   MODE,PROCREC                                                     
         BE    CLOSPROC                                                         
         CLI   MODE,MARKREC                                                     
         BE    CLOSMARK                                                         
         CLI   MODE,BUILDKEY                                                    
         BE    CLOSKEY                                                          
         CLI   MODE,EOF                                                         
         BE    CPRX070                                                          
*                                                                               
EXIT     XMOD1 1                                                                
*                                                                               
FASTEXT  L     RD,SAVED            IMMEDIATE RETURN TO FACPAK.                  
         B     EXIT                                                             
*                                                                               
OKEXIT   CR    RB,RB                                                            
         B     EXIT                RETURN WITH CC OF EQUAL.                     
*                                                                               
ERREXIT  LTR   RB,RB                                                            
         B     EXIT                RETURN WITH CC OF NOT EQUAL.                 
         EJECT                                                                  
CLOSKEY  LA    R8,KEY                                                           
         MVC   KEY,SPACES                                                       
         MVI   IOMODE,SKSEQ1                                                    
         TWAXC MALSTA1H,MALTABH,PROT=Y                                          
         SR    RE,RE                                                            
         LA    R7,MALSTA1H                                                      
         LA    RF,MALTABH-1                                                     
         OI    1(R7),X'20'                                                      
         NI    6(R7),X'FE'                                                      
         IC    RE,0(R7)                                                         
         BXLE  R7,RE,*-12                                                       
         SR    R0,R0                                                            
         LR    R3,RA                                                            
*  /*                                                                           
* // OPTION SXREF                                                               
* ASSGN SYSIN,SYS001                                                            
         CLI   0(R3),0                                                          
         BE    *+14                                                             
         IC    R0,0(R3)                                                         
         AR    R3,R0                                                            
         B     *-14                                                             
         MVI   1(R3),1                                                          
         MVI   2(R3),1                                                          
*                                                                               
         CLI   PASS,0                                                           
         BNE   CKEY100                                                          
         XC    UFORATAB,UFORATAB   CLEAR U/A TABLE.                             
         MVC   KEY(1),COMPANY      SET READ HI KEY ON 1ST PASS.                 
         MVC   KEY+1(2),WUNIT                                                   
         CLC   ACCFILT,SPACES                                                   
         BE    *+14                                                             
         MVC   KEY+3(12),ACCFILT                                                
         B     CKEY040                                                          
         MVC   KEY+3(6),CLIENT                                                  
         ZIC   R1,SLVLALEN                                                      
         LA    R8,3(R1,R8)         R8 = A(LEVEL B KEY START)                    
         MVC   0(6,R8),PRODUCT                                                  
         ZIC   R1,SLVLBLEN                                                      
         AR    R8,R1               R8 = A(LEVEL C KEY START).                   
         MVC   0(6,R8),JOB                                                      
         CLC   KEY+3(12),SPACES                                                 
         BE    CKEY040                                                          
         MVC   ENDKEY(15),KEY                                                   
         CLI   ENDKEY+14,C' '      INSERT HIGH VALUE AT SIGNIFICANT             
         BNE   CKEY040             LENGTH + 1 OF ENDKEY.                        
         LA    RE,ENDKEY+13                                                     
         LA    RF,11                                                            
CKEY030  CLI   0(RE),C' '                                                       
         BE    *+12                                                             
         MVI   1(RE),X'FF'                                                      
         B     CKEY040                                                          
         BCTR  RE,0                                                             
         BCT   RF,CKEY030                                                       
         DC    H'0'                                                             
*                                                                               
CKEY040  MVC   MACHED1,CLOSHED1                                                 
         MVC   MACHED2,CLOSHED2    HEADINGS ARE SET.                            
         OI    MACHED1H+6,X'80'                                                 
         OI    MACHED2H+6,X'80'                                                 
         MVI   LASTLYN,0                                                        
         CLC   KEY+3(12),SPACES    IF NO ACCT CODE IS AVAILABLE,                
         BNE   OKEXIT              FORCE READ HI TO GO BEYOND LEDGETR           
         ZIC   R1,SKEYEND          RECORD.                                      
         LA    R1,KEY(R1)                                                       
         MVI   0(R1),X'41'                                                      
         B     OKEXIT                                                           
*                                                                               
CKEY100  MVC   ACKEYACC,LASTACCT   RESTART FROM LAST USED KEY OF                
         MVI   LASTLYN,0           LAST PASS, START AT TOP OF SCREEN.           
         MVC   UFORATAB(CLOSMAX*2),UFORATAB+(CLOSMAX*2)  ROLL TABLE.            
         B     OKEXIT                                                           
         EJECT                                                                  
*              DISPLAY A RECORD FROM THE ROOT.                                  
*                                                                               
CLOSPROC MVI   IOMODE,SKSEQ1                                                    
         MVC   LASTACCT,ACKEYACC   SAVE THIS KEY FOR NEXT PASS.                 
         TM    ACSTATUS,X'80'                                                   
         BO    OKEXIT              SKIP DELETED ACCOUNTS                        
         CLI   KEY,X'FE'                                                        
         BE    CPRX070             TIDY UP AFTER I/O COUNT EXCEEDED.            
         CLI   ACCLVLNO,ACCLOW                                                  
         BE    CPRC020                                                          
*                                                                               
         CLI   ACTION,C'C'         IF CLOSING, SAVE CLI/PRO                     
         BNE   OKEXIT              U/A.                                         
         TM    COMPSTAT,X'02'                                                   
         BNZ   OKEXIT              NOT IF NO LABELS WANTED.                     
         ZIC   R7,LASTLYN          SET U/A FOR THIS CLI/PRO.                    
         SLL   R7,1                                                             
         LA    R7,UFORATAB(R7)                                                  
         USING ACPROFD,R6                                                       
         ZICM  R6,ADACPROF,4       R6 = A(PROFILE EL).                          
         BZ    OKEXIT              NONE.                                        
         CLC   ACPROFFC,SPACES                                                  
         BNH   OKEXIT                                                           
         MVC   0(2,R7),ACPROFFC                                                 
         ZIC   R0,LASTLYN          PROPAGATE U/A THRU REST                      
         SLL   R0,1                                                             
         LA    R1,L'UFORATAB       OF TABLE.                                    
         SR    R1,R0               R1 = REMAINING L'TABLE.                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     OKEXIT                                                           
         MVC   2(0,R7),0(R7)                                                    
*                                                                               
CPRC020  ZIC   R7,LASTLYN          POINT R7 TO THIS DISPLAY LINE.               
         LA    R6,CLYNLNQ                                                       
         MR    R6,R6                                                            
         LA    R7,MALSTA1H(R7)                                                  
         USING CLOSLYND,R7                                                      
         NI    CLYNSTAH+1,X'DF'    TURN OFF PROTECT BIT FOR MARK FIELD.         
         OI    CLYNSTAH+6,X'80'    TRANSMIT IT.                                 
*                                                                               
         USING ACPROFD,R6                                                       
         ZICM  R6,ADACPROF,4       IF PRESENT, TAKE U/A FOR                     
         BZ    CPRC040             THIS JOB.                                    
         CLC   ACPROFFC,SPACES                                                  
         BNH   CPRC040                                                          
         ZIC   R2,LASTLYN                                                       
         SLL   R2,1                                                             
         LA    R2,UFORATAB(R2)     R2 = A(ENTRY FOR THIS LINE)                  
         MVC   0(2,R2),ACPROFFC                                                 
*                                                                               
         USING ACNAMED,R6                                                       
CPRC040  ZICM  R6,ADACNAM,4                                                     
         BZ    CPRC060                                                          
         ZIC   R1,ACNMLEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     CPRC060                                                          
         MVC   CLYNNAME(0),ACNMNAME                                             
*                                                                               
         USING ACJOBD,R6                                                        
CPRC060  ZICM  R6,ADACJOB,4                                                     
         BZ    CPRC080                                                          
         OC    ACJBCLOS,ACJBCLOS                                                
         BZ    CPRC080                                                          
         GOTO1 DATCON,DMCB,(1,ACJBCLOS),(8,CLYNCLOS)                            
*                                                                               
         USING ACSTATD,R6                                                       
CPRC080  ZICM  R6,ADACSTAT,4                                                    
         BZ    CPRC100                                                          
         GOTO1 DATCON,DMCB,(1,ACSTLAST),(8,CLYNACT) ACTIVITY DATE.              
         TM    CLYNSTAH+1,X'20'                                                 
         BO    CPRC100             ACCOUNT LINE IS LOCKED ALREADY.              
         CLI   ACTION,C'C'                                                      
         BNE   CPRC090             FUNCTION IS REOPEN.                          
         TM    ACSTSTAT,X'40'                                                   
         BZ    CPRC100             JOB IS OPEN, DO NOWT ELSE.                   
         OI    CLYNSTAH+1,X'20'    JOB IS CLOSED, LOCK ACCOUNT LINE.            
         MVC   CLYNWHY(6),=C'CLOSED' DISPLAY THE REASON FOR LOCK.               
         B     CPRC100                                                          
*                                                                               
CPRC090  TM    ACSTSTAT,X'40'                                                   
         BNZ   CPRC100             JOB IS CLOSED. IE, VALID FOR REOPEN.         
         OI    CLYNSTAH+1,X'20'    JOB IS OPEN.                                 
         MVC   CLYNWHY(4),=C'OPEN'                                              
         B     CPRC100                                                          
*                                                                               
         USING ACBALD,R6                                                        
CPRC100  ZICM  R6,ADACBAL,4                                                     
         BZ    CPRC120                                                          
         TM    CLYNSTAH+1,X'20'                                                 
         BO    CPRC120             ACCOUNT LINE ALREADY LOCKED.                 
         CP    ACBLDR,ACBLCR                                                    
         BE    CPRC120             ZERO BALANCE                                 
         OI    CLYNSTAH+1,X'20'    NON-ZERO BALANCE, LOCK SCREEN LINE.          
         MVC   CLYNWHY(7),=C'BALANCE' GIVE THE REASON.                          
         B     CPRC120                                                          
         DROP  R6                                                               
*                                                                               
         USING ASTELD,R6                                                        
CPRC120  ICM   R6,15,ADACAST                                                    
         BZ    CPRC130                                                          
         OC    ASTDRAFT,ASTDRAFT                                                
         BZ    CPRC130             NO DRAFT ITEMS - OK TO CLOSE                 
         TM    CLYNSTAH+1,X'20'                                                 
         BO    CPRC130             LINE ALREADY LOCKED                          
         OI    CLYNSTAH+1,X'20'                                                 
         MVC   CLYNWHY(7),=C'DFT ITM'                                           
         B     CPRC130                                                          
*                                                                               
CPRC130  MVC   CLYNACC,ACKEYACC+3  DISPLAY THE ACCOUNT CODE.                    
         CLI   ACTION,C'C'                                                      
         BNE   CPROCX              IF FUNCTION IS CLOSE, AND                    
         TM    CLYNSTAH+1,X'20'    IF ACCOUNT IS ELIGIBLE FOR CLOSING,          
         BNZ   CPROCX              READ TRANSACTIONS INTO IO2 LOOKING           
         MVI   IOMODE,SEQ2         FOR UNDELETED ORDERS. IE, THOSE WITH         
         LA    R8,IO2              '**' IN ACKEYWRK                             
*                                                                               
CPRC140  GOTO1 SEQ                                                              
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
         CLI   KEY,X'FE'                                                        
         BNE   CPRC150             WITHIN MAX I/O COUNT.                        
         XC    CLYN,CLYN           EXCEEDING MAX I/OS, CLEAR THIS LINE.         
         NI    CLYNSTAH+1,X'DF'                                                 
         CLI   LASTLYN,0           IF THIS IS NOT THE FIRST SCREEN LINE         
         BE    CPRX070             FORCE LASTACCT TO THE LAST DISPLAYED         
         LA    R1,CLYNLNQ          ACCOUNT CODE, ELSE LEAVE AS IS.              
         SR    R7,R1               R7 = A(LAST SCREEN LINE).                    
         MVC   LASTACCT+3(12),CLYNACC                                           
         MVC   KEY,LASTACCT        RESTORE GOOD KEY VALUE.                      
         MVI   IOMODE,SKSEQ1       RETURN TO MAJOR MODE.                        
         LA    R8,IO                                                            
         B     CPRX070                                                          
*                                                                               
CPRC150  CLC   LASTACCT,ACKEYACC                                                
         BNE   CPRC300             END OF TRANSACTIONS FOR ACCOUNT.             
         CLC   ACKEYWRK(2),=C'99'                                               
         BE    CPRC140             BILLING                                      
         TM    ACSTATUS,X'80'                                                   
         BNZ   CPRC140             TRANSACTION IS DELETED.                      
         CLI   ACRECORD,X'44'                                                   
         BNE   CPRC140             TRANSACTIONS ONLY.                           
*                                                                               
         CLC   ACKEYWRK,=C'**'                                                  
         BNE   CPRC160             NOT AN ORDER.                                
         OI    CLYNSTAH+1,X'20'    IS AN ORDER, LOCK LINE & SAY WHY.            
         MVC   CLYNWHY(6),=C'ORDERS'                                            
         B     CPRC300                                                          
*                                                                               
CPRC160  LA    R2,ACRECORD         IS IT HELD?                                  
         USING TRANSD,R2                                                        
         TM    TRNSSTAT,X'04'                                                   
         BZ    CPRC180             NO                                           
*                                                                               
         OI    CLYNSTAH+1,X'20'    HELD ITEM, LOCK LINE & SAY WHY.              
         MVC   CLYNWHY(4),=C'HELD'                                              
         B     CPRC300                                                          
*                                                                               
         USING ACPROFD,R6                                                       
CPRC180  ICM   R6,15,ADACPROF      GET PROFILE ELEMENT                          
         BZ    CPRC190             NOT FOUND, ASSUME NOT CLI BILL               
*                                                                               
         USING ACGOD,R2                                                         
         L     R2,AGOBLOCK                                                      
         CLI   GOBILTYP,C'C'       CLIENT BILL JOB                              
         BE    CPRC220             YES, USE CLIENT BILL LOGIC                   
*                                                                               
CPRC190  OC    ACDTUSED,ACDTUSED   BILLED                                       
         BNZ   CPRC140             YES, LOOK FOR UNBILLED                       
*                                                                               
         LA    R2,ACRECORD         IS IT REVERSED                               
         USING TRANSD,R2                                                        
         TM    TRNSSTAT,X'20'                                                   
         BNZ   CPRC140             YES                                          
*                                                                               
         CLC   ACKEYCON+1(2),=C'SK' INCOME SUSPENCE                             
         BE    CPRC210              YES, CAN'T CLOSE IF UNBILLED                
*                                                                               
         SR    R0,R0                GET SUBSIDARY ACCOUNT                       
*                                                                               
CPRC200  IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0             E-O-R                                        
         BE    CPRC140             YES,                                         
         CLI   0(R2),X'4C'         SUBSIDARY ACCOUNT RECORD                     
         BNE   CPRC200             NO, GET NEXT EL                              
*                                                                               
         USING TRSDESCD,R2                                                      
         CLC   TRSDACCS(2),=C'SK'                                               
         BE    CPRC210             CAN'T BILL                                   
         B     CPRC140                                                          
*                                                                               
CPRC210  OI    CLYNSTAH+1,X'20'    UNBILLED SK                                  
         MVC   CLYNWHY(7),=C'UNBL SK'                                           
         B     CPRC300                                                          
*                                                                               
CPRC220  LA    R2,ACRECORD         CLIENT BILLING JOB                           
         SR    R0,R0                                                            
         ZAP   WORK(6),=P'0'                                                    
*                                                                               
CPRC230  IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0             E-O-R                                        
         BE    CPRC240             YES,                                         
         CLI   0(R2),X'4B'         SUBSIDARY ACCOUNT RECORD                     
         BNE   CPRC230             NO, GET NEXT EL                              
*                                                                               
         USING TRBDETD,R2                                                       
         CLC   TRBDNO,SPACES       BILLED CHARGES ONLY                          
         BNH   CPRC230                                                          
         ICM   RF,15,TRBDAMNT                                                   
         CVD   RF,DUB                                                           
         AP    WORK(6),DUB                                                      
         B     CPRC230                                                          
*                                                                               
         USING TRANSD,R2                                                        
CPRC240  LA    R2,ACRECORD                                                      
         CP    WORK(6),TRNSAMNT                                                 
         BE    CPRC140             FULLY BILLED.                                
*                                                                               
CPRC250  OI    CLYNSTAH+1,X'20'    UNBILLED ITEM                                
         MVC   CLYNWHY(6),=C'UNBILL'                                            
         B     CPRC300                                                          
*                                                                               
CPRC300  MVC   KEY,LASTACCT        RESTORE GOOD KEY VALUE.                      
         MVI   IOMODE,SKSEQ1       RETURN TO MAJOR MODE.                        
         LA    R8,IO                                                            
*                                                                               
CPROCX   CLI   ORDERS,C' '                                                      
         BE    CPRX010             ORDER STATUS IS NOT RELEVANT.                
         LA    R1,X'80'            ACCEPT ACCS WITH ORDERS OUTSTANDING.         
         CLI   ORDERS,C'Y'                                                      
         BE    *+8                                                              
         LA    R1,X'70'            REJECT ACCS WITH ORDERS OUTSTANDING.         
         CLC   CLYNWHY(6),=C'ORDERS'                                            
         EX    R1,*+8                                                           
         B     CPRX050             IT'S A REJECT.                               
         BC    0,CPRX040                                                        
*                                                                               
CPRX010  CLI   HELD,C' '                                                        
         BE    CPRX020             HELD STATUS NOT RELEVENT                     
         LA    R1,X'80'            ACCEPT ACCS WITH HELD ITEMS                  
         CLI   HELD,C'Y'                                                        
         BE    *+8                                                              
         LA    R1,X'70'            REJECT ACCS WITH HELD ITEMS                  
         CLC   CLYNWHY(4),=C'HELD'                                              
         EX    R1,*+8                                                           
         B     CPRX050             IT'S A REJECT.                               
         BC    0,CPRX040                                                        
*                                                                               
CPRX020  CLI   UNBILL,C' '                                                      
         BE    CPRX030             UNBILL STATUS NOT RELEVENT                   
         LA    R1,X'80'            ACCEPT ACCS WITH UNBILLED ITEMS              
         CLI   UNBILL,C'Y'                                                      
         BE    *+8                                                              
         LA    R1,X'70'            REJECT ACCS WITH UNBILLED ITEMS              
         CLC   CLYNWHY(6),=C'UNBILL'                                            
         EX    R1,*+8                                                           
         B     CPRX050             IT'S A REJECT.                               
         BC    0,CPRX040                                                        
*                                                                               
CPRX030  CLI   UNBLSK,C' '                                                      
         BE    CPRX040             UNBILLED SK NOT RELEVENT                     
         LA    R1,X'80'            ACCEPT ACCS WITH UNBILLED SK                 
         CLI   UNBLSK,C'Y'                                                      
         BE    *+8                                                              
         LA    R1,X'70'            REJECT ACCS WITH UNBILLED SK                 
         CLC   CLYNWHY(7),=C'UNBL SK'                                           
         EX    R1,*+8                                                           
         B     CPRX050             IT'S A REJECT.                               
         BC    0,CPRX040                                                        
*                                                                               
CPRX040  CLI   ELIGIBL,C' '                                                     
         BE    CPRX060             ACCEPT ALL ACCOUNTS.                         
         LA    R1,X'80'            ACCEPT THE ELIGIBLE ACCOUNTS.                
         CLI   ELIGIBL,C'Y'                                                     
         BE    *+8                                                              
         LA    R1,X'70'            ACCEPT THE INELIGIBLE ACCOUNTS.              
         TM    CLYNSTAH+1,X'20'                                                 
         EX    R1,*+8                                                           
         B     *+8                 FAILED ELIGIBILITY TEST.                     
         BC    0,CPRX060                                                        
CPRX050  NI    CLYNSTAH+1,X'DF'    ERASE PROTEXT BIT.                           
         XC    CLYN,CLYN           CLEAR THE LINE.                              
         B     OKEXIT              GET NEXT ACCOUNT.                            
*                                                                               
CPRX060  TM    CLYNSTAH+1,X'20'    IF LINE IS ELIGIBLE, SUPPLY                  
         BO    *+14                OPTIONAL MARK.                               
         MVC   CLYNSTA,MARK                                                     
         OI    CLYNSTAH+6,X'81'                                                 
         OI    CLYNH+6,X'80'       DISPLAY THE LINE.                            
         ZIC   R1,LASTLYN                                                       
         LA    R1,1(R1)                                                         
         STC   R1,LASTLYN          BUMP LINE COUNTER.                           
         CLI   LASTLYN,CLOSMAX                                                  
         BNE   OKEXIT                                                           
CPRX070  ZIC   R1,PASS             INCREMENT PASS NUMBER                        
         LA    R1,1(R1)                                                         
         STC   R1,PASS                                                          
*                                                                               
         LA    RF,MALTABH-1        SET CURSOR TO FIRST UNPROTECTED              
         LA    R7,MALSTA1H         MARK FIELD OR TAB FIELD IF ALL               
CPRX080  TM    1(R7),X'20'         ARE PROTECTED.                               
         BNZ   CPRX090             THIS ONE IS PERMENENTLY PROTECTED.           
         TM    6(R7),X'20'                                                      
         BO    CPRX090             PROTECTED.                                   
         CLI   8(R7),X'00'                                                      
         BE    CPRX100             UNMARKED.                                    
CPRX090  ZIC   RE,0(R7)            L'HEADER + L'FIELD.                          
         BXLE  R7,RE,CPRX080                                                    
*                                                                               
CPRX100  OI    6(R7),X'40'         CURSOR HERE.                                 
         OI    MACACTH+6,X'81'     FORCE MODIFIED BIT ON ACTION FIELD.          
         MVI   IOMODE,FINISHED     SCREEN IS FULL, SAY WE'RE FINISHED.          
         SR    R0,R0                                                            
         LR   R3,RA                                                             
         CLI   0(R3),0                                                          
         BE    *+14                                                             
         IC    R0,0(R3)                                                         
         AR    R3,R0                                                            
         B     *-14                                                             
         MVI   1(R3),1                                                          
         MVI   2(R3),1                                                          
         B     OKEXIT                                                           
         EJECT                                                                  
*              CLOSE THE MARKED ELIGIBLE JOBS.                                  
*                                                                               
         USING CLOSLYND,R7                                                      
CLOSMARK LA    R7,MALSTA1H         R7 = A(1ST SCREEN LINE).                     
         LA    R0,CLOSMAX          R0 = MAX NUMBER OF SCREEN LINES.             
*                                                                               
CMRK020  CLI   CLYNSTA,0                                                        
         BE    CMRK100             NO INPUT, SKIP LINE.                         
         CLI   CLYNSTA,C'N'                                                     
         BE    CMRK100             I/P SAYS DON'T MARK, SKIP LINE.              
         CLI   CLYNSTA,C'Y'                                                     
         BE    CMRK022             I/P SAYS MARK, DO IT.                        
         LA    RF,CLYNSTAH         ERRONEOUS I/P, SHOW THE ERROR.               
         ST    RF,FADR                                                          
         MVI   FERN,INVALID                                                     
         MVI   FNDX,0                                                           
         B     ERREXIT                                                          
*                                                                               
CMRK022  MVI   IOMODE,READ2        SET TO READ THE ACCOUNT INTO IO2.            
         LA    R8,IO2                                                           
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),WUNIT                                                   
         MVC   KEY+3(12),CLYNACC   TAKE ACCOUNT FROM SCREEN LINE.               
         GOTO1 READ                                                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   KEY,X'FE'                                                        
         BNE   *+6                                                              
         DC    H'0'                MAX I/O COUNT EXCEEDED.                      
         LA    R6,ACRECORD                                                      
*                                                                               
CMRK060  CLI   0(R6),0                                                          
         BE    CMRK080                                                          
         CLI   0(R6),ACJBELQ                                                    
         BE    CMRK070             JOB ELEMENT.                                 
         CLI   0(R6),ACSTELQ                                                    
         BE    CMRK074             STATUS ELEMENT.                              
CMRK064  ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     CMRK060                                                          
*                                                                               
         USING ACJOBD,R6                                                        
CMRK070  CLI   ACTION,C'C'         IF WE'RE CLOSING, SET JOB CLOSING            
         BNE   CMRK064             DATE TO TODAY.                               
         GOTO1 GETFACT,DMCB,0                                                   
         L     RE,DMCB                                                          
         USING FACTSD,RE                                                        
         MVC   DUB,FADATE                                                       
         DROP  RE                                                               
         GOTO1 DATCON,DMCB,(4,DUB),(1,ACJBCLOS)                                 
         B     CMRK064                                                          
*                                                                               
         USING ACSTATD,R6                                                       
CMRK074  XI    ACSTSTAT,X'40'      INVERT OPEN/CLOSE BIT.                       
         B     CMRK064                                                          
*                                                                               
CMRK080  MVC   KEY,ACKEYACC                                                     
         MVI   IOMODE,WRITE2                                                    
         GOTO1 WRITE               REWRITE RECORD FROM IO2.                     
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   KEY,X'FE'                                                        
         BNE   *+6                                                              
         DC    H'0'                MAX I/O COUNT EXCEEDED.                      
*                                                                               
         CLI   ACTION,C'C'         IF CLOSING, WRITE A LABEL.                   
         BNE   CMRK100                                                          
         TM    COMPSTAT,X'02'                                                   
         BNZ   CMRK100             NO LABELS REQUIRED.                          
         XC    ELEMENT,ELEMENT                                                  
         MVI   ELEMENT+10,12                                                    
         MVC   ELEMENT+26(80),SPACES                                            
         MVC   ELEMENT+26(2),=C'12'                                             
         MVC   ELEMENT+28(1),COMPANY                                            
         LA    R2,CLOSMAX          FIND ENTRY IN U/A TABLE                      
         SR    R2,R0               FOR THIS LINE.                               
         SLL   R2,1                                                             
         LA    R2,UFORATAB(R2)                                                  
         MVC   ELEMENT+29(1),0(R2) MOVE IN U/A.                                 
         ZICM  R8,ADACPROF,4                                                    
         USING ACPROFD,R8                                                       
         CLC   ACPROFFC,SPACES                                                  
         BNH   *+10                                                             
         MVC   ELEMENT+29(1),ACPROFFC                                           
         DROP  R8                                                               
         MVC   ELEMENT+35(15),IO2                                               
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'ACCREQS',ELEMENT,ELEMENT,0             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CMRK100  LA    R7,CLYNLNQ(R7)      GET THE NEXT LINE.                           
         BCT   R0,CMRK020                                                       
         B     OKEXIT                                                           
         DROP  R6,R7                                                            
         EJECT                                                                  
         LTORG                                                                  
         SPACE 1                                                                
CLOSHED1 DC    CL16'M  CLI PROD JOB',16C'-',C'NAME',16C'-'                      
         DC    C' ACTIVITY CLOSING  ACCOUNT'                                    
CLOSHED2 DC    C'K  ',12C'-',40C' ',CL9'DATE',CL7'DATE',C'STATUS '              
         EJECT                                                                  
CLOSLYND DSECT                                                                  
CLYNSTAH DS    CL8                                                              
CLYNSTA  DS    CL1                                                              
CLYNH    DS    CL8                                                              
CLYN     DS    0CL75                                                            
CLYNACC  DS    CL12                ACCOUNT CODE.                                
         DS    CL1                 SPARE.                                       
CLYNNAME DS    CL36                ACCOUNT NAME.                                
         DS    CL1                 SPARE                                        
CLYNACT  DS    CL9                 ACTIVITY DATE.                               
CLYNCLOS DS    CL9                 CLOSING DATE.                                
CLYNWHY  DS    CL7                 REASON ACCOUNT CAN'T BE CLOSED.              
CLYNLNQ  EQU   *-CLOSLYND                                                       
*                                                                               
CLOSMAX  EQU   17                                                               
         EJECT                                                                  
       ++INCLUDE ACMACWRK                                                       
         EJECT                                                                  
       ++INCLUDE ACMACEQU                                                       
*              INCLUDED HERE ARE - ACGENBOTH                                    
*                                  DDCOMFACS                                    
*                                  DDACCFACS                                    
*                                  FATWA                                        
*                                  FAFACTS                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDACCFACS                                                      
       ++INCLUDE FATWA                                                          
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'027ACMAC01   11/20/92'                                      
         END                                                                    
