*          DATA SET ACREPHD02  AT LEVEL 047 AS OF 05/01/02                      
*PHASE ACHD02A                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'REMOVE REQUESTED PEELED ITEMS FIX'                              
ACHD02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACHD**,R9                                                    
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACHDD,RC                                                         
         LA    R8,P                                                             
         USING PLINED,R8                                                        
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
*                                                                               
EXIT     XMOD1 1                                                                
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        FIRST FOR RUN                                                          
*-------------------------------------------------------------------*           
RUNF     DS    0H                                                               
         LA    R7,REQTABL                                                       
         ST    R7,CURREQST                                                      
         MVI   NUMREQST,0                                                       
         B     EXIT                                                             
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        FIRST FOR REQUEST                                                      
*-------------------------------------------------------------------*           
REQF     DS    0H                                                               
         L     R7,CURREQST         CURRENT REQUEST POINTER                      
         USING REQTABLD,R7                                                      
*                                                                               
         CLC   QSTART,SPACES       HAVE TO HAVE A PEELDATE                      
         BH    REQF03                                                           
         MVC   P+2(20),=C'MUST HAVE PEEL DATE!'                                 
         B     EXIT                                                             
*                                                                               
REQF03   XC    REQCOMP(REQTABLN),REQCOMP        CLEAR REQUEST CARD              
         MVC   REQCOMP,QCOMPANY                                                 
         MVC   REQUNIT,QUNIT                                                    
         MVC   REQLEDG,QLEDGER                                                  
         MVC   REQACCNT,QACCOUNT   IF THEY WANT BY ACCOUNT                      
         MVC   REQOPTN,QOPT1       SAVE OPTIONS                                 
         GOTO1 DATCON,DMCB,(0,QSTART),(2,REQPDATE)                              
         CLI   QCNTINUE,C'C'       MORE INFO?                                   
         BNE   REQF04X                                                          
         CLC   QCUL,SPACES                                                      
         BE    *+10                                                             
         MVC   REQCUL,QCUL                                                      
         CLC   QCACCT,SPACES                                                    
         BE    *+10                                                             
         MVC   REQCACCT,QCACCT                                                  
         CLC   QDUEST,SPACES                                                    
         BE    *+10                                                             
         MVC   REQREF,QDUEST                                                    
         CLC   QDUEND,SPACES                                                    
         BE    REQF04X                                                          
         GOTO1 DATCON,DMCB,(0,QDUEND),(1,REQTDATE)                              
*                                                                               
REQF04X  LA    R4,14               UNIT,LEDGER AND ACCOUNT LENGTH               
         LA    RF,QACCOUNT+11      SEE HOW MUCH USER REQUESTED                  
REQF05   CLI   0(RF),C' '                                                       
         BH    REQF08                                                           
         BCTR  RF,0                                                             
         BCT   R4,REQF05                                                        
REQF08   STC   R4,REQCMPLN         SAVE COMPARE UNIT LDGR + ACC LEN             
*                                    INCLUDES COMPANY                           
*                                                                               
* EXTRACT NAMES:                                                                
* COMPANY                                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(1),REQCOMP                                                   
         MVC   REQCNAME,SPACES                                                  
         GOTO1 =A(GETNAME),DMCB,(0,REQCNAME)                                    
* UNIT                                                                          
         XC    KEY,KEY                                                          
         MVC   KEY(2),REQCOMP                                                   
         MVC   REQUNAME,SPACES                                                  
         GOTO1 =A(GETNAME),DMCB,(0,REQUNAME)                                    
* LEDGER                                                                        
         XC    KEY,KEY                                                          
         MVC   KEY(3),REQCOMP                                                   
         MVC   REQLNAME,SPACES                                                  
         GOTO1 =A(GETNAME),DMCB,(0,REQLNAME)                                    
*                                                                               
         AI    NUMREQST,1                                                       
         LA    R7,REQTABLN(R7)                                                  
         ST    R7,CURREQST                                                      
         B     EXIT                                                             
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        RUNLAST                                                                
*-------------------------------------------------------------------*           
RUNL     LA    R2,IO                                                            
         LA    R3,4(R2)                                                         
         MVI   OFFICE2,0           RESET TO READ OFFICE STATUS                  
         MVI   SVCOMP,0                                                         
         MVC   SVOFF,SPACES                                                     
         MVC   SVACC,SPACES                                                     
*                                                                               
* GOT TO SORT REQUEST CARDS                                                     
*                                                                               
         LA    R7,REQTABL                                                       
         ZIC   R6,NUMREQST                                                      
         LA    RF,REQTABLN                                                      
         GOTO1 XSORT,DMCB,(0,REQTABL),(R6),(RF),15,0                            
*                                                                               
         LA    R7,REQTABL                                                       
         ZIC   R6,NUMREQST                                                      
*                                                                               
         USING TRNRECD,R2                                                       
RUNL100  DS    0H                                                               
         MVI   RCSUBPRG,1                                                       
         BAS   RE,RUNLINIT                                                      
         MVC   CURRKEY,SPACES                                                   
*                                                                               
         XC    DMCB(24),DMCB       OPEN ACCHST                                  
         GOTO1 DATAMGR,DMCB,DMDTF,ACCHST                                        
         MVC   HISTDCB,12(R1)      SAVE ADDRESS OF HISTORY DCB                  
         MVI   TMPWRK,C'N'                                                      
         MVC   TMPWRK+1(7),ACCHST                                               
         MVI   TMPWRK+8,C'X'                                                    
         GOTO1 DATAMGR,DMCB,DMOPEN,ACCFIL,TMPWRK,ACFILEC                        
*                                                                               
         LA    R2,IO                                                            
         CLC   SVCOMP,REQCOMP      COMPANY CHANGED?                             
         BE    RUNL200                                                          
         BAS   RE,OFFINFO          UPDATE OFFICE INFORMATION                    
         MVC   SVCOMP,REQCOMP                                                   
*                                                                               
RUNL200  BAS   RE,GETINFO                                                       
         BNE   RUNL700             DONE                                         
*                                                                               
RUNL400  DS    0H                                                               
         LA    R2,CHKKEY                                                        
         MVC   CHKKEY,SPACES                                                    
         MVC   TRNKCULA,CURRKEY   CHECK IF ACCOUNT IN LIVE                      
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,TRNKEY,TRNKEY                         
         CLC   TRNKCULA,CURRKEY                                                 
         BNE   RUNL450             NO ACCOUNT                                   
         CLC   TRNKOFF(TRNKSTA-TRNKOFF),SPACES                                  
         BNH   RUNL600             ACCOUNT, NEXT ONE                            
*                                                                               
         USING ACKEYD,R2                                                        
RUNL450  CLC   SVCACC,CURRKEY                                                   
         BE    RUNL455                                                          
         MVC   P+2(14),CURRKEY+1                                                
         GOTO1 ACREPORT                                                         
         MVC   SVCACC,CURRKEY                                                   
*                                                                               
RUNL455  LA    R2,IO                                                            
         USING TRNELD,R4                                                        
         LA    R4,ACRECORD         POINT TO TRANSACTION ELEMENT                 
         TM    TRNSTAT,X'80'       DEBIT?                                       
         BO    RUNL410                                                          
         AP    TOTCR,TRNAMNT                                                    
         AP    REPCR,TRNAMNT                                                    
         B     RUNL420                                                          
*                                                                               
RUNL410  DS    0H                                                               
         AP    TOTDR,TRNAMNT                                                    
         AP    REPDR,TRNAMNT                                                    
RUNL420  DS    0H                                                               
*                                                                               
RUNL435  AP    RCVRECS,=P'1'                                                    
*                                                                               
RUNL500  LA    R2,IO                                                            
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),ACCHST,CURRKEY,IO                    
         NI    ACSTATUS,X'7F'      UNDELETE FROM HISTORY FILE                   
         CLI   RCWRITE,C'N'        SHOULD WE WRITE IT?                          
         BE    RUNL600             NO, SKIP IT                                  
         GOTO1 DATAMGR,DMCB,DMWRT,ACCHST,ACKEYACC,IO                            
*                                                                               
RUNL600  B     RUNL200                                                          
*                                                                               
RUNL700  LA    R7,REQTABLN(R7)                                                  
         MVC   CURRKEY,SPACES                                                   
         BCT   R6,RUNL200                                                       
*                                                                               
         BAS   RE,SENDTOT                                                       
*                                                                               
         MVC   P+1(100),=100C'-'                                                
         GOTO1 ACREPORT                                                         
         MVC   P+1(28),=C'    TOTAL TRANSACTIONS READ:'                         
         EDIT  INPRECS,(7,P+37),ZERO=NOBLANK                                    
         GOTO1 ACREPORT                                                         
         MVC   P+1(28),=C'TOTAL TRANSACTIONS RESTORED:'                         
         EDIT  RCVRECS,(7,P+37),ZERO=NOBLANK                                    
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         DROP  R2,R4                                                            
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        GETINFO, GET RECORD FROM HISTORY, FOR FILE OPTION ONLY     *           
*-------------------------------------------------------------------*           
         USING TRNRECD,R2                                                       
GETINFO  NTR1                                                                   
         LA    R2,IO                                                            
         ZIC   R3,REQCMPLN         CMP, U/L, ACC LENGTH                         
         CLC   CURRKEY,SPACES      WERE WE HERE BEFORE?                         
         BNE   GET070              YES, READ SEQUENTIAL                         
*                                                                               
         MVC   TRNKEY,SPACES                                                    
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   TRNKEY(0),REQCOMP                                                
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),ACCHST,TRNKEY,IO                     
GET050   EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   TRNKEY(0),REQCOMP                                                
         BNE   GETNO                                                            
         CLI   REQOPT1,C'S'        SPECIAL PEEL, IGNORE DATE?                   
         BE    GET050X                                                          
         CLC   (ACDTPEEL-ACKEYD)(2,R2),REQPDATE                                 
         BNE   GET070                                                           
GET050X  OC    REQCINFO,REQCINFO   CONTINUATION CARD INFO?                      
         BZ    GET060              NO, SKIP EXTRA TESTS                         
         OC    REQCUL,REQCUL                                                    
         BZ    GET051                                                           
         CLC   REQCUL,TRNKULC      COMPARE CONTRA U/L                           
         BNE   GET070                                                           
GET051   OC    REQCACCT,REQCACCT                                                
         BZ    GET052                                                           
         CLC   REQCACCT,TRNKCACT   COMPARE CONTRA ACCT                          
         BNE   GET070                                                           
GET052   OC    REQREF,REQREF                                                    
         BZ    GET053                                                           
         CLC   REQREF,TRNKREF      COMPARE BATCH REFERENCE                      
         BNE   GET070                                                           
GET053   OC    REQTDATE,REQTDATE                                                
         BZ    GET054                                                           
         CLC   REQTDATE,TRNKDATE                                                
         BNE   GET070                                                           
GET054   DS    0H                                                               
*                                                                               
GET060   LA    R4,ACCORFST(R2)                                                  
         CLI   0(R4),TRNELQ        X'44', HAS TO BE A TRANSACTION               
         BE    GETYES                                                           
GET070   GOTO1 DATAMGR,DMCB,(X'08',DMRSEQ),ACCHST,TRNKEY,IO                     
         B     GET050                                                           
*                                                                               
         USING TRNELD,R4                                                        
GETYES   MVC   CURRKEY,TRNKEY                                                   
         AP    INPRECS,=P'1'       KEEP TRACK OF # OF RECORDS                   
         CLI   REQOPT3,C'D'        DUMP RECORDS?                                
         BNE   GETYES2                                                          
         MVC   P+1(5),=C'*DUMP*'                                                
         MVC   P+8(14),TRNKULA                                                  
         EDIT  TRNAMNT,(15,P+30),2,ZERO=NOBLANK,MINUS=YES                       
         MVC   P+46(2),=C'CR'                                                   
         TM    TRNSTAT,X'80'                                                    
         BZ    *+8                                                              
         MVI   P+46,C'D'                                                        
         MVC   P+50(14),TRNKULC    CONTRA                                       
         GOTO1 DATCON,DMCB,(1,TRNKDATE),(5,P+67)                                
         MVC   P+77(6),TRNKREF                                                  
         USING ACKEYD,R2                                                        
         GOTO1 DATCON,DMCB,(2,ACDTPEEL),(X'20',P+85)                            
         GOTO1 ACREPORT                                                         
GETYES2  CR    RE,RE                                                            
         B     *+6                                                              
GETNO    LTR   RE,RE                                                            
         B     XIT                                                              
         DROP  R2,R4                                                            
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        OFFINFO, OFFICE INFORMATION, FOR FILE OPTION ONLY          *           
*-------------------------------------------------------------------*           
         USING CPYRECD,R2                                                       
OFFINFO  NTR1                                                                   
         CLI   OFFICE2,0                                                        
         BNE   OFFXIT                                                           
         MVI   OFFICE2,C'N'        DEFAULT                                      
         LA    R2,IO2                                                           
         MVC   CPYKEY,SPACES                                                    
         MVC   CPYKCPY,REQCOMP     COMPANY                                      
         MVC   SVKEY,CPYKEY                                                     
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCFIL,CPYKEY,IO2                            
         CLC   SVKEY,CPYKEY                                                     
         BNE   OFFXIT                                                           
*                                                                               
         USING CPYELD,R4                                                        
         LA    R4,ACCORFST(R2)                                                  
         TM    CPYSTAT4,CPYSOFF2   NEW OFFICE SYSTEM?                           
         BZ    *+8                                                              
         MVI   OFFICE2,C'Y'                                                     
OFFXIT   B     XIT                                                              
         DROP  R2,R4                                                            
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        PRNTACC, PRINT ACCOUNT                                     *           
*-------------------------------------------------------------------*           
         USING ACTRECD,R2                                                       
         USING ABLELD,R4                                                        
PRNTACC  NTR1                                                                   
         SR    R3,R3               SAVE POINTER TO PARAMETERS                   
         LA    R2,IO2                                                           
         MVC   PACC,SVACC+2                                                     
         MVC   PACCN,SVACCN                                                     
         EDIT  SVAMNT,(15,PPBALBF),2,ZERO=BLANK,MINUS=YES                       
         EDIT  NEWAMT,(15,PABALBF),2,ZERO=BLANK,MINUS=YES                       
         EDIT  TOTDR,(15,PRESTDR),2,ZERO=BLANK,MINUS=YES                        
         EDIT  TOTCR,(15,PRESTCR),2,ZERO=BLANK,MINUS=YES                        
*                                                                               
         BAS   RE,SENDPF                                                        
         GOTO1 ACREPORT                                                         
         ZAP   TOTDR,=P'0'                                                      
         ZAP   TOTCR,=P'0'                                                      
*                                                                               
UACC99X  DS    0H                                                               
         B     XIT                                                              
         DROP  R2,R4                                                            
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        DO CALCULATIONS                                                        
*-------------------------------------------------------------------*           
         USING ACKEYD,R3                                                        
DOCALC   NTR1                                                                   
         LA    R3,IO                                                            
         LR    R4,R3                                                            
         USING TRNELD,R4                                                        
         MVC   PACC,ACKEYACC+3                                                  
*                                                                               
         MVI   ELCODE,TRNELQ       TRANSACTION ELEM, X'44'                      
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
*                                                                               
         TM    TRNSTAT,TRNSDR      DEBIT?                                       
         BO    DODEBITS                                                         
         AP    ACCR,TRNAMNT        KEEP RUNNING ACCOUNT CREDIT TOTAL            
         B     XIT                                                              
*                                                                               
DODEBITS AP    ACDR,TRNAMNT        KEEP RUNNING ACCOUNT DEBIT TOTAL             
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        SEND PRINTER LINE                                                      
*-------------------------------------------------------------------*           
SENDP    NTR1                                                                   
         CLI   FRSTTIME,C'Y'                                                    
         BNE   SENDPNF                                                          
         MVI   FRSTTIME,C'N'                                                    
         B     XIT                                                              
*                                                                               
SENDPNF  EDIT  ACDR,(15,PRESTDR),2,ZERO=BLANK,MINUS=YES                         
         EDIT  ACCR,(15,PRESTCR),2,ZERO=BLANK,MINUS=YES                         
*                                                                               
         MVC   HEAD2+16(36),REQCNAME                                            
*                                                                               
         CLC   REQOPTN(3),=C'ALL'  RECOVER EVERYTHING?                          
         BNE   SENDP1                                                           
         MVC   HEAD3+12(1),ACKEYACC+1        UNIT                               
         MVC   HEAD4+12(1),ACKEYACC+2        LEDGER                             
         B     SENDP3                                                           
*                                                                               
SENDP1   MVC   HEAD3+12(1),REQUNIT                                              
         MVC   HEAD3+16(36),REQUNAME                                            
*                                                                               
         MVC   HEAD4+12(1),REQLEDG                                              
         MVC   HEAD4+16(36),REQLNAME                                            
*                                                                               
         CLI   REQOPT1,C'S'        SPECIAL?                                     
         BNE   SENDP5                                                           
SENDP3   GOTO1 DATCON,DMCB,(2,ACDTPEEL),(5,HEAD5+16)                            
         B     SENDP6                                                           
*                                                                               
SENDP5   GOTO1 DATCON,DMCB,(2,REQPDATE),(5,HEAD5+16)                            
*                                                                               
SENDP6   XC    KEY,KEY                                                          
         MVC   KEY(15),PREVCULA          NEED DESCRIPTION OF ACCOUNT            
         LA    R3,PACCN                                                         
         BAS   RE,GETNAME                                                       
SENDP7   GOTO1 ACREPORT                                                         
         AP    TOTDR,ACDR                RUNNING REPORT TOTAL                   
         AP    TOTCR,ACCR                                                       
         ZAP   ACDR,=P'0'                                                       
         ZAP   ACCR,=P'0'                                                       
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        SEND TOTAL LINE                                                        
*-------------------------------------------------------------------*           
SENDTOT  NTR1                                                                   
         GOTO1 ACREPORT                                                         
         MVC   P+1(5),=C'TOTAL'                                                 
*        EDIT  (P8,REPDR),(17,PRESTDR-3),2,MINUS=YES                            
         ZAP   DUB,REPDR(8)                                                     
         MVC   WORK(18),=X'40202020202020202020202020214B202060'                
         ED    WORK(18),DUB                                                     
         MVC   PRESTDR-3(18),WORK                                               
                                                                                
*        EDIT  (P8,REPCR),(17,PRESTCR-3),2,MINUS=YES                            
         ZAP   DUB,REPCR(8)                                                     
         MVC   WORK(18),=X'40202020202020202020202020214B202060'                
         ED    WORK(18),DUB                                                     
         MVC   PRESTCR-3(18),WORK                                               
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        SEND PRINTER LINE, FILE OPTION ONLY                                    
*-------------------------------------------------------------------*           
SENDPF   NTR1                                                                   
         MVC   HEAD2+16(36),REQCNAME                                            
*                                                                               
         CLC   REQOPTN(3),=C'ALL'  RECOVER EVERYTHING?                          
         BNE   SENDPF1                                                          
         MVC   HEAD3+12(1),SVACC   UNIT                                         
         MVC   HEAD4+12(1),SVACC+1 LEDGER                                       
         B     SENDPF2                                                          
*                                                                               
SENDPF1  MVC   HEAD3+12(1),REQUNIT                                              
         MVC   HEAD3+16(36),REQUNAME                                            
         MVC   HEAD4+12(1),REQLEDG                                              
         MVC   HEAD4+16(36),REQLNAME                                            
*                                                                               
SENDPF2  GOTO1 DATCON,DMCB,(2,REQPDATE),(5,HEAD5+16)                            
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*-------------------------------------------------------------------*           
*        GET NAME ELEMENT                                                       
*-------------------------------------------------------------------*           
GETNAME  NTR1                                                                   
         ZIC   R2,0(R1)            LENGTH                                       
         L     R3,0(R1)            ADDRESS TO PUT IT                            
         LA    R3,0(R3)                                                         
         GOTO1 DATAMGR,DMCB,DMREAD,(0,=C'ACCOUNT'),KEY,IO2                      
         CLI   8(R1),0                                                          
         BNE   GETNAME5                                                         
         USING NAMELD,R4                                                        
         LA    R4,IO2                                                           
         MVI   ELCODE,NAMELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   GETNAME5                                                         
         ZIC   R1,NAMLN                                                         
         LTR   R2,R2                                                            
         BZ    GETNAME3                                                         
         CR    R1,R2                                                            
         BNH   GETNAME3                                                         
         LR    R1,R2                                                            
GETNAME3 SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),NAMEREC                                                  
         B     XIT                                                              
GETNAME5 MVC   0(3,R3),=C'N/A'                                                  
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        RUNLINIT                                                               
*-------------------------------------------------------------------*           
RUNLINIT ZAP   TOTDR,=P'0'                                                      
         ZAP   TOTCR,=P'0'                                                      
         ZAP   OTOTCR,=P'0'                                                     
         ZAP   OTOTDR,=P'0'                                                     
         ZAP   ACDR,=P'0'                                                       
         ZAP   ACCR,=P'0'                                                       
         MVI   FRSTTIME,C'Y'                                                    
         BR    RE                                                               
         EJECT                                                                  
         GETEL R4,DATADISP,ELCODE                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        CONSTANTS                                                              
*-------------------------------------------------------------------*           
PRNTBL   DC    V(PRNTBL)                                                        
HELLO    DC    V(HELLO)                                                         
         DC    X'FF'                                                            
*                                                                               
ACCOUNT  DC    CL8'ACCOUNT'                                                     
*                                                                               
TNDR     DC    PL6'0'                                                           
TNCR     DC    PL6'0'                                                           
*                                                                               
CHAREC   DC    PL6'0'                                                           
*                                                                               
ACDR     DC    PL6'0'              ACCOUNT                                      
ACCR     DC    PL6'0'                                                           
*                                                                               
OFFICE2  DS    C'N'                2-BYTE OFFICE?                               
SVCACC   DS    0CL15                                                            
SVCOMP   DC    X'00'               SAVE COMPANY                                 
SVACC    DC    CL14' '             SAVE ACCOUNT                                 
SVOFF    DC    CL2' '              SAVE 2 BYTE OFFICE                           
OTOTDR   DC    PL8'0'              OFFICE TOTAL DEBIT                           
OTOTCR   DC    PL8'0'              OFFICE TOTAL CREDIT                          
TOTDR    DC    PL8'0'                                                           
TOTCR    DC    PL8'0'                                                           
REPDR    DC    PL8'0'              REPORT DEBIT AND CREDIT                      
REPCR    DC    PL8'0'                                                           
SVAMNT   DC    PL8'0'                                                           
NEWAMT   DC    PL8'0'                                                           
SVACCN   DC    CL35' '                                                          
*                                                                               
DUMPCNT  DC    PL4'0'                                                           
EVERY    DC    PL4'1'                                                           
PDUMP    DC    PL4'0'                                                           
MAXDUMP  DC    PL4'50'                                                          
INPRECS  DC    PL4'0'              # OF INPUT RECORDS                           
REMRECS  DC    PL4'0'              # OF REMAINING RECORDS                       
RCVRECS  DC    PL4'0'              # OF RECOVERED RECORDS                       
SAVELEN  DS    H                                                                
PEELDATE DC    XL2'00'                                                          
CMPULACL DC    X'00'                                                            
FRSTTIME DC    C'Y'                                                             
PREVCULA DC    CL15' '                                                          
NUMREQST DC    X'00'               NUMBER OF REQUEST CARDS                      
CURREQST DC    F'0'                POINTER TO CURRENT REQUEST                   
CURRKEY  DC    CL42' '                                                          
CHKKEY   DC    CL56' '                                                          
SVKEY    DC    CL42' '                                                          
ACOLDN   DC    C'OLDN'                                                          
HISTDCB  DC    A(0)                                                             
ACCHST   DC    CL8'ACCHST'                                                      
ACCFIL   DC    CL8'ACCOUNT'                                                     
ACCDIR   DC    CL8'ACCDIR'                                                      
DMOPEN   DC    CL8'DMOPEN'                                                      
DMCLOSE  DC    CL8'DMCLSE'                                                      
DMDTF    DC    CL8'DTFADD'                                                      
TMPWRK   DC    XL64'00'                                                         
*                                                                               
DISKADDR DC    F'0'                                                             
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
REQMAXNO EQU   20                                                               
REQTABL  DS    (REQMAXNO*REQTABLN)C                                             
         EJECT                                                                  
REQTABLD DSECT                     REQUEST CARDS' DSECT                         
REQCOMP  DS    CL1                 COMPANY CODE                                 
REQUNIT  DS    CL1                 UNIT                                         
REQLEDG  DS    CL1                 LEDGER                                       
REQACCNT DS    CL12                ACCOUNT                                      
REQCMPLN DS    XL1                 REQUEST COMPARE LENGTH (EX CLC)              
REQPDATE DS    CL2                 PEEL DATE (COMPRESSED)                       
REQCNAME DS    CL36                COMPANY NAME                                 
REQUNAME DS    CL36                UNIT NAME                                    
REQLNAME DS    CL36                LEDGER NAME                                  
REQOPTN  DS    0CL7                OPTIONS                                      
REQOPT1  DS    CL1                 OPTION 1                                     
REQOPT2  DS    CL1                 OPTION 2                                     
REQOPT3  DS    CL1                 OPTION 3                                     
REQOPT4  DS    CL1                 OPTION 4                                     
REQOPT5  DS    CL1                 OPTION 5                                     
REQOPT6  DS    CL1                 OPTION 6                                     
REQOPT7  DS    CL1                 OPTION 7                                     
* FROM CONTINUATION CARD                                                        
REQCINFO DS    0CL23               CONTINUATION CARD INFO                       
REQCUL   DS    CL2                 CONTRA U/L                                   
REQCACCT DS    CL12                CONTRA ACCOUNT                               
REQREF   DS    CL6                 BATCH REFERENCE                              
REQTDATE DS    CL3                 TRANSACTION DATE                             
REQTABLN EQU   *-REQTABLD                                                       
*                                                                               
ACHDD    DSECT                                                                  
PARM     DS    6F                                                               
TODAY2   DS    CL2                                                              
DMPSW    DS    CL1                                                              
ACTSW    DS    CL1                                                              
ELCODE   DS    CL1                                                              
WRK      DS    CL(WRKLNQ)                                                       
IOLEN    DS    F                   LENGTH, FIRST 2 BYTES                        
IO       DS    2000C                                                            
IO2      DS    2000C                                                            
         EJECT                                                                  
PLINED   DSECT                                                                  
         DS    XL2                                                              
PACC     DS    CL12                ACCOUNT                                      
         DS    CL6                                                              
PACCN    DS    CL12                ACCOUNT NAME                                 
         DS    CL4                                                              
POFFICE  DS    CL2                 OFFICE                                       
         DS    CL4                                                              
PPBALBF  DS    CL15                PREVIOUS BALANCE B/F                         
         DS    CL5                                                              
PRESTDR  DS    CL15                RESTORED DEBIT                               
         DS    CL5                                                              
PRESTCR  DS    CL15                RESTORED CREDIT                              
         DS    CL5                                                              
PABALBF  DS    CL15                ADJUSTED BALANCE B/F                         
         EJECT                                                                  
*              DSECT FOR WORK RECORD                                            
*                                                                               
WRKD     DSECT                                                                  
WRKCON   DS    XL15                CONTRA                                       
WRKDTE   DS    CL3                 DATE                                         
WRKTYP   DS    XL1                                                              
WRKLEN   EQU   *-WRKD                                                           
WRKDR    DS    PL8                 DEBIT                                        
WRKCR    DS    PL8                 CREDIT                                       
WRKLNQ   EQU   *-WRKD                                                           
         EJECT                                                                  
*              DSECT FOR THE BINSRCH LIST                                       
*                                                                               
BIND     DSECT                                                                  
BININ    DS    F                   NUMBER IN TABLE                              
BINLEN   DS    F                   RECORD LENGTH                                
BINDISP  DS    CL1                 DISPLACEMENT IN RECORD                       
BINKEY   DS    CL3                 KEY LENGTH                                   
BINMAX   DS    F                   MAXIMUM NUMBER IN TABLE                      
BINNUMB  DS    CL1                 NUMBER OF BUCKETS                            
BINFRST  DS    CL1                 DISP. TO FIRST BUCKET                        
BINSTAT  DS    CL1                 X'80' BINARY DATA                            
         DS    CL1                 SPARE                                        
BINTABLE DS    0CL1                                                             
         SPACE 2                                                                
*              DSECT FOR THE CORRECTION TABLE                                   
*                                                                               
TMD      DSECT                                                                  
TMPER    DS    CL12                PERSON                                       
TMOFF    DS    CL2                 OFFICE                                       
TMOLD    DS    XL3                 OLD DATE                                     
TMNEW    DS    XL3                 NEW DATE                                     
TMLNQ    EQU   *-TMD                                                            
         SPACE 2                                                                
*  ACREPWORKD                                                                   
*  ACGENBOTH                                                                    
*  ACGENFILE                                                                    
*  ACGENMODES                                                                   
*  ACMASTD                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'047ACREPHD02 05/01/02'                                      
         END                                                                    
