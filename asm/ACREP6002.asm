*          DATA SET ACREP6002  AT LEVEL 022 AS OF 05/01/02                      
*PHASE AC6002A,+0                                                               
*INCLUDE SQUASHER                                                               
         TITLE 'CASH CONTROL REPORT'                                            
AC6002   CSECT                                                                  
         PRINT NOGEN                                                            
         USING ACWORKD,RA                                                       
         USING AC60D,RC                                                         
         NMOD1 0,**AC60**,RR=R5                                                 
         L     RA,0(R1)                                                         
         LA    RC,SPACEND                                                       
         ST    R5,RELO                                                          
         EJECT                                                                  
**********************************************************************          
*              RUNFRST                                                          
**********************************************************************          
         SPACE 1                                                                
         CLI   MODE,RUNFRST                                                     
         BNE   RQF20                                                            
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         MVI   FCSUPOFC,C'Y'        SUPPRESS OFFICE SECURTIY FILTERING          
         MVI   RCSUBPRG,0                                                       
         MVC   MYLIN1,SPACES                                                    
         MVC   MYLIN2,SPACES                                                    
         MVI   MYSPAC,1                                                         
         MVI   MYHEAD,C'N'                                                      
         GOTO1 DATCON,DMCB,(4,RCDATE),(2,TODAY2)                                
         GOTO1 DATCON,DMCB,(4,RCDATE),(1,TODAYP)                                
         B     CCEXT                                                            
         EJECT                                                                  
**********************************************************************          
*              REQFRST                                                          
**********************************************************************          
         SPACE 1                                                                
RQF20    CLI   MODE,REQFRST                                                     
         BNE   LGF10                                                            
         CLI   QOPT1,C' '                                                       
         BNE   *+12                                                             
         MVI   FORCEHED,C'Y'                                                    
         B     RQF22                                                            
         CLI   LINE,X'2C'                                                       
         BL    *+12                                                             
         MVI   FORCEHED,C'Y'                                                    
         B     RQF22                                                            
         MVI   SPACING,3                                                        
         GOTO1 ACREPORT                                                         
         MVI   MYHEAD,C'Y'                                                      
*                                                                               
RQF22    CLI   PROGPROF,C'Y'                                                    
         BE    *+12                                                             
         MVI   OFFANAL,C'N'                                                     
         B     RQF35                                                            
         L     R4,ADCMPEL                                                       
         USING ACCOMPD,R4                                                       
         TM    ACMPSTAT,X'20'                                                   
         BO    *+12                                                             
         MVI   OFFANAL,C'N'        NO OFFICE ANALYSIS REQUIRED                  
         B     RQF35                                                            
         MVI   OFFANAL,C'Y'                                                     
         SPACE 1                                                                
         L     R8,=A(OFFTAB)                                                    
         A     R8,RELO                                                          
         L     R2,ADCOMP           BUILD OFFICE LIST FROM 2D                    
         MVC   MYKEY,SPACES                                                     
         MVC   MYKEY(1),0(R2)                                                   
         MVC   MYKEY+1(2),=C'2D'                                                
         MVC   SAVEKEY,MYKEY                                                    
*                                                                               
         USING OFFD,R8                                                          
RQF25    XC    OFFCODE,OFFCODE                                                  
         MVC   OFFNAME,SPACES                                                   
         ZAP   OFFBEF,=P'0'                                                     
         ZAP   OFFAFT,=P'0'                                                     
         ZIC   R3,MYKEY+3                                                       
         AH    R3,=H'1'                                                         
         STC   R3,MYKEY+3                                                       
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'ACCOUNT',MYKEY,MYKEY                      
         CLC   SAVEKEY(3),MYKEY                                                 
         BNE   RQF30                                                            
         CLC   MYKEY+4(38),SPACES                                               
         BNE   RQF30                                                            
         MVC   OFFCODE,MYKEY+3     OFFICE CODE                                  
         LA    R2,MYKEY                                                         
         BAS   RE,NAMOUT           OFFICE NAME                                  
         LA    R8,OFFLNQ(R8)                                                    
         B     RQF25                                                            
*                                                                               
RQF30    MVI   OFFCODE,X'FE'                                                    
         MVC   OFFNAME,SPACES                                                   
         MVC   OFFNAME(6),=CL6'OTHERS'                                          
         ZAP   OFFBEF,=P'0'                                                     
         ZAP   OFFAFT,=P'0'                                                     
         LA    R8,OFFLNQ(R8)                                                    
*                                                                               
         MVI   OFFCODE,X'FF'                                                    
         MVC   OFFNAME,SPACES                                                   
         MVC   OFFNAME(17),=CL17'TOTAL UNDISBURSED'                             
         ZAP   OFFBEF,=P'0'                                                     
         ZAP   OFFAFT,=P'0'                                                     
         ST    R8,ATOT                                                          
*                                                                               
RQF35    LA    R3,DEBITS           CLEAR BUCKETS                                
         LA    R1,BUCK#                                                         
         ZAP   0(8,R3),=P'0'                                                    
         LA    R3,8(R3)                                                         
         BCT   R1,*-10                                                          
*                                                                               
RQF50    XC    MEDCNT,MEDCNT                                                    
         MVI   CKSW,C'N'                                                        
         GOTO1 HEXOUT,DMCB,QCOMPANY,HEAD5+12,1                                  
         MVC   SVHEX,HEAD5+12                                                   
         B     CCEXT                                                            
         EJECT                                                                  
**********************************************************************          
*              LEDGFRST                                                         
**********************************************************************          
         SPACE 1                                                                
LGF10    CLI   MODE,LEDGFRST                                                    
         BNE   LVA10                                                            
*&&UK*&& ZAP   CD,=P'0'                                                         
         MVC   LEDGNAM,SPACES                                                   
         MVC   START,SPACES                                                     
         L     R3,ADLEDGER                                                      
         MVC   LEDGNAM(1),2(R3)                                                 
         DROP  R4                                                               
*                                                                               
         USING ACCOMPD,R2                                                       
         USING CHARECD,RF          USE CHECK RECD X'10' DSECT                   
         L     R2,ADCMPEL                                                       
         L     R4,ADLEDGER         PT TO LEDGER RECD                            
         MVI   CHKFLG,C'N'         CLEAR FLAG->DEFAULT: LEDGER RECD             
         MVC   MYKEY,SPACES        READ CHK FILE                                
         LA    RF,MYKEY                                                         
         MVI   CHAKTYP,CHAKTYPQ                                                 
         MVC   CHAKCULA,0(R4)      BUILD CHECK KEY FROM LEDGER KEY              
         GOTO1 DATAMGR,DMCB,DMREAD,=C'ACCOUNT',MYKEY,IOAREA                     
         CLI   DMCB+8,0            WAS RECORD FOUND?                            
         BNE   *+12                NOT THERE - R4 PTS TO LEDGER RECD            
         MVI   CHKFLG,C'Y'         FLAG SHOWS WE'RE USING CHK RECD              
         LA    R4,IOAREA           PT TO CHK RECORD                             
         DROP  RF                                                               
*                                                                               
         USING ACOFFD,R4                                                        
         MVI   ELCODE,X'54'                                                     
         BAS   RE,GETEL                                                         
         BNE   LGF40                                                            
LGF20    CLC   ACOFID,ACMPID                                                    
         BE    LGF30                                                            
         BAS   RE,NEXTEL                                                        
         BE    LGF20                                                            
         SPACE 1                                                                
         L     R4,ADLEDGER                                                      
         CLI   CHKFLG,C'Y'         ARE WE USING CHECK RECD?                     
         BNE   *+8                 NO,                                          
         LA    R4,IOAREA           YES, PT TO CHK RECD                          
         BAS   RE,GETEL            CAN'T FIND PRINCIPAL ID.                     
LGF24    BNE   LGF40                                                            
         TM    ACOFSTAT,X'80'      SO USE ONE THAT IS PRINTED AT DDS            
         BZ    LGF30                                                            
         BAS   RE,NEXTEL                                                        
         B     LGF24                                                            
         SPACE 1                                                                
LGF30    MVC   START,ACOFBEF                                                    
         MVC   END,ACOFAFT                                                      
         MVC   LAST,SPACES                                                      
         CLI   ACOFLEN,X'2B'                                                    
         BL    LGF40                                                            
         MVC   LAST,ACOFLAST                                                    
         OC    LAST,SPACES                                                      
*                                                                               
         USING ACNAMED,R4                                                       
LGF40    L     R4,ADLDGNAM                                                      
         ZIC   R3,ACNMLEN                                                       
         SH    R3,=H'3'                                                         
         BM    CCEXT                                                            
         EXMVC R3,LEDGNAM+2,ACNMNAME                                            
         B     CCEXT                                                            
         EJECT                                                                  
**********************************************************************          
*              LEVAFRST                                                         
**********************************************************************          
         SPACE 1                                                                
LVA10    CLI   MODE,LEVAFRST                                                    
         BNE   PTN10                                                            
         CLC   QUNIT(2),=C'SN'     NO MEDIA FOR TALENT                          
         BE    PTN10                                                            
         CLC   QUNIT(2),=C'SX'     NO MEDIA FOR EXPENSE (COKE)                  
         BE    PTN10                                                            
         L     R2,MEDCNT           NUMBER IN TABLE                              
         MH    R2,=H'52'                                                        
         A     R2,=A(MEDTAB)                                                    
         A     R2,RELO                                                          
         MVC   0(36,R2),SPACES                                                  
*                                                                               
         USING ACNAMED,R4                                                       
         L     R4,ADLVANAM                                                      
         ZIC   R3,ACNMLEN                                                       
         SH    R3,=H'3'                                                         
         BM    CCEXT                                                            
         EXMVC R3,0(R2),ACNMNAME                                                
         B     CCEXT                                                            
         EJECT                                                                  
**********************************************************************          
*              PROCTRNS                                                         
**********************************************************************          
         SPACE 1                                                                
         USING TRANSD,R4                                                        
PTN10    CLI   MODE,PROCTRNS                                                    
         BNE   CC60                                                             
         L     R4,ADTRANS                                                       
         CLI   0(R4),X'44'                                                      
         BNE   CCEXT                                                            
LK64     ZIC   R1,1(R4)                                                         
         AR    R4,R1                                                            
         CLI   0(R4),0                                                          
         BE    LK60                                                             
         CLI   0(R4),X'64'         LOOK FOR MANUAL CHEQUE ELEMENT               
         BNE   LK64                                                             
         USING TRMAPD,R4                                                        
         CLC   TRMAPNO,SPACES      IF MARKED BY $INV-CH ACTION                  
         BE    LK60                WE ARE NOT INTERESTED HERE                   
         B     CCEXT                                                            
         SPACE 1                                                                
LK60     L     R4,ADTRANS                                                       
LK62     ZIC   R1,1(R4)                                                         
         AR    R4,R1                                                            
         CLI   0(R4),0                                                          
         BE    NO60                                                             
         CLI   0(R4),X'60'         LOOK FOR DATE ELEMENT                        
         BNE   LK62                                                             
         USING TRSTATD,R4                                                       
*&&US                                                                           
         CLC   QUNIT(2),=C'SN'     ALL OK FOR TALENT                            
         BE    *+14                                                             
         CLC   TRSTDATE,TODAY2                                                  
         BH    CCEXT               ADDED AFTER RUN DATE SKIP                    
*&&                                CANNOT DO THIS IN UK AS                      
*                                  UPDATE CAN RUN AFTER MIDNIGHT                
         B     USEIT                                                            
         SPACE 1                                                                
NO60     L     R4,ADTRANS                                                       
         USING TRANSD,R4                                                        
*&&US                                                                           
         CLC   QUNIT(2),=C'SN'     ALL OK FOR TALENT                            
         BE    *+14                                                             
         CLC   TRNSDATE,TODAYP                                                  
         BH    CCEXT               ADDED AFTER RUN DATE SKIP                    
*&&                                                                             
         SPACE 1                                                                
         USING TRANSD,R4                                                        
USEIT    L     R4,ADTRANS                                                       
         CLI   OFFANAL,C'N'                                                     
         BE    CC50C                                                            
         L     R8,=A(OFFTAB)                                                    
         A     R8,RELO                                                          
         L     R2,ATOT             PT R2 AT TOTAL LINE                          
         SPACE 1                                                                
         USING OFFD,R8                                                          
CC50A    CLC   OFFCODE,TRNSANAL    PT R8 TO OFFICE LINE IN OFFTAB               
         BE    CC50B                                                            
         CLI   OFFCODE,X'FE'                                                    
         BE    CC50B                                                            
         LA    R8,OFFLNQ(R8)                                                    
         B     CC50A                                                            
         SPACE 1                                                                
CC50B    LR    R3,R8               SAVE R8 AT R3                                
CC50C    TM    TRNSSTAT,X'80'      DEBIT                                        
         BO    CC53                                                             
         SPACE 1                                                                
CC51     AP    CREDITS(8),TRNSAMNT      CREDITS  BEFORE  AND                    
         AP    CREDITS+8(8),TRNSAMNT             AFTER                          
         AP    MEDIA(8),TRNSAMNT                                                
         AP    MEDIA+8(8),TRNSAMNT                                              
         LR    RF,R4                                                            
         SH    RF,DATADISP                                                      
         SPACE 2                                                                
         CLC   QUNIT(2),=C'SN'     TALENT LEDGER                                
         BNE   NOSN                                                             
         TM    TRNSSTAT,X'04'      BILLED/PAID OUTSIDE SYSTEM                   
         BO    CC54                DONT USE IT                                  
         SPACE 2                                                                
         USING ACKEYD,RF                                                        
NOSN     TM    TRNSSTAT,X'20'      IF OFFSET AND USED TODAY                     
         BZ    NOSN1                                                            
         CLC   ACDTUSED,TODAY2                                                  
         BNE   NOSN1                                                            
         XC    ACDTUSED,ACDTUSED   COUNT IT AS NIT USED                         
NOSN1    CLC   ACDTUSED,TODAY2     IF USED AFTER RUN DATE                       
         BNH   *+10                HANDLE AS NOT USED                           
         XC    ACDTUSED,ACDTUSED                                                
         OC    ACDTUSED,ACDTUSED                                                
         BZ    CC54                NOT USED YET                                 
         CLC   ACDTUSED,TODAY2                                                  
         BL    CC54                USED BEFORE TODAY                            
USESN    AP    DEBITS+8(8),TRNSAMNT                                             
         AP    CKSWRTN,TRNSAMNT                                                 
         CLI   QOPT2,C'Y'          OPT2=Y GENERATES A TRACE REPORT              
         BNE   *+8                                                              
         BAS   RE,TRACE                                                         
         SP    MEDIA+8(8),TRNSAMNT                                              
         MVI   CKSW,C'Y'                                                        
*&&UK                                                                           
         LR    RF,R4               FIND ANY DISCOUNT ELEMENTS                   
CC52     ZIC   R1,1(RF)                                                         
         AR    RF,R1                                                            
         CLI   0(RF),0                                                          
         BE    CCEXT                                                            
         CLI   0(RF),X'50'                                                      
         BNE   CC52                                                             
         USING TRCASHD,RF                                                       
         CLI   TRCSTYPE,C'D'                                                    
         BNE   CCEXT                                                            
         AP    CD,TRCSAMNT                                                      
*&&                                                                             
         B     CC56                                                             
         SPACE 1                                                                
**********************************************************************          
*             TRACE                                                             
**********************************************************************          
*                                                                               
TRACE    NTR1                                                                   
         MVI   RCSUBPRG,1                                                       
         L     R2,ADACC                                                         
         MVC   P+1(12),3(R2)                                                    
         MVC   P+20(6),TRNSREF                                                  
         GOTO1 DATCON,DMCB,(1,TRNSDATE),(5,P+30)                                
         EDIT  (P6,TRNSAMNT),(13,P+38),2,COMMAS=YES,FLOAT=-                     
         MVC   P+56(5),=C'DEBIT'                                                
         TM    TRNSSTAT,X'80'                                                   
         BO    *+10                                                             
         MVC   P+56(6),=C'CREDIT'                                               
         AP    ACCTOT,TRNSAMNT                                                  
         MVC   HEAD5+12(2),SVHEX                                                
         GOTO1 ACREPORT                                                         
         MVI   GOTONE,C'Y'                                                      
         B     CCEXT                                                            
ACCTOT   DC    PL6'0'                                                           
GOTONE   DC    C'N'                                                             
         SPACE 1                                                                
CC53     DC    0H'0'                                                            
*&&US                                                                           
         CLI   TRNSTYPE,X'81'      IF ITS A DEBIT BUT NOT A CHECK               
         BE    CC53A                                                            
         ZAP   WORK(8),TRNSAMNT                                                 
         MP    WORK(8),=P'-1'      REVERSE SIGN HANDLE AS CREDIT                
         ZAP   TRNSAMNT,WORK+2(6)                                               
*        MP    TRNSAMNT,=P'-1'     REVERSE SIGN HANDLE AS CREDIT                
         B     CC51                                                             
*&&                                                                             
CC53A    AP    DEBITS(8),TRNSAMNT                                               
         AP    DEBITS+8(8),TRNSAMNT                                             
         SP    MEDIA(8),TRNSAMNT                                                
         SP    MEDIA+8(8),TRNSAMNT                                              
         B     CC58                                                             
         SPACE 1                                                                
CC54     CLI   OFFANAL,C'N'                                                     
         BE    CCEXT                                                            
         AP    OFFAFT,TRNSAMNT                                                  
         LR    R8,R2                                                            
         AP    OFFAFT,TRNSAMNT                                                  
         LR    R8,R3                                                            
         SPACE 1                                                                
CC56     CLI   OFFANAL,C'N'                                                     
         BE    CCEXT                                                            
         AP    OFFBEF,TRNSAMNT                                                  
         LR    R8,R2                                                            
         AP    OFFBEF,TRNSAMNT                                                  
         LR    R8,R3                                                            
         B     CCEXT                                                            
         SPACE 1                                                                
CC58     CLI   OFFANAL,C'N'                                                     
         BE    CCEXT                                                            
         SP    OFFBEF,TRNSAMNT                                                  
         SP    OFFAFT,TRNSAMNT                                                  
         LR    R8,R2                                                            
         SP    OFFBEF,TRNSAMNT                                                  
         SP    OFFAFT,TRNSAMNT                                                  
         B     CCEXT                                                            
         EJECT                                                                  
**********************************************************************          
*              LEVALAST                                                         
**********************************************************************          
         SPACE 1                                                                
CC60     CLI   MODE,LEVALAST                                                    
         BNE   CC70                                                             
         CLC   QUNIT(2),=C'SN'                                                  
         BE    CC70                NO MEDIA FOR TALENT                          
         CLC   QUNIT(2),=C'SX'     NO MEDIA FOR EXPENSE (COKE)                  
         BE    CC70                                                             
         L     R2,MEDCNT                                                        
         MH    R2,=H'52'                                                        
         A     R2,=A(MEDTAB)                                                    
         A     R2,RELO                                                          
         ZAP   36(8,R2),MEDIA(8)                                                
         ZAP   44(8,R2),MEDIA+8(8)                                              
         ZAP   MEDIA(8),=P'0'                                                   
         ZAP   MEDIA+8(8),=P'0'                                                 
         L     R3,MEDCNT                                                        
         LA    R3,1(R3)                                                         
         ST    R3,MEDCNT                                                        
         CLC   MEDCNT,=A(MEDMAX)                                                
         BNH   *+6                                                              
         DC    H'00'                                                            
         B     CCEXT                                                            
         EJECT                                                                  
**********************************************************************          
*              REQLAST                                                          
**********************************************************************          
         SPACE 1                                                                
CC70     CLI   MODE,REQLAST                                                     
         BNE   CCAL                                                             
         MVI   RCSUBPRG,0                                                       
         CLI   QOPT2,C'Y'                                                       
         BNE   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         CLI   FORCEHED,C'Y'                                                    
         BNE   *+12                                                             
         BAS   R7,NEWP2                                                         
         B     CC72                                                             
         CLI   MYHEAD,C'Y'                                                      
         BNE   CC72                                                             
         BAS   R7,NEWP2                                                         
         MVI   MYHEAD,C'N'                                                      
         SPACE 1                                                                
CC72     MVC   TOTS,=F'1'                                                       
         CLI   CKSW,C'N'                                                        
         BE    *+10                                                             
         MVC   TOTS,=F'2'                                                       
*&&US*&& MVC   P+2(21),=C'TOTAL CHECKS (DEBITS)'                                
*&&UK*&& MVC   P+2(22),=C'TOTAL CHEQUES (DEBITS)'                               
         LA    R2,DEBITS                                                        
         MVI   SPACING,2                                                        
         MVI   PRTSW,C'Y'                                                       
         BAS   R6,PLINE                                                         
         SPACE 1                                                                
         MVC   P+2(26),=C'TOTAL CLEARANCES (CREDITS)'                           
         LA    R2,CREDITS                                                       
         MVI   SPACING,2                                                        
         MVI   PRTSW,C'Y'                                                       
         BAS   R6,PLINE                                                         
         SPACE 1                                                                
         SP    CREDITS(8),DEBITS(8)                                             
         SP    CREDITS+8(8),DEBITS+8(8)                                         
         LA    R2,CREDITS                                                       
         MVC   P+2(17),=C'TOTAL UNDISBURSED'                                    
         MVI   SPACING,2                                                        
         MVI   PRTSW,C'Y'                                                       
         BAS   R6,PLINE                                                         
         SPACE 1                                                                
         CLC   QUNIT(2),=C'SN'                NO ANAL FOR TALENT                
         BE    CC79                           GO SEE CHECKS WRITTEN             
         CLC   QUNIT(2),=C'SX'     NO MEDIA FOR EXPENSE (COKE)                  
         BE    CC79                                                             
         L     R1,MEDCNT                      IF NO MEDIA PROBABLY A            
         LTR   R1,R1                          ONE LEVEL LEDGER                  
         BZ    CC79                           GO SEE IF CHECKS WRITTEN          
         MVC   P+2(20),=C'UNDISBURSED ANALYSIS'                                 
         MVC   PSECOND+2(20),=20C'-'                                            
SPC2     MVI   SPACING,2                                                        
         BAS   R7,NEWP                                                          
         SPACE 1                                                                
         MVI   PRTSW,C'N'                                                       
         ZAP   MEDIA(8),=P'0'                                                   
         ZAP   MEDIA+8(8),=P'0'                                                 
         L     R5,MEDCNT                                                        
         LTR   R5,R5                                                            
         BZ    CCEXT                                                            
         L     R2,=A(MEDTAB)                                                    
         A     R2,RELO                                                          
CC75     MVC   P+2(36),0(R2)                                                    
         LA    R2,36(R2)                                                        
         AP    MEDIA(8),0(8,R2)                                                 
         AP    MEDIA+8(8),8(8,R2)                                               
         SPACE 1                                                                
         ST    R2,FULL                                                          
         BAS   R6,PLINE                                                         
         L     R2,FULL                                                          
         LA    R2,16(R2)                                                        
         BCT   R5,CC75                                                          
         MVC   HEAD5+12(2),SVHEX                                                
         GOTO1 ACREPORT                                                         
         SPACE 1                                                                
         LA    R2,MEDIA                                                         
         MVC   P+2(17),=C'TOTAL UNDISBURSED'                                    
         MVI   PRTSW,C'Y'                                                       
         MVI   SPACING,2                                                        
         BAS   R6,PLINE                                                         
         SPACE 1                                                                
CC76     CLI   OFFANAL,C'N'                                                     
         BE    CC79                                                             
         SPACE 1                                                                
         MVC   P+2(15),=C'OFFICE ANALYSIS'                                      
         MVC   PSECOND+2(15),=20C'-'                                            
         MVI   SPACING,2                                                        
         BAS   R7,NEWP                                                          
         SPACE 1                                                                
         MVI   PRTSW,C'N'                                                       
         L     R8,=A(OFFTAB)                                                    
         A     R8,RELO                                                          
         USING OFFD,R8                                                          
         SPACE 1                                                                
CC77     CLI   OFFCODE,X'FE'                                                    
         BNE   CC770                                                            
         MVC   P+2(6),=C'OTHERS'                                                
         B     CC78                                                             
         SPACE 1                                                                
CC770    CLI   OFFCODE,X'FF'                                                    
         BNE   CC771                                                            
         MVC   HEAD5+12(2),SVHEX                                                
         GOTO1 ACREPORT                                                         
         MVC   P+2(17),=C'TOTAL UNDISBURSED'                                    
         MVI   PRTSW,C'Y'                                                       
         MVI   SPACING,2                                                        
         B     CC78                                                             
CC771    MVC   P+2(1),OFFCODE                                                   
         MVI   P+3,C','                                                         
         MVC   P+4(L'OFFNAME),OFFNAME                                           
CC78     LA    R2,OFFBEF                                                        
         BAS   R6,PLINE                                                         
         SPACE 1                                                                
         CLI   OFFCODE,X'FF'                                                    
         BE    CC79                                                             
         LA    R8,OFFLNQ(R8)                                                    
         B     CC77                                                             
         SPACE 1                                                                
CC79     CLI   CKSW,C'N'                                                        
         BE    CCEXT                                                            
         SPACE 1                                                                
*&&US*&& MVC   P+2(14),=C'CHECKS WRITTEN'                                       
*&&UK                                                                           
         MVC   P+2(15),=C'CHEQUES WRITTEN'                                      
         CP    CD,=P'0'                                                         
         BE    CC80                                                             
         MVC   P+2(15),=C'TOTAL DISBURSED'                                      
         LA    R4,P+20                                                          
         LA    R3,1                                                             
         LA    R2,CKSWRTN                                                       
         BAS   R6,PLN1                                                          
         MVC   P+2(9),=C'DISCOUNTS'                                             
         LA    R2,CD                                                            
         LA    R4,P+20                                                          
         LA    R3,1                                                             
         BAS   R6,PLN1                                                          
         SP    CKSWRTN,CD                                                       
         MVC   P+2(15),=C'CHEQUES WRITTEN'                                      
CC80     DS    0H                                                               
*&&                                                                             
         LA    R4,P+20                                                          
         LA    R3,1                                                             
         LA    R2,CKSWRTN                                                       
         BAS   R6,PLN1                                                          
         SPACE 2                                                                
         CLC   START,SPACES                                                     
         BE    CCEXT                                                            
         BAS   R7,NEWP                                                          
         PACK  DUB,END                                                          
         SP    DUB,=P'1'                                                        
         UNPK  END,DUB                                                          
         OI    END+5,X'F0'                                                      
         MVC   P+2(13),=C'START NUMBER='                                        
         MVC   P+15(6),START                                                    
         MVC   P+21(12),=C',END NUMBER='                                        
         MVC   P+33(6),END                                                      
         CLC   LAST,SPACES                                                      
         BE    CC82                                                             
         MVC   P+39(22),=C',NUMBER LEFT IN STOCK='                              
         PACK  DUB,END+1(L'END-1)                                               
         PACK  DOUBLE,LAST+1(L'LAST-1)                                          
         SP    DOUBLE,DUB                                                       
         EDIT  (P8,DOUBLE),(6,P+61),ALIGN=LEFT                                  
CC82     BAS   R7,NEWP                                                          
CCAL     CLI   MODE,ACCLAST                                                     
         BNE   CCEXT                                                            
         CLI   GOTONE,C'Y'                                                      
         BNE   CCEXT                                                            
         MVC   HEAD5+12(2),SVHEX                                                
         GOTO1 ACREPORT                                                         
         MVC   P+1(13),=C'ACCOUNT TOTAL'                                        
         EDIT  (P6,ACCTOT),(13,P+38),2,COMMAS=YES,FLOAT=-                       
         SP    ACCTOT,ACCTOT                                                    
         MVC   HEAD5+12(2),SVHEX                                                
         MVI   SPACING,3                                                        
         GOTO1 ACREPORT                                                         
         MVI   SPACING,1                                                        
         MVI   GOTONE,C'N'                                                      
         SPACE 3                                                                
CCEXT    XIT1                                                                   
         EJECT                                                                  
NEWP     CLI   LINE,X'36'                                                       
         BL    NEWP4                                                            
         MVC   MYLIN1,P                                                         
         MVC   MYLIN2,PSECOND                                                   
         MVC   MYSPAC,SPACING                                                   
         MVI   FORCEHED,C'Y'                                                    
NEWP2    MVC   P,SPACES                                                         
         MVC   PSECOND,SPACES                                                   
         SPACE 1                                                                
         MVI   SPACING,3                                                        
         MVC   HEAD5+12(2),SVHEX                                                
         GOTO1 ACREPORT                                                         
         MVC   P+1(6),=C'LEDGER'                                                
         MVC   P+8(40),LEDGNAM                                                  
         GOTO1 =V(SQUASHER),DMCB,P+1,(0,48),RR=RB                               
         L     R3,DMCB+4                                                        
         SH    R3,=H'2'                                                         
         MVI   PSECOND+1,C'-'                                                   
         EXMVC R3,PSECOND+2,PSECOND+1                                           
*                                                                               
*&&US*&& MVC   P+40(13),=C'BEFORE CHECKS'                                       
*&&US*&& MVC   PSECOND+40(13),=13C'-'                                           
*&&UK*&& MVC   P+40(14),=C'BEFORE CHEQUES'                                      
*&&UK*&& MVC   PSECOND+40(14),=14C'-'                                           
         CLI   CKSW,C'N'                                                        
         BE    *+16                                                             
*&&US*&& MVC   P+60(12),=C'AFTER CHECKS'                                        
*&&US*&& MVC   PSECOND+60(12),=13C'-'                                           
*&&UK*&& MVC   P+60(13),=C'AFTER CHEQUES'                                       
*&&UK*&& MVC   PSECOND+60(13),=14C'-'                                           
         MVI   SPACING,2                                                        
         MVC   HEAD5+12(2),SVHEX                                                
         GOTO1 ACREPORT                                                         
         SPACE 1                                                                
         CLC   MYLIN1,SPACES                                                    
         BE    0(R7)                                                            
         MVC   P,MYLIN1                                                         
         MVC   PSECOND,MYLIN2                                                   
         MVC   SPACING,MYSPAC                                                   
NEWP4    GOTO1 ACREPORT                                                         
         MVC   MYLIN1,SPACES                                                    
         MVC   MYLIN2,SPACES                                                    
         MVI   MYSPAC,1                                                         
         BR    R7                                                               
         SPACE 3                                                                
PLINE    CLI   PRTSW,C'Y'                                                       
         BE    PLN0                                                             
         CP    0(8,R2),=P'0'                                                    
         BNE   PLN0                                                             
         CP    8(8,R2),=P'0'                                                    
         BNE   PLN0                                                             
         MVC   P,SPACES                                                         
         MVI   SPACING,1                                                        
         BR    R6                  DONT PRINT NIL AMOUNTS                       
PLN0     L     R3,TOTS                                                          
*&&US                                                                           
*&&US*&& LA    R4,P+40                                                          
PLN1     EDIT  (P8,0(R2)),(15,0(R4)),2,MINUS=YES,COMMAS=YES                     
*&&                                                                             
         SPACE 1                                                                
*&&UK                                                                           
         LA    R4,P+41                                                          
PLN1     EDIT  (P8,0(R2)),(14,0(R4)),2,MINUS=YES,COMMAS=YES                     
*&&                                                                             
         LA    R2,8(R2)                                                         
         LA    R4,19(R4)                                                        
         BCT   R3,PLN1                                                          
         BAS   R7,NEWP                                                          
         BR    R6                                                               
*                                                                               
**********************************************************************          
*              NAMOUT                                                           
**********************************************************************          
         USING OFFD,R8                                                          
         USING ACNAMED,R4                                                       
NAMOUT   NTR1                      R2 PTS TO MYKEY,R8 TO LINE IN OFFTAB         
         MVI   ELCODE,X'20'                                                     
         LR    R4,R2                                                            
         BAS   RE,GETEL                                                         
         BNE   NAMOUTX                                                          
         ZIC   R1,ACNMLEN                                                       
         SH    R1,=H'3'                                                         
         BM    NAMOUTX                                                          
         CH    R1,=Y(L'OFFNAME-1)                                               
         BNH   *+8                                                              
         LA    R1,L'OFFNAME-1                                                   
         EXMVC R1,OFFNAME,ACNMNAME                                              
NAMOUTX  B     CCEXT                                                            
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         SPACE 2                                                                
         DC    C'**OFTB**'                                                      
OFFTAB   DS    0F                                                               
         DS    (OFFMAX)CL(OFFLNQ)                                               
         DC    C'**MDTB**'                                                      
MEDTAB   DS    0F                                                               
         DS    (MEDMAX)CL52        36 NAME- 2PL8 BEFORE,AFTER                   
         EJECT                                                                  
AC60D    DSECT                                                                  
OFFMAX   EQU   50                                                               
MEDMAX   EQU   250                                                              
RELO     DS    F                                                                
DEBITS   DS    2PL8           BEFORE,AFTER                                      
CREDITS  DS    2PL8           BEFORE,AFTER                                      
CKSWRTN  DS    PL8            CHECKS WRITTEN                                    
MEDIA    DS    2PL8           BEFORE,AFTER   UNDISBURSED                        
BUCK#    EQU   (*-DEBITS)/8                                                     
         SPACE 1                                                                
MEDCNT   DS    F              NUMBER IN MEDIA TABLE                             
         SPACE 1                                                                
ATOT     DS    A                                                                
SAVEKEY  DS    CL3                 COMPANY,UNIT,LEDGER                          
OFFANAL  DS    CL1                 OFFICE ANALYSIS SWITCH                       
ELCODE   DS    CL1                                                              
LEDGNAM  DS    CL40                                                             
CKSW     DS    CL1           CHECKS WRITTEN N=NO, Y=YES                         
PRTSW    DS    CL1                                                              
TOTS     DS    F                                                                
CD       DS    PL8                                                              
SVHEX    DS    CL2                                                              
         SPACE 1                                                                
MYLIN1   DS    CL132                                                            
MYLIN2   DS    CL132                                                            
MYSPAC   DS    CL1                                                              
MYHEAD   DS    CL1                                                              
CHKFLG   DS    CL1                                                              
TODAY2   DS    CL2                                                              
TODAYP   DS    CL3                                                              
START    DS    CL6                                                              
END      DS    CL6                                                              
LAST     DS    CL6                                                              
MYKEY    DS    CL49                                                             
IOAREA   DS    CL2048                                                           
         SPACE 2                                                                
OFFD     DSECT                                                                  
OFFCODE  DS    CL1                                                              
OFFNAME  DS    CL36                                                             
OFFBEF   DS    PL8                                                              
OFFAFT   DS    PL8                                                              
OFFLNQ   EQU   *-OFFD                                                           
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'022ACREP6002 05/01/02'                                      
         END                                                                    
