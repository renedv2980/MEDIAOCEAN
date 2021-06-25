*          DATA SET ACREP3602  AT LEVEL 022 AS OF 06/03/15                      
*PHASE AC3602A,+0                                                               
*INCLUDE SORTER                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE CENTER                                                                 
*INCLUDE SQUASHER                                                               
         TITLE 'INTERAGENCY BRAND SELLOFF REPORT'                               
AC3602   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**AC36**,RA                                                    
         L     R8,0(R1)                                                         
         USING ACWORKD,R8                                                       
         SPACE 1                                                                
         USING AC36D,R7                                                         
         LA    R7,SPACEND                                                       
         EJECT                                                                  
*              ROUTINE FOR RUN FIRST                                            
         SPACE 1                                                                
RUNF     CLI   MODE,RUNFRST                                                     
         BNE   REQF                                                             
         SPACE 1                                                                
         RELOC RELO                                                             
         LA    RE,RELOTAB          RELOCATE MY A TYPES                          
         LA    R1,ATYPES                                                        
RELOOP   L     RF,0(RE)                                                         
         LA    RF,0(RF)                                                         
         A     RF,RELO                                                          
         ST    RF,0(R1)                                                         
         LA    RE,4(RE)                                                         
         LA    R1,4(R1)                                                         
         CLI   0(RE),X'FF'                                                      
         BNE   RELOOP                                                           
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE FOR REQUEST FIRST                                        
         SPACE 1                                                                
REQF     CLI   MODE,REQFRST                                                     
         BNE   PRCT                                                             
         MVI   RCSUBPRG,0                                                       
         SPACE 1                                                                
         MVC   PROFS,PROGPROF                                                   
         XC    ALSORT,ALSORT       CLEAR A(LAST SORT)                           
         LA    R1,SRTKLNQ                                                       
         CVD   R1,DUB              CONVERT KEY LENGTH TO CHARACTER              
         OI    DUB+7,X'0F'                                                      
         UNPK  SORTCARD+15(3),DUB+6(2)                                          
         LA    R1,SRTLNQ                                                        
         CVD   R1,DUB              CONVERT RECORD LENGTH TO CHARACTER           
         OI    DUB+7,X'0F'                                                      
         UNPK  RECCARD+22(3),DUB+6(2)                                           
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD,0                                   
         SPACE 1                                                                
         XC    STDATE1,STDATE1     NO START DATE                                
         CLC   QSTART,SPACES       UNLESS SPECIFIED                             
         BE    REQF2                                                            
         GOTO1 DATCON,DMCB,(0,QSTART),(1,STDATE1)                               
*                                                                               
REQF2    GOTO1 DATCON,DMCB,(4,RCDATE),(0,ENDATE0)                               
         CLC   QEND,SPACES         USE TODAY AS END                             
         BE    *+10                                                             
         MVC   ENDATE0,QEND        UNLESS END SPECIFIED                         
         GOTO1 DATCON,DMCB,(0,ENDATE0),(1,ENDATE1)                              
         MVC   MEDTOTS(16),=2PL8'00'  CLEAR MEDIA                               
         MVC   ACCTOTS(16),=2PL8'00'        ACCOUNT                             
         MVC   REQTOTS(16),=2PL8'00'        REQUEST TOTALS                      
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE FOR PROCESS TRANSACTION                                  
         SPACE 1                                                                
PRCT     CLI   MODE,PROCTRNS                                                    
         BNE   REQL                                                             
         USING TRANSD,R4                                                        
         L     R4,ADTRANS                                                       
         CLI   TRNSEL,X'44'                                                     
         BNE   XIT                                                              
         MVC   TYPE,TRNSTYPE                                                    
         CLI   TRNSTYPE,51         INTERAGENCY                                  
         BE    *+12                                                             
         CLI   TRNSTYPE,9          MEDIA TRANSFER                               
         BNE   XIT                                                              
         CLC   TRNSDATE,STDATE1                                                 
         BL    XIT                 BEFORE START                                 
         CLC   TRNSDATE,ENDATE1                                                 
         BH    XIT                 AFTER END DATE                               
         TM    TRNSSTAT,X'80'                                                   
         BO    XIT                 BRANCH IF DEBIT ONLY INTERESTED              
*                                   IN CREDITS                                  
         LR    R3,R4                                                            
         SH    R3,DATADISP                                                      
         USING ACKEYD,R3                                                        
         CLI   QOPT1,C'P'                                                       
         BNE   PRCT2               IF OPTION 1 =P ONLY WANT PAID                
         OC    ACDTUSED,ACDTUSED                                                
         BZ    XIT                                                              
PRCT2    CLI   QOPT1,C'U'                                                       
         BNE   PRCT5               IF OPTION 1 =U ONLY WANT UNPAID              
         OC    ACDTUSED,ACDTUSED                                                
         BNZ   XIT                                                              
         SPACE 1                                                                
         USING SRTD,R5                                                          
PRCT5    LA    R5,SRTWRK                                                        
         XC    SRTWRK(SRTLNQ),SRTWRK                                            
         ZAP   SRTPAY,=P'0'                                                     
         ZAP   SRTGRS,=P'0'                                                     
         MVC   SRTACC,ACKEYACC     ACCOUNT CODE                                 
*                                                                               
         L     R3,ADSUBAC                                                       
         USING TRSUBHD,R3                                                       
         MVC   SRTCON,TRSBACNT     CONTRA ACCOUNT                               
         MVC   SRTDSC,SPACES                                                    
         MVC   SRTEST,SPACES                                                    
         ZIC   R1,TRSBLEN                                                       
         SH    R1,=H'18'                                                        
         BM    PRCT6                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SRTDSC(0),TRSBNAME                                               
PRCT6    MVC   SRTDTE,TRNSDATE     TRANSACTION DATE                             
         MVC   SRTINV,TRNSREF      INVOICE NUMBER                               
         ZAP   SRTPAY,TRNSAMNT     AMOUNT                                       
*                                                                               
         L     R2,ADTRANS          GET OTHER NUMBER ELEMENT                     
         SH    R2,DATADISP                                                      
         GOTO1 GETEL,DMCB,(X'46',(R2)),0                                        
         CLI   ELERR,0                                                          
         BNE   PRCT7                                                            
         USING TRPAYD,R2                                                        
         L     R2,ELADDR           EXTRA PAY HAS CLIENT/PROD                    
         MVC   WORK,SPACES                                                      
         MVC   WORK(20),TRPYCLI                                                 
         MVI   WORK+21,C'-'                                                     
         MVC   WORK+22(20),TRPYPROD                                             
         GOTO1 SQUASHER,DMCB,WORK,45                                            
         MVC   SRTDSC,WORK                                                      
*                                                                               
PRCT7    L     R2,ADTRANS          GET OTHER NUMBER ELEMENT                     
         SH    R2,DATADISP                                                      
         GOTO1 GETEL,DMCB,(X'23',(R2)),0                                        
         CLI   ELERR,0                                                          
         BNE   PRCT9                                                            
         USING ACOTHERD,R2                                                      
         L     R2,ELADDR                                                        
         CLI   TYPE,9              MEDIA TRANSFER                               
         BNE   *+14                                                             
         MVC   SRTEST(3),ACOTNUM+3 ESTIMATE IS IN A DIFFERENT PLACE             
         B     *+10                                                             
         MVC   SRTEST,ACOTNUM      ESTIMATE NUMBER                              
         MVC   SRTMED,ACOTPROF     MEDIA                                        
         MVC   SRTMOS,ACOTDATE     AND MONTH OF SERVICE                         
         L     RF,AMONACC                                                       
         USING ACMD,RF                                                          
         CLC   SRTMOS,ACMMSTR       MONTH OF SERVICE FILTER                     
         BL    XIT                                                              
         CLC   SRTMOS,ACMMEND                                                   
         BH    XIT                                                              
*                                                                               
         SPACE 1                                                                
PRCT9    L     R2,ADTRANS          GET GROSS ELEMENT                            
         SH    R2,DATADISP                                                      
         GOTO1 GETEL,DMCB,(X'50',(R2)),(1,=C'G')                                
         CLI   ELERR,0                                                          
         BNE   PRCT12                                                           
         USING TRCASHD,R2                                                       
         L     R2,ELADDR                                                        
         ZAP   SRTGRS,TRCSAMNT     GROSS CREDITS                                
         SPACE 1                                                                
PRCT12   L     R2,ADTRANS          GET SALES ELEMENT                            
         SH    R2,DATADISP                                                      
         GOTO1 GETEL,DMCB,(X'1A',(R2)),0                                        
         CLI   ELERR,0                                                          
         BNE   PRCT13                                                           
         USING ACMTD,R2                                                         
         L     R2,ELADDR                                                        
         ICM   R0,15,ACMTGRS                                                    
         CVD   R0,DUB                                                           
         ZAP   SRTGRS,DUB                                                       
PRCT13   DS    0H                                                               
         GOTO1 SORTER,DMCB,=C'PUT',(R5)                                         
         MVI   ALSORT,1            ACTIVITY SWITCH                              
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE FOR REQUEST LAST                                         
         SPACE 1                                                                
REQL     CLI   MODE,REQLAST                                                     
         BNE   XIT                                                              
         OC    ALSORT,ALSORT                                                    
         BZ    REQL99              NO DATA                                      
         XC    LSTWRK(SRTLNQ),LSTWRK                                            
         SPACE 1                                                                
REQL2    GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R5,DMCB+4                                                        
         ST    R5,ALSORT           ADDRESS OF LAST SORT                         
         LTR   R5,R5                                                            
         BZ    REQL9                 END OF RECORDS FROM SORT                   
         MVC   SRTWRK(SRTLNQ),0(R5)  SAVE CURRENT SORT RECORD                   
         OC    LSTWRK(SRTLNQ),LSTWRK DO I HAVE ONE SAVED                        
         BNZ   REQL5                 BRANCH IF I DO.                            
REQL3    MVC   LSTWRK(SRTLNQ),SRTWRK SAVE THIS ONE                              
         B     REQL2                 AND GET NEXT.                              
         SPACE 1                                                                
REQL5    CLC   LSTWRK(SRTKLNQ),SRTWRK                                           
         BNE   REQL9               NOT SAME KEY                                 
         LA    R6,LSTWRK                                                        
         AP    SRTPAY-SRTD(L'SRTPAY,R6),SRTPAY   ADD'EM UP                      
         AP    SRTGRS-SRTD(L'SRTGRS,R6),SRTGRS                                  
         B     REQL2                             AND GET NEXT                   
         SPACE 1                                                                
REQL9    BAS   RE,FORMAT           SETUP PRINT                                  
         OC    ALSORT,ALSORT       IS IT END OF FILE                            
         BNZ   REQL11              NOT END OF FILE                              
         BAS   RE,MEDTOT           PRINT MEDIA TOTAL                            
         BAS   RE,ACCTOT           ACCOUNT TOTAL                                
         BAS   RE,REQTOT           AND REQUEST                                  
         B     REQL99              AND GET OUT                                  
         SPACE 1                                                                
REQL11   CLC   LSTWRK(SRTMLNQ),SRTWRK      IS IT SAME MEDIA AND ACCOUNT         
         BE    REQL3                       IF IT IS GET NEXT                    
         BAS   RE,MEDTOT                   PRINT MEDIA TOTAL                    
         CLC   LSTWRK(L'SRTACC),SRTWRK     IS IT SAME ACCOUNT                   
         BE    REQL3                                                            
         BAS   RE,ACCTOT                   PRINT ACCOUNT TOTAL                  
         B     REQL3                                                            
         SPACE 1                                                                
REQL99   GOTO1 SORTER,DMCB,=C'END'                                              
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO FORMAT DETAIL LINE                                    
         SPACE 1                                                                
FORMAT   NTR1                                                                   
         LA    R5,LSTWRK                                                        
         LA    R2,MEDTOTS          ADD TO MED/ACC/REQ                           
         LA    R0,3                                                             
FRMT3    AP    0(8,R2),SRTGRS      GROSS                                        
         AP    8(8,R2),SRTPAY      PAYABLE                                      
         LA    R2,16(R2)                                                        
         BCT   R0,FRMT3                                                         
*                                                                               
         USING SRTD,R5                                                          
         LA    R5,LSTWRK                                                        
         MVC   P+1(6),SRTINV       INVOICE TO PRINT                             
         MVC   P+9(36),SRTDSC     DESCRIPTION                                   
         MVC   P+44(2),SRTMED      MEDIA                                        
         OC    SRTMOS,SRTMOS                                                    
         BZ    FRMT5                                                            
         MVC   WORK(2),SRTMOS      MONTH OF SERVICE                             
         MVI   WORK+2,1                                                         
         GOTO1 DATCON,DMCB,(1,WORK),(6,P+49)                                    
FRMT5    MVC   P+59(6),SRTEST      ESTIMATE NUMBER                              
         OC    SRTDTE,SRTDTE                                                    
         BZ    FRMT7                                                            
         GOTO1 DATCON,DMCB,(1,SRTDTE),(8,P+69)                                  
FRMT7    EDIT  SRTGRS,(12,P+79),2,MINUS=YES                                     
         EDIT  SRTPAY,(12,P+91),2,MINUS=YES                                     
         CP    SRTGRS,=P'0'                                                     
         BE    FRMT9                                                            
         CP    SRTPAY,=P'0'                                                     
         BE    FRMT9                                                            
         ZAP   PL16,SRTPAY                                                      
         MP    PL16,=P'1000000'                                                 
         DP    PL16,SRTGRS                                                      
         EDIT  (P4,PL16+4),(8,P+103),4                                          
         LA    R0,2                REMOVE 2 TRAILING ZEROS                      
         LA    R1,P+110                                                         
         CLI   0(R1),C'0'                                                       
         BNE   *+14                                                             
         MVI   0(R1),C' '                                                       
         BCTR  R1,0                                                             
         BCT   R0,*-14                                                          
         MVI   1(R1),C'%'            AND ADD PERCENT SIGN                       
FRMT9    BAS   RE,PRNT                                                          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO HANDLE MEDIA TOTAL                                    
*                                                                               
MEDTOT   NTR1                                                                   
         BAS   RE,PRNT             SKIP LINE BEFORE MEDIA TOTAL                 
         CLC   MEDTOTS(16),=2PL8'0'                                             
         BE    XIT                 NO DATA                                      
         LA    R5,LSTWRK                                                        
         MVC   P+11(15),=C'TOTAL FOR MEDIA'                                     
         MVC   P+27(2),SRTMED                                                   
*                               GET MEDIA NAME FOR SI ACCOUNT                   
         USING ACKEYD,R3                                                        
         L     R3,AIO                                                           
         MVC   ACKEYACC(42),SPACES                                              
         MVC   ACKEYACC(1),SRTACC        COMPANY                                
         MVC   ACKEYACC+1(2),=C'SI'      SI                                     
         MVC   ACKEYACC+3(2),SRTMED      INCOME ACCOUNT                         
         MVC   KEYSAVE,ACKEYACC                                                 
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'ACCOUNT',(R3),(R3)                        
         CLC   ACKEYACC(15),KEYSAVE                                             
         BNE   MEDTOT10            CAN'T READ ACCOUNT                           
         SPACE 1                                                                
         GOTO1 GETEL,DMCB,(X'20',(R3)),0                                        
         CLI   ELERR,0                                                          
         BNE   MEDTOT10            NO NAME ELEMENT                              
         MVC   P+21(36),SPACES                                                  
         L     R2,ELADDR                                                        
         USING ACNAMED,R2          NAME TO HEADLINES                            
         ZIC   R3,ACNMLEN          ACCOUNT NAME                                 
         SH    R3,=H'3'                                                         
         BM    MEDTOT10                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   P+21(0),ACNMNAME                                                 
MEDTOT10 LA    R6,MEDTOTS                                                       
         BAS   RE,ALLTOT                                                        
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO HANDLE ACCOUNT/REQUEST TOTALS                         
*                                                                               
ACCTOT   NTR1                                                                   
         BAS   RE,PRNT             SKIP LINE BEFORE ACCOUNT TOTAL               
         CLC   ACCTOTS(16),=2PL8'0'                                             
         BE    XIT                 NO DATA                                      
         MVC   P+11(9),=C'TOTAL FOR'                                            
         MVC   P+21(36),ACCTNAME                                                
         LA    R6,ACCTOTS                                                       
         BAS   RE,ALLTOT                                                        
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         B     XIT                                                              
*                                                                               
REQTOT   NTR1                                                                   
         BAS   RE,PRNT             SKIP LINE BEFORE REQUEST TOTAL               
         CLC   REQTOTS(16),=2PL8'0'                                             
         BE    XIT                 NO DATA                                      
         MVC   P+11(17),=C'TOTAL FOR REQUEST'                                   
         LA    R6,REQTOTS                                                       
         BAS   RE,ALLTOT                                                        
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         B     XIT                                                              
*                                                                               
*                                                                               
ALLTOT   DS    0H                                                               
         ST    RE,SAVRE                                                         
         EDIT  (P8,0(R6)),(12,P+79),2,MINUS=YES                                 
         EDIT  (P8,8(R6)),(12,P+91),2,MINUS=YES                                 
         CP    0(8,R6),=P'0'                                                    
         BE    ALLTOT03                                                         
         CP    8(8,R6),=P'0'                                                    
         BE    ALLTOT03                                                         
         ZAP   PL16,8(8,R6)                   PAYAYBLE                          
         MP    PL16,=P'1000000'                                                 
         DP    PL16,0(8,R6)                   DIVIDED BY GROSS                  
         EDIT  (P4,PL16+4),(8,P+103),4                                          
         LA    R0,2                REMOVE 2 TRAILING ZEROS                      
         LA    R1,P+110                                                         
         CLI   0(R1),C'0'                                                       
         BNE   *+14                                                             
         MVI   0(R1),C' '                                                       
         BCTR  R1,0                                                             
         BCT   R0,*-14                                                          
         MVI   1(R1),C'%'            AND ADD PERCENT SIGN                       
ALLTOT03 MVC   0(16,R6),=2PL8'0'                                                
         MVI   SPACING,2                                                        
         BAS   RE,PRNT                                                          
         L     RE,SAVRE                                                         
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
*              ROUTINE TO PRINT                                                 
*                                                                               
PRNT     NTR1                                                                   
         L     R2,ADCMPNAM                                                      
         USING ACNAMED,R2                NAME TO HEADLINES                      
         ZIC   R1,ACNMLEN                                                       
         SH    R1,=H'3'                                                         
         EXMVC R1,HEAD1+40,ACNMNAME      COMPANY NAME                           
         GOTO1 CENTER,DMCB,HEAD1+40,36                                          
*                                                                               
*              GET ACCOUNT RECORD                                               
*                                                                               
         USING ACKEYD,R3                                                        
         L     R3,AIO                                                           
         MVC   ACCTNAME,SPACES                                                  
         LA    R5,LSTWRK                                                        
         CLC   ACKEYACC(15),SRTACC       ALREADY HAVE ACCOUNT                   
         BE    PRNT04                                                           
         MVC   ACKEYACC(42),SPACES                                              
         MVC   ACKEYACC(15),SRTACC                                              
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'ACCOUNT',(R3),(R3)                        
         CLC   ACKEYACC(15),SRTACC                                              
         BE    *+6                                                              
         DC    H'0'                      CAN'T READ ACCOUNT                     
*                                                                               
PRNT04   GOTO1 GETEL,DMCB,(X'20',(R3)),0                                        
         CLI   ELERR,0                                                          
         BNE   PRNT06                    NO NAME ELEMENT                        
         L     R2,ELADDR                                                        
         ZIC   R1,ACNMLEN                ACCOUNT NAME                           
         SH    R1,=H'3'                                                         
         EXMVC R1,ACCTNAME,ACNMNAME                                             
         MVC   HEAD4+2(36),ACCTNAME                                             
*                                                                               
PRNT06   CLI   QOPT2,C'Y'                Y=SHOW ACCOUNT CODE                    
         BNE   *+10                                                             
         MVC   HEAD5+2(14),ACKEYACC+1                                           
         GOTO1 ACREPORT                  AND PRINT IT                           
         B     XIT                                                              
         EJECT                                                                  
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
         GOTO1 HELLO,ELIST,(C'G',=C'ACCOUNT '),((R4),(R2)),((R5),(R3))          
         B     XIT                                                              
XIT      XIT1                                                                   
         EJECT                                                                  
*              CONSTANTS                                                        
         SPACE 1                                                                
RELOTAB  DS    0A                                                               
         DC    V(SORTER)                                                        
         DC    V(HELLO)                                                         
         DC    V(CENTER)                                                        
         DC    V(SQUASHER)                                                      
         DC    A(IO)                                                            
         DC    A(IO2)                                                           
         DC    X'FF'                                                            
         SPACE 1                                                                
SORTCARD DC    CL80'SORT FIELDS=(1,000,A),FORMAT=BI,WORK=1'                     
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=(000,,,,)'                             
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         SPACE 1                                                                
IO       DS    1008C                                                            
IO2      DS    1008C                                                            
         EJECT                                                                  
*              DSECT FOR STORAGE AREA                                           
         SPACE 1                                                                
AC36D    DSECT                                                                  
RELO     DS    F                                                                
ATYPES   DS    0A                                                               
SORTER   DS    A                                                                
HELLO    DS    A                                                                
CENTER   DS    A                                                                
SQUASHER DS    A                                                                
AIO      DS    A                                                                
AIO2     DS    A                                                                
         SPACE 1                                                                
ALSORT   DS    A                   A(LAST SORT RECORD)                          
PROFS    DS    CL16                PROGRAM PROFILES                             
SRTWRK   DS    (SRTLNQ)C           WORK AREA FOR SORT RECORD                    
LSTWRK   DS    (SRTLNQ)C           WORK AREA FOR LAST RECORD                    
ACCTNAME DS    CL36                ACCOUNT NAME                                 
PL16     DS    PL16                                                             
EBLWRK   DS    CL24                                                             
TYPE     DS    CL1                                                              
         SPACE 1                                                                
SAVRE    DS    F                                                                
ENDATE0  DS    CL6                 AS OF DATE YYMMDD                            
ENDATE1  DS    CL3                            PWOS                              
STDATE1  DS    CL3                            PWOS                              
         SPACE 1                                                                
MEDTOTS  DS    2PL8                MEDIA TOTALS                                 
ACCTOTS  DS    2PL8                ACCOUNT TOTALS                               
REQTOTS  DS    2PL8                REQUEST TOTALS                               
         SPACE 1                                                                
ELIST    DS    3F                  FOR GETL, ADDEL, DELEL                       
ELERR    DS    CL1                                                              
         ORG   ELERR               ERROR CODE FROM HELLO                        
ELADDR   DS    F                   ADDRESS OF ELEMENT FROM HELLO                
         DS    2F                                                               
         EJECT                                                                  
*              DSECT FOR SORT RECORD                                            
         SPACE 1                                                                
SRTD     DSECT                                                                  
SRTKEY   DS    0C                                                               
SRTACC   DS    CL15                PAYABLE ACCOUNT                              
SRTMED   DS    CL2                 MEDIA CODE                                   
SRTMLNQ  EQU   *-SRTKEY            MEDIA BREAK                                  
SRTCON   DS    CL15                CONTRA ACCOUNT                               
SRTDTE   DS    CL3                 INVOICE DATE (PWOS)                          
SRTINV   DS    CL6                 INVOICE NUMBER                               
SRTEST   DS    CL6                 ESTIMATE NUMBER                              
SRTMOS   DS    CL2                 MONTH OF SERVICE                             
SRTDSC   DS    CL36                DESCRIPTION                                  
SRTKLNQ  EQU   *-SRTKEY            KEY LENGTH                                   
SRTGRS   DS    PL8                 GROSS AMOUNT                                 
SRTPAY   DS    PL8                 PAYABLE                                      
SRTLNQ   EQU   *-SRTKEY            RECORD LENGTH                                
         EJECT                                                                  
*        DDLOGOD                                                                
*        ACGENBOTH                                                              
*        ACGENPOST                                                              
*        ACREPWORKD                                                             
*        ACGENMODES                                                             
*        ACMASTD                                                                
*        DDCNTRL                                                                
*        DDREPXTRAD                                                             
*        DDREPMASTD                                                             
*        DDREMOTED                                                              
*        DDEBLOCK                                                               
         PRINT OFF                                                              
       ++INCLUDE DDLOGOD                                                        
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENPOST                                                      
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACMASTD                                                        
       ++INCLUDE DDCNTRL                                                        
       ++INCLUDE DDREPXTRAD                                                     
       ++INCLUDE DDREPMASTD                                                     
       ++INCLUDE DDREMOTED                                                      
       ++INCLUDE DDEBLOCK                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'022ACREP3602 06/03/15'                                      
         END                                                                    
