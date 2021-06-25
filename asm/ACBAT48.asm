*          DATA SET ACBAT48    AT LEVEL 074 AS OF 12/04/12                      
*PHASE T61B48A                                                                  
*INCLUDE RIGHT                                                                  
T61B48   TITLE 'New Invoice - TYPE 72'                                          
T61B48   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 PROGDX-PROGD,**BAT48,R7,R8,CLEAR=YES,RR=R2                       
         USING TWAD,RA                                                          
         L     R9,4(R1)                                                         
         USING GWS,R9                                                           
         USING PROGD,RC                                                         
         L     R1,=V(RIGHT)                                                     
         AR    R1,R2                                                            
         ST    R1,RIGHT                                                         
         ST    R2,RELO1                                                         
*                                                                               
         GOTO1 VDICTAT,DMCB,C'L   ',DICI,DICO                                   
*                                                                               
         BAS   RE,GETODAY                                                       
         MVC   SAVEDATE,BOWORK1                                                 
         MVC   TMPMODE,MODE                                                     
         NI    TMPMODE,X'0F'                                                    
*                                                                               
         CLI   PFKEY,X'FF'                                                      
         BNE   *+8                                                              
         MVI   PFKEY,0                                                          
         CLI   CSSPROG,0           MAIN SCREEN?                                 
         BH    DET0010                                                          
         CLI   PFKEY,0                                                          
         BE    INV0010                                                          
*                                                                               
INV0005  CLI   PFKEY,PFK09                                                      
         BNE   INV0010                                                          
         CLI   CSACT,ACTDSP        SWITCH SCREEN ONLY FOR DISPLAY               
         BNE   INV0010             OTHER ACTIONS HAS TO VALIDATE FIELDS         
         BAS   RE,CALLDETS                                                      
         MVI   TWASCRN,DTLSCRN                                                  
         GOTO1 =A(BLDDBTS),DMCB,(RC),RR=RELO1   BUILD DEBIT SCREEN              
         B     MAIN900                                                          
*                                                                               
INV0010  DS    0H                                                               
         LA    R2,IVMORDH                                                       
         TM    IVMORDH+4,X'20'     VALIDATED?                                   
         BO    INV0050                                                          
         CLI   IVMORDH+5,0         NOTHING IN ORDER                             
         BZ    INV0050                                                          
         BAS   RE,ORDCHCK          VALIDATE ORDER                               
         BNE   ERROR                                                            
         OI    IVMORDH+4,X'20'     MARK VALIDATED                               
*                                                                               
INV0050  LA    R2,IVMREFH          VALIDATE/SAVE DOC NUMBER AND DATE            
         BAS   RE,ANY                                                           
         MVC   REFSAVE,IVMREF                                                   
         OC    REFSAVE,SPACES                                                   
         MVC   INVREF,REFSAVE                                                   
         OI    4(R2),X'20'                                                      
*                                                                               
         LA    R2,IVMDOCH          VALIDATE DOC NUMBER                          
         CLI   IVMDOCH+5,0         NO DOCUMENT NUMBER?                          
         BNZ   *+14                                                             
         MVC   IVMDOC,INVREF       USE FIRST 6 BYTES OF REFERENCE               
         OI    6(R2),X'80'                                                      
*                                                                               
         MVC   DOCSAVE,IVMDOC                                                   
         OC    DOCSAVE,SPACES                                                   
         MVC   DETLREF,DOCSAVE                                                  
         OI    4(R2),X'20'                                                      
*                                                                               
         LA    R2,IVMDAT1H                                                      
         BAS   RE,ANY              FORCE USER TO ENTER INVOICE DATE             
         GOTO1 VALDATE,DMCB,IVMDAT1H                                            
         BNE   ERROR                                                            
         MVC   IVCDATE,SAVEDATE                                                 
         MVC   INVDTE,SAVEDATE                                                  
         LA    R2,IVMPDATH                                                      
         GOTO1 VALDATE,DMCB,IVMPDATH                                            
         BNE   ERROR                                                            
         MVC   PAYDATE,SAVEDATE                                                 
*                                                                               
INV0060  LA    R2,IVMURGH         VALIDATE URGENT                               
         CLI   5(R2),0                                                          
         BE    INV0065                                                          
         CLI   8(R2),C'U'                                                       
         BE    INV0065                                                          
         CLI   8(R2),C'Y'                                                       
         BE    INV0065                                                          
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     ERROR                                                            
*                                                                               
INV0065  OI    4(R2),X'20'        TURN ON VALIDATED BIT                         
         OC    SVCSHNAM(51),SVCSHNAM  VALIDATE CASH ACCT                        
         LA    R2,IVMCSHH                                                       
         CLI   5(R2),0                                                          
         BE    INV0080                                                          
*                                                                               
         ZIC   R3,5(R2)           LENGTH OF CASH ACCT                           
         BCTR  R3,0                                                             
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),=C'SC'    CASH U/L                                      
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),8(R2)     CASH ACCT CODE                                
         BAS   RE,GETACC                                                        
         MVC   FVMSGNO,=AL2(AE$INACP)                                           
         BAS   RE,CHECKACC        VERIFY ACCT                                   
*                                                                               
         MVC   SVCSHNUM,ACCTNUM   CASH ACCT #                                   
         MVC   SVCSHNAM,ACCTNAME  CASH ACCT NAME                                
         LA    R3,IVMCSDH                                                       
         OI    6(R3),X'80'                                                      
         MVC   8(36,R3),ACCTNAME  DISPLAY CASH ACCT EXPANSION                   
*                                                                               
         LA    R2,IVMCDH                                                        
         CLI   5(R2),0            IS THERE A PROD CD?                           
         BNE   INV0155            YES - FLAG ERROR                              
*                                                                               
         LA    R2,IVMCD2H                                                       
         CLI   5(R2),0            IS THERE AN EXP CD?                           
         BNE   INV0155            YES - FLAG ERROR                              
*                                                                               
INV0080  ZAP   DISC,=P'0'         INIT DISCOUNT SAVES                           
         ZAP   DISC2,=P'0'                                                      
         OC    SVPVEN(102),SVPVEN CLEAR CREDIT ACCOUNT SAVES                    
*                                                                               
INV0090  LA    R2,IVMPVENH        PRODUCTION VENDOR FIELD                       
         CLI   5(R2),0            IS THERE A PROD VENDOR?                       
         BE    INV0300            NO                                            
         MVI   ADVSW,C'Y'         SET PROD VENDOR SWITCH                        
*                                                                               
INV0100  ZIC   R3,5(R2)           =LEN OF CREDIT ACCOUNT INPUT                  
         BCTR  R3,0               MINUS 1 FOR EX INSTRUCTION                    
         LA    R1,8(R2)                                                         
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY     FROM TDA                                      
         CLI   8(R2),C'*'         UNIT LEDGER INPUT OVERRIDE                    
         BE    INV0110            YES                                           
         LA    RF,COMPEL                                                        
         USING ACCOMPD,RF                                                       
         CLI   ADVSW,C'Y'         IS IT PROD VENDOR?                            
         BNE   INV0105            NO                                            
         MVC   KEY+1(2),ACMPSUPP  ASSUME UNLESS OVERRIDDEN                      
         B     INV0140                                                          
*                                                                               
INV0105  MVI   KEY+1,C'S'                                                       
         MVC   KEY+2(1),ACMPSUPX  ASSUME UNLESS OVERRIDDEN                      
         B     INV0140                                                          
         DROP  RF                                                               
*                                                                               
INV0110  CLI   SVCSHNUM,0         IS THERE A CASH ACCOUNT?                      
         BNE   INV0115            YES - *SC NOT ALLOWED                         
         CLC   9(2,R2),=C'SC'     IS IT CASH ACCT?                              
         BE    INV0130                                                          
*                                                                               
INV0115  MVC   FVMSGNO,=AL2(AE$INACP)                                           
         LA    RF,ADVCLIST        LIST OF OVERRIDES FOR PROD VEND               
         CLI   ADVSW,C'Y'         IS IT PRODUCTION VENDOR?                      
         BE    INV0120            YES                                           
         LA    RF,AGYCLIST        LIST OF OVERRIDES FOR EXP VEND                
*                                                                               
INV0120  CLI   0(RF),X'FF'        END OF LIST                                   
         BE    ERROR                                                            
         CLC   9(2,R2),0(RF)      INPUT TO LIST                                 
         BE    INV0130                                                          
         LA    RF,2(RF)           NEXT VALID ENTRY TO CREDIT                    
         B     INV0120                                                          
*                                                                               
INV0130  MVC   KEY+1(2),9(R2)     2 CHAR U/L FROM INPUT                         
         SH    R3,=H'3'           SUBTRACT *U/L FROM INPUT LEN                  
         LA    R1,11(R2)          POINT TO ACCT INPUT                           
INV0140  EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),0(R1)     CREDIT ACCT TO KEY                            
*                                                                               
INV0150  CLI   ADVSW,C'Y'         IS THIS A PROD VENDOR?                        
         BNE   INV0153            NO                                            
         LA    R1,IVMCDH          CD FOR PROD VENDOR                            
         B     *+8                                                              
*                                                                               
INV0153  LA    R1,IVMCD2H         CD FOR EXP VENDOR                             
         CLI   5(R1),0            INPUT                                         
         BE    INV0160            NO                                            
         CLI   SVCSHNUM,0         IS THERE A CASH ACCOUNT?                      
         BE    INV0157            NO                                            
*                                                                               
INV0155  MVC   FVMSGNO,=Y(AE$CDALL)   NO CASH DISCOUNT ALLOWED                  
         B     ERRXIT             ERROR                                         
*                                                                               
INV0157  CLI   8(R1),C'N'         IS THERE CASH DISCOUNT                        
         BE    INV0160            NO GO GET ACCT                                
         CLI   8(R1),C'Y'                                                       
         BE    INV0160                                                          
         LR    R2,R1                                                            
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     ERROR                                                            
*                                                                               
INV0160  MVC   RKEY,KEY           READ VENDOR FOR CD AND OTHER INFO             
         GOTO1 ARDHI,AIOAREA1                                                   
         L     RE,AIOAREA1                                                      
         CLC   KEY(15),0(RE)      WAS REC RETRIEVED                             
         BE    INV0170                                                          
         MVC   FVMSGNO,=AL2(AE$INACC)                                           
         B     ERROR                                                            
*                                                                               
INV0170  LA    R0,KEY              MOVE RECORD TO LOCAL STORAGE                 
         LA    R1,1000                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R4,IOAREA          FIND DISCOUNT ELEMENT                         
INV0180  CLI   0(R4),0                                                          
         BNE   INV0190            DIDN'T FIND DISCOUNT ELEMENT                  
         CLI   ADVSW,C'Y'         IS THIS A PROD VENDOR?                        
         BNE   INV0185            NO                                            
         OI    IVMCDH+6,X'80'     PROD VENDOR CD                                
         MVI   IVMCD,C' '                                                       
         B     INV0210            SKIP CD FOR PROD VENDOR                       
*                                                                               
INV0185  OI    IVMCD2H+6,X'80'    EXP VENDOR CD                                 
         MVI   IVMCD2,C' '                                                      
         B     INV0210            SKIP CD FOR EXP VENDOR                        
*                                                                               
INV0190  CLI   0(R4),X'38'        IS THIS A DIS ELEM?                           
         BE    INV0200            YES                                           
         CLI   0(R4),ITCELQ        TEST FOR INPUT TAX DEFAULT                   
         BE    INV0207                                                          
INV0195  ZIC   R3,1(R4)           LEN OF CURRENT EL                             
         AR    R4,R3              TO NEXT EL                                    
         B     INV0180                                                          
*                                                                               
INV0200  CLI   SVCSHNUM,0         TEST FOR CASH ACCOUNT                         
         BNE   INV0195             YES-NO CASH DISCOUNT ALLOWED                 
         LA    RE,IVMCDH           SET POINTER TO CD FIELD                      
         CLI   ADVSW,C'Y'          TEST PROD OR EXP VENDOR                      
         BE    *+8                                                              
         LA    RE,IVMCD2H                                                       
         CLI   8(RE),C'N'          TEST CD SUPPRESSED                           
         BE    INV0195             YES                                          
*                                                                               
         MVC   HALF,2(R4)         PLACE CD IN DISC SAVE FLD                     
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         CLI   ADVSW,C'Y'         IS THIS A PROD VENDOR?                        
         BNE   INV0203            NO                                            
         ZAP   DISC,DUB           CD VALUE FOR PROD VENDOR                      
         LA    R5,DISC                                                          
         LA    R1,IVMCDH                                                        
         LA    R6,IVMCDD                                                        
         B     INV0205            OUTPUT CD                                     
*                                                                               
INV0203  ZAP   DISC2,DUB          CD VALUE FOR EXP VENDOR                       
         LA    R5,DISC2                                                         
         LA    R1,IVMCD2H                                                       
         LA    R6,IVMCDD2                                                       
*                                                                               
INV0205  OI    6(R1),X'80'                                                      
         MVI   8(R1),C'Y'                                                       
         EDIT  (P3,(R5)),(6,(R6)),2                                             
         B     INV0195                                                          
*                                                                               
         USING ITCELD,R4                                                        
INV0207  LA    RE,PVENTYPE         SET POINTER TO DEFAULT TAX                   
         CLI   ADVSW,C'Y'          TEST FOR PRODUCTION VENDOR                   
         BE    *+8                 YES                                          
         LA    RE,EVENTYPE                                                      
         CLC   SVDATE1,ITCEFFD     TEST TRANS DATE >= EFF DATE                  
         BL    INV0195                                                          
         OC    ITCPROV,ITCPROV     NO PROV=GST                                  
         BZ    INV0209                                                          
         MVC   VENDPSTT,ITCTYPE    SAVE PST TYPE & PROV                         
         MVC   VENDPROV,ITCPROV                                                 
         B     INV0195                                                          
*                                                                               
INV0209  MVC   0(1,RE),ITCTYPE     SET DEFAULT TYPE                             
         MVC   VENDTYPE,ITCTYPE                                                 
         B     INV0195                                                          
*                                                                               
INV0210  SR    R6,R6              CLEAR FOR GETACC USE                          
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         GOTO1 AGETACC,DMCB,KEY,(R6) DONE MYSELF TO HANDLE DFLT                 
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   ERROR                                                            
*                                                                               
INV0220  MVC   FVMSGNO,=AL2(AE$INACP)                                           
         TM    ACCTSTAT,X'80'     BALANCE ELEMENT                               
         BZ    ERROR                                                            
         TM    ACCTSTAT,X'30'     LOCKED OR CLOSED                              
         BNZ   ERROR                                                            
*                                                                               
INV0230  CLI   USERPROV,C'Y'       DID USER ENTERED PROVINCE?                   
         BE    INV0235                                                          
         CLI   AGYCTRY,CTRYCAN                                                  
         BNE   INV0235                                                          
         OC    VENDPROV,VENDPROV                                                
         BZ    INV0235                                                          
         L     R2,APROVH                                                        
         MVI   5(R2),2                                                          
         OI    6(R2),X'80'         MODIFIED                                     
         MVC   8(2,R2),VENDPROV                                                 
*                                                                               
INV0235  CLI   ADVSW,C'Y'         IS THIS A PROD VENDOR?                        
         BNE   INV0280            NO                                            
         CLI   IVMPVENH+5,0       WAS PROD VENDOR INPUT?                        
         BE    INV0300            NO                                            
         MVC   SVPVEN,ACCTNUM     SAVE PROD VEN KEY                             
         MVC   SVPVENNM,ACCTNAME  SAVE PROD VEN NAME                            
         MVC   PRSTAT,ACCTSTAT    PRO 2C BIT(X'04')                             
*                                                                               
INV0240  DS    0H                                                               
         CLI   SVCSHNUM,0         IS THERE A CASH ACCT?                         
         BE    INV0245            NO - DON'T WANT 2C OR 27                      
         TM    PRSTAT,X'04'       DO WE NEED 2C FOR PROD VENDOR?                
         BZ    INV0245            NO.                                           
         MVC   KEY(15),SVPVEN     ACCT #                                        
         MVC   KEY+1(2),=C'2C'    U/L                                           
         BAS   RE,GETACC                                                        
         MVI   PCONSULT,C'Y'      SET 2C SWITCH                                 
         MVC   P2CNUM,ACCTNUM                                                   
         MVC   P2CNAM,ACCTNAME                                                  
         MVC   KEY+1(14),=CL14'27999'  BUILD 27 ACCT                            
         BAS   RE,GETACC                                                        
         MVC   PROTROL,ACCTNUM                                                  
         MVC   PROTROLN,ACCTNAME                                                
*                                                                               
INV0245  LA    R3,IVMPVNMH                                                      
         OI    6(R3),X'80'        SET TRANSBIT                                  
         MVC   8(36,R3),SVPVENNM  PRODUCTION NAME                               
         B     INV0300                                                          
         EJECT                                                                  
*                                                                               
INV0280  CLI   IVMEVENH+5,0       WAS EXP VENDOR INPUT?                         
         BNE   INV0282                                                          
         LA    RF,COMPEL                                                        
         USING CPYELD,RF                                                        
         TM    CPYSTAT5,CPYSVEND  COPY PROD VENDOR TO EXP VENDOR?               
         BZ    INV0300                                                          
         MVC   IVMEVENH+5(1),IVMPVENH+5                                         
         MVC   IVMEVEN,IVMPVEN                                                  
         OI    IVMEVENH+6,X'80'                                                 
         DROP  RF                                                               
*                                                                               
INV0282  MVC   SVEVEN,ACCTNUM     EXP VENDOR ACCT #                             
         MVC   SVEVENNM,ACCTNAME  EXP VENDOR NAME                               
         MVC   EXSTAT,ACCTSTAT    EXP 2C BIT (X'04')                            
         MVI   V29SW,0                                                          
         MVC   KEYSAVE(15),KEY                                                  
         MVC   KEY(15),SVEVEN     EXP VENDOR.                                   
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'ACCOUNT',KEY,KEY .                
         MVC   FVMSGNO,=AL2(AE$NOERR)                                           
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         BNE   ERRXIT                                                           
         LA    R1,IOAREA                                                        
         SR    RE,RE                                                            
*                                                                               
INV0283  CLI   0(R1),0            IS IT END OF RECORD?                          
         BE    INV0289                                                          
         CLI   0(R1),ACSTELQ      IS IT X'30'?                                  
         BE    INV0285                                                          
         IC    RE,1(R1)                                                         
         AR    R1,RE                                                            
         B     INV0283                                                          
*                                                                               
         USING ACSTATD,R1         STATUS DSECT                                  
INV0285  TM    ACSTSTX,X'80'      DO WE NEED VENDOR AS CONTRA?                  
         BZ    INV0289            NO.                                           
INV0287  MVI   V29SW,C'Y'         YES.                                          
*                                                                               
INV0289  MVC   KEY(15),KEYSAVE    SAVE KEY                                      
*                                                                               
INV0290  CLI   SVCSHNUM,0         IS THERE A CASH ACCT?                         
         BE    INV0295            NO - DON'T WANT 2C OR 27                      
         TM    EXSTAT,X'04'       DO WE NEED 2C FOR EXP VENDOR?                 
         BZ    INV0295            NO.                                           
*                                                                               
         MVC   KEY(15),SVEVEN                                                   
         MVC   KEY+1(2),=C'2C'                                                  
         BAS   RE,GETACC                                                        
         MVI   ECONSULT,C'Y'                                                    
         MVC   E2CNUM,ACCTNUM                                                   
         MVC   E2CNAM,ACCTNAME                                                  
         MVC   KEY+1(14),=CL14'27999'   BUILD 27                                
         BAS   RE,GETACC          GET ACCT                                      
         MVC   EXPTROL,ACCTNUM                                                  
         MVC   EXPTROLN,ACCTNAME                                                
*                                                                               
INV0295  DS    0H                                                               
         LA    R3,IVMEVNMH                                                      
         OI    6(R3),X'80'         SET TRANS                                    
         MVC   8(36,R3),SVEVENNM   DISPLAY EXP VEND EXPANSION                   
*                                                                               
INV0300  CLI   FRSTPASS,0          IS THIS THE SECOND PASS                      
         BNZ   INV0310             YES                                          
         LA    R1,IVMEVENH                                                      
         CLI   5(R1),0             IS THERE AN EXP VENDOR?                      
         BE    INV0310             NO                                           
         LR    R2,R1                                                            
         MVI   ADVSW,0                                                          
         MVI   AGYSW,C'Y'          YES - WE HAVE EXP VENDOR                     
         MVI   FRSTPASS,1          FINISHED 1ST PASS                            
         B     INV0100             NOW VERIFY EXP VENDOR                        
*                                                                               
INV0310  TM    COMPSTA2,X'04'      IS VENDOR REQUIRED?                          
         BZ    INV0320             NO                                           
         CLI   SVPVEN,0            WAS PROD VENDOR ENTERED?                     
         BNE   INV0320             YES                                          
         CLI   SVEVEN,0            WAS EXP VENDOR ENTERED?                      
         BNE   INV0320             YES                                          
         LA    R2,IVMPVENH         POINT TO PROD VENDOR FIELD                   
         MVC   FVMSGNO,=Y(AE$PEVIR)  MUST HAVE AT LEAST 1 VENDOR                
         B     ERRXIT              ERROR                                        
*                                                                               
INV0320  TM    IVMVAMTH+4,X'20'    VALIDATED?                                   
         BO    INV0330             YES, SKIP                                    
         LA    R2,IVMVAMTH                                                      
         BAS   RE,ANY              HAS TO BE SOMETHING                          
         BAS   RE,VALAMNT                                                       
         BNE   ERROR                                                            
         ZAP   VENDAMT,DUB                                                      
         EDIT  VENDAMT,(12,IVMVAMT),2,ZERO=NOBLANK                              
         OI    IVMVAMTH+6,X'80'    TRANSMIT                                     
         OI    IVMVAMTH+4,X'20'    MARK VALIDATED                               
*                                                                               
INV0330  LA    R2,IVMCOFFH                                                      
         CLI   IVMCOFFH+5,0        CREDIT OFFICE MAY NOT BE REQUIRED            
         BE    INV0340                                                          
         MVC   SVOFFICE,8(R2)                                                   
         OC    SVOFFICE,SPACES                                                  
         GOTO1 AVALOFFC,DMCB,(0,SVOFFICE)                                       
         BE    INV0340                                                          
         MVC   FVMSGNO,=AL2(AE$OFCNF)                                           
         MVC   FVXTRA(2),SVOFFICE                                               
         B     ERRXIT                                                           
*                                                                               
INV0340  LA    R2,IVMNARRH                                                      
         MVC   CRNARRLN,IVMNARRH+5                                              
         MVC   CRNARR,SPACES                                                    
         SR    R1,R1                                                            
         ICM   R1,1,CRNARRLN       COPY NARRATIVE                               
         BZ    INV0350                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CRNARR(0),IVMNARR                                                
*                                                                               
INV0350  LA    R4,IVMSUBH          VALIDATE DEBIT                               
         ST    R4,ACURLN                                                        
         LR    R1,R4                                                            
         SR    RF,RF                                                            
         BAS   RE,CHKLINE          HAS MAIN DEBIT CHANGED?                      
         BE    INV0355                                                          
         BAS   RE,VALLINE                                                       
         L     R2,FVADDR                                                        
         BNE   ERRXIT                                                           
         ZAP   TOTDETS,VENDAMT     USE VENDOR AMT AS DEBIT                      
         ZAP   NETPOST,VENDAMT                                                  
         ZAP   CRDTOT,VENDAMT                                                   
         OI    PRGSTAT,PRGMDEBT    USED MAIN DEBIT                              
*                                                                               
INV0355  MVI   FRSTPASS,1          NOTE SECOND PASS                             
         CLI   AGYCTRY,CTRYCAN     TEST CANADA                                  
         BNE   INV0600                                                          
         MVI   FRSTPASS,0          YES-USE TWO PASS METHOD                      
         LA    R2,IVCGORNH                                                      
         CLI   IVCGORNH+5,0                                                     
         BNE   *+14                                                             
         MVC   FVMSGNO,=Y(AE$MISIF)                                             
         B     ERRXIT                                                           
*                                                                               
         LA    R2,IVCGORNH         POINT TO GROSS/NET                           
         CLI   IVCGORN,C'G'        HAS TO BE 'G' OR 'N'                         
         BE    INV0360                                                          
         CLI   IVCGORN,C'N'                                                     
         BE    INV0360                                                          
         MVC   FVMSGNO,=Y(AE$GRNET)                                             
         B     ERRXIT                                                           
*                                                                               
         USING VTCD,R1                                                          
INV0360  LA    R1,GSTVATBL                                                      
         MVC   FLD,SPACES                                                       
         CLI   IVCGTYP,C'*'                                                     
         BE    INV0399             SKIP GST                                     
         MVI   VTCACTN,VTCAIVAL    VALIDATE TAX TYPE                            
         MVC   VTCCPY,COMPANY                                                   
         MVC   VTCOFFC,COFFICE                                                  
         MVC   VTCCOMF,ACOMFACS                                                 
         MVC   VTCINVD,IVCDATE                                                  
         LA    R2,IVCGTYPH                                                      
         ST    R2,VTCAFLDH                                                      
         GOTO1 VATICAN                                                          
         BE    INV0370                                                          
         MVC   FVMSGNO,=Y(AE$INVGT)                                             
         B     EXIT                                                             
*                                                                               
INV0370  TM    VTCINDS,VTCINA      IS GST AVAILABLE?                            
         BNO   INV0380                                                          
         MVC   FVMSGNO,=Y(AE$GSTNA)                                             
         B     EXIT                                                             
*                                                                               
INV0380  LA    R2,IVCGTNMH                                                      
         LA    R1,GSTVATBL                                                      
         MVC   FLD(1),VTCTYPE                                                   
         MVC   FLD+2(L'VTCTTYPE),VTCTTYPE                                       
         MVC   FLD+14(5),=C'RATE='                                              
         SR    R0,R0                                                            
         ICM   R0,3,VTCRATE                                                     
*                                                                               
         TM    VTCINDS,VTCIDC3                                                  
         BZ    INV0382                                                          
         EDIT  (R0),(5,FLD+18),3,ALIGN=LEFT                                     
         B     INV0385                                                          
*                                                                               
INV0382  EDIT  (R0),(5,FLD+19),2,ALIGN=LEFT                                     
*                                                                               
INV0385  GOTO1 SQUASHER,DMCB,FLD,L'FLD                                          
*                                                                               
INV0399  MVC   IVCGTNM(20),FLD                                                  
         OI    IVCGTNMH+6,X'80'                                                 
*                                                                               
         LA    R2,IVCGAMTH                                                      
         CLI   IVCGAMTH+5,0        ANY OVERRIDE?                                
         BZ    INV0400                                                          
         BAS   RE,VALAMNT                                                       
         BNE   ERROR                                                            
         EDIT  GSTAMT,(10,IVCGAMT),2,FLOAT=-                                    
         OI    IVCGAMTH+6,X'80'    TRANSMIT                                     
*                                                                               
INV0400  LA    R2,IVCPROVH         VALIDATE PROVINCE                            
         MVC   FLD,SPACES                                                       
         CLI   IVCPROVH+5,0        NO PROVINCE?                                 
         BE    INV0499                                                          
         LA    RF,PRVTAB                                                        
INV0410  CLI   0(RF),X'FF'         EOT                                          
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$PROVX)                                           
         B     ERRXIT                                                           
         CLC   IVCPROV,0(RF)       PROVINCE MATCHES?                            
         BE    INV0430                                                          
         LA    RF,L'PRVTAB(RF)                                                  
         B     INV0410                                                          
*                                                                               
INV0430  LA    R1,PSTVATBL                                                      
         CLI   IVCPTYP,C'*'                                                     
         BE    INV0499             SKIP PST                                     
         MVI   VTCACTN,VTCAIVAL    VALIDATE TAX TYPE                            
         MVC   VTCCPY,COMPANY                                                   
         MVC   VTCOFFC,COFFICE                                                  
         MVC   VTCCOMF,ACOMFACS                                                 
         MVC   VTCINVD,IVCDATE                                                  
         LA    R2,IVCPTYPH                                                      
         ST    R2,VTCAFLDH                                                      
         GOTO1 VATICAN                                                          
         BE    INV0470                                                          
         MVC   FVMSGNO,=Y(AE$INVPT)                                             
         B     EXIT                                                             
*                                                                               
INV0470  TM    VTCINDS,VTCINA      IS PST AVAILABLE?                            
         BNO   INV0480                                                          
         MVC   FVMSGNO,=Y(AE$PSTNA)                                             
         B     EXIT                                                             
*                                                                               
INV0480  LA    R2,IVCPTNMH                                                      
         LA    R1,PSTVATBL                                                      
         MVC   FLD(1),VTCTYPE                                                   
         MVC   FLD+2(L'VTCTTYPE),VTCTTYPE                                       
         MVC   FLD+14(5),=C'RATE='                                              
         SR    R0,R0                                                            
         ICM   R0,3,VTCRATE                                                     
*                                                                               
         TM    VTCINDS,VTCIDC3                                                  
         BZ    INV0482                                                          
         EDIT  (R0),(5,FLD+18),3,ALIGN=LEFT                                     
         B     INV0485                                                          
*                                                                               
INV0482  EDIT  (R0),(5,FLD+19),2,ALIGN=LEFT                                     
*                                                                               
INV0485  GOTO1 SQUASHER,DMCB,FLD,L'FLD                                          
*                                                                               
INV0499  MVC   IVCPTNM(20),FLD                                                  
         OI    IVCPTNMH+6,X'80'                                                 
*                                                                               
         LA    R2,IVCPAMTH                                                      
         CLI   IVCPAMTH+5,0        ANY OVERRIDE?                                
         BZ    INV0600                                                          
         BAS   RE,VALAMNT                                                       
         BNE   ERROR                                                            
         ZAP   PSTAMT,DUB                                                       
         EDIT  PSTAMT,(10,IVCPAMT),2,FLOAT=-                                    
         OI    IVCPAMTH+6,X'80'    TRANSMIT                                     
*                                                                               
         DROP  R1                                                               
*                                                                               
INV0600  DS    0H                                                               
         CLI   CSACT,ACTINP                                                     
         BNE   INV0610                                                          
         TM    PRGSTAT,PRGMDEBT                                                 
         BZ    INV0650                                                          
         B     POSTCRD                                                          
INV0610  MVC   GROUPNO,CSGIN       USE INVOICE NUMBER SAVED                     
         B     MAIN900                                                          
*                                                                               
INV0650  BAS   RE,CALLDETS         SWITCH TO DEBIT SCREEN                       
         MVI   TWASCRN,DTLSCRN                                                  
         GOTO1 =A(BLDDBTS),DMCB,(RC),RR=RELO1   BUILD DEBIT SCREEN              
         B     MAIN900                                                          
         EJECT                                                                  
*---------------------------------------------------------------------          
* POSTINGS TO CREDIT                                                            
*---------------------------------------------------------------------          
         USING DLDSEL,R5                                                        
POSTCRD  DS    0H                                                               
         LA    RE,IOAREA          CLEAR IOAREA                                  
         LA    RF,3000                                                          
         XCEF                                                                   
*                                                                               
         TM    PRGSTAT,PRGMDEBT    WAS THERE A MAIN DEBIT?                      
         BZ    INV0700                                                          
         GOTO1 =A(POSTDBT),DMCB,(RC),RR=RELO1                                   
         MVI   IVMVAMT,C'*'                                                     
         OI    IVMVAMTH+6,X'80'                                                 
         NI    IVMVAMTH+4,X'FF'-X'20'                                           
         B     MAIN900                                                          
*                                                                               
INV0700  LA    R5,IOAREA+2                                                      
         MVI   DLDSEL,DLDSELQ      X'64' POSTING DESCR ELEMENT                  
         MVC   DLDSREF,INVREF                                                   
         MVC   DLDSDATE,INVDTE                                                  
         MVI   DLDSSBRF,0                                                       
         MVI   DLDSSTAT,0                                                       
         CLI   IVMURGH+5,0         URGENT?                                      
         BZ    *+8                                                              
         OI    DLDSSTAT,X'40'      MARK URGENT                                  
*                                                                               
         MVI   DLDSLEN,DLDSLN1Q    NO NARRATIVE                                 
         SR    RE,RE                                                            
         ICM   RE,1,CRNARRLN       ANY NARRATIVE?                               
         BZ    INV0805                                                          
         LA    R1,DLDSLN1Q                                                      
         AR    R1,RE                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   DLDSNARR(0),CRNARR                                               
         STC   R1,DLDSLEN          NEW LENGTH OF ELEMENT W/ NARR                
INV0805  SR    RE,RE                                                            
         IC    RE,1(R5)                                                         
*                                                                               
         AR    R5,RE                                                            
         OC    INVORDNO,INVORDNO                                                
         BZ    *+8                                                              
         BAS   RE,PSTFFNEL         POST FFNELS                                  
*                                                                               
         USING GINELD,R5                                                        
INV0810  MVI   GINEL,GINELQ                                                     
         MVI   GINLN,GINLN2Q                                                    
         MVC   GININV,GROUPNO                                                   
         MVI   GINTYP,0                                                         
         XC    GINHISN,GINHISN                                                  
         SR    RE,RE                                                            
         IC    RE,GINLN                                                         
*                                                                               
         USING DLPOSTD,R5                                                       
         AR    R5,RE                                                            
         MVI   DLPSEL,DLPSECRQ     CREDIT VENDOR                                
         MVI   DLPSLEN,DLPSLNQ                                                  
         CLC   SVCLINUM,SPACES                                                  
         BNH   INV0820                                                          
         MVC   DLPSDBAC,SVCLINUM   CONTRA = CLIENT                              
         MVC   DLPSDBNM,SVCLINM                                                 
         B     *+16                                                             
*                                                                               
INV0820  MVC   DLPSDBAC,SVEXPNUM   CONTRA = EXPENSE                             
         MVC   DLPSDBNM,SVEXPNM                                                 
         MVC   DLPSCRAC,SVPVEN     VENDOR                                       
         MVC   DLPSCRNM,SVPVENNM                                                
         MVI   DLPSTYPE,0                                                       
         MVC   DLPSANAL,SVOFFICE                                                
         CLC   SVOFFICE,SPACES                                                  
         BH    *+10                                                             
         MVC   DLPSANAL,SVCLIOFF                                                
*        ZAP   DLPSAMNT,CRDTOT     AMOUNT TO CREDIT                             
         ZAP   DLPSAMNT,=P'0'                                                   
         IC    RE,1(R5)                                                         
         AR    R5,RE                                                            
*                                                                               
         MVI   0(R5),0             MARK EOR                                     
         LA    RF,IOAREA-1         GET LENGTH OF RECORD                         
         SR    R5,RF                                                            
         STH   R5,HALF                                                          
         MVC   IOAREA(2),HALF                                                   
         ZAP   TRANSAMT,CRDTOT                                                  
         ZAP   DUB,CRDTOT                                                       
*                                                                               
         XC    BOWORK1,BOWORK1                                                  
         MVC   BOWORK1(6),INVREF                                                
*                                                                               
         LA    RF,BOELEM                                                        
         USING GINELD,RF                                                        
         MVI   GINEL,GINELQ        GROUP INVOICE NUMBER ELEMENT                 
         MVI   GINLN,GINLN3Q                                                    
         MVC   GININV,GROUPNO                                                   
         LA    RE,1                                                             
         STCM  RE,3,GINHISN                                                     
         OI    CSLSTCUR+(LSTBIND2-LSTTABD),LSTBISIN   BATCH HAS S/INVS          
         MVC   BOWORK1+11(1),CSBTYP                                             
         ZAP   BOPL61,CRDTOT                                                    
         GOTO1 AADACDAY,BOPARM,(X'80',IOAREA),BOPL61,(0,BOWORK1),BOELEM         
         XC    GROUPNO,GROUPNO     INIT GROUP NO                                
         MVI   IVMVAMT,C'*'                                                     
         OI    IVMVAMTH+6,X'80'                                                 
         NI    IVMVAMTH+4,X'FF'-X'20'                                           
*                                                                               
         B     MAIN900                                                          
         DROP  R5                                                               
         EJECT                                                                  
*---------------------------------------------------------------------          
* Detail part of program                                                        
*---------------------------------------------------------------------          
*                                                                               
DET0010  CLI   PFKEY,PFK12         USER WANTS TO RETURN TO MAIN SCREEN          
         BNE   DET0020                                                          
         BAS   RE,CALLMAIN                                                      
         B     CURSIT                                                           
*                                                                               
DET0020  CLI   CSSPROG,2                                                        
         BE    ZOOM010                                                          
*                                                                               
         CLI   PFKEY,9             SHOULDN'T BE ANY                             
         BNE   DET0025                                                          
         BAS   RE,CALLZOOM         ZOOM IN ON DETAIL                            
         BE    MAIN900                                                          
         MVC   FVMSGNO,=AL2(AE$IVPFK)                                           
         B     ERRXIT                                                           
*                                                                               
DET0025  MVC   CURLNNO,=H'01'                                                   
         CLI   PFKEY,10            ADD ITEM                                     
         BE    *+12                                                             
         CLI   PFKEY,0             SHOULDN'T BE ANY                             
         BNE   BADKEY                                                           
*                                                                               
         MVI   PRGSTAT,0           RESET STATUS                                 
         USING CURLINED,R2                                                      
         LA    R2,IVDSUBH          FIRST DETAIL LINE                            
         LA    R3,IVDTABH          LAST INPUT                                   
         CLI   CSACT,ACTDSP        ACTION DISPLAY?                              
         BE    DET9000             SKIP VALIDATION                              
         DROP  R2                                                               
*                                                                               
         USING CURLINED,R4                                                      
         LA    R4,IVDSUBH          FIRST LINE                                   
DET0100  ST    R4,ACURLN           SAVE ADDR OF LINE                            
         LR    R1,R4                                                            
         LA    RF,1                                                             
         BAS   RE,CHKLINE          DID LINE CHANGE?                             
         BNZ   DET0110             YES, VALIDATE                                
         TM    CURSUBH+4,X'80'     NEW SUB-ACTION?                              
         BZ    DET3000                                                          
DET0110  CLC   CURSUB,=CL3'DEL'                                                 
         BNE   DET0120                                                          
         BAS   RE,DETDEL                                                        
         L     R2,ACURLN                                                        
         BNE   ERRXIT                                                           
         B     DET3000             NEXT                                         
*                                                                               
DET0120  MVI   PRGSTAT,0           RESET STATUS                                 
         BAS   RE,VALLINE                                                       
         L     R2,FVADDR                                                        
         BNE   ERROR                                                            
*                                                                               
         EJECT                                                                  
DET2000  OI    CUREXPH+4,X'20'     MARK AS VALIDATED                            
         MVC   FVMSGNO,=AL2(AE$MISIF)                                           
         OI    CURAMTH+6,X'80'                                                  
         LA    R2,CURAMTH          GET DEBIT AMOUNT                             
         CLI   CURAMTH+5,0                                                      
         BZ    ERROR               HAS TO BE AN AMOUNT                          
         BAS   RE,VALAMNT                                                       
         BNE   ERROR                                                            
         ZAP   NETPOST,DUB                                                      
         EDIT  NETPOST,(11,CURAMT),2,ZERO=NOBLANK                               
         AP    TOTDETS,NETPOST                                                  
         AP    CRDTOT,NETPOST                                                   
         OI    CURAMTH+4,X'20'     VALIDATED                                    
DET2100  MVC   CURSUB,=C'*  '                                                   
         OI    CURSUBH+6,X'80'     TRANSMIT                                     
         GOTO1 =A(POSTDBT),DMCB,(RC),RR=RELO1     POST DEBIT ITEM               
*                                                                               
         OC    INVORDNO,INVORDNO   DON'T UPDATE ORDER IF NONE                   
         BZ    DET3000                                                          
         BAS   RE,UPDOAM           UPDATE OAMEL FOR TODAY                       
         BNE   DET3000             ERROR, DON'T WRITE                           
         GOTO1 AWRITE,AIO1                                                      
         BNE   ERRXIT                                                           
*                                                                               
DET3000  SR    RE,RE                                                            
         ICM   RE,3,CURLNNO                                                     
         LA    RE,1(RE)                                                         
         STCM  RE,3,CURLNNO                                                     
*                                                                               
         LA    R4,CURLLNQ(R4)      BUMP TO NEXT LINE                            
         LA    RF,IVDLLNH                                                       
         CR    R4,RF               ARE WE FINISHED?                             
         BNL   DET8999                                                          
         MVI   STFSW,C' '          RESET SWITCHES                               
         MVI   DEPSW,C' '                                                       
         MVI   ECONSULT,C' '                                                    
         MVI   COSTANAL,C' '                                                    
         B     DET0100             NO, KEEP AT IT                               
*                                                                               
DET8999  ZAP   DUB,VENDAMT         SHOW CREDIT-DEBITS                           
         SP    DUB,TOTDETS                                                      
         ZAP   BCWORK(8),DUB                                                    
         EDIT  (P8,BCWORK),(11,IVDDIFF),2,ZERO=NOBLANK,FLOAT=-                  
         OI    IVDDIFFH+6,X'80'                                                 
*                                                                               
         CLI   PFKEY,PFK10         ADD THE ITEM?                                
         BE    FINISH                                                           
DET9000  MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(AI$AOKNX)                                           
         B     EXIT                                                             
         EJECT                                                                  
*--------------------------------------------------------------------           
* ADD CREDIT PART TO FILE                                                       
*--------------------------------------------------------------------           
FINISH   BAS   RE,CALLMAIN                                                      
         LA    R2,IVCORDH                                                       
         B     POSTCRD                                                          
         EJECT                                                                  
*--------------------------------------------------------------------           
* ZOOM SCREEN FOR DEBIT DETAILS                                                 
*--------------------------------------------------------------------           
ZOOM010  DS    0H                                                               
         LA    R2,IVZWCH                                                        
         ST    R2,SAVEWC                                                        
         GOTO1 AGETWC,IVZWC        GET AND VALIDATE WORKCODE                    
         BE    *+8                                                              
         B     ERROR                                                            
         OI    IVZWCH+6,X'80'                                                   
*                                                                               
         CLI   IVZWCH+5,0                                                       
         BZ    ZOOM015                                                          
         LA    R2,IVZCPJH                                                       
         ST    R2,SAVEEXP                                                       
         OI    IVZCPJH+6,X'80'                                                  
         BAS   RE,ADVEXP                                                        
         BNE   ERROR                                                            
*                                                                               
ZOOM015  LA    R2,IVZEXPH          VALIDATE EXPENSE ACCOUNT                     
         ST    R2,SAVEEXP                                                       
         BAS   RE,AGYEXP                                                        
         BZ    *+8                                                              
         B     ERROR                                                            
*                                                                               
         LA    R2,IVZODSH                                                       
         MVC   OFFICE,SPACES       INITIALIZE                                   
         MVC   DEPT,SPACES                                                      
         MVC   STAFF,SPACES                                                     
         XC    OFFICEL,OFFICEL                                                  
         XC    DEPTL,DEPTL                                                      
         XC    STAFFL,STAFFL                                                    
*                                                                               
         ST    R2,SAVEODS          SAVE ADDRESS OF OFF/DEPT/STAFF               
         XC    BLOCK(96),BLOCK                                                  
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         GOTO1 SCANNER,DMCB,(R2),(6,BLOCK),C',=,,'                              
         CLI   DMCB+4,4                                                         
         BH    ERROR               MORE THAN 4 (OFF,DEPT,STAFF,ANOF)            
*                                                                               
         MVC   OFFICEL,BLOCK       SAVE OFFICE AND L'OFFICE ENTERED             
         MVC   OFFICE(L'OFFICE),BLOCK+12                                        
         OC    OFFICE,SPACES                                                    
         MVC   CURROFFC,OFFICE                                                  
*                                                                               
         MVC   DEPTL,BLOCK+32      SAVE DEPT AND L'DEPT ENTERED                 
         MVC   DEPT(L'DEPT),BLOCK+44                                            
         OC    DEPT,SPACES                                                      
*                                                                               
         MVC   STAFFL,BLOCK+64     SAVE STAFF AND L'STAFF ENTERED               
         MVC   STAFF(L'STAFF),BLOCK+76                                          
         OC    STAFF,SPACES                                                     
*                                                                               
         GOTO1 =A(ERRCHCK),DMCB,(RC),RR=RELO1    CHECK OFF/DPT/STAFF            
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   ERROR                                                            
*                                                                               
         GOTO1 AVALOFFC,DMCB,(X'80',OFFICE)                                     
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BE    ZOOM030                                                          
         MVC   FVMSGNO,=Y(AE$IVOFF)                                             
         B     ERROR                                                            
*                                                                               
ZOOM030  LA    R2,IVZAOFH          VALIDATE ANALYSIS OFFICE                     
         MVC   OFFICEL,IVZAOFH+5                                                
         MVC   OFFICE(L'OFFICE),IVZAOF                                          
         OC    OFFICE,SPACES                                                    
*                                                                               
         GOTO1 AVALOFFC,DMCB,(X'80',OFFICE)                                     
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$IVOFF)                                           
         B     ERROR                                                            
*                                                                               
         MVC   FVMSGNO,=AL2(AE$MISIF)                                           
         OI    IVZAMTH+6,X'80'                                                  
         LA    R2,IVZAMTH                                                       
         CLI   IVZAMTH+5,0                                                      
         BZ    ERROR               HAS TO BE AN AMOUNT                          
         BAS   RE,VALAMNT                                                       
         BNE   ERROR                                                            
         ZAP   NETPOST,DUB                                                      
         EDIT  NETPOST,(11,IVZAMT),2,ALIGN=LEFT,ZERO=NOBLANK                    
*                                                                               
         B     MAIN900                                                          
         EJECT                                                                  
MAIN900  MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         EJECT                                                                  
BADKEY   MVC   FVMSGNO,=AL2(AE$IVPFK)                                           
         LA    R2,CONTABH                                                       
         B     ERRXIT                                                           
*                                                                               
ADVCLIST DS    0H    PRD. U/L'S VALID TO CREDIT                                 
         DC    C'SXSWSYSBSA'       SPACE OK-CASH NOT ALLOWED                    
         DC    X'FF'                                                            
*                                                                               
AGYCLIST DS    0H    EXP. U/L'S VALID TO CREDIT                                 
         DC    C'SVSWSYSBSASFSL'   SPACE OK-CASH NOT ALLOWED                    
         DC    X'FF'                                                            
         EJECT                                                                  
*--------------------------------------------------------------------           
* PUT ORDER NUMBERS IN POSTINGS                                                 
*--------------------------------------------------------------------           
         USING FFNELD,R5           ORDER NUMBER ELEMENT                         
PSTFFNEL MVI   FFNEL,FFNELQ                                                     
         MVI   FFNLN,FFNLN2Q                                                    
         MVC   FFNONUM,INVORDNO                                                 
*        TM    PRGOSTAT,PRGOPART   PARTIAL ORDER                                
*        BZ    *+8                                                              
*        MVI   FFNSTAT,FFNSPRTQ                                                 
         SR    R0,R0                                                            
         IC    R0,1(R5)            BUMP TO NEXT POSITION                        
         AR    R5,R0                                                            
         BR    RE                                                               
         DROP  R5                                                               
         EJECT                                                                  
*--------------------------------------------------------------------           
* VALIDATE A DETAIL LINE                                                        
*--------------------------------------------------------------------           
VALLINE  NTR1                                                                   
         MVI   PRGSTAT,0           RESET STATUS                                 
         CLI   CURWCH+5,0          ANY WORKCODE?                                
         BNE   VLN0150                                                          
         CLI   CURCPJH+5,0         ANY EXPENSE ACCT?                            
         BNE   VLN0200                                                          
         LA    R2,CURWCH           HAS TO BE ONE OF THESE 2                     
         MVC   FVMSGNO,=AL2(AE$MISIF)                                           
         B     VLNERR                                                           
*                                                                               
VLN0150  DS    0H                                                               
         OI    PRGSTAT,PRGPROD     WORKCODE IMPLIES PRODUCTION                  
         GOTO1 AGETWC,CURWC        GET AND VALIDATE WORKCODE                    
         BE    *+12                                                             
         LA    R2,CURWCH                                                        
         B     VLNERR                                                           
         NI    PRGSTAT,X'FF'-PRGNCOMM                                           
         CLI   CURWCH+5,2                                                       
         BNH   VLN0155                                                          
         CLC   CURWC+2(2),=C',N'   NON COMMISSIONABLE?                          
         BNE   VLNERR                                                           
         OI    PRGSTAT,PRGNCOMM                                                 
VLN0155  OI    CURWCH+6,X'80'                                                   
         OI    CURWCH+4,X'20'      MARK AS VALIDATED                            
*                                                                               
         CLI   CURCPJH+5,0         HAS TO BE CLI/PRD/JOB                        
         BNZ   VLN0200                                                          
         LA    R2,CURCPJH                                                       
         MVC   FVMSGNO,=AL2(AE$MISIF)                                           
         B     VLNERR                                                           
*                                  VALIDATE CLI/PRD/JOB OR EXPENSE ACCT         
VLN0200  OI    CURCPJH+4,X'20'     MARK AS VALIDATED                            
         LA    R2,CURWCH                                                        
         ST    R2,SAVEWC                                                        
         LA    R2,CURAMTH                                                       
         ST    R2,SAVEAMNT                                                      
         LA    R2,CURCPJH                                                       
         ST    R2,SAVEEXP                                                       
         OI    CURCPJH+6,X'80'                                                  
         TM    PRGSTAT,PRGPROD     PRODUCTION?                                  
         BZ    VLN0250                                                          
         BAS   RE,ADVEXP                                                        
         BNE   VLNERR                                                           
         CLI   CUREXPH+5,0         USER'S INPUT TAKES PRIORITY                  
         BNE   VLN0300                                                          
         TM    PRGSTAT,PRGXJOB     IS IT AN XJOB?                               
         BZ    VLN0300                                                          
         L     RF,AGOXBLK          EXTENDED BLOCK                               
         USING GOXBLOCK,RF                                                      
*                                                                               
         MVC   FINOFF,GOAWOFOF     FINANCIAL OFFICE                             
         OC    GOAWOA,GOAWOA       IS THERE AN EXPENSE ACCT IN OPT MNT          
         BZ    VLN0240                                                          
         MVC   FLD,SPACES                                                       
         CLC   GOAWOA(2),=C'SE'    IS THIS AN 'SE' ACCOUNT?                     
         BE    *+18                                                             
         MVI   FLD,C'*'                                                         
         MVC   FLD+1(L'GOAWOA),GOAWOA                                           
         B     *+10                                                             
         MVC   FLD(L'GOAWOA-2),GOAWOA+2                                         
         LA    R2,CUREXPH                                                       
         BAS   RE,MOVEFLD                                                       
         STC   RF,5(R2)                                                         
         OI    CUREXPH+6,X'80'                                                  
         OI    CUREXPH+4,X'20'     MARK AS VALIDATED                            
         ST    R2,SAVEEXP                                                       
         BAS   RE,AGYEXP                                                        
         L     RF,AGOXBLK          EXTENDED BLOCK                               
*                                                                               
VLN0240  OC    GOAWOAOF,GOAWOAOF   OFFICE?                                      
         BZ    VLN0300                                                          
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'GOAWOAOF),GOAWOAOF                                         
         LA    R1,FLD+1                                                         
         CLI   0(R1),C' '                                                       
         BNH   *+8                                                              
         LA    R1,1(R1)                                                         
         MVI   0(R1),C','          ADD IN DELIMETER                             
         LA    R1,1(R1)                                                         
         MVC   0(L'GOAWODEP,R1),GOAWODEP                                        
         LA    R1,L'GOAWODEP(R1)                                                
         MVI   0(R1),C','                                                       
         LA    R1,1(R1)                                                         
         MVC   0(L'GOAWOSTF,R1),GOAWOSTF                                        
         LA    R2,CURODSH                                                       
         BAS   RE,MOVEFLD                                                       
         STC   RF,5(R2)                                                         
         OI    CURODSH+6,X'80'                                                  
         B     VLN0300                                                          
         DROP  RF                                                               
*                                                                               
VLN0250  DS    0H                  VALIDATE EXPENSE ACCOUNT                     
         LA    R2,CURCPJH                                                       
         CLC   SVEVEN,SPACES       HAVE TO BE AN EXPENSE VENDOR                 
         BH    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$VMEXP)                                           
         B     VLNERR                                                           
         BAS   RE,AGYEXP                                                        
*                                                                               
VLN0300  OI    CURCPJH+4,X'20'     MARK AS VALIDATED                            
         OI    CURODSH+6,X'80'                                                  
         CLI   CURODSH+5,0         ANY INPUT?                                   
         BH    VLN0310                                                          
         TM    PRGSTAT,PRGXJOB     EXPENSE JOB?                                 
         BO    VLN0310             YES, NEED OFFICE                             
         TM    PRGSTAT,PRGPROD     PRODUCTION?                                  
         BO    VLN0400             YES, DON'T NEED IT                           
VLN0310  LA    R2,CURODSH                                                       
         MVC   OFFICE,SPACES       INITIALIZE                                   
         MVC   DEPT,SPACES                                                      
         MVC   STAFF,SPACES                                                     
         XC    OFFICEL,OFFICEL                                                  
         XC    DEPTL,DEPTL                                                      
         XC    STAFFL,STAFFL                                                    
*                                                                               
         ST    R2,SAVEODS          SAVE ADDRESS OF OFF/DEPT/STAFF               
         XC    BLOCK(96),BLOCK                                                  
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         GOTO1 SCANNER,DMCB,(R2),(6,BLOCK),C',=,,'                              
         CLI   DMCB+4,4                                                         
         BH    VLNERR              MORE THAN 4 (OFF,DEPT,STAFF,ANOF)            
*                                                                               
         MVC   OFFICEL,BLOCK       SAVE OFFICE AND L'OFFICE ENTERED             
         MVC   OFFICE(L'OFFICE),BLOCK+12                                        
         OC    OFFICE,SPACES                                                    
         MVC   CURROFFC,OFFICE                                                  
*                                                                               
VLN0350  MVC   DEPTL,BLOCK+32      SAVE DEPT AND L'DEPT ENTERED                 
         MVC   DEPT(L'DEPT),BLOCK+44                                            
         OC    DEPT,SPACES                                                      
*                                                                               
         MVC   STAFFL,BLOCK+64     SAVE STAFF AND L'STAFF ENTERED               
         MVC   STAFF(L'STAFF),BLOCK+76                                          
         OC    STAFF,SPACES                                                     
*                                                                               
         GOTO1 =A(ERRCHCK),DMCB,(RC),RR=RELO1    CHECK OFF/DPT/STAFF            
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   VLNERR                                                           
*                                                                               
         GOTO1 AVALOFFC,DMCB,(X'80',OFFICE)                                     
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BE    VLN0400                                                          
         MVC   FVMSGNO,=Y(AE$IVOFF)                                             
         B     VLNERR                                                           
         EJECT                                                                  
*--------------------------------------------------------------                 
*        SCAN CLI/PRO FIELD                                                     
*--------------------------------------------------------------                 
*                                                                               
VLN0400  OI    CURODSH+4,X'20'     MARK AS VALIDATED                            
         XC    COSTNUM,COSTNUM                                                  
         XC    CLIPRON,CLIPRON                                                  
         XC    DEPSTFN,DEPSTFN                                                  
*                                                                               
VLN0410  LA    R2,CUREXPH                                                       
         OI    CUREXPH+6,X'80'                                                  
         TM    PRGSTAT,PRGXJOB     XJOB?                                        
         BZ    VLN0420                                                          
         CLI   5(R2),0                                                          
         BE    VLN0450                                                          
         ST    R2,SAVEEXP                                                       
         BAS   RE,AGYEXP                                                        
*                                                                               
         LA    RF,PROFILE          FIND 1/C ACCOUNT                             
         USING ACPROFD,RF                                                       
         MVC   COSTNUM,ACPRCOST                                                 
         MVC   KEY,SPACES                                                       
         MVC   KEY(15),ACPRCOST                                                 
         BAS   RE,GETACC                                                        
         MVC   FVMSGNO,=AL2(AE$INACP)                                           
         TM    ACCTSTAT,X'80'      BALANCE ELEMENT                              
         BZ    BADACC                                                           
         TM    ACCTSTAT,X'10'      LOCKED                                       
         BO    BADACC                                                           
         MVC   COSTNAME,ACCTNAME                                                
         DROP  RF                                                               
*                                                                               
         B     VLN0450                                                          
*                                                                               
VLN0420  ST    R2,SAVCLPR          SAVE ADDRESS OF CLI/PRO                      
         XC    SCAN(64),SCAN                                                    
         CLI   5(R2),0             IF NOTHING THERE,DON'T BOTHER                
         BE    VLN0450             WITH SCANNER                                 
         CLI   STFSW,C'Y'                                                       
         BE    VLN0430                                                          
         LA    RF,BCCPYEL                                                       
         TM    BCCPYST5-BCCPYEL(RF),CPYSNCST TEST NEW COSTING                   
         BO    VLN0430                                                          
         CLI   COSTANAL,C' '       IF NO STAFF OR COSTING,                      
         BNE   VLN0430                                                          
         MVC   FVMSGNO,=Y(AE$ANFAN) INPUT TO CLIENT NOT ALLOWED                 
         B     ERRXIT                                                           
*                                                                               
VLN0430  MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         GOTO1 SCANNER,DMCB,(R2),(4,SCAN),C',=,,'                               
         CLI   DMCB+4,2                                                         
         BH    VLNERR              MORE THAN CLI,PROD                           
         LA    R6,CLIPROF                                                       
         LA    RF,COMPEL                                                        
         USING ACCOMPD,RF                                                       
         MVC   KEY+1(48),SPACES                                                 
         MVC   KEY+1(2),ACMPJOB    PRODUCTION LEDGER                            
         DROP  RF                                                               
         ZIC   R3,SCAN             LENGTH OF CLI INPUT                          
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),SCAN+12    CLIENT TO KEY                                
         BAS   RE,GETACC                                                        
         MVC   CLIPRON,ACCTNAME                                                 
         MVC   CLIPRO,ACCTNUM                                                   
*                                                                               
         XC    PRODPROF,PRODPROF                                                
         XC    JOBPROF,JOBPROF                                                  
         CLI   SCAN+32,0           IS PRODUCT INPUT                             
         BE    VLN0440                                                          
         LA    R4,KEY+3                                                         
         IC    R3,CLILNGTH         LEVEL A LENGTH                               
         AR    R4,R3                                                            
         ZIC   R3,SCAN+32          PRODUCT LENGTH                               
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),SCAN+44     PRODUCT                                      
         LA    R6,PRODPROF                                                      
         BAS   RE,GETACC                                                        
         MVC   CLIPRON,ACCTNAME                                                 
         MVC   CLIPRO,ACCTNUM                                                   
*                                                                               
VLN0440  BAS   RE,PROFMERG                                                      
         SR    R6,R6               NO MORE PROFILES                             
         LA    RF,PROFILE          FIND 1/C ACCOUNT                             
         USING ACPROFD,RF                                                       
         MVC   COSTNUM,ACPRCOST                                                 
         MVC   KEY,SPACES                                                       
         MVC   KEY(15),ACPRCOST                                                 
         BAS   RE,GETACC                                                        
         MVC   FVMSGNO,=AL2(AE$INACP)                                           
         TM    ACCTSTAT,X'80'      BALANCE ELEMENT                              
         BZ    BADACC                                                           
         TM    ACCTSTAT,X'10'      LOCKED                                       
         BO    BADACC                                                           
         MVC   COSTNAME,ACCTNAME                                                
         DROP  RF                                                               
*                                                                               
VLN0450  TM    PRGSTAT,PRGXJOB     EXPENSE JOB?                                 
         BO    VLN0455             HAVE TO GET EXPENSE STUFF                    
         TM    PRGSTAT,PRGPROD     PRODUCTION                                   
         BO    VLN2000             CODE BELOW IS FOR EXPENSE                    
VLN0455  LA    RF,BCCPYEL                                                       
         TM    BCCPYST5-BCCPYEL(RF),CPYSNCST TEST NEW COSTING                   
         BO    VLN0500                                                          
         CLI   COSTANAL,C' '       IS ANALYSIS REQUIRED (ANALYSIS=)             
         BE    VLN0500             NO                                           
         CLI   SCAN,0              IF ANALYSIS REQUIRED,                        
         BNE   VLN0500             CLIENT IS REQUIRED                           
         CLI   STFSW,C'Y'                                                       
         BNE   VLN0500                                                          
         MVC   FVMSGNO,=Y(AE$ICLPQ)                                             
         B     ERRXIT                                                           
         EJECT                                                                  
*--------------------------------------------------------------                 
*        BUILD 1P ACCOUNT                                                       
*--------------------------------------------------------------                 
*                                                                               
VLN0500  DS    0H                                                               
         XC    CR13NUM,CR13NUM                                                  
         L     R2,SAVEODS          CURSOR TO OFFICE/DEP/STF FIELD               
*                                                                               
         TM    COMPSTAT,X'10'                                                   
         BNO   *+8                                                              
         OI    COSTSW,COSTACC                                                   
         LA    RF,BCCPYEL                                                       
         TM    BCCPYST5-BCCPYEL(RF),CPYSNCST TEST NEW COSTING                   
         BNO   VLN0520                                                          
         MVI   COSTSW,0                                                         
         LA    R1,CATBLK                                                        
         USING CATD,R1                                                          
         MVC   CATDMGR,DATAMGR     BUILD CONTROL BLOCK                          
         MVC   CATSEAC,POSTACC     DEBIT ACCOUNT                                
         MVC   CATOFF,OFFICE       OFFICE                                       
         MVC   CATDPT,DEPT         DEPARTMENT                                   
         GOTO1 VCATCALL,CATD                                                    
         L     R2,SAVEEXP          POINT TO CURRENT EXPENSE ACCOUNT             
         MVC   KEY,SPACES                                                       
         MVC   KEY(15),CATACC3                                                  
         CLI   CATERR,0                                                         
         BE    VLN0510                                                          
         MVC   FVMSGNO,=AL2(AE$IANAL)                                           
         B     VLNERR                                                           
*                                                                               
VLN0510  CLI   CATPST,C'N'         NO COST POSTING                              
         BE    VLN0520                                                          
         OI    COSTSW,COSTACC+COSTNEW                                           
         MVC   COSTANAL,CATCDE                                                  
         MVC   CR13NUM,CATACC3     SAVE 13 ACCOUNT                              
*                                                                               
VLN0520  CLI   COSTANAL,X'40'                                                   
         BE    *+12                IF NO COSTING REQUIRED                       
         TM    COSTSW,COSTACC                                                   
         BO    VLN0550                                                          
         CLI   STFSW,C'Y'          OR STAFF IS NOT REQUIRED                     
         BE    VLN0600                                                          
         L     RE,SAVCLPR                                                       
         CLI   5(RE),0             THEN CLIENT INPUT NOT ALLOWED                
         BE    VLN0530                                                          
         L     R2,SAVCLPR                                                       
         B     VLN0540                                                          
*                                                                               
VLN0530  L     R2,SAVCLPR          AND PRODUCT INPUT NOT ALLOWED                
         CLI   5(R2),0                                                          
         BE    VLN0600                                                          
*                                                                               
VLN0540  MVC   FVMSGNO,=Y(AE$ANFAN)  ACCT NOT FLAGGED FOR ANALYSIS              
         B     ERRXIT                                                           
*                                                                               
VLN0550  OI    COSTSW,COSTACC      COST ACCOUNTING                              
         L     R2,SAVCLPR                                                       
         CLI   FVILEN-FVIHDR(R2),0                                              
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$MISIF)                                           
         B     ERRXIT                                                           
*                                                                               
* SAME STUPID TEST REGARDLESS                                                   
*        L     R2,SAVCLPR                                                       
*        LA    RF,BCCPYEL          TEST PRODUCT REQUIRED FOR EXPENSE            
*        TM    BCCPYST5-BCCPYEL(RF),CPYSEXPP EXPENSE PRODUCT                    
*        BNO   *+8                                                              
*        BAS   RE,ANY                                                           
         L     R2,SAVEODS                                                       
         MVC   KEY+1(14),SPACES                                                 
         MVC   KEY+1(2),=C'1P'    1P/OF/DPT/CATEGORY                            
         LA    RF,BCCPYEL                                                       
         TM    BCCPYST5-BCCPYEL(RF),CPYSNCST TEST NEW COSTING                   
         BNO   VLN0560                                                          
         MVC   KEY+3(12),=C'999999999999' SAYS VANESSA                          
         B     VLN0580                                                          
*                                                                               
*                                                                               
VLN0560  MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),=C'1P'     SET UP 1/P ACCOUNT                           
         LA    RF,KEY+3                                                         
*                                                                               
         CLI   OFFSW,C'N'                                                       
         BE    VLN0570                                                          
         ZIC   R1,OFCLNGTH                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),OFFICE      OFFICE                                       
         LA    RF,1(R1,RF)                                                      
*                                                                               
VLN0570  DS    0H                                                               
         LA    R3,=C'9999'         DEFAULT DEPT                                 
         CLI   DEPSW,C'Y'                                                       
         BNE   *+8                                                              
         LA    R3,DEPT             OR REAL DEPT                                 
*                                                                               
         ZIC   R1,DPTLNGTH                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),0(R3)                                                    
         LA    RF,1(R1,RF)                                                      
         MVC   0(1,RF),COSTANAL                                                 
*                                                                               
VLN0580  BAS   RE,GETACC                                                        
         TM    ACCTSTAT,X'80'      BALANCE ELEMENT                              
         BZ    BADACC                                                           
         MVC   CRCNUM,ACCTNUM                                                   
         MVC   CRCNAME,ACCTNAME                                                 
         B     VLN0600                                                          
         EJECT                                                                  
*--------------------------------------------------------------                 
*        BUILD 13 ACCOUNT                                                       
*--------------------------------------------------------------                 
*                                                                               
VLN0600  DS    0H                                                               
         OC    CR13NUM,CR13NUM                                                  
         BNZ   VLN0700                                                          
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),=C'13'                                                  
         MVC   KEY+3(1),COSTANAL                                                
         BAS   RE,GETACC                                                        
         MVC   CR13NUM,ACCTNUM                                                  
         MVC   CR13NAME,ACCTNAME                                                
         B     VLN0700                                                          
         EJECT                                                                  
*--------------------------------------------------------------                 
*        BUILD 2D ACCOUNT                                                       
*--------------------------------------------------------------                 
*                                                                               
VLN0700  DS    0H                                                               
         CLI   DEPSW,C'N'          NO 2D POSTING IF SWITCH NOT=Y                
         BE    VLN0800                                                          
*                                                                               
         L     R2,SAVEODS          POINT TO OFF/DEPT/STAFF                      
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),=C'2D'                                                  
         LA    R1,KEY+3                                                         
*                                                                               
         CLI   OFFSW,C'Y'          OFFICE IN ACCT                               
         BNE   VLN0750                                                          
         ZIC   RF,OFCLNGTH                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),OFFICE      OFFICE                                       
         LA    R1,1(RF,R1)                                                      
*                                                                               
VLN0750  DS    0H                                                               
         ZIC   RF,DPTLNGTH                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),DEPT        DEPARTMENT                                   
*                                                                               
         SR    R6,R6                                                            
         BAS   RE,GETACC                                                        
         BAS   RE,CHECKACC                                                      
         MVC   DEPNAME,ACCTNAME                                                 
         MVC   DEPNUM,ACCTNUM                                                   
         MVC   DEPSTFN,ACCTNAME                                                 
         B     VLN0800                                                          
         EJECT                                                                  
*--------------------------------------------------------------                 
*        BUILD 2P ACCOUNT                                                       
*--------------------------------------------------------------                 
*                                                                               
VLN0800  DS    0H                                                               
         CLI   STFSW,C'Y'                                                       
         BNE   VLN2000                                                          
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),=C'2P'                                                  
         LA    R1,KEY+3                                                         
*                                                                               
         CLI   LEVEL,3             ONLY MOVE OFFICE FOR 3 LEVEL ACCTS           
         BL    VLN0810                                                          
         ZIC   RF,OFCLNGTH                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),OFFICE      MOVE OFFICE INTO KEY                         
         LA    R1,1(RF,R1)         BUMP TO NEXT SPOT IN KEY                     
*                                                                               
VLN0810  DS    0H                                                               
         CLI   LEVEL,2                                                          
         BL    VLN0820                                                          
         ZIC   RF,DPTLNGTH                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),DEPT                                                     
         LA    R1,1(RF,R1)                                                      
*                                                                               
VLN0820  DS    0H                                                               
         CLI   STAFFL,0                                                         
         BE    VLN0830                                                          
         ZIC   RF,STAFFL                                                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),STAFF       MOVE STAFF INTO KEY                          
*                                                                               
VLN0830  SR    R6,R6                                                            
         BAS   RE,GETACC                                                        
         BAS   RE,CHECKACC                                                      
         MVC   STAFFNUM,ACCTNUM                                                 
         MVC   STAFFNAM,ACCTNAME                                                
         MVC   DEPSTFN,ACCTNAME                                                 
         B     VLN0900                                                          
         EJECT                                                                  
*--------------------------------------------------------------                 
*        BUILD 29 ACCOUNT                                                       
*--------------------------------------------------------------                 
*                                                                               
VLN0900  DS    0H                                                               
         ZIC   RF,0(R2)            POINT TO CLI/PROD                            
         AR    R2,RF                                                            
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),=C'29'     SET UP 2/9 ACCOUNT                           
         MVC   KEY+3(3),=C'999'                                                 
*                                                                               
         LA    RF,BCCPYEL                                                       
         TM    BCCPYST5-BCCPYEL(RF),CPYSNCST TEST NEW COSTING                   
         BNO   VLN0930                                                          
         MVC   KEY+3(12),=C'999999999999'                                       
         B     VLN0950                                                          
*                                                                               
VLN0930  CLI   OFFSW,C'Y'                                                       
         BNE   VLN0950                                                          
         MVC   KEY+3(46),SPACES                                                 
         ZIC   R1,OFCLNGTH                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),OFFICE                                                  
         LA    RF,KEY+3                                                         
         LA    RF,1(R1,RF)                                                      
         MVC   0(4,RF),=C'9999'    MOVE IN 9999 AS DEFAULT                      
*                                                                               
VLN0950  OC    COSTNUM,COSTNUM                                                  
         BZ    *+10                                                             
         MVC   KEY+3(12),COSTNUM+3 OR USE COSTING                               
         BAS   RE,GETACC                                                        
         BAS   RE,CHECKACC                                                      
         MVC   FVMSGNO,=AL2(AE$INACP)                                           
         TM    ACCTSTAT,X'80'      BALANCE ELEMENT                              
         BZ    BADACC                                                           
         TM    ACCTSTAT,ACSLOCK    LOCKED?                                      
         BO    BADACC                                                           
         MVC   CRPSNUM,ACCTNUM                                                  
         MVC   CRPSNAME,ACCTNAME                                                
*                                                                               
VLN2000  OI    CUREXPH+4,X'20'     MARK AS VALIDATED                            
         MVC   FVMSGNO,=AL2(AE$MISIF)                                           
         OI    CURAMTH+6,X'80'                                                  
         SR    RE,RE                                                            
*                                                                               
VLNERR   LTR   RE,RE                                                            
         B     EXIT                                                             
*--------------------------------------------------------------------           
* VALIDATE DEBIT INFO FROM MAIN SCREEN                                          
*--------------------------------------------------------------------           
         USING WCTABD,R4                                                        
         USING ORDRECD,R5                                                       
VALDEBT  NTR1                                                                   
         NI    PRGSTAT,X'FF'-PRGPROD                                            
         LA    R4,WCTABLE                                                       
         CLI   IVMWCH+5,0                                                       
         BZ    VDB100                                                           
         LA    R2,IVMWCH           WORKCODE                                     
         GOTO1 AGETWC,IVMWC                                                     
         BNE   VDBNE                                                            
         OI    PRGSTAT,PRGPROD     ASSUME PRODUCTION                            
         NI    PRGSTAT,X'FF'-PRGNCOMM                                           
         MVC   WCTWC,IVMWC                                                      
         CLI   IVMWCH+5,2                                                       
         BNH   VDB010                                                           
         CLC   IVMWC+2(2),=C',N'                                                
         BNE   VDBNE                                                            
         OI    PRGSTAT,PRGNCOMM                                                 
         OI    IVMWCH+6,X'80'                                                   
*                                                                               
VDB010   CLI   IVMCPJH+5,0         MUST HAVE CLI/PRO/JOB                        
         BNE   VDB020                                                           
         LA    R2,IVMCPJH                                                       
         MVC   FVMSGNO,=AL2(AE$MISIF)                                           
         B     VDBNE                                                            
*                                                                               
VDB020   DS    0H                                                               
*                                                                               
VDB100   CLC   IVMWC,SPACES        IS THERE A WORKCODE?                         
         BNH   VDB300              NO, MUST BE EXPENSE                          
         LA    R2,IVMCPJH                                                       
         GOTO1 ANY                                                              
*                                                                               
VDB300   DS    0H                                                               
*                                                                               
VDB900   ZAP   NETPOST,VENDAMT                                                  
         SR    RE,RE                                                            
VDBNE    LTR   RE,RE                                                            
         XIT1  REGS=(R2)                                                        
*                                                                               
         DROP  R4,R5                                                            
         EJECT                                                                  
*--------------------------------------------------------------------           
* UPDATE ORDER AMOUNT ELEMENTS                                                  
*--------------------------------------------------------------------           
         USING ORDRECD,R5                                                       
UPDOAM   NTR1                                                                   
         XC    KEY,KEY             FIND ORDER RECORD                            
         LA    R5,KEY                                                           
         MVI   ORDKTYP,ORDKTYPQ    X'1A'                                        
         MVC   ORDKCPY,COMPANY                                                  
         MVC   ORDKORD,INVORDNO    MOVE IN ORDER NUMBER                         
         GOTO1 READ                                                             
*                                                                               
         USING OAMEL,R3                                                         
         L     R3,AIO1                                                          
         MVI   ELCODE,OAMELQ                                                    
         BAS   RE,GETEL                                                         
         B     *+8                                                              
UPDOAM10 BAS   RE,NEXTEL                                                        
         BNE   UPDOAMNO            NO NEED TO UPDATE                            
*                                                                               
         CLC   OAMWORK,SVWRKCD     WORKCODE HAS TO MATCH                        
         BNE   UPDOAM10                                                         
*        CLC   OAMLAST,TODAYP                                                   
*        BE    *+10                                                             
*        ZAP   OAMTVAL,=P'0'                                                    
         AP    OAMTVAL,NETPOST                                                  
         MVC   OAMLAST,TODAYP                                                   
*                                                                               
         SR    RE,RE                                                            
UPDOAMNO LTR   RE,RE                                                            
         B     EXIT                                                             
         DROP  R3,R5                                                            
         EJECT                                                                  
*--------------------------------------------------------------------           
* DELETE DETAIL                                                                 
*--------------------------------------------------------------------           
         USING GINPASD,R4                                                       
         USING TRNELD,R3                                                        
         USING CURLINED,R2                                                      
DETDEL   NTR1                                                                   
*                                                                               
         LR    R2,R4                                                            
         LA    R4,IOKEY            FIND AMOUNT IN LINE                          
         XC    GINPKEY,GINPKEY     THEN SUBTRACT FROM TOTAL                     
         MVI   GINPTYP,GINPTYPQ                                                 
         MVC   GINPCPY,COMPANY                                                  
         MVC   GINPINV,GROUPNO                                                  
         MVC   GINPISN,CURLNNO                                                  
         XC    IODAOVER,IODAOVER                                                
         LA    R1,IOACCDIR+IOHID+IO1                                            
         GOTO1 AIO                                                              
         BNL   *+6                                                              
         DC    H'0'                HARDWARE ERROR                               
         TM    IOERR,IOEEOF                                                     
         BZ    *+6                                                              
         DC    H'0'                EOF                                          
         MVC   IODAOVER,IODA                                                    
         CLC   IOKEYSAV(GINPISN-GINPASD),IOKEY                                  
         BE    DETDEL20                                                         
         MVC   FVMSGNO,=AL2(AE$RCNOF)    RECORD NOT FOUND                       
         B     DETDELX             NOT GOOD                                     
DETDEL20 TM    GINPKSTA,X'80'            RECORD DELETED?                        
         BZ    DETDEL50                                                         
         MVC   FVMSGNO,=AL2(AE$RECID)                                           
         B     DETDELX                                                          
*                                                                               
DETDEL50 GOTO1 AIO,IOACCMST+IOGET+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,AIO1                                                          
         AH    R3,=Y(ACCRFST-ACCRECD)     SHOULD BE A TRANSACTION               
         SP    CRDTOT,TRNAMNT             SUBTRACT FROM TOTAL                   
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,CURLNNO        CURRENT LINE NUMBER                          
         BAS   RE,DLINE                                                         
*                                                                               
         MVC   CURSUB,=CL3'-  '                                                 
         OI    CURSUBH+6,FOUTTRN                                                
         OI    CURWCH+1,FATBPROT                                                
         OI    CURCPJH+1,FATBPROT                                               
         OI    CURODSH+1,FATBPROT                                               
         OI    CUREXPH+1,FATBPROT                                               
         OI    CURAMTH+1,FATBPROT                                               
         OI    CURWCH+6,FOUTTRN                                                 
         OI    CURCPJH+6,FOUTTRN                                                
         OI    CURODSH+6,FOUTTRN                                                
         OI    CUREXPH+6,FOUTTRN                                                
         OI    CURAMTH+6,FOUTTRN                                                
*                                                                               
         SR    RE,RE                                                            
DETDELX  LTR   RE,RE                                                            
         B     EXIT                                                             
*                                                                               
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
*--------------------------------------------------------------------           
* CHECK PRODUCTION ORDER                                                        
*    CC = NE IF ERROR                                                           
*--------------------------------------------------------------------           
         USING ORDRECD,R5                                                       
ORDCHCK  NTR1                                                                   
         XC    BOWORK1,BOWORK1                                                  
         GOTO1 SCANNER,DMCB,(R2),(2,BOWORK1),0                                  
         CLI   4(R1),0             ANY LINES?                                   
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     ORDCHCKN                                                         
*                                                                               
         LA    RF,BOWORK1          EXPAND ORDER NUMBER                          
         MVC   INVORDNO,=6C'0'                                                  
         LA    RE,INVORDNO+6                                                    
         ZIC   R1,0(RF)                                                         
         SR    RE,R1                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),12(RF)                                                   
         LA    RF,32(RF)           NEXT LINE                                    
         CLI   0(RF),0             THAT'S ALL FOLKS!                            
         BE    ORDCHK05                                                         
         IC    R1,0(RF)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,RF),=C'PART'                                                
         BE    ORDCHK05                                                         
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     ORDCHCKN                                                         
*                                                                               
ORDCHK05 MVC   8(6,R2),INVORDNO    REDISPLAY ORDER NUMBER                       
         CLI   0(RF),0                                                          
         BE    *+10                                                             
         MVC   14(2,R2),=C',P'                                                  
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R5,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   ORDKTYP,ORDKTYPQ                                                 
         MVC   ORDKCPY,COMPANY                                                  
         MVC   ORDKORD,INVORDNO                                                 
         BAS   RE,HIGH                                                          
         CLC   KEY(ORDKSEQ-ORDKEY),KEYSAVE                                      
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$ORDNF)                                           
         B     ORDCHCKN                                                         
*                                                                               
         TM    ORDRSTA,ORDSFMCH    FULLY MATCHED?                               
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$OFMCH)                                           
         B     ORDCHCKN                                                         
*                                                                               
         USING ORDELD,R3                                                        
         L     R3,AIOAREA1                                                      
         MVI   ELCODE,ORDELQ                                                    
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   IVMPVEN,SPACES                                                   
         MVC   IVMPVEN(L'ORDSUPA),ORDSUPA   USE SUPPLIER                        
         OI    IVMPVENH+6,X'80'             TRANSMIT                            
         MVC   ORDCPJ,ORDJOB                ORDER'S CLIENT/PRODUCT/JOB          
*                                                                               
         USING OAMELD,R3                                                        
         USING WCTABD,R4                                                        
         ZAP   ORAMOUNT,=P'0'      INITIALIZE COUNTERS                          
         ZAP   ORINVTDT,=P'0'                                                   
*                                                                               
         XC    WCTABLE,WCTABLE                                                  
         LA    R4,WCTABLE                                                       
         MVI   ELCODE,OAMELQ       ORDER AMOUNT ELEMENTS                        
ORDCHK10 BAS   RE,NEXTEL                                                        
         BNE   ORDCHCKY                                                         
         AP    ORAMOUNT,OAMAMNT                                                 
         AP    ORINVTDT,OAMINUM                                                 
         MVC   WCTWC,OAMWORK       WORKCODE                                     
         TM    OAMSTAT,X'80'                                                    
         BZ    *+8                                                              
         OI    WCTWCS,X'80'        NON-COMMISIONABLE                            
         ZAP   DUB,OAMAMNT         ESTIMATED AMOUNT                             
         SP    DUB,OAMIVAL         LESS INVOICED TO DATE                        
         CLC   OAMLAST,TODAYP      ANY USED TODAY?                              
         BNE   *+10                                                             
         SP    DUB,OAMTVAL         YES, LESS INVOICED TODAY                     
         ZAP   WCTAMT,DUB                                                       
         LA    R4,WCTABLNQ(R4)     BUMP TO NEXT ONE                             
         B     ORDCHK10                                                         
*                                                                               
ORDCHCKY MVC   INVORDNO,ORDKORD                                                 
         EDIT  ORAMOUNT,IVMOAMT,2,ZERO=NOBLANK                                  
         EDIT  ORINVTDT,IVMITD,ZERO=NOBLANK                                     
         OI    IVMOAMTH+6,X'80'                                                 
         OI    IVMITDH+6,X'80'                                                  
         SR    RE,RE                                                            
ORDCHCKN LTR   RE,RE                                                            
         B     EXIT                                                             
         DROP  R3,R4,R5                                                         
         EJECT                                                                  
*--------------------------------------------------------------------           
* CHECK IF LINE WAS CHANGED                                                     
*    R1 - POINTS TO CURRENT LINE                                                
*    CC = EQ, IF NOTHING NEW                                                    
*       = NE, IF SOMETHING NEW                                                  
*--------------------------------------------------------------------           
         USING CURLINED,R1                                                      
CHKLINE  NTR1                                                                   
         LA    R6,5                                                             
         SR    R0,R0                                                            
         LA    R4,CURWCH                                                        
CHKLINE3 CLI   5(R4),0             NOTHING TO VALIDATE                          
         BE    CHKLINE5                                                         
         LTR   RF,RF                                                            
         BZ    CHKLINEX                                                         
         TM    4(R4),X'20'         VALIDATED BEFORE?                            
         BZ    CHKLINEX            NO, NEW INPUT                                
CHKLINE5 IC    R0,0(R4)                                                         
         AR    R4,R0               BUMP TO NEXT FIELD                           
         BCT   R6,CHKLINE3                                                      
         SR    RE,RE                                                            
*                                                                               
CHKLINEX LTR   RE,RE                                                            
         B     EXIT                                                             
*                                                                               
         DROP  R1                                                               
         EJECT                                                                  
*--------------------------------------------------------------------           
* BUILD SUB-SCREEN ELEMENTS                                                     
*    R2 - POINTS TO CURRENT LINE                                                
*--------------------------------------------------------------------           
         USING CURLINED,R2                                                      
         USING SFSELD,R5                                                        
BLDSSEL  NTR1                                                                   
         SR    R0,R0                                                            
         LA    R4,CURWCH                                                        
         LA    R3,CURAMTH                                                       
*                                                                               
BLDSS20  TM    FVATRB-FVIHDR(R4),FVAXTND        EXTENDED FIELD HEADER?          
         BZ    BLDSS60             NOPE, SKIP                                   
         SR    RF,RF                                                            
         IC    RF,FVTLEN-FVIHDR(R4)                                             
         AR    RF,R4                                                            
         SH    RF,=Y(L'FVIHDR)     RF --> EXTENDED FIELD HEADER                 
         CLI   0(RF),0                                                          
         BE    BLDSS60                                                          
         MVI   SFSEL,SFSELQ        BUILD SCREEN ELEMENT                         
         MVC   SFSFLDN,0(RF)       FIELD NUMBER                                 
         LA    RE,L'FVIHDR(R4)     RE --> FIELD DATA                            
*                                                                               
BLDSS40  BCTR  RF,0                RF --> END OF FIELD DATA                     
         CR    RF,RE                                                            
         BL    BLDSS60                                                          
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         B     BLDSS40                                                          
         SR    RF,RE                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SFSFIELD(0),0(RE)                                                
BLDSS50  CLI   SFSFIELD,C' '       LEFT PADDED W/ SPACES?                       
         BH    BLDSS55                                                          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SFSFIELD(0),SFSFIELD+1                                           
         B     BLDSS50                                                          
*                                                                               
BLDSS55  LA    RF,SFSLN1Q+1(RF)    RF=L'ELEMENT                                 
         STC   RF,SFSLN                                                         
         AR    R5,RF                                                            
BLDSS60  IC    R0,FVTLEN-FVIHDR(R4) BUMP TO NEXT TWA FIELD                      
         AR    R4,R0                                                            
         CR    R4,R3               TEST END OF LINE                             
         BNH   BLDSS20                                                          
*                                                                               
         XIT1  REGS=(R5)                                                        
         DROP  R5                                                               
         EJECT                                                                  
*---------------------------------------------------------------                
AGYEXP   NTR1                                                                   
         L     R2,SAVEEXP          A(CURRENT EXPENSE FLD)                       
         MVC   KEY(42),SPACES                                                   
         MVC   KEY(1),COMPANY                                                   
*                                                                               
         MVC   KEY+1(2),=C'SE'     U/L DEBIT                                    
         ZIC   RF,5(R2)            INPUT LEN                                    
         LA    R5,8(R2)                                                         
*                                                                               
AGY06    BCTR  RF,0                SUB 1 FOR EXECUTE                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),0(R5)      EXP ACC TO KEY                               
         SR    R6,R6               NO PROFILES                                  
         BAS   RE,GETACC                                                        
         MVC   POSTACC,ACCTNUM                                                  
         MVC   POSTACCN,ACCTNAME                                                
         MVC   SVEXPNUM,ACCTNUM                                                 
         MVC   SVEXPNM,ACCTNAME                                                 
*                                                                               
         TM    ACCTSTAT,X'80'      BALANCE ELEMENT                              
         BZ    ERROR                                                            
         TM    ACCTSTAT,X'30'      LOCKED OR CLOSED                             
         BNZ   ERROR                                                            
         MVI   STFSW,C'N'                                                       
         MVI   DEPSW,C'N'                                                       
         MVI   OFFSW,C'N'                                                       
         TM    COMPSTAT,X'20'      MANDATORY OFFICE                             
         BZ    AGY30                                                            
         MVI   OFFSW,C'Y'                                                       
AGY30    TM    ACCTSTAT,X'40'      PERSONAL EXPENSE (STAFF=Y)                   
         BZ    AGY35                                                            
         MVI   STFSW,C'Y'                                                       
AGY35    TM    ACCTSTAT,X'08'      DEPT EXPENSE (DEPT=Y)                        
         BZ    AGY40                                                            
         MVI   DEPSW,C'Y'                                                       
AGY40    MVC   COSTANAL(1),ACCTCOST   (ANALYSIS=)                               
*        OI    COSTBYTE,X'40'                                                   
*                                                                               
         CLI   TENO,X'F0'          LENGTH OF ACCT WANTED                        
         BL    AGY45                                                            
*                                                                               
         L     R2,SAVEEXP                                                       
         PACK  DUB,TENO                                                         
         CVB   R4,DUB                                                           
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         ZIC   R6,5(R2)            INPUT LENGTH                                 
         SR    R6,R4               IF DESIRED LEN GREATER THAN GIVEN            
         BM    ERROR               ERROR                                        
*                                                                               
AGY45    CLI   DEPSW,C'Y'          SET UP 2/8 ACCOUNT                           
         BNE   BLD1A                                                            
         MVC   KEY+1(2),=C'28'                                                  
         SR    R6,R6               CLEAR FOR NO PROFILE                         
         BAS   RE,GETACC                                                        
         BAS   RE,CHECKACC                                                      
         MVC   CRDSNUM,ACCTNUM                                                  
         MVC   CRDSNAME,ACCTNAME                                                
         B     BLD1A                                                            
         EJECT                                                                  
*---------------------------------------------------------------                
         USING ACCOMPD,RF                                                       
ADVEXP   NTR1                      VALIDATE C/P/J INPUT                         
         LA    RF,COMPEL                                                        
         MVC   KEY(42),SPACES                                                   
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),ACMPJOB    U/L FOR CLI/PRD/JOB                          
         LA    R6,KEY+3            R4=CLIENT POSITION                           
         DROP  RF                                                               
*                                                                               
         SR    R3,R3               ACCUM CLIENT INPUT LENGTH IN R3              
         LA    R4,8(R2)            R4 POINTS TO EXPENSE INPUT                   
         LA    R1,10               MAXIMUM CLIENT CODE LENGHT                   
ADV1     CLI   0(R4),C','          SCAN FOR DILIMETER                           
         BE    ADV2                                                             
         LA    R3,1(R3)            ACCUM LENGTH OF CLIENT INPUT                 
         LA    R4,1(R4)            BUMP A(INPUT)                                
         BCT   R1,ADV1                                                          
CLTERR   MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     BADRTRN                                                          
*                                                                               
ADV2     CH    R1,=H'10'           DID INPUT BEGIN WITH A COMMA                 
         BE    CLTERR              YES                                          
         BCTR  R3,0                FOR EX INST.                                 
         EX    R3,ADVMVC                                                        
         B     ADV3                                                             
ADVMVC   MVC   0(0,R6),8(R2)       CLT CODE TO KEY                              
*                                                                               
ADV3     DS    0H                                                               
*                                                                               
ADV6     XC    CLIPROF,CLIPROF     CLEAR CLIENT PROFILE FLD                     
         LA    R6,CLIPROF                                                       
         BAS   RE,GETACC                                                        
         MVC   FVMSGNO,=AL2(AE$INACP)                                           
         TM    ACCTSTAT,X'10'                                                   
         BO    BADRTRN                                                          
*                                                                               
         MVC   FVMSGNO,=AL2(AE$SECLK)                                           
         OC    TWAACCS,TWAACCS     TEST LIMIT ACCESS                            
         BZ    ADV7                                                             
         CLC   TWAACCS,SPACES                                                   
         BE    ADV7                                                             
         CLI   TWAACCS,C'*'        OFFICE -CODE                                 
         BE    ADV7                                                             
         CLI   TWAACCS,C'$'        AND LIST WILL BE CHECK BY BASE               
         BE    ADV7                                                             
         CLC   TWAACCS(2),KEY+3    2 CHARACTER CLIENT MATCH                     
         BNE   BADRTRN                                                          
*                                                                               
ADV7     MVC   SVCLINUM,KEY                                                     
         MVC   SVCLINM,ACCTNAME                                                 
         MVC   SVCLIOFF,PPRGAOFF-PPREL(R6)                                      
*                                                                               
         LA    R6,KEY+3            EDIT PRODUCT                                 
         ZIC   R0,CLILNGTH         LEVA LENGTH                                  
         AR    R6,R0               POINT TO PRODUCT POSITION                    
         ZIC   R0,5(R2)            TOTAL LENGTH OF INPUT                        
         LA    R3,2(R3)            ADD 2 TO ADV LN FOR COMMA & EX INST.         
         SR    R0,R3               ANYMORE INPUT                                
         BNZ   ADV8                YES, R1=REMAINING LEN OF INPUT               
         MVC   FVMSGNO,=AL2(AE$MISIF)                                           
         B     BADRTRN                                                          
*                                                                               
ADV8     LA    R4,8(R2)            POINT TO INPUT                               
         AR    R4,R3               TO START POINT PROD INPUT                    
         LR    R5,R4               SAVE                                         
         LA    R1,0                ACCUM PROD LEN                               
         SH    R0,=H'2'            SUB. 1 CHAR. MIN. JOB CODE AND COMMA         
         BNP   ADV9A               NO JOB ERROR                                 
         ST    R0,FULL             SAVE                                         
ADV9     CLI   0(R5),C','          SCAN FOR DILIMETER BETWEEN PRD,JOB           
         BE    ADV10                                                            
         LA    R1,1(R1)            ACCUM LEN OF PRD INPUT                       
         LA    R5,1(R5)            BUMP A(INPUT)                                
         BCT   R0,ADV9                                                          
ADV9A    MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     BADRTRN                                                          
*                                                                               
ADV10    ST    R5,FULL1            SAVE FOR START OF JOB INPUT                  
         L     R5,FULL             DID INPUT BEGIN WITH A COMMA                 
         CR    R0,R5                                                            
         BE    ADV9A               YES                                          
         BCTR  R1,0                FOR EX INST.                                 
         ST    R1,PRDLEN           SAVE LENGTH OF PRD INPUT FOR POSTING         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),0(R4)       PRD CODE TO KEY                              
         XC    PRODPROF,PRODPROF                                                
         LA    R6,PRODPROF                                                      
         BAS   RE,GETACC           READ FOR PRD REC                             
         MVC   SVPRDNM,ACCTNAME    SAVE PRD NAME                                
         MVC   SVPRDNUM,KEY        SAVE PRD KEY                                 
*                                                                               
         LA    R6,KEY+3                                                         
         ZIC   R0,PRDLNGTH                                                      
         AR    R6,R0               POINT TO JOB LOCATION                        
         LA    R1,2(R1)            ADD FOR COMMA AND EX INST.                   
         AR    R3,R1               R3=CLT/PRD INPUT PLUS 2 COMMAS               
         ZIC   R1,5(R2)            =TOTAL INPUT                                 
         SR    R1,R3               =REMAINING INPUT                             
         BP    ADV11A                                                           
ADV11AA  MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     BADRTRN                                                          
*                                                                               
ADV11A   L     R5,FULL1            RESTORE A(JOB INPUT)                         
         LA    R5,1(R5)            POINT TO JOB CODE                            
         BCTR  R1,0                FOR EX INSTR.                                
         ST    R1,JOBLEN           SAVE LENGTH OF JOB INPUT FOR POSTING         
         CLC   JOBLEN,=F'5'                                                     
         BH    ADV11AA                                                          
         EX    R1,*+8              R1=REMAINING INPUT WHICH = JOBLEN            
         B     *+10                                                             
         MVC   0(0,R6),0(R5)       JOB CODE TO KEY                              
ADV12    XC    JOBPROF,JOBPROF     CLEAR                                        
         LA    R6,JOBPROF                                                       
         BAS   RE,GETACC           READ FOR JOB                                 
*                                                                               
         MVC   FVMSGNO,=AL2(AE$INACP)                                           
         TM    ACCTSTAT,X'80'                                                   
         BZ    BADRTRN                                                          
         TM    ACCTSTAT,X'10'      LOCKED                                       
         BO    BADRTRN                                                          
         MVC   FVMSGNO,=AL2(AE$ACTCL)                                           
         TM    ACCTSTAT,X'20'      CLOSED JOB?                                  
         BO    BADRTRN                                                          
         MVC   SVJOBNM,ACCTNAME                                                 
         MVC   SVJOBNUM,KEY                                                     
*                                                                               
         BAS   RE,PROFMERG         PICK UP ANALYSIS FILTER                      
         LA    R4,PROFILE          FROM A PROFILE                               
         USING ACPROFD,R4                                                       
         MVC   PSCLINUM,ACPRCOST   SAVE FOR STAFF POSTING                       
         MVC   CURROFFC,ACPROFFC                                                
         DROP  R4                                                               
*                                                                               
ADV13    MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         L     R2,SAVEWC           =A(CURRENT WORK CODE INPUT)                  
         CLI   5(R2),2             CHECK FOR 2 CHARACTER INPUT                  
         BL    BADRTRN                                                          
*                                                                               
*CONFIRM THAT IT IS A BILLABLE WORK CODE FOR THIS CLT/PRD/JOB                   
*                                                                               
         LA    R4,SVJOBNUM                                                      
         GOTO1 ASETJOB,DMCB,(R4)                                                
         NI    PRGSTAT,X'FF'-PRGXJOB                                            
         TM    ACOPSTAT,ACOXJOB    IS THIS AN X-JOB ?                           
         BZ    *+8                 NO                                           
         OI    PRGSTAT,PRGXJOB     YES, MARK IT                                 
*                                                                               
         L     R4,AGOBLOCK                                                      
         USING GOBLOCKD,R4                                                      
         MVI   PASSCD,C'N'                                                      
         CLI   COCDPASS,C'N'       IS COMP. KEEPING ALL CD'S                    
         BE    *+10                YES                                          
         MVC   PASSCD,GOCLICD                                                   
         LA    RF,GOUWLIST                                                      
         BAS   RE,VALW01                                                        
         B     VALW03                                                           
         DROP  R4                                                               
*                                                                               
VALW01   LA    R0,6                                                             
VALW02   CLC   8(2,R2),0(RF)       CHK FOR MATCH ON NON-BILLABLE WC'S           
         BE    BADRTRN                                                          
         LA    RF,2(RF)                                                         
         BCT   R0,VALW02                                                        
         BR    RE                                                               
*                                                                               
*READ ANALYSIS RECORD FOR VALID WORK CODE                                       
*                                                                               
VALW03   GOTO1 AGETWC,8(R2)                                                     
         BNE   BADRTRN                                                          
         LA    R3,SVWRKNAM                                                      
         MVC   0(L'ACANDESC,R3),WORK                                            
         MVC   SVWRKCD(2),8(R2)                                                 
         B     BLD1A                                                            
*=====================================================================          
*                                                                               
*ADVANCE TO COINCIDING AMOUNT FLD CHECKING FOR NONAPPLICABLE INPUT              
*IN OFF/DEPT/STAFF AND CLI/PRD FIELDS                                           
*                                                                               
         L     R2,SAVEEXP          RESTORE R2 TO A(CURRENT EXP. FLD.)           
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         LA    R0,2                                                             
VALW06   ZIC   R1,0(R2)            TOTAL LENGTH OF FIELD                        
         AR    R2,R1               NEXT FIELD                                   
         CLI   5(R2),0                                                          
         BNE   BADRTRN                                                          
         BCT   R0,VALW06                                                        
*                                                                               
         MVC   FVMSGNO,=AL2(AE$MISIF)                                           
         ZIC   R1,0(R2)            TOTAL LENGTH OF FIELD                        
         AR    R2,R1               TO AMOUNT FLD HEADER                         
*                                                                               
         CLI   5(R2),0                                                          
         BE    BADRTRN             MISSING INPUT FIELD                          
         ST    R2,SAVEAMNT         SAVE A(CURRENT AMOUNT FLD)                   
*                                                                               
         ZIC   R0,5(R2)            LENGTH OF AMOUNT INPUT                       
         GOTO1 CASHVAL,DMCB,(X'82',8(R2)),(R0)                                  
         MVC   FVMSGNO,=AL2(AE$INAMT)                                           
         CLI   0(R1),0                                                          
         BNE   BADRTRN                                                          
         ZAP   SVAMNT,4(8,R1)      SAVE CURRENT AMOUNT                          
         ZAP   NETPOST,SVAMNT                                                   
         ZAP   GRSPOST,SVAMNT      ALWAYS INIT WITH INPUT AMOUNT                
         CLI   FRSTPASS,1          TEST FOR SECOND PASS                         
         BNE   VALW09                                                           
*                                                                               
         CLI   AGYCTRY,CTRYCAN                                                  
         BNE   VALW08                                                           
         CLI   GSTSW,C'Y'          TEST GST APPLICABLE                          
         BNE   VALW08                                                           
         L     R3,AAMT             GET A(AMOUNT TABLE ENTRY)                    
         USING AMTD,R3                                                          
         ZAP   GRSPOST,AMTGRS                                                   
         ZAP   NETPOST,AMTNET                                                   
         ZAP   GSTPOST,AMTGST                                                   
         DROP  R3                                                               
*                                                                               
VALW08   AP    CSHTOT,GRSPOST      ACCUM AMMOUNT                                
*                                                                               
VALW09   GOTO1 AOPTVAL                                                          
         BNE   BADRTRN             ERROR                                        
*                                                                               
         CLI   CHECK,C' '          CHECKING AMOUNT ?                            
         BE    VALW13              NO                                           
         MVC   FVMSGNO,=AL2(AE$AEEWC)                                           
         LA    R3,SVWRKCD                                                       
         GOTO1 AWRKVAL,DMCB,(R3)                                                
         BH    BADRTRN                                                          
*                                                                               
VALW13   B     BLD1A                                                            
*-------------------------------------------------------------------            
*VALW13  CP    DISC,=P'0'          SAVED DISCOUNT                               
         BE    VALW14                                                           
         ZAP   DUB,NETPOST         CALCULATE CD                                 
         MP    DUB,DISC                                                         
         SRP   DUB,64-4,5          ROUNDED DIVIDE BY 10,000                     
         ZAP   CDAMNT,DUB                                                       
*                                                                               
VALW14   ZAP   DUB,SVAMNT                                                       
         SP    DUB,CDAMNT                                                       
VALW15   CP    DISC,=P'0'          IS THERE CASH DISC                           
         BE    BLD                 NO                                           
         CLI   SVCSHNUM,0          IS THERE A CASH ACCT?                        
         BNE   BLD                 YES - NO CD ALLOWED                          
         CLI   PASSCD,C'N'         PASSING CD TO CLIENT                         
         BNE   BLD                 YES - REDUCE PAYABLE                         
*                                                                               
VALW16   DS    0H                                                               
         MVC   KEY,SPACES                                                       
         MVI   KEY,X'09'           READ MEDIA RECORD                            
         MVC   KEY+1(1),COMPANY                                                 
         MVC   KEY+2(1),SVJOBNUM+9 1ST BYTE OF JOB IS MEDIA CODE                
         BAS   RE,HIGH                                                          
         CLC   KEY(15),KEYSAVE                                                  
         BE    VALW17                                                           
         MVC   FVMSGNO,=Y(AE$MMR)   MISSING MEDIA RECORD'                       
         B     BADRTRN                                                          
*                                                                               
VALW17   MVI   ELCODE,ACMDELQ                                                   
         L     R3,AIOAREA1                                                      
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACMEDIAD,R3                                                      
         MVC   KEY,SPACES                                                       
         MVC   KEY(L'ACMDCSHD),ACMDCSHD                                         
         CLC   ACMDCSHD,SPACES                                                  
         BH    VALW22                                                           
*                                                                               
VALW18   DS    0H                                                               
         MVC   KEY,SPACES          READ LEDGER RECORD                           
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),=C'SJ'     PRODUCTION LEDGER                            
         BAS   RE,HIGH                                                          
         CLC   KEY(15),KEYSAVE                                                  
         BE    VALW19                                                           
         MVC   FVMSGNO,=Y(AE$MSJL)                                              
         B     BADRTRN                                                          
*                                                                               
VALW19   MVI   ELCODE,ACLTELQ      GET COMMISSION ACCT FROM LEDGER REC          
         L     R3,AIOAREA1                                                      
         BAS   RE,GETEL                                                         
         BNE   VALW20              USE DEFAULT OF SIMD                          
         USING ACLEDGD,R3                                                       
         CLC   ACLTCDAC,SPACES                                                  
         BNH   VALW20                                                           
*                                                                               
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(14),ACLTCDAC                                               
         B     VALW22                                                           
*                                                                               
VALW20   MVC   KEY,SPACES          ELSE POST TO INCOME                          
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(4),=C'SIMD'                                                
*                                                                               
VALW22   L     R2,SAVEAMNT         A(CURRENT AMOUNT HEADER)                     
         BAS   RE,GETACC                                                        
         BAS   RE,CHECKACC                                                      
         MVC   SIMDAC,ACCTNUM                                                   
         MVC   SIMDACN,ACCTNAME                                                 
*                                                                               
BLD      CLI   FRSTPASS,1          TEST SECOND PASS LOGIC                       
         BNE   BLD1                NO                                           
         GOTO1 BLDELS,DMCB,(RC)    BUILD ELEMENTS FOR EDITED LINE               
BLD1     DS    0H                                                               
*        BAS   RE,DISPNMS          DISPLAY EXPANSIONS                           
         L     RE,AAMT             BUMP AMOUNT POINTER                          
         LA    RE,AMTLNQ(RE)                                                    
         ST    RE,AAMT                                                          
         CLI   GSTSW,C'Y'                                                       
         BNE   BLD1A                                                            
         L     RE,APOST            BUMP TO NEXT POSTINGS                        
         ZICM  R0,0(RE),2                                                       
         AR    RE,R0                                                            
         ST    RE,APOST                                                         
BLD1A    SR    R0,R0               SET GOOD CC                                  
RTRN     XMOD1                                                                  
BADRTRN  LTR   RB,RB               SET BAD CC                                   
         B     RTRN                                                             
*                                                                               
*                                                                               
DLIST    DS    0H                  U/L'S VALID TO DEBIT                         
         DC    C'SASBSFSLSC'       GIDEON - CASH OK                             
         DC    X'FF'                                                            
*                                                                               
         EJECT                                                                  
*---------------------------------------------------------------                
VALDATE  NTR1                                                                   
         L     R2,0(R1)                                                         
         MVC   FVMSGNO,=AL2(AE$INDAT)                                           
         CLI   5(R2),0                                                          
         BZ    VDATE010                                                         
         GOTO1 VPERVAL,DMCB,(5(R2),8(R2)),(X'60',PVBLK)                         
         CLI   DMCB+4,4                                                         
         BNE   XITCCNE                                                          
*                                                                               
         USING PERVALD,R5                                                       
         LA    R5,PVBLK                                                         
         MVC   WORK(6),PVALESTA                                                 
         DROP  R5                                                               
         B     *+8                                                              
VDATE010 BAS   RE,GETODAY                                                       
         GOTO1 DATCON,DMCB,(0,WORK),(1,SAVEDATE)                                
         GOTO1 DATECHK,DMCB,SAVEDATE                                            
         CLI   DMCB,X'FF'                                                       
         BE    XITCCNE                                                          
         GOTO1 DATCON,DMCB,(1,SAVEDATE),(5,8(R2))                               
         OI    6(R2),X'80'                                                      
         B     XITCCEQ                                                          
         EJECT                                                                  
VALAMNT  NTR1                                                                   
         MVC   FVMSGNO,=AL2(AE$INAMT)                                           
         ZIC   R0,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,(X'82',8(R2)),(R0)                                  
         CLI   0(R1),0             TEST OK                                      
         BNE   XITCCNE                                                          
         ZAP   DUB,4(8,R1)                                                      
XITCCEQ  SR    RE,RE                                                            
XITCCNE  LTR   RE,RE                                                            
         B     EXIT                                                             
         EJECT                                                                  
*---------------------------------------------------------------------          
* DELETE OLD GINPAS FOR VENDOR POSTINGS                                         
*---------------------------------------------------------------------          
         USING GINPASD,RE                                                       
DELGMAIN NTR1                                                                   
         LA    RE,IOKEY                                                         
         XC    GINPKEY,GINPKEY                                                  
         MVI   GINPTYP,GINPTYPQ                                                 
         MVC   GINPCPY,COMPANY                                                  
         MVC   GINPINV,GROUPNO                                                  
         GOTO1 ADELGIN                                                          
         B     EXIT                                                             
         EJECT                                                                  
*---------------------------------------------------------------------          
* SUB-ROUTINE TO DELETE A LINE FROM DETAILS, R1 = LINE NUMBER                   
*---------------------------------------------------------------------          
*                                                                               
         USING GINPASD,RE                                                       
DLINE    NTR1                                                                   
         LA    RE,IOKEY                                                         
         XC    GINPKEY,GINPKEY                                                  
         MVI   GINPTYP,GINPTYPQ                                                 
         MVC   GINPCPY,COMPANY                                                  
         MVC   GINPINV,GROUPNO                                                  
         STCM  R1,3,GINPISN                                                     
         GOTO1 ADELGIN                                                          
         B     EXIT                                                             
*                                                                               
         DROP  RE                                                               
         EJECT                                                                  
*---------------------------------------------------------------------          
* SUB-ROUTINE TO MOVE OUT DISPLAY DATA                                          
*---------------------------------------------------------------------          
*                                                                               
MOVEFLD  ST    RE,SVRE                                                          
         ST    R3,SVR3                                                          
         ZIC   R1,0(R2)                                                         
         LA    RE,8+1                                                           
         TM    1(R2),X'02'         TEST FOR EXTENDED FIELD                      
         BZ    *+8                                                              
         LA    RE,8+8+1                                                         
         SR    R1,RE                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),FLD                                                      
*                                                                               
         LA    R3,8(R2,R1)         GET LAST BYTE OF FIELD                       
         LA    RF,1(R1)            GET LENGTH OF FIELD                          
         CLI   0(R3),C' '          LOOK FOR SIGNIFICANT DATA                    
         BH    *+10                                                             
         BCTR  R3,0                                                             
         BCT   RF,*-10                                                          
         OI    6(R2),X'80'                                                      
*                                                                               
MOVEFLDX L     R3,SVR3                                                          
         L     RE,SVRE                                                          
         BR    RE                                                               
         EJECT                                                                  
         GETEL R3,DATADISP,ELCODE                                               
*                                                                               
       ++INCLUDE ACBATCODE                                                      
         EJECT                                                                  
*---------------------------------------------------------------------          
* CALL DETAIL SCREEN                                                            
*---------------------------------------------------------------------          
CALLDETS NTR1                                                                   
         STC   R1,BOBYTE1                                                       
         MVI   CSSPROG,1                                                        
         OI    CSOIND1,CSOIOVRS    OVERLAY CALLS SUB SCREEN                     
         OI    CSINDSL2,CSIOVKEP   KEEP OVERLAY VALUES                          
         GOTO1 ANTRSES,0                                                        
         GOTO1 AOVRSCR,BOPARM,('DTLSCRN',CONTABH)                               
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   CSREC,RECSIT                                                     
         GOTO1 ARECACT,CSREC                                                    
         XIT1                                                                   
         EJECT                                                                  
*---------------------------------------------------------------------          
* CALL ZOOM SCREEN                                                              
*---------------------------------------------------------------------          
CALLZOOM NTR1                                                                   
         STC   R1,BOBYTE1                                                       
*                                                                               
         USING TIOBD,RF                                                         
         L     RF,AINP             A(TIOB)                                      
         SR    R2,R2                                                            
         ICM   R2,3,TIOBCURD       DISPLACEMENT TO CURSOR                       
         A     R2,ATWA                                                          
         DROP  RF                                                               
         LA    RF,IVDSUBH                                                       
         BL    CALLZERR                                                         
         LA    R1,IVDTABH          AFTER LAST LINE                              
         CR    R2,R1               BEFORE FIRST LINE?                           
         BNL   CALLZERR                                                         
*                                                                               
         LA    RF,IVDSUBH                                                       
CALLZ100 CR    RF,R1               ARE WE DONE?                                 
         BNL   CALLZERR            YES, ERROR                                   
         LA    RE,CURLLNQ(RF)      NEXT LINE                                    
         CR    R2,RE                                                            
         BL    CALLZ200            PAST THIS LINE                               
         LR    RF,RE                                                            
         B     CALLZ100                                                         
*                                                                               
CALLZ200 LR    R2,RF               USE THIS LINE                                
         USING CURLINED,R2                                                      
*        LA    R2,IVDSUBH                                                       
         MVC   TMPWC,CURWC                                                      
         MVC   TMPCPJ,CURCPJ                                                    
         MVC   TMPODS,CURODS                                                    
         MVC   TMPEXP,CUREXP                                                    
         MVC   TMPAMT,CURAMT                                                    
*                                                                               
         MVI   CSSPROG,2                                                        
         OI    CSOIND1,CSOIOVRS    OVERLAY CALLS SUB SCREEN                     
         OI    CSINDSL2,CSIOVKEP   KEEP OVERLAY VALUES                          
         GOTO1 ANTRSES,0                                                        
         GOTO1 AOVRSCR,BOPARM,('ZOOMSCRN',CONTABH)                              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   CSREC,RECSIT                                                     
*                                                                               
         MVC   IVZWC,TMPWC                                                      
         MVC   IVZCPJ,TMPCPJ                                                    
         MVC   IVZODS,TMPODS                                                    
         MVC   IVZEXP,TMPEXP                                                    
         MVC   IVZAMT,TMPAMT                                                    
         OI    IVZWCH+6,X'80'                                                   
         OI    IVZCPJH+6,X'80'                                                  
         OI    IVZODSH+6,X'80'                                                  
         OI    IVZEXPH+6,X'80'                                                  
         OI    IVZAMTH+6,X'80'                                                  
*                                                                               
         SR    RE,RE                                                            
CALLZERR LTR   RE,RE                                                            
         XIT1                                                                   
         EJECT                                                                  
*---------------------------------------------------------------------*         
* CALL MAIN SCREEN                                                    *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
CALLMAIN NTR1                                                                   
         STC   R1,BOBYTE1                                                       
         OI    CSINDSL2,CSIOVKEP   KEEP OVERLAY VALUES                          
         GOTO1 AXITSES                                                          
*                                                                               
         ZIC   RE,CSSPROG                                                       
         BCTR  RE,0                                                             
         STC   RE,CSSPROG                                                       
         LTR   RE,RE                                                            
         BNZ   CMAIN01                                                          
         NI    CSOIND1,X'FF'-CSOIOVRS                                           
         MVI   CSREC,RECITE                                                     
         GOTO1 ARECACT,CSREC                                                    
*                                                                               
CMAIN01  LA    R1,BASOLY2H         SET ALL UNPROTECTED FIELDS VALIDATED         
         USING FLDHDRD,R1                                                       
         LA    RF,OSVALS-1                                                      
         XR    RE,RE                                                            
CMAIN02  TM    FLDATB,FATBPROT                                                  
         BNZ   *+8                                                              
         OI    FLDIIND,FINPVAL                                                  
         ICM   RE,1,FLDLEN                                                      
         BZ    *+8                                                              
         BXLE  R1,RE,CMAIN02                                                    
         XIT1                                                                   
         EJECT                                                                  
*--------------------------------------------------------------                 
*        LITERAL DECLARATIONS                                                   
*--------------------------------------------------------------                 
*                                                                               
DICI     DS    0X                                                               
         DCDDL AC#PST,3                                                         
         DCDDL AC#QST,3                                                         
         DCDDL AC#ONT,3                                                         
         DCDDL AC#HST,3                                                         
         DC    AL1(EOT)                                                         
*                                                                               
       ++INCLUDE ACPRVTAB                                                       
         LTORG                                                                  
MAXDBTS  EQU   9                   MAX DEBITS ON SCREEN                         
         EJECT                                                                  
*--------------------------------------------------------------------           
* BUILD DEBIT SCREEN                                                            
*--------------------------------------------------------------------           
         USING CURLINED,R2                                                      
         USING GINPASD,R4                                                       
BLDDBTS  DS    0D                                                               
         NMOD1 0,*BLDDBT*                                                       
         L     RC,0(R1)                                                         
*                                                                               
         ZAP   TOTDETS,=P'0'       INITIALIZE                                   
         LA    R2,IVDSUBH          SHOW SAVED DATA ON SCREEN                    
         OC    GROUPNO,GROUPNO     HAS TO HAVE A GROUP NUMBER                   
         BNZ   *+14                                                             
         ZAP   CRDTOT,=P'0'                                                     
         B     BLDDBT50                                                         
*                                                                               
         LR    R0,R2               KEEP TRACK OF FIRST LINE                     
         CLI   CSACT,ACTDSP                                                     
         BNE   *+8                                                              
         BAS   RE,PROTDBTS                                                      
         LA    R4,IOKEY            READ PASSIVE PTRS FOR DEBITS                 
         XC    GINPKEY,GINPKEY                                                  
         MVI   GINPTYP,GINPTYPQ                                                 
         MVC   GINPCPY,COMPANY                                                  
         MVC   GINPINV,GROUPNO                                                  
         MVC   GINPISN,=H'1'                                                    
         XC    IODAOVER,IODAOVER                                                
         LA    R1,IOACCDIR+IOHID+IO1                                            
BLDDBT05 LA    R4,IOKEY                                                         
         GOTO1 AIO                                                              
         BNL   *+6                                                              
         DC    H'0'                HARDWARE ERROR                               
         TM    IOERR,IOEEOF                                                     
         BZ    *+6                                                              
         DC    H'0'                EOF                                          
         MVC   IODAOVER,IODA                                                    
         CR    R2,R0               ON FIRST ONE?                                
         BNE   *+14                                                             
         CLC   IOKEYSAV(GINPULA-GINPASD),IOKEY                                  
         B     *+10                                                             
         CLC   IOKEYSAV(GINPISN-GINPASD),IOKEY                                  
         BNE   BLDDBT50                                                         
         OC    GINPPTYP,GINPPTYP                                                
         BNZ   BLDDBT05                                                         
         GOTO1 AIO,IOACCMST+IOGET+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING SFSELD,R3                                                        
*                                                                               
         TWAXC CURSUBH,CURAMTH,PROT=Y                                           
         LA    R4,CURWCH                                                        
         L     R3,AIO1                                                          
         AH    R3,=Y(ACCRFST-ACCRECD)                                           
*                                                                               
         MVI   ELCODE,SFSELQ       X'E1'                                        
BLDDBT10 BAS   RE,NEXTEL                                                        
         BNE   BLDDBT40                                                         
         ZIC   R1,SFSLN                                                         
         SH    R1,=Y(SFSLN1Q)                                                   
BLDDBT20 ZIC   RF,0(R4)                                                         
         SH    RF,=H'8'            MINUS EXTENDED FIELD HEADER LEN              
         AR    RF,R4                                                            
         CLC   SFSFLDN,0(RF)       HAS TO BE A DETAIL                           
         BE    BLDDBT30                                                         
         MVI   5(R4),0                                                          
         LA    R4,8(RF)                                                         
         B     BLDDBT20                                                         
*                                                                               
BLDDBT30 STC   R1,5(R4)            SAVE LENGTH OF INPUT                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R4),SFSFIELD                                                 
*                                                                               
         OI    6(R4),X'80'         TRANSMIT AND BUMP TO NEXT FIELD              
         OI    4(R4),X'20'         MARK AS VALIDATED                            
         SR    R1,R1                                                            
         IC    R1,0(R4)                                                         
         AR    R4,R1                                                            
         B     BLDDBT10                                                         
*                                                                               
BLDDBT40 LA    R4,IOKEY                                                         
         MVI   CURSUB,C'*'                                                      
         TM    GINPKSTA,X'80'      DELETED?                                     
         BZ    BLDDBT45                                                         
         MVI   CURSUB,C'-'                                                      
         BAS   RE,PROTLINE                                                      
*                                                                               
BLDDBT45 OI    CURSUBH+6,X'80'     TRANSMIT                                     
         LR    RF,R2                                                            
         LA    R2,CURAMTH                                                       
         BAS   RE,VALAMNT                                                       
         LR    R2,RF                                                            
         ZAP   NETPOST,DUB                                                      
         EDIT  NETPOST,(11,CURAMT),2,ZERO=NOBLANK                               
         TM    GINPKSTA,X'80'      DELETED?                                     
         BO    *+10                                                             
         AP    TOTDETS,NETPOST                                                  
*                                                                               
         LA    R2,CURLLNQ(R2)      BUMP TO NEXT LINE                            
         LA    R1,IOACCDIR+IOSQD+IO1                                            
         B     BLDDBT05                                                         
*                                                                               
BLDDBT50 BAS   RE,SHOWWCT          SHOW WORKCODE TABLE, IF ANY                  
*                                                                               
         ZAP   DUB,VENDAMT         SHOW CREDIT-DEBITS                           
         SP    DUB,TOTDETS                                                      
         ZAP   BCWORK(8),DUB                                                    
         EDIT  (P8,BCWORK),(11,IVDDIFF),2,ZERO=NOBLANK,FLOAT=-                  
         OI    IVDDIFFH+6,X'80'                                                 
*                                                                               
BLDDBTSX XIT1                                                                   
         EJECT                                                                  
*--------------------------------------------------------------------           
* PROTECT DEBIT SCREEN FIELDS                                                   
*--------------------------------------------------------------------           
         USING CURLINED,R2                                                      
PROTDBTS NTR1                                                                   
         LA    R3,MAXDBTS                                                       
         LA    R2,IVDSUBH                                                       
PROTDBT5 BAS   RE,PROTLINE         PROTECT LINE                                 
         LA    R2,CURLLNQ(R2)                                                   
         BCT   R3,PROTDBT5                                                      
         B     BLDDBTSX                                                         
         SPACE 3                                                                
PROTLINE OI    CURWCH+1,FATBPROT                                                
         OI    CURCPJH+1,FATBPROT                                               
         OI    CURODSH+1,FATBPROT                                               
         OI    CUREXPH+1,FATBPROT                                               
         OI    CURAMTH+1,FATBPROT                                               
         OI    CURWCH+6,FOUTTRN    TRANSMIT                                     
         OI    CURCPJH+6,FOUTTRN                                                
         OI    CURODSH+6,FOUTTRN                                                
         OI    CUREXPH+6,FOUTTRN                                                
         OI    CURAMTH+6,FOUTTRN                                                
         BR    RE                                                               
         EJECT                                                                  
*--------------------------------------------------------------------           
* SHOW WORKCODE TABLE, IF PRODUCTION ORDER READ                                 
*    R2 = CURRENT LINE                                                          
*--------------------------------------------------------------------           
         USING WCTABD,R4                                                        
SHOWWCT  NTR1                                                                   
         CLI   CSACT,ACTDSP        DON'T SHOW FOR ACTION DISPLAY                
         BE    BLDDBTSX                                                         
         CLC   ORDCPJ,SPACES       ANY WORKCODE TABLE SET?                      
         BNH   BLDDBTSX            NO, LEAVE                                    
*                                                                               
         MVC   FLD,SPACES                                                       
         LA    RF,FLD                                                           
         LA    R4,ORDCPJ+3                                                      
         SR    R1,R1                                                            
         IC    R1,CLILNGTH                                                      
         LR    R3,R1                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),0(R4)       PUT CLIENT ON SCREEN                         
*                                                                               
         AR    R4,R3                                                            
         AR    RF,R3                                                            
         MVI   0(RF),C','                                                       
         LA    RF,1(RF)                                                         
         IC    R1,PRDLNGTH                                                      
         SR    R1,R3                                                            
         LR    R3,R1                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),0(R4)       PUT PRODUCT ON SCREEN                        
*                                                                               
         AR    R4,R3                                                            
         AR    RF,R3                                                            
         MVI   0(RF),C','                                                       
         LA    RF,1(RF)                                                         
         IC    R1,JOBLNGTH                                                      
         IC    R3,PRDLNGTH                                                      
         SR    R1,R3                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),0(R4)       PUT JOB ON SCREEN                            
         LA    RF,0(R1,RF)                                                      
*                                                                               
         LA    R3,FLD                                                           
         SR    RF,R3                                                            
*                                                                               
         LA    R4,WCTABLE                                                       
SHOWWCT4 CLI   0(R4),0             EOT?                                         
         BE    SHOWWCTX                                                         
         MVC   CURWC(2),WCTWC      WORKCODE                                     
         TM    WCTWCS,WCTWCNC      NON-COMMISIONABLE?                           
         BZ    *+10                                                             
         MVC   CURWC+2(2),=C',N'                                                
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   CURCPJ(0),FLD       CLIENT/PRODUCT/JOB                           
         EDIT  WCTAMT,(11,CURAMT),2,ZERO=NOBLANK,FLOAT=-                        
*                                                                               
         OI    CURWCH+6,X'81'      TRANSMIT & MARK INPUT THIS TIME              
         OI    CURCPJH+6,X'81'                                                  
         OI    CURAMTH+6,X'81'                                                  
         LA    R2,CURLLNQ(R2)                                                   
         LA    R4,WCTABLNQ(R4)                                                  
         B     SHOWWCT4            LOOP BACK FOR MORE WORKCODES                 
*                                                                               
SHOWWCTX XC    WCTABLE,WCTABLE     GET RID OF IT WHEN FINISHED                  
         XC    ORDCPJ,ORDCPJ                                                    
         B     BLDDBTSX                                                         
         SPACE 3                                                                
         DROP  R2,R4                                                            
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------           
* DO POSTINGS FOR DEBITS                                                        
*--------------------------------------------------------------------           
POSTDBT  DS    0D                                                               
         NMOD1 0,*PDBT*                                                         
         L     RC,0(R1)                                                         
*                                                                               
         MVI   CURPSTNO,0                                                       
         OC    GROUPNO,GROUPNO     GET GROUP INVOICE #, IF WE DIDN'T            
         BNZ   POST0100                                                         
         GOTO1 AGETGIN                                                          
         MVC   GROUPNO,CSGIN                                                    
*                                                                               
         USING DLDESCD,R5                                                       
POST0100 LA    RE,IOAREA           CLEAR                                        
         LA    RF,3000                                                          
         XCEF                                                                   
*                                                                               
         LA    R5,IOAREA+2                                                      
         MVI   DLDSEL,DLDSELQ      X'64' POSTING DESCR ELEMENT                  
         MVC   DLDSREF,DETLREF                                                  
         MVC   DLDSDATE,INVDTE                                                  
         MVI   DLDSSBRF,0                                                       
         MVI   DLDSSTAT,0                                                       
         MVI   DLDSLEN,DLDSLN1Q    NO NARRATIVE                                 
         SR    RE,RE                                                            
         ICM   RE,1,CRNARRLN       ANY NARRATIVE?                               
         BZ    POST0105                                                         
         LA    R1,DLDSLN1Q                                                      
         AR    R1,RE                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   DLDSNARR(0),CRNARR                                               
         STC   R1,DLDSLEN                                                       
*                                                                               
POST0105 SR    RE,RE                                                            
         IC    RE,1(R5)                                                         
*                                                                               
         USING GINELD,R5                                                        
         AR    R5,RE                                                            
         BAS   RE,PSTCOMM                                                       
*                                                                               
         L     R2,ACURLN           GET ADDRESS OF CURRENT LINE                  
         BAS   RE,BLDSSEL          BUILD SUB-SCREEN ELEMENT                     
*                                                                               
         TM    PRGSTAT,PRGXJOB     XJOB?                                        
         BZ    POST0107                                                         
         USING TRCASHD,R5                                                       
         MVI   TRCSEL,TRCSELQ      BUILD MEMO WITH AMOUNT                       
         MVI   TRCSLEN,TRCSLNQ1                                                 
         MVI   TRCSTYPE,C'S'                                                    
         ZAP   TRCSAMNT,NETPOST                                                 
         LA    R5,TRCSLNQ1(R5)                                                  
*                                                                               
         USING DLPOSTD,R5                                                       
POST0107 MVI   DLPSEL,DLPSEDRQ     BUILD DEBIT ELEMENT                          
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVI   DLPSTYPE,0                                                       
         ZAP   DLPSAMNT,NETPOST                                                 
         TM    PRGSTAT,PRGPROD     PRODUCTION?                                  
         BO    POST0110            YES, USE PROD STUFF                          
*                                                                               
         MVC   DLPSDBAC,SVEXPNUM   EXPENSE ACCT                                 
         MVC   DLPSDBNM,SVEXPNM      NAME                                       
         MVC   DLPSCRAC,SVEVEN     EXP VENDOR                                   
         MVC   DLPSCRNM,SVEVENNM     NAME                                       
         MVC   DLPSANAL,OFFICE                                                  
         B     POST0115                                                         
*                                                                               
POST0110 MVC   DLPSDBAC,SVJOBNUM   CLI/PRO/JOB ACCT                             
         MVC   DLPSDBNM,SVJOBNM      NAME                                       
         MVC   DLPSCRAC,SVPVEN     PROD VENDOR                                  
         MVC   DLPSCRNM,SVPVENNM     NAME                                       
         MVC   DLPSANAL,SVOFFICE                                                
         CLC   SVOFFICE,SPACES                                                  
         BH    *+10                                                             
         MVC   DLPSANAL,SVCLIOFF                                                
         TM    PRGSTAT,PRGNCOMM    NON-COMMISSIONABLE?                          
         BZ    *+8                                                              
         OI    DLPSTYPE,X'40'      SET FOR NO COMMISSION                        
         MVC   DLPSANAL,SVWRKCD                                                 
         TM    PRGSTAT,PRGXJOB     XJOB?                                        
         BZ    POST0115                                                         
         MVC   DLPSCRAC,SVEXPNUM   EXPENSE ACCT                                 
         MVC   DLPSCRNM,SVEXPNM      NAME                                       
         ZAP   DLPSAMNT,=P'0'                                                   
*                                                                               
POST0115 ZIC   RE,1(R5)                                                         
         AR    R5,RE                                                            
*                                                                               
         BAS   RE,PSTCOMM                                                       
*                                                                               
         USING DLPOSTD,R5                                                       
         MVI   DLPSEL,DLPSECRQ     BUILD CREDIT ELEMENT                         
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVI   DLPSTYPE,0                                                       
         TM    PRGSTAT,PRGPROD     PRODUCTION?                                  
         BO    POST0120            NO, USE PROD STUFF                           
*                                                                               
         MVC   DLPSDBAC,SVEXPNUM   EXPENSE ACCT                                 
         MVC   DLPSDBNM,SVEXPNM      NAME                                       
         MVC   DLPSCRAC,SVEVEN     EXP VENDOR                                   
         MVC   DLPSCRNM,SVEVENNM     NAME                                       
         MVC   DLPSANAL,OFFICE                                                  
         B     POST0125                                                         
*                                                                               
POST0120 MVC   DLPSDBAC,SVCLINUM   CLIENT ACCT                                  
         MVC   DLPSDBNM,SVCLINM      NAME                                       
         MVC   DLPSCRAC,SVPVEN     PROD VENDOR                                  
         MVC   DLPSCRNM,SVPVENNM     NAME                                       
         MVC   DLPSANAL,SVOFFICE                                                
         CLC   SVOFFICE,SPACES                                                  
         BH    *+10                                                             
         MVC   DLPSANAL,SVCLIOFF                                                
POST0125 ZAP   DLPSAMNT,NETPOST                                                 
         ZIC   RE,1(R5)                                                         
         AR    R5,RE                                                            
*                                                                               
         TM    PRGSTAT,PRGXJOB     XJOB?                                        
         BZ    POST0200                                                         
         BAS   RE,PSTCOMM                                                       
*                                                                               
         MVI   DLPSEL,DLPSEDRQ     BUILD DEBIT ELEMENT                          
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVI   DLPSTYPE,0                                                       
         MVC   DLPSDBAC,SVEXPNUM   EXPENSE ACCT                                 
         MVC   DLPSDBNM,SVEXPNM      NAME                                       
         MVC   DLPSCRAC,SVPVEN     PROD VENDOR                                  
         MVC   DLPSCRNM,SVPVENNM     NAME                                       
         MVC   DLPSANAL,OFFICE                                                  
         TM    PRGSTAT,PRGXJOB     XJOB?                                        
         BZ    *+10                                                             
         MVC   DLPSANAL,FINOFF     FINANCIAL OFFICE                             
         ZAP   DLPSAMNT,NETPOST                                                 
         ZIC   RE,1(R5)                                                         
         AR    R5,RE                                                            
         EJECT                                                                  
*--------------------------------------------------------------                 
*        2P WITH 29 CONTRA POSTING                                              
*--------------------------------------------------------------                 
*                                                                               
POST0200 CLI   STFSW,C'Y'                                                       
         BNE   POST0400                                                         
         BAS   RE,PSTCOMM                                                       
*                                                                               
         USING DLPOSTD,R5                                                       
         MVI   DLPSEL,DLPSEDRQ     DEBIT STAFF                                  
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSDBAC,STAFFNUM   DEBIT 2P                                     
         MVC   DLPSDBNM,STAFFNAM                                                
         MVC   DLPSCRNM,CRPSNAME   CONTRA 29                                    
         ZAP   DLPSAMNT,NETPOST                                                 
         OI    DLPSTYPE,X'80'      SUBSIDIARY FROM HERE                         
         MVI   DLPSCRAC,C'*'       CONTRA IS *EXPENSE-CLIENT                    
         L     R2,SAVEEXP          CREDIT SE ACCT                               
         ZIC   R1,5(R2)            INPUT LENGTH                                 
         BCTR  R1,0                FOR EX INST.                                 
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DLPSCRAC+1(0),8(R2)                                              
         MVC   DLPSANAL,OFFICE                                                  
         TM    PRGSTAT,PRGXJOB     XJOB?                                        
         BZ    *+10                                                             
         MVC   DLPSANAL,FINOFF     FINANCIAL OFFICE                             
*                                                                               
         CLI   TENO,X'F0'          LENGTH OF ACCT WANTED                        
         BL    POST0300                                                         
         PACK  DUB,TENO                                                         
         CVB   R4,DUB                                                           
         MVC   DLPSCRAC+1(14),SPACES                                            
         L     R2,SAVEEXP                                                       
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         LA    R6,1(R1)            INPUT LENGTH                                 
         SR    R6,R4               IF DESIRED LEN GREATER THAN GIVEN            
         BNM   *+6                                                              
         DC    H'0'                SHOULD HAVE CAUGHT THIS EARLIER              
         LA    R3,8(R6,R2)         R3=A(DESIRED PORTION ACCT INPUT)             
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   DLPSCRAC+1(0),0(R3) = * +DESIRED PORTION ACCT INPUT              
         LR    R1,R4                                                            
*                                                                               
POST0300 STC   R1,BYTE             LEN OF ACCT INPUT DESIRED                    
         LA    RF,DLPSCRAC+2(R1)                                                
         MVI   0(RF),C'-'                                                       
         LA    R1,DLPSCRAC+14                                                   
         SR    R1,RF                                                            
         BCTR  R1,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   1(0,RF),CRPSNUM+3   2/9 ACCT KEY                                 
         MVC   WORK(15),DLPSCRAC   SAVE FOR DEBIT SIDE OF CRD CLI EL            
         ZIC   R4,DLPSLEN                                                       
         AR    R5,R4                                                            
         EJECT                                                                  
*--------------------------------------------------------------                 
*        29 WITH 2P CONTRA POSTING                                              
*--------------------------------------------------------------                 
*                                                                               
         BAS   RE,PSTCOMM                                                       
*                                                                               
         USING DLPOSTD,R5                                                       
         MVI   DLPSEL,DLPSECRQ     CREDIT CLIENT                                
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSDBNM,STAFFNAM   CONTRA 2P                                    
         MVC   DLPSCRNM,CRPSNAME   CREDIT 29                                    
         ZAP   DLPSAMNT,NETPOST                                                 
         OI    DLPSTYPE,X'80'      SUBSIDIARY FROM HERE                         
         MVC   DLPSDBAC(15),WORK   SAVED FROM DEBIT ELEMENT                     
         MVC   DLPSCRAC,CRPSNUM                                                 
         IC    R1,BYTE                                                          
         LA    R1,1(R1)                                                         
         LA    RF,DLPSDBAC+1(R1)                                                
         LA    R1,DLPSDBAC+14                                                   
         SR    R1,RF                                                            
         BCTR  R1,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   1(0,RF),STAFFNUM+3  CONTRA IS *EXPENSE-STAFF                     
         MVC   DLPSANAL,OFFICE                                                  
         TM    PRGSTAT,PRGXJOB     XJOB?                                        
         BZ    *+10                                                             
         MVC   DLPSANAL,FINOFF     FINANCIAL OFFICE                             
*                                                                               
         CLI   V29SW,C'Y'          SHOULD CONTRA BE VENDOR?                     
         BNE   POST0390            NO                                           
         MVC   DLPSDBAC,SVEVEN     EXP VENDOR                                   
         MVC   DLPSDBNM,SVEVENNM   EXP VENDOR NAME                              
*                                                                               
POST0390 ZIC   R4,DLPSLEN                                                       
         AR    R5,R4                                                            
         B     POST0400                                                         
         EJECT                                                                  
*--------------------------------------------------------------                 
*        2D WITH 28 CONTRA POSTING  DR                                          
*        28 WITH 2D CONTRA POSTING  CR                                          
*--------------------------------------------------------------                 
*                                                                               
POST0400 DS    0H                                                               
         CLI   DEPSW,C'Y'                                                       
         BNE   POST0500                                                         
*                                                                               
         BAS   RE,PSTCOMM                                                       
*                                                                               
         USING DLPOSTD,R5                                                       
         MVI   DLPSEL,DLPSEDCQ                                                  
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSDBAC,DEPNUM     DEBIT 2D   CONTRA = 28                       
         MVC   DLPSDBNM,DEPNAME                                                 
         MVC   DLPSCRAC,CRDSNUM    CREDIT 28  CONTRA = 2D                       
         MVC   DLPSCRNM,CRDSNAME                                                
         ZAP   DLPSAMNT,NETPOST                                                 
         MVC   DLPSANAL,SPACES                                                  
         MVC   DLPSANAL,OFFICE                                                  
         TM    PRGSTAT,PRGXJOB     XJOB?                                        
         BZ    *+10                                                             
         MVC   DLPSANAL,FINOFF     FINANCIAL OFFICE                             
         OI    DLPSTYPE,X'80'      IN CASE WE MISSED IT ON ELEMENT 2            
         ZIC   R4,DLPSLEN                                                       
         AR    R5,R4                                                            
         B     POST0500                                                         
         EJECT                                                                  
*--------------------------------------------------------------                 
*        2C WITH 27 CONTRA POSTING                                              
*        27 WITH 2C CONTRA POSTING                                              
*--------------------------------------------------------------                 
*                                                                               
POST0500 CLI   ECONSULT,C'Y'       DO WE NEED EXP 2C?                           
         BNE   POST0600            NO.                                          
*                                                                               
         BAS   RE,PSTCOMM                                                       
*                                                                               
         USING DLPOSTD,R5                                                       
         MVI   DLPSEL,DLPSEDCQ     DB & CVR                                     
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSDBAC,E2CNUM     ACCT #                                       
         MVC   DLPSDBNM,E2CNAM     ACCT NAME                                    
         MVC   DLPSCRAC,EXPTROL    27                                           
         MVC   DLPSCRNM,EXPTROLN   27                                           
         ZAP   DLPSAMNT,GRSPOST    AMT                                          
         MVC   DLPSANAL,OFFICE                                                  
         TM    PRGSTAT,PRGXJOB     XJOB?                                        
         BZ    *+10                                                             
         MVC   DLPSANAL,FINOFF     FINANCIAL OFFICE                             
         OI    DLPSTYPE,X'80'                                                   
         ZIC   R4,DLPSLEN          LEN                                          
         AR    R5,R4               INCREMENT                                    
         B     POST0600                                                         
         EJECT                                                                  
*--------------------------------------------------------------                 
*        1C WITH 1P CONTRA POSTING                                              
*--------------------------------------------------------------                 
*                                                                               
POST0600 CLI   COSTANAL,C' '                                                    
         BE    POSTDXT                                                          
         TM    COSTSW,COSTACC                                                   
         BZ    POSTDXT                                                          
*                                                                               
         BAS   RE,PSTCOMM                                                       
*                                                                               
         USING DLPOSTD,R5                                                       
         MVI   DLPSEL,DLPSEDRQ     DEBIT DEPT C/A CLIENT                        
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSCRAC,COSTNUM    CONTRA  1C                                   
         MVC   DLPSCRNM,COSTNAME                                                
         MVC   DLPSDBAC,CRCNUM     DEBIT 1P                                     
         MVC   DLPSDBNM,CRCNAME                                                 
         ZAP   DLPSAMNT,NETPOST                                                 
         OI    DLPSTYPE,X'80'                                                   
         MVC   DLPSANAL,OFFICE                                                  
         TM    PRGSTAT,PRGXJOB     XJOB?                                        
         BZ    *+10                                                             
         MVC   DLPSANAL,FINOFF     FINANCIAL OFFICE                             
         ZIC   R4,DLPSLEN                                                       
         AR    R5,R4                                                            
*                                                                               
         BAS   RE,PSTCOMM                                                       
*                                                                               
         USING DLPOSTD,R5                                                       
         MVI   DLPSEL,DLPSECRQ     CREDIT CLIENT                                
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSCRAC,COSTNUM    CR 1C                                        
         MVC   DLPSCRNM,COSTNAME                                                
         MVC   DLPSDBAC,CR13NUM    CONTRA 13                                    
         MVC   DLPSDBNM,CR13NAME                                                
         OI    DLPSTYPE,X'80'                                                   
         MVC   DLPSANAL,OFFICE                                                  
         TM    PRGSTAT,PRGXJOB     XJOB?                                        
         BZ    *+10                                                             
         MVC   DLPSANAL,FINOFF     FINANCIAL OFFICE                             
         ZAP   DLPSAMNT,NETPOST                                                 
         ZIC   R4,DLPSLEN                                                       
         LR    R6,R5                                                            
         AR    R5,R4                                                            
*--------------------------------------------------------------------           
* ADD SUB-ITEMS TO FILE                                                         
*--------------------------------------------------------------------           
POSTDXT  MVI   0(R5),0             MARK EOR                                     
         LA    RF,IOAREA-1         GET LENGTH OF RECORD                         
         SR    R5,RF                                                            
         STH   R5,HALF                                                          
         MVC   IOAREA(2),HALF                                                   
*        ZAP   CRDTOT,NETPOST      TESTING                                      
         ZAP   DUB,NETPOST                                                      
         ZAP   TRANSAMT,NETPOST                                                 
*                                                                               
         XC    BOWORK1,BOWORK1        ADD JOB/EXP TXS BUT NOT ITEM              
         MVC   BOWORK1(6),DOCSAVE     REFERENCE                                 
         MVC   BOWORK1+11(1),CSBTYP   BATCH TYPE                                
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         TM    PRGSTAT,PRGMDEBT                                                 
         BO    POSTDXT5                                                         
         ZAP   BOPL61,NETPOST                                                   
         GOTO1 AADACDAY,BOPARM,IOAREA,BOPL61,(X'80',BOWORK1),0                  
         B     POSTDXTX                                                         
*                                                                               
POSTDXT5 ZAP   BOPL61,CRDTOT                                                    
         GOTO1 AADACDAY,BOPARM,(X'80',IOAREA),BOPL61,(0,BOWORK1),BOELEM         
POSTDXTX XIT1                                                                   
         EJECT                                                                  
*--------------------------------------------------------------                 
*        ROUTINE TO PUT COMMON POSTINGS PARTS IN IO                             
*--------------------------------------------------------------                 
*                                                                               
         USING GINELD,R5                                                        
PSTCOMM  TM    PRGSTAT,PRGMDEBT    DID WE USE A MAIN DEBIT?                     
         BOR   RE                                                               
         MVI   GINEL,GINELQ                                                     
         MVI   GINLN,GINLN2Q                                                    
         MVC   GININV,GROUPNO                                                   
         MVC   GINHISN,CURLNNO     CURRENT LINE NUMBER                          
         MVC   GINTYP,CURPSTNO     CURRENT POST NUMBER                          
         SR    R1,R1                                                            
         IC    R1,1(R5)                                                         
         AR    R5,R1                                                            
*                                                                               
         USING FFNELD,R5           ORDER NUMBER ELEMENT                         
         OC    INVORDNO,INVORDNO                                                
         BZ    PSTCOMMX                                                         
         MVI   FFNEL,FFNELQ                                                     
         MVI   FFNLN,FFNLN2Q                                                    
         MVC   FFNONUM,INVORDNO                                                 
*        TM    PRGOSTAT,PRGOPART   PARTIAL ORDER                                
*        BZ    *+8                                                              
*        MVI   FFNSTAT,FFNSPRTQ                                                 
         IC    R1,1(R5)            BUMP TO NEXT POSITION                        
         AR    R5,R1                                                            
PSTCOMMX IC    R1,CURPSTNO                                                      
         LA    R1,1(R1)                                                         
         STC   R1,CURPSTNO                                                      
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------                 
*        ERROR CHECK ROUTINE FOR OFFICE/DEPT/STAFF                              
*--------------------------------------------------------------                 
*                                                                               
ERRCHCK  DS    0D                                                               
         NMOD1 0,*ERCH*,R7                                                      
         L     RC,0(R1)                                                         
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         CLC   DEPTL(1),DPTLNGTH                                                
         BNH   ERRB                                                             
         MVC   FVMSGNO,=Y(AE$INVDL)    INVALID DEPT LENGTH                      
         B     ERRX                                                             
*                                                                               
ERRB     DS    0H                                                               
         CLI   STAFFL,8                                                         
         BNH   ERRC                                                             
         MVC   FVMSGNO,=Y(AE$INVSL)    INVALID STAFF LENGTH                     
         B     ERRX                                                             
*                                                                               
ERRC     DS    0H                                                               
         MVI   ODSSW,0             CLEAR REQUIRED FLAG                          
         CLI   OFFSW,C'Y'                                                       
         BNE   *+8                                                              
         OI    ODSSW,OFFCBIT       IF OFFICE ENTERED THEN X'04' ON              
         CLI   DEPSW,C'Y'                                                       
         BNE   *+8                                                              
         OI    ODSSW,DEPTBIT       IF DEPT ENTERED THEN X'02' ON                
         CLI   STFSW,C'Y'                                                       
         BNE   *+8                                                              
         OI    ODSSW,STFFBIT       IF STAFF ENTERED THEN X'01' ON               
*                                                                               
         CLI   STFSW,C'Y'                                                       
         BNE   ERRD                ONLY READ 2P IF STAFF IS ON                  
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),=C'2P'     SET UP 2/P ACCOUNT                           
*                                                                               
         MVC   RKEY,KEY                                                         
         GOTO1 AREAD,AIOAREA1      READ LEDGER FOR STRUCTURE                    
         BE    *+6                                                              
         DC    H'0'                MISSING 2P LEDGER                            
*                                                                               
         SR    RE,RE                                                            
         L     R3,AIOAREA                                                       
         AH    R3,DATADISP                                                      
         CLI   0(R3),ACHRELQ       MUST HAVE HEIRARCHY ELEMENT                  
         BE    *+14                                                             
         IC    RE,1(R3)                                                         
         AR    R3,RE                                                            
         B     *-14                                                             
*                                                                               
         USING ACHEIRD,R3                                                       
         MVI   LEVEL,3                                                          
         CLI   ACHRLEVC,12         3 LEVEL ACCT?                                
         BE    ERRC1                                                            
*                                                                               
         MVI   LEVEL,2                                                          
         CLI   ACHRLEVB,12         2 LEVEL ACCT?                                
         BE    ERRC1                                                            
*                                                                               
         MVI   LEVEL,1                                                          
         CLI   ACHRLEVA,12         1 LEVEL ACCT?                                
         BE    ERRC1                                                            
         DC    H'0'                BAD ACCT LEVEL                               
*                                                                               
ERRC1    CLI   LEVEL,1             FOR 2 & 3 LEVEL, NEED DEPT                   
         BE    ERRD                                                             
         OI    ODSSW,DEPTBIT       MAKE DEPT REQUIRED AS INPUT                  
*                                                                               
ERRD     DS    0H                  'ODSSW' HAS CORRECT BIT SETTINGS             
         MVI   INPUTSW,0           'INPUT' WILL HAVE BITS FOR WHAT WAS          
         CLI   OFFICEL,0            ENTERED IN THE OF/DP/ST FIELD.              
         BE    *+8                                                              
         OI    INPUTSW,OFFCBIT                                                  
         CLI   DEPTL,0                                                          
         BE    *+8                                                              
         OI    INPUTSW,DEPTBIT                                                  
         CLI   STAFFL,0                                                         
         BE    *+8                                                              
         OI    INPUTSW,STFFBIT                                                  
*                                                                               
         XC    INPUTSW(1),ODSSW    IF ALL IS OK, INPUT SHOULD BE 0              
         CLI   OFFSW,C'Y'                                                       
         BE    *+8                                                              
         NI    INPUTSW,X'03'       ALLOW OFFICE IF OFFSW=N                      
         CLI   INPUTSW,0                                                        
         BE    ERROK                                                            
*                                                                               
         MVI   ERRNUM,SPECIAL                                                   
         L     R2,SAVEODS          POINT TO OFFICE/DPT/STFF FIELD               
         MVC   MSG,SPACES                                                       
         TM    ODSSW,OFFCBIT       SHOULD OFFICE BE ENTERED?                    
         BZ    ERRE                                                             
         MVC   FVMSGNO,=Y(AE$CFO)                                               
*                                                                               
ERRE     TM    ODSSW,DEPTBIT       SHOULD DEPT BE ENTERED?                      
         BZ    ERRF                                                             
         MVC   FVMSGNO,=Y(AE$CFOD)                                              
*                                                                               
ERRF     TM    ODSSW,STFFBIT       SHOULD STAFF BE ENTERED?                     
         BZ    ERRG                                                             
         MVC   FVMSGNO,=Y(AE$CFODS)                                             
         B     ERRX                                                             
*                                                                               
ERRG     CLI   ODSSW,0                                                          
         BNE   ERRX                                                             
         MVC   FVMSGNO,=Y(AE$NOIPP)  NO INPUT                                   
         B     ERRX                                                             
*                                                                               
ERROK    MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
ERRX     XMOD1                                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------                 
*        LOCAL STORAGE                                                          
*--------------------------------------------------------------                 
*                                                                               
PROGD    DSECT                                                                  
RELO1    DS    A                                                                
RIGHT    DS    V                                                                
*                                                                               
* SCREEN DIRECTORY                                                              
*                                                                               
AORDH    DS    A                   A(ORDER NUMBER HEADER)                       
AOHSTH   DS    A                   A(ORDER HISTORY HEADER)                      
ADOCH    DS    A                   A(DOCUMENT NUMBER HEADER)                    
ADATH    DS    A                   A(TRANSACTION DATE HEADER)                   
ACLIH    DS    A                   A(CLIENT HEADER)                             
ACLINH   DS    A                   A(CLIENT NAME HEADER                         
APROH    DS    A                   A(PRODUCT HEADER)                            
APRONH   DS    A                   A(PRODUCT NAME HEADER)                       
AJOBH    DS    A                   A(JOB HEADER)                                
AJOBNH   DS    A                   A(JOB NAME HEADER)                           
ASUPH    DS    A                   A(SUPPLIER HEADER)                           
ASUPNH   DS    A                   A(SUPPLIER NAME HEADER)                      
AURGH    DS    A                   A(URGENT HEADER)                             
ACDH     DS    A                   A(CASH DISCOUNT HEADER)                      
AWRKH    DS    A                   A(WORK CODE HEADER)                          
AWCNMH   DS    A                   A(WORK CODE NAMES HEADER)                    
AAMTH    DS    A                   A(AMOUNTS HEADER)                            
ANCWKH   DS    A                   A(NON-COMM WORKCODES HEADER)                 
AWNNMH   DS    A                   A(NON-COMM WORKCODE NAMES HEADER)            
ANCAH    DS    A                   A(AMOUNTS HEADER)                            
ACOFH    DS    A                   A(CREDIT OFFICE HEADER)                      
ACOFNH   DS    A                   A(CREDIT OFFICE NAME HEADER)                 
ATYPEH   DS    A                   A(GST TYPE HEADER)                           
ATYPNH   DS    A                   A(GST TYPE NAME FIELD HEADER)                
AGORNH   DS    A                   A(GROSS OR NET HEADER)                       
AGSTXH   DS    A                   A(GST EXTRA DATA HEADER)                     
AGAMTH   DS    A                   A(GST AMOUNTS)                               
APROVH   DS    A                   A(PST PROVINCE)                              
AAOFH    DS    A                   A(ANALYSIS OFFICE HEADER)                    
ADEPH    DS    A                   A(DEPARTMENT HEADER)                         
ASTFH    DS    A                   A(STAFF HEADER)                              
ASTFNH   DS    A                   A(STAFF NAME HEADER)                         
AEXPH    DS    A                   A(EXPENSE HEADER)                            
AEXPNH   DS    A                   A(EXPENSE NAME HEADER)                       
AFOFH    DS    A                   A(FINANCIAL OFFICE HEADER)                   
AFOFNH   DS    A                   A(FINANCIAL OFFICE NAME HEADER)              
ANARH    DS    A                   A(NARRATIVE HEADER)                          
ATOTH    DS    A                   A(TOTAL HEADER)                              
ATOT     DS    A                   A(TOTAL FIELD)                               
*                                                                               
SV3REGS  DS    0F                                                               
SVR2     DS    F                                                                
SVR3     DS    F                                                                
SVR4     DS    F                                                                
SVR6     DS    F                                                                
SVRE     DS    F                                                                
LINES    DS    C                                                                
ELCODE   DS    C                                                                
*                                                                               
CNTNUM   DS    CL15                CONTRACT NUMBER                              
CNTNAME  DS    CL36                CONTRACT NAME                                
*                                                                               
CTRNUM   DS    CL15                CONTRACTOR NUMBER                            
CTRNAME  DS    CL36                CONTRACTOR NAME                              
*                                                                               
SVCSHNUM DS    CL15                SC CASH ACCT #                               
SVCSHNAM DS    CL36                SC CASH ACCT NAME                            
P2CNUM   DS    CL15                2C NUM PROD                                  
P2CNAM   DS    CL36                2C NAME PROD                                 
E2CNUM   DS    CL15                2C NUM EXP                                   
E2CNAM   DS    CL36                2C NAME EXP                                  
PROTROL  DS    CL15                27 NUM PROD                                  
PROTROLN DS    CL36                27 NAME PROD                                 
EXPTROL  DS    CL15                27 NUM EXP                                   
EXPTROLN DS    CL36                27 NAME EXP                                  
SVPRDNUM DS    CL15                PRODUCT ACCT                                 
SVPRDNM  DS    CL36                PRODUCT NAME                                 
SVJOBNUM DS    CL15                JOB ACCT                                     
SVJOBNM  DS    CL36                JOB NAME                                     
CURROFFC DS    CL2                 CURRENT OFFICE                               
AOFFC    DS    A                   ADDRESS OF CURRENT OFFICE                    
PRDLEN   DS    F                   LENGTH OF PRD INPUT                          
JOBLEN   DS    F                   LENGTH OF JOB INPUT                          
PSCLINUM DS    CL15                RETURNED FROM PROFMERGE                      
SVWRKNAM DS    CL35                WORK CODE EXPANSION                          
SAVEEXP  DS    A                   A(CURRENT EXPENSE FIELD HEADER)              
SAVEWC   DS    A                   A(CURRENT WORK CODE FIELD HEADER)            
SAVEAMNT DS    A                   A(CURRENT AMOUNT FIELD HEADER)               
ACURLN   DS    A                   A(CURRENT LINE)                              
PASSCD   DS    CL1                 PASS CD TO CLIENT                            
COMSW    DS    CL1                 COMMISIONABLE WORK CODE INDICATOR            
DFLTSW   DS    CL1                 DEFAULT CREDIT ACCT READ INDICATOR           
GSTSW    DS    CL1                                                              
GRSPOST  DS    PL6                                                              
NETPOST  DS    PL6                                                              
GSTPOST  DS    PL6                                                              
PSTPOST  DS    PL6                                                              
SVAMNT   DS    PL6                                                              
SVWRKCD  DS    CL2                                                              
CURLNNO  DS    XL2                 CURRENT LINE NUMBER                          
CURPSTNO DS    XL1                 CURRENT POST NUMBER                          
*                                                                               
SAVEODS  DS    A                   SAVED A(OFF,DEPT,STAFF HDR)                  
SAVCLPR  DS    A                   SAVED A(CLI,PRO HDR)                         
BLOCK    DS    6CL32               SCANNER AREA OFF,DEPT,STAFF                  
CLIPRO   DS    CL15                CLI,PRO ACCT KEY                             
CLIPRON  DS    CL36                CLI,PRD NAME FOR DISP                        
DEPNUM   DS    CL15                DEPT ACCT KEY                                
DEPNAME  DS    CL36                DEPT NAME                                    
DEPSTFN  DS    CL36                OFF,DEPT,STAFF NAME FOR DISP                 
POSTACC  DS    CL15                COST ACCT KEY                                
POSTACCN DS    CL36                NAME                                         
CRDSNUM  DS    CL15                2/8 ACCT KEY                                 
CRDSNAME DS    CL36                NAME                                         
COSTNUM  DS    CL15                1/C ACCT KEY                                 
COSTNAME DS    CL36                NAME                                         
CRCNUM   DS    CL15                1/P ACCT KEY                                 
CRCNAME  DS    CL36                NAME                                         
CR13NUM  DS    CL15                1/3 ACCT KEY                                 
CR13NAME DS    CL36                NAME                                         
CRPSNUM  DS    CL15                2/9 ACCT KEY                                 
CRPSNAME DS    CL36                NAME                                         
STAFFNUM DS    CL15                2/P ACCT KEY                                 
STAFFNAM DS    CL36                STAFF NAME                                   
OFFICE   DS    CL2                                                              
OFFICEL  DS    XL1                                                              
DEPT     DS    CL4                                                              
DEPTL    DS    XL1                                                              
STAFF    DS    CL7                                                              
STAFFL   DS    XL1                                                              
OFFSW    DS    CL1                                                              
DEPSW    DS    CL1                                                              
STFSW    DS    CL1                                                              
COSTSW   DS    XL1                                                              
COSTACC  EQU   X'80'               COST ACCOUNTING                              
COSTNEW  EQU   X'40'               NEW COSTING                                  
COSTANAL DS    CL5                 COST CODE FROM SE ACCOUNT                    
*                                                                               
LEVEL    DS    XL1                                                              
ODSSW    DS    XL1                                                              
INPUTSW  DS    XL1                                                              
*                                                                               
OFFCBIT  EQU   X'04'               OFFICE BIT                                   
DEPTBIT  EQU   X'02'               DEPARTMENT BIT                               
STFFBIT  EQU   X'01'               STAFF BIT                                    
*                                                                               
SCAN     DS    4CL32               SCANNER AREA CLI,PRO                         
BYTE     DS    CL1                 TENO DIRECTED EXP ACCT IPT LEN               
*                                                                               
SIMDSW   DS    CL1                                                              
SIMDAC   DS    CL15                                                             
SIMDACN  DS    CL36                                                             
*                                                                               
PRGSTAT  DS    CL1                 PROGRAM STATUS                               
PRGPROD  EQU   X'80'               PRODUCTION                                   
PRGXJOB  EQU   X'40'               EXPENSE JOB IS USED                          
PRGNCOMM EQU   X'20'               NON-COMMISIONABLE                            
PRGMDEBT EQU   X'10'               MAIN SCREEN DEBIT USED                       
*                                                                               
ADVSW    DS    CL1                                                              
AGYSW    DS    CL1                                                              
FRSTPASS DS    CL1                                                              
CDSW     DS    CL1                 CD ACCOUNT INDICATOR                         
CDNUM    DS    CL15                CD ACCOUNT NUMBER                            
CDNAME   DS    CL36                CD ACCOUNT NAME                              
DISC     DS    PL3                 CD AMOUNT                                    
DISC2    DS    PL3                 CD AMOUNT 2                                  
*                                                                               
PVENTYPE DS    CL1                                                              
EVENTYPE DS    CL1                                                              
SAVEDATE DS    CL3                                                              
SVDATE1  DS    CL3                                                              
IVCDATE  DS    CL3                 INVOICE DATE                                 
PAYDATE  DS    CL3                 PAYABLE DATE                                 
SECNDSW  DS    CL1                                                              
*                                                                               
COFFICE  DS    CL2                                                              
*                                                                               
WRKCODE  DS    CL2                 WORKCODE                                     
RETRATIO DS    PL4                 RETAIL RATIO                                 
*                                                                               
CSHTOT   DS    PL6                                                              
CDAMNT   DS    PL6                                                              
*                                                                               
VENDOR   DS    CL1                 VENDOR REQUIRED INDICATOR                    
VENDTYPE DS    CL1                 VENDOR'S GST TYPE                            
VENDPSTT DS    CL1                 VENDOR'S PST TYPE                            
VENDPROV DS    CL2                 VENDOR'S PROVINCE, VNDR REC (LFM)            
*                                                                               
TOTESTA  DS    PL6                                                              
TOTUSRA  DS    PL6                                                              
TOTTAXS  DS    PL6                                                              
TOTCALC  DS    PL6                                                              
*                                                                               
*STSW    DS    CL1                 Y=GST APPLICABLE, N=NOT APPLICABLE           
GORN     DS    CL1                 G=GROSS, N=NET                               
NAMTS    DS    XL1                 N'AMOUNT ENTRIES                             
NGST     DS    XL1                 N'GST AMOUNTS INPUT                          
AMTTAB   DS    8XL(AMTLNQ)                                                      
ALARGEST DS    A                                                                
AAMT     DS    A                                                                
APOST    DS    A                                                                
BLDELS   DS    A                                                                
*        SPACE 1                                                                
SVWCNMS  DS    CL((4*(L'ACANDESC-4+2))-2)                                       
TSTWCNMS DS    CL((4*(L'ACANDESC-4+2))-2)                                       
MULTWCSW DS    X         USED FOR PREFIXING ', ' BEFORE MULT WC NAMES           
FSTWC    EQU   B'10000000'                                                      
SAVWLINE DS    X                                                                
SAVALINE DS    X                                                                
THISLINE DS    X                                                                
REFLEN   DS    CL1                                                              
REFSAVE  DS    CL21                                                             
DOCLEN   DS    CL1                                                              
DOCSAVE  DS    CL6                                                              
SVSTAT   DS    CL1                                                              
SVSTAT2  DS    CL1                                                              
PCONSULT DS    CL1                 1099(2C) VENDOR SWITCH PROD VENDOR           
ECONSULT DS    CL1                 1099(2C) VENDOR SWITCH EXP VENDOR            
PRSTAT   DS    CL1                 PROD VENDOR 1099 STATUS                      
EXSTAT   DS    CL1                 EXP VENDOR 1099 STATUS                       
V29SW    DS    CL1                 EXPENSE VENDOR CONTRA                        
POSTEL   DS    CL(DLPSLNQ)         POSTING ELEMENT AREA                         
ORDNOEL  DS    XL10                                                             
MEMO4C   DS    XL(L'ACKEYACC-1+2)                                               
MEMO50   DS    XL(TRCSLNQ1)                                                     
TRSELEM  DS    XL(TRSLNQ)                                                       
LPRO     DS    XL1                                                              
FINOFF   DS    CL2                 FINANCIAL OFFICE                             
*                                                                               
GSTVATBL DS    CL(VTCLNQ)          GST VATICAN BLOCK                            
PSTVATBL DS    CL(VTCLNQ)          PST VATICAN BLOCK                            
*                                                                               
EXCWORK  DS    0D                                                               
DTLSCRN  EQU   X'B9'               DETAIL SCREEN                                
ZOOMSCRN EQU   X'B8'               ZOOM SCREEN                                  
CATBLK   DS    CL(CATLNQ)          CATEGORY BLOCK                               
         DS    CL(EXCELNQ)                                                      
ORAMOUNT DS    PL6                                                              
ORINVTDT DS    PL6                                                              
*                                                                               
DICO     DS    0C                                                               
         DSDDL PRINT=YES                                                        
         DS    XL1                                                              
         EJECT                                                                  
       ++INCLUDE ACBATSTAX                                                      
       ++INCLUDE ACBATCTAX                                                      
*                                                                               
TMPMODE  DS    CL1                 TEMPORARY MODE                               
PVBLK    DS    CL(L'PVALOUTB)      PERVAL BLOCK                                 
TMPWC    DS    CL4                                                              
TMPCPJ   DS    CL14                                                             
TMPODS   DS    CL19                                                             
TMPEXP   DS    CL14                                                             
TMPAMT   DS    CL11                                                             
*                                                                               
KEY      DS    CL49                                                             
*                                                                               
IOAREA   DS    3000C                                                            
PROGDX   DS    0C                                                               
         EJECT                                                                  
SPOOLD   DSECT                     ONLINE PRINT CONTROL DSECT                   
SPLAREA  DS    CL4000                                                           
         ORG   SPLAREA                                                          
         DS    D                                                                
LINE     DS    XL1                 PRESET TO 99                                 
ALLOWLIN DS    XL1                 ENSURE THAT N LINES REMAIN ON PAGE           
MAXLINES DS    XL1                 PRESET TO 60                                 
SPACING  DS    XL1                                                              
HEADHOOK DS    V                   USER SUPPLIED A(HEADLINE ROUTINE)            
MIDHOOK  DS    V                   USER SUPPLIED A(MIDLINE ROUTINE)             
CLEARHED DS    CL1                 OPTION TO CLEAR HEADLINES DEFAULT=Y          
FORCEHED DS    CL1                                                              
FORCEMID DS    CL1                                                              
FORCEFUT DS    CL1                                                              
FORCECLR DS    CL1                                                              
SKIPSPEC DS    CL1                                                              
PAGE     DS    XL2                                                              
SUBPAGE  DS    XL2                                                              
SPOOLIND DS    XL1                                                              
SPNSPACE EQU   X'80'               NO SPACE AFTER HEADLINES                     
SPNGAPS  EQU   SPNSPACE+X'20'      NO GAPS BETEEN HEADS AND MIDS                
SPUINIT  EQU   X'40'               ALLOW USER INITIALIZED FIELDS                
SPHHOOK  EQU   X'04'               APPLICATION CALLED WITH HEADHOOK             
SPMHOOK  EQU   X'02'               APPLICATION CALLED WITH MIDHOOK              
SPFHOOK  EQU   X'01'               APPLICATION CALLED WITH FOOTHOOK             
*                                                                               
         DS    XL1                                                              
         SPACE 1                                                                
SPOOLKEY DS    CL48                                                             
SPOOLPAG DS    H                                                                
SPOOLLIN DS    H                                                                
SPOOLDM  DS    A                                                                
SPOOLBUF DS    A                                                                
SPECS    DS    A                                                                
SPOOLID  DS    CL3                                                              
SPOOLRPN DS    CL2                                                              
SPMODE   DS    CL1                                                              
ACTPAGES DS    H                                                                
MAXPAGES DS    H                                                                
RCDATE   DS    CL8                                                              
RCPROG   DS    CL4                                                              
RCSUBPRG DS    CL1                                                              
SPCONSYS DS    CL1                                                              
RCDATCON DS    A                                                                
VPRINT   DS    A                                                                
BUFFALO  DS    V                                                                
SORTER   DS    V                                                                
WORKER   DS    V                                                                
ABOX     DS    V                                                                
FOOTLNS  DS    X                   NUMBER OF FOOTLINES REQUIRED                 
         ORG   FOOTLNS                                                          
FOOTHOOK DS    V                                                                
SPOOLQLK DS    A                   A(EXTENDED SPOOL KEY) (128 BYTES)            
*                                  FIRST BYTE OF SPOOLQLK MUST BE 0             
RCCOMFAC DS    V                                                                
SPOTPROF DS    CL16                                                             
         DS    CL16                                                             
         SPACE 1                                                                
         DS    D                   HEADLINES                                    
HEAD1    DS    CL132                                                            
HEAD2    DS    CL132                                                            
HEAD3    DS    CL132                                                            
HEAD4    DS    CL132                                                            
HEAD5    DS    CL132                                                            
HEAD6    DS    CL132                                                            
HEAD7    DS    CL132                                                            
HEAD8    DS    CL132                                                            
HEAD9    DS    CL132                                                            
HEAD10   DS    CL132                                                            
HEAD11   DS    CL132                                                            
HEAD12   DS    CL132                                                            
HEAD13   DS    CL132                                                            
HEAD14   DS    CL132                                                            
H1       EQU   HEAD1                                                            
H2       EQU   HEAD2                                                            
H3       EQU   HEAD3                                                            
H4       EQU   HEAD4                                                            
H5       EQU   HEAD5                                                            
H6       EQU   HEAD6                                                            
H7       EQU   HEAD7                                                            
H8       EQU   HEAD8                                                            
H9       EQU   HEAD9                                                            
H10      EQU   HEAD10                                                           
H11      EQU   HEAD11                                                           
H12      EQU   HEAD12                                                           
H13      EQU   HEAD13                                                           
H14      EQU   HEAD14                                                           
         SPACE 1                                                                
         DS    CL8                 MID LINES                                    
MID1     DS    CL132                                                            
MID2     DS    CL132                                                            
         DS    CL8                 PRINT LINES                                  
P        DS    0CL132                                                           
P1       DS    CL132                                                            
P2       DS    CL132                                                            
P3       DS    CL132                                                            
P4       DS    CL132                                                            
         DS    CL132               SPACES FIELD                                 
MONTHS   DS    CL36                MONTH TABLE (JAN-DEC)                        
DAYTABL  DS    CL21                DAY TABLE (MON-SUN)                          
USERNAME DS    CL33                                                             
USERADDR DS    CL33                                                             
USERQSTR DS    CL6                                                              
USERQEND DS    CL6                                                              
USERPROF DS    CL16                                                             
USERLANG DS    XL1                                                              
         DS    CL1                                                              
SPOOLEND DS    D                                                                
         EJECT                                                                  
WCTABD   DSECT                                                                  
WCTWC    DS    CL2                 WORKCODE                                     
WCTWCS   DS    XL1                 WORKCODE STATUS                              
WCTWCNC  EQU   X'80'               WORKCODE IS NON-COMMISIONABLE                
WCTAMT   DS    PL6                 AMOUNT                                       
WCTABLNQ EQU   *-WCTABD                                                         
         SPACE 3                                                                
*--------------------------------------------------------------                 
*        CURRENT DETAIL LINE DSECT                                              
*--------------------------------------------------------------                 
*                                                                               
CURLINED DSECT                                                                  
CURSUBH  DS    CL8                 SUBACTION                                    
CURSUB   DS    CL(L'IVDSUB)                                                     
         DS    CL8                                                              
CURWCH   DS    CL8                 WORKCODE                                     
CURWC    DS    CL(L'IVDWC)                                                      
         DS    CL8                                                              
CURCPJH  DS    CL8                 C/P/J OR EXPENSE ACCT                        
CURCPJ   DS    CL(L'IVDCPJ)                                                     
         DS    CL8                                                              
CURODSH  DS    CL8                 OFFICE/DEPT/STAFF/ANOF                       
CURODS   DS    CL(L'IVDODS)                                                     
         DS    CL8                                                              
CUREXPH  DS    CL8                 EXPENSE ACCT OR C/P/J                        
CUREXP   DS    CL(L'IVDEXP)                                                     
         DS    CL8                                                              
CURAMTH  DS    CL8                 AMOUNT                                       
CURAMT   DS    CL(L'IVDAMT)                                                     
         DS    CL8                                                              
CURLLNQ  EQU   *-CURLINED                                                       
         EJECT                                                                  
DTABD    DSECT                     DEBIT TABLE DSECT                            
DTABWC   DS    CL2                 WORKCODE                                     
DTABWCS  DS    XL1                 WORKCODE STATUS                              
DTABWCCM EQU   X'80'               COMMISSION                                   
DTABCPJ  DS    CL14                CLIENT/PRODUCT/JOB                           
DTABEXP  DS    CL14                EXPENSE ACCOUNT                              
DTABODS  DS    CL14                OFFICE/DEPT/STAFF                            
DTABAMT  DS    PL6                 DEBIT AMOUNT                                 
         DS    CL20                SPARE                                        
DTABLNQ  EQU   *-DTABD                                                          
         EJECT                                                                  
*--------------------------------------------------------------                 
*        KEY DSECT                                                              
*--------------------------------------------------------------                 
*                                                                               
KEYD     DSECT                                                                  
KVENNUM  DS    CL15                                                             
         DS    CL17                                                             
KDATE    DS    XL3                                                              
KDOC     DS    CL6                                                              
*                                                                               
KEYSAVED DSECT                                                                  
KSVENNUM DS    CL15                                                             
         DS    CL17                                                             
KSDATE   DS    XL3                                                              
KSDOC    DS    CL6                                                              
         SPACE 2                                                                
VALD     DSECT                                                                  
VALWRKH  DS    CL8                                                              
VALWRK   DS    CL11                                                             
         DS    CL8                                                              
VALWNMH  DS    CL8                                                              
VALWNM   DS    CL49                                                             
         DS    CL8                                                              
         DS    CL17                                                             
VALAMTH  DS    CL8                                                              
VALAMT   DS    CL40                                                             
         SPACE 2                                                                
* DSECT TO COVER WORKCODE AMOUNT TABLE                                          
*                                                                               
AMTD     DSECT                                                                  
AMTWC    DS    CL2                 WORKCODE                                     
AMTNET   DS    PL6                 NET                                          
AMTGRS   DS    PL6                 GROSS                                        
AMTGST   DS    PL6                 GST                                          
AMTLNQ   EQU   *-AMTD                                                           
         EJECT                                                                  
       ++INCLUDE ACBATDSECT                                                     
         EJECT                                                                  
*--------------------------------------------------------------                 
*        ACBATBBD - MAIN SCREEN DSECT                                           
*--------------------------------------------------------------                 
*                                                                               
       ++INCLUDE ACBATBBD                                                       
         EJECT                                                                  
         ORG   OSVALS                                                           
*                                                                               
USERAREA DS    0F                                                               
TAXAMT   DS    PL6                                                              
USERPROV DS    CL1                                                              
*                                                                               
SVPVEN   DS    CL15                PROD VENDOR KEY                              
SVEVEN   DS    CL15                EXP  VENDOR KEY                              
SVPVENNM DS    CL36                PROD VENDOR NAME                             
SVEVENNM DS    CL36                EXP  VENDOR NAME                             
VENDORPA DS    PL7                 PRODUCTION VENDOR AMOUNT                     
VENDORXA DS    PL7                 EXPENSE VENDOR AMOUNT                        
CNTRACC  DS    CL15                CONTRACTOR                                   
CNTRNAME DS    CL36                CONTRACTOR NAME                              
CNTRADD1 DS    CL(L'ADRADD1)       CONTRACTOR ADDRESS LINE 1                    
CNTRADD2 DS    CL(L'ADRADD2)       CONTRACTOR ADDRESS LINE 2                    
CNTRADD3 DS    CL(L'ADRADD3)       CONTRACTOR ADDRESS LINE 3                    
CNTRADD4 DS    CL(L'ADRADD4)       CONTRACTOR ADDRESS LINE 4                    
SVEXPNUM DS    CL15                EXPENSE ACCOUNT                              
SVEXPNM  DS    CL36                EXPENSE ACCOUNT NAME                         
INVREF   DS    CL21                INVOICE REFERENCE                            
INVDTE   DS    PL3                 INVOICE DATE                                 
INVDDTE  DS    PL3                 INVOICE DUE DATE                             
INVORDNO DS    CL6                 ORDER NUMBER                                 
DETLREF  DS    CL6                 DETAIL REFERENCE                             
CRDTOT   DS    PL7                 CREDIT SCREEN TOTAL                          
DBTTOT   DS    PL7                 DEBIT SCREEN TOTAL                           
GSTAMT   DS    PL7                 GST OVERRIDE AMOUNT                          
PSTAMT   DS    PL7                 PST OVERRIDE AMOUNT                          
TOTDETS  DS    PL7                 TOTAL DEBITS                                 
GROUPNO  DS    XL4                 GROUP INVOICE NUMBER                         
SVOFFICE DS    CL2                 SAVED OFFICE                                 
SVCLINUM DS    CL15                CLIENT ACCT                                  
SVCLINM  DS    CL36                CLIENT NAME                                  
SVCLIOFF DS    CL2                 CLIENT OFFICE                                
CRNARR   DS    CL64                CREDIT NARRATIVE                             
CRNARRLN DS    X                   LENGTH OF CREDIT NARRATIVE                   
ORDCPJ   DS    CL15                ORDER'S CLIENT/PRODUCT/JOB                   
WCTABLE  DS    CL72                WORKCODE TABLE FOR ORDERS ONLY               
         DS    XL1                 EOT                                          
VENDAMT  DS    PL6                 VENDOR AMOUNT                                
SVPOSTLN DS    X                   LENGTH OF SAVEPOST                           
SAVEPOST DS    CL256               SAVE POSTING FOR CREDIT                      
*                                                                               
         EJECT                                                                  
*--------------------------------------------------------------                 
*        ACBATBAD - CANADIAN SCREEN DSECT                                       
*--------------------------------------------------------------                 
*                                                                               
         ORG   CONTABH                                                          
       ++INCLUDE ACBATBAD                                                       
         EJECT                                                                  
*--------------------------------------------------------------                 
*        ACBATB9D - DETAIL SCREEN DSECT                                         
*--------------------------------------------------------------                 
*                                                                               
         ORG   CONTABH                                                          
       ++INCLUDE ACBATB9D                                                       
         EJECT                                                                  
*--------------------------------------------------------------                 
*        ACBATB8D - ZOOM DETAIL SCREEN DSECT                                    
*--------------------------------------------------------------                 
*                                                                               
         ORG   CONTABH                                                          
       ++INCLUDE ACBATB8D                                                       
         EJECT                                                                  
*ACEXCELD                                                                       
       ++INCLUDE ACEXCELD                                                       
         EJECT                                                                  
*ACGENBOTH                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
*ACGENDAY                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACGENDAY                                                       
         PRINT ON                                                               
* DDFLDHDR                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDFLDHDR                                                       
         PRINT ON                                                               
*DDFLDIND                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDFLDIND                                                       
         PRINT ON                                                               
*DDSCANBLKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
*ACCATCALLD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACCATCALLD                                                     
         PRINT ON                                                               
*ACDDEQUS                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'074ACBAT48   12/04/12'                                      
         END                                                                    
