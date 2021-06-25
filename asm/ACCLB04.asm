*          DATA SET ACCLB04    AT LEVEL 022 AS OF 08/16/00                      
*PHASE T62104A                                                                  
CLB04    TITLE '- BILL PROGRAM - FORMAT ACTIVITY INTO BILL RECORD'              
CLB04    CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL 0,**CLB4**,R8,R7,R6,CLEAR=YES,RR=RE                              
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         USING TWAD,RA             RA=A(TWA)                                    
         L     RC,AOVERWRK                                                      
         USING OVERWRK,RC                                                       
         ST    RE,BORELO                                                        
         LA    R5,OSVALS                                                        
         USING OSVALSD,R5                                                       
*                                                                               
         USING ACTRECD,R2                                                       
INI00    MVC   IODAOVER,BCJOBDA    READ JOB FOR ALLOCATION TOTALS               
         GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,AGOPBLK          BILLING EXTENSION REQUIRED                   
         MVC   GOABEXT-GOBLOCKD(L'GOABEXT,RE),AJOBBLK                           
         GOTO1 AGETOPT,BODMCB,AIO1                                              
*                                                                               
         L     R2,AIO1                                                          
         LA    R3,ACTRFST                                                       
         SR    R0,R0                                                            
         USING SCIEL,R3                                                         
INI10    CLI   SCIEL,0                                                          
         BE    INIERR              NO ALLOCATION                                
         CLI   SCIEL,SCIELQ                                                     
         BE    INI30                                                            
INI20    IC    R0,SCILN                                                         
         AR    R3,R0                                                            
         B     INI10                                                            
INI30    CLI   SCITYPE,SCITCBAP                                                 
         BNE   INI20                                                            
         ZAP   FJOBNET,SCIAMNT     NET ALLOCATION                               
         ZAP   FJOBCOM,SCIADMN     COMMISSION ALLOCATED                         
         BNZ   INI40                                                            
         ZAP   FJOBNET,FJOBNET                                                  
         BZ    INIERR              NO ALLOCATION                                
*                                                                               
INI40    LA    R2,IOKEY            READ FORMAT CONTROL RECORD                   
         USING PBCRECD,R2                                                       
         XC    PBCKEY,PBCKEY                                                    
         MVI   PBCKTYP,PBCKTYPQ                                                 
         MVC   PBCKCPY,CUABIN                                                   
         MVI   PBCKSUB,PBCKCONQ                                                 
         MVC   PBCKFMT,CSFORMAT                                                 
*        MVC   PBCKLANG,GOBILLNG   BILL LANGUAGE MAY BE SET IN OPT MNT          
         MVI   PBCKLANG,0          ** TEMP **                                   
         GOTO1 AIO,IO3+IOACCMST+IORD                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO3                                                          
         MVC   FBILGRPB,PBCRGRPB   SAVE GROUPING BASIS                          
         MVI   FHRSPARA,1          SET DEFAULT XSORT KEY FOR TIME PARA          
         MVI   FOOPPARA,1          SET DEFAULT XSORT KEY FOR OOPS PARA          
         MVI   FSORTING,C'Y'       PARA LINES ADDED OUT OF SEQUENCE             
         CLI   FBILGRPB,PBCKWCQ    TEST WORKCODE PER PARAGRAPH                  
         BE    *+12                                                             
         CLI   FBILGRPB,PBCKTRNQ   TEST TRANSACTION PER PARAGRAPH               
         BNE   HDR00                                                            
         MVI   FSORTING,C'N'       PARA LINES ARE ADDED IN SEQUENCE             
         TM    PBCRSTA2,PBCSHRSF   TEST IF HOURS COME FIRST                     
         BNO   *+12                                                             
         MVI   FOOPPARA,X'FF'      SET OOP PARA KEY HIGH                        
         B     HDR00                                                            
         TM    PBCRSTA2,PBCSOOPF   TEST IF OOP PARAS COME FIRST                 
         BNO   HDR00                                                            
         MVI   FHRSPARA,X'FF'      SET TIME PARA KEY HIGH                       
         B     HDR00                                                            
*                                                                               
INIERR   B     INIE010                                                          
*                                                                               
INIE010  MVC   FVMSGNO,=AL2(AE$NACOJ)                                           
         B     EXITN                                                            
         DROP  R2                                                               
         EJECT                                                                  
**********************************************************************          
*        ADD A NEW BILL HEADER RECORD                                *          
*                                                                    *          
**********************************************************************          
         SPACE 1                                                                
HDR00    LA    R2,IOKEY            PROD LEDGER FOR NEXT DRAFT BILL              
         USING LDGRECD,R2                                                       
         MVC   LDGKEY,BCSPACES                                                  
         MVC   LDGKCPY,CUABIN      COMPANY                                      
         MVC   LDGKUNT(L'BCCPYPRD),BCCPYPRD PRODUCTION LEDGER                   
         GOTO1 AIO,IO2+IOACCMST+IORDUP                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO2                                                          
         LA    R3,LDGRFST                                                       
         SR    RE,RE                                                            
         USING SCIELD,R3                                                        
HDR20    CLI   SCIEL,0             END OF RECORD                                
         BE    HDR40                                                            
         CLI   SCIEL,SCIELQ                                                     
         BE    HDR50                                                            
HDR30    IC    RE,SCILN                                                         
         AR    R3,RE                                                            
         B     HDR20                                                            
*                                                                               
HDR40    LA    R3,BOELEM           ADD A DRAFT BILL NUMBER ELEMENT              
         XC    SCIEL(SCILN1Q),SCIEL                                             
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCITYPE,SCITDBNO                                                 
         MVI   SCILN,SCILN1Q                                                    
         ZAP   SCIAMNT,=P'0'                                                    
         GOTO1 VHELLO,BCDMCB,(C'P',ACCMST),AIO2,(R3),0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         ICM   R3,15,16(R1)        R3=A(NEW ELEMENT)                            
         B     *+12                                                             
*                                                                               
HDR50    CLI   SCITYPE,SCITDBNO    DRAFT BILL NUMBER SEQ TYPE?                  
         BNE   HDR30                                                            
         AP    SCIAMNT,=P'1'       SET NEXT DRAFT BILL NUMBER                   
         UNPK  BCDUB,SCIAMNT                                                    
         MVC   FBILLNO,BCDUB+2                                                  
         OI    FBILLNO+L'FBILLNO-1,X'F0'                                        
*                                                                               
HDR60    LA    R2,IOKEY            READ FOR DRAFT BILL                          
         USING PBRRECD,R2                                                       
         XC    PBRPAS,PBRPAS                                                    
         MVI   PBRPTYP,PBRPTYPQ                                                 
         MVC   PBRPCPY,CUABIN                                                   
         MVI   PBRPSUB,PBRPPASQ    PASSIVE RECORD                               
         MVC   PBRPBLNO,FBILLNO    BILL NUMBER                                  
         MVI   PBRPIND,PBRPIDFT    DRAFT BILL                                   
         GOTO1 AIO,IO1+IOACCDIR+IOHIGH                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   PBRPAS(PBRPUSER-PBRPAS),IOKEYSAV                                 
         BNE   HDR150                                                           
         CP    SCIAMNT,=P'999999'  RECORD EXISTS - TRY AGAIN                    
         BNE   HDR50                                                            
         ZAP   SCIAMNT,=P'0'       RESTART DRAFT SEQUENCE                       
         B     HDR50                                                            
*                                  WRITE BACK UPDATED LEDGER RECORD             
HDR150   GOTO1 AIO,IO2+IOACCMST+IOPUTREC                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
HDR160   LA    R2,IOKEY                                                         
         USING PBRRECD,R2                                                       
         XC    PBRKEY,PBRKEY                                                    
         MVI   PBRKTYP,PBRKTYPQ                                                 
         MVC   PBRKCPY,CUABIN      COMPANY CODE                                 
         MVI   PBRKSUB,PBRKACTQ    ACTIVE RECORD                                
         MVC   PBRKJOB,BCJOBCOD    JOB CODE                                     
         GOTO1 AIO,IO1+IOACCDIR+IOHIGH GET BILL RECORD                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LH    RE,=XL2'FFFF'                                                    
         CLC   PBRKEY(PBRKSEQ-PBRKEY),IOKEYSAV RECORD FOUND?                    
         BNE   HDR170                                                           
         SR    RE,RE                                                            
         ICM   RE,3,PBRKSEQ                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         BCTR  RE,0                                                             
HDR170   L     R2,AIO1                                                          
         MVC   PBRKEY,IOKEYSAV                                                  
         XC    PBRRSTA,PBRRSTA                                                  
         STCM  RE,3,PBRKSEQ                                                     
         MVC   FPBRKEY,PBRKEY      SAVE HEADER KEY                              
         LA    RE,PBRRFST                                                       
         MVI   0(RE),0                                                          
         LA    RE,1(RE)                                                         
         SR    RE,R2                                                            
         STCM  RE,3,PBRRLEN                                                     
         LA    R3,BOELEM                                                        
         USING BLHELD,R3                                                        
         XC    BOELEM,BOELEM                                                    
         MVI   BLHEL,BLHELQ                                                     
         MVI   BLHLN,BLHLNQ                                                     
         MVC   BLHJOB,BCJOBCOD                                                  
         MVC   BLHBLNO,FBILLNO                                                  
         MVC   BLHUSER,CUUSER                                                   
         MVC   BLHPERS,CUPASS                                                   
         MVC   BLHCRED,ASCDAT                                                   
         MVC   BLHFORM,CSFORMAT                                                 
         CLC   CSBILCUR,CSCPYCUR   TEST FOREIGN CURRENCY                        
         BE    *+16                                                             
         MVC   BLHCUR,CSBILCUR     SET CURRENCY CODE IN BILL                    
         MVC   BLHRVAL,CSEXCVAL    SET EXCHANGE RATE                            
         L     RF,AGOPBLK                                                       
         L     RF,GOABEXT-GOBLOCK(RF)                                           
         ZAP   BLHDSC,GODSCPCT-GOBBLOCK(L'GODSCPCT,RF)                          
         ZAP   BLHSCH,GOSRGPCT-GOBBLOCK(L'GOSRGPCT,RF)                          
         GOTO1 VDATCON,BCDMCB,(2,BCTODAYC),(0,BOWORK1)                          
         SR    RF,RF                                                            
         ICM   RF,1,BCP101         GET NON-STANDARD RETENTION DAYS              
         BNZ   *+8                                                              
         LA    RF,7                DEFAULT IS 7                                 
         GOTO1 VADDAY,BCDMCB,BOWORK1,BOWORK1+6,(RF)                             
         GOTO1 VDATCON,BCDMCB,(0,BOWORK1+6),(2,BLHEXPD)                         
         GOTO1 VHELLO,BCDMCB,(C'P',ACCMST),AIO1,(R3),0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R3,BOELEM                                                        
         USING NDXELD,R3                                                        
         XC    BOELEM,BOELEM                                                    
         MVI   NDXEL,NDXELQ                                                     
         MVI   NDXLN,NDXLNQ                                                     
         MVI   NDXHIGH,1                                                        
         MVI   NDXACTV,1                                                        
         MVI   NDXINDX,1                                                        
         GOTO1 VHELLO,BCDMCB,(C'P',ACCMST),AIO1,(R3),0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIO,IO1+IOACCMST+IOADDREC                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,IOKEY                                                         
         USING PBRRECD,R2                                                       
         XC    PBRPAS,PBRPAS                                                    
         XC    PBRKSTA,PBRKSTA                                                  
         MVI   PBRPTYP,PBRPTYPQ    RECORD TYPE                                  
         MVC   PBRPCPY,CUABIN      COMPANY CODE                                 
         MVI   PBRPSUB,PBRPPASQ    PASSIVE RECORD                               
         MVC   PBRPBLNO,FBILLNO    BILL NUMBER                                  
         MVI   PBRPIND,PBRPIDFT    DRAFT BILL                                   
         MVC   PBRPUSER,CUUSER     USER-ID                                      
         MVC   PBRPJOB,BCJOBCOD    JOB                                          
         MVC   PBRPCRED,ASCDAT     CREATED DATE                                 
         MVC   PBRPFORM,CSFORMAT                                                
         MVC   PBRPPERS,CUPASS     PERSON CODE                                  
         MVC   PBRKDA,IODA         DISK ADDRESS                                 
         MVC   FHDRDA,IODA         SAVE HEADER DISK ADDRESS                     
         GOTO1 AIO,IO1+IOACCDIR+IOADDREC                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
**********************************************************************          
*        READ JOB TRANSACTIONS AND ADD PARAGRAPH/LINE RECORDS        *          
*                                                                    *          
**********************************************************************          
         SPACE 1                                                                
         USING TRNRECD,R2                                                       
TXT010   MVI   FJOBOVER,C'N'                                                    
         ZAP   FPARNET,=P'0'                                                    
         ZAP   FPARCOM,=P'0'                                                    
         L     RE,AIO5             RE=A(D/A BUFFER)                             
         LA    R4,L'TRNKDA(RE)     R4=A(FIRST D/A)                              
         ST    R4,0(RE)                                                         
         LA    R0,MAXADDRS         R0=MAX COUNT OF SAVED D/A S                  
         XC    0(L'TRNKDA,R4),0(R4)                                             
         LA    R2,IOKEY                                                         
         MVC   TRNKEY,BCSPACES                                                  
         MVC   TRNKCPY,CUABIN                                                   
         MVC   TRNKUNT(L'BCCPYPRD),BCCPYPRD                                     
         MVC   TRNKACT,BCJOBCOD                                                 
         LA    R1,IOHI+IOACCDIR+IO1                                             
         B     *+8                                                              
TXT012   LA    R1,IOSQ+IOACCDIR+IO1                                             
         GOTO1 AIO,(R1)                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   TRNKCULA,IOKEYSAV+TRNKCULA-TRNKEY                                
         BNE   TXT020              FINISHED                                     
         CLC   TRNKWORK,=C'99'                                                  
         BE    TXT020              FINISHED                                     
         CLC   TRNKREF,BCSPACES                                                 
         BNH   TXT014              SAVE CONTRA HEADER                           
         TM    TRNKSTA2,TRNSBILP                                                
         BNO   TXT012                                                           
TXT014   MVC   0(L'TRNKDA,R4),TRNKDA                                            
         LA    R4,L'TRNKDA(R4)                                                  
         XC    0(L'TRNKDA,R4),0(R4)                                             
         BCT   R0,TXT012                                                        
         B     TXT020              BUFFER FULL - PROCESS WHAT WE HAVE           
*                                                                               
TXT020   L     RE,AIO5             RE=A(D/A BUFFER)                             
         L     RF,0(RE)            RF=A(NEXT D/A)                               
         OC    0(L'TRNKDA,RF),0(RF)                                             
         BNZ   TXT022              NO MORE SAVED D/A                            
*                                                                               
         MVI   FJOBOVER,C'Y'       NEW JOB - ADD LAST PARA IF PENDING           
         CLI   FBILGRPB,PBCKWCQ    TEST WORKCODE PER PARAGRAPH                  
         BE    TXT032              PARAGRAPH PENDING                            
         CLI   FBILGRPB,PBCKTRNQ   TEST TRANSACTION PER PARAGRAPH               
         BE    TXT032              PARAGRAPH PENDING                            
         B     TXT090              NO PENDING PARAGRAPHS                        
*                                                                               
TXT022   MVC   IODAOVER,0(RF)                                                   
         LA    RF,L'TRNKDA(RF)                                                  
         ST    RF,0(RE)            SAVE A(NEXT D/A)                             
         GOTO1 AIO,IOACCMST+IOGET+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,AIO1                                                          
         CLI   TRNRFST,TRNELQ                                                   
         BE    TXT028              TRANSACTION                                  
         CLI   TRNRFST,CACELQ                                                   
         BNE   TXT020              MUST BE A STRANGE RECORD                     
         MVC   FCTRNAM,BCSPACES                                                 
         SR    RF,RF               SAVE CONTRA ACCOUNT NAME                     
         IC    RF,TRNRFST+CACLN-CACEL                                           
         SH    RF,=Y(CACLN1Q+1)                                                 
         BNP   TXT020                                                           
         EX    RF,*+8                                                           
         B     TXT020                                                           
         MVC   FCTRNAM(0),TRNRFST+CACNAME-CACEL                                 
TXT028   SR    R0,R0                                                            
         CLC   CSCPYCUR,CSBILCUR   CALL PRORATA FOR ALLOCATED AMOUNTS           
         BE    *+8                                                              
         LA    R0,CSEXCVAL                                                      
         GOTO1 VPRORATA,BODMCB,AIO1,AGOPBLK,ACOM,(R0),PRATBLK                   
         LA    RE,PP$AALLO                                                      
         CLC   CSBILCUR,CSCPYCUR                                                
         BE    *+8                                                              
         LA    RE,PP$FALLO                                                      
         CP    0(L'PP$AALLO,RE),=P'0'                                           
         BNE   TXT030                                                           
         CP    L'PP$AALLO(L'PP$AALLO,RE),=P'0'                                  
         BE    TXT020              NOTHING PENDING - GET NEXT                   
*                                                                               
TXT030   ZAP   FTRNNET,0(L'PP$AALLO,RE)                                         
         ZAP   FTRNCOM,L'PP$AALLO(L'PP$AALLO,RE)                                
*                                                                               
TXT032   L     R2,AIO2                                                          
         USING PBRRECD,R2                                                       
         MVC   PBRKEY,FPBRKEY      TAKE KEY OF PREVIOUS LINE RECORD             
         CLI   FJOBOVER,C'Y'                                                    
         BE    TXT070                                                           
         CLI   FBILGRPB,PBCKTRNQ                                                
         BE    TXT070              NEW PARAGRAPH EVERY TRANSACTION              
*                                                                               
         L     RF,AIO1             RF=A(TRANSACTION)                            
         CLI   FBILGRPB,PBCKWCQ    TEST WORKCODE PER PARAGRAPH                  
         BNE   TXT034                                                           
         CLC   FTRNWC,TRNKWORK-TRNRECD(RF)                                      
         BE    TXT080              SAME WORKCODE - SAME PARAGRAPH               
         B     TXT070              NEW PARAGRAPH                                
*                                                                               
TXT034   BAS   RE,RDWCODE          READ WORKCODE RECORD INTO IO4                
         L     R4,AIO4                                                          
         LA    R4,WCORFST-WCORECD(R4)                                           
         MVC   FTRNWG,WCOGRP-WCOELD(R4)                                         
         MVI   FTRNHTYP,PGHHCSTQ   SET NEW PARAGRAPH TYPE=COST                  
         TM    WCOSTAT-WCOELD(R4),WCOSHCOE                                      
         BNO   *+8                                                              
         MVI   FTRNHTYP,PGHHHRSQ   NEW PARAGRAPH TYPE=HOURS                     
         CLI   FBILGRPB,PBCKUSEQ   TEST USER SPECIFIED PARAGRAPH                
         BNE   TXT040                                                           
         LA    RF,TRNRFST-TRNRECD(RF)                                           
         USING PTAEL,RF                                                         
         SR    R0,R0                                                            
TXT036   CLI   PTAEL,0             FIND THE PENDING PTAEL                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   PTAEL,PTAELQ                                                     
         BE    TXT038                                                           
TXT037   IC    R0,PTALN                                                         
         AR    RF,R0                                                            
         B     TXT036                                                           
TXT038   CLI   PTATYPE,PTATRAL                                                  
         BNE   TXT037                                                           
         TM    PTASTAT1,PTASPEND                                                
         BNO   TXT037                                                           
         MVC   FPARCODE,PTARCODE                                                
         B     TXT042                                                           
*                                                                               
TXT040   L     R4,AIO4             SET R4 TO A(WCODE ELEMENT)                   
         LA    R4,WCORFST-WCORECD(R4)                                           
         CLI   FBILGRPB,PBCKWGQ    TEST IF PARAGRAPH BY WORKGROUP               
         BNE   TXT046                                                           
         MVC   FPARCODE,WCOGRP-WCOELD(R4)                                       
TXT042   CLI   FPARCODE,0                                                       
         BNE   TXT050                                                           
         MVI   FPARCODE,1          IF NO CODE SET FIRST PARA                    
         B     TXT050                                                           
*                                                                               
TXT046   CLI   FBILGRPB,PBCKHRSQ                                                
         BE    *+6                                                              
         DC    H'0'                UNKNOWN SORT GROUPING                        
         MVI   FPARCODE,X'01'      SET HOURS                                    
         TM    WCOSTAT-WCOELD(R4),WCOSHCOE                                      
         BO    *+8                                                              
         MVI   FPARCODE,X'02'      SET OUT-OF-POCKET                            
*                                                                               
TXT050   BAS   RE,GETPARA          SET PARA/LINE KEY                            
         B     TXT080                                                           
*                                                                               
TXT070   CLI   PBRKPARA,0          NO PARAGRAPHS YET                            
         BE    TXT072                                                           
         BAS   RE,ADDPARA                                                       
*                                                                               
TXT072   CLI   FJOBOVER,C'Y'       TEST IF CHANGE OF JOB DETECTED               
         BE    TXT090              YES - ALL OVER                               
         ZAP   FPARNET,=P'0'       SET UP FOR NEW PARAGRAPH                     
         ZAP   FPARCOM,=P'0'                                                    
         L     R4,AIO4                                                          
         L     RF,AIO1                                                          
         BAS   RE,RDWCODE          ENSURE WORKCODE RECORD IS READ               
         L     R4,AIO4                                                          
         LA    R4,WCORFST-WCORECD(R4)                                           
         MVC   FTRNWG,WCOGRP-WCOELD(R4)                                         
         MVI   FTRNHTYP,PGHHCSTQ   SET NEW PARAGRAPH TYPE=COST                  
         TM    WCOSTAT-WCOELD(R4),WCOSHCOE                                      
         BNO   *+8                                                              
         MVI   FTRNHTYP,PGHHHRSQ   NEW PARAGRAPH TYPE=HOURS                     
         MVI   PBRKLINE,0                                                       
         SR    RE,RE                                                            
         IC    RE,PBRKPARA                                                      
         LA    RE,1(RE)                                                         
         CH    RE,=Y(MAXPARAQ)                                                  
         BNH   *+6                                                              
         DC    H'0'                MAXIMUM PARAGRAPHS EXCEEDED                  
         STC   RE,PBRKPARA                                                      
*                                                                               
TXT080   AP    FPARNET,FTRNNET                                                  
         AP    FPARCOM,FTRNCOM                                                  
         BAS   RE,FORMAT           FORMAT THIS TRANSACTION                      
         CLI   FTXTNDX,0           TEST ANY LINES USED                          
         BNE   TXT081                                                           
         L     R2,AIO2                                                          
         CLI   PBRKLINE,0          TEST ANY LINES FOR THIS PARAGRAPH            
         BE    *+12                                                             
         BAS   RE,SETPARA          ENSURE PARAGRAPH AMOUNTS UPDATED             
         B     TXT020                                                           
         MVI   FTXTNDX,1           ENSURE AT LEAST ONE LINE                     
TXT081   SR    RE,RE               TEST ROOM IN THIS PARAGRAPH                  
         IC    RE,PBRKLINE         CURRENT LINE                                 
         SR    RF,RF                                                            
         IC    RF,FTXTNDX          LINES REQUIRED                               
         AR    RE,RF                                                            
         CH    RE,=Y(MAXLINEQ)                                                  
         BNH   TXT086                                                           
         CLI   FSORTING,C'Y'                                                    
         BNE   TXT082                                                           
         BAS   RE,REMPARA          REMOVE THIS PARA FROM LIST                   
         BAS   RE,GETPARA          GET NEW PARAGRPAH NUMBER                     
         B     TXT086                                                           
TXT082   BAS   RE,ADDPARA          LINE LIMIT BLOWN - ADD PARAGRAPH             
         ZAP   FPARNET,FTRNNET     START NEW PARAGRAPH                          
         ZAP   FPARCOM,FTRNCOM                                                  
         MVI   PBRKLINE,0                                                       
         IC    RE,PBRKPARA                                                      
         LA    RE,1(RE)                                                         
         CH    RE,=Y(MAXPARAQ)                                                  
         BNH   *+6                                                              
         DC    H'0'                MAXIMUM PARAGRAPHS EXCEEDED                  
         STC   RE,PBRKPARA                                                      
*                                                                               
TXT086   LA    R3,BOELEM                                                        
         USING FFTELD,R3                                                        
         XC    FFTEL(FFTLN1Q),FFTEL                                             
         MVI   FFTEL,FFTELQ                                                     
         MVI   FFTLN,MAXLLNQ+(FFTDATA-FFTEL)                                    
         MVI   FFTTYPE,FFTTBLIN                                                 
         MVI   FFTSEQ,0                                                         
         MVI   FFTDLEN,MAXLLNQ                                                  
         MVC   FFTDATA(MAXLLNQ),FTXTTRN                                         
         LA    RE,FTXTTRN          CHOPPER OUPUT BLOCK                          
         SR    RF,RF               REDUCE COUNT OF LINES                        
         IC    RF,FTXTNDX                                                       
         BCTR  RF,0                                                             
         STC   RF,FTXTNDX                                                       
         LTR   RF,RF                                                            
         BZ    TXT088                                                           
         MVC   0(MAXLLNQ,RE),MAXLLNQ(RE)                                        
         LA    RE,MAXLLNQ(RE)                                                   
         BCT   RF,*-10                                                          
*                                                                               
TXT088   MVC   0(MAXLLNQ,RE),BCSPACES                                           
         L     R2,AIO2                                                          
         XC    PBRRSTA,PBRRSTA                                                  
         MVI   PBRRFST,0                                                        
         MVC   PBRRLEN,=Y(PBRRFST+1-PBRRECD)                                    
         SR    RE,RE                                                            
         IC    RE,PBRKLINE                                                      
         LA    RE,1(RE)                                                         
         STC   RE,PBRKLINE                                                      
         MVC   FPBRKEY,PBRKEY                                                   
         GOTO1 VHELLO,BODMCB,(C'P',ACCMST),AIO2,FFTEL,0                         
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIO,IO2+IOACCMST+IOADDREC                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   FTXTNDX,0                                                        
         BNE   TXT086                                                           
         BAS   RE,SETPARA                                                       
         B     TXT020                                                           
*                                                                               
TXT090   CLI   FHIPARA,1                                                        
         BE    TXT200                                                           
         MVC   IODAOVER,FHDRDA                                                  
         GOTO1 AIO,IOGETRUP+IOACCMST+IO2                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO2                                                          
         LA    R3,PBRRFST                                                       
         SR    RF,RF                                                            
         USING NDXELD,R3                                                        
TXT095   CLI   NDXEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   NDXEL,NDXELQ                                                     
         BE    TXT098                                                           
         IC    RF,NDXLN                                                         
         AR    R3,RF                                                            
         B     TXT095                                                           
TXT098   MVC   NDXHIGH,FHIPARA                                                  
         MVC   NDXACTV,FHIPARA                                                  
*                                                                               
         SR    R0,R0               R0=(NUMBER OF TABLE ENTRIES)                 
         ICM   R0,1,FHIPARA                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     RF,ACOM                                                          
         L     RF,CXSORT-COMFACSD(RF)                                           
         GOTO1 (RF),BODMCB,FPARATAB,(R0),L'FPARATAB,L'FPARAKEY,0                
         LA    RF,NDXINDX          INDEX IN ELEMENT                             
         LA    RE,FPARATAB         SORTED TABLE OF PARAGRAPHS                   
         MVC   0(1,RF),FPARAPAR-FPARATAB(RE)                                    
         LA    RF,1(RF)            NEXT INDEX ENTRY                             
         LA    RE,L'FPARATAB(RE)   NEXT SORTED PARAGRAPH                        
         BCT   R0,*-14                                                          
         B     TXT100                                                           
*                                                                               
TXT100   GOTO1 AIO,IOACCMST+IOPUTREC+IO2                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
TXT200   MVC   CSBILNUM,FBILLNO                                                 
         B     EXITY                                                            
*                                                                               
EXITN    MVI   FVOMTYP,GTMERR                                                   
         LTR   RB,RB                                                            
         B     EXIT                                                             
EXITY    MVI   FVOMTYP,GTMINF                                                   
         CR    RB,RB                                                            
EXIT     XIT1  ,                                                                
*                                                                               
SUBEDIT  DC    AL1(RECBIL,ACTEDT)                                               
         DC    AL1(1,0,0,0)                                                     
         EJECT                                                                  
**********************************************************************          
*        FORMAT THE TRANSACTION INTO PRINTLINE AREA                  *          
*                                                                    *          
**********************************************************************          
         SPACE 1                                                                
         USING TRNRECD,R2                                                       
FORMAT   NTR1                                                                   
         MVI   FTXTNDX,0           SET PRINT LINE COUNT                         
         LA    RE,FTXTTRN          CLEAR PRINT BUFFER                           
         LA    R0,FTXTLINS                                                      
         MVC   0(L'FTXTTRN,RE),BCSPACES                                         
         LA    RE,L'FTXTTRN(RE)                                                 
         BCT   R0,*-10                                                          
*                                                                               
         L     R2,AIO1             R2=A(TRANSACTION RECORD)                     
         L     R4,AIO3             R4=A(CONTROL RECORD)                         
         MVC   FTRNWC,TRNKWORK                                                  
         LA    R4,PBCRFST-PBCRECD(R4)                                           
         USING BLFELD,R4                                                        
FOR0100  CLI   BLFEL,0             TEST E-O-R                                   
         BE    FOR0900                                                          
         CLI   BLFEL,BLFELQ        TEST DATA FORMAT ELEMENT                     
         BNE   FOR0120                                                          
         CLC   FTRNHTYP,BLFTYPE    MATCH ON PARAGRAPH FORMAT                    
         BE    FOR0150                                                          
FOR0120  SR    R0,R0                                                            
         IC    R0,BLFLN                                                         
         AR    R4,R0                                                            
         B     FOR0100                                                          
FOR0150  LA    R1,FORMTYPS         TEST FORMAT APPLIES TO THIS DATA             
FOR0150A CLI   0(R1),0                                                          
         BE    FOR0120                                                          
         CLC   BLFFLD,0(R1)                                                     
         BE    *+12                                                             
         LA    R1,L'FORMTYPS(R1)                                                
         B     FOR0150A                                                         
         SR    R3,R3                                                            
         IC    R3,BLFLINF          START LINE NUMBER                            
         BCTR  R3,0                                                             
         MH    R3,=Y(MAXLLNQ)                                                   
         LA    R3,FTXTTRN(R3)                                                   
         SR    RF,RF                                                            
         IC    RF,BLFCOLF                                                       
         BCTR  RF,0                                                             
         AR    R3,RF               R3=A(DESTINATION)                            
*                                                                               
         SR    RF,RF               GET DATA                                     
         ICM   RF,3,1(R1)                                                       
         LA    RF,CLB04(RF)                                                     
         BASR  RE,RF                                                            
         BNE   FOR0120             NOT FOUND/EDITED DIRECT TO LINE              
         STCM  R1,15,BODMCB        LENGTH/ADDRESS OF SOURCE                     
         STCM  R3,15,BODMCB+4      A(DESTINATION)                               
         MVC   BODMCB+4(1),BLFCOLN WIDTH OF DESTINATION                         
         SR    RE,RE                                                            
         IC    RE,BLFLINN          SET MAXIMUM LINES                            
         ST    RE,BODMCB+8                                                      
         MVI   BODMCB+8,MAXLLNQ                                                 
         GOTO1 VCHOPPER,BODMCB                                                  
         L     RF,BODMCB+8         SET HIGHEST NUMBER OF LINES USED             
         SR    RE,RE                                                            
         IC    RE,BLFLINF          ADD START LINE NUMBER                        
         AR    RF,RE                                                            
         CLM   RF,1,FTXTNDX                                                     
         BL    FOR0120                                                          
         STC   RF,FTXTNDX                                                       
         B     FOR0120                                                          
*                                                                               
FOR0900  B     EXIT                                                             
*                                                                               
GETWC    LA    R1,TRNKWORK                                                      
         ICM   R1,8,=AL1(L'TRNKWORK)                                            
         CR    RE,RE                                                            
         BR    RE                                                               
*                                                                               
GETREF   LA    R1,TRNKREF                                                       
         ICM   R1,8,=AL1(L'TRNKREF)                                             
         CR    RE,RE                                                            
         BR    RE                                                               
*                                                                               
GETDAT   NTR1                                                                   
         MVC   BOWORK1,BCSPACES    CLEAR WORK AREA                              
         GOTO1 VDATCON,BODMCB,(1,TRNKDATE),(8,BOWORK1)                          
         B     SETABOW                                                          
*                                                                               
*                                                                               
GETSUP   LA    R1,FCTRNAM+L'FCTRNAM-1                                           
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         LA    RF,FCTRNAM                                                       
         SR    R1,RF                                                            
         LA    R1,1(R1)                                                         
         SLDL  R0,32                                                            
         LA    R1,FCTRNAM                                                       
         SLL   R1,8                                                             
         SRDL  R0,8                                                             
         CR    RE,RE                                                            
         BR    RE                                                               
*                                                                               
GETNAR   SR    R1,R1                                                            
         IC    R1,TRNRFST+(TRNLN-TRNEL)                                         
         SH    R1,=Y(TRNLN1Q)                                                   
         SLDL  R0,32               SAVE LENGTH                                  
         LA    R1,TRNRFST+(TRNNARR-TRNEL)                                       
         SLL   R1,8                                                             
         SRDL  R0,8                                                             
         CR    RE,RE                                                            
         BR    RE                                                               
*                                                                               
GETHRS   NTR1                                                                   
         CP    PA$HOURS,=P'0'                                                   
         BNE   *+10                                                             
         LTR   RB,RB                                                            
         B     XITR1                                                            
         XC    BODUB1,BODUB1                                                    
         ZAP   BODUB2,PP$AALLO     ALLOCATED CASH                               
         MP    BODUB1(16),PA$HOURS ALLOCATED CASH*TOTAL HOURS                   
         SRP   BODUB1(16),2,0                                                   
         DP    BODUB1(16),PA$NET   (ALLOC CASH*TOT HRS)/TOTAL CASH              
         SRP   BODUB1,64-2,5       BODUB1=ALLOC HOURS                           
         MVC   BOWORK1,BCSPACES                                                 
         SR    RF,RF                                                            
         IC    RF,BLFCOLN                                                       
         CURED BODUB1,((RF),(R3)),2,MINUS=YES,DMCB=BODMCB                       
         LTR   RB,RB                                                            
         B     EXIT                                                             
*                                                                               
GETRAT   NTR1                                                                   
         CP    PA$HOURS,=P'0'                                                   
         BNE   *+10                                                             
         LTR   RB,RB                                                            
         B     XITR1                                                            
         ZAP   BODUB1(16),PA$NET   TRANSACTION AMOUNT                           
         SRP   BODUB1(16),4,0                                                   
         DP    BODUB1(16),PA$HOURS TOTAL POSTED HOURS                           
         SRP   BODUB1,64-2,5                                                    
         SR    RF,RF                                                            
         IC    RF,BLFCOLN                                                       
         CURED BODUB1,((RF),(R3)),2,MINUS=YES,DMCB=BODMCB                       
         LTR   RB,RB                                                            
         B     EXIT                                                             
*                                                                               
SETABOW  LA    R1,BOWORK1+L'BOWORK1-1                                           
         CLI   0(R1),C' '          FIND REAL OUTPUT LENGTH                      
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         LA    RF,BOWORK1                                                       
         SR    R1,RF                                                            
         LA    R1,1(R1)            R1=(REAL O/P LENGTH)                         
         SLDL  R0,32               SHUNT INTO LOW R0 BYTE                       
         LA    R1,BOWORK1          SET ADDRESS OF SOURCE                        
         SLL   R1,8                SHIFT UP                                     
         SRDL  R0,8                SHIFT BACK WITH LENGTH                       
         CR    RB,RB                                                            
         B     XITR1                                                            
*                                                                               
XITR1    XIT1  REGS=(R1)                                                        
*                                                                               
FORMTYPS DS    0XL3                                                             
         DC    AL1(BLFFWCQ),AL2(GETWC-CLB04)      WORKCODE                      
         DC    AL1(BLFFREFQ),AL2(GETREF-CLB04)    TRANSACTION REFERENCE         
         DC    AL1(BLFFDATQ),AL2(GETDAT-CLB04)    TRANSACTION DATE              
         DC    AL1(BLFFSUPQ),AL2(GETSUP-CLB04)    SUPPLIER/PERSON NAME          
         DC    AL1(BLFFNARQ),AL2(GETNAR-CLB04)    NARRATIVE                     
         DC    AL1(BLFFHRSQ),AL2(GETHRS-CLB04)    HOURS                         
         DC    AL1(BLFFRATQ),AL2(GETRAT-CLB04)    SALES RATE                    
         DC    AL1(0)                                                           
         EJECT                                                                  
**********************************************************************          
*        READ WORKCODE RECORD                                        *          
**********************************************************************          
         SPACE 1                                                                
         USING TRNRECD,R2                                                       
         USING WCORECD,R4                                                       
RDWCODE  NTR1                                                                   
         L     R2,AIO1                                                          
         L     R4,AIO4                                                          
         CLC   WCOKWRK,TRNKWORK                                                 
         BE    RDWX                WORKCODE RECORD ALREADY THERE                
         LA    R4,IOKEY                                                         
         MVC   WCOKEY,BCSPACES                                                  
         MVI   WCOKTYP,WCOKTYPQ                                                 
         MVC   WCOKCPY,CUABIN                                                   
         MVC   WCOKUNT(L'BCCPYPRD),BCCPYPRD                                     
         MVC   WCOKWRK,TRNKWORK                                                 
         LH    R1,=Y(IO4)                                                       
         GOTO1 AIO,IORD+IOACCDIR(R1)                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         LH    R1,=Y(IO4)                                                       
         GOTO1 AIO,IOACCMST+IOGET(R1)                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
RDWX     B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
*        ADD PARAGRAPH HEADER RECORD                                 *          
*        PARAGRAPH LINES ARE ALREADY ON FILE                         *          
**********************************************************************          
         SPACE 1                                                                
         USING PBRRECD,R2                                                       
ADDPARA  NTR1                                                                   
         MVI   PBRRFST,0                                                        
         MVC   PBRRLEN,=Y(PBRRFST+1-PBRRECD)                                    
         SR    RF,RF                                                            
         ICM   RF,1,PBRKLINE       RF=HIGHEST LINE NUMBER                       
         BNZ   *+6                                                              
         DC    H'0'                FORCED DUMP PREVENTS SYSTEM CRASH            
         MVI   PBRKLINE,0          SET LINE 0 FOR PARA HDR RECORD               
         LA    R3,BOELEM                                                        
         USING NDXELD,R3                                                        
         XC    NDXEL(NDXLNQ),NDXEL ADD PARAGRAPH LINE INDEX                     
         MVI   NDXEL,NDXELQ                                                     
         MVI   NDXLN,NDXLNQ                                                     
         STC   RF,NDXHIGH          HIGHEST LINE NUMBER                          
         STC   RF,NDXACTV          HIGHEST ACTIVE LINE NUMBER                   
         LA    RE,NDXINDX(RF)                                                   
         BCTR  RE,0                                                             
         STC   RF,0(RE)                                                         
         BCT   RF,*-6                                                           
         GOTO1 VHELLO,BODMCB,(C'P',ACCMST),AIO2,NDXEL,0                         
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING FFTELD,R3                                                        
         XC    FFTEL(FFTLN1Q),FFTEL                                             
         MVI   FFTEL,FFTELQ                                                     
         MVI   FFTLN,FFTLN1Q+(L'FTXTPAR+L'FFTSEQ)                               
         MVI   FFTTYPE,FFTTPGHC                                                 
         MVI   FFTSEQ,0                                                         
         MVI   FFTDLEN,L'FTXTPAR                                                
*                                                                               
         MVC   FTXTPAR,BCSPACES                                                 
         CLI   FBILGRPB,PBCKWCQ                                                 
         BNE   ADDP10                                                           
         L     RE,AIO4                                                          
         LA    RE,WCORFST-WCORECD(RE)                                           
         MVC   FTXTPAR(L'WCODESC),WCODESC-WCOELD(RE)                            
*                                                                               
ADDP10   MVC   FFTDATA(L'FTXTPAR),FTXTPAR                                       
         GOTO1 VHELLO,BODMCB,(C'P',ACCMST),AIO2,FFTEL,0                         
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PGHELD,R3                                                        
         XC    PGHEL(PGHLNQ),PGHEL PARAGRAPH HEADER ELEMENT                     
         MVI   PGHEL,PGHELQ                                                     
         MVI   PGHLN,PGHLNQ                                                     
         ZAP   PGHNET,FPARNET                                                   
         ZAP   PGHCOM,FPARCOM                                                   
         MVI   PGHTAX,0                                                         
         MVC   PGHHTYP,FTRNHTYP    SET HEADING TYPE REQUIRED                    
         GOTO1 VHELLO,BODMCB,(C'P',ACCMST),AIO2,PGHEL,0                         
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIO,IO2+IOACCMST+IOADDREC                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FHIPARA,PBRKPARA    SET HIGH PARAGRAPH ADDED                     
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
ACCMST   DC    C'ACCMST '                                                       
FF       EQU   X'FF'                                                            
         EJECT                                                                  
**********************************************************************          
*        ADD/UPDATE PARAGRAPH HEADER RECORD                          *          
*        PARAGRAPHS ARE BEING ADDED OUT OF SEQUENCE                  *          
**********************************************************************          
         SPACE 1                                                                
         USING PBRRECD,R2                                                       
SETPARA  NTR1                                                                   
         MVC   IOKEY(L'PBRKEY),PBRKEY                                           
         LA    R4,FPARATAB                                                      
         USING FPARATAB,R4                                                      
         B     *+8                                                              
SETP10   LA    R4,L'FPARATAB(R4)                                                
         CLI   0(R4),0                                                          
         BE    SETP60              PARA NOT ADDED - ADD IT                      
         CLC   FPARAPAR,PBRKPARA                                                
         BNE   SETP10                                                           
         CLI   FSORTING,C'Y'                                                    
         BNE   SETPX                                                            
         MVC   FPARAHIL,PBRKLINE   SET NEW HIGH LINE NUMBER                     
         MVI   PBRKLINE,0          SET PARAGRAPH KEY                            
         MVI   IOKEY+PBRKLINE-PBRKEY,0                                          
*                                                                               
         GOTO1 AIO,IO2+IORD+IOACCDIR                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIO,IO2+IOACCMST+IOGETRUP                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R3,PBRRFST                                                       
         SR    RF,RF                                                            
SETP20   CLI   0(R3),0                                                          
         BE    SETP50                                                           
         CLI   0(R3),NDXELQ                                                     
         BE    SETP30                                                           
         CLI   0(R3),PGHELQ                                                     
         BE    SETP40                                                           
SETP25   IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     SETP20                                                           
         USING NDXELD,R3                                                        
SETP30   MVC   NDXHIGH,FPARAHIL                                                 
         MVC   NDXACTV,FPARAHIL                                                 
         ICM   RF,1,NDXACTV                                                     
         BNZ   *+6                                                              
         DC    H'0'                PRECAUTION                                   
         LA    RE,NDXINDX(RF)                                                   
         BCTR  RE,0                                                             
         STC   RF,0(RE)                                                         
         BCT   RF,*-6                                                           
         B     SETP25                                                           
*                                                                               
         USING PGHELD,R3                                                        
SETP40   AP    PGHNET,FPARNET                                                   
         AP    PGHCOM,FPARCOM                                                   
         B     SETP25                                                           
*                                  WRITE BACK PARAGRAPH HEADER                  
SETP50   GOTO1 AIO,IO2+IOACCMST+IOPUTREC                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         B     SETP90                                                           
*                                  ADD NEW PARAGRAPH                            
SETP60   CLI   FSORTING,C'Y'                                                    
         BNE   SETP70                                                           
         MVC   FPARACOD,FPARCODE   SET NEW PARA IN TABLE                        
         MVC   FPARAHIL,PBRKLINE   SET HIGH LINE NUMBER                         
         MVC   FPARAPAR,PBRKPARA   REAL PARAGRAPH NUMBER                        
         BAS   RE,ADDPARA          ADD PARAGRAPH                                
         B     SETP90                                                           
*                                                                               
SETP70   MVC   FPARAPAR,PBRKPARA                                                
         MVC   FPARACOD,FHRSPARA                                                
         L     RE,AIO4             SET R4 TO A(WCODE ELEMENT)                   
         LA    RE,WCORFST-WCORECD(RE)                                           
         TM    WCOSTAT-WCOELD(RE),WCOSHCOE                                      
         BO    SETPX                                                            
         MVC   FPARACOD,FOOPPARA                                                
         B     SETPX                                                            
*                                                                               
SETP90   ZAP   FPARNET,=P'0'                                                    
         ZAP   FPARCOM,=P'0'                                                    
*                                                                               
SETPX    B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
*        GET REAL PARAGRAPH NUMBER FOR LINE RECORDS                  *          
*        PARAGRAPHS ARE BEING ADDED OUT OF SEQUENCE                  *          
**********************************************************************          
         SPACE 1                                                                
         USING PBRRECD,R2                                                       
GETPARA  NTR1                                                                   
         LA    R4,FPARATAB                                                      
         USING FPARATAB,R4                                                      
         B     *+8                                                              
GETP10   LA    R4,L'FPARATAB(R4)                                                
         CLI   0(R4),0                                                          
         BE    GETP20              NEW PARAGRAPH CODE                           
         CLC   FPARACOD,FPARCODE                                                
         BNE   GETP10                                                           
         MVC   PBRKPARA,FPARAPAR   GET REAL PARAGRAPH NUMBER                    
         MVC   PBRKLINE,FPARAHIL   GET HIGH LINE NUMBER                         
         B     GETPX                                                            
*                                                                               
GETP20   SR    RF,RF               INCREMENT HIGHEST PARA ADDED                 
         IC    RF,FHIPARA                                                       
         LA    RF,1(RF)                                                         
         STC   RF,PBRKPARA         NEW PARA                                     
         MVI   PBRKLINE,0                                                       
         B     GETPX                                                            
*                                                                               
GETPX    B     EXIT                                                             
         DROP  R4                                                               
*                                                                               
REMPARA  NTR1                                                                   
         LA    R4,FPARATAB                                                      
         USING FPARATAB,R4                                                      
         B     *+8                                                              
REMP10   LA    R4,L'FPARATAB(R4)                                                
         CLI   0(R4),0                                                          
         BE    REMPX                                                            
         CLC   FPARACOD,FPARCODE                                                
         BNE   REMP10                                                           
         MVI   FPARACOD,X'FF'      THIS EFFECTIVELY REMOVES IT                  
REMPX    B     EXIT                                                             
         EJECT                                                                  
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* ACCLBWORK                                                                     
       ++INCLUDE ACCLBWORK                                                      
         SPACE 1                                                                
TWAD     DSECT                                                                  
         ORG   BASOLAYH                                                         
       ++INCLUDE ACCLBF5D                                                       
*                                                                               
WORKD    DSECT                                                                  
         ORG   OVERWRK                                                          
PRATBLK  DS    0C                                                               
       ++INCLUDE ACPRORATAD                                                     
FPBRKEY  DS    CL(L'PBRKEY)        PREVIOUS KEY                                 
FBILLNO  DS    CL6                 NEW DRAFT BILL NUMBER                        
FHDRDA   DS    XL4                 HEADER RECORD DISK ADDRESS                   
FJOBNET  DS    PL6                 NET ALLOCATION - BILL                        
FJOBCOM  DS    PL6                 COMM ALLOCATION - BILL                       
FPARNET  DS    PL6                 NET ALLOCATION - PARAGRAPH                   
FPARCOM  DS    PL6                 COMM ALLOCATION - PARAGRAPH                  
FTRNNET  DS    PL6                 NET ALLOCATION - ITEM                        
FTRNCOM  DS    PL6                 COMM ALLOCATION - ITEM                       
FTXTPAR  DS    CL36                PARAGRAPH DESCRIPTION                        
FCTRNAM  DS    CL36                LAST CONTRA NAME                             
FTRNKEY  DS    CL(L'TRNKEY)        SAVED LAST TRANSACTION KEY                   
FTRNWC   DS    CL(L'TRNKWORK)      SAVED LAST WORKCODE                          
FTRNWG   DS    CL(L'WGRKCODE)      SAVED LAST WORKGROUP                         
FTRNHTYP DS    XL1                 WORKCODE PARAGRAPH TYPE                      
FBILGRPB DS    CL(L'PBCKGRPB)      GROUPING BASIS                               
FSORTING DS    CL1                 Y=PARAGRAPHS ADDED OUT OF SEQUENCE           
FHRSPARA DS    XL1                 KEY FOR HOURLY PARA                          
FOOPPARA DS    XL1                 KEY FOR OOP PARA                             
FJOBOVER DS    CL1                 Y=CHANGE OF JOB DETECTED                     
FHIPARA  DS    CL1                 HIGH PARAGRAPH NUMBER                        
FPARHDR  DS    CL(PGHLNQ)          PARAGRAPH HEADER ELEMENT                     
FPARCODE DS    CL1                 PARAGRAPH CODE FOR CURRENT XACTN             
*                                                                               
FTXTLINS EQU   9                   MAXIMUM NUMBER OF TEXT LINES                 
FTXTNDX  DS    CL1                 NUMBER OF CHOPPER OUTPUT LINES               
FTXTTRN  DS    (FTXTLINS+1)CL(MAXLLNQ)                                          
*                                  TABLE OF PARAS ADDED OUT OF SEQ              
FPARATAB DS    0CL3                                                             
FPARAKEY DS    0XL2                XSORT KEY                                    
FPARACOD DS    XL1                 PARA CODE (USER CODE/WORKGROUP ETC)          
FPARAPAR DS    XL1                 REAL PARAGRAPH NUMBER                        
FPARAHIL DS    XL1                 HIGHEST LINE SO FAR ADDED                    
         DS    (MAXPARAQ)XL(L'FPARATAB)                                         
*                                                                               
MAXPARAQ EQU   200                 MAXIMUM NUMBER OF PARAGRAPHS                 
MAXLINEQ EQU   200                 MAXIMUM LINES PER PARAGRAPH                  
MAXADDRS EQU   498                 MAXIMUM D/AS IN AIO5                         
*                                                                               
OSVALSD  DSECT                                                                  
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'022ACCLB04   08/16/00'                                      
         END                                                                    
