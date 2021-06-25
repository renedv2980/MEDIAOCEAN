*          DATA SET PPREPTS02  AT LEVEL 059 AS OF 01/31/05                      
*PHASE PPTS02C                     **** NOTE "C" PHASE                          
         TITLE 'PPTS02  CHANGE LOG'                                             
*                                                                               
*    SMYE 12/27/04   ADD WEBIO INSERTION ORDERS TO "LAST I/O" HANDLING          
*                                                                               
*    SMYE 12/12/95   CHANGED DTCNV TO DATCON WITH NEW PARAM'S                   
*                                                                               
*    BPLA 3/29/95    ONLY HAVE PRODUCT NAME IN HEADLINES                        
*                    IF QPRODUCT = ALL OR ONE PRODUCT                           
*                    DISPLAY PUB ADDRESS (PPTS01)                               
*                    TELEPHONE AND FAX IN HEADLINES                             
*                    IF NEWSPAPERS DISPLAY STATE, CITY                          
*                    IF OUTDOOR (AND NO ZONE) ALSO DISPLAY STATE, CITY          
*                                                                               
*        REQUEST OPTIONS                                                        
*                                                                               
*    QOPT1 = STATUS FILTER                                                      
*            IF ENTERED ONLY BUYS WHOSE TEAR STATUS MATCHES QOPT1               
*            ARE REPORTED                                                       
*            IF = X - REPORT ALL STATUS                                         
*                                                                               
*     NOTE - IF BLANK, BUYS WITH NO TEARSHEET ELEM ARE ALSO REPORTED            
*                                                                               
*    QOPT2 Y= FLAG BILLED/PAID//TRAFFICKED ITEMS                                
*                                                                               
*    QOPT3    PAID/UNPAID FILTER                                                
*          P= PAID ITEMS ONLY                                                   
*          U= UNPAID ITEMS ONLY                                                 
*      BLANK= ALL ITEMS                                                         
*                                                                               
*    QOPT4    BILLED/UNBILLED FILTER                                            
*          B= BILLED ITEMS ONLY                                                 
*          U= UNBILLED ITEMS ONLY                                               
*      BLANK= ALL ITEMS                                                         
*                                                                               
*    QBUYLIN (COL 60+61)   MAY CONTAIN LINE NUMBER                              
*                                                                               
*                                                                               
*     TS PROFILE OPTIONS (IN PROGPROF)                                          
*                                                                               
*    +0     Y= PAYABLE DATE                                                     
*    +1     Y= BILLABLE DATE                                                    
*    +2     Y= ON-SALE DATE                                                     
*    +3     Y= CLOSING AND MATERIALS CLOSING DATE                               
*                                                                               
*    +4     Y= LAST I/O                                                         
*    +5     Y= DISPLAY REGUALR BUY COMMENT                                      
*    +6     Y= DISPLAY I/O COMMENTS                                             
*    +7     Y= DISPLAY POSITION COMMENTS                                        
*    +11    Y= SUPPRESS LAST DATA CHANGED (DATE AND BUYER REMAIN)               
*                                                                               
         PRINT NOGEN                                                            
         TITLE 'PPTS02 - TEARSHEET REPORT'                                      
PPTS02   CSECT                                                                  
         NMOD1 0,PPTS02,RR=R9                                                   
*                                                                               
*                                                                               
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         LA    R8,2048(RC)                                                      
         LA    R8,2048(R8)                                                      
         USING PPFILED,RC,R8                                                    
*                                 USE R8 TO ACCESS PUBREC                       
         L     R5,=V(TSWORK)                                                    
         AR    R5,R9                                                            
         USING TSWORKD,R5                                                       
*                                                                               
         ST    R9,RELO                                                          
*                                                                               
         CLI   MODE,FBUYREQ                                                     
         BNE   CKMODE                                                           
         BAS   R9,INITIAL                                                       
         B     EXT                                                              
*                                                                               
CKMODE   CLI   MODE,FBUYCLI                                                     
         BNE   CKM2                                                             
         GOTO1 VCLIFRST                                                         
         B     EXT                                                              
*                                                                               
CKM2     DS    0H                                                               
*                                                                               
CKM3     CLI   MODE,FBUYPUB                                                     
         BNE   CKM4                                                             
         MVI   FORCEHED,C'Y'                                                    
         B     EXT                                                              
*                                                                               
CKM4     CLI   MODE,PROCBUY                                                     
         BNE   CKM6                                                             
         B     PROCESS                                                          
*                                                                               
CKM6     DS    0H                                                               
         CLI   MODE,RUNFRST                                                     
         BNE   CKM8                                                             
         CLC   RCPROG,=C'TT'     SEE IF DOING TURNAROUND                        
         BNE   CKM8                                                             
         MVI   RCREQREP,C'N'     NO REQUEST DETAILS                             
         B     EXT                                                              
*                                                                               
CKM8     DS    0H                                                               
*                                                                               
CKM9     DS    0H                                                               
*                                                                               
         B     EXT                                                              
*                                                                               
         EJECT                                                                  
PROCESS  EQU   *                                                                
         OC    KEY+21(3),KEY+21       IGNORE PASSIVE POINTERS                   
         BNZ   EXT                                                              
*                                                                               
         CLI   QBUYLIN,C' '         SEE IF LINE NUMBER GIVEN                    
         BE    CKQOPT1                                                          
         PACK  DUB,QBUYLIN                                                      
         CVB   R0,DUB                                                           
         STC   R0,BYTE                                                          
         CLC   PBUYKLIN,BYTE          NOT RIGHT LINE NUMBER - SKIP              
         BNE   EXT                                                              
*                                                                               
CKQOPT1  DS    0H                                                               
         MVI   HAVTEAR,C'N'                                                     
         LA    R3,PBDELEM           LOOK FOR TEARSHEET ELEM                     
         MVI   ELCODE,X'95'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   CKQOPT1D                                                         
*                                                                               
         USING PTSHTELD,R3                                                      
         MVI   HAVTEAR,C'Y'                                                     
         CLI   QOPT1,C'X'            SEE IF PROCESSING ALL STATUS               
         BE    CKQOPT4                                                          
         CLC   QOPT1,PTSHSTAT        OTHERWISE MATCH STATUS                     
         BNE   NEXTBUY                                                          
         B     CKQOPT4                                                          
*                                                                               
CKQOPT1D DS    0H        I GET HERE IF NO TEAR ELEMENT WAS FOUND                
         CLI   QOPT1,C'X'    SEE IF PROCESSING ALL STATUS                       
         BE    CKQOPT4                                                          
         CLI   QOPT1,C' '  SEE IF PROCESSING BUYS WITH                          
         BE    CKQOPT4     NO TEARSHEET ELEMS OR STATUS BLANK                   
         B     NEXTBUY                                                          
*                                                                               
CKQOPT4  DS    0H                                                               
         CLI   QOPT4,C' '      SEE IF BILLED OPTION PRESENT                     
         BE    BUYOK                                                            
*                                                                               
         CLI   QOPT4,C'U'       SEE IF UNBILLED ITEMS ONLY                      
*                              (REALLY BILLABLE ITEMS)                          
         BNE   CKQOPT4G                                                         
         CLC   GROSS(12),BGROSS                                                 
         BNE   BUYOK                                                            
         OC    GROSS(12),GROSS    SEE IF FREE BUY                               
         BNE   NEXTBUY                                                          
         LA    R3,PBUYREC+33                                                    
         MVI   ELCODE,X'26'                                                     
CKQOPT4C BAS   RE,NEXTEL                                                        
         BNE   BUYOK                                                            
         OC    5(3,R3),5(R3)         CHECK FOR DATE                             
         BNZ   NEXTBUY               BUY WAS BILLED                             
         B     CKQOPT4C                                                         
*                                                                               
CKQOPT4G CLI   QOPT4,C'B'       SEE IF BILLED ITEMS ONLY                        
*                              (REALLY FULLY BILLED)                            
         BNE   BUYOK                                                            
*                                                                               
         OC    GROSS(12),GROSS    SEE IF FREE BUY                               
         BE    CKQOPT4I                                                         
         CLC   GROSS(12),BGROSS                                                 
         BE    BUYOK                                                            
         B     NEXTBUY                                                          
*                                                                               
CKQOPT4I DS    0H               FREE BUY-CHECK FOR BILLING ELEM                 
         CLC   GROSS(12),BGROSS                                                 
         BNE   BUYOK                                                            
         LA    R3,PBUYREC+33                                                    
         MVI   ELCODE,X'26'                                                     
CKQOPT4J BAS   RE,NEXTEL                                                        
         BNE   NEXTBUY                                                          
         OC    5(3,R3),5(R3)         CHECK FOR DATE                             
         BNZ   BUYOK                 BUY WAS BILLED                             
         B     CKQOPT4J                                                         
*                                                                               
NEXTEL   DS    0H       GET  NEXT ELEMENT                                       
         CLI   0(R3),0                                                          
         BE    NEXTELX                                                          
         ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLC   0(1,R3),ELCODE                                                   
         BER   RE                                                               
         B     NEXTEL                                                           
NEXTELX  LTR   RE,RE                                                            
         BR    RE                                                               
*                                                                               
BUYOK    DS    0H                                                               
*                                                                               
         MVI   FORCEHED,C'Y'    ONE BUY PER PAGE                                
*                                                                               
         LA    R2,BUYOUTA                                                       
         USING PPBYOUTD,R2                                                      
         MVI   PBYOCTL,X'28'                                                    
         MVI   PBYOCLT2,0                                                       
         GOTO1 PPBYOUT,DMCB,BUYOUTA                                             
         MVC   P+3(8),PBYOINS                                                   
         CLI   PBYOINS2,C' '                                                    
         BE    PPBY00                                                           
         MVI   PSECOND+3,C'+'                                                   
         MVC   PSECOND+4(8),PBYOINS2                                            
         B     PPBYX                                                            
PPBY00   DS    0H                                                               
*                                                                               
         CLC   PBYOISNM,SPACES                                                  
         BE    PPBYX                                                            
         MVC   PSECOND+4(11),PBYOISNM                                           
         B     PPBYX                                                            
PPBYX    DS    0H                                                               
*                                                                               
         TM    PBUYCNTL,X'80'           SEE IF DELETED                          
         BZ    PRTN1                                                            
         MVI   P+11,C'D'                                                        
*                                                                               
PRTN1    MVC   P+13(6),PBDJOB                                                   
PRTN2    DS    0H                                                               
         CLI   QMEDIA,C'N'                                                      
         BNE   PRTMAG                                                           
         CLI   PBYOSPC,C' '                                                     
         BE    PRTN5                                                            
         MVC   P+21(08),PBYOSPC                                                 
         MVC   PSECOND+21(11),PBYOPRM                                           
         B     PRTN10                                                           
*                                                                               
PRTN5    MVC   P+21(07),PBYOUNTS                                                
         MVC   PSECOND+21(11),PBYOPRM                                           
         B     PRTTEAR                                                          
*                                                                               
PRTN10   MVC   P+30(7),PBYOUNTS                                                 
         LA    R1,5                                                             
         LA    R4,P+30                                                          
PRTN20   CLI   P+36,C' '                                                        
         BNE   PRTTEAR                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R4),PBYOUNTS                                                 
         MVI   0(R4),C' '                                                       
         LA    R4,1(R4)                                                         
         BCT   R1,PRTN20                                                        
         B     PRTTEAR                                                          
*                                                                               
PRTMAG   DS    0H                    REALLY NON-NEWSPAPERS                      
         MVC   P+21(20),PBYOSPC                                                 
         MVC   PSECOND+21(20),PBYOSPC2                                          
*                                                                               
PRTTEAR  DS    0H                                                               
         CLI   QOPT2,C'Y' SEE IF FLAGGING BILLED/PAID/TRAFFICKED ITEMS          
         BNE   *+8                                                              
         BAS   RE,TSTBLTR                                                       
*                                                                               
PRTE5    DS    0H                                                               
         CLI   HAVTEAR,C'Y'                                                     
         BNE   PRTE5X                                                           
         LA    R4,5                                                             
         LA    R1,P+41                                                          
         LA    RE,PTSHSTAT                                                      
PRTE5C   MVC   0(1,R1),0(RE)                                                    
         LA    R1,6(R1)                                                         
         LA    RE,1(RE)                                                         
         BCT   R4,PRTE5C                                                        
*                                                                               
         MVC   P+71(1),PTSHIND5     ZONES                                       
*                                                                               
         CLI   PTSHREPO,0                                                       
         BE    PRTE5G                                                           
         EDIT  (B1,PTSHREPO),(2,P+76),0,ALIGN=LEFT                              
*                                                                               
PRTE5G   DS    0H                                                               
         MVC   P+81(10),PTSHPAGE                                                
         OC    PTSHCDAT,PTSHCDAT     CHECK FOR CHANGE DATE                      
         BNZ   PRTE5I                                                           
*                                                                               
         CLI   PROGPROF+11,C'Y'      SEE IF SUPPRESSING DATA CHANGED            
         BE    PRTE5X                                                           
*                                                                               
*        GOTO1 DTCNV,DMCB,(1,PTSHIDAT),(3,P+92)                                 
         GOTO1 DATCON,DMCB,(3,PTSHIDAT),(5,P+92)                                
         MVC   P+102(3),PTSHBID                                                 
         MVC   P+107(05),=C'ADDED'                                              
         B     PRTE5X                                                           
*                                                                               
PRTE5I   DS    0H                                                               
         MVI   PLINESW,1            TELLS ME I'M ON LINE 1                      
*                                                                               
*        GOTO1 DTCNV,DMCB,(1,PTSHCDAT),(3,P+92)                                 
         GOTO1 DATCON,DMCB,(3,PTSHCDAT),(5,P+92)                                
         MVC   P+102(3),PTSHBID                                                 
*                                                                               
         CLI   PROGPROF+11,C'Y'      SEE IF SUPPRESSING DATA CHANGED            
         BE    PRTE5X                                                           
*                                                                               
         LA    RE,P+107                                                         
         TM    PTSHCIN1,X'01'         TEAR OK                                   
         BZ    PRTE5J                                                           
         MVC   0(7,RE),=C'STATUS,'                                              
         LA    RE,7(RE)                                                         
*                                                                               
PRTE5J   DS    0H                                                               
         TM    PTSHCIN1,X'02'         SPACE OK                                  
         BZ    PRTE5K                                                           
         MVC   0(6,RE),=C'SPACE,'                                               
         LA    RE,6(RE)                                                         
*                                                                               
PRTE5K   TM    PTSHCIN1,X'04'         CAPTION OK                                
         BZ    PRTE5L                                                           
         MVC   0(6,RE),=C'CAPTN,'                                               
         LA    RE,6(RE)                                                         
*                                                                               
PRTE5L   TM    PTSHCIN1,X'08'        POSITION OK                                
         BZ    PRTE5M                                                           
         MVC   0(6,RE),=C'POSTN,'                                               
         LA    RE,6(RE)                                                         
*                                                                               
PRTE5M   DS    0H                                                               
         TM    PTSHCIN1,X'10'         INS DATE OK                               
         BZ    PRTE5N                                                           
         LA    RF,P+126          SEE IF I HAVE ROOM ON THIS LINE                
         CR    RE,RF                                                            
         BNH   PRTE5M5                                                          
         BCTR  RE,0                                                             
         MVI   0(RE),C' '      BLANK LAST COMMA                                 
         LA    RE,PSECOND+107   USE NEXT LINE                                   
         MVI   PLINESW,2        TELLS ME I'M ON 2ND LINE                        
*                                                                               
PRTE5M5  MVC   0(5,RE),=C'DATE,'                                                
         LA    RE,5(RE)                                                         
*                                                                               
PRTE5N   DS    0H                                                               
         TM    PTSHCIN2,X'01'      REPO QUALITY                                 
         BZ    PRTE5O                                                           
         CLI   PLINESW,2            SEE IF I'M ON LINE 2                        
         BE    PRTE5N5              I MUST HAVE ROOM                            
*                                                                               
         LA    RF,P+126          SEE IF I HAVE ROOM ON THIS LINE                
         CR    RE,RF                                                            
         BNH   PRTE5N5                                                          
         MVI   0(RE),C' '      BLANK LAST COMMA                                 
         LA    RE,PSECOND+107   USE NEXT LINE                                   
         MVI   PLINESW,2        TELLS ME I'M ON 2ND LINE                        
*                                                                               
PRTE5N5  MVC   0(5,RE),=C'REPO,'                                                
         LA    RE,5(RE)                                                         
*                                                                               
PRTE5O   DS    0H                                                               
         TM    PTSHCIN2,X'02'    COMMENTS CHANGED                               
         BZ    PRTE5P                                                           
         CLI   PLINESW,2         SEE IF I'M ON LINE 2                           
         BE    PRTE5O5                                                          
*                                                                               
         LA    RF,P+123          SEE IF I HAVE ROOM ON THIS LINE                
         CR    RE,RF                                                            
         BNH   PRTE5O5                                                          
         MVI   0(RE),C' '      BLANK LAST COMMA                                 
         LA    RE,PSECOND+107   USE NEXT LINE                                   
         MVI   PLINESW,2        TELLS ME I'M ON 2ND LINE                        
*                                                                               
PRTE5O5  MVC   0(8,RE),=C'COMMENTS'                                             
         LA    RE,8(RE)                                                         
*                                                                               
PRTE5P   DS    0H                                                               
         BCTR  RE,0           BACK-UP ONE BYTE                                  
         CLI   0(RE),C','     BLANK OUT LAST COMMA                              
         BNE   *+8                                                              
         MVI   0(RE),C' '                                                       
*                                                                               
PRTE5X   GOTO1 VPRINTIT                                                         
         B     PRTEX                                                            
PRTEX    DS    0H                                                               
*                                                                               
*                               NOW PRINT OTHER BUY DATA                        
         DROP  R3                                                               
*                                                                               
BUYINFO  DS    0H                                                               
         CLC   QPRODUCT(3),SPACES SEE IF NOT RUNNING IN PRODUCT ORDER           
         BNE   BUYI2                                                            
         MVC   P+13(4),=C'PRD='                                                 
         MVC   P+18(3),PBUYKPRD                                                 
         CLC   QEST,=C'ALL'                                                     
         BE    BUYI1X                                                           
         CLC   QEST(3),SPACES    SEE IF NOT RUNNING IN ESTIMATE ORDER           
         BE    BUYI1C                                                           
         CLC   QESTEND,SPACES     CHK FOR RANGE                                 
         BE    BUYI1X                                                           
*                                                                               
BUYI1C   MVC   P+22(4),=C'EST='                                                 
         MVC   HALF,PBUYKEST                                                    
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+27(3),DUB                                                      
BUYI1X   GOTO1 VPRINTIT                                                         
         B     BUYI3                                                            
*                                                                               
BUYI2    DS    0H                                                               
         CLC   QEST,=C'ALL'                                                     
         BE    BUYI3                                                            
         CLC   QEST(3),SPACES    SEE IF NOT RUNNING IN ESTIMATE ORDER           
         BE    BUYI2C                                                           
         CLC   QESTEND,SPACES     CHK FOR RANGE                                 
         BE    BUYI3                                                            
*                                                                               
BUYI2C   DS    0H                                                               
         MVC   P+13(4),=C'EST='                                                 
         MVC   HALF,PBUYKEST                                                    
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+18(3),DUB                                                      
         GOTO1 VPRINTIT                                                         
*                                                                               
BUYI3    DS    0H                                                               
         CLI   PROGPROF+0,C'Y'    SEE IF DISPLAYING PAYABLE DATE                
         BNE   BUYI5                                                            
         MVC   P+13(13),=C'PAYABLE DATE='                                       
*        GOTO1 DTCNV,DMCB,(1,PBDPDATE),(3,P+26)                                 
         GOTO1 DATCON,DMCB,(3,PBDPDATE),(5,P+26)                                
         GOTO1 VPRINTIT                                                         
*                                                                               
BUYI5    DS    0H                                                               
         CLI   PROGPROF+1,C'Y'    SEE IF DISPLAYING BILLABLE DATE               
         BNE   BUYI10                                                           
         MVC   P+13(14),=C'BILLABLE DATE='                                      
*        GOTO1 DTCNV,DMCB,(1,PBDBDATE),(3,P+27)                                 
         GOTO1 DATCON,DMCB,(3,PBDBDATE),(5,P+27)                                
         GOTO1 VPRINTIT                                                         
*                                                                               
BUYI10   DS    0H                                                               
         CLI   PROGPROF+2,C'Y'    SEE IF DISPLAYING ON-SALE DATE                
         BNE   BUYI15                                                           
         OC    PBDSDATE,PBDSDATE    SEE IF I HAVE ONE                           
         BZ    BUYI15                                                           
         MVC   P+13(13),=C'ON-SALE DATE='                                       
*        GOTO1 DTCNV,DMCB,(1,PBDSDATE),(3,P+26)                                 
         GOTO1 DATCON,DMCB,(3,PBDSDATE),(5,P+26)                                
         GOTO1 VPRINTIT                                                         
*                                                                               
BUYI15   DS    0H                                                               
         CLI   PROGPROF+3,C'Y'    SEE IF DISPLAYING CLOSING DATES               
         BNE   BUYI20                                                           
         OC    PBDCDATE,PBDCDATE    SEE IF I HAVE ONE                           
         BZ    BUYI15C                                                          
         MVC   P+13(13),=C'CLOSING DATE='                                       
*        GOTO1 DTCNV,DMCB,(1,PBDCDATE),(3,P+26)                                 
         GOTO1 DATCON,DMCB,(3,PBDCDATE),(5,P+26)                                
         GOTO1 VPRINTIT                                                         
*                                                                               
BUYI15C  OC    PBDMDATE,PBDMDATE  SEE IF I HAVE MATERIALS CLOSING DATE          
         BZ    BUYI20                                                           
         MVC   P+13(18),=C'MAT. CLOSING DATE='                                  
*        GOTO1 DTCNV,DMCB,(1,PBDMDATE),(3,P+31)                                 
         GOTO1 DATCON,DMCB,(3,PBDMDATE),(5,P+31)                                
         GOTO1 VPRINTIT                                                         
*                                                                               
BUYI20   DS    0H                                                               
         LA    R6,PBUYREC+33     CHECK FOR REF ELEM                             
         MVI   ELCODE,X'83'                                                     
         BAS   RE,LNEXTEL                                                       
         BNE   LTST2                                                            
         MVC   P+13(4),=C'REF='                                                 
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'3'            ELCODE + LEN + 1 FOR EX                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P+17(0),2(R6)                                                    
         GOTO1 VPRINTIT                                                         
*                                                                               
LTST2    LA    R6,PBUYREC+33     CHECK FOR SPECIAL REP ELEM                     
         MVI   ELCODE,X'80'                                                     
         BAS   RE,LNEXTEL                                                       
         BNE   LTST4                                                            
         MVC   P+13(5),=C'SREP='                                                
         MVC   P+19(4),2(R6)                                                    
         GOTO1 VPRINTIT                                                         
*                                                                               
LTST4    CLI   PROGPROF+4,C'Y'     SHOW LAST I/O ?                              
         BNE   RLMTH               NO                                           
         XC    SV70ELM,SV70ELM                                                  
         LA    R6,PBUYREC+33                                                    
         MVI   ELCODE,X'70'                                                     
LTST5    BAS   RE,LNEXTEL                                                       
         BNE   LTST10                                                           
         OC    2(3,R6),2(R6)                                                    
         BZ    LTST5                                                            
         CLC   SV70ELM+2(3),2(R6)                                               
*SMY*    BNL   LTST5                                                            
         BH    LTST5                                                            
         MVC   SV70ELM(11),0(R6)                                                
         B     LTST5                                                            
*                                                                               
LTST10   DS    0H                  NOW CHECK FOR WEBIO ELEM                     
         LA    R6,PBUYREC+33                                                    
         MVI   ELCODE,X'71'                                                     
LTST11   BAS   RE,LNEXTEL                                                       
         BNE   LTST15                                                           
         OC    2(3,R6),2(R6)                                                    
         BZ    LTST11                                                           
         CLC   SV70ELM+2(3),2(R6)                                               
         BH    LTST11                                                           
         MVC   SV70ELM(11),0(R6)                                                
         B     LTST11                                                           
*                                                                               
SV70ELM  DS    CL11           TO SAVE FIRST 11 BYTES OF 70 OR 71 ELEM           
ELCODE   DS    CL1                                                              
*                                                                               
LNEXTEL  DS    0H       GET  NEXT ELEMENT                                       
         CLI   0(R6),0                                                          
         BE    LNEXTELX                                                         
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLC   0(1,R6),ELCODE                                                   
         BER   RE                                                               
         B     LNEXTEL                                                          
LNEXTELX LTR   RE,RE                                                            
         BR    RE                                                               
*                                                                               
*                                                                               
*                                                                               
LTST15   DS    0H                                                               
         OC    SV70ELM,SV70ELM                                                  
         BZ    RLMTH                                                            
         MVC   P+13(9),=C'LAST I/O='                                            
         CLI   SV70ELM,X'71'       WEBIO ?                                      
         BE    LTST45              YES                                          
         GOTO1 DATCON,DMCB,(3,SV70ELM+2),(0,P+23)                               
         MVC   P+22(1),PBUYREC+2                                                
         MVI   P+23,C'-'                                                        
         MVI   P+29,C'-'                                                        
         MVC   HALF,SV70ELM+5                                                   
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+30(4),DUB                                                      
*                                                                               
         CLI   SV70ELM+10,C'N'                                                  
         BNE   LTST20                                                           
         MVC   P+35(3),=C'NEW'                                                  
         B     LTST30                                                           
*                                                                               
LTST20   CLI   SV70ELM+10,C'C'                                                  
         BNE   LTST25                                                           
         MVC   P+35(3),=C'CHA'                                                  
         B     LTST30                                                           
*                                                                               
LTST25   CLI   SV70ELM+10,C'D'                                                  
         BNE   LTST30                                                           
         MVC   P+35(3),=C'CAN'                                                  
*                                                                               
LTST30   GOTO1 DATCON,DMCB,(3,SV70ELM+2),(5,P+39)                               
         GOTO1 VPRINTIT                                                         
         B     RLMTH               DONE WITH REGULAR INS/ORD LAST I/O           
*                                                                               
*                                  WEBIO INS ORDER HANDLING                     
LTST45   DS    0H                  LAST I/O IS WEBIO                            
         LA    R6,SV70ELM                                                       
         USING PWIOELD,R6                                                       
*                                                                               
         LA    R7,P+22             START WEBIO NUMBER IN P+22                   
*                                                                               
         MVC   0(1,R7),PBUYREC+2   SET MEDIA                                    
*                                                                               
         AHI   R7,1                BUMP TO NEXT POSITION                        
*                                                                               
         SR    RF,RF                                                            
         IC    RF,PWIO#YER         GET IO# YEAR                                 
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  0(2,R7),DUB         SET YEAR                                     
*                                                                               
         AHI   R7,2                BUMP TO CLIENT PART                          
         MVC   0(3,R7),PBUYREC+4   SET CLIENT                                   
         AHI   R7,2                BUMP POINTER                                 
         CLI   0(R7),C' '          IF EMPTY                                     
         BH    *+8                                                              
         MVI   0(R7),C'-'             FILL IN WITH DASH                         
*                                                                               
         AHI   R7,1                 BUMP PAST LAST OF CODE                      
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,7,PWIO#SQ#       GET SEQUENCE NUMBER                          
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  0(4,R7),DUB         SEQUENCE NUMBER                              
*                                                                               
         AHI   R7,4                NEXT OUTPUT AREA                             
*                                                                               
         OC    PWIO#REV,PWIO#REV   SKIP IF NO REVISION NUMBER                   
         BZ    LTST45X             DONE WITH WEBIO NUMBER                       
*                                                                               
         MVC   0(3,R7),=C'REV'     REVISION INDICATOR                           
         AHI   R7,3                                                             
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,PWIO#REV       GET REVISION NUMBER                          
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  0(3,R7),DUB         SEQUENCE NUMBER                              
*                                                                               
         AHI   R7,3                NEXT OUTPUT AREA                             
*                                                                               
LTST45X  DS    0H                                                               
         GOTO1 DATCON,DMCB,(3,PWIODATE),(5,1(R7))                               
         GOTO1 VPRINTIT                                                         
*                                                                               
         DROP  R6                                                               
*                                                                               
RLMTH    DS    0H          PRINT UP TO 5 COMMENTS                               
         CLC   PBYOBFD(L'PBYOBFD),SPACES                                        
         BE    RL0                                                              
         MVC   P+13(L'PBYOBFD),PBYOBFD                                          
         GOTO1 VPRINTIT                                                         
*                                                                               
RL0      DS    0H                                                               
         MVI   COPYOR,C'N'                                                      
         MVI   CAPOR,C'N'                                                       
         LA    R3,PBYOCOMS                                                      
         LA    R4,5                                                             
RL1      CLC   0(47,R3),SPACES                                                  
         BE    RL2X                                                             
         CLC   0(5,R3),=C'COPY='    COPY OVERRIDE                               
         BNE   RL1C                                                             
         MVI   COPYOR,C'Y'                                                      
         B     RL1P                                                             
*                                                                               
RL1C     CLC   0(4,R3),=C'CAP='    CAPTION OVERRIDE                             
         BNE   RL1F                                                             
         MVI   CAPOR,C'Y'                                                       
         B     RL1P                                                             
*                                                                               
RL1F     CLI   PROGPROF+5,C'Y'     SEE IF DISPLAY REGULAR BUY COMMENTS          
         BE    RL1P                                                             
         B     RL1X                                                             
*                                                                               
RL1P     MVC   P+13(47),0(R3)                                                   
         GOTO1 VPRINTIT                                                         
RL1X     LA    R3,47(R3)                                                        
         BCT   R4,RL1                                                           
*                                                                               
         DROP  R2                                                               
*                                                                               
RL2X     DS    0H                                                               
         LA    R2,PPFILED+4095                                                  
         LA    R2,1(R2)                                                         
         USING PPFILED+4096,R2                                                  
         OC    PBDJOB,PBDJOB                                                    
         BZ    RLM2                                                             
*                                                                               
RLM1     DS    0H                                                               
         CLI   COPYOR,C'Y'       SEE IF COPY OVERRIDE PRINTED                   
         BE    RLM1A                                                            
         MVC   P+13(6),=C'COPY ='                                               
         MVC   P+13+7(17),PJOBCPY                                               
         GOTO1 VPRINTIT                                                         
         B     RLM1B                                                            
*                                                                               
RLM1A    DS    0H                                                               
*                                                                               
RLM1B    DS    0H                                                               
         CLI   CAPOR,C'Y'       SEE IF CAPTION OVERRIDDEN                       
         BE    RLM2                                                             
         MVC   P+13(25),PJOBCAP1                                                
         MVC   PSECOND+13(25),PJOBCAP2                                          
*                                                                               
         DROP  R2                                                               
*                                                                               
         GOTO1 VPRINTIT                                                         
*                                                                               
RLM2     DS    0H                                                               
         CLI   PROGPROF+6,C'Y'      SEE IF PRINT I/O COMMENTS                   
         BNE   RLM4                                                             
         LA    R2,BUYOUTA                                                       
         USING PPBYOUTD,R2                                                      
         MVI   PBYOCTL,X'24'                                                    
         MVI   PBYOCLT2,0                                                       
         GOTO1 PPBYOUT,DMCB,BUYOUTA                                             
*                                                                               
         LA    R3,PBYOCOMS                                                      
         LA    R4,5                                                             
RLM2B    CLC   0(47,R3),SPACES                                                  
         BE    RLM2X                                                            
*                                                                               
         CH    R4,=H'5'       SEE IF DOING FIRST COMMENT                        
         BNE   RLM2D                                                            
         GOTO1 VPRINTIT       SKIP BEFORE FIRST                                 
         MVC   P+0(12),=C'I/O COMMENTS'                                         
*                                                                               
RLM2D    DS    0H                                                               
         MVC   P+13(47),0(R3)                                                   
         GOTO1 VPRINTIT                                                         
RLM2X    LA    R3,47(R3)                                                        
         BCT   R4,RLM2B                                                         
*                                                                               
         DROP  R2                                                               
*                                                                               
*                                                                               
RLM4     DS    0H                                                               
         CLI   PROGPROF+7,C'Y'      SEE IF PRINTING POSITION COMMENTS           
         BNE   RLM5                                                             
         LA    R2,BUYOUTA                                                       
         USING PPBYOUTD,R2                                                      
         MVI   PBYOCTL,0                                                        
         MVI   PBYOCLT2,X'80'                                                   
         GOTO1 PPBYOUT,DMCB,BUYOUTA                                             
*                                                                               
         LA    R3,PBYOCOMS                                                      
         LA    R4,5                                                             
RLM4B    CLC   0(47,R3),SPACES                                                  
         BE    RLM4X                                                            
*                                                                               
         CH    R4,=H'5'        SEE IF DOING FIRST COMMENT                       
         BNE   RLM4C                                                            
         GOTO1 VPRINTIT        SKIP BEFORE FIRST COMMENT                        
         MVC   P+4(08),=C'POSITION'                                             
RLM4C    MVC   P+13(47),0(R3)                                                   
         GOTO1 VPRINTIT                                                         
RLM4X    LA    R3,47(R3)                                                        
         BCT   R4,RLM4B                                                         
*                                                                               
         DROP  R2                                                               
*                                                                               
RLM5     DS    0H                                                               
         GOTO1 VPRINTIT                                                         
*                                                                               
RLM7     DS    0H                NOW DO TEARSHEET COMMENTS                      
         MVI   TEARCOM,C'N'                                                     
         LA    R3,PBUYREC+33                                                    
         MVI   ELCODE,X'69'                                                     
RLM7C    BAS   RE,NEXTEL                                                        
         BNE   RLM7X                                                            
         ZIC   R1,1(R3)                                                         
         SH    R1,=H'3'                                                         
         BM    RLM7C                                                            
         CLI   TEARCOM,C'N'          SEE IF FIRST COMMENT                       
         BNE   *+10                                                             
         MVC   P+38(13),=C'TEAR COMMENTS'                                       
*                                                                               
         MVI   TEARCOM,C'Y'                                                     
         EX    R1,MOVCOM                                                        
         GOTO1 VPRINTIT                                                         
         B     RLM7C                                                            
*                                                                               
MOVCOM   MVC   P+53(0),2(R3)            EXECUTED                                
*                                                                               
RLM7X    DS    0H                                                               
         CLI   TEARCOM,C'Y'           SEE IF I PRINTED ANY                      
         BNE   RLM8                                                             
         GOTO1 VPRINTIT               SKIP A LINE AFTER                         
*                                                                               
RLM8     DS    0H                                                               
NEXTBUY  DS    0H                                                               
         B     EXT                                                              
         EJECT                                                                  
INITIAL  GOTO1 =V(IINITIAL)                                                     
         BR    R9                                                               
         EJECT                                                                  
*              ROUTINE TO FLAG BILLED/TRAFFICED ITEMS                           
TSTBLTR  NTR                                                                    
         LA    R3,PBDELEM                                                       
TSTB1    CLI   0(R3),X'26'                                                      
         BNE   TSTB3                                                            
         OC    5(3,R3),5(R3)       CK FOR ANY DATE                              
         BZ    TSTB4                                                            
         CLI   ASOFDTE,0                                                        
         BNE   TSTB2                                                            
         MVI   P+0,C'B'                                                         
         B     NEXTBEL                                                          
*                                                                               
TSTB2    CLC   5(3,R3),ASOFDTE     CHK VS. AS OF DATE                           
         BH    NEXTBEL                                                          
         MVI   P+0,C'B'                                                         
         B     NEXTBEL                                                          
*                                                                               
TSTB3    CLI   0(R3),X'25'         PAY ELEM                                     
         BNE   TSTB4                                                            
         OC    2(3,R3),2(R3)       CK FOR ANY DATE                              
         BZ    TSTB4                                                            
         CLI   ASOFDTE,0                                                        
         BNE   TSTB3C                                                           
         MVI   P+1,C'P'                                                         
         B     NEXTBEL                                                          
*                                                                               
TSTB3C   CLC   2(3,R3),ASOFDTE     CHK VS. AS OF DATE                           
         BH    NEXTBEL                                                          
         MVI   P+1,C'P'                                                         
         B     NEXTBEL                                                          
*                                                                               
TSTB4    CLI   0(R3),X'70'                                                      
         BE    TSTB5                                                            
         CLI   0(R3),X'71'                                                      
         BNE   NEXTBEL                                                          
TSTB5    OC    2(3,R3),2(R3)                                                    
         BZ    NEXTBEL                                                          
         CLI   ASOFDTE,0                                                        
         BNE   TSTB6                                                            
         MVI   P+2,C'T'                                                         
         B     NEXTBEL                                                          
*                                                                               
TSTB6    CLC   2(3,R3),ASOFDTE                                                  
         BH    NEXTBEL                                                          
         MVI   P+2,C'T'                                                         
*                                                                               
NEXTBEL  SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),0         END OF REC                                       
         BNE   TSTB1                                                            
*                                                                               
TSTBX    XIT                                                                    
EXT      XMOD1 1                                                                
         LTORG                                                                  
FWORK    DS    20F                                                 L01          
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
IINITIAL CSECT                                                                  
         NMOD1 0,INITL                                                          
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         USING TSWORKD,R5                                                       
         OI    DMINBTS,X'08'       SET TO PASS DELETES                          
         L     RF,=V(CLTEND)                                                    
         A     RF,RELO                                                          
         ST    RF,VCLTEND                                                       
         L     RF,=V(PRINTIT)                                                   
         A     RF,RELO                                                          
         ST    RF,VPRINTIT                                                      
         L     RF,=V(BLDMLST)                                                   
         A     RF,RELO                                                          
         ST    RF,VBLDMLST                                                      
         L     RF,=V(TSWORK)                                                    
         A     RF,RELO                                                          
         ST    RF,VTSWORK                                                       
         L     RF,=V(CLIF)                                                      
         A     RF,RELO                                                          
         ST    RF,VCLIFRST                                                      
         LA    R2,BUYOUTA                                                       
         USING PPBYOUTD,R2                                                      
         XC    PBYOINPT(24),PBYOINPT                                            
         LA    RE,PBUYREC                                                       
         ST    RE,0(R2)                                                         
         L     R6,DATCON                                                        
         LA    R7,GROSS                                                         
         STM   R6,R7,4(R2)                                                      
         MVI   PBYOCTL,X'28'                                                    
*                                                                               
         DROP  R2                                                               
*                                                                               
         MVI   FCRDBUY,C'Y'        RESET TO READ 20 POINTERS                    
         MVI   FCRDACTV,C'Y'       SET TO READ ACTIVE ONLY                      
         MVI   PRDSW,1                                                          
         CLC   QPRODUCT(3),=C'   '                                              
         BNE   *+12                                                             
         MVI   FCRDBUY,X'21'       USE 21 POINTERS                              
         MVI   PRDSW,0                                                          
*                                                                               
         MVI   FCGTJOB,C'Y'        SO PPG WILL GET JOBRECS                      
*                                                                               
INIT1    DS    0H                                                               
         MVI   FCGTREP,C'N'                                                     
         CLI   QOPT6,C'Y'          SEE IF SHOWING PAYING ADDRESS                
         BE    INIT2                                                            
         CLC   QSORT,=C'08'        OR SORTING BY REP                            
         BE    INIT2                                                            
         CLC   QSORT,=C'09'        OR SORTING BY PAY ADDR NAME                  
         BE    INIT2                                                            
         B     INIT2B                                                           
INIT2    MVI   FCGTREP,C'Y'                                                     
INIT2B   DS    0H                                                               
*                                                                               
INIT3    MVC   FCPDFILT,QOPT3      P=PAID,U=UNPAID                              
         CLI   QOPT3,C' '                                                       
         BNE   *+8                                                              
INIT4    MVI   FCPDFILT,C'N'       RESET TO N                                   
INIT8    MVC   PAGE,=H'1'                                                       
         MVI   REQERR,0                                                         
INOUTX   XIT                                                                    
         LTORG                                                                  
         EJECT                                                                  
BLDMLST  CSECT                                                                  
         NMOD1 0,BLDMLST                                                        
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         L     R5,VTSWORK                                                       
         USING TSWORKD,R5                                                       
         XC    ASOFDTE,ASOFDTE                                                  
         XC    ASDATE,ASDATE                                                    
         CLI   QPAY,C' '           AS OF DATE IN QPAY YYMMDD                    
         BE    BLDM5                                                            
*        GOTO1 DTCNV,DMCB,(0,QPAY),(1,ASOFDTE)                                  
         GOTO1 DATCON,DMCB,(0,QPAY),(3,ASOFDTE)                                 
*        GOTO1 DTCNV,DMCB,(1,ASOFDTE),(3,ASDATE)                                
         GOTO1 DATCON,DMCB,(3,ASOFDTE),(5,ASDATE)                               
*                                                                               
BLDM5    DS    0H                                                               
         MVI   MTHACT,0            CLEAR ACTIVITY INDICATORS                    
         MVI   PRDACT,0                                                         
         MVI   PUBACT,0                                                         
         MVI   REPACT,0                                                         
         MVI   CLTACT,0                                                         
*        GOTO1 DTCNV,DMCB,(0,QSTART),(1,REQST)                                  
         GOTO1 DATCON,DMCB,(0,QSTART),(3,REQST)                                 
*        GOTO1 DTCNV,DMCB,(0,QEND),(1,REQEND)                                   
         GOTO1 DATCON,DMCB,(0,QEND),(3,REQEND)                                  
*                                                                               
*                                                                               
BLMEXT   XIT1                                                                   
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
PRINTIT  CSECT                                                                  
         NMOD1 0,PRINTIT                                                        
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         L     R5,VTSWORK                                                       
         USING TSWORKD,R5                                                       
*                                                                               
         CLI   PRDSW,0            SEE IF DOING PRDS SEPERATELY                  
         BE    PRNT5                                                            
         MVC   HEAD5(7),=C'PRODUCT'                                             
         MVC   HEAD5+9(3),PPRDKPRD                                              
         MVC   HEAD5+13(20),PPRDNAME                                            
*                                                                               
PRNT5    DS    0H                                                               
*                                                                               
         MVI   RCSUBPRG,0                                                       
         CLC   QPROG,=C'TT'        SEE IF TEARSHEET TURNAROUND                  
         BNE   *+8                                                              
         MVI   RCSUBPRG,10                                                      
*                                                                               
         CLI   PROGPROF+11,C'Y'    SEE IF SUPPRESSING CHANGE DATA               
         BNE   CKTELE                                                           
         ZIC   R1,RCSUBPRG                                                      
         AH    R1,=H'1'                                                         
         STC   R1,RCSUBPRG                                                      
*                                                                               
CKTELE   DS    0H                                                               
         XC    HEAD8+53(30),HEAD8+53                                            
         XC    HEAD9+53(30),HEAD9+53                                            
*                                                                               
         LA    RF,PUBREC+33                                                     
         CLI   0(RF),X'11'                                                      
         BE    IS11EL                                                           
NOKILLA  ZIC   RE,1(RF)                                                         
         AR    RF,RE                                                            
         CLI   0(RF),0                                                          
         BE    IS11ELX                                                          
         CLI   0(RF),X'11'                                                      
         BNE   NOKILLA                                                          
         USING PPDUMD11,RF                                                      
*                                                                               
IS11EL   DS    0H                                                               
         LA    RE,HEAD8+53       TT REPORT                                      
         CLI   RCSUBPRG,1                                                       
         BH    *+8                                                              
         LA    RE,HEAD8+55       TS REPORT                                      
*                                                                               
         CLC   PUBTEL(12),SPACES                                                
         BNH   IS11EL5                                                          
         MVC   0(6,RE),=C'PHONE-'                                               
         MVC   7(12,RE),PUBTEL                                                  
         LA    RE,132(RE)                                                       
*                                                                               
IS11EL5  DS    0H                                                               
         CLC   PUBSFAXN,SPACES                                                  
         BNH   IS11ELX                                                          
         MVC   0(4,RE),=C'FAX-'                                                 
         MVC   5(12,RE),PUBSFAXN                                                
         DROP  RF                                                               
*                                                                               
IS11ELX  DS    0H                                                               
         CLI   QMEDIA,C'N'       SEE IF NEWSPAPERS                              
         BE    STCITY                                                           
         CLI   QMEDIA,C'O'       SEE IF OUTDOOR                                 
         BNE   CKBP                                                             
         CLC   PUBZNAME,SPACES   ONLY DISPLAY STATE, CITY IF NO ZONE            
         BH    CKBP                                                             
*                                                                               
STCITY   DS    0H                                                               
         MVC   HEAD10(2),PUBSTATE                                               
         MVI   HEAD10+2,C','                                                    
         MVC   HEAD10+4(L'PUBCITY),PUBCITY                                      
*                                                                               
CKBP     CLI   QBPDATE,C'B'                                                     
         BE    BILMSG                                                           
         CLI   QBPDATE,C'P'                                                     
         BNE   CKASOF                                                           
         MVC   HEAD4+54(19),=C'** PAYING PERIOD **'                             
         B     CKASOF                                                           
*                                                                               
BILMSG   MVC   HEAD4+53(20),=C'** BILLING PERIOD **'                            
*                                                                               
CKASOF   CLI   ASOFDTE,0                                                        
         BE    CKPD           AS OF DATE NOT USED                               
         MVC   HEAD5+45(5),=C'AS OF'                                            
         MVC   HEAD5+51(8),ASDATE                                               
*                                                                               
CKPD     DS    0H                                                               
         CLC   QOPT3(2),=C'  '   SEE IF PAID OR BILLED OPTIONS PRESENT          
         BE    CKBLX                                                            
         CLC   QOPT3(2),=C'PB'     SEE IF PAID AND BILLED                       
         BNE   CKPD5                                                            
         MVC   HEAD7+97(24),=C'*PAID BILLED ITEMS ONLY*'                        
         B     CKBLX                                                            
*                                                                               
CKPD5    CLC   QOPT3(2),=C'UU'     SEE IF UNPAID AND UNBILLED                   
         BNE   CKPD10                                                           
         MVC   HEAD7+97(28),=C'*UNPAID UNBILLED ITEMS ONLY*'                    
         B     CKBLX                                                            
*                                                                               
CKPD10   CLC   QOPT3(2),=C'UB'     SEE IF UNPAID AND BILLED                     
         BNE   CKPD15                                                           
         MVC   HEAD7+97(26),=C'*UNPAID BILLED ITEMS ONLY*'                      
         B     CKBLX                                                            
*                                                                               
CKPD15   CLC   QOPT3(2),=C'PU'     SEE IF PAID AND UNBILLED                     
         BNE   CKPD20                                                           
         MVC   HEAD7+97(26),=C'*PAID UNBILLED ITEMS ONLY*'                      
         B     CKBLX                                                            
*                                                                               
CKPD20   CLI   QOPT3,C' '                                                       
         BE    CKBL                                                             
         CLI   QOPT3,C'P'                                                       
         BNE   CKPD25                                                           
         MVC   HEAD7+97(17),=C'*PAID ITEMS ONLY*'                               
         B     CKBLX                                                            
*                                                                               
CKPD25   CLI   QOPT3,C'U'                                                       
         BNE   CKBL                                                             
         MVC   HEAD7+97(19),=C'*UNPAID ITEMS ONLY*'                             
         B     CKBLX                                                            
*                                                                               
CKBL     CLI   QOPT4,C' '                                                       
         BE    CKBLX                                                            
         CLI   QOPT4,C'B'                                                       
         BNE   CKBL5                                                            
         MVC   HEAD7+97(19),=C'*BILLED ITEMS ONLY*'                             
         B     CKBLX                                                            
*                                                                               
CKBL5    CLI   QOPT4,C'U'                                                       
         BNE   CKBLX                                                            
         MVC   HEAD7+97(21),=C'*UNBILLED ITEMS ONLY*'                           
         B     CKBLX                                                            
*                                                                               
CKBLX    DS    0H                                                               
         CLI   QOPT1,C'X'  SEE IF REPORTING ALL STATUS VALUES                   
         BE    PRINTX                                                           
         MVC   HEAD8+97(10),=C'*STATUS= *'                                      
         MVC   HEAD8+105(1),QOPT1                                               
         CLI   QOPT1,C' '       STATUS = BLANK                                  
         BNE   PRINTX                                                           
         MVC   HEAD8+105(12),=C'NOT ENTERED*'                                   
PRINTX   DS    0H                                                  L01          
PRINTX5  GOTO1 REPORT                                                           
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
CLTEND   CSECT                                                                  
         NMOD1 0,CLTEND                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         L     R5,VTSWORK                                                       
         USING TSWORKD,R5                                                       
         CLI   CLTACT,C'Y'                                                      
         BNE   CLTENDX                                                          
*                                                                               
CLTE15   MVI   FORCEHED,C'Y'                                                    
         MVI   PUBPSW,0                                                         
         MVC   P+1(20),=C' ** CLIENT TOTALS **'                                 
         GOTO1 VPRINTIT                                                         
*                                                                               
CLTENDX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
CLIF     CSECT                                                                  
         NMOD1 0,CLIF                                                           
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         L     R5,VTSWORK                                                       
         USING TSWORKD,R5                                                       
         MVI   FORCEHED,C'Y'                                                    
         XC    SAVEYMD,SAVEYMD                                                  
         XC    SAVEPRD,SAVEPRD                                                  
         XC    SAVEPUB,SAVEPUB                                                  
*                                                                               
CLIEXT   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
TSWORK   CSECT                                                                  
         DS    1000C                                                            
*                                                                               
*                                                                               
TSWORKD  DSECT                                                                  
MTHACT   DS    CL1                                                              
PRDACT   DS    CL1                                                              
PUBACT   DS    CL1                                                              
REPACT   DS    CL1                                                              
CLTACT   DS    CL1                                                              
SAVELINE DS    CL1                                                              
COPYOR   DS    CL1                                                              
CAPOR    DS    CL1                                                              
TEARCOM  DS    CL1                                                              
PLINESW  DS    XL1                                                              
HAVTEAR  DS    CL1      SET TO 'Y' IF BUY HAS A TEARSHEET ELEM                  
PUBSW    DS    CL1                                                              
PUBPSW   DS    CL1      X'01' IF PUB NAME PRINTED                               
PRDSW    DS    CL1      X'01' IF DOING PRDS SEPERATELY                          
LASTYM   DS    CL2                                                              
SAVEYMD  DS    CL3                                                              
SAVEPRD  DS    CL3                                                              
SAVEPUB  DS    CL6                                                              
PPGKEY   DS    CL32                                                             
SAVEKEY  DS    CL32                                                             
SAVEPKEY DS    CL32                                                             
STKEY    DS    CL32                                                             
REQERR   DS    CL1                                                              
REQST    DS    CL3                                                              
REQEND   DS    CL3                                                              
ASOFDTE  DS    CL3                    AS OF DATE YMD                            
ASDATE   DS    CL8           AS  OF DATE MMDD/YY                                
REQEST   DS    H                                                                
WKDUB    DS    PL8                                                              
REQPUB   DS    CL6                                                              
SVMEDCLI DS    CL4                 SAVED MEDIA/CLIENT                           
*                                                                               
SAVPLIN1 DS    CL132                                                            
SAVPLIN2 DS    CL132                                                            
*                                                                               
VCLIFRST DS    V                                                                
VPUBFRST DS    V                                                                
VCLTEND  DS    V                                                                
VPRINTIT DS    V                                                                
VBLDMLST DS    V                                                                
VTSWORK  DS    V                                                                
         DS    V                                                                
         DS    V                                                                
TOTALS   DS    6D                  WAS 5D                          L01          
         DS    0F                                                               
BUYOUTA  DS    600C                OUTPUT AREA FOR PPBUYOUT                     
         SPACE                                                                  
RELO     DS    F                                                                
*                                                                               
*                                                                               
BUYTOTS  DS    0D             BUY LINE TOTALS                                   
*                                                                               
PRDTOTS  DS    0D             PRODUCT TOTALS                                    
*                                                                               
PUBTOTS  DS    0D             PUB TOTALS                                        
*                                                                               
*                                                                               
         PRINT OFF                                                 L01          
       ++INCLUDE PPMODEQU                                                       
       ++INCLUDE PPWORKD                                                        
       ++INCLUDE PPNEWFILE                                                      
         PRINT ON                                                               
PPWORKD  DSECT                                                                  
         ORG     QPUBFREQ                                                       
QBUYLIN  DS      CL2                   BUY LINE NUMBER                          
*                                                                               
PTSHTELD DSECT                                                                  
       ++INCLUDE PTSHTEL                                                        
PWIOELD  DSECT                                                                  
       ++INCLUDE PPGENBYIO                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'059PPREPTS02 01/31/05'                                      
         END                                                                    
