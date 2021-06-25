*          DATA SET ACBUD03    AT LEVEL 030 AS OF 02/25/13                      
*PHASE T61003A                                                                  
         TITLE 'ACBUD03 - BUDGET PROGRAM - DISPLAY/CHECK/UPDATE'                
ACBUD03  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**BUD3**,RA,RR=RE                                              
*                                                                               
         USING GWS,R9                                                           
         USING TWAD,R8                                                          
*                                                                               
         L     RC,ASAVE                                                         
*                                                                               
         USING LOCALD,RC                                                        
*                                                                               
         L     RF,=A(GETACC)       SAVE A(GETACC)                               
         AR    RF,RE                                                            
         ST    RF,AGETACC                                                       
*                                                                               
WHCHMODE CLI   NEXTMODE,FRMFIRST   WHICH MODE/FORMAT                            
         BE    PRIM0                                                            
         CLI   NEXTMODE,INPFIRST                                                
         BE    PRIM0                                                            
         CLI   NEXTMODE,DISPMODE                                                
         BNE   WHCH02                                                           
         CLI   BUDFORM,C'P'                                                     
         BE    PERD0                                                            
         B     CACD0                                                            
*                                                                               
WHCH02   CLI   NEXTMODE,CHCKMODE                                                
         BE    CHCK0                                                            
         CLI   NEXTMODE,UPDTMODE                                                
         BE    *+6                                                              
         DC    H'0'                UNRECOGNISED MODE                            
         CLI   BUDFORM,C'P'                                                     
         BE    PERU0                                                            
         B     CACU0                                                            
         EJECT ,                                                                
* PRIMARY I/O CALL (ACC IF FORMAT=P/C, CAC IF FORMAT=A)                         
         SPACE 1                                                                
PRIM0    DS    0H                                                               
         GOTO1 AGETACC,APRIMIOC                                                 
         BE    PRIM06                                                           
         MVC   MSG(18),=C'ACTION COMPLETED -'                                   
         LA    R1,MSG+19                                                        
         CLI   NEXTMODE,FRMFIRST                                                
         MVI   NEXTMODE,FRMFIRST                                                
         BNE   PRIM2                                                            
         MVC   MSG(23),=C'NO RECORDS TO DISPLAY -'                              
         LA    R1,MSG+24                                                        
*                                                                               
PRIM2    MVC   0(17,R1),=C'ENTER NEXT ACTION'                                   
         LA    R1,VIRACTH                                                       
         ST    R1,FADR                                                          
         B     OKEND                                                            
*                                                                               
PRIM06   MVC   THISPRIC,ACCTWA     SAVE CODE + NAME                             
         MVC   THISPRIN,ACCNAM                                                  
*                                                                               
PRIM08   CLI   BUDFORM,C'P'                                                     
         BE    PERD0                                                            
         B     CACD0                                                            
         EJECT ,                                                                
* DISPLAY FOR PERIOD FORMAT                                                     
         SPACE 1                                                                
PERD0    DS    0H                  WHICH CONTRA                                 
         XC    HITLIST,HITLIST     CLEAR COLM-LN ACTIVITY LIST                  
*                                                                               
PERD01   GOTO1 AGETACC,ASECDIOC                                                 
         BNE   PRIM0               NONE - TRY FOR NEXT ACCOUNT                  
*&&US                                                                           
         BAS   RE,FCONOF           FILTER CONTRA OFFICE                         
         BNE   PERD01                                                           
*&&                                                                             
         MVC   THISSECC,ACCTWA                                                  
         MVC   THISSECN,ACCNAM                                                  
*                                                                               
PERD02   MVI   COUNT,0             INITIALISE W/S                               
         L     RE,AACCUMS                                                       
         LA    RF,14*40                                                         
         XCEF                                                                   
*                                                                               
         USING BUDTD,R7                                                         
         USING ACKEYD,R5                                                        
         USING IOCBD,RF                                                         
*                                                                               
         LA    R5,KEY              BUILD BASIC BUDGET KEY                       
         LA    R7,BUDTAB                                                        
*                                                                               
         XC    ACBTKEY,ACBTKEY                                                  
         MVI   ACBTKTYP,ACBTKTEQ                                                
         LA    RF,ACCNTRL                                                       
         LLC   R1,IOCDS15                                                       
         LA    R1,IOCNOWKY(R1)                                                  
*                                                                               
         DROP  RF                                                               
*                                                                               
         MVC   ACBTKACC,0(R1)                                                   
         MVC   ACBTKWRK,SPACES                                                  
         CLI   OFFILT+1,C' '                                                    
         BNH   *+10                                                             
         MVC   ACBTKWRK,OFFILT                                                  
         MVC   ACBTKCON,ACCODE     RETURNED BY CONTRA GETACC CALL               
         L     R5,ABUDREC                                                       
*                                                                               
PERD04   MVC   KEY+(ACBTKBNO-ACBTKEY)(2),BUDTNUM                                
         CLC   ACBTKEY,KEY                                                      
         BE    PERD08              WE HAVE RECORD ALREADY                       
         GOTO1 ARDHI,ABUDREC                                                    
*                                                                               
PERD05   TM    DMCB+8,X'FD'        SKIP DELETES                                 
         BNZ   EXIT                                                             
         TM    DMCB+8,2                                                         
         BNO   PERD05A                                                          
         L     RF,ASEQ                                                          
         BASR  RE,RF                                                            
         B     PERD05                                                           
*                                                                               
PERD05A  CLC   ACBTKEY(ACBTKBNO-ACBTKEY),KEYSAVE                                
         BNE   PERD25              WRONG A/C+C/A                                
*                                                                               
PERD06   CLC   ACBTKBNO,BUDTNUM    CHECK BUDGET TYPE                            
         BE    PERD08                                                           
         BL    PERD04                                                           
         LA    R7,BUDTLEN(,R7)                                                  
         CLI   0(R7),X'FF'                                                      
         BNE   PERD06                                                           
         B     PERD25                                                           
*                                                                               
PERD08   MVI   COUNT,C'Y'          MATCHING KEY                                 
         LA    R6,ACRECORD                                                      
*                                                                               
PERD09   CLI   0(R6),0             PROCESS BUDGET AMOUNT EL                     
         BE    PERD20                                                           
         CLI   0(R6),ACBAELEQ                                                   
         BNE   PERD15                                                           
*                                                                               
         USING ACBAD,R6                                                         
*                                                                               
         MVC   HALF,ACBAMNTH                                                    
         BAS   RE,DATOMM           CONVERT PWOS TO BINARY MONTHS                
         CLC   HALF,BUDTSRCE                                                    
         BL    PERD15                                                           
         LLC   R0,BUDTMNUM                                                      
         BCTR  R0,0                                                             
         AH    R0,BUDTSRCE                                                      
         CH    R0,HALF                                                          
         BL    PERD20                                                           
         LH    RE,HALF             FIND ACCUMULATOR FOR IT                      
         SH    RE,BUDTSRCE                                                      
         SRDL  RE,32                                                            
         LLC   R1,MPERLINE                                                      
         DR    RE,R1               RF = ACCUM LINE NUM                          
         LA    RE,L'ACCUMS                                                      
         MR    RE,RE                                                            
         A     RF,AACCUMS        RF = A(ACCUM LINE)                             
         IC    RE,BUDTCOL                                                       
         BCTR  RE,0                                                             
         SLL   RE,3                RE = DISP INTO LINE                          
         AR    RF,RE                                                            
         ZAP   DUB1,ACBABUDG                                                    
         MVN   DUB1+6(1),DUB1+7    CONVERT TO POUNDS                            
         ZAP   DUB,DUB1(7)                                                      
         BAS   RE,UPLIFT                                                        
*                                                                               
PERD14   OC    0(8,RF),0(RF)       MOVE OR ADD                                  
         BNZ   PERD14A                                                          
         ZAP   0(8,RF),DUB                                                      
         B     PERD15                                                           
*                                                                               
PERD14A  AP    0(8,RF),DUB                                                      
*                                                                               
PERD15   LLC   RE,ACBALEN          BUMP TO NEXT EL                              
         AR    R6,RE                                                            
         B     PERD09                                                           
*                                                                               
PERD20   LA    R7,BUDTLEN(,R7)     END OF REC - BUMP BUDTAB                     
         CLI   0(R7),X'FF'                                                      
         BNE   PERD04                                                           
*                                                                               
PERD25   CLI   COUNT,0             END OF BUDTAB - CHECK FOR HITS               
         BNE   PERD27              IF NONE & NOT ADD - SKIP TO NEXT             
         CLI   ACTION,ADD          CONTRA                                       
         BNE   PERD0                                                            
*                                                                               
PERD27   OC    THISPRIC,THISPRIC   DISPLAY ACCOUNT CODE/NAME                    
         BZ    PERD28                                                           
         MVC   BUDACC,SPACES                                                    
         MVC   BUDACC(L'THISPRIC),THISPRIC                                      
         BAS   RE,DISPOFF          APPEND OFFICE FILTER IF ANY                  
         MVC   BUDACCN,THISPRIN                                                 
         OI    BUDACCH+6,TRANSMIT                                               
         OI    BUDACCNH+6,TRANSMIT                                              
*                                                                               
PERD28   OC    THISSECC,THISSECC   DISPLAY CONTRA CODE/NAME                     
         BZ    PERD30                                                           
         MVC   BUDCAC,SPACES                                                    
         MVC   BUDCAC(L'THISSECC),THISSECC                                      
         MVC   BUDCACN(34),=C'WARNING - CONTRA ACCOUNT NOT FOUND'               
         CLC   THISSECN,SPACES                                                  
         BNH   *+10                                                             
         MVC   BUDCACN,THISSECN                                                 
         OI    BUDCACH+6,TRANSMIT                                               
         OI    BUDCACNH+6,TRANSMIT                                              
*                                                                               
PERD30   TM    SAVSTAT,TOTAL       ADD TOTAL LINE                               
         BNO   PERD40                                                           
         CLI   MPERLINE,12                                                      
         BNL   PERD40                                                           
         BAS   RE,TOTUP                                                         
*                                                                               
PERD40   BAS   RE,DISPVAL                                                       
         B     DISPALL             GO TO SELECT NEXT MODE                       
*                                                                               
         DROP  R5,R6,R7                                                         
         EJECT ,                                                                
* DISPLAY FOR CONTRA OR ACCOUNT FORMAT                                          
         SPACE 1                                                                
CACD0    MVI   COUNT,0             INITIALISE W/S                               
         XC    HITLIST,HITLIST     CLEAR COLM-LN ACTIVITY LIST                  
         L     RE,AACCUMS                                                       
         LA    RF,14*40                                                         
         XCEF                                                                   
         L     R6,AACCUMS          R6 = ACCUMULATOR LINE POINTER                
         LH    R3,DFSTFLDH         R3 = TWA FLD HDR POINTER                     
         AR    R3,R8                                                            
*                                                                               
CACD02   TM    SAVSTAT,INPUTISY                                                 
         BO    CACD04                                                           
         GOTO1 AGETACC,ASECDIOC                                                 
*&&US                                                                           
         BNE   CACD34              END OF CONTRA                                
         BAS   RE,FCONOF           FILTER CONTRA OFFICE                         
         BE    CACD06              OK,                                          
         B     CACD02              NO, GET NEXT                                 
*&&                                                                             
*&&DO                                                                           
         BE    CACD06                                                           
         B     CACD34                                                           
*&&                                                                             
*                                                                               
CACD04   MVI   NEXTMODE,DISPMODE   CHECK INPUT ACCOUNT FOR INPUT=Y              
         GOTO1 AFVAL,(R3)                                                       
         BNE   CACD04A                                                          
         C     R6,AACCUMS          IF 1ST LINE MISSING                          
         BE    CACD05              ERROR                                        
         B     CACD32                                                           
*                                                                               
CACD04A  MVC   ACCTWA,FLD                                                       
         GOTO1 AGETACC,ASECDIOC                                                 
*&&US                                                                           
         BNE   CACD05              END OF CONTRA                                
         BAS   RE,FCONOF           FILTER CONTRA OFFICE                         
         BE    CACD05A             OK,                                          
         MVC   MSG,SPACES                                                       
         MVC   MSG(L'OFFMESS),OFFMESS                                           
         MVI   FERN,SPECIAL                                                     
         B     ERRXIT              NO, ERROR                                    
*&&                                                                             
*&&DO                                                                           
         BE    CACD05A                                                          
*&&                                                                             
*                                                                               
CACD05   GOTO1 APROTVAL            INVALID LEDGER OR WRONG LEVEL                
         B     EXIT                                                             
*                                                                               
CACD05A  MVI   COUNT,C'Y'                                                       
*                                                                               
CACD06   MVC   THISSECC,ACCTWA     SAVE SECONDARY CODE AND NAME                 
         MVC   THISSECN,ACCNAM                                                  
         CLI   ACTION,ADD                                                       
         BNE   CACD08                                                           
         MVI   COUNT,C'Y'                                                       
*                                                                               
         USING ACKEYD,R5                                                        
         USING BUDTD,R7                                                         
         USING IOCBD,RF                                                         
*                                                                               
CACD08   LA    R7,BUDTAB           POINT TO BUDTAB AND BUILD BASIC KEY          
         LA    R5,KEY              BUILD BASIC BUDGET KEY                       
         XC    ACBTKEY,ACBTKEY                                                  
         MVI   ACBTKTYP,ACBTKTEQ                                                
         LA    RF,ACCNTRL                                                       
         LLC   R1,IOCDS15                                                       
         LA    R1,IOCNOWKY(R1)                                                  
         MVC   ACBTKACC,0(R1)                                                   
         MVC   ACBTKWRK,SPACES                                                  
         CLI   OFFILT+1,C' '                                                    
         BNH   *+10                                                             
         MVC   ACBTKWRK,OFFILT                                                  
         LA    RF,CACNTRL                                                       
         LLC   R1,IOCDS15                                                       
         LA    R1,IOCNOWKY(R1)                                                  
*                                                                               
         DROP  RF                                                               
*                                                                               
         MVC   ACBTKCON,0(R1)                                                   
         L     R5,ABUDREC                                                       
*                                                                               
CACD10   DS    0H                  COMPLETE KEY WITH BUDTAB TYPE NUMBER         
         MVC   KEY+(ACBTKBNO-ACBTKEY)(2),BUDTNUM                                
         CLC   ACBTKEY(ACBTKBNO-ACBTKEY),KEY                                    
         BNE   CACD10A                                                          
         CLC   ACBTKBNO,BUDTNUM                                                 
         BNL   CACD14              WEVE GOT IT ALREADY                          
*                                                                               
CACD10A  GOTO1 ARDHI,ABUDREC                                                    
*                                                                               
CACD11   TM    DMCB+8,X'FD'        SKIP DELETES                                 
         BNZ   EXIT                                                             
         TM    DMCB+8,2                                                         
         BNO   CACD12                                                           
         L     RF,ASEQ                                                          
         BASR  RE,RF                                                            
         B     CACD11                                                           
*                                                                               
CACD12   CLC   ACBTKEY(ACBTKBNO-ACBTKEY),KEY                                    
         BNE   CACD28              WRONG ACC/CAC                                
*                                                                               
CACD14   TM    SAVSTAT,NAME        SAVE NAME IF NEEDED                          
         BNO   CACD16                                                           
         GOTO1 AGETNAME,ABUDREC                                                 
         CLC   WORK(36),SPACES                                                  
         BE    CACD16                                                           
         MVC   THISSECN,WORK                                                    
*                                                                               
CACD16   CLC   ACBTKBNO,BUDTNUM    CHECK BUDGET TYPE                            
         BE    CACD18                                                           
         BL    CACD10                                                           
         BH    CACD26                                                           
*                                                                               
CACD18   MVI   COUNT,C'Y'                                                       
         LA    R4,ACRECORD                                                      
*                                                                               
CACD22   CLI   0(R4),0             FIND REQUIRED AMOUNT EL                      
         BE    CACD26                                                           
         CLI   0(R4),ACBAELEQ                                                   
         BNE   CACD24                                                           
*                                                                               
         USING ACBAD,R4                                                         
*                                                                               
         MVC   HALF,ACBAMNTH                                                    
         BAS   RE,DATOMM           CONVERT TO BINARY MONTHS                     
         CLC   HALF,BUDTSRCE                                                    
         BL    CACD24                                                           
         LLC   R0,BUDTMNUM                                                      
         BCTR  R0,0                                                             
         AH    R0,BUDTSRCE                                                      
         CH    R0,HALF                                                          
         BL    CACD26                                                           
         ZAP   DUB1,ACBABUDG                                                    
         MVN   DUB1+6(1),DUB1+7    CONVERT TO POUNDS                            
         ZAP   DUB,DUB1(7)                                                      
         BAS   RE,UPLIFT           GET AMOUNT (+ UPLIFT) INTO DUB               
         LLC   RF,BUDTCOL          FIND ACCUM FOR COLUMN                        
         BCTR  RF,0                                                             
         SLL   RF,3                                                             
         AR    RF,R6                                                            
         OC    0(8,RF),0(RF)       MOVE OR ADD                                  
         BNZ   CACD23                                                           
         ZAP   0(8,RF),DUB                                                      
         B     CACD24                                                           
*                                                                               
CACD23   AP    0(8,RF),DUB                                                      
*                                                                               
CACD24   LLC   RE,ACBALEN          BUMP EL                                      
         AR    R4,RE                                                            
         B     CACD22                                                           
*                                                                               
CACD26   LA    R7,BUDTLEN(,R7)     BUMP BUDTAB                                  
         CLI   0(R7),X'FF'                                                      
         BNE   CACD10                                                           
*                                                                               
CACD28   CLI   COUNT,0             AT END FOR THIS ACC/CAC, CHECK FOR           
         BE    CACD02              HITS - NONE - IGNORE THIS SECONDARY          
         OC    THISPRIC,THISPRIC                                                
         BZ    CACD30                                                           
         LA    R1,BUDACCH          DISPLAY PRIMARY CODE & NAME 1ST TIME         
         CLI   BUDFORM,C'C'                                                     
         BE    *+8                                                              
         LA    R1,BUDCACH                                                       
         OI    6(R1),TRANSMIT                                                   
         LLC   RF,0(,R1)                                                        
         SH    RF,=H'9'                                                         
         EXMVC RF,8(R1),SPACES                                                  
         MVC   8(L'THISPRIC,R1),THISPRIC                                        
         CLI   BUDFORM,C'C'                                                     
         BNE   *+8                                                              
         BAS   RE,DISPOFF          APPEND OFFICE FILTER TO PRIMARY A/C          
         LA    R1,9(RF,R1)                                                      
         MVC   8(36,R1),THISPRIN                                                
         OI    6(R1),TRANSMIT                                                   
         XC    THISPRIC,THISPRIC                                                
*                                                                               
CACD30   MVC   8(14,R3),THISSECC   DISPLAY SECONDARY CODE                       
         OI    6(R3),TRANSMIT                                                   
         TM    SAVSTAT,NAME        AND NAME IF REQUIRED                         
         BNO   CACD32                                                           
         LLC   RF,0(,R3)                                                        
         AR    RF,R3                                                            
         MVI   8(RF),C'*'                                                       
         MVC   9(36,RF),THISSECN                                                
         OI    4(RF),VALPREV                                                    
         OI    6(RF),TRANSMIT                                                   
*                                                                               
CACD32   LA    R6,L'ACCUMS(,R6)    BUMP ACCUM AND TWA POINTERS                  
         SR    R4,R4                                                            
         LLC   R1,FPERLINE                                                      
         OI    4(R3),VALPREV                                                    
*                                                                               
CACD33   IC    R4,0(,R3)                                                        
         AR    R3,R4                                                            
         CLI   0(R3),9             CHECK FOR EOS TAB FLD                        
         BE    CACD38                                                           
         BCT   R1,CACD33                                                        
         MVI   COUNT,0                                                          
         B     CACD02                                                           
*                                                                               
CACD34   LH    RF,DFSTFLDH         AT END OF SECONDARY KEYS FOR PRIMARY         
         AR    RF,R8               IF NOTHING TO DISPLAY, GO FOR NEXT           
         CR    RF,R3               PRIMARY                                      
         BE    PRIM0                                                            
         LH    R5,DLSTFLDH                                                      
         AR    R5,R8                                                            
         BCTR  R5,0                                                             
*                                                                               
CACD36   LLC   R4,0(R3)            OTHERWISE CLEAR REST OF SCREEN               
         SH    R4,=H'9'                                                         
         EX    R4,*+8                                                           
         B     *+10                                                             
         XC    8(0,R3),8(R3)                                                    
         OI    4(R3),VALPREV                                                    
         OI    6(R3),TRANSMIT+PROTECT                                           
         LA    R4,9(,R4)                                                        
         BXLE  R3,R4,CACD36                                                     
*                                                                               
CACD38   TM    SAVSTAT,TOTAL       TOTAL ACROSS                                 
         BNO   CACD40                                                           
         CLI   BUDTAB+BUDTLEN,X'FF'                                             
         BE    CACD40                                                           
         BAS   RE,TOTUP                                                         
*                                                                               
CACD40   BAS   RE,DISPVAL          DISPLAY VALUES                               
         B     DISPALL                                                          
*                                                                               
         DROP  R4,R5,R7                                                         
         EJECT ,                                                                
* FINAL ACTIONS FOR DISPLAY MODE                                                
         SPACE 1                                                                
DISPALL  MVC   MSG(19),=C'BUDGETS DISPLAYED -'                                  
         CLI   ACTION,DIS                                                       
         BNE   DISA20                                                           
*                                                                               
DISA02   MVC   MYTEMP,ACCNTRL      DISPLAY - LOOK AHEAD TO SEE IF               
         TM    SAVSTAT,INPUTISY    SEQUENCE HAS FINISHED                        
         BO    DISA04                                                           
         GOTO1 AGETACC,ASECDIOC                                                 
         BE    DISA06                                                           
*                                                                               
DISA04   GOTO1 AGETACC,APRIMIOC                                                 
         BE    DISA06                                                           
         MVI   NEXTMODE,FRMFIRST   IT HAS                                       
         MVC   MSG+20(17),=C'ENTER NEXT ACTION'                                 
         LA    R1,VIRACTH                                                       
         ST    R1,FADR                                                          
         B     OKEND                                                            
*                                                                               
DISA06   MVC   ACCNTRL(L'MYTEMP),MYTEMP MORE TO COME (PROBABLY)                 
         MVC   MSG+20(18),=C'HIT ENTER FOR NEXT'                                
         OI    VIRACTH+6,X'81'                                                  
         LH    R1,DLSTFLDH                                                      
         AR    R1,R8                                                            
         ST    R1,FADR             CURSOR ON TAB                                
         MVI   NEXTMODE,DISPMODE                                                
         TM    SAVSTAT,INPUTISY                                                 
         BNO   OKEND                                                            
         MVI   NEXTMODE,INPFIRST                                                
         B     OKEND                                                            
*                                                                               
DISA20   DS    0H                  ADD OR CHANGE                                
         MVC   MSG+20(17),=C'ENTER NEW AMOUNTS'                                 
         TM    SAVSTAT,NAME                                                     
         BNO   *+10                                                             
         MVC   MSG+30(17),=C'NAMES AND AMOUNTS'                                 
         MVI   NEXTMODE,CHCKMODE                                                
         LH    R1,DFSTFLDH                                                      
         AR    R1,R8                                                            
         SR    R0,R0                                                            
*                                                                               
DISA20A  TM    1(R1),PROTECT       CURSOR ON 1ST UNP IN MATRIX                  
         BNO   DISA20B                                                          
         IC    R0,0(,R1)                                                        
         AR    R1,R0                                                            
         B     DISA20A                                                          
*                                                                               
DISA20B  TM    SAVSTAT,INPUTISY    OR 2ND IF INPUT=YES                          
         BNO   DISA22                                                           
*                                                                               
DISA21   IC    R0,0(,R1)                                                        
         AR    R1,R0                                                            
         TM    1(R1),PROTECT                                                    
         BO    DISA21                                                           
*                                                                               
DISA22   ST    R1,FADR                                                          
         B     OKEND                                                            
         EJECT ,                                                                
* CHECK VALUES FOR ALL FORMATS                                                  
         SPACE 1                                                                
CHCK0    LH    R3,DFSTFLDH         'NEXT' IN 1ST UNPROT MEANS SKIP              
         AR    R3,R8               THIS SCREENFUL                               
         SR    R4,R4                                                            
         IC    R4,0(,R3)                                                        
         AR    R3,R4               SKIP LINE TITLE                              
         TM    SAVSTAT,NAME                                                     
         BNO   CHCK00A                                                          
         IC    R4,0(,R3)                                                        
         AR    R3,R4               AND NAME COL IF ANY                          
*                                                                               
CHCK00A  TM    1(R3),PROTECT                                                    
         BNO   CHCK00B                                                          
         IC    R4,0(,R3)                                                        
         AR    R3,R4                                                            
         B     CHCK00A                                                          
*                                                                               
CHCK00B  CLI   8(R3),C'S'          'SKIP' MEANS SKIP TO NEXT                    
         BNE   CHCK00C             PRIME ACCOUNT                                
         MVI   NEXTMODE,DISPMODE                                                
         B     PRIM0                                                            
*                                                                               
CHCK00C  CLI   8(R3),C'N'                                                       
         BNE   CHCK01                                                           
         CLI   BUDFORM,C'P'                                                     
         BE    PERU18                                                           
         B     CACU20                                                           
*                                                                               
CHCK01   BAS   RE,ACCVAL           CHECK VALS AND GET INTO ACCUMS               
         BNE   EXIT                ERROR                                        
         MVI   NEXTMODE,UPDTMODE                                                
         TM    SAVSTAT,TOTAL       IF NO TOTALS GO AND UPDATE                   
         BO    CHCK02                                                           
         CLI   BUDFORM,C'P'                                                     
         BE    PERU02                                                           
         B     CACU02                                                           
*                                                                               
CHCK02   BAS   RE,TOTUP            TOTALS                                       
         BAS   RE,DISPVAL          RE-DISPLAY                                   
         LH    R1,DLSTFLDH                                                      
         AR    R1,R8                                                            
         ST    R1,FADR             CURSOR ON TAB FLD                            
         OI    VIRACTH+6,X'81'     MODIFY INPUT                                 
         MVC   MSG(L'TOTALMSG),TOTALMSG                                         
         B     OKEND                                                            
         EJECT ,                                                                
* UPDATE FOR PERIOD FORMAT                                                      
         SPACE 1                                                                
         USING ACKEYD,R5                                                        
         USING IOCBD,RF                                                         
         SPACE 1                                                                
PERU0    BAS   RE,ACCVAL           GET VALUES INTO ACCUMS IF NOT THERE          
*                                                                               
PERU02   LA    R5,KEY              BUILD BASIC BUDGET KEY                       
         XC    ACBTKEY,ACBTKEY                                                  
         MVI   ACBTKTYP,ACBTKTEQ                                                
         LA    RF,ACCNTRL                                                       
         LLC   R1,IOCDS15                                                       
         LA    R1,IOCNOWKY(R1)                                                  
         MVC   ACBTKACC,0(R1)                                                   
         MVC   ACBTKWRK,SPACES                                                  
         CLI   OFFILT+1,C' '                                                    
         BNH   *+10                                                             
         MVC   ACBTKWRK,OFFILT                                                  
         LA    RF,CACNTRL                                                       
         LLC   R1,IOCDS15                                                       
         LA    R1,IOCNOWKY(R1)                                                  
         MVC   ACBTKCON,0(R1)                                                   
         LA    R7,BUDTAB           POINT TO BUDGET TABLE                        
*                                                                               
         DROP  RF                                                               
*                                                                               
         USING BUDTD,R7                                                         
*                                                                               
PERU04   TM    BUDTSTAT,PROTECT    LOOP FOR BUDGET TYPE                         
         BO    PERU14              IGNORE IF PROT                               
         MVC   ACBTKBNO,BUDTNUM                                                 
         L     R5,ABUDREC                                                       
         CLI   WRITPEND,0                                                       
         BE    PERU06                                                           
         CLC   ACBTKEY,KEY                                                      
         BE    PERU08                                                           
         BAS   RE,WRITBUDG                                                      
         BNE   EXIT                                                             
*                                                                               
PERU06   GOTO1 AREADL,ABUDREC      READ AND LOCK                                
         MVI   WRITPEND,C'W'                                                    
         BE    PERU08                                                           
         MVI   ACSTATUS,0                                                       
         TM    DMCB+8,X'10'                                                     
         BZ    PERU08                                                           
         MVC   ACBTKEY,KEY         NOT FOUND - BUILD SKELETON                   
         XC    ACLENGTH(9),ACLENGTH                                             
         MVI   ACLENGTH+1,L'ACBTKEY+8                                           
         MVI   WRITPEND,C'A'       ADD PENDING                                  
*                                                                               
PERU08   LH    R3,BUDTSTA          R3/4/5 = BXLE REGS                           
         LLC   R4,MPERLINE                                                      
         LLC   R5,BUDTMNUM                                                      
         AH    R5,BUDTSTA                                                       
         BCTR  R5,0                                                             
         LLC   R6,BUDTCOL          R6 = A(1ST ACCUM FOR COLUMN)                 
         BCTR  R6,0                                                             
         SLL   R6,3                                                             
         A     R6,AACCUMS                                                       
*                                                                               
PERU10   OC    0(8,R6),0(R6)       MOVE VALUE INTO RECORD                       
         BZ    PERU12                                                           
*                                                                               
*DETERMINE IF ACTIVITY HAS OCCURED FOR CURRENT VALUE-IF NOT DO NOT              
*REPHASE                                                                        
         GOTO1 HITCHK,(R6)                                                      
         BNO   PERU12              NO ACTIVITY LOOP FOR NEXT                    
*                                                                               
         STH   R3,HALF1                                                         
         GOTO1 PUTVAL,DMCB,((R4),HALF1),(R6)                                    
PERU12   LA    R6,L'ACCUMS(,R6)                                                 
         BXLE  R3,R4,PERU10                                                     
*                                                                               
PERU14   LA    R7,BUDTLEN(,R7)     BUMP BUDGET TABLE                            
         LA    R5,KEY                                                           
         CLI   0(R7),X'FF'                                                      
         BNE   PERU04                                                           
*                                                                               
PERU16   CLI   WRITPEND,0          FINAL WRITE                                  
         BE    PERU18                                                           
         BAS   RE,WRITBUDG                                                      
         BNE   EXIT                                                             
*                                                                               
PERU18   MVI   NEXTMODE,DISPMODE   THEN GO AND GET NEXT DISPLAY                 
         B     PERD0                                                            
*                                                                               
         DROP  R5,R7                                                            
         EJECT ,                                                                
* UPDATE FOR CONTRA OR ACCOUNT FORMAT                                           
         SPACE 1                                                                
         USING ACKEYD,R5                                                        
         USING IOCBD,RF                                                         
         SPACE 1                                                                
CACU0    BAS   RE,ACCVAL           GET VALUES INTO ACCUMS IF NOT THERE          
*                                                                               
CACU02   LA    R5,KEY              BUILD BASIC BUDGET KEY                       
         XC    ACBTKEY,ACBTKEY                                                  
         MVI   ACBTKTYP,ACBTKTEQ                                                
         MVC   ACBTKWRK,SPACES                                                  
         CLI   OFFILT+1,C' '                                                    
         BNH   *+10                                                             
         MVC   ACBTKWRK,OFFILT                                                  
         L     RF,APRIMIOC         MOVE IN PRIMARY ACCOUNT                      
         LLC   R1,IOCDS14                                                       
         LA    R1,IOCNOWKY(R1)                                                  
         LA    R4,ACBTKACC                                                      
         LA    R2,ACBTKCON                                                      
         CLI   BUDFORM,C'C'                                                     
         BE    CACU02A                                                          
         LA    R1,BUDCAC           ACC FORMAT MUST TAKE C/A FROM SCREEN         
         LR    R2,R4               SWITCH R4 AND R2                             
         LA    R4,ACBTKCON                                                      
*                                                                               
CACU02A  BAS   RE,CACSET           MOVE CODE FROM (R1) INTO (R4)                
         LR    R4,R2               SAVE A(SECONDARY ACC IN KEY) IN R4           
         LH    R3,DFSTFLDH         R3 = A(SECONDARY ACCOUNT FLD HDR)            
         AR    R3,R8                                                            
         L     R6,AACCUMS          R6 = A(ACCUM LINE)                           
*                                                                               
CACU03   GOTO1 AFVAL,(R3)          BUILD SECONDARY ACCOUNT KEY                  
         BZ    CACU16              MISSING                                      
         LA    R7,BUDTAB           R7 = A(BUDTAB)                               
*                                                                               
         USING BUDTD,R7                                                         
*                                                                               
         LA    R1,FLD                                                           
         L     RF,ASECDIOC                                                      
         BAS   RE,CACSET                                                        
*&&DO                                                                           
         CLC   ACBTKWRK,SPACES     TEST NEW OFFICES                             
         BE    CACU04              NO,                                          
         CLC   ACBTKACC+1(2),=C'1C'                                             
         BNE   CACU04                                                           
         TM    IOCSTAT,IOCSCLDG    SPECIAL CONTRA LEDGER                        
         BNO   CACU04                                                           
         LLC   R1,IOCOFPOS         GET OFFPOS                                   
         LA    R1,ACBTKCON+2(R1)                                                
         CLC   0(2,R1),SPACES                                                   
         BE    CACU04                                                           
         CLC   0(2,R1),ACBTKWRK                                                 
         BE    *+6                                                              
         DC    H'0'                                                             
*&&                                                                             
*                                                                               
CACU04   TM    SAVSTAT,NAME        COMPLETE KEY - UNLESS ITS A PROT COL         
         BO    CACU05              AND NO NAME OPTION                           
         TM    BUDTSTAT,PROTECT                                                 
         BO    CACU14                                                           
*                                                                               
CACU05   MVC   ACBTKBNO,BUDTNUM                                                 
         L     R5,ABUDREC                                                       
         CLI   WRITPEND,0                                                       
         BE    CACU06                                                           
         CLC   ACBTKEY,KEY                                                      
         BE    CACU08                                                           
         BAS   RE,WRITBUDG                                                      
         BNE   EXIT                                                             
*                                                                               
CACU06   GOTO1 AREADL,ABUDREC      READ AND LOCK                                
         MVI   WRITPEND,C'W'                                                    
         MVI   ACSTATUS,0                                                       
         TM    DMCB+8,X'10'                                                     
         BZ    CACU08                                                           
         MVC   ACBTKEY,KEY         NOT FOUND - BUILD SKELETON                   
         XC    ACLENGTH(9),ACLENGTH                                             
         MVI   ACLENGTH+1,L'ACBTKEY+8                                           
         MVI   WRITPEND,C'A'       ADD PENDING                                  
*                                                                               
CACU08   TM    BUDTSTAT,PROTECT    FIND ACCUM FOR COLUMN                        
         BO    CACU12                                                           
         LLC   R2,BUDTCOL                                                       
         BCTR  R2,0                                                             
         SLL   R2,3                                                             
         AR    R2,R6                                                            
*                                                                               
CACU10   OC    0(8,R2),0(R2)       MOVE VALUE INTO RECORD                       
         BZ    CACU11                                                           
*                                                                               
*DETERMINE IF ACTIVITY HAS OCCURED FOR CURRENT VALUE-IF NOT DO NOT              
*REPHASE                                                                        
         GOTO1 HITCHK,(R2)                                                      
         BNO   CACU11              NO ACTIVITY LOOP FOR NEXT                    
*                                                                               
         GOTO1 PUTVAL,DMCB,(BUDTMNUM,BUDTSTA),(R2)                              
CACU11   TM    SAVSTAT,NAME                                                     
         BNO   CACU14                                                           
*                                                                               
CACU12   LLC   R2,0(,R3)           HANDLE NAME OPTION                           
         AR    R2,R3               R2 = A(NAME FLD HDR)                         
         CLI   8(R2),C'*'                                                       
         BE    CACU14              ONLY UPDATE IF LEADING * HAS GONE            
         GOTO1 VHELLO,DMCB,(C'G',ACCOUNT),(X'20',ABUDREC),0                     
         CLI   12(R1),0                                                         
         BNE   CACU13                                                           
         MVI   0(R1),C'D'          DELETE NAME EL IF ANY                        
         BASR  RE,RF                                                            
*                                                                               
CACU13   CLI   5(R2),0                                                          
         BE    CACU14                                                           
         LA    RE,WORK             BUILD NEW EL IN WORK                         
*                                                                               
         USING ACNAMED,RE                                                       
*                                                                               
         MVI   ACNMEL,X'20'                                                     
         MVC   ACNMNAME,8(R2)                                                   
         LLC   R0,5(,R2)                                                        
         AH    R0,=H'2'                                                         
         STC   R0,ACNMLEN                                                       
         ST    RE,8(,R1)                                                        
         MVI   0(R1),C'P'          PUT NEW EL                                   
         BASR  RE,RF                                                            
*                                                                               
         DROP  RE                                                               
*                                                                               
CACU14   LA    R7,BUDTLEN(,R7)     BUMP BUDGET TABLE                            
         LA    R5,KEY                                                           
         CLI   0(R7),X'FF'                                                      
         BNE   CACU04                                                           
*                                                                               
CACU16   LA    R6,L'ACCUMS(,R6)    BUMP TO NEXT LINE                            
         LLC   R0,FPERLINE         BY BUMPING N FLDS                            
         SR    R1,R1                                                            
*                                                                               
CACU17   IC    R1,0(,R3)                                                        
         AR    R3,R1                                                            
         BCT   R0,CACU17                                                        
         CLI   0(R3),9             CHECK FOR EOS TAB FLD                        
         BNE   CACU03                                                           
*                                                                               
CACU18   CLI   WRITPEND,0          FINAL WRITE                                  
         BE    CACU20                                                           
         BAS   RE,WRITBUDG                                                      
         BNE   EXIT                                                             
*                                                                               
CACU20   MVI   NEXTMODE,DISPMODE   THEN GO AND GET NEXT DISPLAY                 
         TM    SAVSTAT,INPUTISY                                                 
         BO    PRIM0                                                            
         B     CACD0                                                            
         EJECT ,                                                                
         SPACE 1                                                                
CACSET   NTR1                      SET UP 15 BYTE AC/CAC CODE IN (R4)           
         MVI   0(R4),C' '          USING U/L/A IN (R1)                          
         MVC   1(14,R4),0(R1)                                                   
         OC    1(14,R4),SPACES                                                  
         CLC   1(14,R4),SPACES                                                  
         BE    EXIT                'ALL' C/A'S                                  
         CLC   1(4,R4),=CL4'ALL'                                                
         BNE   CACSET10                                                         
         MVC   0(15,R4),SPACES                                                  
         B     EXIT                                                             
*                                                                               
CACSET10 MVC   0(1,R4),COMPANY     OTHERWISE COMPANY PREFIX                     
         LLC   R5,IOCDS14          CHECK FOR NON-STANDARD                       
         BCTR  R5,0                INDICATED BY IOCDS14-IOCDS15 NEQ 1           
         LLC   R2,IOCDS15                                                       
         SR    R5,R2                                                            
         BZ    EXIT                                                             
         MVC   0(15,R4),SPACES     IN WHICH CASE                                
         LA    R2,1(R4,R5)         MOVE PSEUDO-U/L/A FURTHER DOWN KEY           
         LA    RE,13                                                            
         SR    RE,R5                                                            
         EXMVC RE,0(R2),0(R1)                                                   
         B     EXIT                                                             
*                                                                               
         DROP  R5,R7,RF                                                         
         EJECT ,                                                                
* UPLIFT A VALUE BY PERCENTAGE IN BUDTPCNT                                      
*                                                                               
* PACKED VALUE IS IN DUB AND A (BUDGET TABLE ENTRY) IN R7                       
* RETURNS UPLIFTED VALUE IN DUB                                                 
         SPACE 1                                                                
         USING BUDTD,R7                                                         
         SPACE 1                                                                
UPLIFT   DS    0H                                                               
         CLI   BUDTPCNT,0          ANY UPLIFT                                   
         BER   RE                  NO                                           
         LLC   R1,BUDTPCNT                                                      
         TM    BUDTSTAT,NEGATIVE   CALCULATE 100 + OR - PCNT                    
         BNO   *+6                                                              
         LNR   R1,R1                                                            
         AH    R1,=H'100'                                                       
         ZAP   DIV,DUB                                                          
         CVD   R1,DUB                                                           
         MP    DIV,DUB                                                          
         ZAP   DUB,=P'50'          ROUND                                        
         CP    DIV,=P'0'                                                        
         BNL   *+10                                                             
         MP    DUB,=P'1'                                                        
         AP    DIV,DUB                                                          
         DP    DIV,=PL8'100'                                                    
         ZAP   DUB,DIV(8)                                                       
         BR    RE                                                               
*                                                                               
         DROP  R7                                                               
         EJECT ,                                                                
* CONVERT PWOS DATE TO BINARY MONTHS,                                           
*        I.E. (YEAR TIMES (NUMBER OF MONTH IN A PERIOD)) + MONTH - 1            
*                                                                               
* HALF CONTAINS DATE ON INPUT AND RETURN                                        
         SPACE 1                                                                
DATOMM   NTR1                                                                   
         MVC   WORK(2),HALF        PACKED YYMM                                  
         MVI   WORK+2,1            TO     BINARY YYMM01                         
         GOTO1 VDATCON,DMCB,(1,WORK),(3,WORK+3)                                 
         LLC   R1,WORK+3                                                        
         LA    R0,13                                                            
         TM    SAVSTAT,THIRTEEN    13     PERIOD BUDGET TYPES IN USE ?          
         BO    *+6                 YES,   SKIP                                  
         BCTR  R0,0                12     PERIOD BUDGET TYPES                   
         MR    R0,R0                                                            
         IC    R0,WORK+4                                                        
         BCTR  R0,0                                                             
         AR    R1,R0                                                            
         STH   R1,HALF                                                          
         B     EXIT                                                             
         EJECT ,                                                                
* APPEND OFFICE FILTER IF ANY TO ACCOUNT CODE                                   
         SPACE 1                                                                
DISPOFF  CLI   OFFILT,C' '                                                      
         BNHR  RE                  NO FILTER                                    
         NTR1                                                                   
         LA    R1,BUDACC+13        FIND END OF A/C CODE                         
*                                                                               
DISPOF10 CLI   0(R1),C' '                                                       
         BH    DISPOF20                                                         
         BCT   R1,DISPOF10                                                      
*                                                                               
DISPOF20 TM    COMPSTA4,X'01'      TEST NEW OFFICES                             
         BO    OKXIT                                                            
         MVC   1(4,R1),=C',OF='    TACK ON 'OF=X'                               
         MVC   5(1,R1),OFFILT                                                   
         B     OKXIT                                                            
         EJECT ,                                                                
* DISPLAY VALUES IN ACCUMULATOR BLOCK                                           
         SPACE 1                                                                
DISPVAL  NTR1                                                                   
         LLC   R2,FPERLINE         R2 = COUNT OF VALUES PER LINE                
         BCTR  R2,0                                                             
         LH    R3,DFSTFLDH         R3/4/5 = BXLE REGS FOR LINE CONTROL          
         AR    R3,R8                                                            
         LLC   R4,0(,R3)                                                        
         AR    R3,R4                                                            
         LH    R5,DLSTFLDH                                                      
         AR    R5,R8                                                            
         BCTR  R5,0                                                             
         L     R6,AACCUMS          R6/E = ACCUM POINTERS                        
*                                                                               
DISPV2   LR    RE,R6               LOOP FOR A LINE                              
         LR    RF,R2                                                            
         TM    SAVSTAT,NAME        IF NAME OPTION BUMP TO NEXT FLD              
         BO    DISPV8                                                           
*                                                                               
DISPV4   XC    8(10,R3),8(R3)      LOOP FOR A VALUE                             
         OC    0(8,RE),0(RE)                                                    
         BZ    DISPV6                                                           
         CLI   0(RE),C'D'                                                       
         BNE   DISPV4A                                                          
         MVC   8(6,R3),=C'DELETE'                                               
         B     DISPV6                                                           
*                                                                               
DISPV4A  MVC   DIV(8),0(RE)                                                     
         TM    SAVSTAT,THOUSAND    DOES TYPE SHOW VALUES IN 1000'S              
         BNO   DISPV5                                                           
         EDIT  (P8,DIV),(10,8(R3)),3,ALIGN=LEFT,FLOAT=-                         
         LA    R1,7(,R3)                                                        
         AR    R1,R0               DROP NON-SIGNIFICANT DEC PLACES              
*                                                                               
DISPV4B  CLI   0(R1),C'.'                                                       
         BE    DISPV4C                                                          
         CLI   0(R1),C'0'                                                       
         BNE   DISPV5A                                                          
         MVI   0(R1),C' '                                                       
         BCT   R1,DISPV4B                                                       
*                                                                               
DISPV4C  MVI   0(R1),C' '                                                       
         B     DISPV5A                                                          
*                                                                               
DISPV5   EDIT  (P8,DIV),(10,8(R3)),ALIGN=LEFT,FLOAT=-                           
*                                                                               
DISPV5A  CLI   8(R3),C' '                                                       
         BNE   DISPV6                                                           
         OI    8(R3),C'0'                                                       
*                                                                               
DISPV6   LA    RE,8(,RE)           BUMP ACCUM                                   
*                                                                               
DISPV8   OI    4(R3),VALPREV       SET FLD HDR INDICS AND BUMP                  
         OI    6(R3),TRANSMIT                                                   
         IC    R4,0(,R3)                                                        
         AR    R3,R4                                                            
         BCT   RF,DISPV4                                                        
*                                                                               
DISPV10  LA    R6,L'ACCUMS(,R6)    AT END OF LINE                               
         IC    R4,0(,R3)           BUMP PAST LINE TITLE                         
         BXLE  R3,R4,DISPV2                                                     
         B     EXIT                                                             
         EJECT ,                                                                
* UPDATE BUDGET RECORD WITH A BUDGET AMOUNT                                     
*                                                                               
* P1 B0   = NUMBER OF MONTHS OVER WHICH AMOUNT IS TO BE SPREAD                  
*    B1-3 = A(START MM AS A BINARY HALF WORD)                                   
* P2      = A(PACKED AMOUNT)                                                    
         SPACE 1                                                                
         USING ACBAD,R5                                                         
         SPACE 1                                                                
PUTVAL   NTR1  WORK=(R2,2)                                                      
         MVC   0(8,R2),0(R1)                                                    
         LM    R3,R4,0(R1)                                                      
         LA    R6,VALLIST                                                       
         MVC   FULL(2),0(R3)       CONVERT START MM TO PWOS IN WORK             
         LH    R0,FULL                                                          
         SRDL  R0,32                                                            
         LA    RF,13                                                            
         TM    SAVSTAT,THIRTEEN                                                 
         BO    *+6                                                              
         BCTR  RF,0                                                             
         DR    R0,RF                                                            
         AH    R0,=H'1'                                                         
         STC   R1,FULL                                                          
         STC   R0,FULL+1                                                        
         MVI   FULL+2,1                                                         
         GOTO1 VDATCON,DMCB,(3,FULL),(1,WORK1)                                  
         MVC   VALLIST(8),0(R4)    MOVE AMOUNT TO VALLIST STRING                
         CLI   0(R2),1             NO PHASING IF 1 MONTH                        
         BE    PUTV05                                                           
         CLI   VALLIST,C'D'        OR DELETE                                    
         BE    PUTV05                                                           
         ZAP   DIV,0(8,R4)         OTHERWISE DIVIDE AMOUNT BY MONTHS            
         LLC   R0,0(,R2)                                                        
         CVD   R0,DUB                                                           
         DP    DIV,DUB                                                          
*                                                                               
PUTV00   ZAP   0(8,R6),DIV(8)      AND MOVE RESULT INTO VALLIST STRING          
         LA    R6,8(,R6)                                                        
         BCT   R0,PUTV00                                                        
         CP    DIV+8(8),=P'0'      IF REMAINDER, SPREAD ACROSS 1ST N            
         BE    PUTV05              VALUES IN STRING - $1 EACH                   
         ZAP   DUB,DIV+8(8)                                                     
         CVB   R0,DUB                                                           
         LA    RF,PUTVAP           EX AP                                        
         LTR   R0,R0                                                            
         BP    PUTV01                                                           
         LA    RF,PUTVSP           EX SP                                        
         LCR   R0,R0                                                            
*                                                                               
PUTV01   LA    R6,VALLIST                                                       
         CLI   PROGPROF,C'L'       OR LAST N VALUES                             
         BNE   PUTV02                                                           
         LLC   R6,0(,R2)                                                        
         SR    R6,R0                                                            
         SLL   R6,3                                                             
         LA    R6,VALLIST(R6)                                                   
*                                                                               
PUTV02   SR    R1,R1                                                            
*                                                                               
PUTV04   EX    R1,0(,RF)                                                        
         LA    R6,8(,R6)                                                        
         BCT   R0,PUTV04                                                        
*                                                                               
PUTV05   LA    R6,VALLIST                                                       
         LLC   R0,0(,R2)                                                        
*                                                                               
PUTV10   DS    0H                  LOOP TO HANDLE A VALUE IN THE STRING         
         GOTO1 VHELLO,DMCB,(C'G',ACCOUNT),(X'1D',ABUDREC),(2,WORK1)             
         CLI   12(R1),0                                                         
         BE    PUTV12                                                           
         CLI   0(R6),C'D'          NOT FOUND IS OK IF DELETE                    
         BE    PUTV16                                                           
         LA    R5,WORK             OTHERWISE ADD A ZERO VALUE ONE               
         MVI   ACBAEL,ACBAELEQ                                                  
         MVI   ACBALEN,ACBALNEQ                                                 
         MVC   ACBAMNTH,WORK1                                                   
         ZAP   ACBABUDG,=P'0'                                                   
         GOTO1 VHELLO,DMCB,(C'P',ACCOUNT),ABUDREC,ACBAD                         
         B     PUTV10                                                           
*                                                                               
PUTV12   L     R5,12(,R1)          POINT TO ELEMENT                             
         CLI   0(R6),C'D'          MARK ELS FOR LATER DELETION                  
         BNE   PUTV14                                                           
         MVI   ACBAEL,X'FF'                                                     
         MVI   DELPEND,C'Y'                                                     
         B     PUTV16                                                           
*                                                                               
PUTV14   ZAP   ACBABUDG,0(8,R6)    REPLACE VALUE                                
         MP    ACBABUDG,=P'100'    INPENNIES                                    
         LA    R6,8(,R6)           BUMP TO NEXT ONE                             
*                                                                               
PUTV16   DS    0H                  BUMP TO   NEXT MONTH                         
         CLI   WORK1+1,X'13'       AT   EXTRA     MONTH ?                       
         BNE   PUTV16A             NO,  SKIP                                    
         MVI   WORK1+1,X'12'       RESET     MONTH                              
         B     PUTV16B             ADD  ONE  MONTH                              
*                                                                               
PUTV16A  DS    0H                                                               
         CLI   WORK1+1,X'12'       AT   LAST MONTH     IN   YEAR ?              
         BNE   PUTV16B             NO,  SKIP                                    
         TM    SAVSTAT,THIRTEEN    13   PERIOD    BUDGET ?                      
         BZ    PUTV16B             NO,  ADD  ONE  MONTH                         
         MVI   WORK1+1,X'13'       SAY  EXTRA     MONTH                         
         B     PUTV18              CONTINUE                                     
*                                                                               
PUTV16B  DS    0H                  ADD  ONE  MONTH                              
         GOTO1 VDATCON,DMCB,(1,WORK1),(0,WORK1+3)      CHAR YYMM01              
         GOTO1 VADDAY,DMCB,(C'M',WORK1+3),WORK1+3,1    NEXT MONTH               
         GOTO1 VDATCON,DMCB,(0,WORK1+3),(1,WORK1)      PACKED                   
*                                                                               
PUTV18   DS    0H                  ANY  MORE MONTH ?                            
         BCT   R0,PUTV10           LOOP ON   MONTH     COUNT                    
*                                                                               
PUTV20   CLI   DELPEND,C'Y'        DELETE ANY ELS MARKED FOR DELETION           
         BNE   EXIT                                                             
         MVI   DELPEND,0                                                        
         GOTO1 VHELLO,DMCB,(C'D',ACCOUNT),(X'FF',ABUDREC),0                     
*                                                                               
*                                  MAKE SURE THERE IS AT LEAST 1 ELEM           
         GOTO1 VHELLO,DMCB,(C'G',ACCOUNT),(X'1D',ABUDREC),0                     
         CLI   12(R1),X'06'        NO MORE X'1D' ELEMENTS?                      
         BNE   EXIT                                                             
*                                                                               
*                                  IF THERE ISN'T MAKE SURE DUMMY EXIST         
         GOTO1 VHELLO,DMCB,(C'G',ACCOUNT),(X'FE',ABUDREC),0                     
         CLI   12(R1),X'06'        NO DUMMY ELEMENT?                            
         BNE   EXIT                                                             
         MVC   FULL,=X'FE030000'   NONE, MAKE A DUMMY ELEMENT                   
         GOTO1 VHELLO,DMCB,(C'P',ACCOUNT),ABUDREC,FULL                          
*                                                                               
         B     EXIT                                                             
*                                                                               
         DROP  R5                                                               
*                                                                               
PUTVAP   AP    0(8,R6),=P'1'                                                    
*                                                                               
PUTVSP   SP    0(8,R6),=P'1'                                                    
         EJECT ,                                                                
* GET AMOUNTS FROM TWA INTO ACCUMS, CHECKING VALIDITY                           
*                                                                               
* RETURN CC=NEQ IF ERROR AND FERN & FADR SET                                    
         SPACE 1                                                                
ACCVAL   NTR1                                                                   
         LH    R3,DFSTFLDH         R3/4/5 = BXLE REGS FOR LINE CONTROL          
         AR    R3,R8                                                            
         SR    R4,R4                                                            
         LH    R5,DLSTFLDH                                                      
         AR    R5,R8                                                            
         BCTR  R5,0                                                             
         LLC   R2,FPERLINE         R2 = NUMBER OF VALS PER LINE EX TOTS         
         BCTR  R2,0                TITLE FLD                                    
         MVI   FLAG,0              FLAG=T FOR TOTAL COLUMN                      
         CLI   BUDFORM,C'P'                                                     
         BE    ACCV02                                                           
         TM    SAVSTAT,TOTAL                                                    
         BNO   ACCV02                                                           
         CLI   BUDTAB+BUDTLEN,X'FF'                                             
         BE    ACCV02                                                           
         BCTR  R2,0                                                             
         MVI   FLAG,C'T'                                                        
ACCV02   L     R7,AACCUMS          R7 = ACCUM LINE POINTER                      
         LR    R6,R7               R6 = ACCUM POINTER                           
         LR    RE,R6                                                            
         LA    RF,14*40                                                         
         XCEF                                                                   
*                                                                               
ACCV05   CLC   8(5,R3),TOTALMSG    LOOP FOR A LINE                              
         BE    EXIT                                                             
         IC    R4,0(,R3)           SKIP TITLE FLD                               
         AR    R3,R4                                                            
         LR    R0,R2               SET FLD PER LINE COUNT                       
         TM    SAVSTAT,NAME                                                     
         BO    ACCV15              SKIP NAME FLD                                
*                                                                               
ACCV06   LLC   R1,0(,R3)           HANDLE A FLD - MUST BE ONE OF                
         SH    R1,=H'8'            NULL,'DELETE' OR A +/- INTEGER               
         LA    RF,7(R1,R3)         OR IF IN 000'S A NUM TO 3 DP'S               
*                                                                               
ACCV06A  CLI   0(RF),C' '                                                       
         BH    ACCV06B                                                          
         BCTR  RF,0                                                             
         BCT   R1,ACCV06A                                                       
*                                                                               
ACCV06B  LTR   RE,R1               RE=R1 = SIGNIFICANT LEN OF UN/PROT           
         BZ    ACCV10                                                           
         LA    RF,8(,R3)                                                        
         CLI   8(R3),C'-'          CHECK FOR NEGATIVE                           
         BNE   ACCV06C                                                          
         LA    RF,9(,R3)                                                        
         BCTR  R1,0                                                             
*                                                                               
ACCV06C  BCTR  R1,0                                                             
         MVI   FERN,INVNUM                                                      
         LTR   R1,R1                                                            
         BM    ACCVERR                                                          
         MVC   WORK(10),=10C'0'                                                 
         EX    R1,EXMVZ                                                         
         CLC   WORK(10),=10C'0'                                                 
         BNE   ACCV07                                                           
         EX    R1,EXPACK                                                        
         TM    SAVSTAT,THOUSAND    CHECK FOR UNITS IN THOUSANDS                 
         BNO   ACCV06D                                                          
         MP    DIV,=P'1000'                                                     
*                                                                               
ACCV06D  OC    DIV(11),DIV         INVALID IF GREATER THAN 999 MILLION          
         MVI   FERN,TOOBIG                                                      
         BNZ   ACCVERR                                                          
         CLI   8(R3),C'-'                                                       
         BNE   *+10                                                             
         MP    DIV,=P'-1'                                                       
         ZAP   0(8,R6),DIV                                                      
         B     ACCV10                                                           
*                                                                               
EXMVZ    MVZ   WORK(0),0(RF)                                                    
*                                                                               
EXPACK   PACK  DIV,0(0,RF)                                                      
*                                                                               
ACCV07   CLC   8(6,R3),=C'DELETE'  CHECK FOR DELETE                             
         BNE   ACCV08                                                           
         MVI   FERN,INVALID                                                     
         CLI   ACTION,ADD                                                       
         BE    ACCVERR                                                          
         MVI   0(R6),C'D'          PUT D IN ACCUM FOR DELETE                    
         B     ACCV10                                                           
*                                                                               
ACCV08   TM    SAVSTAT,THOUSAND    IF IN 000'S ALLOW 3 DP'S                     
         BNO   ACCVERR                                                          
         LA    RF,8(,R3)                                                        
         ST    RE,DMCB+4           SIGNIFICANT LENGTH                           
         GOTO1 VCASHVAL,DMCB,(3,(RF))                                           
         CLI   0(R1),X'FF'                                                      
         BE    ACCVERR                                                          
         L     R1,4(,R1)                                                        
         LPR   R1,R1               R1 CONTAINS POSITIVE UNITS                   
         CVD   R1,DUB                                                           
         ZAP   DIV,DUB                                                          
         B     ACCV06D                                                          
*                                                                               
ACCV10   TM    4(R3),VALPREV       ANY ACTIVITY                                 
         BO    ACCV12              NO                                           
         GOTO1 HITSET,(R6)  SET COLUMN ROW INDICATION TDA FOR FLD               
*                                                                               
ACCV12   LA    R6,8(,R6)           BUMP ACCUM                                   
*                                                                               
ACCV15   IC    R4,0(,R3)           BUMP TWA                                     
         BXLE  R3,R4,ACCV16                                                     
         B     OKXIT               END OF SCREEN                                
*                                                                               
ACCV16   BCT   R0,ACCV06                                                        
         LA    R7,L'ACCUMS(,R7)    END OF LINE                                  
         LR    R6,R7                                                            
         CLI   FLAG,C'T'           ONE MORE FLD FOR TOTAL COL                   
         BNE   ACCV05                                                           
         BXLE  R3,R4,ACCV05                                                     
         B     OKXIT                                                            
*                                                                               
ACCVERR  NI    4(R3),ALL-VALPREV   ERROR EXIT                                   
         ST    R3,FADR                                                          
         B     ERRXIT                                                           
         EJECT ,                                                                
* TOTAL N ACCUMULATOR LINE/COLS INTO LINE/COL N+1                               
         SPACE 1                                                                
TOTUP    NTR1                                                                   
         LLC   R1,FPERLINE         SET UP REGS FOR CROSS TOTALS                 
         SH    R1,=H'2'                                                         
         TM    SAVSTAT,NAME                                                     
         BNO   *+6                                                              
         BCTR  R1,0                R1 = NUMBER OF COLS BEFORE TOTAL             
         LA    R2,13               R2 = NUMBER OF LINES                         
         LA    R6,8                R6/R7 = BUMP FACTORS                         
         LA    R7,L'ACCUMS                                                      
         L     RE,AACCUMS                                                       
         CLI   BUDFORM,C'P'        MODIFY FOR DOWN TOTALS (PERIOD FORM)         
         BNE   TOTUP0                                                           
         LA    R2,1(,R1)           R2 = NUMBER OF COLS (NO TOTAL COL)           
         LLC   R1,BUDTAB+(BUDTMNUM-BUDTD)                                       
         SR    R0,R0                                                            
         LLC   RF,MPERLINE                                                      
         DR    R0,RF               R1 = NUMBER OF LINES BEFORE TOTAL            
         LR    R6,R7                                                            
         LA    R7,8                SWAP BUMP FACTORS                            
*                                                                               
TOTUP0   LTR   R1,R1               NO TOTALS REQUIRED                           
         BZ    EXIT                                                             
*                                                                               
TOTUP1   LR    RF,RE                                                            
         LR    R0,R1                                                            
         ZAP   DUB,=P'0'                                                        
         SR    R3,R3               R3 = HIT IND                                 
*                                                                               
TOTUP2   OC    0(8,RF),0(RF)                                                    
         BZ    TOTUP3                                                           
         LA    R3,1                                                             
         CLI   0(RF),C'D'          DELETE                                       
         BE    TOTUP3                                                           
         AP    DUB,0(8,RF)                                                      
*                                                                               
TOTUP3   AR    RF,R6                                                            
         BCT   R0,TOTUP2                                                        
         XC    0(8,RF),0(RF)                                                    
         LTR   R3,R3               ANY HITS                                     
         BZ    *+10                                                             
         ZAP   0(8,RF),DUB         TOTAL INTO N+1                               
         AR    RE,R7                                                            
         BCT   R2,TOTUP1                                                        
         B     EXIT                                                             
         EJECT ,                                                                
* WRITE OR ADD A BUDGET RECORD DEPENDING ON WRITPEND (W OR A)                   
         SPACE 1                                                                
         USING ACKEYD,R5                                                        
         SPACE 1                                                                
WRITBUDG NTR1                                                                   
         MVC   KEYSAVE,KEY                                                      
         L     R5,ABUDREC                                                       
         CLI   WRITPEND,C'A'       DONT ADD IF NO ELEMENTS                      
         BNE   WRTP00                                                           
         CLI   ACRECORD,0                                                       
         BE    WRTP03                                                           
         B     WRTP01                                                           
*                                                                               
WRTP00   L     RE,ATIA             DONT WRITE BACK IF NO CHANGES                
         CLC   ACLENGTH,ACLENGTH-ACKEYD(RE)                                     
         BNE   WRTP01                                                           
         LH    RF,ACLENGTH                                                      
         LR    R0,R5                                                            
         LR    R1,RF                                                            
         CLCL  R0,RE                                                            
         BE    WRTP03                                                           
*                                                                               
WRTP01   DS    0H                  CHECK FOR NOT MORE THAN 36 MONTHS            
         LA    R6,ACRECORD                                                      
         LR    RE,R6                                                            
         SR    R0,R0                                                            
*                                                                               
WRTP01A  CLI   0(RE),ACBAELEQ      FIND 1ST NON-AMOUNT EL                       
         BNE   WRTP01B                                                          
         IC    R0,1(,RE)                                                        
         AR    RE,R0                                                            
         B     WRTP01A                                                          
*                                                                               
WRTP01B  SR    RE,R6               RE = TOTAL SIZE OF AMOUNT ELS                
         LA    R1,ACBALNEQ                                                      
         MH    R1,=Y(MONLIMIT)                                                  
         SR    RE,R1               RE = EXCESS OVER MAX SIZE                    
         BNP   WRTP02              OK                                           
         LH    R7,ACLENGTH                                                      
         SR    R7,RE                                                            
         STH   R7,ACLENGTH         REDUCE LENGTH                                
         AR    RE,R6                                                            
         LR    RF,R7                                                            
         MVCL  R6,RE               SHUFFLE BACK RECORD                          
*                                                                               
WRTP02   MVC   KEY,ACBTKEY         ADD OR WRITE                                 
         LA    R1,ABUDREC                                                       
         L     RF,AADD                                                          
         CLI   WRITPEND,C'A'                                                    
         BE    *+8                                                              
         L     RF,AWRITE                                                        
         BASR  RE,RF                                                            
*                                                                               
         CLI   ACBTKWRK,C' '       TEST FOR OFFICE BUDGETS                      
         BNH   WRTP03              NO,                                          
         BAS   RE,ADDOFA           ADD OFFICE/ACCOUNT RECORD                    
*                                                                               
WRTP03   MVI   WRITPEND,0                                                       
         MVC   KEY,KEYSAVE                                                      
         B     EXIT                CC SET BY ADD/WRITE                          
*                                                                               
         DROP  R5                                                               
***********************************************************************         
* ADD OFFICE/ACCOUNT RECORD AND PASSIVE IF NOT ON FILE                *         
***********************************************************************         
ADDOFA   NTR1  ,                                                                
         LA    R5,KEY                                                           
         USING ACKEYD,R5                                                        
         LA    R4,DKEY                                                          
         USING ACTRECD,R4                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCULA,ACBTKACC    FIRST READ ACCOUNT                          
         GOTO1 VDATAMGR,DMCB,DMREAD,ACCDIR,DKEY,DREC                            
         CLI   8(R1),0             TEST EVERYTHING OK                           
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R4,DREC                                                          
         TM    ACTKSTAT,ACTSABLP   TEST BALANCE ELEMENT                         
         BNO   EXIT                NO, NO NEED TO GO FURTHER                    
                                                                                
         LA    R4,DKEY             NOW LOOK FOR OFFICE/ACCOUNT                  
         USING OFARECD,R4                                                       
         MVC   OFAKEY,SPACES                                                    
         MVC   OFAKCULA,ACBTKACC   ACCOUNT                                      
         MVC   OFAKOFF,ACBTKWRK    OFFICE                                       
         GOTO1 VDATAMGR,DMCB,DMREAD,ACCDIR,DKEY,DREC                            
         CLI   8(R1),0             TEST EVERYTHING OK                           
         BE    EXIT                YES,                                         
         CLI   8(R1),X'10'         TEST RECORD NOT FOUND                        
         BE    *+6                 YES, THAT'S OK                               
         DC    H'0'                SOME OTHER I/O ERROR                         
                                                                                
         L     R4,AIOAREA3         BUILD ACCOUNT/ OFFICE RECORD                 
         XC    OFARECD(100),OFARECD                                             
         MVC   OFAKEY,DKEY                                                      
                                                                                
         LA    R3,WORK                                                          
         USING ABLELD,R3                                                        
         XC    ABLEL(ABLLN3Q),ABLEL                                             
         MVI   ABLEL,ABLELQ                                                     
         MVI   ABLLN,ABLLN3Q                                                    
         ZAP   ABLFRWD,=P'0'                                                    
         ZAP   ABLDR,=P'0'                                                      
         ZAP   ABLCR,=P'0'                                                      
         ZAP   ABLURG,=P'0'                                                     
         GOTOR VHELLO,DMCB,(C'P',ACCMST),AIOAREA3,ABLELD,0                      
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R3                                                               
                                                                                
         XC    DA,DA                                                            
         GOTO1 VDATAMGR,DMCB,ADDREC,ACCMST,DA,OFARECD,0                         
         CLI   8(R1),0             TEST EVERYTHING OK                           
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         LA    R3,DREC                                                          
         USING OAPRECD,R3                                                       
         XC    OAPKEY,OAPKEY                                                    
         MVI   OAPKTYP,OAPKTYPQ                                                 
         MVC   OAPKCUL,OFAKCULA                                                 
         MVC   OAPKOFF,OFAKOFF                                                  
         MVC   OAPKACT,OFAKACT                                                  
         MVC   OAPKDA,DA                                                        
         MVC   OAPKSTA,OFARSTA                                                  
         GOTO1 VDATAMGR,DMCB,DMADD,ACCDIR,DREC,DREC                             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         B     EXIT                                                             
                                                                                
         DROP  R3,R4,R5                                                         
                                                                                
ACCMST   DC    CL8'ACCMST  '                                                    
ACCDIR   DC    CL8'ACCDIR  '                                                    
DMREAD   DC    CL8'DMREAD  '                                                    
DMADD    DC    CL8'DMADD   '                                                    
ADDREC   DC    CL8'ADDREC  '                                                    
*                                                                               
OFFMESS  DC    C'Cannot override office'                                        
         EJECT ,                                                                
*ROUTINE TO SET OR CHECK A HIT BIT IN HITLIST CORRESPONDING TO                  
*AN ACCUMALATOR IN ACCUMS                                                       
* NTRY POINT  HITSET = SETS THE HIT BIT FOR ACCUMALATOR ADDRESSED BY R1         
* NTRY POINT  HITCHK = CHECKS WHETHER HIT BIT FOR ACCUMALATOR                   
*                      ADDRESSED BY R1 IS ON OR NOT                             
*                      SETS CC TO ON OR NOT                                     
         SPACE 1                                                                
HITSET   LA    RF,HITOI            RF=A(EXECUTE INSTRUCTION                     
         B     HITIT                                                            
*                                                                               
HITCHK   LA    RF,HITTM                                                         
*                                                                               
HITIT    NTR1                                                                   
         S     R1,AACCUMS          R1=DISP INTO ACCUMS                          
         SR    R0,R0               CLEAR FOR DIVIDE                             
         LA    RE,L'ACCUMS         DIVISOR                                      
         DR    R0,RE               DIVIDE BY LINE WIDTH                         
         LA    R1,HITLIST(R1) USE QUOTIENT TO INDEX TO HIT BYTE ON LINE         
         LA    RE,X'80'       USE REMAINDER TO ADJUST FOR COLM IN LINE          
         SRL   R0,3           DIVIDE BY 8 FOR COLUMN NUMBER MINUS 1             
         AH    R0,=H'1'                                                         
         B     HITIT20                                                          
*                                                                               
HITIT10  SRL   RE,1      COL 1 = X'80', 2 = X'40', 3 = X'20', 4 = X'10'         
*                                                                               
HITIT20  BCT   R0,HITIT10                                                       
         EX    RE,0(,RF)           TM OR OI                                     
         B     EXIT                                                             
*                                                                               
HITOI    OI    0(R1),0                                                          
*                                                                               
HITTM    TM    0(R1),0                                                          
*&&US                                                                           
         EJECT                                                                  
FCONOF   NTR1  ,                   FILTER CONTRA OFFICE                         
         CLI   OFFILT+1,C' '       TEST NEW OFFICES                             
         BNH   OKXIT               NO,                                          
         LA    RF,CACNTRL                                                       
         USING IOCBD,RF                                                         
         TM    IOCSTAT,IOCSCLDG+IOCSCKOF                                        
         BNO   OKXIT                                                            
*                                                                               
         LA    R5,IOCNOWKY                                                      
         USING CACRECD,R5                                                       
         LLC   R1,IOCOFPOS         GET OFFPOS                                   
         LA    R1,CACKCULC+2(R1)                                                
         CLC   IOCDLSTA,IOCLOOFF   DISP TO LEVEL VS. DISP TO OFFICE             
         BL    OKXIT                                                            
         BH    FCONOF3                                                          
         CLC   0(2,R1),SPACES                                                   
         BE    OKXIT                                                            
*                                                                               
FCONOF3  CLC   0(2,R1),OFFILT      TEST THIS OFFICE                             
         BE    OKXIT                                                            
         B     ERRXIT                                                           
         DROP  R5,RF                                                            
*&&                                                                             
         EJECT ,                                                                
* EXITS FROM ROUTINES AND RETURN TO ROOT (USED BY BOTH CSECTS)                  
         SPACE 1                                                                
OKEND    MVI   FERN,OK             OK COMPLETION                                
         B     EXIT                                                             
*                                                                               
OKXIT    SR    R8,R8               CC=EQU FOR OK EXITS FROM ROUTINES            
*                                                                               
ERRXIT   LTR   R8,R8                                                            
*                                                                               
EXIT     XIT1                                                                   
         EJECT ,                                                                
* LITERALS ETC                                                                  
         SPACE 1                                                                
ACCOUNT  DC    CL8'ACCOUNT'                                                     
DASHES   DC    7C'-'                                                            
TOTALMSG DC    CL38'TOTALS DISPLAYED - HIT ENTER TO UPDATE'                     
         SPACE 1                                                                
         LTORG                                                                  
*                                                                               
         DROP  RA                  KEEP IT CLEAN                                
         EJECT ,                                                                
         SPACE 1                                                                
FLDHEDS  DS    0CL4                1ST HALF OF FLD HDRS FOR SCREEN GEN.         
         DC    X'46280291'         1ST HEADLINE (ONLY USED IF PERNUM=0)         
*                                  IF USED REMAINING FLDADR'S MUST BE           
*                                  INCREMENTED BY ONE LINE (+80)                
         DC    X'55280281'         2ND HEADLINE                                 
         DC    X'552802D1'         3RD HEADLINE                                 
         DC    X'16280321'         LINE TITLE (AS FOR CONTRA FORMAT)            
         DC    X'12000331'         5 VALUE FIELDS (4+TOTAL)                     
         DC    X'1200033E'                                                      
         DC    X'1200034B'                                                      
         DC    X'12000358'                                                      
         DC    X'12000365'                                                      
*                                                                               
FLDHNAME DC    X'2D000331'         NAME FIELD FOR NAME OPTION                   
         EJECT ,                                                                
* GET NEXT PRIMARY OR SECONDARY ACCOUNT FOR TRANSACTION SEQUENCE                
*                                                                               
* ADDRESS OF PRIMARY OR SECONDARY I/O CONTROL BLOCK IS ADDRESSED BY R1          
* SEE DSECT IOCBD FOR DETAILS                                                   
*                                                                               
* RETURN WITH ACCODE=15BYTE A/C KEY OR NULLS IF NOT FOUND                       
*             ACCTWA=14BYTE DISPLAY A/C CODE                                    
*             ACCNAM=36BYTE NAME                                                
*             AACCIO=A(RECORD) IF ACCOUNT CALL NOT CONTRA                       
*             CC=NEQ IF NOT FOUND                                               
*                                                                               
* IF CALL IS FOR INPUT=Y ACCTWA CONTAINS INPUT CODE ON INPUT AND                
* CC=NEQ IS RETURNED IF ERROR (EG LEDGER INVALID/INCOMPATIBLE-B.TYPE)           
         SPACE 1                                                                
         USING ACKEYD,R5           R5 = A(RECORD)                               
         USING IOCBD,R7                                                         
         SPACE 1                                                                
GETACC   CSECT                                                                  
         NMOD1 0,*GETACC*                                                       
         L     RC,ASAVE                                                         
         L     R7,0(,R1)                                                        
         LA    R1,ACCNTRL                                                       
         MVI   ACCTYPE,C'A'        SET ACCTYPE TO ACC OR CAC                    
         CR    R7,R1                                                            
         BE    *+8                                                              
         MVI   ACCTYPE,C'C'                                                     
         C     R7,APRIMIOC         IF PRIMARY CALL, CLEAR CURRENT               
         BNE   GETA02              SECONDARY KEY                                
         L     R1,ASECDIOC                                                      
         XC    0(L'IOCNOWKY,R1),0(R1)                                           
         CLI   BUDFORM,C'A'        IF FORMAT IS ACCOUNT AND ITS DRIVEN          
         BNE   GETA02              BY BUDGET RECS SKIP PRIMARY IO 1ST           
         CLI   ACTION,ADD          TIME                                         
         BE    GETA02                                                           
         OC    IOCNOWKY,IOCNOWKY                                                
         BNZ   GETANG                                                           
         MVI   IOCNOWKY,X'FF'      FORCE IT TO FAIL 2ND TIME                    
         XC    ACCTWA,ACCTWA                                                    
         B     GETAOK                                                           
*                                                                               
GETA02   XC    ACCODE,ACCODE       INITIALISE RETURN VALUES                     
         MVC   ACCNAM,SPACES                                                    
         XC    KEY,KEY                                                          
         CLI   IOCLOKEY,X'01'                                                   
         BE    GETA03                                                           
         XC    ACCTWA,ACCTWA                                                    
         B     GETA04                                                           
*                                                                               
GETA03   DS    0H                  HANDLE INPUT=YES CALL                        
         LLC   R1,ACCNTRL+(IOCDS15-IOCBD)                                       
         LA    R1,ACCNTRL(R1)                                                   
         MVC   KEY,SPACES          BUILD A KEY FOR CONTRA-HEADER                
         MVC   KEY(15),0(R1)       ACKEYACC FROM CURRENT A/C KEY                
         LLC   R1,IOCDS14                                                       
         LA    R1,KEY(R1)          ACKEYCON FROM INPUT CODE                     
         CLI   IOCDS14,18                                                       
         BNE   *+10                                                             
         MVC   KEY+17(1),COMPANY                                                
         MVC   0(14,R1),ACCTWA                                                  
         L     R5,AIOAREA1                                                      
         GOTO1 AREAD,AIOAREA1      READ FOR IT                                  
         BE    GETA18                                                           
         MVC   ACKEYD(42),KEY      IF NOT FOUND PUT DUMMY IN IOAREA             
         MVI   ACRECORD,0          WITH NO ELS                                  
         B     GETA18                                                           
*                                                                               
GETA04   CLC   IOCHIKEY,SPACES     HANDLE 'ALL' CASES (CONTRA=SPACES)           
         BNE   GETA06                                                           
         OC    IOCNOWKY,IOCNOWKY                                                
         BNZ   GETANG                                                           
         MVC   IOCNOWKY,SPACES                                                  
         MVC   ACCODE(L'ACCODE+L'ACCTWA),SPACES                                 
         MVC   ACCTWA(3),=CL4'ALL'                                              
         B     GETAOK                                                           
*                                                                               
GETA06   OC    IOCLOKEY,IOCLOKEY   IF LOW KEY IS NULLS ITS A CONTRA             
         BNZ   GETA08                                                           
         LLC   RE,IOCDLEND         THAT IS CONTROLLED BY THE ACCOUNT            
         BCTR  RE,0                                                             
         EXCLC RE,IOCNOWKY,ACCNTRL                                              
         BNL   GETA10              WERE PAST KEY OF ACCOUNT                     
         CLI   ACCNTRL,ACBTKTEQ    WERE NOT - BUDGET REC WILL DO                
         BE    GETA07                                                           
         LLC   RF,IOCDLSTA         ACCOUNT REC IS OK IF                         
         LA    R1,ACCNTRL(RF)      IT HAS A CONTRA VALUE                        
         SR    RE,RF                                                            
         EXCLC RE,0(R1),SPACES                                                  
         MVC   IOCNOWKY,ACCNTRL                                                 
         BE    GETA10              NO - SO GO TO READ HIGH FROM IT              
*                                                                               
GETA07   L     R5,AACCIO           YES - SO USE IT (IT MUST BE IN W/S)          
         B     GETA18                                                           
*                                                                               
GETA08   OC    IOCNOWKY,IOCNOWKY   CURRENT KEY IS NULLS SO WE START             
         BNZ   GETA10              FROM LOW KEY                                 
         MVC   KEY(L'IOCLOKEY),IOCLOKEY                                         
         B     GETA12                                                           
*                                                                               
GETA10   LLC   RE,IOCDLEND         OTHERWISE BUMP CURRENT KEY BY                
         LA    RE,KEY(RE)          FORCING TO NEXT AT REQUIRED LEVEL            
         MVC   KEY(L'IOCNOWKY),IOCNOWKY                                         
         MVI   0(RE),X'FF'                                                      
*                                                                               
GETA12   LA    R1,ABUDREC          READ HIGH                                    
         CLI   KEY,ACBTKTEQ                                                     
         BE    *+8                                                              
         LA    R1,AIOAREA1                                                      
         GOTO1 ARDHI                                                            
*                                                                               
GETA13   TM    DMCB+8,X'FD'        SKIP DELETES                                 
         BNZ   GETANG                                                           
         TM    DMCB+8,2                                                         
         BNO   GETA13A                                                          
GETSEQ   L     RF,ASEQ                                                          
         BASR  RE,RF                                                            
         B     GETA13                                                           
*                                                                               
GETA13A  CLI   ACCTYPE,C'A'                                                     
         BNE   *+10                                                             
         MVC   AACCIO,AIOAREA      SAVE ADDR IF ACC CALL                        
         L     R5,AIOAREA                                                       
*                                                                               
GETA14   OC    IOCHIKEY,IOCHIKEY   CHECK FOR CONTROL BREAKS                     
         BNZ   GETA16              IF ITS A CONTRA CONTROLLED BY ACC            
         LLC   RE,IOCDS15          A/C LEN IS DISP TO C/A CODE(NO WC)           
         BCTR  RE,0                                                             
         EXCLC RE,ACKEYACC,ACCNTRL CHECK WHETHER ACC HAS CHANGED                
         BE    GETA18                                                           
         B     GETANG              IF SO NOT FOUND                              
*                                                                               
GETA16   CLC   IOCHIKEY,ACKEYACC   OTHERWISE CHECK IF WERE PAST HIGHKEY         
         BNL   GETA18                                                           
         CLC   IOCLOKEY,IOCHIKEY                                                
         BNE   GETANG                                                           
         OC    IOCNOWKY,IOCNOWKY                                                
         BNE   GETANG                                                           
         MVC   ACKEYD(42),KEYSAVE  SINGLE CAC DOESNT HAVE TO EXIST              
         MVI   ACRECORD,0                                                       
*                                                                               
GETA18   MVC   IOCNOWKY,ACKEYACC   RESET CURRENT KEY                            
*                                                                               
GETA20   CLI   ACCTYPE,C'C'        CHECK FOR CHANGE OF CONTRA LEDGER            
         BNE   GETA30                                                           
GETA21   CLI   BUDRULES,C'+'       UNLESS NON-STANDARD                          
         BE    GETA35                                                           
         LLC   R2,IOCDS15                                                       
         LA    R2,ACKEYACC(R2)                                                  
         CLC   IOCLCODE,1(R2)                                                   
         BE    GETA35              SAME                                         
         LA    R3,BUDRULES         DIFFERENT - IS IT VALID FOR BUDTYPES         
         OC    BUDRULES(2),SPACES                                               
         LA    R0,5                                                             
         MVI   WORK,X'FF'          (GET NEXT HIGHER VALID UL INTO WORK)         
*                                                                               
GETA22   CLI   0(R3),X'FF'                                                      
         BE    GETA26                                                           
         CLC   0(2,R3),1(R2)                                                    
         BE    GETA28              IT IS VALID                                  
         BL    GETA24                                                           
         CLC   0(2,R3),WORK                                                     
         BH    GETA24                                                           
         MVC   WORK(3),0(R3)                                                    
*                                                                               
GETA24   LA    R3,3(,R3)                                                        
         BCT   R0,GETA22                                                        
*                                                                               
GETA26   CLI   IOCLOKEY,X'01'      NOT VALID - ERROR FOR INPUT=YES              
         BNE   GETA26A                                                          
         MVI   FERN,BTYPCALE                                                    
         B     GETANG                                                           
*                                                                               
GETA26A  CLI   WORK,X'FF'                                                       
         BE    GETANG              NO OTHER UL IS                               
         MVC   1(2,R2),WORK        ANOTHER IS                                   
*                                                                               
GETA27   XC    3(42,R2),3(R2)                                                   
         MVC   KEY,ACKEYD                                                       
         B     GETA12              GO AND READ HIGH FOR IT                      
*                                                                               
GETA28   DS    0H                  CHECK NEW LEDGER                             
         GOTO1 ACHECKUL,DMCB,1(R2),(R7)                                         
         BE    GETA29              OK                                           
         TM    SAVSTAT,INPUTISY                                                 
         BO    GETANG              LEDGER INVALID FOR INPUT=Y                   
         LLC   R1,2(,R2)           OTHERWISE READ HIGH                          
         LA    R1,1(,R1)                                                        
         STC   R1,2(,R2)                                                        
         B     GETA27                                                           
*                                                                               
GETA29   LLC   R1,2(,R3)           RESET LEVEL DATA                             
         GOTO1 ALEVSET                                                          
*                                                                               
GETA30   CLI   ACCTYPE,C'A'        CHECK SECURITY IF ACCOUNT CALL               
         BNE   GETA35                                                           
         CLI   ACBTKTYP,ACBTKTEQ   IF ITS A BUDGET REC - GET ACCOUNT            
         BNE   GETA32                                                           
         MVC   KEY,SPACES                                                       
         MVC   KEY(L'ACKEYACC),ACBTKACC                                         
         GOTO1 AREAD,AIOAREA1                                                   
         BNE   GETA10              CANT FIND A/C - SO SKIP TO NEXT              
*                                                                               
GETA32   GOTO1 AGETNAME,AIOAREA                                                 
         MVC   ACCNAM,WORK                                                      
         GOTO1 ASECHECK,DMCB,AIOAREA,(R7)                                       
         BE    GETA35                                                           
         MVC   ACCNAM,SPACES       IF LOCKOUT OPTIMAL NEXT KEY FOR              
         B     GETA12              READ HIGH RETURNED IN KEY                    
*                                                                               
GETA35   CLI   ACBTKTYP,ACBTKTEQ   CHECK LEVEL (MUSTNT BE ALL SPACES            
         BE    GETA40              BETWEEN START & END OF REQUIRED LEV)         
         LLC   RE,IOCDLSTA                                                      
         LLC   RF,IOCDLEND                                                      
         SR    RF,RE                                                            
         LA    RE,ACKEYD(RE)                                                    
         BCTR  RF,0                                                             
         EXCLC RF,0(RE),SPACES                                                  
         BNE   GETA40                                                           
*&&US                                                                           
         CLI   OFFILT+1,C' '       TEST 2 CHARACTER OFFICE                      
         BNH   GETA36              NO,                                          
         CLC   IOCDLSTA,IOCLOOFF   IS OFFICE THE MISSING LEVEL                  
         BNE   GETA36              NO,                                          
         MVC   0(2,RE),OFFILT      FILL IN MISSING OFFICE                       
*                                                                               
         LLC   R1,2(R3)            CURRENT LEVEL                                
         SHI   R1,1                                                             
         BZ    GETA40              ALREADY AT LEVEL 1                           
         GOTO1 ALEVSET             GET PREVIOUS LEVEL                           
         LLC   RE,IOCDLSTA         MAKE SURE IT'S NOT SPACES                    
         LLC   RF,IOCDLEND                                                      
         SR    RF,RE                                                            
         LA    RE,ACKEYD(RE)                                                    
         BCTR  RF,0                                                             
         EXCLC RF,0(RE),SPACES                                                  
         BE    GETA37                                                           
         AHI   R1,1                RESET LEVEL                                  
         GOTO1 ALEVSET             GET PREVIOUS LEVEL                           
         B     GETA40                                                           
*&&                                                                             
GETA36   CLI   IOCLOKEY,X'01'                                                   
         BNE   GETA10                                                           
*                                                                               
GETA37   MVI   FERN,WRONGLEV       ERROR IF INPUT=YES                           
         B     GETANG                                                           
*                                                                               
GETA40   CLI   ACCTYPE,C'C'        CHECK FOR CONTRA LEVEL TOO LOW               
         BNE   GETA42              BY SEEING IF THERE ARE NON-SPACES            
         CLI   ACTION,ADD          BEYOND END OF REQUIRED LEVEL                 
         BNE   GETA42                                                           
         LLC   R1,IOCDLEND                                                      
         LA    RF,ACBTKBNO-ACBTKEY                                              
         SR    RF,R1                                                            
         SH    RF,=H'1'                                                         
         BM    GETA42                                                           
         LA    R1,ACKEYACC(R1)                                                  
         EXCLC RF,0(R1),SPACES     IF THERE ARE                                 
         BE    GETA42                                                           
         CLI   IOCLOKEY,X'01'      IF INPUT=YES                                 
         BE    GETA37              IT'S AN ERROR                                
         EXMVC RF,0(R1),SPACES     OTHERWISE INSERT TRAILING SPACES             
         MVC   IOCNOWKY,ACKEYACC   TO RAISE LEVEL                               
         LH    RF,DATADISP                                                      
         AR    RF,R5                                                            
         MVI   0(RF),0             HIDE ELS AS THIS NAME IS NOT RIGHT           
*                                                                               
GETA42   DS    0H                  FILTERS                                      
         LA    R4,CACNTRL                                                       
CAC      USING IOCBD,R4                                                         
         LLC   RE,CAC.IOCDS14                                                   
         LA    RE,ACKEYD(RE)                                                    
         LA    RF,L'IOCFILT-1                                                   
         CLI   ACCTYPE,C'C'                                                     
         BE    GETA44                                                           
         CLI   BUDFORM,C'A'        ACCOUNT FORMAT FILTERING ON BUDGET           
         BNE   GETA60              RECORDS - CAFILTER = SINGLE CAC              
         CLI   ACTION,ADD                                                       
         BE    GETA60                                                           
         MVC   CACNTRL(L'IOCNOWKY),IOCNOWKY  EQUATE CAC AND ACC KEYS            
         CLC   CAC.IOCFILT,0(RE)                                                
         BE    GETA50                                                           
         BL    GETA10                                                           
         B     GETA46                                                           
*                                                                               
GETA44   OC    CAC.IOCFILT,CAC.IOCFILT   APPLY FILTER IF CONTRA CALL            
         BZ    GETA50                                                           
*                                                                               
GETA44A  LA    R1,CAC.IOCFILT(RF)                                               
         CLI   0(R1),C' '                                                       
         BH    GETA45                                                           
         BCT   RF,GETA44A                                                       
*                                                                               
GETA45   EXCLC RF,CAC.IOCFILT,0(RE)                                             
         BE    GETA50              REQUIRED                                     
         BL    GETANG                                                           
*                                                                               
GETA46   XC    0(42,RE),0(RE)                                                   
         MVC   0(L'IOCFILT,RE),CAC.IOCFILT                                      
         MVC   KEY,ACKEYD                                                       
         B     GETA12                                                           
         DROP  CAC                                                              
*                                                                               
GETA50   TM    SAVSTAT,NAME        GET CONTRA NAME IF REQUIRED                  
         BO    GETA51                                                           
         CLC   ACCTYPE,BUDFORM                                                  
         BE    GETA60                                                           
*                                                                               
GETA51   MVC   KEYSAVE,ACKEYD                                                   
         ST    R5,AIOAREA                                                       
*                                                                               
GETA52   GOTO1 AGETNAME,AIOAREA    GET NAME INTO WORK FROM BUD/ACC/CAC          
         CLC   WORK,SPACES         RECORD                                       
         BNE   GETA59              FOUND ONE                                    
         CLI   ACCTYPE,C'A'        WE DIDNT FIND ONE -BUT WE HAVE ACC           
         BE    GETA60                                                           
*                                                                               
GETA54   MVC   KEY,SPACES          WE DIDN'T FIND ONE                           
         CLI   KEYSAVE,ACBTKTEQ    IF THIS WAS A BUDGET REC, TRY THE            
         BNE   GETA56              CONTRA HEADER                                
         MVC   KEY(32),KEYSAVE+1                                                
         B     GETA58                                                           
*                                                                               
GETA56   CLC   KEYSAVE+15(17),SPACES    IF THIS WAS A CONTRA HEADER TRY         
         BE    GETA60              THE ACCOUNT, IF AN ACCOUNT GIVE UP           
         MVC   KEY(15),KEYSAVE+17                                               
*                                                                               
GETA58   GOTO1 ARDHI,AIOAREA2                                                   
         BNE   GETA54                                                           
         L     R1,0(,R1)                                                        
         CLC   KEYSAVE,0(R1)                                                    
         BE    GETA52                                                           
         B     GETA54                                                           
*                                                                               
GETA59   MVC   ACCNAM,WORK                                                      
*                                                                               
GETA60   LLC   RE,IOCDS15          RETURN ACCODE & ACCTWA                       
         LA    R1,14                                                            
         LLC   RF,IOCDS14                                                       
         AR    R1,RE                                                            
         SR    R1,RF                                                            
         LA    RE,IOCNOWKY(RE)                                                  
         LA    RF,IOCNOWKY(RF)                                                  
         MVC   ACCODE,0(RE)                                                     
         MVC   ACCTWA,SPACES                                                    
         EXMVC R1,ACCTWA,0(RF)                                                  
         CLC   ACCTWA,SPACES                                                    
         BNE   GETAOK                                                           
         MVC   ACCTWA(3),=CL4'ALL'                                              
*        B     GETAOK                                                           
*                                                                               
GETAOK   SR    R8,R8               CC=EQU FOR OK EXITS FROM ROUTINES            
*                                                                               
GETANG   LTR   R8,R8                                                            
         XIT1                                                                   
*                                                                               
         DROP  R5,R7                                                            
         SPACE 1                                                                
         LTORG                                                                  
         EJECT ,                                                                
* LOCAL WORKING STORAGE IN SAVE (PRE-XCED)                                      
         SPACE 1                                                                
LOCALD   DSECT                                                                  
*                                                                               
MONLIMIT EQU   36                  MAX NUMBER OF MONTHS PER BUDGET REC          
*                                                                               
AGETACC  DS    A                                                                
THISPRIC DS    CL14                PRIMARY ACC CODE                             
THISPRIN DS    CL36                PRIMARY ACC NAME                             
THISSECC DS    CL14                SECONDARY ACC CODE                           
THISSECN DS    CL36                SECONDARY ACC NAME                           
VALLIST  DS    13PL8               PHASED AMOUNTS GENERATED BY PUTVAL           
WRITPEND DS    C                   0=NO, A=ADD PENDING, W=WRITE PENDING         
DELPEND  DS    C                   Y=ELEMENT DELETE PENDING                     
MYTEMP   DS    CL160                                                            
         EJECT                                                                  
* ACBUDDSECT                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACBUDDSECT                                                     
         PRINT ON                                                               
* ACGENBOTH                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* ACMSGEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACMSGEQUS                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'030ACBUD03   02/25/13'                                      
         END                                                                    
