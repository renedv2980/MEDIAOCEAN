*          DATA SET REREPCV02  AT LEVEL 122 AS OF 05/01/02                      
*          DATA SET REREPCV02  AT LEVEL 120 AS OF 06/03/86                      
*PHASE RECV02A                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
         TITLE 'RECV02 - REPPAK MPI CONVERSION PROGRAM'                         
RECV02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**CV02**,RR=RE                                                 
*                                                                               
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         L     RC,FILEC                                                         
         USING FILED,RC                                                         
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING RECV02+4096,R9                                                   
*                                                                               
         CLI   MODE,REQFRST                                                     
         BNE   EXIT                                                             
         MVC   REPCODE,QOPTION1    SAVE THE REPCODE                             
         B     CV001                                                            
*                                                                               
*                                                                               
EQXIT    CR    RB,RB                                                            
         B     EXIT                                                             
*                                                                               
NEXIT    LTR   RB,RB                                                            
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
CV001    CLI   QOPTION3,C'2'       EXPECTING USED ADV/AGY LIST ?                
         BNE   CV004                                                            
         L     R3,AAGYFILE                                                      
         OPEN  ((R3),INPUT)                                                     
         L     R2,ASCAGY                                                        
*                                                                               
CV001A   GET   (R3),(R2)                                                        
         LA    R2,6(R2)                                                         
         C     R2,ASCAGYX                                                       
         BL    CV001A                                                           
         DC    H'0'                                                             
*                                                                               
CV002    CLOSE ((R3))                                                           
         MVI   0(R2),X'FF'                                                      
         L     R3,AADVFILE                                                      
         OPEN  ((R3),INPUT)                                                     
         L     R2,ASCADV                                                        
*                                                                               
CV002A   GET   (R3),(R2)                                                        
         LA    R2,4(R2)                                                         
         C     R2,ASCADVX                                                       
         BL    CV002A                                                           
         DC    H'0'                                                             
*                                                                               
CV003    CLOSE ((R3))                                                           
         MVI   0(R2),X'FF'                                                      
         L     R3,ASTAFILE                                                      
         OPEN  ((R3),INPUT)                                                     
         L     R2,ASCSTA                                                        
*                                                                               
CV003A   GET   (R3),(R2)                                                        
         LA    R2,5(R2)                                                         
         C     R2,ASCSTAX                                                       
         BL    CV003A                                                           
         DC    H'0'                                                             
*                                                                               
CV003X   CLOSE ((R3))                                                           
         MVI   0(R2),X'FF'                                                      
*                                                                               
CV004    L     R2,AIN              OPEN THE INPUT FILE                          
         OPEN  ((R2),INPUT)                                                     
         XC    MKEY,MKEY           CLEAR MPI TYPE KEY                           
*                                                                               
CV005    L     R0,AINBLOCK         GET A RECORD                                 
         L     R1,AIN                                                           
         GET   (R1),(R0)                                                        
         L     R2,AINBLOCK                                                      
         LA    R3,4                4 X 1024 BYTE LOGICAL BLOCKS                 
*                                                                               
CV010    LA    R4,1024(R2)         R4 = START OF NEXT LOGICAL BLOCK             
         LH    R5,0(R2)                                                         
         AR    R5,R2               R5 = LOGICAL BLOCK END                       
         LA    R2,12(R2)           GET PAST 12 BYTE LOGICAL BLOCK HDR           
         CLC   0(2,R2),XFF         TEST FOR EOF                                 
         BE    CV900                                                            
         OC    0(2,R2),0(R2)                                                    
         BZ    CV900                                                            
*                                                                               
CV012    LH    R7,0(R2)            RECORD LENGTH                                
         SH    R7,=H'6'            SUBTRACT 6 BYTE RECORD HEADER                
         BCTR  R7,0                GET R7 READY FOR EXECUTED MOVE               
         ZIC   RF,4(R2)            NO OF KEY COMPRESSION BYTES                  
         ZIC   R1,5(R2)            ACTUAL NO OF KEY BYTES                       
         AR    R1,RF                                                            
         CH    R1,=H'36'           KEY MUST BE 36 BYTES                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R1,INREC(RF)        EXECUTED MOVE TO R1                          
         XCEF  INREC,1000          CLEAR RECORD AREA                            
         MVC   INREC(36),MKEY      MOVE IN PREVIOUS KEY                         
         LA    R6,6(R2)            MOVE FROM BEYOND RECORD HEADER               
*                                                                               
CV015    CH    R7,=H'256'          MOVE IN 256 BYTE CHUNKS                      
         BL    CV017                                                            
         MVC   0(256,R1),0(R6)                                                  
         LA    R1,256(R1)                                                       
         LA    R6,256(R6)                                                       
         SH    R7,=H'256'                                                       
         B     CV015                                                            
*                                                                               
CV017    EX    R7,*+8              MOVE REST OF RECORD                          
         B     *+10                                                             
         MVC   0(0,R1),0(R6)       * EXECUTED                                   
         MVC   MKEY,INREC          SAVE THE KEY                                 
*                                                                               
         STM   R2,R5,REGSAVE                                                    
         LA    R1,RECIDS                                                        
*                                                                               
CV020    CLI   0(R1),X'FF'                                                      
         BE    CV060                                                            
         CLC   0(2,R1),INREC                                                    
         BE    CV030                                                            
         LA    R1,11(R1)                                                        
         B     CV020                                                            
*                                                                               
CV030    SR    RF,RF                                                            
         ICM   RF,7,2(R1)          BRANCH ADDRESS                               
         SR    R2,R2                                                            
         ICM   R2,7,5(R1)          RECORD COUNTER ADDR                          
         SR    R3,R3                                                            
         ICM   R3,7,8(R1)          RECORDS ADDED COUNTER ADDR                   
         LA    R8,INREC                                                         
         BR    RF                  BRANCH TO RECORD ROUTINE                     
*                                                                               
CV040    AP    0(8,R3),=P'1'       AUGMENT RECORDS ADDED COUNTER                
         B     CV060                                                            
*                                                                               
CV050    B     CV060               RECORD NOT ADDED                             
*V050    BR    RE                  *** SPECIAL PATCH TO ADD ALL ***             
*                                                                               
CV060    LM    R2,R5,REGSAVE                                                    
         LH    RE,0(R2)            RECORD LENGTH                                
         AR    R2,RE               NEXT RECORD                                  
         CR    R2,R5               TEST FOR END OF LOGICAL BLOCK                
         BNL   CV065                                                            
         OC    0(2,R2),0(R2)       NO - TEST FOR MORE RECORDS IN BLOCK          
         BZ    CV065                                                            
         CLC   0(2,R2),XFF                                                      
         BNE   CV012                                                            
         DC    H'0'                                                             
*                                                                               
CV065    LR    R2,R4               NEXT LOGICAL BLOCK                           
         BCT   R3,CV010            LOOP FOR ALL LOGICAL BLOCKS                  
*                                                                               
         B     CV005               READ NEXT PHYSICAL BLOCK                     
*                                                                               
*                                                                               
CV900    MVC   P(11),=C'RECORD TYPE'    E-O-F                                   
         MVC   PSECOND(11),DASHES                                               
         MVC   P+13(12),=C'RECORDS READ'                                        
         MVC   PSECOND+13(12),DASHES                                            
         MVC   P+27(13),=C'RECORDS ADDED'                                       
         MVC   PSECOND+27(13),DASHES                                            
         MVI   PTHIRD,0                                                         
         GOTO1 REPORT                                                           
         MVC   P(3),=C'REP'                                                     
         EDIT  (P8,REPCNT),(12,P+13)                                            
         EDIT  (P8,REPADD),(12,P+28)                                            
         GOTO1 REPORT                                                           
         MVC   P(6),=C'OFFICE'                                                  
         EDIT  (P8,OFFCNT),(12,P+13)                                            
         EDIT  (P8,OFFADD),(12,P+28)                                            
         GOTO1 REPORT                                                           
         MVC   P(7),=C'STATION'                                                 
         EDIT  (P8,STACNT),(12,P+13)                                            
         EDIT  (P8,STAADD),(12,P+28)                                            
         GOTO1 REPORT                                                           
         MVC   P(10),=C'ADVERTISER'                                             
         EDIT  (P8,ADVCNT),(12,P+13)                                            
         EDIT  (P8,ADVADD),(12,P+28)                                            
         GOTO1 REPORT                                                           
         MVC   P(6),=C'AGENCY'                                                  
         EDIT  (P8,AGYCNT),(12,P+13)                                            
         EDIT  (P8,AGYADD),(12,P+28)                                            
         GOTO1 REPORT                                                           
         MVC   P(11),=C'SALESPERSON'                                            
         EDIT  (P8,SALCNT),(12,P+13)                                            
         EDIT  (P8,SALADD),(12,P+28)                                            
         GOTO1 REPORT                                                           
         MVC   P(8),=C'CONTRACT'                                                
         EDIT  (P8,CONCNT),(12,P+13)                                            
         EDIT  (P8,CONADD),(12,P+28)                                            
         GOTO1 REPORT                                                           
*                                                                               
         GOTO1 REPORT              PRINT HIGHEST CONTRACT NUMBER                
         MVC   P(15),=C'HIGH CONTRACT ='                                        
         ZAP   DUB(5),=P'0'                                                     
         MVO   DUB(5),HICONT                                                    
         EDIT  (P5,DUB),(9,P+16),ZERO=BLANK,ALIGN=LEFT                          
         GOTO1 REPORT                                                           
*                                                                               
         CLI   QOPTION3,C'1'       CHECK FOR AGENCY/ADV DUMP                    
         BNE   CV990                                                            
         L     R3,AAGYFILE                                                      
         OPEN  ((R3),OUTPUT)                                                    
         GOTO1 REPORT              PRINT USED AGENCY CODES                      
         MVC   P(17),=C'USED AGENCY CODES'                                      
         GOTO1 REPORT                                                           
         L     R2,AAGYLIST                                                      
*                                                                               
CV910    OC    0(6,R2),0(R2)                                                    
         BZ    CV930                                                            
         CLI   6(R2),C'Y'                                                       
         BNE   CV920                                                            
         MVC   P(6),0(R2)                                                       
         GOTO1 REPORT                                                           
         PUT   (R3),(R2)           SAVE AGY TO AGYFILE                          
*                                                                               
CV920    LA    R2,7(R2)                                                         
         C     R2,AAGYLSTX                                                      
         BL    CV910                                                            
         DC    H'0'                                                             
*                                                                               
CV930    GOTO1 REPORT              PRINT USED ADV CODES                         
         CLOSE ((R3))                                                           
         L     R3,AADVFILE                                                      
         OPEN  ((R3),OUTPUT)                                                    
         MVC   P(21),=C'USED ADVERTISER CODES'                                  
         GOTO1 REPORT                                                           
         L     R2,AADVLIST                                                      
*                                                                               
CV940    OC    0(4,R2),0(R2)                                                    
         BZ    CV960                                                            
         CLI   4(R2),C'Y'                                                       
         BNE   CV950                                                            
         MVC   P(4),0(R2)                                                       
         GOTO1 REPORT                                                           
         PUT   (R3),(R2)           PUT ADV TO ADVFILE                           
*                                                                               
CV950    LA    R2,5(R2)                                                         
         C     R2,AADVLSTX                                                      
         BL    CV940                                                            
         DC    H'0'                                                             
*                                                                               
CV960    CLOSE ((R3))                                                           
         L     R3,ASTAFILE                                                      
         OPEN  ((R3),OUTPUT)                                                    
         GOTO1 REPORT              PRINT USED STATION CODES                     
         MVC   P(18),=C'USED STATION CODES'                                     
         GOTO1 REPORT                                                           
         L     R2,ASTALIST                                                      
*                                                                               
CV970    OC    0(5,R2),0(R2)                                                    
         BZ    CV985                                                            
         CLI   5(R2),C'Y'                                                       
         BNE   CV980                                                            
         MVC   P(5),0(R2)                                                       
         GOTO1 REPORT                                                           
         PUT   (R3),(R2)           SAVE STA TO STAFILE                          
*                                                                               
CV980    LA    R2,6(R2)                                                         
         C     R2,ASTALSTX                                                      
         BL    CV970                                                            
         DC    H'0'                                                             
*                                                                               
CV985    CLOSE ((R3))                                                           
*                                                                               
CV990    L     R2,AIN                                                           
         CLOSE ((R2))                                                           
         L     R1,AOUT                                                          
         CLOSE ((R1))                                                           
         B     EXIT                                                             
         SPACE 2                                                                
RECIDS   DC    XL2'0001',AL3(REP),AL3(REPCNT),AL3(REPADD)                       
         DC    XL2'0002',AL3(STA),AL3(STACNT),AL3(STAADD)                       
         DC    XL2'0003',AL3(OFF),AL3(OFFCNT),AL3(OFFADD)                       
         DC    XL2'0004',AL3(SAL),AL3(SALCNT),AL3(SALADD)                       
         DC    XL2'000A',AL3(AGY),AL3(AGYCNT),AL3(AGYADD)                       
         DC    XL2'000B',AL3(ADV),AL3(ADVCNT),AL3(ADVADD)                       
         DC    XL2'0010',AL3(CON),AL3(CONCNT),AL3(CONADD)                       
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
* REP RECORD                                                                    
*                                                                               
         SPACE                                                                  
         USING MREPREC,R8                                                       
REP      CLC   MREPKREP,QUESTOR                                                 
         BNE   CV060                                                            
         AP    0(8,R2),=P'1'       AUGMENT RECORD COUNTER                       
*        XC    KEY,KEY             BUILD REPREC KEY                             
*        MVI   KEY,1                                                            
*        MVC   KEY+25(2),QREP                                                   
*        CLC   RREPKEY,KEY         DOES IT EXIST                                
*        BNE   *+8                                                              
*        BAS   RE,CV050            YES - EXIT                                   
         XC    RREPREC(179),RREPREC                                             
*        MVC   RREPKEY,KEY         REP KEY                                      
         MVI   RREPKTYP,1                                                       
         MVC   RREPKREP,REPCODE                                                 
*        MVC   RREPKREP,QREP                                                    
         MVI   RREPLEN+1,X'B3'                                                  
         MVI   RREPCODE,1          BUILD 01 ELEMENT                             
         MVI   RREPELLN,X'91'                                                   
         MVC   RREPNAME,MREPNAME                                                
         MVC   RREPADDR,SPACES                                                  
         MVC   RREPSHRT,SPACES                                                  
         MVC   RREPABBR,MREPABBR                                                
         MVI   RREPPROF,C'0'                                                    
         MVC   RREPPROF+1(29),RREPPROF                                          
         MVI   RREPFMON,1                                                       
         LA    R1,RREPREC                                                       
         BAS   RE,PUT                                                           
         B     CV040                                                            
         DROP  R8                                                               
         EJECT                                                                  
*                                                                               
* OFFICE RECORD                                                                 
*                                                                               
         SPACE                                                                  
         USING MOFFREC,R8                                                       
OFF      CLC   MOFFKREP,QUESTOR                                                 
         BNE   CV060                                                            
         AP    0(8,R2),=P'1'       AUGMENT RECORD COUNTER                       
*                                                                               
         L     RF,ASOFLIST         CONVERT MPI OFFICE CODE TO DDS CODE          
         CLC   REPCODE,=C'TO'                                                   
         BNE   OF005                                                            
         L     RF,ATOFLIST         TORBET OFFICE LIST                           
         CLC   MOFFKOFF,=C'BU'     TEST FOR BUDGET OFFICE                       
         BE    CV060                                                            
*                                                                               
OF005    CLI   0(RF),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   MOFFKOFF,0(RF)                                                   
         BE    OF007                                                            
         LA    RF,4(RF)                                                         
         B     OF005                                                            
*                                                                               
OF007    MVC   WORK(2),2(RF)       DDS OFFICE CODE IN WORK(2)                   
*        MVI   INFCODE,4           CHECK FOR OFFICE ALREADY EXISTS              
*        BAS   RE,FIRSTINF                                                      
*        B     *+8                                                              
*        BAS   RE,NEXTINF                                                       
*        BNE   OF010                                                            
*        CLC   WORK(2),4(R4)                                                    
*        BNE   *-14                                                             
*        BAS   RE,CV050            OFFICE FOUND                                 
*                                                                               
OF010    L     R1,ASOFFICE         SEE IF OFFICE RECORD ALREADY ADDED           
         CLC   REPCODE,=C'TO'                                                   
         BNE   OF020                                                            
         L     R1,ATOFFICE                                                      
*                                                                               
OF020    CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   WORK(2),0(R1)                                                    
         BE    *+12                                                             
         LA    R1,23(R1)                                                        
         B     OF020                                                            
         CLI   2(R1),C'1'                                                       
         BE    CV060               RECORD ALREADY ADDED                         
         MVI   2(R1),C'1'                                                       
*                                                                               
         XC    ROFFREC(108),ROFFREC                                             
         MVI   ROFFKTYP,4          BUILD OFFICE KEY                             
         MVC   ROFFKREP,REPCODE                                                 
*        MVC   ROFFKREP,QREP                                                    
         MVC   ROFFKOFF,WORK       DDS OFFICE CODE                              
         MVI   ROFFLEN+1,X'6C'                                                  
*                                                                               
         MVI   ROFFCODE,1          BUILD 01 ELEMENT                             
         MVI   ROFFELLN,X'4A'                                                   
         MVC   ROFFNAME,3(R1)      NAME                                         
         MVC   ROFFREG,SPACES      REGION                                       
         MVI   ROFFPROF,C'0'                                                    
         MVC   ROFFPROF+1(9),ROFFPROF                                           
         MVC   ROFFADD1,SPACES                                                  
         MVC   ROFFADD2,SPACES                                                  
         LA    R1,ROFFREC                                                       
         BAS   RE,PUT                                                           
         B     CV040                                                            
         DROP  R8                                                               
         EJECT                                                                  
*                                                                               
* STATION RECORD                                                                
*                                                                               
         SPACE                                                                  
         USING MSTAREC,R8                                                       
STA      CLC   MSTAKREP,QUESTOR                                                 
         BNE   CV060                                                            
         AP    0(8,R2),=P'1'       AUGMENT RECORD COUNTER                       
*                                                                               
*                                                                               
*        CLC   MSTAKMED(3),=C'AZ9'  OLD STATION                                 
*        BE    CV060                                                            
*        CLC   MSTAKMED,=C'LO'      LOST STATION                                
*        BE    CV060                                                            
         MVC   WORK(4),MSTAKSTA    STATION CALL LETTERS                         
         LA    R1,MSTAKMED                                                      
         BAS   RE,STAMED           FIND THE STATION MEDIA                       
         MVC   WORK+4(1),0(R1)                                                  
*                                                                               
         CLI   QOPTION3,C'2'       SEE IF ONLY LOADING USED STATIONS            
         BNE   ST020                                                            
         L     RE,ASCSTA           CHECK IF STATION IS USED                     
*                                                                               
ST010    CLI   0(RE),X'FF'                                                      
         BE    CV060               UNUSED                                       
         CLC   0(5,RE),WORK                                                     
         BE    ST020               USED STATION                                 
         LA    RE,5(RE)                                                         
         B     ST010                                                            
*                                                                               
*        MVI   INFCODE,2           CHECK FOR STATION ALREADY EXISTS             
*        BAS   RE,FIRSTINF                                                      
*        B     *+8                                                              
*        BAS   RE,NEXTINF                                                       
*        BNE   ST020                                                            
*        CLC   WORK(5),2(R4)                                                    
*        BNE   *-14                                                             
*        BAS   RE,CV050            STATION FOUND                                
*                                                                               
ST020    XC    RSTAREC(117),RSTAREC                                             
         MVI   RSTAKTYP,2          BUILD STATION KEY                            
         MVC   RSTAKREP,REPCODE                                                 
*        MVC   RSTAKREP,QREP                                                    
         MVC   RSTAKSTA,WORK                                                    
         MVI   RSTALEN+1,X'75'                                                  
*                                                                               
         MVI   RSTACODE,1          BUILD 01 ELEMENT                             
         MVI   RSTAELLN,X'53'                                                   
         MVC   RSTAMKT,MSTAMKT     MARKET                                       
         LA    R1,WORK             CHANNEL                                      
         LA    RE,MSTACHAN                                                      
         LA    RF,6                                                             
*                                                                               
ST030    CLI   0(RE),C'0'                                                       
         BL    *+14                                                             
         MVC   0(1,R1),0(RE)                                                    
         LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         BCT   RF,ST030                                                         
         LA    RF,WORK                                                          
         SR    R1,RF                                                            
         BCTR  R1,0                                                             
         LTR   R1,R1                                                            
         BM    ST040                                                            
         EX    R1,CHANPACK                                                      
         CVB   R1,DUB                                                           
         STCM  R1,3,RSTACHAN                                                    
*                                                                               
ST040    MVC   RSTAAFFL,MSTAAFFL   AFFILIATE                                    
         LA    R1,MSTAJOIN         START DATE                                   
         BAS   RE,DATCONV                                                       
         MVC   RSTASTRT,WORK                                                    
         LA    R1,MSTALEFT         END DATE                                     
         BAS   RE,DATCONV                                                       
         MVC   RSTAEND,WORK                                                     
         MVI   RSTAGRUP,C'R'       RADIO GROUP                                  
         MVC   RSTAGRUP+1(1),RSTAKSTA+4                                         
         MVI   RSTAPROF,C'0'       PROFILE                                      
         MVC   RSTAPROF+1(27),RSTAPROF                                          
*                                                                               
         L     R1,AOUTREC                                                       
         MVC   0(117,R1),RSTAREC   MOVE FIRST PART OF RECORD TO OUT             
*                                                                               
* FOLLOWING CODE WAS TO BUILD A STATION COMMENT ELEMENT                         
*                                                                               
*        LA    R4,MSTAELM1         BUILD STATION COMMENT ELEMENT                
*        MVI   NELCODE,6           GET MPI COMMENT ELEMENT                      
*        BAS   RE,GETNEL                                                        
*        BNE   ST050                                                            
*        MVI   ELEM,3              STATION COMMENT ELEMENT ID                   
*        CH    RF,=H'59'           COMMENT 60 CHARS MAX                         
*        BNH   *+8                                                              
*        LH    RF,=H'59'                                                        
*        LA    RE,5(RF)            +5                                           
*        STC   RE,ELEM+1           DDS ELEMENT LEN                              
*        MVI   ELEM+2,C'M'         MANUAL                                       
*        EX    RF,STCMTMOV                                                      
*        BAS   RE,ADDEL            ADD THE COMMENT ELEMENT                      
*                                                                               
ST050    CLI   RSTAKSTA+4,C'C'     TEST FOR COMBINED STATION                    
         BNE   ST080                                                            
         LA    R4,MSTAELM1         BUILD AM/FM ELEMENT FOR COMBINED             
         MVI   NELCODE,5           GET MPI AM/FM ELEMENT                        
         BAS   RE,GETNEL                                                        
         BNE   ST080                                                            
         XC    ELEM,ELEM                                                        
         MVI   ELEM,9              AM/FM ELEMENT ID                             
         MVI   ELEM+1,10                                                        
         MVC   ELEM+2(8),SPACES                                                 
         MVC   ELEM+2(4),RSTAKSTA    FORCE AM STATION TO BE SAME                
         OC    2(6,R4),2(R6)         TEST FOR FIRST STATION NULL                
         BZ    ST060                                                            
         CLC   6(2,R4),=C'FM'                                                   
         BNE   ST060                                                            
         MVC   ELEM+6(4),2(R4)                                                  
*                                                                               
ST060    CH    RF,=H'5'            TEST FOR SECOND STATION                      
         BNH   ST070                                                            
         OC    8(6,R4),8(R4)       YES - BUT TEST FOR NULL STATION              
         BZ    ST070                                                            
         CLC   12(2,R4),=C'FM'                                                  
         BNE   ST070                                                            
         MVC   ELEM+6(4),8(R4)                                                  
*                                                                               
ST070    BAS   RE,ADDEL            ADD THE AM/FM ELEMENT                        
*                                                                               
ST080    LA    R1,RSTAREC                                                       
         BAS   RE,PUT                                                           
         CLI   QOPTION3,C'1'                                                    
         BNE   ST100                                                            
         L     RE,ASTALIST                                                      
*                                                                               
ST090    CLC   0(5,RE),RSTAKSTA                                                 
         BE    ST100                                                            
         OC    0(5,RE),0(RE)                                                    
         BNZ   *+14                                                             
         MVC   0(5,RE),RSTAKSTA                                                 
         B     ST100                                                            
         LA    RE,6(RE)                                                         
         C     RE,ASTALSTX                                                      
         BL    ST090                                                            
         DC    H'0'                                                             
*                                                                               
ST100    B     CV040                                                            
         DROP  R8                                                               
*                                                                               
CHANPACK PACK  DUB,WORK(0)         * EXECUTED                                   
STCMTMOV MVC   ELEM+4(0),2(R4)     *                                            
         EJECT                                                                  
*                                                                               
* SALESMAN RECORD                                                               
*                                                                               
         SPACE                                                                  
         USING MSALREC,R8                                                       
SAL      CLC   MSALKREP,QUESTOR                                                 
         BNE   CV060                                                            
         AP    0(8,R2),=P'1'       AUGMENT RECORD COUNTER                       
*                                                                               
         CLC   MSALKSAL(2),=C'$$'  IGNORE $$ SALESPEOPLE                        
         BE    CV060                                                            
         CLC   MSALKSAL,=C'BUGT'   IGNORE BUDGET SALESPERSON                    
         BE    CV060                                                            
         LA    R1,MSALKSAL                                                      
         BAS   RE,CONVSP           CONVERT THE SP CODE                          
         BE    *+6                                                              
         DC    H'0'                                                             
*        MVI   INFCODE,6           CHECK FOR SALESPERSON ALREADY EXISTS         
*        BAS   RE,FIRSTINF                                                      
*        B     *+8                                                              
*        BAS   RE,NEXTINF                                                       
*        BNE   SP010                                                            
*        CLC   WORK(3),4(R4)                                                    
*        BNE   *-14                                                             
*        BAS   RE,CV050            SALESPERSON FOUND                            
*                                                                               
SP010    BAS   RE,SPOFFC           FIND SP OFFICE                               
         CLI   0(R1),C'0'          CHECK SP RECORD ALREADY ADDED                
         BNE   CV060               YES                                          
         MVI   0(R1),C'1'                                                       
*                                                                               
         XC    RSALREC(120),RSALREC                                             
         MVI   RSALKTYP,6          BUILD SALESPERSON KEY                        
         MVC   RSALKREP,REPCODE                                                 
*        MVC   RSALKREP,QREP                                                    
         MVC   RSALKSAL,WORK                                                    
         MVI   RSALLEN+1,120                                                    
*                                                                               
         MVI   RSALELEM,1          BUILD 01 ELEMENT                             
         MVI   RSALELLN,86                                                      
         CLC   REPCODE,=C'TO'                                                   
         BE    SP020                                                            
         MVC   RSALNAME(3),WORK    NAME                                         
         MVC   RSALNAME+3(17),SPACES                                            
         MVC   RSALTEL,=C'999-999-9999'  TELEPHONE                              
         B     SP030                                                            
*                                                                               
SP020    MVC   RSALNAME,MSALNAME   TORBET NAME                                  
         MVC   RSALTEL,MSALNMBR    TORBET TELEPHONE                             
*                                                                               
SP030    MVC   RSALTEAM,=C'R '     RADIO TEAM                                   
         MVC   RSALOFF,WORK+3      SALESMAN OFFICE                              
         MVI   RSALPROF+2,C'0'     PROFILE                                      
         MVC   RSALPROF+3(27),RSALPROF+2                                        
*                                                                               
         LA    R1,RSALREC          PUT SALESPERSON RECORD                       
         BAS   RE,PUT                                                           
         B     CV040                                                            
         DROP  R8                                                               
         EJECT                                                                  
*                                                                               
* AGENCY RECORD                                                                 
*                                                                               
         SPACE                                                                  
         USING MAGYREC,R8                                                       
AGY      CLC   MAGYKREP,QUESTOR                                                 
         BNE   CV060                                                            
         AP    0(8,R2),=P'1'       AUGMENT RECORD COUNTER                       
*                                                                               
         CLI   QOPTION3,C'2'       SEE IF ONLY LOADING USED AGENCIES            
         BNE   AG005                                                            
         L     RE,ASCAGY           CHECK IF AGENCY IS USED                      
*                                                                               
AG003    CLI   0(RE),X'FF'                                                      
         BE    CV060               UNUSED                                       
         CLC   0(6,RE),MAGYKAGY                                                 
         BE    AG005               USED AGENCY                                  
         LA    RE,6(RE)                                                         
         B     AG003                                                            
*                                                                               
AG005    XC    RAGYREC(156),RAGYREC   BUILD AGENCY KEY                          
         MVI   RAGYKTYP,X'0A'                                                   
         MVC   RAGYKAGY,MAGYKAGY                                                
         MVC   RAGYKAOF,MAGYKAOF                                                
         MVC   RAGYKREP,QREP                                                    
*        BAS   RE,GETAGY           SEE IF AGENCY ALREADY EXISTS                 
*        BNE   AG010                                                            
*        CLC   KEY(27),KEYSAVE                                                  
*        BNE   *+8                                                              
*        BAS   RE,CV050            AGENCY FOUND                                 
*                                                                               
AG010    MVC   RAGYKREP,REPCODE                                                 
*G010    MVC   RAGYKREP,QREP                                                    
         MVI   RAGYLEN+1,156                                                    
         MVI   RAGYCODE,1          BUILD 01 ELEMENT                             
         MVI   RAGYELLN,122                                                     
         MVC   RAGYNAM1,MAGYNAM1                                                
         MVC   RAGYNAM2,MAGYNAM2                                                
         MVC   RAGYADD1,MAGYADD1                                                
         MVC   RAGYADD2,MAGYADD2                                                
         MVC   RAGYSTAT,SPACES                                                  
         MVC   RAGYZIP,=C'00000'                                                
*                                                                               
         LA    R1,RAGYREC          PUT AGENCY RECORD                            
         BAS   RE,PUT                                                           
         CLI   QOPTION3,C'1'       CHECK FOR AGY LIST OPTION                    
         BNE   AG030                                                            
         L     RE,AAGYLIST         BUILD LIST OF AGENCIES                       
*                                                                               
AG020    CLC   0(6,RE),MAGYKAGY                                                 
         BE    AG030               FOUND                                        
         OC    0(6,RE),0(RE)                                                    
         BNZ   *+14                                                             
         MVC   0(6,RE),MAGYKAGY    ADD                                          
         B     AG030                                                            
         LA    RE,7(RE)                                                         
         C     RE,AAGYLSTX                                                      
         BL    AG020                                                            
         DC    H'0'                                                             
*                                                                               
AG030    B     CV040                                                            
         DROP  R8                                                               
         EJECT                                                                  
*                                                                               
* ADVERTISER RECORD                                                             
*                                                                               
         SPACE                                                                  
         USING MADVREC,R8                                                       
ADV      CLC   MADVKREP,QUESTOR                                                 
         BNE   CV060                                                            
         AP    0(8,R2),=P'1'       AUGMENT RECORD COUNTER                       
*                                                                               
         CLI   QOPTION3,C'2'       CHECK FOR ONLY LOADING USED ADV              
         BNE   AD005                                                            
         L     RE,ASCADV           CHECK IF ADVERTISER IS USED                  
*                                                                               
AD003    CLI   0(RE),X'FF'                                                      
         BE    CV060               UNUSED                                       
         CLC   0(4,RE),MADVKADV                                                 
         BE    AD005               USED ADVERTISER                              
         LA    RE,4(RE)                                                         
         B     AD003                                                            
*                                                                               
AD005    XC    RADVREC(100),RADVREC   BUILD ADVERTISER KEY                      
         MVI   RADVKTYP,X'08'                                                   
         MVC   RADVKADV,MADVKADV                                                
         MVC   RADVKREP,QREP                                                    
*        BAS   RE,GETADV           SEE IF ADV ALREADY EXISTS                    
*        BNE   AD010                                                            
*        CLC   KEY(27),KEYSAVE                                                  
*        BNE   *+8                                                              
*        BAS   RE,CV050            ADVERTISER FOUND                             
*                                                                               
AD010    MVC   RADVKREP,REPCODE                                                 
*D010    MVC   RADVKREP,QREP                                                    
         MVI   RADVLEN+1,100                                                    
         MVI   RADVCODE,1          BUILD 01 ELEMENT                             
         MVI   RADVELLN,66                                                      
         MVC   RADVNAME,MADVNAME                                                
         MVC   RADVCITY,SPACES                                                  
         MVC   RADVCLSS,SPACES                                                  
         MVC   RADVCATG,SPACES                                                  
*                                                                               
         LA    R1,RADVREC          PUT ADVERTISER RECORD                        
         BAS   RE,PUT                                                           
         CLI   QOPTION3,C'1'       CHECK FOR LISTING ADV                        
         BNE   AD030                                                            
         L     RE,AADVLIST         BUILD LIST OF ADVERTISERS                    
*                                                                               
AD020    CLC   0(4,RE),MADVKADV                                                 
         BE    AD030               FOUND                                        
         OC    0(4,RE),0(RE)                                                    
         BNZ   *+14                                                             
         MVC   0(4,RE),MADVKADV    ADD                                          
         B     AD030                                                            
         LA    RE,5(RE)                                                         
         C     RE,AADVLSTX                                                      
         BL    AD020                                                            
         DC    H'0'                                                             
*                                                                               
AD030    B     CV040                                                            
         DROP  R8                                                               
         EJECT                                                                  
*                                                                               
* CONTRACT RECORD                                                               
*                                                                               
         SPACE                                                                  
         USING MCONREC,R8          CONTRACT RECORD                              
CON      CLC   MCONKREP,QUESTOR                                                 
         BNE   CV060                                                            
         AP    0(8,R2),=P'1'       AUGMENT RECORD COUNTER                       
*                                                                               
         MVI   CELCODE,X'10'       FIND DESCRIPTION RECORD                      
         LA    R4,MCONKEY+36                                                    
         BAS   RE,GETCEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   4(2,R4),=X'7C1E'    COMPARE END DATE TO 12/30/84                 
         BL    CV060               BEFORE - REJECT                              
         CLC   REPCODE,=C'TO'      CHECK FOR TORBET                             
         BE    *+14                                                             
         CLC   4(2,R4),=X'8C1D'    COMPARE END DATE TO 12/29/85                 
         BH    CV060               AFTER - REJECT                               
         ST    R4,ACONDISC         SAVE A(DESCRIPTION ELEMENT)                  
*                                                                               
         MVI   RCONKTYP,12         BUILD CONTRACT KEY                           
         MVC   RCONKREP,REPCODE                                                 
*        MVC   RCONKREP,QREP                                                    
         MVC   RCONKSTA(4),MCONKSTA                                             
         LA    R1,MCONKMED                                                      
         BAS   RE,STAMED           FIND THE STATION'S MEDIA                     
         MVC   RCONKSTA+4(1),0(R1)                                              
         MVI   RCONKGRP,C'R'       GROUP = R                                    
         MVC   RCONKGRP+1(1),0(R1) SUBGROUP                                     
         CLC   MCONKSAL(2),=C'$$'  IGNORE CONTRACT FOR $$ SALESPERSON           
         BE    CV060                                                            
         CLC   MCONKSAL,=C'BUGT'   IGNORE BUDGET SALESPERSON CONTRACT           
         BE    CV060                                                            
         LA    R1,MCONKSAL                                                      
         BAS   RE,CONVSP           GET DDS SP CODE                              
         BNE   CV060               UNKNOWN - REJECT THIS CONTRACT               
         BAS   RE,SPOFFC           GET SP OFFICE                                
         MVC   RCONKOFF,WORK+3     SALESPERSON OFFICE                           
         MVC   RCONKAGY,MCONKAGY   AGENCY                                       
         MVC   RCONKAOF,MCONKAOF   AGENCY OFFICE                                
         MVC   RCONKADV,MCONKADV   ADVERTISER                                   
*        ZAP   PCONNUM,=P'0'       CONTRACT NUMBER                              
*        MVO   PCONNUM,MCONKCON+1(3)                                            
*        AP    PCONNUM,=P'100000'  ADD 100,000 FOR TS                           
*        CLC   REPCODE,=C'TS'                                                   
*        BE    *+10                                                             
*        AP    PCONNUM,=P'200000'  ADD 300,000 FOR SC                           
*        LM    R0,R1,PCONNUM                                                    
*        SRDL  R0,4                                                             
*        STCM  R1,15,RCONKCON      CONTRACT NUMBER                              
*        ST    R1,CONTNUM                                                       
         MVC   RCONKCON+1(3),MCONKCON+1                                         
         MVC   CONTNUM,RCONKCON                                                 
         CLC   CONTNUM,HICONT      SAVE HIGHEST CONTRACT NUMBER                 
         BNH   *+10                                                             
         MVC   HICONT,CONTNUM                                                   
         MVI   RCONLEN+1,X'5E'     RECORD LENGTH                                
*                                                                               
         MVI   RCONCODE,1          BUILD 01 ELEMENT                             
         MVI   RCONELLN,X'3C'                                                   
         MVC   RCONBUYR,SPACES                                                  
         MVI   CELCODE,X'20'       GET MPI BUYER ELEM                           
         LA    R4,MCONKEY+36                                                    
         BAS   RE,GETCEL                                                        
         BNE   *+8                                                              
         EX    RF,BUYRMOVE         BUYER                                        
         MVC   RCONPRD,SPACES      PRODUCT                                      
         MVC   RCONTEM,=CL2'R '    RADIO TEAM                                   
         MVC   RCONSAL,WORK        DDS SP CODE STILL IN WORK                    
         L     R4,ACONDISC                                                      
         LA    R1,2(R4)                                                         
         BAS   RE,DATCONV                                                       
         MVC   RCONDATE(3),WORK    JOIN DATE                                    
         LA    R1,4(R4)                                                         
         BAS   RE,DATCONV                                                       
         MVC   RCONDATE+3(3),WORK  LEAVE DATE                                   
         MVC   RCONMOD,7(R4)       MODIFICATION NUMBER                          
         LA    R1,8(R4)                                                         
         BAS   RE,DATCONV                                                       
         MVC   RCONMODD,WORK       DATE OF LAST MODIFICATION                    
         XC    RCONMODR,RCONMODR   LAST MODIFICATION REASON ***                 
         LA    R1,16(R4)           CREATION DATE                                
         BAS   RE,DATCONV                                                       
         MVC   RCONCREA,WORK                                                    
         XC    RESDATE,RESDATE     (INITIALIZE RESOLUTION DATE)                 
         MVI   RCONRTGS,0                                                       
         TM    12(R4),X'80'        RATING SERVICE                               
         BZ    *+12                                                             
         MVI   RCONRTGS,C'N'                                                    
         B     CN020                                                            
         TM    12(R4),X'40'                                                     
         BZ    CN020                                                            
         MVI   RCONRTGS,C'A'                                                    
*                                                                               
CN020    MVC   RCONWKS,1(R4)       NUMBER OF WEEKS                              
         MVC   WEEKS,1(R4)                                                      
         MVC   RCONHDRD,RCONCREA   HEADER CREATION DATE                         
         MVC   CREADATE,RCONHDRD                                                
         MVI   RCONTYPE,0          CONTRACT TYPE                                
         CLI   10(R4),C'N'         NETWORK                                      
         BE    *+12                                                             
         CLI   10(R4),C'S'         SPOT                                         
         BNE   *+10                                                             
         MVC   RCONTYPE,10(R4)                                                  
         MVC   RCONCTGY,=C'CV'     CATEGORY CODE - DEFAULT                      
*                                                                               
         L     R1,AOUTREC                                                       
         MVC   0(94,R1),RCONREC    MOVE FIRST PART OF RECORD TO OUT             
*                                                                               
         CLI   QOPTION3,C'1'       CHECK FOR LISTING USED AGY/ADV               
         BNE   CN030                                                            
         L     RE,AAGYLIST         LOOK FOR AGY IN LIST                         
*                                                                               
CN023    CLC   0(6,RE),RCONKAGY                                                 
         BNE   *+12                                                             
         MVI   6(RE),C'Y'          FOUND - INDICATE AGY USED                    
         B     CN025                                                            
         OC    0(6,RE),0(RE)                                                    
         BNZ   *+18                                                             
         MVC   0(6,RE),RCONKAGY    END OF LIST - ADD AGY TO LIST                
         MVI   6(RE),C'Y'                        AND INDICATE USED              
         B     CN025                                                            
         LA    RE,7(RE)                                                         
         C     RE,AAGYLSTX                                                      
         BL    CN023                                                            
         DC    H'0'                                                             
*                                                                               
CN025    L     RE,AADVLIST         LOOK FOR ADV IN LIST                         
*                                                                               
CN027    CLC   0(4,RE),RCONKADV                                                 
         BNE   *+12                                                             
         MVI   4(RE),C'Y'          FOUND - INDICATE ADV USED                    
         B     CN028                                                            
         OC    0(4,RE),0(RE)                                                    
         BNZ   *+18                                                             
         MVC   0(4,RE),RCONKADV    END OF LIST - ADD ADV TO LIST                
         MVI   4(RE),C'Y'                        AND INDICATE USED              
         B     CN028                                                            
         LA    RE,5(RE)                                                         
         C     RE,AADVLSTX                                                      
         BL    CN027                                                            
         DC    H'0'                                                             
*                                                                               
CN028    L     RE,ASTALIST         LOOK FOR STATION IN LIST                     
*                                                                               
CN029    CLC   0(5,RE),RCONKSTA                                                 
         BNE   *+12                                                             
         MVI   5(RE),C'Y'          FOUND - INDICATE STA USED                    
         B     CN030                                                            
         OC    0(5,RE),0(RE)                                                    
         BNZ   *+18                                                             
         MVC   0(5,RE),RCONKSTA    END OF LIST - ADD STA TO LIST                
         MVI   5(RE),C'Y'                        AND INDICATE USED              
         B     CN030                                                            
         LA    RE,6(RE)                                                         
         C     RE,ASTALSTX                                                      
         BL    CN029                                                            
         DC    H'0'                                                             
         SPACE 2                                                                
BUYRMOVE MVC   RCONBUYR(0),1(R4)   * EXECUTED                                   
         EJECT                                                                  
*                                                                               
* CONTRACT COMMENT ELEMENT - NOW NOT NEEDED                                     
*                                                                               
         SPACE                                                                  
*        MVI   NELCODE,1           FIND MPI COMMENT ELEMENT                     
*        BAS   RE,GETNEL                                                        
*        BNE   CN030                                                            
*        CH    RF,=H'59'           COMMENT MUST BE LE 60 CHARS                  
*        BNH   *+8                                                              
*        LH    RF,=H'59'                                                        
*        MVI   ELEM,2              BUILD COMMENT ELEMENT                        
*        LA    RE,3(RF)                                                         
*        STC   RE,ELEM+1                                                        
*        EX    RF,CNCMTMOV                                                      
*        BAS   RE,ADDEL            ADD IT                                       
*                                                                               
*        BAS   RE,NEXTNEL          LOOK FOR 2ND COMMENT                         
*        BNE   CN030                                                            
*        CH    RF,=H'59'           COMMENT MUST BE LE 60 CHARS                  
*        BNH   *+8                                                              
*        LH    RF,=H'59'                                                        
*        MVI   ELEM,2              BUILD 2ND COMMENT ELEMENT                    
*        LA    RE,3(RF)                                                         
*        STC   RE,ELEM+1                                                        
*        EX    RF,CNCMTMOV                                                      
*        BAS   RE,ADDEL            ADD IT                                       
         EJECT                                                                  
*                                                                               
* CONTRACT BUCKET ELEMENTS                                                      
*                                                                               
         SPACE                                                                  
CN030    MVI   CELCODE,X'B0'       BUILD ESTIMATE BUCKETS                       
         BAS   RE,ADDBUCK                                                       
*                                                                               
         MVI   CELCODE,X'C0'       BUILD INVOICE BUCKETS                        
         BAS   RE,ADDBUCK                                                       
         EJECT                                                                  
*                                                                               
* CONTRACT EXPANSION ELEMENT (PRODUCT NAME)                                     
*                                                                               
         SPACE                                                                  
         LA    R4,MCONKEY+36       LOOK FOR MPI PRODUCT ELEMENT                 
         MVI   CELCODE,X'30'                                                    
         BAS   RE,GETCEL                                                        
         BNE   CN040                                                            
         CLC   1(2,R4),=C'##'      MUST BE NO PRODUCT CODES                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         CH    RF,=H'19'           MAKE SURE PRODUCT LENGTH LE 20               
         BNH   *+8                                                              
         LH    RF,=H'19'                                                        
         MVI   ELEM,5              BUILD CONTRACT EXPANSION ELEMENT             
         MVI   ELEM+1,22                                                        
         MVC   ELEM+2(20),SPACES                                                
         EX    RF,CNPRDMOV         MOVE IN THE PRODUCT                          
         BAS   RE,ADDEL            ADD THE ELEMENT                              
         B     CN040                                                            
         SPACE 2                                                                
CNPRDMOV MVC   ELEM+2(0),1(R4)     * EXECUTED                                   
         EJECT                                                                  
*                                                                               
* CONTRACT EPL ELEMENT                                                          
*                                                                               
         SPACE                                                                  
CN040    MVI   NELCODE,X'0C'       LOOK FOR EPL ELEMENT                         
         LA    R4,MCONREC+36                                                    
         BAS   RE,GETNEL                                                        
         BNE   CN100                                                            
         LA    R5,1                INIT STATION COUNT                           
         LA    R6,ELEM+18          FIRST COMP STATION POSITION                  
         LA    R7,EPLCMT           EPL COMMENT SAVE AREA                        
         MVI   0(R7),C' '                                                       
         MVC   1(255,R7),0(R7)     560 (8 X 70) CHARS                           
         MVC   256(256,R7),0(R7)                                                
         MVC   512(48,R7),0(R7)                                                 
         MVI   ELEM,6              EPL ELEMENT ID                               
*                                                                               
CN050    CLI   2(R4),0             BASE RECORD                                  
         BNE   CN060                                                            
         MVC   HALF(1),3(R4)       COMPETITIVE Y/M                              
         MVI   HALF+1,1                                                         
         LA    R1,HALF                                                          
         BAS   RE,DATCONV                                                       
         MVC   ELEM+2(2),WORK      REPORTING Y/M                                
         MVI   ELEM+4,0                                                         
         CLI   12(R4),100          TEST FOR 100 PERCENT OF MARKET               
         BNE   *+8                                                              
         OI    ELEM+4,X'80'                                                     
         LA    R1,6(R4)            CREATION/ACTIVITY DATE                       
         BAS   RE,DATCONV                                                       
         OC    WORK(3),WORK        IS IT NON-ZERO ?                             
         BZ    CN055                                                            
         OC    RESDATE,RESDATE     YES - SET THE RESOLUTION DATE                
         BZ    *+14                                                             
         CLC   RESDATE,WORK                                                     
         BNH   CN055                                                            
         MVC   RESDATE,WORK                                                     
*                                                                               
CN055    MVC   ELEM+5(3),WORK      ENTRY DATE                                   
         MVC   ELEM+9(5),RCONKSTA  THIS STATION                                 
         MVC   ELEM+14(4),8(R4)    THIS STATION DOLLARS                         
         B     CN090                                                            
*                                                                               
CN060    CLI   2(R4),1             COMP STATION ELEMENT                         
         BNE   CN080                                                            
         MVC   0(4,R6),4(R4)       MOVE COMP STATION                            
         LA    R1,8(R4)                                                         
         BAS   RE,STAMED                                                        
         MVC   4(1,R6),0(R1)       MOVE THE STATION'S MEDIA                     
         LA    R5,1(R5)                                                         
         LA    R6,9(R6)                                                         
         B     CN090                                                            
*                                                                               
CN080    CLI   2(R4),2             EPL COMMENT LINE                             
         BE    *+6                                                              
         DC    H'0'                                                             
         C     R7,=A(EPLCMTX)      TEST FOR COMMENT SAVE AREA FULL              
         BNL   CN090                                                            
         LA    R1,0(RF,R7)                                                      
         C     R1,=A(EPLCMTX)      TEST FOR ROOM FOR WHOLE LINE                 
         BNH   CN085                                                            
         L     R1,=A(EPLCMTX)      NO - MOVE WHATEVER THERE'S ROOM FOR          
         SR    R1,R7                                                            
         BCTR  R1,0                                                             
         EX    R1,CNEPLCMT                                                      
         L     R7,=A(EPLCMTX)                                                   
         B     CN090                                                            
*                                                                               
CN085    EX    RF,CNEPLCMT         MOVE COMMENT LINE                            
         AR    R7,RF                                                            
         BCTR  R7,0                                                             
         MVI   0(R7),C' '          ADD A BLANK AFTER                            
         LA    R7,1(R7)            POSITION FOR NEXT LINE                       
*                                                                               
CN090    BAS   RE,NEXTNEL          GET ANOTHER EPL ELEMENT                      
         BE    CN050                                                            
         STC   R5,ELEM+8           NO - SET NO OF MINI ELEMENTS                 
         MH    R5,=H'9'                                                         
         LA    R5,9(R5)                                                         
         STC   R5,ELEM+1           EPL ELEMENT LENGTH                           
         BAS   RE,ADDEL            ADD THE SPL ELEMENT                          
         B     CN095                                                            
         SPACE 2                                                                
CNEPLCMT MVC   0(0,R7),3(R4)       * EXECUTED                                   
         EJECT                                                                  
*                                                                               
* CONTRACT EPL COMMENT ELEMENT                                                  
*                                                                               
         SPACE                                                                  
CN095    S     R7,=A(EPLCMT)       GET LENGTH OF EPL COMMENT                    
         BNP   CN100                                                            
         GOTO1 SQUASHER,DMCB,EPLCMT,(R7)                                        
         ICM   R7,15,DMCB+4        SQUASHED LENGTH                              
         BZ    CN100                                                            
         LA    R5,4                ALLOW 4 LINES                                
*                                                                               
CN097    LR    R6,R7               CALCULATE LENGTH OF THIS ELEM                
         LA    RE,EPLCMT                                                        
         CH    R6,=H'65'                                                        
         BNH   *+8                                                              
         LH    R6,=H'65'                                                        
         BCTR  R6,0                                                             
         EX    R6,EPLCMTMV         MOVE COMMENT TO ELEMENT                      
         MVI   ELEM,7              EPL COMMENT ID                               
         LA    R6,3(R6)                                                         
         STC   R6,ELEM+1           ELEMENT LENGTH                               
         BAS   RE,ADDEL            ADD THE ELEMENT                              
         BCTR  R6,0                                                             
         BCTR  R6,0                                                             
         AR    RE,R6               NEXT CHUNK OF COMMENT                        
         SR    R7,R6               REMAINING LENGTH                             
         BNP   CN100               DONE                                         
         BCT   R5,CN097            NEXT LINE                                    
         B     CN100                                                            
         SPACE                                                                  
EPLCMTMV MVC   ELEM+2(0),0(RE)                                                  
*                                                                               
*** DECIDE WHAT TO DO WITH THE SPL COMMENT ELEMENT X'03' ***                    
*** - ONLY IF REP USED TV SPL SCREEN. MAYBE TORBET ?? SELCOM DOESN'T *          
         EJECT                                                                  
*                                                                               
* CONTRACT BOP ELEMENT                                                          
*                                                                               
         SPACE                                                                  
CN100    MVI   ELEM,X'10'                                                       
         MVI   ELEM+1,90                                                        
         MVC   ELEM+2(3),CREADATE  DATE BOP ADDED/CHANGED                       
         MVC   ELEM+5(4),CONTNUM   REFERENCE CONTRACT NUMBER                    
         MVC   ELEM+9(20),SPACES   PREFFERED TIMES (DAYPARTS)                   
         MVI   CELCODE,X'50'                                                    
         BAS   RE,GETCEL           LOOK FOR DAYPARTS ELEMENT                    
         BNE   CN140                                                            
         LA    R5,5                MAX DDS DAYPARTS                             
         LA    R6,1(R4)            FIRST MPI DAYPART                            
         LA    R7,ELEM+9           A(DDS DAYPARTS)                              
         LA    RF,1(RF)            NUMBER OF MPI DAYPARTS                       
*                                                                               
CN110    L     RE,ADPTLIST         CONVERT MPI DAYPARTS                         
*                                                                               
CN120    CLI   0(RE),X'FF'                                                      
         BE    CN130                                                            
         CLC   0(1,RE),0(R6)                                                    
         BE    *+12                                                             
         LA    RE,4(RE)                                                         
         B     CN120                                                            
         MVC   0(3,R7),1(RE)                                                    
         MVI   3(R7),C','                                                       
         LA    R7,4(R7)                                                         
         BCT   R5,CN130                                                         
         B     CN140                                                            
*                                                                               
CN130    LA    R6,1(R6)                                                         
         BCT   RF,CN110                                                         
*                                                                               
CN140    MVC   ELEM+29(20),SPACES  BUYING OBJECTIVES                            
         MVI   CELCODE,X'40'                                                    
         LA    R4,MCONKEY+36       LOOK FOR MPI BUDGET ELEMENT                  
         BAS   RE,GETCEL                                                        
         BNE   CN150                                                            
         CH    RF,=H'19'           MAX 20 CHARS                                 
         BNH   *+8                                                              
         LH    RF,=H'19'                                                        
         EX    RF,CNBOPBUD                                                      
*                                                                               
CN150    MVC   ELEM+49(20),SPACES  DEMOGRAPHICS                                 
         MVC   ELEM+69(5),SPACES   MARKET                                       
         L     R4,ACONDISC                                                      
         MVC   BYTE,6(R4)          PRIMARY DEMO                                 
         CLI   BYTE,0                                                           
         BNE   CN160                                                            
         MVI   CELCODE,X'80'       NO PRIMARY DEMO - LOOK FOR DEMO ELEM         
         LA    R4,MCONKEY+36                                                    
         BAS   RE,GETCEL                                                        
         BNE   CN190                                                            
         LTR   RF,RF                                                            
         BM    CN190                                                            
         MVC   BYTE,1(R4)          PRIMARY DEMO FIRST IN DEMO ELEM              
*                                                                               
CN160    CLI   BYTE,225            TEST FOR DEMO CODE 225 OR LESS               
         BH    CN170                                                            
         ZIC   R6,BYTE                                                          
         SRDL  R6,32                                                            
         D     R6,=F'75'           DIVIDE CODE BY 75                            
         BCTR  R6,0                REMAINDER IS INDEX FOR CONVERSION            
         BNM   *+8                                                              
         A     R6,=F'75'           DEMO CODE 74 (MOD 75)                        
         SLL   R6,3                                                             
         L     RE,ADEMLIST                                                      
         AR    RE,R6                                                            
         MVC   ELEM+49(7),1(RE)    DDS DEMO NAME                                
         LTR   R7,R7               R7 INDICATES THE MARKET                      
         BNZ   *+14                                                             
         MVC   ELEM+69(5),=C'TSA  '                                             
         B     CN190                                                            
         BCTR  R7,0                                                             
         LTR   R7,R7                                                            
         BNZ   *+14                                                             
         MVC   ELEM+69(5),=C'ADI  '                                             
         B     CN190                                                            
         MVC   ELEM+69(5),=C'METRO'                                             
         B     CN190                                                            
*                                                                               
CN170    L     RE,ADEMLIS2         DEMO CODE 226 OR MORE                        
*                                                                               
CN180    CLI   0(RE),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   BYTE,0(RE)                                                       
         BE    *+12                                                             
         LA    RE,13(RE)                                                        
         B     CN180                                                            
         MVC   ELEM+49(7),1(RE)                                                 
         MVC   ELEM+69(5),8(RE)                                                 
*                                                                               
CN190    MVC   ELEM+74(5),SPACES   COMMERCIAL LENGTH                            
         MVI   ELEM+79,C'Y'        BOP REPORT                                   
         MVC   ELEM+80(1),WEEKS    AIR WEEKS                                    
*                                                                               
         BAS   RE,ADDEL            ADD THE BOP ELEMENT                          
         B     CN195                                                            
         SPACE 2                                                                
CNBOPBUD MVC   ELEM+29(0),1(R4)    * EXECUTED                                   
         EJECT                                                                  
*                                                                               
* CONTRACT BOP COMMENT ELEMENT                                                  
*                                                                               
         SPACE                                                                  
CN195    MVI   NELCODE,X'02'       LOOK FOR ACTIVITY COMMENT ELEMENT            
         LA    R4,MCONKEY+36                                                    
         BAS   RE,GETNEL                                                        
         BNE   CN210                                                            
         CLC   2(2,R4),=C'##'      IGNORE COMMENT CODES                         
         BE    CN210                                                            
         LA    R5,4                ALLOW FOR 4 X 35 CHAR BOP COMMENTS           
*                                                                               
CN200    LR    R6,RF                                                            
         CH    R6,=H'34'           MAX 35 CHARS                                 
         BNH   *+8                                                              
         LH    R6,=H'34'                                                        
         MVI   ELEM,7              BUILD BOP COMMENT ELEMENT                    
         EX    R6,CNBOPMOV         MOVE THE COMMENT                             
         LA    R6,3(R6)            ELEMENT LENGTH                               
         STC   R6,ELEM+1                                                        
         BAS   RE,ADDEL            ADD THE ELEMENT                              
         SH    RF,=H'35'                                                        
         BM    CN210               END OF COMMENT                               
         BCT   R5,CN200            MOVE NEXT PART OF COMMENT                    
         B     CN210                                                            
*                                                                               
CNCMTMOV MVC   ELEM+2(0),2(R4)     * EXECUTED                                   
CNBOPMOV MVC   ELEM+2(0),2(R4)     *                                            
         EJECT                                                                  
CN210    L     R1,AOUTREC          PUT CONTRACT RECORD                          
         OC    RESDATE,RESDATE     SET THE RESOLUTION DATE, IF ANY              
         BZ    *+10                                                             
         MVC   RCONCREA-RCONELEM+34(3,R1),RESDATE                               
         BAS   RE,PUT                                                           
         B     CV040                                                            
         EJECT                                                                  
*                                                                               
* ROUTINE TO LOOK FOR BUCKET ELEMENTS AND ADD THEM TO DDS FILE                  
* INPUT  : CELCODE = MPI BUCKET ELEMENT CODE                                    
*                                                                               
         SPACE                                                                  
ADDBUCK  NTR1                                                                   
         LA    R4,MCONREC+36                                                    
         BAS   RE,GETCEL           GET FIRST MPI BUCKET ELEMENT                 
         B     *+8                                                              
*                                                                               
AB010    BAS   RE,NEXTCEL          NEXT MPI BUCKET ELEMENT                      
         BNE   EXIT                                                             
         LR    R5,RF                                                            
         MVI   ELEM,3                                                           
         CLI   CELCODE,X'B0'                                                    
         BE    *+8                                                              
         MVI   ELEM,4                                                           
         MVI   ELEM+1,10           ELEMENT LENGTH                               
         MVC   HALF(1),1(R4)       GET Y/M OF SERVICE                           
         MVI   HALF+1,1                                                         
         LA    R1,HALF                                                          
         BAS   RE,DATCONV                                                       
         MVC   ELEM+2(2),WORK      YEAR/MONTH                                   
         LA    R1,2(R4)            MONDAY ACTIVITY DATE                         
         BAS   RE,DATCONV                                                       
         GOTO1 DATCON,DMCB,(3,WORK),(2,ELEM+4)                                  
*                                                                               
         ICM   RE,15,4(R4)         AMOUNT                                       
         LA    R5,2(R5)            GET LENGTH OF ELEMENT                        
         CH    R5,=H'6'            TEST FOR NO PENNIES                          
         BE    *+14                                                             
         CH    R5,=H'8'            TEST FOR AMOUNT IN PENNIES                   
         BE    *+14                                                             
         DC    H'0'                                                             
         SRA   RE,16               NO PENNIES- ONLY 2 BYTES FOR DOLLARS         
         MH    RE,=H'100'          X 100 FOR PENNIES                            
         STCM  RE,15,ELEM+6        STORE AMOUNT IN ELEMENT                      
*                                                                               
         LTR   RE,RE               TEST FOR NON-ZERO AMOUNT                     
         BZ    AB020                                                            
         OC    RESDATE,RESDATE     YES - SET THE RESOLUTION DATE                
         BZ    *+14                                                             
         CLC   RESDATE,WORK                                                     
         BNH   AB020                                                            
         MVC   RESDATE,WORK                                                     
*                                                                               
AB020    BAS   RE,ADDEL            ADD THE DDS BUCKET ELEMENT                   
         B     AB010               NEXT BUCKET                                  
         DROP  R8                                                               
         EJECT                                                                  
*                                                                               
* ROUTINE TO ADD AN ELEMENT TO RECORD IN OUTREC                                 
*                                                                               
         SPACE                                                                  
ADDEL    NTR1                                                                   
         L     R2,AOUTREC                                                       
         GOTO1 =V(HELLO),DMCB,(C'P',REPFILE),(R2),ELEM                          
         XC    ELEM,ELEM                                                        
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* CONVERT 2-BYTE MPI STATION MEDIA CODE TO 1-BYTE DDS CODE                      
* INPUT  : R1 = A(MPI CODE)                                                     
* OUTPUT : R1 = A(DDS CODE)                                                     
*                                                                               
         SPACE                                                                  
STAMED   LA    RF,MEDCODES         FIND THE STATION'S MEDIA                     
*                                                                               
SM010    CLI   0(RF),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(2,R1),0(RF)                                                    
         BE    *+12                                                             
         LA    RF,3(RF)                                                         
         B     SM010                                                            
         LA    R1,2(RF)                                                         
         BR    RE                                                               
*                                                                               
MEDCODES DC    CL3'TV '            MEDIA CONVERSION TABLE                       
         DC    CL3'AMA'                                                         
         DC    CL3'FMF'                                                         
         DC    CL3'Q C'                                                         
         DC    CL3'AFC'                                                         
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
* CONVERT MPI SALESPERSON CODE TO DDS SALESPERSON CODE                          
* INPUT  : R1 = A(4-BYTE MPI CODE)                                              
* OUTPUT : WORK(3) = DDS CODE                                                   
*          CC EQ 0 : CODE FOUND                                                 
*          CC NE 0 : CODE NOT FOUND                                             
*                                                                               
         SPACE                                                                  
CONVSP   L     RF,ASALTABS         CONVERT MPI SP CODE TO DDS SP CODE           
         CLC   REPCODE,=C'TO'                                                   
         BNE   CS010                                                            
         L     RF,ASALTABT         TORBET SALESPERSON TABLE                     
*                                                                               
CS010    CLI   0(RF),X'FF'                                                      
         BNE   *+8                                                              
         LTR   RE,RE               NOT FOUND                                    
         BR    RE                                                               
         CLC   0(4,R1),0(RF)       MPI = 4 BYTES                                
         BE    CS020                                                            
         LA    RF,7(RF)                                                         
         B     CS010                                                            
*                                                                               
CS020    MVC   WORK(3),4(RF)       DDS = 3 BYTES                                
         CR    RE,RE                                                            
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
* FIND A DDS SALESPERSON'S OFFICE CODE                                          
* INPUT  : WORK(3) = SALESPERSON CODE                                           
* OUTPUT : WORK+3(2) = OFFICE CODE                                              
*          R1 = A(SP RECORD ADDED INDICATOR 0=NO,1=YES)                         
*                                                                               
         SPACE                                                                  
SPOFFC   L     RF,ASALDDSS         FIND SP IN DDS SP LIST (SELCOM)              
         CLC   REPCODE,=C'TO'                                                   
         BNE   SC010                                                            
         L     RF,ASALDDST                                                      
*                                                                               
SC010    CLI   0(RF),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   WORK(3),0(RF)                                                    
         BE    SC020                                                            
         LA    RF,6(RF)                                                         
         B     SC010                                                            
*                                                                               
SC020    MVC   WORK+3(2),3(RF)     SP OFFICE                                    
         LA    R1,5(RF)                                                         
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
* CONVERT 2-BYTE MPI DATE TO 3-BYTE DDS DATE                                    
* INPUT  : R1 = A(MPI 4/4/8 YMD DATE)                                           
* OUTPUT : WORK(3) = DDS DATE YMD                                               
*                                                                               
         SPACE                                                                  
DATCONV  XC    WORK,WORK                                                        
         OC    0(2,R1),0(R1)                                                    
         BZR   RE                                                               
         SR    R0,R0                                                            
         ICM   R0,3,0(R1)                                                       
         SRDL  R0,12               YEAR                                         
         AH    R0,=H'77'           BASE YEAR = 1977                             
         STC   R0,WORK                                                          
         SR    R0,R0               MONTH                                        
         SLDL  R0,4                                                             
         STC   R0,WORK+1                                                        
         SR    R0,R0               DAY                                          
         SLDL  R0,8                                                             
         STC   R0,WORK+2                                                        
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
* ROUTINE TO FIND INFORMATION ELEMENT FROM INFO POOL                            
* INPUT:  INFCODE = ELEMENT CODE                                                
*         OUTPUT: R4 = A(ELEMENT)                                               
*                                                                               
         SPACE                                                                  
FIRSTINF LA    R4,SPACEND+4                                                     
*                                                                               
FI010    CLI   0(R4),0                                                          
         BE    FI999                                                            
         CLC   INFCODE,0(R4)                                                    
         BER   RE                                                               
*                                                                               
NEXTINF  SR    R0,R0                                                            
         ICM   R0,1,1(R4)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R4,R0                                                            
         B     FI010                                                            
*                                                                               
FI999    LTR   RE,RE                                                            
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
* ROUTINE TO GET A NORMAL MPI ELEMENT (LIKE DDS ELEMENT)                        
* INPUT  : NELCODE = SOUGHT ELEMENT CODE                                        
*          R4      = A(FIRST ELEMENT)                                           
* OUTPUT : CC=EQ - R4 = A(FOUND ELEMENT)                                        
*                  RF = LENGTH FOR MOVE FROM ELEMENT+2                          
*          CC=NE - ELEMENT NOT FOUND                                            
*                                                                               
         SPACE                                                                  
GETNEL   DS    0H                                                               
GN005    CLI   0(R4),0                                                          
         BE    GN900                                                            
         CLI   0(R4),X'0F'                                                      
         BNH   GN010                                                            
         IC    R0,0(R4)                                                         
         N     R0,=X'0000000F'                                                  
         B     GN020                                                            
*                                                                               
NEXTNEL  SR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         B     GN020                                                            
*                                                                               
GN010    SR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLC   0(1,R4),NELCODE                                                  
         BNE   GN020                                                            
         LR    RF,R0                                                            
         SLL   RF,1                                                             
         SH    RF,=H'3'                                                         
         BNM   GN990                                                            
         DC    H'0'                                                             
*                                                                               
GN020    AR    R4,R0                                                            
         AR    R4,R0                                                            
         B     GN005                                                            
*                                                                               
GN900    LTR   R4,R4               NE - NOT FOUND                               
         BR    RE                                                               
*                                                                               
GN990    CR    R4,R4               EQ - FOUND                                   
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
* ROUTINE TO GET A COMPRESSED MPI ELEMENT                                       
* INPUT  : CELCODE = SOUGHT ELEMENT CODE                                        
*          R4      = A(FIRST ELEMENT)                                           
* OUTPUT : CC=EQ - R4 = A(FOUND ELEMENT)                                        
*                  RF = LENGTH FOR MOVE FROM ELEMENT+1                          
*          CC=NE - ELEMENT NOT FOUND                                            
*                                                                               
         SPACE                                                                  
GETCEL   DS    0H                                                               
GC005    CLI   0(R4),0                                                          
         BE    GC900                                                            
         CLI   0(R4),X'0F'                                                      
         BH    GC010                                                            
         SR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         LTR   R0,R0                                                            
         BNZ   GC030                                                            
         DC    H'0'                                                             
*                                                                               
GC010    MVC   BYTE,CELCODE                                                     
         OI    BYTE,X'0F'                                                       
         CLC   0(1,R4),BYTE                                                     
         BH    NEXTCEL                                                          
         CLC   0(1,R4),CELCODE                                                  
         BH    GC990               FOUND                                        
*                                                                               
NEXTCEL  IC    R0,0(R4)                                                         
         N     R0,=X'0000000F'                                                  
*                                                                               
GC030    AR    R4,R0                                                            
         AR    R4,R0                                                            
         B     GC005                                                            
*                                                                               
GC900    LTR   R4,R4               NE - NOT FOUND                               
         BR    RE                                                               
*                                                                               
GC990    IC    RF,0(R4)                                                         
         N     RF,=X'0000000F'                                                  
         SLL   RF,1                                                             
         SH    RF,=H'2'                                                         
         BNM   *+6                                                              
         DC    H'0'                                                             
         CR    R4,R4               EQ - FOUND                                   
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
* PUT A RECORD TO TAPE -- R1 HAS ADDRESS OF RECORD                              
*                                                                               
PUT      NTR1                                                                   
         L     RF,AOUTREC          TEST FOR RECORD ALREADY IN OUT AREA          
         CR    R1,RF                                                            
         BE    PT030                                                            
         SR    RE,RE               MOVE RECORD TO OUT AREA                      
         ICM   RE,3,27(R1)         RECORD LENGTH                                
*                                                                               
PT010    CH    RE,=H'256'                                                       
         BL    PT020                                                            
         MVC   0(256,RF),0(R1)                                                  
         LA    R1,256(R1)                                                       
         LA    RF,256(RF)                                                       
         SH    RE,=H'256'                                                       
         BZ    PT030                                                            
         B     PT010                                                            
*                                                                               
PT020    BCTR  RE,0                                                             
         EX    RE,PTMOVE                                                        
*                                                                               
PT030    L     R1,AOUTREC          FIND REP RECORD LENGTH                       
         LA    R1,34(R1)                                                        
         LA    RE,34               KEY LENGTH                                   
         SR    RF,RF                                                            
*                                                                               
PT035    CLI   0(R1),0                                                          
         BE    PT037                                                            
         ICM   RF,1,1(R1)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    RE,RF                                                            
         AR    R1,RF                                                            
         B     PT035                                                            
*                                                                               
PT037    L     R1,AOUTREC                                                       
         STCM  RE,3,27(R1)         STORE IT IN REP RECORD                       
         LA    RE,4(RE)            SET RECORD LENGTH IN OUT RECORD              
         SLL   RE,16                                                            
         L     R1,AOUTRECH                                                      
         ST    RE,0(R1)                                                         
*                                                                               
         BC    0,PT040             OPEN FIRST TIME                              
         OI    *-3,X'F0'                                                        
         L     R2,AOUT                                                          
         OPEN  ((R2),OUTPUT)                                                    
*                                                                               
PT040    L     R0,AOUTRECH         PUT THE RECORD                               
         L     R1,AOUT                                                          
         PUT   (R1),(R0)                                                        
*                                                                               
         L     RE,AOUTRECH                                                      
         XCEF  (RE),1004           CLEAR                                        
         B     EXIT                                                             
*                                                                               
PTMOVE   MVC   0(0,RF),0(R1)       * EXECUTED                                   
         EJECT                                                                  
*                                                                               
*        DATA MANAGER INTERFACE (DIRECTORY)                                     
*                                                                               
         SPACE                                                                  
GETCONT  MVC   KEY,RCONKEY                                                      
         B     HIGHDIR                                                          
*                                                                               
GETAGY   MVC   KEY,RAGYKEY                                                      
         B     HIGHDIR                                                          
*                                                                               
GETADV   MVC   KEY,RADVKEY                                                      
         B     HIGHDIR                                                          
*                                                                               
HIGHDIR  NTR1                                                                   
         MVC   KEYSAVE,KEY                                                      
         MVC   TRACEKEY,KEY                                                     
         MVC   DMCB(1),DMINBTS                                                  
         GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,KEY,KEY,0                             
         B     DMCHECK                                                          
         EJECT                                                                  
*                                                                               
*        DATA MANAGER INTERFACE (CHECK ERRORS)                                  
*                                                                               
         SPACE                                                                  
DMCHECK  TM    DMCB+8,X'10'        TEST FOR RECORD NOT FOUND                    
         BO    NEXIT                                                            
         TM    DMCB+8,X'EF'        TEST FOR OTHER ERRORS                        
         BZ    EQXIT                                                            
*                                                                               
         MVC   WORK(41),=C'*** DATA MANAGER ERROR IN INPUT PHASE ***'           
         GOTO1 LOGIO,WORK+48,1,(41,WORK)                                        
         MVC   WORK(41),SPACES                                                  
         BASR  RE,RF                                                            
         BAS   RE,DMTRACE                                                       
         DC    H'0'            BLOW UP                                          
         EJECT                                                                  
*                                                                               
*        ROUTINE TO TRACE DATA MANAGER CALLS                                    
*                                                                               
         SPACE                                                                  
DMTRACE  NTR1                                                                   
         MVC   P,SPACES                                                         
         LM    R2,R5,DMCB                                                       
         MVC   TRACEDM8,DMCB+8                                                  
         MVC   P(8),0(R2)          COMMAND                                      
         MVC   P+10(8),0(R3)       FILE                                         
         LA    R6,4                                                             
         CLC   3(3,R3),=C'DIR'                                                  
         BNE   DMTRACE2                                                         
         LA    R4,TRACEKEY                                                      
         GOTO1 HEXOUT,DMCB,(R4),P+20,32,=C'N'                                   
         MVI   SKIPSPEC,C'Y'                                                    
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,(R1),(R5)                                                 
         GOTO1 HEXOUT,DMCB,(R5),P+20,32,=C'N'                                   
         B     DMTRACE4                                                         
*                                                                               
DMTRACE2 GOTO1 HEXOUT,DMCB,(R4),P+20,(R6),=C'N'                                 
         LA    R5,34(R5)                                                        
         GOTO1 HEXOUT,DMCB,(R5),P+75,25                                         
*                                                                               
DMTRACE4 DS    0H                                                               
         GOTO1 HEXOUT,DMCB,TRACEDM8,P+130,1                                     
         MVI   SKIPSPEC,C'Y'                                                    
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         SPACE 2                                                                
TRACEDM8 DS    C                                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         DS    0D                                                               
         DC    CL8'*STORGE*'                                                    
REPCNT   DC    PL8'0'                                                           
OFFCNT   DC    PL8'0'                                                           
STACNT   DC    PL8'0'                                                           
ADVCNT   DC    PL8'0'                                                           
AGYCNT   DC    PL8'0'                                                           
SALCNT   DC    PL8'0'                                                           
CONCNT   DC    PL8'0'                                                           
REPADD   DC    PL8'0'                                                           
OFFADD   DC    PL8'0'                                                           
STAADD   DC    PL8'0'                                                           
ADVADD   DC    PL8'0'                                                           
AGYADD   DC    PL8'0'                                                           
SALADD   DC    PL8'0'                                                           
CONADD   DC    PL8'0'                                                           
PCONNUM  DS    PL8                                                              
*                                                                               
ADEMLIST DC    A(DEMLIST)                                                       
ADEMLIS2 DC    A(DEMLIST2)                                                      
ADPTLIST DC    A(DPTLIST)                                                       
ASALTABS DC    A(SALTABS)                                                       
ASALTABT DC    A(SALTABT)                                                       
ASALDDSS DC    A(SALDDSS)                                                       
ASALDDST DC    A(SALDDST)                                                       
ASOFLIST DC    A(SOFFLIST)                                                      
ATOFLIST DC    A(TOFFLIST)                                                      
ASOFFICE DC    A(SOFFICES)                                                      
ATOFFICE DC    A(TOFFICES)                                                      
AIN      DC    A(IN)                                                            
AOUT     DC    A(OUT)                                                           
AAGYFILE DC    A(AGYFILE)                                                       
AADVFILE DC    A(ADVFILE)                                                       
ASTAFILE DC    A(STAFILE)                                                       
AINBLOCK DC    A(INBLOCK)                                                       
AOUTREC  DC    A(OUTREC)                                                        
AOUTRECH DC    A(OUTRECH)                                                       
AAGYLIST DC    A(AGYLIST)                                                       
AAGYLSTX DC    A(AGYLISTX)                                                      
AADVLIST DC    A(ADVLIST)                                                       
AADVLSTX DC    A(ADVLISTX)                                                      
ASTALIST DC    A(STALIST)                                                       
ASTALSTX DC    A(STALISTX)                                                      
ASCAGY   DC    A(SCAGY)                                                         
ASCAGYX  DC    A(SCAGYX)                                                        
ASCADV   DC    A(SCADV)                                                         
ASCADVX  DC    A(SCADVX)                                                        
ASCSTA   DC    A(SCSTA)                                                         
ASCSTAX  DC    A(SCSTAX)                                                        
ACONDISC DS    A                                                                
CONTNUM  DS    F                                                                
HICONT   DC    F'0'                                                             
REGSAVE  DS    4F                                                               
*                                                                               
REPCODE  DS    CL2                                                              
INFCODE  DS    CL1                                                              
NELCODE  DS    CL1                                                              
CELCODE  DS    CL1                                                              
WEEKS    DS    XL1                                                              
RESDATE  DS    XL3                                                              
CREADATE DS    XL3                                                              
TRACEKEY DS    CL32                                                             
ELEM     DC    XL255'00'                                                        
EPLCMT   DS    560C                                                             
EPLCMTX  EQU   *                                                                
DASHES   DC    15C'-'                                                           
XFF      DC    8X'FF'                                                           
MKEY     DS    CL36                                                             
*                                                                               
         DS    0D                                                               
         DC    CL8'**INREC*'                                                    
INREC    DS    1000C                                                            
*                                                                               
         DC    CL8'*OUTREC*'                                                    
OUTRECH  DC    F'0'                                                             
OUTREC   DC    1000X'00'                                                        
*                                                                               
         DC    CL8'**DCBS**'                                                    
IN       DCB   DDNAME=TAPEIN,DSORG=PS,MACRF=(GM),EODAD=CV900,          X        
               RECFM=F,LRECL=4096                                               
*                                                                               
OUT      DCB   DDNAME=TAPEOUT,DSORG=PS,MACRF=(PM),                     X        
               RECFM=VB,LRECL=4004,BUFNO=2,BLKSIZE=32760                        
*                                                                               
AGYFILE  DCB   DDNAME=AGYFILE,DSORG=PS,MACRF=(GM,PM),                  X        
               RECFM=FB,LRECL=6,BLKSIZE=600,EODAD=CV002                         
*                                                                               
ADVFILE  DCB   DDNAME=ADVFILE,DSORG=PS,MACRF=(GM,PM),                  X        
               RECFM=FB,LRECL=4,BLKSIZE=400,EODAD=CV003                         
*                                                                               
STAFILE  DCB   DDNAME=STAFILE,DSORG=PS,MACRF=(GM,PM),                  X        
               RECFM=FB,LRECL=5,BLKSIZE=500,EODAD=CV003X                        
*                                                                               
         DS    0D                                                               
         DC    CL8'*INBLOCK'                                                    
INBLOCK  DS    4096X               INPUT BLOCK                                  
*                                                                               
         DC    CL8'*AGYLST*'                                                    
AGYLIST  DC    10000XL7'00'                                                     
AGYLISTX EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'*ADVLST*'                                                    
ADVLIST  DC    20000XL5'00'                                                     
ADVLISTX EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'*STALST*'                                                    
STALIST  DC    1000XL6'00'                                                      
STALISTX EQU   *                                                                
         EJECT                                                                  
*                                                                               
* DAYPART CONVERSION TABLE - SELCOM & TORBET                                    
*                                                                               
         SPACE                                                                  
DPTLIST  DC    CL4'AMFA'                                                        
         DC    CL4'PMFP'                                                        
         DC    CL4'MMFM'                                                        
         DC    CL4'EMFE'                                                        
         DC    CL4'DMFD'                                                        
         DC    CL4'GDRV'                                                        
         DC    CL4'HA&&M'                                                       
         DC    CL4'IM&&P'                                                       
         DC    CL4'JMPE'                                                        
         DC    CL4'KP&&E'                                                       
         DC    CL4'LAPE'                                                        
         DC    CL4'RROS'                                                        
         DC    CL4'SSPT'                                                        
         DC    CL4'NNEW'                                                        
         DC    CL4'XSPC'                                                        
         DC    CL4'WWKD'                                                        
         DC    CL4'FFRM'                                                        
         DC    CL4'CMAM'           TORBET ONLY                                  
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
* DEMOGRAPHIC CODE CONVERSION TABLE - FOR SELCOM & TORBET                       
*                                                                               
         SPACE                                                                  
DEMLIST  DC    AL1(001),CL7'AD12+  '                                            
         DC    AL1(002),CL7'MN12+  '                                            
         DC    AL1(003),CL7'WN12+  '                                            
         DC    AL1(004),CL7'TN12-17'                                            
         DC    AL1(005),CL7'MN12-17'                                            
         DC    AL1(006),CL7'FM12-17'                                            
         DC    AL1(007),CL7'AD12-24'                                            
         DC    AL1(008),CL7'MN12-24'                                            
         DC    AL1(009),CL7'WN12-24'                                            
         DC    AL1(010),CL7'AD12-34'                                            
         DC    AL1(011),CL7'MN12-24'                                            
         DC    AL1(012),CL7'WN12-24'                                            
         DC    AL1(013),CL7'AD18+  '                                            
         DC    AL1(014),CL7'MN18+  '                                            
         DC    AL1(015),CL7'WN18+  '                                            
         DC    AL1(016),CL7'AD18-24'                                            
         DC    AL1(017),CL7'MN18-24'                                            
         DC    AL1(018),CL7'WN18-24'                                            
         DC    AL1(019),CL7'AD18-34'                                            
         DC    AL1(020),CL7'MN18-34'                                            
         DC    AL1(021),CL7'WN18-34'                                            
         DC    AL1(022),CL7'AD18-44'                                            
         DC    AL1(023),CL7'MN18-44'                                            
         DC    AL1(024),CL7'WN18-44'                                            
         DC    AL1(025),CL7'AD18-49'                                            
         DC    AL1(026),CL7'MN18-49'                                            
         DC    AL1(027),CL7'WN18-49'                                            
         DC    AL1(028),CL7'AD18-54'                                            
         DC    AL1(029),CL7'MN18-54'                                            
         DC    AL1(030),CL7'WN18-54'                                            
         DC    AL1(031),CL7'AD18-64'                                            
         DC    AL1(032),CL7'MN18-64'                                            
         DC    AL1(033),CL7'WN18-64'                                            
         DC    AL1(034),CL7'AD25+  '                                            
         DC    AL1(035),CL7'MN25+  '                                            
         DC    AL1(036),CL7'WN25+  '                                            
         DC    AL1(037),CL7'AD25-34'                                            
         DC    AL1(038),CL7'MN25-34'                                            
         DC    AL1(039),CL7'WN25-34'                                            
         DC    AL1(040),CL7'AD25-44'                                            
         DC    AL1(041),CL7'MN25-44'                                            
         DC    AL1(042),CL7'WN25-44'                                            
         DC    AL1(043),CL7'AD25-49'                                            
         DC    AL1(044),CL7'MN25-49'                                            
         DC    AL1(045),CL7'WN25-49'                                            
         DC    AL1(046),CL7'AD25-54'                                            
         DC    AL1(047),CL7'MN25-54'                                            
         DC    AL1(048),CL7'WN25-54'                                            
         DC    AL1(049),CL7'AD25-64'                                            
         DC    AL1(050),CL7'MN25-64'                                            
         DC    AL1(051),CL7'WN25-64'                                            
         DC    AL1(052),CL7'AD35+  '                                            
         DC    AL1(053),CL7'MN35+  '                                            
         DC    AL1(054),CL7'WN35+  '                                            
         DC    AL1(055),CL7'AD35-44'                                            
         DC    AL1(056),CL7'MN35-44'                                            
         DC    AL1(057),CL7'WN35-44'                                            
         DC    AL1(058),CL7'AD35-54'                                            
         DC    AL1(059),CL7'MN35-54'                                            
         DC    AL1(060),CL7'WN35-54'                                            
         DC    AL1(061),CL7'AD35-64'                                            
         DC    AL1(062),CL7'MN35-64'                                            
         DC    AL1(063),CL7'WN35-64'                                            
         DC    AL1(064),CL7'AD50+  '                                            
         DC    AL1(065),CL7'MN50+  '                                            
         DC    AL1(066),CL7'WN50+  '                                            
         DC    AL1(067),CL7'AD50-54'                                            
         DC    AL1(068),CL7'MN50-54'                                            
         DC    AL1(069),CL7'WN50-54'                                            
         DC    AL1(070),CL7'AD50-64'                                            
         DC    AL1(071),CL7'MN50-64'                                            
         DC    AL1(072),CL7'WN50-64'                                            
         DC    AL1(073),CL7'AD65+  '                                            
         DC    AL1(074),CL7'MN65+  '                                            
         DC    AL1(075),CL7'WN65+  '                                            
*                                                                               
DEMLIST2 DC    AL1(226),CL7'FARM   ',CL5'TSA  '                                 
         DC    AL1(227),CL7'FARM   ',CL5'ADI  '                                 
         DC    AL1(228),CL7'FARM   ',CL5'METRO'                                 
         DC    AL1(229),CL7'BLACK  ',CL5'TSA  '                                 
         DC    AL1(230),CL7'BLACK  ',CL5'ADI  '                                 
         DC    AL1(231),CL7'BLACK  ',CL5'METRO'                                 
         DC    AL1(232),CL7'HSPANIC',CL5'TSA  '                                 
         DC    AL1(233),CL7'HSPANIC',CL5'ADI  '                                 
         DC    AL1(234),CL7'HSPANIC',CL5'METRO'                                 
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
* SALESPERSON CONVERSION TABLE - SELCOM                                         
*                                                                               
         SPACE                                                                  
SALTABS  DC    CL4'ASKE',CL3'KAH'  *** TS FILE ***                              
         DC    CL4'ASKH',CL3'KAH'                                               
         DC    CL4'BERJ',CL3'FIE'                                               
         DC    CL4'BINK',CL3'GAF'                                               
         DC    CL4'BUCA',CL3'EST'                                               
         DC    CL4'BURJ',CL3'DON'                                               
         DC    CL4'CHAB',CL3'CHM'                                               
         DC    CL4'CHBO',CL3'CHA'                                               
         DC    CL4'CINK',CL3'EST'                                               
         DC    CL4'CRAJ',CL3'CRA'                                               
         DC    CL4'DONB',CL3'DON'                                               
         DC    CL4'EARB',CL3'GAE'                                               
         DC    CL4'ESTB',CL3'EST'                                               
         DC    CL4'FARA',CL3'CHM'                                               
         DC    CL4'FERB',CL3'DON'                                               
         DC    CL4'FIEJ',CL3'FIE'                                               
         DC    CL4'FLOB',CL3'FLO'                                               
         DC    CL4'FORJ',CL3'FOE'                                               
         DC    CL4'FREB',CL3'DON'                                               
         DC    CL4'GOOA',CL3'DUN'                                               
         DC    CL4'GRAJ',CL3'EST'                                               
         DC    CL4'GUBI',CL3'VOG'                                               
         DC    CL4'HALB',CL3'GAE'                                               
         DC    CL4'HALM',CL3'GAE'                                               
         DC    CL4'HALS',CL3'HUT'                                               
         DC    CL4'HARD',CL3'SEV'                                               
         DC    CL4'HECK',CL3'DON'                                               
         DC    CL4'HILS',CL3'FOE'                                               
         DC    CL4'HUDD',CL3'VOG'                                               
         DC    CL4'HUTA',CL3'HUT'                                               
         DC    CL4'IWAC',CL3'IWA'                                               
         DC    CL4'JONT',CL3'GAO'                                               
         DC    CL4'KAHL',CL3'FIE'                                               
         DC    CL4'KEAD',CL3'LRB'                                               
         DC    CL4'KEBA',CL3'KAH'                                               
         DC    CL4'KEHW',CL3'DON'                                               
         DC    CL4'KIMM',CL3'CHM'                                               
         DC    CL4'LAPL',CL3'DON'                                               
         DC    CL4'LAWB',CL3'FOE'                                               
         DC    CL4'LEAM',CL3'MYE'                                               
         DC    CL4'LESB',CL3'CHM'                                               
         DC    CL4'LEUD',CL3'LRB'                                               
         DC    CL4'LONB',CL3'CHM'                                               
         DC    CL4'LURB',CL3'FIE'                                               
         DC    CL4'MAHL',CL3'SEV'                                               
         DC    CL4'MANA',CL3'SEV'                                               
         DC    CL4'MAYJ',CL3'KAH'                                               
         DC    CL4'MCGB',CL3'MCG'                                               
         DC    CL4'MCGJ',CL3'MAT'                                               
         DC    CL4'MIRB',CL3'MIR'                                               
         DC    CL4'MURJ',CL3'DON'                                               
         DC    CL4'PARC',CL3'GAF'                                               
         DC    CL4'PETB',CL3'PET'                                               
         DC    CL4'PETG',CL3'PET'                                               
         DC    CL4'PHEL',CL3'GAF'                                               
         DC    CL4'PRAB',CL3'DON'                                               
         DC    CL4'RABE',CL3'FIE'                                               
         DC    CL4'ROCR',CL3'IWA'                                               
         DC    CL4'RUNS',CL3'GAE'                                               
         DC    CL4'RUTD',CL3'GAO'                                               
         DC    CL4'SAUP',CL3'GAF'                                               
         DC    CL4'SCHJ',CL3'EST'                                               
         DC    CL4'SEGL',CL3'SEV'                                               
         DC    CL4'SMIG',CL3'GAE'                                               
         DC    CL4'STEF',CL3'FRI'                                               
         DC    CL4'SWEG',CL3'GAE'                                               
         DC    CL4'THLY',CL3'GAO'                                               
         DC    CL4'THOB',CL3'FOE'                                               
         DC    CL4'THOL',CL3'GAF'                                               
         DC    CL4'TSIC',CL3'SEV'                                               
         DC    CL4'WEBK',CL3'EST'                                               
         DC    CL4'WINP',CL3'DON'                                               
         DC    CL4'WOBR',CL3'MAT'                                               
         DC    CL4'WOCE',CL3'EST'                                               
         DC    CL4'URIA',CL3'EST'                                               
         DC    CL4'ZIER',CL3'EST'                                               
*                                                                               
         DC    CL4'BELB',CL3'BEL'  *** SC FILE ***                              
         DC    CL4'FERL',CL3'FER'                                               
         DC    CL4'FLOT',CL3'FLO'                                               
         DC    CL4'JACC',CL3'JAC'                                               
         DC    CL4'MCGR',CL3'MCG'                                               
         DC    CL4'MIRL',CL3'MIR'                                               
         DC    CL4'PETG',CL3'PET'                                               
         DC    CL4'DONA',CL3'DON'                                               
         DC    CL4'SERV',CL3'SEV'                                               
         DC    CL4'BONN',CL3'CHA'                                               
         DC    CL4'JONB',CL3'JON'                                               
         DC    CL4'LORB',CL3'LRB'                                               
         DC    CL4'FIEJ',CL3'FIE'                                               
         DC    CL4'PAGJ',CL3'PAG'                                               
         DC    CL4'WERS',CL3'WER'                                               
         DC    CL4'MCAL',CL3'MCD'                                               
         DC    CL4'FROR',CL3'FRO'                                               
         DC    CL4'CHAM',CL3'CHM'                                               
         DC    CL4'CRAJ',CL3'CRA'                                               
         DC    CL4'MATC',CL3'MAT'                                               
         DC    CL4'FORJ',CL3'FOE'                                               
         DC    CL4'MCQJ',CL3'MCQ'                                               
         DC    CL4'MYEL',CL3'MYE'                                               
         DC    CL4'KAZA',CL3'KAH'                                               
         DC    CL4'ESTB',CL3'EST'                                               
         DC    CL4'FRIS',CL3'FRI'                                               
         DC    CL4'HOLB',CL3'HOL'                                               
         DC    CL4'PAPD',CL3'PAP'                                               
         DC    CL4'IWAC',CL3'IWA'                                               
         DC    CL4'VOGS',CL3'VOG'                                               
         DC    CL4'ROBB',CL3'ROB'                                               
         DC    CL4'DUND',CL3'DUN'                                               
         DC    CL4'GAFM',CL3'GAF'                                               
         DC    CL4'GAFP',CL3'GAO'                                               
         DC    CL4'GAFS',CL3'GAE'                                               
         DC    CL4'HUTA',CL3'HUT'                                               
         DC    CL4'CARJ',CL3'JAC'                                               
         DC    CL4'DONS',CL3'DON'                                               
         DC    CL4'FIEN',CL3'IWA'                                               
         DC    CL4'FIRJ',CL3'IWA'                                               
         DC    CL4'ZZAL',CL3'SEV'                                               
         DC    CL4'ZZBL',CL3'SEV'                                               
         DC    CL4'ZZBR',CL3'LRB'                                               
         DC    CL4'ZZCR',CL3'FIE'                                               
         DC    CL4'ZZDR',CL3'FOE'                                               
         DC    CL4'ZZFR',CL3'GAF'                                               
         DC    CL4'ZZHR',CL3'KAH'                                               
         DC    CL4'KIMN',CL3'CHM'                                               
         DC    CL4'TIED',CL3'CHM'                                               
         DC    CL4'ZZJR',CL3'CHM'                                               
         DC    CL4'ZZLR',CL3'EST'                                               
         DC    CL4'ZZMR',CL3'IWA'                                               
         DC    CL4'JILL',CL3'DON'                                               
         DC    CL4'PATW',CL3'DON'                                               
         DC    CL4'ZZNW',CL3'DON'                                               
         DC    CL4'ZZPR',CL3'VOG'                                               
         DC    CL4'ZZPU',CL3'GAO'                                               
         DC    CL4'GEOP',CL3'PET'                                               
         DC    CL4'GOOL',CL3'DON'                                               
         DC    CL4'HUDN',CL3'DON'                                               
         DC    CL4'LAPN',CL3'DON'                                               
         DC    CL4'PETN',CL3'PET'                                               
         DC    CL4'WINN',CL3'DON'                                               
         DC    CL4'ZZSC',CL3'DON'                                               
         DC    CL4'TIER',CL3'HUT'                                               
         DC    CL4'ZZSR',CL3'HUT'                                               
         DC    CL4'SHRW',CL3'GAE'                                               
         DC    CL4'ZZWA',CL3'GAE'                                               
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
* SALESPERSON CONVERSION TABLE - TORBET                                         
*                                                                               
         SPACE                                                                  
SALTABT  DC    CL4'KEAD',CL3'KEA'                                               
         DC    CL4'LEUD',CL3'LEU'                                               
         DC    CL4'KAHL',CL3'KAH'                                               
         DC    CL4'KERD',CL3'KER'                                               
         DC    CL4'LURB',CL3'LUR'                                               
         DC    CL4'STEM',CL3'STE'                                               
         DC    CL4'AUSM',CL3'LUR'                                               
         DC    CL4'BATG',CL3'LUR'                                               
         DC    CL4'BERJ',CL3'LUR'                                               
         DC    CL4'CHOU',CL3'LUR'                                               
         DC    CL4'GEAJ',CL3'LUR'                                               
         DC    CL4'GHEH',CL3'LUR'                                               
         DC    CL4'LYKL',CL3'LUR'                                               
         DC    CL4'MCNC',CL3'LUR'                                               
         DC    CL4'OCOS',CL3'LUR'                                               
         DC    CL4'PACS',CL3'LUR'                                               
         DC    CL4'RABE',CL3'LUR'                                               
         DC    CL4'SABL',CL3'LUR'                                               
         DC    CL4'SMUB',CL3'LUR'                                               
         DC    CL4'ZZCH',CL3'LUR'                                               
         DC    CL4'MELC',CL3'MEL'                                               
         DC    CL4'MELP',CL3'MEL'                                               
         DC    CL4'THOB',CL3'HIL'                                               
         DC    CL4'PAZB',CL3'HIL'                                               
         DC    CL4'DHOU',CL3'HIL'                                               
         DC    CL4'ROSC',CL3'CRS'                                               
         DC    CL4'SLEJ',CL3'HIL'                                               
         DC    CL4'SNYC',CL3'HIL'                                               
         DC    CL4'VIDM',CL3'HIL'                                               
         DC    CL4'ZZDE',CL3'HIL'                                               
         DC    CL4'HILS',CL3'HIL'                                               
         DC    CL4'MCGJ',CL3'MCG'                                               
         DC    CL4'WOBR',CL3'WOR'                                               
         DC    CL4'BAIR',CL3'MCG'                                               
         DC    CL4'BECK',CL3'MCG'                                               
         DC    CL4'MATG',CL3'MAT'                                               
         DC    CL4'ZZDN',CL3'MCG'                                               
         DC    CL4'DNHO',CL3'MCG'                                               
         DC    CL4'HIXB',CL3'HIX'                                               
         DC    CL4'GEIK',CL3'GEI'                                               
         DC    CL4'GRAJ',CL3'GRA'                                               
         DC    CL4'PARL',CL3'PAR'                                               
         DC    CL4'ZIER',CL3'ZIE'                                               
         DC    CL4'WOCE',CL3'WOC'                                               
         DC    CL4'URIA',CL3'GRA'                                               
         DC    CL4'CINK',CL3'GRA'                                               
         DC    CL4'FANM',CL3'GRA'                                               
         DC    CL4'GOST',CL3'GRA'                                               
         DC    CL4'HUMD',CL3'GRA'                                               
         DC    CL4'LHOU',CL3'GRA'                                               
         DC    CL4'LOEB',CL3'GRA'                                               
         DC    CL4'MAGB',CL3'GRA'                                               
         DC    CL4'SCPE',CL3'GRA'                                               
         DC    CL4'WOOR',CL3'GRA'                                               
         DC    CL4'YARS',CL3'GRA'                                               
         DC    CL4'ZZLA',CL3'GRA'                                               
         DC    CL4'HARB',CL3'BIC'                                               
         DC    CL4'SAMH',CL3'HAL'                                               
         DC    CL4'ZZLO',CL3'HAL'                                               
         DC    CL4'LOHO',CL3'HAL'                                               
         DC    CL4'BEJA',CL3'ROS'                                               
         DC    CL4'BELL',CL3'BEL'                                               
         DC    CL4'BOBM',CL3'MAH'                                               
         DC    CL4'MAHB',CL3'MAH'                                               
         DC    CL4'BURJ',CL3'BUR'                                               
         DC    CL4'WINC',CL3'WIN'                                               
         DC    CL4'CHEM',CL3'WIN'                                               
         DC    CL4'ERNM',CL3'MET'                                               
         DC    CL4'METE',CL3'MET'                                               
         DC    CL4'METC',CL3'MET'                                               
         DC    CL4'HECK',CL3'HEC'                                               
         DC    CL4'HEKA',CL3'HEC'                                               
         DC    CL4'HUDD',CL3'VOG'                                               
         DC    CL4'MURJ',CL3'MUR'                                               
         DC    CL4'WEIT',CL3'WEI'                                               
         DC    CL4'TERM',CL3'WEI'                                               
         DC    CL4'MESJ',CL3'HAR'                                               
         DC    CL4'WINP',CL3'PAT'                                               
         DC    CL4'VINM',CL3'DEL'                                               
         DC    CL4'WEIL',CL3'HAR'                                               
         DC    CL4'NATM',CL3'HAR'                                               
         DC    CL4'ZNNY',CL3'KEH'                                               
         DC    CL4'POLD',CL3'KEH'                                               
         DC    CL4'MORI',CL3'KEH'                                               
         DC    CL4'LOVS',CL3'KEH'                                               
         DC    CL4'KEHW',CL3'KEH'                                               
         DC    CL4'HOUS',CL3'HAR'                                               
         DC    CL4'DWOS',CL3'KEH'                                               
         DC    CL4'ALAM',CL3'HAR'                                               
         DC    CL4'CIKA',CL3'CIN'                                               
         DC    CL4'DELV',CL3'DEL'                                               
         DC    CL4'HARA',CL3'HAR'                                               
         DC    CL4'HONY',CL3'HAR'                                               
         DC    CL4'JENM',CL3'HAR'                                               
         DC    CL4'AUGK',CL3'HAR'                                               
         DC    CL4'BARN',CL3'HAR'                                               
         DC    CL4'BELB',CL3'HAR'                                               
         DC    CL4'BRID',CL3'HAR'                                               
         DC    CL4'CONR',CL3'HAR'                                               
         DC    CL4'DWSU',CL3'HAR'                                               
         DC    CL4'GOLS',CL3'HAR'                                               
         DC    CL4'GUBJ',CL3'HAR'                                               
         DC    CL4'KEHB',CL3'HAR'                                               
         DC    CL4'KANN',CL3'CIN'                                               
         DC    CL4'LEOJ',CL3'HAR'                                               
         DC    CL4'LEVJ',CL3'HAR'                                               
         DC    CL4'MAHA',CL3'HAR'                                               
         DC    CL4'MESJ',CL3'HAR'                                               
         DC    CL4'MOOP',CL3'HAR'                                               
         DC    CL4'MORE',CL3'HAR'                                               
         DC    CL4'NATJ',CL3'HAR'                                               
         DC    CL4'YOUD',CL3'HAR'                                               
         DC    CL4'ZSNY',CL3'HAR'                                               
         DC    CL4'CORP',CL3'HAR'                                               
         DC    CL4'DOEJ',CL3'DOE'                                               
         DC    CL4'GUBI',CL3'GUB'                                               
         DC    CL4'GUBP',CL3'GUB'                                               
         DC    CL4'PHOU',CL3'GUB'                                               
         DC    CL4'ZZPH',CL3'GUB'                                               
         DC    CL4'BAKE',CL3'RUT'                                               
         DC    CL4'JONT',CL3'TED'                                               
         DC    CL4'POHO',CL3'RUT'                                               
         DC    CL4'RUTD',CL3'RUT'                                               
         DC    CL4'SAIJ',CL3'RUT'                                               
         DC    CL4'SALJ',CL3'RUT'                                               
         DC    CL4'STES',CL3'RUT'                                               
         DC    CL4'THLY',CL3'THR'                                               
         DC    CL4'ZZPO',CL3'RUT'                                               
         DC    CL4'BINK',CL3'BIN'                                               
         DC    CL4'BAIB',CL3'BIN'                                               
         DC    CL4'WORB',CL3'BIN'                                               
         DC    CL4'HALM',CL3'RUN'                                               
         DC    CL4'HALR',CL3'RHA'                                               
         DC    CL4'RUNS',CL3'RUN'                                               
         DC    CL4'SMIG',CL3'SMI'                                               
         DC    CL4'SWEG',CL3'SWE'                                               
         DC    CL4'PRIB',CL3'BRO'                                               
         DC    CL4'TRIP',CL3'DHA'                                               
         DC    CL4'ZZTA',CL3'DHA'                                               
         DC    CL4'TAHS',CL3'DHA'                                               
         DC    CL4'SUSL',CL3'DHA'                                               
         DC    CL4'PEGN',CL3'DHA'                                               
         DC    CL4'LOUM',CL3'DHA'                                               
         DC    CL4'KAYW',CL3'DHA'                                               
         DC    CL4'JPLA',CL3'DHA'                                               
         DC    CL4'DONH',CL3'DHA'                                               
         DC    CL4'ANNF',CL3'FAR'                                               
         DC    CL4'BETL',CL3'LES'                                               
         DC    CL4'BARK',CL3'LON'                                               
         DC    CL4'BARL',CL3'LON'                                               
         DC    CL4'BONB',CL3'LON'                                               
         DC    CL4'CARC',CL3'LON'                                               
         DC    CL4'DIRK',CL3'LON'                                               
         DC    CL4'JUDC',CL3'LON'                                               
         DC    CL4'MICD',CL3'LON'                                               
         DC    CL4'STEV',CL3'LON'                                               
         DC    CL4'TDHO',CL3'LON'                                               
         DC    CL4'ZZTD',CL3'LON'                                               
         DC    CL4'BONN',CL3'KEL'                                               
         DC    CL4'CARO',CL3'KEL'                                               
         DC    CL4'DONA',CL3'KEL'                                               
         DC    CL4'LESS',CL3'KEL'                                               
         DC    CL4'LONG',CL3'KEL'                                               
         DC    CL4'MCDY',CL3'KEL'                                               
         DC    CL4'KELY',CL3'KEL'                                               
         DC    CL4'BOBC',CL3'CHI'                                               
         DC    CL4'TMHO',CL3'CHI'                                               
         DC    CL4'ZZTM',CL3'CHI'                                               
         DC    CL4'BECP',CL3'RUN'                                               
         DC    CL4'EARB',CL3'BAK'                                               
         DC    CL4'MINL',CL3'RUN'                                               
         DC    CL4'MOER',CL3'RUN'                                               
         DC    CL4'SEHO',CL3'RUN'                                               
         DC    CL4'STAR',CL3'RUN'                                               
         DC    CL4'ZZSE',CL3'RUN'                                               
         DC    CL4'DONJ',CL3'DON'                                               
         DC    CL4'DEAH',CL3'HAD'                                               
         DC    CL4'THOL',CL3'HAD'                                               
         DC    CL4'KANC',CL3'HAD'                                               
         DC    CL4'HOLR',CL3'HAD'                                               
         DC    CL4'MARS',CL3'HAD'                                               
         DC    CL4'PARC',CL3'HAD'                                               
         DC    CL4'SCHP',CL3'HAD'                                               
         DC    CL4'SHOU',CL3'HAD'                                               
         DC    CL4'WRIJ',CL3'HAD'                                               
         DC    CL4'ZZSF',CL3'HAD'                                               
         DC    CL4'CHRT',CL3'TSI'                                               
         DC    CL4'TSIM',CL3'TSI'                                               
         DC    CL4'MARM',CL3'MAY'                                               
         DC    CL4'BHOU',CL3'LEU'                                               
         DC    CL4'ZZBO',CL3'LEU'                                               
         DC    CL4'SEHO',CL3'RUN'                                               
         DC    CL4'ZZSE',CL3'RUN'                                               
         DC    CL4'BRAG',CL3'GRA'                                               
         DC    CL4'CALP',CL3'CAL'                                               
         DC    CL4'HEIJ',CL3'HIL'                                               
         DC    CL4'LAWB',CL3'HIL'                                               
         DC    CL4'PETP',CL3'LUR'                                               
         DC    CL4'ROCR',CL3'ROS'                                               
         DC    CL4'ROGJ',CL3'ROS'                                               
         DC    CL4'TRAJ',CL3'GRA'                                               
         DC    CL4'JANP',CL3'POP'                                               
         DC    CL4'KALK',CL3'MCG'                                               
         DC    CL4'SULK',CL3'MCG'                                               
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
* DDS SALESPERSON CODE LIST - SELCOM                                            
*                                                                               
         SPACE                                                                  
SALDDSS  DC    C'BEL',C'NY',C'0'                                                
         DC    C'CHA',C'AT',C'0'                                                
         DC    C'CHM',C'DA',C'0'                                                
         DC    C'CRA',C'DA',C'0'                                                
         DC    C'DON',C'NY',C'0'                                                
         DC    C'DUN',C'SF',C'0'                                                
         DC    C'EST',C'LA',C'0'                                                
         DC    C'FER',C'NY',C'0'                                                
         DC    C'FIE',C'CH',C'0'                                                
         DC    C'FLO',C'NY',C'0'                                                
         DC    C'FOE',C'DE',C'0'                                                
         DC    C'FRI',C'LA',C'0'                                                
         DC    C'FRO',C'DA',C'0'                                                
         DC    C'GAE',C'SE',C'0'                                                
         DC    C'GAF',C'SF',C'0'                                                
         DC    C'GAO',C'PO',C'0'                                                
         DC    C'HOL',C'LA',C'0'                                                
         DC    C'HUT',C'SL',C'0'                                                
         DC    C'IWA',C'MN',C'0'                                                
         DC    C'JAC',C'NY',C'0'                                                
         DC    C'JON',C'AT',C'0'                                                
         DC    C'KAH',C'HO',C'0'                                                
         DC    C'LRB',C'BO',C'0'                                                
         DC    C'MAT',C'DN',C'0'                                                
         DC    C'MCD',C'CH',C'0'                                                
         DC    C'MCG',C'NY',C'0'                                                
         DC    C'MCQ',C'DE',C'0'                                                
         DC    C'MIR',C'NY',C'0'                                                
         DC    C'MYE',C'DE',C'0'                                                
         DC    C'PAG',C'CH',C'0'                                                
         DC    C'PAP',C'LA',C'0'                                                
         DC    C'PET',C'NY',C'0'                                                
         DC    C'ROB',C'SF',C'0'                                                
         DC    C'SEV',C'AT',C'0'                                                
         DC    C'VOG',C'PH',C'0'                                                
         DC    C'WER',C'CH',C'0'                                                
         DC    C'001',C'NY',C'0'   *** TEST                                     
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
* DDS SALESPERSON CODE LIST - TORBET                                            
*                                                                               
         SPACE                                                                  
SALDDST  DC    C'KEA',C'BO',C'0'                                                
         DC    C'LEU',C'BO',C'0'                                                
         DC    C'KAH',C'CH',C'0'                                                
         DC    C'KER',C'CH',C'0'                                                
         DC    C'LUR',C'CH',C'0'                                                
         DC    C'STE',C'CH',C'0'                                                
         DC    C'HEI',C'DE',C'0'                                                
         DC    C'MEL',C'DE',C'0'                                                
         DC    C'THO',C'DE',C'0'                                                
         DC    C'HIL',C'DE',C'0'                                                
         DC    C'CRS',C'DE',C'0'                                                
         DC    C'MCG',C'DN',C'0'                                                
         DC    C'MAT',C'DN',C'0'                                                
         DC    C'CAL',C'DN',C'0'                                                
         DC    C'HIX',C'DN',C'0'                                                
         DC    C'WOR',C'DN',C'0'                                                
         DC    C'GEI',C'LA',C'0'                                                
         DC    C'GRA',C'LA',C'0'                                                
         DC    C'PAR',C'LA',C'0'                                                
         DC    C'ZIE',C'LA',C'0'                                                
         DC    C'WOC',C'LA',C'0'                                                
         DC    C'BIC',C'SL',C'0'                                                
         DC    C'HAL',C'SL',C'0'                                                
         DC    C'ROS',C'MN',C'0'                                                
         DC    C'HAR',C'NY',C'0'                                                
         DC    C'PAT',C'NY',C'0'                                                
         DC    C'DOE',C'NY',C'0'                                                
         DC    C'BEL',C'NY',C'0'                                                
         DC    C'BUR',C'NY',C'0'                                                
         DC    C'KEH',C'NY',C'0'                                                
         DC    C'MAH',C'NY',C'0'                                                
         DC    C'WIN',C'NY',C'0'                                                
         DC    C'MET',C'NY',C'0'                                                
         DC    C'HEC',C'NY',C'0'                                                
         DC    C'MUR',C'NY',C'0'                                                
         DC    C'WEI',C'NY',C'0'                                                
         DC    C'CIN',C'NY',C'0'                                                
         DC    C'DEL',C'NY',C'0'                                                
         DC    C'GUB',C'PH',C'0'                                                
         DC    C'THR',C'PO',C'0'                                                
         DC    C'RUT',C'PO',C'0'                                                
         DC    C'TED',C'PO',C'0'                                                
         DC    C'BIN',C'UT',C'0'                                                
         DC    C'RHA',C'SE',C'0'                                                
         DC    C'RUN',C'SE',C'0'                                                
         DC    C'SMI',C'SE',C'0'                                                
         DC    C'BAK',C'SE',C'0'                                                
         DC    C'SWE',C'SE',C'0'                                                
         DC    C'BRO',C'AT',C'0'                                                
         DC    C'DHA',C'AT',C'0'                                                
         DC    C'FAR',C'DA',C'0'                                                
         DC    C'LES',C'DA',C'0'                                                
         DC    C'LON',C'DA',C'0'                                                
         DC    C'KEL',C'HO',C'0'                                                
         DC    C'DON',C'SF',C'0'                                                
         DC    C'HAD',C'SF',C'0'                                                
         DC    C'POP',C'SF',C'0'                                                
         DC    C'KCI',C'SF',C'0'                                                
         DC    C'TSI',C'AT',C'0'                                                
         DC    C'MAY',C'AT',C'0'                                                
         DC    C'CHI',C'MM',C'0'                                                
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
* OFFICE CODE CONVERSION LIST - SELCOM                                          
*                                                                               
         SPACE                                                                  
SOFFLIST DC    CL4'ATAT'           ** TS CODES **                               
         DC    CL4'NYNY'                                                        
         DC    CL4'CHCH'                                                        
         DC    CL4'HOHO'                                                        
         DC    CL4'DADA'                                                        
         DC    CL4'LALA'                                                        
         DC    CL4'PHPH'                                                        
         DC    CL4'DEDE'                                                        
         DC    CL4'BSBO'                                                        
         DC    CL4'BOBO'                                                        
         DC    CL4'DVDN'                                                        
         DC    CL4'POPO'                                                        
         DC    CL4'SESE'                                                        
         DC    CL4'SFSF'                                                        
         DC    CL4'SLSL'                                                        
         DC    CL4'MNMN'                                                        
         DC    CL4'SASF'                                                        
*                                                                               
         DC    CL4'SCNY'           ** SC CODES **                               
         DC    CL4'NWNY'                                                        
         DC    CL4'ALAT'                                                        
         DC    CL4'BRBO'                                                        
         DC    CL4'CRCH'                                                        
         DC    CL4'JRDA'                                                        
         DC    CL4'DRDE'                                                        
         DC    CL4'DNDN'                                                        
         DC    CL4'HRHO'                                                        
         DC    CL4'LRLA'                                                        
         DC    CL4'MRMN'                                                        
         DC    CL4'PRPH'                                                        
         DC    CL4'PUPO'                                                        
         DC    CL4'SRSL'                                                        
         DC    CL4'FRSF'                                                        
         DC    CL4'WASE'                                                        
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
* SELCOM OFFICES                                                                
*                                                                               
         SPACE                                                                  
SOFFICES DC    CL2'AT',C'0',CL20'ATLANTA'                                       
         DC    CL2'NY',C'0',CL20'NEW YORK'                                      
         DC    CL2'CH',C'0',CL20'CHICAGO'                                       
         DC    CL2'HO',C'0',CL20'HOUSTON'                                       
         DC    CL2'DA',C'0',CL20'DALLAS'                                        
         DC    CL2'LA',C'0',CL20'LOS ANGELES'                                   
         DC    CL2'PH',C'0',CL20'PHILADELPHIA'                                  
         DC    CL2'DE',C'0',CL20'DETROIT'                                       
         DC    CL2'BO',C'0',CL20'BOSTON'                                        
         DC    CL2'DN',C'0',CL20'DENVER'                                        
         DC    CL2'PO',C'0',CL20'PORTLAND'                                      
         DC    CL2'SE',C'0',CL20'SEATTLE'                                       
         DC    CL2'SF',C'0',CL20'SAN FRANCISCO'                                 
         DC    CL2'SL',C'0',CL20'ST LOUIS'                                      
         DC    CL2'MN',C'0',CL20'MINNEAPOLIS'                                   
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
* OFFICE CODE CONVERSION LIST - TORBET                                          
*                                                                               
         SPACE                                                                  
TOFFLIST DC    CL4'BOBO'                                                        
         DC    CL4'BSBO'                                                        
         DC    CL4'CHCH'                                                        
         DC    CL4'DEDE'                                                        
         DC    CL4'DNDN'                                                        
         DC    CL4'DVDN'                                                        
         DC    CL4'LALA'                                                        
         DC    CL4'LOSL'                                                        
         DC    CL4'MNMN'                                                        
         DC    CL4'NYNY'                                                        
         DC    CL4'PHPH'                                                        
         DC    CL4'POPO'                                                        
         DC    CL4'SAUT'                                                        
         DC    CL4'SESE'                                                        
         DC    CL4'SFSF'                                                        
         DC    CL4'STNY'                                                        
         DC    CL4'TAAT'                                                        
         DC    CL4'TDDA'                                                        
         DC    CL4'THHO'                                                        
         DC    CL4'TMMM'                                                        
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
* TORBET OFFICES                                                                
*                                                                               
         SPACE                                                                  
TOFFICES DC    CL2'BO',C'0',CL20'BOSTON'                                        
         DC    CL2'CH',C'0',CL20'CHICAGO'                                       
         DC    CL2'DE',C'0',CL20'DETROIT'                                       
         DC    CL2'DN',C'0',CL20'DENVER'                                        
         DC    CL2'LA',C'0',CL20'LOS ANGELES'                                   
         DC    CL2'SL',C'0',CL20'ST LOUIS'                                      
         DC    CL2'MN',C'0',CL20'MINNEAPOLIS'                                   
         DC    CL2'NY',C'0',CL20'NEW YORK'                                      
         DC    CL2'PH',C'0',CL20'PHILADELPHIA'                                  
         DC    CL2'PO',C'0',CL20'PORTLAND'                                      
         DC    CL2'UT',C'0',CL20'SALT LAKE CITY'                                
         DC    CL2'SE',C'0',CL20'SEATTLE'                                       
         DC    CL2'SF',C'0',CL20'SAN FRANCISCO'                                 
         DC    CL2'AT',C'0',CL20'ATLANTA'                                       
         DC    CL2'DA',C'0',CL20'DALLAS'                                        
         DC    CL2'HO',C'0',CL20'HOUSTON'                                       
         DC    CL2'MM',C'0',CL20'MEMPHIS'                                       
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
* LIST OF USED AGENCY CODES                                                     
*                                                                               
         SPACE                                                                  
SCAGY    DS    5000CL6                                                          
SCAGYX   EQU   *                                                                
         EJECT                                                                  
*                                                                               
* LIST OF USED ADVERTISER CODES                                                 
*                                                                               
         SPACE                                                                  
SCADV    DS    8000CL4                                                          
SCADVX   EQU   *                                                                
         EJECT                                                                  
*                                                                               
* LIST OF USED STATION CODES                                                    
*                                                                               
         SPACE                                                                  
SCSTA    DS    1000CL5                                                          
SCSTAX   EQU   *                                                                
         EJECT                                                                  
*                                                                               
* MPI RECORD DSECTS                                                             
*                                                                               
         SPACE                                                                  
MREPREC  DSECT                     REP RECORD                                   
         DS    CL34                                                             
MREPKREP DS    CL2                                                              
*                                                                               
MREPELM1 DS    X'01'                                                            
         DS    XL1                                                              
MREPNAME DS    CL34                                                             
MREPABBR DS    CL6                                                              
         EJECT                                                                  
MOFFREC  DSECT                     OFFICE RECORD                                
         DS    CL30                                                             
MOFFKREP DS    CL2                                                              
MOFFKOFF DS    CL2                 OFF CODE                                     
         DS    CL2                                                              
*                                                                               
MOFFELM1 DS    X'01'                                                            
         DS    XL1                                                              
MOFFNAME DS    CL20                                                             
MOFFADD1 DS    CL30                                                             
MOFFADD2 DS    CL30                                                             
         EJECT                                                                  
MSTAREC  DSECT                     STATION RECORD                               
         DS    CL26                                                             
MSTAKREP DS    CL2                 REP                                          
MSTAKSTA DS    CL4                 STATION                                      
MSTAKMED DS    CL2                 MEDIA                                        
MSTAKGRP DS    CL2                                                              
*                                                                               
MSTAELM1 DS    X'01'                                                            
         DS    CL1                                                              
MSTAMKT  DS    CL20                MARKET                                       
MSTACHAN DS    CL6                 CHANNEL                                      
MSTAAFFL DS    CL4                 AFFILIATE                                    
MSTAJOIN DS    CL2                 START DATE                                   
MSTALEFT DS    CL2                 END DATE                                     
         EJECT                                                                  
MSALREC  DSECT                     SALESPERSON RECORD                           
         DS    CL26                                                             
MSALKREP DS    CL2                 REP                                          
MSALKSAL DS    CL4                 SALESPERSON                                  
         DS    CL2                                                              
MSALKOFF DS    CL2                 OFFICE                                       
*                                                                               
MSALELM1 DS    X'01'                                                            
         DS    CL1                                                              
MSALNAME DS    CL30                NAME                                         
MSALNMBR DS    CL12                TELEPHONE NUMBER                             
         EJECT                                                                  
MAGYREC  DSECT                     AGENCY RECORD                                
         DS    CL28                                                             
MAGYKREP DS    CL2                 REP                                          
MAGYKAGY DS    CL4                 AGENCY                                       
MAGYKAOF DS    CL2                 OFFICE                                       
*                                                                               
MAGYELM1 DS    X'01'                                                            
         DS    CL1                                                              
MAGYNAM1 DS    CL20                SHORT NAME                                   
MAGYNAM2 DS    CL34                LONG NAME                                    
MAGYADD1 DS    CL34                ADDRESS                                      
MAGYADD2 DS    CL34                                                             
         EJECT                                                                  
MADVREC  DSECT                     ADVERTISER RECORD                            
         DS    CL30                                                             
MADVKREP DS    CL2                                                              
MADVKADV DS    CL4                 ADVERTISER                                   
*                                                                               
MADVELM1 DS    X'01'                                                            
         DS    CL1                                                              
MADVNAME DS    CL26                ADVERTISER NAME                              
         EJECT                                                                  
MCONREC  DSECT                     CONTRACT RECORD                              
*                                                                               
MCONKEY  DS    0CL36                                                            
         DS    CL2                                                              
MCONKREP DS    CL2                 REP                                          
         DS    CL2                                                              
MCONKSTA DS    CL4                 STATION                                      
MCONKMED DS    CL2                 MEDIA                                        
         DS    CL2                                                              
MCONKOFF DS    CL2                 OFFICE                                       
         DS    CL2                                                              
MCONKSAL DS    CL4                 SALESPERSON                                  
MCONKAGY DS    CL4                 AGENCY                                       
MCONKAOF DS    CL2                 AGENCY OFFICE                                
MCONKADV DS    CL4                 ADVERTISER                                   
MCONKCON DS    CL4                 CONTRACT NUMBER                              
         EJECT                                                                  
         PRINT ON                                                               
       ++INCLUDE REGENALL                                                       
       ++INCLUDE REREPWORKD                                                     
         EJECT                                                                  
       ++INCLUDE REREPMODES                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'122REREPCV02 05/01/02'                                      
         END                                                                    
