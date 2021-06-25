*          DATA SET ACREQ04    AT LEVEL 058 AS OF 02/17/21                      
*PHASE T60404B                                                                  
*INCLUDE PUBVAL                                                                 
         TITLE 'ACREQ04 - REQUEST - VALIDATE FIELDS - PART 2'                   
*--------------------------------------------------------------------*          
* PID  LVL DATE    COMMENTS                                                     
* ---------------------------------                                             
* GHOA 056 02AUG19 SPEC-30973 SUPPORT DOLLAR TOLERANCE                          
* GHOA 057 22OCT20 SPEC-51025 1099 NEW REQUIREMENTS FOR 2020                    
* GHOA 058 21JAN21 SPEC-53267 USER DUMPS REQUESTING T7                          
*--------------------------------------------------------------------*          
T60404   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LWSX-LWS,T60404,RA,RR=R5                                         
         USING LWS,RC              RC=A(LOCAL WORKING STORAGE)                  
         L     R9,0(R1)                                                         
         USING GWS,R9              R9=A(GLOBAL WORKINB STORAGE)                 
         LA    R8,RCARDS                                                        
         USING ACQD,R8             R8=A(REQUEST CARDS)                          
         L     R3,ASAVE                                                         
         USING TWAD,R3             R3=A(TWA)                                    
         ST    R5,RELO                                                          
*                                                                               
         L     R1,FLDHADR          R1=A(FLD HDR TO BE VALIDATED)                
         ST    R1,FADR                                                          
*                                                                               
         SR    RF,RF                                                            
         IC    RF,ROUTNUM          RF=ROUTINE NUM REQUIRED                      
         SLL   RF,2                                                             
         L     RE,=A(ROUTADRT)                                                  
         A     RE,RELO                                                          
         L     RF,0(RF,RE)                                                      
         A     RF,RELO             RF=A(VALIDATION ROUTINE)                     
         BASR  RE,RF                                                            
         B     EXIT                                                             
*                                                                               
SPARE    NTR1                                                                   
EXIT     XIT1                                                                   
*                                                                               
         EJECT                                                                  
***********************************************************************         
*              VALIDATE PAYCODE                                       *         
***********************************************************************         
*                                                                               
PAYCVAL  NTR1                                                                   
         GOTO1 AINITV                                                           
         BNE   EXIT                                                             
         CLI   IFLDH+5,5                                                        
         BH    PAYCE                                                            
         OC    IFLD,SPACES                                                      
         MVC   NAME,SPACES                                                      
*                                                                               
         USING PAYRECD,R6                                                       
         L     R6,AIO1                                                          
         MVC   PAYKEY,SPACES                                                    
         MVI   PAYKTYP,PAYKTYPQ    X'3E'                                        
         MVI   PAYKSUB,PAYKSUBQ    X'03'                                        
         MVC   PAYKCPY,ACQCPY                                                   
         MVI   PAYKSEQ,0                                                        
*                                                                               
PAYC100  L     R6,AIO1                                                          
         MVC   SVKEY,PAYKEY        UPDATE SAVED KEY                             
         GOTO1 DATAMGR,DMCB,DMREAD,ACCOUNT,(R6),(R6)                            
         TM    DMCB+8,X'10'        TEST RECORD NOT FOUND                        
         BO    PAYCE                                                            
         CLC   PAYKEY(PAYKSEQ-PAYKEY),SVKEY                                     
         BNE   PAYCE                                                            
         MVI   ELCODE,PAYELQ       X'84' ELEMENT                                
         BAS   RE,GETEL                                                         
         BNE   PAYC150                                                          
*                                                                               
         USING PAYELD,R6                                                        
PAYC125  CLC   PAYCODE,IFLD        PAYROLL CODE                                 
         BNE   *+20                                                             
         MVC   NAME(L'PAYDESC),PAYDESC                                          
         MVC   ACQSEL(L'PAYCODE),PAYCODE                                        
         B     PAYCX                                                            
         BAS   RE,NEXTEL                                                        
         BE    PAYC125                                                          
*                                                                               
         USING PAYRECD,R6                                                       
PAYC150  L     R6,AIO1             CHECK FOR LINKED RECORDS                     
         MVC   PAYKEY,SVKEY                                                     
         SR    R1,R1                                                            
         IC    R1,PAYKSEQ                                                       
         LA    R1,1(R1)                                                         
         STC   R1,PAYKSEQ                                                       
         B     PAYC100                                                          
*                                                                               
PAYCX    DS    0H                                                               
         OI    FIND,FIVAL          SET VALID ENTRY BIT                          
         BAS   RE,DISPNAME                                                      
         B     EXIT                                                             
*                                                                               
PAYCE    MVC   FERN,=AL2(NTONFILE)                                              
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              VALIDATE REPORT AND SET FIND FORMAT BITS               *         
***********************************************************************         
*                                                                               
REPRTVAL NTR1                                                                   
         GOTO1 AINITV                        SET R4=A(HDR) & R5=L'DATA          
         BNE   EXIT                                                             
         CLI   IFLDH+5,2                                                        
         BL    REPRTINV                                                         
         BH    REPRTVO                                                          
         MVC   TEMP+1(2),IFLD                                                   
         MVI   TEMP,C' '           INSERT LEADING SPACE                         
         MVC   IFLD(3),TEMP                                                     
*                                                                               
REPRTVO  MVC   ACQOPTS(3),IFLD                                                  
         OI    FIND,FIVAL                    REPRT VALID =XX                    
         B     REPRTVX                                                          
*                                                                               
REPRTINV MVC   FERN,=AL2(INVINPT)                                               
REPRTVX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              VALIDATE BILL NUMBER                                   *         
***********************************************************************         
*                                                                               
BNOVAL   NTR1                                                                   
         GOTO1 AINITV                                                           
         CLI   IFLDH+5,6           MUST BE 6 CHARACTERS                         
         BNE   BNOERR                                                           
         CLC   ACQPROG,=C'23'      FOR PROGRAMS 23, 24, 29, 30 & BX             
         BE    BNOV10                  SPECIAL CODE                             
         CLC   ACQPROG,=C'24'                                                   
         BE    BNOV10                                                           
         CLC   ACQPROG,=C'29'                                                   
         BE    BNOV10                                                           
         CLC   ACQPROG,=C'30'                                                   
         BE    BNOV10                                                           
         CLC   ACQPROG,=C'BX'                                                   
         BNE   BNOV50                                                           
BNOV10   LA    R0,2                                                             
         LA    RF,IFLD                                                          
         LA    R1,ACQSEL                                                        
BNOV20   CLI   0(RF),C'A'          1ST  2 CHARS MAY BE A-C                      
         BL    BNOERR                                                           
         CLI   0(RF),C'C'                                                       
         BNH   BNOV30                                                           
         CLI   0(RF),C'0'          OR   0-9                                     
         BL    BNOERR                                                           
         CLI   0(RF),C'9'                                                       
         BH    BNOERR                                                           
BNOV30   MVC   0(1,R1),0(RF)       SAVE THIS CHARACTER                          
         LA    R1,1(,R1)                                                        
         LA    RF,1(,RF)                                                        
         BCT   R0,BNOV20                                                        
         LA    R0,4                LAST 4 CHARS MUST BE NUMERIC                 
         LA    RF,IFLD+2                                                        
         LA    R1,ACQSEL+2                                                      
         B     BNOV60                                                           
*                                                                               
BNOV50   LA    R0,6                                                             
         LA    RF,IFLD                                                          
         LA    R1,ACQSEL                                                        
*                                                                               
BNOV60   CLI   0(RF),C'0'          CHECK FOR NUMERICS                           
         BL    BNOERR                                                           
         CLI   0(RF),C'9'                                                       
         BH    BNOERR                                                           
         MVC   0(1,R1),0(RF)                                                    
         LA    R1,1(,R1)                                                        
         LA    RF,1(,RF)                                                        
         BCT   R0,BNOV60                                                        
         OI    FIND,FIVAL                                                       
         B     BNOVX                                                            
*                                                                               
BNOERR   MVC   FERN,=AL2(INVINPT)                                               
BNOVX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              VALIDATE BILL NUMBER                                   *         
***********************************************************************         
*                                                                               
YRLYVAL  NTR1                                                                   
         GOTO1 AINITV                                                           
         BNE   EXIT                                                             
         CLI   IFLD,C'Y'                                                        
         BNE   YRLYV5                                                           
         CLI   IFLDH+5,1                                                        
         BNE   YRLYVE              INVALID                                      
         OI    FIND,FIALT          VALID Y                                      
         MVC   TEMP+2(2),IFLD                                                   
         B     YRLYVO                                                           
*                                                                               
YRLYV5   SR    R5,R5                                                            
         IC    R5,IFLDH+5                                                       
         L     R4,FADR                                                          
         GOTO1 ARJN,DMCB,(R5),(R4)                                              
         CLC   FERN,=AL2(FE)                                                    
         BL    YRLYVE                                                           
         CLC   TEMP+2(2),=C'01'                                                 
         BL    YRLYVE                                                           
         CLC   TEMP+2(2),=C'90'                                                 
         BH    YRLYVE                                                           
         OI    FIND,FIVAL          VALID 01-90                                  
         B     YRLYVO                                                           
*                                                                               
YRLYVE   MVC   FERN,=AL2(INVINPT)  INVALID INPUT                                
         B     YRLYVX                                                           
*                                                                               
YRLYVO   MVC   ACQOPTS+3(2),TEMP+2                                              
YRLYVX   B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              DATA TYPES                                             *         
***********************************************************************         
*                                                                               
DTYSVAL  NTR1                                                                   
         BRAS  RE,DTSVAL                                                        
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              BUDGET VALIDATION                                      *         
***********************************************************************         
*                                                                               
BDTVAL   NTR1                                                                   
         BAS   RE,BDTVLALL         VALIDATE BUDGET                              
         MVC   ACQSRTAR(1),BYTE2   RETURNS BUDGET NUMBER IN BYTE2               
         B     EXIT                                                             
*                                                                               
BDTVAL2  NTR1                      2ND BUDGET TYPE IN ACQSRTAR+1                
         BAS   RE,BDTVLALL         VALIDATE BUDGET                              
         MVC   ACQSRTAR+1(1),BYTE2 MOVE NEW ONE TO +1                           
         B     EXIT                                                             
*                                                                               
HBUDVAL  NTR1                      STD HOURS BUDGET                             
         BAS   RE,BDTVLALL         VALIDATE BUDGET                              
         MVC   ACQAPPL(1),BYTE2    RETURNS BUDGET NUMBER IN BYTE2               
         B     EXIT                                                             
*                                                                               
RBUDVAL  NTR1                      STD RATE BUDGET                              
         BAS   RE,BDTVLALL         VALIDATE BUDGET                              
         MVC   ACQAPPL+1(1),BYTE2  RETURNS BUDGET NUMBER IN BYTE2               
         B     EXIT                                                             
*                                                                               
TPERVAL  NTR1                      TARGET PERCENT BUDGET                        
         BAS   RE,BDTVLALL         VALIDATE BUDGET                              
         MVC   ACQAPPL+2(1),BYTE2  RETURNS BUDGET NUMBER IN BYTE2               
         B     EXIT                                                             
*                                                                               
BDTVLALL NTR1                                                                   
         MVI   BYTE2,0                                                          
         GOTO1 AINITV                                                           
         BNE   BDTVX                                                            
         CLI   IFLDH+5,10                                                       
         BH    BDTVE               INVALID                                      
*                                                                               
BDTV20   L     R6,AIO1                                                          
         XC    KEY,KEY             MUST READ HIGH FOR BUDGET                    
         MVI   KEY,X'1B'                                                        
         MVC   KEY+1(1),ACQCPY                                                  
         MVC   KEY+5(10),IFLD                                                   
         GOTO1 DATAMGR,DMCB,(0,DMRDHI),ACCOUNT,KEY,(R6)                         
         CLC   KEY(15),0(R6)                                                    
         BNE   BDTVNFD             BUDGET TYPE NOT FOUND                        
         MVC   BYTE2,16(R6)        STORE BINARY NUMBER FOR REQUEST              
*                                                                               
         CLC   ACQPROG,=C'B4'      ONLY FOR THE B4                              
         BNE   BDTV21                                                           
         MVI   ELCODE,BCNELQ       GET TYPE ELEMENT                             
         BAS   RE,GETEL                                                         
         BNE   BDTV21                                                           
         USING BCNELD,R6                                                        
         TM    BCNSTAT,X'80'       TEST OFFICE BUDGET                           
         BO    BDTVE               FOR NOW - CAN'T REQUEST                      
         DROP  R6                                                               
*                                                                               
BDTV21   L     R6,AIO1                                                          
         MVI   ELCODE,X'20'        GET NAME ELEMENT                             
         BAS   RE,GETEL                                                         
         BNE   BDTVO                                                            
         SR    R1,R1                                                            
         IC    R1,1(R6)            NAME ELEMENT                                 
         AHI   R1,-3                                                            
         BM    BDTVO                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   NAME(0),2(R6)                                                    
*                                                                               
BDTVO    OI    FIND,FIVAL          SET VALID ENTRY BIT                          
         B     BDTVX                                                            
*                                                                               
BDTVE    MVC   FERN,=AL2(INVINPT)  FLD INVALID                                  
         B     EXIT                                                             
*                                                                               
BDTVNFD  MVC   FERN,=AL2(NTONFILE) NOT FOUND                                    
*                                                                               
BDTVX    BAS   RE,DISPNAME                                                      
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              SELECT FIELD VALIDATION                                *         
***********************************************************************         
*                                                                               
RSELVAL  NTR1                      ALPHA NUMERIC VALIDATION                     
         GOTO1 AINITV              MOVES UPTO 6 CHARS TO ACQSEL                 
         BNE   RSELVX                                                           
*                                                                               
         CLC   ACQPROG,=C'FI'                                                   
         BE    *+14                                                             
         CLC   ACQPROG,=C'M2'                                                   
         BNE   RSELV2                                                           
         MVI   BYTE,X'00'                                                       
         LA    R0,6                ANY 6 CHARACTERS ALLOWED                     
         B     RSELV4                                                           
*                                                                               
RSELV2   CLC   ACQPROG,=C'TE'                                                   
         BNE   RSELV10                                                          
         CLI   IFLDH+5,3                                                        
         BH    RSELVE                                                           
         MVI   BYTE,C'0'                                                        
         LA    R0,3                ANY 3 DIGIT NUMERIC FIELD                    
RSELV4   LA    RF,IFLD                                                          
RSELV6   CLI   0(RF),C' '                                                       
         BE    RSELV8                                                           
         CLC   0(1,RF),BYTE                                                     
         BL    RSELVE                                                           
RSELV8   LA    RF,1(RF)                                                         
         BCT   R0,RSELV6                                                        
         B     RSELVO                                                           
*                                                                               
RSELV10  LA    R1,EXCPTLST         LIST OF ALLOWABLE REASONS                    
         CLC   ACQPROG,=C'67'                                                   
         BE    RSELV11                                                          
         CLC   ACQPROG,=C'59'                                                   
         BNE   RSELVO                                                           
*                                                                               
RSELV11  SR    R0,R0                                                            
         IC    R0,IFLDH+5                                                       
         LA    RF,IFLD                                                          
*                                                                               
RSELV12  SR    RE,RE                                                            
*                                                                               
RSELV13  CLI   0(R1),X'FF'         END OF LIST - NO MATCH                       
         BE    RSELVE                                                           
         CLC   0(1,R1),0(RF)       MATCH                                        
         BE    *+16                                                             
         LA    R1,1(R1)            NEXT ON LIST                                 
         LA    RE,1(RE)                                                         
         B     RSELV13                                                          
         LA    RF,1(RF)            NEXT INPUT FIELD                             
         SR    R1,RE               BEGINNING OF LIST                            
         BCT   R0,RSELV12                                                       
         B     RSELVO                                                           
*                                                                               
RSELVE   MVC   FERN,=AL2(INVINPT)                                               
         B     RSELVX                                                           
*                                                                               
RSELVO   OI    FIND,FIVAL          VALID ALF/NUM                                
         MVC   ACQSEL,IFLD                                                      
*                                                                               
RSELVX   B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              CONTRA VALIDATION                                      *         
***********************************************************************         
*                                                                               
CONTRVAL NTR1                                                                   
         GOTO1 AINITV                                                           
         BNE   EXIT                                                             
         CLI   IFLDH+5,6           LENGTH > 6 IS AN ERROR                       
         BH    CVALE                                                            
*                                                                               
         CLI   IFLD,C'+'           A LIST IS INVALID                            
         BE    CVALE                                                            
*                                                                               
         CLI   IFLD,C'*'                                                        
         BNE   CVALO2                                                           
         MVC   ACQSEL,IFLD+1       EXCLUDE                                      
         NI    ACQSEL,X'BF'        TURN OFF X'40' BIT                           
         OI    FIND,FIVAL                                                       
         B     EXIT                                                             
*                                                                               
CVALO2   MVC   ACQSEL,IFLD                                                      
         OI    FIND,FIVAL                                                       
         B     EXIT                                                             
*                                                                               
CVALE    MVC   FERN,=AL2(INVINPT)                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              NARRATIVE VALIDATION                                   *         
***********************************************************************         
*                                                                               
NARRVAL  NTR1                                                                   
         GOTO1 AINITV                                                           
         BNE   NARRX               MISSING                                      
         CLI   IFLDH+5,1                                                        
         BH    NARR2                                                            
         MVC   IFLD+1(1),IFLD      CHANGE X TO 0X                               
         MVI   IFLD,C'0'                                                        
*                                                                               
NARR2    LA    R1,IFLD                                                          
         LA    R0,2                                                             
NARR4    CLI   0(R1),C'0'          MUST BE 00-99                                
         BL    NARRE                                                            
         CLI   0(R1),C'9'                                                       
         BH    NARRE                                                            
         LA    R1,1(R1)                                                         
         BCT   R0,NARR4                                                         
         MVC   ACQSEL(2),IFLD                                                   
         OI    FIND,FIVAL                                                       
         B     NARRX                                                            
*                                                                               
NARRE    MVC   FERN,=AL2(FLDNNUM)                                               
NARRX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              BILLING DUE DAYS VALIDATION                            *         
***********************************************************************         
*                                                                               
BDAYVAL  NTR1                                                                   
         MVI   ACQBDAY,X'FF'       MUST INITIALIZE BINARY FIELD OR              
         GOTO1 AINITV                ENDAY WILL FILL WITH SPACES                
         BNE   BDAYX                                                            
*                                                                               
         TM    IFLDH+4,X'08'       MUST BE NUMERIC                              
         BNZ   *+14                                                             
         MVC   FERN,=AL2(FLDNNUM)                                               
         B     BDAYX                                                            
*                                                                               
         SR    RF,RF                                                            
         IC    RF,IFLDH+5                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,IFLDH+8(0)                                                   
         CVB   RF,DUB                                                           
         STC   RF,ACQBDAY          SAVE AS HEX VALUE                            
         OI    FIND,FIVAL          SET VALID BIT                                
*                                                                               
BDAYX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              MOS VALIDATION                                         *         
***********************************************************************         
         SPACE 1                                                                
MOSVAL   NTR1                                                                   
         BRAS  RE,MOSVALR                                                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              NUMBER VAL                                             *         
***********************************************************************         
*                                                                               
NUMVAL   NTR1                                                                   
         GOTO1 AINITV                                                           
         BNE   NUMX                                                             
*                                                                               
NUMV1    CLI   ACQCPY,X'F5'        BBDO                                         
         BE    NUMV5                                                            
         CLI   ACQCPY,X'F6'        WEC                                          
         BE    NUMV5                                                            
         CLI   ACQCPY,X'54'        CME                                          
         BNE   NUMV6                                                            
*                                                                               
NUMV5    CLI   IFLDH+5,5           INPUT LENGTH MUST BE 5 DIGITS                
         BE    *+14                                                             
         MVC   FERN,=AL2(FLD2SHRT)                                              
         B     NUMX                                                             
*                                                                               
NUMV6    TM    IFLDH+4,X'08'       IS FIELD NUMERIC                             
         BNZ   *+14                                                             
         MVC   FERN,=AL2(FLDNNUM)  NUMERIC DATA REQUIRED                        
         B     NUMX                                                             
*                                                                               
         MVC   ACQSEL(5),IFLD                                                   
         OI    FIND,FIVAL                                                       
*                                                                               
NUMX     B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              NUMERIC VALIDATION                                     *         
***********************************************************************         
*                                                                               
MINMVAL  NTR1                                                                   
         GOTO1 AINITV                                                           
         BNE   MINMX                                                            
*                                                                               
         TM    IFLDH+4,X'08'                                                    
         BNZ   *+14                                                             
         MVC   FERN,=AL2(FLDNNUM)  NUMERIC DATA REQUIRED                        
         B     MINMX                                                            
*                                                                               
         SR    R5,R5                                                            
         IC    R5,IFLDH+5                                                       
         GOTO1 CASHVAL,DMCB,(0,IFLD),(R5)                                       
         CLI   DMCB,X'FF'                                                       
         BNE   *+14                                                             
         MVC   FERN,=AL2(INVAMNT)  INVALID AMOUNT                               
         B     MINMX                                                            
*                                                                               
         L     R1,DMCB+4           MUST BE GREATER THAN ZERO                    
         CH    R1,=H'0'                                                         
         BH    *+14                                                             
         MVC   FERN,=AL2(INVAMNT)  INVALID AMOUNT                               
         B     MINMX                                                            
*                                                                               
         MVC   ACQSRTAR(3),IFLD    MOVE IN 3 BYTES                              
         OI    FIND,FIVAL          VALID AMOUNT                                 
*                                                                               
MINMX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              CONVERSION VALIDATION                                  *         
***********************************************************************         
*                                                                               
CONVAL   NTR1                                                                   
         GOTO1 AINITV              04=VALID CONVERSION (FORMAT 9.99)            
         BNE   CONVX               MISSING                                      
         LA    R1,TEMP                                                          
         CLI   IFLD,C'.'           IF FIRST CHARACTER IS A PERIOD               
         BNE   CONV2                                                            
         CLI   IFLDH+5,3           THEN L'INPUT MUST BE 3                       
         BNE   CONVE                                                            
         MVI   0(R1),C'0'          INSERT LEADING ZERO                          
         LA    R1,1(R1)                                                         
*                                                                               
CONV2    MVC   0(4,R1),IFLD        MOVE IN INPUT                                
         CLI   TEMP+1,C'.'         CHECK FOR DECIMAL POINT                      
         BNE   CONVE                                                            
         MVC   ACQBILGP(1),TEMP    MOVE DIGITS INTO BILL GROUP                  
         MVC   ACQBILGP+1(2),TEMP+2                                             
         MVC   TEMP(3),=C'000'     INSURE NUMERIC DATA                          
         MVZ   TEMP(3),ACQBILGP                                                 
         CLC   TEMP(3),=C'000'                                                  
         BE    CONVO                                                            
*                                                                               
CONVE    MVC   FERN,=AL2(INVINPT)  INVALID INPUT FIELD                          
         B     CONVX                                                            
*                                                                               
CONVO    OI    FIND,FIVAL                                                       
CONVX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              COKE EXPENDITURE FILTERS (ACCX)                        *         
***********************************************************************         
*                                                                               
AGYFVAL  NTR1                                                                   
         GOTO1 AINITV              04=3 BYTES VALID NUMERIC                     
         BNE   AGYFVX                                                           
         TM    IFLDH+4,X'08' IS FIELD NUMERIC                                   
         BZ    AGYFVE1                                                          
         CLI   IFLDH+5,3           MUST BE 3 DIGITS                             
         BNE   AGYFVE2                                                          
         MVC   ACQSRTAR(3),IFLD                                                 
         OI    FIND,FIVAL                                                       
         B     AGYFVX                                                           
*                                                                               
AGYFVE1  MVC   FERN,=AL2(FLDNNUM)  NUMERIC INPUT REQUIRED                       
         B     AGYFVX                                                           
*                                                                               
AGYFVE2  MVC   FERN,=AL2(FLD2SHRT) I/P LENGTH TOO SHORT                         
*                                                                               
AGYFVX   B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              FILTER VALIDATION                                      *         
***********************************************************************         
*                                                                               
BUDFVAL  NTR1                                                                   
         LA    R5,ACQSRTAR+5       BUDGET FILTER                                
         B     MEDFVAL2                                                         
*                                                                               
MEDFVAL  NTR1                                                                   
         LA    R5,ACQSRTAR+3       MEDIA FILTER                                 
*                                                                               
MEDFVAL2 GOTO1 AINITV              04=2 BYTES                                   
         BNE   MEDX                MISSING                                      
         CLI   IFLDH+5,2           MUST BE 2 CHARS                              
         BE    *+14                                                             
         MVC   FERN,=AL2(FLD2SHRT) I/P LENGTH TOO SHORT                         
         B     MEDX                                                             
         MVC   0(2,R5),IFLD                                                     
         OI    FIND,FIVAL                                                       
*                                                                               
MEDX     B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              WORK-CODE FILTER                                       *         
***********************************************************************         
*                                                                               
         USING WCOELD,R6                                                        
WKCDVAL  NTR1                      04=XX OR *XX                                 
         GOTO1 AINITV                                                           
         BL    WKCDVO              MISSING                                      
         CLI   IFLDH+5,2                                                        
         BH    WKCDV1                                                           
         MVC   HALF,IFLD                                                        
         B     WKCDV2                                                           
*                                                                               
WKCDV1   CLI   IFLD,C'*'           NEGATIVE FILTER                              
         BNE   WKCDVE                                                           
         MVC   HALF,IFLD+1                                                      
*                                                                               
WKCDV2   MVC   KEY,SPACES          BUILD KEY FOR WORK-CODE RECORD               
         MVI   KEY,X'0A'                                                        
         MVC   KEY+1(3),ACQCPY                                                  
         MVC   KEY+4(2),HALF                                                    
         GOTO1 AIOREAD                                                          
         BNE   WKCDVE              WORK-CODE NOT FOUND                          
         L     R6,AIO1                                                          
         MVI   ELCODE,X'12'        GET ANALYSIS ELEMENT                         
         BAS   RE,GETEL                                                         
         BNE   WKCDVE                                                           
         MVC   ACQTRNF,HALF        WORK-CODE TO REQUEST                         
         CLI   IFLD,C'*'                                                        
         BNE   *+8                                                              
         NI    ACQTRNF,X'BF'       TURN OFF X'40' BIT IF EXCLUDE                
         OI    FIND,FIVAL                                                       
         B     WKCDVO                                                           
*                                                                               
WKCDVE   MVC   FERN,=AL2(INVWKCD)  WORK-CODE INVALID                            
WKCDVO   B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              UNION                                                  *         
***********************************************************************         
*                                                                               
UNCVAL   NTR1                      04=XXX                                       
         LA    R2,1                FOR CODE OUTPUT R2 = 1                       
         B     UNIV1                                                            
*                                                                               
UNIVAL   NTR1                      04=XXX                                       
         LA    R2,0                FOR EQUATE OUTPUT R2 = 0                     
*                                                                               
UNIV1    GOTO1 AINITV                                                           
         BL    UNIVO               MISSING                                      
         TM    FIND,FIALL                                                       
         BZ    UNIV1E                                                           
         OR    R2,R2                                                            
         BNZ   *+8                 SELECT STAYS BLANK                           
         MVI   ACQSEL+5,0          'ALL' GETS BINARY ZERO                       
         B     UNIVX                                                            
*                                                                               
UNIV1E   L     R1,=A(UNITAB)       VALIDATE UNION CODE                          
         A     R1,RELO                                                          
UNIV2    CLI   0(R1),X'FF'                                                      
         BE    UNIVE               INPUT INVALID                                
         CLC   IFLD(3),0(R1)                                                    
         BE    *+12                                                             
         LA    R1,L'UNITAB(R1)                                                  
         B     UNIV2                                                            
         OR    R2,R2                                                            
         BZ    *+14                B IF EQUATE FORMAT                           
         MVC   ACQSEL(3),0(R1)     FOR CODE FORMAT MOVE INTO SELECT             
         B     UNIVX                                                            
         MVC   ACQSEL+5(1),3(R1)   AND MOVE EQUATE TO REQUEST                   
*                                                                               
UNIVX    OI    FIND,FIVAL                                                       
         B     UNIVO                                                            
*                                                                               
UNIVE    MVC   FERN,=AL2(INVINPT)  INVALID INPUT FIELD                          
UNIVO    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              GET GROUP CODE FROM SCREEN                             *         
*              VALIDATION DONE IN POST VALIDATION (02)                *         
***********************************************************************         
         SPACE 1                                                                
GETGRP   NTR1                      LEDGER                                       
         GOTO1 AINITV                                                           
         BNE   GETGX               IF NOT ENTERED - EXIT                        
*                                                                               
         CLC   ACQPROG,=C'WI'      WI PUTS GROUP CODE AT QAPPL+0 (7)            
         BE    GETG10                                                           
         CLC   ACQPROG,=C'FR'      FR PUTS GROUP CODE AT QAPPL+6 (6)            
         BNE   GETGX                                                            
         MVC   ACQAPPL+6(6),SPACES                                              
         MVC   ACQAPPL+6(6),IFLD   PUT GROUP CODE AT QAPPL+6                    
         MVI   FIND,FIVAL          SET FIELD AS FOUND                           
         B     GETGX                                                            
*                                                                               
GETG10   MVC   ACQAPPL,SPACES                                                   
         MVC   ACQAPPL(7),IFLD     PUT GROUP CODE AT START QAPPL                
         MVI   FIND,FIVAL          SET FIELD AS FOUND                           
*                                                                               
GETGX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              DEPARTMENT VALIDATION                                  *         
***********************************************************************         
*                                                                               
DEPTVAL  NTR1                                                                   
         GOTO1 AINITV                                                           
         BNE   DEPTX                                                            
         OC    IFLD,SPACES                                                      
*                                                                               
         SR    R1,R1                                                            
         IC    R1,SV1RLEV2         VALIDATE LENGTH OF INPUT                     
         SR    R0,R0                                                            
         IC    R0,SV1RLEV1                                                      
         SR    R1,R0                                                            
         STC   R1,BYTE                                                          
         CLC   IFLDH+5(1),BYTE                                                  
         BH    DEPTE                                                            
*                                                                               
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),ACQCPY                                                    
         MVC   KEY+1(2),=C'1R'     READ FOR OFFICE/DEPT                         
         MVC   KEY+3(2),ACQAPPL    OFFICE IS IN ACQAPPL(2)                      
         LA    RF,KEY+3                                                         
         IC    R0,SV1RLEV1                                                      
         AR    RF,R0                                                            
         IC    R1,SV1RLEV2                                                      
         SR    R1,R0                                                            
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),IFLD        MOVE DEPT INTO KEY                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACQSEL(0),IFLD      DEPARTMENT                                   
         GOTO1 AIOREAD                                                          
         BNE   DEPTE                                                            
         OC    ACQSEL(3),SPACES                                                 
         OI    FIND,FIVAL                                                       
         B     DEPTX                                                            
*                                                                               
DEPTE    MVC   FERN,=AL2(INVINPT)  INVALID INPUT FIELD                          
*                                                                               
DEPTX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              TASK VALIDATION                                        *         
***********************************************************************         
*                                                                               
TASKVAL  NTR1                                                                   
         GOTO1 AINITV                                                           
         BNE   TASKX                                                            
         OI    FIND,FIVAL          VALID                                        
         OC    IFLD,SPACES                                                      
         MVC   ACQAPPL+2(2),IFLD   TASK CODE INTO ACQAPPL+2(2)                  
*                                                                               
TASKX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              MEDIA GROUP                                            *         
***********************************************************************         
*                                                                               
MEDGVAL  NTR1                      04=X OR *X                                   
         GOTO1 AINITV                                                           
         BL    MEDGVO              MISSING                                      
         CLI   IFLDH+5,1                                                        
         BH    MEDGV1                                                           
         MVC   BYTE,IFLD                                                        
         B     MEDGV2                                                           
*                                                                               
MEDGV1   CLI   IFLD,C'*'           NEGATIVE FILTER                              
         BNE   MEDGVI                                                           
         MVC   BYTE,IFLD+1                                                      
*                                                                               
         USING MGRRECD,R6                                                       
MEDGV2   LA    R6,KEY              BUILD KEY FOR MEDIA GROUP RECORD             
         XC    KEY,KEY                                                          
         MVI   MGRKTYP,MGRKTYPQ    RECORD TYPE X'2C'                            
         MVI   MGRKSUB,MGRKSUBQ   SUB RECORD X'06'                              
         MVC   MGRKCPY(3),ACQCPY                                                
         CLC   ACQLDG,SPACES       IF NO LEDGER                                 
         BH    *+10                                                             
         MVC   MGRKUNT(2),LREQJOB SET U/L TO SJ                                 
         MVC   MGRKCODE,BYTE                                                    
         GOTO1 AIOREAD                                                          
         BNE   MEDGVE              MEDIA GROUP NOT FOUND                        
         CLC   ACQPROG,=C'66'      DONT DISPLAY NAME ON AC66 SCRN               
         BE    MED100                                                           
         CLC   ACQPROG,=C'67'      DONT DISPLAY NAME ON AC67 SCRN               
         BE    MED100                                                           
         CLC   ACQPROG,=C'21'      DONT DISPLAY NAME ON AC21 SCRN               
         BE    MED100                                                           
         CLC   ACQPROG,=C'22'      DONT DISPLAY NAME ON AC22 SCRN               
         BE    MED100                                                           
         CLC   ACQPROG,=C'27'      DONT DISPLAY NAME ON AC27 SCRN               
         BE    MED100                                                           
         CLC   ACQPROG,=C'28'      DONT DISPLAY NAME ON AC28 SCRN               
         BE    MED100                                                           
         BAS   RE,DISPNAME         DISPLAY NAME                                 
*                                                                               
MED100   MVC   ACQMEDGP,BYTE       MEDIA GROUP TO REQUEST                       
         CLI   IFLD,C'*'                                                        
         BNE   *+8                                                              
         NI    ACQMEDGP,X'BF'      TURN OFF X'40' BIT IF EXCLUDE                
         OI    FIND,FIVAL                                                       
         B     MEDGVO                                                           
*                                                                               
MEDGVE   MVC   FERN,=AL2(NTONFILE) RECORD NOT FOUND                             
         B     EXIT                                                             
*                                                                               
MEDGVI   MVC   FERN,=AL2(INVINPT)  INVALID INPUT                                
*                                                                               
MEDGVO   B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              ATTRIBUTE VALIDATION                                   *         
***********************************************************************         
*                                                                               
ATTRVAL  NTR1                                                                   
         GOTO1 AINITV              04=VALID ATTRIBUTES                          
         BNE   ATTRVX              MISSING                                      
         CLI   IFLDH+5,2                                                        
         BL    ATTRVE              ERROR IF LENGTH < 2                          
         BE    *+12                                                             
         CLI   IFLD+1,C' '         CHECK THAT THERE ARE NO SPACES               
         BE    ATTRVE                                                           
         CLI   IFLD,C' '                                                        
         BE    ATTRVE                                                           
         SR    R0,R0                                                            
         IC    R0,IFLDH+5          R0=NUMBER OF ATTRIBUTES                      
         LA    R2,IFLD             R2=A(CURRENT ATTRIBUTE INPUT)                
*                                                                               
ATTRV3   LA    R1,ATTRTAB          R1=A(ENTRY IN ATTRTAB)                       
*                                                                               
ATTRV5   CLI   0(R1),X'FF'                                                      
         BE    ATTRVE              ERROR IF NOT FOUND IN ATTRTAB                
         CLC   0(1,R2),0(R1)       IF NO MATCH                                  
         BE    *+12                                                             
         LA    R1,1(R1)            GET NEXT ATTRTAB ENTRY                       
         B     ATTRV5              KEEP LOOKING                                 
         LA    R2,1(R2)            ELSE GET NEXT ATTRIBUTE INPUT                
         BCT   R0,ATTRV3           LOOP FOR NUM OF ATTRIBUTES                   
*                                                                               
         CLC   IFLD(1),IFLD+1      CHECK THAT THERE ARE NO DUPLICATES           
         BE    ATTRVE                                                           
         CLI   IFLDH+5,2                                                        
         BE    ATTRVO                                                           
         CLC   IFLD(1),IFLD+2                                                   
         BE    ATTRVE                                                           
         CLC   IFLD+1(1),IFLD+2                                                 
         BNE   ATTRVO                                                           
*                                                                               
ATTRVE   MVC   FERN,=AL2(INVINPT)  INVALID INPUT                                
         B     ATTRVX                                                           
*                                                                               
ATTRVO   OI    FIND,FIVAL          VALID INPUT                                  
         MVC   ACQOPT4(3),IFLD     MOVE TO OPT4, 5, & 6                         
*                                                                               
ATTRVX   B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              ESTIMATE NUMBER                                        *         
***********************************************************************         
*                                                                               
ESTVAL   NTR1                      04=XXX OR *XXX                               
         GOTO1 AINITV                                                           
         BL    ESTVO               MISSING                                      
         SR    R1,R1                                                            
         IC    R1,IFLDH+5          R1=LENGTH OF NUMERIC INPUT                   
         CLI   IFLDH+5,3                                                        
         BH    ESTV1                                                            
         CLI   IFLD,C'*'                                                        
         BE    ESTV1A                                                           
         MVC   FULL(3),IFLD                                                     
         B     ESTV2                                                            
*                                                                               
ESTV1    CLI   IFLD,C'*'           IF LENGTH > 3                                
         BNE   ESTVI               MUST HAVE NEGATIVE FILTER                    
*                                                                               
ESTV1A   MVC   FULL(3),IFLD+1                                                   
         AHI   R1,-1               SUBTRACT FOR *                               
*                                                                               
ESTV2    DS    0H                  CHECK INPUT HAS ONLY NUMBERS                 
         LA    RF,FULL                                                          
         LR    R0,R1                                                            
ESTV5    CLI   0(RF),C'0'                                                       
         BL    ESTVI                                                            
         CLI   0(RF),C'9'                                                       
         BH    ESTVI                                                            
         LA    RF,1(RF)                                                         
         BCT   R0,ESTV5                                                         
*                                                                               
         AHI   R1,-1               PAD WITH HIGH ORDER ZEROS                    
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  HALF,FULL(0)                                                     
         UNPK  FULL(3),HALF                                                     
         MVC   ACQBILGP,FULL       ESTIMATE NUMBER TO REQUEST                   
         CLI   IFLD,C'*'                                                        
         BNE   *+8                                                              
         NI    ACQBILGP,X'BF'      TURN OFF X'40' BIT IF EXCLUDE                
         OI    FIND,FIVAL                                                       
         B     ESTVO                                                            
*                                                                               
ESTVI    MVC   FERN,=AL2(INVINPT)  INVALID INPUT                                
ESTVO    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              USER VALIDATION                                        *         
***********************************************************************         
*                                                                               
USRVAL   NTR1                      USER FIELD                                   
         GOTO1 AINITV                                                           
         BNE   USRVO               ALL OR MISSING                               
         CLI   IFLDH+5,2           LENGTH MUST BE 2                             
         BNE   USRVE                                                            
         MVC   ACQAPPL(2),IFLD                                                  
         OI    FIND,FIVAL          X'04'=XX                                     
         B     USRVO                                                            
*                                                                               
USRVE    MVC   FERN,=AL2(INVINPT)  INVALID INPUT                                
USRVO    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              GENERIC MOVE ROUTINE                                   *         
***********************************************************************         
*                                                                               
GENERIC  NTR1                      GENERIC MOVE                                 
         GOTO1 AINITV                                                           
         BNE   GENERX              ALL OR MISSING                               
         CLC   ACQPROG,=C'AA'      FOR THE AA PUT MEDIA IN QAPPL+8              
         BE    GENER50                                                          
         CLC   ACQPROG,=C'AC'                                                   
         BNE   GENERX                                                           
*                                                                               
GENER50  LA    RE,ACQAPPL+8                                                     
         LA    R1,2                                                             
*                                                                               
GENER99  AHI   R1,-1               PREPARE FOR EX INSTRUCTION                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),IFLD                                                     
         OI    FIND,FIVAL          X'04'=XX                                     
*                                                                               
GENERX   B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              JOB GROUP VALIDATION                                   *         
***********************************************************************         
*                                                                               
JGRPVAL  NTR1                      JOB GROUP - 04=XXX OR *XXX                   
         GOTO1 AINITV                                                           
         BNE   JGRPVO              JGRP = ALL OR MISSING                        
         CLI   IFLD,C'*'                                                        
         BE    JGRPV1              EXCLUDE JGRP                                 
         CLI   IFLDH+5,3                                                        
         BH    JGRPVE                                                           
         SR    R1,R1                                                            
         IC    R1,IFLDH+5                                                       
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACQAPPL(0),IFLD                                                  
         B     JGRPVO                                                           
*                                                                               
JGRPV1   CLI   IFLDH+5,1                                                        
         BNH   JGRPVE                                                           
         SR    R1,R1                                                            
         IC    R1,IFLDH+5                                                       
         AHI   R1,-2               DON'T MOVE *                                 
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACQAPPL(0),IFLD+1                                                
         NI    ACQAPPL,X'BF'       TURN OFF X'40' BIT                           
         B     JGRPVO                                                           
*                                                                               
JGRPVE   MVC   FERN,=AL2(INVINPT)  JGRP INVALID                                 
         B     JGRPVO2                                                          
*                                                                               
JGRPVO   OI    FIND,FIVAL          JGRP = X THRU XXX                            
JGRPVO2  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              WORKCODE TYPE VALIDATION                               *         
***********************************************************************         
*                                                                               
WKTYVAL  NTR1                      WORKCODE TYPE IN ACQAPPL+3(1)                
         GOTO1 AINITV                                                           
         BNE   WKTYVO              ALL OR MISSING                               
         LA    R1,WKTYPES          R1=A(VALID WORKCODE TYPES)                   
*                                                                               
WKTYV10  CLI   0(R1),X'FF'                                                      
         BE    WKTYVE                                                           
         CLC   0(1,R1),IFLD                                                     
         BE    *+12                                                             
         LA    R1,1(R1)                                                         
         B     WKTYV10                                                          
         MVC   ACQAPPL+3(1),IFLD                                                
         OI    FIND,FIVAL          X'04'=X                                      
         B     WKTYVO                                                           
*                                                                               
WKTYVE   MVC   FERN,=AL2(INVINPT)  WORKCODE TYPE INVALID                        
WKTYVO   B     EXIT                                                             
*                                                                               
WKTYPES  DC    C'OPMTR'                                                         
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*              FORMAT VALIDATION                                      *         
***********************************************************************         
*                                                                               
FRM2VAL  NTR1                      FORMAT IN ACQAPPL+4(1)                       
         GOTO1 AINITV                                                           
         BNE   FRM2VO              ALL OR MISSING                               
         CLC   ACQPROG,=C'21'      FOR PROGRAMS 21, 22, 23, 24                  
         BE    FRM2V4                                                           
         CLC   ACQPROG,=C'22'                                                   
         BE    FRM2V4                                                           
         CLC   ACQPROG,=C'23'                                                   
         BE    FRM2V4                                                           
         CLC   ACQPROG,=C'24'                                                   
         BE    FRM2V4                                                           
*                                                                               
         CLI   IFLD,C'1'           1ST BYTE MUST BE 1-7                         
         BL    FRM2VE                                                           
         CLI   IFLD,C'7'                                                        
         BNH   FRM2V2                                                           
         CLI   IFLD,C'8'                                                        
         BNE   FRM2VE                                                           
         CLI   DDS,1                                                            
         BNE   FRM2VE                                                           
*                                                                               
FRM2V2   MVC   ACQAPPL+4(1),IFLD                                                
         OI    FIND,FIVAL          X'04'=X                                      
*                                                                               
FRM2VO   B     EXIT                                                             
*                                                                               
FRM2V4   CLI   IFLD,C'1'           ONLY ONE VALID FOR NOW                       
         BE    FRM2V2                                                           
*                                                                               
FRM2VE   MVC   FERN,=AL2(INVINPT)                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              TITLE START VALIDATION                                 *         
***********************************************************************         
*                                                                               
MTHTVAL  NTR1                      MONTH FOR TITLE IN ACQDTSTR                  
         GOTO1 AINITV                                                           
         BNE   MTHTVO              ALL OR MISSING                               
         CLI   IFLDH+5,9           TAKE FORMAT MMMDD/YY OR MMM/YY               
         BH    MTHTVE                                                           
*                                                                               
         GOTO1 DATVAL,DMCB,(0,IFLD),ACQDTSTR                                    
         OC    DMCB(4),DMCB                                                     
         BNZ   MTHTVO              DATE = YYMMDD                                
*                                                                               
MTHTV02  GOTO1 DATVAL,DMCB,(2,IFLD),ACQDTSTR                                    
         OC    DMCB(4),DMCB                                                     
         BZ    MTHTVE              DATE = YYMMM                                 
         MVC   ACQDTSTR+4(2),SPACES   CLEAR DAY 00                              
*                                                                               
MTHTVO   DS    0H                                                               
         OI    FIND,FIVAL                                                       
         MVI   ACQTYP1,ACQDATE     INDICATE FIELD CONTAINS A DATE               
         MVI   ACQDTTYP,ACQDTTIT   FOR TITLES                                   
         B     EXIT                                                             
*                                                                               
MTHTVE   MVC   FERN,=AL2(INVINPT)                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              TITLE END VALIDATION                                   *         
***********************************************************************         
*                                                                               
MTH2VAL  NTR1                      MONTH FOR TITLE IN ACQDTEND                  
         GOTO1 AINITV                                                           
         BNE   MTH2VO              ALL OR MISSING                               
         CLC   ACQDTSTR,SPACES     DO WE HAVE A START DATE?                     
         BE    MTH2VE              NO, CAN'T HAVE AN END THEN                   
*                                                                               
         CLI   IFLDH+5,9           TAKE FORMAT MMMDD/YY OR MMM/YY               
         BH    MTH2VE                                                           
*                                                                               
         GOTO1 DATVAL,DMCB,(0,IFLD),ACQDTEND                                    
         OC    DMCB(4),DMCB                                                     
         BNZ   MTH2V02                                                          
*                                                                               
         GOTO1 DATVAL,DMCB,(2,IFLD),ACQDTEND                                    
         OC    DMCB(4),DMCB                                                     
         BZ    MTH2VE              INVALID DATE                                 
         MVC   ACQDTEND+4(2),SPACES                                             
*                                                                               
MTH2V02  CLC   ACQDTSTR,ACQDTEND                                                
         BL    MTH2VO                                                           
*                                                                               
MTH2VE   MVC   FERN,=AL2(INVINPT)                                               
*                                                                               
MTH2VO   DS    0H                                                               
         OI    FIND,FIVAL                                                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              INPUT START MONTH VALIDATION                           *         
***********************************************************************         
         SPACE 1                                                                
MNTHSVL  NTR1                                                                   
         GOTO1 AINITV                                                           
         BL    EXIT                ALL OR MISSING                               
         LA    R1,TEMP                                                          
         USING SOFDATD,R1                                                       
         XC    SOFDATD(SOFDATL),SOFDATD                                         
         MVI   SOFITYPE,SOFITYM                                                 
         MVI   SOFIINDS,SOFIIANY+SOFIIOUT+SOFIIONE                              
         TM    RFPSTAT,RFPINUSE                                                 
         BNZ   *+8                                                              
         OI    SOFIINDS,SOFIIRES   NOT RFP - RESOLVE DATES                      
         MVI   SOFOTYPE,SOFOTSD1                                                
         MVC   SOFAINP,FADR                                                     
         LA    R0,ACQSTART                                                      
         ST    R0,SOFAOUT                                                       
         MVC   SOFACOM,ACOMFACS                                                 
         MVI   SOFSYSN,6           SET ACCPAK SYSTEM                            
         MVI   SOFLANG,2                                                        
         MVC   SOFACFST,COMPMOSX                                                
         GOTO1 VSOFDAT                                                          
         BNZ   MNTHSERR                                                         
         OI    FIND,FIVAL                                                       
         B     EXIT                                                             
*                                                                               
MNTHSERR MVC   FERN,=AL2(INVINPT)  MONTH INVALID                                
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              INPUT MONTH VALIDATION                                 *         
***********************************************************************         
         SPACE 1                                                                
MNTHEVL  NTR1                                                                   
         GOTO1 AINITV                                                           
         BL    EXIT                ALL OR MISSING                               
         LA    R1,TEMP                                                          
         USING SOFDATD,R1                                                       
         XC    SOFDATD(SOFDATL),SOFDATD                                         
         MVI   SOFITYPE,SOFITYM                                                 
         MVI   SOFIINDS,SOFIIANY+SOFIIOUT+SOFIIONE                              
         TM    RFPSTAT,RFPINUSE                                                 
         BNZ   *+8                                                              
         OI    SOFIINDS,SOFIIRES   NOT RFP - RESOLVE DATES                      
         MVI   SOFOTYPE,SOFOTSD1                                                
         MVC   SOFAINP,FADR                                                     
         LA    R0,ACQEND                                                        
         CLC   ACQPROG,=C'AA'      FOR THE AA PUT DATE IN QEND                  
         BE    MNTHE50                                                          
         CLC   ACQPROG,=C'AC'      FOR THE ACC CASHFLOW IN QEND                 
         BE    *+8                                                              
         LA    R0,ACQMOSND                                                      
*                                                                               
MNTHE50  ST    R0,SOFAOUT                                                       
         MVC   SOFACOM,ACOMFACS                                                 
         MVI   SOFSYSN,6           SET ACCPAK SYSTEM                            
         MVI   SOFLANG,2                                                        
         MVC   SOFACFST,COMPMOSX                                                
         GOTO1 VSOFDAT                                                          
         BNZ   MNTHERR                                                          
         OI    FIND,FIVAL                                                       
         B     EXIT                                                             
*                                                                               
MNTHERR  MVC   FERN,=AL2(INVINPT)  MONTH INVALID                                
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              LEVEL VALIDATION                                       *         
***********************************************************************         
*                                                                               
LEVVAL   NTR1                      LEVEL IN ACQAPPL+5(1)                        
         GOTO1 AINITV                                                           
         BNE   EXIT                ALL OR MISSING                               
         CLI   IFLD,C'0'                                                        
         BE    LEVVAL2                                                          
         CLI   IFLD,C'C'                                                        
         BE    LEVVAL2                                                          
         CLI   IFLD,C'P'                                                        
         BNE   LEVVALE                                                          
*                                                                               
LEVVAL2  MVC   ACQAPPL+5(1),IFLD                                                
         OI    FIND,FIVAL          X'04'=X                                      
         B     EXIT                                                             
*                                                                               
LEVVALE  MVC   FERN,=AL2(INVINPT)  FORMAT INVALID                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              ACCOUNT VALIDATION                                     *         
***********************************************************************         
*                                                                               
ACCVAL   NTR1                      ACCOUNT - 02=ALL 04=X THRU X(12)             
         GOTO1 AINITV                                                           
         CLI   FIND,FIINP                                                       
         BNE   ACCVO               ACNO ALL IS INVALID                          
*                                                                               
ACCV2    DS    0H                                                               
         CLC   ACQPROG,=C'3D'                                                   
         BNE   ACCV4                                                            
         CLC   IFLD(3),=C'MI='     USING MI RECORD?                             
         BNE   ACCV4                                                            
         MVC   KEY,SPACES                                                       
         MVI   KEY,X'08'                                                        
         MVC   KEY+1(1),ACQCPY     GET MI RECORD                                
         MVC   KEY+2(2),IFLD+3                                                  
         GOTO1 AIOREAD                                                          
         BNE   ACCVO                                                            
*                                                                               
         USING MDIELD,R6                                                        
         L     R6,AIO1                                                          
         MVI   ELCODE,MDIELQ       X'19' MEDIA INTERFACE ELEMENT                
         BAS   RE,GETEL                                                         
         BNE   ACCVO                                                            
*                                                                               
ACCV3    MVC   KEY,SPACES                                                       
         MVC   KEY(1),ACQCPY                                                    
         MVC   KEY+1(2),=C'SI'                                                  
         MVC   IFLD,SPACES                                                      
         MVC   IFLD(12),MDICOMM+2                                               
         MVI   IFLDH+5,12                                                       
         B     ACCV5                                                            
*                                                                               
ACCV4    CLI   ACQLDG,C' '                                                      
         BE    ACCVE               ACCOUNT SPECIFIC & LEDGER NOT                
         GOTO1 AGRPVAL             ACCOUNT GROUP VALIDATION                     
         CLI   FERN,X'FF'          ANY ERRORS?                                  
         BNE   ACCVO               YES                                          
         TM    FIND,FIVAL                                                       
         BNZ   ACCVO                                                            
         MVC   KEY,SPACES                                                       
         MVC   KEY(3),ACQCPY                                                    
         MVC   KEY+3(12),IFLD                                                   
*                                                                               
ACCV5    MVC   ACQACT,IFLD                                                      
         GOTO1 AIOREAD             READ ACCOUNT RECORD                          
         BE    *+14                                                             
         MVC   FERN,=AL2(NTONFILE) ACCOUNT NOT ON FILE                          
         B     ACCVB                                                            
         CLI   IFLDH+5,12                                                       
         BH    ACCVB                                                            
         OI    FIND,FIVAL          ACCOUNT = X THRU XXXXXXXXXXXX                
*                                                                               
         USING OFFALD,R1                                                        
         L     R1,AOFFBLK                                                       
*                                                                               
         TM    OFFACST4,X'01'      TEST FOR NEW OFFICES                         
         BO    ACCSEC              YES-JUST DO SECURITY NUMBER CHECK            
         CLI   LOFFPOS,0                                                        
         BE    ACCSEC              NO OFFICE CHK                                
         CLI   LOFFPOS,12                                                       
         BH    ACCSEC              OFFICE POSITION NOT IN ACNO                  
         L     R0,AIO1                                                          
         ST    R0,OFFAREC                                                       
         MVC   OFFAOPOS,LOFFPOS                                                 
         MVI   OFFAACT,OFFATST     TEST SECURITY                                
         GOTO1 OFFAL                                                            
         BE    ACCSEC                                                           
         MVC   FERN,=AL2(SECLOCK)                                               
         B     ACCVO                                                            
*                                                                               
ACCVB    CLI   ACQLDG,C'P'                                                      
         BE    ACCVC                                                            
         CLI   ACQLDG,C'Q'                                                      
         BNE   ACCVO                                                            
*                                                                               
ACCVC    DS    0H                                                               
         SR    R5,R5                                                            
         IC    R5,IFLDH+5                                                       
         AHI   R5,-1                                                            
         GOTO1 =V(PUBVAL),PLIST,((R5),IFLD+1),(1,TEMP),RR=RB                    
         CLI   0(R1),X'FF'                                                      
         BE    ACCVE                                                            
         MVC   KEY,SPACES                                                       
         MVC   KEY(3),ACQCPY                                                    
         MVC   KEY+3(1),IFLD                                                    
         MVC   KEY+4(11),TEMP                                                   
         GOTO1 AIOREAD                                                          
         BNE   ACCVE               NOT FOUND                                    
         OI    FIND,FIVAL                                                       
         MVC   FERN,=AL2(FF)                                                    
         MVC   ACQACT,KEY+3                                                     
*                                                                               
ACCSEC   DS    0H                  SECURITY CHECK                               
         CLC   ACQPROG,=C'CM'      **NO ACCOUNT SECURITY FOR                    
         BE    ACCVO               **TIMESHEET EDIT REPORT - ACCM               
         L     R6,AIO1                                                          
         MVI   ELCODE,X'30'                                                     
         BAS   RE,GETEL                                                         
         BNE   ACCVO                                                            
         USING RSTELD,R6                                                        
         CLC   ACQPROG,=C'TX'      CHECK FOR LOCKED OR CLOSED FOR TX            
         BNE   ACCSEC2                                                          
         TM    RSTSTAT1,RSTSACIC                                                
         BZ    *+14                                                             
         MVC   FERN,=AL2(ACCCLOS)  ACCOUNT IS CLOSED                            
         B     ACCVO                                                            
         TM    RSTSTAT1,RSTSACIL                                                
         BZ    *+14                                                             
         MVC   FERN,=AL2(ACCLOCK)  ACCOUNT IS LOCKED                            
         B     ACCVO                                                            
*                                                                               
ACCSEC2  CLC   TWAAUTH+1(1),RSTSECY+1                                           
         BNL   ACCVO                                                            
         MVC   FERN,=AL2(SECLOCK)  SECURITY LOCK OUT                            
         B     ACCVO                                                            
         DROP  R6                                                               
*                                                                               
ACCVE    MVC   FERN,=AL2(INVINPT)  ACCOUNT INVALID                              
ACCVO    BAS   RE,DISPNAME                                                      
         B     EXIT                                                             
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
*              MONTH VALIDATION                                       *         
***********************************************************************         
*                                                                               
MVAL     NTR1                      MONTH IN ACQAPPL+6(2)                        
         GOTO1 AINITV                                                           
         BNE   MVO                 ALL OR MISSING                               
         CLI   IFLDH+5,6           TAKE ONLY OF FORMAT MMM/YY                   
         BNE   MVE                 INVALID DATE                                 
         XC    TEMP(80),TEMP                                                    
         GOTO1 DATVAL,DMCB,(2,IFLD),TEMP                                        
         OC    DMCB(4),DMCB                                                     
         BZ    MVE                 INVALID DATE                                 
         MVC   TEMP+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,TEMP),(2,ACQAPPL+6)                               
         CLC   ACQPROG,=C'21'                                                   
         BE    *+14                                                             
         CLC   ACQPROG,=C'22'                                                   
         BNE   MVO                                                              
         GOTO1 DATCON,DMCB,(0,TEMP),(1,TEMP+6)                                  
         MVC   ACQAPPL+6(2),TEMP+6                                              
*                                                                               
MVO      OI    FIND,FIVAL                                                       
         B     EXIT                                                             
*                                                                               
MVE      MVC   FERN,=AL2(INVINPT)  MONTH INVALID                                
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              CLIENT ACCOUNT                                         *         
***********************************************************************         
*                                                                               
CLIACC   NTR1                      CLIENT ACCOUNT                               
         GOTO1 AINITV                                                           
         BNE   EXIT                ALL OR MISSING                               
         CLI   IFLDH+5,3           MUST HAVE AT LEAST U/L/ACCT                  
         BL    CLIERR2                                                          
         CLC   IFLD(2),LREQJOB     U/L=SJ                                       
         BE    CLI100                                                           
         CLC   IFLD(2),=C'1C'      U/L=1C                                       
         BNE   CLIERR2                                                          
*                                                                               
CLI100   MVC   KEY,SPACES                                                       
         MVC   KEY(1),ACQCPY                                                    
         MVC   KEY+1(14),IFLD                                                   
         GOTO1 AIOREAD             READ RECORD                                  
         BNE   CLIERR2             ERROR IN READ                                
         MVC   ACQCNTRA,IFLD                                                    
         OI    FIND,FIVAL                                                       
         BAS   RE,DISPNAME                                                      
         B     EXIT                                                             
*                                                                               
CLIERR2  MVC   FERN,=AL2(INVINPT)                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              NARRATIVE VALIDATION - ACQCOMNT(6)                     *         
***********************************************************************         
*                                                                               
NARVAL   NTR1                                                                   
         GOTO1 AINITV                                                           
         BNE   EXIT                INPUT IS OPTIONAL                            
         MVC   TEMP(80),SPACES                                                  
         MVC   TEMP+6(6),IFLD      RIGHT JUSTIFY                                
         LA    R0,6                                                             
         LA    R1,TEMP+11                                                       
NARVAL3  CLI   0(R1),C' '                                                       
         BH    NARVAL5                                                          
         BCTR  R1,0                                                             
         BCT   R0,NARVAL3                                                       
         B     NARVALE                                                          
*                                                                               
NARVAL5  AHI   R1,-5               RIGHT JUSTIFY                                
         MVC   ACQCOMNT(6),0(R1)                                                
         XC    KEY,KEY             BINARY ZERO FILLED KEY                       
         MVI   KEY,X'0C'           COMMENT RECORD                               
         MVC   KEY+1(1),ACQCPY                                                  
         MVC   KEY+2(6),ACQCOMNT   RIGHT JUSTIFIED FIELD                        
         GOTO1 AIOREAD             READ RECORD                                  
         BNE   NARVALE             ERROR IN READ                                
         OI    FIND,FIVAL                                                       
         B     EXIT                                                             
*                                                                               
NARVALE  MVC   FERN,=AL2(INVINPT)                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              INCREMENT PERCENT VALIDATION - ACQSRTAR(4)             *         
***********************************************************************         
*                                                                               
INCVAL   NTR1                                                                   
         XC    ACQSRTAR+2(4),ACQSRTAR+2                                         
         GOTO1 AINITV                                                           
         BNE   EXIT                INPUT IS OPTIONAL                            
         SR    R5,R5                                                            
         IC    R5,IFLDH+5                                                       
         GOTO1 CASHVAL,DMCB,(2,IFLD),(R5)                                       
         CLI   DMCB,X'FF'                                                       
         BE    INCE                                                             
         ICM   R1,15,DMCB+4                                                     
         C     R1,=F'9999'                                                      
         BH    INCE                                                             
         C     R1,=F'-9999'                                                     
         BL    INCE                                                             
         MVC   ACQSRTAR+2(4),DMCB+4 MOVE IN 4 BYTES ONLY                        
         OI    FIND,FIVAL          VALID AMOUNT                                 
         B     EXIT                                                             
*                                                                               
INCE     MVC   FERN,=AL2(INVAMNT)                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              REVERSAL FIELD - ACQREVOP(1)                           *         
***********************************************************************         
*                                                                               
REVVAL   NTR1                                                                   
         GOTO1 AINITV                                                           
         BNE   EXIT                INPUT IS OPTIONAL                            
         CLI   IFLD,C'Y'                                                        
         BE    REV100                                                           
         CLI   IFLD,C'N'                                                        
         BE    REV100                                                           
         CLC   ACQPROG,=C'31'      ONLY AC31 CAN HAVE 'ONLY' AS                 
         BNE   REVERR              VALID INPUT.                                 
         CLI   IFLD,C'O'                                                        
         BNE   REVERR                                                           
*                                                                               
REV100   MVC   ACQREVOP,IFLD                                                    
         OI    FIND,FIVAL          VALID FIELD                                  
         B     EXIT                                                             
*                                                                               
REVERR   MVC   FERN,=AL2(INVINPT)                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              INCLUDE FLAGS - QOPT7                                  *         
***********************************************************************         
*                                                                               
FLAGVAL  NTR1                                                                   
         GOTO1 AINITV                                                           
         BNE   FLAGX                                                            
         CLI   IFLD,C'N'           N=SUPPRESS                                   
         BE    FLAGX                                                            
         CLI   IFLD,C'O'           O=ONLY FLAGS                                 
         BE    FLAGX                                                            
         CLI   IFLD,C'H'           H=HIRE/TERM                                  
         BE    FLAGX                                                            
         CLI   IFLD,C'Z'           Z=ZERO WKS                                   
         BE    FLAGX                                                            
         MVC   FERN,=AL2(INVINPT)  ELSE ERROR                                   
         B     EXIT                                                             
*                                                                               
FLAGX    MVC   ACQOPT7,IFLD         SAVE SEQUENCE NUMBER                        
         OI    FIND,FIVAL                                                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              STATUS FIELD - ACQTRNST(1) - VALID INPUT = A,H,*H,*A   *         
***********************************************************************         
*                                                                               
STATVAL  NTR1                                                                   
         GOTO1 AINITV                                                           
         BNE   EXIT                INPUT IS OPTIONAL                            
         LA    R5,IFLD                                                          
         CLI   IFLD,C'*'                                                        
         BNE   *+8                                                              
         LA    R5,1(R5)                                                         
         CLI   0(R5),C'A'          APPROVED ITEMS                               
         BE    STAT200                                                          
         CLC   ACQPROG,=C'55'      ONLY APPROVED ITEMS FOR CHECKS               
         BE    STATERR                                                          
         CLI   0(R5),C'H'          HELD ITEMS                                   
         BNE   STATERR                                                          
*                                                                               
STAT200  MVC   ACQTRNST,0(R5)                                                   
         CLI   IFLD,C'*'                                                        
         BNE   *+8                                                              
         NI    ACQTRNST,X'BF'      MARK TO EXCLUDE                              
         OI    FIND,FIVAL          VALID AMOUNT                                 
         B     EXIT                                                             
*                                                                               
STATERR  MVC   FERN,=AL2(INVINPT)                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              SEQUENCE FIELD - ACQREPSQ                              *         
***********************************************************************         
*                                                                               
SEQVAL   NTR1                                                                   
         GOTO1 AINITV                                                           
         BNE   SEQVO                                                            
         CLI   IFLD,C'A'           A(CCOUNT)                                    
         BE    SEQVO                                                            
         CLI   IFLD,C'D'           D(ETAIL)                                     
         BE    SEQVO                                                            
         CLI   IFLD,C'O'           O(FFICE)                                     
         BE    SEQVO                                                            
         MVC   FERN,=AL2(INVINPT)  ELSE ERROR                                   
         B     EXIT                                                             
*                                                                               
SEQVO    MVC   ACQREPSQ,IFLD       SAVE SEQUENCE NUMBER                         
         OI    FIND,FIVAL                                                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              XJOB FIELD - ACQXJOB                                   *         
***********************************************************************         
*                                                                               
XJOBVAL  NTR1                                                                   
         GOTO1 AINITV                                                           
         BNE   XJOBX                                                            
         CLI   IFLD,C'Y'           Y=INCLUDE X-JOBS                             
         BE    XJOBX                                                            
         CLI   IFLD,C'N'           N=EXCLUDE X-JOBS                             
         BE    XJOBX                                                            
         CLI   IFLD,C'O'           O=ONLY X-JOBS                                
         BE    XJOBX                                                            
         MVC   FERN,=AL2(INVINPT)  ELSE ERROR                                   
         B     EXIT                                                             
*                                                                               
XJOBX    MVC   ACQXJOB,IFLD        SAVE SEQUENCE NUMBER                         
         OI    FIND,FIVAL                                                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              DJOB FIELD - ACQDJOB                                   *         
***********************************************************************         
*                                                                               
DJOBVAL  NTR1                                                                   
         GOTO1 AINITV                                                           
         BNE   DJOBX                                                            
         CLI   IFLD,C'Y'           Y=INCLUDE DRAFT JOBS                         
         BE    DJOBX                                                            
         CLI   IFLD,C'N'           N=EXCLUDE DRAFT JOBS                         
         BE    DJOBX                                                            
         CLI   IFLD,C'O'           O=ONLY DRAFT JOBS                            
         BE    DJOBX                                                            
         MVC   FERN,=AL2(INVINPT)  ELSE ERROR                                   
         B     EXIT                                                             
*                                                                               
DJOBX    MVC   ACQDJOB,IFLD        SAVE SEQUENCE NUMBER                         
         OI    FIND,FIVAL                                                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              OVERHEAD  - ACQCNTRA                                  *          
***********************************************************************         
*                                                                               
OVERVAL  NTR1                                                                   
         GOTO1 AINITV                                                           
         BNE   OVERX                                                            
         SR    R5,R5                                                            
         IC    R5,IFLDH+5          YES, GET LENGTH                              
         GOTO1 CASHVAL,DMCB,(2,IFLD),(R5)                                       
         CLI   DMCB,X'FF'                                                       
         BE    OVER4                                                            
*                                                                               
         CLC   ACQPROG,=C'TX'      ACTX AMOUNT IS IN FILTER #5                  
         BNE   OVER2                                                            
         L     R1,DMCB+4           MUST BE LESS THAN 1000.00                    
         C     R1,=F'0'            AND GREATER THAN 0                           
         BL    OVER4                                                            
         CVD   R1,DUB                                                           
*                                                                               
         UNPK  ACQFLT5(8),DUB+3(5)                                              
         OI    ACQFLT5+7,X'F0'                                                  
         OI    FIND,FIVAL                                                       
         MVI   ACQTYP5,C'5'        SET INDICATOR FOR ACTX                       
         B     OVERX                                                            
                                                                                
OVER2    L     R1,DMCB+4           MUST BE LESS THAN 1000.00                    
         C     R1,=F'99999'                                                     
         BH    OVER4                                                            
         C     R1,=F'0'            AND GREATER THAN 0                           
         BL    OVER4                                                            
         CVD   R1,DUB                                                           
*                                                                               
         UNPK  ACQFLT1(5),DUB+5(3)                                              
         OI    ACQFLT1+4,X'F0'                                                  
         MVI   ACQTYP1,ACQOVHD                                                  
         OI    FIND,FIVAL                                                       
         B     OVERX                                                            
*                                                                               
OVER4    MVC   FERN,=AL2(INVAMNT)  INVALID AMOUNT                               
*                                                                               
OVERX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              TOLERANCE - QAPPL                                      *         
***********************************************************************         
*                                                                               
TOLRVAL  NTR1                                                                   
         BRAS  RE,LOOKAA                                                        
         GOTO1 AINITV                                                           
         BL    TOLR3               BLANK VALUES, VALIDATE PROFILE VALS?         
         BNE   OVERX                                                            
                                                                                
         CLI   USETOLR,C'Y'                                                     
         BNE   TOLR5                                                            
         SR    R5,R5                                                            
         IC    R5,IFLDH+5          GET LENGTH                                   
         GOTO1 CASHVAL,DMCB,(2,IFLD),(R5)                                       
         CLI   DMCB,X'FF'                                                       
         BE    TOLR4               INVALID RESULT FROM CASHVAL                  
*                                                                               
         L     R1,DMCB+4           MUST BE LESS THAN 1099                       
         CHI   R1,1099                                                          
         BH    TOLR4                                                            
         CHI   R1,0                AND GREATER THAN 0                           
         BL    TOLR4                                                            
         STCM  R1,3,ACQAPPL                                                     
         MVC   ACQOPT10,USETOLR                                                 
         OI    FIND,FIVAL                                                       
         B     TOLRX                                                            
*                                                                               
TOLR3    MVC   ACQOPT10,USETOLR    SAVE THE TOLERANCE FLAG                      
         CLI   USETOLR,C'N'        TOLERANCE TYPE=N                             
         JE    TOLR31                                                           
         LLC   R1,TOLINTG                                                       
         MHI   R1,100                                                           
         LLC   R0,TOLDECI                                                       
         AR    R1,R0                                                            
         STCM  R1,3,ACQAPPL                                                     
*                                                                               
TOLR31   CLI   USETOLR,C'Y'        TOLERANCE TYPE=Y                             
         JNE   OVERX                                                            
         CLI   TOLINTG,X'0A'       TOLERANCE VALUE ~ 0 - 10                     
         JNH   OVERX               EXIT                                         
         OI    FIND,FIINP                                                       
         J     TOLR5               ERROR - INCONSISTENT PROFILE                 
*                                                                               
TOLR4    MVC   FERN,=AL2(INVAMNT)  INVALID AMOUNT                               
         B     EXIT                                                             
*                                                                               
TOLR5    MVC   FERN,=AL2(AE$INAAP)   INCONSISTENT WITH AA PROFILE               
*                                                                               
TOLRX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              TOLERANCE - QAPPL  (FOR $$)                            *         
***********************************************************************         
*                                                                               
TOLR2VAL NTR1                                                                   
         GOTO1 AINITV                                                           
         BNE   OVERX                                                            
         BRAS  RE,LOOKAA                                                        
                                                                                
         CLI   USETOLR,C'D'                                                     
         BNE   TOLR2_5                                                          
         SR    R5,R5                                                            
         IC    R5,IFLDH+5          GET LENGTH                                   
         GOTO1 CASHVAL,DMCB,(2,IFLD),(R5)                                       
         CLI   DMCB,X'FF'                                                       
         BE    TOLR2_4             INVALID RESULT FROM CASH VAL                 
*                                                                               
         L     R1,DMCB+4           MUST BE LESS THAN 25599                      
         C     R1,=F'25599'                                                     
         BH    TOLR2_4                                                          
         C     R1,=F'0'            AND GREATER THAN 0                           
         BL    TOLR2_4                                                          
         STCM  R1,3,ACQAPPL                                                     
         MVC   ACQOPT10,USETOLR                                                 
         OI    FIND,FIVAL                                                       
         B     TOLR2X                                                           
*                                                                               
TOLR2_4  MVC   FERN,=AL2(INVAMNT)  INVALID AMOUNT                               
         B     EXIT                                                             
*                                                                               
TOLR2_5  MVC   FERN,=AL2(AE$INAAP)   INCONSISTENT WITH AA PROFILE               
*                                                                               
TOLR2X   B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              FORM = N/A                                             *         
***********************************************************************         
*                                                                               
NAVAL    NTR1                                                                   
         MVC   FERN,=AL2(INVREQN)                                               
NAVALX   B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              FORM = QAPPL                                           *         
***********************************************************************         
*                                                                               
FRM99VAL NTR1                                                                   
         GOTO1 AINITV                                                           
         MVI   ACQCONT1,ACQCONTQ                                                
         MVI   ACQAPPL,C'M'        DEFAULT MISC FORM                            
         SR    R5,R5                                                            
         IC    R5,IFLDH+5          GET LENGTH                                   
         LTR   R5,R5                                                            
         BZ    FRM99X                                                           
*                                                                               
         CLI   IFLD,C'M'           MISC FORM                                    
         BE    FRM99010                                                         
         CLI   IFLD,C'N'           NEC FORM                                     
         BNE   FRM99ERR            ELSE ERROR                                   
         MVC   ACQPROG,=C'T7'      NEC USES T7 JCL                              
*                                                                               
FRM99010 MVC   ACQAPPL(1),IFLD                                                  
         OI    FIND,FIVAL                                                       
         B     FRM99X                                                           
*                                                                               
FRM99ERR MVC   FERN,=AL2(INVINPT)                                               
FRM99X   B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              CONTRA ACCOUNT - ACQCNTR                               *         
***********************************************************************         
*                                                                               
CNTRVAL  NTR1                      CLIENT ACCOUNT                               
         GOTO1 AINITV                                                           
         BNE   EXIT                ALL OR MISSING                               
*        CLI   IFLDH+5,3           MUST HAVE AT LEAST U/L/ACCT                  
*        BL    CNTRVE                                                           
*        CLC   IFLD(2),=C'1R'      U/L=1R                                       
*        BNE   CNTRVE                                                           
*                                                                               
         MVC   ACQCACUL,=C'1R'                                                  
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),ACQCPY                                                    
         MVC   KEY+1(2),ACQCACUL                                                
         MVC   KEY+3(12),IFLD                                                   
         GOTO1 AIOREAD             READ RECORD                                  
         BNE   CNTRVE              ERROR IN READ                                
*        MVC   ACQCNTRA,IFLD                                                    
         MVC   ACQCACT,IFLD                                                     
         OI    FIND,FIVAL                                                       
         BAS   RE,DISPNAME                                                      
         B     EXIT                                                             
*                                                                               
CNTRVE   MVC   FERN,=AL2(INVINPT)                                               
         MVI   ROUTNUM,188                                                      
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY NAME ROUTINE                                                *         
***********************************************************************         
         SPACE 1                                                                
DISPNAME NTR1                                                                   
         CLC   NAME,SPACES                                                      
         BNH   EXIT                                                             
         L     R6,FLDHADR                                                       
         SR    R1,R1                                                            
         IC    R1,IFLDH                                                         
         AR    R6,R1                                                            
         FOUT  (R6),NAME                                                        
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GETEL                                                               *         
***********************************************************************         
         SPACE 1                                                                
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
***********************************************************************         
*              CONSTANTS                                              *         
***********************************************************************         
*                                                                               
*              ATTRIBUTE TABLE                                                  
*                                                                               
ATTRTAB  DC    C'DPT9C8EV',X'FF'            ATTRIBUTE                           
EXCPTLST DC    C'123456789ABCDEFGHIJKLMNOP',X'FF'  LISTS FOR RSELVAL            
DMRDHI   DC    CL8'DMRDHI'                                                      
DMREAD   DC    CL8'DMREAD'                                                      
ACCOUNT  DC    CL8'ACCOUNT'                                                     
DATADISP DC    H'49'                                                            
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*              TABLES                                                 *         
***********************************************************************         
*                                                                               
*              UNION TABLE                                                      
*                                                                               
UNITAB   DS    0CL4                                                             
         DC    C'AFT',X'80'                                                     
         DC    C'SAG',X'40'                                                     
         DC    C'AFM',X'20'                                                     
         DC    C'NON',X'08'                                                     
         DC    C'ACT',X'04'                                                     
         DC    X'FF'                                                            
         EJECT ,                                                                
***********************************************************************         
*              VALIDATION ADDRESS TABLE                               *         
***********************************************************************         
*                                                                               
ROUTADRT DC    F'0'                00 - N/D                                     
         DC    A(PAYCVAL)          01 - PAYROLL CODE IN QSELECT                 
         DC    A(REPRTVAL)         02 - REPORT                                  
         DC    A(BNOVAL)           03 - BILL NUMBER                             
         DC    A(YRLYVAL)          04 - YEARLY IN OPT 4-5                       
         DC    A(DTYSVAL)          05 - DATA TYPES IN QSRTAREA                  
         DC    A(BDTVAL)           06 - BUDGET TYPE                             
         DC    A(RSELVAL)          07 - EXCEPTION LIST ,OFF/DEPT, APG           
         DC    A(CONTRVAL)         08 - CONTRA SELECT                           
         DC    A(NARRVAL)          09 - NARRATIVE 00-99                         
         DC    A(MTH2VAL)          10 - TITLE END IN QDTEND                     
         DC    A(BDAYVAL)          11 - BILLING DUE DAYS                        
         DC    A(MOSVAL)           12 - MOS START/END MONTHS                    
         DC    A(NUMVAL)           13 - NUMERIC CODE IN QSELECT                 
         DC    A(MINMVAL)          14 - MINIMUM AMT IN QSRTAREA(EBCDIC)         
         DC    A(CONVAL)           15 - CURRENCY CONVERSION IN QBILGRUP         
         DC    A(MEDMOS)           16 - MEDIA MOS START/END                     
         DC    A(AGYFVAL)          17 - AGENCY FILTER IN QSRTAREA               
         DC    A(MEDFVAL)          18 - MEDIA FILTER IN QSRTAREA+3              
         DC    A(BUDFVAL)          19 - BUDGET FILTER IN QSRTAREA+5             
         DC    A(BDTVAL2)          20 - BUDGET TYPE IN QSRTAREA+1               
         DC    A(WKCDVAL)          21 - WORK-CODE FILTER IN QTRNSFILT           
         DC    A(UNIVAL)           22 - UNION EQUATE IN QSELECT+5               
         DC    A(UNCVAL)           23 - UNION CODE IN QSELECT(3)                
         DC    A(GETGRP)           24 - VALIDATE GROUP CODE                     
         DC    A(DEPTVAL)          25 - DEPARTMENT IN QSELECT(6)                
         DC    A(TASKVAL)          26 - TASK IN QAPPL+2(2)                      
         DC    A(MEDGVAL)          27 - MEDIA GROUP IN QMGROUP                  
         DC    A(ATTRVAL)          28 - ATTRIBUTE IN QOPT4, 5, & 6              
         DC    A(ESTVAL)           29 - ESTIMATE NUMBER IN QBILGRUP             
         DC    A(DTSVAL2)          30 - DATA TYPES IN QAPPL                     
         DC    A(USRVAL)           31 - USER FIELD IN QAPPL(2)                  
         DC    A(GENERIC)          32 - GENERIC MOVE ROUTINE                    
         DC    A(JGRPVAL)          33 - JOB GROUP IN QAPPL(3)                   
         DC    A(WKTYVAL)          34 - WORKCODE TYPE IN QAPPL+3(1)             
         DC    A(FRM2VAL)          35 - FORMAT IN QAPPL+4(1)                    
         DC    A(MTHTVAL)          36 - TITLE START IN QDTSTR                   
         DC    A(HBUDVAL)          37 - STD HRS BUD IN QAPPL(1)                 
         DC    A(RBUDVAL)          38 - STD RTE BUD IN QAPPL+1(1)               
         DC    A(TPERVAL)          39 - TARGET PERCENT IN QAPPL+2(1)            
         DC    A(MNTHEVL)          40 - MONTH IN QMOSEND YYMM                   
         DC    A(LEVVAL)           41 - LEVEL IN QAPPL+5(1)                     
         DC    A(ACTSTVL)          42 - ACTIVITY STRT IN QACTSTRT(6)            
         DC    A(ACTENDVL)         43 - ACTIVITY END IN QACTEND(6)              
         DC    A(ACCVAL)           44 - ACCOUNT                                 
         DC    A(MVAL)             45 - MONTH IN QAPPL(2)                       
         DC    A(CLIACC)           46 - CLIENT ACCOUNT IN QCONTRA               
         DC    A(MNTHSVL)          47 - MONTH IN QSTART YYMM                    
         DC    A(NARVAL)           48 - NARRATIVE IN QCOMMENT(6)                
         DC    A(INCVAL)           49 - INCREMENT PERCENT IN QAPL(3)            
         DC    A(REVVAL)           50 - REVERSAL IN QREVERSE(1)                 
         DC    A(STATVAL)          51 - STATUS IN QSTATUS(1)                    
         DC    A(FLAGVAL)          52 - INCLUDE FLAGS IN QOPT7                  
         DC    A(SEQVAL)           53 - SEQUENCE INTO QSEQ                      
         DC    A(XJOBVAL)          54 - EXPENSE JOB  Y/N/O                      
         DC    A(OVERVAL)          55 - OVERHEAD                                
         DC    A(METHVAL)          56 - METHOD                                  
         DC    A(CAFILT1)          57 - CA FILTER 1                             
         DC    A(CAFILT2)          58 - CA FILTER 2                             
         DC    A(CAFILT3)          59 - CA FILTER 3                             
         DC    A(CAFILT4)          60 - CA FILTER 4                             
         DC    A(CAFILT5)          61 - CA FILTER 5                             
         DC    A(DJOBVAL)          62 - DRAFT JOB Y/N/O                         
         DC    A(CNTRVAL)          63 - CONTRA ACCOUNT                          
         DC    A(TOLRVAL)          64 - TOLERANCE QAPPL(2)                      
         DC    A(TOLR2VAL)         65 - TOLERANCE QAPPL(2) $$                   
         DC    A(NAVAL)            66 - N/A                                     
         DC    A(FRM99VAL)         67 - FORM                                    
         EJECT                                                                  
***********************************************************************         
*              LOOK UP AA PROFILE                                     *         
***********************************************************************         
         SPACE 1                                                                
LOOKAA   NTR1  BASE=*,LABEL=*                                                   
         USING PROFKD,R4                                                        
         LA    R4,WORK             READ AA PROFILE RECORD                       
         XC    WORK,WORK                                                        
         MVI   PROFKSYS,C'A'         ACCOUNT SYSTEM                             
         MVC   PROFKPGM+1(2),=C'AA'  AA PROGRAM                                 
         MVC   PROFKAGY,TWAAGY     ALPHA ID                                     
         XC    DMCB(24),DMCB                                                    
         GOTO1 GETPROF,DMCB,PROFKEY,WORK+L'PROFKEY,DATAMGR,0                    
         OC    WORK+L'PROFKEY(L'PROFKEY),WORK+L'PROFKEY ANY PROFILES?           
         JZ    NO                  NO, DON'T ALLOW OFFICE FOR AA/AB             
         MVC   USETOLR(1),WORK+L'PROFKEY+5     USE TOLERANCE                    
         MVC   TOLINTG(1),WORK+L'PROFKEY+6     TOLERANCE-INTEGER BYTE           
         MVC   TOLDECI(1),WORK+L'PROFKEY+7     TOLERANCE-DECIMAL BYTE           
*                                                                               
YES      CR    RB,RB                                                            
         B     *+6                                                              
NO       LTR   RB,RB                                                            
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
***********************************************************************         
*              MOS VALIDATION                                         *         
***********************************************************************         
         SPACE 1                                                                
MOSVALR  NTR1  BASE=*,LABEL=*                                                   
         GOTO1 AINITV              04=MMM/YY(-MMM/YY)                           
         BNE   MOSVALX             08=(MMM/YY)-MMM/YY  HAS END DATE             
*                                                                               
         LA    R1,TEMP                                                          
         USING SOFDATD,R1                                                       
         XC    SOFDATD(SOFDATL),SOFDATD                                         
         MVI   SOFITYPE,SOFITYM                                                 
         MVI   SOFIINDS,SOFIIANY+SOFIIOUT                                       
         TM    RFPSTAT,RFPINUSE                                                 
         BNZ   *+8                                                              
         OI    SOFIINDS,SOFIIRES   NOT RFP - RESOLVE DATES                      
         CLC   =C'R6',ACQPROG                                                   
         BE    *+8                                                              
         OI    SOFIINDS,SOFIIF1O   SET START OPTIONAL IF NOT R6 RPT             
         MVI   SOFOTYPE,SOFOTSD1                                                
         MVC   SOFAINP,FADR                                                     
         LA    R0,ACQMOSST                                                      
         ST    R0,SOFAOUT                                                       
         MVC   SOFACOM,ACOMFACS                                                 
         MVI   SOFSYSN,6           SET ACCPAK SYSTEM                            
         MVI   SOFLANG,2                                                        
         MVC   SOFACFST,COMPMOSX                                                
         GOTO1 VSOFDAT                                                          
         BNZ   MOSVE2                                                           
         OI    FIND,FIVAL+FIALT    SET VALID BIT                                
         B     MOSVX                                                            
*                                                                               
MOSVE2   MVC   FERN,=AL2(INVINPT)  INVALID FIELD                                
MOSVALX  TM    RFPSTAT,RFPINUSE                                                 
         BZ    MOSVX                                                            
*                                                                               
*              $RFP - VALIDATE SYMBOLIC EQUATE                                  
*                                                                               
         OI    FIND,FIVAL                                                       
         MVC   FERN,=AL2(FF)                                                    
         XC    QRFPBLK(QRFPBLKQ),QRFPBLK                                        
         MVI   QRFPMODE,QRFPSYMB                                                
         MVC   QRFPWORK,IFLD                                                    
         OC    QRFPWORK,SPACES                                                  
         GOTO1 ARFP,DMCB                                                        
         CLI   QRFPMODE,QRFPOK                                                  
         BE    *+14                                                             
         MVC   FERN,=AL2(ISYMBEQU)      INVALID SYMBOLIC EQUATE                 
         B     MOSVX                                                            
*                                                                               
         CLC   QRFPDICT,=Y(E#MOARG)     MOARANGE (YYMMDD)                       
         BE    *+14                                                             
         MVC   FERN,=AL2(ISYMBFLD)      INVALID SYMBOLIC EQUATE                 
         B     MOSVX                                                            
*                                                                               
         MVC   ACQMOSST(8),SPACES       STORE ESCAPE SEQ IN REQCARD             
         MVC   ACQMOSST(L'QRFPESC),QRFPESC                                      
         MVI   ACQMOSST+3,8             L'MOA FIELD                             
         MVC   FERN,=AL2(FF)                                                    
MOSVX    XIT1                                                                   
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* MEDIA MOS VALIDATION - ACQDTSTR/ACQDTEND                            *         
***********************************************************************         
         SPACE 1                                                                
MEDMOS   NTR1  BASE=*,LABEL=*                                                   
         GOTO1 AINITV              04=MMM/YY(-MMM/YY)                           
         BNE   MEDMX               08=(MMM/YY)-MMM/YY  HAS END DATE             
*                                                                               
         USING SOFDATD,R1                                                       
         LA    R1,TEMP                                                          
         XC    SOFDATD(SOFDATL),SOFDATD                                         
         MVI   SOFITYPE,SOFITYM    FIRST CALL YYMM                              
         MVI   SOFIINDS,SOFIIANY+SOFIIOUT                                       
         TM    RFPSTAT,RFPINUSE                                                 
         BNZ   *+8                                                              
         OI    SOFIINDS,SOFIIRES   NOT RFP - RESOLVE DATES                      
         CLC   =C'R6',ACQPROG                                                   
         BE    *+8                                                              
         OI    SOFIINDS,SOFIIF1O   SET START OPTIONAL IF NOT R6 RPT             
         MVI   SOFOTYPE,SOFOTSD1+SOFOTSPC                                       
         MVC   SOFAINP,FADR                                                     
         LA    R0,WORK                                                          
         ST    R0,SOFAOUT                                                       
         MVC   SOFACOM,ACOMFACS                                                 
         MVI   SOFSYSN,6           SET ACCPAK SYSTEM                            
         MVI   SOFLANG,2                                                        
         MVC   SOFACFST,COMPMOSX                                                
         GOTO1 VSOFDAT                                                          
         BNZ   MEDM10                                                           
         OI    FIND,FIVAL          SET VALID BIT                                
         MVC   ACQDTSTR(12),SPACES    CLEAR BOTH START AND END FIELDS           
         MVC   ACQDTSTR(4),WORK                                                 
         MVC   ACQDTSTR+4(2),SPACES                                             
         MVC   ACQDTEND,WORK+4                                                  
         MVC   ACQDTEND+4(2),SPACES                                             
         B     MEDM20                                                           
*                                                                               
MEDM10   MVI   SOFITYPE,SOFITYMD   SECOND CALL YYMMDD                           
         MVI   SOFOTYPE,SOFOTSD2                                                
         GOTO1 (RF),(R1)                                                        
         BNZ   MEDME                                                            
         OI    FIND,FIALT          SET VALID BIT                                
         MVC   ACQDTSTR(12),SPACES    CLEAR BOTH START AND END FIELDS           
         MVC   ACQDTSTR(12),WORK      FILL IN STR/END DATES FROM SOFDAT         
*                                                                               
MEDM20   MVI   ACQTYP1,ACQDATE                                                  
         MVI   ACQDTTYP,ACQDTMOS   MEDIA MONTH OF SERVICE                       
         B     MEDMX                                                            
*                                                                               
MEDME    MVC   FERN,=AL2(INVDATEF) INVALID DATE                                 
********                                                                        
******                                                                          
*EDMOSX  TM    RFPSTAT,RFPINUSE                                                 
*        BZ    MEDMX                                                            
*                                                                               
*              $RFP - VALIDATE SYMBOLIC EQUATE                                  
*                                                                               
*        OI    FIND,FIVAL                                                       
*        MVC   FERN,=AL2(FF)                                                    
*        XC    QRFPBLK(QRFPBLKQ),QRFPBLK                                        
*        MVI   QRFPMODE,QRFPSYMB                                                
*        MVC   QRFPWORK,IFLD                                                    
*        OC    QRFPWORK,SPACES                                                  
*        GOTO1 ARFP,DMCB                                                        
*        CLI   QRFPMODE,QRFPOK                                                  
*        BE    *+14                                                             
*        MVC   FERN,=AL2(ISYMBEQU)      INVALID SYMBOLIC EQUATE                 
*        B     MEDMX                                                            
*                                                                               
*        CLC   QRFPDICT,=Y(E#MOARG)     MOARANGE (YYMMDD)                       
*        BE    *+14                                                             
*        MVC   FERN,=AL2(ISYMBFLD)      INVALID SYMBOLIC EQUATE                 
*        B     MEDMX                                                            
*                                                                               
*        MVC   ACQMOSST(8),SPACES       STORE ESCAPE SEQ IN REQCARD             
*        MVC   ACQMOSST(L'QRFPESC),QRFPESC                                      
*        MVI   ACQMOSST+3,8             L'MOA FIELD                             
*        MVC   FERN,=AL2(FF)                                                    
*        B     MEDMX                                                            
*                                                                               
MEDMX    XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CONTRA ACCOUNT FILTERS 1 - 5                                        *         
***********************************************************************         
         SPACE 1                                                                
CAFILT1  NTR1  BASE=*,LABEL=*                                                   
         LA    R6,ACQCFLT1         ** FILTER #1 **                              
         J     CFTRVAL                                                          
*                                                                               
CAFILT2  NTR1  BASE=*,LABEL=*                                                   
         LA    R6,ACQCFLT2         ** FILTER #2 **                              
         J     CFTRVAL                                                          
*                                                                               
CAFILT3  NTR1  BASE=*,LABEL=*                                                   
         LA    R6,ACQCFLT3         ** FILTER #3 **                              
         J     CFTRVAL                                                          
*                                                                               
CAFILT4  NTR1  BASE=*,LABEL=*                                                   
         LA    R6,ACQCFLT4         ** FILTER #4 **                              
         J     CFTRVAL                                                          
*                                                                               
CAFILT5  NTR1  BASE=*,LABEL=*                                                   
         LA    R6,ACQCFLT5         ** FILTER #5 **                              
         J     CFTRVAL                                                          
*                                                                               
CFTRVAL  GOTO1 AINITV                                                           
         JL    FTRVO               FILTER MISSING                               
         CLI   IFLDH+5,1                                                        
         JH    FTRV1                                                            
         MVC   0(1,R6),IFLD                                                     
         OI    FIND,FIVAL          FILTER = X                                   
         J     FTRVO                                                            
*                                                                               
FTRV1    CLI   IFLD,C'*'           NEGATIVE FILTER                              
         JNE   FTRVE                                                            
         MVC   0(1,R6),IFLD+1                                                   
         NI    0(R6),X'BF'         TURN OFF X'40' BIT                           
         OI    FIND,FIVAL          FILTER=-X                                    
         J     FTRVO                                                            
*                                                                               
FTRVE    MVC   FERN,=AL2(INVINPT)  FILTER INVALID                               
*                                                                               
FTRVO    XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*              ACTIVITY START DATE                                    *         
***********************************************************************         
         SPACE 1                                                                
ACTSTVL  NTR1  BASE=*,LABEL=*                                                   
         GOTO1 AINITV                                                           
         BNE   ACTSTXIT            ALL OR MISSING                               
*                                                                               
         LA    R1,TEMP                                                          
         USING SOFDATD,R1                                                       
         XC    SOFDATD(SOFDATL),SOFDATD                                         
         MVI   SOFITYPE,SOFITYMD                                                
         MVI   SOFIINDS,SOFIIANY+SOFIIOUT+SOFIIONE                              
         TM    RFPSTAT,RFPINUSE                                                 
         BNZ   *+8                                                              
         OI    SOFIINDS,SOFIIRES   NOT RFP - RESOLVE DATES                      
         MVI   SOFOTYPE,SOFOTSD2                                                
         MVC   SOFAINP,FADR                                                     
         LA    R0,ACQACTST                                                      
         CLC   ACQPROG,=C'77'      FOR AC77 PUT IN QSTART FIELD                 
         BNE   *+8                                                              
         LA    R0,ACQSTART                                                      
         ST    R0,SOFAOUT                                                       
         MVC   SOFACOM,ACOMFACS                                                 
         MVI   SOFSYSN,6           SET ACCPAK SYSTEM                            
         MVI   SOFLANG,2                                                        
         MVC   SOFACFST,COMPMOSX                                                
         GOTO1 VSOFDAT                                                          
         BNZ   ACTSTE                                                           
         OI    FIND,FIVAL                                                       
         B     ACTSTXIT                                                         
*                                                                               
ACTSTE   MVC   FERN,=AL2(INVDATEF) INVALID START DATE                           
         TM    RFPSTAT,RFPINUSE                                                 
         BZ    ACTSTXIT                                                         
*                                                                               
*              $RFP - VALIDATE SYMBOLIC EQUATE                                  
*                                                                               
         OI    FIND,FIVAL                                                       
         XC    QRFPBLK(QRFPBLKQ),QRFPBLK                                        
         MVI   QRFPMODE,QRFPSYMB                                                
         MVC   QRFPWORK,IFLD                                                    
         OC    QRFPWORK,SPACES                                                  
         GOTO1 ARFP,DMCB                                                        
         CLI   QRFPMODE,QRFPOK                                                  
         BE    *+14                                                             
         MVC   FERN,=AL2(ISYMBEQU)      INVALID SYMBOLIC EQUATE                 
         B     ACTSTXIT                                                         
*                                                                               
         CLC   QRFPDICT,=Y(E#STARTA)    YYMMDD                                  
         BE    ACTST10                                                          
*                                                                               
         CLC   QRFPDICT,=Y(E#TODAY)     YYMMDD                                  
         BE    *+14                                                             
         MVC   FERN,=AL2(ISYMBFLD)      INVALID SYMBOLIC EQUATE                 
         B     ACTSTXIT                                                         
*                                                                               
ACTST10  CLC   ACQPROG,=C'77'           FOR AC77 DATE IS IN QSTART              
         BNE   ACTST20                                                          
         MVC   ACQSTART,SPACES          STORE ESCAPE SEQ IN REQCARD             
         MVC   ACQSTART(L'QRFPESC),QRFPESC                                      
         MVI   ACQSTART+3,L'ACQSTART                                            
         B     ACTST30                                                          
*                                                                               
ACTST20  MVC   ACQACTST,SPACES                                                  
         MVC   ACQACTST(L'QRFPESC),QRFPESC                                      
         MVI   ACQACTST+3,L'ACQACTST                                            
ACTST30  MVC   FERN,=AL2(FF)                                                    
*                                                                               
ACTSTXIT XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*              ACTIVITY END DATE                                      *         
***********************************************************************         
         SPACE 1                                                                
ACTENDVL NTR1  BASE=*,LABEL=*                                                   
         GOTO1 AINITV                                                           
         BNE   ACTENXIT            ALL OR MISSING                               
*                                                                               
         LA    R1,TEMP                                                          
         USING SOFDATD,R1                                                       
         XC    SOFDATD(SOFDATL),SOFDATD                                         
         MVI   SOFITYPE,SOFITYMD                                                
         MVI   SOFIINDS,SOFIIANY+SOFIIOUT+SOFIIONE                              
         TM    RFPSTAT,RFPINUSE                                                 
         BNZ   *+8                                                              
         OI    SOFIINDS,SOFIIRES   NOT RFP - RESOLVE DATES                      
         MVI   SOFOTYPE,SOFOTSD2                                                
         MVC   SOFAINP,FADR                                                     
         LA    R0,ACQACTND                                                      
         CLC   ACQPROG,=C'77'      FOR AC77 PUT IN QEND FIELD                   
         BNE   *+8                                                              
         LA    R0,ACQEND                                                        
         ST    R0,SOFAOUT                                                       
         MVC   SOFACOM,ACOMFACS                                                 
         MVI   SOFSYSN,6           SET ACCPAK SYSTEM                            
         MVI   SOFLANG,2                                                        
         MVC   SOFACFST,COMPMOSX                                                
         GOTO1 VSOFDAT                                                          
         BNZ   ACTE10                                                           
         OI    FIND,FIVAL                                                       
         TM    SOFIINDS,SOFIIRES                                                
         BZ    ACTENXIT                                                         
*                                                                               
         LA    R2,ACQACTND         A(ACTIVITY END)                              
         LA    R1,ACQACTST         A(ACTIVITY START)                            
         CLC   ACQPROG,=C'77'                                                   
         BNE   *+12                                                             
         LA    R2,ACQEND           A77 USES DIFFERENT FIELDS                    
         LA    R1,ACQSTART                                                      
*                                                                               
         CLI   0(R1),C' '          ANY START DATE?                              
         BE    ACTENXIT            NO                                           
         CLC   0(L'ACQACTST,R1),0(R2)                                           
         BNH   ACTENXIT            START MUST BE LOWER THAN END                 
         MVC   FERN,=AL2(INVDATEF) INVALID END DATE                             
         B     ACTENXIT                                                         
*                                                                               
ACTE10   MVC   FERN,=AL2(INVDATEF) INVALID END DATE                             
         TM    RFPSTAT,RFPINUSE                                                 
         BZ    ACTENXIT                                                         
*                                                                               
*              $RFP - VALIDATE SYMBOLIC EQUATE                                  
*                                                                               
         OI    FIND,FIVAL                                                       
         XC    QRFPBLK(QRFPBLKQ),QRFPBLK                                        
         MVI   QRFPMODE,QRFPSYMB                                                
         MVC   QRFPWORK,IFLD                                                    
         OC    QRFPWORK,SPACES                                                  
         GOTO1 ARFP,DMCB                                                        
         CLI   QRFPMODE,QRFPOK                                                  
         BE    *+14                                                             
         MVC   FERN,=AL2(ISYMBEQU)      INVALID SYMBOLIC EQUATE                 
         B     ACTENXIT                                                         
*                                                                               
         CLC   QRFPDICT,=Y(E#ENDA)      YYMMDD                                  
         BE    ACTE20                                                           
*                                                                               
         CLC   QRFPDICT,=Y(E#TODAY)     YYMMDD                                  
         BE    *+14                                                             
         MVC   FERN,=AL2(ISYMBFLD)      INVALID SYMBOLIC EQUATE                 
         B     ACTENXIT                                                         
*                                                                               
ACTE20   CLC   ACQPROG,=C'77'           FOR AC77 DATE IS IN QEND                
         BNE   ACTE30                                                           
         MVC   ACQEND,SPACES            STORE ESCAPE SEQ IN REQCARD             
         MVC   ACQEND(L'QRFPESC),QRFPESC                                        
         MVI   ACQEND+3,L'ACQEND                                                
         B     ACTE40                                                           
*                                                                               
ACTE30   MVC   ACQACTND,SPACES                                                  
         MVC   ACQACTND(L'QRFPESC),QRFPESC                                      
         MVI   ACQACTND+3,L'ACQACTND                                            
ACTE40   MVC   FERN,=AL2(FF)                                                    
*                                                                               
ACTENXIT XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*              METHOD VALIDATION                                      *         
***********************************************************************         
*                                                                               
METHVAL  NTR1  BASE=*,LABEL=*                                                   
         GOTO1 AINITV                                                           
         BNE   METHX               ALL OR MISSING                               
         CLI   IFLDH+5,L'CAHKMTHD                                               
         BH    METH100                                                          
*                                                                               
         USING CAHRECD,R6                                                       
         LA    R6,KEY                                                           
         MVC   CAHKEY,SPACES       READ FOR METHOD BY NUMBER                    
         MVI   CAHKTYP,CAHKTYPQ    X'3E'                                        
         MVI   CAHKSUB,CAHKSUBQ    X'01'                                        
         MVC   CAHKCPY,ACQCPY                                                   
         MVC   CAHKMTHD,IFLD                                                    
         XC    CAHKOFC,CAHKOFC                                                  
         L     R6,AIO1                                                          
         GOTO1 DATAMGR,DMCB,(0,DMREAD),ACCOUNT,KEY,(R6)                         
         CLI   DMCB+8,0                                                         
         BNE   METH100             METHOD NOT FOUND - TRY BY NAME               
         OI    FIND,FIVAL          SET VALID ENTRY BIT                          
         MVC   ACQMTHD,IFLD                                                     
         B     METHX                                                            
*                                                                               
         USING CMTRECD,R6                                                       
METH100  LA    R6,KEY                                                           
         MVC   CMTKEY,SPACES       READ FOR METHOD BY NAME                      
         MVI   CMTKTYP,CMTKTYPQ    X'3E'                                        
         MVI   CMTKSUB,CMTKSUBQ    X'02'                                        
         MVC   CMTKCPY,ACQCPY                                                   
         MVC   CMTKMTHD,IFLD                                                    
         OC    CMTKMTHD,SPACES                                                  
         L     R6,AIO1                                                          
         GOTO1 DATAMGR,DMCB,(0,DMREAD),ACCOUNT,KEY,(R6)                         
         CLI   DMCB+8,0                                                         
         BNE   METHNFD             METHOD NOT FOUND                             
         MVI   ELCODE,METELQ       X'82' ELEMENT                                
         BAS   RE,GETEL                                                         
         BNE   METHNFD                                                          
         USING METELD,R6                                                        
         MVC   ACQMTHD,METNUM                                                   
         OI    FIND,FIVAL          SET VALID ENTRY BIT                          
         B     METHX                                                            
*                                                                               
METHNFD  MVC   FERN,=AL2(NTONFILE) NOT FOUND                                    
*                                                                               
METHX    XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*              DATA TYPES                                             *         
***********************************************************************         
*                                                                               
DTSVAL   NTR1  BASE=*,LABEL-*                                                   
         MVI   BCSTAT1,0                                                        
         LA    R7,ACQSRTAR         PUT IN ACQSRTAR                              
         XC    ACQSRTAR,ACQSRTAR   INIT TO BINARY ZEROS                         
         MVI   KEYWORD,C'Y'        SET TO ALLOW KEY WORD INPUT                  
         J     DTSVALL                                                          
*                                                                               
DTSVAL2  NTR1                                                                   
         MVI   BCSTAT1,0                                                        
         CLC   ACQPROG,=C'FI'                                                   
         JE    DTSVAL2A                                                         
         CLC   ACQPROG,=C'M2'                                                   
         JE    DTSVAL2A                                                         
         CLC   ACQPROG,=C'IV'                                                   
         JNE   *+8                                                              
DTSVAL2A MVI   BCSTAT1,BCST1ADJ                                                 
         LA    R7,ACQAPPL          PUT IN ACQAPPL                               
         XC    ACQAPPL,ACQAPPL                                                  
         MVI   KEYWORD,C'N'        SET TO NOT ALLOW KEY WORD INPUT              
*                                                                               
DTSVALL  DS    0H                                                               
         GOTO1 AINITV                                                           
         JL    EXIT                                                             
         MVI   HALF2,0             NO KEY WORD INPUT                            
*                                                                               
         LA    R5,BLOCK                                                         
         XC    BLOCK,BLOCK                                                      
         GOTO1 SCANNER,DMCB,(0,IFLDH),(7,BLOCK),C',/'                           
         CLI   DMCB+4,7                                                         
         JH    DTSVE                                                            
         SR    R2,R2                                                            
         IC    R2,DMCB+4           R2 = #TIMES TO LOOP                          
*                                                                               
DTSV5    CLI   0(R5),0             NO INPUT                                     
         JNH   DTSV30                                                           
         CLI   0(R5),10                                                         
         JH    DTSVE                                                            
*                                                                               
         CLI   KEYWORD,C'Y'        IF KEY WORD INPUT ALLOWED                    
         JNE   DTSV20                                                           
         CLI   0(R5),3                                                          
         JL    DTSV20              SKIP TO READ LOGIC                           
         CLI   0(R5),7                                                          
         JH    DTSV20              SKIP TO READ LOGIC                           
*                                                                               
         USING FWRDD,R4                                                         
*        L     R4,=A(FWRDTAB)      SEARCH FIXED WORD TABLE                      
         LA    R4,FWRDTAB          SEARCH FIXED WORD TABLE                      
         A     R4,RELO                                                          
DTSV10   CLC   FWRDKEY,12(R5)                                                   
         JNE   DTSV15                                                           
         MVC   0(1,R7),FWRDNUM     SAVE BINARY NUMBER FROM TABLE                
         CLI   FWRDNUM,250         SEE IF ACT                                   
         JE    *+8                                                              
         OI    HALF2,X'01'         KEY WORD INPUT                               
         LA    R7,1(R7)            REQUIRE ACT + BUDGET OR 2 BUDGETS            
         J     DTSV30                                                           
DTSV15   LA    R4,FWRDQ(R4)                                                     
         CLI   0(R4),X'FF'         END OF TABLE                                 
         JNE   DTSV10                                                           
*                                                                               
DTSV20   XC    KEY,KEY                                                          
         MVI   KEY,X'1B'                                                        
         MVC   KEY+1(1),ACQCPY                                                  
         MVC   KEY+5(10),12(R5)                                                 
         L     R6,AIO1                                                          
         GOTO1 DATAMGR,DMCB,(0,DMRDHI),ACCOUNT,KEY,(R6)                         
         CLC   KEY(15),0(R6)                                                    
         JNE   DTSVNFD             BUDGET TYPE NOT FOUND                        
         MVC   0(1,R7),16(R6)      STORE BINARY NUMBER IN ACQSRTAR              
         TM    BCSTAT1,BCST1ADJ                                                 
         JNO   DTSV25                                                           
         SR    R1,R1               FOR APG REPORTS NECESSARY TO                 
         IC    R1,0(R7)            ADJUST BUDGET NUMBER BY 48                   
         AHI   R1,48               SO DOESNT GET MISTAKEN AS AN ESCAPE          
         STC   R1,0(R7)            SEQUENCE IN $RFP                             
DTSV25   LA    R7,1(R7)                                                         
*                                                                               
DTSV30   LA    R5,32(R5)           NEXT ENTRY IN BLOCK                          
         BCT   R2,DTSV5                                                         
*                                                                               
         CLI   HALF2,0             SEE IF KEY WORD REQUESTED                    
         JE    DTSV40              NO                                           
*                                  YES REQUIRE ACT +BUDGET OR 2 BUDGETS         
         LA    R2,7                FOR BCT                                      
         LA    R7,ACQSRTAR                                                      
         SR    R5,R5                                                            
DTSV33   CLI   0(R7),0                                                          
         JE    DTSV35                                                           
         CLI   0(R7),250                                                        
         JH    DTSV34                                                           
         LA    R5,1(R5)            ACT OR BUDGET TYPE                           
         J     DTSV35                                                           
DTSV34   CLI   0(R7),254                                                        
         JL    *+8                                                              
         LA    R5,1(R5)            BUDGET TYPE                                  
DTSV35   LA    R7,1(R7)                                                         
         BCT   R2,DTSV33                                                        
         C     R5,=F'2'                                                         
         JL    DTSVE               INVALID                                      
DTSV40   OI    FIND,FIVAL+FIINP    SET VALID ENTRIES BIT AND INPUT              
         MVC   FERN,=AL2(FF)                                                    
         J     DTSVX                                                            
*                                                                               
DTSVE    MVC   FERN,=AL2(INVINPT)  FLD INVALID                                  
         J     DTSVX                                                            
DTSVNFD  MVC   FERN,=AL2(NTONFILE) NOT FOUND                                    
DTSVX    J     EXIT                                                             
         SPACE 3                                                                
*                                                                               
*              FIXED WORD TABLE FOR DTSVAL                                      
*                                                                               
FWRDTAB  DS    0C                                                               
         DC    C'ACT    ',AL1(250)                                              
         DC    C'VAR    ',AL1(251)                                              
         DC    C'PER    ',AL1(252)                                              
         DC    C'INDEX  ',AL1(253)                                              
         DC    C'BALANCE',AL1(254)                                              
         DC    X'FF'                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
PROFKD   DSECT                                                                  
PROFKEY  DS    0CL16                                                            
PROFKSYS DS    CL1                                                              
PROFKPGM DS    CL3                                                              
         DS    CL1                                                              
PROFKUNL DS    CL2                                                              
PROFKACC DS    CL3                                                              
PROFKAST DS    CL1                                                              
PROFKOFF DS    CL1                                                              
PROFKAGY DS    CL2                                                              
PROFKOFC DS    CL2                 NEW OFFICE                                   
***********************************************************************         
*              FWRD DSECT                                             *         
***********************************************************************         
*                                                                               
FWRDD    DSECT                                                                  
FWRDKEY  DS    CL7                 KEYWORD                                      
FWRDNUM  DS    XL1                 BUDGET NUMBER                                
FWRDQ    EQU   *-FWRDD                                                          
         EJECT                                                                  
***********************************************************************         
*              WORKING STORAGE                                        *         
***********************************************************************         
*                                                                               
LWS      DSECT                                                                  
RELO     DS    F                                                                
BCSTAT1  DS    XL1                                                              
BCST1ADJ EQU   X'80'               ADJUST BUDGET #                              
SVKEY    DS    CL42                                                             
KEYWORD  DS    XL1                                                              
WORK     DS    CL60                WORK AREA FOR MEDIA MOS                      
USETOLR  DS    C                   USE TOLERANCE                                
TOLINTG  DS    XL1                 TOLERANCE - INTEGER VALUE                    
TOLDECI  DS    XL1                 TOLERANCE - DECIMAL VALUE                    
BLOCK    DS    XL255                                                            
LWSX     EQU   *                                                                
                                                                                
       ++INCLUDE ACREQWORK                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'058ACREQ04   02/17/21'                                      
         END                                                                    
