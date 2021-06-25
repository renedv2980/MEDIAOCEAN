*          DATA SET PEREQ03    AT LEVEL 003 AS OF 05/01/02                      
*PHASE TE0403A                                                                  
         TITLE 'PEREQ03 - REQUEST - VALIDATE DATA FIELDS - PART 1'              
         PRINT NOGEN                                                            
TE0403   CSECT                                                                  
         NMOD1 040,**RQ03**,RR=R7                                               
         L     R9,0(R1)                                                         
         USING REQTEMP,R9          R9=A(W/S)                                    
         L     R3,ASAVE                                                         
         USING REQSAVE,R3          R3=A(TWA)                                    
         ST    R7,RELO                                                          
         SPACE 2                                                                
         L     R1,FLDHADR          R1=A(FLD HDR TO BE VALIDATED)                
         SR    RF,RF                                                            
         IC    RF,ROUTNUM          RF=ROUTINE NUM REQUIRED                      
         SLL   RF,2                                                             
         L     RF,ROUTADRT(RF)                                                  
         A     RF,RELO             RF=A(VALIDATION ROUTINE)                     
         BASR  RE,RF                                                            
         SPACE 2                                                                
EXIT     XMOD1 1                                                                
         EJECT                                                                  
FTPVAL   NTR1                      POSTCODE/TOWN FILT - 04=X                    
         GOTO1 AINITV                                                           
         CLI   FIND,1                                                           
         BL    FTPVO               FIELD MISSING                                
         CLI   IFLDH+5,12                                                       
         BH    FTPVE                                                            
         MVC   RFILTVTP,IFLD                                                    
         OI    FIND,X'04'          FIELD VALID                                  
         B     FTPVO                                                            
*                                                                               
FTPVE    MVI   FERN,02             INVALID                                      
FTPVO    EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
         SPACE 3                                                                
FTPTVAL  NTR1                      POSTCODE/TOWN TYPE - 04=X                    
         GOTO1 AINITV                                                           
         CLI   FIND,1                                                           
         BL    FTPTVO              FIELD MISSING                                
         CLI   IFLDH+5,1                                                        
         BH    FTPTVE                                                           
         CLI   IFLD,C'P'                                                        
         BE    FTPTOK                                                           
         CLI   IFLD,C'T'                                                        
         BNE   FTPTVE                                                           
FTPTOK   MVC   RFILTCD,IFLD                                                     
         OI    FIND,X'04'          FIELD VALID                                  
         B     FTPTVO                                                           
*                                                                               
FTPTVE   MVI   FERN,02             INVALID                                      
FTPTVO   EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
STRDVAL  NTR1                      START DATE - 04=YYMMDD 08=YYMM               
         GOTO1 AINITV                                                           
         CLI   FIND,1                                                           
         BL    STRDVO              START DATE MISSING                           
         BH    STRDVE                                                           
         CLI   IFLDH+5,9                                                        
         BH    STRDVE                                                           
*                                                                               
         GOTO1 DATVAL,PLIST,(0,IFLD),RSTRD                                      
         OC    PLIST(4),PLIST                                                   
         BE    STRDV1                                                           
         OI    FIND,X'04'          START DATE = YYMMDD                          
         B     STRDVO                                                           
*                                                                               
STRDV1   GOTO1 (RF),(R1),(2,IFLD),RSTRD                                         
         OC    PLIST(4),PLIST                                                   
         BE    STRDVE                                                           
         MVC   RSTRD+4(2),=C'  '                                                
         OI    FIND,X'08'          START DATE = YYMM                            
         B     STRDVO                                                           
*                                                                               
STRDVE   MVI   FERN,28             START DATE INVALID                           
STRDVO   EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
ENDDVAL  NTR1                      END DATE - 04=YYMMDD 08=YYMM                 
         GOTO1 AINITV                                                           
         CLI   FIND,1                                                           
         BL    ENDDVO              END DATE MISSING                             
         BH    ENDDVE                                                           
         CLI   IFLDH+5,9                                                        
         BH    ENDDVE                                                           
*                                                                               
         GOTO1 DATVAL,PLIST,(0,IFLD),RENDD                                      
         OC    PLIST(4),PLIST                                                   
         BE    ENDDV1                                                           
         OI    FIND,X'04'          END DATE = YYMMDD                            
         B     ENDDV2                                                           
*                                                                               
ENDDV1   GOTO1 (RF),(R1),(2,IFLD),RENDD                                         
         OC    PLIST(4),PLIST                                                   
         BE    ENDDVE                                                           
         MVC   RENDD+4(2),=C'  '                                                
         OI    FIND,X'08'          END DATE = YYMM                              
*                                                                               
ENDDV2   CLI   RSTRD,C' '                                                       
         BE    ENDDVO                                                           
         CLC   RSTRD,RENDD         CHECK START LE END                           
         BNH   ENDDVO                                                           
         MVI   FERN,45             END DATE INCOMPATIBLE                        
         B     ENDDVE+4                                                         
*                                                                               
ENDDVE   MVI   FERN,28             END DATE INVALID                             
         NI    FIND,1                                                           
ENDDVO   EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
TYPDVAL  NTR1                      DATE TYPE - 04=X                             
         GOTO1 AINITV                                                           
         CLI   FIND,1                                                           
         BL    TYPDVO              DATE TYPE MISSING                            
         ZIC   R1,IFLDH+5                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         BE    TYPD1                                                            
         CLC   IFLD(0),=CL8'ADD'                                                
         EX    R1,*+8                                                           
         BE    TYPD1                                                            
         CLC   IFLD(0),=CL8'BORN'                                               
         EX    R1,*+8                                                           
         BE    TYPD1                                                            
         CLC   IFLD(0),=CL8'CHANGE'                                             
         EX    R1,*+8                                                           
         BE    TYPD1                                                            
         CLC   IFLD(0),=CL8'DUE'                                                
         EX    R1,*+8                                                           
         BE    TYPD1                                                            
         CLC   IFLD(0),=CL8'JOIN'                                               
         EX    R1,*+8                                                           
         BE    TYPD1                                                            
         CLC   IFLD(0),=CL8'LAPSE'                                              
         EX    R1,*+8                                                           
         BE    TYPD1                                                            
         CLC   IFLD(0),=CL8'RENEW'                                              
         B     TYPDVE                                                           
TYPD1    MVC   RTYPD,IFLD                                                       
         OI    FIND,X'04'          DATE TYPE VALID                              
         B     TYPDVO                                                           
*                                                                               
TYPDVE   MVI   FERN,02             DATE TYPE INVALID                            
TYPDVO   EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
AGESVAL  NTR1                      AGES - 04=NN-NN                              
         GOTO1 AINITV                                                           
         CLI   FIND,1                                                           
         BL    AGESVO              AGES MISSING                                 
         CLI   IFLDH+5,2                                                        
         BL    AGESVE                                                           
         BE    AGES1               NN VALID                                     
         CLI   IFLD+2,C'-'                                                      
         BNE   AGESVE                                                           
         CLI   IFLDH+5,5           NN-NN VALID                                  
         BNE   AGESVE                                                           
AGES1    MVC   RSTRAGE,IFLD                                                     
         MVC   RENDAGE,IFLD+3                                                   
         CLC   RENDAGE,=C'  '      SET END TO START IF MISSING                  
         BNE   *+10                                                             
         MVC   RENDAGE,RSTRAGE                                                  
         CLC   RENDAGE,RSTRAGE     END CANT BE LT START                         
         BL    AGESVE                                                           
         OI    FIND,X'04'          AGES VALID                                   
         B     AGESVO                                                           
*                                                                               
AGESVE   MVI   FERN,02             AGES INVALID                                 
AGESVO   EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
SEXCVAL  NTR1                      SEX CODE - 04=M/F                            
         GOTO1 AINITV                                                           
         CLI   FIND,1                                                           
         BL    SEXCVO              SEX CODE MISSING                             
         ZIC   R1,IFLDH+5                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         BE    SEXC1                                                            
         CLC   IFLD(0),=CL8'MALE'                                               
         EX    R1,*+8                                                           
         BE    SEXC1                                                            
         CLC   IFLD(0),=CL8'FEMALE'                                             
         B     SEXCVE                                                           
SEXC1    MVC   RSEX,IFLD                                                        
         OI    FIND,X'04'          SEX CODE VALID                               
         B     SEXCVO                                                           
*                                                                               
SEXCVE   MVI   FERN,02             SEX CODE INVALID                             
SEXCVO   EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
CNTRVAL  NTR1                      COUNTRY - 04=XXX                             
         GOTO1 AINITV                                                           
         CLI   FIND,1                                                           
         BL    CNTRVO              COUNTRY MISSING                              
         LA    R4,FILREC                                                        
         MVI   0(R4),C'1'                                                       
         XC    1(24,R4),1(R4)                                                   
         ZIC   R2,IFLDH+5          GET LENGTH TO SCAN                           
         BCTR  R2,0                                                             
         XC    HALF(2),HALF        SET DEFAULT CODE                             
         EX    R2,*+4              MOVE DATA SUPPLIED TO KEY                    
         MVC   1(0,R4),IFLD                                                     
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI  '),=C'CTFILE  ',(R4),(R4)             
         TM    DMCB+8,X'FD'                                                     
         BZ    CNTRYFND                                                         
         TM    DMCB+8,X'90'                                                     
         BNZ   CNTRNFND            NO RECORD FOUND                              
         DC    H'0'                DIE IF ANY I/O ERROR                         
CNTRYFND CLI   0(R4),C'1'          RIGHT RECORD TYPE ?                          
         BNE   CNTRNFND            NO - NOT FOUND                               
         EX    R2,*+8              DOES THE KEY MATCH ?                         
         B     *+10                                                             
         CLC   1(0,R4),IFLD                                                     
         BNE   CNTRNFND            NO - EXIT CC NEQ                             
         MVC   HALF,23(R4)         COPY COUNTRY CODE RETURNED                   
         IC    R2,HALF                                                          
         LA    R2,C'A'-X'A'(,R2)   MAKE X'A-F' INTO C'A-F'                      
         CLI   HALF,15                                                          
         BNH   *+6                                                              
         DC    H'0'                                                             
         CLI   HALF,9                                                           
         BH    *+8                                                              
         LA    R2,C'0'-(C'A'-X'A')(,R2)   MAKE X'0-9' INTO C'0-9'               
         STC   R2,REGION                                                        
         IC    R2,HALF+1                                                        
         CLI   HALF+1,99                                                        
         BNH   *+6                                                              
         DC    H'0'                                                             
         CVD   R2,DUB                                                           
         UNPK  RCNTRY,DUB+6(2)                                                  
         OI    RCNTRY+1,X'F0'                                                   
         OI    FIND,X'04'          COUNTRY VALID                                
         B     CNTRVO                                                           
*                                                                               
CNTRNFND MVI   FERN,04             COUNTRY INVALID                              
CNTRVO   EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
STATVAL  NTR1                      STATUS - 04=A/D/L                            
         GOTO1 AINITV                                                           
         CLI   FIND,1                                                           
         BL    STATVO              STATUS MISSING                               
         ZIC   R2,IFLDH+5                                                       
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         BE    STAT1                                                            
         CLC   IFLD(0),=CL8'ACTIVE'  ACTIVE (DEFAULT)                           
         EX    R2,*+8                                                           
         BE    STAT1                                                            
         CLC   IFLD(0),=CL8'LAPSED'  LAPSED                                     
         EX    R2,*+8                                                           
         BE    STAT1                                                            
         CLC   IFLD(0),=CL8'BOTH'  ACTIVE AND LAPSED                            
         B     STATVE                                                           
STAT1    MVC   RSTATUS,IFLD        MOVE FIRST BYTE                              
         OI    FIND,X'04'          STATUS VALID                                 
         B     STATVO                                                           
*                                                                               
STATVE   MVI   FERN,02             STATUS INVALID                               
STATVO   EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
SPFVAL1  NTR1                      SPECIAL VALIDATE ROUTINE                     
         LA    R7,RSTAF1                                                        
         B     SPFVAL                                                           
SPFVAL2  NTR1                      SPECIAL VALIDATE ROUTINE                     
         LA    R7,RSTAF2                                                        
         B     SPFVAL                                                           
SPFVAL3  NTR1                      SPECIAL VALIDATE ROUTINE                     
         LA    R7,RSTAF3                                                        
         B     SPFVAL                                                           
SPFVAL4  NTR1                      SPECIAL VALIDATE ROUTINE                     
         LA    R7,RSTAF4                                                        
SPFVAL   GOTO1 AINITV                                                           
         CLI   FIND,1                                                           
         BL    SPFVO              FIELD MISSING                                 
         CLI   IFLDH+5,1                                                        
         BH    SPFVE                                                            
         CLI   IFLD,C'Y'                                                        
         BE    SPFOK                                                            
         CLI   IFLD,C'N'                                                        
         BNE   SPFVE                                                            
SPFOK    MVC   0(1,R7),IFLD                                                     
         OI    FIND,X'04'          FIELD VALID                                  
         B     SPFVO                                                            
*                                                                               
SPFVE    MVI   FERN,02             INVALID                                      
SPFVO    EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
OPT1VAL  NTR1                      OPTION N - 04=X                              
         LA    R7,OPT1VTBL                                                      
         LA    RA,ROPT1                                                         
         B     OPTNVAL                                                          
OPT2VAL  NTR1                                                                   
         LA    R7,OPT2VTBL                                                      
         LA    RA,ROPT2                                                         
         B     OPTNVAL                                                          
OPT3VAL  NTR1                                                                   
         LA    R7,OPT3VTBL                                                      
         LA    RA,ROPT3                                                         
         B     OPTNVAL                                                          
OPT4VAL  NTR1                                                                   
         LA    R7,OPT4VTBL                                                      
         LA    RA,ROPT4                                                         
         B     OPTNVAL                                                          
OPT5VAL  NTR1                                                                   
         LA    R7,OPT5VTBL                                                      
         LA    RA,ROPT5                                                         
         B     OPTNVAL                                                          
OPT6VAL  NTR1                                                                   
         LA    R7,OPT6VTBL                                                      
         LA    RA,ROPT6                                                         
         B     OPTNVAL                                                          
OPT7VAL  NTR1                                                                   
         LA    R7,OPT7VTBL                                                      
         LA    RA,ROPT7                                                         
         B     OPTNVAL                                                          
*                                                                               
OPTNVAL  GOTO1 AINITV                                                           
         CLI   FIND,1                                                           
         BL    OPTNVO              OPTION MISSING                               
         CLI   IFLDH+5,1                                                        
         BH    OPTNVE                                                           
         SR    R8,R8                                                            
OPTNV1   IC    R8,0(R7)            FIND REQ NUM ENTRY                           
         LTR   R8,R8                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLC   1(1,R7),REQNUM                                                   
         BE    OPTNV2                                                           
         AR    R7,R8                                                            
         B     OPTNV1                                                           
OPTNV2   BCTR  R8,0                                                             
         BCTR  R8,0                                                             
OPTNV3   CLI   2(R7),255           ANY VALUE                                    
         BE    OPTNV4                                                           
         CLC   2(1,R7),IFLD        CHECK LIST OF VALUES FOR REQ NUM             
         BE    OPTNV4                                                           
         LA    R7,1(R7)                                                         
         BCT   R8,OPTNV3                                                        
         B     OPTNVE                                                           
OPTNV4   MVC   0(1,RA),IFLD                                                     
         OI    FIND,X'04'          OPTION = X                                   
         B     OPTNVO                                                           
OPTNVE   MVI   FERN,2              OPTION INVALID                               
OPTNVO   EQU   *                                                                
         XIT1                                                                   
*                                                                               
*        OPTION TABLE ENTRY  XL1=LENGTH , XL1=REQNUM , XLN=VALUES               
*                                                                               
OPT1VTBL DS    0C                                                               
         DC    AL1(03,00),X'FF'                                                 
         DC    AL1(08,10),C'123456'                                             
         DC    AL1(04,20),C'CR'                                                 
         DC    AL1(04,30),C'12'                                                 
         DC    AL1(03,40),C'Y'                                                  
OPT1VTX  DC    AL1(0)                                                           
*                                                                               
OPT2VTBL DS    0C                                                               
         DC    AL1(03,00),X'FF'                                                 
         DC    AL1(08,10),C'123456'                                             
         DC    AL1(03,20),C'C'                                                  
         DC    AL1(09,30),C'ABCDEFG'                                            
OPT2VTX  DC    AL1(0)                                                           
*                                                                               
OPT3VTBL DS    0C                                                               
         DC    AL1(03,00),X'FF'                                                 
         DC    AL1(08,10),C'123456'                                             
         DC    AL1(03,20),C'T'                                                  
OPT3VTX  DC    AL1(0)                                                           
*                                                                               
OPT4VTBL DS    0C                                                               
         DC    AL1(03,00),X'FF'                                                 
         DC    AL1(03,10),C'Y'                                                  
         DC    AL1(04,20),C'PN'                                                 
OPT4VTX  DC    AL1(0)                                                           
*                                                                               
OPT5VTBL DS    0C                                                               
         DC    AL1(03,00),X'FF'                                                 
OPT5VTX  DC    AL1(0)                                                           
*                                                                               
OPT6VTBL DS    0C                                                               
         DC    AL1(03,00),X'FF'                                                 
OPT6VTX  DC    AL1(0)                                                           
*                                                                               
OPT7VTBL DS    0C                                                               
         DC    AL1(03,00),X'FF'                                                 
OPT7VTX  DC    AL1(0)                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*        THIS TABLE CONTAINS THE ADDRESSES OF THE VALIDATION ROUTINES           
*        CONTAINED IN THIS PHASE. INDEXED BY ROUTNUM.                           
*                                                                               
ROUTADRT DC    A(0)                00 - N/D                                     
         DC    A(0)                01 - N/D                                     
         DC    A(0)                02 - N/D                                     
         DC    A(0)                03 - N/D                                     
         DC    A(0)                04 - N/D                                     
         DC    A(0)                05 - N/D                                     
         DC    A(0)                06 - N/D                                     
         DC    A(0)                07 - N/D                                     
         DC    A(0)                08 - N/D                                     
         DC    A(0)                09 - N/D                                     
         DC    A(0)                10 - N/D                                     
         DC    A(0)                11 - N/D                                     
         DC    A(FTPVAL)           12 - TOWN/POSTCODE FILTER                    
         DC    A(FTPTVAL)          13 - TOWN/POSTCODE TYPE (T OR P)             
         DC    A(STRDVAL)          14 - START DATE                              
         DC    A(ENDDVAL)          15 - END DATE                                
         DC    A(TYPDVAL)          16 - DATE TYPE                               
         DC    A(AGESVAL)          17 - AGE RANGE                               
         DC    A(SEXCVAL)          18 - SEX                                     
         DC    A(CNTRVAL)          19 - COUNTRY                                 
         DC    A(STATVAL)          20 - STATUS                                  
         DC    A(0)                21 - N/D                                     
         DC    A(0)                22 - N/D                                     
         DC    A(0)                23 - N/D                                     
         DC    A(0)                24 - N/D                                     
         DC    A(0)                25 - N/D                                     
         DC    A(0)                26 - N/D                                     
         DC    A(0)                27 - N/D                                     
         DC    A(0)                28 - N/D                                     
         DC    A(0)                29 - N/D                                     
         DC    A(0)                30 - N/D                                     
         DC    A(0)                31 - N/D                                     
         DC    A(0)                32 - N/D                                     
         DC    A(OPT1VAL)          33 - OPTION#1                                
         DC    A(OPT2VAL)          34 - OPTION#2                                
         DC    A(OPT3VAL)          35 - OPTION#3                                
         DC    A(OPT4VAL)          36 - OPTION#4                                
         DC    A(OPT5VAL)          37 - OPTION#5                                
         DC    A(OPT6VAL)          38 - OPTION#6                                
         DC    A(OPT7VAL)          39 - OPTION#7                                
         DC    A(0)                40 - N/D                                     
         DC    A(0)                41 - N/D                                     
         DC    A(0)                42 - N/D                                     
         DC    A(0)                43 - N/D                                     
         DC    A(0)                44 - N/D                                     
         DC    A(0)                45 - N/D                                     
         DC    A(0)                46 - N/D                                     
         DC    A(0)                47 - N/D                                     
         DC    A(0)                48 - N/D                                     
         DC    A(SPFVAL1)          49 - SPECIAL#1                               
         DC    A(SPFVAL2)          50 - SPECIAL#2                               
         DC    A(SPFVAL3)          51 - SPECIAL#3                               
         DC    A(SPFVAL4)          52 - SPECIAL#4                               
         DC    A(0)                53 - N/D                                     
         EJECT                                                                  
*PEREQSAVE                                                                      
       ++INCLUDE PEREQSAVE                                                      
*PEREQTEMP                                                                      
       ++INCLUDE PEREQTEMP                                                      
*PEGENFILE                                                                      
       ++INCLUDE PEGENFILE                                                      
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003PEREQ03   05/01/02'                                      
         END                                                                    
