*          DATA SET ACBAT07    AT LEVEL 026 AS OF 07/10/18                      
*PHASE T61B07A                                                                  
         TITLE 'OVERLAY TO HANDLE ORDER RECORDS'                                
T61B07   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 PROGDX-PROGD,**ORDS**,R8                                         
         USING TWAD,RA             RA=TWA                                       
         L     R9,4(R1)            PASSED BY ROOT                               
         USING GWS,R9              COVER GLOBAL WORK STORAGE                    
         USING PROGD,RC            RC COVERS LOCAL STORAGE                      
         MVC   ADLIST,8(R1)        STORE ADDRESS OF WORKCODE TABLE              
         GOTO1 DATCON,DMCB,(5,TODAYP),(1,TODAYP)                                
         MVI   ERRNUM,X'FF'                                                     
*   BUILD SCREEN DIRECTORY OF FIELDS THAT ARE UTILIZED IN THE OVERLAY           
*   USE DIFFERENT SCREEN FOR US AND CANADA                                      
*                                                                               
         LA    R1,USTAB47                                                       
         CLI   INPUT,47            IS THIS TYPE 47 ?                            
         BE    BUILDIT             YES, BUILD SCREEN FIELDS                     
         LA    R1,USTAB01          NO, TRY TYPE 1                               
         CLI   AGYCTRY,CTRYUSA     IS THIS A US AGENCY ?                        
         BNE   BUILD01             NO                                           
         CLI   INPUT,X'01'         YES, IS THIS TYPE 1 ?                        
         BE    BUILDIT             YES                                          
         LA    R1,USTAB03          NO, MUST BE TYPE 03                          
         B     BUILDIT                                                          
*                                                                               
BUILD01  LA    R1,CANTAB01                                                      
         CLI   INPUT,01            IS THIS TYPE 01 ?                            
         BE    BUILDIT             YES                                          
         LA    R1,CANTAB03         NO, MUST BE TYPE 03                          
*                                                                               
BUILDIT  CLI   0(R1),X'FF'         END OF TABLE                                 
         BE    ORD0                YES, CONTINUE ON                             
         LM    RE,RF,0(R1)                                                      
         LA    RE,TWAD(RE)         FORM READ ADDRESS OF SCREEN FIELD            
         LA    RF,PROGD(RF)        FORM ADDRESS OF ADCON                        
         ST    RE,0(RF)                                                         
         LA    R1,L'USTAB01(R1)                                                 
         B     BUILDIT                                                          
*                                                                               
ORD0     CLI   MODE,0              DISPLAY ORDER RECORD?                        
         BE    DORDR                                                            
         CLI   MODE,1              DELETE OR AMEND ORDER RECORD?                
         BE    ORD100                                                           
         B     EXIT                OR DO NOTHING?                               
         EJECT                                                                  
*---------------------------------------------------------                      
*        READ AND DISPLAY ORDER RECORD                                          
*---------------------------------------------------------                      
*                                                                               
DORDR    DS    0H                                                               
         L     R2,AORDH                                                         
         MVI   ERRNUM,2                                                         
         XC    BCPARTSW,BCPARTSW                                                
*                                                                               
         CLI   5(R2),0                                                          
         BE    DORD02                                                           
*                                                                               
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=6C'0'      CONTROL RECORD IS BANNED                     
         BE    ERROR                                                            
*                                                                               
DORD02   MVI   ERRNUM,X'FF'                                                     
         CLC   8(5,R2),=C'DUMMY'   DUMMY MEANS NO ORDER INPUT                   
         BE    DORD04                                                           
         CLI   5(R2),0                                                          
         BNE   DORD08                                                           
         B     DORD06                                                           
*                                                                               
DORD04   MVC   ORDNO(5),=C'DUMMY'                                               
         MVI   ORDNO+5,C' '                                                     
         ZAP   ORDAMT,=P'0'                                                     
*                                                                               
DORD06   MVI   MODE,0              NO ORDER NO - EXIT                           
         B     EXIT                                                             
*                                                                               
DORD08   MVI   ERRNUM,2                                                         
         XC    WORK,WORK                                                        
         GOTO1 SCANNER,DMCB,(R2),(2,WORK)                                       
         CLI   4(R1),0                                                          
         BE    ERROR               INVALID DATA                                 
*                                                                               
         USING ORDRECD,R6                                                       
         LA    R6,IOKEY                                                         
         LA    RF,WORK                                                          
         XC    ORDKEY,ORDKEY                                                    
         MVI   ORDKTYP,ORDKTYPQ                                                 
         MVC   ORDKCPY,COMPANY                                                  
         MVC   ORDKORD,=6C'0'     ZERO FILL ORDER NUMBER IN KEY                 
         LA    RE,ORDKORD+6                                                     
         ZIC   R1,0(RF)                                                         
         SR    RE,R1                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),12(RF)      AND RIGHT ALIGN IT                           
         LA    RF,32(RF)                                                        
         CLI   0(RF),0                                                          
         BE    DORD10                                                           
         MVI   ERRNUM,2                                                         
         IC    R1,0(RF)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8              CHECK FOR PARTIAL                            
         B     *+10                                                             
         CLC   12(0,RF),=C'PART'                                                
         BNE   ERROR                                                            
         MVI   BCPARTSW,C'Y'                                                    
                                                                                
DORD10   MVC   KEYSAVE(40),SPACES  BUILD BLOCK FOR UNSCAN                       
         MVC   KEYSAVE(6),IOKEY+4                                               
         MVC   KEYSAVE+20(4),WORK+32+12                                         
         LA    R3,1                                                             
         OC    KEYSAVE+20(4),KEYSAVE+20                                         
         BZ    *+8                                                              
         LA    R3,2                                                             
         GOTO1 UNSCAN,DMCB,((R3),KEYSAVE),(R2)                                  
         OI    6(R2),X'80'                                                      
*                                                                               
         GOTO1 AIO,IOHIGH+IOACCMST+IO1                                          
         CLC   IOKEY(L'ORDKEY),IOKEYSAV                                         
         MVI   ERRNUM,ORDMISS                                                   
         CLC   IOKEY(15),KEYSAVE                                                
         BNE   ERROR                                                            
*                                                                               
         MVC   FVMSGNO,=AL2(AE$OFMCH)                                           
         L     R6,AIO1             TEST ORDER FULLY MATCHED                     
         TM    ORDRSTAT,ORDSFMCH                                                
         BNZ   ERROR                                                            
         MVC   FVMSGNO,=AL2(AE$ORDEL) OR DELETED                                
         TM    ORDRSTAT,ORDSDEL+ORDSLDEL                                        
         BNZ   ERROR                                                            
*                                                                               
         MVC   FVMSGNO,=AL2(AE$ORDNF)                                           
         CLI   ORDKSEQ,ORDKEXTN    IS THIS AN EXTENSION?                        
         BE    ERROR               YES, ERROR                                   
         TM    ORDRSTA2,ORDSEXEX   NO, DOES AN EXTENSION EXIST?                 
         BZ    DORD12              NO                                           
         TM    ORDRSTA2,ORDSAPPR   YES, IS IT APPROVED?                         
         BNZ   DORD12              YES                                          
         TM    ORDRSTAT,ORDGDRCV   NO, IS IT GOODS RECEIVED?                    
         BNZ   DORD12              YES                                          
         MVC   FVMSGNO,=AL2(AE$ORTNF)                                           
         B     ERROR               ERROR 'TRANSACTION NOT ON FILE'              
*                                                                               
DORD12   MVC   ORDNO,ORDKORD       ORDER NUMBER TO TWA                          
         MVI   ERRNUM,X'FF'                                                     
*                                                                               
         ZAP   INVS,=P'0'                                                       
         ZAP   ORDSOFA,=P'0'                                                    
         L     R2,AAMTH                                                         
         XC    8(L'INUAMT,R2),8(R2)                                             
         L     R2,ANCAH                                                         
         XC    8(L'INUNCA,R2),8(R2)                                             
         L     R2,AWRKH                                                         
         XC    8(L'INUWRK,R2),8(R2)                                             
         L     R2,ANCWKH                                                        
         XC    8(L'INUNCWK,R2),8(R2)                                            
*                                                                               
         LA    RF,BOELEM                                                        
         ST    RF,BOADDR2                                                       
         LA    R3,ORDRFST                                                       
*                                                                               
DORD18   CLI   0(R3),0             PICK OUT ORDER DATA FOR DISPLAY              
         BE    DORD48                                                           
         CLI   0(R3),ORDELQ        ORDER ELEMENT = X'67'                        
         BE    DORD22                                                           
         CLI   0(R3),OAMELQ        ORDER AMOUNT ELEMENT = X'68'                 
         BE    DORD38                                                           
         CLI   0(R3),SCMELQ        COMMENT ELEMENT = X'3E'                      
         BE    DORD46                                                           
*                                                                               
DORD20   SR    RF,RF                                                            
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     DORD18              BUMP THROUGH RECORD                          
*                                                                               
         USING ORDELD,R3                                                        
DORD22   L     R2,AORDH            ORDER ELEMENT X'67'                          
         MVI   ERRNUM,2                                                         
         CLC   ORDACCU(2),=C'SJ'   DON'T WANT EXPENSE ORDERS                    
         BNE   ERROR                                                            
         MVC   FVMSGNO,=Y(AE$TY10O)                                             
         TM    ORDSTAT,ORDSTY10    TYPE 10 ONLY?                                
         BO    ERROR                                                            
         MVC   FVMSGNO,=AL2(AE$OFMCH)                                           
         TM    ORDSTAT,ORDSMNUP    FULLY MATCHED                                
         BO    ERROR                                                            
*                                                                               
         L     R2,ACLIH                                                         
         ZIC   RF,CLILNGTH         LEVEL A LENGTH                               
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),ORDACCA                                                  
         BE    DORD24                                                           
         L     R2,ACLINH           CLEAR NAME IF DIFFERENT CODES                
         MVC   FLD,SPACES                                                       
         BAS   RE,MOVEFLD                                                       
         OI    6(R2),X'80'                                                      
*                                                                               
         L     R2,APRONH                                                        
         MVC   FLD,SPACES                                                       
         BAS   RE,MOVEFLD                                                       
         OI    6(R2),X'80'                                                      
*                                                                               
         L     R2,AJOBNH                                                        
         MVC   FLD,SPACES                                                       
         BAS   RE,MOVEFLD                                                       
         OI    6(R2),X'80'                                                      
*                                                                               
         L     R2,APROH                                                         
         NI    4(R2),X'DF'                                                      
         L     R2,AJOBH                                                         
         NI    4(R2),X'DF'                                                      
         L     R2,ACLIH                                                         
         NI    4(R2),X'DF'                                                      
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),ORDACCA     CLIENT CODE TO SCREEN                        
*                                                                               
DORD24   LA    R4,ORDACCA+1(RF)    POINT TO START OF PRODUCT                    
         L     R2,APROH                                                         
         ZIC   R1,PRDLNGTH         LEVEL B LENGTH                               
         SR    R1,RF                                                            
         SH    R1,=H'2'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),0(R4)                                                    
         BNE   DORD26                                                           
         L     R2,APRONH                                                        
         OC    8(L'INUPRO,R2),8(R2)                                             
         BNZ   DORD28                                                           
*                                                                               
DORD26   L     R2,APRONH                                                        
         MVC   FLD,SPACES                                                       
         BAS   RE,MOVEFLD                                                       
         OI    6(R2),X'80'                                                      
*                                                                               
         L     R2,AJOBNH                                                        
         MVC   FLD,SPACES                                                       
         BAS   RE,MOVEFLD                                                       
         OI    6(R2),X'80'                                                      
*                                                                               
         L     R2,AJOBH                                                         
         NI    4(R2),X'DF'                                                      
         L     R2,APROH                                                         
         NI    4(R2),X'DF'                                                      
         ZIC   R1,PRDLNGTH         LEVEL B LENGTH                               
         SR    R1,RF                                                            
         SH    R1,=H'2'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),0(R4)       PRODUCT CODE TO SCREEN                       
                                                                                
DORD28   IC    R1,PRDLNGTH                                                      
         LA    R4,ORDACCA(R1)      POINT TO START OF JOB                        
         IC    RF,JOBLNGTH                                                      
         L     R2,AJOBH                                                         
         SR    RF,R1               GIVES LENGTH OF JOB                          
         CH    RF,=H'6'            MAX SCREEN LENGTH IS 6                       
         BNH   *+8                                                              
         LA    RF,6                                                             
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),0(R4)                                                    
         BNE   DORD30                                                           
         L     R2,AJOBNH                                                        
         OC    8(L'INUJOBN,R2),8(R2)                                            
         BNZ   DORD32                                                           
                                                                                
DORD30   L     R2,AJOBNH                                                        
         MVC   FLD,SPACES                                                       
         BAS   RE,MOVEFLD                                                       
         OI    6(R2),X'80'                                                      
         L     R2,AJOBH                                                         
         NI    4(R2),X'DF'                                                      
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),0(R4)       JOB CODE TO SCREEN                           
                                                                                
DORD32   L     R2,ASUPN                                                         
         MVC   WORK(12),8(R2)                                                   
         OC    WORK(12),SPACES                                                  
         CLC   WORK(12),ORDSUPA                                                 
         BE    DORD20                                                           
         L     R4,ASUPH                                                         
         L     R2,ASUPNH                                                        
         MVC   FLD,SPACES                                                       
         BAS   RE,MOVEFLD                                                       
*                                                                               
         USING CPYELD,RF                                                        
         LA    RF,COMPEL                                                        
         MVC   8(L'PROSUP,R4),SPACES                                            
         CLC   ORDSUPU(2),CPYSUPP                                               
         BNE   DORD34                                                           
         MVC   8(12,R4),ORDSUPA                                                 
         B     DORD36                                                           
         DROP  RF                                                               
*                                                                               
DORD34   DS    0H                                                               
         MVI   8(R4),C'*'                                                       
         MVC   9(14,R4),ORDSUPU                                                 
*                                                                               
DORD36   DS    0H                                                               
         NI    4(R4),X'DF'                                                      
         OI    6(R4),X'80'                                                      
         B     DORD20                                                           
*                                                                               
         USING OAMELD,R3                                                        
DORD38   DS    0H                  ORDER AMOUNT ELEMENT = X'68' ELEM            
         TM    OAMSTAT,OAMSXTRA    IS IT AN EXTRA WORKCODE                      
         BNZ   DORD20                                                           
         AP    INVS,OAMINUM        GET NUMBER INVOICES TO DATE                  
         AP    ORDSOFA,OAMAMNT     GET AMOUNT INVOICED TO DATE                  
         L     RF,AWRKH            POINT TO COMMISSIONABLE FIELDS               
         LA    RF,8(RF)                                                         
*                                                                               
         L     RE,AAMTH                                                         
         LA    RE,8(RE)                                                         
         TM    OAMSTAT,OAMSNOCM    BIT=1,MEANS NON-COMMISSIONABLE               
         BZ    *+20                                                             
         L     RF,ANCWKH                                                        
         LA    RF,8(RF)                                                         
*                                                                               
         L     RE,ANCAH                                                         
         LA    RE,8(RE)                                                         
         TM    OAMSTAT,OAMSMAMI    BIT=1,MEANS MINOR FIELD                      
         BZ    DORD40              BRANCH IF MAJOR                              
*                                                                               
         LA    RF,L'INUWRK-1(RF)   GET TO END OF FIELDS                         
         LA    RE,L'INUAMT-1(RE)                                                
*                                                                               
         CLI   0(RF),X'41'         FIND LAST WORKCODE                           
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C','          SEPARATOR                                    
         LA    RF,2(RF)                                                         
*                                                                               
         CLI   0(RE),X'41'         FIND LAST AMOUNT                             
         BH    *+8                                                              
         BCT   RE,*-8                                                           
         MVI   1(RE),C','          SEPARATOR                                    
         LA    RE,2(RE)                                                         
*                                                                               
DORD40   MVC   0(2,RF),OAMWORK     POP WORK-CODE INTO FIELD                     
         ZAP   LEFT,OAMAMNT                                                     
         SP    LEFT,OAMIVAL        GIVES WHAT IS LEFT OF ORIG. ORDER            
         CLC   TODAYP,OAMLAST                                                   
         BNE   DORD42              IF INVOICED TODAY THEN                       
         SP    LEFT,OAMTVAL        SUBTRACT OFF INVOICED TODAY AMOUNT           
*                                                                               
DORD42   L     RF,BCAUTL                                                        
         TM    TSTAT6-UTLD(RF),TST6SCRP                                         
         BO    DORD44                                                           
         EDIT  LEFT,(10,0(RE)),2,MINUS=YES,FLOAT=*,ALIGN=LEFT                   
         B     DORD20                                                           
*                                                                               
DORD44   EDIT  LEFT,(10,0(RE)),2,MINUS=YES,ALIGN=LEFT                           
         B     DORD20                                                           
*                                                                               
         USING SCMELD,R3                                                        
DORD46   TM    ORDRSTA2,ORDSEXEX   ONLY EBUYER GETS NARRATIVE                   
         BZ    DORD20                                                           
         CLI   SCMTYPE,SCMTOMOC    IS THIS ORDER MATCHING?                      
         BNE   DORD20              NO, SKIP IT                                  
*                                                                               
         L     RE,BOADDR2                                                       
*                                                                               
         SR    R1,R1                                                            
         IC    R1,SCMLN                                                         
         SHI   R1,SCMLN1Q+1        GET LENGTH OF NARRATIVE                      
*                                                                               
         CHI   R1,98               MAX LENGTH IS 98                             
         BNH   *+8                                                              
         LA    R1,98                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),SCMNARR     MOVE AS MUCH TEXT AS POSSIBLE                
         B     DORD20                                                           
*                                                                               
DORD48   OC    BOELEM,BOELEM                                                    
         BZ    DORD50                                                           
*                                                                               
         L     RE,ANARH                                                         
         LA    RE,8(RE)                                                         
         LA    RF,BOELEM                                                        
         LA    R1,49               MAX CHARACTERS PER LINE                      
         SHI   R1,1                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(RF)                                                    
*                                                                               
         LA    RF,1(R1,RF)                                                      
         L     RE,ANARH                                                         
         SR    R2,R2                                                            
         IC    R2,0(RE)                                                         
         AR    RE,R2                                                            
         LA    RE,8(RE)                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(RF)                                                    
*                                                                               
DORD50   L     R2,ACLIH                                                         
         OI    6(R2),X'80'         OUTPUT THE FIELDS WE HAVE BUILT              
         L     R2,APROH                                                         
         OI    6(R2),X'80'                                                      
         L     R2,AJOBH                                                         
         OI    6(R2),X'80'                                                      
         L     R2,ASUPH                                                         
         OI    6(R2),X'80'                                                      
         L     R2,AOHSTH                                                        
         OI    6(R2),X'80'                                                      
         L     R2,AAMTH                                                         
         OI    6(R2),X'80'                                                      
         L     R2,AWRKH                                                         
         OI    6(R2),X'80'                                                      
         L     R2,ANCWKH                                                        
         OI    6(R2),X'80'                                                      
         L     R2,ANCAH                                                         
         OI    6(R2),X'80'                                                      
         L     R2,ANARH                                                         
         OI    6(R2),X'80'                                                      
         SR    RE,RE                                                            
         IC    RE,0(R2)                                                         
         AR    R2,RE                                                            
         OI    6(R2),X'80'                                                      
         L     R2,AOHSTH                                                        
         LA    RF,18(R2)           DISPLAY TOTALS (+10 INTO HISTORY)            
         EDIT  ORDSOFA,(10,0(RF)),2,MINUS=YES,ALIGN=LEFT                        
         LA    RF,46(R2)           (+38 INTO HISTORY)                           
         EDIT  INVS,(2,0(RF)),ZERO=NOBLANK                                      
*                                                                               
         L     R2,AWCNMH           CLEAR WC NAMES AND TRANSMIT                  
         MVC   FLD,SPACES                                                       
         BAS   RE,MOVEFLD                                                       
         OI    6(R2),X'80'                                                      
*                                                                               
         L     R2,AWNNMH                                                        
         MVC   FLD,SPACES                                                       
         BAS   RE,MOVEFLD                                                       
         OI    6(R2),X'80'                                                      
*                                                                               
         CLI   INPUT,47            TYPE 47 ?                                    
         BNE   DORD52              NO, WE HAVE NO SECOND LINE                   
         L     R2,ANNM4H                                                        
         MVC   FLD,SPACES                                                       
         BAS   RE,MOVEFLD                                                       
         OI    6(R2),X'80'                                                      
*                                                                               
         L     R2,AWNM4H                                                        
         MVC   FLD,SPACES                                                       
         BAS   RE,MOVEFLD                                                       
         OI    6(R2),X'80'                                                      
*                                                                               
DORD52   MVC   FVMSGNO,=AL2(AI$ODDEI)                                           
         MVI   FVOMTYP,C'I'                                                     
         MVI   MODE,1                                                           
         L     R2,ASUPH                                                         
         OI    6(R2),X'80'                                                      
         B     EXIT                EXIT BACK TO 01 PHASE                        
         EJECT                                                                  
*---------------------------------------------------------                      
*        DELETE OR AMEND ORDER RECORDS                                          
*---------------------------------------------------------                      
*                                                                               
ORD100   DS    0H                                                               
         XC    KEY,KEY             BUILD KEY TO READ ORDER RECORD               
         MVC   KEY+1(1),COMPANY                                                 
         MVI   KEY,X'1A'                                                        
         MVC   KEY+4(6),ORDNO                                                   
         MVC   IOKEY,KEY                                                        
         GOTO1 AREADL,AIO1                                                      
         BNE   ERRXIT                                                           
*                                                                               
         DS    0H                                                               
         L     R3,AIO1             PART INVOICE ONLY,SO UPDATE ORDER            
         L     R2,AORDH                                                         
         BAS   RE,UPDT68                                                        
*                                                                               
         GOTO1 AWRITE,AIO1         WRITE ORDER BACK                             
         BNE   ERRXIT                                                           
*                                                                               
         L     R2,AORDH                                                         
         MVI   MODE,0                                                           
         MVI   ERRNUM,X'FF'                                                     
         B     EXIT                EXIT BACK TO 01 PHASE                        
         EJECT                                                                  
*---------------------------------------------------------                      
*        UPDATE THE X'68' ELEMENT                                               
*---------------------------------------------------------                      
*                                                                               
*        THIS ROUTINE UPDATES THE NUMBER OF INVOICES TO DATE                    
*        AND UPDATES THE TOTAL AMOUNT INVOICED                                  
*        EXPECTS R3 TO POINT TO FIRST ELEMENT OF ORDER RECORD                   
*                                                                               
UPDT68   NTR1                                                                   
         LA    R3,L'KEY(R3)        ADDRESS DATA                                 
         USING OAMELD,R3                                                        
         XR    RE,RE                                                            
UPD2     CLI   OAMEL,0                                                          
         BE    EXIT                                                             
         CLI   OAMEL,ORDELQ        UPDATE THE ORDER ELEMENT                     
         BE    UPD6                                                             
         CLI   OAMEL,OAMELQ        UPDATE THE ORDER AMOUNT ELEMENT              
         BE    UPD8                                                             
UPD4     IC    RE,OAMLN                                                         
         AR    R3,RE                                                            
         B     UPD2                                                             
*                                                                               
         USING ORDELD,R3                                                        
UPD6     XC    ORDST2,ORDST2                                                    
         CLI   BCPARTSW,C'Y'       IS IT PARTIAL MATCH                          
         BE    UPD7                YES                                          
         OI    ORDSTAT,ORDSMNUP    NO - MARK ORDER FULLY MATCHED                
         B     *+8                                                              
UPD7     OI    ORDSTAT,ORDSPART    MARK ORDER PART MATCHED                      
         CLI   ORDLN,ORDLN3Q                                                    
         BL    UPD4                                                             
         MVC   ORDST2,ORDSTAT2                                                  
         OI    ORDSTAT2,ORDSSTAT                                                
         B     UPD4                                                             
*                                                                               
         USING OAMELD,R3                                                        
         USING WKLINED,R2                                                       
UPD8     L     R2,ADLIST           GET ADDRESS OF WORKCODE TABLE                
         LA    R5,WKMAXROW         MAX NUMBER OF TIMES TO LOOP                  
         ST    R3,BOFULL1                                                       
*                                                                               
UPD10    CLC   WKCODE,SPACES       IS NEXT WORKCODE VALID                       
         BNH   UPD22               NO - SKIP                                    
         CLC   OAMWORK,WKCODE      COMPARE WORKCODE                             
         BE    UPD20                                                            
         SR    RE,RE                                                            
UPD12    IC    RE,OAMLN                                                         
         AR    R3,RE                                                            
         CLI   OAMEL,OAMELQ        UPDATE THE ORDER AMOUNT ELEMENT              
         BE    UPD10                                                            
         CLI   OAMEL,0             END OF RECORD                                
         BNE   UPD12                                                            
                                                                                
         TM    ORDST2,ORDSEXEX     IF BRANDOCEAN ORDER BUILD EXTRA              
         BZ    UPD22                     ORDER AMOUNT ELEMENTS                  
         LA    R3,BOELEM                                                        
         XC    BOELEM,BOELEM                                                    
         MVI   OAMEL,OAMELQ                                                     
         MVI   OAMLN,OAMLN2Q                                                    
         MVI   OAMSTAT,OAMSXTRA                                                 
         ZAP   OAMAMNT,=P'0'                                                    
         ZAP   OAMINUM,=P'0'                                                    
         ZAP   OAMIVAL,=P'0'                                                    
         ZAP   OAMTVAL,WKAMNT      ADD IN CORRECT AMOUNT TO WORKCODE            
         MVC   OAMLAST,TODAYP      STORE TODAYS DATE                            
         MVI   OAMIPND,1                                                        
         MVC   OAMWORK,WKCODE                                                   
                                                                                
         LA    RF,=CL8'ADD=CODE'                                                
         GOTO1 VHELLO,DMCB,(C'P',=C'ACCOUNT'),AIO1,BOELEM,(RF)                  
         CLI   12(R1),0                                                         
         BE    UPD22                                                            
         DC    H'0'                                                             
*                                                                               
UPD20    AP    OAMTVAL,WKAMNT      ADD IN CORRECT AMOUNT TO WORKCODE            
         MVC   OAMLAST,TODAYP      STORE TODAYS DATE                            
         LLC   RF,OAMIPND          INCREMENT PENDING COUNT                      
         AHI   RF,1                ON THE FIRST ELEMENT ONLY                    
         STC   RF,OAMIPND                                                       
                                                                                
UPD22    LA    R2,WKLINEDL(R2)     BUMP TO NEXT LINE IN TABLE                   
         L     R3,BOFULL1                                                       
         BCT   R5,UPD10                                                         
         B     EXIT                                                             
*                                                                               
*                                                                               
* SUB-ROUTINE TO MOVE OUT DISPLAY DATA                                          
*                                                                               
MOVEFLD  ST    RE,SAVERE                                                        
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
MOVEFLDX L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
USTAB01  DS    0D                                                               
         DC    AL4(INUORDH-TWAD),AL4(AORDH-PROGD)                               
         DC    AL4(INUOHSTH-TWAD),AL4(AOHSTH-PROGD)                             
         DC    AL4(INUDOCH-TWAD),AL4(ADOCH-PROGD)                               
         DC    AL4(INUCLIH-TWAD),AL4(ACLIH-PROGD)                               
         DC    AL4(INUCLINH-TWAD),AL4(ACLINH-PROGD)                             
         DC    AL4(INUPROH-TWAD),AL4(APROH-PROGD)                               
         DC    AL4(INUPRONH-TWAD),AL4(APRONH-PROGD)                             
         DC    AL4(INUJOBH-TWAD),AL4(AJOBH-PROGD)                               
         DC    AL4(INUJOBNH-TWAD),AL4(AJOBNH-PROGD)                             
         DC    AL4(INUSUPH-TWAD),AL4(ASUPH-PROGD)                               
         DC    AL4(INUSUP-TWAD),AL4(ASUP-PROGD)                                 
         DC    AL4(INUSUPNH-TWAD),AL4(ASUPNH-PROGD)                             
         DC    AL4(INUSUPN-TWAD),AL4(ASUPN-PROGD)                               
         DC    AL4(INUWRKH-TWAD),AL4(AWRKH-PROGD)                               
         DC    AL4(INUWCNMH-TWAD),AL4(AWCNMH-PROGD)                             
         DC    AL4(INUAMTH-TWAD),AL4(AAMTH-PROGD)                               
         DC    AL4(INUNCWKH-TWAD),AL4(ANCWKH-PROGD)                             
         DC    AL4(INUWNNMH-TWAD),AL4(AWNNMH-PROGD)                             
         DC    AL4(INUNCAH-TWAD),AL4(ANCAH-PROGD)                               
         DC    AL4(INUNARH-TWAD),AL4(ANARH-PROGD)                               
         DC    X'FF'                                                            
*                                                                               
USTAB03  DS    0D                                                               
         DC    AL4(CHUORDH-TWAD),AL4(AORDH-PROGD)                               
         DC    AL4(CHUOHSTH-TWAD),AL4(AOHSTH-PROGD)                             
         DC    AL4(CHUDOCH-TWAD),AL4(ADOCH-PROGD)                               
         DC    AL4(CHUCLIH-TWAD),AL4(ACLIH-PROGD)                               
         DC    AL4(CHUCLINH-TWAD),AL4(ACLINH-PROGD)                             
         DC    AL4(CHUPROH-TWAD),AL4(APROH-PROGD)                               
         DC    AL4(CHUPRONH-TWAD),AL4(APRONH-PROGD)                             
         DC    AL4(CHUJOBH-TWAD),AL4(AJOBH-PROGD)                               
         DC    AL4(CHUJOBNH-TWAD),AL4(AJOBNH-PROGD)                             
         DC    AL4(CHUSUPH-TWAD),AL4(ASUPH-PROGD)                               
         DC    AL4(CHUSUPNH-TWAD),AL4(ASUPNH-PROGD)                             
         DC    AL4(CHUWRKH-TWAD),AL4(AWRKH-PROGD)                               
         DC    AL4(CHUWCNMH-TWAD),AL4(AWCNMH-PROGD)                             
         DC    AL4(CHUAMTH-TWAD),AL4(AAMTH-PROGD)                               
         DC    AL4(CHUNCWKH-TWAD),AL4(ANCWKH-PROGD)                             
         DC    AL4(CHUWNNMH-TWAD),AL4(AWNNMH-PROGD)                             
         DC    AL4(CHUNCAH-TWAD),AL4(ANCAH-PROGD)                               
         DC    AL4(CHUNARH-TWAD),AL4(ANARH-PROGD)                               
         DC    X'FF'                                                            
*                                                                               
USTAB47  DS    0D                                                               
         DC    AL4(PROORDH-TWAD),AL4(AORDH-PROGD)                               
         DC    AL4(PROOHSTH-TWAD),AL4(AOHSTH-PROGD)                             
         DC    AL4(PRODOCH-TWAD),AL4(ADOCH-PROGD)                               
         DC    AL4(PROCLIH-TWAD),AL4(ACLIH-PROGD)                               
         DC    AL4(PROCLINH-TWAD),AL4(ACLINH-PROGD)                             
         DC    AL4(PROPROH-TWAD),AL4(APROH-PROGD)                               
         DC    AL4(PROPRONH-TWAD),AL4(APRONH-PROGD)                             
         DC    AL4(PROJOBH-TWAD),AL4(AJOBH-PROGD)                               
         DC    AL4(PROJOBNH-TWAD),AL4(AJOBNH-PROGD)                             
         DC    AL4(PROSUPH-TWAD),AL4(ASUPH-PROGD)                               
         DC    AL4(PROSUP-TWAD),AL4(ASUP-PROGD)                                 
         DC    AL4(PROSUPNH-TWAD),AL4(ASUPNH-PROGD)                             
         DC    AL4(PROSUPN-TWAD),AL4(ASUPN-PROGD)                               
         DC    AL4(PROWRKH-TWAD),AL4(AWRKH-PROGD)                               
         DC    AL4(PROWCNMH-TWAD),AL4(AWCNMH-PROGD)                             
         DC    AL4(PROAMTH-TWAD),AL4(AAMTH-PROGD)                               
         DC    AL4(PROWNM4H-TWAD),AL4(AWNM4H-PROGD)                             
         DC    AL4(PRONCWKH-TWAD),AL4(ANCWKH-PROGD)                             
         DC    AL4(PROWNNMH-TWAD),AL4(AWNNMH-PROGD)                             
         DC    AL4(PRONCAH-TWAD),AL4(ANCAH-PROGD)                               
         DC    AL4(PRONNM4H-TWAD),AL4(ANNM4H-PROGD)                             
         DC    AL4(PRONARH-TWAD),AL4(ANARH-PROGD)                               
         DC    X'FF'                                                            
*                                                                               
         EJECT                                                                  
*                                                                               
* CA SCREEN TABLE  ( BYTES 0-3=DISP TO FIELD, BYTES 4-7=DISP TO ADCON )         
*                                                                               
CANTAB01 DS    0D                                                               
         DC    AL4(INCORDH-TWAD),AL4(AORDH-PROGD)                               
         DC    AL4(INCOHSTH-TWAD),AL4(AOHSTH-PROGD)                             
         DC    AL4(INCDOCH-TWAD),AL4(ADOCH-PROGD)                               
         DC    AL4(INCCLIH-TWAD),AL4(ACLIH-PROGD)                               
         DC    AL4(INCCLINH-TWAD),AL4(ACLINH-PROGD)                             
         DC    AL4(INCPROH-TWAD),AL4(APROH-PROGD)                               
         DC    AL4(INCPRONH-TWAD),AL4(APRONH-PROGD)                             
         DC    AL4(INCJOBH-TWAD),AL4(AJOBH-PROGD)                               
         DC    AL4(INCJOBNH-TWAD),AL4(AJOBNH-PROGD)                             
         DC    AL4(INCSUPH-TWAD),AL4(ASUPH-PROGD)                               
         DC    AL4(INCSUP-TWAD),AL4(ASUP-PROGD)                                 
         DC    AL4(INCSUPNH-TWAD),AL4(ASUPNH-PROGD)                             
         DC    AL4(INCSUPN-TWAD),AL4(ASUPN-PROGD)                               
         DC    AL4(INCWRKH-TWAD),AL4(AWRKH-PROGD)                               
         DC    AL4(INCWCNMH-TWAD),AL4(AWCNMH-PROGD)                             
         DC    AL4(INCAMTH-TWAD),AL4(AAMTH-PROGD)                               
         DC    AL4(INCNCWKH-TWAD),AL4(ANCWKH-PROGD)                             
         DC    AL4(INCWNNMH-TWAD),AL4(AWNNMH-PROGD)                             
         DC    AL4(INCNCAH-TWAD),AL4(ANCAH-PROGD)                               
         DC    AL4(INCNARH-TWAD),AL4(ANARH-PROGD)                               
         DC    X'FF'                                                            
*                                                                               
CANTAB03 DS    0D                                                               
         DC    AL4(CHCORDH-TWAD),AL4(AORDH-PROGD)                               
         DC    AL4(CHCOHSTH-TWAD),AL4(AOHSTH-PROGD)                             
         DC    AL4(CHCDOCH-TWAD),AL4(ADOCH-PROGD)                               
         DC    AL4(CHCCLIH-TWAD),AL4(ACLIH-PROGD)                               
         DC    AL4(CHCCLINH-TWAD),AL4(ACLINH-PROGD)                             
         DC    AL4(CHCPROH-TWAD),AL4(APROH-PROGD)                               
         DC    AL4(CHCPRONH-TWAD),AL4(APRONH-PROGD)                             
         DC    AL4(CHCJOBH-TWAD),AL4(AJOBH-PROGD)                               
         DC    AL4(CHCJOBNH-TWAD),AL4(AJOBNH-PROGD)                             
         DC    AL4(CHCSUPH-TWAD),AL4(ASUPH-PROGD)                               
         DC    AL4(CHCSUPNH-TWAD),AL4(ASUPNH-PROGD)                             
         DC    AL4(CHCWRKH-TWAD),AL4(AWRKH-PROGD)                               
         DC    AL4(CHCWCNMH-TWAD),AL4(AWCNMH-PROGD)                             
         DC    AL4(CHCAMTH-TWAD),AL4(AAMTH-PROGD)                               
         DC    AL4(CHCNCWKH-TWAD),AL4(ANCWKH-PROGD)                             
         DC    AL4(CHCWNNMH-TWAD),AL4(AWNNMH-PROGD)                             
         DC    AL4(CHCNCAH-TWAD),AL4(ANCAH-PROGD)                               
         DC    AL4(CHCNARH-TWAD),AL4(ANARH-PROGD)                               
         DC    X'FF'                                                            
*---------------------------------------------------------                      
*        EXTERNAL INCLUDE                                                       
*---------------------------------------------------------                      
*                                                                               
       ++INCLUDE ACBATCODE                                                      
         EJECT                                                                  
*---------------------------------------------------------                      
*        LITERAL DECLARATIONS                                                   
*---------------------------------------------------------                      
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE ACBATDSECT                                                     
         ORG   TWAHOLE                                                          
ORDAMT   DS    PL6                 FOR USE BY 01 PHASE                          
         ORG   CONTABH                                                          
       ++INCLUDE ACBATFED          TYPE 1 US                                    
         ORG   CONTABH                                                          
       ++INCLUDE ACBATC1D          TYPE 1 CANADA                                
         ORG   CONTABH                                                          
       ++INCLUDE ACBATFCD          TYPE 3 US                                    
         ORG   CONTABH                                                          
       ++INCLUDE ACBATC2D          TYPE 3 CANADA                                
         ORG   CONTABH                                                          
       ++INCLUDE ACBATCDD          FOR TYPE 47                                  
         EJECT                                                                  
*---------------------------------------------------------                      
*        DSECT TO COVER WORKCODE TABLE PASSED BY 01 PHASE                       
*---------------------------------------------------------                      
*                                                                               
WKLINED  DSECT                                                                  
WKCODE   DS    CL2                 WORKCODE                                     
WKAMNT   DS    PL6                 WORKCODE AMOUNT                              
WKLINEDL EQU   *-WKLINED                                                        
WKMAXROW EQU   8                   8 ROWS IN WORKCODE TABLE                     
         EJECT                                                                  
*---------------------------------------------------------                      
*        DSECT TO COVER LOCAL WORKING STORAGE                                   
*---------------------------------------------------------                      
*                                                                               
PROGD    DSECT                                                                  
ADLIST   DS    A                   ADDRESS OF WORKCODE LIST                     
SAVERE   DS    A                   SAVE AREA FOR RE                             
LEFT     DS    PL6                                                              
AMTWRK   DS    CL40                                                             
INVS     DS    PL3                                                              
ORDSOFA  DS    PL6                                                              
TRANSKEY DS    CL49                                                             
*                                                                               
AORDH    DS    A                   A(ORDER NUMBER HEADER)                       
AOHSTH   DS    A                   A(ORDER HISTORY HEADER)                      
ADOCH    DS    A                   A(DOCUMENT NUMBER HEADER)                    
ACLIH    DS    A                   A(CLIENT HEADER)                             
ACLINH   DS    A                   A(CLIENT NAME HEADER                         
APROH    DS    A                   A(PRODUCT HEADER)                            
APRONH   DS    A                   A(PRODUCT NAME HEADER)                       
AJOBH    DS    A                   A(JOB HEADER)                                
AJOBNH   DS    A                   A(JOB NAME HEADER)                           
ASUPH    DS    A                   A(SUPPLIER HEADER)                           
ASUP     DS    A                   A(SUPPLIER DATA FIELD)                       
ASUPNH   DS    A                   A(SUPPLIER NAME HEADER)                      
ASUPN    DS    A                   A(SUPPLIER NAME FIELD)                       
AWRKH    DS    A                   A(WORK CODE HEADER)                          
AWCNMH   DS    A                   A(WORK CODE NAMES HEADER)                    
AAMTH    DS    A                   A(AMOUNTS HEADER)                            
AWNM4H   DS    A                   A(4TH WORK CODE NAME HEADER)                 
ANCWKH   DS    A                   A(NON-COMM WORKCODES HEADER)                 
AWNNMH   DS    A                   A(NON-COMM WORKCODE NAMES HEADER)            
ANCAH    DS    A                   A(AMOUNTS HEADER)                            
ANNM4H   DS    A                   A(4TH WORK CODE NAME HEADER)                 
ANARH    DS    A                   A(NARRATIVE HEADER)                          
ORDST2   DS    XL1                 SAVED ORDER STATUS 2                         
KEY      DS    CL49                                                             
IOAREA   DS    2000C                                                            
PROGDX   DS    0C                                                               
         EJECT                                                                  
*---------------------------------------------------------                      
*        EXTERNAL DSECTS USED                                                   
*---------------------------------------------------------                      
*                                                                               
* ACGENDAY                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENDAY                                                       
         PRINT ON                                                               
* DDFLDIND                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDFLDIND                                                       
         PRINT ON                                                               
* FAUTL                                                                         
         PRINT OFF                                                              
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'026ACBAT07   07/10/18'                                      
         END                                                                    
