*          DATA SET PEACTV     AT LEVEL 041 AS OF 03/19/15                      
*PHASE PEACTVA                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE LOGIO                                                                  
*INCLUDE CARDS                                                                  
         TITLE 'PEACTV - PERSON - FILE MAINTENANCE PROGRAM'                     
*        1.    LAPSE EXPIRED MEMBERS                                            
*        2.    PURGE MEMBERS LAPSED N MONTHS AGO TO ARCHIVE TAPE.               
*        3.    SET OFF 'NEW RECORD' BIT IN STATUS WHEN OVER                     
*              ONE MONTH OLD.                                                   
*        4.    ACCUMULATE STATS INFO ABOUT DROPPED RECORDS FOR PE71             
*        5.    PRINT COUNTS FOR ANY NON MAIL SYSTEM RECORDS, AND COPY           
*              THEM UNCHANGED TO OUTPUT                                         
*        6.    RELOAD PURGED RECORDS FROM ARCHIVE TAPE BY REQUEST.              
*                                                                               
*        INPUT  - DUMP TAPE FROM PELD                                           
*               - PREVIOUS ARCHIVE TAPE                                         
*        OUTPUT - DUMP TAPE FOR PELD WITHOUT LONG-LAPSED MEMBERS                
*               - UPDATED ARCHIVE TAPE OF LONG-LAPSED MEMBERS                   
*                                                                               
         PRINT NOGEN                                                            
PEACTV   CSECT                                                                  
         NBASE WRKLEN,PEACTV,RA,WORK=A(RCWORK),CLEAR=YES                        
*                                                                               
         LR    R9,RC                                                            
         USING WRKAREA,R9          WORK AREA                                    
         L     R8,VCPRINT                                                       
         USING DPRINT,R8                                                        
         USING PEKEYD,R7           RECORD WORK = R7                             
         SPACE 3                                                                
INIT     EQU   *                                                                
         GOTO1 VDATCON,DMCB,(5,0),(1,TODAY) GET TODAYS DATE (PWOS)              
         MVC   ONEMONTH,TODAY      CALCULATE DATE ONE MONTH AGO                 
         MVC   DUB(1),TODAY+1      COPY MONTH                                   
         MVI   DUB+1,X'0C'         GIVE IT A SIGN                               
         SP    DUB(2),=P'10'       KNOCK 1 MONTH OFF MM1                        
         BP    INI010              SKIP IF STILL POSITIVE                       
         AP    DUB(2),=P'120'      ELSE WRAP TO MONTH 12                        
         MVC   DUB+2(2),DUB        COPY SO DUB=XL4'MM0CMM0C'                    
         MVC   DUB+2(1),TODAY      COPY YEAR SO DUB=XL4'MM0CYY0C'               
         SP    DUB+2(2),=P'10'     MINUS ONE FROM YEAR                          
         MVC   ONEMONTH(1),DUB+2   AND COPY YY-1                                
INI010   MVC   ONEMONTH+1(1),DUB   COPY MM-1                                    
         MVI   ERRFLAG,C'N'                                                     
         MVI   EOFSW,X'FF'         STOP EXTRA HEADINGS                          
*                                                                               
INI020   XC    PARMTAB(10*2),PARMTAB                                            
         MVC   P+9(15),=C'PARAMETER CARDS'                                      
         BAS   RE,PRINTIT                                                       
         MVI   P+9,C'-'                                                         
         MVC   P+10(14),P+9                                                     
         BAS   RE,PRINTIT                                                       
         LA    R2,PARMTAB                                                       
         LA    R3,11                                                            
INI030   GOTO1 =V(CARDS),DMCB,WORK,=C'RE00'                                     
         CLC   WORK(2),=C'/*'                                                   
         BE    INI050                                                           
         BCTR  R3,0                                                             
         MVC   P+9(80),WORK                                                     
         BAS   RE,PRINTIT                                                       
         LTR   R3,R3                                                            
         BZ    INI038              TOO MANY RECORDS MESSAGE                     
         BM    INI030              PRINTS ONLY ONCE                             
         CLC   =C'RELOAD=',WORK    PARM FORMAT IS 'RELOAD=AGY,LST'              
         BNE   INI039                                                           
         CLI   WORK+10,C','                                                     
         BNE   INI039                                                           
         MVI   WORK+10,C'0'        CHANGE COMMA TO 0 TO VALIDATE                
         LA    RE,WORK+7                                                        
         LA    RF,7                                                             
INI031   CLI   0(RE),C'0'                                                       
         BL    INI039                                                           
         CLI   0(RE),C'9'                                                       
         BH    INI039                                                           
         LA    RE,1(,RE)                                                        
         BCT   RF,INI031                                                        
         PACK  DUB,WORK+7(3)                                                    
         CVB   RE,DUB                                                           
         CH    RE,=Y(255)          AGY MUST BE LT 255                           
         BNL   INI039                                                           
         LTR   RE,RE               AGY MUST NOT BE ZERO                         
         BZ    INI039                                                           
         PACK  DUB,WORK+11(3)                                                   
         CVB   RF,DUB                                                           
         CH    RF,=Y(X'C0')        LIST MUST BE LT C1                           
         BH    INI039                                                           
         LTR   RF,RF               LIST MUST NOT BE ZERO                        
         BZ    INI039                                                           
         STC   RE,0(R2)                                                         
         STC   RF,1(R2)                                                         
         LA    RE,PARMTAB          CHECK FOR DUPLICATE                          
INI034   CR    RE,R2                                                            
         BNL   INI035                                                           
         CLC   0(2,RE),0(R2)                                                    
         BE    INI037                                                           
         LA    RE,2(,RE)                                                        
         B     INI034                                                           
INI035   LA    R2,2(,R2)                                                        
         B     INI030                                                           
INI037   MVC   P(14),=C'DUPLICATE CARD'                                         
         BAS   RE,PRINTIT                                                       
         MVI   ERRFLAG,C'Y'                                                     
         B     INI030                                                           
INI038   MVC   P(14),=C'TOO MANY CARDS'                                         
         BAS   RE,PRINTIT                                                       
         MVI   ERRFLAG,C'Y'                                                     
         B     INI030                                                           
INI039   MVC   P(18),=C'ABOVE CARD INVALID'                                     
         BAS   RE,PRINTIT                                                       
         MVI   ERRFLAG,C'Y'                                                     
         B     INI030                                                           
*                                                                               
INI050   BAS   RE,PRINTIT                                                       
         BAS   RE,PRINTIT                                                       
         CLI   ERRFLAG,C'N'                                                     
         BE    INI100                                                           
         MVC   P+13(L'ERR14),ERR14                                              
         LA    R1,L'ERR14                                                       
         MVI   REPSW,X'FF'         NO REPLY                                     
         B     ERRCANC                                                          
*                                                                               
INI100   L     R2,=A(ACTVIN)                                                    
         L     R3,=A(ACTVOUT)                                                   
         L     R4,=A(ARCHIN)                                                    
         L     R5,=A(ARCHOUT)                                                   
         OPEN  ((2),(INPUT),(3),(OUTPUT),(4),(INPUT),(5),(OUTPUT))              
*                                                                               
         MVI   EOFSW,0                                                          
         MVC   AGYNAM,SPACES                                                    
         MVC   AGYNAM(3),=C'000'                                                
*                                                                               
         L     R7,ADDSREC                                                       
         XC    PEKEY,PEKEY         SET LAST KEY FOR GETARCH                     
         BAS   RE,GETREC           READ FIRST RECORD                            
         BAS   RE,GETARCH          READ FIRST ARCHIVE RECORD                    
*                                                                               
         L     R7,ADDSREC          POINT AT ACTVIN RECORD                       
         CLI   EOFSW,0                                                          
         BE    INI110              SKIP IF NOT EOF                              
         MVC   P+13(L'ERR01),ERR01                                              
         LA    R1,L'ERR01                                                       
         MVI   REPSW,X'FF'         NO REPLY                                     
         B     ERRCANC                                                          
INI110   CLI   PEKEY,0                                                          
         BNE   INI115              SKIP IF ITS NOT A FILE HEADER                
         OC    PEKEY(L'PEKEY-1),PEKEY                                           
         BZ    INI120              SKIP IF DUMP TAPE                            
INI115   MVC   P+13(L'ERR02),ERR02                                              
         LA    R1,L'ERR02                                                       
         MVI   REPSW,X'FF'         NO REPLY                                     
         B     ERRCANC                                                          
INI120   CLC   =C'PERFIL',PEKEY+80                                              
         BE    INI200              SKIP IF PERFIL DUMP                          
         MVC   P+13(L'ERR03),ERR03                                              
         LA    R1,L'ERR03                                                       
         MVI   REPSW,X'FF'         NO REPLY                                     
         BAS   RE,ERROUT           TRY IT ANYWAY                                
*                                                                               
INI200   EQU   *                                                                
         MVC   P+13(19),=C'INPUT FILE HEADER -'                                 
         MVC   P+33(12),PEKEY+76                                                
         MVC   P+46(8),PEKEY+56                                                 
         MVC   P+55(2),PEKEY+66                                                 
         MVI   P+57,C'.'                                                        
         MVC   P+58(2),PEKEY+68                                                 
         MVI   P+60,C'.'                                                        
         MVC   P+61(2),PEKEY+70                                                 
         LA    R1,(P+63)-(P+13)                                                 
         MVI   REPSW,X'FF'         NO REPLY                                     
         BAS   RE,MESSOUT                                                       
         ZAP   LINE,=P'100'                                                     
*                                                                               
         B     COPY                GO COPY TO O/P AND READ NEXT                 
         SPACE 3                                                                
FINAL    EQU   *                                                                
         MVI   LASTKEY,X'FF'                                                    
         L     R7,AARCREC                                                       
         NC    ARCHNL,ARCHNL       IF ANY UNMATCHED ARCHIVE RECORD              
         BZ    FIN001              COPY TO OUTPUT ARCHIVE                       
         BAS   RE,PUTARCH                                                       
FIN001   CLI   EOFARCH,0           FLUSH OUT UNMATCHED ARCHIVE RECS             
         BNE   FIN002              TO OUTPUT ARCHIVE                            
         BAS   RE,GETARCH                                                       
         B     FIN001                                                           
*                                                                               
FIN002   L     R2,=A(ACTVIN)                                                    
         L     R3,=A(ACTVOUT)                                                   
         L     R4,=A(ARCHIN)                                                    
         L     R5,=A(ARCHOUT)                                                   
         CLOSE ((2),,(3),,(4),,(5))                                             
*                                                                               
         MVC   SPACING,=C'BL02'                                                 
         ZAP   LINE,=P'100'                                                     
         L     R2,TOTDUP                                                        
         BCT   R2,*+8                                                           
         B     FIN005                                                           
         MVC   P+9(23),=C'DUPLICATE RECORDS INPUT'                              
         EDIT  (R2),(8,P)                                                       
         BAS   RE,PRINTIT                                                       
FIN005   MVC   P+6(23),=C'NO PERFIL RECORDS INPUT'                              
         NC    TOTIN,TOTIN                                                      
         BZ    FIN007                                                           
         EDIT  TOTIN,(8,P)                                                      
FIN007   BAS   RE,PRINTIT                                                       
         MVC   P+6(24),=C'NO ARCHIVE RECORDS INPUT'                             
         NC    TOTAIN,TOTAIN                                                    
         BZ    FIN008                                                           
         EDIT  TOTAIN,(8,P)                                                     
FIN008   BAS   RE,PRINTIT                                                       
         L     R2,TOTMOD                                                        
         LTR   R2,R2                                                            
         BZ    FIN010                                                           
         MVC   P+6(25),=C'NO PERFIL RECORDS CREATED'                            
         BP    FIN009                                                           
         MVC   P+24(7),=C'DELETED'                                              
         LPR   R2,R2                                                            
FIN009   EDIT  (R2),(8,P)                                                       
         BAS   RE,PRINTIT                                                       
FIN010   MVC   P+6(24),=C'NO PERFIL RECORDS OUTPUT'                             
         NC    TOTOUT,TOTOUT                                                    
         BZ    FIN020                                                           
         EDIT  TOTOUT,(8,P)                                                     
FIN020   BAS   RE,PRINTIT                                                       
         MVC   P+6(26),=C'NO PERFIL RECORDS ARCHIVED'                           
         NC    TOTARCH,TOTARCH                                                  
         BZ    FIN030                                                           
         EDIT  TOTARCH,(8,P)                                                    
FIN030   BAS   RE,PRINTIT                                                       
         L     R1,ARCHXTRA                                                      
         A     R1,ARCHNL                                                        
         BZ    FIN040                                                           
         ST    R1,ARCHXTRA                                                      
         MVC   P+9(23),=C'UNKNOWN ARCHIVE RECORDS'                              
         EDIT  ARCHXTRA,(8,P)                                                   
         BAS   RE,PRINTIT                                                       
FIN040   CLI   ERRFLAG,C'Y'                                                     
         BNE   EOJ                                                              
*                                                                               
         MVC   P+13(L'ERRACK),ERRACK IF ANY ERRORS OCCURRED, ASK FOR            
         LA    R1,L'ERRACK         ACKNOWLEDGEMENT FROM OP                      
         MVI   REPSW,0             INDICATE REPLY REQUIRED                      
         BAS   RE,ERROUT                                                        
*                                                                               
EOJ      XBASE                                                                  
*                                                                               
EXIT     XMOD1 1                                                                
*                                                                               
CANCEL   EQU   *                                                                
         ABEND 600,DUMP                                                         
         EJECT                                                                  
*        COPY LAST RECORD TO OUTPUT                                             
*        READ NEXT INPUT RECORD AND CHECK IF SAME AGY/LIST                      
*                                                                               
COPY     BAS   RE,PUTREC           COPY RECORD TO ACTVOUT                       
         SPACE 3                                                                
NEXTREC  L     R7,ADDSREC          RESET TO ACTVIN RECORD                       
         BAS   RE,GETREC           READ NEXT RECORD                             
         TM    PERSTAT,X'80'       IF RECORD IS DELETED, WRITE DIRECT           
         BO    COPY                TO ACTVOUT IN ALL CASES, AND IGNORE          
         CLC   LASTAL,PEKEY        IS IT SAME AGY AND LIST                      
         BE    SAMELIST            YES - GO CONTINUE IT                         
         CLI   LASTAGY,C'D'        DRIVER RECORD                                
         BE    ENDLIST                                                          
         CLI   LASTLST,0           WAS LAST RECORD AN AGENCY HEADER ?           
         BNE   ENDLIST             NO - MUST BE END OF LIST                     
         CLC   LASTAGY,PEKEY       IS IT STILL SAME AGY                         
         BE    SAMEAGY             YES - GO START NEW LIST                      
         CLI   LASTAGY,0           WAS LAST RECORD A FILE HEADER ?              
         BE    NEXTLIST            YES - IT AINT A NUL AGENCY                   
         BAS   RE,PRINTIT          PRINT LAST AGENCY (WITH NO LISTS)            
         B     NEXTLIST            GO START NEW AGY                             
*                                                                               
*        ON CHANGE OF LIST, FIRST CLEAR UP LAST LIST                            
*        -     WRITE NON ZERO STATS ACCUMS TO OUTPUT AS STATS RECORDS           
*                                                                               
ENDLIST  EQU   *                                                                
         CLI   LASTAGY,C'D'        DRIVER RECORD                                
         BE    ENDL105                                                          
         CLI   LASTLST,C'A'        IS IT MAILING LIST SYSTEM ?                  
         BNL   ENDL105             NO - SKIP NO NEED FOR STATS RECORDS          
*                                                                               
         L     R7,AARCREC                                                       
ENDL005  CLC   LASTAL,0(R7)        ARE THERE ANY ARCHIVE RECS FOR THIS          
         BL    ENDL007             LIST TO BE COPIED ACCROSS                    
         BAS   RE,PUTARCH          IF YES FLUSH TO NEXT LIST                    
         BAS   RE,GETARCH                                                       
         B     ENDL005                                                          
*                                                                               
ENDL007  L     R7,ADDSREC                                                       
         LR    RE,R7                                                            
         SH    RE,=Y(4)            POINT TO RECORD HEADER                       
         LA    RF,L'DDSREC+4                                                    
         L     R0,ASAVREC          POINT TO SAVE AREA                           
         SH    R0,=Y(4)            POINT TO RECORD HEADER                       
         LR    R1,RF                                                            
         MVCL  R0,RE               SAVE RECORD FROM IO AREA                     
*                                                                               
         XC    PEKEY(PEQSE),PEKEY  BUILD A STATS RECORD                         
         MVC   PEKEY(2),LASTAL     SET LAST AGY/LIST                            
         MVI   PEKTYPE,X'F0'       STATS RECORD TYPE                            
         LA    R1,PEQSE+12*PEMSTLNQ+1 LENGTH OF STATS RECORD                    
         STCM  R1,3,PERLEN         STORE LENGTH                                 
         LA    R0,4(,R1)           ADD CTL LENGTH                               
         LA    R1,PEKEY                                                         
         SH    R1,=H'4'            POINT TO CTL                                 
         STCM  R0,3,0(R1)          STORE CTL LENGTH                             
*                                                                               
         L     R3,ASTACCS          POINT TO STATS ACCUMS                        
         USING STACC,R3                                                         
         MVI   YY,X'51'            SET 1951 (FIRST YEAR OF ACCUMS)              
         MVI   YYS,X'0C'           SET SIGN                                     
*                                                                               
ENDL010  LA    R2,PERFRST          POINT TO FIRST ELEMENT                       
         USING PEMSTD,R2                                                        
         MVI   MM,X'01'            SET JAN  (FIRST MONTH OF YEAR)               
         MVI   MMS,X'0C'           SET SIGN                                     
         MVI   STASW,0             SET ALL ACCUMS FOR YEAR ARE ZERO             
ENDL020  MVI   PEMSTEL,PEMSTELQ    BUILD STATS ELEMENT FOR MONTH                
         MVI   PEMSTLEN,PEMSTLNQ                                                
         MVC   PEMSTYY,YY                                                       
         MVC   PEMSTMM,MM                                                       
         MVC   PEMSTVAL(STACCL),STACC COPY ALL ACCUMS IN                        
         NC    STACC(STACCL),STACC ARE THEY ALL ZERO ?                          
         BZ    *+8                 YES                                          
         MVI   STASW,X'FF'         NO - SET FLAG                                
         LA    R2,PEMSTLNQ(,R2)    NEXT ELEMENT                                 
         LA    R3,STACCL(,R3)      NEXT ACCUMULATOR                             
         AP    MMP,=P'10'          ADD ONE TO MONTH                             
         CP    MMP,=P'120'                                                      
         BNH   ENDL020             LOOP TILL WHOLE YEAR DONE                    
*                                                                               
         MVI   PEMSTEL,0           SET END OF RECORD                            
         CLI   STASW,0             ANY NON ZERO ACCUMS ?                        
         BE    ENDL030             NO - DONT WRITE THE RECORD                   
         MVC   PEKMSYY,YY          SET YEAR IN KEY                              
         BAS   RE,PUTREC           AND WRITE THE RECORD WE JUST BUILT           
         L     R1,TOTMOD                                                        
         AH    R1,=Y(1)            COUNT RECORDS ADDED                          
         ST    R1,TOTMOD                                                        
ENDL030  CP    YYP,=P'990'         DONE ALL ACCUMS ?                            
         BNL   ENDL100             YES - SKIP                                   
         AP    YYP,=P'10'          NEXT YEAR                                    
         B     ENDL010             GO PROCESS IT                                
         DROP  R2,R3                                                            
ENDL100  L     RE,ADDSREC                                                       
         SH    RE,=Y(4)            POINT TO RECORD HEADER                       
         LA    RF,L'DDSREC+4                                                    
         L     R0,ASAVREC          POINT TO SAVE AREA                           
         SH    R0,=Y(4)            POINT TO RECORD HEADER                       
         LR    R1,RF                                                            
         MVCL  RE,R0               RESTORE RECORD TO IO AREA                    
         SPACE 3                                                                
*        -     PRINT RECORD COUNTS FOR LAST LIST/SYSTEM                         
*                                                                               
ENDL105  L     R2,ARECACCS         POINT TO RECORD CNTRS FOR LAST LIST          
         USING RECACC,R2                                                        
*                                                                               
ENDL110  LA    R1,RTYDRTAB                                                      
         CLI   RECAGY,C'D'         DRIVER RECORD                                
         BE    ENDL120                                                          
         CLI   RECLIST,0           IS IT AGENCY HEADER?                         
         BE    ENDL140             YES - DONT PRINT COUNT LINE                  
         LA    R1,SYSTAB           PICK UP TABLE OF SYSTEMS                     
ENDL115  CLC   RECLIST,0(R1)                                                    
         BNH   ENDL116             SEARCH FOR SYSTEM                            
         LA    R1,SYSTABL(,R1)                                                  
         B     ENDL115                                                          
ENDL116  L     R1,0(R1)            PICK UP ADDRESS OF RECORD TYPE TABLE         
*                                                                               
ENDL120  CLI   0(R1),X'FF'                                                      
         BE    ENDL125                                                          
         CLC   RECTYPE,0(R1)                                                    
         BE    ENDL125                                                          
         LA    R1,RTYTABL(,R1)                                                  
         B     ENDL120                                                          
ENDL125  MVC   P(RTYTABL-1),1(R1)                                               
*                                                                               
         LA    R3,P+14             EDIT ALL ACCUM FIELDS                        
         LA    R4,RECITOT                                                       
         LA    R5,10                                                            
ENDL130  EDIT  (4,0(R4)),(8,0(R3)),ZERO=BLANK                                   
         LA    R3,9(,R3)                                                        
         LA    R4,4(,R4)                                                        
         BCT   R5,ENDL130                                                       
         L     R4,RECOACT                                                       
         A     R4,RECOLAP                                                       
         A     R4,RECODEL                                                       
         EDIT  (R4),(8,0(R3)),ZERO=BLANK                                        
*                                                                               
         BAS   RE,PRINTIT          PRINT ACCUM LINE                             
*                                                                               
ENDL140  LA    R2,RECACCL(,R2)     NEXT ACCUM SET                               
         C     R2,CRECACC          HAVE WE REACHED END                          
         BL    ENDL110             NO - GO AND PRINT IT                         
         DROP  R2                                                               
*                                                                               
         BAS   RE,PRINTIT          PRINT SPACE LINE                             
*                                                                               
         L     RE,CRECACC          POINT TO CURRENT ENTRY                       
         L     RF,ARECACCS         POINT TO FIRST ENTRY                         
         MVC   0(RECACCL,RF),0(RE) COPY CURRENT TO FIRST                        
         ST    RF,CRECACC          SET CURRENT = FIRST                          
         EJECT                                                                  
*        AT START OF NEW LIST, CHECK IF EOF OR NEW AGENCY                       
*                                                                               
NEXTLIST MVI   MERSW,0             SHOW ARCHIVE DATA NOT TO BE MERGED           
         XC    CMEMACC,CMEMACC     SHOW NO MEMREC ACCUMULATOR YET               
         CLC   LASTAGY,PEKAGY      IS IT THE SAME AGENCY                        
         BE    SAMEAGY             YES - SKIP                                   
         CLI   PEKEY,X'FF'         IS IT A FILE TRAILER ?                       
         BNE   NEXTAGY             NO - ITS A NEW AGENCY                        
         CLI   EOFSW,0             IS IT END OF FILE                            
         BE    READEOF             NO - SKIP                                    
         MVC   P+13(L'ERR04),ERR04 PREMATURE EOF                                
         LA    R1,L'ERR04                                                       
         MVI   REPSW,X'FF'         NO REPLY                                     
         B     ERRCANC                                                          
READEOF  BAS   RE,PUTREC           COPY TRAILER                                 
         BAS   RE,GETREC           READ FOR EOF                                 
         CLI   EOFSW,0                                                          
         BNE   FINAL               CLOSE UP IF WE GOT EOF                       
         MVC   P+13(L'ERR05),ERR05 RECORDS AFTER TRAILER                        
         LA    R1,L'ERR05                                                       
         MVI   REPSW,X'FF'         NO REPLY                                     
         B     ERRCANC                                                          
         SPACE 3                                                                
*        NEW AGENCY - FORCE NEW PAGE, AND IF MAIL SYSTEM, ENSURE AGENCY         
*                     HEADER PRESENT.                                           
*                                                                               
NEXTAGY  EQU   *                   SET UP FOR NEW AGENCY                        
         ZIC   R1,PEKAGY                                                        
         CVD   R1,DUB                                                           
         MVC   AGYNAM,SPACES                                                    
         UNPK  AGYNAM(3),DUB+6(2)                                               
         OI    AGYNAM+2,X'F0'                                                   
         ZAP   LINE,=P'100'                                                     
         CLI   PEKEY,C'D'          DRIVER ENTRY                                 
         BE    SAMEAGY                                                          
         CLI   PEKSYS,C'A'         IS IT MAILING LIST SYSTEM ?                  
         BNL   SAMEAGY             NO - SKIP THERE IS NO AGY HEADER             
         CLI   PEKLIST,0           IS IT AN AGENCY HEADER ?                     
         BE    COPY                YES - SKIP                                   
         MVC   P+13(L'ERR07),ERR07 MISSING AGENCY HEADER                        
         LA    R1,L'ERR07                                                       
         MVI   REPSW,X'FF'         NO REPLY                                     
         B     ERRCANC                                                          
         SPACE 3                                                                
*        NEW LIST - ENSURE LIST HEADER PRESENT AND EXTRACT LIST NAME            
*                 - CLEAR ACCUMULATORS FOR NEW LIST                             
*                                                                               
SAMEAGY  EQU   *                   INIT FOR NEW LIST                            
         CLI   PEKEY,C'D'          DRIVER RECORD                                
         BE    SAMAG50                                                          
         CLI   PEKSYS,C'A'         IS IT A MAIL SYSTEM RECORD  ?                
         BNL   SAMAG30             NO - SKIP                                    
         CLI   PEKLIST,0           IS IT ANOTHER AGENCY HEADER ?                
         BNE   SAMAG10             NO - SKIP                                    
         MVC   P+13(L'ERR08),ERR08 EXTRA AGENCY HEADER                          
         LA    R1,L'ERR08                                                       
         MVI   REPSW,X'FF'         NO REPLY                                     
         B     ERRCOPY                                                          
SAMAG10  CLI   PEKTYPE,0           IS IT A LIST HEADER ?                        
         BE    SAMAG20             YES - SKIP                                   
         MVC   P+13(L'ERR09),ERR09 MISSING LIST HEADER                          
         LA    R1,L'ERR09                                                       
         MVI   REPSW,X'FF'         NO REPLY                                     
         B     ERRCANC                                                          
SAMAG20  MVC   P(6),=C'LIST ='                                                  
         MVI   ELCD,PELSTELQ       LOOK FOR LIST NAME                           
         BAS   RE,GETEL                                                         
         BNE   SAMAG90                                                          
         USING PELSTD,R2                                                        
         ZIC   R1,PELSTLEN         LENGTH OF ELEMENT                            
         LA    R0,PELSTFNM-PELSTD+1 LENGTH EXCLUDING NAME (PLUS 1)              
         SR    R1,R0               LENGTH OF NAME (MINUS 1)                     
         BM    SAMAG21             SKIP IF NO NAME                              
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P+7(0),PELSTFNM     MOVE IN FULL NAME                            
SAMAG21  B     SAMAG90                                                          
*                                                                               
SAMAG30  MVC   P(8),=C'SYSTEM ='                                                
         MVC   P+9(1),PEKSYS       MOVE IN SYSTEM                               
         MVI   P+11,C'-'                                                        
         LA    R1,SYSTAB           FIND SYSTEM DESCRIPTION                      
SAMAG35  CLC   PEKSYS,0(R1)                                                     
         BNH   SAMAG40                                                          
         LA    R1,SYSTABL(R1)                                                   
         B     SAMAG35                                                          
SAMAG40  MVC   P+13(16),4(R1)      MOVE IN SYSTEM DESCRIPTION                   
         B     SAMAG90                                                          
SAMAG50  MVC   P(14),=C'DRIVER ENTRIES'                                         
*                                                                               
SAMAG90  BAS   RE,PRINTIT                                                       
         BAS   RE,PRINTIT                                                       
         DROP  R2                                                               
*                                                                               
         CLI   PEKEY,C'D'                                                       
         BE    COPY                                                             
         CLI   PEKSYS,C'A'         IS IT A MAIL SYSTEM RECORD  ?                
         BNL   COPY                NO - SKIP                                    
         LA    RE,PARMTAB                                                       
         LA    RF,10                                                            
SAMAG100 CLC   0(2,RE),PEKAGY      IS THIS LIST IN MERGE LIST                   
         BE    SAMAG105            YES - SET SWITCH                             
         LA    RE,2(,RE)                                                        
         BCT   RF,SAMAG100                                                      
         B     SAMAG110                                                         
SAMAG105 MVI   MERSW,X'FF'         SHOW ARCH DATA TO BE RELOADED                
         MVI   PERLAPSE,X'FF'                                                   
         MVC   P(29),=C'**** ARCHIVE RELOAD REQUESTED'                          
         BAS   RE,PRINTIT                                                       
         BAS   RE,PRINTIT                                                       
SAMAG110 LA    R1,STACCL           R1=LENGTH OF ONE MONTHS ACCUMS               
         LA    RE,12                                                            
         MR    R0,RE               R1=LENGTH OF ONE YEARS ACCUMS                
         LA    RE,50                                                            
         MR    R0,RE               R1=LENGTH OF 50 YEARS ACCUMS                 
         L     R0,ASTACCS                                                       
         LR    RE,R0                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE               CLEAR STATISTACS ACCUMS FOR NEW LIST         
         MVI   RENSW,0             INDICATE NO RENEWAL MECHANISM (YET)          
         B     COPY                COPY LIST HEADER RECORD TO O/P               
         EJECT                                                                  
*        CONTINUE LIST                                                          
*        -     BRANCH ACCORDING TO RECORD TYPE                                  
*                                                                               
SAMELIST CLI   PEKEY,C'D'                                                       
         BE    COPY                                                             
         CLI   PEKSYS,C'A'         IS IT MAIL SYSTEM ?                          
         BNL   COPY                NO - JUST COPY UNCHANGED                     
*                                                                               
         CLI   PEKTYPE,X'20'       IS IT A MEMBER RECORD ?                      
         BE    LISTMEM             YES - HANDLE IT                              
         CLI   PEKTYPE,X'02'       IS IT A FLD CONTROL RECORD                   
         BE    LISTFLD             YES - HANDLE IT                              
         CLI   PEKTYPE,X'F0'       IS IT A STATS RECORD                         
         BE    LISTSTA             YES - HANDLE IT                              
         CLI   PEKTYPE,X'00'       IS IT LIST HEADER REC                        
         BNE   COPY                NO - COPY IT ACCROSS                         
         MVC   P+13(L'ERR10),ERR10 EXTRA LIST HEADER                            
         LA    R1,L'ERR10                                                       
         MVI   REPSW,X'FF'         NO REPLY                                     
         B     ERRCOPY                                                          
         SPACE 3                                                                
LISTFLD  EQU   *                   HANDLE FIELD CONTROL RECORD                  
         CLI   LASTTYP,X'02'       IS IT ANOTHER FIELD CONTROL REC              
         BNE   FLDREC              NO - GOOD                                    
         MVC   P+13(L'ERR11),ERR11 EXTRA FIELD CONTROL RECORD                   
         LA    R1,L'ERR11                                                       
         MVI   REPSW,X'FF'         NO REPLY                                     
         B     ERRCOPY                                                          
         SPACE                                                                  
LISTMEM  EQU   *                   HANDLE MEMBER RECORD                         
         L     R1,CRECACC          CURRENT MEMBER ACCUM ADDRESS                 
         ST    R1,CMEMACC          SET CURRENT MEMBER ACCUM ADDRESS             
         NC    ARCHNL,ARCHNL                                                    
         BZ    MEMREC                                                           
         CLC   LASTAL,LASTARCH     IS THIS ARCH REC FOR THIS LIST               
         BNE   MEMREC              NO ITS FOR A LATER ONE                       
         L     RE,RECIARC-RECACC(,R1)                                           
         A     RE,ARCHNL           ADD IN ANY PENDING ARCH REC                  
         ST    RE,RECIARC-RECACC(,R1)                                           
         XC    ARCHNL,ARCHNL       AND CLEAR COUNT                              
         B     MEMREC                                                           
         SPACE                                                                  
LISTSTA  EQU   *                                                                
         CLC   LASTKEY(4),PEKEY    IS IT SAME YEAR AS PREVIOUS                  
         BNE   STAREC              NO - GOOD                                    
         MVC   P+13(L'ERR12),ERR12 DUPLICATE STATS RECORD                       
         LA    R1,L'ERR12                                                       
         MVI   REPSW,X'FF'         NO REPLY                                     
         B     ERRCOPY                                                          
         EJECT                                                                  
*        FIELD CONTROL RECORD                                                   
*        -     CHECK IF RENEWAL SUPPORTED AND SAVE DATA                         
*                                                                               
FLDREC   EQU   *                                                                
         MVI   ELCD,PEFLDELQ       LOOK FOR RENEWAL CONTROL ELEMENT             
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                DIE IF NONE FOUND                            
         USING PEFLDD,R2                                                        
FLD010   CLI   PEFLDELC,PERENELQ   DOES THIS REFER TO RENEWAL                   
         BE    FLD030              YES - SET IT UP                              
         BAS   RE,NEXTEL           LOOK AT NEXT ONE                             
         BE    FLD010              CHECK IT IF NOT END                          
         CLI   MERSW,0             ALL OK IF NOT RELOADING                      
         BE    FLD100                                                           
         MVC   P(34),=C'**** RELOAD IGNORED, NOT SUPPORTED'                     
         BAS   RE,PRINTIT                                                       
         BAS   RE,PRINTIT                                                       
         B     FLD100                                                           
FLD030   MVI   RENSW,X'FF'         INDICATE RENEWAL SUPPORTED                   
         CLI   MERSW,0             IGNORE LAPSE MECH IF RELOADING               
         BNE   FLD100                                                           
         MVC   PERLAPSE(1),PEFLDEOF SAVE LAPSE HOLD PERIOD                      
         MVI   PERLAPSE+1,X'0C'    SIGN IT                                      
         MVC   LAPMONTH,TODAY      CALCULATE DATE N MONTHS AGO                  
         CLI   PERLAPSE,X'FF'      ARE WE DROPPING LAPSED RECS                  
         BE    FLD100              NO                                           
         MVC   DUB(1),TODAY+1      COPY MONTH                                   
         MVI   DUB+1,X'0C'         GIVE IT A SIGN                               
         MVC   DUB+2(2),DUB        COPY SO DUB=XL4'MM0CMM0C'                    
         MVC   DUB+2(1),TODAY      COPY YEAR SO DUB=XL4'MM0CYY0C'               
         SP    DUB(2),PERLAPSE     KNOCK N MONTHS OFF MM2                       
         BP    FLD040              SKIP IF STILL POSITIVE                       
FLD035   SP    DUB+2(2),=P'10'     MINUS ONE FROM YEAR                          
         AP    DUB(2),=P'120'      ELSE WRAP TO MONTH 12                        
         BNP   FLD035              LOOP TILL MONTH BECOMES POSITIVE             
FLD040   MVC   LAPMONTH(1),DUB+2   COPY YY                                      
         MVC   LAPMONTH+1(1),DUB   COPY MM                                      
*                                                                               
FLD100   B     COPY                COPY FIELD CONTROL RECORD                    
         DROP  R2                                                               
         EJECT                                                                  
*        MEMBER RECORD                                                          
*                                                                               
         SPACE 3                                                                
MEMREC   EQU   *                                                                
         L     R2,AARCREC          R2=ARCHIVE, R7=ACTIVE                        
         CLC   PEKEY,0(R2)         IS ACTIVE LOWER THAN ARCHIVE                 
         BL    MEM005              YES - PROCESS ACTIVE RECORD OUT              
         LR    R7,R2               ELSE DEAL WITH ARCH RECORD                   
         BE    MEM799              IF MATCH, ALWAYS COPY ARCH TO ARCH           
         CLI   MERSW,0             ARE WE MERGING ARCHIVE RECORDS               
         BE    MEM799              NO - COPY ARCH TO ARCH                       
*                                  ELSE MERGE ARCHIVE BACK IN                   
MEM005   XC    AACTEL,AACTEL       ZERO ADDRESS OF ACTIVITY ELEMENT             
         XC    ACTWORK,ACTWORK     AND ACT EL WORK AREA                         
         XC    ARENEL,ARENEL       AND ADDRESS OF RENEW ELEMENT                 
         LA    R1,PERFRST                                                       
         XR    R0,R0                                                            
MEM010   CLI   0(R1),0             END OF RECORD                                
         BE    MEM020                                                           
         IC    R0,1(R1)            GET LENGTH                                   
         CLI   0(R1),PEACTELQ      ACTIVITY ELEMENT ?                           
         BNE   MEM013              NO - SKIP                                    
         CLI   1(R1),PEACTLNQ      IF LONGER THAN MAX DIE                       
         BNH   *+6                                                              
         DC    H'0'                                                             
         ST    R1,AACTEL           SAVE ITS ADDRESS                             
         LR    RE,R0               COPY LENGTH                                  
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ACTWORK(0),0(R1)    COPY ACTIVITY ELEMENT TO WORK AREA           
         B     MEM015                                                           
MEM013   CLI   0(R1),PERENELQ      RENEW ELEMENT ?                              
         BNE   MEM015              NO - SKIP                                    
         ST    R1,ARENEL           SAVE ITS ADDRESS                             
MEM015   AR    R1,R0               NEXT ELEMENT                                 
         B     MEM010                                                           
*                                                                               
*        IF ANY RECORD HAS THE 'NEW' BIT ON AND IS OLDER THAN ONE MONTH         
*        SET THE BIT OFF. LIKEWISE SET ON IF NECCESSARY                         
*                                                                               
MEM020   LA    R2,ACTWORK          POINT TO COPIED ACTIVITY EL                  
         USING PEACTD,R2                                                        
         TM    PERSTAT,X'10'       IS RECORD A NEW ONE ?                        
         BO    MEM025              YES - SKIP                                   
         CLC   PEACTADD,ONEMONTH   ADDED IN LAST MONTH ?                        
         BL    MEM030              NO - SKIP                                    
         OI    PERSTAT,X'10'       YES - THE BIT SHOULD BE ON                   
         LA    R1,RECCNEW-RECACC   COUNT MEMBERS CHANGED TO NEW                 
         BAS   RE,ACCUM                                                         
         B     MEM030                                                           
MEM025   CLC   PEACTADD,ONEMONTH   ADDED IN LAST MONTH ?                        
         BNL   MEM030              YES - SHOULD STILL BE NEW                    
         NI    PERSTAT,255-X'10'   NO  - THE BIT SHOULD BE OFF                  
         LA    R1,RECCOLD-RECACC   COUNT MEMBERS CHANGED TO NOT NEW             
         BAS   RE,ACCUM                                                         
         DROP  R2                                                               
*                                                                               
* PERSON FILE LAPSE MECHANISM                                                   
*                                                                               
*   1.   IF RENEW DATE HAS PASSED, CHANGE STATUS TO LAPSED.                     
*   2.   IF RECORD HAS BEEN LAPSED FOR OVER THREE MONTHS, WRITE IT TO           
*        TAPE AND SET TO DELETE IT                                              
*   3.   ALSO CHECK THAT LAPSED RECORDS ARE IN FACT LAPSED AND IF NOT,          
*        UN-LAPSE THEM                                                          
*                                                                               
MEM030   EQU   *                                                                
         LA    R3,ACTWORK          POINT TO COPIED ACTIVITY EL                  
         USING PEACTD,R3                                                        
         ICM   R2,15,ARENEL        IS RENEW ELEMENT HERE ?                      
         USING PEREND,R2                                                        
         BNZ   MEM035              YES - CHECK STATUS                           
         TM    PERSTAT,X'40'       IS RECORD LAPSED ?                           
         BZ    MEM060              NO - GOOD ALL IS OK                          
         B     MEM050              YES - UNLAPSE IT AS IT SHOULD BE             
MEM035   TM    PERSTAT,X'40'       IS RECORD LAPSED ?                           
         BO    MEM045              YES - SKIP                                   
MEM040   CLC   PERENREN,TODAY      HAS RENEWAL DATE EXPIRED ?                   
         BNL   MEM060              NO - NOTHING TO DO                           
         OI    PERSTAT,X'40'       YES - LAPSE THE MEMBER                       
         LA    R1,RECCLAP-RECACC   COUNT MEMBERS LAPSED                         
         BAS   RE,ACCUM                                                         
         MVC   PEACTLAP,TODAY                                                   
         CLI   PEACTLEN,PEACTLAP+L'PEACTLAP-PEACTD ENOUGH ROOM                  
         BNL   MEM060                              YES - SKIP                   
         MVI   PEACTLEN,PEACTLAP+L'PEACTLAP-PEACTD MAKE IT LONGER               
         B     MEM060                                                           
MEM045   CLC   PERENREN,TODAY      HAS RENEWAL DATE EXPIRED ?                   
         BL    MEM060              YES - LAPSED AS IT SHOULD BE                 
MEM050   NI    PERSTAT,255-X'40'   UNLAPSE THE MEMBER                           
         LA    R1,RECCACT-RECACC   COUNT MEMBERS UNLAPSED                       
         BAS   RE,ACCUM                                                         
         DROP  R2,R3                                                            
*                                                                               
* CHECK IF ACTIVITY ELEMENT CHANGED, AND ENSURE PASSIVE STATUS OK               
*                                                                               
MEM060   LA    R3,ACTWORK                                                       
         USING PEACTD,R3                                                        
         MVI   PEACTEL,PEACTELQ    JUST IN CASE ACT EL WAS MISSING              
*                                  R3 = NEW ACTIVITY ELEMENT (ACTWORK)          
         ICM   R2,15,AACTEL        R2 = OLD ACTIVITY ELEMENT (IN REC)           
         BNZ   MEM070              GO COMPARE NEW WITH OLD                      
         CLI   PEACTLEN,0          IF NO OLD, DID WE BUILD NEW                  
         BE    MEM100              NO - LEAVE IT ALONE                          
         B     MEM080              ELSE GO AND ADD IT TO RECORD                 
MEM070   ZIC   R1,PEACTLEN                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R3),0(R2)       SEE IF ACTIVITY ELEMENT CHANGED              
         BE    MEM100              NO - JUST EXIT                               
         CLC   1(1,R3),1(R2)       DID ITS LENGTH CHANGE                        
         BNE   MEM075              YES - GO DELETE AND ADD                      
         EX    R1,*+8                                                           
         B     MEM100                                                           
         MVC   0(0,R2),0(R3)       ELSE MOVE COPY RIGHT BACK IN                 
MEM075   GOTO1 VHELLO,DMCB,(C'D',PERSON),(PEACTEL,(R7)),0,0                     
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
MEM080   GOTO1 VHELLO,DMCB,(C'P',PERSON),(R7),(R3),0                            
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R3                                                               
*                                                                               
*        ENSURE PASSIVE STATUS BYTES MATCH PRIME KEY DATA                       
*                                                                               
MEM100   LA    R1,PERFRST          SET ALL STATUS FIELDS EQUAL                  
         XR    R0,R0                                                            
MEM110   CLI   0(R1),0             END OF RECORD                                
         BE    MEM130                                                           
         IC    R0,1(R1)            GET LENGTH                                   
         CLI   0(R1),PEPASELQ      PASSIVE POINTER ELEMENT ?                    
         BNE   MEM120              NO - SKIP                                    
         USING PEPASD,R1                                                        
         MVC   PEPASSTA,PERSTAT    COPY STATUS BYTES                            
         DROP  R1                                                               
MEM120   AR    R1,R0               NEXT ELEMENT                                 
         B     MEM110                                                           
MEM130   EQU   *                                                                
         SPACE 3                                                                
*        DETERMINE IF RECORD IS DUE FOR DELETION (ARCHIVING)                    
*                                                                               
MEM700   CLI   MERSW,0             ARE WE MERGING OLD LAPSEES                   
         BNE   MEM800              YES - COPY TO OUTPUT                         
         CLI   PERLAPSE,X'FF'      ARE WE DROPPING OLD LAPSEES                  
         BE    MEM800              NO - JUST COPY TO OUTPUT                     
         TM    PERSTAT,X'40'       IS RECORD LAPSED ?                           
         BZ    MEM800              NO - COPY RECORD ACCROSS                     
         LA    R2,ACTWORK                                                       
         USING PEACTD,R2                                                        
         CLC   PEACTLAP,LAPMONTH   IS IT DUE FOR DELETION                       
         BNL   MEM800              NO - COPY                                    
         DROP  R2                                                               
*                                                                               
*        ACCUMULATE STATISTICS FOR ARCHIVED RECORD                              
*                                                                               
         MVI   ELCD,PERENELQ                                                    
         BAS   RE,GETEL            FIND RENEW ELEMENT                           
         BE    *+6                                                              
         DC    H'0'                DIE IF RENEW NOT FOUND                       
         LR    R6,R2               SAVE ITS ADDRESS                             
         USING PEREND,R6                                                        
*                                                                               
         MVI   ELCD,PEACTELQ                                                    
         BAS   RE,GETEL            FIND ACTIVITY ELEMENT                        
         BE    *+6                                                              
         DC    H'0'                DIE IF ACTIVITY NOT FOUND                    
         LR    R5,R2               SAVE ITS ADDRESS                             
         USING PEACTD,R5                                                        
*                                                                               
         MVC   JOINDAT,PERENJOI    COPY YYMM JOINED                             
         MVC   DUEDAT,PERENREN     COPY YYMM DUE FOR RENEWAL                    
         MVC   RENDAT,PERENJOI     PRESET RENEW YYMM = DATE JOINED              
         CLI   PEACTLEN,PEACTREN-PEACTD+3 IS THIS LONG ENOUGH ?                 
         BL    MEM710              NO - NOT RENEWED                             
         NC    PEACTREN,PEACTREN                                                
         BZ    MEM710              ZERO DATE = NOT RENEWED                      
         MVC   RENDAT,PEACTREN     ELSE COPY YYMM RENEWED                       
MEM710   MVC   LAPSDAT,DUEDAT      COPY YYMM LAPSED = DUE DATE                  
         NC    PERENLRD,PERENLRD   WAS HE DUE BEFORE                            
         BZ    MEM712              ZERO DATE = NOT RENEWED                      
         MVC   LAPSDAT,PERENLRD    ELSE COPY YYMM LAST RENEW DATE               
         DROP  R5,R6                                                            
*        CALCULATE DATE ON WHICH RECORD SHOULD HAVE BEEN DELETED                
MEM712   MVC   DELDAT,=X'9912'     DEFAULT DELDATE = DEC99 (IE NEVER)           
         CLI   PERLAPSE,X'FF'                                                   
         BE    MEM720              SKIP IF LAPSED MEMBERS NOT PURGED            
         MVC   DUB(1),DUEDAT       CALCULATE DELDATE= DUE + X MONTHS            
         MVI   DUB+1,X'0C'                                                      
         MVC   DUB+2(1),DUEDAT+1                                                
         MVI   DUB+3,X'0C'                                                      
         AP    DUB+2(2),PERLAPSE                                                
MEM715   CP    DUB+2(2),=P'120'                                                 
         BNH   MEM717                                                           
         SP    DUB+2(2),=P'120'                                                 
         CP    DUB(2),=P'990'      IF DUE PLUS LAPSE PERIOD EXCEEDS             
         BNL   MEM720              DEC 99, USE DEC99 DEFAULT                    
         AP    DUB(2),=P'10'                                                    
         B     MEM715                                                           
MEM717   MVC   DELDAT(1),DUB                                                    
         MVC   DELDAT+1(1),DUB+2                                                
*                                                                               
MEM720   MVC   YY,JOINDAT          GET JOIN YEAR                                
         MVI   YYS,X'0C'           SET SIGN                                     
         MVC   MM,JOINDAT+1        GET JOIN MONTH                               
         MVI   MMS,X'0C'           SET SIGN                                     
         ZIC   R1,YY               GET YEAR                                     
         SLL   R1,4                SHIFT FOR SIGN                               
         LA    R1,X'0C'(,R1)       INSERT SIGN                                  
         XR    R0,R0                                                            
         STM   R0,R1,DUB                                                        
         XR    RF,RF                                                            
         SP    DUB,=P'51'          GET YEAR RELATIVE TO 1951                    
         BNP   MEM725              TREAT AS ZERO IF BEFORE 1951                 
         CVB   RF,DUB              MAKE IT BINARY                               
         LA    R1,12                                                            
         MR    RE,R1               RF=MONTHS FROM JAN 51 TO START OF YY         
         ZIC   R1,MM               GET MONTH                                    
         SLL   R1,4                SHIFT FOR SIGN                               
         LA    R1,X'0C'(,R1)       INSERT SIGN                                  
         XR    R0,R0                                                            
         STM   R0,R1,DUB                                                        
         CVB   R1,DUB              MAKE IT BINARY                               
         BCTR  R1,0                MAKE RELATIVE TO JAN (JAN=0,DEC=+11)         
         AR    RF,R1               POINT TO YYMM ACCUM                          
         LA    R1,STACCL                                                        
         MR    RE,R1               RF=OFFSET OF YYMM ACCUMS                     
MEM725   A     RF,ASTACCS          POINT TO ACCUMULATOR SET                     
         LR    R5,RF                                                            
         USING STACC,R5                                                         
         L     R1,STAJOI           ACCUMULATE JOINED THIS MONTH                 
         LA    R1,1(R1)                                                         
         ST    R1,STAJOI                                                        
*                                                                               
MEM730   MVC   STADAT(1),YY        SET DATE OF CURRENT ENTRY                    
         MVC   STADAT+1(1),MM                                                   
         CLC   STADAT,DELDAT       SHOULD THE MEMBER BE DELETED BY NOW          
         BH    MEM799              YES - THATS ALL                              
         CLC   STADAT,LAPSDAT      HAD HE LAPSED BY THIS MONTH ?                
         BNH   MEM745              NO - MUST BE VALID THIS MONTH                
         CLC   STADAT,RENDAT       HAD HE RENEWED BY THIS MONTH ?               
         BL    MEM740              NO - MUST BE LAPSED THIS MONTH               
         CLC   STADAT,DUEDAT       WILL HE LAPSE BY THIS MONTH ?                
         BNH   MEM745              NO - MUST BE VALID THIS MONTH                
MEM740   L     R1,STALAP           ACCUMULATE NOT ACTIVE THIS MONTH             
         LA    R1,1(R1)                                                         
         ST    R1,STALAP                                                        
         CLC   STADAT,DUEDAT       WILL HE LAPSE BY THIS MONTH ?                
         BL    MEM755              NO - GO CHECK OTHER STATES                   
         BE    MEM752              ACCUM IF LAPSES ON THIS MONTH                
         B     MEM790              ELSE - HE'S LAPSED DO NEXT MONTH             
MEM745   L     R1,STAVAL           ACCUMULATE ACTIVE THIS MONTH                 
         LA    R1,1(R1)                                                         
         ST    R1,STAVAL                                                        
MEM750   CLC   STADAT,DUEDAT       WILL HE LAPSE THIS MONTH ?                   
         BNE   MEM755              NO - GO CHECK OTHER STATES                   
MEM752   L     R1,STADUE           ACCUMULATE FELL DUE THIS MONTH               
         LA    R1,1(R1)                                                         
         ST    R1,STADUE                                                        
         B     MEM790                                                           
MEM755   CLC   STADAT,LAPSDAT      WAS HE DUE THIS MONTH ?                      
         BNE   MEM790              NO - END                                     
         L     R1,STADUE           ACCUMULATE FELL DUE THIS MONTH               
         LA    R1,1(R1)                                                         
         ST    R1,STADUE                                                        
         L     R1,STAREN           ACCUMULATE RENEWED TO DATE                   
         LA    R1,1(R1)                                                         
         ST    R1,STAREN                                                        
*                                                                               
MEM790   CLC   STADAT,=X'9912'     END OF TABLE ?                               
         BE    MEM799              YES - END                                    
         LA    R5,STACCL(,R5)      NEXT MONTH ACCUMULATORS                      
         AP    MMP,=P'10'          ADD 1 TO MONTH                               
         CP    MMP,=P'120'         BRANCH IF NOT NEXT YEAR                      
         BNH   MEM730                                                           
         ZAP   MMP,=P'10'          ELSE SET JAN NEXT YEAR                       
         AP    YYP,=P'10'                                                       
         B     MEM730                                                           
         SPACE 3                                                                
MEM799   BAS   RE,PUTARCH          WRITE RECORD TO ARCHIVE TAPE                 
         B     MEM900              AND GO DO NEXT RECORD                        
         DROP  R5                                                               
         SPACE 3                                                                
MEM800   BAS   RE,PUTREC           WRITE RECORD TO ACTIVE TAPE                  
         B     MEM900              AND GO DO NEXT RECORD                        
         SPACE 3                                                                
MEM900   C     R7,ADDSREC          DID RECORD COME FROM ACTVIN                  
         BE    NEXTREC             YES - GO GET ANOTHER ACTVIN RECORD           
         BAS   RE,GETARCH          NO - READ NEXT ARCHIVE RECORD                
         L     R7,ADDSREC                                                       
         B     MEMREC              AND GO PROCESS MEMBER AGAIN                  
         EJECT                                                                  
*        STATISTICS RECORD                                                      
*        -     ADD IT INTO STATS TABLE AND DELETE THE RECORD.                   
*              IT WILL BE REBUILT AT LIST END PROCEDURE                         
*                                                                               
STAREC   CLI   MERSW,0             ARE WE MERGING ARCHIVE DATA                  
         BNE   STA100              YES - DROP ALL STATS RECORDS                 
*                                                                               
         MVC   YY,PEKMSYY          GET YEAR OF STATS                            
         MVI   YYS,X'0C'           SET SIGN                                     
         ZIC   R1,YY               GET YEAR                                     
         SLL   R1,4                SHIFT FOR SIGN                               
         LA    R1,X'0C'(,R1)       INSERT SIGN                                  
         XR    R0,R0                                                            
         STM   R0,R1,DUB                                                        
         XR    RF,RF                                                            
         SP    DUB,=P'51'          GET YEAR RELATIVE TO 1951                    
         BZ    STA010              SKIP IF IT IS 51                             
         BP    *+6                 ENSURE NOT BEFORE 1951                       
         DC    H'0'                DIE IF SO                                    
         CVB   RF,DUB              MAKE IT BINARY                               
         LA    R1,12                                                            
         MR    RE,R1               RF=MONTHS FROM JAN 51 TO START OF YY         
         LA    R1,STACCL                                                        
         MR    RE,R1               RF=OFFSET OF YYMM ACCUMS                     
STA010   A     RF,ASTACCS          POINT TO ACCUMULATOR SET                     
         LR    R5,RF                                                            
         USING STACC,R5                                                         
         LA    R6,PERFRST          POINT TO FIRST STATS EL                      
         USING PEMSTD,R6                                                        
*                                                                               
STA030   CLI   PEMSTEL,0           END OF RECORD ?                              
         BE    STA100              YES - SKIP                                   
         LM    R0,R4,STAVAL        GET ACCUMULATORS                             
         A     R0,PEMSTVAL         ADD IN RECORD DATA                           
         A     R1,PEMSTLAP                                                      
         A     R2,PEMSTDUE                                                      
         A     R3,PEMSTREN                                                      
         A     R4,PEMSTJOI                                                      
         STM   R0,R4,STAVAL                                                     
         LA    R5,STACCL(,R5)                                                   
         LA    R6,PEMSTLNQ(,R6)                                                 
         B     STA030                                                           
*                                                                               
STA100   L     R1,TOTMOD                                                        
         SH    R1,=Y(1)            COUNT RECORDS DROPPED                        
         ST    R1,TOTMOD                                                        
         B     NEXTREC             DROP RECORD ONCE ACCUMULATED                 
         DROP  R5,R6                                                            
         SPACE 3                                                                
         EJECT                                                                  
*        SUBROUTINE READS ACTVIN RECORDS TO DDSREC AREA                         
*        -     CHECK FOR SEQUENCE ERRORS                                        
*        -     MAINTAIN ACCUMULATOR SET FOR EACH RECORD TYPE                    
*                                                                               
GETREC   NTR1                                                                   
         L     R2,=A(ACTVIN)                                                    
         L     R7,ADDSREC                                                       
         MVC   LASTKEY,PEKEY       SAVE KEY OF LAST RECORD READ                 
         SH    R7,=Y(4)                                                         
         GET   (2),(7)             READ A NEW RECORD                            
         AH    R7,=Y(4)                                                         
         L     R1,TOTIN            COUNT RECORD READ                            
         LA    R1,1(,R1)                                                        
         ST    R1,TOTIN                                                         
         CLC   =Y(MAXLEN+4),DDSRECH                                             
         BNL   GET010              BRANCH IF RECORD NOT TOO LONG                
         MVC   P+13(L'ERR13),ERR13 RECORD TOO LONG                              
         LA    R1,L'ERR13                                                       
         MVI   REPSW,X'FF'         NO REPLY                                     
         B     ERRCANC                                                          
EOFIN    MVI   EOFSW,X'FF'         INDICATE EOF OCCURED                         
         MVI   PEKEY,X'FF'         AND FILL KEY                                 
         MVC   PEKEY+1(L'PEKEY-1),PEKEY                                         
GET010   B     GETFRST             BRANCH FIRST TIME ONLY                       
         CLC   PEKEY,LASTKEY       TEST FOR SEQUENCE ERRORS                     
         BH    GET020                                                           
GET015   B     GET020              BRANCH FIRST TIME ONLY                       
         L     R1,TOTDUP                                                        
         LA    R1,1(,R1)                                                        
         ST    R1,TOTDUP           COUNT DUPLICATE KEYS                         
         BE    GET020                                                           
         MVC   P+13(L'ERR06),ERR06 SEQUENCE ERROR                               
         LA    R1,L'ERR06                                                       
         MVI   REPSW,X'FF'         NO REPLY                                     
         B     ERRCANC                                                          
GETFRST  MVI   GET010+1,X'00'      ENABLE SEQUENCE CHECK NEXT TIME              
         MVI   GET015+1,X'00'      ENABLE DPLICATE CHECK NEXT TIME              
GET020   CLI   PEKEY,0             IS IT A HEADER REC                           
         BE    GET100              YES - DONT SET UP ACCUMULATOR                
         USING RECACC,RE                                                        
         ICM   RE,15,CRECACC       POINT TO CURRENT RECORD ACCUMULATOR          
         BZ    GET030              SKIP IF NONE ACTIVE                          
         CLC   RECKEY,PEKEY        SAME RECORD KEY ?                            
         BE    GET050              USE IT IF SO                                 
         LA    RE,RECACCL(,RE)     ELSE NEXT ENTRY                              
         B     GET040              AND INIT IT                                  
GET030   L     RE,ARECACCS         POINT TO FIRST ONE IF NONE                   
GET040   ST    RE,CRECACC                                                       
         XC    RECACC(RECACCL),RECACC                                           
         MVC   RECKEY,PEKEY        SAVE AGY/LST/RECTYPE                         
GET050   L     R1,RECITOT                                                       
         LA    R1,1(,R1)                                                        
         ST    R1,RECITOT                                                       
GET100   B     EXIT                                                             
         DROP  RE                                                               
         SPACE 3                                                                
*        SUBROUTINE WRITES ACTVOUT RECORDS FROM R7                              
*        -     CHECK FOR SEQUENCE ERRORS                                        
*        -     UPDATE ACCUMULATORS FOR EACH RECORD                              
*                                                                               
PUTREC   NTR1                                                                   
         L     R2,=A(ACTVOUT)                                                   
         SH    R7,=Y(4)                                                         
         PUT   (2),(7)                                                          
         AH    R7,=Y(4)                                                         
         L     R1,TOTOUT           COUNT RECORD WRITTEN                         
         LA    R1,1(,R1)                                                        
         ST    R1,TOTOUT                                                        
         USING RECACC,RE                                                        
         ICM   RE,15,CRECACC       POINT TO CURRENT RECORD ACCUMULATOR          
         BZ    EXIT                DONT ACCUMULATE IF NONE                      
         CLC   RECKEY,PEKEY        SAME RECORD KEY ?                            
         BE    PUT100              USE IT IF SO                                 
         BH    *+6                 BRANCH IF HIGH                               
         DC    H'0'                DIE IF INSERT BEFORE END OF LIST             
         LA    R0,RECACCL          ELSE LOOK AT PRIOR ENTRY                     
         SR    RE,R0                                                            
         CLC   RECKEY,PEKEY        SAME RECORD KEY ?                            
         BE    PUT100              USE IT IF SO                                 
         BL    *+6                 BRANCH IF LOW                                
         DC    H'0'                DIE IF INSERT OUT OF SEQ                     
         AR    RE,R0               REPOSITION TO CURRENT ENTRY                  
         MVC   RECACC+RECACCL(RECACCL),RECACC MOVE CURR TO NEXT                 
         AR    R0,RE                                                            
         ST    R0,CRECACC          UPDATE CURRENT POINTER                       
         XC    RECACC(RECACCL),RECACC CLEAR NEW ACCUMULATOR SET                 
         MVC   RECKEY,PEKEY        SAVE AGY/LST/RECTYPE                         
PUT100   LA    RF,RECODEL          POINT TO DELETED COUNTER                     
         TM    PERSTAT,X'80'       DELETED  ?                                   
         BO    PUT110              YES - COUNT IT                               
         LA    RF,RECOLAP          POINT TO LAPSED COUNTER                      
         TM    PERSTAT,X'40'       LAPSED  ?                                    
         BO    PUT110              YES - COUNT IT                               
         LA    RF,RECOACT          POINT TO ACTIVE COUNTER                      
PUT110   L     R1,0(,RF)           COUNT RECORD                                 
         LA    R1,1(,R1)                                                        
         ST    R1,0(,RF)                                                        
         B     EXIT                                                             
         DROP  RE                                                               
         SPACE 3                                                                
*        SUBROUTINE READS ARCHIN RECORDS TO ARCREC AREA                         
*        -     CHECK FOR SEQUENCE ERRORS                                        
*        -     MAINTAIN ACCUMULATORS                                            
*        -     FLUSH OUT AND COUNT IF NO MATCHING LIST ON ACTVIN                
*                                                                               
GETARCH  NTR1                                                                   
         L     R2,=A(ARCHIN)                                                    
         L     R7,AARCREC                                                       
GETARCNX SH    R7,=Y(4)                                                         
         GET   (2),(7)             READ RECORD FROM ARCHIVE FILE                
         AH    R7,=Y(4)                                                         
         L     R1,TOTAIN           COUNT ARCHIVE RECORDS READ                   
         LA    R1,1(,R1)                                                        
         ST    R1,TOTAIN                                                        
         CLC   LASTARCH,PEKEY      MUST BE IN SEQUENCE                          
         BL    ARCHSQOK                                                         
         BE    ARCHCOPY            SKIP DUPLICATES ON ARCHIVE TAPE              
         MVC   P+13(L'ERR15),ERR15                                              
         LA    R1,L'ERR15                                                       
         MVI   REPSW,X'FF'         NO REPLY                                     
         B     ERRCANC                                                          
ARCHSQOK MVC   LASTARCH,PEKEY                                                   
         CLI   LASTARCH+2,X'20'    MUST BE TYPE X'20' (MEMBER) RECORD           
         BE    ARCHRTOK                                                         
         MVC   P+13(L'ERR16),ERR16                                              
         LA    R1,L'ERR16                                                       
         MVI   REPSW,X'FF'         NO REPLY                                     
         B     ERRCANC                                                          
ARCHRTOK CLC   PEKEY,LASTAL        SAME AGY, LIST                               
         BE    ARCHSAME            USE IT IF SO                                 
         BL    ARCHFLSH            IF LOW, LIST NOT ON CUR FILE                 
ARCHNMCH L     R1,ARCHNL           IF HIGH ADD ONE TO NEXT LIST COUNT           
         LA    R1,1(,R1)           TO BE ADDED IN LATER                         
         ST    R1,ARCHNL                                                        
         B     ARCHOK                                                           
ARCHFLSH L     R1,ARCHXTRA         FLUSH UNKNOWN LIST                           
         LA    R1,1(,R1)                                                        
         ST    R1,ARCHXTRA                                                      
ARCHCOPY BAS   RE,PUTARCH          COPY TO O/P ARCH                             
         B     GETARCNX            GET NEXT REC                                 
         USING RECACC,RE                                                        
ARCHSAME ICM   RE,15,CMEMACC       GET MEMREC COUNTER FOR THIS LIST             
         BNZ   *+6                                                              
         DC    H'0'                DIE IF NO MEMREC COUNTER YET                 
         L     R1,RECIARC          COUNT RECORD FOR THIS LIST                   
         LA    R1,1(,R1)                                                        
         ST    R1,RECIARC                                                       
         B     ARCHOK                                                           
ARCHEOF  MVI   PEKEY,X'FF'                                                      
         MVC   PEKEY+1(L'PEKEY-1),PEKEY                                         
         MVI   EOFARCH,X'FF'                                                    
ARCHOK   B     EXIT                                                             
         DROP  RE                                                               
         SPACE 3                                                                
*        SUBROUTINE WRITES ARCHOUT RECORDS FROM R7                              
*        -     CHECK FOR SEQUENCE ERRORS                                        
*        -     MAINTAIN ACCUMULATOR SET FOR EACH RECORD TYPE                    
*                                                                               
PUTARCH  NTR1                                                                   
         L     R2,=A(ARCHOUT)                                                   
         SH    R7,=Y(4)                                                         
         PUT   (2),(7)             WRITE RECORD TO ARCHIVE FILE                 
         AH    R7,=Y(4)                                                         
         L     R1,TOTARCH          COUNT RECORD ARCHIVED                        
         LA    R1,1(,R1)                                                        
         ST    R1,TOTARCH                                                       
         USING RECACC,RE                                                        
         ICM   RE,15,CRECACC       POINT TO CURRENT RECORD ACCUMULATOR          
         BNZ   *+6                 SKIP IF ACTIVE                               
         DC    H'0'                                                             
         CLC   RECKEY,PEKEY        SAME RECORD KEY ?                            
         BNE   EXIT                NO - MUST BE FLUSHING                        
         L     R1,RECARCH          COUNT RECORD                                 
         LA    R1,1(,R1)                                                        
         ST    R1,RECARCH                                                       
         B     EXIT                                                             
         DROP  RE                                                               
         SPACE 3                                                                
*        SUBROUTINE UPDATES CURRENT ACCUMULATOR WHOSE OFFSET IS IN R1           
*                                                                               
ACCUM    A     R1,CRECACC          POINT TO CURRENT ACCUM                       
         L     RF,0(R1)                                                         
         LA    RF,1(,RF)                                                        
         ST    RF,0(R1)                                                         
         BR    RE                                                               
         SPACE 3                                                                
*        SUBROUTINE PRINTS LINE AT P                                            
*                                                                               
PRINTIT  CP    LINE,MAXLINE        ARE WE GOING OVER THE PAGE                   
         BNH   PRINT02             NO - JUST PRINT                              
         MVC   TITLE(L'TIT),TIT    ELSE SET UP HEADINGS                         
         CLI   EOFSW,0             IF EOF                                       
         BNE   PRINT01             SKIP SUB HEADS                               
         MVC   MID1(8),=C'AGENCY ='                                             
         MVC   MID1+9(L'AGYNAM),AGYNAM                                          
         MVC   SUB1,HD1                                                         
         MVC   SUB2,HD2                                                         
         MVC   SUB3,HD3                                                         
         B     PRINT02                                                          
PRINT01  MVC   MID1,SPACES                                                      
         MVC   SUB1,SPACES                                                      
         MVC   SUB2,SPACES                                                      
         MVC   SUB3,SPACES                                                      
PRINT02  ST    RE,SAVERE                                                        
         GOTO1 VPRINTER                                                         
         L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 3                                                                
*        SUBROUTINE LOCATES ELEMENT WHOSE CODE IS IN ELCD.                      
*        -     CC EQUAL IF FOUND. R2 POINTS TO ELEMENT FOUND                    
*        -     NEXTEL ALLOWS SEARCH TO CONTINE FOR SUBSEQUENT ELEMENTS          
*              WITH SAME CODE (ASSUMES R2 POINTS TO PREVIOUS ONE)               
*        -     DOES NOT SAVE REGISTERS. DESTROYS R0                             
*                                                                               
GETEL    LA    R2,PERFRST                                                       
         XR    R0,R0                                                            
*                                                                               
GETEL2   CLI   0(R2),0                                                          
         BE    GETELX                                                           
         CLC   ELCD,0(R2)                                                       
         BER   RE                                                               
GETEL3   IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     GETEL2                                                           
GETELX   CLC   ELCD,0(R2)                                                       
         BR    RE                                                               
NEXTEL   XR    R0,R0               GET                                          
         B     GETEL3                                                           
         SPACE 3                                                                
*        SUBROUTINE WRITES ERROR MESSAGE TO PRINTER AND CONSOLE                 
*        -     P+13 CONTAINS MESSAGE TEXT                                       
*        -     R1 = LENGTH OF TEXT (FOR CONSOLE)                                
*        -     ENTRY ERROUT  WRITES MESSAGE AND RETURNS                         
*        -     ENTRY MESSOUT AS ERROUT BUT WONT SET ERROR FLAG                  
*        -     ENTRY ERRCANC CANCELS AFTER MESSAGE WRITTEN                      
*        -     ENTRY ERRCOPY COPIES RECORD TO OUTPUT AFTER MESSAGE              
*                                                                               
ERROUT   MVI   ERRFLAG,C'Y'        INDICATE HAD ERRORS                          
MESSOUT  NTR1                                                                   
         MVC   P(13),=C'*** PEACTV - '                                          
         LA    RF,13(,R1)                                                       
         GOTO1 VLOGIO,DMCB,(REPSW,1),((RF),P)                                   
         ICM   R2,15,SPACING                                                    
         MVC   SPACING,=C'BL02'                                                 
         BAS   RE,PRINTIT                                                       
         BAS   RE,PRINTIT                                                       
         STCM  R2,15,SPACING                                                    
         CLI   REPSW,0                                                          
         BNE   EXIT                                                             
         GOTO1 VLOGIO,DMCB,0,(64,P+13) READ REPLY IF REQUIRED                   
         MVC   P(13),=C'***(REPLY) - '                                          
         BAS   RE,PRINTIT                                                       
         BAS   RE,PRINTIT                                                       
         B     EXIT                                                             
*                                                                               
ERRCANC  BAS   RE,ERROUT           WRITE ERROR AND CANCEL                       
         B     CANCEL                                                           
*                                                                               
ERRCOPY  BAS   RE,ERROUT           WRITE ERROR AND COPY RECORD OUT              
         B     COPY                                                             
         TITLE 'STORAGE AND LITERALS'                                           
         DS    0D                                                               
VLOGIO   DC    V(LOGIO)                                                         
VPRINTER DC    V(PRINTER)                                                       
VDATCON  DC    V(DATCON)                                                        
VHELLO   DC    V(HELLO)                                                         
VCPRINT  DC    V(CPRINT)                                                        
*                                                                               
ADDSREC  DC    A(DDSREC)                                                        
AARCREC  DC    A(ARCREC)                                                        
ASAVREC  DC    A(SAVREC)                                                        
ASTACCS  DC    A(STACCS)                                                        
ARECACCS DC    A(RECACCS)                                                       
*                                                                               
PERSON   DC    CL8'PERSON'                                                      
*                                                                               
ERR01    DC    C'NULL INPUT FILE'                                               
ERR02    DC    C'FIRST RECORD NOT FILE HEADER'                                  
ERR03    DC    C'FILE IS NOT A PERFIL DUMP TAPE'                                
ERR04    DC    C'END OF INPUT AND TRAILER MISSING'                              
ERR05    DC    C'TRAILER NOT LAST RECORD ON FILE'                               
ERR06    DC    C'INPUT NOT IN SEQUENCE'                                         
ERR07    DC    C'AGENCY HEADER RECORD MISSING'                                  
ERR08    DC    C'DUPLICATE AGENCY HEADER'                                       
ERR09    DC    C'LIST HEADER RECORD MISSING'                                    
ERR10    DC    C'DUPLICATE LIST HEADER'                                         
ERR11    DC    C'TOO MANY FIELD CONTROL RECORDS'                                
ERR12    DC    C'DUPLICATE STATISTICS RECORD'                                   
ERR13    DC    C'RECORD TOO LONG'                                               
ERR14    DC    C'PARAMETER CARD ERROR, JOB CANCELLED'                           
ERR15    DC    C'ARCHIVE INPUT NOT IN SEQUENCE'                                 
ERR16    DC    C'ARCHIVE INPUT INVALID RECORD TYPE'                             
ERRACK   DC    C'INFORM SYSTEMS ABOUT ERRORS. HIT ENTER TO ACKNOWLEDGE'         
*                                                                               
* SYSTAB - INDEX TO RECORD TYPE TABLES AND SOURCE OF DESCRIPTIONS               
*          NOTE FIRST BYTE IS HIGH SYSTEM CODE IE 00-C0=MAIL                    
*                                                                               
SYSTAB   DC    X'C0',AL3(RTYMATAB),C'MAIL AND CLUB   '                          
SYSTABL  EQU   *-SYSTAB                                                         
         DC    X'C1',AL3(RTYIATAB),C'I.A.M.B.I.C.    '                          
         DC    X'C2',AL3(RTYMPTAB),C'M.A.P.          '                          
         DC    X'FF',AL3(RTYXXTAB),C'UNKNOWN         '                          
*                                                                               
RTYMATAB DC    X'00',C'LIST HEADER  ' MAIL LIST RECORD TYPES, SYS=00-C0         
RTYTABL  EQU   *-RTYMATAB                                                       
         DC    X'01',C'LABEL CONTROL'                                           
         DC    X'02',C'FIELD CONTROL'                                           
         DC    X'20',C'MEMBER RECORD'                                           
         DC    X'21',C'SURNAME PASV '                                           
         DC    X'22',C'POSTCODE PASV'                                           
         DC    X'F0',C'STATS RECORD '                                           
         DC    X'FF',C'UNKNOWN      '                                           
*                                                                               
RTYIATAB DC    X'02',C'AGENCY RECORD' IAMBICS RECORD TYPES, SYS=C1              
         DC    X'04',C'CLIENT RECORD'                                           
         DC    X'06',C'PRODUCT RECRD'                                           
         DC    X'08',C'NEW BUSINESS '                                           
         DC    X'0A',C'CONTACT RECRD'                                           
         DC    X'0C',C'CURRENCY CONV'                                           
         DC    X'0D',C'CURRENCY DESC'                                           
*                                                                               
RTYMPTAB DC    X'02',C'PERSON RECORD' MAP RECORD TYPES, SYS=C2                  
         DC    X'03',C'USER   RECORD'                                           
         DC    X'04',C'SYSTEM RECORD'                                           
         DC    X'05',C'PROJECT RECRD'                                           
         DC    X'06',C'TASK   RECORD'                                           
         DC    X'07',C'DIARY  RECORD'                                           
         DC    X'08',C'SCRATCH RECRD'                                           
         DC    X'FF',C'UNKNOWN      '                                           
*                                                                               
RTYDRTAB DC    X'20',CL13'DRIVER ENTRY'                                         
         DC    X'FF',CL13'UNKNOWN'                                              
*                                                                               
RTYXXTAB DC    X'FF',C'RECORD       ' UNKNOWN SYSTEM TYPES, SYS=C1-FF           
*                                                                               
TIT      DC    C'PERSON FILE MAINTENANCE'                                       
HD1      DC    CL14' ',CL99' RECORDS  ARCHIVE *--- RECORD STATUS CHANGEX        
               D TO ----*  LAPSED  *-------- RECORDS OUTPUT ---------*'         
         DC    CL19' '                                                          
HD2      DC    CL14'RECORD TYPE',CL99'   INPUT  RECORDS   LAPSED   ACTIX        
               VE  NOT-NEW      NEW  RECORDS   ACTIVE   LAPSED  DELETEDX        
                   TOTAL'                                                       
         DC    CL19' '                                                          
HD3      DC    13C'-',C' ',CL9'--------',CL9'   INPUT ',4CL9'--------'          
         DC    CL9'ARCHIVED',4CL9'--------',CL19' '                             
*                                                                               
         LTORG                                                                  
         TITLE 'DSECTS'                                                         
WRKAREA  DSECT                     WORKING STORAGE                              
DMCB     DS    3D                                                               
DUB      DS    D                                                                
FULL     DS    F                                                                
SAVERE   DS    F                                                                
TOTIN    DS    F                   TOTAL RECORDS READ FROM ACTVIN               
TOTOUT   DS    F                   TOTAL RECORDS WRITTEN TO ACTVOUT             
TOTARCH  DS    F                   TOTAL RECORDS WRITTEN TO ARCHOUT             
TOTAIN   DS    F                   TOTAL RECORDS READ FROM ARCHIN               
TOTMOD   DS    F                   TOTAL RECORDS ADDED/DELETED                  
ARCHXTRA DS    F                   TOTAL UNMATCHED RECORDS ARCHIN               
ARCHNL   DS    F                   ARCHIN RECORDS FOR LATER LIST                
TOTDUP   DS    F                   TOTAL DUPLICATE RECORDS READ                 
CRECACC  DS    A                   ADDRESS OF CURRENT RECORD TYPE ACCUM         
CMEMACC  DS    A                   ADDRESS OF MEMBER REC ACCUM FOR LIST         
AACTEL   DS    A                   ADDRESS OF ACTIVITY ELEMENT                  
ARENEL   DS    A                   ADDRESS OF RENEWAL ELEMENT                   
WORK     DS    CL64                                                             
AGYNAM   DS    CL30                ALPHA NAME OF AGENCY                         
LASTARCH DS    CL36                LAST ARCHIN KEY READ                         
LASTKEY  DS    0CL36               LAST KEY READ                                
LASTALR  DS    0CL3                AGY/LIST/RECTYPE LAST RECORD                 
LASTAL   DS    0CL2                AGY/LIST LAST RECORD                         
LASTAGY  DS    X                   AGY LAST RECORD                              
LASTLST  DS    X                   LIST NUMBER OF LAST RECORD                   
LASTTYP  DS    X                   RECORD TYPE OF LAST RECORD                   
         DS    CL33                REMAINDER OF KEY                             
ACTWORK  DS    CL50                WORK AREA FOR ACTIVITY ELEMENT               
EOFSW    DS    X                   NON-ZERO = HAD EOF ON INPUT                  
EOFARCH  DS    X                   NON-ZERO = HAD EOF ON ARCHIVE                
RENSW    DS    X                   NON-ZERO = RENEWAL SUPPORTED ON LIST         
MERSW    DS    X                   NON-ZERO = LIST ARCHIVE BEING MERGED         
STASW    DS    X                   ZERO     = ALL STATS ZERO FOR YEAR           
REPSW    DS    X                   FF       = MESSOUT NOT TO READ REPLY         
ELCD     DS    X                   ELEMENT CODE REQUIRED FOR GETEL              
ERRFLAG  DS    C                   C'Y' MEANS HAD SERIOUS ERROR                 
YYP      DS    0PL2                PACKED YEAR (X'YY0C')                        
YY       DS    X                   PWOS YEAR (X'YY')                            
YYS      DS    X                   SIGN                                         
MMP      DS    0PL2                PACKED MONTH (X'MM0C')                       
MM       DS    X                   PWOS MONTH (X'MM')                           
MMS      DS    X                   SIGN                                         
PERLAPSE DS    PL2                 PERIOD TO HOLD LAPSED RECS (X'MM0C'          
TODAY    DS    XL3                 PWOS TODAYS DATE                             
ONEMONTH DS    XL3                 PWOS DATE ONE MONTH AGO                      
LAPMONTH DS    XL3                 PWOS DATE PERLAPSE MONTHS AGO                
JOINDAT  DS    XL2                 PWOS YYMM JOINED                             
DUEDAT   DS    XL2                 PWOS YYMM FELL DUE                           
DELDAT   DS    XL2                 PWOS YYMM DELETED FROM FILE                  
RENDAT   DS    XL2                 PWOS YYMM RENEWED PREVIOUSLY                 
LAPSDAT  DS    XL2                 PWOS YYMM LAPSED PREVIOUSLY                  
STADAT   DS    XL2                 PWOS YYMM CURRENT ACCUMULATOR                
PARMTAB  DS    10XL2               10X AGY/LIST FROM PARMS                      
*                                                                               
WRKLEN   EQU   *-WRKAREA                                                        
         SPACE 3                                                                
STACC    DSECT                     STATS ACCUMULATOR LINE (ONE MONTH)           
STAVAL   DS    F                   MEMBERS VALID DURING MONTH                   
STALAP   DS    F                   MEMBERS LAPSED DURING MONTH                  
STADUE   DS    F                   MEMBERS FALLING DUE DURING MONTH             
STAREN   DS    F                   DUE MEMBERS WHO EVENTUALLY RENEWED           
STAJOI   DS    F                   MEMBERS JOINING DURING MONTH                 
*                                                                               
STACCL   EQU   *-STACC                                                          
         SPACE 3                                                                
RECACC   DSECT                     RECORD COUNT LINE (ONE RECORD TYPE)          
RECKEY   DS    0CL3                RECORD KEY                                   
RECAGY   DS    X                   AGENCY                                       
RECLIST  DS    X                   LIST                                         
RECTYPE  DS    X                   RECORD TYPE                                  
         DS    C                   SPARE                                        
RECITOT  DS    F                   NUMBER READ (FROM ACTVIN)                    
RECIARC  DS    F                   NUMBER READ (FROM ACTVIN)                    
RECCLAP  DS    F                   NUMBER CHANGED TO LAPSED                     
RECCACT  DS    F                   NUMBER CHANGED TO ACTIVE (UNLAPSED)          
RECCOLD  DS    F                   NUMBER CHANGED TO OLD (NOT-NEW)              
RECCNEW  DS    F                   NUMBER CHANGED TO NEW (UNUSUAL)              
RECARCH  DS    F                   NUMBER TO ARCHOUT INSTEAD OF ACTVOUT         
RECOACT  DS    F                   ACTIVE RECORDS TO ACTVOUT                    
RECOLAP  DS    F                   LAPSED RECORDS TO ACTVOUT                    
RECODEL  DS    F                   DELETED RECORDS TO ACTVOUT                   
*                                                                               
RECACCL  EQU   *-RECACC                                                         
         TITLE 'DTFS AND I/O AREAS'                                             
PEACTV   CSECT                                                                  
*                                                                               
*        ACTVIN - DUMP OF PERFIL                                                
*                                                                               
ACTVIN   DCB   DDNAME=ACTVIN,RECFM=VB,DSORG=PS,EODAD=EOFIN,MACRF=GM             
*                                                                               
*        ACTVOUT - DUMP OF PERFIL WITH MEMBERSHIP MAINTENENCE APPLIED           
*                                                                               
ACTVOUT  DCB   DDNAME=ACTVOUT,RECFM=VB,DSORG=PS,MACRF=PM                        
*                                                                               
*        ARCHIN - ARCHIVE INPUT                                                 
*                                                                               
ARCHIN   DCB   DDNAME=ARCHIN,RECFM=VB,DSORG=PS,EODAD=ARCHEOF,MACRF=GM           
*                                                                               
*        ARCHOUT - COPY OF EXPIRED MEMBER RECORDS DELETED FROM ACTVOUT          
*                                                                               
ARCHOUT  DCB   DDNAME=ARCHOUT,RECFM=VB,DSORG=PS,MACRF=PM                        
*                                                                               
MAXLEN   EQU   3*512                                                            
         DS    0D                                                               
DDSRECH  DS    CL4                                                              
DDSREC   DS    CL(MAXLEN)                                                       
*                                                                               
ARCRECH  DS    CL4                                                              
ARCREC   DS    CL(MAXLEN)                                                       
*                                                                               
SAVRECH  DS    CL4                                                              
SAVREC   DS    CL(MAXLEN)                                                       
*                                                                               
RCWORK   DS    512D                DYNAMIC WORK AREA                            
         SPACE 3                                                                
STACCS   DS    (50*12)CL(STACCL)   STATISTICAL ACCUMULATORS                     
         SPACE 3                                                                
RECACCS  DS    20CL(RECACCL)       RECORD COUNTERS                              
         SPACE 3                                                                
*PEGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE PEGENFILE                                                      
         PRINT ON                                                               
*DDDPRINT                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'041PEACTV    03/19/15'                                      
         END                                                                    
