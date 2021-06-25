*          DATA SET ACSCR19    AT LEVEL 012 AS OF 08/27/15                      
*PHASE T60C19A,+0                                                               
         TITLE 'PROFIT AND LOSS PROFILE ELEMENTS EXTRA MAINTANCE'               
T60C19   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,60C19,RA,RR=RE                                                 
*                                                                               
         USING TWAD,R5                                                          
         USING WORKD,R7                                                         
         USING LWSD,RC                                                          
         L     RC,APALOCAL                                                      
*                                                                               
         ST    RE,APRELO                                                        
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
         ST    RD,APWORKA                                                       
*                                                                               
         CLC   ATWA,ACURSOR        INSURE     CURSOR    WAS  IN A FIELD         
         BE    SCR01               NO,   SET  CURSOR                            
         CLI   TWALREC,RECPNLPF    IF    1ST  TIME IN,  THEN SET CURSOR         
         BE    SCR02               NO,   SKIP                                   
*                                                                               
SCR01    LA    RE,PNLCODEH         FORMAT     CODE FIELD                        
         ST    RE,ACURSOR                                                       
*                                                                               
SCR02    DS    0H                                                               
         CLI   APMODE,APMVALK                                                   
         BNE   SCR10                                                            
         LA    R2,SCRTXT           TABLE OF   SCREEN    UPDATES                 
*                                                                               
SCR05    CLI   0(R2),X'FF'         END   OF   TABLE?                            
         BE    SCR10               FINISHED                                     
         L     R4,0(,R2)           GET   HEADER    ADDRESS                      
         AR    R4,R5                                                            
         OI    6(R4),FVOXMT        TRANSMIT   FIELD                             
         L     R3,4(,R2)           GET   SCREEN    TEXT NUMBER                  
         GOTO1 TEXTGET,APPARM,(R3),(R4),0                                       
         LA    R2,8(,R2)           BUMP  TO   NEXT                              
         B     SCR05                                                            
*                                                                               
         USING RESRECD,R2                                                       
SCR10    LA    R2,IOKEY                                                         
         SR    RF,RF                                                            
         IC    RF,APMODE                                                        
         SLL   RF,2                                                             
         B     *(RF)                                                            
*                                                                               
         B     VALKEY                                                           
         B     VALREC                                                           
         B     DISKEY                                                           
         B     DISREC                                                           
         B     EXIT                                                             
         B     EXIT                RESTORE                                      
         B     EXIT                VALSEL                                       
         B     EXIT                GETSEL                                       
         B     EXIT                DISSEL                                       
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                LSTSCR                                       
         B     EXIT                VALREQ                                       
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         EJECT ,                                                                
***********************************************************************         
*  MODULE/SUBROUTINE EXIT LOGIC                                       *         
***********************************************************************         
         SPACE 1                                                                
EXIT     CLI   APPFKEY,PFKNEXT                                                  
         BE    EXIT97                                                           
         CLI   APMODE,APMVALK                                                   
         BE    EXIT95                                                           
*                                                                               
EXIT30   TM    TWASWPST,TWASWAP    SWAP  TO   NEW  RECORD ACTION ?              
         BZ    EXIT95              NO                                           
         XC    APCURSOR,APCURSOR   DON'T SET CURSOR ON WRONG SCREEN             
         MVI   APMODE,APMSWP             SWAP                                   
         MVC   APPARM(1),TWASWPRE        SWAP RECORD                            
         MVC   APPARM+1(1),TWASWPAC      SWAP ACTION                            
*                                                                               
EXIT95   OI    TWALSCTL,TWALSHLD                                                
*                                                                               
EXIT97   OI    TWALSCTL,TWALSRTN                                                
*                                                                               
EXIT99   DS    0H                                                               
         CLI   APMODE,APMVALR      IN    VALIDATE  RECORD                       
         BNE   EXIT999             NO,   SKIP                                   
         CLC   FVMSGNO,=AL2(FVFOK) ANY   ERRORS    FOUND ?                      
         BNE   EXIT999             YES,  SKIP                                   
         CLI   APPFKEY,0           ANY   PF   KEY  DEPRESSED ?                  
         BNE   EXIT999             YES,  SKIP                                   
         TM    TWASWPST,TWASWAP    SWAP  TO   NEW  RECORD    ACTION ?           
         BO    EXIT999             YES,  SKIP                                   
         MVC   APCURSOR,ACURSOR    NO,   SET  APPLICATION    CURSOR             
*                                                                               
EXIT999  CLC   FVMSGNO,=AL2(FVFOK) ANY   ERRORS    FOUND ?                      
         BE    XIT                 NO,   EXIT                                   
         CLI   APMODE,APMDISR      MODE  IS   DISPLAY   RECORD ?                
         BNE   XIT                                                              
         OI    APINDS2,APIOVROK    SET   OVERLAY   IS   HAPPY                   
*                                                                               
XIT      XIT1                                                                   
         EJECT ,                                                                
***********************************************************************         
*  VALIDATE KEY                                                       *         
***********************************************************************         
         SPACE 1                                                                
VALKEY   DS    0H                                                               
         MVI   NEWKEY,NO                                                        
         MVC   RESKEY,SPACES                                                    
         MVI   RESKTYP,RESKTYPQ    X'2D'                                        
         MVI   RESKSUB,RESKSUBQ    X'02'                                        
         MVC   RESKCPY,CUABIN      COMPANY    CODE                              
         GOTO1 AFVAL,PNLCODEH                                                   
         MVC   RESKFORM,FVIFLD                                                  
         BE    VALKEY15                                                         
         TM    PNLCODE+4,FVITHIS   ANY   INPUT?                                 
         BZ    *+10                NO                                           
         MVC   SAVFORM,SPACES                                                   
         CLC   SAVFORM,SPACES                                                   
         BNH   IVALEKEY                                                         
         MVC   PNLCODE,SAVFORM                                                  
         OI    PNLCODEH+6,FVOXMT   TRANSMIT                                     
         MVC   RESKFORM,SAVFORM                                                 
*                                                                               
VALKEY15 MVC   SAVFORM,RESKFORM                                                 
         MVC   APRECKEY(L'RESKEY),RESKEY                                        
         LA    R1,IORD+IOACCFIL+IO1                                             
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(,R1)      READ  FOR  UPDATE                            
         GOTO1 AIO                                                              
         BL    VALKEY99            IO    ERROR                                  
         BNE   VALKEY20                                                         
         L     R2,AIOAREA1                                                      
         GOTO1 GETTYPE,(R2)                                                     
         OI    SCRTYPH+6,FVOXMT                                                 
         MVC   SCRTYP(L'APREPCDE),APREPCDE                                      
*        CLC   APREPCDE,AC@PNL     IS    IT   P&L  ?                            
*        BNE   IVALRPTY                                                         
*                                                                               
         MVI   APINDS,APIOKDIS+APIOKCHA+APIOKDEL                                
         TM    TWAMODE,TWAMDFR     2ND   PASS IN   ACTION    COPY ?             
         BZ    VALKEY98                                                         
         CLI   APACTN,ACTCPY                                                    
         BNE   VALKEY98                                                         
         OI    APINDS,APIOKADD     TURN  ON   TO   TRICK    ACTION COPY         
         B     VALKEY98                                                         
*                                                                               
VALKEY20 TM    IOERR,IOEDEL        IS    RECORD    MARKED    DELETED            
         BNZ   VALKEY99            OK    TO   ADD  RECORD                       
         MVI   APINDS,APIOKADD     RECORD     NOT ON FILE, SO OK TO ADD         
         L     R2,AIOAREA1                                                      
         XC    RESKEY(256),RESKEY  RESET AIO  AREA                              
*                                                                               
VALKEY98 MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALKEY99 B     EXIT                                                             
         EJECT ,                                                                
***********************************************************************         
*  DISPLAY KEY                                                        *         
***********************************************************************         
         SPACE 1                                                                
DISKEY   NI    TWASWPST,TURNOFF-TWASWAP                                         
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*  VALIDATE RECORD                                                    *         
***********************************************************************         
         SPACE 1                                                                
VALREC   DS    0H                                                               
         L     R2,AIOAREA1                                                      
         GOTO1 AFVAL,PNLNMEH                                                    
         BNE   VALREC05            NAME  HAS  NOT  BEEN INPUT                   
         GOTO1 ADDNAME,APPARM,(R2),PNLNMEH    ADD  FORMAT    NAME               
         BNE   VALREC99            ON    ERROR,    EXIT                         
*                                                                               
*                                  ************************************         
VALREC05 DS    0H                  * VALIDATE REPORT TYPE             *         
*                                  ************************************         
*                                                                               
         USING REPTABD,R3                                                       
         GOTO1 AFVAL,PNLTYPEH                                                   
         L     R3,ACTYPTAB                                                      
*                                                                               
VALREC10 CLI   REPCODE,EOT                                                      
         BE    IVALTYPE                                                         
         GOTO1 EXPRPTY,(R3)                                                     
         CLC   APREPCDE,PNLRPCDE   MATCH REPORT    TYPE ?                       
         BNE   VALREC15            NO,   SO   GET  NEXT REPORT    TYPE          
         ZIC   R1,FVXLEN           GET   INPUT     LENGTH                       
         EXCLC R1,PNLRPTYP,FVIFLD  INPUT REPORT    TYPE ?                       
         BNE   VALREC15            NO,   SO   GET  NEXT REPORT    TYPE          
         TM    REPFLAG,REPDDS      DDS   ONLY?                                  
         BZ    VALREC18            NO,   SO   OK                                
         TM    CUSTAT,CUSDDS       IS    IT   A    DDS  TERMINAL ?              
         BNZ   VALREC18            YES,  SO   OK                                
*                                                                               
VALREC15 LA    R3,REPLNQ(,R3)      BUMP  TO   NEXT REPORT    TYPE               
         B     VALREC10                                                         
*                                                                               
         USING ACTRECD,R2                                                       
VALREC18 MVC   APREPNUM,REPNUM     SAVE  REPORT    NUMBER                       
         MVC   APREPCDE,PNLRPCDE   SAVE  REPORT    CODE                         
         DROP  R3                                                               
*                                                                               
         GOTO1 ADDREPTY,APPARM,AIOAREA1                                         
         BNE   VALREC99            ON    ERROR,    EXIT                         
         GOTO1 GETTYPE,(R2)        RESET APREP     VALUES                       
         MVI   LDGFLDH,10          SET   DEFAULT   LENGTH                       
         MVC   LDGFLD,APREPUL                                                   
         GOTO1 MAKELIST,APPARM,('RFLLDG',LDGFLDH),(R2),                X        
               ('MAXPARM',PNLBLOCK),APELEM                                      
         GOTO1 ADDEL,(R2)          ADD   X'C5'     LEDGER    FILTER             
         BNE   VALREC99            ON    ERROR,    EXIT                         
*                                                                               
*                                  ************************************         
*                                  * BUILD PROFILE DATA ELEMENT       *         
*                                  ************************************         
*                                                                               
         USING RESRECD,R2                                                       
         USING RPFELD,R9                                                        
         LA    R9,APELEM           BUILD DEFAULT   PROFILE   DATA               
         MVC   RESKEY,APRECKEY                                                  
         XC    APELEM,APELEM                                                    
         MVI   RPFEL,RPFELQ        ELEMENT    CODE                              
         MVI   RPFLN,RPFLNQ        LENGTH     OF   ELEMENT                      
         OI    RPFPOPT,RPFBOX      TURN  ON   BIT  FOR  BOXES                   
         OI    RPFEDOPT,RPFEDCMA+RPFEDTRL     COMMAS,   TRAILING MINUS          
         MVI   RPFPCTS,C'2'        DECIMAL    PLACES    FOR  PERCENTS           
         MVI   RPFRND,PENNY        ROUNDING   OPTION                            
         MVI   APELCODE,RPFELQ     GET   PROFILE   ELEMENT                      
         GOTO1 GETEL,(R2)                                                       
         BNE   VALREC25                                                         
         SR    RF,RF                                                            
         IC    RF,1(,R1)                                                        
         BCTR  RF,0                                                             
         EXMVC RF,APELEM,0(R1)                                                  
         GOTO1 DELEL,(R2)                                                       
         DROP  R2                                                               
*                                  ************************************         
VALREC25 DS    0H                  * PROCESS OPTIONS                  *         
*                                  ************************************         
         MVI   RPFROPT,0           CLEAR FLAGS                                  
         MVI   RPFOPT1,0                                                        
         MVI   RPFOPT2,0                                                        
         MVI   RPFOPT3,0                                                        
         MVI   RPFOPT4,0                                                        
         MVI   RPFOPT5,0                                                        
*                                                                               
         XC    RPFFLT1(4),RPFFLT1  CLEAR FILTER    AREA                         
         MVI   RPFFLT5,0           CLEAR FILTER    5                            
         LA    R4,5                FIVE  FILTER    TYPES                        
         LA    R3,RPFFLT1                                                       
         LA    R1,PNLF1H                                                        
*                                                                               
VALREC27 GOTO1 AFVAL                                                            
         BNE   VALREC28                                                         
         MVC   0(1,R3),FVIFLD                                                   
         CLI   FVIFLD,C'*'         EXCLUDE    FILTER ?                          
         BNE   VALREC28                                                         
         MVC   0(1,R3),FVIFLD+1                                                 
         NI    0(R3),TURNOFF-X'40' MAKE  LOWER     CASE                         
*                                                                               
VALREC28 SR    RE,RE                                                            
         IC    RE,0(,R1)           BUMP  TO   PROTECTED FIELD                   
         AR    R1,RE                                                            
         IC    RE,0(,R1)           BUMP  TO   NEXT INPUT     FILTER             
         AR    R1,RE                                                            
         LA    R3,1(,R3)           BUMP  TO NEXT FILTER IN PROFILE ELEM         
         CLM   R4,1,=AL1(2)        F1-F4 DONE YET  ?                            
         BNE   *+8                 NOT   YET                                    
         LA    R3,RPFFLT5          SWITCH     TO   F5                           
         BCT   R4,VALREC27                                                      
*                                                                               
         MVI   SAVEMTHD,C' '                                                    
         MVI   APELCODE,RPFELQ     X'C4'                                        
         L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)          ADD   PROFILE   ELEMENT   X'C4'              
         BNE   VALREC99            ON    ERROR,    EXIT                         
         MVC   SAVEKEY1,IOKEY                                                   
         DROP  R9                                                               
*                                                                               
*                                  ************************************         
VALREC30 DS    0H                  * VALIDATE SPECIFIC ACCOUNTS       *         
*                                  ************************************         
         GOTO1 MAKELIST,APPARM,('RFLACC',PNLACCTH),(R2),               X        
               ('MAXPARM',PNLBLOCK),APELEM                                      
         LTR   RF,RF               TEST  THE  RETURN    CODE                    
         BM    VALREC38            NO    INPUT                                  
         BP    VALREC99            BAD   INPUT                                  
*                                  GOOD  INPUT                                  
         GOTO1 VALLIST,APPARM,APREPUL,(PNLBLOCK,PNLBLOCK+12),1                  
         LTR   RF,RF               TEST  THE  RETURN    CODE                    
         BM    VALREC31            NO    LIST                                   
         BP    VALREC99            BAD   LIST                                   
*                                  GOOD  LIST                                   
         CLI   NPARMS,1            ONLY  ONE  LIST REQUESTED ?                  
         BE    VALRC33A            YES,  ADD  SPECIFIC  ACCOUNT ELEMENT         
         B     IVAL2MNY            NO,   TOO  MANY LISTS                        
*                                                                               
VALREC31 DS    0H                  VALIDATE   ACCOUNT(S)                        
         SR    R6,R6                                                            
         IC    R6,NPARMS                                                        
         LA    R4,PNLBLOCK                                                      
*                                                                               
         USING ACTRECD,R2                                                       
         LA    R2,IOKEY                                                         
VALREC32 DS    0H                                                               
         CLI   0(R4),0             IS    THE  LEN  OF   ACCOUNT = 0             
         BE    VALREC33            YES,  SKIP                                   
         CLI   0(R4),LACCOUNT      IS    THE  LEN  OF   ACCOUNT > 12 ?          
         BH    IVALACCT            YES,  INVALID   ACCOUNT                      
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUABIN      COMPANY    CODE                              
*                                  UNIT  LEDGER                                 
         MVC   ACTKUNT(LUNLG),APREPUL                                           
         MVC   CURUL,APREPUL       CURRENT    UNIT/LEDGER                       
         MVC   ACTKACT,12(R4)      ACCOUNT    IS   IN   PNLBLOCK                
         GOTO1 AIO,IORD+IOACCFIL+IO2                                            
         BE    VALREC33            VALID ACCOUNT                                
         MVI   BYTE,C'N'           WITHOUT    U/L (FOR  WILDCARD)               
         BAS   RE,WILDCARD                                                      
         BNE   IVALACCT            INVALID    ACCOUNT                           
*                                                                               
VALREC33 LA    R4,32(,R4)          NEXT  BLOCK     IN   PNLBLOCK                
         BCT   R6,VALREC32                                                      
         DROP  R2                                                               
*                                                                               
VALRC33A DS    0H                                                               
         L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)          ADD   SPECIFIC  ACCOUNT   ELEMENT            
         BNE   VALREC99            ON    ERROR,    EXIT                         
*                                                                               
*                                  ************************************         
VALREC38 DS    0H                  * VALIDATE CLIENT LIST             *         
*                                  ************************************         
         GOTO1 MAKELIST,APPARM,('RFLCLI',PNLCLIH),(R2),                X        
               ('MAXPARM',PNLBLOCK),APELEM                                      
         LTR   RF,RF               TEST  THE  RETURN    CODE                    
         BM    VALREC43            NO    INPUT                                  
         BP    VALREC99            BAD   INPUT                                  
*                                  GOOD  INPUT                                  
*                                                                               
         GOTO1 VALLIST,APPARM,=C'SJ',(PNLBLOCK,PNLBLOCK+12),0                   
         LTR   RF,RF               TEST  THE  RETURN    CODE                    
         BM    VALREC39            NO    LIST                                   
         BP    VALREC99            BAD   LIST                                   
*                                  GOOD  LIST                                   
         CLI   NPARMS,2            ONLY  ALLOWED   ON   FOR  A    LIST          
         BL    VALREC42            ADD   THE  VALID     LIST                    
         BH    IVAL2MNY                                                         
         GOTO1 VALLIST,APPARM,=C'SJ',                                  X        
               (PNLBLOCK+L'PNLBLOCK,PNLBLOCK+L'PNLBLOCK+12),0                   
         LTR   RF,RF               TEST  THE  RETURN    CODE                    
*                                  GOOD  LIST                                   
         BZ    VALREC42            ADD   THE  VALID     LISTS                   
         BP    VALREC99            BAD   LIST                                   
*                                  NO    LIST                                   
*                                  BUT - MUST BE   A    LIST                    
         MVC   FVMSGNO,=AL2(ACEIVLT)                                            
         B     VALREC99            THEREFORE, THIS IS   A    BAD  LIST          
*                                                                               
VALREC39 DS    0H                                                               
         MVC   SAVEUL,ACLEDGER     SAVE  U/L  INFORMATION                       
         LA    R1,=C'SJ'                                                        
         GOTO1 GETLEDG,(R1)        FIND  SIZE OF   LVL  1                       
         MVC   SVACLEV1,ACLEV1     SAVE  LVL  1    SIZE                         
         GOTO1 GETLEDG,SAVEUL      RESET UNIT/LEDGER                            
*                                                                               
         SR    R6,R6                                                            
         IC    R6,NPARMS                                                        
         LA    R4,PNLBLOCK                                                      
*                                                                               
         USING ACTRECD,R2                                                       
         LA    R2,IOKEY                                                         
VALREC40 DS    0H                                                               
         CLC   0(1,R4),SVACLEV1    IS    THE  LEN  OF   CLIENT >  MAX ?         
         BH    IVALCLI             YES,  INVALID   CLIENT                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUABIN      COMPANY    CODE                              
         MVC   ACTKUNT(2),=C'SJ'   UNIT  LEDGER                                 
         MVC   ACTKACT,12(R4)      CLIENT                                       
         GOTO1 AIO,IORD+IOACCFIL+IO2                                            
         BE    VALREC41            VALID CLIENT                                 
         MVI   BYTE,C'N'                                                        
         BAS   RE,WILDCARD                                                      
         BNE   IVALCLI             INVALID    CLIENT                            
*                                                                               
VALREC41 LA    R4,32(,R4)          NEXT  BLOCK     IN   PNLBLOCK                
         BCT   R6,VALREC40                                                      
         DROP  R2                                                               
*                                                                               
VALREC42 DS    0H                                                               
         L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)          ADD   CLIENT    LIST                         
         BNE   VALREC99            ON    ERROR,    EXIT                         
*                                                                               
*                                  ************************************         
VALREC43 DS    0H                  * VALIDATE OFFICE                  *         
*                                  ************************************         
         GOTO1 MAKELIST,APPARM,('RFLOFF',PNLOFFH),(R2),                X        
               ('MAXPARM',PNLBLOCK),APELEM                                      
         LTR   RF,RF               TEST  THE  RETURN    CODE                    
         BM    VALREC50            NO    INPUT                                  
         BP    VALREC99            BAD   INPUT                                  
*                                  GOOD  INPUT                                  
*                                                                               
         USING RFLELD,R4                                                        
         MVI   BYTE,NO             ASSUME INCLUDE                               
         LA    R4,APELEM           ->     ELEMENT                               
         TM    RFLIND,RFLXCLD      EXCLUDE     FILTER ?                         
         BZ    *+8                 NO,    SKIP                                  
         MVI   BYTE,YES            SAY    EXCLUDE                               
         DROP  R4                                                               
*                                                                               
         ZIC   R4,NPARMS           NUMBER OF   OFFICES                          
         GOTO1 VOFFICE,APPARM,('VOFTYPEB',PNLBLOCK),(BYTE,(R4))                 
         BNE   VALREC99            BAD    INPUT                                 
*                                                                               
         L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)          ADD   OFFICE    LIST                         
         BNE   VALREC99            ON    ERROR,    EXIT                         
*                                                                               
*                                  ************************************         
VALREC50 DS    0H                  * VALIDATE TRANSACTION TYPE(S)     *         
*&&DO                              ************************************         
         GOTO1 MAKELIST,APPARM,('RFLTTYPE',PNLITTYH),(R2),             X        
               ('MAXPARM',PNLBLOCK),APELEM                                      
         LTR   RF,RF               TEST  THE  RETURN    CODE                    
         BM    VALREC55            NO    INPUT                                  
         BP    VALREC99            BAD   INPUT                                  
*                                  GOOD  INPUT                                  
         USING RFLELD,R9                                                        
         SR    R6,R6                                                            
         IC    R6,NPARMS           SCAN  ENTRIES = NUM  OF   TYPES              
         LA    R9,APELEM                                                        
         LA    RF,RFLLNQ(,R6)      NEW   LENGTH    OF   ELEMENT                 
         STC   RF,RFLLN            STORE IN   ELEMENT                           
         GOTO1 CNVTTYPE,APPARM,(C'N',PNLBLOCK),(NPARMS,RFLDATA)                 
         BNE   VALREC99                                                         
         L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)          ADD   INCLUDE   TRANS     TYPE               
         BNE   VALREC99            ON    ERROR,    EXIT                         
         DROP  R9                                                               
*&&                                                                             
VALREC55 GOTO1 MAKELIST,APPARM,('RFLMTHD',PNLMTHDH),(R2),              X        
               ('MAXPARM',PNLBLOCK),APELEM                                      
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VALREC60            NO   INPUT                                   
         BP    VALREC99            BAD  INPUT                                   
*                                  GOOD INPUT                                   
         BAS   RE,VALMTHD          TEST FOR VALID                               
         BNE   VALREC99            NOT  VALID, EXIT                             
*                                                                               
         USING RFLELD,R1                                                        
         LA    R1,APELEM           ->   ELEMENT AREA                            
         MVI   RFLLN,RFLLNQ+1      CHANGE LENGTH                                
         MVC   RFLDATA(1),SAVEMTHD ALWAYS USE NUMBER INSTEAD OF CODE            
         DROP  R1                                                               
*                                                                               
         L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)          ADD  METHOD    ELEMENT                       
         BNE   VALREC99            ON   ERROR,    EXIT                          
         EJECT                                                                  
VALREC60 DS    0H                                                               
         MVC   IOKEY,SAVEKEY1                                                   
*                                  ************************************         
VALREC95 DS    0H                  * UPDATE THE RECORD WITH ALL THE   *         
*                                  * NEW ELEMENTS                     *         
*                                  ************************************         
         GOTO1 ADDID,APPARM,AIOAREA1                                            
         BNE   VALREC99            ON    ERROR,    EXIT                         
         LA    R1,IOADD+IOACCFIL+IO1                                            
         TM    APINDS,APIOKADD     ADD   A    RECORD ?                          
         BO    VALREC97                                                         
         LA    R1,IOWRITE+IOACCFIL+IO1                                          
         TM    APINDS,APIOKCHA     CHANGE     A    RECORD ?                     
         BO    VALREC97                                                         
         DC    H'0'                WHAT  THE  HELL ?                            
*                                                                               
VALREC97 GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD   WRITE     OR   SOMETHING DUDE          
*                                                                               
VALREC99 CLC   FVMSGNO,=AL2(FVFOK)                                              
         BE    DISREC                                                           
         B     EXIT                                                             
         EJECT ,                                                                
***********************************************************************         
*  VALIDATE METHOD TYPE                                               *         
***********************************************************************         
         SPACE 1                                                                
         USING RFLELD,R1                                                        
         USING CAHRECD,R2                                                       
VALMTHD  NTR1                                                                   
         LA    R1,APELEM           ->   ELEMENT AREA                            
         LA    R2,IOKEY            ->   KEY  AREA                               
         LA    R4,PNLBLOCK         ->   SCAN BLOCK     AREA                     
*                                                                               
         CLI   NPARMS,1            ONLY ONE  METHOD    REQUESTED ?              
         BNE   VALMTHE1            NO,  TOO  MANY PARAMETERS                    
*                                                                               
         TM    RFLIND,RFLXCLD      EXCLUDE   REQUESTED ?                        
         BO    VALMTHER            YES, INVALID   METHOD                        
         DROP  R1                                                               
*                                                                               
         MVC   CAHKEY,SPACES                                                    
         MVI   CAHKTYP,CAHKTYPQ    X'3E'                                        
         MVC   CAHKCPY,CUABIN      COMPANY                                      
         CLI   0(R4),1             ONLY ONE  CHARACTER ?                        
         BH    VALMTH10            NO,  TRY  CODE                               
         TM    2(R4),X'80'         IS   IT   NUMBERIC ?                         
         BZ    VALMTH10            NO,  TRY  CODE                               
         CLI   12(R4),C'1'         LESS THAN C'1' ?                             
         BL    VALMTHER            NO,  INVALID   METHOD                        
         MVI   CAHKSUB,CAHKSUBQ    NUMERIC   =    X'01'                         
         MVC   CAHKMTHD,12(R4)     INSERT    NUMBER                             
         XC    CAHKOFC,CAHKOFC     CLEAR     OFFICE/OFFICE GROUP                
         B     VALMTH20            READ RECORD                                  
*                                                                               
         USING CMTRECD,R2                                                       
VALMTH10 CLI   0(R4),L'CMTKMTHD    MORE THAN 3    CHARACTERS ?                  
         BH    VALMTHER            YES, INVALID   METHOD                        
         MVI   CMTKSUB,CMTKSUBQ    CODE      =    X'02'                         
         MVC   CMTKMTHD,12(R4)     INSERT    CODE                               
*                                                                               
VALMTH20 GOTO1 AIO,IO3+IOACCFIL+IORD                                            
         BNE   VALMTHER            INVALID   METHOD                             
*                                                                               
         USING METELD,R1                                                        
         L     R1,AIOAREA3                                                      
         MVC   SVELEM,APELEM       SAVE      APELEM                             
         XC    APELEM,APELEM       CLEAR     APELEM                             
         MVI   APELCODE,METELQ     X'82'     METHOD    ELEMENT                  
         GOTO1 GETEL                                                            
         BNE   VALMTHER            INVALID   METHOD                             
*                                                                               
         MVC   APELEM,SVELEM       RESTORE   APELEM                             
         MVC   SAVEMTHD,METNUM     SAVE      METHOD    NUMBER                   
         MVC   PNLMTHD,METCODE     DISPLAY   THE  CODE                          
         OI    PNLMTHDH+6,FVOXMT   TRANSMIT                                     
         B     VALMTHEX                                                         
*                                                                               
VALMTHE1 MVC   FVMSGNO,=AL2(ACE2MANY)        TOO  MANY PARAMETERS               
         B     VALMTHEX                                                         
*                                                                               
VALMTHER MVC   FVMSGNO,=AL2(1650)            INVALID   METHOD                   
*                                                                               
VALMTHEX CLC   FVMSGNO,=AL2(FVFOK) SET  CC                                      
         B     XIT                 EXIT                                         
         DROP  R1,R2                                                            
         EJECT ,                                                                
***********************************************************************         
*  TEST FOR A WILD CARD CHARACTER                                     *         
***********************************************************************         
         SPACE 1                                                                
WILDCARD NTR1                                                                   
         SR    R1,R1                                                            
         ICM   R1,1,0(R4)          LENGTH                                       
         LA    RF,12(,R4)          START OF   ACCOUNT                           
*                                                                               
         CLI   BYTE,C'U'           CHECK UNIT/LEDGER                            
         BNE   WILD200                                                          
*                                                                               
         LA    R2,2                JUST  CHECKING  U/L                          
WILD100  CLI   0(RF),C'?'          U/L   HAS  WILDCARD ?                        
         BE    WILDNO              WILDCARD   INVALID                           
         LA    RF,1(,RF)           NEXT  CHAR                                   
         BCTR  R1,0                                                             
         BCT   R2,WILD100                                                       
*                                                                               
WILD200  CLI   0(RF),C'?'          CONTRA     U/L  HAS  WILDCARD ?              
         BE    WILDYES             WILDCARD   INVALID                           
         LA    RF,1(,RF)           NEXT  CHAR                                   
         BCT   R1,WILD200                                                       
*                                                                               
WILDNO   LTR   RE,RE               WILDCARD  INVALID / U/L HAS WILDCARD         
         B     XIT                                                              
*                                                                               
WILDYES  LA    R1,APELEM                                                        
*                                                                               
         USING RFLELD,R1                                                        
*                                                                               
         OI    RFLIND,RFLWILD      TURN  ON   THE  BIT                          
         CR    RE,RE               WILDCARD   IS   GOOD                         
         B     XIT                                                              
*                                                                               
         DROP  R1                                                               
         EJECT ,                                                                
***********************************************************************         
*  DISPLAY PROFIT AND LOSS (P&L) PROFILE DATA                         *         
***********************************************************************         
         SPACE 1                                                                
DISREC   L     R2,AIOAREA1         PROFILE    ELEMENT                           
         TWAXC PNLNMEH,PNLTABH                                                  
         GOTO1 GETNAME,APPARM,(R2),PNLNMEH                                      
         GOTO1 GETPER,APPARM,(R2),PNLOWNH                                       
*                                                                               
         MVC   PNLTYPE(L'APREPTYP),APREPTYP                                     
         MVI   SAVEMTHD,C' '                                                    
*                                                                               
         USING RFLELD,R1                                                        
         MVI   APELCODE,RFLELQ     FILTER/LIST     ELEMENT   X'C5'              
         GOTO1 GETEL,(R2)                                                       
         BNE   DISREC30                                                         
*                                                                               
DISREC10 CLI   RFLSEQ,0            HIGH  LEVEL     FILTER    ONLY ?             
         BNE   DISREC25                                                         
         SR    RF,RF                                                            
         SR    R6,R6                                                            
         IC    R6,RFLLN                                                         
         SH    R6,=Y(RFLLNQ+1)                                                  
         BM    DISREC25            SKIP  ELEMENT   IF  LENGTH TOO SMALL         
*                                                                               
         CLI   RFLTYPE,RFLACC      ACCOUNT    FILTER   LIST                     
         BNE   *+8                                                              
         LA    RF,PNLACCTH         SPECIFIC   ACCOUNT  HEADER                   
*                                                                               
         CLI   RFLTYPE,RFLCLI      CLIENT     LIST                              
         BNE   *+8                                                              
         LA    RF,PNLCLIH          SPECIFIC   CLIENT   HEADER                   
*                                                                               
         CLI   RFLTYPE,RFLOFF      OFFICE     FILTER   LIST                     
         BNE   *+8                                                              
         LA    RF,PNLOFFH          OFFICE HEADER                                
*&&DO                                                                           
         CLI   RFLTYPE,RFLTTYPE    INCLUDE    FILTER   TRANS     TYPE           
         BNE   DISREC15                                                         
         LR    R0,R1                                                            
         GOTO1 CNVTTYPE,APPARM,(C'S',(R0)),(X'00',PNLITTY)                      
         LR    R1,R0                                                            
         OI    PNLITTYH+6,FVOXMT                                                
         SR    RF,RF                                                            
*&&                                                                             
DISREC15 DS    0H                                                               
         LTR   RF,RF               ANY   DATA TO   DISPLAY   ?                  
         BZ    DISREC22            NO,   SKIP                                   
         OI    6(RF),FVOXMT        TURN  TRANSMIT  ON                           
         TM    RFLIND,RFLXCLD      EXCLUDE    BIT  ON   ?                       
         BZ    DISREC20            NO,   MOVE THE  DATA                         
         MVI   8(RF),C'*'          OUTPUT     C'*'                              
         LA    RF,1(,RF)           BUMP  DATA LOCATION                          
*                                                                               
DISREC20 DS    0H                                                               
         EXMVC R6,8(RF),RFLDATA    INSERT     THE  DATA                         
*                                                                               
DISREC22 CLI   RFLTYPE,RFLMTHD     METHOD                                       
         BNE   *+10                                                             
         MVC   SAVEMTHD,RFLDATA                                                 
*                                                                               
DISREC25 DS    0H                                                               
         GOTO1 NEXTEL                                                           
         BE    DISREC10                                                         
*                                                                               
         USING RPFELD,R9                                                        
DISREC30 MVI   APELCODE,RPFELQ                                                  
         GOTO1 GETEL,(R2)                                                       
         BNE   DISREC99                                                         
         LR    R9,R1                                                            
*                                                                               
         LA    R4,5                FIVE  FILTER    TYPES                        
         LA    R3,RPFFLT1                                                       
         LA    R1,PNLF1H                                                        
*                                                                               
DISREC32 CLI   0(R3),X'40'         ANY   CHARACTER ?                            
         BNH   DISREC34                                                         
         OI    6(R1),FVOXMT        TRANSMIT                                     
         MVC   8(1,R1),0(R3)                                                    
         TM    0(R3),X'40'         LOWER OR   UPPER     CASE ?                  
         BNZ   DISREC34            UPPER                                        
         MVI   8(R1),C'*'          EXCLUDE    FILTER                            
         MVC   9(1,R1),0(R3)                                                    
         OI    9(R1),X'40'         MAKE  UPPER     CASE                         
*                                                                               
DISREC34 SR    RE,RE                                                            
         IC    RE,0(,R1)           BUMP  TO   PROTECTED FIELD                   
         AR    R1,RE                                                            
         IC    RE,0(,R1)           BUMP  TO   NEXT INPUT     FILTER             
         AR    R1,RE                                                            
         LA    R3,1(,R3)           BUMP  TO NEXT FILTER IN PROFILE ELEM         
         CLM   R4,1,=AL1(2)        F1-F4 DONE YET  ?                            
         BNE   *+8                 NOT   YET                                    
         LA    R3,RPFFLT5          SWITCH     TO   F5                           
         BCT   R4,DISREC32                                                      
*                                                                               
         BAS   RE,DISMTHD          DISPLAY METHOD                               
*                                                                               
DISREC99 DS    0H                                                               
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   DISRC999                                                         
         CLI   APACTN,ACTDIS       IN    DISPLAY   MODE ?                       
         BE    DISRC999            YES,  SKIP                                   
         CLI   TWALREC,RECPNLPF    IF    1ST  TIME IN,  THEN SET CURSOR         
         BE    DISRC999            NO,   SKIP                                   
*                                                                               
         LA    RE,PNLCODEH         FORMAT     CODE FIELD                        
         ST    RE,APCURSOR         SET   APPLICATION    CURSOR                  
*                                                                               
DISRC999 DS    0H                                                               
         B     EXIT                                                             
         DROP  R1,R9                                                            
         EJECT ,                                                                
***********************************************************************         
*  DISPLAY METHOD TYPE                                                *         
***********************************************************************         
         SPACE 1                                                                
         USING CAHRECD,R2                                                       
DISMTHD  NTR1                                                                   
         CLI   SAVEMTHD,C' '       METHOD TYPE BLANK OR NULL ?                  
         BNH   DISMTHDX            YES, NO METHOD TYPE, SO EXIT                 
*                                                                               
         LA    R2,IOKEY                                                         
         MVC   CAHKEY,SPACES                                                    
         MVI   CAHKTYP,CAHKTYPQ    X'3E'                                        
         MVI   CAHKSUB,CAHKSUBQ    X'01'                                        
         MVC   CAHKCPY,CUABIN      COMPANY                                      
         MVC   CAHKMTHD,SAVEMTHD                                                
         XC    CAHKOFC,CAHKOFC                                                  
         GOTO1 AIO,IO3+IOACCFIL+IORD                                            
         BNE   DISMTHDX                                                         
*                                                                               
         USING METELD,R1                                                        
         L     R1,AIOAREA3                                                      
         MVI   APELCODE,METELQ     X'82' METHOD ELEMENT                         
         GOTO1 GETEL                                                            
         BNE   DISMTHDX                                                         
         MVC   PNLMTHD,METCODE                                                  
         OI    PNLMTHDH+6,FVOXMT                                                
*                                                                               
DISMTHDX B     XIT                                                              
         DROP  R1,R2                                                            
         EJECT ,                                                                
***********************************************************************         
* EXPAND REPORT TYPE                                                  *         
***********************************************************************         
         SPACE 1                                                                
         USING REPTABD,R1                                                       
EXPRPTY  MVC   PNLRPCDE,REPCODE                                                 
         MVC   PNLRPTYP,REPSTYPE                                                
         CLI   PNLRPCDE,ESCHIGHQ   TEST  FOR  ESCAPE    SEQUENCE                
         BNLR  RE                                                               
         DROP  R1                                                               
*                                                                               
         STM   RE,R1,SVREGS                                                     
         GOTO1 VDICTAT,APPARM,C'TU  ',('PNLRPLNQ',PNLRPCDE),0                   
         LM    RE,R1,SVREGS                                                     
         BR    RE                                                               
         EJECT ,                                                                
***********************************************************************         
* ERRORS TO DISPLAY ON TOP OF SCREEN                                  *         
***********************************************************************         
         SPACE 1                                                                
IVALEKEY MVC   FVMSGNO,=AL2(FVFEKEY)      ENTER KEY                             
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALEDUP MVC   FVMSGNO,=AL2(ACEDUPR)      DUPLICATE PARAMETER                   
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVAL2MNY MVC   FVMSGNO,=AL2(ACE2MANY)     TOO MANY PARAMETERS                   
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALACCT MVC   FVMSGNO,=AL2(ACEACTLV)     INVALID ACCOUNT OR LEVEL              
         MVC   FVXTRA(20),12(R4)                                                
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALCLI  MVC   FVMSGNO,=AL2(ACECLI)       INVALID CLIENT                        
         MVC   FVXTRA(20),12(R4)                                                
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALTYPE MVC   FVMSGNO,=AL2(ACERPTYP)     INVALID REPORT TYPE                   
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALEXIT DS    0H                                                               
         B     EXIT                                                             
         EJECT ,                                                                
SCRTXT   DS    0F                                                               
         DC    AL4(PNLPL02H-TWAD,1652)                                          
         DC    X'FF'                                                            
         EJECT ,                                                                
         LTORG                                                                  
         SPACE 1                                                                
         DROP  RA                                                               
         EJECT ,                                                                
***********************************************************************         
*  DSECT FOR LOCAL WORKING STORAGE                                    *         
***********************************************************************         
         SPACE 1                                                                
LWSD     DSECT                                                                  
MAXPARM  EQU   20                                                               
PENNY    EQU   C'P'                                                             
*                                                                               
SVREGS   DS    6A                                                               
PNLRPCDE DS    CL(L'REPCODE)                                                    
PNLRPTYP DS    CL(L'REPSTYPE)                                                   
PNLRPLNQ EQU   *-PNLRPCDE                                                       
PNLBLOCK DS    (MAXPARM)CL32                                                    
SAVEKEY1 DS    CL(L'RESKEY)                                                     
CURUL    DS    CL2                 CURRENT UNIT/LEDGER                          
SAVEUL   DS    CL2                 SAVE  UNIT/LEDGER                            
SVACLEV1 DS    XL1                 SAVE  LENGTH OF LEVEL 1 ENTRY                
SAVEMTHD DS    CL1                                                              
LDGFLDH  DS    CL8                                                              
LDGFLD   DS    CL2                                                              
BYTE     DS    CL1                                                              
SVELEM   DS    CL(L'APELEM)                                                     
LWSX     DS    0C                                                               
         EJECT ,                                                                
*        ACSCRWRK                                                               
         PRINT OFF                                                              
       ++INCLUDE ACSCRWRK                                                       
         PRINT ON                                                               
         EJECT ,                                                                
***********************************************************************         
*  DSECT FOR P&L PROFILE DEFINITIONS                                  *         
***********************************************************************         
         SPACE 1                                                                
TWAD     DSECT                                                                  
         ORG   SCROVLYH                                                         
       ++INCLUDE ACSCRE2D                                                       
         PRINT ON                                                               
         ORG                                                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012ACSCR19   08/27/15'                                      
         END                                                                    
