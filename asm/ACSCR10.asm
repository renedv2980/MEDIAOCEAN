*          DATA SET ACSCR10    AT LEVEL 009 AS OF 09/02/15                      
*PHASE T60C10A,+0                                                               
***********************************************************************         
*                                                                     *         
* THIS VERSION MATCHES THE UK VERSION LEVEL 08 AS OF 05/09/06         *         
*                                                                     *         
***********************************************************************         
*                                                                               
         TITLE 'G/L PROFILE ELEMENTS EXTRA MAINTANCE'                           
T60C10   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,60C10,RA,RR=RE                                                 
*                                                                               
         USING TWAD,R5                                                          
         USING WORKD,R7                                                         
         USING LWSD,RC                                                          
*                                                                               
         L     RC,APALOCAL                                                      
*                                                                               
         ST    RE,APRELO                                                        
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
         ST    RD,APWORKA                                                       
*                                                                               
*                                                                               
         CLC   ATWA,ACURSOR        INSURE     CURSOR    WAS  IN A FIELD         
         BE    SCR01               NO,   SET  CURSOR                            
         CLI   TWALREC,RECGNLPF    IF    1ST  TIME IN,  THEN SET CURSOR         
         BE    SCR02               NO,   SKIP                                   
*                                                                               
SCR01    LA    RE,GNLCODEH         FORMAT     CODE FIELD                        
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
*                                                                               
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
         SPACE 1                                                                
VALKEY   DS    0H                                                               
         MVI   NEWKEY,NO                                                        
         MVC   RESKEY,SPACES                                                    
         MVI   RESKTYP,RESKTYPQ    X'2D'                                        
         MVI   RESKSUB,RESKSUBQ    X'02'                                        
         MVC   RESKCPY,CUABIN      COMPANY    CODE                              
         GOTO1 AFVAL,GNLCODEH                                                   
         MVC   RESKFORM,FVIFLD                                                  
         BE    VALKEY15                                                         
         TM    GNLCODE+4,FVITHIS   ANY   INPUT?                                 
         BZ    *+10                NO                                           
         MVC   SAVFORM,SPACES                                                   
         CLC   SAVFORM,SPACES                                                   
         BNH   IVALEKEY                                                         
         MVC   GNLCODE,SAVFORM                                                  
         OI    GNLCODEH+6,FVOXMT   TRANSMIT                                     
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
*        CLC   APREPCDE,AC@GNL     IS    IT   G/L ?                             
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
         SPACE 1                                                                
DISKEY   NI    TWASWPST,TURNOFF-TWASWAP                                         
         B     EXIT                                                             
*                                                                               
         DROP  R2                  KEEP  IT   CLEAN                             
         EJECT ,                                                                
         SPACE 1                                                                
VALREC   DS    0H                                                               
         L     R2,AIOAREA1                                                      
         GOTO1 AFVAL,GNLNMEH                                                    
         BNE   VALREC05            NAME  HAS  NOT  BEEN INPUT                   
         GOTO1 ADDNAME,APPARM,(R2),GNLNMEH    ADD  FORMAT    NAME               
         BNE   VALREC99            ON    ERROR,    EXIT                         
*                                                                               
*                                  ************************************         
VALREC05 DS    0H                  * VALIDATE UNIT/LEDGER             *         
*                                  ************************************         
         GOTO1 MAKELIST,APPARM,('RFLLDG',GNLTYPEH),AIOAREA1,           X        
               ('MAXPARM',GNLBLOCK),APELEM                                      
         LTR   RF,RF               TEST  THE  RETURN    CODE                    
         BM    IVALCULR            NO    INPUT                                  
         BP    VALREC99            BAD   INPUT                                  
*                                  GOOD  INPUT                                  
         CLI   NPARMS,MAXLDG       TOO   MANY LEDGERS ?                         
         BH    IVAL2MNY            YES,  ERROR     TOO  MANY                    
         LA    R1,APELEM           ->    FILTER    DATA                         
*                                                                               
         USING RFLELD,R1           MAP   FILTER    DATA ELEMENT                 
*                                                                               
         TM    RFLIND,RFLXCLD      EXCLUDE   REQUESTED ?                        
         BO    IVALCULR            YES,  INVALID   UNIT/LEDGER                  
*                                                                               
         DROP  R1                  KEEP  IT   CLEAN                             
*                                                                               
         XC    MULTLDG,MULTLDG     CLEAR                                        
         SR    R0,R0                                                            
         IC    R0,NPARMS                                                        
         LA    R4,GNLBLOCK                                                      
*                                                                               
         MVI   LDGUSED,0           CLEAR LEDGERS   COUNT                        
*                                  CLEAR LEDGERS   SAVE AREA                    
         MVC   LDGSAVE(LDGSAVEL),SPACES                                         
*                                                                               
         USING REPTABD,R3                                                       
*                                                                               
         L     R3,ACTYPTAB                                                      
*                                                                               
VALREC10 CLI   REPCODE,EOT                                                      
         BE    IVALCUL0                                                         
         GOTO1 EXPRPTY,(R3)                                                     
         CLC   APREPCDE,GNLRPCDE   MATCH REPORT    TYPE                         
         BNE   VALREC15            NO,   SO   GET  NEXT REPORT    TYPE          
         CLI   0(R4),2             MUST  BE   LENGTH    TWO  FOR  NOW           
         BNE   IVALCUL0            NO,   INVALID   UNIT/LEDGER                  
         CLI   REPNUM,C'M'                                                      
         BE    VALREC15            NOT   THE  DEFAULT                           
         CLC   REPUL,12(R4)                                                     
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
*                                                                               
VALREC18 LA    R2,IOKEY                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUABIN      COMPANY    CODE                              
         MVC   ACTKUNT(2),REPUL    UNIT/LEDGER                                  
         GOTO1 AIO,IOREAD+IOACCFIL+IO2                                          
         L     R2,AIOAREA1                                                      
         MVC   IOKEY,0(R2)                                                      
         BNE   IVALCUL0            NOT   VALID    LEDGER    FOR COMPANY         
*                                                                               
         MVC   TEMPLDG,REPIND      GET   THIS LEDGER'S BIT  NUMBER              
         NC    TEMPLDG,MULTLDG     'AND' WITH CURR MULTI-LEDGER  BITS           
         BNZ   IVALEDUP            NOT   ZERO,     ERR DUPLICATE LEDGER         
         OC    MULTLDG,REPIND      GOOD, SAVE THIS LEDGER'S BIT  NUMBER         
         ZIC   RE,LDGUSED          GET   NUM  OF   LEDGERS  SO   FAR            
         SLL   RE,1                TIMES TWO                                    
         LA    RF,LDGSAVE          GET   ADDR OF   LEDGER   SAVE AREA           
         AR    RF,RE               PLUS  BYTES     USED SO  FAR                 
         MVC   0(LUNLG,RF),12(R4)  SAVE  UNIT/LEDGER                            
         SRL   RE,1                GET   NUM  OF   LEDGERS  SO   FAR            
         LA    RE,1(,RE)           ADD   ONE                                    
         STC   RE,LDGUSED          NEW   NUM  OF   LEDGERS  USED                
*                                                                               
         LA    R4,32(,R4)          BUMP  TO   NEXT PARMETER                     
         MVC   APREPNUM,REPNUM                                                  
         L     R3,ACTYPTAB                                                      
         BCT   R0,VALREC10                                                      
*                                                                               
         DROP  R3                  KEEP  IT   CLEAN                             
*                                                                               
         L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)          ADD   REPORT    TYPE ELEMENT X'C5'           
         BNE   VALREC99            ON    ERROR,    EXIT                         
         CLI   NPARMS,1            ONLY  ONE  PARAMETER ?                       
         BE    *+8                 YES                                          
         MVI   APREPNUM,C'M'       MULTI LEDGER                                 
         GOTO1 ADDREPTY,APPARM,(R2)                                             
         BNE   VALREC99            ON    ERROR,    EXIT                         
         GOTO1 GETTYPE,(R2)        RESET APREP     VALUES                       
         MVC   APREPIND,MULTLDG                                                 
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
         MVI   RPFPCTS,C'2'                                                     
         MVI   RPFRND,PENNY                                                     
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
*&&US*&& MVI   RPFOPT7,0                                                        
*                                                                               
         XC    RPFFLT1(4),RPFFLT1  CLEAR FILTER    AREA                         
         MVI   RPFFLT5,0           CLEAR FILTER    5                            
         LA    R4,5                FIVE  FILTER    TYPES                        
         LA    R3,RPFFLT1                                                       
         LA    R1,GNLF1H                                                        
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
*                                  ************************************         
*                                  * EXCLUDE RETAINED EARNINGS        *         
*                                  ************************************         
*&&US                                                                           
         GOTO1 VALYNO,GNLRETH                                                   
         BNE   VALREC29                                                         
         CLC   GNLRET,APNO                                                      
         BNE   *+8                                                              
         OI    RPFOPT7,RPFXRTEA    EXCLUDE RETAINED EARNINGS                    
         CLC   GNLRET,APONLY                                                    
         BNE   *+8                                                              
         OI    RPFOPT7,RPFORTEA    RETAINED EARNINGS ONLY                       
*&&                                                                             
VALREC29 L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)          ADD   PROFILE   ELEMENT   X'C4'              
         BNE   VALREC99            ON    ERROR,    EXIT                         
         MVC   SAVEKEY1,IOKEY                                                   
*                                                                               
         DROP  R9                  KEEP  IT   CLEAN                             
*                                                                               
*                                  ************************************         
VALREC30 DS    0H                  * VALIDATE SPECIFIC ACCOUNTS       *         
*                                  ************************************         
         GOTO1 MAKELIST,APPARM,('RFLACC',GNLACCTH),(R2),               X        
               ('MAXPARM',GNLBLOCK),APELEM                                      
         LTR   RF,RF               TEST  THE  RETURN    CODE                    
         BM    VALREC35            NO    INPUT                                  
         BP    VALREC99            BAD   INPUT                                  
*                                  GOOD  INPUT                                  
         GOTO1 VALLIST,APPARM,(LDGUSED,LDGSAVE),                       X        
               (GNLBLOCK,GNLBLOCK+12),1                                         
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
         LA    R4,GNLBLOCK                                                      
         LA    R2,IOKEY                                                         
*                                                                               
         USING ACTRECD,R2                                                       
*                                                                               
VALREC32 DS    0H                                                               
         CLI   0(R4),0             IS    THE  LEN  OF   ACCOUNT = 0             
         BE    VALREC33            YES,  SKIP                                   
         CLI   0(R4),LULACNT       IS    THE  LEN  OF   ACCOUNT > 14 ?          
         BH    IVALACCT            YES,  INVALID   ACCOUNT                      
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUABIN      COMPANY    CODE                              
         CLI   0(R4),LUNLG         IS    THE  LEN  OF   ACCOUNT <= 2 ?          
         BNH   IVALACCT            YES,  INVALID   ACCOUNT                      
*                                  ACCOUNT    IS   IN   GNLBLOCK                
         MVC   ACTKUNT(LULACNT),12(R4)                                          
         GOTO1 AIO,IORD+IOACCFIL+IO2                                            
         BE    VALRC32A            VALID ACCOUNT                                
         MVI   BYTE,C'U'           U/L   LEDGER    FOR  WILDCARD                
         BAS   RE,WILDCARD                                                      
         BNE   IVALACCT            INVALID    ACCOUNT                           
*                                                                               
VALRC32A DS    0H                  MAKE  SURE USING     VALID    LEDGER         
         ZIC   RE,LDGUSED          GET   NUM  OF   VALID     LEDGERS            
         LA    RF,LDGSAVE          ->    LEDGER    SAVE AREA                    
*                                                                               
VALRC32B DS    0H                  COMPARE    WITH SAVE AREA                    
         CLC   0(LUNLG,RF),12(R4)  MATCHES    UNIT LEDGER    SAVE AREA?         
         BE    VALREC33            YES,  VALID     ACCOUNT                      
         LA    RF,LUNLG(,RF)       BUMP  TO   NEXT LEDGER                       
         BCT   RE,VALRC32B         LOOP  TO   COMPARE NEXT SAVED LEDGER         
         B     IVALACCT            INVALID    ACCOUNT                           
*                                                                               
VALREC33 LA    R4,32(,R4)          NEXT  BLOCK     IN   GNLBLOCK                
         BCT   R6,VALREC32                                                      
*                                                                               
         DROP  R2                  KEEP  IT   CLEAN                             
*                                                                               
VALRC33A DS    0H                                                               
         L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)          ADD   SPECIFIC  ACCOUNT   ELEMENT            
         BNE   VALREC99            ON    ERROR,    EXIT                         
*                                                                               
*                                  ************************************         
VALREC35 DS    0H                  * VALIDATE CONTRA ACCOUNT(S)       *         
*                                  ************************************         
         L     R2,AIOAREA1                                                      
         GOTO1 MAKELIST,APPARM,('RFLCNTR',GNLCNTRH),(R2),              X        
               ('MAXPARM',GNLBLOCK),APELEM                                      
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VALREC43            NO   INPUT                                   
         BP    VALREC99            BAD  INPUT                                   
*                                  GOOD INPUT                                   
         MVC   APWORK(2),SPACES    INDICATE CONTRA ACCOUNT                      
         GOTO1 VALLIST,APPARM,APWORK,(GNLBLOCK,GNLBLOCK+12),1                   
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VALREC37            NO   LIST                                    
         BP    VALREC36            BAD  LIST                                    
*                                  GOOD LIST                                    
         CLI   NPARMS,1            ONLY 1 LIST REQUESTED ?                      
         BE    VALREC40            YES, ADD CONTRA LIST                         
         B     IVAL2MNY            NO,  TOO MANY LISTS                          
*                                                                               
VALREC36 DS    0H                  SHOW WHICH CONTRA CODE IS BAD                
*                                  INSERT NAME                                  
         MVC   FVXTRA(20),GNLBLOCK+12                                           
         B     VALREC99            GENERATE INVALID LIST                        
*                                                                               
VALREC37 DS    0H                  VALIDATE CONTRA ACCOUNT(S)                   
         SR    R6,R6                                                            
         IC    R6,NPARMS                                                        
         LA    R4,GNLBLOCK                                                      
         LA    R2,IOKEY                                                         
*                                                                               
         USING ACTRECD,R2                                                       
*                                                                               
VALREC38 DS    0H                  VALIDATE A CONTRA ACCOUNT                    
         CLI   0(R4),0             ANY  ACCOUNT REQUESTED ?                     
         BE    VALREC39            NO,  CHECK NEXT BLOCK                        
         CLI   0(R4),LULACNT       IS   THE LENGTH OF CONTRA  > 14 ?            
         BH    IVALCNTR            YES, INVALID CONTRA ACCOUNT                  
         CLI   0(R4),LUNLG         IS   THE LENGTH OF CONTRA  <  2 ?            
         BL    IVALCNTR            YES, INVALID CONTRA ACCOUNT                  
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUABIN      COMPANY CODE                                 
*                                                                               
*                                  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!         
*                                  ! NOTE: SINCE THIS MODULE IS FOR   !         
*                                  !       GENERAL LEDGER, WE WILL NOT!         
*                                  !       VALIDATE AGAINST THE       !         
*                                  !       CONTRA TABLE.  ALL         !         
*                                  !       UNIT/LEDGERS ARE VALID.    !         
*                                  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!         
*                                                                               
*                                  VALIDATE THAT THE RECORD EXISTS              
         MVC   ACTKUNT(14),12(R4)  CONTRA ACCOUNT IS IN GNLBLOCK                
         GOTO1 AIO,IORD+IOACCFIL+IO2                                            
*                                                                               
*                                  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!         
*                                  ! NOTE: WILD CARD CHARACTERS ARE   !         
*                                  !       NOT VALID FOR CONTRA       !         
*                                  !       ACCOUNTS.                  !         
*                                  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!         
*                                                                               
         BNE   IVALCNTR            NOT VALID, INVALID CONTRA ACCOUNT            
*                                                                               
*                                                                               
VALREC39 DS    0H                  CHECK NEXT BLOCK                             
         LA    R4,32(,R4)          NEXT BLOCK IN GNLBLOCK                       
         BCT   R6,VALREC38                                                      
*                                                                               
         DROP  R2                  KEEP IT CLEAN                                
*                                                                               
VALREC40 DS    0H                                                               
         L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)          ADD  CONTRA ACCOUNT ELEMENT                  
         BNE   VALREC99            ON    ERROR,    EXIT                         
*                                                                               
*                                  ************************************         
VALREC43 DS    0H                  * VALIDATE OFFICE                  *         
*                                  ************************************         
         GOTO1 MAKELIST,APPARM,('RFLOFF',GNLOFFH),(R2),                X        
               ('MAXPARM',GNLBLOCK),APELEM                                      
         LTR   RF,RF               TEST  THE  RETURN    CODE                    
         BM    VALREC50            NO    INPUT                                  
         BP    VALREC99            BAD   INPUT                                  
*                                  GOOD  INPUT                                  
*                                                                               
         USING RFLELD,R4                                                        
*                                                                               
         MVI   BYTE,NO             ASSUME INCLUDE                               
         LA    R4,APELEM           ->     ELEMENT                               
         TM    RFLIND,RFLXCLD      EXCLUDE     FILTER ?                         
         BZ    *+8                 NO,    SKIP                                  
         MVI   BYTE,YES            SAY    EXCLUDE                               
*                                                                               
         DROP  R4                  KEEP   IT   CLEAN                            
*                                                                               
         ZIC   R4,NPARMS           NUMBER OF   OFFICES                          
         GOTO1 VOFFICE,APPARM,('VOFTYPEB',GNLBLOCK),(BYTE,(R4))                 
         BNE   VALREC99            BAD    INPUT                                 
*                                                                               
         L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)          ADD    OFFICE    LIST                        
         BNE   VALREC99            ON     ERROR,    EXIT                        
*                                                                               
*                                  ************************************         
VALREC50 DS    0H                  * VALIDATE TRANSACTION TYPE(S)     *         
*                                  ************************************         
         GOTO1 MAKELIST,APPARM,('RFLTTYPE',GNLITTYH),(R2),             X        
               ('MAXPARM',GNLBLOCK),APELEM                                      
         LTR   RF,RF               TEST  THE  RETURN    CODE                    
         BM    VALREC55            NO    INPUT                                  
         BP    VALREC99            BAD   INPUT                                  
*                                  GOOD  INPUT                                  
         USING RFLELD,R9                                                        
*                                                                               
         SR    R6,R6                                                            
         IC    R6,NPARMS           SCAN  ENTRIES = NUM  OF   TYPES              
         LA    R9,APELEM                                                        
         LA    RF,RFLLNQ(,R6)      NEW   LENGTH    OF   ELEMENT                 
         STC   RF,RFLLN            STORE IN   ELEMENT                           
         GOTO1 CNVTTYPE,APPARM,(C'N',GNLBLOCK),(NPARMS,RFLDATA)                 
         BNE   VALREC99                                                         
         L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)          ADD   INCLUDE   TRANS     TYPE               
         BNE   VALREC99            ON    ERROR,    EXIT                         
         DROP  R9                                                               
*                                                                               
VALREC55 DS    0H                  LABEL FOR ANY FUTURE FILTERS                 
*                                                                               
         MVC   IOKEY,SAVEKEY1                                                   
*                                                                               
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
*---------------------------------------------------------------------*         
* WILDCARD                                                            *         
*---------------------------------------------------------------------*         
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
*&&US                                                                           
***********************************************************************         
*  VALIDATE A FIELD FOR YES, NO, OR ONLY                              *         
*                                                                     *         
*        ON ENTRY - R1      = ADDRESS OF HEADER FOR FIELD             *         
*        ON EXIT  - FVMSGNO = FVFOK OR FVFNOTV OR FVMISS              *         
*                   CC      = EQUAL     I.E. FVMSGNO = FVFOK          *         
*                   CC      = NOT EQUAL I.E. FVMSGNO = FVFNOTV|FVMISS *         
***********************************************************************         
         SPACE 1                                                                
VALYNO   NTR1                                                                   
         MVC   FVMSGNO,=AL2(FVFOK) ASSUME OKAY                                  
         LR    R2,R1               SAVE   REGISTER                              
         GOTO1 AFVAL               ANY    DATA ?                                
         BNE   VALYNOMS            NO,    MISSING                               
         CLC   8(1,R2),APYES       'YES'  ?                                     
         BE    VALYNOEX            YES,   OKAY                                  
         CLC   8(1,R2),APNO        'NO'   ?                                     
         BE    VALYNOEX            YES,   OKAY                                  
         CLC   8(1,R2),APONLY      'ONLY' ?                                     
         BE    VALYNOEX            YES,   OKAY                                  
*                                                                               
VALYNONG DS    0H                  NOT    VALID                                 
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALYNOEX            RETURN                                       
*                                                                               
VALYNOMS DS    0H                  MISSING                                      
         MVC   FVMSGNO,=AL2(FVFMISS)                                            
*        B     VALYNOEX            RETURN                                       
*                                                                               
VALYNOEX DS    0H                  RETURN                                       
         CLC   FVMSGNO,=AL2(FVFOK) SET    CONDITION CODE                        
         B     XIT                 EXIT                                         
*&&                                                                             
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*                        DISPLAY G/L PROFILE DATA                     *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
DISREC   L     R2,AIOAREA1         PROFILE    ELEMENT                           
         TWAXC GNLNMEH,GNLTABH                                                  
         GOTO1 GETNAME,APPARM,(R2),GNLNMEH                                      
         GOTO1 GETPER,APPARM,(R2),GNLOWNH                                       
*                                                                               
*        MVC   GNLTYPE(L'APREPTYP),APREPTYP                                     
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
         LA    RF,GNLACCTH         SPECIFIC   ACCOUNT  HEADER                   
*                                                                               
         CLI   RFLTYPE,RFLLDG      LEDGER     FILTER   LIST                     
         BNE   *+8                                                              
         LA    RF,GNLTYPEH         LEDGER     LIST HEADER                       
*                                                                               
         CLI   RFLTYPE,RFLCNTR     CONTRA     FILTER   LIST                     
         BNE   *+8                                                              
         LA    RF,GNLCNTRH         CONTRA     ACCOUNT  HEADER                   
*                                                                               
         CLI   RFLTYPE,RFLOFF      OFFICE     FILTER   LIST                     
         BNE   *+8                                                              
         LA    RF,GNLOFFH          OFFICE HEADER                                
*                                                                               
         CLI   RFLTYPE,RFLTTYPE    INCLUDE    FILTER   TRANS     TYPE           
         BNE   DISREC15                                                         
         LR    R0,R1                                                            
         GOTO1 CNVTTYPE,APPARM,(C'S',(R0)),(X'00',GNLITTY)                      
         LR    R1,R0                                                            
         OI    GNLITTYH+6,FVOXMT                                                
         SR    RF,RF                                                            
*                                                                               
DISREC15 DS    0H                                                               
         LTR   RF,RF               ANY   DATA TO   DISPLAY   ?                  
         BZ    DISREC25            NO,   SKIP                                   
         OI    6(RF),FVOXMT        TURN  TRANSMIT  ON                           
         TM    RFLIND,RFLXCLD      EXCLUDE    BIT  ON   ?                       
         BZ    DISREC20            NO,   MOVE THE  DATA                         
         MVI   8(RF),C'*'          OUTPUT     C'*'                              
         LA    RF,1(,RF)           BUMP  DATA LOCATION                          
*                                                                               
DISREC20 DS    0H                                                               
         EXMVC R6,8(RF),RFLDATA    INSERT     THE  DATA                         
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
         LA    R1,GNLF1H                                                        
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
DISREC40 DS    0H                                                               
*&&US                                                                           
         MVC   GNLRET,APYES        DEFAULT IS YES                               
         TM    RPFOPT7,RPFXRTEA    EXCLUDE RETAINED EARNINGS?                   
         BZ    *+14                                                             
         MVC   GNLRET,APNO                                                      
         MVI   GNLRET+2,C' '                                                    
         TM    RPFOPT7,RPFORTEA    RETAINED EARNINGS ONLY?                      
         BZ    *+10                                                             
         MVC   GNLRET,APONLY                                                    
         OI    GNLRETH+6,FVOXMT                                                 
*&&                                                                             
*                                                                               
DISREC99 DS    0H                                                               
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   DISRC999                                                         
         CLI   APACTN,ACTDIS       IN    DISPLAY   MODE ?                       
         BE    DISRC999            YES,  SKIP                                   
         CLI   TWALREC,RECGNLPF    IF    1ST  TIME IN,  THEN SET CURSOR         
         BE    DISRC999            NO,   SKIP                                   
*                                                                               
         LA    RE,GNLCODEH         FORMAT     CODE FIELD                        
         ST    RE,APCURSOR         SET   APPLICATION    CURSOR                  
*                                                                               
DISRC999 DS    0H                                                               
         B     EXIT                                                             
*                                                                               
         DROP R1,R9                KEEP  IT   CLEAN                             
         EJECT ,                                                                
         SPACE 1                                                                
         USING REPTABD,R1                                                       
         SPACE 1                                                                
EXPRPTY  MVC   GNLRPCDE,REPCODE                                                 
         MVC   GNLRPTYP,REPSTYPE                                                
         CLI   GNLRPCDE,ESCHIGHQ   TEST  FOR  ESCAPE    SEQUENCE                
         BNLR  RE                                                               
*                                                                               
         DROP  R1                                                               
*                                                                               
         STM   RE,R1,SVREGS                                                     
         GOTO1 VDICTAT,APPARM,C'TU  ',('GNLRPLNQ',GNLRPCDE),0                   
         LM    RE,R1,SVREGS                                                     
         BR    RE                                                               
         EJECT ,                                                                
***********************************************************************         
* ERRORS TO DISPLAY ON TOP OF SCREEN                                  *         
***********************************************************************         
         SPACE 1                                                                
*VALRPTY MVC   FVMSGNO,=AL2(ACERPTYP)     INVALID REPORT TYPE                   
*        MVC   FVXTRA(3),FFNUMBER-FFNELD+1(R1)                                  
*        LA    R1,SCRTYPH                                                       
*        ST    R1,FVADDR                                                        
*        B     IVALEXIT                                                         
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
IVALCUL0 MVC   FVXTRA(10),12(R4)          INVALID UNIT/LEDGER                   
*                                                                               
IVALCULR MVC   FVMSGNO,=AL2(ACEIVUL)      INVALID UNIT/LEDGER                   
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALACCT MVC   FVMSGNO,=AL2(ACEACTLV)     INVALID ACCOUNT OR LEVEL              
         MVC   FVXTRA(20),12(R4)                                                
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALCNTR MVC   FVMSGNO,=AL2(ACEICNTR)     INVALID CONTRA ACCOUNT                
         MVC   FVXTRA(20),12(R4)                                                
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALEXIT DS    0H                                                               
         B     EXIT                                                             
         EJECT ,                                                                
         SPACE 1                                                                
SCRTXT   DS    0F                                                               
         DC    AL4(GNLGL02H-TWAD,1652)                                          
         DC    AL4(GNLGL03H-TWAD,1671)                                          
         DC    X'FF'                                                            
         EJECT ,                                                                
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 4                                                                
         DROP  RA                  KEEP IT CLEAN                                
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*                   DSECT FOR LOCAL WORKING STORAGE                   *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
LWSD     DSECT                                                                  
MAXPARM  EQU   20                                                               
MAXLDG   EQU   10                  MAXIMUM NUMBER OF LEDGERS                    
PENNY    EQU   C'P'                                                             
*                                                                               
SVREGS   DS    6A                                                               
GNLRPCDE DS    CL(L'REPCODE)                                                    
GNLRPTYP DS    CL(L'REPSTYPE)                                                   
GNLRPLNQ EQU   *-GNLRPCDE                                                       
GNLBLOCK DS    (MAXPARM)CL32                                                    
SAVEKEY1 DS    CL(L'RESKEY)                                                     
MULTLDG  DS    AL4                 MULTIPLE  LEDGER BIT NUMBERS                 
TEMPLDG  DS    AL4                 TEMPORARY LEDGER BIT NUMBER                  
BYTE     DS    CL1                                                              
LDGUSED  DS    X                   NUMBER OF LEDGERS SPECIFIED                  
LDGSAVE  DS    (MAXLDG)CL(LUNLG)   SAVE AREA FOR LEDGERS SPECIFIED              
LDGSAVEL EQU   *-LDGSAVE           LENGTH OF LDGSAVE AREA                       
LWSX     DS    0C                                                               
         EJECT ,                                                                
*        ACSCRWRK                                                               
         PRINT OFF                                                              
       ++INCLUDE ACSCRWRK                                                       
         PRINT ON                                                               
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*                    DSECT FOR G/L PROFILE DEFINITIONS                *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
TWAD     DSECT                                                                  
         ORG   SCROVLYH                                                         
       ++INCLUDE ACSCRE4D                                                       
         PRINT ON                                                               
         ORG                                                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009ACSCR10   09/02/15'                                      
         END                                                                    
