*          DATA SET ACSCR0B    AT LEVEL 064 AS OF 08/27/15                      
*PHASE T60C0BA                                                                  
*&&ONLIN SET   Y                                                                
***********************************************************************         
*                                                                     *         
* THIS VERSION MATCHES THE UK VERSION LEVEL 66 AS OF 11/19/12         *         
*                                                                     *         
***********************************************************************         
*                                                                               
* PID  LVL DATE    COMMENTS                                                     
* ---------------------------------                                             
* SMAN 065 16DEC11 PR002242 UK/US MERGE                                         
* JFOS 066 01MAR12 PR002742 NEW PROFILE 'INCLUDE MANUAL PAYMENTS'               
*                                                                               
         TITLE 'PAY PROFILE ELEMENTS EXTRA MAINTANCE'                           
T60C0B   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,60C0B,RA,RR=RE                                                 
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
         CLC   ATWA,ACURSOR        INSURE CURSOR WAS IN A FIELD                 
         BE    SCR01               NO,  SET CURSOR                              
         CLI   TWALREC,RECPAYPF    IF   FIRST TIME IN, THEN SET CURSOR          
         BE    SCR02               NO,  SKIP                                    
*                                                                               
SCR01    LA    RE,PAYCDEH          FORMAT CODE FIELD                            
         ST    RE,ACURSOR                                                       
*                                                                               
SCR02    DS    0H                                                               
         CLI   APMODE,APMVALK                                                   
         BNE   SCR10                                                            
         LA    R2,SCRTXT           TABLE OF SCREEN UPDATES                      
*                                                                               
SCR05    CLI   0(R2),X'FF'         END OF TABLE?                                
         BE    SCR10               FINISHED                                     
         L     R4,0(,R2)           GET HEADER ADDRESS                           
         AR    R4,R5                                                            
         OI    6(R4),FVOXMT        TRANSMIT FIELD                               
         L     R3,4(,R2)           GET SCREEN TEXT NUMBER                       
         GOTO1 TEXTGET,APPARM,(R3),(R4),0                                       
         LA    R2,8(,R2)           BUMP TO NEXT                                 
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
EXIT     CLI   APPFKEY,PFKNEXT                                                  
         BE    EXIT97                                                           
         CLI   APMODE,APMVALK                                                   
         BE    EXIT95                                                           
*                                                                               
EXIT30   TM    TWASWPST,TWASWAP    SWAP TO NEW RECORD ACTION?                   
         BZ    EXIT95              NO                                           
         XC    APCURSOR,APCURSOR   DON'T SET CURSOR ON WRONG SCREEN             
         MVI   APMODE,APMSWP           SWAP                                     
         MVC   APPARM(1),TWASWPRE      SWAP RECORD                              
         MVC   APPARM+1(1),TWASWPAC    SWAP ACTION                              
*                                                                               
EXIT95   OI    TWALSCTL,TWALSHLD                                                
*                                                                               
EXIT97   OI    TWALSCTL,TWALSRTN                                                
*                                                                               
EXIT99   DS    0H                                                               
         CLI   APMODE,APMVALR      IN   VALIDATE  RECORD                        
         BNE   EXIT999             NO,  SKIP                                    
         CLC   FVMSGNO,=AL2(FVFOK) ANY  ERRORS    FOUND ?                       
         BNE   EXIT999             YES, SKIP                                    
         CLI   APPFKEY,0           ANY  PF   KEY  DEPRESSED ?                   
         BNE   EXIT999             YES, SKIP                                    
         TM    TWASWPST,TWASWAP    SWAP TO   NEW  RECORD ACTION ?               
         BO    EXIT999             YES, SKIP                                    
         MVC   APCURSOR,ACURSOR    NO,  SET  APPLICATION CURSOR                 
*                                                                               
EXIT999  CLC   FVMSGNO,=AL2(FVFOK) ANY  ERRORS    FOUND ?                       
*                                                                               
XIT      XIT1                                                                   
         EJECT ,                                                                
VALKEY   DS    0H                                                               
         MVI   NEWKEY,NO                                                        
         MVC   RESKEY,SPACES                                                    
         MVI   RESKTYP,RESKTYPQ    X'2D'                                        
         MVI   RESKSUB,RESKSUBQ    X'02'                                        
         MVC   RESKCPY,CUABIN      COMPANY CODE                                 
         GOTO1 AFVAL,PAYCDEH                                                    
         MVC   RESKFORM,FVIFLD                                                  
         BE    VALKEY15                                                         
         TM    PAYCDE+4,FVITHIS    ANY INPUT?                                   
         BZ    *+10                NO                                           
         MVC   SAVFORM,SPACES                                                   
         CLC   SAVFORM,SPACES                                                   
         BNH   IVALEKEY                                                         
         MVC   PAYCDE,SAVFORM                                                   
         OI    PAYCDEH+6,FVOXMT    TRANSMIT                                     
         MVC   RESKFORM,SAVFORM                                                 
*                                                                               
VALKEY15 MVC   SAVFORM,RESKFORM                                                 
         MVC   APRECKEY(L'RESKEY),RESKEY                                        
         LA    R1,IORD+IOACCFIL+IO1                                             
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(,R1)      READ FOR UPDATE                              
         GOTO1 AIO                                                              
         BL    VALKEY99            IO ERROR                                     
         BNE   VALKEY20                                                         
         L     R2,AIOAREA1                                                      
         GOTO1 GETTYPE,(R2)                                                     
         OI    SCRTYPH+6,FVOXMT                                                 
         MVC   SCRTYP(L'APREPCDE),APREPCDE                                      
         CLC   APREPCDE,AC@PAY     IS IT PAY?                                   
         BNE   IVALRPTY                                                         
*                                                                               
VALKEY18 MVI   APINDS,APIOKDIS+APIOKCHA+APIOKDEL                                
         TM    TWAMODE,TWAMDFR     2ND PASS IN ACTION COPY?                     
         BZ    VALKEY98                                                         
         CLI   APACTN,ACTCPY                                                    
         BNE   VALKEY98                                                         
         OI    APINDS,APIOKADD     TURN ON TO TRICK ACTION COPY                 
         B     VALKEY98                                                         
*                                                                               
VALKEY20 TM    IOERR,IOEDEL        IS RECORD MARKED DELETED                     
         BNZ   VALKEY99            OK TO ADD RECORD                             
         MVI   APINDS,APIOKADD     RECORD NOT ON FILE, SO OK TO ADD             
         L     R2,AIOAREA1                                                      
         XC    RESKEY(256),RESKEY  RESET AIO AREA                               
*                                                                               
VALKEY98 MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALKEY99 B     EXIT                                                             
         EJECT ,                                                                
DISKEY   NI    TWASWPST,TURNOFF-TWASWAP                                         
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT ,                                                                
VALREC   DS    0H                                                               
         L     R2,AIOAREA1                                                      
         GOTO1 AFVAL,PAYNMEH                                                    
         BNE   VALREC05            NAME HAS  NOT  BEEN   INPUT                  
         GOTO1 ADDNAME,APPARM,(R2),PAYNMEH   ADD  FORMAT NAME                   
         BNE   VALREC99            ON   ERROR,    EXIT                          
*                                  ************************************         
VALREC05 DS    0H                  * VALIDATE UNIT/LEDGER             *         
*                                  ************************************         
         GOTO1 MAKELIST,APPARM,('RFLLDG',PAYTYPEH),AIOAREA1,           X        
               ('MAXPARM',PAYBLOCK),APELEM                                      
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    IVALCULR            NO   INPUT                                   
         BP    VALREC99            BAD  INPUT                                   
*                                  GOOD INPUT                                   
         CLI   NPARMS,MAXLDG       TOO  MANY LEDGERS ?                          
         BH    IVAL2MNY            YES, ERROR TOO MANY                          
         LA    R1,APELEM           ->   FILTER DATA                             
*                                                                               
         USING RFLELD,R1           MAP  FILTER DATA ELEMENT                     
         TM    RFLIND,RFLXCLD      EXCLUDE REQUESTED ?                          
         BO    IVALCULR            YES, INVALID UNIT/LEDGER                     
         DROP  R1                  KEEP IT CLEAN                                
*                                                                               
         XC    MULTLDG,MULTLDG     CLEAR                                        
         SR    R0,R0                                                            
         IC    R0,NPARMS                                                        
         LA    R4,PAYBLOCK                                                      
         MVI   LDGUSED,0                 CLEAR LEDGERS COUNT                    
         MVC   LDGSAVE(LDGSAVEL),SPACES  CLEAR LEDGERS SAVE AREA                
*                                                                               
         USING REPTABD,R3                                                       
         L     R3,ACTYPTAB                                                      
VALREC10 CLI   REPCODE,EOT                                                      
         BE    IVALCUL0                                                         
         GOTO1 EXPRPTY,(R3)                                                     
         CLC   APREPCDE,PAYRPCDE   MATCH REPORT TYPE                            
         BNE   VALREC15            NO, SO GET NEXT REPORT TYPE                  
         CLI   0(R4),2             MUST BE LENGTH TWO FOR NOW                   
         BNE   IVALCUL0            NO,  INVALID UNIT/LEDGER                     
         CLI   REPNUM,C'M'                                                      
         BE    VALREC15            NOT THE DEFAULT                              
         CLC   REPUL,12(R4)                                                     
         BNE   VALREC15            NO, SO GET NEXT REPORT TYPE                  
         TM    REPFLAG,REPDDS      DDS ONLY?                                    
         BZ    VALREC18            NO, SO OK                                    
         TM    CUSTAT,CUSDDS       IS IT A DDS TERMINAL?                        
         BNZ   VALREC18            YES, SO OK                                   
VALREC15 LA    R3,REPLNQ(,R3)      BUMP TO NEXT REPORT TYPE                     
         B     VALREC10                                                         
*                                                                               
         USING ACTRECD,R2                                                       
VALREC18 LA    R2,IOKEY                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUABIN      COMPANY CODE                                 
         MVC   ACTKUNT(2),REPUL    UNIT/LEDGER                                  
         GOTO1 AIO,IOREAD+IOACCFIL+IO2                                          
         L     R2,AIOAREA1                                                      
         MVC   IOKEY,0(R2)                                                      
         BNE   IVALCUL0            NOT A VALID LEDGER FOR COMPANY               
*                                                                               
         MVC   TEMPLDG,REPIND      GET   THIS LEDGER'S BIT NUMBER               
         NC    TEMPLDG,MULTLDG     'AND' WITH CURRENT MULTI-LEDGER BITS         
         BNZ   IVALEDUP            NOT   ZERO, ERROR DUPLICATE LEDGER           
         OC    MULTLDG,REPIND      GOOD, SAVE THIS LEDGER'S BIT NUMBER          
         ZIC   RE,LDGUSED          GET   NUMBER OF LEDGERS SO FAR               
         SLL   RE,1                TIMES TWO                                    
         LA    RF,LDGSAVE          GET   ADDRESS OF LEDGER SAVE AREA            
         AR    RF,RE               PLUS  BYTES USED SO FAR                      
         MVC   0(LUNLG,RF),12(R4)  SAVE  UNIT/LEDGER                            
         SRL   RE,1                GET   NUMBER OF LEDGERS SO FAR               
         LA    RE,1(,RE)           ADD   ONE                                    
         STC   RE,LDGUSED          NEW   NUMBER OF LEDGERS USED                 
*                                                                               
         LA    R4,32(,R4)          BUMP TO NEXT PARMETER                        
         MVC   APREPNUM,REPNUM                                                  
         L     R3,ACTYPTAB                                                      
         BCT   R0,VALREC10                                                      
         DROP  R3                  KEEP IT CLEAN                                
*                                                                               
         L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)          ADD REPORT TYPE ELEMENT X'C5'                
         BNE   VALREC99            ON  ERROR, EXIT                              
         CLI   NPARMS,1            ONLY ONE PARAMETER?                          
         BE    *+8                 YES                                          
         MVI   APREPNUM,C'M'       MULTI LEDGER                                 
         GOTO1 ADDREPTY,APPARM,(R2)                                             
         BNE   VALREC99            ON    ERROR, EXIT                            
         GOTO1 GETTYPE,(R2)        RESET APREP VALUES                           
         MVC   APREPIND,MULTLDG                                                 
*                                  ************************************         
*                                  * BUILD PROFILE DATA ELEMENT       *         
*                                  ************************************         
         USING RESRECD,R2                                                       
         USING RPFELD,R9                                                        
         LA    R9,APELEM           BUILD DEFAULT PROFILE DATA                   
         MVC   RESKEY,APRECKEY                                                  
         XC    APELEM,APELEM                                                    
         MVI   RPFEL,RPFELQ        ELEMENT CODE                                 
         MVI   RPFLN,RPFLN2Q       LENGTH OF ELEMENT                            
         OI    RPFPOPT,RPFBOX      TURN ON BIT FOR BOXES                        
         OI    RPFEDOPT,RPFEDCMA+RPFEDTRL      COMMAS, TRAILING MINUS           
         MVI   RPFPCTS,C'2'                                                     
         MVI   RPFRND,PENNY                                                     
*&&US                                                                           
         MVI   RPFFLDD,C' '        DEFAULT, FIELD DELIMITER                     
         MVI   RPFTXTD,C'"'        DEFAULT, TEXT DELIMITER                      
         MVI   RPFEOLD,X'5E'       DEFAULT, END OF LINE                         
         MVI   RPFEORD,C':'        DEFAULT, END OF REPORT                       
*&&                                                                             
         MVI   APELCODE,RPFELQ     GET PROFILE ELEMENT                          
         GOTO1 GETEL,(R2)                                                       
         BNE   VALREC25            NOT FOUND, ADD ONE                           
         SR    RF,RF                                                            
         IC    RF,1(,R1)                                                        
         BCTR  RF,0                                                             
         EXMVC RF,APELEM,0(R1)                                                  
         MVI   RPFLN,RPFLN2Q       ELSE ENSURE RE-ADDED WITH LATEST L'          
         GOTO1 DELEL,(R2)          (DELETE OLD ONE)                             
         DROP  R2                                                               
*                                  ************************************         
VALREC25 DS    0H                  * PROCESS OPTIONS                  *         
*                                  ************************************         
         MVI   RPFROPT,0           CLEAR FLAGS                                  
         MVI   RPFOPT1,0                                                        
         MVI   RPFOPT2,0                                                        
         MVI   RPFOPT4,0                                                        
         MVI   RPFOPT6,0                                                        
         NI    RPFOPT7,X'FF'-(RPFXMAPY+RPFOMAPY)                                
         NI    RPFROPT,TURNOFF-RPFBALO   NOT VALID ANY MORE (TURN OFF)          
*                                                                               
*        CLC   PAYBALO,APYES                                                    
*        BNE   *+8                                                              
*        OI    RPFROPT,RPFBALO     BALANCE OUTSTANDING ONLY                     
*                                                                               
         CLC   PAYAPRV,APNO                                                     
         BNE   *+8                                                              
         OI    RPFOPT1,RPFXAPRV    EXCLUDE APPROVED ITEMS                       
*                                                                               
         CLC   PAYAPRV,APONLY                                                   
         BNE   *+8                                                              
         OI    RPFOPT1,RPFOAPRV    APPROVED ITEMS ONLY                          
*                                                                               
         CLC   PAYCR$,APYES                                                     
         BNE   *+8                                                              
         OI    RPFROPT,RPFBALCR    CREDIT BALANCE ONLY                          
*                                                                               
         CLC   PAYHELD,APNO                                                     
         BNE   *+8                                                              
         OI    RPFOPT1,RPFXHELD    EXCLUDE HELD ITEMS                           
*                                                                               
         CLC   PAYHELD,APONLY                                                   
         BNE   *+8                                                              
         OI    RPFOPT1,RPFOHELD    DRAFT ITEMS ONLY                             
*                                                                               
         CLC   PAYCD$,APYES                                                     
         BNE   *+8                                                              
         OI    RPFOPT2,RPFOCD$     CASH DISCOUNT ONLY                           
*&&US                                                                           
         CLC   PAYZTRN,APYES                                                    
         BNE   *+8                                                              
         OI    RPFOPT6,RPFZPST     INCLUDE $0 TRANSACTIONS                      
*&&                                                                             
         CLC   PAYOFFS,APNO                                                     
         BNE   *+8                                                              
         OI    RPFOPT1,RPFXOFFS    EXCLUDE OFFSET ITEMS                         
*                                                                               
         CLC   PAYOFFS,APONLY                                                   
         BNE   *+8                                                              
         OI    RPFOPT1,RPFOOFFS    OFFSET ITEMS ONLY                            
*                                                                               
         CLC   PAYLOCK,APNO                                                     
         BNE   *+8                                                              
         OI    RPFROPT,RPFXLOCK    EXCLUDE LOCKED ACCOUNTS                      
*                                                                               
         CLC   PAYLOCK,APONLY                                                   
         BNE   *+8                                                              
         OI    RPFROPT,RPFOLOCK    LOCKED ACCOUNTS ONLY                         
*                                                                               
         CLC   PAYRVRS,APNO                                                     
         BNE   *+8                                                              
         OI    RPFROPT,RPFXRVRS    EXCLUDE REVERESED ITEMS                      
*                                                                               
         CLC   PAYRVRS,APONLY                                                   
         BNE   *+8                                                              
         OI    RPFROPT,RPFORVRS    REVERESED ITEMS ONLY                         
*                                                                               
         CLC   PAYVNDR,APNO                                                     
         BNE   *+8                                                              
         OI    RPFOPT2,RPFXVNDR    EXCLUDE PAY=NO VENDORS                       
*                                                                               
         CLC   PAYVNDR,APONLY                                                   
         BNE   *+8                                                              
         OI    RPFOPT2,RPFOVNDR    PAY=NO VENDORS ONLY                          
*                                                                               
         CLC   PAYDRFT,APYES                                                    
         BNE   *+8                                                              
         OI    RPFOPT1,RPFIDRFT    INCLUDE DRAFT ITEMS                          
*                                                                               
         CLC   PAYDRFT,APONLY                                                   
         BNE   *+8                                                              
         OI    RPFOPT1,RPFODRFT    DRAFT ITEMS ONLY                             
*                                                                               
         CLC   PAYCSHD,APNO                                                     
         BE    *+8                                                              
         OI    RPFOPT2,RPFSPEC     FILTER SPECS FOR ALTERNATE DATES             
*                                                                               
         CLC   PAYURGN,APNO                                                     
         BNE   *+8                                                              
         OI    RPFOPT2,RPFXUGNT    EXCLUDE URGENT                               
*                                                                               
         CLC   PAYURGN,APONLY                                                   
         BNE   *+8                                                              
         OI    RPFOPT2,RPFOUGNT    URGENT ONLY                                  
*                                                                               
         CLC   PAYXJOB,APNO                                                     
         BNE   *+8                                                              
         OI    RPFOPT4,RPFXEXPS    EXCLUDE X-JOBS                               
*                                                                               
         CLC   PAYXJOB,APONLY                                                   
         BNE   *+8                                                              
         OI    RPFOPT4,RPFOEXPS    ONLY X-JOB                                   
*&&UK                                                                           
         CLC   PAYMAPY,APNO                                                     
         BNE   *+8                                                              
         OI    RPFOPT7,RPFXMAPY    EXCLUDE MANUAL PAYMENTS                      
*                                                                               
         CLC   PAYMAPY,APONLY                                                   
         BNE   *+8                                                              
         OI    RPFOPT7,RPFOMAPY    MANUAL PAYMENTS ONLY                         
*&&                                                                             
         XC    RPFFLT1(4),RPFFLT1  CLEAR FILTER AREA                            
         MVI   RPFFLT5,0           CLEAR FILTER 5                               
         LA    R4,5                FIVE FILTER TYPES                            
         LA    R3,RPFFLT1                                                       
         LA    R1,PAYF1H                                                        
*                                                                               
VALREC27 GOTO1 AFVAL                                                            
         BNE   VALREC28                                                         
         MVC   0(1,R3),FVIFLD                                                   
         CLI   FVIFLD,C'*'         EXCLUDE FILTER?                              
         BNE   VALREC28                                                         
         MVC   0(1,R3),FVIFLD+1                                                 
         NI    0(R3),TURNOFF-X'40' MAKE LOWER CASE                              
*                                                                               
VALREC28 SR    RE,RE                                                            
         IC    RE,0(,R1)           BUMP TO PROTECTED FIELD                      
         AR    R1,RE                                                            
         IC    RE,0(,R1)           BUMP TO NEXT INPUT FILTER                    
         AR    R1,RE                                                            
         LA    R3,1(,R3)           BUMP TO NEXT FILTER IN PROFILE ELEM          
         CLM   R4,1,=AL1(2)        F1-F4 DONE YET?                              
         BNE   *+8                 NOT YET                                      
         LA    R3,RPFFLT5          SWITCH TO F5                                 
         BCT   R4,VALREC27                                                      
*                                                                               
         L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)          ADD PROFILE ELEMENT X'C4'                    
         BNE   VALREC99            ON  ERROR,  EXIT                             
         MVC   SAVEKEY1,IOKEY                                                   
         DROP  R9                                                               
*                                  ************************************         
VALREC30 DS    0H                  * VALIDATE SPECIFIC ACCOUNTS       *         
*                                  ************************************         
         GOTO1 MAKELIST,APPARM,('RFLACC',PAYACCTH),(R2),               X        
               ('MAXPARM',PAYBLOCK),APELEM                                      
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VALREC38            NO   INPUT                                   
         BP    VALREC99            BAD  INPUT                                   
*                                  GOOD INPUT                                   
         GOTO1 VALLIST,APPARM,(LDGUSED,LDGSAVE),                       X        
               (PAYBLOCK,PAYBLOCK+12),1                                         
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VALREC31            NO   LIST                                    
         BP    VALREC99            BAD  LIST                                    
*                                  GOOD LIST                                    
         CLI   NPARMS,1            ONLY 1 LIST REQUESTED ?                      
         BE    VALRC33A            YES, ADD SPECIFIC ACCOUNT ELEMENT            
         B     IVAL2MNY            NO,  TOO MANY LISTS                          
*                                                                               
         USING ACTRECD,R2                                                       
VALREC31 SR    R6,R6               VALIDATE ACCOUNT(S)                          
         IC    R6,NPARMS                                                        
         LA    R4,PAYBLOCK                                                      
         LA    R2,IOKEY                                                         
*                                                                               
VALREC32 DS    0H                  NEXT 2 LINES ADDED FOR COMPATIBLITY          
         CLI   0(R4),0             IS   THE LENGTH OF ACCOUNT = 0               
         BE    VALREC33            YES, SKIP                                    
         CLI   0(R4),LULACNT       IS   THE LENGTH OF ACCOUNT > 14 ?            
         BH    IVALACCT            YES, INVALID ACCOUNT                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUABIN      COMPANY CODE                                 
         CLI   0(R4),LUNLG         IS   THE LENGTH OF ACCOUNT <= 2 ?            
         BNH   IVALACCT            YES, INVALID ACCOUNT                         
*                                  ACCOUNT IS IN PAYBLOCK                       
         MVC   ACTKUNT(LULACNT),12(R4)                                          
         GOTO1 AIO,IORD+IOACCFIL+IO2                                            
         BE    VALRC32A            VALID ACCOUNT                                
         MVI   BYTE,C'U'           U/L  LEDGER FOR WILDCARD                     
         BAS   RE,WILDCARD                                                      
         BNE   IVALACCT            INVALID ACCOUNT                              
*                                                                               
VALRC32A DS    0H                  MAKE SURE USING A VALID LEDGER               
         ZIC   RE,LDGUSED          GET  NUMBER OF VALID LEDGERS                 
         LA    RF,LDGSAVE          ->   LEDGER SAVE AREA                        
*                                                                               
VALRC32B DS    0H                  COMPARE WITH SAVE AREA                       
         CLC   0(LUNLG,RF),12(R4)  MATCHES UNIT LEDGER SAVE AREA ?              
         BE    VALREC33            YES, VALID ACCOUNT                           
         LA    RF,LUNLG(,RF)       BUMP TO NEXT LEDGER                          
         BCT   RE,VALRC32B         LOOP TO COMPARE NEXT SAVED LEDGER            
         B     IVALACCT            INVALID ACCOUNT                              
*                                                                               
VALREC33 LA    R4,32(,R4)          NEXT BLOCK IN PAYBLOCK                       
         BCT   R6,VALREC32                                                      
         DROP  R2                  KEEP IT CLEAN                                
*                                                                               
VALRC33A DS    0H                                                               
         L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)          ADD  SPECIFIC ACCOUNT ELEMENT                
         BNE   VALREC99            ON   ERROR, EXIT                             
*                                  ************************************         
VALREC38 DS    0H                  * VALIDATE CLIENT                  *         
*                                  ************************************         
         GOTO1 MAKELIST,APPARM,('RFLCLI',PAYCLIH),(R2),                X        
               ('MAXPARM',PAYBLOCK),APELEM                                      
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VALREC43            NO   INPUT                                   
         BP    VALREC99            BAD  INPUT                                   
*                                  GOOD INPUT                                   
         GOTO1 VALLIST,APPARM,=C'SJ',(PAYBLOCK,PAYBLOCK+12),0                   
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VALREC39            NO   LIST                                    
         BP    VALREC99            BAD  LIST                                    
*                                  GOOD LIST                                    
         CLI   NPARMS,2            ONLY ALLOWED ON FOR A LIST                   
         BL    VALREC42            ADD  THE VALID LIST                          
         BH    IVAL2MNY                                                         
         GOTO1 VALLIST,APPARM,=C'SJ',                                  X        
               (PAYBLOCK+L'PAYBLOCK,PAYBLOCK+L'PAYBLOCK+12),0                   
         LTR   RF,RF               TEST THE RETURN CODE                         
*                                  GOOD LIST                                    
         BZ    VALREC42            ADD  THE VALID LISTS                         
         BP    VALREC99            BAD  LIST                                    
*                                  NO   LIST                                    
*                                  BUT  - MUST BE A LIST                        
         MVC   FVMSGNO,=AL2(ACEIVLT)                                            
         B     VALREC99            THEREFORE, THIS IS A BAD LIST                
*                                                                               
VALREC39 DS    0H                                                               
         SR    R6,R6                                                            
         IC    R6,NPARMS                                                        
         LA    R4,PAYBLOCK                                                      
         LA    R2,IOKEY                                                         
*                                                                               
         USING ACTRECD,R2                                                       
VALREC40 DS    0H                                                               
         CLI   0(R4),LACCOUNT      IS   THE LENGTH OF CLIENT > 12 ?             
         BH    IVALCLI             YES, INVALID CLIENT                          
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUABIN      COMPANY CODE                                 
         MVC   ACTKUNT(2),=C'SJ'   UNIT LEDGER                                  
         MVC   ACTKACT,12(R4)      CLIENT                                       
         GOTO1 AIO,IORD+IOACCFIL+IO2                                            
         BE    VALREC41            VALID CLIENT                                 
         MVI   BYTE,C'N'                                                        
         BAS   RE,WILDCARD                                                      
         BNE   IVALCLI             INVALID CLIENT                               
*                                                                               
VALREC41 LA    R4,32(,R4)          NEXT BLOCK IN PAYBLOCK                       
         BCT   R6,VALREC40                                                      
         DROP  R2                                                               
*                                                                               
VALREC42 DS    0H                                                               
         L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)          ADD  CLIENT LIST                             
         BNE   VALREC99            ON   ERROR, EXIT                             
*                                                                               
*                                  ************************************         
VALREC43 DS    0H                  * VALIDATE OFFICE                  *         
*                                  ************************************         
         GOTO1 MAKELIST,APPARM,('RFLOFF',PAYOFFH),(R2),                X        
               ('MAXPARM',PAYBLOCK),APELEM                                      
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VALREC50            NO   INPUT                                   
         BP    VALREC99            BAD  INPUT                                   
*                                  GOOD INPUT                                   
         USING RFLELD,R4                                                        
         MVI   BYTE,NO             ASSUME INCLUDE                               
         LA    R4,APELEM           ->     ELEMENT                               
         TM    RFLIND,RFLXCLD      EXCLUDE     FILTER ?                         
         BZ    *+8                 NO,    SKIP                                  
         MVI   BYTE,YES            SAY    EXCLUDE                               
         DROP  R4                                                               
*                                                                               
         ZIC   R4,NPARMS           NUMBER OF   OFFICES                          
         GOTO1 VOFFICE,APPARM,('VOFTYPEB',PAYBLOCK),(BYTE,(R4))                 
         BNE   VALREC99            BAD    INPUT                                 
*                                                                               
         L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)          ADD  OFFICE LIST                             
         BNE   VALREC99            ON   ERROR, EXIT                             
*                                                                               
*                                  ************************************         
VALREC50 DS    0H                  * VALIDATE TRANSACTION TYPE(S)     *         
*                                  ************************************         
         GOTO1 MAKELIST,APPARM,('RFLTTYPE',PAYITTYH),(R2),             X        
               ('MAXPARM',PAYBLOCK),APELEM                                      
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VALREC55            NO   INPUT                                   
         BP    VALREC99            BAD  INPUT                                   
*                                  GOOD INPUT                                   
         USING RFLELD,R9                                                        
         SR    R6,R6                                                            
         IC    R6,NPARMS           SCAN ENTRIES = NUMBER OF TYPES               
         LA    R9,APELEM                                                        
         LA    RF,RFLLNQ(,R6)      NEW  LENGTH OF ELEMENT                       
         STC   RF,RFLLN            STORE IN ELEMENT                             
         GOTO1 CNVTTYPE,APPARM,(C'N',PAYBLOCK),(NPARMS,RFLDATA)                 
         BNE   VALREC99                                                         
         L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)          ADD  INCLUDE TRANS TYPE                      
         BNE   VALREC99            ON   ERROR,  EXIT                            
         DROP  R9                                                               
*                                                                               
*                                  ************************************         
VALREC55 DS    0H                  * AUTHORIZATION STATUS             *         
*                                  ************************************         
*&&UK                                                                           
         GOTO1 MAKELIST,APPARM,('RFLAUTH',PAYAUTHH),AIOAREA1,          X        
               ('MAXPARM',PAYBLOCK),APELEM                                      
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VALREC60            NO   INPUT                                   
         BP    VALREC99            BAD  INPUT                                   
*                                  GOOD INPUT                                   
         LA    R9,APELEM                                                        
         USING RFLELD,R9                                                        
         CLI   NPARMS,1            ONLY 1 LIST REQUESTED ?                      
         BH    IVAL2MNY            NO,  TOO MANY LISTS                          
         TM    RFLIND,RFLXCLD      EXCLUDE EXCEPTION ?                          
         BO    IVALIPUT            YES, INVALID INPUT                           
         LA    R4,PAYBLOCK                                                      
         ZIC   RF,0(R4)                                                         
         BCTR  RF,0                LENGTH OF INPUT - 1                          
         EXCLC RF,12(R4),AC@BOTH   DEFAULT - BOTH?                              
         BE    VALREC60                                                         
         MVI   BYTE,AUTHQ                                                       
         EXCLC RF,12(R4),AC@ATHED  AUTH?                                        
         BE    VALREC57                                                         
         MVI   BYTE,UNAUTHQ                                                     
         EXCLC RF,12(R4),AC@UATH   UNAUTH?                                      
         BNE   IVALIPUT            NO, INVALID INPUT                            
*                                                                               
VALREC57 MVC   RFLDATA(L'BYTE),BYTE                                             
         MVI   RFLLN,RFLLNQ+L'BYTE                                              
         L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)          ADD  AUTH STATUS ELEMENT                     
         BNE   VALREC99                                                         
         DROP  R9                                                               
*                                  ************************************         
VALREC60 DS    0H                  * VALIDATE VAT REGION              *         
*                                  ************************************         
         GOTO1 MAKELIST,APPARM,('RFLVATRG',PAYVATRH),AIOAREA1,         X        
               ('MAXPARM',PAYBLOCK),APELEM                                      
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VALREC65            NO   INPUT                                   
         BP    VALREC99            BAD  INPUT                                   
*                                  GOOD INPUT                                   
         USING RFLELD,R9                                                        
         LA    R9,APELEM           ->     ELEMENT                               
         TM    RFLIND,RFLXCLD      EXCLUDE     FILTER ?                         
         BO    IVALIPUT            YES, INVALID INPUT                           
*                                                                               
         SR    R6,R6                                                            
         IC    R6,NPARMS                                                        
         LA    R4,PAYBLOCK                                                      
*                                                                               
VALREC62 DS    0H                                                               
         CLI   0(R4),1             HAS TO BE 1 CHARACTER                        
         BH    IVALIPUT                                                         
         CLI   12(R4),C'N'         NATIONAL                                     
         BE    *+8                                                              
         CLI   12(R4),C'E'         EU                                           
         BE    *+12                                                             
         CLI   12(R4),C'X'         FOREIGN BUT NOT EU                           
         BNE   IVALIPUT                                                         
*                                                                               
         LA    R4,32(,R4)                                                       
         BCT   R6,VALREC62                                                      
*                                                                               
         L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)          ADD  AUTH STATUS ELEMENT                     
         BNE   VALREC99                                                         
         DROP  R9                                                               
*&&                                                                             
VALREC65 DS    0H                  LABEL FOR ANY FUTURE FILTERS                 
         MVC   IOKEY,SAVEKEY1                                                   
*                                  ************************************         
VALREC95 DS    0H                  * UPDATE THE RECORD WITH ALL THE   *         
*                                  * NEW ELEMENTS                     *         
*                                  ************************************         
         GOTO1 ADDID,APPARM,AIOAREA1                                            
         BNE   VALREC99            ON   ERROR, EXIT                             
         LA    R1,IOADD+IOACCFIL+IO1                                            
         TM    APINDS,APIOKADD          ADD    A    RECORD?                     
         BO    VALREC97                                                         
         LA    R1,IOWRITE+IOACCFIL+IO1                                          
         TM    APINDS,APIOKCHA          CHANGE A    RECORD?                     
         BO    VALREC97                                                         
         DC    H'0'                     WHAT   THE  HELL?                       
*                                                                               
VALREC97 GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD  WRITE  OR   SOMETHING DUDE              
*                                                                               
VALREC99 CLC   FVMSGNO,=AL2(FVFOK)                                              
         BE    DISREC                                                           
         B     EXIT                                                             
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*  WILDCARD                                                           *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
WILDCARD NTR1                                                                   
         SR    R1,R1                                                            
         ICM   R1,1,0(R4)          LENGTH                                       
         LA    RF,12(,R4)          START OF ACCOUNT                             
*                                                                               
         CLI   BYTE,C'U'           CHECK UNIT/LEDGER                            
         BNE   WILD200                                                          
*                                                                               
         LA    R2,2                JUST CHECKING U/L                            
WILD100  CLI   0(RF),C'?'          U/L HAS WILDCARD?                            
         BE    WILDNO              WILDCARD INVALID                             
         LA    RF,1(,RF)           NEXT CHAR                                    
         BCTR  R1,0                                                             
         BCT   R2,WILD100                                                       
*                                                                               
WILD200  CLI   0(RF),C'?'          CONTRA U/L HAS WILDCARD?                     
         BE    WILDYES             WILDCARD INVALID                             
         LA    RF,1(,RF)           NEXT CHAR                                    
         BCT   R1,WILD200                                                       
*                                                                               
WILDNO   LTR   RE,RE               WILDCARD INVALID / U/L HAS WILDCARD          
         B     XIT                                                              
*                                                                               
WILDYES  LA    R1,APELEM                                                        
*                                                                               
         USING RFLELD,R1                                                        
         OI    RFLIND,RFLWILD      TURN ON THE BIT                              
         CR    RE,RE               WILDCARD IS GOOD                             
         B     XIT                                                              
         DROP  R1                                                               
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*                        DISPLAY PAY PROFILE DATA                     *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
DISREC   L     R2,AIOAREA1         PROFILE ELEMENT                              
         TWAXC PAYNMEH,PAYTABH                                                  
         GOTO1 GETNAME,APPARM,(R2),PAYNMEH                                      
         GOTO1 GETPER,APPARM,(R2),PAYOWNH                                       
*                                                                               
*        MVC   PAYTYPE(L'APREPTYP),APREPTYP                                     
         MVC   PAYLOCK,APYES       DEFAULT, SHOW LOCKED ACCOUNTS                
*        MVC   PAYBALO,APNO        DEFAULT, SHOW ALL ACCOUNTS                   
         MVC   PAYCR$,APNO         DEFAULT, CR PAYEES ONLY                      
         MVC   PAYCD$,APNO         DEFAULT, CASH DISCOUNT ONLY                  
*&&US*&& MVC   PAYZTRN,APNO        DEFAULT, INCLUDE $0 TRANSACTIONS             
         MVC   PAYVNDR,APYES       DEFAULT, INCLUDE PAY=NO VENDORS              
         MVC   PAYDRFT,APNO        DEFAULT, EXCLUDE DRAFT ITEMS                 
         MVC   PAYHELD,APYES       DEFAULT, INCLUDE HELD ITEMS                  
         MVC   PAYURGN,APYES       DEFAULT, INCLUDE URGENT ITEMS                
         MVC   PAYAPRV,APYES       DEFAULT, INCLUDE APPROVED ITEMS              
         MVC   PAYRVRS,APYES       DEFAULT, INCLUDE REVERSED ITEMS              
         MVC   PAYOFFS,APYES       DEFAULT, INCLUDE OFFSET ITEMS                
         MVC   PAYCSHD,APNO        DEFAULT, NOT CASH DISBURSEMENTS              
         MVC   PAYXJOB,APYES       DEFAULT, INCLUDE X-JOBS                      
*&&UK*&& MVC   PAYAUTH,AC@BOTH     DEFAULT, INCLUDE BOTH                        
*&&UK*&& MVC   PAYVATR,SPACES      DEFAULT, SHOW ALL                            
*&&UK*&& MVC   PAYMAPY,APYES       DEFAULT, INCLUDE MANUAL PAYMENTS             
*                                                                               
         USING RFLELD,R1                                                        
         MVI   APELCODE,RFLELQ     FILTER/LIST ELEMENT X'C5'                    
         GOTO1 GETEL,(R2)                                                       
         BNE   DISREC30                                                         
*                                                                               
DISREC10 CLI   RFLSEQ,0            HIGH LEVEL FILTER ONLY                       
         BNE   DISREC25                                                         
         SR    RF,RF                                                            
         SR    R6,R6                                                            
         IC    R6,RFLLN                                                         
         SH    R6,=Y(RFLLNQ+1)                                                  
         BM    DISREC25            SKIP ELEMENT IF LENGTH TOO SMALL             
*                                                                               
*&&UK                                                                           
         CLI   RFLTYPE,RFLAUTH                                                  
         BNE   DISREC12                                                         
         CLI   RFLDATA,C' '        NO INPUT                                     
         BNH   DISREC25                                                         
         MVC   PAYAUTH(L'AC@ATHED),AC@ATHED                                     
         CLI   RFLDATA,AUTHQ       AUTH STATUS?                                 
         BE    *+10                                                             
         MVC   PAYAUTH(L'AC@UATH),AC@UATH                                       
         OI    PAYAUTHH+6,FVOXMT   TURN    TRANSMIT  ON                         
         B     DISREC25                                                         
*&&                                                                             
DISREC12 CLI   RFLTYPE,RFLACC      ACCOUNT FILTER LIST                          
         BNE   *+8                                                              
         LA    RF,PAYACCTH         SPECIFIC ACCOUNT HEADER                      
*                                                                               
         CLI   RFLTYPE,RFLLDG      LEDGER FILTER LIST                           
         BNE   *+8                                                              
         LA    RF,PAYTYPEH         LEDGER LIST HEADER                           
*                                                                               
         CLI   RFLTYPE,RFLCLI      CLIENT FILTER LIST                           
         BNE   *+8                                                              
         LA    RF,PAYCLIH          CLIENT HEADER                                
*                                                                               
         CLI   RFLTYPE,RFLOFF      OFFICE FILTER LIST                           
         BNE   *+8                                                              
         LA    RF,PAYOFFH          OFFICE HEADER                                
*&&UK                                                                           
         CLI   RFLTYPE,RFLVATRG    VAT REGION LIST                              
         BNE   *+8                                                              
         LA    RF,PAYVATRH         VAT REGION HEADER                            
*&&                                                                             
         CLI   RFLTYPE,RFLTTYPE    INCLUDE FILTER TRANS TYPE                    
         BNE   DISREC15                                                         
         LR    R0,R1                                                            
         GOTO1 CNVTTYPE,APPARM,(C'S',(R0)),(X'00',PAYITTY)                      
         LR    R1,R0                                                            
         OI    PAYITTYH+6,FVOXMT                                                
         SR    RF,RF                                                            
*                                                                               
DISREC15 DS    0H                                                               
         LTR   RF,RF               ANY     DATA TO   DISPLAY   ?                
         BZ    DISREC25            NO,     SKIP                                 
         OI    6(RF),FVOXMT        TURN    TRANSMIT  ON                         
         TM    RFLIND,RFLXCLD      EXCLUDE BIT  ON   ?                          
         BZ    DISREC20            NO,     MOVE THE  DATA                       
         MVI   8(RF),C'*'          OUTPUT  C'*'                                 
         LA    RF,1(,RF)           BUMP    DATA LOCATION                        
*                                                                               
DISREC20 DS    0H                                                               
         EXMVC R6,8(RF),RFLDATA    INSERT  THE  DATA                            
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
         TM    RPFROPT,RPFXLOCK                                                 
         BZ    *+10                                                             
         MVC   PAYLOCK,APNO        EXCLUDE LOCKED ACCOUNTS                      
         TM    RPFROPT,RPFOLOCK                                                 
         BZ    *+10                                                             
         MVC   PAYLOCK,APONLY      LOCKED ACCOUNTS ONLY                         
         OI    PAYLOCKH+6,FVOXMT                                                
*                                                                               
         TM    RPFROPT,RPFXRVRS                                                 
         BZ    *+10                                                             
         MVC   PAYRVRS,APNO        EXCLUDE REVERSALS                            
         TM    RPFROPT,RPFORVRS                                                 
         BZ    *+10                                                             
         MVC   PAYRVRS,APONLY      REVERSALS ONLY                               
         OI    PAYRVRSH+6,FVOXMT                                                
*                                                                               
         TM    RPFOPT1,RPFXHELD                                                 
         BZ    *+10                                                             
         MVC   PAYHELD,APNO        EXCLUDE HELD                                 
         TM    RPFOPT1,RPFOHELD                                                 
         BZ    *+10                                                             
         MVC   PAYHELD,APONLY      HELD ONLY                                    
         OI    PAYHELDH+6,FVOXMT                                                
*                                                                               
         TM    RPFOPT1,RPFXOFFS                                                 
         BZ    *+10                                                             
         MVC   PAYOFFS,APNO        EXCLUDE OFFSET                               
         TM    RPFOPT1,RPFOOFFS                                                 
         BZ    *+10                                                             
         MVC   PAYOFFS,APONLY      OFFSETS ONLY                                 
         OI    PAYOFFSH+6,FVOXMT                                                
*                                                                               
         TM    RPFOPT1,RPFXAPRV                                                 
         BZ    *+10                                                             
         MVC   PAYAPRV,APNO        EXCLUDE APPROVED                             
         TM    RPFOPT1,RPFOAPRV                                                 
         BZ    *+10                                                             
         MVC   PAYAPRV,APONLY      APPROVED ONLY                                
         OI    PAYAPRVH+6,FVOXMT                                                
*                                                                               
         TM    RPFOPT1,RPFIDRFT                                                 
         BZ    *+10                                                             
         MVC   PAYDRFT,APYES       INCLUDE DRAFT ITEMS                          
         TM    RPFOPT1,RPFODRFT                                                 
         BZ    *+10                                                             
         MVC   PAYDRFT,APONLY      DRAFT ITEMS ONLY                             
         OI    PAYDRFTH+6,FVOXMT                                                
*                                                                               
         TM    RPFROPT,RPFBALCR                                                 
         BZ    *+10                                                             
         MVC   PAYCR$,APYES        SHOW CREDIT (NEGATIVE) BALANCES ONLY         
         OI    PAYCR$H+6,FVOXMT                                                 
*                                                                               
         TM    RPFOPT2,RPFXVNDR    EXCLUDE PAY=NO VENDORS                       
         BZ    *+10                                                             
         MVC   PAYVNDR,APNO                                                     
         TM    RPFOPT2,RPFOVNDR    ONLY PAY=NO VENDORS                          
         BZ    *+10                                                             
         MVC   PAYVNDR,APONLY                                                   
         OI    PAYVNDRH+6,FVOXMT                                                
*                                                                               
*        TM    RPFROPT,RPFBALO                                                  
*        BZ    *+10                                                             
*        MVC   PAYBALO,APYES       SHOW OUTSTANDING BALANCES ONLY               
*        OI    PAYBALOH+6,FVOXMT                                                
*                                                                               
         TM    RPFOPT2,RPFOCD$                                                  
         BZ    *+10                                                             
         MVC   PAYCD$,APYES       SHOW ACCOUNTS W/CASH DISCOUNT ONLY            
         OI    PAYCD$H+6,FVOXMT                                                 
*&&US                                                                           
         TM    RPFOPT6,RPFZPST                                                  
         BZ    *+10                                                             
         MVC   PAYZTRN,APYES      INCLUDE $0 TRANSACTIONS                       
         OI    PAYZTRNH+6,FVOXMT                                                
*&&                                                                             
         TM    RPFOPT2,RPFSPEC                                                  
         BZ    *+10                                                             
         MVC   PAYCSHD,APYES       CASH DISBURSEMENT                            
         OI    PAYCSHDH+6,FVOXMT                                                
*                                                                               
         TM    RPFOPT2,RPFXUGNT                                                 
         BZ    *+10                                                             
         MVC   PAYURGN,APNO        EXCLUDE URGENT                               
         TM    RPFOPT2,RPFOUGNT                                                 
         BZ    *+10                                                             
         MVC   PAYURGN,APONLY      URGENT ONLY                                  
         OI    PAYURGNH+6,FVOXMT                                                
*                                                                               
         TM    RPFOPT4,RPFXEXPS                                                 
         BZ    *+10                                                             
         MVC   PAYXJOB,APNO        EXCLUDE X-JOBS                               
         TM    RPFOPT4,RPFOEXPS                                                 
         BZ    *+10                                                             
         MVC   PAYXJOB,APONLY      X-JOBS ONLY                                  
         OI    PAYXJOBH+6,FVOXMT                                                
*&&UK                                                                           
         CLI   RPFLN,RPFLN2Q       TEST LATEST ELEMENT LENGTH                   
         BL    DISREC32            NO - OLD-STYLE, SO LEAVE DEFAULT             
         TM    RPFOPT7,RPFXMAPY                                                 
         BZ    *+10                                                             
         MVC   PAYMAPY,APNO        EXCLUDE MANUAL PAYMENTS                      
         TM    RPFOPT7,RPFOMAPY                                                 
         BZ    *+10                                                             
         MVC   PAYMAPY,APONLY      MANUAL PAYMENTS ONLY                         
         OI    PAYMAPYH+6,FVOXMT                                                
DISREC32 DS    0H                                                               
*&&                                                                             
         LA    R4,5                FIVE FILTER TYPES                            
         LA    R3,RPFFLT1                                                       
         LA    R1,PAYF1H                                                        
*                                                                               
DISREC38 CLI   0(R3),X'40'         ANY CHARACTER?                               
         BNH   DISREC40                                                         
         OI    6(R1),FVOXMT        TRANSMIT                                     
         MVC   8(1,R1),0(R3)                                                    
         TM    0(R3),X'40'         LOWER OR UPPER CASE?                         
         BNZ   DISREC40            UPPER                                        
         MVI   8(R1),C'*'          EXCLUDE FILTER                               
         MVC   9(1,R1),0(R3)                                                    
         OI    9(R1),X'40'         MAKE UPPER CASE                              
*                                                                               
DISREC40 SR    RE,RE                                                            
         IC    RE,0(,R1)           BUMP TO PROTECTED FIELD                      
         AR    R1,RE                                                            
         IC    RE,0(,R1)           BUMP TO NEXT INPUT FILTER                    
         AR    R1,RE                                                            
         LA    R3,1(,R3)           BUMP TO NEXT FILTER IN PROFILE ELEM          
         CLM   R4,1,=AL1(2)        F1-F4 DONE YET?                              
         BNE   *+8                 NOT YET                                      
         LA    R3,RPFFLT5          SWITCH TO F5                                 
         BCT   R4,DISREC38                                                      
*                                                                               
DISREC99 DS    0H                                                               
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   DISRC999                                                         
         CLI   APACTN,ACTDIS       IN   DISPLAY MODE ?                          
         BE    DISRC999            YES, SKIP                                    
         CLI   TWALREC,RECPAYPF    IF   FIRST TIME IN, THEN SET CURSOR          
         BE    DISRC999            NO,  SKIP                                    
*                                                                               
         LA    RE,PAYCDEH          FORMAT CODE FIELD                            
         ST    RE,APCURSOR         SET  APPLICATION CURSOR                      
*                                                                               
DISRC999 DS    0H                                                               
         B     EXIT                                                             
*                                                                               
         DROP R1,R9                KEEP IT CLEAN                                
         EJECT ,                                                                
         USING REPTABD,R1                                                       
         SPACE 1                                                                
EXPRPTY  MVC   PAYRPCDE,REPCODE                                                 
         MVC   PAYRPTYP,REPSTYPE                                                
         CLI   PAYRPCDE,ESCHIGHQ   TEST FOR ESCAPE SEQUENCE                     
         BNLR  RE                                                               
         DROP  R1                                                               
*                                                                               
         STM   RE,R1,SVREGS                                                     
         GOTO1 VDICTAT,APPARM,C'TU  ',('PAYRPLNQ',PAYRPCDE),0                   
         LM    RE,R1,SVREGS                                                     
         BR    RE                                                               
         EJECT ,                                                                
***********************************************************************         
* ERRORS TO DISPLAY ON TOP OF SCREEN                                  *         
***********************************************************************         
         SPACE 1                                                                
IVALRPTY MVC   FVMSGNO,=AL2(ACERPTYP)     INVALID REPORT TYPE                   
         MVC   FVXTRA(3),FFNUMBER-FFNELD+1(R1)                                  
         LA    R1,SCRTYPH                                                       
         ST    R1,FVADDR                                                        
         B     IVALEXIT                                                         
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
IVALCUL0 MVC   FVXTRA(10),12(R4)          INVALID UNIT/LEDGER                   
*                                                                               
IVALCULR MVC   FVMSGNO,=AL2(ACEIVUL)      INVALID UNIT/LEDGER                   
         B     IVALEXIT                                                         
*                                                                               
IVALIPUT MVC   FVMSGNO,=AL2(FVFNOTV)      INVALID INPUT                         
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALEXIT DS    0H                                                               
         B     EXIT                                                             
         EJECT ,                                                                
         SPACE 1                                                                
SCRTXT   DS    0F                                                               
         DC    AL4(PAYPA02H-TWAD,1652)                                          
*        DC    AL4(PAYPA08H-TWAD,1640)                                          
         DC    AL4(PAYPA08H-TWAD,1641)                                          
         DC    AL4(PAYPA09H-TWAD,1642)                                          
         DC    AL4(PAYPA10H-TWAD,1643)                                          
         DC    AL4(PAYPA11H-TWAD,1644)                                          
         DC    AL4(PAYPA12H-TWAD,1645)                                          
         DC    AL4(PAYPA13H-TWAD,1646)                                          
         DC    AL4(PAYPA14H-TWAD,1647)                                          
         DC    AL4(PAYPA15H-TWAD,1648)                                          
         DC    AL4(PAYPA16H-TWAD,1649)                                          
         DC    AL4(PAYPA17H-TWAD,1650)                                          
         DC    AL4(PAYPA18H-TWAD,1651)                                          
         DC    AL4(PAYPA19H-TWAD,5663)                                          
*&&US*&& DC    AL4(PAYPA20H-TWAD,5683)  INCLUDE $0 TRANSACTIONS                 
*&&UK*&& DC    AL4(PAYPA20H-TWAD,5683)  AUTHORIZATION STATUS                    
*&&UK*&& DC    AL4(PAYPA21H-TWAD,5691)  VAT REGION                              
*&&UK*&& DC    AL4(PAYPA22H-TWAD,1639)  INCLUDE MANUAL PAYMENTS                 
         DC    X'FF'                                                            
         EJECT ,                                                                
         LTORG                                                                  
         SPACE 2                                                                
         DROP  RA                                                               
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*                   DSECT FOR LOCAL WORKING STORAGE                   *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
LWSD     DSECT                                                                  
MAXPARM  EQU   20                                                               
MAXLDG   EQU   10                  MAXIMUM NUMBER OF LEDGERS                    
PENNY    EQU   C'P'                                                             
AUTHQ    EQU   C'A'                                                             
UNAUTHQ  EQU   C'U'                                                             
*                                                                               
SVREGS   DS    6A                                                               
PAYRPCDE DS    CL(L'REPCODE)                                                    
PAYRPTYP DS    CL(L'REPSTYPE)                                                   
PAYRPLNQ EQU   *-PAYRPCDE                                                       
PAYBLOCK DS    (MAXPARM)CL32                                                    
ONEXONLY DS    XL1                 ONE TIME ONLY FLAG                           
PARM_N   DS    XL1                 CURRENT PARAMETER WORKING ON                 
SAVEKEY1 DS    CL(L'RESKEY)                                                     
TEMPCHAR DS    CL1                                                              
TYPECDE  DS    CL4                                                              
MULTLDG  DS    AL4                 MULTIPLE  LEDGER BIT NUMBERS                 
TEMPLDG  DS    AL4                 TEMPORARY LEDGER BIT NUMBER                  
DUB      DS    D                                                                
WORK     DS    CL20                                                             
BYTE     DS    CL1                                                              
LDGUSED  DS    X                   NUMBER OF LEDGERS SPECIFIED                  
LDGSAVE  DS    (MAXLDG)CL(LUNLG)   SAVE AREA FOR LEDGERS SPECIFIED              
LDGSAVEL EQU   *-LDGSAVE           LENGTH OF LDGSAVE AREA                       
LWSX     DS    0C                                                               
         EJECT ,                                                                
         PRINT OFF                                                              
       ++INCLUDE ACSCRWRK                                                       
         PRINT ON                                                               
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*                    DSECT FOR PAY PROFILE DEFINITIONS                *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
TWAD     DSECT                                                                  
         ORG   SCROVLYH                                                         
       ++INCLUDE ACSCRF2D                                                       
         PRINT ON                                                               
         ORG                                                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'064ACSCR0B   08/27/15'                                      
         END                                                                    
