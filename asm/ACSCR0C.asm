*          DATA SET ACSCR0C    AT LEVEL 067 AS OF 08/27/15                      
*PHASE T60C0CA,+0                                                               
***********************************************************************         
*                                                                     *         
* THIS VERSION MATCHES THE UK VERSION LEVEL 67 AS OF 05/09/06         *         
*                                                                     *         
***********************************************************************         
         TITLE 'EXP PROFILE ELEMENTS EXTRA MAINTANCE'                           
T60C0C   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,60C0C,RA,RR=RE                                                 
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
         CLI   TWALREC,RECEXPPF    IF   FIRST TIME IN, THEN SET CURSOR          
         BE    SCR02               NO,  SKIP                                    
*                                                                               
SCR01    LA    RE,EXPCODEH         FORMAT CODE FIELD                            
         ST    RE,ACURSOR                                                       
*                                                                               
SCR02    DS    0H                                                               
         CLI   APMODE,APMVALK                                                   
         BNE   SCR10                                                            
         LA    R2,SCRTXT                                                        
*                                                                               
SCR05    CLI   0(R2),X'FF'         END OF TABLE?                                
         BE    SCR10                                                            
         L     R4,0(,R2)           GET HEADER ADDRESS                           
         AR    R4,R5                                                            
         OI    6(R4),FVOXMT        TRANSMIT FIELD                               
         L     R3,4(,R2)           GET SCREEN NUMBER                            
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
         GOTO1 AFVAL,EXPCODEH                                                   
         MVC   RESKFORM,FVIFLD                                                  
         BE    VALKEY15                                                         
         TM    EXPCODE+4,FVITHIS   ANY INPUT?                                   
         BZ    *+10                NO                                           
         MVC   SAVFORM,SPACES                                                   
         CLC   SAVFORM,SPACES                                                   
         BNH   IVALEKEY                                                         
         MVC   EXPCODE,SAVFORM                                                  
         OI    EXPCODEH+6,FVOXMT   TRANSMIT                                     
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
         MVC   SCRTYP(L'APREPCDE),APREPCDE    MAKE TYPE EXP                     
         CLC   APREPCDE,AC@EXP     IS IT TYPE EXP?                              
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
         DROP  R2                                                               
         EJECT ,                                                                
DISKEY   NI    TWASWPST,TURNOFF-TWASWAP                                         
         B     EXIT                                                             
         EJECT ,                                                                
         SPACE 1                                                                
VALREC   DS    0H                                                               
         L     R2,AIOAREA1                                                      
         GOTO1 AFVAL,EXPNMEH                                                    
         BNE   VALREC02            NAME HAS  NOT  BEEN INPUT                    
         GOTO1 ADDNAME,APPARM,(R2),EXPNMEH   ADD  FORMAT NAME                   
         BNE   VALREC99            ON   ERROR,    EXIT                          
*                                                                               
VALREC02 MVC   SAVEUL,ACLEDGER                                                  
         GOTO1 GETLEDG,=C'13'                                                   
         MVC   SVACLEV1,ACLEV1     MAKE LEVEL VALIDATION                        
         GOTO1 GETLEDG,SAVEUL      RESET UNIT/LEDGER                            
*                                  ************************************         
VALREC05 DS    0H                  * VALIDATE UNIT/LEDGER             *         
*                                  ************************************         
         GOTO1 MAKELIST,APPARM,('RFLLDG',EXPTYPEH),AIOAREA1,           X        
               ('MAXPARM',EXPBLOCK),APELEM                                      
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
         DROP  R1                                                               
*                                                                               
         XC    MULTLDG,MULTLDG     CLEAR                                        
         SR    R0,R0                                                            
         IC    R0,NPARMS                                                        
         LA    R4,EXPBLOCK                                                      
         MVI   LDGUSED,0                  CLEAR LEDGERS COUNT                   
         MVC   LDGSAVE(LDGSAVEL),SPACES   CLEAR LEDGERS SAVE AREA               
*                                                                               
         USING REPTABD,R3                                                       
         L     R3,ACTYPTAB                                                      
VALREC10 CLI   REPCODE,EOT                                                      
         BE    IVALCUL0                                                         
         GOTO1 EXPRPTY,(R3)                                                     
         CLC   APREPCDE,EXPRPCDE   MATCH REPORT TYPE                            
         BNE   VALREC15            NO, SO GET NEXT REPORT TYPE                  
         CLI   0(R4),2             MUST BE LENGTH TWO FOR NOW                   
         BNE   IVALCUL0                                                         
         CLI   REPNUM,C'M'                                                      
         BE    VALREC15                                                         
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
         BNE   IVALCUL0            NOT A VAILD LEDGER FOR COMPANY               
*                                                                               
         MVC   TEMPLDG,REPIND      GET   THIS LEDGER'S BIT NUMBER               
         NC    TEMPLDG,MULTLDG     'AND' WITH CURRENT MULTI-LEDGER BITS         
         BNZ   IVALEDUP            NOT   ZERO, ERROR DUPLICATE LEDGER           
         OC    MULTLDG,REPIND      GOOD, SAVE THIS LEDGER'S BIT NUMBER          
         ZIC   RE,LDGUSED          GET   NUMBER OF LEDGERS SO FAR               
         SLL   RE,1                TIMES TWO                                    
         LA    RF,LDGSAVE          GET   ADDRESS OF LEDGER SAVE AREA            
         AR    RF,RE               PLUS  BYTES USED SO FAR                      
         MVC   0(LULQ,RF),12(R4)   SAVE  UNIT/LEDGER                            
         SRL   RE,1                GET   NUMBER OF LEDGERS SO FAR               
         LA    RE,1(,RE)           ADD   ONE                                    
         STC   RE,LDGUSED          NEW   NUMBER OF LEDGERS USED                 
*                                                                               
         LA    R4,32(,R4)          BUMP  TO NEXT PARMETER                       
         MVC   APREPNUM,REPNUM                                                  
         L     R3,ACTYPTAB                                                      
         BCT   R0,VALREC10                                                      
         DROP  R3                                                               
*                                                                               
         L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)          ADD  REPORT TYPE ELEMENT X'C5'               
         BNE   VALREC99            ON   ERROR, EXIT                             
         CLI   NPARMS,1            ONLY ONE LEDGER                              
         BE    *+8                 YES                                          
         MVI   APREPNUM,C'M'       MULTI LEGDGER                                
         GOTO1 ADDREPTY,APPARM,(R2)                                             
         BNE   VALREC99            ON    ERROR, EXIT                            
         GOTO1 GETTYPE,(R2)        RESET APREP  VALUES                          
         MVC   APREPIND,MULTLDG                                                 
*                                  ************************************         
*                                  * BUILD PROFILE DATA ELEMENT       *         
*                                  ************************************         
         USING RPFELD,R9                                                        
         USING RESRECD,R2                                                       
         LA    R9,APELEM           BUILD DEFAULT PROFILE DATA                   
         MVC   RESKEY,APRECKEY                                                  
         XC    APELEM,APELEM                                                    
         MVI   RPFEL,RPFELQ        ELEMENT CODE                                 
         MVI   RPFLN,RPFLNQ        LENGTH OF ELEMENT                            
         OI    RPFPOPT,RPFBOX      TURN ON BIT FOR BOXES                        
         OI    RPFEDOPT,RPFEDCMA+RPFEDTRL      COMMAS, TRAILING MINUS           
         MVI   RPFPCTS,C'2'                                                     
         MVI   RPFRND,PENNY                                                     
         MVI   APELCODE,RPFELQ     GET PROFILE ELEMENT                          
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
*                                                                               
         CLC   EXPLOCK,APNO                                                     
         BNE   *+8                                                              
         OI    RPFROPT,RPFXLOCK    EXCLUDE LOCKED ACCOUNTS                      
         CLC   EXPLOCK,APONLY                                                   
         BNE   *+8                                                              
         OI    RPFROPT,RPFOLOCK    LOCKED ACCOUNTS ONLY                         
*                                                                               
         CLC   EXPRVRS,APNO                                                     
         BNE   *+8                                                              
         OI    RPFROPT,RPFXRVRS    EXCLUDE REVERESED ITEMS                      
         CLC   EXPRVRS,APONLY                                                   
         BNE   *+8                                                              
         OI    RPFROPT,RPFORVRS    REVERESED ITEMS ONLY                         
*                                                                               
         CLC   EXPDRFT,APYES                                                    
         BNE   *+8                                                              
         OI    RPFOPT1,RPFIDRFT    INCLUDE DRAFT ITEMS                          
         CLC   EXPDRFT,APONLY                                                   
         BNE   *+8                                                              
         OI    RPFOPT1,RPFODRFT    DRAFT ITEMS ONLY                             
*                                                                               
         CLC   EXPANDP,APNO                                                     
         BNE   *+8                                                              
         OI    RPFOPT3,RPFXADPT    EXCLUDE ANALYSIS DEPARTMENT                  
         CLC   EXPANDP,APONLY                                                   
         BNE   *+8                                                              
         OI    RPFOPT3,RPFOADPT    ANALYSIS DEPARTMENT ITEMS ONLY               
*                                                                               
         CLC   EXPANPR,APNO                                                     
         BNE   *+8                                                              
         OI    RPFOPT3,RPFXASTF    EXCLUDE ANALYSIS STAFF                       
         CLC   EXPANPR,APONLY                                                   
         BNE   *+8                                                              
         OI    RPFOPT3,RPFOASTF    ANALYSIS STAFF ITEMS ONLY                    
*                                                                               
         CLC   EXPANCL,APNO                                                     
         BNE   *+8                                                              
         OI    RPFOPT3,RPFXACLI    EXCLUDE ANALYSIS BY CLIENT                   
         CLC   EXPANCL,APONLY                                                   
         BNE   *+8                                                              
         OI    RPFOPT3,RPFOACLI    ANALYSIS BY CLIENT ITEMS ONLY                
*                                                                               
         XC    RPFFLT1(4),RPFFLT1  CLEAR FILTER AREA                            
         MVI   RPFFLT5,0           CLEAR FILTER 5                               
         LA    R4,5                FOR FILTER TYPES                             
         LA    R3,RPFFLT1                                                       
         LA    R1,EXPF1H                                                        
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
         BNE   *+8                 NOT  YET                                     
         LA    R3,RPFFLT5          SWITCH TO F5                                 
         BCT   R4,VALREC27                                                      
*                                                                               
         L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)          ADD  PROFILE ELEMENT X'C4'                   
         BNE   VALREC99            ON   ERROR, EXIT                             
         MVC   SAVEKEY1,IOKEY                                                   
         DROP  R9                                                               
*                                  ************************************         
*                                  * VALIDATE SPECIFIC ACCOUNT(S)     *         
*                                  ************************************         
         GOTO1 MAKELIST,APPARM,('RFLACC',EXPACCTH),(R2),               X        
               ('MAXPARM',EXPBLOCK),APELEM                                      
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VALREC30            NO   INPUT                                   
         BP    VALREC99            BAD  INPUT                                   
*                                  GOOD INPUT                                   
         GOTO1 VALACT,RFLACC       VALIDATE THE ACCOUNT(S)                      
         CLC   FVMSGNO,=AL2(FVFOK) WAS  AN ERROR FOUND ?                        
         BNE   VALREC99            YES, DISPLAY THE ERROR                       
*                                  ************************************         
VALREC30 DS    0H                  * VALIDATE TRANSACTION TYPE(S)     *         
*                                  ************************************         
         GOTO1 MAKELIST,APPARM,('RFLTTYPE',EXPITTYH),(R2),             X        
               ('MAXPARM',EXPBLOCK),APELEM                                      
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VALREC34            NO   INPUT                                   
         BP    VALREC99            BAD  INPUT                                   
*                                  GOOD INPUT                                   
         USING RFLELD,R9                                                        
         SR    R6,R6                                                            
         IC    R6,NPARMS           SCAN ENTRIES = NUMBER OF TYPES               
         LA    R9,APELEM                                                        
         LA    RF,RFLLNQ(,R6)      NEW  LENGTH OF ELEMENT                       
         STC   RF,RFLLN            STORE IN ELEMENT                             
         GOTO1 CNVTTYPE,APPARM,(C'N',EXPBLOCK),(NPARMS,RFLDATA)                 
         BNE   VALREC99            INVALID, ERROR                               
         L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)          ADD  INCLUDE TRANS TYPE                      
         BNE   VALREC99            ON   ERROR, EXIT                             
         DROP  R9                                                               
*                                  ************************************         
VALREC34 DS    0H                  * VALIDATE CONTRA ACCOUNT          *         
*                                  ************************************         
         GOTO1 MAKELIST,APPARM,('RFLCNTR',EXPCNTRH),(R2),              X        
               ('MAXPARM',EXPBLOCK),APELEM                                      
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VALREC36            NO   INPUT                                   
         BP    VALREC99            BAD  INPUT                                   
*                                  GOOD INPUT                                   
         GOTO1 VALACT,RFLCNTR      VALIDATE THE CONTRA ACCOUNT(S)               
         CLC   FVMSGNO,=AL2(FVFOK) WAS  AN ERROR FOUND ?                        
         BNE   VALREC99            YES, DISPLAY THE ERROR                       
*                                  ************************************         
VALREC36 DS    0H                  * VALIDATE COST ACCOUNT(S)         *         
*                                  ************************************         
         GOTO1 MAKELIST,APPARM,('RFLCOST',EXPCOSTH),(R2),              X        
               ('MAXPARM',EXPBLOCK),APELEM                                      
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VALREC38            NO   INPUT                                   
         BP    VALREC99            BAD  INPUT                                   
*                                  GOOD INPUT                                   
         GOTO1 VALACT,RFLCOST      VALIDATE COST ACCOUNT                        
         CLC   FVMSGNO,=AL2(FVFOK) WAS  AN ERROR FOUND ?                        
         BNE   VALREC99            YES, DISPLAY THE ERROR                       
*                                  ************************************         
VALREC38 DS    0H                  * VALIDATE OFFICE                  *         
*                                  ************************************         
         GOTO1 MAKELIST,APPARM,('RFLOFF',EXPOFFH),(R2),                X        
               ('MAXPARM',EXPBLOCK),APELEM                                      
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VALREC55            NO   INPUT                                   
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
         GOTO1 VOFFICE,APPARM,('VOFTYPEB',EXPBLOCK),(BYTE,(R4))                 
         BNE   VALREC99            BAD    INPUT                                 
*                                                                               
         L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)          ADD    OFFICE    LIST                        
         BNE   VALREC99            ON     ERROR,    EXIT                        
*                                  ************************************         
VALREC55 DS    0H                  * VALIDATE DEPARTMENT ACCOUNT(S)   *         
*                                  ************************************         
         GOTO1 MAKELIST,APPARM,('RFLDEPT',EXPDEPTH),(R2),              X        
               ('MAXPARM',EXPBLOCK),APELEM                                      
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VALREC64            NO   INPUT                                   
         BP    VALREC99            BAD  INPUT                                   
*                                  GOOD INPUT                                   
         GOTO1 VALACT,RFLDEPT      VALIDATE DEPARTMENT ACCOUNT(S)               
         CLC   FVMSGNO,=AL2(FVFOK) WAS  AN ERROR FOUND ?                        
         BNE   VALREC99            YES, DISPLAY THE ERROR                       
*                                  ************************************         
VALREC64 DS    0H                  * VALIDATE PERSON ACCOUNT(S)       *         
*                                  ************************************         
         GOTO1 MAKELIST,APPARM,('RFLPRSN',EXPPRSNH),(R2),              X        
               ('MAXPARM',EXPBLOCK),APELEM                                      
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VALREC66            NO   INPUT                                   
         BP    VALREC99            BAD  INPUT                                   
*                                  GOOD INPUT                                   
         GOTO1 VALACT,RFLPRSN      VALIDATE PERSON ACCOUNT(S)                   
         CLC   FVMSGNO,=AL2(FVFOK) WAS  AN ERROR FOUND ?                        
         BNE   VALREC99            YES, DISPLAY THE ERROR                       
*                                  ************************************         
VALREC66 DS    0H                  * VALIDATE EXPENSE ACCOUNT(S)      *         
*                                  ************************************         
         GOTO1 MAKELIST,APPARM,('RFLXCAT',EXPXCATH),(R2),              X        
               ('MAXPARM',EXPBLOCK),APELEM                                      
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VALREC68            NO   INPUT                                   
         BP    VALREC99            BAD  INPUT                                   
*                                  GOOD INPUT                                   
         GOTO1 VALACT,RFLXCAT      VALIDATE EXPENSE (13) ACCOUNT(S)             
         CLC   FVMSGNO,=AL2(FVFOK) WAS  AN ERROR FOUND ?                        
         BNE   VALREC99            YES, DISPLAY THE ERROR                       
*                                  ************************************         
VALREC68 DS    0H                  * VALIDATE VENDOR ACCOUNT(S)       *         
*                                  ************************************         
         GOTO1 MAKELIST,APPARM,('RFLVNDR',EXPVNDRH),(R2),              X        
               ('MAXPARM',EXPBLOCK),APELEM                                      
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VALREC70            NO   INPUT                                   
         BP    VALREC99            BAD  INPUT                                   
*                                  GOOD INPUT                                   
         GOTO1 VALACT,RFLVNDR      VALIDATE VENDOR ACCOUNT(S)                   
         CLC   FVMSGNO,=AL2(FVFOK) WAS  AN ERROR FOUND ?                        
         BNE   VALREC99            YES, DISPLAY THE ERROR                       
*                                                                               
VALREC70 DS    0H                  LABEL FOR ANY FUTURE FILTERS                 
*                                                                               
VALREC90 MVC   IOKEY,SAVEKEY1      RESTORE THE KEY                              
*                                  ************************************         
VALREC95 DS    0H                  * UPDATE THE RECORD WITH ALL THE   *         
*                                  * NEW ELEMENTS                     *         
*                                  ************************************         
         GOTO1 ADDID,APPARM,AIOAREA1                                            
         BNE   VALREC99            ON   ERROR,    EXIT                          
         LA    R1,IOADD+IOACCFIL+IO1                                            
         TM    APINDS,APIOKADD     ADD  A     RECORD ?                          
         BO    VALREC97            YES, OKAY                                    
         LA    R1,IOWRITE+IOACCFIL+IO1                                          
         TM    APINDS,APIOKCHA     CHANGE     A   RECORD ?                      
         BO    VALREC97 YES, OKAY                                               
         DC    H'0'                NO,  WHAT  THE HELL- ABEND                   
*                                                                               
VALREC97 DS    0H                                                               
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD  WRITE OR  SOMETHING DUDE                
*                                                                               
VALREC99 DS    0H                                                               
         CLC   FVMSGNO,=AL2(FVFOK)                                              
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
WILD100  CLI   0(RF),C'?'          U/L HAS WILDCARD ?                           
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
*                        DISPLAY EXP PROFILE DATA                     *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
DISREC   L     R2,AIOAREA1         PROFILE ELEMENT                              
         TWAXC EXPNMEH,EXPTABH                                                  
         GOTO1 GETNAME,APPARM,(R2),EXPNMEH                                      
         GOTO1 GETPER,APPARM,(R2),EXPOWNH                                       
*                                                                               
         MVC   EXPLOCK,APYES       DEFAULT, SHOW LOCKED ACCOUNTS                
         MVC   EXPDRFT,APNO        DEFAULT, EXCLUDE DRAFT ITEMS                 
         MVC   EXPRVRS,APYES       DEFAULT, INCLUDE REVERSED ITEMS              
         MVC   EXPANDP,APYES       ANALYSIS BY DEPARTMET                        
         MVC   EXPANPR,APYES       ANALYSIS BY STAFF                            
         MVC   EXPANCL,APYES       ANALYSIS BY CLIENT                           
*                                                                               
         USING RFLELD,R1                                                        
         MVI   APELCODE,RFLELQ     FILTER/LIST ELEMENT X'C5'                    
         GOTO1 GETEL,(R2)                                                       
         BNE   DISREC40                                                         
*                                                                               
DISREC10 CLI   RFLSEQ,0            HIGH LEVEL FILTER ONLY                       
         BNE   DISREC25                                                         
         SR    RF,RF                                                            
         SR    R6,R6                                                            
         IC    R6,RFLLN                                                         
         SH    R6,=Y(RFLLNQ+1)                                                  
         BM    DISREC25            SKIP ELEMENT IF LENGTH TOO SMALL             
*                                                                               
         CLI   RFLTYPE,RFLACC      **** ACCOUNT FILTER LIST ?                   
         BNE   *+8                                                              
         LA    RF,EXPACCTH         SPECIFIC ACCOUNT HEADER                      
*                                                                               
         CLI   RFLTYPE,RFLLDG      **** LEDGER LIST ?                           
         BNE   *+8                                                              
         LA    RF,EXPTYPEH         LEDGER LIST HEADER                           
*                                                                               
         CLI   RFLTYPE,RFLCOST     **** COST ACCOUNT FILTER LIST ?              
         BNE   *+8                                                              
         LA    RF,EXPCOSTH         COST ACCOUNT HEADER                          
*                                                                               
         CLI   RFLTYPE,RFLCNTR     **** CONTRA ACCOUNT(S) FILTER LIST ?         
         BNE   *+8                                                              
         LA    RF,EXPCNTRH         CONTRA ACCOUNT(S) HEADER                     
*                                                                               
         CLI   RFLTYPE,RFLOFF      **** OFFICE FILTER LIST ?                    
         BNE   *+8                                                              
         LA    RF,EXPOFFH          OFFICE HEADER                                
*                                                                               
         CLI   RFLTYPE,RFLDEPT     **** DEPARTMENT FILTER LIST ?                
         BNE   *+8                                                              
         LA    RF,EXPDEPTH         DEPARTMENT HEADER                            
*                                                                               
         CLI   RFLTYPE,RFLPRSN     **** PERSON FILTER LIST ?                    
         BNE   *+8                                                              
         LA    RF,EXPPRSNH         PERSON HEADER                                
*                                                                               
         CLI   RFLTYPE,RFLXCAT     **** EXPENSE CATEGORY 13 ACCOUNT ?           
         BNE   *+8                                                              
         LA    RF,EXPXCATH         EXPENSE 13 HEADER                            
*                                                                               
         CLI   RFLTYPE,RFLVNDR     **** VENDOR FILTER LIST ?                    
         BNE   *+8                                                              
         LA    RF,EXPVNDRH         VENDOR HEADER                                
*                                                                               
         CLI   RFLTYPE,RFLTTYPE    **** INCLUDE TRANSACTION TYPE ?              
         BNE   DISREC15                                                         
         LR    R0,R1                                                            
         GOTO1 CNVTTYPE,APPARM,(C'S',(R0)),(X'00',EXPITTY)                      
         LR    R1,R0                                                            
         OI    EXPITTYH+6,FVOXMT                                                
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
DISREC40 MVI   APELCODE,RPFELQ                                                  
         GOTO1 GETEL,(R2)                                                       
         BNE   DISREC99                                                         
         LR    R9,R1                                                            
*                                                                               
         TM    RPFROPT,RPFXLOCK                                                 
         BZ    *+10                                                             
         MVC   EXPLOCK,APNO        EXCLUDE LOCKED ACCOUNTS                      
         TM    RPFROPT,RPFOLOCK                                                 
         BZ    *+10                                                             
         MVC   EXPLOCK,APONLY      LOCKED ACCOUNTS ONLY                         
         OI    EXPLOCKH+6,FVOXMT                                                
*                                                                               
         TM    RPFROPT,RPFXRVRS                                                 
         BZ    *+10                                                             
         MVC   EXPRVRS,APNO        EXCLUDE REVERSALS                            
         TM    RPFROPT,RPFORVRS                                                 
         BZ    *+10                                                             
         MVC   EXPRVRS,APONLY      REVERSALS ONLY                               
         OI    EXPRVRSH+6,FVOXMT                                                
*                                                                               
         TM    RPFOPT1,RPFIDRFT                                                 
         BZ    *+10                                                             
         MVC   EXPDRFT,APYES       INCLUDE DRAFT ITEMS                          
         TM    RPFOPT1,RPFODRFT                                                 
         BZ    *+10                                                             
         MVC   EXPDRFT,APONLY      DRAFT ITEMS ONLY                             
         OI    EXPDRFTH+6,FVOXMT                                                
*                                                                               
         TM    RPFOPT3,RPFXADPT                                                 
         BZ    *+10                                                             
         MVC   EXPANDP,APNO        EXCLUDE ANALYSIS DEPARTMENT                  
         TM    RPFOPT3,RPFOADPT                                                 
         BZ    *+10                                                             
         MVC   EXPANDP,APONLY      ANALYSIS DEPARTMENT ONLY                     
         OI    EXPANDPH+6,FVOXMT                                                
*                                                                               
         TM    RPFOPT3,RPFXASTF                                                 
         BZ    *+10                                                             
         MVC   EXPANPR,APNO        EXCLUDE ANALYSIS STAFF                       
         TM    RPFOPT3,RPFOASTF                                                 
         BZ    *+10                                                             
         MVC   EXPANPR,APONLY      ANALYSIS STAFF ONLY                          
         OI    EXPANPRH+6,FVOXMT                                                
*                                                                               
         TM    RPFOPT3,RPFXACLI                                                 
         BZ    *+10                                                             
         MVC   EXPANCL,APNO        EXCLUDE ANALYSIS BY CLIENT                   
         TM    RPFOPT3,RPFOACLI                                                 
         BZ    *+10                                                             
         MVC   EXPANCL,APONLY      ANALYSIS BY CLIENT ONLY                      
         OI    EXPANCLH+6,FVOXMT                                                
*                                                                               
         LA    R4,5                FIVE FILTER TYPES                            
         LA    R3,RPFFLT1                                                       
         LA    R1,EXPF1H                                                        
*                                                                               
DISREC42 CLI   0(R3),X'40'         ANY CHARACTER?                               
         BNH   DISREC44                                                         
         OI    6(R1),FVOXMT        TRANSMIT                                     
         MVC   8(1,R1),0(R3)                                                    
         TM    0(R3),X'40'         LOWER OR UPPER CASE?                         
         BNZ   DISREC44            UPPER                                        
         MVI   8(R1),C'*'          EXCLUDE FILTER                               
         MVC   9(1,R1),0(R3)                                                    
         OI    9(R1),X'40'         MAKE UPPER CASE                              
*                                                                               
DISREC44 SR    RE,RE                                                            
         IC    RE,0(,R1)           BUMP TO PROTECTED FIELD                      
         AR    R1,RE                                                            
         IC    RE,0(,R1)           BUMP TO NEXT INPUT FILTER                    
         AR    R1,RE                                                            
         LA    R3,1(,R3)           BUMP TO NEXT FILTER IN PROFILE ELEM          
         CLM   R4,1,=AL1(2)        F1-F4 DONE YET?                              
         BNE   *+8                 NOT YET                                      
         LA    R3,RPFFLT5          SWICTH TO F5                                 
         BCT   R4,DISREC42                                                      
*                                                                               
DISREC99 DS    0H                                                               
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   DISRC999                                                         
         CLI   APACTN,ACTDIS       IN   DISPLAY MODE ?                          
         BE    DISRC999            YES, SKIP                                    
         CLI   TWALREC,RECEXPPF    IF   FIRST TIME IN, THEN SET CURSOR          
         BE    DISRC999            NO,  SKIP                                    
*                                                                               
         LA    RE,EXPCODEH         FORMAT CODE FIELD                            
         ST    RE,APCURSOR         SET  APPLICATION CURSOR                      
*                                                                               
DISRC999 DS    0H                                                               
         B     EXIT                                                             
         DROP  R1,R9                                                            
         EJECT ,                                                                
***********************************************************************         
*  VALIDATE AN ACCOUNT TYPE FIELD                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING ACFLTD,R3                                                        
VALACT   NTR1                                                                   
         LA    R3,ACFLTTAB                                                      
*                                                                               
VALACT05 CLI   0(R3),EOT           END    OF   TABLE     ?                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLM   R1,1,ACFLTTY        MATCH  TYPE OF   ACCOUNT                     
         BE    VALACT10                                                         
         LA    R3,ACFLTQ(,R3)      BUMP   UP   IN   TABLE                       
         B     VALACT05                                                         
*                                                                               
VALACT10 TM    ACFLTIND,ACFLTLST   VALIDATE    FOR  LIST ?                      
         BZ    VALACT20                                                         
         ZIC   R4,ACFLT#LG         INSERT NUM  OF   LEDGERS                     
         LA    R6,ACFLTLDG         ->     LEDGER    LIST                        
         TM    ACFLTIND,ACFLTSAV   USE    LDGSAVE   ?                           
         BZ    VALACT15            NO,    SKIP                                  
         IC    R4,LDGUSED          YES,   GET  NUM  OF   SAVED LEDGERS          
         LA    R6,LDGSAVE                 ->   SAVED     LEDGERS                
*                                                                               
VALACT15 DS    0H                                                               
         SR    R2,R2                                                            
         TM    ACFLTIND,ACFLTLGL   IS     LEDGER    LIST OKAY ?                 
         BZ    *+8                 NO,    SKIP                                  
         LA    R2,1                YES,   LEDGER    OR   ACCOUNT   LIST         
*                                  VALIDATE    THE  LIST                        
         GOTO1 VALLIST,APPARM,((R4),(R6)),(EXPBLOCK,EXPBLOCK+12),(R2)           
         LTR   RF,RF               TEST   THE  RETURN    CODE                   
         BM    VALACT20            NO     LIST                                  
         BP    VALACTX             BAD    LIST                                  
*                                  GOOD   LIST                                  
         CLI   NPARMS,2            HOW    MANY PARAMETERS     ?                 
         BL    VALACT90            ONE    IS   OK,  ADD  ACNT ELEMENT           
         BH    VALACT2M            >      2,   TOO  MANY LISTS                  
         TM    ACFLTIND,ACFLTDLT   SUPPORT     TWO  LISTS     ?                 
         BZ    VALACT2M            NO,    TOO  MANY LISTS                       
*                                                                               
         GOTO1 VALLIST,APPARM,((R4),(R6)),                             X        
               (EXPBLOCK+L'EXPBLOCK,EXPBLOCK+L'EXPBLOCK+12),(R2)                
         LTR   RF,RF               TEST   THE  RETURN    CODE                   
         BZ    VALACT90            GOOD   LIST,     ADD  ACNT ELEMENT           
         BP    VALACTX             BAD    LIST                                  
*                                  NO     LIST                                  
*                                  BUT    -    MUST BE   A    LIST              
         MVC   FVMSGNO,=AL2(ACEIVLT)                                            
         MVC   FVXTRA(20),EXPBLOCK+L'EXPBLOCK+12                                
         B     VALACTX             THEREFORE,  THIS IS   A    BAD  LIST         
*                                                                               
         USING RFLELD,R1           FILTER DATA ELEMENT   (X'C5')                
VALACT20 LA    R1,APELEM           ->     FILTER    ELEMENT                     
         TM    RFLIND,RFLXCLD      EXCLUDE     REQUESTED ?                      
         BZ    VALACT22            NO,    SKIP                                  
         TM    ACFLTIND,ACFLTEXC   EXCLUDE     SUPPORTED ?                      
         BZ    VALACTIP            NO,    INVALID   INPUT                       
         DROP  R1                                                               
*                                                                               
VALACT22 SR    R6,R6                                                            
         IC    R6,NPARMS                                                        
         LA    R4,EXPBLOCK                                                      
         LA    R2,IOKEY                                                         
*                                                                               
         USING ACTRECD,R2                                                       
VALACT25 CLI   0(R4),0             NULL   FIELD     ?                           
         BE    VALACT60            YES,   SKIP                                  
         CLC   ACFLTMIN,0(R4)      LENGTH OF   ACCOUNT   <    MIN  ?            
         BH    VALACTER            YES,   INVALID   ACCOUNT                     
         TM    ACFLTIND,ACFLTLV1   USE    SVACLEV1  FOR  MAX  LNG  ?            
         BO    VALACT30            YES,   SKIP                                  
         CLC   ACFLTMAX,0(R4)      LENGTH OF   ACCOUNT   >    MAX  ?            
         BL    VALACTER            YES,   INVALID   ACCOUNT                     
         B     VALACT35            OK,    CONTINUE                              
*                                                                               
VALACT30 CLC   SVACLEV1,0(R4)      LENGTH OF   ACCOUNT   >    MAX  ?            
         BL    VALACTER            YES,   INVALID   ACCOUNT                     
*                                                                               
VALACT35 MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUABIN      COMPANY     CODE                             
         MVC   ACTKUNT(2),ACFLTLDG ACCOUNT     LEDGER    TO   USE               
*                                                                               
         CLI   ACFLTTY,RFLCNTR     CONTRA TYPE ?                                
         BE    VALACT45            CHECK CONTRA U/L/ACCOUNT                     
         CLI   ACFLT#LG,1          IS THERE ONLY ONE LEDGER APPLICABLE          
         BE    VALACT45            YES, SO OK TO PROCESS                        
         CLI   0(R4),2             ACCOUNT NEEDS AT LEAST U/L                   
         BL    VALACTEL                                                         
*                                                                               
VALACT38 ZIC   R1,ACFLT#LG         NUMBER OF   LEDGERS                          
         LA    RF,ACFLTLDG         LIST   OF   VALID     LEDGERS                
         TM    ACFLTIND,ACFLTSAV   VALIDATE    AGAINST   LDGSAVE   ?            
         BZ    VALACT40            NO,    ALL  SET                              
         IC    R1,LDGUSED          NUMBER OF   LEDGERS                          
         LA    RF,LDGSAVE          LIST   OF   VALID     LEDGERS                
*                                                                               
VALACT40 CLC   12(2,R4),0(RF)      MATCH  UNIT/LEDGER                           
         BE    VALACT45            YES,   VALID     UNIT/LEDGER                 
         LA    RF,2(,RF)           BUMP   TO   NEXT UNIT/LEDGER                 
         BCT   R1,VALACT40         TEST   THE  NEXT UNIT/LEDGER                 
         B     VALACTEU            INVALID     UNIT/LEDGER                      
*                                                                               
VALACT45 LA    RF,ACTKACT          ->     ACCOUNT   PART OF   KEY               
         CLI   ACFLT#LG,1          IS THERE ONLY ONE LEDGER APPLICABLE          
         BE    *+8                 YES, SO START AT ACCOUNT                     
         LA    RF,ACTKUNT          NO , SO START AT UNIT/LEDGER                 
         MVC   0(14,RF),12(R4)     INSERT ACCOUNT   INTO KEY                    
*                                  VALIDATE    ACCOUNT                          
         GOTO1 AIO,IORD+IOACCFIL+IO2                                            
         BE    VALACT60            VALID  ACCOUNT                               
         TM    ACFLTIND,ACFLTWLD   WILDARD     VALID     ?                      
         BZ    VALACTER            NO,    INVALID   ACCOUNT                     
         MVI   BYTE,C'N'           NO ASSUME NO     U/L  FOR  WILDCARD          
         CLI   ACFLT#LG,1          IS THERE ONLY ONE LEDGER APPLICABLE          
         BE    *+8                 NO,    SKIP                                  
         MVI   BYTE,C'U'           YES,   CHECK     U/L  FOR  WILDCARD          
         BAS   RE,WILDCARD                                                      
         BNE   VALACTER            INVALID     ACCOUNT                          
*                                                                               
VALACT60 DS    0H                  VALID  ACCOUNT                               
         LA    R4,32(,R4)          NEXT   BLOCK     IN   EXPBLOCK               
         BCT   R6,VALACT25                                                      
*                                                                               
         CLI   ACFLTTY,RFLACC      ACCOUNT ?                                    
         BNE   VALACT90            NO                                           
         CLI   NPARMS,1            ONLY ONE ACCOUNT SPECIFIED                   
         BH    VALACT90            NO, MORE THAN ONE                            
         LA    R4,EXPBLOCK                                                      
         CLI   0(R4),2             LENGTH OF UNIT/LEDGER                        
         BNH   VALACTEL                                                         
*                                                                               
VALACT90 DS    0H                                                               
         L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)          ADD    SPECIFIC  ACCOUNT ELEMENT             
         B     VALACTX             EXIT                                         
*                                                                               
VALACTIP DS    0H                  INVALID          INPUT                       
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALACTX                                                          
*                                                                               
VALACT2M DS    0H                  TOO    MANY      LISTS                       
         MVC   FVMSGNO,=AL2(ACE2MANY)                                           
         B     VALACTX                                                          
*                                                                               
VALACTER DS    0H                  INVALID          ACCOUNT                     
         MVC   FVMSGNO,ACFLTERR    INSERT MESSAGE   NUMBER                      
         MVC   FVXTRA(20),12(R4)   DISPLAY ACCOUNT  IN   ERROR                  
         B     VALACTX                                                          
*                                                                               
VALACTEU DS    0H                  INVALID UNIT     LEDGER                      
         MVC   FVMSGNO,=AL2(ACEIVUL)                                            
         MVC   FVXTRA(20),12(R4)   DISPLAY ACCOUNT  IN   ERROR                  
         B     VALACTX                                                          
*                                                                               
VALACTEL DS    0H                  LEVEL TO SMALL                               
         MVC   FVMSGNO,=AL2(112)                                                
         MVC   FVXTRA(20),12(R4)   DISPLAY ACCOUNT  IN   ERROR                  
         B     VALACTX                                                          
*                                                                               
VALACTX  DS    0H                  EXIT                                         
         XIT1                                                                   
         EJECT ,                                                                
         USING REPTABD,R1                                                       
EXPRPTY  MVC   EXPRPCDE,REPCODE                                                 
         MVC   EXPRPTYP,REPSTYPE                                                
         CLI   EXPRPCDE,ESCHIGHQ   TEST FOR ESCAPE SEQUENCE                     
         BNLR  RE                                                               
         DROP  R1                                                               
*                                                                               
         STM   RE,R1,SVREGS                                                     
         GOTO1 VDICTAT,APPARM,C'TU  ',('EXPRPLNQ',EXPRPCDE),0                   
         LM    RE,R1,SVREGS                                                     
         BR    RE                                                               
         EJECT ,                                                                
***********************************************************************         
*  INVALID ERRORS TO DISPLAY ON TOP OF SCREEN                         *         
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
IVALCUL0 MVC   FVXTRA(10),12(R4)          INVALID UNIT/LEDGER                   
*                                                                               
IVALCULR MVC   FVMSGNO,=AL2(ACEIVUL)      INVALID UNIT/LEDGER                   
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALEXIT DS    0H                                                               
         B     EXIT                                                             
         EJECT ,                                                                
ACFLTTAB DS    0F                                                               
*                                  ACCOUNT                                      
         DC    AL1(RFLACC,2,14)                                                 
         DC    AL1(ACFLTLST+ACFLTSAV+ACFLTWLD+ACFLTEXC+ACFLTLGL)                
         DC    AL2(ACEACTLV)       INVALID ACCOUNT OR LEVEL                     
         DC    AL1(5)                                                           
         DC    CL18'SESBSLSF2D'                                                 
*                                  CONTRA ACCOUNT                               
         DC    AL1(RFLCNTR,1,14)                                                
         DC    AL1(ACFLTLST+ACFLTEXC+ACFLTLGL)                                  
         DC    AL2(ACEICNTR)       INVALID CONTRA                               
         DC    AL1(2)                                                           
         DC    CL18'  '                                                         
*                                  COST ACCOUNT LIST                            
         DC    AL1(RFLCOST,1,12)                                                
         DC    AL1(ACFLTLST+ACFLTWLD+ACFLTEXC)                                  
         DC    AL2(ACEACTLV)       INVALID ACCOUNT OR LEVEL                     
         DC    AL1(1)                                                           
         DC    CL18'1C'                                                         
*                                  PERSON                                       
         DC    AL1(RFLPRSN,1,12)                                                
         DC    AL1(ACFLTLST+ACFLTWLD+ACFLTEXC)                                  
         DC    AL2(ACEACTLV)       INVALID ACCOUNT OR LEVEL                     
         DC    AL1(1)                                                           
         DC    CL18'2P'                                                         
*                                  VENDOR                                       
         DC    AL1(RFLVNDR,1,14)                                                
         DC    AL1(ACFLTLST+ACFLTWLD+ACFLTEXC+ACFLTLGL)                         
         DC    AL2(ACEACTLV)       INVALID ACCOUNT OR LEVEL                     
         DC    AL1(3)                                                           
         DC    CL18'SVSXSY'                                                     
*                                  EXPENSE CATEGORY (13 ACCOUNT)                
         DC    AL1(RFLXCAT,1,12)                                                
         DC    AL1(ACFLTLST+ACFLTWLD+ACFLTEXC+ACFLTLV1)                         
         DC    AL2(ACEACTLV)       INVALID ACCOUNT OR LEVEL                     
         DC    AL1(1)                                                           
         DC    CL18'13'                                                         
*                                  DEPARTMENT                                   
         DC    AL1(RFLDEPT,1,12)                                                
         DC    AL1(ACFLTLST+ACFLTWLD+ACFLTEXC)                                  
         DC    AL2(ACEACTLV)       INVALID ACCOUNT OR LEVEL                     
         DC    AL1(1)                                                           
         DC    CL18'2D'                                                         
*                                                                               
         DC    AL1(EOT)                                                         
         EJECT ,                                                                
SCRTXT   DS    0F                                                               
         DC    AL4(EXPEX02H-TWAD,1652)                                          
         DC    AL4(EXPEX03H-TWAD,1671)                                          
         DC    AL4(EXPEX06H-TWAD,1654)                                          
         DC    AL4(EXPEX10H-TWAD,1655)                                          
         DC    AL4(EXPEX12H-TWAD,1646)                                          
         DC    AL4(EXPEX13H-TWAD,1647)                                          
         DC    AL4(EXPEX14H-TWAD,1649)                                          
         DC    AL4(EXPEX15H-TWAD,1668)                                          
         DC    AL4(EXPEX16H-TWAD,1669)                                          
         DC    AL4(EXPEX17H-TWAD,1670)                                          
         DC    X'FF'                                                            
         SPACE 4                                                                
         LTORG                                                                  
         EJECT ,                                                                
ACFLTD   DSECT                                                                  
ACFLTTY  DS    AL1                 REPORT FILTER NUMBER/TYPE                    
ACFLTMIN DS    AL1                 MIN  LENGTH REQUIRED                         
ACFLTMAX DS    AL1                 MAX  LENGTH OF ACCOUNT TO VERIFY             
*                                       W/O    EXCLUDE CHARACTER                
ACFLTIND DS    AL1                 INDICATOR                                    
ACFLTLST EQU   X'80'                    VALIDATE FOR  LIST TYPE                 
ACFLTDLT EQU   X'40'                    SUPPORT  TWO  LISTS                     
ACFLTSAV EQU   X'20'                    VALIDATE WITH LDGSAVE                   
ACFLTWLD EQU   X'08'                    WILDCARD SUPPORTED                      
ACFLTEXC EQU   X'04'                    EXCLUDE  SUPPORTED                      
ACFLTLV1 EQU   X'02'                    USE      SVACLEV1  FOR ACFLTMAX         
ACFLTLGL EQU   X'01'                    OKAY     LEDGER    LIST                 
ACFLTERR DS    AL2                 ERROR NUMBER                                 
ACFLT#LG DS    AL1                 NUMBER OF LEDGERS                            
ACFLTLDG DS    CL18                LEDGERS                                      
ACFLTQ   EQU   *-ACFLTD                                                         
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*                   DSECT FOR LOCAL WORKING STORAGE                   *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
LWSD     DSECT                                                                  
MAXPARM  EQU   20                                                               
MAXLDG   EQU   10                                                               
PENNY    EQU   C'P'                                                             
*                                                                               
SVREGS   DS    6A                                                               
EXPRPCDE DS    CL(L'REPCODE)                                                    
EXPRPTYP DS    CL(L'REPSTYPE)                                                   
EXPRPLNQ EQU   *-EXPRPCDE                                                       
EXPBLOCK DS    (MAXPARM)CL32                                                    
SAVEUL   DS    CL2                                                              
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
SVACLEV1 DS    XL1                 SAVE ACLEV1 FOR X13 ENTRY                    
LDGUSED  DS    X                   NUMBER OF LEDGERS SPECIFIED                  
LDGSAVE  DS    (MAXLDG)CL(LULQ)    SAVE AREA FOR LEDGERS SPECIFIED              
LDGSAVEL EQU   *-LDGSAVE           LENGTH OF LDGSAVE AREA                       
LWSX     DS    0C                                                               
         EJECT ,                                                                
         PRINT OFF                                                              
       ++INCLUDE ACSCRWRK                                                       
         PRINT ON                                                               
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*                    DSECT FOR EXP PROFILE DEFINITIONS                *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
TWAD     DSECT                                                                  
         ORG   SCROVLYH                                                         
       ++INCLUDE ACSCRF1D                                                       
         PRINT ON                                                               
         ORG                                                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'067ACSCR0C   08/27/15'                                      
         END                                                                    
