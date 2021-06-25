*          DATA SET ACSCR0A    AT LEVEL 013 AS OF 08/27/15                      
*PHASE T60C0AA,+0                                                               
***********************************************************************         
*                                                                     *         
* THIS VERSION MATCHES THE UK VERSION LEVEL 13 AS OF 05/09/06         *         
*                                                                     *         
***********************************************************************         
         TITLE 'INC PROFILE ELEMENTS EXTRA MAINTANCE'                           
T60C0A   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,60C0A,RA,RR=RE                                                 
*                                                                               
         USING TWAD,R5                                                          
         USING WORKD,R7                                                         
         USING LWSD,RC                                                          
*                                                                               
         L     RC,APALOCAL                                                      
*                                                                               
         USING RESRECD,R2                                                       
*                                                                               
         ST    RE,APRELO                                                        
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
         ST    RD,APWORKA                                                       
*                                                                               
         CLC   ATWA,ACURSOR        INSURE CURSOR WAS IN A FIELD                 
         BE    SCR01               NO,  SET CURSOR                              
         CLI   TWALREC,RECINCPF    IF FIRST TIME IN, THEN SET CURSOR            
         BE    SCR02               NO,  SKIP                                    
*                                                                               
SCR01    LA    RE,INCCODEH         FORMAT CODE FIELD                            
         ST    RE,ACURSOR                                                       
*                                                                               
SCR02    DS    0H                                                               
         CLI   APMODE,APMVALK                                                   
         BNE   SCR10                                                            
         L     R2,=A(SCRTXT)                                                    
         A     R2,APRELO                                                        
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
*        TM    TWAMODE,TWAMLSM     LIST/SELECT MODE?                            
*        BZ    EXIT30                                                           
*        CLI   APPFKEY,PFKEXIT     EXIT TO LIST                                 
*        BE    EXIT99                                                           
*                                                                               
EXIT30   TM    TWASWPST,TWASWAP    SWAP TO NEW RECORD ACTION?                   
         BZ    EXIT95              NO                                           
         XC    APCURSOR,APCURSOR   DON'T SET CURSOR ON WRONG SCREEN             
*        NI    TWASWPST,TURNOFF-TWASWAP                                         
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
         SPACE 1                                                                
VALKEY   DS    0H                                                               
         MVI   NEWKEY,NO                                                        
         MVC   RESKEY,SPACES                                                    
         MVI   RESKTYP,RESKTYPQ    X'2D'                                        
         MVI   RESKSUB,RESKSUBQ    X'02'                                        
         MVC   RESKCPY,CUABIN      COMPANY CODE                                 
         GOTO1 AFVAL,INCCODEH                                                   
         MVC   RESKFORM,FVIFLD                                                  
         BE    VALKEY15                                                         
         TM    INCCODE+4,FVITHIS   ANY INPUT?                                   
         BZ    *+10                NO                                           
         MVC   SAVFORM,SPACES                                                   
         CLC   SAVFORM,SPACES                                                   
         BNH   IVALEKEY                                                         
         MVC   INCCODE,SAVFORM                                                  
         OI    INCCODEH+6,FVOXMT   TRANSMIT                                     
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
         MVC   SCRTYP(L'APREPCDE),APREPCDE                                      
         OI    SCRTYPH+6,FVOXMT                                                 
         CLC   APREPCDE,AC@INC                                                  
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
         SPACE 1                                                                
DISKEY   NI    TWASWPST,TURNOFF-TWASWAP                                         
         B     EXIT                                                             
         EJECT ,                                                                
         SPACE 1                                                                
VALREC   DS    0H                                                               
         L     R2,AIOAREA1                                                      
         GOTO1 AFVAL,INCNMEH                                                    
         BNE   VREC050             NAME HAS  NOT  BEEN   INPUT                  
         GOTO1 ADDNAME,APPARM,(R2),INCNMEH   ADD  FORMAT NAME                   
         BNE   VREC999             ON   ERROR,    EXIT                          
*                                                                               
         USING REPTABD,R3                                                       
*                                                                               
VREC050  GOTO1 AFVAL,INCTYPEH                                                   
         L     R3,ACTYPTAB                                                      
*                                                                               
VREC100  CLI   REPCODE,EOT                                                      
         BE    IVALTYPE            INVALID TYPE USED                            
         LR    R1,R3                                                            
         BAS   RE,EXPRPTY                                                       
         CLC   APREPCDE,INCRPCDE   MATCH REPORT TYPE                            
         BNE   VREC150             NO, SO GET NEXT REPORT TYPE                  
         SR    R1,R1                                                            
         IC    R1,FVXLEN                                                        
         EXCLC R1,INCRPTYP,FVIFLD                                               
         BNE   VREC150             NO, SO GET NEXT REPORT TYPE                  
         TM    REPFLAG,REPDDS      DDS ONLY?                                    
         BZ    VREC200             NO, SO OK                                    
         TM    CUSTAT,CUSDDS       IS IT A DDS TERMINAL?                        
         BO    VREC200             YES, SO OK                                   
*                                                                               
VREC150  LA    R3,REPLNQ(,R3)      BUMP TO NEXT REPORT TYPE                     
         B     VREC100                                                          
*                                                                               
VREC200  MVC   APREPNUM,REPNUM     COULD BE SI, SK, OR 1C                       
         MVC   APREPCDE,INCRPCDE                                                
         GOTO1 ADDREPTY,APPARM,AIOAREA1                                         
         BNE   VREC999             ON   ERROR, EXIT                             
         DROP  R3                                                               
*                                                                               
         GOTO1 GETTYPE,(R2)                                                     
         MVI   LDGFLDH,10                                                       
         MVC   LDGFLD,APREPUL                                                   
         GOTO1 MAKELIST,APPARM,('RFLLDG',LDGFLDH),(R2),                X        
               ('MAXPARM',INCBLOCK),APELEM                                      
         GOTO1 ADDEL,(R2)          ADD X'C5' LEDGER FILTER                      
         BNE   VREC999             ON  ERROR, EXIT                              
         GOTO1 GETTYPE,(R2)        RESET APREP VALUES                           
*                                                                               
         USING RPFELD,R9                                                        
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
         BNE   VREC210                                                          
         SR    RF,RF                                                            
         IC    RF,1(,R1)           GET ELEMENT LENGTH                           
         BCTR  RF,0                                                             
         EXMVC RF,APELEM,0(R1)                                                  
         GOTO1 DELEL,(R2)                                                       
*                                                                               
VREC210  XC    RPFROPT,RPFROPT                                                  
         XC    RPFOPT1,RPFOPT1                                                  
*                                                                               
         CLC   INCRVRS,APNO                                                     
         BNE   *+8                                                              
         OI    RPFROPT,RPFXRVRS    EXCLUDE REVERSALS                            
         CLC   INCRVRS,APONLY                                                   
         BNE   *+8                                                              
         OI    RPFROPT,RPFORVRS    REVERSALS ONLY                               
*                                                                               
         CLC   INCTY56,APNO                                                     
         BE    *+8                                                              
         OI    RPFROPT,RPFTYP56    INCLUDE TYPE 56                              
*                                                                               
         CLC   INCLOCK,APNO                                                     
         BNE   *+8                                                              
         OI    RPFROPT,RPFXLOCK    EXCLUDE LOCKED ACCOUNTS                      
         CLC   INCLOCK,APONLY                                                   
         BNE   *+8                                                              
         OI    RPFROPT,RPFOLOCK    LOCKED ACCOUNTS ONLY                         
*&&US                                                                           
         CLC   INCDRFT,APYES                                                    
         BNE   *+8                                                              
         OI    RPFOPT1,RPFIDRFT    EXCLUDE DRAFT ITEMS                          
         CLC   INCDRFT,APONLY                                                   
         BNE   *+8                                                              
         OI    RPFOPT1,RPFODRFT    DRAFT ITEMS ONLY                             
*&&                                                                             
         XC    RPFFLT1(4),RPFFLT1  CLEAR FILTER AREA                            
         MVI   RPFFLT5,0           CLEAR F5 AREA                                
         LA    R4,5                FIVE FILTER TYPES                            
         LA    R3,RPFFLT1                                                       
         LA    R1,INCF1H                                                        
*                                                                               
VREC270  GOTO1 AFVAL                                                            
         BNE   VREC280                                                          
         MVC   0(1,R3),FVIFLD                                                   
         CLI   FVIFLD,C'*'         EXCLUDE FILTER?                              
         BNE   VREC280                                                          
         MVC   0(1,R3),FVIFLD+1                                                 
         NI    0(R3),TURNOFF-X'40' MAKE LOWER CASE                              
*                                                                               
VREC280  SR    RE,RE                                                            
         IC    RE,0(,R1)           BUMP TO PROTECTED FIELD                      
         AR    R1,RE                                                            
         IC    RE,0(,R1)           BUMP TO NEXT INPUT FILTER                    
         AR    R1,RE                                                            
         LA    R3,1(,R3)           BUMP TO NEXT FILTER IN PROFILE ELEM          
         CLM   R4,1,=AL1(2)        DONE F1-F4 YET                               
         BNE   *+8                 NOT YET                                      
         LA    R3,RPFFLT5          POINT TO PROFILE F5                          
         BCT   R4,VREC270                                                       
*                                                                               
         L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)          ADD PROFILE ELEMENT X'C4'                    
         BNE   VREC999             ON  ERROR, EXIT                              
         MVC   SAVEKEY1,IOKEY                                                   
*                                                                               
         USING DFLD,R3                                                          
*                                                                               
         L     R3,=A(DISFLD)                                                    
         A     R3,APRELO                                                        
*                                                                               
VREC300  SR    R6,R6               MATCH DISPLACEMENTS                          
         ICM   R6,3,DFLDSP         ANY  LEFT ?                                  
         BZ    VREC400             NO,  DONE                                    
         CLI   DFLRPTY,0           DEFAULT FIELD ?                              
         BE    VREC310             NO,  SKIP NEXT TEST                          
         CLC   DFLRPTY,APREPNUM    MATCHING REPORT TYPE ?                       
         BNE   VREC350             NO,  SKIP ENTRY                              
*                                                                               
VREC310  A     R6,ATWA             GET  THE FIELD                               
         SH    R6,=H'08'           POINT TO HEADER                              
         GOTO1 MAKELIST,APPARM,(DFLTYPE,(R6)),(R2),                    X        
               ('MAXPARM',INCBLOCK),APELEM                                      
         LTR   RF,RF               TEST THE  RETURN CODE                        
         BM    VREC350             NO   INPUT                                   
         BP    VREC999             BAD  INPUT                                   
*                                  GOOD INPUT                                   
         CLI   DFLROUT,0           ANY  ROUTINE ?                               
         BE    IVALIPUT            NO,  INVALID INPUT                           
*                                                                               
         USING RFLELD,R9                                                        
*                                                                               
         LA    R9,APELEM                                                        
         TM    RFLIND,RFLXCLD                                                   
         BZ    VREC330                                                          
         TM    DFLIND,DFLXCLD                                                   
         BZ    IVALIPUT                                                         
*                                                                               
         DROP  R9                  KEEP IT CLEAN                                
*                                                                               
VREC330  SR    RF,RF                                                            
         IC    RF,DFLROUT                                                       
         SLL   RF,2                                                             
         B     *(RF)                                                            
         B     VACCT               ACCOUNT                                      
         B     VCNTR               CONTRA                                       
         B     VCOST               COST ACCOUNT                                 
         B     VCLNT               CLIENT                                       
         B     VOFFC               OFFICE                                       
         B     VTYPE               TRANSACTION TYPE                             
*                                                                               
VREC340  L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)          ADD SPECIFIC RFLELD ELEMENT                  
         BNE   VREC999             ON  ERROR, EXIT                              
*                                                                               
VREC350  LA    R3,DFLLNQ(,R3)                                                   
         B     VREC300                                                          
*                                                                               
         DROP  R2                  KEEP IT CLEAN                                
         EJECT ,                                                                
***********************************************************************         
*  VALIDATE THE ACCOUNT                                               *         
***********************************************************************         
         SPACE 1                                                                
VACCT    DS    0H                                                               
         GOTO1 VALLIST,APPARM,APREPUL,(INCBLOCK,INCBLOCK+12),1                  
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VACCT10             NO   LIST                                    
         BP    VREC999             BAD  LIST                                    
*                                  GOOD LIST                                    
         CLI   NPARMS,1            ONLY 1 LIST REQUESTED ?                      
         BE    VACCT90             YES, ADD ACCOUNT LIST                        
         B     IVAL2MNY            NO,  TOO MANY LISTS                          
*                                                                               
VACCT10  DS    0H                  VALIDATE ACCOUNT(S)                          
         SR    R6,R6                                                            
         IC    R6,NPARMS                                                        
         LA    R4,INCBLOCK                                                      
         LA    R2,IOKEY                                                         
*                                                                               
         USING ACTRECD,R2                                                       
*                                                                               
VACCT20  DS    0H                                                               
         CLI   0(R4),LACCOUNT      IS   THE LENGTH OF ACCOUNT > 12 ?            
         BH    IVALACCT            YES, INVALID ACCOUNT                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUABIN      COMPANY CODE                                 
         MVC   ACTKUNT(2),APREPUL  UNIT LEDGER                                  
         MVC   CURUL,APREPUL       CURRENT UNIT/LEDGER                          
         MVC   ACTKACT,12(R4)      ACCOUNT IS IN INCBLOCK                       
         GOTO1 AIO,IORD+IOACCFIL+IO2                                            
         BE    VACCT30             VALID ACCOUNT                                
         MVI   BYTE,C'N'                                                        
         BAS   RE,WILDCARD         CHECK FOR WILD CARD CHARACTER                
         BNE   IVALACCT            INVALID ACCOUNT                              
*                                                                               
VACCT30  LA    R4,32(,R4)          NEXT BLOCK IN INCBLOCK                       
         BCT   R6,VACCT20                                                       
*                                                                               
VACCT90  DS    0H                  ADD  ACCOUNT LIST                            
         B     VREC340             EXIT                                         
*                                                                               
         DROP  R2                  KEEP IT CLEAN                                
         EJECT ,                                                                
***********************************************************************         
*  VALIDATE COST ACCOUNT                                              *         
***********************************************************************         
         SPACE 1                                                                
VCOST    DS    0H                                                               
         GOTO1 VALLIST,APPARM,=C'1C',(INCBLOCK,INCBLOCK+12),0                   
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VCOST10             NO   LIST                                    
         BP    VREC999             BAD  LIST                                    
*                                  GOOD LIST                                    
         CLI   NPARMS,1            ONLY 1 LIST REQUESTED ?                      
         BE    VCOST90             YES, ADD COST LIST                           
         B     IVAL2MNY            NO,  TOO MANY LISTS                          
*                                                                               
VCOST10  DS    0H                  VALIDATE COST(S)                             
         SR    R6,R6                                                            
         IC    R6,NPARMS                                                        
         LA    R4,INCBLOCK                                                      
         LA    R2,IOKEY                                                         
*                                                                               
         USING ACTRECD,R2                                                       
*                                                                               
VCOST20  DS    0H                                                               
         CLI   0(R4),LACCOUNT      IS   THE LENGTH OF ACCOUNT > 12 ?            
         BH    IVALACCT            YES, INVALID ACCOUNT                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUABIN      COMPANY CODE                                 
         MVC   ACTKUNT(2),=C'1C'   COST UNIT LEDGER                             
         MVC   CURUL,=C'1C'        CURRENT UNIT/LEDGER                          
         MVC   ACTKACT,12(R4)      ACCOUNT IS IN INCBLOCK                       
         GOTO1 AIO,IORD+IOACCFIL+IO2                                            
         BE    VCOST30             VALID ACCOUNT                                
         MVI   BYTE,C'N'                                                        
         BAS   RE,WILDCARD         CHECK FOR WILD CARD CHARACTER                
         BNE   IVALACCT            INVALID ACCOUNT                              
*                                                                               
VCOST30  LA    R4,32(,R4)          NEXT BLOCK IN INCBLOCK                       
         BCT   R6,VCOST20                                                       
*                                                                               
VCOST90  DS    0H                  ADD  COST RECORD                             
         B     VREC340                                                          
*                                                                               
         DROP  R2                  KEEP IT CLEAN                                
         EJECT ,                                                                
***********************************************************************         
*  VALIDATE CONTRA ACCOUNT                                            *         
*                                                                     *         
*  NOTE: WILDCARD CHARACTERS ARE NOT VALID FOR CONTRA ACCOUNTS        *         
***********************************************************************         
         SPACE 1                                                                
VCNTR    DS    0H                                                               
         MVC   SAVEKEY1,IOKEY                                                   
         MVC   APWORK(2),SPACES                                                 
         GOTO1 VALLIST,APPARM,APWORK,(INCBLOCK,INCBLOCK+12),1                   
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VCNTR10             NO   LIST                                    
         BP    VCNTR05             BAD  LIST                                    
*                                  GOOD LIST                                    
         CLI   NPARMS,1            ONLY 1 LIST REQUESTED ?                      
         BE    VCNTR90             YES, ADD CONTRA LIST                         
         B     IVAL2MNY            NO,  TOO MANY LISTS                          
*                                                                               
VCNTR05  DS    0H                                                               
         ZIC   R1,INCBLOCK         SHOW WHICH CONTRA CODE IS BAD                
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FVXTRA(0),INCBLOCK+12                                            
         B     VREC999             SHOW BAD LIST                                
*                                                                               
VCNTR10  DS    0H                  VALIDATE CONTRA ACCOUNT(S)                   
         SR    R6,R6                                                            
         IC    R6,NPARMS                                                        
         LA    R4,INCBLOCK                                                      
         LA    R2,IOKEY                                                         
*                                                                               
         USING ACTRECD,R2                                                       
*                                                                               
VCNTR20  DS    0H                                                               
         CLI   0(R4),LULACNT       IS   THE LENGTH OF CONTRA  > 14 ?            
         BH    IVALCNTR            YES, INVALID CONTRA ACCOUNT                  
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUABIN      COMPANY CODE                                 
         CLI   0(R4),2             MUST BE 2 OR GREATER                         
         BL    IVALCNTR            INVALID CONTRA ACCOUNT                       
*                                                                               
         USING CNTRTBLD,RF         CONRA ACCOUNT TABLE                          
*                                                                               
         L     RF,ACCNTTAB         ADDR OF CONTRA TABLE                         
*                                                                               
VCNTR30  DS    0H                                                               
         CLC   APREPNUM,CNTRRTYP   MATCHING REPORT TYPE ?                       
         BNE   VCNTR40             NO,  TRY NEXT ENTRY                          
         CLC   CNTRUNLG,12(R4)     MATCHING UNIT/LEDGER ?                       
         BE    VCNTR50             YES, GOOD UNIT/LEDGER                        
*                                                                               
VCNTR40  DS    0H                                                               
         LA    RF,CNTRLNQ(,RF)     NEXT TABLE ENTRY                             
         CLI   0(RF),EOT           END  OF CONTRA TABLE ?                       
         BNE   VCNTR30             NO,  TRY AGAIN                               
         B     IVALCNTR            YES, INVALID CONTRA UNIT/LEDGER              
*                                                                               
         DROP  RF                  KEEP IT CLEAN                                
*                                                                               
VCNTR50  DS    0H                  VALID CONTRA UNIT/LEDGER                     
         MVC   ACTKUNT(14),12(R4)  CONTRA ACCOUNT IS IN INCBLOCK                
         GOTO1 AIO,IORD+IOACCFIL+IO2                                            
         BNE   IVALCNTR            INVALID CONTRA ACCOUNT                       
*                                                                               
         LA    R4,32(,R4)          NEXT BLOCK IN INCBLOCK                       
         BCT   R6,VCNTR20                                                       
*                                                                               
VCNTR90  B     VREC340             ADD  CONTRA ACCOUNT                          
*                                                                               
         DROP  R2                  KEEP IT CLEAN                                
         EJECT ,                                                                
***********************************************************************         
*  VALIDATE CLIENT                                                    *         
*                                                                     *         
*  SUPPORTS A STRING LIST AS WELL AS ONE OR TWO +/- LISTS             *         
***********************************************************************         
         SPACE 1                                                                
VCLNT    DS    0H                                                               
         GOTO1 VALLIST,APPARM,=C'SJ',(INCBLOCK,INCBLOCK+12),0                   
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VCLNT10             NO   LIST                                    
         BP    VREC999             BAD  LIST                                    
*                                  GOOD LIST                                    
         CLI   NPARMS,2            SUPPORTS UP TO TWO LIST                      
         BL    VCLNT90             ONE  LIST, ADD THE VALID LIST                
         BH    IVAL2MNY            TOO  MANY                                    
         GOTO1 VALLIST,APPARM,=C'SJ',                                  X        
               (INCBLOCK+L'INCBLOCK,INCBLOCK+L'INCBLOCK+12),0                   
         LTR   RF,RF               TEST THE RETURN CODE                         
         BZ    VCLNT90             GOOD LIST, ADD THE VALID LISTS               
         BP    VREC999             BAD  LIST                                    
         B     IVALLIST            NO   LIST                                    
*                                                                               
VCLNT10  DS    0H                                                               
         MVI   FLDMAXL,LACCOUNT    MAX  LENGTH    OF   FIELD                    
         CLC   APREPUL,=C'1C'      INCOME/COSTING                               
         BNE   VCLNT15             NO,  SKIP                                    
         MVC   SAVEUL,ACLEDGER     SAVE U/L  INFORMATION                        
         LA    R1,=C'SJ'                                                        
         GOTO1 GETLEDG,(R1)        FIND SIZE OF   LVL  1                        
         MVC   FLDMAXL,ACLEV1      SAVE LVL  1    SIZE                          
         GOTO1 GETLEDG,SAVEUL      RESET     UNIT/LEDGER                        
*                                                                               
VCLNT15  DS    0H                                                               
         SR    R6,R6                                                            
         IC    R6,NPARMS                                                        
         LA    R4,INCBLOCK                                                      
         LA    R2,IOKEY                                                         
*                                                                               
         USING ACTRECD,R2                                                       
*                                                                               
VCLNT20  DS    0H                                                               
         CLC   0(1,R4),FLDMAXL     IS   THE LENGTH OF CLIENT  > MAX ?           
         BH    IVALCLI             YES, INVALID CLIENT                          
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUABIN      COMPANY CODE                                 
         MVC   ACTKUNT(2),=C'SJ'   UNIT LEDGER                                  
         MVC   ACTKACT,12(R4)      CLIENT                                       
         GOTO1 AIO,IORD+IOACCFIL+IO2                                            
         BE    VCLNT30             VALID CLIENT                                 
         MVI   BYTE,C'N'                                                        
         BAS   RE,WILDCARD         CHECK FOR WILD CARD CHARACTER                
         BNE   IVALCLI             INVALID CLIENT                               
*                                                                               
VCLNT30  LA    R4,32(,R4)          NEXT BLOCK IN INCBLOCK                       
         BCT   R6,VCLNT20                                                       
*                                                                               
VCLNT90  DS    0H                                                               
         B     VREC340             ADD CLIENT LIST                              
*                                                                               
         DROP  R2                  KEEP IT CLEAN                                
         EJECT ,                                                                
***********************************************************************         
*  VALIDATE OFFICE                                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING RFLELD,R4                                                        
         SPACE 1                                                                
VOFFC    DS    0H                                                               
         MVI   BYTE,NO             ASSUME INCLUDE                               
         LA    R4,APELEM           ->     ELEMENT                               
         TM    RFLIND,RFLXCLD      EXCLUDE     FILTER ?                         
         BZ    *+8                 NO,    SKIP                                  
         MVI   BYTE,YES            SAY    EXCLUDE                               
*                                                                               
         DROP  R4                  KEEP   IT   CLEAN                            
*                                                                               
         ZIC   R4,NPARMS           NUMBER OF   OFFICES                          
         GOTO1 VOFFICE,APPARM,('VOFTYPEB',INCBLOCK),(BYTE,(R4))                 
         BNE   VREC999             BAD    INPUT                                 
         B     VREC340             GOOD   INPUT                                 
         EJECT ,                                                                
***********************************************************************         
*  VALIDATE TRANSACTION TYPE(S)                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING RFLELD,R9                                                        
         SPACE 1                                                                
VTYPE    DS    0H                                                               
         SR    R6,R6                                                            
         IC    R6,NPARMS           SCAN ENTRIES = NUMBER OF TYPES               
         LA    R9,APELEM                                                        
         LA    RF,RFLLNQ(,R6)      NEW  LENGTH OF ELEMENT                       
         STC   RF,RFLLN            SAVE LENGTH IN ELEMENT                       
         GOTO1 CNVTTYPE,APPARM,(C'N',INCBLOCK),(NPARMS,RFLDATA)                 
         BE    VREC340             ADD  INCLUDE TRANSACTION TYPE(S)             
         B     VREC999                                                          
*                                                                               
         DROP  R9                                                               
         EJECT ,                                                                
***********************************************************************         
*  UPDATE THE RECORD WITH ALL THE ELEMENTS                            *         
***********************************************************************         
         SPACE 1                                                                
VREC400  DS    0H                                                               
         MVC   IOKEY,SAVEKEY1                                                   
         SPACE 1                                                                
VREC950  DS    0H                                                               
         GOTO1 ADDID,APPARM,AIOAREA1                                            
         BNE   VREC999             ON     ERROR,    EXIT                        
         LA    R1,IOADD+IOACCFIL+IO1                                            
         TM    APINDS,APIOKADD     ADD    A    RECORD ?                         
         BO    VREC970             YES,   OKAY                                  
         LA    R1,IOWRITE+IOACCFIL+IO1                                          
         TM    APINDS,APIOKCHA     CHANGE A    RECORD ?                         
         BO    VREC970             YES,   OKAY                                  
         DC    H'0'                NO,    WHAT THE HELL - ABEND !               
*                                                                               
VREC970  DS    0H                                                               
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD   WRITE OR SOMETHING DUDE                
*                                                                               
VREC999  CLC   FVMSGNO,=AL2(FVFOK)                                              
         BE    DISREC                                                           
         B     EXIT                                                             
         EJECT ,                                                                
***********************************************************************         
*  WILDCARD                                                           *         
***********************************************************************         
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
*                                                                               
WILD100  CLI   0(RF),C'?'          U/L  HAS WILDCARD ?                          
         BE    WILDNO              WILDCARD INVALID                             
         LA    RF,1(,RF)           NEXT CHAR                                    
         BCTR  R1,0                                                             
         BCT   R2,WILD100                                                       
*                                                                               
WILD200  CLI   0(RF),C'?'          ACCOUNT HAS WILDCARD ?                       
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
*                                                                               
         OI    RFLIND,RFLWILD      TURN ON THE BIT                              
         CR    RE,RE               WILDCARD IS GOOD                             
         B     XIT                                                              
*                                                                               
         DROP  R1                                                               
         EJECT ,                                                                
***********************************************************************         
*  DISPLAY INC PROFILE DATA                                           *         
***********************************************************************         
         SPACE 1                                                                
DISREC   L     R2,AIOAREA1         PROFILE ELEMENT                              
         TWAXC INCNMEH,INCTABH                                                  
         GOTO1 GETNAME,APPARM,(R2),INCNMEH                                      
         GOTO1 GETPER,APPARM,(R2),INCOWNH                                       
*                                                                               
         MVC   INCTYPE(L'APREPTYP),APREPTYP                                     
         MVC   INCRVRS,APYES       DEFAULT, INCLUDE REVERSAL                    
         MVC   INCLOCK,APYES       DEFAULT, SHOW LOCKED ACCOUNTS                
         MVC   INCTY56,APNO        DEFAULT, INCLUDE TYPE 56                     
*&&US*&& MVC   INCDRFT,APNO        DEFAULT, NOT TO INCLUDE DRAFT ITEMS          
*                                                                               
         USING RFLELD,R1                                                        
*                                                                               
         MVI   APELCODE,RFLELQ     FILTER/LIST ELEMENT X'C5'                    
         GOTO1 GETEL,(R2)                                                       
         BNE   DISREC30                                                         
*                                                                               
DISREC10 CLI   RFLSEQ,0            HIGH LEVEL ONLY                              
         BNE   DISREC22                                                         
         SR    R6,R6                                                            
         IC    R6,RFLLN                                                         
         SH    R6,=Y(RFLLNQ+1)                                                  
         BM    DISREC22            NO   DATA, SKIP THIS ELEMENT                 
*                                                                               
DISREC12 L     R3,=A(DISFLD)       START OF TABLE                               
         A     R3,APRELO                                                        
         SR    R4,R4                                                            
*                                                                               
DISREC13 ICM   R4,3,DFLDSP         ANY DISPLACMENT TO FIELD?                    
         BZ    DISREC22            NO SO DON'T BOTHER                           
         CLC   RFLTYPE,DFLTYPE     MATCH ON TYPE                                
         BNE   DISREC14            NO, SO TRY NEXT                              
         CLI   DFLRPTY,0           MATCH REPORT TYPE                            
         BE    DISREC16            DEFAULT                                      
         CLC   DFLRPTY,APREPNUM                                                 
         BE    DISREC16                                                         
*                                                                               
DISREC14 LA    R3,DFLLNQ(,R3)                                                   
         B     DISREC13                                                         
*                                                                               
DISREC16 AR    R4,R5               BASE OF TWA                                  
         LR    R8,R4               LOAD ELEMENT ADDRESS                         
         SH    R8,=H'8'            GET  HEADER  ADDRESS                         
         CLI   RFLTYPE,RFLTTYPE    SPECIAL FOR TRANSACTION TYPE                 
         BE    DISREC18                                                         
         TM    RFLIND,RFLXCLD                                                   
         BZ    DISREC17                                                         
         MVI   0(R4),C'*'                                                       
         LA    R4,1(,R4)                                                        
*                                                                               
DISREC17 DS    0H                                                               
         EXMVC R6,0(R4),RFLDATA                                                 
         OI    6(R8),FVOXMT        TRANSMIT FIELD                               
         B     DISREC22                                                         
*                                                                               
DISREC18 LR    R9,R1               SAVE ADDRESS TO ELEMENT                      
         GOTO1 CNVTTYPE,APPARM,(C'S',(R9)),(X'00',INCITTY)                      
         LR    R1,R9               RESTORE ADDRESS TO ELEMENT                   
         OI    INCITTYH+6,FVOXMT   TRANSMIT FIELD                               
*                                                                               
DISREC22 GOTO1 NEXTEL                                                           
         BE    DISREC10                                                         
*                                                                               
         USING RPFELD,R9                                                        
DISREC30 MVI   APELCODE,RPFELQ                                                  
         GOTO1 GETEL,(R2)                                                       
         BNE   DISREC99                                                         
         LR    R9,R1                                                            
*                                                                               
         TM    RPFROPT,RPFXRVRS                                                 
         BZ    *+10                                                             
         MVC   INCRVRS,APNO        EXCLUDE REVERSALS                            
         TM    RPFROPT,RPFORVRS                                                 
         BZ    *+10                                                             
         MVC   INCRVRS,APONLY      REVERSALS ONLY                               
         OI    INCRVRSH+6,FVOXMT   TRANSMIT                                     
*                                                                               
         TM    RPFROPT,RPFTYP56                                                 
         BZ    *+10                                                             
         MVC   INCTY56,APYES       INCLUDE REVERSALS TYPE 56                    
         OI    INCTY56H+6,FVOXMT   TRANSMIT                                     
*                                                                               
         TM    RPFROPT,RPFXLOCK                                                 
         BZ    *+10                                                             
         MVC   INCLOCK,APNO        EXCLUDE LOCKED ACCOUNTS                      
         TM    RPFROPT,RPFOLOCK                                                 
         BZ    *+10                                                             
         MVC   INCLOCK,APONLY      LOCKED ACCOUNTS ONLY                         
         OI    INCLOCKH+6,FVOXMT   TRANSMIT                                     
*&&US                                                                           
         TM    RPFOPT1,RPFIDRFT                                                 
         BZ    *+10                                                             
         MVC   INCDRFT,APYES       INCLUDE DRAFT ITEMS                          
         TM    RPFOPT1,RPFODRFT                                                 
         BZ    *+10                                                             
         MVC   INCDRFT,APONLY      DRAFT ITEMS ONLY                             
         OI    INCDRFTH+6,FVOXMT   TRANSMIT                                     
*&&                                                                             
         LA    R4,5                FIVE FILTER TYPES                            
         LA    R3,RPFFLT1                                                       
         LA    R1,INCF1H                                                        
*                                                                               
DISREC32 CLI   0(R3),X'40'         ANY CHARACTER?                               
         BNH   DISREC34                                                         
         OI    6(R1),FVOXMT        TRANSMIT                                     
         MVC   8(1,R1),0(R3)                                                    
         TM    0(R3),X'40'         LOWER OR UPPER CASE?                         
         BNZ   DISREC34            UPPER                                        
         MVI   8(R1),C'*'          EXCLUDE FILTER                               
         MVC   9(1,R1),0(R3)                                                    
         OI    9(R1),X'40'         MAKE UPPER CASE                              
*                                                                               
DISREC34 SR    RE,RE                                                            
         IC    RE,0(,R1)           BUMP TO PROTECTED FIELD                      
         AR    R1,RE                                                            
         IC    RE,0(,R1)           BUMP TO NEXT INPUT FILTER                    
         AR    R1,RE                                                            
         LA    R3,1(,R3)           BUMP TO NEXT FILTER IN PROFILE ELEM          
         CLM   R4,1,=AL1(2)        DONE F1-F4 YET                               
         BNE   *+8                 NOT YET                                      
         LA    R3,RPFFLT5          POINT TO PROFILE F5                          
         BCT   R4,DISREC32                                                      
*                                                                               
DISREC99 DS    0H                                                               
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   DISRC999                                                         
         CLI   APACTN,ACTDIS       IN   DISPLAY MODE ?                          
         BE    DISRC999            YES, SKIP                                    
         CLI   TWALREC,RECINCPF    IF   FIRST TIME IN, THEN SET CURSOR          
         BE    DISRC999            NO,  SKIP                                    
*                                                                               
         LA    RE,INCCODEH         FORMAT CODE FIELD                            
         ST    RE,APCURSOR         SET  APPLICATION CURSOR                      
*                                                                               
DISRC999 DS    0H                                                               
         B     EXIT                                                             
*                                                                               
         DROP  R1,R9               KEEP IT CLEAN                                
         EJECT ,                                                                
         SPACE 1                                                                
         USING REPTABD,R1                                                       
         SPACE 1                                                                
EXPRPTY  MVC   INCRPCDE,REPCODE                                                 
         MVC   INCRPTYP,REPSTYPE                                                
         CLI   INCRPCDE,ESCHIGHQ   TEST FOR ESCAPE SEQUENCE                     
         BNLR  RE                                                               
*                                                                               
         DROP  R1                                                               
*                                                                               
         STM   RE,R1,SVREGS                                                     
         GOTO1 VDICTAT,APPARM,C'TU  ',('INCRPLNQ',INCRPCDE),0                   
         LM    RE,R1,SVREGS                                                     
         BR    RE                                                               
         EJECT ,                                                                
***********************************************************************         
* ERRORS TO DISPLAY ON TOP OF SCREEN                                  *         
***********************************************************************         
         SPACE 1                                                                
IVALTYPE MVC   FVMSGNO,=AL2(ACERPTYP)                                           
         B     IVALEXIT                                                         
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
IVAL2MNY MVC   FVMSGNO,=AL2(ACE2MANY)     TOO MANY PARAMETERS                   
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALACCT MVC   FVMSGNO,=AL2(ACEACTLV)     INVALID ACCOUNT OR LEVEL              
         MVC   FVXTRA(2),CURUL                                                  
         MVC   FVXTRA+2(20),12(R4)                                              
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALCNTR MVC   FVMSGNO,=AL2(ACEICNTR)     INVALID CONTRA ACCOUNT                
         MVC   FVXTRA(20),12(R4)                                                
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALCLI  MVC   FVMSGNO,=AL2(ACECLI)       INVALID CLEINT                        
         MVC   FVXTRA(20),12(R4)                                                
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALLIST MVC   FVMSGNO,=AL2(ACEIVLT)      INVALID LIST USED                     
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALIPUT MVC   FVMSGNO,=AL2(FVFNOTV)      INVALID INPUT                         
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALEXIT DS    0H                                                               
         B     EXIT                                                             
         EJECT ,                                                                
         SPACE 1                                                                
SCRTXT   DS    0F                                                               
         DC    AL4(INCIN02H-TWAD,1653)                                          
         DC    AL4(INCIN03H-TWAD,1654)                                          
         DC    AL4(INCIN04H-TWAD,1671)                                          
         DC    AL4(INCIN09H-TWAD,1647)                                          
         DC    AL4(INCIN10H-TWAD,1656)                                          
         DC    AL4(INCIN11H-TWAD,1646)                                          
*&&US*&& DC    AL4(INCIN12H-TWAD,1649)                                          
         DC    X'FF'                                                            
         EJECT ,                                                                
         SPACE 1                                                                
DISFLD   DC    AL2(INCACT-TWAD),AL1(XCLD+ALST+SLST,RFLACC,REP#INC,1)            
         DC    AL2(INCACT-TWAD),AL1(XCLD+ALST+SLST,RFLACC,REP#SUP,1)            
         DC    AL2(INCACT-TWAD),AL1(XCLD+ALST+SLST,RFLCOST,REP#ICST,0)          
         DC    AL2(INCCOST-TWAD),AL1(XCLD+ALST+SLST,RFLACC,REP#ICST,1)          
         DC    AL2(INCCOST-TWAD),AL1(XCLD+ALST+SLST,RFLCOST,REP#INC,3)          
         DC    AL2(INCCOST-TWAD),AL1(XCLD+ALST+SLST,RFLCOST,REP#SUP,3)          
         DC    AL2(INCCNTR-TWAD),AL1(XCLD+ALST+SLST,RFLCNTR,0,2)                
         DC    AL2(INCCLI-TWAD),AL1(XCLD+ALST+SLST,RFLCLI,0,4)                  
         DC    AL2(INCOFF-TWAD),AL1(XCLD+SLST,RFLOFF,0,5)                       
         DC    AL2(INCITTY-TWAD),AL1(XCLD+SLST,RFLTTYPE,0,6)                    
         DC    AL2(0)                                                           
         EJECT ,                                                                
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 4                                                                
         DROP  R3,RA               KEEP IT CLEAN                                
         EJECT ,                                                                
***********************************************************************         
*  DSECT FOR LOCAL WORKING STORAGE                                    *         
***********************************************************************         
         SPACE 1                                                                
LWSD     DSECT                                                                  
MAXPARM  EQU   20                                                               
PENNY    EQU   C'P'                                                             
XCLD     EQU   X'80'               EXCLUDE STRING                               
ALST     EQU   X'40'               ACCOUNT LIST (+,-)                           
SLST     EQU   X'20'               STRING LIST (COMMA DELIMITED)                
*                                                                               
DUB      DS    D                                                                
SVREGS   DS    6A                                                               
INCRPCDE DS    CL(L'REPCODE)                                                    
INCRPTYP DS    CL(L'REPSTYPE)                                                   
INCRPLNQ EQU   *-INCRPCDE                                                       
INCBLOCK DS    (MAXPARM)CL32                                                    
ONEXONLY DS    XL1                 ONE  TIME ONLY FLAG                          
PARM_N   DS    XL1                 CURRENT PARAMETER WORKING ON                 
SAVEKEY1 DS    CL(L'RESKEY)                                                     
TEMPCHAR DS    CL1                                                              
TYPECDE  DS    CL4                                                              
CURUL    DS    CL2                                                              
SAVEUL   DS    CL2                 SAVE UNIT/LEDGER                             
LDGFLDH  DS    CL8                                                              
LDGFLD   DS    CL2                                                              
FLDMAXL  DS    XL1                 MAX  FIELD LENGTH                            
WORK     DS    CL20                                                             
BYTE     DS    CL1                                                              
LWSX     DS    0C                                                               
         EJECT ,                                                                
         SPACE 1                                                                
DFLD     DSECT                                                                  
DFLDSP   DS    AL2                                                              
DFLIND   DS    XL1                                                              
DFLXCLD  EQU   X'80'                                                            
DFLALST  EQU   X'40'                                                            
DFLSLST  EQU   X'20'                                                            
DFLTYPE  DS    AL1                                                              
DFLRPTY  DS    AL1                                                              
DFLROUT  DS    AL1                                                              
DFLLNQ   EQU   *-DFLD                                                           
         EJECT ,                                                                
         SPACE 1                                                                
         PRINT OFF                                                              
       ++INCLUDE ACSCRWRK                                                       
         PRINT ON                                                               
         EJECT ,                                                                
***********************************************************************         
*  DSECT FOR INC PROFILE DEFINITIONS                                  *         
***********************************************************************         
         SPACE 1                                                                
TWAD     DSECT                                                                  
         ORG   SCROVLYH                                                         
       ++INCLUDE ACSCRF3D                                                       
         PRINT ON                                                               
         ORG                                                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013ACSCR0A   08/27/15'                                      
         END                                                                    
