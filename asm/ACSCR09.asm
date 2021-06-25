*          DATA SET ACSCR09    AT LEVEL 004 AS OF 09/02/15                      
*PHASE T60C09A,+0                                                               
***********************************************************************         
*                                                                     *         
* THIS VERSION MATCHES THE UK VERSION LEVEL 04 AS OF 02/17/10         *         
*                                                                     *         
***********************************************************************         
*                                                                               
         TITLE 'RCV PROFILE ELEMENTS EXTRA MAINTANCE'                           
T60C09   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,60C09,RA,RR=RE                                                 
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
         CLI   TWALREC,RECRCVPF    IF   FIRST TIME IN, THEN SET CURSOR          
         BE    SCR02               NO,  SKIP                                    
*                                                                               
SCR01    LA    RE,RCVCODEH         FORMAT CODE FIELD                            
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
         TM    TWASWPST,TWASWAP    SWAP TO NEW RECORD ACTION?                   
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
         GOTO1 AFVAL,RCVCODEH                                                   
         MVC   RESKFORM,FVIFLD                                                  
         BE    VALKEY15                                                         
         TM    RCVCODE+4,FVITHIS   ANY INPUT?                                   
         BZ    *+10                NO                                           
         MVC   SAVFORM,SPACES                                                   
         CLC   SAVFORM,SPACES                                                   
         BNH   IVALEKEY                                                         
         MVC   RCVCODE,SAVFORM                                                  
         OI    RCVCODEH+6,FVOXMT   TRANSMIT                                     
         MVC   RESKFORM,SAVFORM                                                 
*                                                                               
VALKEY15 MVC   SAVFORM,RESKFORM                                                 
         MVC   APRECKEY(L'RESKEY),RESKEY                                        
         LA    R1,IORD+IOACCFIL+IO1                                             
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(R1)       READ FOR UPDATE                              
         GOTO1 AIO                                                              
         BL    VALKEY99            IO ERROR                                     
         BNE   VALKEY20                                                         
         L     R2,AIOAREA1                                                      
         GOTO1 GETTYPE,(R2)                                                     
         MVC   SCRTYP(L'APREPCDE),APREPCDE      MAKE TYPE RCV                   
         OI    SCRTYPH+6,FVOXMT                                                 
         CLC   APREPCDE,AC@RCV                                                  
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
         GOTO1 AFVAL,RCVNMEH                                                    
         BNE   VALREC05            NAME HAS  NOT  BEEN INPUT                    
         GOTO1 ADDNAME,APPARM,(R2),RCVNMEH   ADD  FORMAT NAME                   
         BNE   VALREC99            ON   ERROR,    EXIT                          
*                                                                               
         USING REPTABD,R3                                                       
*                                                                               
VALREC05 GOTO1 AFVAL,RCVTYPEH                                                   
         L     R3,ACTYPTAB                                                      
*                                                                               
VALREC10 CLI   REPCODE,EOT                                                      
         BE    IVALTYPE            INVALID TYPE USED                            
         LR    R1,R3                                                            
         BAS   RE,EXPRPTY                                                       
         CLC   APREPCDE,RCVRPCDE   MATCH REPORT TYPE                            
         BNE   VALREC15            NO, SO GET NEXT REPORT TYPE                  
         SR    R1,R1                                                            
         IC    R1,FVXLEN                                                        
         EXCLC R1,RCVRPTYP,FVIFLD                                               
         BNE   VALREC15            NO, SO GET NEXT REPORT TYPE                  
         TM    REPFLAG,REPDDS      DDS ONLY?                                    
         BZ    VALREC20            NO, SO OK                                    
         TM    CUSTAT,CUSDDS       IS IT A DDS TERMINAL?                        
         BNZ   VALREC20            YES, SO OK                                   
*                                                                               
VALREC15 LA    R3,REPLNQ(,R3)      BUMP TO NEXT REPORT TYPE                     
         B     VALREC10                                                         
*                                                                               
VALREC20 MVC   APREPNUM,REPNUM                                                  
         MVC   APREPCDE,RCVRPCDE                                                
*                                                                               
         DROP  R3                  KEEP IT CLEAN                                
*                                                                               
         GOTO1 ADDREPTY,APPARM,AIOAREA1                                         
         BNE   VALREC99            ON  ERROR, EXIT                              
         GOTO1 GETTYPE,(R2)        RESET APREP VALUES                           
         MVI   LDGFLDH,10          SET DEFAULT LENGTH                           
         MVC   LDGFLD,APREPUL                                                   
         GOTO1 MAKELIST,APPARM,('RFLLDG',LDGFLDH),(R2),                X        
               ('MAXPARM',RCVBLOCK),APELEM                                      
         GOTO1 ADDEL,(R2)          ADD X'C5' LEDGER FILTER                      
         BNE   VALREC99            ON  ERROR, EXIT                              
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
         BNE   VALREC21                                                         
         SR    RF,RF                                                            
         IC    RF,1(,R1)           GET ELEMENT LENGTH                           
         BCTR  RF,0                                                             
         EXMVC RF,APELEM,0(R1)                                                  
*                                                                               
VALREC21 GOTO1 DELEL,(R2)                                                       
         XC    RPFROPT,RPFROPT                                                  
         XC    RPFOPT1,RPFOPT1                                                  
         XC    RPFOPT2,RPFOPT2                                                  
         XC    RPFOPT3,RPFOPT3                                                  
*&&US*&& XC    RPFOPT6,RPFOPT6                                                  
*                                                                               
         CLC   RCVRVRS,APNO                                                     
         BNE   *+8                                                              
         OI    RPFROPT,RPFXRVRS    EXCLUDE REVERSALS                            
         CLC   RCVRVRS,APONLY                                                   
         BNE   *+8                                                              
         OI    RPFROPT,RPFORVRS    REVERSALS ONLY                               
*&&US                                                                           
         CLC   RCVTY56,APNO                                                     
         BE    *+8                                                              
         OI    RPFROPT,RPFTYP56    INCLUDE REVERSALS TYPE 56                    
*                                                                               
         CLC   RCVZTRN,APYES                                                    
         BNE   *+8                                                              
         OI    RPFOPT6,RPFZPST    INCLUDE $0 TRANSACTIONS                       
*                                                                               
*&&                                                                             
         CLC   RCVLOCK,APNO                                                     
         BNE   *+8                                                              
         OI    RPFROPT,RPFXLOCK    EXCLUDE LOCKED ACCOUNTS                      
         CLC   RCVLOCK,APONLY                                                   
         BNE   *+8                                                              
         OI    RPFROPT,RPFOLOCK    LOCKED ACCOUNTS ONLY                         
*                                                                               
         CLC   RCVCSHR,APNO                                                     
         BE    *+8                                                              
         OI    RPFOPT2,RPFSPEC     FILTER SPECS FOR ALTERNATE DATES             
*                                                                               
         CLC   RCVOUTB,APNO                                                     
         BE    *+8                                                              
         OI    RPFROPT,RPFBALO     SHOW OUSTAND BILLS ONLY                      
*&&US                                                                           
         CLC   RCVDRFT,APYES                                                    
         BNE   *+8                                                              
         OI    RPFOPT1,RPFIDRFT    INCLUDE DRAFT ITEMS                          
         CLC   RCVDRFT,APONLY                                                   
         BNE   *+8                                                              
         OI    RPFOPT1,RPFODRFT    DRAFT ITEMS ONLY                             
*&&                                                                             
         CLC   RCVHLQR,APYES       INCLUDE HELD/QUERIED ITEMS?                  
         BE    VALREC24            YES                                          
         CLC   RCVHLQR,APNO        EXCLUDE HELD/QUERIED ITESM?                  
         BNE   VALREC22            NO                                           
         OI    RPFOPT1,RPFXHELD    EXCLUDE HELD ITEMS                           
         OI    RPFOPT2,RPFXQRED    EXCLUDE QUERIED ITEMS                        
*                                                                               
VALREC22 CLC   RCVHLQR,APONLY      INCLUDE ONLY HELD/QUERIED ITEMS              
         BNE   VALREC23            NO                                           
         OI    RPFOPT1,RPFOHELD    HELD ITEMS ONLY                              
         OI    RPFOPT2,RPFOQRED    QUERIED ITEMS ONLY                           
*                                                                               
VALREC23 CLI   RCVHLQR,C'H'        HELD ONLY                                    
         BNE   *+8                                                              
         OI    RPFOPT1,RPFOHELD    HELD ITEMS ONLY                              
         CLI   RCVHLQR,C'Q'        QUERIED ONLY                                 
         BNE   *+8                                                              
         OI    RPFOPT2,RPFOQRED    QUERIED ITEMS ONLY                           
*                                                                               
VALREC24 DS    0H                                                               
*&&UK                                                                           
         CLC   RCVCLRD,APYES       INCLUDE CLEARED ITEMS                        
         BE    VALREC25            YES                                          
         CLC   RCVCLRD,APONLY                                                   
         BNE   *+8                                                              
         OI    RPFOPT3,RPFOCLRD    CLEARED ITEMS ONLY                           
         CLC   RCVCLRD,APNO                                                     
         BNE   *+8                                                              
         OI    RPFOPT3,RPFXCLRD    EXCLUDE CLEARED ITEMS                        
*&&                                                                             
*                                                                               
VALREC25 XC    RPFFLT1(4),RPFFLT1  CLEAR FILTER AREA                            
         MVI   RPFFLT5,0           CLEAR FILTER 5                               
         LA    R4,5                FOR FILTER TYPES                             
         LA    R3,RPFFLT1                                                       
         LA    R1,RCVF1H                                                        
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
         CLM   R4,1,=AL1(2)        SEE IF WE DONE F1-F4                         
         BNE   *+8                 NO NOT YET                                   
         LA    R3,RPFFLT5          POINT TO PROFILE F5 AREA                     
         BCT   R4,VALREC27                                                      
*                                                                               
         DROP  R9                  KEEP IT CLEAN                                
*                                                                               
         L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)          ADD PROFILE ELEMENT X'C4'                    
         BNE   VALREC99            ON  ERROR, EXIT                              
*                                                                               
         DROP  R2                  KEEP IT CLEAN                                
*                                                                               
         MVC   SAVEKEY1,IOKEY                                                   
*                                                                               
*                                  ************************************         
VALREC30 DS    0H                  * VALIDATE SPECIFIC ACCOUNT(S)     *         
*                                  ************************************         
         GOTO1 MAKELIST,APPARM,('RFLACC',RCVACCTH),(R2),               X        
               ('MAXPARM',RCVBLOCK),APELEM                                      
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VALREC34            NO   INPUT                                   
         BP    VALREC99            BAD  INPUT                                   
*                                  GOOD INPUT                                   
         GOTO1 VALLIST,APPARM,APREPUL,(RCVBLOCK,RCVBLOCK+12),1                  
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VALREC31            NO   LIST                                    
         BP    VALREC99            BAD  LIST                                    
*                                  GOOD LIST                                    
         CLI   NPARMS,1            ONLY 1 LIST REQUESTED ?                      
         BE    VALRC33A            YES, ADD ACCOUNT LIST                        
         B     IVAL2MNY            NO,  TOO MANY LISTS                          
*                                                                               
VALREC31 DS    0H                  VALIDATE ACCOUNT(S)                          
         SR    R6,R6                                                            
         IC    R6,NPARMS                                                        
         LA    R4,RCVBLOCK                                                      
         LA    R2,IOKEY                                                         
*                                                                               
         USING ACTRECD,R2                                                       
*                                                                               
VALREC32 DS    0H                                                               
         CLI   0(R4),LACCOUNT      IS   THE LENGTH OF ACCOUNT > 12 ?            
         BH    IVALACCT            YES, INVALID ACCOUNT                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUABIN      COMPANY CODE                                 
         MVC   ACTKUNT(2),APREPUL  UNIT LEDGER                                  
         MVC   CURUL,APREPUL       CURRENT UNIT/LEDGER                          
         MVC   ACTKACT,12(R4)      ACCOUNT IS IN RCVBLOCK                       
         GOTO1 AIO,IORD+IOACCFIL+IO2                                            
         BE    VALREC33            VALID ACCOUNT                                
         MVI   BYTE,C'N'           CHECK FOR WILD CARD                          
         BAS   RE,WILDCARD                                                      
         BNE   IVALACCT            INVALID ACCOUNT                              
*                                                                               
VALREC33 LA    R4,32(,R4)          NEXT BLOCK IN RCVBLOCK                       
         BCT   R6,VALREC32                                                      
*                                                                               
         DROP  R2                  KEEP IT  CLEAN                               
*                                                                               
VALRC33A DS    0H                                                               
         L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)          ADD SPECIFIC ACCOUNT ELEMENT                 
         BNE   VALREC99            ON  ERROR, EXIT                              
*                                                                               
*                                  ************************************         
VALREC34 DS    0H                  * VALIDATE BILLING SOURCE CODE(S)  *         
*                                  ************************************         
         GOTO1 MAKELIST,APPARM,('RFLBSR',RCVMNH),(R2),                 X        
               ('MAXPARM',RCVBLOCK),APELEM                                      
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VALREC38            NO   INPUT                                   
         BP    VALREC99            BAD  INPUT                                   
*                                  GOOD INPUT                                   
         CLI   APREPNUM,REP#RCV                                                 
         BNE   IVALIPUT                                                         
         GOTO1 VALLIST,APPARM,=C'ME',(RCVBLOCK,RCVBLOCK+12),LITTMED             
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VALREC35            NO   LIST                                    
         BP    VALREC99            BAD  LIST                                    
*                                  GOOD LIST                                    
         CLI   NPARMS,1                                                         
         BE    VALREC37            ADD BILLING SOURCE CODES LIST                
         B     IVAL2MNY            TOO MANY LISTS                               
*                                                                               
VALREC35 DS    0H                                                               
         SR    R6,R6                                                            
         IC    R6,NPARMS                                                        
         LA    R4,RCVBLOCK                                                      
*                                                                               
*                                  NOTE: WE CANNOT VALIDATE THE                 
*                                        BILLING ACCOUNT ID                     
VALREC36 DS    0H                                                               
         CLI   0(R4),12            IS THE LENGTH OF BILLING SRC > 12 ?          
         BH    IVALBSC             YES, INVALID                                 
         CLI   0(R4),0             IS THE LENGTH OF BILLING SRC = 0 ?           
         BE    IVALBSC             YES, INVALID                                 
         LA    R4,32(,R4)          NO,  GET TO NEXT BLOCK IN RCVBLOCK           
         BCT   R6,VALREC36         TEST NEXT BILLING ACCOUNT                    
*                                                                               
VALREC37 DS    0H                  ADD BILLING SOURCE CODES                     
         L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)          ADD BILLING SOURCE CODES                     
         BNE   VALREC99            ON  ERROR, EXIT                              
*                                                                               
*                                  ************************************         
VALREC38 DS    0H                  * VALIDATE CLIENT LIST             *         
*                                  ************************************         
*&&US                                                                           
         GOTO1 MAKELIST,APPARM,('RFLCLI',RCVCLIH),(R2),                X        
               ('MAXPARM',RCVBLOCK),APELEM                                      
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VALREC43            NO   INPUT                                   
         BP    VALREC99            BAD  INPUT                                   
*                                  GOOD INPUT                                   
*        CLI   APREPNUM,REP#RCV                                                 
*        BNE   IVALIPUT                                                         
         GOTO1 VALLIST,APPARM,=C'SJ',(RCVBLOCK,RCVBLOCK+12),0                   
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VALREC39            NO   LIST                                    
         BP    VALREC99            BAD  LIST                                    
*                                  GOOD LIST                                    
         CLI   NPARMS,2            ONLY ALLOWED ON FOR A LIST                   
         BL    VALREC42            ADD  THE VALID LIST                          
         BH    IVAL2MNY                                                         
         GOTO1 VALLIST,APPARM,=C'SJ',                                  X        
               (RCVBLOCK+L'RCVBLOCK,RCVBLOCK+L'RCVBLOCK+12),0                   
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
         LA    R4,RCVBLOCK                                                      
         LA    R2,IOKEY                                                         
*                                                                               
         USING ACTRECD,R2                                                       
*                                                                               
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
VALREC41 LA    R4,32(,R4)          NEXT BLOCK IN RCVBLOCK                       
         BCT   R6,VALREC40                                                      
*                                                                               
         DROP  R2                  KEEP IT CLEAN                                
*                                                                               
VALREC42 DS    0H                                                               
         L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)          ADD CLIENT LIST                              
         BNE   VALREC99            ON  ERROR, EXIT                              
*&&                                                                             
*                                  ************************************         
VALREC43 DS    0H                  * VALIDATE OFFICE                  *         
*                                  ************************************         
         GOTO1 MAKELIST,APPARM,('RFLOFF',RCVOFFH),(R2),                X        
               ('MAXPARM',RCVBLOCK),APELEM                                      
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VALREC50            NO   INPUT                                   
         BP    VALREC99            BAD  INPUT                                   
*                                  GOOD INPUT                                   
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
         GOTO1 VOFFICE,APPARM,('VOFTYPEB',RCVBLOCK),(BYTE,(R4))                 
         BNE   VALREC99            BAD    INPUT                                 
*                                                                               
         L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)          ADD    OFFICE     LIST                       
         BNE   VALREC99            ON     ERROR,     EXIT                       
*                                                                               
*                                  ************************************         
VALREC50 DS    0H                  * TRANSACTION TYPE(S)              *         
*                                  ************************************         
         GOTO1 MAKELIST,APPARM,('RFLTTYPE',RCVITTYH),(R2),             X        
               ('MAXPARM',RCVBLOCK),APELEM                                      
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VALREC55            NO   INPUT                                   
         BP    VALREC99            BAD  INPUT                                   
*                                  GOOD INPUT                                   
         USING RFLELD,R9                                                        
*                                                                               
         SR    R6,R6                                                            
         IC    R6,NPARMS           SCAN ENTRIES = NUMBER OF TYPES               
         LA    R9,APELEM                                                        
         LA    RF,RFLLNQ(,R6)      NEW  LENGTH OF ELEMENT                       
         STC   RF,RFLLN            STORE IN ELEMENT                             
         GOTO1 CNVTTYPE,APPARM,(C'N',RCVBLOCK),(NPARMS,RFLDATA)                 
         BNE   VALREC99                                                         
         L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)          ADD  INCLUDE TRANS TYPE                      
         BNE   VALREC99            ON   ERROR, EXIT                             
*                                                                               
         DROP  R9                                                               
*                                                                               
*                                  ************************************         
VALREC55 DS    0H                  * VALIDATE VAT REGION              *         
*                                  ************************************         
*&&UK                                                                           
         GOTO1 MAKELIST,APPARM,('RFLVATRG',RCVVATRH),(R2),             X        
               ('MAXPARM',RCVBLOCK),APELEM                                      
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VALREC60            NO   INPUT                                   
         BP    VALREC99            BAD  INPUT                                   
*                                  GOOD INPUT                                   
         USING RFLELD,R9                                                        
         LA    R9,APELEM           ->   FILTER DATA ELEMENT                     
         TM    RFLIND,RFLXCLD      EXCLUDE EXCEPTION ?                          
         BO    IVALIPUT            YES, INVALID INPUT                           
*                                                                               
         SR    R6,R6                                                            
         IC    R6,NPARMS                                                        
         LA    R4,RCVBLOCK                                                      
*                                                                               
VALREC58 DS    0H                                                               
         CLI   0(R4),1             HAS  TO BE 1 CHARACTER                       
         BH    IVALIPUT                                                         
         CLI   12(R4),C'N'         NATIONAL                                     
         BE    *+8                                                              
         CLI   12(R4),C'E'         EU                                           
         BE    *+12                                                             
         CLI   12(R4),C'X'         FOREIGN BUT NOT EU                           
         BNE   IVALIPUT                                                         
*                                                                               
         LA    R4,32(,R4)                                                       
         BCT   R6,VALREC58                                                      
*                                                                               
         L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)          ADD  VAT REGION ELEMENT                      
         BNE   VALREC99                                                         
         DROP  R9                                                               
*&&                                                                             
VALREC60 DS    0H                  LABEL FOR ANY FUTURE FILTERS                 
*                                                                               
         MVC   IOKEY,SAVEKEY1      RESTORE THE KEY                              
*                                                                               
*                                  ************************************         
VALREC95 DS    0H                  * UPDATE THE RECORD WITH ALL THE   *         
*                                  * NEW ELEMENTS                     *         
*                                  ************************************         
         GOTO1 ADDID,APPARM,AIOAREA1                                            
         BNE   VALREC99            ON     ERROR, EXIT                           
         LA    R1,IOADD+IOACCFIL+IO1                                            
         TM    APINDS,APIOKADD     ADD    A      RECORD ?                       
         BO    VALREC97                                                         
         LA    R1,IOWRITE+IOACCFIL+IO1                                          
         TM    APINDS,APIOKCHA     CHANGE A      RECORD ?                       
         BO    VALREC97                                                         
         DC    H'0'                WHAT   THE    HELL - ABEND !                 
*                                                                               
VALREC97 GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD    WRITE  OR SOMETHING DUDE              
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
*                                                                               
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
*                                                                               
         OI    RFLIND,RFLWILD      TURN ON THE BIT                              
         CR    RE,RE               WILDCARD IS GOOD                             
         B     XIT                                                              
*                                                                               
         DROP  R1                                                               
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*                        DISPLAY RCV PROFILE DATA                     *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
DISREC   L     R2,AIOAREA1         PROFILE ELEMENT                              
         TWAXC RCVNMEH,RCVTABH                                                  
         GOTO1 GETNAME,APPARM,(R2),RCVNMEH                                      
         GOTO1 GETPER,APPARM,(R2),RCVOWNH                                       
*                                                                               
         MVC   RCVTYPE(L'APREPTYP),APREPTYP                                     
         MVC   RCVOUTB,APNO        DEFAULT, SHOW OUTSTANDING BAL. ONLY          
         MVC   RCVLOCK,APYES       DEFAULT, SHOW LOCKED ACCOUNTS                
         MVC   RCVRVRS,APYES       DEFAULT, SHOW REVERSALS                      
*&&US*&& MVC   RCVTY56,APNO        DEFAULT, EXCLUDE TYPE 56                     
         MVC   RCVCSHR,APNO        DEFAULT, NOT CASH RECEIPT                    
*&&US*&& MVC   RCVDRFT,APNO        DEFAULT, NOT TO INCLUDE DRAFT ITEMS          
         MVC   RCVHLQR,APYES       DEFAULT, SHOW HELD/QUERIED ITEMS             
*&&UK*&& MVC   RCVCLRD,APYES       DEFAULT, SHOW CLEARED ITEMS                  
*&&US*&& MVC   RCVZTRN,APNO        DEFAULT, EXCLUDE $0 TRANSACTIONS             
*&&UK*&& MVC   RCVVATR,SPACES      DEFAULT, SHOW ALL                            
*                                                                               
         USING RFLELD,R1                                                        
*                                                                               
         MVI   APELCODE,RFLELQ     FILTER LIST ELEMENT X'C5'                    
         GOTO1 GETEL,(R2)                                                       
         BNE   DISREC30                                                         
*                                                                               
DISREC10 CLI   RFLSEQ,0            HIGH LEVEL FILTERS ONLY                      
         BNE   DISREC25            LOOP                                         
         SR    RF,RF CLEAR REGISTER - NOTHING TO DISPLAY                        
         SR    R6,R6                                                            
         IC    R6,RFLLN                                                         
         SH    R6,=Y(RFLLNQ+1)                                                  
         BM    DISREC25            SKIP ELEMENT IF LENGTH TOO SMALL             
*                                                                               
         CLI   RFLTYPE,RFLACC      ACCOUNT FILTER LIST                          
         BNE   *+8                                                              
         LA    RF,RCVACCTH         SPECIFIC ACCOUNT FILTER HEADER               
*                                                                               
         CLI   RFLTYPE,RFLBSR      BILLING SOURCE LIST                          
         BNE   *+8                                                              
         LA    RF,RCVMNH           BILLING SOURCE HEADER                        
*                                                                               
*&&US                                                                           
         CLI   RFLTYPE,RFLCLI      CLIENT FILTER LIST                           
         BNE   *+8                                                              
         LA    RF,RCVCLIH          CLIENT HEADER                                
*&&                                                                             
*                                                                               
         CLI   RFLTYPE,RFLOFF      OFFICE FILTER LIST                           
         BNE   *+8                                                              
         LA    RF,RCVOFFH          OFFICE HEADER                                
*                                                                               
*&&UK                                                                           
         CLI   RFLTYPE,RFLVATRG    VAT REGION LIST                              
         BNE   *+8                                                              
         LA    RF,RCVVATRH         VAT REGION HEADER                            
*&&                                                                             
*                                                                               
         CLI   RFLTYPE,RFLTTYPE    TRANSACTION TYPE                             
         BNE   DISREC15                                                         
         LR    R0,R1                                                            
         GOTO1 CNVTTYPE,APPARM,(C'S',(R0)),(X'00',RCVITTY)                      
         LR    R1,R0                                                            
         OI    RCVITTYH+6,FVOXMT                                                
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
         LA    R4,5                FIVE FILTER TYPES                            
         LA    R3,RPFFLT1                                                       
         LA    R1,RCVF1H                                                        
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
         CLM   R4,1,=AL1(2)                                                     
         BNE   *+8                                                              
         LA    R3,RPFFLT5          POINT TO PROFILE F5 INSTEAD                  
         BCT   R4,DISREC32                                                      
*                                                                               
         TM    RPFROPT,RPFXRVRS    EXCLUDE REVERSALS                            
         BZ    *+10                                                             
         MVC   RCVRVRS,APNO        YES EXCLUDE                                  
         TM    RPFROPT,RPFORVRS    REVERSALS ONLY                               
         BZ    *+10                                                             
         MVC   RCVRVRS,APONLY      YES REVERSALS ONLY                           
         OI    RCVRVRSH+6,FVOXMT   TRANSMIT                                     
*                                                                               
*&&US                                                                           
         TM    RPFROPT,RPFTYP56    INCLUDE REVERSALS TYPE 56                    
         BZ    *+10                                                             
         MVC   RCVTY56,APYES       YES INCLUDE                                  
         OI    RCVTY56H+6,FVOXMT   TRANSMIT                                     
*&&                                                                             
*                                                                               
         TM    RPFROPT,RPFXLOCK    EXCLUDE LOCKED ACCOUNTS                      
         BZ    *+10                                                             
         MVC   RCVLOCK,APNO        YES EXCLUDE                                  
         TM    RPFROPT,RPFOLOCK    LOCKED ACCOUNTS ONLY                         
         BZ    *+10                                                             
         MVC   RCVLOCK,APONLY      YES LOCKED ACCOUNTS ONLY                     
         OI    RCVLOCKH+6,FVOXMT   TRANSMIT                                     
*                                                                               
         TM    RPFOPT2,RPFSPEC                                                  
         BZ    *+10                                                             
         MVC   RCVCSHR,APYES       YES                                          
         OI    RCVCSHRH+6,FVOXMT   TRANSMIT                                     
*                                                                               
         TM    RPFROPT,RPFBALO     BALANCE OUTSTANDING ONLY                     
         BZ    *+10                                                             
         MVC   RCVOUTB,APYES       YES                                          
         OI    RCVOUTBH+6,FVOXMT   TRANSMIT                                     
*                                                                               
*&&US                                                                           
         TM    RPFOPT1,RPFIDRFT    INCLUDE DRAFT ITEMS                          
         BZ    *+10                                                             
         MVC   RCVDRFT,APYES       YES INCLUDE                                  
*                                                                               
         TM    RPFOPT1,RPFODRFT    DRAFT ITEMS ONLY                             
         BZ    *+10                                                             
         MVC   RCVDRFT,APONLY      YES                                          
         OI    RCVDRFTH+6,FVOXMT   TRANSMIT                                     
*                                                                               
         TM    RPFOPT6,RPFZPST                                                  
         BZ    *+10                                                             
         MVC   RCVZTRN,APYES       INCLUDE $0 TRANSACTIONS                      
         OI    RCVZTRNH+6,FVOXMT   TRANSMIT                                     
*&&                                                                             
*                                                                               
         TM    RPFOPT1,RPFXHELD    EXCLUDE HELD ITEMS                           
         BZ    *+14                                                             
         MVC   RCVHLQR,APNO        EXCLUDE HELD, QUERIED, CLEARED               
         B     DISREC40                                                         
         SR    R1,R1                                                            
         TM    RPFOPT1,RPFOHELD    HELD ITEMS ONLY                              
         BZ    *+12                                                             
         LA    R1,1(,R1)                                                        
         MVI   RCVHLQR,C'H'        YES                                          
         TM    RPFOPT2,RPFOQRED    QUERIED ITEMS ONLY                           
         BZ    *+12                                                             
         LA    R1,1(,R1)                                                        
         MVI   RCVHLQR,C'Q'        YES                                          
         CH    R1,=H'02'                                                        
         BL    DISREC40                                                         
         MVC   RCVHLQR,APONLY      ONLY HELD, QUERIED, CLEARED ITEMS            
*                                                                               
DISREC40 DS    0H                                                               
         OI    RCVHLQRH+6,FVOXMT   TRANSMIT                                     
*&&UK                                                                           
         TM    RPFOPT3,RPFOCLRD    CLEARED ITEMS ONLY                           
         BZ    *+10                                                             
         MVC   RCVCLRD,APONLY      ONLY                                         
         TM    RPFOPT3,RPFXCLRD    EXCLUDE CLEARED ITEMS                        
         BZ    *+10                                                             
         MVC   RCVCLRD,APNO        NO                                           
         OI    RCVCLRDH+6,FVOXMT   TRANSMIT                                     
*&&                                                                             
*                                                                               
DISREC99 DS    0H                                                               
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   DISRC999                                                         
         CLI   APACTN,ACTDIS       IN   DISPLAY MODE ?                          
         BE    DISRC999            YES, SKIP                                    
         CLI   TWALREC,RECRCVPF    IF   FIRST TIME IN, THEN SET CURSOR          
         BE    DISRC999            NO,  SKIP                                    
*                                                                               
         LA    RE,RCVCODEH         FORMAT CODE FIELD                            
         ST    RE,APCURSOR         SET  APPLICATION CURSOR                      
*                                                                               
DISRC999 DS    0H                                                               
         B     EXIT                                                             
*                                                                               
         DROP  R1                                                               
         EJECT ,                                                                
         SPACE 1                                                                
         USING REPTABD,R1                                                       
         SPACE 1                                                                
EXPRPTY  MVC   RCVRPCDE,REPCODE                                                 
         MVC   RCVRPTYP,REPSTYPE                                                
         CLI   RCVRPCDE,ESCHIGHQ   TEST FOR ESCAPE SEQUENCE                     
         BNLR  RE                                                               
*                                                                               
         DROP  R1                                                               
*                                                                               
         STM   RE,R1,SVREGS                                                     
         GOTO1 VDICTAT,APPARM,C'TU  ',('RCVRPLNQ',RCVRPCDE),0                   
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
IVALCLI  MVC   FVMSGNO,=AL2(ACECLI)       INVALID CLEINT                        
         MVC   FVXTRA(20),12(R4)                                                
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALIPUT MVC   FVMSGNO,=AL2(FVFNOTV)      INVALID INPUT                         
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALBSC  MVC   FVMSGNO,=AL2(ACEIBSC)      INVALID BILLING SOURCE                
         MVC   FVXTRA(20),12(R4)                                                
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALEXIT DS    0H                                                               
         B     EXIT                                                             
         EJECT ,                                                                
         SPACE 1                                                                
SCRTXT   DS    0F                                                               
         DC    AL4(RCVRC02H-TWAD,1652)                                          
         DC    AL4(RCVRC09H-TWAD,1647)                                          
*&&US*&& DC    AL4(RCVRC10H-TWAD,1656)                                          
         DC    AL4(RCVRC11H-TWAD,1646)                                          
         DC    AL4(RCVRC12H-TWAD,1640)                                          
         DC    AL4(RCVRC13H-TWAD,1657)                                          
*&&US*&& DC    AL4(RCVRC14H-TWAD,1649)                                          
         DC    AL4(RCVRC15H-TWAD,1659)                                          
*&&US*&& DC    AL4(RCVRC16H-TWAD,5683)    INCLUDE 0 TRANSACTIONS                
*&&UK*&& DC    AL4(RCVRC16H-TWAD,1690)                                          
*&&UK*&& DC    AL4(RCVRC17H-TWAD,5691)                                          
         DC    X'FF'                                                            
         SPACE 4                                                                
         LTORG                                                                  
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*                   DSECT FOR LOCAL WORKING STORAGE                   *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
LWSD     DSECT                                                                  
MAXPARM  EQU   20                                                               
PENNY    EQU   C'P'                                                             
*                                                                               
DUB      DS    D                                                                
*                                                                               
SVREGS   DS    6A                                                               
*                                                                               
ONEXONLY DS    XL1                 ONE  TIME ONLY FLAG                          
PARM_N   DS    XL1                 CURRENT   PARAMETER WORKING ON               
TEMPCHAR DS    CL1                                                              
TYPECDE  DS    CL4                                                              
CURUL    DS    CL2                                                              
WORK     DS    CL20                                                             
BYTE     DS    CL1                                                              
*                                                                               
LDGFLDH  DS    CL8                                                              
LDGFLD   DS    CL2                                                              
*                                                                               
RCVRPCDE DS    CL(L'REPCODE)                                                    
RCVRPTYP DS    CL(L'REPSTYPE)                                                   
RCVRPLNQ EQU   *-RCVRPCDE                                                       
*                                                                               
SAVEKEY1 DS    CL(L'RESKEY)                                                     
RCVBLOCK DS    (MAXPARM)CL32                                                    
*                                                                               
LWSX     DS    0C                                                               
         EJECT ,                                                                
*        ACSCRWRK                                                               
         PRINT OFF                                                              
       ++INCLUDE ACSCRWRK                                                       
         PRINT ON                                                               
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*                    DSECT FOR RCV PROFILE DEFINITIONS                *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
TWAD     DSECT                                                                  
         ORG   SCROVLYH                                                         
       ++INCLUDE ACSCRF4D                                                       
         ORG                                                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004ACSCR09   09/02/15'                                      
         END                                                                    
